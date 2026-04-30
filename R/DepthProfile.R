#' Depth-parameter characteristic names
#'
#' Returns the set of characteristic names that represent depth parameters
#' (e.g., Secchi, thalweg), which are handled specially in depth consolidation
#' and plotting.
#'
#' @return Character vector of characteristic names treated as depth parameters.
#'
#' @noRd
.depth_param_names <- function() {
  c(
    "DEPTH, SECCHI DISK DEPTH",
    "DEPTH, SECCHI DISK DEPTH (CHOICE LIST)",
    "DEPTH, SECCHI DISK DEPTH REAPPEARS",
    "TRANSPARENCY, SECCHI TUBE WITH DISK",
    "DEPTH, DATA-LOGGER (NON-PORTED)",
    "DEPTH, DATA-LOGGER (PORTED)",
    "RBP STREAM DEPTH - RIFFLE",
    "RBP STREAM DEPTH - RUN",
    "THALWEG DEPTH",
    "SAMPLING DEPTH IN"
  )
}

#' Normalize "null" or NULL numeric inputs
#'
#' Converts character "null" (case-insensitive) or NULL to NA_real_ for
#' numeric options such as surfacevalue/bottomvalue. Leaves other values
#' unchanged.
#'
#' @param x A value expected to be numeric, the character "null", or NULL.
#'
#' @return A numeric value or NA_real_.
#'
#' @examples
#' # .normalize_null_numeric("null") -> NA_real_
#' # .normalize_null_numeric(NULL)    -> NA_real_
#' # .normalize_null_numeric(2)       -> 2
#'
#' @noRd
.normalize_null_numeric <- function(x) {
  if (is.character(x) && tolower(x) == "null") {
    return(NA_real_)
  }
  if (is.null(x)) {
    return(NA_real_)
  }
  x
}

#' Ensure depth-category columns exist
#'
#' Ensures the columns produced by TADA_FlagDepthCategory are present. If
#' missing, runs TADA_FlagDepthCategory with the supplied thresholds.
#' When allow_na_thresholds is TRUE and one or both thresholds are NA,
#' the function runs with defaults and then blanks out categories that
#' cannot be determined.
#'
#' @param .data A TADA-compatible data.frame.
#' @param surfacevalue Numeric or NA. Threshold for Surface category (m).
#' @param bottomvalue Numeric or NA. Threshold for Bottom category (m).
#' @param allow_na_thresholds Logical; if TRUE, permits NA thresholds and
#'   post-adjusts depth-category flags accordingly.
#'
#' @return A data.frame with TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Unit,
#'   TADA.ConsolidatedDepth.Bottom, and TADA.DepthCategory.Flag present.
#'
#' @noRd
.ensure_depth_flag_columns <- function(
  .data,
  surfacevalue = 2,
  bottomvalue = 2,
  allow_na_thresholds = FALSE
) {
  needed <- c(
    "TADA.ConsolidatedDepth",
    "TADA.ConsolidatedDepth.Unit",
    "TADA.ConsolidatedDepth.Bottom",
    "TADA.DepthCategory.Flag"
  )

  if (all(needed %in% names(.data))) {
    message(
      "TADA: Necessary columns from TADA_FlagDepthCategory function are included in the data frame."
    )
    return(.data)
  }

  if (allow_na_thresholds && (is.na(surfacevalue) || is.na(bottomvalue))) {
    message(
      "TADA: Running TADA_FlagDepthCategory to add columns; NA thresholds requested, post-adjusting flags."
    )
    # run with defaults and then blank out flags that cannot be determined
    tmp <- TADA_FlagDepthCategory(.data, surfacevalue = 2, bottomvalue = 2)
    if (is.na(surfacevalue) && is.na(bottomvalue)) {
      tmp$TADA.DepthCategory.Flag <- NA_character_
    } else if (is.na(surfacevalue)) {
      tmp$TADA.DepthCategory.Flag <- ifelse(
        tmp$TADA.DepthCategory.Flag %in% c("Surface", "Middle"),
        NA_character_,
        tmp$TADA.DepthCategory.Flag
      )
    } else if (is.na(bottomvalue)) {
      tmp$TADA.DepthCategory.Flag <- ifelse(
        tmp$TADA.DepthCategory.Flag %in% c("Bottom", "Middle"),
        NA_character_,
        tmp$TADA.DepthCategory.Flag
      )
    }
    return(tmp)
  }

  message(
    "TADA: Running TADA_FlagDepthCategory function to add required columns to data frame."
  )
  TADA_FlagDepthCategory(
    .data,
    surfacevalue = surfacevalue,
    bottomvalue = bottomvalue
  )
}

#' Drop mean-aggregated rows from data
#'
#' Removes rows that were created by dailyagg = "avg" in TADA_FlagDepthCategory,
#' identified by ResultIdentifier values prefixed with "TADA-".
#'
#' @param .data A data.frame that may include mean-aggregated rows.
#'
#' @return The input data.frame with any "TADA-" ResultIdentifier rows removed.
#'
#' @noRd
.drop_avg_aggregates <- function(.data) {
  if (!"ResultIdentifier" %in% names(.data)) {
    return(.data)
  }
  dplyr::filter(.data, !grepl("^TADA-", .data$ResultIdentifier))
}

#' TADA_FlagDepthCategory
#'
#' This function creates a new column, TADA.DepthCategory.Flag with values: "No
#' depth info", "Surface", "Bottom", and
#' "Middle" when multiple depths are available.
#' Categories are: less than 2m (or user specified value) depth = "Surface",
#' from bottom up to 2m (or user specified value) from bottom = "Bottom", and
#' all depths in between the Surface and Bottom are assigned to the "Middle"
#' category.
#'
#' When more than one result is available for a TADA.MonitoringLocationIdentifier,
#' ActivityStartDate, OrganizationIdentifier, and TADA.CharacteristicName, the
#' user can choose a single result value (average, max, or min value) to use
#' for that day and location. If results vary with depth, the user may also
#' define whether the daily aggregation occurs over each depth category
#' (surface, middle, or bottom) or for the entire depth profile.
#'
#' @param .data TADA dataframe which must include the columns
#' TADA.ActivityDepthHeightMeasure.MeasureValue,
#' TADA.ResultDepthHeightMeasure.MeasureValue,
#' TADA.ActivityBottomDepthHeightMeasure.MeasureValue, and
#' ActivityRelativeDepthName.
#'
#' @param dailyagg Character argument; with options "none", "avg", "min", or
#' "max". The default is dailyagg = "none". When dailyagg = "none", all results
#' will be retained. When dailyagg == "avg", the mean value in each group of
#' results (as determined by the depth category) will be identified or calculated for each
#' TADA.MonitoringLocation, ActivityDate, Organization ID, and TADA.CharacteristicName combination.
#' When dailyagg == "min" or when dailyagg == "max", the min or max
#' value in each group of results (as determined by the depth category) will
#' be identified or calculated for each TADA.MonitoringLocation, ActivityDate, and
#' TADA.CharacteristicName combination. An additional column, TADA.DepthProfileAggregation.Flag will
#' be added to describe aggregation.
#'
#' @param bycategory character argument with options "no", "all", "surface", "middle",
#' "bottom". The default is bycategory = "no" which means that any aggregate values
#' are based on the entire water column at a Monitoring Location. When bycategory
#' = "all", any aggregate values are determined for each depth category for each
#' Monitoring Location. When bycategory = "surface", "middle", or "bottom", the data
#' frame is filtered only to include results in the selected category and aggregate
#' values are determined ONLY for results with TADA.DepthCategory.Flags
#' "Surface", "Bottom", or "Middle"
#' results respectively.
#'
#' @param bottomvalue numeric argument. The user enters how many meters from the
#' bottom should be included in the "Bottom" category. Default is
#' bottomvalue = 2. If bottomvalue = "null", "Bottom" and "Middle" results cannot
#' be identified, however TADA.ConsolidatedDepth and TADA.ConsolidatedDepth.Bottom
#' will still be determined.
#'
#' @param surfacevalue numeric argument. The user enters how many meters from the
#' surface should be included in the "Surface" category. Default is surfacevalue = 2.
#' If surfacevalue = "null", "Surface" and "Middle" results cannot
#' be identified, however TADA.ConsolidatedDepth and TADA.ConsolidatedDepth.Bottom
#' will still be determined.
#'
#' @param aggregatedonly Boolean argument with options TRUE or FALSE. The
#' default is aggregatedonly = FALSE which means that all results are returned.
#' When aggregatedonly = TRUE, only aggregate values are returned.
#' Note: aggregatedonly = TRUE has no effect when dailyagg = "none" and will raise an error
#' (no aggregates to return).
#'
#' @param clean Boolean argument with options TRUE or FALSE. The
#' default is clean = FALSE which means that all results are returned.
#' When clean = TRUE, only aggregate results which can be assigned to a depth
#' category are included in the returned dataframe.
#'
#' @return The same input TADA dataframe with additional columns TADA.DepthCategory.Flag,
#' TADA.DepthProfileAggregation.Flag, TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Bottom,
#' and TADA.ConsolidatedDepth.Unit. The consolidated depth fields are created by reviewing
#' multiple WQC columns where users may input depth information. If dailyagg = "avg",
#' "min", or "max", aggregation status is described in TADA.DepthProfileAggregation.Flag.
#' In the case of dailyagg = "avg", additional rows to display averages will be
#' added to the data frame. Aggregated rows are identified by ResultIdentifier prefixed
#' with "TADA-". When dailyagg = "avg", the aggregated result retains metadata from a
#' deterministically selected representative record (first by ResultIdentifier within the group).
#'
#' @export
#'
#' @examples
#' # Load data frame
#' utils::data(Data_6Tribes_5y)
#'
#' # assign TADA.DepthCategory.Flag with no aggregation
#' Data_6Tribs_5y_DepthCat <- TADA_FlagDepthCategory(Data_6Tribes_5y)
#'
#' # assign TADA.DepthCategory.Flag and determine average values by depth
#' # category and returning only aggregate values
#' Data_6Tribs_5y_Mean <- TADA_FlagDepthCategory(Data_6Tribes_5y,
#'   bycategory = "all", dailyagg = "avg", aggregatedonly = FALSE
#' )
#'
TADA_FlagDepthCategory <- function(
  .data,
  bycategory = "no",
  bottomvalue = 2,
  surfacevalue = 2,
  dailyagg = "none",
  aggregatedonly = FALSE,
  clean = FALSE
) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.ActivityDepthHeightMeasure.MeasureValue",
    "TADA.ResultDepthHeightMeasure.MeasureValue",
    "TADA.ActivityBottomDepthHeightMeasure.MeasureValue",
    "ActivityRelativeDepthName",
    "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
    "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
    "TADA.CharacteristicName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.ResultMeasureValue",
    "ResultIdentifier",
    "TADA.MonitoringLocationIdentifier",
    "OrganizationIdentifier",
    "ActivityStartDate"
  )
  TADA_CheckColumns(.data, expected_cols)
  # check aggregatedonly is boolean
  TADA_CheckType(aggregatedonly, "logical")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # additional input and enum validation
  TADA_CheckType(.data, "data.frame", "Input object")
  valid_bycategory <- c("no", "all", "surface", "middle", "bottom")
  if (!bycategory %in% valid_bycategory) {
    stop(
      "TADA_FlagDepthCategory: bycategory must be one of: 'no', 'all', 'surface', 'middle', 'bottom'."
    )
  }
  valid_dailyagg <- c("none", "avg", "min", "max")
  if (!dailyagg %in% valid_dailyagg) {
    stop(
      "TADA_FlagDepthCategory: dailyagg must be one of: 'none', 'avg', 'min', 'max'."
    )
  }

  # normalize 'null' and NULL inputs to NA_real_
  surfacevalue <- .normalize_null_numeric(surfacevalue)
  bottomvalue <- .normalize_null_numeric(bottomvalue)

  # validate types if provided
  if (!is.na(surfacevalue) && !is.numeric(surfacevalue)) {
    stop(
      "TADA_FlagDepthCategory: surfacevalue must be numeric, NULL, or 'null'."
    )
  }
  if (!is.na(bottomvalue) && !is.numeric(bottomvalue)) {
    stop(
      "TADA_FlagDepthCategory: bottomvalue must be numeric, NULL, or 'null'."
    )
  }

  # execute function after checks are passed

  depthcat.list <- c("Surface", "Bottom", "Middle")

  ard.ref <- utils::read.csv(system.file(
    "extdata",
    "TADAActivityRelativeDepthRef.csv",
    package = "EPATADA"
  )) |>
    dplyr::rename(
      ARD_Category = TADA.DepthCategory.Flag,
      ActivityRelativeDepthName = Name
    ) |>
    dplyr::select(ARD_Category, ActivityRelativeDepthName)

  depth.count <- .data |>
    dplyr::filter(
      !is.na(TADA.ActivityDepthHeightMeasure.MeasureValue) |
        !is.na(TADA.ResultDepthHeightMeasure.MeasureValue)
    ) |>
    nrow()

  # derive cattype after bycategory validation
  if (bycategory == "no") {
    cattype <- "for the entire depth profile"
  }
  if (bycategory == "all") {
    cattype <- "for each depth category"
  }
  if (bycategory == "bottom") {
    cattype <- "for Bottom"
  }
  if (bycategory == "middle") {
    cattype <- "for Middle"
  }
  if (bycategory == "surface") {
    cattype <- "for Surface"
  }

  depth.params <- .depth_param_names()

  if (depth.count > 0) {
    message(paste(
      "TADA_FlagDepthCategory: checking data set for depth values. ",
      depth.count,
      " results have depth values available.",
      sep = ""
    ))

    message("TADA_FlagDepthCategory: assigning depth categories.")

    # 1) Consolidate depth and units first
    .data <- .data |>
      dplyr::mutate(
        # set equal to TADA.ResultDepthHeighMeasure.MeasureValue if available, otherwise use TADA.ActivityDepthHeightMeasure.MeasureValue
        TADA.ConsolidatedDepth = ifelse(
          !is.na(TADA.ResultDepthHeightMeasure.MeasureValue),
          TADA.ResultDepthHeightMeasure.MeasureValue,
          TADA.ActivityDepthHeightMeasure.MeasureValue
        ),
        TADA.ConsolidatedDepth.Unit = ifelse(
          !is.na(TADA.ResultDepthHeightMeasure.MeasureUnitCode),
          TADA.ResultDepthHeightMeasure.MeasureUnitCode,
          TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ),
        # Override with ResultMeasureValue for depth-parameter characteristics
        TADA.ConsolidatedDepth = ifelse(
          TADA.CharacteristicName %in% depth.params,
          TADA.ResultMeasureValue,
          TADA.ConsolidatedDepth
        ),
        TADA.ConsolidatedDepth.Unit = ifelse(
          TADA.CharacteristicName %in% depth.params,
          TADA.ResultMeasure.MeasureUnitCode,
          TADA.ConsolidatedDepth.Unit
        ),
        TADA.ConsolidatedDepth.Unit = tolower(TADA.ConsolidatedDepth.Unit)
      )

    # 2) Validate there is only one depth unit in use (assumes conversion already done)
    units_present <- .data |>
      dplyr::filter(!is.na(TADA.ConsolidatedDepth.Unit)) |>
      dplyr::pull(TADA.ConsolidatedDepth.Unit) |>
      unique()

    if (length(units_present) > 1) {
      stop(
        "TADA_FlagDepthCategory: Multiple depth units detected. Convert depth units to a single unit before categorizing."
      )
    }

    # 3) Proceed to compute bottom depth and assign categories (NA-aware)
    # use group_by to identify profile data
    .data <- .data |>
      dplyr::group_by(
        ActivityStartDate,
        TADA.MonitoringLocationIdentifier,
        OrganizationIdentifier
      ) |>
      # determine the number of Depths per group
      dplyr::mutate(
        DepthsPerGroup = dplyr::n_distinct(
          TADA.ConsolidatedDepth,
          na.rm = TRUE
        ),
        # determine bottom value using TADA.ActivityBottomDepthHeightMeasure.MeasureValue or the max depth record for profile data
        has_depths = any(!is.na(TADA.ConsolidatedDepth)),
        TADA.ConsolidatedDepth.Bottom = dplyr::case_when(
          DepthsPerGroup > 1 &
            is.na(TADA.ActivityBottomDepthHeightMeasure.MeasureValue) &
            has_depths ~ max(TADA.ConsolidatedDepth, na.rm = TRUE),
          DepthsPerGroup > 1 &
            is.na(TADA.ActivityBottomDepthHeightMeasure.MeasureValue) &
            !has_depths ~ NA_real_,
          TRUE ~ TADA.ActivityBottomDepthHeightMeasure.MeasureValue
        )
      ) |>
      dplyr::select(-has_depths) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        # Only assign depth categories when the needed thresholds are available
        TADA.DepthCategory.Flag = dplyr::case_when(
          # Surface only if surfacevalue is provided
          !is.na(surfacevalue) &
            !is.na(TADA.ConsolidatedDepth) &
            TADA.ConsolidatedDepth <= surfacevalue ~ "Surface",

          # Bottom only if bottomvalue and bottom depth are available
          !is.na(bottomvalue) &
            !is.na(TADA.ConsolidatedDepth.Bottom) &
            !is.na(TADA.ConsolidatedDepth) &
            TADA.ConsolidatedDepth >=
              (TADA.ConsolidatedDepth.Bottom - bottomvalue) &
            TADA.ConsolidatedDepth <= TADA.ConsolidatedDepth.Bottom ~ "Bottom",

          # Middle only if both surfacevalue and bottomvalue are provided (and bottom available)
          !is.na(surfacevalue) &
            !is.na(bottomvalue) &
            !is.na(TADA.ConsolidatedDepth.Bottom) &
            !is.na(TADA.ConsolidatedDepth) &
            TADA.ConsolidatedDepth > surfacevalue &
            TADA.ConsolidatedDepth <
              (TADA.ConsolidatedDepth.Bottom - bottomvalue) ~ "Middle",

          TRUE ~ NA_character_
        )
      ) |>
      # Join ARD reference as fallback
      dplyr::left_join(ard.ref, by = "ActivityRelativeDepthName") |>
      dplyr::mutate(
        TADA.DepthCategory.Flag = ifelse(
          is.na(TADA.DepthCategory.Flag),
          ARD_Category,
          TADA.DepthCategory.Flag
        ),
        TADA.DepthCategory.Flag = ifelse(
          is.na(TADA.ActivityDepthHeightMeasure.MeasureValue) &
            is.na(TADA.ConsolidatedDepth.Bottom) &
            is.na(TADA.ResultDepthHeightMeasure.MeasureValue) &
            is.na(TADA.DepthCategory.Flag),
          "No depth info",
          TADA.DepthCategory.Flag
        ),
        TADA.DepthCategory.Flag = ifelse(
          is.na(TADA.DepthCategory.Flag),
          "Not enough depth info to determine category",
          TADA.DepthCategory.Flag
        )
      ) |>
      dplyr::select(-ARD_Category, -DepthsPerGroup)
  }

  if (depth.count == 0) {
    message(
      "TADA_FlagDepthCategory: No depth information was found in the dataset. The columns TADA.DepthCategory.Flag and TADA.ConsolidatedDepth are being added and populated with NA values."
    )

    .data <- .data |>
      dplyr::mutate(
        TADA.DepthCategory.Flag = NA_character_,
        TADA.ConsolidatedDepth = as.numeric(NA),
        TADA.ConsolidatedDepth.Unit = NA_character_,
        TADA.ConsolidatedDepth.Bottom = as.numeric(NA)
      ) |>
      TADA_OrderCols()

    return(.data)
  }

  if (clean == TRUE) {
    .data <- .data |> dplyr::filter(TADA.DepthCategory.Flag %in% depthcat.list)
  }

  if (clean == FALSE) {
    .data <- .data
  }

  if (bycategory == "all") {
    message(
      "TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, ActivityStartDate, and TADA.DepthCategory.Flag for aggregation by TADA.DepthCategory.Flag."
    )

    group.list <- c(
      "TADA.MonitoringLocationIdentifier",
      "OrganizationIdentifier",
      "TADA.CharacteristicName",
      "ActivityStartDate",
      "TADA.DepthCategory.Flag"
    )

    .data <- .data
  } else {
    # unify grouping branches
    if (bycategory == "no") {
      message(
        "TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for entire water column."
      )
      .data <- .data
    }
    if (bycategory == "surface") {
      message(
        "TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for surface samples only."
      )
      .data <- .data |> dplyr::filter(TADA.DepthCategory.Flag == "Surface")
    }
    if (bycategory == "middle") {
      message(
        "TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for middle samples only."
      )
      .data <- .data |> dplyr::filter(TADA.DepthCategory.Flag == "Middle")
    }
    if (bycategory == "bottom") {
      message(
        "TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for bottom samples only."
      )
      .data <- .data |> dplyr::filter(TADA.DepthCategory.Flag == "Bottom")
    }
    group.list <- c(
      "TADA.MonitoringLocationIdentifier",
      "OrganizationIdentifier",
      "TADA.CharacteristicName",
      "ActivityStartDate"
    )
  }

  if (dailyagg == "none") {
    message("TADA_FlagDepthCategory: No aggregation performed.")

    # add TADA.ResultValue.Aggregation.Flag, remove unecessary columns, and order columns
    orig.data <- .data |>
      dplyr::group_by_at(group.list) |>
      dplyr::mutate(
        DepthsByGroup = dplyr::n_distinct(TADA.ConsolidatedDepth, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(
          DepthsByGroup > 1,
          "No aggregation performed",
          "No aggregation needed"
        )
      ) |>
      dplyr::select(-DepthsByGroup) |>
      dplyr::ungroup() |>
      TADA_OrderCols()

    if (aggregatedonly == TRUE) {
      stop(
        "aggregatedonly = TRUE requires dailyagg = 'avg', 'min' or 'max'; nothing to return when dailyagg = 'none'."
      )
    }

    if (aggregatedonly == FALSE) {
      return(orig.data)
    }
  }
  if ((dailyagg == "avg")) {
    message(
      "TADA_FlagDepthCategory: Calculating mean aggregate value with deterministically selected metadata."
    )

    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data |>
      dplyr::group_by_at(group.list) |>
      dplyr::mutate(
        DepthsByGroup = dplyr::n_distinct(TADA.ConsolidatedDepth, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(
          DepthsByGroup > 1,
          paste(
            "Considered in averaging results ",
            cattype,
            " but not selected as aggregate value"
          ),
          "No aggregation needed"
        ),
        TADA.DepthProfileAggregation.Flag = ifelse(
          !TADA.DepthCategory.Flag %in% depthcat.list,
          "No aggregation needed",
          TADA.DepthProfileAggregation.Flag
        )
      )

    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, calculate mean result value per group, and assign deterministic metadata from group.
    agg.data <- orig.data |>
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) |>
      dplyr::mutate(
        TADA.ResultMeasureValue1 = mean(TADA.ResultMeasureValue, na.rm = TRUE)
      ) |>
      # choose a deterministic representative row for reproducibility
      dplyr::arrange(ResultIdentifier) |>
      dplyr::slice(1) |>
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = paste0(
          "Calculated mean aggregate value ",
          cattype,
          ", with deterministically selected metadata from a row in the aggregate group"
        )
      ) |>
      dplyr::select(-TADA.ResultMeasureValue, -DepthsByGroup) |>
      dplyr::rename(TADA.ResultMeasureValue = TADA.ResultMeasureValue1) |>
      dplyr::mutate(ResultIdentifier = paste0("TADA-", ResultIdentifier)) |>
      dplyr::ungroup()

    if (aggregatedonly == TRUE) {
      rm(orig.data)

      return(agg.data)
    }

    if (aggregatedonly == FALSE) {
      # combine original and aggregate data
      comb.data <- dplyr::bind_rows(orig.data, agg.data) |>
        dplyr::ungroup() |>
        dplyr::select(-DepthsByGroup) |>
        TADA_OrderCols()

      rm(agg.data, orig.data)

      return(comb.data)
    }
  }
  if ((dailyagg == "min")) {
    message("TADA_FlagDepthCategory: Selecting minimum aggregate value.")

    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data |>
      dplyr::group_by_at(group.list) |>
      dplyr::mutate(
        DepthsByGroup = dplyr::n_distinct(TADA.ConsolidatedDepth, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(
          DepthsByGroup > 1,
          paste(
            "Considered in minimum aggregation ",
            cattype,
            "but not selected"
          ),
          "No aggregation needed"
        ),
        TADA.DepthProfileAggregation.Flag = ifelse(
          !TADA.DepthCategory.Flag %in% depthcat.list,
          "No aggregation needed",
          TADA.DepthProfileAggregation.Flag
        )
      )

    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, and select minimum result value per group.
    agg.data <- orig.data |>
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) |>
      dplyr::slice_min(
        order_by = TADA.ResultMeasureValue,
        n = 1,
        with_ties = FALSE
      ) |>
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = paste0(
          "Selected as min aggregate value ",
          cattype
        )
      ) |>
      dplyr::select(-DepthsByGroup) |>
      dplyr::ungroup()

    if (aggregatedonly == TRUE) {
      rm(orig.data)

      return(agg.data)
    }

    if (aggregatedonly == FALSE) {
      # create list of result identifiers for selected aggregate data
      agg.list <- agg.data |>
        dplyr::ungroup() |>
        dplyr::select(ResultIdentifier) |>
        unique() |>
        dplyr::pull()

      # combine original and aggregate data
      comb.data <- orig.data |>
        dplyr::filter(!ResultIdentifier %in% agg.list) |>
        dplyr::bind_rows(agg.data) |>
        dplyr::ungroup() |>
        dplyr::select(-DepthsByGroup) |>
        TADA_OrderCols()

      rm(agg.data, orig.data, agg.list)

      return(comb.data)
    }
  }

  if ((dailyagg == "max")) {
    message("TADA_FlagDepthCategory: Selecting maximum aggregate value.")

    # Flag all rows (in groups with >1 depth) as considered/not selected by default
    orig.data <- .data |>
      dplyr::group_by_at(group.list) |>
      dplyr::mutate(
        DepthsByGroup = dplyr::n_distinct(TADA.ConsolidatedDepth, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(
          DepthsByGroup > 1,
          paste(
            "Considered in maximum aggregation ",
            cattype,
            "but not selected"
          ),
          "No aggregation needed"
        ),
        # If a row is outside depth categories, mark as "No aggregation needed"
        TADA.DepthProfileAggregation.Flag = ifelse(
          !TADA.DepthCategory.Flag %in% depthcat.list,
          "No aggregation needed",
          TADA.DepthProfileAggregation.Flag
        )
      )

    # Select the maximum result per group (only rows in depth categories)
    agg.data <- orig.data |>
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) |>
      dplyr::slice_max(
        order_by = TADA.ResultMeasureValue,
        n = 1,
        with_ties = FALSE
      ) |>
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = paste0(
          "Selected as max aggregate value ",
          cattype
        )
      ) |>
      dplyr::select(-DepthsByGroup) |>
      dplyr::ungroup()

    if (aggregatedonly == TRUE) {
      rm(orig.data)
      return(agg.data)
    }

    if (aggregatedonly == FALSE) {
      # Remove the selected rows from the original so they are not duplicated,
      # then add them back with the "selected" flag applied above
      agg.list <- agg.data |>
        dplyr::ungroup() |>
        dplyr::select(ResultIdentifier) |>
        unique() |>
        dplyr::pull()

      comb.data <- orig.data |>
        dplyr::filter(!ResultIdentifier %in% agg.list) |>
        dplyr::bind_rows(agg.data) |>
        dplyr::ungroup() |>
        dplyr::select(-DepthsByGroup) |>
        TADA_OrderCols()

      rm(agg.data, orig.data, agg.list)
      return(comb.data)
    }
  }
}

#' TADA_IDDepthProfiles
#'
#' This function identifies depth profiles within a data frame to assist the user in
#' selecting params for TADA_DepthProfilePlot. A TADA compatible data set is required.
#' If TADA_FlagDepthCategory has not yet been run, it will be run as part of this
#' function. The output data frame is grouped by TADA.MonitoringLocationIdentifier,
#' OrganizationIdentifier, and ActivityStartDate.
#'
#' A new column, TADA.CharacteristicsForDepthProfile, is created which lists the
#' characteristics available for depth profile analysis. Using the, nresults param,
#' users can specify whether characteristic names should be followed by the number
#' of results available for the characteristic in parentheses.
#'
#' @param .data TADA dataframe which must include the columns ActivityStartDate,
#' TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Unit, TADA.ConsolidatedDepth.Bottom,
#' TADA.ResultMeasureValue, TADA.ResultMeasure.MeasureUnitCode,
#' OrganizationIdentifier, TADA.MonitoringLocationName, TADA.MonitoringLocationIdentifier,
#' and TADA.ComparableDataIdentifier.
#'
#' @param nresults Boolean argument with options "TRUE" or "FALSE". The
#' default is nresults = TRUE, which means that the number of results for each
#' characteristic are added within the TADA.CharacteristicsForDepthProfile column.
#' When nresults = FALSE, the number of results is not appended to
#' TADA.CharacteristicsForDepthProfile.
#'
#' @param nvalue numeric argument to specify the number of results required to identify
#' a depth profile. The default is 2, which means that a depth profile will be identified
#' if 2 or more results at different depths exists for the same ActivityStartDate,
#' TADA.MonitoringLocationIdentifier, OrganizationIdentifier, and TADA.ComparableDataIdentifier.
#' A few characteristics are excluded from this requirement because they are expected to
#' have only a single result in depth units (ex: secchi disk depth).
#'
#' @param aggregates Boolean argument with options "TRUE" or "FALSE". The default is
#' aggregates = FALSE, which means that any aggregate values created (means) in
#' TADA_FlagDepthCategory are excluded from identifying depth profile data. Aggregate
#' values that were selected from the existing data set (max and min) remain.
#' Only columns created/add by TADA_FlagDepthCategory are removed when aggregates =
#' FALSE. When aggregates = TRUE, all aggregate values are included when identifying
#' depth profile data.
#'
#' @return A dataframe with the columns TADA.MonitoringLocationIdentifier,
#' TADA.MonitoringLocationName, OrganizationIdentifier, ActivityStartDate,
#' TADA.CharacteristicsForDepthProfile. Based on the user input for the nresults
#' param, TADA.CharacteristicsForDepthProfile may or may not contain the number
#' of results for each characteristic.
#'
#' @details
#' Inputs nresults and aggregates must be logical scalars; non-logical values will
#' raise an error. nvalue must be a single numeric value.
#'
#' @export
#'
#' @examples
#' # Load data frame
#' utils::data(Data_6Tribes_5y)
#'
#' # find depth profile data without showing number of results
#' Data_6Tribes_5y_DepthProfileID_Nresults <-
#'   TADA_IDDepthProfiles(Data_6Tribes_5y, nresults = FALSE)
#'
#' # find depth profile data showing number of results
#' Data_6Tribes_5y_DepthProfileID <- TADA_IDDepthProfiles(Data_6Tribes_5y)
#'
TADA_IDDepthProfiles <- function(
  .data,
  nresults = TRUE,
  nvalue = 2,
  aggregates = FALSE
) {
  # input type validation
  TADA_CheckType(.data, "data.frame", "Input object")
  TADA_CheckType(nresults, "logical", "nresults")
  TADA_CheckType(aggregates, "logical", "aggregates")
  if (!is.numeric(nvalue) || length(nvalue) != 1) {
    stop("TADA_IDDepthProfiles: nvalue must be a single numeric value.")
  }

  # check for columns created in TADA_FlagDepthCategory and run the function if they are missing
  # add check that depth category flag function has been run, run it if it has not
  .data <- .ensure_depth_flag_columns(.data)

  depth.params <- .depth_param_names()

  # when aggregates == FALSE, robust removal of mean-aggregated rows (created by avg)
  if (!aggregates) {
    .data <- .drop_avg_aggregates(.data)
  }

  if (nresults == TRUE) {
    .data <- .data |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier,
        TADA.MonitoringLocationName,
        TADA.MonitoringLocationTypeName,
        OrganizationIdentifier,
        ActivityStartDate,
        TADA.CharacteristicName,
        TADA.ComparableDataIdentifier,
        TADA.ConsolidatedDepth,
        TADA.ConsolidatedDepth.Unit,
        TADA.ConsolidatedDepth.Bottom
      ) |>
      dplyr::group_by(
        TADA.MonitoringLocationIdentifier,
        OrganizationIdentifier,
        ActivityStartDate,
        TADA.ComparableDataIdentifier
      ) |>
      dplyr::mutate(
        TADA.NResults = dplyr::n_distinct(TADA.ConsolidatedDepth, na.rm = TRUE),
        has_depth_param = any(
          TADA.CharacteristicName %in% depth.params,
          na.rm = TRUE
        ),
        TADA.CharacteristicsForDepthProfile = paste0(
          TADA.ComparableDataIdentifier,
          " (",
          TADA.NResults,
          ")"
        )
      ) |>
      dplyr::filter(TADA.NResults >= nvalue | has_depth_param) |>
      dplyr::ungroup() |>
      dplyr::group_by(
        TADA.MonitoringLocationIdentifier,
        OrganizationIdentifier,
        ActivityStartDate
      ) |>
      # check that for results with only a single depth unit (ex: secchi disk depth) that other results are available in group
      dplyr::mutate(MeanResults = mean(TADA.NResults)) |>
      dplyr::filter(MeanResults > 1) |>
      dplyr::mutate(
        TADA.CharacteristicsForDepthProfile = paste(
          unique(TADA.CharacteristicsForDepthProfile),
          ";",
          collapse = ""
        ),
        TADA.CharacteristicsForDepthProfile = stringr::str_replace_all(
          paste(
            sort(unique(unlist(strsplit(
              TADA.CharacteristicsForDepthProfile,
              ";",
            )))),
            collapse = ";"
          ),
          " ;",
          "; "
        )
      ) |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier,
        TADA.MonitoringLocationName,
        TADA.MonitoringLocationTypeName,
        OrganizationIdentifier,
        ActivityStartDate,
        TADA.CharacteristicsForDepthProfile
      ) |>
      unique()

    return(.data)
  }

  if (identical(nresults, FALSE)) {
    .data <- .data |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier,
        TADA.MonitoringLocationName,
        TADA.MonitoringLocationTypeName,
        OrganizationIdentifier,
        ActivityStartDate,
        TADA.CharacteristicName,
        TADA.ComparableDataIdentifier,
        TADA.ConsolidatedDepth,
        TADA.ConsolidatedDepth.Unit,
        TADA.ConsolidatedDepth.Bottom
      ) |>
      dplyr::group_by(
        TADA.MonitoringLocationIdentifier,
        OrganizationIdentifier,
        ActivityStartDate,
        TADA.ComparableDataIdentifier
      ) |>
      dplyr::mutate(
        TADA.NResults = dplyr::n_distinct(TADA.ConsolidatedDepth, na.rm = TRUE)
      ) |>
      dplyr::filter(
        TADA.NResults >= nvalue | TADA.CharacteristicName %in% depth.params
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(
        TADA.MonitoringLocationIdentifier,
        OrganizationIdentifier,
        ActivityStartDate
      ) |>
      # check that for results with only a single depth unit (ex: secchi disk depth) that other results are available in group
      dplyr::mutate(MeanResults = mean(TADA.NResults)) |>
      dplyr::filter(MeanResults > 1) |>
      dplyr::mutate(
        TADA.CharacteristicsForDepthProfile = paste(
          unique(TADA.ComparableDataIdentifier),
          ";",
          collapse = ""
        ),
        TADA.CharacteristicsForDepthProfile = stringr::str_replace_all(
          paste(
            sort(unique(unlist(strsplit(
              TADA.CharacteristicsForDepthProfile,
              ";",
            )))),
            collapse = ";"
          ),
          " ;",
          "; "
        )
      ) |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier,
        TADA.MonitoringLocationName,
        TADA.MonitoringLocationTypeName,
        OrganizationIdentifier,
        ActivityStartDate,
        TADA.CharacteristicsForDepthProfile
      ) |>
      unique()

    return(.data)
  }

  # ensure function doesn’t fall through silently
  stop("TADA_IDDepthProfiles: nresults must be TRUE or FALSE.")
}

#' Create A Three-Characteristic Depth Profile
#'
#' @param .data TADA data frame containing the data downloaded from the WQP,
#'   where each row represents a unique data record. TADA_FlagDepthCategory
#'   has been run as data frame must include the columns TADA.DepthCategory.Flag,
#'   TADA.ResultDepthHeightMeasure.MeasureUnitCode, TADA.ActivityDepthHeightMeasure.MeasureUnitCode,
#'   and TADA.ActivityDepthHeightMeasure.MeasureValue. Units for all depth fields
#'   must be the same. This can be accomplished using TADA_AutoClean() or
#'   TADA_ConvertDepthUnits.
#'
#' @param groups A vector of up to three identifiers from the TADA.ComparableDataIdentifier column.
#'   For example, the groups could be 'DISSOLVED OXYGEN (DO)_NA_NA_UG/L' and 'PH_NA_NA_NA'.
#'   These groups will be specific to your data frame. The TADA_IDDepthProfiles can be
#'   used to identify available groups. If more than three identifiers are supplied,
#'   only the first three are used and a warning is issued.
#'
#' @param location A single TADA.MonitoringLocationIdentifier to plot the depth profile.
#'   A TADA.MonitoringLocationIdentifier must be entered or an error will be returned and
#'   no depth profile will be created.
#'
#' @param activity_date The date the depth profile results were collected.
#'
#' @param depthcat Boolean argument indicating whether delineation between depth
#'   categories should be shown on the depth profile figure. depthcat = TRUE is the
#'   default and displays solid black lines to delineate between surface, middle, and
#'   bottom samples and labels each section of the plot.
#'   When depthcat = TRUE, at least one of surfacevalue or bottomvalue must be provided
#'   (non-NA), otherwise the function will stop. If bottom depth cannot be determined for
#'   the selection, “Bottom” and “Middle” delineations are omitted.
#'
#' @param bottomvalue numeric argument. The user enters how many meters from the
#'   bottom should be included in the "Bottom" category. Default is
#'   bottomvalue = 2.
#'
#' @param surfacevalue numeric argument. The user enters how many meters from the
#'   surface should be included in the "Surface" category. Default is surfacevalue = 2.
#'
#' @param unit Character argument. The user enters either "m" or "ft" to specify which
#'   depth units should be used for the plot. Default is "m".
#'   Non-depth-parameter rows must already be in the specified unit. Depth-parameter
#'   rows (e.g., Secchi) are converted to the specified unit for plotting when necessary.
#'
#' @return A depth profile plot displaying up to three parameters for a single
#'   TADA.MonitoringLocationIdentifier. Displaying depth categories is optional with the
#'   depthcat argument. The function excludes duplicate depth-parameter rows from the
#'   main profile series and, if any are included via groups, plots them as single
#'   horizontal reference lines in the requested unit.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example dataframe:
#' utils::data(Data_6Tribes_5y_Harmonized)
#' # Create a depth profile figure with three parameters for a single
#' # monitoring location and date
#' TADA_DepthProfilePlot(Data_6Tribes_5y_Harmonized,
#' groups = c(
#'  "TEMPERATURE_NONE_NONE_DEG C", "PH_NONE_NONE_NONE",
#'  "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"
#' ),
#' location = "REDLAKE_WQX-ANKE",
#' activity_date = "2018-10-04"
#' )
#'
#' # Load example data frame:
#' utils::data(Data_6Tribes_5y_Harmonized)
#' # Create a depth profile figure with two parameters for a single monitoring
#' # location and date without displaying depth categories
#' TADA_DepthProfilePlot(Data_6Tribes_5y_Harmonized,
#' groups = c("PH_NONE_NONE_NONE", "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"),
#' location = "REDLAKE_WQX-JOHN",
#' activity_date = "2018-07-31",
#' depthcat = FALSE
#' )
#' }
#'
TADA_DepthProfilePlot <- function(
  .data,
  groups = NULL,
  location = NULL,
  activity_date = NULL,
  depthcat = TRUE,
  surfacevalue = 2,
  bottomvalue = 2,
  unit = "m"
) {
  # check to see if TADA.ComparableDataIdentifier column is present
  if (!"TADA.ComparableDataIdentifier" %in% colnames(.data)) {
    stop(
      "TADA.ComparableDataIdentifier column not present in data set. Run TADA_CreateComparableID to create TADA.ComparableDataIdentifier."
    )
  }

  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # validate unit and groups length
  if (!unit %in% c("m", "ft")) {
    stop("TADA_DepthProfilePlot: unit must be 'm' or 'ft'.")
  }
  if (length(groups) > 3) {
    warning(
      "TADA_DepthProfilePlot: More than 3 groups supplied; only the first 3 will be used."
    )
    groups <- groups[1:3]
  }

  # Normalize "null" to NA
  surfacevalue <- .normalize_null_numeric(surfacevalue)
  bottomvalue <- .normalize_null_numeric(bottomvalue)

  # Add check that depth category flag function has been run, run it if it has not
  .data <- .ensure_depth_flag_columns(
    .data,
    surfacevalue = surfacevalue,
    bottomvalue = bottomvalue,
    allow_na_thresholds = TRUE
  )

  # Define depth-parameter characteristics (needed before unit checks)
  depth.params <- .depth_param_names()

  # Enforce unit consistency only across non-depth-parameter rows; depth-parameter rows will be converted later
  .data <- .data |> dplyr::filter(!is.na(TADA.ConsolidatedDepth))

  non_depth_rows <- .data |>
    dplyr::filter(!TADA.CharacteristicName %in% depth.params)
  if (nrow(non_depth_rows) > 0) {
    units_present <- unique(stats::na.omit(
      non_depth_rows$TADA.ConsolidatedDepth.Unit
    ))
    if (length(units_present) > 1 || units_present != unit) {
      stop(
        "TADA_DepthProfilePlot: Convert non-depth-parameter depth units to match `unit` before plotting."
      )
    } else {
      message(
        "TADA_DepthProfilePlot: Depth unit for non-depth-parameter rows matches `unit`."
      )
    }
  } else {
    message(
      "TADA_DepthProfilePlot: Only depth-parameter rows detected; unit check skipped (conversion will be applied as needed)."
    )
  }

  # create ID Depth Profiles data.frame to check against params
  param.check <- TADA_IDDepthProfiles(.data)

  # Early required-argument checks
  if (is.null(location) || is.null(activity_date) || is.null(groups)) {
    stop(
      "TADA_DepthProfilePlot: Please supply 'location', 'activity_date', and 'groups'."
    )
  }

  # Validate they exist in the data
  if (!location %in% .data$TADA.MonitoringLocationIdentifier) {
    stop("TADA_DepthProfilePlot: `location` is not present in the data.")
  }
  if (!activity_date %in% .data$ActivityStartDate) {
    stop("TADA_DepthProfilePlot: `activity_date` is not present in the data.")
  }
  missing_groups <- setdiff(groups, unique(.data$TADA.ComparableDataIdentifier))
  if (length(missing_groups) > 0) {
    stop(paste0(
      "TADA_DepthProfilePlot: The following `groups` are not present in the data: ",
      paste(missing_groups, collapse = ", ")
    ))
  }

  # remove param.check
  rm(param.check)

  # Ensure optional datetime column exists for hover text
  if (!"ActivityStartDateTime" %in% names(.data)) {
    .data$ActivityStartDateTime <- NA_character_
  }

  # list required columns (include fields used in hover/name text)
  required_cols <- c(
    "TADA.ResultDepthHeightMeasure.MeasureValue",
    "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
    "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
    "TADA.ActivityDepthHeightMeasure.MeasureValue",
    "TADA.DepthCategory.Flag",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.MonitoringLocationIdentifier",
    "TADA.MonitoringLocationName",
    "ActivityStartDate",
    "ActivityStartDateTime",
    "TADA.ConsolidatedDepth",
    "TADA.ConsolidatedDepth.Unit",
    "TADA.ConsolidatedDepth.Bottom",
    "TADA.ActivityMediaName",
    "ActivityMediaSubdivisionName",
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "ActivityRelativeDepthName",
    "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText"
  )

  # check .data has required columns
  TADA_CheckColumns(.data, required_cols)

  message("TADA_DepthProfilePlot: Identifying available depth profile data.")

  # exclude depth-parameter rows from depthprofile.avail to avoid duplication
  depthprofile.avail <- .data |>
    dplyr::filter(
      !is.na(TADA.ConsolidatedDepth),
      TADA.MonitoringLocationIdentifier %in% location,
      ActivityStartDate %in% activity_date,
      TADA.ActivityMediaName == "WATER",
      !TADA.CharacteristicName %in% depth.params
    ) |>
    dplyr::group_by(
      TADA.ComparableDataIdentifier,
      ActivityStartDate,
      TADA.ConsolidatedDepth
    ) |>
    dplyr::slice_sample(n = 1) |>
    dplyr::ungroup() |>
    dplyr::group_by(
      TADA.MonitoringLocationIdentifier,
      TADA.ComparableDataIdentifier,
      ActivityStartDate
    ) |>
    dplyr::mutate(N = length(TADA.ResultMeasureValue)) |>
    dplyr::filter(N > 2) |>
    dplyr::ungroup() |>
    dplyr::select(-N)

  depth.params.groups <- .data |>
    dplyr::filter(
      TADA.MonitoringLocationIdentifier %in% location,
      ActivityStartDate %in% activity_date,
      TADA.ActivityMediaName == "WATER",
      TADA.CharacteristicName %in% depth.params
    ) |>
    dplyr::select(TADA.ComparableDataIdentifier) |>
    unique() |>
    dplyr::pull()

  # Use user-specified depth unit for the figure
  fig.depth.unit <- unit

  # if any depth parameter (ex: secchi) data
  if (length(intersect(groups, depth.params.groups)) > 0) {
    # add depth param (ex: secchi) results
    depth.params.string <- paste(depth.params, collapse = "; ")

    depth.params.avail <- .data |>
      dplyr::filter(
        TADA.MonitoringLocationIdentifier %in% location,
        TADA.CharacteristicName %in% depth.params,
        ActivityStartDate %in% activity_date,
        TADA.ActivityMediaName == "WATER",
        TADA.ComparableDataIdentifier %in% groups
      ) |>
      dplyr::group_by(
        TADA.CharacteristicName,
        ActivityStartDate,
        TADA.MonitoringLocationIdentifier
      ) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::ungroup()

    units_match <- all(
      stats::na.omit(depth.params.avail$TADA.ConsolidatedDepth.Unit) ==
        fig.depth.unit
    )
    if (units_match) {
      message(paste(
        "TADA_DepthProfilePlot: Any results for",
        depth.params.string,
        "match the depth unit selected for the figure."
      ))
    } else {
      message(paste(
        "TADA_DepthProfilePlot: Converting depth units for any results for",
        depth.params.string,
        "results to match depth units selected for the figure."
      ))

      # consolidated conversion map for depth-parameter rows
      conv_df <- data.frame(
        TADA.ConsolidatedDepth.Unit = c(
          "m",
          "ft",
          "in",
          "ft",
          "in",
          "m",
          "in",
          "m",
          "ft",
          "cm",
          "cm",
          "cm"
        ),
        YAxis.DepthUnit = c(
          "m",
          "m",
          "m",
          "ft",
          "ft",
          "ft",
          "ft",
          "in",
          "in",
          "m",
          "ft",
          "in"
        ),
        SecchiConversion = c(
          "1",
          "0.3048",
          "0.0254",
          "3.281",
          "0.083",
          "39.3701",
          "12",
          "0.01",
          "0.032808",
          "0.39",
          "0.39",
          "0.39"
        ),
        stringsAsFactors = FALSE
      )

      depth.params.avail <- depth.params.avail |>
        dplyr::mutate(YAxis.DepthUnit = fig.depth.unit) |>
        dplyr::left_join(
          conv_df,
          by = c("TADA.ConsolidatedDepth.Unit", "YAxis.DepthUnit")
        ) |>
        dplyr::mutate(
          TADA.ConsolidatedDepth.Unit = fig.depth.unit,
          TADA.ConsolidatedDepth = TADA.ResultMeasureValue *
            as.numeric(SecchiConversion)
        ) |>
        dplyr::select(-YAxis.DepthUnit, -SecchiConversion)
    }

    profile.data <- dplyr::bind_rows(depthprofile.avail, depth.params.avail)
    rm(depth.params.avail, depthprofile.avail)
  } else {
    # no depth-parameter groups requested; use the main profile data only
    profile.data <- depthprofile.avail
  }

  # this subset must include all fields included in plot hover below
  plot.data <- profile.data |>
    dplyr::filter(TADA.ComparableDataIdentifier %in% groups) |>
    dplyr::select(
      dplyr::all_of(required_cols),
      "TADA.ComparableDataIdentifier"
    ) |>
    dplyr::mutate(
      TADA.ResultMeasure.MeasureUnitCode = ifelse(
        is.na(TADA.ResultMeasure.MeasureUnitCode),
        "NA",
        TADA.ResultMeasure.MeasureUnitCode
      )
    )

  # Ensure there is data to plot for the selected location/date/groups
  if (nrow(plot.data) == 0) {
    stop(
      "TADA_DepthProfilePlot: No data found for the selected location, activity_date, and groups."
    )
  }

  rm(profile.data)

  # break into subsets for each parameter
  param1 <- plot.data |>
    dplyr::filter(TADA.ComparableDataIdentifier %in% groups[1])
  param2 <- plot.data |>
    dplyr::filter(TADA.ComparableDataIdentifier %in% groups[2])
  param3 <- plot.data |>
    dplyr::filter(TADA.ComparableDataIdentifier %in% groups[3])

  # Ensure each requested group has data for this location/date
  present_groups <- plot.data |>
    dplyr::count(TADA.ComparableDataIdentifier) |>
    dplyr::pull(TADA.ComparableDataIdentifier)
  missing_in_subset <- setdiff(groups, present_groups)
  if (length(missing_in_subset) > 0) {
    stop(paste0(
      "TADA_DepthProfilePlot: The following `groups` have no data for the selected location and activity_date: ",
      paste(missing_in_subset, collapse = ", ")
    ))
  }

  # create title for figure, conditional on number of groups/characteristics selected

  # title for three characteristics
  if (length(groups) == 3) {
    title <- stringr::str_wrap(
      paste0(
        param1$TADA.CharacteristicName[1],
        "; ",
        param2$TADA.CharacteristicName[1],
        " and ",
        param3$TADA.CharacteristicName[1],
        " for ",
        plot.data$TADA.MonitoringLocationName[1],
        " on ",
        format(as.Date(plot.data$ActivityStartDate[1]), "%B %d, %Y")
      ),
      width = 50
    )
  }

  # title for two characteristics
  if (length(groups) == 2) {
    title <- stringr::str_wrap(
      paste0(
        param1$TADA.CharacteristicName[1],
        " and ",
        param2$TADA.CharacteristicName[1],
        " for ",
        # figure out addition of weird \n in name
        plot.data$TADA.MonitoringLocationName[1],
        " on ",
        format(as.Date(plot.data$ActivityStartDate[1]), "%B %d, %Y")
      ),
      width = 50
    )
  }

  # title for one characteristic
  if (length(groups) == 1) {
    title <- stringr::str_wrap(
      paste0(
        param1$TADA.CharacteristicName[1],
        " for ",
        # figure out addition of weird \n in name
        plot.data$TADA.MonitoringLocationName[1],
        " on ",
        format(as.Date(plot.data$ActivityStartDate[1]), "%B %d, %Y")
      ),
      width = 50
    )
  }

  # figure margin
  mrg <- list(
    l = 50,
    r = 50,
    b = 100,
    t = (25 + (ceiling(nchar(title) / 50)) * 25), # top margin is variable based on number of lines in title
    pad = 0
  )

  # determine x + y max and range for plotting
  xmax <- max(plot.data$TADA.ResultMeasureValue, na.rm = TRUE) +
    0.5 * max(plot.data$TADA.ResultMeasureValue, na.rm = TRUE)
  xrange <- c(0, xmax)

  ymax <- max(plot.data$TADA.ConsolidatedDepth, na.rm = TRUE) +
    0.1 * max(plot.data$TADA.ConsolidatedDepth, na.rm = TRUE)
  yrange <- c(0, ymax)

  # set palette
  tada.pal <- TADA_ColorPalette()

  # create base of scatter plot
  scatterplot <- plotly::plot_ly(type = "scatter", mode = "lines+markers") |>
    plotly::layout(
      xaxis = list(
        # title = title.x,
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r",
        linecolor = "black",
        rangemode = "tozero",
        showgrid = FALSE,
        tickcolor = "black"
      ),
      yaxis = list(
        title = paste0(
          "Depth",
          " (",
          param1$TADA.ConsolidatedDepth.Unit[1],
          ")"
        ),
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r",
        linecolor = "black",
        rangemode = "tozero",
        showgrid = FALSE,
        tickcolor = "black",
        autorange = "reversed"
      ),
      hoverlabel = list(bgcolor = "white"),
      title = list(text = title, xref = "paper", x = 0.5),
      plot_bgcolor = "#e5ecf6",
      margin = mrg,
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center",
        yanchor = "top"
      )
    )

  # first parameter has a depth profile
  if (
    length(groups) >= 1 &&
      nrow(param1) > 0 &&
      !param1$TADA.CharacteristicName[1] %in% depth.params
  ) {
    # config options https://plotly.com/r/configuration-options/
    scatterplot <- scatterplot |>
      plotly::config(displaylogo = FALSE) |> # , displayModeBar = TRUE) # TRUE makes bar always visible
      plotly::add_trace(
        data = param1,
        x = ~TADA.ResultMeasureValue,
        y = ~TADA.ConsolidatedDepth,
        name = TADA_CharStringRemoveNANone(paste0(
          param1$TADA.ResultSampleFractionText[1],
          " ",
          param1$TADA.CharacteristicName[1],
          " ",
          param1$TADA.MethodSpeciationName[1],
          " ",
          "(",
          param1$TADA.ResultMeasure.MeasureUnitCode[1],
          ")"
        )),
        marker = list(size = 10, color = tada.pal[10]),
        line = list(color = tada.pal[5], width = 2),
        hoverinfo = "text",
        hovertext = paste(
          "Result:",
          paste0(
            param1$TADA.ResultMeasureValue,
            " ",
            param1$TADA.ResultMeasure.MeasureUnitCode
          ),
          "<br>",
          "Activity Start Date:",
          param1$ActivityStartDate,
          "<br>",
          "Activity Start Date Time:",
          param1$ActivityStartDateTime,
          "<br>",
          "Depth:",
          paste0(
            param1$TADA.ConsolidatedDepth,
            " ",
            param1$TADA.ConsolidatedDepth.Unit
          ),
          "<br>",
          "Activity Relative Depth Name:",
          param1$ActivityRelativeDepthName,
          "<br>",
          "TADA.DepthCategory.Flag:",
          paste0(param1$TADA.DepthCategory.Flag),
          "<br>"
        )
      )
  }

  # first parameter has a single value where units are depth
  if (
    length(groups) >= 1 &&
      nrow(param1) > 0 &&
      param1$TADA.CharacteristicName[1] %in% depth.params
  ) {
    scatterplot <- scatterplot |>
      plotly::add_lines(
        y = param1$TADA.ConsolidatedDepth[1],
        x = xrange,
        name = TADA_CharStringRemoveNANone(paste0(
          param1$TADA.ResultSampleFractionText[1],
          " ",
          param1$TADA.CharacteristicName[1],
          " ",
          param1$TADA.MethodSpeciationName[1],
          " ",
          "(",
          param1$TADA.ResultMeasure.MeasureUnitCode[1],
          ")"
        )),
        showlegend = TRUE,
        line = list(color = tada.pal[10], dash = "dash"),
        hoverinfo = "text",
        hovertext = paste(
          "Result:",
          paste0(
            param1$TADA.ResultMeasureValue,
            " ",
            param1$TADA.ResultMeasure.MeasureUnitCode
          ),
          "<br>",
          "Activity Start Date:",
          param1$ActivityStartDate,
          "<br>",
          "Activity Start Date Time:",
          param1$ActivityStartDateTime,
          "<br>",
          "Depth:",
          paste0(
            param1$TADA.ConsolidatedDepth,
            " ",
            param1$TADA.ConsolidatedDepth.Unit
          ),
          "<br>",
          "Activity Relative Depth Name:",
          param1$ActivityRelativeDepthName,
          "<br>",
          "TADA.DepthCategory.Flag:",
          paste0(param1$TADA.DepthCategory.Flag),
          "<br>"
        )
      )
  }

  # second parameter has a depth profile
  if (
    length(groups) >= 2 &&
      nrow(param2) > 0 &&
      !param2$TADA.CharacteristicName[1] %in% depth.params
  ) {
    scatterplot <- scatterplot |>
      plotly::add_trace(
        data = param2,
        x = ~TADA.ResultMeasureValue,
        y = ~TADA.ConsolidatedDepth,
        name = TADA_CharStringRemoveNANone(paste0(
          param2$TADA.ResultSampleFractionText[1],
          " ",
          param2$TADA.CharacteristicName[1],
          " ",
          param2$TADA.MethodSpeciationName[1],
          " ",
          "(",
          param2$TADA.ResultMeasure.MeasureUnitCode[1],
          ")"
        )),
        marker = list(size = 10, color = tada.pal[12]),
        line = list(color = tada.pal[3], width = 2),
        hoverinfo = "text",
        hovertext = paste(
          "Result:",
          paste0(
            param2$TADA.ResultMeasureValue,
            " ",
            param2$TADA.ResultMeasure.MeasureUnitCode
          ),
          "<br>",
          "Activity Start Date:",
          param2$ActivityStartDate,
          "<br>",
          "Activity Start Date Time:",
          param2$ActivityStartDateTime,
          "<br>",
          "Depth:",
          paste0(
            param2$TADA.ConsolidatedDepth,
            " ",
            param2$TADA.ConsolidatedDepth.Unit
          ),
          "<br>",
          "Activity Relative Depth Name:",
          param2$ActivityRelativeDepthName,
          "<br>",
          "TADA.DepthCategory.Flag:",
          paste0(param2$TADA.DepthCategory.Flag),
          "<br>"
        )
      )
  }

  # second parameter has a single value where units are depth
  if (
    length(groups) >= 2 &&
      nrow(param2) > 0 &&
      param2$TADA.CharacteristicName[1] %in% depth.params
  ) {
    scatterplot <- scatterplot |>
      plotly::add_lines(
        y = param2$TADA.ConsolidatedDepth[1],
        x = xrange,
        name = TADA_CharStringRemoveNANone(paste0(
          param2$TADA.ResultSampleFractionText[1],
          " ",
          param2$TADA.CharacteristicName[1],
          " ",
          param2$TADA.MethodSpeciationName[1],
          " ",
          "(",
          param2$TADA.ResultMeasure.MeasureUnitCode[1],
          ")"
        )),
        # inherit = FALSE,
        showlegend = TRUE,
        line = list(color = tada.pal[12], dash = "dash"),
        hoverinfo = "text",
        hovertext = ~ paste(
          "Result:",
          paste0(
            param2$TADA.ResultMeasureValue,
            " ",
            param2$TADA.ResultMeasure.MeasureUnitCode
          ),
          "<br>",
          "Activity Start Date:",
          param2$ActivityStartDate,
          "<br>",
          "Activity Start Date Time:",
          param2$ActivityStartDateTime,
          "<br>",
          "Depth:",
          paste0(
            param2$TADA.ConsolidatedDepth,
            " ",
            param2$TADA.ConsolidatedDepth.Unit
          ),
          "<br>",
          "Activity Relative Depth Name:",
          param2$ActivityRelativeDepthName,
          "<br>",
          "TADA.DepthCategory.Flag:",
          paste0(param2$TADA.DepthCategory.Flag),
          "<br>"
        )
      )
  }

  # third parameter has a depth profile
  if (
    length(groups) >= 3 &&
      nrow(param3) > 0 &&
      !param3$TADA.CharacteristicName[1] %in% depth.params
  ) {
    scatterplot <- scatterplot |>
      plotly::add_trace(
        data = param3,
        x = ~TADA.ResultMeasureValue,
        y = ~TADA.ConsolidatedDepth,
        name = TADA_CharStringRemoveNANone(paste0(
          param3$TADA.ResultSampleFractionText[1],
          " ",
          param3$TADA.CharacteristicName[1],
          " ",
          param3$TADA.MethodSpeciationName[1],
          " ",
          "(",
          param3$TADA.ResultMeasure.MeasureUnitCode[1],
          ")"
        )),
        marker = list(size = 10, color = tada.pal[11]),
        line = list(color = tada.pal[9], width = 2),
        hoverinfo = "text",
        hovertext = paste(
          "Result:",
          paste0(
            param3$TADA.ResultMeasureValue,
            " ",
            param3$TADA.ResultMeasure.MeasureUnitCode
          ),
          "<br>",
          "Activity Start Date:",
          param3$ActivityStartDate,
          "<br>",
          "Activity Start Date Time:",
          param3$ActivityStartDateTime,
          "<br>",
          "Depth:",
          paste0(
            param3$TADA.ConsolidatedDepth,
            " ",
            param3$TADA.ConsolidatedDepth.Unit
          ),
          "<br>",
          "Activity Relative Depth Name:",
          param3$ActivityRelativeDepthName,
          "<br>",
          "TADA.DepthCategory.Flag:",
          paste0(param3$TADA.DepthCategory.Flag),
          "<br>"
        )
      )
  }

  # third parameter has a single value where units are depth
  if (
    length(groups) >= 3 &&
      nrow(param3) > 0 &&
      param3$TADA.CharacteristicName[1] %in% depth.params
  ) {
    scatterplot <- scatterplot |>
      plotly::add_lines(
        y = param3$TADA.ConsolidatedDepth[1],
        x = xrange,
        name = TADA_CharStringRemoveNANone(paste0(
          param3$TADA.ResultSampleFractionText[1],
          " ",
          param3$TADA.CharacteristicName[1],
          " ",
          param3$TADA.MethodSpeciationName[1],
          " ",
          "(",
          param3$TADA.ResultMeasure.MeasureUnitCode[1],
          ")"
        )),
        # inherit = FALSE,
        showlegend = TRUE,
        line = list(color = tada.pal[11], dash = "dash"),
        hoverinfo = "text",
        hovertext = paste(
          "Result:",
          paste0(
            param3$TADA.ResultMeasureValue,
            " ",
            param3$TADA.ResultMeasure.MeasureUnitCode
          ),
          "<br>",
          "Activity Start Date:",
          param3$ActivityStartDate,
          "<br>",
          "Activity Start Date Time:",
          param3$ActivityStartDateTime,
          "<br>",
          "Depth:",
          paste0(
            param3$TADA.ConsolidatedDepth,
            " ",
            param3$TADA.ConsolidatedDepth.Unit
          ),
          "<br>",
          "Activity Relative Depth Name:",
          param3$ActivityRelativeDepthName,
          "<br>",
          "TADA.DepthCategory.Flag:",
          paste0(param3$TADA.DepthCategory.Flag),
          "<br>"
        )
      )
  }

  # add horizontal lines for depth profile category
  if (isTRUE(depthcat) && is.na(surfacevalue) && is.na(bottomvalue)) {
    stop(
      "TADA_DepthProfilePlot: No depth categories can be determined when both surfacevalue and bottomvalue are NA. Supply one or both values and run the function again."
    )
  }

  if (isTRUE(depthcat) && (!is.na(surfacevalue) || !is.na(bottomvalue))) {
    # create list to store depth annotation text
    depth_annotations <- list()

    # adjust margins of plot
    scatterplot <- scatterplot |>
      plotly::layout(
        margin = list(
          l = 50,
          r = 100,
          b = 100,
          t = (25 + (ceiling(nchar(title) / 50)) * 25),
          pad = 0
        )
      )

    if (is.numeric(surfacevalue)) {
      message("TADA_DepthProfilePlot: Adding surface delination to figure.")

      # add surface line
      scatterplot <- scatterplot |>
        plotly::add_lines(
          y = surfacevalue,
          x = xrange,
          inherit = FALSE,
          showlegend = FALSE,
          line = list(color = tada.pal[1]),
          hoverinfo = "text",
          hovertext = paste(surfacevalue, fig.depth.unit, sep = " ")
        )

      surface_text <- list(
        x = 1,
        y = surfacevalue / 2,
        xref = "paper",
        yref = "y",
        text = "Surface",
        showarrow = F,
        align = "right",
        xanchor = "left",
        yanchor = "center"
      )

      depth_annotations <- append(depth_annotations, list(surface_text))
    }

    if (is.numeric(bottomvalue)) {
      # find bottom depth robustly; skip annotation if no finite bottom
      bd <- suppressWarnings(max(
        plot.data$TADA.ConsolidatedDepth.Bottom,
        na.rm = TRUE
      ))
      if (is.finite(bd)) {
        message("TADA_DepthProfilePlot: Adding bottom delineation to figure.")
        scatterplot <- scatterplot |>
          plotly::add_lines(
            y = bd - bottomvalue,
            x = xrange,
            inherit = FALSE,
            showlegend = FALSE,
            line = list(color = tada.pal[1]),
            hoverinfo = "text",
            hovertext = paste(
              round((bd - bottomvalue), digits = 1),
              fig.depth.unit,
              sep = " "
            )
          )

        bottom_text <- list(
          x = 1,
          y = (ymax + (bd - bottomvalue)) / 2,
          xref = "paper",
          yref = "y",
          text = "Bottom",
          showarrow = F,
          align = "right",
          xanchor = "left",
          yanchor = "center"
        )

        depth_annotations <- append(depth_annotations, list(bottom_text))
      } else {
        message(
          "TADA_DepthProfilePlot: Bottom depth is not available; bottom delineation omitted."
        )
      }
    }

    if (is.numeric(surfacevalue) & is.numeric(bottomvalue) && is.finite(bd)) {
      middle_text <- list(
        x = 1,
        y = (surfacevalue + (bd - bottomvalue)) / 2,
        xref = "paper",
        yref = "y",
        text = "Middle",
        showarrow = F,
        align = "right",
        xanchor = "left",
        yanchor = "center"
      )

      depth_annotations <- append(depth_annotations, list(middle_text))
    }

    scatterplot <- scatterplot |>
      plotly::layout(annotations = depth_annotations)
  }

  # return plot with no depth profile category
  if (depthcat == FALSE) {
    scatterplot <- scatterplot
  }
  return(scatterplot)
}
