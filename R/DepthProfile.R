#' TADA_FlagDepthCategory
#'
#' This function creates a new column, TADA.DepthCategory.Flag with values: "No
#' depth info", "Surface", "Bottom", and
#' "Middle" when multiple depths are available.
#' Categories are: less than 2m (or user specified value) depth = "Surface", from
#' bottom up to 2m (or user specified value) from bottom = "Bottom", and all depths
#' in between the Surface and Bottom are assigned to the "Middle" category.
#'
#' When more than one result is available for a MonitoringLocationIdentifier,
#' ActivityStartDate, OrganizationIdentifier, and TADA.CharacteristicName, the
#' user can choose a single result value (average, max, or min value) to use for that
#' day and location. If results vary with depth, the user may also define whether
#' the daily aggregation occurs over each depth category (surface, middle, or bottom)
#' or for the entire depth profile.
#'
#' @param .data TADA dataframe which must include the columns
#' TADA.ActivityDepthHeightMeasure.MeasureValue, TADA.ResultDepthHeightMeasure.MeasureValue,
#' TADA.ActivityBottomDepthHeightMeasure.MeasureValue, and ActivityRelativeDepthName.
#'
#' @param dailyagg Character argument; with options "none", "avg", "min", or
#' "max". The default is dailyagg = "none". When dailyagg = "none", all results
#' will be retained. When dailyagg == "avg", the mean value in each group of
#' results (as determined by the depth category) will be identified or calculated for each
#' MonitoringLocation, ActivityDate, Organization ID, and TADA.CharacteristicName combination.
#' When dailyagg == "min" or when dailyagg == "max", the min or max
#' value in each group of results (as determined by the depth category) will
#' be identified or calculated for each MonitoringLocation, ActivityDate, and TADA.CharacteristicName
#' combination. An additional column, TADA.DepthProfileAggregation.Flag will be added
#' to describe aggregation.
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
#' @param aggregatedonly Boolean argument with options "TRUE" or "FALSE". The
#' default is aggregatedonly = "FALSE" which means that all results are returned.
#' When aggregatedonly = "TRUE", only aggregate values are returned.
#'
#' @param clean Boolean argument with options "TRUE" or "FALSE". The
#' default is clean = "FALSE" which means that all results are returned.
#' When clean = "TRUE", only aggregate results which can be assigned to a depth
#' category are included in the returned dataframe.
#'
#' @param .data TADA dataframe
#'
#' @return The same input TADA dataframe with additional columns TADA.DepthCategory.Flag,
#' TADA.DepthProfileAggregation.Flag, TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Bottom,
#' and TADA.ConsolidatedDepth.Unit. The consolidated depth fields are created by reviewing
#' multiple WQC columns where users may input depth information. If a daily_agg = "avg",
#' "min", or "max", aggregated values will be identified in the TADA.ResultAggregation.Flag
#' column. In the case of daily_agg = "avg", additional rows to display averages will be
#' added to the dataframe. They can be identified by the prefix ("TADA-") of
#' their result identifiers.
#'
#' @export
#'
#' @examples
#' # Load dataframe
#' data(Data_6Tribes_5y)
#'
#' # assign TADA.DepthCategory.Flag with no aggregation
#' Data_6Tribs_5y_DepthCat <- TADA_FlagDepthCategory(Data_6Tribes_5y)
#'
#' # assign TADA.DepthCategory.Flag and determine average values by depth category and returning only aggregate values
#' Data_6Tribs_5y_Mean <- TADA_FlagDepthCategory(Data_6Tribes_5y, bycategory = "all", dailyagg = "avg", aggregatedonly = FALSE)
#'
TADA_FlagDepthCategory <- function(.data, bycategory = "no", bottomvalue = 2, surfacevalue = 2, dailyagg = "none", aggregatedonly = FALSE, clean = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check aggregatedonly is boolean
  TADA_CheckType(aggregatedonly, "logical")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, c(
    "TADA.ActivityDepthHeightMeasure.MeasureValue",
    "TADA.ResultDepthHeightMeasure.MeasureValue",
    "ActivityRelativeDepthName",
    "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
    "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
    "TADA.CharacteristicName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ResultIdentifier",
    "MonitoringLocationIdentifier",
    "OrganizationIdentifier",
    "ActivityStartDate"
  ))

  # execute function after checks are passed

  depthcat.list <- c("Surface", "Bottom", "Middle")

  ard.ref <- utils::read.csv(system.file("extdata", "TADAActivityRelativeDepthRef.csv", package = "EPATADA")) %>%
    dplyr::rename(
      ARD_Category = TADA.DepthCategory.Flag,
      ActivityRelativeDepthName = Name
    ) %>%
    dplyr::select(ARD_Category, ActivityRelativeDepthName)

  depth.count <- .data %>%
    dplyr::filter(!is.na(TADA.ActivityDepthHeightMeasure.MeasureValue) |
      !is.na(TADA.ResultDepthHeightMeasure.MeasureValue)) %>%
    nrow()

  length.units <- c("M", "FT", "IN")

  depth.params <- c(
    "DEPTH, SECCHI DISK DEPTH",
    "DEPTH, SECCHI DISK DEPTH (CHOICE LIST)",
    "DEPTH, SECCHI DISK DEPTH REAPPEARS",
    "DEPTH, DATA-LOGGER (NON-PORTED)",
    "DEPTH, DATA-LOGGER (PORTED)",
    "RBP STREAM DEPTH - RIFFLE",
    "RBP STREAM DEPTH - RUN",
    "THALWEG DEPTH"
  )

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


  if (depth.count > 0) {
    print(paste("TADA_FlagDepthCategory: checking data set for depth values. ", depth.count, " results have depth values available.", sep = ""))

    print("TADA_FlagDepthCategory: assigning depth categories.")

    .data <- .data %>%
      # set equal to TADA.ResultDepthHeighMeasure.MeasureValue if available, otherwise use TADA.ActivityDepthHeightMeasure.MeasureValue
      dplyr::mutate(
        TADA.ConsolidatedDepth = ifelse(!is.na(TADA.ResultDepthHeightMeasure.MeasureValue), TADA.ResultDepthHeightMeasure.MeasureValue,
          TADA.ActivityDepthHeightMeasure.MeasureValue
        ),
        TADA.ConsolidatedDepth.Unit = ifelse(!is.na(TADA.ResultDepthHeightMeasure.MeasureUnitCode),
          TADA.ResultDepthHeightMeasure.MeasureUnitCode, TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ),
        TADA.ConsolidatedDepth = ifelse(TADA.CharacteristicName %in% depth.params,
          TADA.ResultMeasureValue, TADA.ConsolidatedDepth
        ),
        TADA.ConsolidatedDepth.Unit = ifelse(TADA.CharacteristicName %in% depth.params,
          TADA.ResultMeasure.MeasureUnitCode, TADA.ConsolidatedDepth.Unit
        ),
        TADA.ConsolidatedDepth.Unit = tolower(TADA.ConsolidatedDepth.Unit)
      ) %>%
      # use group_by to identify profile data
      dplyr::group_by(ActivityStartDate, MonitoringLocationIdentifier, OrganizationIdentifier) %>%
      # determine the number of Depths per group
      dplyr::mutate(
        DepthsPerGroup = length(unique(TADA.ConsolidatedDepth)),
        # determine bottom value using TADA.ActivityBottomDepthHeightMeasure.MeasureValue or the max depth record for profile data
        TADA.ConsolidatedDepth.Bottom = ifelse(DepthsPerGroup > 1 & is.na(TADA.ActivityBottomDepthHeightMeasure.MeasureValue), max(TADA.ConsolidatedDepth, na.rm = TRUE), TADA.ActivityBottomDepthHeightMeasure.MeasureValue)
      ) %>%
      dplyr::ungroup() %>%
      # assign depth categories by using depth information
      dplyr::mutate(TADA.DepthCategory.Flag = dplyr::case_when(
        TADA.ConsolidatedDepth <= surfacevalue ~ "Surface",
        TADA.ConsolidatedDepth <= TADA.ConsolidatedDepth.Bottom & TADA.ConsolidatedDepth >= TADA.ConsolidatedDepth.Bottom - bottomvalue ~ "Bottom",
        TADA.ConsolidatedDepth > surfacevalue & TADA.ConsolidatedDepth < TADA.ConsolidatedDepth.Bottom - bottomvalue ~ "Middle"
      )) %>%
      # assign depth categories that could not be assigned using depth
      dplyr::left_join(ard.ref, by = "ActivityRelativeDepthName") %>%
      dplyr::mutate(
        TADA.DepthCategory.Flag = ifelse(is.na(TADA.DepthCategory.Flag), ARD_Category, TADA.DepthCategory.Flag),
        TADA.DepthCategory.Flag = ifelse(is.na(TADA.ActivityDepthHeightMeasure.MeasureValue) & is.na(TADA.ConsolidatedDepth.Bottom) & is.na(TADA.ResultDepthHeightMeasure.MeasureValue) & is.na(TADA.DepthCategory.Flag), "No depth info", TADA.DepthCategory.Flag),
        TADA.DepthCategory.Flag = ifelse(is.na(TADA.DepthCategory.Flag), "Not enough depth info to determine category", TADA.DepthCategory.Flag)
      ) %>%
      dplyr::select(-ARD_Category, -DepthsPerGroup)

    if (depth.count == 0) {
      print(paste("TADA_FlagDepthCategory: checking data set for depth values. No results have depth values available, TADA_FlagDepthCategory cannot be used on this data set.", sep = ""))

      return(.data)
    }
  }

  if (clean == TRUE) {
    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag %in% depthcat.list)
  }

  if (clean == FALSE) {
    .data <- .data
  }

  if (bycategory == "all") {
    print("TADA_FlagDepthCategory: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, ActivityStartDate, and TADA.DepthCategory.Flag for aggregation by TADA.DepthCategory.Flag.")

    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate",
      "TADA.DepthCategory.Flag"
    )

    .data <- .data
  }

  if (bycategory == "no") {
    print("TADA_FlagDepthCategory: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for entire water column.")

    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )

    .data <- .data
  }

  if (bycategory == "surface") {
    print("TADA_FlagDepthCategory: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for surface samples only.")

    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )

    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag == "Surface")
  }

  if (bycategory == "middle") {
    print("TADA_FlagDepthCategory: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for middle samples only.")

    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )

    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag == "Middle")
  }

  if (bycategory == "bottom") {
    print("TADA_FlagDepthCategory: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for bottom samples only.")

    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )

    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag == "Bottom")
  }

  if (dailyagg == "none") {
    print("TADA_FlagDepthCategory: No aggregation performed.")

    # add TADA.ResultValue.Aggregation.Flag, remove unecessary columns, and order columns
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, "No aggregation perfomed", "No aggregation needed")) %>%
      dplyr::select(-DepthsByGroup) %>%
      dplyr::ungroup() %>%
      TADA_OrderCols()

    if (aggregatedonly == TRUE) {
      stop("Function not executed because clean cannot be TRUE while daily_agg is 'no'")
    }

    if (aggregatedonly == FALSE) {
      return(orig.data)
    }
  }
  if ((dailyagg == "avg")) {
    print("TADA_FlagDepthCategory: Calculating mean aggregate value with randomly selected metadata.")

    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, paste("Used in averaging results ", cattype, " but not selected as aggregate value"), "No aggregation needed"),
        TADA.DepthProfileAggregation.Flag = ifelse(!TADA.DepthCategory.Flag %in% depthcat.list, "No aggregation needed", TADA.DepthProfileAggregation.Flag)
      )

    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, calculate mean result value per group, and assign random metadata from group.
    agg.data <- orig.data %>%
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) %>%
      dplyr::mutate(TADA.ResultMeasureValue1 = mean(TADA.ResultMeasureValue, na.rm = TRUE)) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = paste0("Calculated mean aggregate value ", cattype, ", with randomly selected metadata from a row in the aggregate group")) %>%
      dplyr::select(-TADA.ResultMeasureValue, -DepthsByGroup) %>%
      dplyr::rename(TADA.ResultMeasureValue = TADA.ResultMeasureValue1) %>%
      dplyr::mutate(ResultIdentifier = paste0("TADA-", ResultIdentifier)) %>%
      dplyr::ungroup()

    if (aggregatedonly == TRUE) {
      rm(orig.data)

      return(agg.data)
    }

    if (aggregatedonly == FALSE) {
      # combine original and aggregate data
      comb.data <- plyr::rbind.fill(orig.data, agg.data) %>%
        dplyr::ungroup() %>%
        dplyr::select(-DepthsByGroup) %>%
        TADA_OrderCols()

      rm(agg.data, orig.data)

      return(comb.data)
    }
  }
  if ((dailyagg == "min")) {
    print("TADA_FlagDepthCategory: Selecting minimum aggregate value.")

    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, paste("Used in minimum aggregation ", cattype, "but not selected"), "No aggregation needed"),
        TADA.DepthProfileAggregation.Flag = ifelse(!TADA.DepthCategory.Flag %in% depthcat.list, "No aggregation needed", TADA.DepthProfileAggregation.Flag)
      )

    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, and select minimum result value per group.
    agg.data <- orig.data %>%
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) %>%
      dplyr::slice_min(order_by = TADA.ResultMeasureValue, n = 1, with_ties = FALSE) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = paste0("Selected as min aggregate value ", cattype)) %>%
      dplyr::select(-DepthsByGroup) %>%
      dplyr::ungroup()

    if (aggregatedonly == TRUE) {
      rm(orig.data)

      return(agg.data)
    }

    if (aggregatedonly == FALSE) {
      # create list of result identifiers for selected aggregate data
      agg.list <- agg.data %>%
        dplyr::ungroup() %>%
        dplyr::select(ResultIdentifier) %>%
        unique() %>%
        dplyr::pull()

      # combine original and aggregate data
      comb.data <- orig.data %>%
        dplyr::filter(!ResultIdentifier %in% agg.list) %>%
        plyr::rbind.fill(agg.data) %>%
        dplyr::ungroup() %>%
        dplyr::select(-DepthsByGroup) %>%
        TADA_OrderCols()

      rm(agg.data, orig.data, agg.list)

      return(comb.data)
    }
  }

  if ((dailyagg == "max")) {
    print("TADA_FlagDepthCategory: Selecting maximum aggregate value.")

    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, paste("Used in maximum aggregation ", cattype, "but not selected"), "No aggregation needed"))

    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, and select maximum result value per group.
    agg.data <- orig.data %>%
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) %>%
      dplyr::slice_max(order_by = TADA.ResultMeasureValue, n = 1, with_ties = FALSE) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = paste0("Selected as max aggregate value ", cattype)) %>%
      dplyr::mutate(ResultIdentifier = paste0("TADA-", ResultIdentifier)) %>%
      dplyr::select(-DepthsByGroup) %>%
      dplyr::ungroup()

    if (aggregatedonly == TRUE) {
      rm(orig.data)

      return(agg.data)
    }

    if (aggregatedonly == FALSE) {
      # create list of result identifiers for selected aggregate data
      agg.list <- agg.data %>%
        dplyr::ungroup() %>%
        dplyr::select(ResultIdentifier) %>%
        unique() %>%
        dplyr::pull()

      # combine original and aggregate data
      comb.data <- orig.data %>%
        dplyr::filter(!ResultIdentifier %in% agg.list) %>%
        plyr::rbind.fill(agg.data) %>%
        dplyr::ungroup() %>%
        dplyr::select(-DepthsByGroup) %>%
        TADA_OrderCols()

      rm(agg.data, orig.data, agg.list)

      return(comb.data)
    }
  }
}


#' TADA_IDDepthProfiles
#'
#' This function identifies depth profiles within a dataframe to assist the user in
#' selecting params for TADA_DepthProfilePlot. A TADA compatible data set is required.
#' If TADA_FlagDepthCategory has not yet been run, it will be run as part of this
#' function. The output dataframe is grouped by MonitoringLocationIdentifier,
#' OrganizationIdentifier, and ActivityStartDate.
#'
#' A new column, TADA.CharacteristicsForDepthProfile, is created which lists the
#' characteristics available for depth profile analysis. Using the, nresults param,
#' users can specify whether characteristic names should be followed by the number
#' of results available for the characteristic in parentheses.
#'
#' @param .data TADA dataframe which must include the columns ActivityStartDate,
#' TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Unit, TADA.ConsolidatedDepth.Bottom,
#' TADA.ResultMeasureValue, TADA.ResultMeasureValue.UnitCode,
#' OrganizationIdentifier, MonitoringLocationName, MonitoringLocationIdentifier,
#' and TADA.ComparableDataIdentifier.
#'
#' @param nresults Boolean argument with options "TRUE" or "FALSE". The
#' default is nresults = TRUE, which means that the number of results for each
#' characteristic are added within the TADA.CharacteristicsForDepthProfile column.
#' When nresults = FALSE.
#'
#' @param nvalue numeric argument to specify the number of results required to identify
#' a depth profile. The default is 2, which means that a depth profile will be identified
#' if 2 or more results at different depths exists for the same ActivityStartDate,
#' MonitoringLocationIdentifier, OrganizationIdentifier, and TADA.ComparableDataIdentifier.
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
#' @return A dataframe with the columns MonitoringLocationIdentifier,
#' MonitoringLocationName, OrganizationIdentifier, ActivityStartDate,
#' TADA.CharacteristicsForDepthProfile. Based on the user input for the nresults
#' param, TADA.CharacteristicsForDepthProfile may or may not contain the number
#' of results for each characteristic.
#'
#' @export
#'
#' @examples
#' # Load dataframe
#' data(Data_6Tribes_5y)
#'
#' # find depth profile data without showing number of results
#' Data_6Tribes_5y_DepthProfileID_Nresults <- TADA_IDDepthProfiles(Data_6Tribes_5y, nresults = FALSE)
#'
#' # find depth profile data showing number of results
#' Data_6Tribes_5y_DepthProfileID <- TADA_IDDepthProfiles(Data_6Tribes_5y)
#'
TADA_IDDepthProfiles <- function(.data, nresults = TRUE, nvalue = 2, aggregates = FALSE) {
  # check for columns created in TADA_FlagDepthCategory and run the function if they are missing
  # add check that depth category flag function has been run, run it if it has not
  flag.func.cols <- c(
    "TADA.ConsolidatedDepth", "TADA.ConsolidatedDepth.Unit",
    "TADA.ConsolidatedDepth.Bottom, TADA.DepthCategory.Flag",
    "TADA.DepthProfileAggregation.Flag"
  )

  if (all(flag.func.cols %in% colnames(.data)) == TRUE) {
    print("TADA_IDDepthProfiles: Necessary columns from TADA_FlagDepthCategory function are included in the dataframe.")

    .data <- .data
  }

  if (any(flag.func.cols %in% colnames(.data)) == FALSE) {
    print("TADA_IDDepthProfiles: Necessary columns are being added to the dataframe using TADA_DepthCatgegory.Flag function.")

    .data <- TADA_FlagDepthCategory(.data)
  }

  depth.params <- c("DEPTH, SECCHI DISK DEPTH")

  if (aggregates == FALSE) {
    if ("TADA.DepthProfileAggregation.Flag" %in% names(.data) == TRUE) {
      .data <- .data %>%
        dplyr::filter(TADA.DepthProfileAggregation.Flag != c("Calculated mean aggregate value, with randomly selected metadata from a row in the aggregate group"))

      if ("TADA.DepthProfileAggregation.Flag" %in% names(.data) == FALSE) {
        .data <- .data
      }
    }


    if (aggregates == TRUE) {
      .data <- .data
    }
  }


  if (nresults == TRUE) {
    .data <- .data %>%
      dplyr::select(
        MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName,
        OrganizationIdentifier, ActivityStartDate, TADA.CharacteristicName, TADA.ComparableDataIdentifier,
        TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Unit, TADA.ConsolidatedDepth.Bottom
      ) %>%
      dplyr::group_by(
        MonitoringLocationIdentifier, OrganizationIdentifier, ActivityStartDate,
        TADA.ComparableDataIdentifier
      ) %>%
      dplyr::mutate(
        TADA.NResults = length(unique(TADA.ConsolidatedDepth)),
        TADA.CharacteristicsForDepthProfile = paste(
          TADA.ComparableDataIdentifier, " (", TADA.NResults, ")",
          sep = ""
        )
      ) %>%
      dplyr::filter(TADA.NResults >= nvalue | TADA.CharacteristicName %in% depth.params) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(MonitoringLocationIdentifier, OrganizationIdentifier, ActivityStartDate) %>%
      # check that for results with only a single depth unit (ex: secchi disk depth) that other results are available in group
      dplyr::mutate(MeanResults = mean(TADA.NResults)) %>%
      dplyr::filter(MeanResults > 1) %>%
      dplyr::mutate(
        TADA.CharacteristicsForDepthProfile = paste(
          unique(TADA.CharacteristicsForDepthProfile), ";",
          collapse = ""
        ),
        TADA.CharacteristicsForDepthProfile = stringr::str_replace_all(paste(sort(unique(unlist(strsplit(TADA.CharacteristicsForDepthProfile, ";", )))), collapse = ";"), " ;", "; ")
      ) %>%
      dplyr::select(
        MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, OrganizationIdentifier, ActivityStartDate,
        TADA.CharacteristicsForDepthProfile
      ) %>%
      unique()

    return(.data)
  }

  if (nresults == FALSE) {
    .data <- .data %>%
      dplyr::select(
        MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName,
        OrganizationIdentifier, ActivityStartDate, TADA.CharacteristicName, TADA.ComparableDataIdentifier,
        TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Unit, TADA.ConsolidatedDepth.Bottom
      ) %>%
      dplyr::group_by(
        MonitoringLocationIdentifier, OrganizationIdentifier, ActivityStartDate,
        TADA.ComparableDataIdentifier
      ) %>%
      dplyr::mutate(TADA.NResults = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::filter(TADA.NResults >= nvalue | TADA.CharacteristicName %in% depth.params) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(MonitoringLocationIdentifier, OrganizationIdentifier, ActivityStartDate) %>%
      # check that for results with only a single depth unit (ex: secchi disk depth) that other results are available in group
      dplyr::mutate(MeanResults = mean(TADA.NResults)) %>%
      dplyr::filter(MeanResults > 1) %>%
      dplyr::mutate(
        TADA.CharacteristicsForDepthProfile = paste(
          unique(TADA.ComparableDataIdentifier), ";",
          collapse = ""
        ),
        TADA.CharacteristicsForDepthProfile = stringr::str_replace_all(paste(sort(unique(unlist(strsplit(TADA.CharacteristicsForDepthProfile, ";", )))), collapse = ";"), " ;", "; ")
      ) %>%
      dplyr::select(
        MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, OrganizationIdentifier, ActivityStartDate,
        TADA.CharacteristicsForDepthProfile
      ) %>%
      unique()

    return(.data)
  }
}

#' Create A Three-Characteristic Depth Profile
#'
#' @param .data TADA dataframe containing the data downloaded from the WQP,
#'   where each row represents a unique data record. TADA_FlagDepthCategory
#'   has been run as dataframe must include the columns TADA.DepthCategory.Flag,
#'   TADA.ResultDepthHeightMeasure.MeasureUnitCode, TADA.ActivityDepthHeightMeasure.MeasureUnitCode,
#'   and TADA.ActivityDepthHeightMeasure.MeasureValue. Units for all depth fields
#'   must be the same. This can be accomplished using TADA_AutoClean() or
#'   TADA_ConvertDepthUnits.
#'
#' @param groups A vector of two identifiers from the TADA.ComparableDataIdentifier column.
#' For example, the groups could be 'DISSOLVED OXYGEN (DO)_NA_NA_UG/L' and 'PH_NA_NA_NA'.
#' These groups will be specific to your dataframe. The TADA_IDDepthProfiles can be
#' used to identify available groups.
#'
#' @param location A single MonitoringLocationIdentifier to plot the depth profile.
#' A MonitoringLocationIdentifier must be entered or an error will be returned and
#' no depth profile will be created.
#'
#' @param activity_date The date the depth profile results were collected.
#'
#' @param depthcat Boolean argument indicating whether delineation between depth
#' categories should be shown on the depth profile figure. depthcat = TRUE is the
#' default and displays solid black lines to delineate between surface, middle, and
#' bottom samples and labels each section of the plot.
#'
#' @param bottomvalue numeric argument. The user enters how many meters from the
#' bottom should be included in the "Bottom" category. Default is
#' bottomvalue = 2.
#'
#' @param surfacevalue numeric argument. The user enters how many meters from the
#' surface should be included in the "Surface" category. Default is surfacevalue = 2.
#'
#' @param unit Character argument. The enters either "m" or "ft" to specify which
#' depth units should be used for the plot. Default is "m".
#'
#' @return A depth profile plot displaying up to three parameters for a single
#' MonitoringLocationIdentifier. Displaying depth categories is optional with the
#' depthcat argument.
#'
#' @export
#'
#' @examples
#' # Load example dataframe:
#' data(Data_6Tribes_5y_Harmonized)
#' # Create a depth profile figure with three parameters for a single monitoring location and date
#' TADA_DepthProfilePlot(Data_6Tribes_5y_Harmonized,
#'   groups = c("TEMPERATURE_NA_NA_DEG C", "PH_NA_NA_STD UNITS", "DEPTH, SECCHI DISK DEPTH_NA_NA_M"),
#'   location = "REDLAKE_WQX-ANKE",
#'   activity_date = "2018-10-04"
#' )
#'
#' # Load example dataframe:
#' data(Data_6Tribes_5y_Harmonized)
#' # Create a depth profile figure with two parameters for a single monitoring location and date without displaying depth categories
#' TADA_DepthProfilePlot(Data_6Tribes_5y_Harmonized,
#'   groups = c("CONDUCTIVITY_NA_NA_US/CM", "DISSOLVED OXYGEN (DO)_NA_NA_MG/L"),
#'   location = "REDLAKE_WQX-JOHN",
#'   activity_date = "2018-07-31",
#'   depthcat = FALSE
#' )
TADA_DepthProfilePlot <- function(.data,
                                  groups = NULL,
                                  location = NULL,
                                  activity_date = NULL,
                                  depthcat = TRUE,
                                  surfacevalue = 2,
                                  bottomvalue = 2,
                                  unit = "m") {
  # check to see if TADA.ComparableDataIdentifier column is present
  if ("TADA.ComparableDataIdentifier" %in% colnames(.data)) {
    .data <- .data

    if (!"TADA.ComparableDataIdentifier" %in% colnames(.data)) {
      print("TADA.ComparableDataIdentifier column not present in data set. Run TADA_CreateComparableID to create TADA.ComparableDataIdentifier.")

      stop()
    }
  }


  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # add check that depth category flag function has been run, run it if it has not
  flag.func.cols <- c(
    "TADA.ConsolidatedDepth", "TADA.ConsolidatedDepth.Unit",
    "TADA.ConsolidatedDepth.Bottom, TADA.DepthCategory.Flag"
  )

  if (all(flag.func.cols %in% colnames(.data)) == TRUE) {
    print("TADA_DepthProfilePlot: Necessary columns from TADA_FlagDepthCategory function are included in the dataframe")

    .data <- .data
  }

  if (any(flag.func.cols %in% colnames(.data)) == FALSE) {
    print("TADA_DepthProfilePlot: Running TADA_FlagDepthCategory function to add required columns to dataframe")


    if (bottomvalue == "null" & surfacevalue == "null") {
      .data <- TADA_FlagDepthCategory(.data, surfacevalue = 2, bottomvalue = 2) %>%
        dplyr::mutate(TADA.DepthCategory.Flag = NA)
    }


    if (surfacevalue == "null" & is.numeric(bottomvalue)) {
      .data <- TADA_FlagDepthCategory(.data, surfacevalue = 2, bottomvalue = bottomvalue) %>%
        dplyr::mutate(TADA.DepthCatgeory.Flag = ifelse(TADA.DepthCategory.Flag %in% c("Surface", "Middle"),
          NA, TADA.DepthCategory.Flag
        ))
    }

    if (bottomvalue == "null" & is.numeric(surfacevalue)) {
      .data <- TADA_FlagDepthCategory(.data, surfacevalue = surfacevalue, bottomvalue = 2) %>%
        dplyr::mutate(TADA.DepthCatgeory.Flag = ifelse(TADA.DepthCategory.Flag %in% c("Bottom", "Middle"),
          NA, TADA.DepthCategory.Flag
        ))
    }

    if (is.numeric(bottomvalue) & is.numeric(surfacevalue)) {
      .data <- TADA_FlagDepthCategory(.data, surfacevalue = surfacevalue, bottomvalue = bottomvalue)
    }
  }

  # add convert depth unit (this still needs to be added), for now print warning and stop function if units don't match
  .data <- .data %>% dplyr::filter(!is.na(TADA.ConsolidatedDepth))

  if (.data$TADA.ConsolidatedDepth.Unit[1] == unit) {
    print("TADA_DepthProfilePlot: Depth unit in data set matches depth unit specified by user for plot. No conversion necessary.")

    .data <- .data

    if (.data$TADA.ConsolidatedDepth.Unit[1] != unit) {
      stop("TADA_DepthProfilePlot: Depth unit in data set does not match depth unit specified by user for plot. Convert units in data or specify correct unit in TADA_DepthProfilePlot function.")
    }
  }

  # create ID Depth Profiles data.frame to check against params

  param.check <- TADA_IDDepthProfiles(.data)


  if (is.null(location)) {
    print("TADA_DepthProfilePlot: No MonitoringLocationIdentifier selected, a depth profile cannot be generated.")

    stop()

    if (!location %in% param.check$MonitoringLocationIdentifier) {
      print("TADA_DepthProfilePlot: MonitoringLocationIdentifier selected is not in data set.")

      stop()
    }

    if (location %in% param.check$MonitoringLocationIdentifier) {
      print("TADA_DepthProfilePlot: MonitoringLocationIdentifier selected.")
    }
  }

  if (is.null(activity_date)) {
    print("TADA_DepthProfilePlot: No ActivityStartDate selected, a depth profile cannot be generated.")

    stop()

    if (!activity_date %in% param.check$ActivityStartDate) {
      print("TADA_DepthProfilePlot: ActivityStartDate selected is not in data set.")
    }

    stop()

    if (activity_date %in% param.check$ActivityStartDate) {
      print("TADA_DepthProfilePlot: ActivityStartDate selected.")
    }
  }

  if (is.null(groups)) {
    print("TADA_DepthProfilePlot: No groups selected, a depth profile cannot be generated.")

    stop()

    if (!is.null(groups)) {
      groups.length <- length(groups)

      if (groups.length > 0) {
        if (stringr::str_detect(param.check$TADA.CharacteristicsForDepthProfile, groups[1]) == FALSE) {
          print("TADA_DepthProfilePlot: First of groups for depth profile plot does not exist in data set.")
        }

        stop()

        if (stringr::str_detect(param.check$TADA.CharacteristicsForDepthProfile, groups[1]) == TRUE) {
          print("TADA:DepthProfilePlot: First of groups for depth profile exists in data set.")
        }
      }

      if (groups.length > 1) {
        if (stringr::str_detect(param.check$TADA.CharacteristicsForDepthProfile, groups[2]) == FALSE) {
          print("TADA_DepthProfilePlot: Second of groups for depth profile plot does not exist in data set.")
        }

        stop()

        if (stringr::str_detect(param.check$TADA.CharacteristicsForDepthProfile, groups[2]) == TRUE) {
          print("TADA:DepthProfilePlot: Second of groups for depth profile exists in data set.")
        }
      }

      if (groups.length > 2) {
        if (stringr::str_detect(param.check$TADA.CharacteristicsForDepthProfile, groups[3]) == FALSE) {
          print("TADA_DepthProfilePlot: Third of groups for depth profile plot does not exist in data set.")
        }

        stop()

        if (stringr::str_detect(param.check$TADA.CharacteristicsForDepthProfile, groups[3]) == TRUE) {
          print("TADA:DepthProfilePlot: Third of groups for depth profile exists in data set.")
        }
      }
    }


    if (!activity_date %in% param.check$ActivityStartDate) {
      print("TADA_DepthProfilePlot: ActivityStartDate selected is not in data set.")
    }

    stop()

    if (activity_date %in% param.check$ActivityStartDate) {
      print("TADA_DepthProfilePlot: ActivityStartDate selected.")
    }

    param.check <- param.check %>%
      dplyr::filter(ActivityStartDate == activity_date)
  }

  # remove param.check
  rm(param.check)

  # list required columns
  reqcols <- c(
    "TADA.ResultDepthHeightMeasure.MeasureValue",
    "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
    "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
    "TADA.ActivityDepthHeightMeasure.MeasureValue",
    "TADA.DepthCategory.Flag",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "MonitoringLocationIdentifier",
    "MonitoringLocationName",
    "ActivityStartDate",
    "ActivityStartDateTime",
    "TADA.ConsolidatedDepth",
    "TADA.ConsolidatedDepth.Unit",
    "TADA.ConsolidatedDepth.Bottom"
  )

  # check .data has required columns
  TADA_CheckColumns(.data, reqcols)

  print("TADA_DepthProfilePlot: Identifying available depth profile data.")

  # identify depth profile data
  depth.params <- c(
    "DEPTH, SECCHI DISK DEPTH",
    "DEPTH, SECCHI DISK DEPTH (CHOICE LIST)",
    "DEPTH, SECCHI DISK DEPTH REAPPEARS",
    "DEPTH, DATA-LOGGER (NON-PORTED)",
    "DEPTH, DATA-LOGGER (PORTED)",
    "RBP STREAM DEPTH - RIFFLE",
    "RBP STREAM DEPTH - RUN",
    "THALWEG DEPTH"
  )

  depthprofile.avail <- .data %>%
    dplyr::filter(
      !is.na(TADA.ConsolidatedDepth),
      MonitoringLocationIdentifier %in% location,
      ActivityStartDate %in% activity_date,
      TADA.ActivityMediaName == "WATER"
    ) %>%
    dplyr::group_by(
      TADA.ComparableDataIdentifier,
      ActivityStartDate, TADA.ConsolidatedDepth
    ) %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      MonitoringLocationIdentifier, TADA.ComparableDataIdentifier,
      ActivityStartDate
    ) %>%
    dplyr::mutate(N = length(TADA.ResultMeasureValue)) %>%
    dplyr::filter(N > 2 | TADA.CharacteristicName %in% depth.params) %>%
    dplyr::ungroup() %>%
    dplyr::select(-N)

  depth.params.groups <- depthprofile.avail %>%
    dplyr::filter(
      TADA.ComparableDataIdentifier %in% groups,
      TADA.CharacteristicName %in% depth.params
    ) %>%
    dplyr::select(TADA.ComparableDataIdentifier) %>%
    unique() %>%
    dplyr::pull()

  # identify depth unit being used in graph
  fig.depth.unit <- depthprofile.avail %>%
    dplyr::select(TADA.ConsolidatedDepth.Unit) %>%
    dplyr::filter(!is.na(TADA.ConsolidatedDepth.Unit)) %>%
    unique() %>%
    dplyr::pull()

  # if any depth parameter (ex: secchi) data

  if (length(intersect(groups, depth.params.groups)) == 0) {
    depth.params.string <- toString(depth.params, sep = "; ") %>%
      stringi::stri_replace_last(" or ", fixed = "; ")

    profile.data <- depthprofile.avail


    rm(depth.params.string, depthprofile.avail)
  }

  if (length(intersect(groups, depth.params.groups)) > 0) {
    # add depth param (ex: secchi) results
    depth.params.string <- toString(depth.params, sep = "; ") %>%
      stringi::stri_replace_last(" or ", fixed = "; ")

    depth.units <- c("m", "ft", "in", "m", "m", "ft", "ft", "in", "in", "m", "ft", "in")

    depth.params.avail <- .data %>%
      dplyr::filter(
        MonitoringLocationIdentifier %in% location,
        TADA.CharacteristicName %in% depth.params,
        ActivityStartDate %in% activity_date,
        TADA.ActivityMediaName == "WATER"
      ) %>%
      dplyr::group_by(TADA.CharacteristicName, ActivityStartDate, MonitoringLocationIdentifier) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::ungroup()

    if (unique(depth.params.avail$TADA.ConsolidatedDepth.Unit) == fig.depth.unit) {
      print(paste("TADA_DepthProfilePlot: Any results for", depth.params.string, "match the depth unit selected for the figure."))

      depth.params.avail <- depth.params.avail


      if (unique(depth.params.avail$TADA.ConsolidatedDepth.Unit) != fig.depth.unit) {
        print(paste(
          "TADA_DepthProfilePlot: Converting depth units for any results for",
          depth.params.string, "results to match depth units selected for the figure."
        ))

        depth.units <- c("m", "ft", "in", "m", "m", "ft", "ft", "in", "in", "m", "ft", "in")

        result.units <- c("m", "ft", "in", "ft", "in", "m", "in", "m", "ft", "cm", "cm", "cm")

        convert.factor <- c("1", "1", "1", "0.3048", "0.0254", "3.281", "0.083", "39.3701", "12", "0.01", "0.032808", "0.39")

        secchi.conversion <- data.frame(result.units, depth.units, convert.factor) %>%
          dplyr::rename(
            TADA.ConsolidatedDepth.Unit = result.units,
            YAxis.DepthUnit = depth.units,
            SecchiConversion = convert.factor
          )

        depth.params.avail <- depth.params.avail %>%
          dplyr::mutate(YAxis.DepthUnit = fig.depth.unit) %>%
          dplyr::left_join(secchi.conversion) %>%
          dplyr::mutate(
            TADA.ConsolidatedDepth.Unit = fig.depth.unit,
            TADA.ConsolidatedDepth = TADA.ResultMeasureValue * as.numeric(SecchiConversion)
          ) %>%
          dplyr::select(-YAxis.DepthUnit, -SecchiConversion)

        rm(secchi.conversion, depth.params.string, depth.units, result.units, convert.factor)
      }
    }

    profile.data <- depthprofile.avail %>%
      dplyr::full_join(depth.params.avail, by = c(names(depthprofile.avail)))

    rm(depth.params.avail, depthprofile.avail)
  }


  # this subset must include all fields included in plot hover below
  plot.data <- profile.data %>%
    dplyr::filter(dplyr::if_any(TADA.ComparableDataIdentifier, ~ .x %in% groups)) %>%
    dplyr::select(dplyr::all_of(reqcols), "TADA.ComparableDataIdentifier", "ActivityStartDateTime", "MonitoringLocationName", "TADA.ActivityMediaName", "ActivityMediaSubdivisionName", "ActivityRelativeDepthName", "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText") %>%
    dplyr::mutate(TADA.ResultMeasure.MeasureUnitCode = ifelse(is.na(TADA.ResultMeasure.MeasureUnitCode),
      "NA", TADA.ResultMeasure.MeasureUnitCode
    ))

  rm(profile.data)

  # break into subsets for each parameter
  param1 <- plot.data %>%
    dplyr::filter(dplyr::if_any(TADA.ComparableDataIdentifier, ~ .x %in% groups[1]))

  param2 <- plot.data %>%
    dplyr::filter(dplyr::if_any(TADA.ComparableDataIdentifier, ~ .x %in% groups[2]))

  param3 <- plot.data %>%
    dplyr::filter(dplyr::if_any(TADA.ComparableDataIdentifier, ~ .x %in% groups[3]))

  # create title for figure, conditional on number of groups/characteristics selected

  # title for three characteristics
  if (length(groups) == 3) {
    title <- TADA_InsertBreaks(
      paste0(
        param1$TADA.CharacteristicName[1],
        "; ",
        param2$TADA.CharacteristicName[1],
        " and ",
        param3$TADA.CharacteristicName[1],
        " for ",
        plot.data$MonitoringLocationName[1],
        " on ",
        format(as.Date(plot.data$ActivityStartDate[1]), "%B %d, %Y")
      ),
      len = 50
    )
  }

  # title for two characteristics
  if (length(groups) == 2) {
    title <- TADA_InsertBreaks(
      paste0(
        param1$TADA.CharacteristicName[1],
        " and ",
        param2$TADA.CharacteristicName[1],
        " for ",
        # figure out addition of weird \n in name
        plot.data$MonitoringLocationName[1],
        " on ",
        format(as.Date(plot.data$ActivityStartDate[1]), "%B %d, %Y")
      ),
      len = 50
    )
  }

  # title for one characteristic
  if (length(groups) == 1) {
    title <- TADA_InsertBreaks(
      paste0(
        param1$TADA.CharacteristicName[1],
        " for ",
        # figure out addition of weird \n in name
        plot.data$MonitoringLocationName[1],
        " on ",
        format(as.Date(plot.data$ActivityStartDate[1]), "%B %d, %Y")
      ),
      len = 50
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
  xmax <- max(plot.data$TADA.ResultMeasureValue, na.rm = TRUE) + 0.5 * max(plot.data$TADA.ResultMeasureValue, na.rm = TRUE)
  xrange <- c(0, xmax)

  ymax <- max(plot.data$TADA.ConsolidatedDepth, na.rm = TRUE) + 0.1 * max(plot.data$TADA.ConsolidatedDepth, na.rm = TRUE)
  yrange <- c(0, ymax)

  # set palette
  tada.pal <- TADA_ColorPalette()

  # create base of scatter plot
  scatterplot <- plotly::plot_ly(type = "scatter", mode = "lines+markers") %>%
    plotly::layout(
      xaxis = list(
        # title = title.x,
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
        showgrid = FALSE, tickcolor = "black"
      ),
      yaxis = list(
        title = paste0("Depth", " (", param1$TADA.ConsolidatedDepth.Unit[1], ")"),
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
        showgrid = FALSE, tickcolor = "black",
        autorange = "reversed"
      ),
      hoverlabel = list(bgcolor = "white"),
      title = list(
        text = title,
        xref = "paper",
        x = 0.5
      ),
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
  if (length(groups) >= 1 & !param1$TADA.CharacteristicName[1] %in% depth.params) {
    # config options https://plotly.com/r/configuration-options/
    scatterplot <- scatterplot %>%
      plotly::config(displaylogo = FALSE) %>% # , displayModeBar = TRUE) # TRUE makes bar always visible
      plotly::add_trace(
        data = param1,
        x = ~TADA.ResultMeasureValue,
        y = ~TADA.ConsolidatedDepth,
        name = TADA_CharStringRemoveNA(paste0(
          param1$TADA.ResultSampleFractionText[1], " ",
          param1$TADA.CharacteristicName[1], " ",
          param1$TADA.MethodSpeciationName[1], " ",
          "(", param1$TADA.ResultMeasure.MeasureUnitCode[1], ")"
        )),
        marker = list(
          size = 10,
          color = tada.pal[10]
        ),
        line = list(color = tada.pal[5], width = 2),
        hoverinfo = "text",
        hovertext = paste(
          "Result:", paste0(param1$TADA.ResultMeasureValue, " ", param1$TADA.ResultMeasure.MeasureUnitCode), "<br>",
          "Activity Start Date:", param1$ActivityStartDate, "<br>",
          "Activity Start Date Time:", param1$ActivityStartDateTime, "<br>",
          "Depth:", paste0(
            param1$TADA.ConsolidatedDepth, " ",
            param1$TADA.ConsolidatedDepth.Unit
          ), "<br>",
          "Activity Relative Depth Name:", param1$ActivityRelativeDepthName, "<br>",
          "TADA.DepthCategory.Flag:", paste0(
            param1$TADA.DepthCategory.Flag
          ), "<br>"
        )
      )
  }

  # first parameter has a single value where units are depth
  if (length(groups) >= 1 & param1$TADA.CharacteristicName[1] %in% depth.params) {
    scatterplot <- scatterplot %>%
      plotly::add_lines(
        y = param1$TADA.ResultMeasureValue[1],
        x = xrange,
        name = TADA_CharStringRemoveNA(paste0(
          param1$TADA.ResultSampleFractionText[1], " ",
          param1$TADA.CharacteristicName[1], " ",
          param1$TADA.MethodSpeciationName[1], " ",
          "(", param1$TADA.ResultMeasure.MeasureUnitCode[1], ")"
        )),
        showlegend = TRUE,
        line = list(color = tada.pal[10], dash = "dash"),
        hoverinfo = "text",
        hovertext = paste(
          "Result:", paste0(param1$TADA.ResultMeasureValue, " ", param3$TADA.ResultMeasure.MeasureUnitCode), "<br>",
          "Activity Start Date:", param1$ActivityStartDate, "<br>",
          "Activity Start Date Time:", param1$ActivityStartDateTime, "<br>",
          "Depth:", paste0(
            param1$TADA.ConsolidatedDepth, " ",
            param1$TADA.ConsolidatedDepth.Unit
          ), "<br>",
          "Activity Relative Depth Name:", param1$ActivityRelativeDepthName, "<br>",
          "TADA.DepthCategory.Flag:", paste0(
            param1$TADA.DepthCategory.Flag
          ), "<br>"
        )
      )
  }

  # second parameter has a depth profile
  if (length(groups) >= 2 & !param2$TADA.CharacteristicName[1] %in% depth.params) {
    scatterplot <- scatterplot %>%
      plotly::add_trace(
        data = param2,
        x = ~TADA.ResultMeasureValue,
        y = ~TADA.ConsolidatedDepth,
        name = TADA_CharStringRemoveNA(paste0(
          param2$TADA.ResultSampleFractionText[1], " ",
          param2$TADA.CharacteristicName[1], " ",
          param2$TADA.MethodSpeciationName[1], " ",
          "(", param2$TADA.ResultMeasure.MeasureUnitCode[1], ")"
        )),
        marker = list(
          size = 10,
          color = tada.pal[12]
        ),
        line = list(color = tada.pal[3], width = 2),
        hoverinfo = "text",
        hovertext = paste(
          "Result:", paste0(param2$TADA.ResultMeasureValue, " ", param2$TADA.ResultMeasure.MeasureUnitCode), "<br>",
          "Activity Start Date:", param2$ActivityStartDate, "<br>",
          "Activity Start Date Time:", param2$ActivityStartDateTime, "<br>",
          "Depth:", paste0(
            param2$TADA.ConsolidatedDepth, " ",
            param2$TADA.ConsolidatedDepth.Unit
          ), "<br>",
          "Activity Relative Depth Name:", param2$ActivityRelativeDepthName, "<br>",
          "TADA.DepthCategory.Flag:", paste0(
            param2$TADA.DepthCategory.Flag
          ), "<br>"
        )
      )
  }

  # second parameter has a single value where units are depth
  if (length(groups) >= 2 & param2$TADA.CharacteristicName[1] %in% depth.params) {
    scatterplot <- scatterplot %>%
      plotly::add_lines(
        y = param2$TADA.ResultMeasureValue[1],
        x = xrange,
        name = TADA_CharStringRemoveNA(paste0(
          param2$TADA.ResultSampleFractionText[1], " ",
          param2$TADA.CharacteristicName[1], " ",
          param2$TADA.MethodSpeciationName[1], " ",
          "(", param2$TADA.ResultMeasure.MeasureUnitCode[1], ")"
        )),
        # inherit = FALSE,
        showlegend = TRUE,
        line = list(color = tada.pal[12], dash = "dash"),
        hoverinfo = "text",
        hovertext = ~ paste(
          "Result:", paste0(param2$TADA.ResultMeasureValue, " ", param2$TADA.ResultMeasure.MeasureUnitCode), "<br>",
          "Activity Start Date:", param2$ActivityStartDate, "<br>",
          "Activity Start Date Time:", param2$ActivityStartDateTime, "<br>",
          "Depth:", paste0(
            param2$TADA.ConsolidatedDepth, " ",
            param2$TADA.ConsolidatedDepth.Unit
          ), "<br>",
          "Activity Relative Depth Name:", param2$ActivityRelativeDepthName, "<br>",
          "TADA.DepthCategory.Flag:", paste0(
            param2$TADA.DepthCategory.Flag
          ), "<br>"
        )
      )
  }

  # third parameter has a depth profile
  if (length(groups) >= 3 & !param3$TADA.CharacteristicName[1] %in% depth.params) {
    scatterplot <- scatterplot %>%
      plotly::add_trace(
        data = param3,
        x = ~TADA.ResultMeasureValue,
        y = ~TADA.ConsolidatedDepth,
        name = TADA_CharStringRemoveNA(paste0(
          param3$TADA.ResultSampleFractionText[1], " ",
          param3$TADA.CharacteristicName[1], " ",
          param3$TADA.MethodSpeciationName[1], " ",
          "(", param3$TADA.ResultMeasure.MeasureUnitCode[1], ")"
        )),
        marker = list(
          size = 10,
          color = tada.pal[11]
        ),
        line = list(color = tada.pal[9], width = 2),
        hoverinfo = "text",
        hovertext = paste(
          "Result:", paste0(param3$TADA.ResultMeasureValue, " ", param2$TADA.ResultMeasure.MeasureUnitCode), "<br>",
          "Activity Start Date:", param3$ActivityStartDate, "<br>",
          "Activity Start Date Time:", param3$ActivityStartDateTime, "<br>",
          "Depth:", paste0(
            param3$TADA.ConsolidatedDepth, " ",
            param3$TADA.ConsolidatedDepth.Unit
          ), "<br>",
          "Activity Relative Depth Name:", param3$ActivityRelativeDepthName, "<br>",
          "TADA.DepthCategory.Flag:", paste0(
            param3$TADA.DepthCategory.Flag
          ), "<br>"
        )
      )
  }

  # third parameter has a single value where units are depth
  if (length(groups) >= 3 & param3$TADA.CharacteristicName[1] %in% depth.params) {
    scatterplot <- scatterplot %>%
      plotly::add_lines(
        y = param3$TADA.ResultMeasureValue[1],
        x = xrange,
        name = TADA_CharStringRemoveNA(paste0(
          param3$TADA.ResultSampleFractionText[1], " ",
          param3$TADA.CharacteristicName[1], " ",
          param3$TADA.MethodSpeciationName[1], " ",
          "(", param3$TADA.ResultMeasure.MeasureUnitCode[1], ")"
        )),
        # inherit = FALSE,
        showlegend = TRUE,
        line = list(color = tada.pal[11], dash = "dash"),
        hoverinfo = "text",
        hovertext = paste(
          "Result:", paste0(param3$TADA.ResultMeasureValue, " ", param3$TADA.ResultMeasure.MeasureUnitCode), "<br>",
          "Activity Start Date:", param3$ActivityStartDate, "<br>",
          "Activity Start Date Time:", param3$ActivityStartDateTime, "<br>",
          "Depth:", paste0(
            param3$TADA.ConsolidatedDepth, " ",
            param3$TADA.ConsolidatedDepth.Unit
          ), "<br>",
          "Activity Relative Depth Name:", param3$ActivityRelativeDepthName, "<br>",
          "TADA.DepthCategory.Flag:", paste0(
            param3$TADA.DepthCategory.Flag
          ), "<br>"
        )
      )
  }

  # add horizontal lines for depth profile category
  if (depthcat == TRUE & is.null(surfacevalue) & is.null(bottomvalue)) {
    stop("TADA_DepthProfilePlot: No depth categories can be determined when both surfacevalue and bottomvalue are null. Supply one or both of these values and run the function again.")
  }

  if ((depthcat == TRUE & !is.null(surfacevalue)) | (depthcat == TRUE & !is.null(bottomvalue))) {
    # create list to store depth annotation text
    depth_annotations <- list()

    # adjust margins of plot
    scatterplot <- scatterplot %>%
      plotly::layout(margin = list(
        l = 50,
        r = 100,
        b = 100,
        t = (25 + (ceiling(nchar(title) / 50)) * 25),
        pad = 0
      ))

    if (is.numeric(surfacevalue)) {
      print("TADA_DepthProfilePlot: Adding surface delination to figure.")

      # add surface line
      scatterplot <- scatterplot %>%
        plotly::add_lines(
          y = surfacevalue,
          x = xrange,
          inherit = FALSE,
          showlegend = FALSE,
          line = list(color = tada.pal[1]),
          hoverinfo = "text",
          hovertext = paste(surfacevalue, fig.depth.unit, sep = " ")
        )

      surface_text <-
        list(
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
      # find bottom depth
      bot.depth <- plot.data %>%
        dplyr::select(TADA.ConsolidatedDepth.Bottom) %>%
        unique() %>%
        dplyr::slice_max(TADA.ConsolidatedDepth.Bottom) %>%
        dplyr::pull()


      print("TADA_DepthProfilePlot: Adding bottom delination to figure.")

      scatterplot <- scatterplot %>%
        plotly::add_lines(
          y = bot.depth - bottomvalue,
          x = xrange,
          inherit = FALSE,
          showlegend = FALSE,
          line = list(color = tada.pal[1]),
          hoverinfo = "text",
          hovertext = paste(round((bot.depth - bottomvalue), digits = 1), fig.depth.unit, sep = " ")
        )


      bottom_text <-
        list(
          x = 1,
          y = (ymax + (bot.depth - bottomvalue)) / 2,
          xref = "paper",
          yref = "y",
          text = "Bottom",
          showarrow = F,
          align = "right",
          xanchor = "left",
          yanchor = "center"
        )

      depth_annotations <- append(depth_annotations, list(bottom_text))
    }

    if (is.numeric(surfacevalue) & is.numeric(bottomvalue)) {
      middle_text <-
        list(
          x = 1,
          y = (surfacevalue + (bot.depth - bottomvalue)) / 2,
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

    scatterplot <- scatterplot %>%
      plotly::layout(annotations = depth_annotations)
  }


  # return plot with no depth profile category
  if (depthcat == FALSE) {
    scatterplot <- scatterplot
  }
  return(scatterplot)
}
