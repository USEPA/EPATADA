#' Unique Field Values Table
#'
#' Function creates a table yielding the number of unique values in each field (column) returned.
#' The first column of the summary table holds the field name and the second column holds the
#' unique value count.
#'
#' @param .data TADA dataframe
#' @param display A character string denoting what fields to return in the summary table. Defaults to "key". "all" will return all fields in the dataset, "most" will return most field names except those holding numeric values or units, and "key" returns the most important columns to review. Note that if a field is completely NA, it will not be shown on the summary table.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A summary table yielding the number of unique values in each field.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#' # Count table of key fields in Data_Nutrients_UT dataset
#' fieldCountUT <- TADA_FieldCounts(Data_Nutrients_UT)
#' # Count table of most fields in Data_Nutrients_UT, filtered to only
#' # AMMONIA results.
#' fieldCountUTAmmonia <- TADA_FieldCounts(Data_Nutrients_UT,
#'   display = "most", characteristicName = "AMMONIA"
#' )
TADA_FieldCounts <- function(
  .data,
  display = c("key", "most", "all"),
  characteristicName = "null"
) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  display <- match.arg(display)

  # filter to characteristic if provided
  if (!characteristicName %in% c("null")) {
    .data <- subset(
      .data,
      .data$TADA.CharacteristicName %in% c(characteristicName)
    )

    if (dim(.data)[1] < 1) {
      stop(
        "Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization of synonyms from different WQP data providers (USGS's NWIS and EPA's WQX)."
      )
    }
  }

  # remove fields with only NAs from df
  df <- .data |> dplyr::select(where(~ !all(is.na(.x))))

  if (display == "key") {
    cols <- c(
      "TADA.ActivityType.Flag",
      "TADA.Media.Flag",
      "TADA.ActivityMediaName",
      "ActivityMediaSubdivisionName",
      "TADA.MonitoringLocationTypeName",
      "OrganizationFormalName",
      "TADA.CharacteristicName",
      "SubjectTaxonomicName",
      "TADA.MeasureQualifierCode.Def",
      "HydrologicCondition",
      "HydrologicEvent",
      "BiologicalIntentName",
      "AssemblageSampledName",
      "CharacteristicNameUserSupplied",
      "SampleTissueAnatomyName",
      "CharacteristicNameUserSupplied",
      "TADA.ComparableDataIdentifier",
      "ActivityRelativeDepthName",
      "ResultStatusIdentifier",
      "ResultValueTypeName"
    )
  }
  if (display == "most") {
    cols <- c(
      "ActivityTypeCode",
      "TADA.ActivityType.Flag",
      "TADA.Media.Flag",
      "ActivityGroup",
      "OrganizationIdentifier",
      "OrganizationFormalName",
      "ActivityTypeCode",
      "ActivityMediaSubdivisionName",
      "ActivityRelativeDepthName",
      "ProjectIdentifier",
      "ProjectName",
      "TADA.MonitoringLocationIdentifier",
      "MonitoringLocationName",
      "ActivityCommentText",
      "SampleAquifer",
      "HydrologicCondition",
      "HydrologicEvent",
      "SampleCollectionMethod.MethodIdentifier",
      "SampleCollectionMethod.MethodIdentifierContext",
      "SampleCollectionMethod.MethodName",
      "SampleCollectionMethod.MethodDescriptionText",
      "SampleCollectionEquipmentName",
      "ResultDetectionConditionText",
      "ResultStatusIdentifier",
      "ResultValueTypeName",
      "DataQuality.PrecisionValue",
      "DataQuality.BiasValue",
      "DataQuality.ConfidenceIntervalValue",
      "DataQuality.UpperConfidenceLimitValue",
      "DataQuality.LowerConfidenceLimitValue",
      "ResultCommentText",
      "USGSPCode",
      "SubjectTaxonomicName",
      "SampleTissueAnatomyName",
      "ResultAnalyticalMethod.MethodIdentifier",
      "ResultAnalyticalMethod.MethodIdentifierContext",
      "ResultAnalyticalMethod.MethodName",
      "ResultAnalyticalMethod.MethodDescriptionText",
      "LaboratoryName",
      "ResultLaboratoryCommentText",
      "DetectionQuantitationLimitTypeName",
      "ProviderName",
      "MonitoringLocationDescriptionText",
      "HUCEightDigitCode",
      "HorizontalCoordinateReferenceSystemDatumName",
      "VerticalCoordinateReferenceSystemDatumName",
      "AquiferName",
      "LocalAqfrName",
      "FormationTypeText",
      "AquiferTypeName",
      "ProjectDescriptionText",
      "SamplingDesignTypeCode",
      "QAPPApprovalAgencyName",
      "TADA.ActivityMediaName",
      "TADA.CharacteristicName",
      "CharacteristicNameUserSupplied",
      "TADA.MeasureQualifierCode.Def",
      "TADA.MethodSpeciationName",
      "TADA.ResultSampleFractionText",
      "TADA.ComparableDataIdentifier",
      "TADA.MonitoringLocationTypeName",
      "AssemblageSampledName",
      "BiologicalIntentName"
    )
  }
  if (display == "all") {
    cols <- names(df)
  }

  df <- df |> dplyr::select(dplyr::contains(cols))

  # CREATE LIST OF FIELDS
  # Find count of unique values in each column
  col.names <- data.frame(Count = apply(df, 2, function(x) length(unique(x))))
  # Create "Fields" column from row names
  col.names$Fields <- row.names(col.names)
  # Remove row names
  row.names(col.names) <- NULL
  # Reorder columns
  col.names <- col.names[, c(2, 1)]

  # Reorder Count column in col.names from largest to smallest number
  col.names <- col.names |> dplyr::arrange(desc(Count))

  return(col.names)
}

#' Field Values Summary Table
#'
#' Function creates a dataframe containing the relative proportions of values in a given field in a TADA dataset.
#'
#' @param .data TADA dataframe
#' @param field The field (column) the user would like to see represented in a pie chart.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A summary dataframe.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#'
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter:
#' TADA_FieldValuesTable(Data_Nutrients_UT, field = "TADA.CharacteristicName")
#'
TADA_FieldValuesTable <- function(
  .data,
  field = "null",
  characteristicName = "null"
) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  if (!field %in% names(.data)) {
    stop(
      "Field input does not exist in dataset. Please populate the 'field' argument with a valid field name. Enter ?TADA_FieldValuesTable in console for more information."
    )
  }

  # change NAs to "NA" (character string)
  .data[[field]][is.na(.data[[field]])] <- "NA"

  # filter to characteristic if provided
  if (!characteristicName %in% c("null")) {
    .data <- .data |>
      dplyr::filter(TADA.CharacteristicName %in% characteristicName)
    if (dim(.data)[1] < 1) {
      stop(
        "Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization."
      )
    }
  }

  dat <- as.data.frame(table(.data[, field]))
  names(dat) <- c("Value", "Count")
  dat <- dat |> dplyr::arrange(desc(Count))
  return(dat)
}

#' TADA_MediaFilter
#'
#' Process a TADA dataframe to flag or filter data by media type.
#'
#' Behavior overview:
#' - If `clean = FALSE`, classify each row and add `TADA.Media.Flag`. The function
#'   prints a single message with counts by media and returns the original data with the flag.
#' - If `clean = TRUE`, classify and then remove rows whose media types are selected
#'   via function toggles. The function prints a single message with counts by media
#'   before filtering, and then a single summary message:
#'   - When no toggles are selected (all FALSE): informs the user and returns the original
#'     data without `TADA.Media.Flag`.
#'   - When some toggles are selected but no rows match (e.g., data already contain only SURFACE WATER):
#'     informs the user that nothing was removed and returns the data unchanged (without flag columns).
#'   - When rows are removed: reports how many rows were removed and returns cleaned data
#'     (without `TADA.Media.Flag`).
#'   Additionally, a warning is issued if all media toggles are `TRUE` (which would remove
#'   all media types), and if the filter removes all rows.
#'
#' Inputs used for classification:
#' - `MonitoringLocationTypeName`, `ActivityMediaSubdivisionName` and `ActivityMediaName`
#' - Groundwater-related fields: `AquiferName`, `AquiferTypeName`, `LocalAqfrName`,
#'   `ConstructionDateText`, `WellDepthMeasure.MeasureValue`, `WellDepthMeasure.MeasureUnitCode`,
#'   `WellHoleDepthMeasure.MeasureValue`, `WellHoleDepthMeasure.MeasureUnitCode`
#'
#' Classification details:
#' - ActivityMediaName of "Soil", "Sediment" (and common variants like "Soil or Sediment")
#'   map to `SEDIMENT` even if groundwater fields are present.
#' - `ActivityMediaSubdivisionName` is reviewed for identifying "Surface Water",
#'   "Groundwater", and "Sediment".
#' - If the subdivision is blank and `ActivityMediaName` is "Water" with no groundwater
#'   fields present, the row is classified as `SURFACE WATER`.
#' - Groundwater fields are considered present if they are non-NA and non-blank (for
#'   character/factor fields) or non-NA (for numeric fields).
#' - If a monitoring location type reference is available via `TADA_GetMonLocTypeRef()`,
#'   its `TADA.Media.Flag` takes precedence when present.
#' - Media flags are normalized to the core set: `SURFACE WATER`, `GROUNDWATER`,
#'   `SEDIMENT`, `OTHER`. Values such as `HABITAT`, `AIR`, `BIOLOGICAL`, empty strings,
#'   or non-core values are coerced to `OTHER`.
#'
#' Requirements and defaults:
#' - Required columns: `MonitoringLocationTypeName`, `ActivityMediaSubdivisionName`, and `AquiferName`.
#'   If any required columns are missing (rare), the function stops with an error of the form
#'   "Missing required columns: <col1>, <col2>, ...".
#' - Optional columns are created if missing (with appropriate types) and filled with `NA`:
#'   `ActivityMediaName`, `AquiferTypeName`, `LocalAqfrName`, `ConstructionDateText`,
#'   `WellDepthMeasure.MeasureValue` (numeric), `WellDepthMeasure.MeasureUnitCode`,
#'   `WellHoleDepthMeasure.MeasureValue` (numeric), `WellHoleDepthMeasure.MeasureUnitCode`.
#' - If the input data frame has 0 rows, the function emits a message and returns `NULL`.
#'
#' @param .data A data frame representing a TADA profile object.
#' @param clean Logical. If `TRUE`, remove rows according to the media toggles and return
#'   data without flag columns. If `FALSE`, only flag media and return all rows with
#'   `TADA.Media.Flag`. Default `FALSE`.
#' @param surface_water Logical (used only when `clean = TRUE`). If `TRUE`, remove `SURFACE WATER` results. Default `FALSE`.
#' @param ground_water Logical (used only when `clean = TRUE`). If `TRUE`, remove `GROUNDWATER` results. Default `FALSE`.
#' @param sediment Logical (used only when `clean = TRUE`). If `TRUE`, remove `SEDIMENT` results. Default `FALSE`.
#' @param other Logical (used only when `clean = TRUE`). If `TRUE`, remove `OTHER` results. Default `FALSE`.
#'
#' @return A data frame.
#' - If `clean = FALSE`, returns all rows with the column `TADA.Media.Flag` added.
#' - If `clean = TRUE`, returns rows with the selected media removed and no flag columns added.
#' - If the input has 0 rows, returns `NULL`.
#'
#' @section Messages and warnings:
#' - Always prints counts by media before filtering.
#' - When `clean = TRUE`, prints a single summary message indicating whether rows were removed,
#'   whether no rows matched the selected media, or whether no toggles were selected.
#' - Warns if all media toggles are `TRUE`, and if all rows were removed by the filter.
#'
#' @seealso [TADA_GetMonLocTypeRef()], [TADA_OrderCols()]
#'
#' @export
#'
#' @examples
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Example 1: Do not clean; classify media and add TADA.Media.Flag
#' Data_Flag <- TADA_MediaFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = FALSE
#' )
#' unique(Data_Flag$TADA.Media.Flag)
#'
#' # Example 2: Remove groundwater and sediment; no flag column in output
#' Data_Clean1 <- TADA_MediaFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = TRUE,
#'   ground_water = TRUE,
#'   sediment = TRUE
#' )
#' "TADA.Media.Flag" %in% names(Data_Clean1) # FALSE
#'
#' # Example 3: Keep only surface water by removing groundwater, sediment, and other
#' Data_Clean2 <- TADA_MediaFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = TRUE,
#'   ground_water = TRUE,
#'   sediment = TRUE,
#'   other = TRUE
#' )
#'
#' # Example 4: Remove surface water only
#' Data_Clean3 <- TADA_MediaFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = TRUE,
#'   surface_water = TRUE
#' )
TADA_MediaFilter <- function(
  .data,
  clean = FALSE,
  surface_water = FALSE,
  ground_water = FALSE,
  sediment = FALSE,
  other = FALSE
) {
  # Validate input
  if (!is.data.frame(.data)) {
    stop("Input object must be a data frame.")
  }
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)
  }

  # Require only MonitoringLocationTypeName; other fields are optional and will be created if missing
  required_columns <- c(
    "MonitoringLocationTypeName",
    "AquiferName",
    "ActivityMediaSubdivisionName"
  )
  missing_required <- setdiff(required_columns, names(.data))
  if (length(missing_required) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_required, collapse = ", ")
    ))
  }

  # Ensure optional columns exist to avoid downstream errors
  # Split aquifer metadata (do NOT imply groundwater) from true well/GW fields
  aquifer_meta_cols <- c("AquiferName", "AquiferTypeName", "LocalAqfrName")
  gw_field_cols <- c(
    "ConstructionDateText",
    "WellDepthMeasure.MeasureValue",
    "WellDepthMeasure.MeasureUnitCode",
    "WellHoleDepthMeasure.MeasureValue",
    "WellHoleDepthMeasure.MeasureUnitCode"
  )

  optional_cols <- c(
    "ActivityMediaSubdivisionName",
    "ActivityMediaName",
    aquifer_meta_cols,
    gw_field_cols
  )
  for (col in setdiff(optional_cols, names(.data))) {
    .data[[col]] <- NA_character_
  }

  # Read the monitoring location reference table
  monitoring_location_types <- TADA_GetMonLocTypeRef()

  # Initialize has_ref and standardize names for robust detection
  has_ref <- FALSE
  std_names <- tolower(gsub("[^a-z.]", "", names(monitoring_location_types)))

  # Exact matches: "Name" and "TADA.Media.Flag" (after normalization)
  idx_name <- match("name", std_names)
  idx_flag <- match("tada.media.flag", std_names)

  if (!is.na(idx_name)) {
    names(monitoring_location_types)[idx_name] <- "Name"
    monitoring_location_types <- monitoring_location_types |>
      dplyr::mutate(Name = toupper(.data$Name))
    has_ref <- TRUE
  } else {
    has_ref <- FALSE
  }

  if (!is.na(idx_flag)) {
    names(monitoring_location_types)[idx_flag] <- "Ref.TADA.Media.Flag"
  } else if (has_ref) {
    monitoring_location_types$Ref.TADA.Media.Flag <- NA_character_
  }

  # Uppercase ML type name in .data for reliable joining
  .data <- .data |>
    dplyr::mutate(
      MonitoringLocationTypeName = toupper(.data$MonitoringLocationTypeName)
    )

  # Build a groundwater indicator from true well/GW fields only (exclude aquifer metadata)
  present_gw_field_cols <- intersect(gw_field_cols, names(.data))
  if (length(present_gw_field_cols) > 0) {
    gw_logicals <- lapply(present_gw_field_cols, function(col) {
      x <- .data[[col]]
      if (is.factor(x)) {
        x <- as.character(x)
      }
      if (is.character(x)) {
        !is.na(x) & nzchar(trimws(x))
      } else {
        !is.na(x)
      }
    })
    gw_has_fields <- Reduce(`|`, gw_logicals, init = rep(FALSE, nrow(.data)))
  } else {
    gw_has_fields <- rep(FALSE, nrow(.data))
  }
  .data$gw_has_fields <- gw_has_fields

  # Precompute normalized media/subdivision for robust comparisons
  ams <- tolower(trimws(as.character(.data$ActivityMediaSubdivisionName)))
  am <- tolower(trimws(as.character(.data$ActivityMediaName)))
  ams_blank <- is.na(ams) | !nzchar(ams)
  am_blank <- is.na(am) | !nzchar(am)

  # Classify media from data columns
  # Classify media from data columns
  .data <- .data |>
    dplyr::mutate(
      TADA.Media.Flag = dplyr::case_when(
        # 1) Sediment by media name ...
        !am_blank &
          am %in%
            c(
              "sediment",
              "soil",
              "soil or sediment",
              "soil/sediment",
              "soil-sediment"
            ) ~ "SEDIMENT",
        # 2) Sediment by subdivision
        ams == "sediment" ~ "SEDIMENT",

        # 3) Groundwater by subdivision, well/GW fields, or WELL location type (override any SW subdivision)
        ams == "groundwater" |
          gw_has_fields |
          grepl("\\bWELL\\b", .data$MonitoringLocationTypeName) ~ "GROUNDWATER",

        # 4) Explicit surface water subdivision
        ams == "surface water" ~ "SURFACE WATER",

        # 5) Water heuristic (no GW fields) -> SURFACE WATER
        ams_blank &
          !am_blank &
          am == "water" &
          !gw_has_fields ~ "SURFACE WATER",

        # 6) Keep other ActivityMediaName as-is
        !am_blank & am != "water" ~ toupper(trimws(as.character(
          .data$ActivityMediaName
        ))),
        TRUE ~ NA_character_
      )
    )

  # Join with reference (if available) and coalesce media flag
  if (isTRUE(has_ref)) {
    .data <- .data |>
      dplyr::left_join(
        monitoring_location_types,
        by = c("MonitoringLocationTypeName" = "Name")
      ) |>
      dplyr::mutate(
        # Prefer computed classification; use reference as fallback
        TADA.Media.Flag = dplyr::coalesce(
          .data$TADA.Media.Flag,
          .data$Ref.TADA.Media.Flag,
          "OTHER"
        )
      ) |>
      dplyr::select(-dplyr::any_of(c("Ref.TADA.Media.Flag")))
  } else {
    .data <- .data |>
      dplyr::mutate(
        TADA.Media.Flag = dplyr::coalesce(.data$TADA.Media.Flag, "OTHER")
      )
  }

  # Normalize TADA.Media.Flag to core values
  core_flags <- c("SURFACE WATER", "GROUNDWATER", "SEDIMENT", "OTHER")
  .data <- .data |>
    dplyr::mutate(
      TADA.Media.Flag = toupper(trimws(.data$TADA.Media.Flag)),
      TADA.Media.Flag = dplyr::case_when(
        is.na(.data$TADA.Media.Flag) ~ "OTHER",
        .data$TADA.Media.Flag == "" ~ "OTHER",
        .data$TADA.Media.Flag %in% c("HABITAT", "AIR", "BIOLOGICAL") ~ "OTHER",
        !(.data$TADA.Media.Flag %in% core_flags) ~ "OTHER",
        TRUE ~ .data$TADA.Media.Flag
      )
    )

  # Aquifer-only classification: aquifer metadata present, no well/GW fields, and not a well location
  aquifer_meta_present <- Reduce(
    `|`,
    lapply(intersect(aquifer_meta_cols, names(.data)), function(col) {
      x <- .data[[col]]
      if (is.factor(x)) {
        x <- as.character(x)
      }
      if (is.character(x)) !is.na(x) & nzchar(trimws(x)) else !is.na(x)
    }),
    init = rep(FALSE, nrow(.data))
  )

  non_well_loc <- !grepl(
    "\\bWELL\\b",
    .data$MonitoringLocationTypeName,
    ignore.case = TRUE
  )

  idx_aquifer_only <- aquifer_meta_present & !.data$gw_has_fields & non_well_loc
  .data <- .data |>
    dplyr::mutate(
      TADA.Media.Flag = dplyr::case_when(
        idx_aquifer_only &
          (is.na(.data$TADA.Media.Flag) |
            .data$TADA.Media.Flag == "" |
            .data$TADA.Media.Flag == "SURFACE WATER") ~ "OTHER",
        TRUE ~ .data$TADA.Media.Flag
      )
    )

  # Build removal set based on arguments (used only when clean = TRUE)
  remove_media <- c(
    if (isTRUE(surface_water)) "SURFACE WATER",
    if (isTRUE(ground_water)) "GROUNDWATER",
    if (isTRUE(sediment)) "SEDIMENT",
    if (isTRUE(other)) "OTHER"
  )

  if (clean) {
    # Pre-flight guard: warn when all media toggles are TRUE
    all_selected <- isTRUE(surface_water) &&
      isTRUE(ground_water) &&
      isTRUE(sediment) &&
      isTRUE(other)

    if (all_selected) {
      warning("All media types are selected for removal", call. = FALSE)
    }

    # Helpers for robust counts (always return a 4-length named vector)
    count_by_core <- function(v) {
      f <- factor(v, levels = core_flags)
      stats::setNames(
        tabulate(as.integer(f), nbins = length(core_flags)),
        core_flags
      )
    }
    format_counts <- function(cnts) {
      paste(
        sprintf("%s: %s", names(cnts), format(cnts, big.mark = ",")),
        collapse = " | "
      )
    }

    # Counts BEFORE filtering
    flags_before <- .data$TADA.Media.Flag
    counts_pre <- count_by_core(flags_before)
    message(sprintf(
      "TADA_MediaFilter: Counts by media (before filter) - %s",
      format_counts(counts_pre)
    ))

    # Determine rows to remove and perform removal
    removed_idx <- flags_before %in% remove_media
    n_remove <- sum(removed_idx)
    .data <- .data[!removed_idx, , drop = FALSE]

    # Warn if all rows were removed, except when all toggles were TRUE
    if (nrow(.data) == 0 && !all_selected) {
      warning(
        "TADA_MediaFilter: All rows were removed by the media filter.",
        call. = FALSE
      )
    }

    # Drop flag/helper columns and order
    .data <- .data |>
      dplyr::select(-dplyr::any_of(c("TADA.Media.Flag", "gw_has_fields"))) |>
      TADA_OrderCols()

    # Final single summary message
    if (length(remove_media) == 0) {
      message(
        "TADA_MediaFilter: No media types selected for removal (all toggles are FALSE). Returning original data without TADA.Media.Flag."
      )
    } else if (n_remove == 0) {
      message(sprintf(
        "TADA_MediaFilter: No rows matched the selected media types for removal (%s). Returning data unchanged (without flag columns).",
        paste(remove_media, collapse = ", ")
      ))
    } else {
      message(sprintf(
        "TADA_MediaFilter: Removed %s rows matching media types: %s. Returning cleaned data without flag columns.",
        format(n_remove, big.mark = ","),
        paste(remove_media, collapse = ", ")
      ))
    }
  } else {
    # Counts when not cleaning
    count_by_core <- function(v) {
      f <- factor(v, levels = core_flags)
      stats::setNames(
        tabulate(as.integer(f), nbins = length(core_flags)),
        core_flags
      )
    }
    format_counts <- function(cnts) {
      paste(
        sprintf("%s: %s", names(cnts), format(cnts, big.mark = ",")),
        collapse = " | "
      )
    }
    counts <- count_by_core(.data$TADA.Media.Flag)
    message(sprintf(
      "TADA_MediaFilter: Counts by media - %s",
      format_counts(counts)
    ))

    # Do not clean; keep flag and drop helper
    .data <- .data |>
      dplyr::select(-dplyr::any_of("gw_has_fields")) |>
      TADA_OrderCols()

    message(
      "TADA_MediaFilter: Returning all results with TADA.Media.Flag; media toggles ignored because clean = FALSE."
    )
  }

  return(.data)
}
