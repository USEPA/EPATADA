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
#' Process a TADA profile object to flag or filter data based on media type.
#' - If `clean = FALSE`, adds `TADA.Media.Flag` indicating the classified media for each row.
#' - If `clean = TRUE`, removes rows where the media type is set to `TRUE` via function arguments,
#'   and does not add `TADA.Media.Flag` to the output. A warning is issued if all media toggles are `TRUE`
#'   (which would remove all media types), and if the filter removes all rows.
#'
#' The function utilizes various columns including `MonitoringLocationTypeName`,
#' `ActivityMediaName`, `ActivityMediaSubdivisionName`, `AquiferName`,
#' `LocalAqfrName`, `ConstructionDateText`, `WellDepthMeasure.MeasureValue`,
#' `WellDepthMeasure.MeasureUnitCode`, `WellHoleDepthMeasure.MeasureValue`, and
#' `WellHoleDepthMeasure.MeasureUnitCode`, and others to determine the media type.
#' Users can specify which media types (surface water, groundwater, sediment, other)
#' should be included or excluded.
#'
#' Media classification uses `MonitoringLocationTypeName` (joined to the reference table's `Name`)
#' along with `ActivityMediaSubdivisionName`, `ActivityMediaName`, and groundwater-related fields.
#' Certain media values are normalized to `OTHER` (HABITAT, empty string, AIR, BIOLOGICAL,
#' and any non-core value).
#'
#' @param .data A data frame representing a TADA profile object.
#' @param clean Logical. If `TRUE`, remove rows according to the media toggles. If `FALSE`, only flag media. Default `FALSE`.
#' @param surface_water Logical (used only when `clean = TRUE`). If `TRUE`, remove SURFACE WATER results. Default `FALSE`.
#' @param ground_water Logical (used only when `clean = TRUE`). If `TRUE`, remove GROUNDWATER results. Default `FALSE`.
#' @param sediment Logical (used only when `clean = TRUE`). If `TRUE`, remove SEDIMENT results. Default `FALSE`.
#' @param other Logical (used only when `clean = TRUE`). If `TRUE`, remove OTHER results. Default `FALSE`.
#'
#' @return A data frame.
#' - If `clean = FALSE`, returns all rows with `TADA.Media.Flag` added.
#' - If `clean = TRUE`, returns rows with selected media removed and no flag columns added.
#'
#' @export
#'
#' @examples
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Example 1: Do not clean; just classify media and add TADA.Media.Flag
#' Data_Flag <- TADA_MediaFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = FALSE
#' )
#' unique(Data_Flag$TADA.Media.Flag)
#'
#' # Example 2: Clean the data by removing groundwater and sediment; no flag column returned
#' Data_Clean1 <- TADA_MediaFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = TRUE,
#'   ground_water = TRUE,
#'   sediment = TRUE
#' )
#' "TADA.Media.Flag" %in% names(Data_Clean1) # should be FALSE
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

  # Required columns (MonitoringLocationTypeName must exist)
  required_columns <- c(
    "ActivityMediaSubdivisionName",
    "AquiferName",
    "MonitoringLocationTypeName"
  )
  missing_columns <- setdiff(required_columns, names(.data))
  if (length(missing_columns) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    ))
  }

  # Ensure optional column exists to avoid downstream errors
  if (!"ActivityMediaName" %in% names(.data)) {
    .data$ActivityMediaName <- NA_character_
  }

  # Read the monitoring location reference table
  monitoring_location_types <- TADA_GetMonLocTypeRef()

  # Standardize names for robust detection
  std_names <- tolower(gsub("[^a-z.]", "", names(monitoring_location_types)))

  # Exact matches: "Name" and "TADA.Media.Flag"
  idx_name <- match("name", std_names)
  idx_flag <- match("tada.media.flag", std_names)

  if (!is.na(idx_name)) {
    names(monitoring_location_types)[idx_name] <- "Name"
    monitoring_location_types <- monitoring_location_types |>
      dplyr::mutate(Name = toupper(Name))
  } else {
    has_ref <- FALSE
  }

  if (!is.na(idx_flag)) {
    names(monitoring_location_types)[idx_flag] <- "Ref.TADA.Media.Flag"
  } else {
    monitoring_location_types$Ref.TADA.Media.Flag <- NA_character_
  }

  # Uppercase ML type name in .data for reliable joining
  .data <- .data |>
    dplyr::mutate(
      MonitoringLocationTypeName = toupper(MonitoringLocationTypeName)
    )

  # Build a groundwater indicator from any available groundwater-related fields
  gw_cols <- c(
    "AquiferName",
    "AquiferTypeName",
    "LocalAqfrName",
    "ConstructionDateText",
    "WellDepthMeasure.MeasureValue",
    "WellDepthMeasure.MeasureUnitCode",
    "WellHoleDepthMeasure.MeasureValue",
    "WellHoleDepthMeasure.MeasureUnitCode"
  )
  present_gw_cols <- intersect(gw_cols, names(.data))
  if (length(present_gw_cols) > 0) {
    gw_has_fields <- apply(
      .data[, present_gw_cols, drop = FALSE],
      1,
      function(row) any(!is.na(row))
    )
  } else {
    gw_has_fields <- rep(FALSE, nrow(.data))
  }
  .data$gw_has_fields <- gw_has_fields

  # Classify media from data columns
  .data <- .data |>
    dplyr::mutate(
      TADA.Media.Flag = dplyr::case_when(
        # If subdivision is missing and media name is WATER (and not groundwater), treat as SURFACE WATER
        (is.na(ActivityMediaSubdivisionName) |
          !nzchar(trimws(ActivityMediaSubdivisionName))) &
          !is.na(ActivityMediaName) &
          nzchar(trimws(ActivityMediaName)) &
          tolower(trimws(ActivityMediaName)) == "water" &
          !gw_has_fields ~ "SURFACE WATER",
        ActivityMediaSubdivisionName == "Groundwater" |
          gw_has_fields ~ "GROUNDWATER",
        ActivityMediaSubdivisionName == "Surface Water" ~ "SURFACE WATER",
        ActivityMediaSubdivisionName == "Sediment" ~ "SEDIMENT",

        # Keep any non-"water" ActivityMediaName as-is (uppercased)
        !is.na(ActivityMediaName) &
          nzchar(trimws(ActivityMediaName)) &
          tolower(trimws(ActivityMediaName)) != "water" ~ toupper(trimws(
          ActivityMediaName
        )),
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
        TADA.Media.Flag = dplyr::coalesce(
          TADA.Media.Flag,
          Ref.TADA.Media.Flag,
          "OTHER"
        )
      ) |>
      dplyr::select(-dplyr::any_of(c("Ref.TADA.Media.Flag")))
  } else {
    .data <- .data |>
      dplyr::mutate(TADA.Media.Flag = dplyr::coalesce(TADA.Media.Flag, "OTHER"))
  }

  # Normalize TADA.Media.Flag
  core_flags <- c("SURFACE WATER", "GROUNDWATER", "SEDIMENT", "OTHER")
  .data <- .data |>
    dplyr::mutate(
      TADA.Media.Flag = toupper(trimws(TADA.Media.Flag)),
      TADA.Media.Flag = dplyr::case_when(
        is.na(TADA.Media.Flag) ~ "OTHER",
        TADA.Media.Flag == "" ~ "OTHER",
        TADA.Media.Flag %in% c("HABITAT", "AIR", "BIOLOGICAL") ~ "OTHER",
        !(TADA.Media.Flag %in% core_flags) ~ "OTHER",
        TRUE ~ TADA.Media.Flag
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

    # Inform if no toggles are set (clean requested but nothing to remove)
    if (length(remove_media) == 0) {
      message(
        "TADA_MediaFilter: No media types selected for removal (all toggles are FALSE). Returning original data without TADA.Media.Flag."
      )
    }

    # Remove requested media and drop flag/helper columns
    .data <- .data |> dplyr::filter(!(TADA.Media.Flag %in% remove_media))

    # Warn if all rows were removed, except when all toggles were TRUE (pre-flight warning already emitted)
    if (nrow(.data) == 0 && !all_selected) {
      warning(
        "TADA_MediaFilter: All rows were removed by the media filter.",
        call. = FALSE
      )
    }

    # Build a readable list of which media types were set to TRUE
    removed_types_str <- if (length(remove_media) > 0) {
      paste(remove_media, collapse = ", ")
    } else {
      "none"
    }

    .data <- .data |>
      dplyr::select(-dplyr::any_of(c("TADA.Media.Flag", "gw_has_fields"))) |>
      TADA_OrderCols()

    message(sprintf(
      "TADA_MediaFilter: Removed media types: %s. Returning cleaned data without flag columns.",
      removed_types_str
    ))
  } else {
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
