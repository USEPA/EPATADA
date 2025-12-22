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

  # run required flagging/cleaning functions
  if ("TADA.UseForAnalysis.Flag" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create TADA.UseForAnalysis.Flag
    .data <- TADA_AnalysisDataFilter(.data)
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
      "TADA.UseForAnalysis.Flag",
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
      "TADA.UseForAnalysis.Flag",
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

#' TADA_AnalysisDataFilter
#'
#' This function processes a TADA profile object to flag or filter data based on media type.
#' By default (`clean = FALSE`), it adds two columns: `TADA.UseForAnalysis.Flag` and 
#' `TADA.Media.Flag`. The `TADA.UseForAnalysis.Flag` indicates whether each row should be included 
#' in the analysis along with the media type. If `clean = TRUE`, the function filters out rows 
#' not suitable for analysis, and these columns will not be added.
#'
#' The function utilizes various columns including `MonitoringLocationTypeName`, 
#' `ActivityMediaName`, `ActivityMediaSubdivisionName`, `AquiferName`,
#' `LocalAqfrName`, `ConstructionDateText`, `WellDepthMeasure.MeasureValue`,
#' `WellDepthMeasure.MeasureUnitCode`, `WellHoleDepthMeasure.MeasureValue`, and
#' `WellHoleDepthMeasure.MeasureUnitCode`, and others to determine the media type. 
#' Users can specify which media types (surface water, groundwater, sediment, other) 
#' should be included or excluded.
#'
#' @param .data A data frame representing a TADA profile object.
#' @param clean Logical. If `TRUE`, removes rows that are not flagged for inclusion Default is `FALSE`.
#' @param surface_water Logical. If `TRUE`, surface water results are flagged for inclusion. Default is `TRUE`.
#' @param ground_water Logical. If `TRUE`, groundwater results are flagged for inclusion. Default is `FALSE`.
#' @param sediment Logical. If `TRUE`, sediment results are flagged for inclusion. Default is `FALSE`.
#' @param other Logical. If `TRUE`, "other" results are flagged for inclusion. Default is `TRUE`.
#'
#' @return A data frame. If `clean = TRUE`, only rows flagged for inclusion are returned. 
#' If `clean = FALSE`, all rows are returned with additional `TADA.UseForAnalysis.Flag` and 
#' `TADA.Media.Flag` columns indicating the media type and inclusion status.
#'
#' @export
#'
#' @examples
#' utils::data(Data_R5_TADAPackageDemo)
#' 
#' # Example 1: Retain only surface water results without adding flag columns
#' Data_Assessment1 <- TADA_AnalysisDataFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = TRUE,
#'   surface_water = TRUE, 
#'   ground_water = FALSE, 
#'   sediment = FALSE,
#'   other = TRUE
#' )
#' 
#' # View unique values in TADA.UseForAnalysis.Flag to understand inclusion criteria
#' unique(Data_Assessment1$TADA.UseForAnalysis.Flag)
#' 
#' # View unique values in TADA.Media.Flag to see media type classification
#' unique(Data_Assessment1$TADA.Media.Flag)
#' 
#' # Example 2: Flag surface water results for analysis and include flag columns
#' Data_Assessment2 <- TADA_AnalysisDataFilter(
#'   Data_R5_TADAPackageDemo,
#'   clean = FALSE,
#'   surface_water = TRUE, 
#'   ground_water = FALSE, 
#'   sediment = FALSE,
#'   other = FALSE
#' )
#' 
#' # View unique values in TADA.UseForAnalysis.Flag to understand inclusion criteria
#' unique(Data_Assessment2$TADA.UseForAnalysis.Flag)
#' 
#' # View unique values in TADA.Media.Flag to see media type classification
#' unique(Data_Assessment2$TADA.Media.Flag)
#'
TADA_AnalysisDataFilter <- function(
    .data,
    clean = FALSE,
    surface_water = TRUE,
    ground_water = FALSE,
    sediment = FALSE,
    other = TRUE
) {
  
  # Check if .data is a data frame
  if (!is.data.frame(.data)) {
    stop("Input object must be a data frame.")
  }
  
  # Early exit if the data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)
  }
  
  # Check for required columns
  required_columns <- c("ActivityMediaSubdivisionName", "AquiferName", "MonitoringLocationTypeName")
  missing_columns <- setdiff(required_columns, names(.data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Ensure optional columns exist so dplyr verbs don't fail
  if (!"ActivityMediaName" %in% names(.data)) {
    .data$ActivityMediaName <- NA_character_
  }
  
  # Import monitoring location types and their associated media flags (and uppercase for join)
  monitoring_location_types <- utils::read.csv(
    system.file("extdata", "WQXMonitoringLocationTypeNameRef.csv", package = "EPATADA")
  )
  monitoring_location_types <- monitoring_location_types |>
    dplyr::mutate(MonitoringLocationTypeName = toupper(MonitoringLocationTypeName))
  
  # Uppercase MonitoringLocationTypeName in .data so join works reliably
  .data <- .data |>
    dplyr::mutate(MonitoringLocationTypeName = toupper(MonitoringLocationTypeName))
  
  # Build a groundwater indicator from any available groundwater-related fields
  gw_cols <- c(
    "AquiferName", "AquiferTypeName", "LocalAqfrName", "ConstructionDateText",
    "WellDepthMeasure.MeasureValue", "WellDepthMeasure.MeasureUnitCode",
    "WellHoleDepthMeasure.MeasureValue", "WellHoleDepthMeasure.MeasureUnitCode"
  )
  present_gw_cols <- intersect(gw_cols, names(.data))
  if (length(present_gw_cols) > 0) {
    gw_has_fields <- apply(.data[, present_gw_cols, drop = FALSE], 1, function(row) any(!is.na(row)))
  } else {
    gw_has_fields <- rep(FALSE, nrow(.data))
  }
  .data$gw_has_fields <- gw_has_fields
  
  # Add TADA.Media.Flag column based on various criteria, then fill from monitoring location ref
  .data <- .data |>
    dplyr::mutate(
      TADA.Media.Flag = dplyr::case_when(
        ActivityMediaSubdivisionName == "Groundwater" | gw_has_fields ~ "GROUNDWATER",
        ActivityMediaSubdivisionName == "Surface Water" ~ "SURFACE WATER",
        !is.na(ActivityMediaName) & !ActivityMediaName %in% c("WATER", "Water", "water") ~ toupper(ActivityMediaName),
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::left_join(monitoring_location_types, by = "MonitoringLocationTypeName") |>
    dplyr::mutate(
      TADA.Media.Flag = dplyr::coalesce(TADA.Media.Flag, ML.Media.Flag, "OTHER"),
      TADA.Media.Flag = toupper(TADA.Media.Flag)
    ) |>
    dplyr::select(-ML.Media.Flag)
  
  # Add TADA.UseForAnalysis.Flag column based on media flags
  .data <- .data |>
    dplyr::mutate(
      TADA.UseForAnalysis.Flag = dplyr::case_when(
        TADA.Media.Flag == "SEDIMENT" ~ paste0(ifelse(sediment, "Include", "Exclude"), " - ", TADA.Media.Flag),
        TADA.Media.Flag == "SURFACE WATER" ~ paste0(ifelse(surface_water, "Include", "Exclude"), " - ", TADA.Media.Flag),
        TADA.Media.Flag == "GROUNDWATER" ~ paste0(ifelse(ground_water, "Include", "Exclude"), " - ", TADA.Media.Flag),
        TADA.Media.Flag == "OTHER" ~ paste0(ifelse(other, "Include", "Exclude"), " - ", TADA.Media.Flag),
        TRUE ~ paste0("Exclude - ", TADA.Media.Flag)
      )
    )
  
  if (clean) {
    # Filter out rows not flagged for inclusion and remove flag columns
    .data <- .data |>
      dplyr::filter(stringr::str_detect(TADA.UseForAnalysis.Flag, "Include")) |>
      dplyr::select(-c(TADA.UseForAnalysis.Flag, TADA.Media.Flag)) |>
      TADA_OrderCols()
    
    message("TADA_AnalysisDataFilter: Removing results flagged for exclusion from analyses.")
  } else {
    # Return all rows with flag columns
    .data <- .data |>
      TADA_OrderCols()
    
    message("TADA_AnalysisDataFilter: Returning all results with TADA.Media.Flag column and TADA.UseForAnalysis.Flag column which indicates if result should be included or excluded from analyses.")
  }
  
  return(.data)
}
