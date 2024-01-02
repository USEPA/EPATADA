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
#' data(Data_Nutrients_UT)
#' # Count table of key fields in Data_Nutrients_UT dataset
#' fieldCountUT <- TADA_FieldCounts(Data_Nutrients_UT)
#' # Count table of most fields in Data_Nutrients_UT, filtered to only AMMONIA results.
#' fieldCountUTAmmonia <- TADA_FieldCounts(Data_Nutrients_UT, display = "most", characteristicName = "AMMONIA")
TADA_FieldCounts <- function(.data, display = c("key", "most", "all"), characteristicName = "null") {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  display <- match.arg(display)

  # filter to characteristic if provided
  if (!characteristicName %in% c("null")) {
    .data <- subset(.data, .data$TADA.CharacteristicName %in% c(characteristicName))

    if (dim(.data)[1] < 1) {
      stop("Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization of synonyms from different WQP data providers (USGS's NWIS and EPA's WQX).")
    }
  }

  # remove fields with only NAs from df
  df <- .data %>% dplyr::select(where(~ !all(is.na(.x))))

  if (display == "key") {
    cols <- c(
      "ActivityTypeCode", "TADA.ActivityMediaName",
      "ActivityMediaSubdivisionName",
      "ActivityCommentText", "MonitoringLocationTypeName",
      "StateName", "TribalLandName",
      "OrganizationFormalName", "TADA.CharacteristicName",
      "HydrologicCondition", "HydrologicEvent",
      "BiologicalIntentName", "MeasureQualifierCode",
      "ActivityGroup", "AssemblageSampledName",
      "ProjectName", "CharacteristicNameUserSupplied",
      "DetectionQuantitationLimitTypeName",
      "SampleTissueAnatomyName", "LaboratoryName"
    )
  }
  if (display == "most") {
    cols <- c(
      "ResultIdentifier",
      "OrganizationIdentifier",
      "OrganizationFormalName",
      "ActivityIdentifier",
      "ActivityTypeCode",
      "ActivityMediaName",
      "ActivityMediaSubdivisionName",
      "ActivityRelativeDepthName",
      "ProjectIdentifier",
      "ProjectName",
      "ActivityConductingOrganizationText",
      "MonitoringLocationIdentifier",
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
      "MethodSpeciationName",
      "CharacteristicName",
      "ResultSampleFractionText",
      "MeasureQualifierCode",
      "ResultStatusIdentifier",
      "StatisticalBaseCode",
      "ResultValueTypeName",
      "ResultWeightBasisText",
      "ResultTimeBasisText",
      "ResultTemperatureBasisText",
      "ResultParticleSizeBasisText",
      "DataQuality.PrecisionValue",
      "DataQuality.BiasValue",
      "DataQuality.ConfidenceIntervalValue",
      "DataQuality.UpperConfidenceLimitValue",
      "DataQuality.LowerConfidenceLimitValue",
      "ResultCommentText",
      "USGSPCode",
      "ResultDepthAltitudeReferencePointText",
      "SubjectTaxonomicName",
      "SampleTissueAnatomyName",
      "BinaryObjectFileName",
      "BinaryObjectFileTypeCode",
      "ResultFileUrl",
      "ResultAnalyticalMethod.MethodIdentifier",
      "ResultAnalyticalMethod.MethodIdentifierContext",
      "ResultAnalyticalMethod.MethodName",
      "ResultAnalyticalMethod.MethodUrl",
      "ResultAnalyticalMethod.MethodDescriptionText",
      "LaboratoryName",
      "ResultLaboratoryCommentText",
      "ResultDetectionQuantitationLimitUrl",
      "DetectionQuantitationLimitTypeName",
      "LabSamplePreparationUrl",
      "ProviderName",
      "MonitoringLocationTypeName",
      "MonitoringLocationDescriptionText",
      "HUCEightDigitCode",
      "LatitudeMeasure",
      "LongitudeMeasure",
      "SourceMapScaleNumeric",
      "HorizontalCollectionMethodName",
      "HorizontalCoordinateReferenceSystemDatumName",
      "VerticalCollectionMethodName",
      "VerticalCoordinateReferenceSystemDatumName",
      "CountryCode",
      "StateCode",
      "CountyCode",
      "AquiferName",
      "LocalAqfrName",
      "FormationTypeText",
      "AquiferTypeName",
      "ConstructionDateText",
      "MethodSpecificationName",
      "ProjectDescriptionText",
      "SamplingDesignTypeCode",
      "QAPPApprovedIndicator",
      "QAPPApprovalAgencyName",
      "ProjectFileUrl",
      "ProjectMonitoringLocationWeightingUrl",
      "TADA.LatitudeMeasure",
      "TADA.LongitudeMeasure",
      "TADA.InvalidCoordinates.Flag",
      "TADA.ActivityMediaName",
      "TADA.CharacteristicName",
      "TADA.CharacteristicGroup",
      "CharacteristicNameUserSupplied",
      "TADA.SuggestedCharacteristicName",
      "TADA.CharacteristicNameAssumptions",
      "TADA.TotalN_TotalP_CharacteristicNames_AfterSummation",
      "TADA.TotalN_TotalP_Summation_Identifier",
      "TADA.TotalN_TotalP_ComboLogic",
      "TADA.AggregatedContinuousData.Flag",
      "TADA.ResultMeasureValue",
      "TADA.ResultMeasureValueDataTypes.Flag",
      "TADA.CensoredData.Flag",
      "TADA.CensoredMethod",
      "TADA.ResultUnit.Flag",
      "TADA.MethodSpecificationName",
      "TADA.AnalyticalMethod.Flag",
      "TADA.MethodSpeciation.Flag",
      "TADA.ResultSampleFractionText",
      "TADA.SampleFraction.Flag",
      "TADA.SuggestedSampleFraction",
      "TADA.FractionAssumptions",
      "TADA.ComparableDataIdentifier",
      "TADA.RemoveReason"
    )
  }
  if (display == "all") {
    cols <- names(df)
  }

  df <- df[, names(df) %in% cols]

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
  col.names <- col.names %>%
    dplyr::arrange(desc(Count))

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
#' data(Data_Nutrients_UT)
#'
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter:
#' TADA_FieldValuesTable(Data_Nutrients_UT, field = "TADA.CharacteristicName")
#'
TADA_FieldValuesTable <- function(.data, field = "null", characteristicName = "null") {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  if (!field %in% names(.data)) {
    stop("Field input does not exist in dataset. Please populate the 'field' argument with a valid field name. Enter ?TADA::filterPie in console for more information.")
  }

  # filter to characteristic if provided
  if (!characteristicName %in% c("null")) {
    .data <- subset(.data, .data$TADA.CharacteristicName %in% c(characteristicName))
    if (dim(.data)[1] < 1) {
      stop("Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization.")
    }
  }

  dat <- as.data.frame(table(.data[, field]))
  names(dat) <- c("Value", "Count")
  dat <- dat %>% dplyr::arrange(desc(Count))
  return(dat)
}
