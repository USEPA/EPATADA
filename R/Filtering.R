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
      "ActivityTypeCode",
      "TADA.ActivityMediaName",
      "ActivityMediaSubdivisionName",
      "ActivityCommentText",
      "MonitoringLocationTypeName",
      "StateCode",
      "OrganizationFormalName",
      "TADA.CharacteristicName",
      "TADA.MeasureQualifierCode.Def",
      "HydrologicCondition",
      "HydrologicEvent",
      "BiologicalIntentName",
      "ActivityGroup",
      "AssemblageSampledName",
      "ProjectName",
      "CharacteristicNameUserSupplied",
      "DetectionQuantitationLimitTypeName",
      "SampleTissueAnatomyName",
      "LaboratoryName"
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
      "MethodSpeciationName",
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
      "TADA.MeasureQualifierCode.Def",
      "TADA.CensoredData.Flag",
      "TADA.CensoredMethod",
      "TADA.ResultUnit.Flag",
      "TADA.MethodSpeciationName",
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

  df <- df %>%
    dplyr::select(dplyr::contains(cols))

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

#' TADA_AssessmentDataFilter
#'
#' This function will filter the data set and retain only the media types
#' selected by the user. It uses ConstructionDateText, 
#' WellDepthMeasure.MeasureValue, WellDepthMeasure.MeasureUnitCode,
#' WellHoleDepthMeasure.MeasureValue, and WellHoleDepthMeasure.MeasureUnitCode
#' to identify groundwater samples and creates a column "TADA.Groundwater.Flag".
#' Users can select whether sediment, fish tissue and/or surface water should 
#' be included in the data set. An additional column TADA.AssessmentData.Flag
#' specifies whether each row should be included in the assessment workflow.
#' Setting clean = TRUE, means that all results not flagged for use in 
#' assessment workflow will be removed and the TADA.Groundwater.Flag and 
#' TADA.AssessmentData.Flag columns will not be added.
#' 
#' *Need to add fish tissue to this function once new WQX profiles are available.
#' (HRM, 1/22/4)
#'
#'
#' @param .data A TADA profile object
#' 
#' @param clean Boolean argument; removes all results not flagged for use in
#' assessment workflow. TADA.Groundwater.Flag and TADA.AssessmentData.Flag 
#' columns will not be added Default is clean = TRUE.
#' 
#' @param surface_water Boolean argument; specifies whether surface water
#' results should be included in the returned data frame. Default is 
#' surface_water = TRUE, surface water samples are retained in the data frame.
#' 
#' @param ground_water Boolean argument; specifies whether ground water
#' results should be included in the returned data frame. Default is 
#' ground_water = FALSE, ground water samples are  not retained in the data 
#' frame.
#' 
#' @param sediment Boolean argument; specifies whether sediment results should 
#' be included in the returned data frame. Default is sediment = FALSE, 
#' sediment samples are not retained in the data frame.
#' 
#' @param fish_tissue Boolean argument; specifies whether fish tissue 
#' results should be included in the returned data frame. Default is 
#' fish_tissue = FALSE, fish tissue samples are not retained in the data frame.
#'
#' @return If clean = TRUE, returns the data frame with only the media types
#' selected by the user. If clean = FALSE, returns the data frame with two
#' additional columns, "TADA.Groundwater.Flag" and "TADA.AssessmentData.Flag",
#' indicating which results should be excluded from assessments based on user
#' input.
#' 
#' @export
#' 
#' @examples
#' Return data frame with only surface water results
#' data(Data_6Tribes_5y_Harmonized)
#' Data_6Tribes_Assessment <- TADA_AssessmentDataFilter(Data_6Tribes_5y_Harmonized)
#' 
#' Return data frame with surface water and sediment results
#' Data_6Tribes_Assessment <- TADA_AssessmentDataFilter(Data_6Tribes_5y_Harmonized, clean = TRUE, surface_water = TRUE, ground_water = FALSE, sediment = TRUE)

TADA_AssessmentDataFilter <- function(.data, 
                                      clean = TRUE,
                                      surface_water = TRUE,
                                      ground_water = FALSE,
                                      sediment = FALSE) {
  
  # import MonitoringLocationTypeNames and TADA.Groundwater.Flags
  sw.sitetypes <- utils::read.csv(system.file("extdata", "WQXMonitoringLocationTypeNameRef.csv", package = "TADA")) %>%
    dplyr::select(MonitoringLocationTypeName, TADA.Groundwater.Flag) %>%
    dplyr::rename(ML.Groundwater.Flag = TADA.Groundwater.Flag)
  
  
  # add TADA.Groundwater.Flag column
  .data <- .data %>%
    # identify TADA.Groundwater.Flag using ActivityMediaSubdivisionName and columns related to groundwater
    dplyr::mutate(TADA.Groundwater.Flag = dplyr::case_when(ActivityMediaSubdivisionName == "Groundwater" ~ "Groundwater",
                                                           !is.na(AquiferName) |
                                                             !is.na(AquiferTypeName) |
                                                             !is.na(LocalAqfrName) |
                                                             !is.na(ConstructionDateText) |
                                                             !is.na(WellDepthMeasure.MeasureValue) |
                                                             !is.na(WellDepthMeasure.MeasureUnitCode) |
                                                             !is.na(WellHoleDepthMeasure.MeasureValue) |
                                                             !is.na(WellHoleDepthMeasure.MeasureUnitCode) ~ "Groundwater",
                                                           ActivityMediaSubdivisionName == "Surface Water" ~ "Surface Water",
                                                           TADA.ActivityMediaName != "WATER" ~ "Not Water")) %>%
    # add TADA.Groundwater.Flag for additional rows based on MonitoringLocationTypeName
    dplyr::left_join(sw.sitetypes) %>%
    dplyr::mutate(TADA.Groundwater.Flag = ifelse(is.na(TADA.Groundwater.Flag),
                                                 ML.Groundwater.Flag, TADA.Groundwater.Flag)) %>%
    dplyr::select(-ML.Groundwater.Flag)
  
  { if (surface_water == TRUE)
    
    sur.water.data <- .data %>%
      dplyr::filter(TADA.Groundwater.Flag == "Surface Water") %>%
      dplyr::mutate(TADA.UseForAssessment.Flag = "Yes")
    }
  
  { if (surface_water == FALSE)
    
    sur.water.data <- .data %>%
      dplyr::filter(TADA.Groundwater.Flag == "Surface Water") %>%
      dplyr::mutate(TADA.UseForAssessment.Flag = "No")
    
  }
  
  
  if (ground_water == TRUE) {
    
    gr.water.data <- .data %>%
      dplyr::filter(TADA.Groundwater.Flag == "Groundwater") %>%
      dplyr::mutate(TADA.UseForAssessment.Flag = "Yes")
    
  }
  
  if (ground_water == FALSE) {
    gr.water.data <- .data %>%
      dplyr::filter(TADA.Groundwater.Flag == "Groundwater") %>%
      dplyr::mutate(TADA.UseForAssessment.Flag = "No")
    
  }
  if (sediment == TRUE) {
    sed.data <- .data %>%
      dplyr::filter(TADA.ActivityMediaName == "SEDIMENT") %>%
      dplyr::mutate(TADA.UseForAssessment.Flag = "Yes")
    
  }
  
  if (sediment == FALSE) {
    sed.data <- .data %>%
      dplyr::filter(TADA.ActivityMediaName == "SEDIMENT") %>%
      dplyr::mutate(TADA.UseForAssessment.Flag = "No")
  }
  
  if (clean == TRUE) {
    
    assessment.data <- .data %>%
      dplyr::left_join(sur.water.data) %>%
      dplyr::left_join(gr.water.data) %>%
      dplyr::left_join(sed.data) %>%
      dplyr::filter(TADA.UseForAssessment.Flag == "Yes") %>%
      dplyr::select(-TADA.UseForAssessment.Flag, -TADA.Groundwater.Flag)
    
    rm(sur.water.data, gr.water.data, sed.data)
    
    return(assessment.data)
    
    
  }
  
  if (clean == FALSE) {
    
    assessment.data <- .data %>%
      dplyr::left_join(sur.water.data) %>%
      dplyr::left_join(gr.water.data) %>%
      dplyr::left_join(sed.data) %>%
      dplyr::mutate(TADA.UseForAssessment.Flag = ifelse(is.na(TADA.UseForAssessment.Flag), "No", TADA.UseForAssessment.Flag))
    
    rm(sur.water.data, gr.water.data, sed.data)
    
    return(assessment.data)
  }
}
