#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



# write global variables. Gets rid of global variable NOTE in check:
utils::globalVariables(c("TADA.ResultValueAboveUpperThreshold.Flag", "ActivityIdentifier",  "ActivityMediaName",
                         "ActivityStartDate", "TADA.ResultValueBelowUpperThreshold.Flag", "TADA.ResultValueBelowLowerThreshold.Flag", "CharacteristicName",
                         "Conversion.Factor", "Count", "Description", "FieldName", "FieldValue",
                         "MethodSpecificationName", "MonitoringLocationIdentifier",
                         "OrganizationFormalName", "OrganizationIdentifier", "ProjectDescriptionText",
                         "ProjectFileUrl", "ProjectIdentifier",
                         "ProjectMonitoringLocationWeightingUrl", "ProjectName",
                         "QAPPApprovalAgencyName", "QAPPApprovedIndicator",
                         "ResultDetectionConditionText", "ResultMeasureValue",
                         "SamplingDesignTypeCode", "Source", "Status", "TADA.AggregatedContinuousData.Flag",
                         "TADA.InvalidCoordinates.Flag", "TADA.PotentialDupRowIDs.Flag", "TADA.QAPPDocAvailable",
                         "Target.Unit", "Type", "Value.Unit", "TADA.AnalyticalMethod.Flag",
                         "TADA.MethodSpeciation.Flag", "TADA.ResultUnit.Flag",
                         "TADA.SampleFraction.Flag", "YearSummarized", "where", "TADA.CharacteristicName",
                         "ResultIdentifier", "TADA.ResultMeasureValue", "n_sites",
                         "n_records", "statecodes_df", "STUSAB" ,"ActivityStartTime.Time", "numorgs", "dup_id",
                         "LatitudeMeasure", "TADA.ResultMeasureValueDataTypes.Flag", "Name", "TADA.Detection_Type",
                         "DetectionQuantitationLimitTypeName", "TADA.Limit_Type", "multiplier", "summ", "cf",
                         "LongitudeMeasure", "TADA.CensoredData.Flag", "Censored_Count", "TADA.ResultMeasureValueDataTypes.Flag",
                         "Status2", "ActivityTypeCode", "SampleCollectionEquipmentName", 
                         "ResultTimeBasisText", "StatisticalBaseCode", "ResultValueTypeName", 
                         "masked", "TADA.env", "Legend", "Fields", "desc", "WQXActivityType_Cached", 
                         "TADA.ActivityType.Flag", "Code", "ActivityTypeCode", "ResultCount", 
                         "tot_n", "MonitoringLocationName", "TADA.LatitudeMeasure", 
                         "TADA.LongitudeMeasure", "median", "sd", "TADA.ComparableDataIdentifier", 
                         "desc", "Legend", "roundRV", "TADA.DuplicateID", "maxRV", "within10", 
                         "AllGroups"))



#' TADA_AutoClean
#'
#' Removes rows of data that are true duplicates. Creates new columns with
#' prefix "TADA." and capitalizes fields to harmonize data. This function runs
#' "TADA_ConvertSpecialChars", "TADA_ConvertResultUnits",
#' "TADA_ConvertDepthUnits", and "TADA_IDCensoredData" functions, which perform
#' the following QA steps: remove true duplicates, convert result values to
#' numeric, harmonize result and depth units (note: all depth-related columns
#' with populated values are converted to meters in a TADA-specific column),
#' convert text to uppercase letters, substitute outdated (deprecated) characteristic names
#' with updated names in TADA.CharacteristicName, and categorize detection limit
#' data. Original affected columns are not changed: new columns are added to the
#' end of the dataframe with the prefix "TADA." This function makes important
#' character fields uppercase so that they're interoperable with the WQX
#' validation reference tables and reduces issues with case-sensitivity when
#' joining data.TADA_AutoClean can be run as a stand alone function but is
#' primarily used by the TADA_dataRetrieval function.
#' 
#' @param .data TADA dataframe
#'
#' @return Input dataframe with several added TADA-specific columns, including:
#'   TADA.ActivityMediaName, TADA.CharacteristicName, TADA.ResultMeasureValue,
#'   TADA.ResultMeasure.MeasureUnitCode, TADA.ResultMeasureValueDataTypes.Flag,
#'   TADA.CensoredData.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure,
#'   TADA.ResultSampleFractionText, TADA.MethodSpecificationName, and more.
#'   Please note that the number of TADA-specific depth columns in the returned
#'   dataframe depends upon the number of depth columns with one or more results
#'   populated with a numeric value. If all depth columns contain only NA's, no
#'   conversion is necessary and no TADA depth columns are created.
#' 
#' @export 
#'

TADA_AutoClean <- function(.data) {
  
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  
  # .data required columns
  required_cols <- c(
    "ActivityMediaName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",
    "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName", 
    "DetectionQuantitationLimitMeasure.MeasureUnitCode", "ResultDetectionConditionText"
  ) 
  
  # check .data has required columns
  TADA_CheckColumns(.data, required_cols)
  
  # execute function after checks are passed
  
  # capitalize fields with known synonyms that only differ in caps
  print("TADA_Autoclean: creating TADA-specific columns.")
  .data$TADA.CharacteristicName <- toupper(.data$CharacteristicName)
  .data$TADA.ResultSampleFractionText <- toupper(.data$ResultSampleFractionText)
  .data$TADA.MethodSpecificationName <- toupper(.data$MethodSpecificationName)
  .data$TADA.ResultMeasure.MeasureUnitCode <- toupper(.data$ResultMeasure.MeasureUnitCode)
  .data$TADA.ActivityMediaName <- toupper(.data$ActivityMediaName)
  .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode <-
    toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  
  # Remove complex biological data. Un-comment after new WQX 3.0 Profiles are released. May not be needed if implemented via WQP UI/services.
  # .data$TADA.BiologicalIntentName = toupper(.data$BiologicalIntentName)
  # TADAProfile = dplyr::filter(TADAProfile, TADA.BiologicalIntentName != "TISSUE" | "TOXICITY" | is.na(TADA.BiologicalIntentName) == TRUE)
  
  # Remove data for non-water media types. Un-comment. Discuss possibly adding back
  # later once new workflow for documenting "removed" data is set up. May not be needed for R package.
  # TADAProfile <- dplyr::filter(TADAProfile, TADA.ActivityMediaName == "WATER")
  
  # Remove duplicate rows - turned into a test because duplicated() takes a long
  # time acting on all columns in a large dataset.
  # test relies only on ResultIdentifier to identify exact duplicates. It 
  # speeds the process up quite a bit.
  print("TADA_Autoclean: checking for exact duplicates.")
  if(!length(unique(.data$ResultIdentifier))==dim(.data)[1]){
    print("Duplicate results may be present. Filtering to unique results. This may take a while on large datasets.")
    dup_rids = names(table(.data$ResultIdentifier)[table(.data$ResultIdentifier)>1])
    dup_check = .data%>%dplyr::filter(ResultIdentifier%in%dup_rids)%>%dplyr::group_by(ResultIdentifier)%>%dplyr::distinct()
    not_dups = .data%>%dplyr::filter(!ResultIdentifier%in%dup_rids)
    .data = plyr::rbind.fill(dup_check, not_dups)
  }
  
  # run TADA_ConvertSpecialChars function
  # .data <- MeasureValueSpecialCharacters(.data)
  print("TADA_Autoclean: checking for special characters.")
  .data <- TADA_ConvertSpecialChars(.data, "ResultMeasureValue")
  .data <- TADA_ConvertSpecialChars(.data, "DetectionQuantitationLimitMeasure.MeasureValue")
  
  # Move detection limit value and unit to TADA Result Measure Value and Unit columns
  .data$TADA.ResultMeasureValue <- ifelse(is.na(.data$TADA.ResultMeasureValue)&!is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue),.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue,.data$TADA.ResultMeasureValue)
  .data$TADA.ResultMeasure.MeasureUnitCode <- ifelse(is.na(.data$TADA.ResultMeasure.MeasureUnitCode)&!is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode),.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,.data$TADA.ResultMeasure.MeasureUnitCode)
  .data$TADA.ResultMeasureValueDataTypes.Flag <- ifelse(.data$TADA.ResultMeasureValueDataTypes.Flag=="ND or NA"&!is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue),"Result Value/Unit Copied from Detection Limit",.data$TADA.ResultMeasureValueDataTypes.Flag)
  
  # Identify detection limit data
  print("TADA_Autoclean: identifying detection limit data.")
  .data <- TADA_IDCensoredData(.data)
  
  # change latitude and longitude measures to class numeric
  .data$TADA.LatitudeMeasure <- as.numeric(.data$LatitudeMeasure)
  .data$TADA.LongitudeMeasure <- as.numeric(.data$LongitudeMeasure)
  
  # Implement unit harmonization
  print("TADA_Autoclean: harmonizing result and depth units.")
  .data = TADA_ConvertResultUnits(.data, transform = TRUE)
  .data = TADA_ConvertDepthUnits(.data, unit = "m")
  
  # #convert 'meters' to 'm' - EDH MOVED TO CONVERT DEPTH UNITS
  # .data$TADA.ActivityDepthHeightMeasure.MeasureUnitCode[.data$ActivityDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  # .data$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode[.data$ActivityTopDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  # .data$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode[.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  # .data$TADA.ResultDepthHeightMeasure.MeasureUnitCode[.data$ResultDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  .data$TADA.ResultMeasure.MeasureUnitCode[.data$TADA.ResultMeasure.MeasureUnitCode == 'meters'] <- 'm'
  
  # Substitute updated characteristic name for deprecated names
  print("TADA_Autoclean: updating deprecated characterisitc names.")
  .data = TADA_SubstituteDeprecatedChars(.data)
  
  # create comparable data identifier column
  .data = TADA_CreateComparableID(.data)
  
  print("NOTE: This version of the TADA package is designed to work with quantitative (numeric) data with media name: 'WATER'. TADA_AutoClean does not currently remove (filter) data with non-water media types. If desired, the user must make this specification on their own outside of package functions. Example: dplyr::filter(.data, TADA.ActivityMediaName == 'WATER')")

  .data <- TADA_OrderCols(.data)
  
  return(.data)
}



#' Decimal Places
#'
#' for numeric data type
#'
#' @param x Numeric data field from TADA profile
#'
#' @return Number of values to the right of the decimal point for numeric type data.
#'

TADA_DecimalPlaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}



#' Check Type
#'
#' This function checks if the inputs to a function are of the expected type. It
#' is used at the beginning of TADA functions to ensure the
#' inputs are suitable.
#'
#' @param arg An input argument to check
#' @param type Expected class of input argument
#' @param paramName Optional name for argument to use in error message
#'

TADA_CheckType <- function(arg, type, paramName) {
  if ((type %in% class(arg)) == FALSE) {
    # if optional parameter name not specified use arg in errorMessage
    if (missing(paramName)) {
      paramName = arg
    }
    errorMessage <- paste(paramName, " must be of class '", type, "'", sep = "")
    return(stop(errorMessage))
  }
}



#' Check Columns
#'
#' This function checks if the expected column names are in the dataframe. It is
#' used at the beginning of TADA functions to ensure the input data frame is
#' suitable (i.e. is either the full physical/chemical results profile
#' downloaded from WQP or the TADA profile template downloaded from the EPA TADA
#' webpage.)
#'
#' @param .data A dataframe
#' @param expected_cols A vector of expected column names as strings
#'

TADA_CheckColumns <- function(.data, expected_cols) {
  if (all(expected_cols %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
}



#' TADA_ConvertSpecialChars
#' 
#' This function will screen a column of the user's choice for special
#' characters. It creates a NEW column that describes the content of the column
#' prior to conversion to numeric (named "TADA.COLUMN NAME DataTypes.Flag"). It
#' also creates a NEW column to hold the new, numeric format (named "TADA.COLUMN
#' NAME"). This function will successfully convert some special character
#' formats to numeric: whitespace, >, <, ~, %, and commas are removed before
#' converting a result value to numeric. Result values in the format # - # are
#' converted to an average of the two numbers. Result values
#' containing any other text or non-numeric characters become NA in
#' the newly created "TADA.COLUMN NAME" and labeled accordingly in "TADA.COLUMN
#' NAME DataTypes.Flag".
#' 
#' 
#' @param .data A TADA profile object
#' @param col A character column to be converted to numeric
#' 
#' @return Returns the original dataframe with two new columns: the input column 
#' with the prefix "TADA.", which holds the numeric form of the original column, 
#' and "TADA.ResultValueDataTypes.Flag", which has text describing the type of data
#' contained within the column of interest, including "Numeric","Less Than" (<), "Greater Than" (>),
#' "Approximate Value" (~), "Text" (A-z), "Percentage" (%), "Comma-Separated Numeric" (#,###),
#' and "Numeric Range - Averaged" (# - #)
#' 
#' @export
#' 

TADA_ConvertSpecialChars <- function(.data,col){
  if(!col%in%names(.data)){
    stop("Invalid column name specified for input dataset.")
  }
  
  # Define new column names
  numcol = paste0("TADA.",col)
  flagcol = paste0("TADA.",col,"DataTypes.Flag")
  
  # Create dummy columns for easy handling in function
  chars.data = .data
  names(chars.data)[names(chars.data)==col] = "orig"
  chars.data$masked = chars.data$orig
  
  # If column is already numeric, just discern between NA and numeric
  if(is.numeric(chars.data$orig)){
    clean.data = chars.data%>%
      dplyr::mutate(flag = dplyr::case_when(
        is.na(masked) ~ as.character("ND or NA"),
        TRUE ~ as.character("Numeric")
      ))
  }else{
    chars.data$masked = gsub(" ","",chars.data$masked) # get rid of white space for subsequent sorting
    
    # Detect special characters in column and populate new flag column with descriptor
    # of the specific type of character/data type
    clean.data = chars.data%>%
      dplyr::mutate(flag = dplyr::case_when(
        is.na(masked) ~ as.character("ND or NA"),
        (masked=="ND") ~ as.character("ND or NA"),
        (!is.na(suppressWarnings(as.numeric(masked)) == TRUE)) ~ as.character("Numeric"),
        (grepl("<", masked) == TRUE) ~ as.character("Less Than"),
        (grepl(">", masked) == TRUE) ~ as.character("Greater Than"),
        (grepl("~", masked) == TRUE) ~ as.character("Approximate Value"),
        (grepl("[A-Za-z]", masked) == TRUE) ~ as.character("Text"),
        (grepl("%", masked) == TRUE) ~ as.character("Percentage"),
        (grepl(",", masked) == TRUE) ~ as.character("Comma-Separated Numeric"),
        (grepl('\\d\\-\\d',masked) == TRUE) ~ as.character("Numeric Range - Averaged"),
        (!stringi::stri_enc_mark(masked)%in%c("ASCII")) ~ as.character("Non-ASCII Character(s)"),
        TRUE ~ "Coerced to NA"
      ))
    
    # Result Values that are numeric ranges with the format #-# are converted to an average of the two numbers expressed in the range.
    if(any(clean.data$flag=="Numeric Range - Averaged")){
      numrange = subset(clean.data, clean.data$flag%in%c("Numeric Range - Averaged"))
      notnumrange = subset(clean.data, !clean.data$flag%in%c("Numeric Range - Averaged"))
      numrange = numrange%>%tidyr::separate(masked,into = c("num1","num2"),sep = "-", remove = TRUE)%>%dplyr::mutate_at(c('num1', 'num2'), as.numeric)
      numrange$masked = as.character(rowMeans(numrange[,c('num1', 'num2')], na.rm=TRUE))
      numrange = numrange[,!names(numrange)%in%c('num1','num2')]
      
      clean.data = plyr::rbind.fill(notnumrange,numrange)
    }
    # In the new TADA column, convert to numeric and remove some specific special 
    # characters.
    clean.data$masked = suppressWarnings(as.numeric(stringr::str_replace_all(
      clean.data$masked,c("<" = "",">" = "","~" = "","," = "","%" = ""))))
  }
  
  # Rename to original column name, TADA column name, and flag column name
  names(clean.data)[names(clean.data)=="orig"] = col
  names(clean.data)[names(clean.data)=="masked"] = numcol
  names(clean.data)[names(clean.data)=="flag"] = flagcol
  
  clean.data = TADA_OrderCols(clean.data)
  
  return(clean.data)
}

#' Order TADA Columns and Rows
#' 
#' This utility function moves all TADA-created columns to the end of the dataframe 
#' in an order that improves readability and orders the dataframe rows by ResultIdentifier. 
#' 
#' @param .data TADA dataframe 
#' 
#' @return A TADA handled dataframe with the TADA-created columns at the end and rows ordered by ResultIdentifier.
#'
#' @export
#' 
#' 

TADA_OrderCols <- function(.data){
  
  dretcols = c("OrganizationIdentifier","
              OrganizationFormalName",
              "ActivityIdentifier",
              "ActivityTypeCode",
              "ActivityMediaName",
              "ActivityMediaSubdivisionName",
              "ActivityStartDate",
              "ActivityStartTime.Time",
              "ActivityStartTime.TimeZoneCode",
              "ActivityEndDate",
              "ActivityEndTime.Time",
              "ActivityEndTime.TimeZoneCode",
              "ActivityRelativeDepthName",
              "ActivityDepthHeightMeasure.MeasureValue",
              "ActivityDepthHeightMeasure.MeasureUnitCode",
              "ActivityDepthAltitudeReferencePointText",
              "ActivityTopDepthHeightMeasure.MeasureValue",
              "ActivityTopDepthHeightMeasure.MeasureUnitCode",
              "ActivityBottomDepthHeightMeasure.MeasureValue",
              "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
              "ProjectIdentifier",
              "ProjectName",
              "ActivityConductingOrganizationText",
              "MonitoringLocationIdentifier",
              "MonitoringLocationName",
              "ActivityCommentText",
              "SampleAquifer",
              "HydrologicCondition",
              "HydrologicEvent",
              "ActivityLocation.LatitudeMeasure",
              "ActivityLocation.LongitudeMeasure",
              "SampleCollectionMethod.MethodIdentifier",
              "SampleCollectionMethod.MethodIdentifierContext",
              "SampleCollectionMethod.MethodName",
              "SampleCollectionMethod.MethodDescriptionText",
              "SampleCollectionEquipmentName",
              "ResultIdentifier",
              "ResultDetectionConditionText",
              "MethodSpeciationName",
              "CharacteristicName",
              "ResultSampleFractionText",
              "ResultMeasureValue",
              "ResultMeasure.MeasureUnitCode",
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
              "ResultDepthHeightMeasure.MeasureValue",
              "ResultDepthHeightMeasure.MeasureUnitCode",
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
              "AnalysisStartDate",
              "ResultLaboratoryCommentText",
              "ResultDetectionQuantitationLimitUrl",
              "DetectionQuantitationLimitTypeName",
              "DetectionQuantitationLimitMeasure.MeasureValue",
              "DetectionQuantitationLimitMeasure.MeasureUnitCode",
              "LabSamplePreparationUrl",
              "LastUpdated",
              "ProviderName",
              "timeZoneStart",
              "timeZoneEnd",
              "ActivityStartDateTime",
              "ActivityEndDateTime",
              "MonitoringLocationTypeName",
              "MonitoringLocationDescriptionText",
              "HUCEightDigitCode",
              "DrainageAreaMeasure.MeasureValue",
              "DrainageAreaMeasure.MeasureUnitCode",
              "ContributingDrainageAreaMeasure.MeasureValue",
              "ContributingDrainageAreaMeasure.MeasureUnitCode",
              "LatitudeMeasure",
              "LongitudeMeasure",
              "SourceMapScaleNumeric",
              "HorizontalAccuracyMeasure.MeasureValue",
              "HorizontalAccuracyMeasure.MeasureUnitCode",
              "HorizontalCollectionMethodName",
              "HorizontalCoordinateReferenceSystemDatumName",
              "VerticalMeasure.MeasureValue",
              "VerticalMeasure.MeasureUnitCode",
              "VerticalAccuracyMeasure.MeasureValue",
              "VerticalAccuracyMeasure.MeasureUnitCode",
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
              "WellDepthMeasure.MeasureValue",
              "WellDepthMeasure.MeasureUnitCode",
              "WellHoleDepthMeasure.MeasureValue",
              "WellHoleDepthMeasure.MeasureUnitCode",
              "MethodSpecificationName",
              "ProjectDescriptionText",
              "SamplingDesignTypeCode",
              "QAPPApprovedIndicator",
              "QAPPApprovalAgencyName",
              "ProjectFileUrl",
              "ProjectMonitoringLocationWeightingUrl"
  )
  
  tadacols = c("TADA.LatitudeMeasure",
           "TADA.LongitudeMeasure",
           "TADA.InvalidCoordinates.Flag",
           "TADA.QAPPDocAvailable",
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
           "TADA.ResultMeasure.MeasureUnitCode",
           "WQX.TargetUnit",
           "WQX.ConversionFactor",
           "WQX.ResultMeasureValue.UnitConversion",
           "TADA.SuggestedResultUnit",
           "TADA.UnitConversionFactor",
           "TADA.UnitConversionCoefficient",
           "WQX.DetectionLimitMeasureValue.UnitConversion",
           "AboveWQXUpperThreshold",
           "BelowWQXLowerThreshold",
           "TADA.ResultUnit.Flag",
           "CombinationValidity",
           "WQX.ResultMeasureValue.UnitConversion",
           "TADA.MethodSpecificationName",
           "TADA.AnalyticalMethod.Flag",
           "TADA.MethodSpeciation.Flag",
           "TADA.SuggestedSpeciation",
           "TADA.SpeciationAssumptions",
           "TADA.SpeciationConversionFactor",
           "TADA.ResultSampleFractionText",
           "TADA.SampleFraction.Flag",
           "TADA.SuggestedSampleFraction",
           "TADA.FractionAssumptions",
           "TADA.ComparableDataIdentifier",
           "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
           "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode",
           "TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag",
           "TADA.ActivityDepthHeightMeasure.MeasureValue",
           "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
           "TADA.ActivityDepthHeightMeasure.MeasureValueDataTypes.Flag",
           "TADA.ActivityTopDepthHeightMeasure.MeasureValue",
           "TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode",
           "TADA.ActivityTopDepthHeightMeasure.MeasureValueDataTypes.Flag",
           "TADA.ActivityBottomDepthHeightMeasure.MeasureValue",
           "TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode",
           "TADA.ActivityBottomDepthHeightMeasure.MeasureValueDataTypes.Flag",
           "TADA.ResultDepthHeightMeasure.MeasureValue",
           "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
           "TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag",
           "WQXConversionFactor.ActivityDepthHeightMeasure",
           "WQXConversionFactor.ActivityTopDepthHeightMeasure",
           "WQXConversionFactor.ActivityBottomDepthHeightMeasure",
           "WQXConversionFactor.ResultDepthHeightMeasure",
           "TADA.ProbableDuplicate",
           "TADA.DuplicateID",
           "TADA.Remove",
           "TADA.RemovalReason",
           "TADAShiny.tab"
  )
  dret_cols = dretcols[dretcols%in%names(.data)]
  tada_cols = tadacols[tadacols%in%names(.data)]
  
  rearranged = .data%>%dplyr::relocate(dplyr::any_of(dret_cols))%>%dplyr::relocate(dplyr::any_of(tada_cols),.after = dplyr::last_col())
  rearranged = rearranged[order(rearranged$ResultIdentifier),]
  
  return(rearranged)
  
}

#' Substitute Preferred Characteristic Name for Deprecated Names
#' 
#' This utility function uses the WQX Characteristic domain table to substitute
#' deprecated characteristic names with the new name in the
#' TADA.CharacteristicName column. Used in TADA_Autoclean function on data
#' pulled from the WQP.
#' 
#' @param .data TADA dataframe 
#' 
#' @return Input TADA dataframe with substituted characteristic names in
#'   TADA.CharacteristicName column. Original columns are unchanged.
#'
#' @export
#' 

TADA_SubstituteDeprecatedChars <- function(.data){
  TADA_CheckColumns(.data, expected_cols = c("CharacteristicName","TADA.CharacteristicName"))
  
  # read in characteristic reference table with deprecation information and filter to deprecated terms
  ref.table = TADA_GetCharacteristicRef()%>%dplyr::filter(Char_Flag == "Deprecated")
  
  # merge to dataset
  .data = merge(.data, ref.table, all.x = TRUE)
  # if CharacteristicName is deprecated and comparable name is not BLANK, use the provided Comparable.Name. Otherwise, keep TADA.CharacteristicName as-is.
  .data$TADA.CharacteristicName = ifelse(!is.na(.data$Char_Flag)&!.data$Comparable.Name%in%c(""),.data$Comparable.Name, .data$TADA.CharacteristicName)
  
  howmany = length(.data$Char_Flag[!is.na(.data$Char_Flag)])
  
  if(howmany>0){
    chars = unique(.data$CharacteristicName[!is.na(.data$Char_Flag)])
    chars = paste0(chars, collapse = "; ")
    print(paste0(howmany, " results in your dataset have one of the following deprecated characteristic names: ",chars,". These names have been substituted with the updated preferred names in the TADA.CharacteristicName field."))
  }else{print("No deprecated characteristic names found in dataset.")}
  
  .data = .data%>%dplyr::select(-Char_Flag, -Comparable.Name)
  .data = TADA_OrderCols(.data)
  return(.data)
}

#' Create TADA.ComparableDataIdentifier Column
#' 
#' This utility function creates the TADA.ComparableDataIdentifier column by pasting
#' together TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpecificationName,
#' and TADA.ResultMeasure.MeasureUnitCode.
#' 
#' @param .data TADA dataframe 
#' 
#' @return Input TADA dataframe with added TADA.ComparableDataIdentifier column.
#'
#' @export
#' 

TADA_CreateComparableID <- function(.data){
  TADA_CheckColumns(.data, expected_cols = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpecificationName","TADA.ResultMeasure.MeasureUnitCode"))
  .data$TADA.ComparableDataIdentifier = paste(.data$TADA.CharacteristicName,.data$TADA.ResultSampleFractionText, .data$TADA.MethodSpecificationName, .data$TADA.ResultMeasure.MeasureUnitCode,sep = "_")
  return(.data)
}

#' Get TADA spreadsheet template
#' 
#' This function downloads a .xlsx template with all required columns for TADA 
#' to the user's computer to cater custom datasets to the TADA format. 
#' It contains one sample row to guide users when adding their own data to the 
#' spreadsheet. Please note that it downloads the spreadsheet to the user's current
#' working directory.
#' 
#' @return A .xlsx spreadsheet to be saved in the user's working directory.
#' 
#' @export
#' 

TADA_GetTemplate <- function(){
  colsreq = names(TADA::Nutrients_Utah)[!grepl("TADA.",names(TADA::Nutrients_Utah))]
  writexl::write_xlsx(TADA::Nutrients_Utah[1,colsreq], path = "TADATemplate.xlsx")
}


#' Identify and group nearby monitoring locations (UNDER ACTIVE DEVELOPMENT)
#'
#' This function takes a TADA dataset and creates a distance matrix for all sites in the dataset. It then uses the buffer input value to determine which sites are within the buffer and should be identified as part of a nearby site group.
#'
#' @param .data TADA dataframe OR TADA sites dataframe
#' @param dist_buffer Numeric. The maximum distance (in meters) two sites can be from one another to be considered "nearby" and grouped together.
#'
#' @return Input dataframe with one or more TADA.SiteGroup columns (depending upon if a site belongs to more than one nearby group), which identify each site group as "Group_#" for easy filtering. 
#'
#' @export

TADA_FindNearbySites <- function(.data, dist_buffer=100){
  
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  
  # .data required columns
  required_cols <- c("MonitoringLocationIdentifier","TADA.LongitudeMeasure","TADA.LatitudeMeasure")
  # check .data has required columns
  TADA_CheckColumns(.data, required_cols)
  
  # create spatial dataset based on sites
  data_sf = unique(.data[,c("MonitoringLocationIdentifier","TADA.LongitudeMeasure","TADA.LatitudeMeasure")])
  # convert to sf object
  data_sf = sf::st_as_sf(data_sf, coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
                         # Change to your CRS
                         crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  # create a distance matrix in meters
  dist.mat <- data.frame(sf::st_distance(data_sf)) # Great Circle distance since in lat/lon
  
  row.names(dist.mat) = data_sf$MonitoringLocationIdentifier
  colnames(dist.mat) = data_sf$MonitoringLocationIdentifier
  
  # convert distances to those within buffer (1) and beyond buffer (0)
  dist.mat1 = apply(dist.mat, c(1,2), function(x){
    if(x<=dist_buffer){x=1}else{x=0}
  })
  
  # create empty dataframe for groups
  groups = data.frame()
  
  # loop through distance matrix and extract site groups that are within the buffer distance from one another
  for(i in 1:dim(dist.mat1)[1]){
    fsite = rownames(dist.mat1)[i] # focal site
    dat = data.frame(Count = dist.mat1[i,]) # get focal site count row as a column
    dat$MonitoringLocationIdentifier = colnames(dist.mat1) # give df site names along with counts
    sites = dat$MonitoringLocationIdentifier[dat$Count==1] # filter to sites within buffer
    sites1 = sites[!sites%in%fsite] # get site list within buffer that does not include focal site
    if(length(sites1)>0){ # if this list is greater than 0, combine sites within buffer into data frame
      df = data.frame(MonitoringLocationIdentifier = sites, TADA.SiteGroup = paste0(sites, collapse=", "))
      groups = plyr::rbind.fill(groups, df)
    }
  }
  
  # get unique groups (since represented multiple times for each site looped through, above)
  groups = unique(groups)
  
  if(dim(groups)[1]>0){ # if there are groups of nearby sites...
    # create group ID's for easier understanding
    grp = data.frame(TADA.SiteGroup = unique(groups$TADA.SiteGroup), TADA.SiteGroupID = paste0("Group_",1:length(unique(groups$TADA.SiteGroup))))
    groups = merge(groups, grp, all.x = TRUE)
    groups = unique(groups[,!names(groups)%in%c("TADA.SiteGroup")])
    
    # find any sites within multiple groups
    summ_sites = groups%>%dplyr::group_by(MonitoringLocationIdentifier)%>%dplyr::mutate(GroupCount = 1:length(MonitoringLocationIdentifier))
    
    # pivot wider if a site belongs to multiple groups
    groups_wide = merge(groups, summ_sites, all.x = TRUE)
    groups_wide = tidyr::pivot_wider(groups_wide, id_cols = "MonitoringLocationIdentifier",names_from = "GroupCount", names_prefix = "TADA.SiteGroup",values_from = "TADA.SiteGroupID")
    
    # merge data to site groupings
    .data = merge(.data, groups_wide, all.x = TRUE)
  }else{ # if no groups, give a TADA.SiteGroup1 column filled with NA
    .data$TADA.SiteGroup1 = NA
    print("No nearby sites detected using input buffer distance.")
  }
  
  # order columns
  .data = TADA_OrderCols(.data)
  
  # move site id cols to right place
  grpcols = names(.data)[grepl("TADA.SiteGroup",names(.data))]
  
  # relocate site group columns to TADA area
  .data = .data%>%dplyr::relocate(dplyr::all_of(grpcols), .after="TADA.LongitudeMeasure")
  
  return(.data)
}
