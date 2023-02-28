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
                         "TADA.InvalidCoordinates.Flag", "TADA.PotentialDupRowIDs.Flag", "QAPPDocAvailable",
                         "Target.Unit", "Type", "Value.Unit", "TADA.AnalyticalMethod.Flag",
                         "TADA.MethodSpeciation.Flag", "TADA.ResultUnit.Flag",
                         "TADA.SampleFraction.Flag", "YearSummarized", "where", "TADA.CharacteristicName",
                         "ResultIdentifier", "TADA.ResultMeasureValue", "n_sites",
                         "n_records", "statecodes_df", "STUSAB" ,"ActivityStartTime.Time", "numorgs", "dup_id",
                         "LatitudeMeasure", "TADA.ResultMeasureValue.DataTypeFlag", "Name", "TADA.Detection_Type",
                         "DetectionQuantitationLimitTypeName", "TADA.Limit_Type", "multiplier", "summ",
                         "LongitudeMeasure"))



#' autoclean
#'
#' Removes rows of data that are true duplicates. Creates new columns with prefix
#' "TADA." and capitalizes fields to harmonize data. This function includes and 
#' runs the TADA "ConvertSpecialChars" function as well.
#'
#' Within "BiologicalIntentName", only the allowable values "tissue", "toxicity",
#' and "NA" apply to non-biological data (the function removes all others).
#' Toxicity and fish tissue data will be kept, but other types of biological
#' monitoring data will not.
#'
#' This function makes certain fields uppercase so that they're interoperable
#' with the WQX validation reference tables and to avoid any issues with
#' case-sensitivity when joining data. This function also performs immediate QA steps 
#' (removes true duplicates, converts result values to numeric, capitalizes 
#' letters, etc.). It can be run as a stand alone function or can be tacked onto
#' other functions.
#'
#' @param .data TADA dataframe
#'
#' @return autocleaned TADA data profile
#'
#' @export 
#'

autoclean <- function(.data) {
  
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  
  # .data required columns
  required_cols <- c(
    "ActivityMediaName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",
    "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName", 
    "DetectionQuantitationLimitMeasure.MeasureUnitCode"
  ) 
  
  # check .data has required columns
  checkColumns(.data, required_cols)
  
  # execute function after checks are passed
  
  # capitalize fields with known synonyms that only differ in caps
  .data$TADA.CharacteristicName <- toupper(.data$CharacteristicName)
  .data$TADA.ResultSampleFractionText <- toupper(.data$ResultSampleFractionText)
  .data$TADA.MethodSpecificationName <- toupper(.data$MethodSpecificationName)
  .data$TADA.ResultMeasure.MeasureUnitCode <- toupper(.data$ResultMeasure.MeasureUnitCode)
  .data$TADA.ActivityMediaName <- toupper(.data$ActivityMediaName)
  .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode <-
    toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  
  # Remove complex biological data. Un-comment after new WQX 3.0 Profiles are released.
  # .data$TADA.BiologicalIntentName = toupper(.data$BiologicalIntentName)
  # TADAProfile = dplyr::filter(TADAProfile, TADA.BiologicalIntentName != "TISSUE" | "TOXICITY" | is.na(TADA.BiologicalIntentName) == TRUE)
  
  # Remove data for non-water media types. Un-comment and add back later once new workflow for 
  # documenting "removed" data is set up
  # TADAProfile <- dplyr::filter(TADAProfile, TADA.ActivityMediaName == "WATER")
  
  # Remove duplicate rows - turned into a test because duplicated() takes a long
  # time acting on all columns in a large dataset.
  if(!length(unique(.data$ResultIdentifier))==dim(.data)[1]){
    print("Duplicate records may be present. Filtering to unique records. This may take a while on large datasets.")
    dup_rids = names(table(.data$ResultIdentifier)[table(.data$ResultIdentifier)>1])
    dup_check = .data%>%dplyr::filter(ResultIdentifier%in%dup_rids)%>%dplyr::group_by(ResultIdentifier)%>%dplyr::distinct()
    not_dups = .data%>%dplyr::filter(!ResultIdentifier%in%dup_rids)
    .data = plyr::rbind.fill(dup_check, not_dups)
  }
  
  # run ConvertSpecialChars function
  # .data <- MeasureValueSpecialCharacters(.data)
  .data <- ConvertSpecialChars(.data, "ResultMeasureValue")
  .data <- ConvertSpecialChars(.data, "DetectionQuantitationLimitMeasure.MeasureValue")
  
  # Move detection limit value and unit to TADA.RV and Unit columns
  .data$TADA.ResultMeasureValue = ifelse(is.na(.data$TADA.ResultMeasureValue)&!is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue),.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue,.data$TADA.ResultMeasureValue)
  .data$TADA.ResultMeasure.MeasureUnitCode = ifelse(is.na(.data$TADA.ResultMeasure.MeasureUnitCode)&!is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode),.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,.data$TADA.ResultMeasure.MeasureUnitCode)
  .data$TADA.ResultMeasureValueDataTypes.Flag = ifelse(.data$TADA.ResultMeasureValueDataTypes.Flag=="ND or NA"&!is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue),"Result Value/Unit Copied from Detection Limit",.data$TADA.ResultMeasureValueDataTypes.Flag)
  
  # change latitude and longitude measures to class numeric
  .data$TADA.LatitudeMeasure <- as.numeric(.data$LatitudeMeasure)
  .data$TADA.LongitudeMeasure <- as.numeric(.data$LongitudeMeasure)
  
  # #convert 'meters' to 'm' - EDH MOVED TO CONVERT DEPTH UNITS
  # .data$TADA.ActivityDepthHeightMeasure.MeasureUnitCode[.data$ActivityDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  # .data$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode[.data$ActivityTopDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  # .data$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode[.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  # .data$TADA.ResultDepthHeightMeasure.MeasureUnitCode[.data$ResultDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  .data$TADA.ResultMeasure.MeasureUnitCode[.data$TADA.ResultMeasure.MeasureUnitCode == 'meters'] <- 'm'
  
  .data = OrderTADACols(.data)
  
  return(.data)
}



#' Check for Special Characters in Measure Value Fields
#'
#' Function checks for special characters and non-numeric values in the
#' ResultMeasureValue and DetectionQuantitationLimitMeasure.MeasureValue
#' fields and appends flag columns indicating if special characters are included
#' and if so, what the special characters are. The ResultMeasureValue and
#' DetectionQuantitationLimitMeasure.MeasureValue fields are also converted to
#' class numeric.
#'
#' @param .data TADA dataframe
#'
#' @return Full dataframe with column indicating presence of special characters in
#' the ResultMeasureValue and DetectionQuantitationLimitMeasure.MeasureValue
#' fields. Additionally, the ResultMeasureValue and
#' DetectionQuantitationLimitMeasure.MeasureValue fields are converted to class
#' numeric, and copies of each column are created to preserve original
#' character values.
#'

MeasureValueSpecialCharacters <- function(.data) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  
  # .data required columns
  required_cols <- c("ResultMeasureValue", "DetectionQuantitationLimitMeasure.MeasureValue")
  # check .data has required columns
  checkColumns(.data, required_cols)
  
  # execute function after checks are passed
  # define check.data
  check.data <- .data
  
  # copy MeasureValue columns to MeasureValue.Original
  check.data$ResultMeasureValue.Original <- check.data$ResultMeasureValue
  check.data$DetectionLimitMeasureValue.Original <-
    check.data$DetectionQuantitationLimitMeasure.MeasureValue
  
  # add TADA.ResultMeasureValue.Flag column
  flag.data <- check.data %>%
    # apply function row by row
    dplyr::rowwise() %>%
    # create flag column
    dplyr::mutate(TADA.ResultMeasureValue.Flag = dplyr::case_when(
      is.na(ResultMeasureValue.Original) ~ as.character("ND or NA"),
      (grepl("<", ResultMeasureValue.Original) == TRUE) ~ as.character("Less Than"),
      (grepl(">", ResultMeasureValue.Original) == TRUE) ~ as.character("Greater Than"),
      (grepl("~", ResultMeasureValue.Original) == TRUE) ~ as.character("Approximate Value"),
      (grepl("[A-Za-z]", ResultMeasureValue.Original) == TRUE) ~ as.character("Text"),
      (grepl("\\d", ResultMeasureValue.Original) == TRUE) ~ as.character("Numeric"),
      TRUE ~ "Coerced to NA"
    ))
  
  # add TADA.DetectionLimitMeasureValue.Flag column
  flag.data <- flag.data %>%
    # apply function row by row
    dplyr::rowwise() %>%
    # create flag column
    dplyr::mutate(TADA.DetectionLimitMeasureValue.Flag = dplyr::case_when(
      is.na(DetectionLimitMeasureValue.Original) ~ as.character(NA),
      (grepl("<", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Less Than"),
      (grepl(">", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Greater Than"),
      (grepl("~", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Approximate Value"),
      (grepl("[A-Za-z]", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Text"),
      (grepl("\\d", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Numeric"),
      TRUE ~ "Coerced to NA"
    ))
  
  # remove special characters before converting to numeric
  flag.data$ResultMeasureValue <- stringr::str_replace_all(
    flag.data$ResultMeasureValue,
    c("<" = "", ">" = "", "~" = "", "," = "")
  )
  flag.data$DetectionQuantitationLimitMeasure.MeasureValue <- stringr::str_replace_all(
    flag.data$DetectionQuantitationLimitMeasure.MeasureValue,
    c("<" = "", ">" = "", "~" = "", "," = "")
  )
  
  # change measure value columns to numeric
  # rename df
  clean.data <- flag.data
  # ResultMeasureValue
  clean.data$ResultMeasureValue <- suppressWarnings(
    as.numeric(clean.data$ResultMeasureValue)
  )
  # DetectionQuantitationLimitMeasure.MeasureValue
  clean.data$DetectionQuantitationLimitMeasure.MeasureValue <-
    suppressWarnings(as.numeric(clean.data$DetectionQuantitationLimitMeasure.MeasureValue))
  
  # reorder columns
  # place flag column next to relevant fields
  clean.data <- clean.data %>%
    dplyr::relocate("ResultMeasureValue.Original",
                    .after = "ResultMeasureValue"
    ) %>%
    dplyr::relocate("TADA.ResultMeasureValue.Flag",
                    .after = "ResultMeasureValue.Original"
    ) %>%
    dplyr::relocate("DetectionLimitMeasureValue.Original",
                    .after = "DetectionQuantitationLimitMeasure.MeasureValue"
    ) %>%
    dplyr::relocate("TADA.DetectionLimitMeasureValue.Flag",
                    .after = "DetectionLimitMeasureValue.Original"
    )
  return(clean.data)
}



#' AutoFilter
#'
#' Function can be used to autofilter and simplify a WQP dataframe.
#' After applying this function, the dataframe will only contain result values for
#' water media types or chemicals in tissue (e.g. mercury in fish tissue).
#' More complex biological data (counts and macroinvertebrates) is removed.
#' The function looks at the following fields to autofilter:
#' ActivityMediaName, ActivityMediaSubDivisionName, AssemblageSampledName
#'
#' @param .data TADA dataframe
#' @param clean Indicates whether flag columns should be appended to the data
#' (clean = FALSE), or flagged data is transformed/filtered from the
#' dataframe and no columns are appended (clean = TRUE).
#'
#' @return When clean = FALSE, flag column is appended to the dataframe. When
#' clean = TRUE, flag column is not appended and relevant rows are removed.
#'

AutoFilter <- function(.data, clean = TRUE) {
  field.names <- colnames(.data)
  
  if (TADAprofileCheck(.data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if (TADAprofileCheck(.data) == TRUE) {
    if (clean == TRUE) {
      # Remove all data where media name does NOT equal WATER (ignore punctuation)
      cleandata <- dplyr::filter(.data, ActivityMediaName == "Water")
      
      return(cleandata)
    }
    
    if (clean == FALSE) {
      # NEED TO EDIT TO ADD FLAGS, currently removes other water Water
      flagdata <- dplyr::filter(.data, ActivityMediaName == "water")
      
      return(flagdata)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}



#' RemoveEmptyColumns
#'
#' Removes any columns with only NA values. Used to quickly reduce the number of
#' columns in a dataframe to improve management and readability of the dataframe.
#'
#' @param .data Dataframe
#'
#' @return Full dataframe with empty data columns removed
#'

RemoveEmptyColumns <- function(.data) {
  # Remove columns with only NAs
  .data %>%
    dplyr::select(where(~ !all(is.na(.x))))
}



#' decimalplaces
#'
#' for numeric data type
#'
#' @param x Numeric data field from TADA profile
#'
#' @return Number of values to the right of the decimal point for numeric type data.
#'

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}



#' decimalnumcount
#'
#' for character data type
#'
#' @param x Numeric data field from TADA profile
#'
#' @return Number of values to the right of the decimal point for character type data.
#' 

decimalnumcount <- function(x) {
  stopifnot(class(x) == "character")
  x <- gsub("(.*)(\\.)|(*$)", "", x)
  nchar(x)
}


#' TADA Profile Check
#'
#' This function checks if the column names in a dataframe include the TADA
#' profile fields. It is used at the beginning of TADA functions to ensure the
#' input data frame is suitable (i.e. is either the full physical/chemical
#' results profile downloaded from WQP or the TADA profile template downloaded
#' from the EPA TADA webpage.)
#'
#' @param .data A dataframe
#'
#' @return Boolean result indicating whether or not the input dataframe contains
#' all of the TADA profile fields.
#'

TADAprofileCheck <- function(.data) {
  TADA.fields <- c(
    "OrganizationIdentifier", "OrganizationFormalName",
    "ActivityIdentifier", "ActivityTypeCode",
    "ActivityMediaName", "ActivityMediaSubdivisionName",
    "ActivityStartDate", "ActivityStartTime.Time",
    "ActivityStartTime.TimeZoneCode", "ActivityEndDate",
    "ActivityEndTime.Time", "ActivityEndTime.TimeZoneCode",
    "ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode",
    "ActivityDepthAltitudeReferencePointText", "ActivityTopDepthHeightMeasure.MeasureValue",
    "ActivityTopDepthHeightMeasure.MeasureUnitCode", "ActivityBottomDepthHeightMeasure.MeasureValue",
    "ActivityBottomDepthHeightMeasure.MeasureUnitCode", "ProjectIdentifier",
    "ActivityConductingOrganizationText", "MonitoringLocationIdentifier",
    "ActivityCommentText", "SampleAquifer",
    "HydrologicCondition", "HydrologicEvent",
    "SampleCollectionMethod.MethodIdentifier", "SampleCollectionMethod.MethodIdentifierContext",
    "SampleCollectionMethod.MethodName", "SampleCollectionEquipmentName",
    "ResultDetectionConditionText", "CharacteristicName",
    "ResultSampleFractionText", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode", "MeasureQualifierCode",
    "ResultStatusIdentifier", "StatisticalBaseCode",
    "ResultValueTypeName", "ResultWeightBasisText",
    "ResultTimeBasisText", "ResultTemperatureBasisText",
    "ResultParticleSizeBasisText", "PrecisionValue",
    "ResultCommentText", "USGSPCode",
    "ResultDepthHeightMeasure.MeasureValue", "ResultDepthHeightMeasure.MeasureUnitCode",
    "ResultDepthAltitudeReferencePointText", "SubjectTaxonomicName",
    "SampleTissueAnatomyName", "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext", "ResultAnalyticalMethod.MethodName",
    "MethodDescriptionText", "LaboratoryName",
    "AnalysisStartDate", "ResultLaboratoryCommentText",
    "DetectionQuantitationLimitTypeName", "DetectionQuantitationLimitMeasure.MeasureValue",
    "DetectionQuantitationLimitMeasure.MeasureUnitCode", "PreparationStartDate",
    "ProviderName", "ActivityStartDateTime", "ActivityEndDateTime"
  )
  
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  
  if (all(TADA.fields %in% colnames(.data)) == TRUE) {
    TRUE
  } else {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
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

checkType <- function(arg, type, paramName) {
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

checkColumns <- function(.data, expected_cols) {
  if (all(expected_cols %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
}



#' ConvertSpecialChars
#' 
#' This function will screen a column of the user's choice for special characters.
#' It creates a new column that describes the content of the column prior to
#' conversion to numeric. It also creates a new column to hold the new, numeric
#' column
#' 
#' @param .data A TADA profile object
#' @param col A character column to be converted to numeric
#' 
#' @return Returns the original dataframe with two new columns: the input column 
#' with the prefix "TADA.", which holds the numeric form of the original column, 
#' and "TADA.ResultValueDataTypes.Flag", which has text describing the type of data
#' contained within the column of interest, including "Numeric","Less Than" (<), "Greater Than" (>),
#' "Approximate Value" (~), "Text" (A-z), "Percentage" (%), and "Comma-Separated Numeric" (#,###)
#' 
#' @export
#' 

ConvertSpecialChars <- function(.data,col){
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
  if(class(chars.data$orig)=="numeric"){
    clean.data = chars.data%>%
      dplyr::mutate(flag = dplyr::case_when(
        is.na(masked) ~ as.character("ND or NA"),
        TRUE ~ as.character("Numeric")
      ))
  }else{
    # Detect special characters in column and populate new flag column with descriptor
    # of the specific type of character/data type
    clean.data = chars.data%>%
      dplyr::mutate(flag = dplyr::case_when(
        is.na(masked) ~ as.character("ND or NA"),
        (!is.na(suppressWarnings(as.numeric(masked)) == TRUE)) ~ as.character("Numeric"),
        (grepl("<", masked) == TRUE) ~ as.character("Less Than"),
        (grepl(">", masked) == TRUE) ~ as.character("Greater Than"),
        (grepl("~", masked) == TRUE) ~ as.character("Approximate Value"),
        (grepl("[A-Za-z]", masked) == TRUE) ~ as.character("Text"),
        (grepl("%", masked) == TRUE) ~ as.character("Percentage"),
        (grepl(",", masked) == TRUE) ~ as.character("Comma-Separated Numeric"),
        TRUE ~ "Coerced to NA"
      ))
    
    # In the new TADA column, convert to numeric and remove some specific special 
    # characters.
    clean.data$masked = suppressWarnings(as.numeric(stringr::str_replace_all(
      clean.data$orig,c("<" = "", ">" = "", "~" = "", "," = "","%" = ""))))
  }
  
  # Rename to original column name, TADA column name, and flag column name
  names(clean.data)[names(clean.data)=="orig"] = col
  names(clean.data)[names(clean.data)=="masked"] = numcol
  names(clean.data)[names(clean.data)=="flag"] = flagcol
  
  clean.data = OrderTADACols(clean.data)
  
  return(clean.data)
}

#' Order TADA Columns
#' 
#' This utility function moves all TADA-created columns to the end of the dataframe 
#' in an order that improves readability. 
#' 
#' @param .data TADA dataframe 
#' 
#' @return A TADA handled dataframe with the TADA-created columns at the end.
#'
#' @export
#' 
#' 

OrderTADACols <- function(.data){
  cols = c("TADA.LatitudeMeasure",
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
           "TADA.Censored_Flag",
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
           "TADA.PotentialDupRowIDs.Flag"
  )
  
  focal_cols = cols[cols%in%names(.data)]
  other_cols = names(.data)[!names(.data)%in%focal_cols]
  
  rearranged = .data%>%dplyr::relocate(dplyr::any_of(focal_cols),.after = dplyr::last_col())
  
  return(rearranged)
  
}