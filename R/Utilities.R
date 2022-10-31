#' autoclean
#'
#' Removes complex biological data. Removes non-water media samples.
#' Removes rows of data that are true duplicates. Capitalizes fields to harmonize
#' data. This function includes and runs the TADA "MeasureValueSpecialCharacters" 
#' function as well. 
#'
#' Within "BiologicalIntentName", only the allowable values "tissue", "toxicity",
#' and "NA" apply to non-biological data (the function removes all others).
#' Toxicity and fish tissue data will be kept, but other types of biological
#' monitoring data will not.
#'
#' We decided to make some fields uppercase that way they're more compatible
#' with the WQX validation reference tables and to avoid any issues with
#' case-sensitivity when joining data. Therefore, we might need to tack on any
#' immediate QA steps (removing true duplicates, converting result values to numeric,
#' capitalizing letters, etc.) to this function, as well as the other retrieval functions.
#'
#' @param .data TADA dataframe
#'
#' @return autocleaned TADA data profile
#'

autoclean <- function(.data) {
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if ("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c(
    "ActivityMediaName", "ResultMeasure.MeasureUnitCode",
    "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName"
  ) %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use this TADA function. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if (all(c(
    "ActivityMediaName", "ResultMeasure.MeasureUnitCode",
    "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName"
  ) %in% colnames(.data)) == TRUE) {
    # capitalize fields (just those used w/ ref tables for now)
    .data$CharacteristicName <- toupper(.data$CharacteristicName)
  }
  .data$ResultSampleFractionText <- toupper(.data$ResultSampleFractionText)
  .data$MethodSpecificationName <- toupper(.data$MethodSpecificationName)
  .data$ResultMeasure.MeasureUnitCode <- toupper(.data$ResultMeasure.MeasureUnitCode)
  .data$ActivityMediaName <- toupper(.data$ActivityMediaName)
  .data$DetectionQuantitationLimitMeasure.MeasureUnitCode <-
    toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  # .data$BiologicalIntentName = toupper(.data$BiologicalIntentName)
  
  # Remove duplicate rows
  .data <- .data[!duplicated(.data), ]
  
  # Remove complex biological data
  .data <- dplyr::filter(.data, ActivityMediaName == "WATER")
  # .data = dplyr::filter(.data, BiologicalIntentName != "TISSUE" | "TOXICITY" | is.na(InvalidCoordinates)== TRUE)
  
  # run MeasureValueSpecialCharacters function
  .data <- MeasureValueSpecialCharacters(.data)
  
  #convert 'meters' to 'm'
  .data$ActivityDepthHeightMeasure.MeasureUnitCode[.data$ActivityDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  .data$ActivityTopDepthHeightMeasure.MeasureUnitCode[.data$ActivityTopDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  .data$ActivityBottomDepthHeightMeasure.MeasureUnitCode[.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  .data$ResultDepthHeightMeasure.MeasureUnitCode[.data$ResultDepthHeightMeasure.MeasureUnitCode == 'meters'] <- 'm'
  .data$ResultMeasure.MeasureUnitCode[.data$ResultMeasure.MeasureUnitCode == 'meters'] <- 'm'
  
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
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c("ResultMeasureValue", "DetectionQuantitationLimitMeasure.MeasureValue")
          %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  # execute function after checks are passed
  if (all(c("ResultMeasureValue", "DetectionQuantitationLimitMeasure.MeasureValue")
          %in% colnames(.data)) == TRUE) {
    
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


# to get rid of global variable NOTE in check:
utils::globalVariables(c("AboveWQXUpperThreshold", "ActivityIdentifier",  "ActivityMediaName",
                         "ActivityStartDate", "BelowWQXUpperThreshold", "CharacteristicName",
                         "Conversion.Factor", "Count", "Description", "FieldName", "FieldValue",
                         "MethodSpecificationName", "MonitoringLocationIdentifier",
                         "OrganizationFormalName", "OrganizationIdentifier", "ProjectDescriptionText",
                         "ProjectFileUrl", "ProjectIdentifier",
                         "ProjectMonitoringLocationWeightingUrl", "ProjectName",
                         "QAPPApprovalAgencyName", "QAPPApprovedIndicator",
                         "ResultDetectionConditionText", "ResultMeasureValue",
                         "SamplingDesignTypeCode", "Source", "Status", "TADA.AggregatedContinuousData",
                         "TADA.InvalidCoordinates", "TADA.PotentialDupRowID", "TADA.QAPPDocAvailable",
                         "Target.Unit", "Type", "Value.Unit", "WQX.AnalyticalMethodValidity",
                         "WQX.MethodSpeciationValidity", "WQX.ResultUnitValidity",
                         "WQX.SampleFractionValidity", "YearSummarized", "where"))


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