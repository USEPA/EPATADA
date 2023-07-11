#' Check for Invalid Analytical Methods
#'
#' Function checks the validity of each characteristic-analytical method
#' combination in the dataframe. When clean = TRUE, rows with invalid
#' characteristic-analytical method combinations are removed. Default is
#' clean = TRUE. When errorsonly = TRUE, dataframe is filtered to show only
#' invalid characteristic-analytical method combinations. Default is
#' errorsonly = FALSE.
#' 
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-analytical
#' method combinations from the dataframe when clean = TRUE. Default is
#' clean = TRUE.
#' @param errorsonly Boolean argument; filters dataframe to show only "Invalid"
#' characteristic-analytical method combinations when errorsonly = TRUE. Default
#' is errorsonly = FALSE.
#' 
#' @return This function adds the TADA.AnalyticalMethod.Flag to a TADA dataframe. This column 
#' flags invalid CharacteristicName, ResultAnalyticalMethod/MethodIdentifier,
#' and ResultAnalyticalMethod/MethodIdentifierContext combinations in your dataframe
#' as either "Nonstandardized", "Invalid", or "Valid". When clean = FALSE and 
#' errorsonly = TRUE, the dataframe is filtered to show only "Invalid" 
#' characteristic-analytical method combinations; the column TADA.AnalyticalMethod.Flag
#' is still appended. When clean = TRUE and errorsonly = FALSE, "Invalid" rows 
#' are removed from the dataframe and no column will be appended.
#' 
#' @export
#'
#' @examples 
#' # Load example dataset
#' data(Data_Nutrients_UT)
#' 
#' # Remove invalid characteristic-analytical method combinations from dataframe:
#' InvalidMethod_clean <- TADA_FlagMethod(Data_Nutrients_UT)
#' 
#' # Flag, but do not remove, invalid characteristic-analytical method combinations
#' # in new column titled "TADA.AnalyticalMethod.Flag":
#' InvalidMethod_flags <- TADA_FlagMethod(Data_Nutrients_UT, clean = FALSE)
#' 
#' # Show only invalid characteristic-analytical method combinations:
#' InvalidMethod_errorsonly <- TADA_FlagMethod(Data_Nutrients_UT, clean = FALSE, errorsonly = TRUE)
#' 

TADA_FlagMethod <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "TADA.CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext"
  )
  TADA_CheckColumns(.data, required_cols)
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # execute function after checks are passed - removes flag column in case reference table has changed.
  # delete existing flag column
  if (("TADA.AnalyticalMethod.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.AnalyticalMethod.Flag)
  }
  # read in speciation reference table from sysdata.rda and filter
  meth.ref <- TADA_GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicMethod")
  
  # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
  check.data <- merge(.data, meth.ref[, c("Characteristic", "Source", "Status", "Value")],
                      by.x = c(
                        "TADA.CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier",
                        "ResultAnalyticalMethod.MethodIdentifierContext"
                      ),
                      by.y = c("Characteristic", "Value", "Source"), all.x = TRUE
  )
  
  # rename Status column to WQX.AnalyticalMethodValidity
  check.data <- check.data %>%
    dplyr::rename(TADA.AnalyticalMethod.Flag = Status) %>% dplyr::distinct()
  # rename NA values to Nonstandardized in WQX.AnalyticalMethodValidity column
  check.data["TADA.AnalyticalMethod.Flag"][is.na(check.data["TADA.AnalyticalMethod.Flag"])] <- "Nonstandardized"
  
  if (errorsonly == FALSE) {
    
    #if all rows are "Valid" or NA "Nonstandardized", return input unchanged
    ##note: Cristina edited this on 9/19/22 to keep Nonstandardized/NA data when clean = TRUE. Now only Invalid data is removed.
    if (any("Invalid" %in%
            unique(check.data$TADA.AnalyticalMethod.Flag)) == FALSE) {
      print("No invalid method/characteristic combinations in your dataframe. Returning the input dataframe with TADA.AnalyticalMethod.Flag column for tracking.")
      check.data <- TADA_OrderCols(check.data)
      return(check.data)
    }
    
    # flagged output, all data
    if (clean == FALSE) {
      check.data <- TADA_OrderCols(check.data)
      return(check.data)
    }
    
    # clean output
    if (clean == TRUE) {
      # filter out invalid characteristic-unit-method combinations
      clean.data <- dplyr::filter(check.data, TADA.AnalyticalMethod.Flag != "Invalid")
      
      # remove WQX.AnalyticalMethodValidity column
      # clean.data <- dplyr::select(clean.data, -TADA.AnalyticalMethod.Flag)
      clean.data <- TADA_OrderCols(clean.data)
      return(clean.data)
    }
  }

  # flagged output, errors only
  if (clean == FALSE & errorsonly == TRUE) {
    # filter to show only invalid characteristic-unit-method combinations
    invalid.data <- dplyr::filter(check.data, TADA.AnalyticalMethod.Flag == "Invalid")
    if (nrow(invalid.data) == 0) {
      # invalid.data <- dplyr::select(invalid.data, -TADA.AnalyticalMethod.Flag)
      print("This dataframe is empty because we did not find any invalid method/characteristic combinations in your dataframe")
    }
    invalid.data <- TADA_OrderCols(invalid.data)
    return(invalid.data)
  }
}



#' Check for Aggregated Continuous Data
#'
#' The Water Quality Portal (WQP) is not currently designed to store high-frequency
#' sensor data as typical result values. However, sometimes data providers 
#' choose to aggregate their continuous data to a daily avg, max, or min value, 
#' and then submit that aggregated data to the WQP through WQX. Alternatively, 
#' some organizations aggregate their high frequency data (15 min or 1 hour data) 
#' to 2 or 4 hour interval averages, and they also submit that data to the WQP through WQX.
#' This type of high frequency data may (or may not) be suitable for integration with discrete 
#' water quality data for assessments. Therefore, this function uses metadata 
#' submitted by data providers to flag rows with aggregated continuous data. 
#' This is done by flagging results where the ResultDetectionConditionText = 
#' "Reported in Raw Data (attached)". When clean = FALSE and errorsonly = FALSE, a column titled 
#' "TADA.AggregatedContinuousData.Flag" is added to the dataframe to indicate if the row 
#' includes aggregated continuous data, "Y", or not,  "N". When clean = FALSE and
#' errorsonly = TRUE, the dataframe will be filtered to show only the rows flagged 
#' "Y" for aggregated continuous data. When clean = TRUE and errorsonly = FALSE,
#' rows with aggregated continuous data are removed from the dataframe and no 
#' column will be appended. When clean = TRUE and errorsonly = TRUE, the function 
#' does not execute and an error message is returned. The default is clean = TRUE
#' and errorsonly = FALSE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes aggregated continuous data from
#' the dataframe when clean = TRUE. Default is clean = TRUE.
#' @param errorsonly Boolean argument; filters dataframe to show only aggregated
#' continuous data when errorsonly = TRUE. Default is errorsonly = FALSE.
#'
#' @return When clean = FALSE and errorsonly = FALSE, a column flagging rows with 
#' aggregated continuous data is appended to the input data set. When clean = FALSE
#' and errorsonly = TRUE, the dataframe is filtered to show only the flagged
#' aggregated continuous data and flag column is still appended. When clean = TRUE
#' and errorsonly = FALSE, aggregated continuous data is removed from the dataframe
#' and no column is appended. The default is clean = TRUE and errorsonly = FALSE.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset
#' data(Data_Nutrients_UT)
#' 
#' # Remove aggregated continuous data from dataframe:
#' AggContinuous_clean <- TADA_FindContinuousData(Data_Nutrients_UT)
#' 
#' # Flag, but do not remove, aggregated continuous data in new column
#' # titled "TADA.AggregatedContinuousData.Flag":
#' AggContinuous_flags <- TADA_FindContinuousData(Data_Nutrients_UT, clean = FALSE)
#' 
#' # Show only rows flagged for aggregated continuous data:
#' AggContinuous_errorsonly <- TADA_FindContinuousData(Data_Nutrients_UT, 
#' clean = FALSE, errorsonly = TRUE)
#' 

TADA_FindContinuousData <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, "ResultDetectionConditionText")
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }
  
  # set default flag to "unknown"
  .data$TADA.AggregatedContinuousData.Flag = "Unknown"

  # execute function after checks are passed
  # flag continuous data
  # make cont.data data frame
  # with new profiles might want to check for zip files? Do these columns show up in TADA_DataRetrieval?
  cont.data = .data%>%dplyr::filter((ActivityTypeCode=="Field Msr/Obs"&ResultDetectionConditionText=="Reported in Raw Data (attached)")|(ActivityTypeCode=="Field Msr/Obs"&SampleCollectionEquipmentName=="Probe/Sensor"&!is.na(ResultTimeBasisText)&!is.na(StatisticalBaseCode)&ResultValueTypeName=="Calculated"))
  
  # everything not in cont dataframe
  noncont.data = subset(.data, !.data$ResultIdentifier%in%cont.data$ResultIdentifier)
  
  # if there is aggregated continuous data is in the data set
  if (nrow(cont.data) != 0) {
    # change contents of ContDataFlag column
    cont.data$TADA.AggregatedContinuousData.Flag <- "Y"
    # join cont.data to flag.data
    flag.data <- plyr::rbind.fill(cont.data, noncont.data)
    
    # flagged output, all data
    if (clean == FALSE & errorsonly == FALSE) {
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    
    # clean output
    if (clean == TRUE & errorsonly == FALSE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(flag.data, !(TADA.AggregatedContinuousData.Flag %in% "Y"))
      
      # remove TADA.AggregatedContinuousData column
      # clean.data <- dplyr::select(clean.data, -TADA.AggregatedContinuousData.Flag)
      clean.data <- TADA_OrderCols(clean.data)
      return(clean.data)
    }
    
    # flagged output, only aggregated continuous data
    if (clean == FALSE & errorsonly == TRUE) {
      #filter to show only invalid characteristic-unit-media combinations
      aggcont.data <- dplyr::filter(flag.data, TADA.AggregatedContinuousData.Flag == "Y")
      aggcont.data <- TADA_OrderCols(aggcont.data)
      return(aggcont.data)
    }
  }
  
  # if no aggregated continuous data is in the data set
  if (nrow(cont.data) == 0) {
    if (errorsonly == FALSE) {
      print("No evidence of aggregated continuous data in your dataframe. Returning the input dataframe with TADA.AggregatedContinuousData.Flag column for tracking.")
      .data <- TADA_OrderCols(.data)
      return(.data)
    }
    
    if (errorsonly == TRUE) {
      print("This dataframe is empty because we did not find any aggregated continuous data in your dataframe")
      cont.data <- TADA_OrderCols(cont.data)
      return(cont.data)
    }
  }
}

#' Check Result Value Against WQX Upper Threshold
#'
#' EPA's Water Quality Exchange (WQX) has generated statistics and data from
#' millions of water quality data points around the country. This function
#' leverages the statistical data from WQX to flag any data that is above the
#' upper threshold of result values submitted to WQX for a given characteristic.
#' When clean = FALSE and errorsonly = FALSE, a column which flags data above 
#' the upper WQX threshold is appended to the dataframe. When clean = FALSE and 
#' errorsonly = TRUE, the dataframe is filtered to show only data found above 
#' the WQX threshold. When clean = TRUE and errorsonly = FALSE, rows with values 
#' that are above the upper WQX threshold are removed from the dataframe and no 
#' column is appended. When clean = TRUE and and errorsonly = TRUE, the function 
#' is not executed and an error message is returned. Defaults are clean = TRUE 
#' and errorsonly = FALSE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data that is above the upper WQX
#' threshold from the dataframe when clean = TRUE. Default is clean = TRUE.
#' @param errorsonly Boolean argument; filters dataframe to show only the data
#' flagged as above the upper WQX threshold. Default is errorsonly = FALSE.
#'
#' @return The input TADA dataset with the added
#'   TADA.AboveNationalWQXUpperThreshold column. When clean = FALSE and
#'   errorsonly = TRUE, the dataframe is filtered to show only data found above
#'   the WQX threshold. When clean = TRUE and errorsonly = FALSE, rows with
#'   values that are above the upper WQX threshold are removed from the
#'   dataframe and no column is appended. When clean = TRUE and and 
#'   errorsonly = TRUE, the function is not executed and an error message 
#'   is returned. Defaults are clean = TRUE and errorsonly = FALSE.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' 
#' # Remove data that is above the upper WQX threshold from dataframe:
#' WQXUpperThreshold_clean <- TADA_FlagAboveThreshold(Data_Nutrients_UT)
#' 
#' # Flag, but do not remove, data that is above the upper WQX threshold in
#' # new column titled "TADA.ResultValueAboveUpperThreshold.Flag":
#' WQXUpperThreshold_flags <- TADA_FlagAboveThreshold(Data_Nutrients_UT, clean = FALSE)
#' 
#' # Show only data flagged as above the upper WQX threshold:
#' WQXUpperThreshold_flagsonly <- TADA_FlagAboveThreshold(Data_Nutrients_UT, 
#' clean = FALSE, errorsonly = TRUE)
#' 

TADA_FlagAboveThreshold <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, required_cols)
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }

  # execute function after checks are passed

  # delete existing flag column - removes flag column in case reference table has changed.
  if (("TADA.ResultValueAboveUpperThreshold.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ResultValueAboveUpperThreshold.Flag)
  }
  
  # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
  unit.ref <- TADA_GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" &
                    Status == "Valid")
  
  # join unit.ref to raw.data
  check.data <- merge(.data, unit.ref[, c(
    "Characteristic", "Source",
    "Value", "Maximum"
  )],
  by.x = c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName",
    "TADA.ResultMeasure.MeasureUnitCode"
  ),
  by.y = c("Characteristic", "Source", "Value"), all.x = TRUE
  )
  
  # Create flag column, flag rows where ResultMeasureValue > Maximum
  flag.data <- check.data %>%
    # create flag column
    dplyr::mutate(TADA.ResultValueAboveUpperThreshold.Flag = dplyr::case_when(
      TADA.ResultMeasureValue >= Maximum ~ as.character("Y"),
      TADA.ResultMeasureValue < Maximum ~ as.character("N"),
      TRUE ~ as.character("No threshold available") # this occurs when the char/unit combo is not in the table
    ))
  
  # remove extraneous columns, fix field names
  flag.data <- flag.data %>%
    dplyr::select(-"Maximum") %>% dplyr::distinct()
  
  # if no data above WQX threshold is found
  if (any("Y" %in%
          unique(flag.data$TADA.ResultValueAboveUpperThreshold.Flag)) == FALSE) {
    if (errorsonly == FALSE) {
      print("No data above the WQX Upper Threshold was found in your dataframe. Returning the input dataframe with TADA.ResultAboveUpperThreshold.Flag column for tracking.")
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    if (errorsonly == TRUE) {
      print("This dataframe is empty because no data above the WQX Upper Threshold was found in your dataframe")
      emptyflag.data <- dplyr::filter(flag.data, TADA.ResultValueAboveUpperThreshold.Flag %in% "Y")
      # emptyflag.data <- dplyr::select(emptyflag.data, -TADA.ResultValueAboveUpperThreshold.Flag)
      emptyflag.data <- TADA_OrderCols(emptyflag.data)
      return(emptyflag.data)
    }
  }
  
  # flagged, all data
  if (clean == FALSE & errorsonly == FALSE) {
    flag.data <- TADA_OrderCols(flag.data)
    return(flag.data)
  }
  
  # clean data
  if (clean == TRUE & errorsonly == FALSE) {
    # filter out rows where TADA.ResultValueAboveUpperThreshold.Flag = Y; remove TADA.ResultValueAboveUpperThreshold.Flag column
    clean.data <- flag.data %>%
      dplyr::filter(!(TADA.ResultValueAboveUpperThreshold.Flag %in% "Y")) #%>%
      # dplyr::select(-TADA.ResultValueAboveUpperThreshold.Flag)
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }
  
  # flagged, errors only
  if (clean == FALSE & errorsonly == TRUE) {
    # filter to show only rows above WQX upper threshold
    flagsonly.data <- flag.data %>%
      dplyr::filter(TADA.ResultValueAboveUpperThreshold.Flag %in% "Y")
    flagsonly.data <- TADA_OrderCols(flagsonly.data)
    return(flagsonly.data)
  }
}



#' Check Result Value Against WQX Lower Threshold
#'
#' EPA's Water Quality Exchange (WQX) has generated statistics and data from
#' millions of water quality data points around the country. This functions
#' leverages that statistical data from WQX to flag any data that is below the
#' lower threshold of result values submitted to WQX for a given characteristic.
#' When clean = TRUE, rows with values that are below the lower WQX threshold
#' are removed from the dataframe and no column will be appended. Default is
#' clean = TRUE. When errorsonly = TRUE, the dataframe is filtered to show only
#' rows with values that are flagged below the lower WQX threshold. Default is 
#' errorsonly = FALSE
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data that is below the lower WQX
#' threshold from the dataframe when clean = TRUE. Default is clean = TRUE.
#' @param errorsonly Boolean argument; filters data to show only the values
#' flagged as below the lower WQX threshold when errorsonly = TRUE. Default is
#' errorsonly = FALSE.
#'
#' @return The input TADA dataset with the added
#'   TADA.BelowNationalWQXLowerThreshold column. This column flags rows with
#'   data that are below the lower WQX threshold. When clean = FALSE and
#'   errorsonly = TRUE, the dataframe is filtered to show only the rows which
#'   are flagged as below the WQX threshold; the column
#'   TADA.ResultValueBelowLowerThreshold.Flag is still appended. When clean =
#'   TRUE and errorsonly = FALSE, data that is below the lower WQX threshold is
#'   removed from the dataframe. When clean = TRUE and errorsonly = TRUE, the
#'   function does not execute and an error message is returned. The defaults
#'   are clean = TRUE and errorsonly = FALSE.
#' 
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' 
#' # Remove data that is below the lower WQX threshold from the dataframe:
#' WQXLowerThreshold_clean <- TADA_FlagBelowThreshold(Data_Nutrients_UT)
#' 
#' # Flag, but do not remove, data that is below the lower WQX threshold in
#' # new column titled "TADA.ResultValueBelowLowerThreshold.Flag":
#' WQXLowerThreshold_flags <- TADA_FlagBelowThreshold(Data_Nutrients_UT, clean = FALSE)
#' 
#' # Show only data that is below the lower WQX threshold:
#' WQXLowerThreshold_flagsonly <- TADA_FlagBelowThreshold(Data_Nutrients_UT,
#' clean = FALSE, errorsonly = TRUE)
#' 

TADA_FlagBelowThreshold <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, required_cols)
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }

  # execute function after checks are passed - removes flag column in case reference table has changed.
  # delete existing flag column
  if (("TADA.ResultValueBelowLowerThreshold.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ResultValueBelowLowerThreshold.Flag)
  }
  
  # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
  unit.ref <- TADA_GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" &
                    Status == "Valid")
  
  # join unit.ref to raw.data
  check.data <- merge(.data, unit.ref[, c(
    "Characteristic", "Source",
    "Value", "Minimum"
  )],
  by.x = c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName",
    "TADA.ResultMeasure.MeasureUnitCode"
  ),
  by.y = c("Characteristic", "Source", "Value"), all.x = TRUE
  )
  
  # Create flag column, flag rows where TADA.ResultMeasureValue < Minimum
  flag.data <- check.data %>%
    # create flag column
    dplyr::mutate(TADA.ResultValueBelowLowerThreshold.Flag = dplyr::case_when(
      TADA.ResultMeasureValue <= Minimum ~ as.character("Y"),
      TADA.ResultMeasureValue > Minimum ~ as.character("N"),
      TRUE ~ as.character("No threshold available") # this occurs when the char/unit combo is not in the table
    ))
  
  # remove extraneous columns, fix field names
  flag.data <- flag.data %>%
    dplyr::select(-"Minimum") %>% dplyr::distinct()
  
  # if no data below WQX lower threshold is found
  if (any("Y" %in%
          unique(flag.data$TADA.ResultValueBelowLowerThreshold.Flag)) == FALSE) {
    if (errorsonly == FALSE) {
      print("No data below the WQX Lower Threshold were found in your dataframe. Returning the input dataframe with TADA.ResultValueBelowLowerThreshold.Flag column for tracking.")
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    if (errorsonly == TRUE) {
      print("This dataframe is empty because no data below the WQX Lower Threshold was found in your dataframe")
      emptyflag.data <- dplyr::filter(flag.data, TADA.ResultValueBelowLowerThreshold.Flag %in% "Y")
      # emptyflag.data <- dplyr::select(emptyflag.data, -TADA.ResultValueBelowLowerThreshold.Flag)
      emptyflag.data <- TADA_OrderCols(emptyflag.data)
      return(emptyflag.data)
    }
  }
  
  # flagged, all data
  if (clean == FALSE & errorsonly == FALSE) {
    flag.data = TADA_OrderCols(flag.data)
    return(flag.data)
  }
  
  # clean data
  if (clean == TRUE & errorsonly == FALSE) {
    # filter out rows where TADA.ResultValueBelowLowerThreshold.Flag = Y; remove TADA.ResultValueBelowLowerThreshold.Flag column
    clean.data <- flag.data %>%
      dplyr::filter(!(TADA.ResultValueBelowLowerThreshold.Flag %in% "Y")) #%>%
      # dplyr::select(-TADA.ResultValueBelowLowerThreshold.Flag)
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }
  
  # only flagged data
  if (clean == FALSE & errorsonly == TRUE) {
    # filter to show only rows where TADA.ResultValueBelowLowerThreshold.Flag = Y
    flagsonly.data <- flag.data %>%
      dplyr::filter(TADA.ResultValueBelowLowerThreshold.Flag %in% "Y")
    flagsonly.data <- TADA_OrderCols(flagsonly.data)
    return(flagsonly.data)
  }
}



#' Check data for an approved QAPP
#'
#' Function checks data submitted under the column "QAPPApprovedIndicator".
#' Some organizations submit data for this field to indicate if the data
#' produced has an approved Quality Assurance Project Plan (QAPP) or not.
#' Y indicates yes, N indicates no.  This function has three default inputs:
#' clean = TRUE, cleanNA = FALSE, and errorsonly == FALSE. The default flags 
#' rows of data where the QAPPApprovedIndicator equals "N". Users could 
#' remove NA's in addition to N's using the inputs clean = TRUE, cleanNA = TRUE,
#' and errorsonly = FALSE. If errorsonly = TRUE, the function will filter out all
#' rows where the QAPPApprovedIndicator is 'Y'. If clean = FALSE, cleanNA = FALSE, 
#' and errorsonly = FALSE, the function will not make any changes to the data.
#'
#' Note: This is not a required field, so it is often left blank (NA) even if
#' the data has an associated QAPP. All states and tribes that collect
#' monitoring data using 106 funding (almost all state and tribal data in WQX)
#' are required to have an EPA approved QAPP to receive 106 funding. Therefore,
#' most of these organizations data has an approved QAPP even if the data
#' submitted to WQP is NA.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument with two possible values called "TRUE" and
#' "FALSE". When clean=TRUE, rows of data where the QAPPApprovedIndicator equals
#' "N" will be removed. When, clean=FALSE, rows of data where the
#' QAPPApprovedIndicator equals "N" will be retained.
#' @param cleanNA Boolean argument with two possible values called "TRUE" and
#' "FALSE". When cleanNA=TRUE, rows of data where the QAPPApprovedIndicator
#' equals "NA" will be removed. When, cleanNA=FALSE, rows of data where the
#' the QAPPApprovedIndicator equals "NA" will be retained.
#' @param errorsonly Boolean argument; when errorsonly = TRUE, the dataframe will
#' be filtered to remove any rows where the QAPPApprovedIndicator equals "Y".
#'
#' @return Several combinations of inputs are possible:
#' When clean = TRUE, cleanNA = FALSE, and errorsonly = FALSE, the dataframe will
#' be filtered to show only rows where QAPPAprrovedIndicator is "Y" or "NA";
#' When clean = TRUE, cleanNA = TRUE, and errorsonly = FALSE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "Y";
#' When clean = FALSE, cleanNA = TRUE, and errorsonly = FALSE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "Y" or "N";
#' When clean = FALSE, cleanNA = FALSE, and errorsonly = FALSE, no rows are
#' removed from the dataframe;
#' When clean = TRUE, cleanNA = TRUE, and errorsonly = TRUE, the function will
#' not execute and an error message will be returned;
#' When clean = TRUE, cleanNA = FALSE, and errorsonly = TRUE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "NA";
#' When clean = FALSE, cleanNA = TRUE, and errorsonly = TRUE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "N";
#' When clean = FALSE, cleanNA = FALSE, and errorsonly = TRUE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "N" or "NA"
#' 
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' 
#' # Show data where the QAPPApprovedIndicator equals "Y" or "NA":
#' QAPPapproved_clean <- TADA_FindQAPPApproval(Data_Nutrients_UT)
#' 
#' # Show only data where the QAPPApprovedIndicator equals "Y":
#' QAPPapproved_cleanNAs <- TADA_FindQAPPApproval(Data_Nutrients_UT, cleanNA = TRUE)
#' 
#' # Show data where the QAPPApprovedIndicator equals "N" or "NA":
#' QAPPIndicator_N_NA <- TADA_FindQAPPApproval(Data_Nutrients_UT, clean = FALSE, 
#' cleanNA = FALSE, errorsonly = TRUE)
#' 
#' # Show data where the QAPPApprovedIndicator equals "N":
#' QAPPIndicator_N <- TADA_FindQAPPApproval(Data_Nutrients_UT, clean = FALSE, 
#' cleanNA = TRUE, errorsonly = TRUE)
#'
#' # Note: When clean = FALSE, cleanNA = FALSE, and errorsonly = FALSE, no data is removed
#' # Note: When clean = TRUE, cleanNA = TRUE, and errorsonly = TRUE, an error message is returned
#' 

TADA_FindQAPPApproval <- function(.data, clean = FALSE, cleanNA = FALSE, errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check cleanNA is boolean
  TADA_CheckType(cleanNA, "logical") 
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, "QAPPApprovedIndicator")
  # check that clean, cleanNA and errorsonly are not all TRUE
  if (clean == TRUE & cleanNA == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean, cleanNA, and errorsonly cannot all be TRUE")
  }

  # execute function after checks are passed
  
  # if errorsonly = FALSE
  if (errorsonly == FALSE) {
    if (clean == TRUE) {
      .data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE | QAPPApprovedIndicator == "Y")
      
      if (nrow(.data) == 0) {
        print("All QAPPApprovedIndicator data is N")
      }
    }
    if (cleanNA == TRUE) {
      .data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == FALSE)
      
      if (nrow(.data) == 0 & clean == TRUE) {
        print("All QAPPApprovedIndicator data is NA or N")
      } else if (nrow(.data) == 0 & clean == FALSE) {
        print("All QAPPApprovedIndicator data is NA")
      }
    }
    if (clean == FALSE & cleanNA == FALSE) {
      print("Data is flagged but not removed because clean and cleanNA were FALSE")
    }
    .data = TADA_OrderCols(.data)
    return(.data)
  }
  
  # if errorsonly = TRUE
  if (errorsonly == TRUE & clean == TRUE & cleanNA == FALSE) {
    NA.data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE)
    if (nrow(NA.data) == 0) {
      warning("All QAPPApprovedIndicator data is 'Y' or 'N'")
    }
    NA.data = TADA_OrderCols(NA.data)
    return(NA.data)
  }
  if (errorsonly == TRUE & clean == FALSE & cleanNA == TRUE) {
    N.data <- dplyr::filter(.data, QAPPApprovedIndicator == "N")
    if (nrow(N.data) == 0) {
      warning("All QAPPApprovedIndicator data is NA or 'Y'")
    }
    N.data <- TADA_OrderCols(N.data)
    return(N.data)
  }
  if (errorsonly == TRUE & clean == FALSE & cleanNA == FALSE) {
    NAorN.data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE | QAPPApprovedIndicator == "N")
    if (nrow(NAorN.data) == 0) {
      warning("All QAPPApprovedIndicator data is 'Y'")
    }
    NAorN.data <- TADA_OrderCols(NAorN.data)
    return(NAorN.data)
  }
}



#' Check if an approved QAPP document URL is provided
#'
#' Function checks data submitted under the "ProjectFileUrl" column
#' to determine if a QAPP document is available to review. When clean = FALSE,
#' a column will be appended to flag results that have an associated
#' QAPP document URL provided. When clean = TRUE, rows that do not
#' have an associated QAPP document are removed from the dataframe and no column
#' will be appended. This function should only be used to remove data if an
#' accompanying QAPP document is required to use data in assessments.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data without an associated QAPP
#' document from the dataframe when clean = TRUE. Default is clean = FALSE.
#'
#' @return Returns input dataframe with the added "TADA.QAPPDocAvailable" column. 
#' When clean = FALSE, no data are removed and the TADA.QAPPDocAvailable column
#' flags rows with an associated QAPP document. When clean = TRUE,
#' data without an associated QAPP document are removed from the dataframe.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' 
#' # Flag, but do not remove, data without an associated QAPP document in
#' # new column titled "TADA.QAPPDocAvailable":
#' FlagData_MissingQAPPDocURLs <- TADA_FindQAPPDoc(Data_Nutrients_UT)
#' 
#' # Remove data without an associated QAPP document available:
#' RemoveData_MissingQAPPDocURLs <- TADA_FindQAPPDoc(Data_Nutrients_UT, clean = TRUE)
#' 

TADA_FindQAPPDoc <- function(.data, clean = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, "ProjectFileUrl")
  
  # default flag column
  .data$TADA.QAPPDocAvailable = "N"
  
  # execute function after checks are passed
  # flag data where QAPP document url is provided
  # make QAPPdoc.data data frame
  QAPPdoc.data <- dplyr::filter(.data, grepl("/", ProjectFileUrl))
  NQAPPdoc.data = subset(.data, !.data$ResultIdentifier%in%QAPPdoc.data$ResultIdentifier)
  
  # if there is data without an associated QAPP url in the data set
  if (nrow(QAPPdoc.data) != 0) {
    
    # change flag column
    QAPPdoc.data$TADA.QAPPDocAvailable <- "Y_ProjectFileUrlProvided"
    
    # join QAPPdoc.data to flag.data
    flag.data <- plyr::rbind.fill(QAPPdoc.data, NQAPPdoc.data)
    
    # flagged output
    if (clean == FALSE) {
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    
    # clean output
    if (clean == TRUE) {
      # remove data without an associated QAPP url
      clean.data <- dplyr::filter(flag.data, grepl("/", ProjectFileUrl))
      
      # remove TADA.QAPPDocAvailable column
      # clean.data <- dplyr::select(clean.data, -TADA.QAPPDocAvailable)
      clean.data <- TADA_OrderCols(clean.data)
      return(clean.data)
    }
  }
  
  # if no associated QAPP url data is in the data set
  if (nrow(QAPPdoc.data) == 0) {
    if (clean == FALSE) {
      print("No QAPP document url data found in your dataframe. Returning input dataframe with TADA.QAPPDocAvailable column for tracking.")
      .data <- TADA_OrderCols(.data)
      return(.data)
    }
    if (clean == TRUE) {
      print("This dataframe is empty because we did not find any QAPP document url data in your dataframe")
      QAPPdoc.data <- TADA_OrderCols(QAPPdoc.data)
      return(QAPPdoc.data)
    }
  }
}



#' Invalid coordinates
#'
#' This function identifies and flags invalid coordinate data. When
#' clean_outsideUSA = "no" and clean_imprecise = FALSE,
#' a column will be appended titled "TADA.InvalidCoordinates.Flag" with the following
#' flags: 1) If the latitude is less than zero, the row will be
#' flagged with "LAT_OutsideUSA" (with the exception of American Samoa, 
#' Northern Mariana Islands, and Guam), 2) If the longitude is greater than zero AND less than 145,
#' the row will be flagged as "LONG_OutsideUSA" (with the exception of 
#' American Samoa, Northern Mariana Islands, and Guam), 3) If the latitude or longitude
#' contains the string, "999", the row will be flagged as invalid, and 4) Finally,
#' precision can be measured by the number of decimal places in the latitude and longitude
#' provided. If either the latitude or longitude does not have at least three numbers to the 
#' right of the decimal point, the row will be flagged as "Imprecise". Occasionally
#' latitude and longitude measurements are flagged as outside of the United States
#' because the data was entered as negative when it should be positive or vice versa.
#' This function offers the option of clean_outsideUSA = "change sign" to fix this
#' issue. However, data owners should fix the raw data through WQX. For assistance
#' with changing raw data, email the WQX help desk: \email{WQX@@epa.gov}
#'
#' @param .data TADA dataframe
#' @param clean_outsideUSA Character argument with options "no", "remove", and "change sign"; 
#' flags coordinates as outside the USA when clean_outsideUSA = "no";
#' removes data with coordinates outside of the United States when clean_outsideUSA = "remove";
#' changes sign of lat/long coordinates flagged as outside the USA when 
#' clean_outside = "change sign"; Default is clean_outsideUSA = "no".
#' @param clean_imprecise Boolean argument; removes imprecise data when
#' clean_imprecise = TRUE. Default is clean_imprecise = FALSE.
#' @param errorsonly Boolean argument; Return only flagged data when errorsonly = TRUE;
#' default is errorsonly = FALSE.
#'
#' @return Returns input TADA dataset with the added "TADA.InvalidCoordinates.Flag" column.
#' When clean_outsideUSA is "no", "change sign", or clean_imprecise argument is FALSE,
#' a column flagging rows with the respective QA check is appended to the input
#' dataframe. When clean_outsideUSA is "remove" or clean_imprecise is TRUE, 
#' "invalid" or "imprecise" data is removed, respectively. When errorsonly is TRUE, 
#' the dataframe will be filtered to show only the data flagged as invalid, imprecise, 
#' or out of the United States. Defaults are clean_outsideUSA = "no", 
#' clean_imprecise = FALSE, and errorsonly = FALSE.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' 
#' # Flag, but do not remove, data with invalid coordinates in new column 
#' # titled "TADA.InvalidCoordinates.Flag":
#' # Return ALL data:
#' InvalidCoord_flags <- TADA_FlagCoordinates(Data_Nutrients_UT)
#' 
#' # Flag, but do not remove, data with invalid coordinates in new column 
#' # titled "TADA.InvalidCoordinates.Flag"
#' # Return ONLY the flagged data:
#' InvalidCoord_flags_errorsonly <- TADA_FlagCoordinates(Data_Nutrients_UT, errorsonly = TRUE)
#' 
#' # Remove data with coordinates outside the USA, but keep flagged data with 
#' # imprecise coordinates:
#' OutsideUSACoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove")
#' 
#' # Change the sign of coordinates flagged as outside the USA and keep all
#' # flagged data:
#' OutsideUSACoord_changed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "change sign")
#' 
#' # Remove data with imprecise coordinates, but keep flagged data with coordinates outside the USA;
#' # imprecise data may include a series of 999's to the right of the decimal points;
#' # alternatively, imprecise data may have less than 3 significant figures to the right
#' # of the decimal point:
#' ImpreciseCoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_imprecise = TRUE)
#' 
#' # Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
#' InvalidCoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove", clean_imprecise = TRUE)
#' 

TADA_FlagCoordinates <- function(.data, 
                               clean_outsideUSA = c("no", "remove", "change sign"), 
                               clean_imprecise = FALSE, 
                               errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean_outsideUSA is character
  TADA_CheckType(clean_outsideUSA, "character")
  # check clean_imprecise is boolean
  TADA_CheckType(clean_imprecise, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, c("TADA.LatitudeMeasure", "TADA.LongitudeMeasure"))
  # check lat and long are "numeric"
  if (!is.numeric(.data$TADA.LongitudeMeasure)) {
    warning("TADA.LongitudeMeasure field must be numeric")
  }
  
  if (!is.numeric(.data$TADA.LatitudeMeasure)) {
    warning("TADA.LatitudeMeasure field must be numeric")
  }
  # check that clean_outsideUSA is either "no", "remove", or "change sign"
  clean_outsideUSA <- match.arg(clean_outsideUSA)
  
  orig_dim = dim(.data)[1]
  
  # execute function after checks are passed
  .data <- .data %>%
    dplyr::mutate(TADA.InvalidCoordinates.Flag = dplyr::case_when(
      TADA.LatitudeMeasure < -11.046934 & TADA.LatitudeMeasure > -14.548699 & TADA.LongitudeMeasure < -168.1433 & TADA.LongitudeMeasure > -171.089874 ~ NA_character_, #American Samoa
      TADA.LatitudeMeasure < 20.553802 & TADA.LatitudeMeasure > 14.110472 & TADA.LongitudeMeasure < 146.064818 & TADA.LongitudeMeasure > 144.886331 ~ NA_character_, #Northern Mariana Islands
      TADA.LatitudeMeasure < 13.654383 & TADA.LatitudeMeasure > 13.234189 & TADA.LongitudeMeasure < 144.956712 & TADA.LongitudeMeasure > 144.618068 ~ NA_character_, #Guam
      TADA.LatitudeMeasure < 0 ~ "LAT_OutsideUSA",
      TADA.LongitudeMeasure > 0 & TADA.LongitudeMeasure < 145 ~ "LONG_OutsideUSA",
      grepl("999", TADA.LatitudeMeasure) ~ "Imprecise_Latincludes999",
      grepl("999", TADA.LongitudeMeasure) ~ "Imprecise_Longincludes999",
      # for below, lat and long fields must be numeric
      # this checks if there are at least 3 significant figures to the 
      # right of the decimal point
      sapply(.data$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 3 
      | sapply(.data$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 3 ~ "Imprecise_lessthan3decimaldigits"
    ))
  
  # Fill in flag for coordinates that appear OK/PASS tests
  .data$TADA.InvalidCoordinates.Flag[is.na(.data$TADA.InvalidCoordinates.Flag)] = "OK"
  
  # if clean_imprecise is TRUE, remove imprecise station metadata
  if (clean_imprecise == TRUE) {
    .data <- dplyr::filter(.data,
                           !TADA.InvalidCoordinates.Flag%in%c("Imprecise_Latincludes999",
                                                              "Imprecise_Longincludes999",
                                                              "Imprecise_lessthan3decimaldigits"))
  }
  
  # if clean_outsideUSA is "remove", remove stations flagged as outside the USA
  if (clean_outsideUSA == "remove") {
    .data <- dplyr::filter(.data,
                           !TADA.InvalidCoordinates.Flag%in%c("LAT_OutsideUSA","LONG_OutsideUSA"))
  }
  
  # if clean_outsideUSA is "change sign", change the sign of lat/long coordinates outside of USA
  if (clean_outsideUSA == "change sign") {
    print("When clean_outsideUSA == change sign, the sign for any lat/long coordinates flagged as outside of USA are switched. This is a temporary solution. Data owners should fix the raw data to address invalid coordinates through WQX. For assistance fixing data errors you see in the WQP, email the WQX helpdesk (WQX@epa.gov).")
    .data <- .data %>% 
      dplyr::mutate(
        TADA.LatitudeMeasure = dplyr::case_when(
          TADA.InvalidCoordinates.Flag == "LAT_OutsideUSA" ~ TADA.LatitudeMeasure*(-1),
          TRUE ~ TADA.LatitudeMeasure),
        TADA.LongitudeMeasure = dplyr::case_when(
          TADA.InvalidCoordinates.Flag == "LONG_OutsideUSA" ~ TADA.LongitudeMeasure*(-1),
          TRUE ~ TADA.LongitudeMeasure)
      )
  }
  
  # return only flagged data if errorsonly = true
  if ((errorsonly == TRUE)) {
    .data <- dplyr::filter(.data, !TADA.InvalidCoordinates.Flag%in%c("OK"))
  }
  
  if (all(.data$TADA.InvalidCoordinates.Flag%in%c("OK")) == TRUE) {
    if(orig_dim==dim(.data)[1]){
      print("Your dataframe does not contain monitoring stations with invalid coordinates. Returning input dataframe with TADA.InvalidCoordinates.Flag column for tracking.")
    }else{
      print("All invalid coordinates were removed. Returning input dataframe with TADA.InvalidCoordinates.Flag column for tracking.")
    }
    }
  .data <- TADA_OrderCols(.data)
  return(.data)
}

#' Identify Potential Duplicate Data Uploads Between Organizations (UNDER ACTIVE DEVELOPMENT)
#' 
#' Identifies data records uploaded by different organizations with the same date,
#' time, characteristic name, and result value within X meters of each other and
#' flags as potential duplicates. However, it is at the discretion of the data user
#' to determine if the data records are unique or represent overlap that could cause
#' issues in the data analysis.
#' 
#' @param .data TADA dataframe
#' @param dist_buffer Numeric. The distance in meters below which two sites with 
#' measurements at the same time on the same day of the same parameter will
#' be flagged as potential duplicates.
#'
#' @return The same input TADA dataframe with additional columns: a
#'   TADA.ProbableBetweenOrgDuplicate column indicating if there is evidence that two or
#'   more results are duplicated, a TADA.BetweenOrgDupGroupID column, containing a
#'   number unique to results that may represent duplicated measurement events,
#'   and one or more TADA.SiteGroup columns indicating monitoring locations
#'   within the distance buffer from each other.
#'   
#' @export
#' 

TADA_FindPotentialDuplicatesBetweenOrgs <- function(.data, dist_buffer = 100){
  # get all data that are not NA and round to 2 digits
  dupsprep = .data%>%dplyr::select(OrganizationIdentifier,ResultIdentifier,ActivityStartDate, ActivityStartTime.Time, TADA.CharacteristicName,TADA.ResultMeasureValue)%>%dplyr::filter(!is.na(TADA.ResultMeasureValue))%>%dplyr::mutate(roundRV = round(TADA.ResultMeasureValue,digits=2))
  # group by date, time, characteristic, and rounded result value and summarise the number of organizations that have those same row values, and filter to those summary rows with more than one organization
  dups_sum = dupsprep%>%dplyr::group_by(ActivityStartDate, ActivityStartTime.Time, TADA.CharacteristicName,roundRV)%>%dplyr::summarise(numorgs = length(unique(OrganizationIdentifier)))%>%dplyr::filter(numorgs>1)
   
  # if there are potential duplicates based on grouping above, check if sites are nearby
  if(dim(dups_sum)[1]>0){
    # give potential duplicates a grouping ID
    dups_sum$TADA.BetweenOrgDupGroupID = seq(1:dim(dups_sum)[1])
    # merge to narrow dataset to match to true result value
    dupsdat = merge(dups_sum, dupsprep, all.x = TRUE)
    # make sure potential dupes are within 10% of the max value in the group
    dupsdat = dupsdat%>%dplyr::group_by(TADA.BetweenOrgDupGroupID)%>%dplyr::mutate(maxRV = max(TADA.ResultMeasureValue))%>%dplyr::mutate(within10 = ifelse(min(TADA.ResultMeasureValue)>=0.9*maxRV,"Y","N"))%>%dplyr::filter(within10=="Y")%>%dplyr::select(!c(roundRV,maxRV,within10,numorgs))%>%dplyr::ungroup()
    
    # merge to data
    dupsdat = dplyr::left_join(dupsdat, .data)
    
    rm(dupsprep)
    # from those datapoints, determine which are in adjacent sites
    if(!"TADA.SiteGroup1"%in%names(.data)){
      .data = TADA_FindNearbySites(.data, dist_buffer = dist_buffer)
    }
    dupsitesids = names(.data)[grepl("TADA.SiteGroup",names(.data))]
    dupsites = unique(.data[,c("MonitoringLocationIdentifier","TADA.LatitudeMeasure","TADA.LongitudeMeasure",dupsitesids)])
    
    # get rid of results with no site group added - not duplicated spatially
    dupsites = subset(dupsites, !is.na(dupsites$TADA.SiteGroup1))
    
    if(dim(dupsites)[1]>0){ # if results are potentially duplicated temporally and spatially, determine if they share site group membership
      # narrow down dataset more
      dupsdat = subset(dupsdat, dupsdat$MonitoringLocationIdentifier%in%dupsites$MonitoringLocationIdentifier)
      
      if(dim(dupsdat)[1]>0){ # nearby sites exist but do not overlap with potentially duplicated result values
        sitecomp = dupsites%>%dplyr::select(MonitoringLocationIdentifier,dplyr::all_of(dupsitesids))%>%tidyr::pivot_longer(dplyr::all_of(dupsitesids), values_to = "AllGroups")%>%dplyr::filter(!is.na(AllGroups))%>%dplyr::distinct() # get unique groups in dataset
        dupids = unique(dupsdat$TADA.BetweenOrgDupGroupID) # will loop by ID
        
        # create empty dataframe to hold result of spatial grouping tests
        dupdata = data.frame()
        
        for(i in 1:length(dupids)){ # loop through each duplicate group
          dat = subset(dupsdat, dupsdat$TADA.BetweenOrgDupGroupID==dupids[i])
          if(dim(dat)[1]>1&length(unique(dat$OrganizationIdentifier))>1){ # if more than one result in dataset and multiple organizations
            sitecompj = subset(sitecomp, sitecomp$MonitoringLocationIdentifier%in%dat$MonitoringLocationIdentifier)  
            for(j in 1:length(unique(sitecompj$AllGroups))){ # loop through each group
              sitegp = subset(sitecompj, sitecompj$AllGroups==unique(sitecompj$AllGroups)[j])
              if(dim(sitegp)[1]>1){
                dat$TADA.ProbableBetweenOrgDuplicate = ifelse(dat$MonitoringLocationIdentifier%in%sitegp$MonitoringLocationIdentifier,"Y","N") # mark as "Y" probable duplicate if sites in dup id subset are in group id subset.
                dupdata = plyr::rbind.fill(dupdata, dat) #bind to all other potential duplicate results
              }
            }
          }
          # print(i)
        }
        
        dupdata = subset(dupdata, dupdata$TADA.ProbableBetweenOrgDuplicate=="Y")
        # connect back to original dataset
        .data = merge(.data, dupdata[,c("ResultIdentifier","TADA.BetweenOrgDupGroupID","TADA.ProbableBetweenOrgDuplicate")], all.x = TRUE)
        .data$TADA.ProbableBetweenOrgDuplicate[is.na(.data$TADA.ProbableBetweenOrgDuplicate)] = "N"
        .data$TADA.BetweenOrgDupGroupID[is.na(.data$TADA.ProbableBetweenOrgDuplicate)] = NA
      }else{
        .data$TADA.BetweenOrgDupGroupID = NA
        .data$TADA.ProbableBetweenOrgDuplicate = "N"
        print("No duplicate results detected. Returning input dataframe with duplicate flagging columns set to N.")
      }
    }else{ # if no site duplicates detected
      .data$TADA.BetweenOrgDupGroupID = NA
      .data$TADA.ProbableBetweenOrgDuplicate = "N"
      print("No duplicate results detected. Returning input dataframe with duplicate flagging columns set to N.")
    }
  }else{ # if no result/org duplicates detected
    .data$TADA.BetweenOrgDupGroupID = NA
    .data$TADA.ProbableBetweenOrgDuplicate = "N"
    print("No duplicate results detected. Returning input dataframe with duplicate flagging columns set to N.")
  }
  
  .data = TADA_OrderCols(.data)
  
  return(.data)
}

#' Identify Potential Duplicate Data Uploads Within An Organization (UNDER ACTIVE DEVELOPMENT)
#' 
#' Identifies data records uploaded by the same organization with the same date,
#' time, monitoring location, activity type, characteristic name, fraction,
#' taxonomic name, depth columns, and result value and flags as potential
#' duplicates. However, it is at the discretion of the data user to determine if
#' the data records are unique or represent overlap that could cause issues in
#' the data analysis.
#' 
#' @param .data TADA dataframe
#' @param handling_method Defaults to 'none'. When input is 'none', function
#'   flags groups of potentially duplicated results, but makes no decisions on
#'   which (if any) should be selected/excluded. Alternatively, user may
#'   populate handling_method = 'pick_one' and function will randomly select one
#'   result in each group and flag all others as duplicates.
#' 
#' @return The same input TADA dataframe with additional columns: a
#'   TADA.WithinOrgDupGroupID column indicating whether a result is part of a
#'   group that shares the same date, time, location, characteristic, etc. and
#'   TADA.ResultSelectedWithinOrg, which defaults to TRUE (all results
#'   selected), unless the user specifies the handling method as 'pick_one', in
#'   which case there exists one result in each duplicate group where
#'   TADA.ResultSelectedWithinOrg = TRUE and for all other results in the group
#'   TADA.ResultSelectedWithinOrg = FALSE.
#' 
#' @export
#' 

TADA_FindPotentialDuplicatesWithinOrg <- function(.data, handling_method = 'none'){
  
  # find the depth columns in the dataset
  depthcols = names(.data)[grepl("^TADA.*DepthHeightMeasure.MeasureValue$", names(.data))]
  
  # tack depth columns onto additional grouping columns
  colss = c("OrganizationIdentifier", "MonitoringLocationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "ActivityTypeCode", "TADA.CharacteristicName", "SubjectTaxonomicName", "TADA.ResultSampleFractionText","TADA.ResultMeasureValue", depthcols)
  
  # find where the grouping using the columns above results in more than result identifier
  dups_sum_org = .data%>%dplyr::group_by(dplyr::across(dplyr::any_of(colss)))%>%dplyr::summarise(numres = length(unique(ResultIdentifier)))%>%dplyr::filter(numres>1)%>%dplyr::mutate(TADA.WithinOrgDupGroupID = dplyr::cur_group_id())
  
  if(dim(dups_sum_org)[1]>0){
    # apply to .data and remove numres column
    .data = merge(.data, dups_sum_org, all.x = TRUE)
    .data = .data %>% dplyr::select(-numres)
    .data$TADA.WithinOrgDupGroupID[is.na(.data$TADA.WithinOrgDupGroupID)] = "Not a duplicate"

    # if handling_method = 'pick_one', randomly select
    if(handling_method=="pick_one"){
      dup_rids = subset(.data, !is.na(.data$TADA.WithinOrgDupGroupID))$ResultIdentifier
      picks = .data %>% dplyr::filter(!is.na(TADA.WithinOrgDupGroupID)) %>% dplyr::group_by(TADA.WithinOrgDupGroupID) %>% dplyr::slice_sample(n = 1)
      not_picked_rids = dup_rids[!dup_rids%in%picks$ResultIdentifier]
      .data$TADA.ResultSelectedWithinOrg = TRUE
      .data$TADA.ResultSelectedWithinOrg = ifelse(.data$ResultIdentifier%in%not_picked_rids,FALSE,.data$TADA.ResultSelectedWithinOrg)
      print(paste0(dim(dups_sum_org)[1]," potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.WithinOrgDupGroupID column and the function randomly selected one result from each group to represent a single, unduplicated value. Selected values are indicated in the TADA.ResultSelectedWithinOrg as TRUE, while duplicates are flagged as FALSE for easy filtering."))
    }else{
        .data$TADA.ResultSelectedWithinOrg = TRUE
        print(paste0(dim(dups_sum_org)[1]," potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.WithinOrgDupGroupID column."))
    }

  }else{
    .data$TADA.WithinOrgDupGroupID = "Not a duplicate"
    .data$TADA.ResultSelectedWithinOrg = TRUE
    print("No duplicates detected within organizations in the dataset. 'TADA.WithinOrgDupGroupID' column set to 'Not a duplicate' and 'TADA.ResultSelectedWithinOrg' set to TRUE for all results.")
  }
  
  .data = TADA_OrderCols(.data)
  return(.data)
  
}
