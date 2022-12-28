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
#' @return When clean = FALSE and errorsonly = FALSE, this function adds the 
#' following column to your dataframe: WQX.AnalyticalMethodValidity. This column 
#' flags invalid CharacteristicName, ResultAnalyticalMethod/MethodIdentifier,
#' and ResultAnalyticalMethod/MethodIdentifierContext combinations in your dataframe
#' as either "Nonstandardized", "Invalid", or "Valid". When clean = FALSE and 
#' errorsonly = TRUE, the dataframe is filtered to show only "Invalid" 
#' characteristic-analytical method combinations; the column WQX.AnalyticalMethodValidity
#' is still appended. When clean = TRUE and errorsonly = FALSE, "Invalid" rows 
#' are removed from the dataframe and no column will be appended.
#' 
#' @export
#'
#' @examples 
#' # Load example dataset
#' data(Nutrients_Utah)
#' 
#' # Remove invalid characteristic-analytical method combinations from dataframe:
#' InvalidMethod_clean <- InvalidMethod(Nutrients_Utah)
#' 
#' # Flag, but do not remove, invalid characteristic-analytical method combinations
#' # in new column titled "WQX.AnalyticalMethodValidity":
#' InvalidMethod_flags <- InvalidMethod(Nutrients_Utah, clean = FALSE)
#' 
#' # Show only invalid characteristic-analytical method combinations:
#' InvalidMethod_errorsonly <- InvalidMethod(Nutrients_Utah, clean = FALSE, errorsonly = TRUE)

InvalidMethod <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext"
  )
  checkColumns(.data, required_cols)
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # execute function after checks are passed
  # delete existing flag column
  if (("WQX.AnalyticalMethodValidity" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -WQX.AnalyticalMethodValidity)
  }
  # read in speciation reference table from sysdata.rda and filter
  meth.ref <- GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicMethod")
  
  # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
  check.data <- merge(.data, meth.ref[, c("Characteristic", "Source", "Status", "Value")],
                      by.x = c(
                        "CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier",
                        "ResultAnalyticalMethod.MethodIdentifierContext"
                      ),
                      by.y = c("Characteristic", "Value", "Source"), all.x = TRUE
  )
  
  # rename Status column to WQX.AnalyticalMethodValidity
  check.data <- check.data %>%
    dplyr::rename(WQX.AnalyticalMethodValidity = Status)
  # rename NA values to Nonstandardized in WQX.AnalyticalMethodValidity column
  check.data["WQX.AnalyticalMethodValidity"][is.na(check.data["WQX.AnalyticalMethodValidity"])] <- "Nonstandardized"
  
  # reorder column names to match .data
  # get .data column names
  col.order <- colnames(.data)
  # add WQX.AnalyticalMethodValidity column to the list
  col.order <- append(col.order, "WQX.AnalyticalMethodValidity")
  # reorder columns in flag.data
  check.data <- check.data[, col.order]
  # place flag columns next to relevant fields
  check.data <- check.data %>%
    dplyr::relocate("WQX.AnalyticalMethodValidity",
                    .after = "ResultAnalyticalMethod.MethodName"
    )
  
  # if all rows are "Valid" or NA "Nonstandardized", return input unchanged
  ##note: Cristina edited this on 9/19/22 to keep Nonstandardized/NA data when clean = TRUE. Now only Invalid data is removed.
  if (any("Invalid" %in%
          unique(check.data$WQX.AnalyticalMethodValidity)) == FALSE) {
    print("No changes were made, because we did not find any invalid method/characteristic combinations in your dataframe")
    return(.data)
  }
  
  # flagged output, all data
  if (clean == FALSE & errorsonly == FALSE) {
    return(check.data)
  }
  
  # clean output
  if (clean == TRUE & errorsonly == FALSE) {
    # filter out invalid characteristic-unit-method combinations
    clean.data <- dplyr::filter(check.data, WQX.AnalyticalMethodValidity != "Invalid")
    
    # remove WQX.AnalyticalMethodValidity column
    clean.data <- dplyr::select(clean.data, -WQX.AnalyticalMethodValidity)
    
    return(clean.data)
  }
  
  # flagged output, errors only
  if (clean == FALSE & errorsonly == TRUE) {
    # filter to show only invalid characteristic-unit-method combinations
    invalid.data <- dplyr::filter(check.data, WQX.AnalyticalMethodValidity == "Invalid")
    return(invalid.data)
  }
}


#' Check for Aggregated Continuous Data
#'
#' The Water Quality Portal (WQP) is not currently designed to store high-frequency
#' sensor data (more than 1 value per day). However, sometimes data providers 
#' choose to aggregate their continuous data to a daily avg, max, or min value, 
#' and then submit that aggregated data to the WQP through WQX. Alternatively, 
#' some organizations aggregate their high frequency data (15 min or 1 hour data) 
#' to 2 or 4 hour interval averages, and they also submit that data to the WQP through WQX.
#' This type of high frequency data may (or may not) be suitable for integration with discrete 
#' water quality data for assessments. Therefore, this function uses metadata 
#' submitted by data providers to flag rows with aggregated continuous data. 
#' This is done by flagging results where the ResultDetectionConditionText = 
#' "Reported in Raw Data (attached)". When clean = FALSE and errorsonly = FALSE, a column titled 
#' "TADA.AggregatedContinuousData" is added to the dataframe to indicate if the row 
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
#' data(Nutrients_Utah)
#' 
#' # Remove aggregated continuous data from dataframe:
#' AggContinuous_clean <- AggregatedContinuousData(Nutrients_Utah)
#' 
#' # Flag, but do not remove, aggregated continuous data in new column
#' titled "TADA.AggregatedContinuousData":
#' AggContinuous_flags <- AggregatedContinuousData(Nutrients_Utah, clean = FALSE)
#' 
#' # Show only rows flagged for aggregated continuous data:
#' AggContinuous_errorsonly <- AggregatedContinuousData(Nutrients_Utah, clean = FALSE, errorsonly = TRUE)

AggregatedContinuousData <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  checkColumns(.data, "ResultDetectionConditionText")
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # execute function after checks are passed
  # flag continuous data
  # make cont.data data frame
  cont.data <- dplyr::filter(
    .data,
    ResultDetectionConditionText == "Reported in Raw Data (attached)"
  )
  
  # if there is aggregated continuous data is in the data set
  if (nrow(cont.data) != 0) {
    # append ContDataFlag column
    cont.data$TADA.AggregatedContinuousData <- "Y"
    # join cont.data to flag.data
    flag.data <- merge(.data, cont.data, all.x = TRUE)
    
    # flagged output, all data
    if (clean == FALSE & errorsonly == FALSE) {
      return(flag.data)
    }
    
    # clean output
    if (clean == TRUE & errorsonly == FALSE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(flag.data, !(TADA.AggregatedContinuousData %in% "Y"))
      
      # remove TADA.AggregatedContinuousData column
      clean.data <- dplyr::select(clean.data, -TADA.AggregatedContinuousData)
      
      return(clean.data)
    }
    
    # flagged output, only aggregated continuous data
    if (clean == FALSE & errorsonly == TRUE) {
      #filter to show only invalid characteristic-unit-media combinations
      aggcont.data <- dplyr::filter(flag.data, (TADA.AggregatedContinuousData %in% "Y"))
      return(aggcont.data)
    }
  }
  
  # if no aggregated continuous data is in the data set
  if (nrow(cont.data) == 0) {
    print("No changes were made, because we did not find any aggregated continuous data in your dataframe")
    
    return(.data)
  }
}


#' Check for Potential Duplicates
#'
#' Sometimes multiple organizations submit the exact same data set to the 
#' Water Quality Portal (WQP), which can affect water quality analyses. This
#' function checks for and identifies data that is identical in all fields
#' excluding organization-specific and comment text fields. When clean = FALSE,
#' a column titled "TADA.PotentialDupRowID" is added to the dataframe which
#' assigns each pair of potential duplicates a unique ID for review.
#' When clean = TRUE, the function retains the first occurrence of each 
#' potential duplicate in the dataframe. Default is clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes potential duplicate data from
#' the dataframe when clean = TRUE. When clean = FALSE,
#' a column titled "TADA.PotentialDupRowID" is added to the dataframe which
#' assigns each pair of potential duplicates a unique ID for review.
#' Default is clean = TRUE.
#' @param errorsonly Boolean argument; filters dataframe to show only potential 
#' duplicate rows of data when errorsonly = TRUE. Default is errorsonly = FALSE.
#'
#' @return When clean = FALSE, the following column will be added to you dataframe: 
#' TADA.PotentialDupRowID. This column flags potential duplicate rows of data 
#' in your dataframe, and assigns each potential duplicate combination a unique number
#' linking the two potential duplication rows. When clean = FALSE the first of
#' each group of potential duplicate rows will be removed from the dataframe
#' and no column is appended.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Remove potential duplicate data from dataframe:
#' PotentialDup_clean <- PotentialDuplicateRowID(Nutrients_Utah)
#' 
#' # Flag, but do not remove, potential duplicate data in new column titled "TADA.PotentialDupRowID":
#' PotentialDup_flagcolumnadded <- PotentialDuplicateRowID(Nutrients_Utah, clean = FALSE)
#' 
#' # Flag and review potential duplicate data only:
#' PotentialDup_reviewduplicatesonly <- PotentialDuplicateRowID
#' (Nutrients_Utah, clean = FALSE, errorsonly = TRUE) 
#' 

PotentialDuplicateRowID <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "ActivityIdentifier", "ActivityConductingOrganizationText",
    "OrganizationFormalName", "OrganizationIdentifier",
    "ProjectIdentifier", "ResultCommentText",
    "ActivityCommentText"
    )
  checkColumns(.data, required_cols)
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # execute function after checks are passed
  # get list of field names in .data
  field.names <- colnames(.data)
  # create list of fields to exclude when looking for duplicate rows
  excluded.fields <- c(
    "ActivityIdentifier", "ActivityConductingOrganizationText",
    "OrganizationFormalName", "OrganizationIdentifier",
    "ProjectIdentifier", "ResultCommentText", "ActivityCommentText"
  )
  # create list of fields to check for duplicates across
  dupe.fields <- field.names[!field.names %in% excluded.fields]
  
  # subset list of duplicate rows
  dupe.data <- .data[duplicated(.data[dupe.fields]), ]
  
  # if no potential duplicates are found
  if (nrow(dupe.data) == 0) {
    print("No changes were made, because we did not find any potential duplicates in your dataframe")
    return(.data)
  }
  
  # if potential duplicates are found
  if (nrow(dupe.data) != 0) {
    
    # flag potential duplicates
    dupe.data$TADA.PotentialDupRowID <- as.integer(seq_len(nrow(dupe.data)))
    
    # merge flag column into .data
    flag.data <- merge(.data, dupe.data, by = dupe.fields, all.x = TRUE)
    
    # remove extraneous columns, fix field names
    flag.data <- flag.data %>%
      # remove ".x" suffix from column names
      dplyr::rename_at(
        dplyr::vars(dplyr::ends_with(".x")),
        ~ stringr::str_replace(., "\\..$", "")
      ) %>%
      # remove columns with ".y" suffix
      dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add TADA.PotentialDupRowID column to the list
    col.order <- append(col.order, "TADA.PotentialDupRowID")
    # reorder columns in flag.data
    flag.data <- flag.data[, col.order]
    
    # flagged output, all data
    if (clean == FALSE & errorsonly == FALSE) {
      return(flag.data)
    }
    
    # clean output
    if (clean == TRUE & errorsonly == FALSE) {
      # remove duplicate rows
      # seperate data into 2 dataframes by TADA.PotentialDupRowID (no NAs and NAs)
      dup.data <- flag.data[!is.na(flag.data$TADA.PotentialDupRowID), ]
      NAdup.data <- flag.data[is.na(flag.data$TADA.PotentialDupRowID), ]
      
      nodup.data <- dup.data[!duplicated(dup.data$TADA.PotentialDupRowID), ]
      
      clean.data <- rbind(nodup.data, NAdup.data)
      
      # remove TADA.PotentialDupRowID column
      clean.data <- dplyr::select(clean.data, -TADA.PotentialDupRowID)
      
      return(clean.data)
    }
    
    # flagged data, errors only
    if (clean == FALSE & errorsonly == TRUE) {
      # filter to show duplicate data only
      dup.data <- flag.data[!is.na(flag.data$TADA.PotentialDupRowID), ]
      return(dup.data)
    }
  }
}


#' Check Result Value Against WQX Upper Threshold
#'
#' EPA's Water Quality Exchange (WQX) has generated statistics and data from
#' millions of water quality data points around the country. This function
#' leverages the statistical data from WQX to flag any data that is above the
#' upper threshold of result values submitted to WQX for a given characteristic.
#' When clean = FALSE and errorsonly = FALSE, a column which flags data above the
#' upper WQX threshold is appended to the dataframe. When clean = FALSE and 
#' errorsonly = TRUE, the dataframe is filtered to show only data found above 
#' the WQX threshold. When clean = TRUE and errorsonly = FALSE, rows with values 
#' that are above the upper WQX threshold are removed from the dataframe and no 
#' column is appended. When clean = TRUE and and errorsonly = TRUE, the function 
#' is not executed and an error message is returned. Defaults are clean = TRUE and
#' errorsonly = FALSE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data that is above the upper WQX
#' threshold from the dataframe when clean = TRUE. Default is clean = TRUE.
#' @param errorsonly Boolean argument; filters dataframe to show only the data
#' flagged as above the upper WQX threshold. Default is errorsonly = FALSE.
#'
#' @return When clean = FALSE and errorsonly = FALSE, a column which flags data above the
#' upper WQX threshold is appended to the dataframe. When clean = FALSE and 
#' errorsonly = TRUE, the dataframe is filtered to show only data found above 
#' the WQX threshold. When clean = TRUE and errorsonly = FALSE, rows with values 
#' that are above the upper WQX threshold are removed from the dataframe and no 
#' column is appended. When clean = TRUE and and errorsonly = TRUE, the function 
#' is not executed and an error message is returned. Defaults are clean = TRUE and
#' errorsonly = FALSE.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Remove data that is above the upper WQX threshold from dataframe:
#' WQXUpperThreshold_clean <- AboveNationalWQXUpperThreshold(Nutrients_Utah)
#' 
#' # Flag, but do not remove, data that is above the upper WQX threshold in
#' # new column titled "AboveWQXUpperThreshold":
#' WQXUpperThreshold_flags <- AboveNationalWQXUpperThreshold(Nutrients_Utah, clean = FALSE)
#' 
#' # Show only data flagged as above the upper WQX threshold:
#' WQXUpperThreshold_flagsonly <- AboveNationalWQXUpperThreshold(Nutrients_Utah, 
#' clean = FALSE, errorsonly = TRUE)

AboveNationalWQXUpperThreshold <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode"
  )
  checkColumns(.data, required_cols)
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$ResultMeasureValue)) {
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }

  # execute function after checks are passed

  # delete existing flag column
  if (("AboveWQXUpperThreshold" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -AboveWQXUpperThreshold)
  }
  
  # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
  unit.ref <- GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" &
                    Status == "Valid")
  
  # join unit.ref to raw.data
  check.data <- merge(.data, unit.ref[, c(
    "Characteristic", "Source",
    "Value", "Maximum"
  )],
  by.x = c(
    "CharacteristicName", "ActivityMediaName",
    "ResultMeasure.MeasureUnitCode"
  ),
  by.y = c("Characteristic", "Source", "Value"), all.x = TRUE
  )
  
  # If ResultMeasureValue is not numeric, run ConvertDepthUnits function to convert class to numeric
  if (!is.numeric(check.data$ResultMeasureValue)) {
    check.data <- ConvertDepthUnits(check.data, transform = TRUE)
  }
  
  # Create flag column, flag rows where ResultMeasureValue > Maximum
  flag.data <- check.data %>%
    # apply function row by row
    dplyr::rowwise() %>%
    # create flag column
    dplyr::mutate(AboveWQXUpperThreshold = dplyr::case_when(
      ResultMeasureValue >= Maximum ~ as.character("Y"),
      ResultMeasureValue < Maximum ~ as.character("N")
    ))
  
  # remove extraneous columns, fix field names
  flag.data <- flag.data %>%
    dplyr::select(-"Maximum")
  
  # reorder column names to match .data
  # get .data column names
  col.order <- colnames(.data)
  # add AboveWQXUpperThreshold column to the list
  col.order <- append(col.order, "AboveWQXUpperThreshold")
  # reorder columns in flag.data
  flag.data <- flag.data[, col.order]
  
  # if no data above WQX threshold is found
  if (any("Y" %in%
          unique(flag.data$AboveWQXUpperThreshold)) == FALSE) {
    print("No changes were made because no data above the WQX Upper Threshold was found in your dataframe")
    return(.data)
  }
  
  # flagged, all data
  if (clean == FALSE & errorsonly == FALSE) {
    return(flag.data)
  }
  
  # clean data
  if (clean == TRUE & errorsonly == FALSE) {
    # filter out rows where AboveWQXUpperThreshold = Y; remove AboveWQXUpperThreshold column
    clean.data <- flag.data %>%
      dplyr::filter(!(AboveWQXUpperThreshold %in% "Y")) %>%
      dplyr::select(-AboveWQXUpperThreshold)
    return(clean.data)
  }
  
  # flagged, errors only
  if (clean == FALSE & errorsonly == TRUE) {
    # filter to show only rows above WQX upper threshold
    flagsonly.data <- flag.data %>%
      dplyr::filter(AboveWQXUpperThreshold %in% "Y")
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
#' @return When clean = FALSE and errorsonly = FALSE, the following column is 
#' added to your dataframe: BelowWQXLowerThreshold. This column flags rows with 
#' data that are below the lower WQX threshold. When clean = FALSE and 
#' errorsonly = TRUE, the dataframe is filtered to show only the rows which are
#' flagged as below the WQX threshold; the column BelowWQXThreshold is still
#' appended. When clean = TRUE and errorsonly = FALSE, data that is below the lower 
#' WQX threshold is removed from the dataframe. When clean = TRUE and
#' errorsonly = TRUE, the function does not execute and an error message is
#' returned. The defaults are clean = TRUE and errorsonly = FALSE.
#' 
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Remove data that is below the lower WQX threshold from the dataframe:
#' WQXLowerThreshold_clean <- BelowNationalWQXLowerThreshold(Nutrients_Utah)
#' 
#' # Flag, but do not remove, data that is below the lower WQX threshold in
#' # new column titled "BelowWQXLowerThreshold":
#' WQXLowerThreshold_flags <- BelowNationalWQXLowerThreshold(Nutrients_Utah, clean = FALSE)
#' 
#' # Show only data that is below the lower WQX threshold:
#' WQXLowerThreshold_flagsonly <- BelowNationalWQXLowerThreshold(Nutrients_Utah,
#' clean = FALSE, errorsonly = TRUE)

BelowNationalWQXLowerThreshold <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode"
  )
  checkColumns(.data, required_cols)
  # check that clean and errorsonly are not both TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$ResultMeasureValue)) {
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }

  # execute function after checks are passed
  # delete existing flag column
  if (("BelowWQXLowerThreshold" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -BelowWQXLowerThreshold)
  }
  
  # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
  unit.ref <- GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" &
                    Status == "Valid")
  
  # join unit.ref to raw.data
  check.data <- merge(.data, unit.ref[, c(
    "Characteristic", "Source",
    "Value", "Minimum"
  )],
  by.x = c(
    "CharacteristicName", "ActivityMediaName",
    "ResultMeasure.MeasureUnitCode"
  ),
  by.y = c("Characteristic", "Source", "Value"), all.x = TRUE
  )
  
  # If ResultMeasureValue is not numeric, run ConvertDepthUnits function to convert class to numeric
  if (!is.numeric(check.data$ResultMeasureValue)) {
    check.data <- ConvertDepthUnits(check.data, transform = TRUE)
  }
  
  # Create flag column, flag rows where ResultMeasureValue < Minimum
  flag.data <- check.data %>%
    # apply function row by row
    dplyr::rowwise() %>%
    # create flag column
    dplyr::mutate(BelowWQXLowerThreshold = dplyr::case_when(
      ResultMeasureValue <= Minimum ~ as.character("Y"),
      ResultMeasureValue > Minimum ~ as.character("N")
    ))
  
  # remove extraneous columns, fix field names
  flag.data <- flag.data %>%
    dplyr::select(-"Minimum")
  
  # reorder column names to match .data
  # get .data column names
  col.order <- colnames(.data)
  # add BelowWQXLowerThreshold column to the list
  col.order <- append(col.order, "BelowWQXLowerThreshold")
  # reorder columns in flag.data
  flag.data <- flag.data[, col.order]
  
  # if no data below WQX lower threshold is found
  if (any("Y" %in%
          unique(flag.data$BelowWQXLowerThreshold)) == FALSE) {
    print("No changes were made because no data below the WQX Lower Threshold was found in your dataframe")
    return(.data)
  }
  
  # flagged, all data
  if (clean == FALSE & errorsonly == FALSE) {
    return(flag.data)
  }
  
  # clean data
  if (clean == TRUE & errorsonly == FALSE) {
    # filter out rows where BelowWQXLowerThreshold = Y; remove BelowWQXLowerThreshold column
    clean.data <- flag.data %>%
      dplyr::filter(!(BelowWQXLowerThreshold %in% "Y")) %>%
      dplyr::select(-BelowWQXLowerThreshold)
    
    return(clean.data)
  }
  
  # only flagged data
  if (clean == FALSE & errorsonly == TRUE) {
    # filter to show only rows where BelowWQXLowerThreshold = Y
    flagsonly.data <- flag.data %>%
      dplyr::filter(BelowWQXLowerThreshold %in% "Y")
    return(flagsonly.data)
  }
}



#' Check data for an approved QAPP
#'
#' Function checks data submitted under the column "QAPPApprovedIndicator".
#' Some organizations submit data for this field to indicate if the data
#' produced has an approved Quality Assurance Project Plan (QAPP) or not.
#' Y indicates yes, N indicates no.  This function has three default inputs:
#' clean = TRUE, cleanNA = FALSE, and errorsonly == FALSE. The default removes 
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
#' data(Nutrients_Utah)
#' 
#' # Show data where the QAPPApprovedIndicator equals "Y" or "NA":
#' QAPPapproved_clean <- QAPPapproved(Nutrients_Utah)
#' 
#' # Show only data where the QAPPApprovedIndicator equals "Y":
#' QAPPapproved_cleanNAs <- QAPPapproved(Nutrients_Utah, cleanNA = TRUE)
#' 
#' # Show data where the QAPPApprovedIndicator equals "N" or "NA":
#' QAPPIndicator_N_NA <- QAPPapproved(Nutrients_Utah, clean = FALSE, 
#' cleanNA = FALSE, errorsonly = TRUE)
#' 
#' # Show data where the QAPPApprovedIndicator equals "N":
#' QAPPIndicator_N <- QAPPapproved(Nutrients_Utah, clean = FALSE, 
#' cleanNA = TRUE, errorsonly = TRUE)
#'
#' # Note: When clean = FALSE, cleanNA = FALSE, and errorsonly = FALSE, no data is removed
#' # Note: When clean = TRUE, cleanNA = TRUE, and errorsonly = TRUE, an error message is returned

QAPPapproved <- function(.data, clean = TRUE, cleanNA = FALSE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check cleanNA is boolean
  checkType(cleanNA, "logical") 
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  checkColumns(.data, "QAPPApprovedIndicator")
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
        warning("All QAPPApprovedIndicator data is N")
      }
    }
    if (cleanNA == TRUE) {
      .data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == FALSE)
      
      if (nrow(.data) == 0 & clean == TRUE) {
        warning("All QAPPApprovedIndicator data is NA or N")
      } else if (nrow(.data) == 0 & clean == FALSE) {
        warning("All QAPPApprovedIndicator data is NA")
      }
    }
    if (clean == FALSE & cleanNA == FALSE) {
      warning("No changes were made because clean and cleanNA were FALSE")
    }
    return(.data)
  }
  
  # if errorsonly = TRUE
  if (errorsonly == TRUE & clean == TRUE & cleanNA == FALSE) {
    NA.data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE)
    if (nrow(NA.data) == 0) {
      warning("All QAPPApprovedIndicator data is 'Y' or 'N'")
    }
    return(NA.data)
  }
  if (errorsonly == TRUE & clean == FALSE & cleanNA == TRUE) {
    N.data <- dplyr::filter(.data, QAPPApprovedIndicator == "N")
    if (nrow(N.data) == 0) {
      warning("All QAPPApprovedIndicator data is NA or 'Y'")
    }
    return(N.data)
  }
  if (errorsonly == TRUE & clean == FALSE & cleanNA == FALSE) {
    NAorN.data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE | QAPPApprovedIndicator == "N")
    if (nrow(NAorN.data) == 0) {
      warning("All QAPPApprovedIndicator data is 'Y'")
    }
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
#' @return When clean = FALSE, a column is appended to the input data set that
#' flags rows with an associated QAPP document. When clean = TRUE,
#' data without an associated QAPP document is removed from the dataframe.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Flag, but do not remove, data without an associated QAPP document in
#' # new column titled "TADA.QAPPDocAvailable":
#' FlagData_MissingQAPPDocURLs <- QAPPDocAvailable(Nutrients_Utah)
#' 
#' # Remove data without an associated QAPP document available:
#' RemoveData_MissingQAPPDocURLs <- QAPPDocAvailable(Nutrients_Utah, clean = TRUE)

QAPPDocAvailable <- function(.data, clean = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check .data has required columns
  checkColumns(.data, "ProjectFileUrl")
  
  # execute function after checks are passed
  # flag data where QAPP document url is provided
  # make QAPPdoc.data data frame
  QAPPdoc.data <- dplyr::filter(.data, grepl("/", ProjectFileUrl))
  
  # if there is data without an associated QAPP url in the data set
  if (nrow(QAPPdoc.data) != 0) {
    
    # append flag column
    QAPPdoc.data$TADA.QAPPDocAvailable <- "Y_ProjectFileUrlProvided"
    
    # join QAPPdoc.data to flag.data
    flag.data <- merge(.data, QAPPdoc.data, all.x = TRUE)
    
    # flagged output
    if (clean == FALSE) {
      return(flag.data)
    }
    
    # clean output
    if (clean == TRUE) {
      # remove data without an associated QAPP url
      clean.data <- dplyr::filter(flag.data, grepl("/", ProjectFileUrl))
      
      # remove TADA.QAPPDocAvailable column
      clean.data <- dplyr::select(clean.data, -TADA.QAPPDocAvailable)
      
      return(clean.data)
    }
  }
  
  # if no associated QAPP url data is in the data set
  if (nrow(QAPPdoc.data) == 0) {
    print("No changes were made, because we did not find any QAPP document url data in your dataframe")
    
    return(.data)
  }
}


#' Invalid coordinates
#'
#' This function identifies and flags invalid coordinate data. When
#' clean_outsideUSA = FALSE and clean_imprecise = FALSE,
#' a column will be appended titled "TADA.InvalidCoordinates" with the following
#' flags: 1) If the latitude is less than zero, the row will be
#' flagged with "LAT_OutsideUSA", 2) If the longitude is greater than zero AND less than 145,
#' the row will be flagged as "LONG_OutsideUSA", 3) If the latitude or longitude
#' contains the string, "999", the row will be flagged as invalid, and 4) Finally,
#' precision can be measured by the number of decimal places in the latitude and longitude
#' provided. If either the latitude or longitude does not have any numbers to the 
#' right of the decimal point, the row will be flagged as "Imprecise".
#'
#' @param .data TADA dataframe
#' @param clean_outsideUSA Boolean argument; removes data with coordinates outside
#' of the United States when clean_outsideUSA = TRUE. Default is clean = FALSE.
#' @param clean_imprecise Boolean argument; removes imprecise data when
#' clean_imprecise = TRUE. Default is clean_imprecise = FALSE.
#' @param errorsonly Boolean argument; Return flagged data only when errorsonly = FALSE
#'
#' @return When either the clean_outsideUSA or clean_imprecise argument is FALSE,
#' a column flagging rows with the respective QA check is appended to the input
#' dataframe. When either argument is TRUE, "invalid" or "imprecise" data is
#' removed, respectively.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Flag, but do not remove, data with invalid coordinates in new column 
#' titled "TADA.InvalidCoordinates":
#' # Return ALL data:
#' InvalidCoord_flags <- InvalidCoordinates(Nutrients_Utah)
#' 
#' # Flag, but do not remove, data with invalid coordinates in new column 
#' titled "TADA.InvalidCoordinates"
#' # Return ONLY the flagged data:
#' InvalidCoord_flags_errorsonly <- InvalidCoordinates
#' (Nutrients_Utah, errorsonly = TRUE)
#' 
#' # Remove data with coordinates outside the USA, but keep flagged data with 
#' imprecise coordinates:
#' OutsideUSACoord_removed <- InvalidCoordinates(Nutrients_Utah, 
#' clean_outsideUSA = TRUE)
#' 
#' # Remove data with imprecise coordinates, but keep flagged data with coordinates outside the USA:
#' # imprecise data may include a series of 999's to the right of the decimal points
#' # alternatively, imprecise data may have less than 3 significant figures to the right
#' # of the decimal point
#' ImpreciseCoord_removed <- InvalidCoordinates(Nutrients_Utah, clean_imprecise = TRUE)
#' 
#' # Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
#' InvalidCoord_removed <- InvalidCoordinates(Nutrients_Utah, clean_outsideUSA = TRUE, clean_imprecise = TRUE)

InvalidCoordinates <- function(.data, clean_outsideUSA = FALSE, clean_imprecise = FALSE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean_outsideUSA is boolean
  checkType(clean_outsideUSA, "logical")
  # check clean_imprecise is boolean
  checkType(clean_imprecise, "logical")
  # check .data has required columns
  checkColumns(.data, c("LatitudeMeasure", "LongitudeMeasure"))
  #check lat and long are "numeric"
  if (class(.data$LongitudeMeasure) != "numeric") {
    warning("LongitudeMeasure field must be numeric")
  }
  
  if (class(.data$LatitudeMeasure) != "numeric") {
    warning("LatitudeMeasure field must be numeric")
  }
  
  # execute function after checks are passed
  .data <- .data %>%
    dplyr::mutate(TADA.InvalidCoordinates = dplyr::case_when(
      LatitudeMeasure < 0 ~ "LAT_OutsideUSA",
      LongitudeMeasure > 0 & LongitudeMeasure < 145 ~ "LONG_OutsideUSA",
      grepl("999", LatitudeMeasure) ~ "Imprecise_Latincludes999",
      grepl("999", LongitudeMeasure) ~ "Imprecise_Longincludes999",
      #for below, lat and long fields must be numeric
      # this checks if there are at least 3 significant figures to the 
      # right of the decimal point
      sapply(.data$LatitudeMeasure, decimalplaces) < 3 
      | sapply(.data$LongitudeMeasure, decimalplaces) < 3 ~ "Imprecise_lessthan3decimaldigits"
    ))
  
  # clean output, remove all data with invalid station metadata
  if ((clean_outsideUSA == TRUE) & (clean_imprecise == TRUE)) {
    .data <- dplyr::filter(.data, is.na(TADA.InvalidCoordinates) == TRUE)
  }
  
  if ((clean_outsideUSA == FALSE) & (clean_imprecise == TRUE)) {
    .data <- dplyr::filter(.data, 
                           TADA.InvalidCoordinates != "Imprecise_Latincludes999" 
                           & TADA.InvalidCoordinates != "Imprecise_Longincludes999"
                           & TADA.InvalidCoordinates != "Imprecise_lessthan3decimaldigits"
                           | is.na(TADA.InvalidCoordinates) == TRUE)
  }
  
  if ((clean_outsideUSA == TRUE) & (clean_imprecise == FALSE)) {
    .data <- dplyr::filter(.data, 
                           TADA.InvalidCoordinates != "LAT_OutsideUSA" 
                           & TADA.InvalidCoordinates != "LONG_OutsideUSA"
                           | is.na(TADA.InvalidCoordinates) == TRUE)
  }
  
  #return only flagged data if errorsonly = true
  if ((errorsonly == TRUE)) {
    .data <- dplyr::filter(.data, is.na(TADA.InvalidCoordinates) != TRUE)
  }
  
  if (all(is.na(.data$TADA.InvalidCoordinates) == TRUE)) {
    print("All invalid coordinates were removed or your dataframe does not contain monitoring stations with invalid coordinates")
    return(dplyr::select(.data, -TADA.InvalidCoordinates))
  } else {
    return(.data)
  }
}
