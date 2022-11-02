#' Check for Invalid Analytical Methods
#'
#' Function checks the validity of each characteristic-analytical method
#' combination in the dataframe. When clean = TRUE, rows with invalid
#' characteristic-analytical method combinations are removed. Default is
#' clean = TRUE.
#' 
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-analytical
#' method combinations from the dataframe when clean = TRUE. Default is
#' clean = TRUE.
#' 
#' @return When clean = FALSE, this function adds the following column to your
#' dataframe: WQX.AnalyticalMethodValidity. This column flags invalid 
#' CharacteristicName, ResultAnalyticalMethod/MethodIdentifier,
#' and ResultAnalyticalMethod/MethodIdentifierContext combinations in your dataframe
#' as either "Nonstandardized", "Invalid", or "Valid". When clean = TRUE, "Invalid" rows are removed from the dataframe and no column 
#' will be appended.
#' 
#' @export
#'
#' 

InvalidMethod <- function(.data, clean = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check .data has required columns
  required_cols <- c(
    "CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext"
  )
  checkColumns(.data, required_cols)

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
  
  # flagged output
  if (clean == FALSE) {
    return(check.data)
  }
  
  # clean output
  if (clean == TRUE) {
    # filter out invalid characteristic-unit-method combinations
    clean.data <- dplyr::filter(check.data, WQX.AnalyticalMethodValidity != "Valid")
    
    # remove WQX.AnalyticalMethodValidity column
    clean.data <- dplyr::select(clean.data, -WQX.AnalyticalMethodValidity)
    
    return(clean.data)
  }
}


#' Check for Aggregated Continuous Data
#'
#' The Water Quality Portal (WQP) is not designed to store high-frequency
#' sensor data. However, sometimes data providers choose to aggregate
#' their continuous data and submit it to WQP as one value. This type of data
#' may not be suitable for integration with discrete water quality data for
#' assessments. Therefore, this function uses metadata submitted by data providers
#' to flags rows with aggregated continuous data. This is done by flagging results
#' where the ResultDetectionConditionText = "Reported in Raw Data (attached)".
#' When clean = TRUE, rows with aggregated continuous data are removed from the
#' dataframe and no column will be appended. Default is clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes aggregated continuous data from
#' the dataframe when clean = TRUE. Default is clean = TRUE.
#'
#' @return When clean = FALSE, a column flagging rows with aggregated continuous
#' data is appended to the input data set. When clean = TRUE, aggregated
#' continuous data is removed from the dataframe.
#'
#' @export

AggregatedContinuousData <- function(.data, clean = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check .data has required columns
  checkColumns(.data, "ResultDetectionConditionText")

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
    
    # flagged output
    if (clean == FALSE) {
      return(flag.data)
    }
    
    # clean output
    if (clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(flag.data, !(TADA.AggregatedContinuousData %in% "Y"))
      
      # remove TADA.AggregatedContinuousData column
      clean.data <- dplyr::select(clean.data, -TADA.AggregatedContinuousData)
      
      return(clean.data)
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
#' Sometimes multiple organizations submit the exact same data to Water Quality
#' Portal (WQP), which can affect water quality analyses and assessments. This
#' function checks for and identifies data that is identical in all fields
#' excluding organization-specific and comment text fields. Each pair or group
#' of potential duplicate rows is flagged with a unique ID. When clean = TRUE,
#' the function retains the first occurrence of each potential duplicate in the
#' dataframe. Default is clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes potential duplicate data from
#' the dataframe when clean = TRUE. When clean = FALSE, a column indicating
#' potential duplicate rows with a unique number linking rows is appended to the
#' input data set. Default is clean = TRUE.
#'
#' @return When clean = FALSE, the following column will be added to you dataframe: 
#' TADA.PotentialDupRowID. This column flags potential duplicate rows of data 
#' in your dataframe, and assigns each potential duplicate combination a unique number
#' linking the two potential duplication rows. When clean = FALSE the first of
#' each group of potential duplicate rows will be removed from the dataframe
#' and no column is appended.
#'
#' @export

PotentialDuplicateRowID <- function(.data, clean = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check .data has required columns
  required_cols <- c(
    "ActivityIdentifier", "ActivityConductingOrganizationText",
    "OrganizationFormalName", "OrganizationIdentifier",
    "ProjectIdentifier", "ResultCommentText",
    "ActivityCommentText"
    )
  checkColumns(.data, required_cols)

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
  
  # flagged output
  if (clean == FALSE) {
    return(flag.data)
  }
  
  # clean output
  if (clean == TRUE) {
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
}


#' Check Result Value Against WQX Upper Threshold
#'
#' EPA's Water Quality Exchange (WQX) has generated statistics and data from
#' millions of water quality data points around the country. This functions
#' leverages that statistical data from WQX to flag any data that is above the
#' upper threshold of result values submitted to WQX for a given characteristic.
#' When clean = TRUE, rows with values that are above the upper WQX threshold
#' are removed from the dataframe and no column will be appended. Default is
#' clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data that is above the upper WQX
#' threshold from the dataframe when clean = TRUE. Default is clean = TRUE
#'
#' @return When clean = FALSE, the following column is added to your dataframe: 
#' AboveNationalWQXUpperThreshold. This column flags rows with data that are above
#' the upper WQX threshold. When clean = TRUE, data that is above the upper 
#' WQX threshold is removed from the dataframe.
#'
#' @export

AboveNationalWQXUpperThreshold <- function(.data, clean = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check .data has required columns
  required_cols <- c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode"
  )
  checkColumns(.data, required_cols)

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$ResultMeasureValue)) {
    stop("The ResultMeasureValue column must of class 'numeric'.")
  }

  # execute function after checks are passed

  # delete existing flag column
  if (("AboveNationalWQXUpperThreshold" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -AboveNationalWQXUpperThreshold)
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
    dplyr::mutate(AboveNationalWQXUpperThreshold = dplyr::case_when(
      ResultMeasureValue >= Maximum ~ as.character("Y"),
      ResultMeasureValue < Maximum ~ as.character("N")
    ))
  
  # remove extraneous columns, fix field names
  flag.data <- flag.data %>%
    dplyr::select(-"Maximum")
  
  # reorder column names to match .data
  # get .data column names
  col.order <- colnames(.data)
  # add TADA.PotentialDupRowID column to the list
  col.order <- append(col.order, "AboveNationalWQXUpperThreshold")
  # reorder columns in flag.data
  flag.data <- flag.data[, col.order]
  
  if (clean == FALSE) {
    return(flag.data)
  }
  if (clean == TRUE) {
    # filter out rows where AboveNationalWQXUpperThreshold = Y; remove AboveNationalWQXUpperThreshold column
    clean.data <- flag.data %>%
      dplyr::filter(!(AboveNationalWQXUpperThreshold %in% "Y")) %>%
      dplyr::select(-AboveNationalWQXUpperThreshold)
    
    return(clean.data)

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
#' clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data that is below the lower WQX
#' threshold from the dataframe when clean = TRUE. Default is clean = TRUE.
#'
#' @return When clean = FALSE, the following column is added to your dataframe: 
#' BelowNationalWQXLowerThreshold. This column flags rows with data that are below
#' the lower WQX threshold. When clean = TRUE, data that is below the lower 
#' WQX threshold is removed from the dataframe.
#' 
#' @export

BelowNationalWQXLowerThreshold <- function(.data, clean = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")

# check .data has required columns
  required_cols <- c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode"
  )
  checkColumns(.data, required_cols)

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$ResultMeasureValue)) {
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }

  # execute function after checks are passed
  # delete existing flag column
  if (("BelowWQXUpperThreshold" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -BelowWQXUpperThreshold)
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
    dplyr::mutate(BelowNationalWQXLowerThreshold = dplyr::case_when(
      ResultMeasureValue <= Minimum ~ as.character("Y"),
      ResultMeasureValue > Minimum ~ as.character("N")
    ))
  
  # remove extraneous columns, fix field names
  flag.data <- flag.data %>%
    dplyr::select(-"Minimum")
  
  # reorder column names to match .data
  # get .data column names
  col.order <- colnames(.data)
  # add TADA.PotentialDupRowID column to the list
  col.order <- append(col.order, "BelowNationalWQXLowerThreshold")
  # reorder columns in flag.data
  flag.data <- flag.data[, col.order]
  
  if (clean == FALSE) {
    return(flag.data)
  }
  if (clean == TRUE) {
    # filter out rows where BelowNationalWQXLowerThreshold = Y; remove BelowNationalWQXLowerThreshold column
    clean.data <- flag.data %>%
      dplyr::filter(!(BelowNationalWQXLowerThreshold %in% "Y")) %>%
      dplyr::select(-BelowNationalWQXLowerThreshold)
    
    return(clean.data)

  }
}



#' Check data for an approved QAPP
#'
#' Function checks data submitted under the column "QAPPApprovedIndicator".
#' Some organizations submit data for this field to indicate if the data
#' produced has an approved Quality Assurance Project Plan (QAPP) or not.
#' Y indicates yes, N indicates no.  This function has two default inputs:
#' clean = TRUE and cleanNA = FALSE. The default removes rows of data where the
#' QAPPApprovedIndicator equals "N". Users could alternatively remove both N's
#' and NA's using the inputs clean = TRUE and cleanNA = TRUE. If both
#' clean = FALSE and cleanNA = FALSE, the function will not make any changes to
#' the data.
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
#'
#' @return When clean = FALSE and cleanNA = FALSE, no data is removed from the
#' dataframe.
#'
#' @export
#'

QAPPapproved <- function(.data, clean = TRUE, cleanNA = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check cleanNA is boolean
  checkType(cleanNA, "logical") 
  # check .data has required columns
  checkColumns(.data, "QAPPApprovedIndicator")

  # execute function after checks are passed
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
  return(.data)
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
#' Function identifies and flags invalid coordinate data. When
#' clean_outsideUSA = FALSE and clean_imprecise = FALSE,
#' a column will be appended titled "TADA.InvalidCoordinates" with the following
#' flags (if relevant to dataframe). If the latitude is less than zero, the row will be
#' flagged with "LAT_OutsideUSA". If the longitude is greater than zero AND less than 145,
#' the row will be flagged as "LONG_OutsideUSA". If the latitude or longitude
#' contains the string, "999", the row will be flagged as invalid. Finally,
#' precision can be measured by the number of decimal places in the latitude and longitude
#' provided. If either does not have any numbers to the right of the decimal point,
#' the row will be flagged as "Imprecise".
#'
#' @param .data TADA dataframe
#' @param clean_outsideUSA Boolean argument; removes data with coordinates outside
#' of the United States when clean_outsideUSA = TRUE. Default is clean = FALSE.
#' @param clean_imprecise Boolean arguments; removes imprecise data when
#' clean_imprecise = TRUE. Default is clean_imprecise = FALSE.
#'
#' @return When either the clean_outsideUSA or clean_imprecise argument is FALSE,
#' a column flagging rows with the respective QA check is appended to the input
#' dataframe. When either argument is TRUE, "invalid" or "imprecise" data is
#' removed, respectively.
#'
#' @export
#'

InvalidCoordinates <- function(.data, clean_outsideUSA = FALSE, clean_imprecise = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean_outsideUSA is boolean
  checkType(clean_outsideUSA, "logical")
  # check clean_imprecise is boolean
  checkType(clean_imprecise, "logical")
  # check .data has required columns
  checkColumns(.data, c("LatitudeMeasure", "LongitudeMeasure"))

  # execute function after checks are passed
  .data <- .data %>%
    dplyr::mutate(TADA.InvalidCoordinates = dplyr::case_when(
      LatitudeMeasure < 0 ~ "LAT_OutsideUSA",
      LongitudeMeasure > 0 & LongitudeMeasure < 145 ~ "LONG_OutsideUSA",
      grepl("999", LatitudeMeasure) ~ "Imprecise",
      grepl("999", LongitudeMeasure) ~ "Imprecise",
      sapply(.data$LatitudeMeasure, decimalnumcount) < 4 | sapply(.data$LongitudeMeasure, decimalnumcount) < 4 ~ "Imprecise"
    ))
  
  # clean output, remove all data for stations outside of the US
  if ((clean_outsideUSA == TRUE) & (clean_imprecise == TRUE)) {
    .data <- dplyr::filter(.data, is.na(TADA.InvalidCoordinates) == TRUE)
  }
  
  if ((clean_outsideUSA == FALSE) & (clean_imprecise == TRUE)) {
    .data <- dplyr::filter(.data, TADA.InvalidCoordinates != "Imprecise" | is.na(TADA.InvalidCoordinates) == TRUE)
  }
  
  if ((clean_outsideUSA == TRUE) & (clean_imprecise == FALSE)) {
    .data <- dplyr::filter(.data, TADA.InvalidCoordinates == "Imprecise" | is.na(TADA.InvalidCoordinates) == TRUE)
  }
  
  if (all(is.na(.data$TADA.InvalidCoordinates) == TRUE)) {
    print("All invalid coordinates were removed or your dataframe does not contain monitoring stations with invalid coordinates")
    return(dplyr::select(.data, -TADA.InvalidCoordinates))
  } else {
    return(.data)
  }
}
