#' Check for Uncommon Analytical Methods
#' 
#' Function that checks the validity of each characteristic-analytical method 
#' combination in the dataset. When clean = TRUE, rows with invalid 
#' characteristic-analytical method combinations are removed. 
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes "Invalid" characteristic-analytical 
#' method combinations from the dataset when clean = TRUE. Default is 
#' clean = FALSE.
#'
#' @return When clean = FALSE, a column indicating the validity of the 
#' combination of CharacteristicName, ResultAnalyticalMethod/MethodIdentifier, 
#' and ResultAnalyticalMethod/MethodIdentifierContext values is appended to the
#' input data set. When clean = FALSE, "Invalid" rows will be removed from the 
#' dataset and no column will be appended.
#' 
#' @export
#' 


UncommonAnalyticalMethodID <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier", 
           "ResultAnalyticalMethod.MethodIdentifierContext") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier", 
           "ResultAnalyticalMethod.MethodIdentifierContext") %in% colnames(.data)) == TRUE) {
    # delete existing flag column
    if(("UncommonAnalyticalMethod" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -UncommonAnalyticalMethod)
    }
    # read in speciation reference table from sysdata.rda and filter
    meth.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicMethod")
   
    # define raw.data
    raw.data <- .data
     # duplicate and capitalize result unit column in raw.data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    
    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(raw.data, meth.ref[, c("Characteristic", "Source", "Status", "Value")],
                        by.x = c("Char.Upper", "ResultAnalyticalMethod.MethodIdentifier", 
                                 "ResultAnalyticalMethod.MethodIdentifierContext"),
                        by.y = c("Characteristic", "Value", "Source"), all.x = TRUE)
    
    # rename Status column to UncommonAnalyticalMethod
    check.data <- check.data %>%
      dplyr::rename(UncommonAnalyticalMethod = Status)
    # rename values in UncommonAnalyticalMethod
    check.data["UncommonAnalyticalMethod"][check.data["UncommonAnalyticalMethod"] == "Valid"] <- "N"
    check.data["UncommonAnalyticalMethod"][check.data["UncommonAnalyticalMethod"] == "Invalid"] <- "Y"
    # rename NA values to Unknown in UncommonAnalyticalMethod column 
    check.data["UncommonAnalyticalMethod"][is.na(check.data["UncommonAnalyticalMethod"])] <- "Unknown"
    
    # remove extraneous columns, fix field names
    check.data <- check.data %>%
      dplyr::select(-c("Char.Upper"))
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add UncommonAnalyticalMethod column to the list
    col.order <- append(col.order, "UncommonAnalyticalMethod")
      # reorder columns in flag.data
    check.data <- check.data[, col.order]
    
    # flagged output
    if(clean == FALSE) {
      return(check.data)
    }
    
    # clean output
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, UncommonAnalyticalMethod != "Y")
      
      # remove UncommonAnalyticalMethod column
      clean.data <- dplyr::select(clean.data, -UncommonAnalyticalMethod)
      
      return(clean.data)
    } else {
      stop("clean argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Check for Aggregated Continuous Data
#' 
#' The Water Quality Portal (WQP) is not designed to store high-frequency 
#' (i.e. sensor) data. However, sometimes data providers choose to aggregate
#' their continuous data and submit it to WQP as one value. This type of data
#' may not be suitable for a water quality assessment, therefore, this function
#' uses metadata submitted by data providers to flags rows with aggregated 
#' continuous data. When clean = TRUE, rows with aggregated continuous data
#' are removed from the dataset and no column will be appended.
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes aggregated continuous data from 
#' the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column flagging rows with aggregated continuous
#' data is appended to the input data set. When clean = TRUE, aggregated 
#' continuous data is removed from the dataset.
#' 
#' @export

AggregatedContinuousData <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("ResultDetectionConditionText") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("ResultDetectionConditionText") %in% colnames(.data)) == TRUE) {
    
    # flag continuous data
      # make cont.data data frame
    cont.data <- dplyr::filter(.data, 
                               ResultDetectionConditionText == "Reported in Raw Data (attached)")
    
    # if there is aggregated continuous data is in the data set
    if(nrow(cont.data) != 0){
      # append ContDataFlag column
      cont.data$AggregatedContinuousData <- "Y"
      # join cont.data to flag.data
      flag.data <- merge(.data, cont.data, all.x = TRUE)
      
      # flagged output
      if(clean == FALSE) {
        return(flag.data)
      }
      
      # clean output
      if(clean == TRUE) {
        # filter out invalid characteristic-unit-media combinations
        clean.data <- dplyr::filter(flag.data, !(AggregatedContinuousData %in% "Y"))
        
        # remove AggregatedContinuousData column
        clean.data <- dplyr::select(clean.data, -AggregatedContinuousData)
        
        return(clean.data)
      } else {
        stop("clean argument must be Boolean (TRUE or FALSE)")
      }
    }  
    
    # if no aggregated continuous data is in the data set
    if(nrow(cont.data) == 0){
      warning("The dataset does not contain aggregated continuous data.")
      
      return(.data)
    }   
  }
}


#' Check for Potential Duplicates
#' 
#' **Placeholder text for function description
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes potential duplicate data from 
#' the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column indicating potential duplicate rows with
#' a unique number linking potential duplicate rows is appended to the
#' input data set. When clean = FALSE the first of each group of potential 
#' duplicate rows will be removed from the dataset and no column is appended.
#' 
#' @export

PotentialDuplicateRowID <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("ActivityIdentifier", "ActivityConductingOrganizationText",
           "OrganizationFormalName", "OrganizationIdentifier",
           "ProjectIdentifier", "ResultCommentText", 
           "ActivityCommentText") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA.
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("ActivityIdentifier", "ActivityConductingOrganizationText",
           "OrganizationFormalName", "OrganizationIdentifier",
           "ProjectIdentifier", "ResultCommentText", 
           "ActivityCommentText") %in% colnames(.data)) == TRUE) {
    
    # get list of field names in .data
    field.names <- colnames(.data)
    # create list of fields to exclude when looking for duplicate rows
    excluded.fields <- c("ActivityIdentifier", "ActivityConductingOrganizationText",
                         "OrganizationFormalName", "OrganizationIdentifier",
                         "ProjectIdentifier", "ResultCommentText", "ActivityCommentText")
    # create list of fields to check for duplicates across
    dupe.fields <- field.names[!field.names %in% excluded.fields] 
    
    # subset list of duplicate rows
    dupe.data <- .data[duplicated(.data[dupe.fields]),]
    
    # flag potential duplicates
    dupe.data$PotentialDupRowID <- as.integer(seq_len(nrow(dupe.data)))
    
    # merge flag column into .data
    flag.data <- merge(.data, dupe.data, by = dupe.fields, all.x = TRUE)
    
    # remove extraneous columns, fix field names
    flag.data <- flag.data %>%
      # remove ".x" suffix from column names
      dplyr::rename_at(dplyr::vars(dplyr::ends_with(".x")), 
                       ~stringr::str_replace(., "\\..$","")) %>%
      # remove columns with ".y" suffix
      dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add PotentialDupRowID column to the list
    col.order <- append(col.order, "PotentialDupRowID")
      # reorder columns in flag.data
    flag.data <- flag.data[, col.order]
    
    # flagged output
    if(clean == FALSE) {
      return(flag.data)
    }
    
    # clean output
    if(clean == TRUE) {
      # remove duplicate rows
        # seperate data into 2 dataframes by PotentialDupRowID (no NAs and NAs)
      dup.data <- flag.data[!is.na(flag.data$PotentialDupRowID),]
      NAdup.data <- flag.data[is.na(flag.data$PotentialDupRowID),] 
      
      nodup.data <- dup.data[!duplicated(dup.data$PotentialDupRowID),]
      
      clean.data <-rbind(nodup.data, NAdup.data)
      
      # remove PotentialDupRowID column
      clean.data <- dplyr::select(clean.data, -PotentialDupRowID)
      
      return(clean.data)
    } else {
      stop("clean argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Check Result Value Against WQX Upper Threshold
#' 
#' EPA's Water Quality Exchange (WQX) has generated statistics and data from 
#' millions of water quality data points around the country. This functions 
#' leverages that statistical data from WQX to flag any data that is above the 
#' upper threshold of result values submitted to WQX for a given characteristic.
#' When clean = TRUE, rows with values that are above the upper WQX threshold
#' are removed from the dataset and no column will be appended.
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes data that is above the upper WQX
#' threshold from the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column flagging rows with data that are above
#' the upper WQX threshold is appended to the input data set. When clean = TRUE,
#' data that is above the upper WQX threshold is removed from the dataset.
#' 
#' @export

AboveNationalWQXUpperThreshold <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }

  
  # check ResultMeasureValue column is of class numeric
  if(class(.data$ResultMeasureValue) != "numeric") {
    stop("The ResultMeasureValue column must of class 'numeric'.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == TRUE) {
    
    # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
    unit.ref <- TADA::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" & 
                      Status == "Valid")
    
    # define raw.data
    raw.data <- .data
    # duplicate and capitalize CharName, ActivityMediaName, and ResultMeasureUnitCode columns in .data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    raw.data$Media.Upper <- toupper(raw.data$ActivityMediaName)
    raw.data$Unit.Upper <- toupper(raw.data$ResultMeasure.MeasureUnitCode)
    
    # join unit.ref to raw.data
    check.data <- merge(raw.data, unit.ref[, c("Characteristic", "Source",
                                            "Value", "Value.Unit", "Maximum",
                                            "Conversion.Factor")],
                        by.x = c("Char.Upper", "Media.Upper", "Unit.Upper"),
                        by.y = c("Characteristic", "Source", "Value"), all.x = TRUE)
      # Run ResultValueSpecialCharacter function to remove special characters and convert class to numeric
      check.data <- ResultValueSpecialCharacters(check.data, clean = TRUE)
    
    # Convert result measure value to "Value.Unit" units
    check.data$Converted.Value <- 
      check.data[,"ResultMeasureValue"]/check.data[,"Conversion.Factor"]
    
    # Create flag column, flag rows where Converted.Value > Maximum
    flag.data <- check.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(AboveWQXUpperThreshold = dplyr::case_when(
        Converted.Value >= Maximum ~ as.character("Y"),
        Converted.Value < Maximum ~ as.character("N")))
    
    # remove extraneous columns, fix field names
    flag.data <- flag.data %>%
      dplyr::select(-c("Char.Upper", "Media.Upper", "Unit.Upper", "Value.Unit",
                       "Maximum", "Conversion.Factor", "Converted.Value"))
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add PotentialDupRowID column to the list
    col.order <- append(col.order, "AboveWQXUpperThreshold")
      # reorder columns in flag.data
    flag.data <- flag.data[, col.order]
    
    if(clean == FALSE) {
      return(flag.data)
    }
    if(clean == TRUE) {
      # filter out rows where AboveWQXUpperThreshold = Y; remove AboveWQXUpperThreshold column
      clean.data <- flag.data %>%
        dplyr::filter(!(AboveWQXUpperThreshold %in% "Y")) %>%
        dplyr::select(-AboveWQXUpperThreshold)
      
      return(clean.data)
    } else {
      stop("clean argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Check Result Value Against WQX Lower Threshold
#' 
#' EPA's Water Quality Exchange (WQX) has generated statistics and data from 
#' millions of water quality data points around the country. This functions 
#' leverages that statistical data from WQX to flag any data that is below the 
#' lower threshold of result values submitted to WQX for a given characteristic.
#' When clean = TRUE, rows with values that are below the lower WQX threshold 
#' are removed from the dataset and no column will be appended.
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes data that is below the lower  WQX
#' threshold from the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column flagging rows with data that is below the
#' lower WQX threshold is appended to the input data set. When clean = TRUE, 
#' data that is below the lower WQX threshold is removed from the dataset.
#' 
#' @export

BelowNationalWQXUpperThreshold <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  
  # check ResultMeasureValue column is of class numeric
  if(class(.data$ResultMeasureValue) != "numeric") {
    stop("The ResultMeasureValue column must of class 'numeric'.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == TRUE) {
    
    # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
    unit.ref <- TADA::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" & 
                      Status == "Valid")
    
    # define raw.data
    raw.data <- .data
    # duplicate and capitalize CharName, ActivityMediaName, and ResultMeasureUnitCode columns in .data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    raw.data$Media.Upper <- toupper(raw.data$ActivityMediaName)
    raw.data$Unit.Upper <- toupper(raw.data$ResultMeasure.MeasureUnitCode)
    
    # join unit.ref to raw.data
    check.data <- merge(raw.data, unit.ref[, c("Characteristic", "Source",
                                               "Value", "Value.Unit", "Minimum",
                                               "Conversion.Factor")],
                        by.x = c("Char.Upper", "Media.Upper", "Unit.Upper"),
                        by.y = c("Characteristic", "Source", "Value"), all.x = TRUE)
    # Run ResultValueSpecialCharacter function to remove special characters and convert class to numeric
    check.data <- ResultValueSpecialCharacters(check.data, clean = TRUE)
    
    # Convert result measure value to "Value.Unit" units
    check.data$Converted.Value <- 
      check.data[,"ResultMeasureValue"]/check.data[,"Conversion.Factor"]
    
    # Create flag column, flag rows where Converted.Value < Minimum
    flag.data <- check.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(BelowWQXUpperThreshold = dplyr::case_when(
        Converted.Value <= Minimum ~ as.character("Y"),
        Converted.Value > Minimum ~ as.character("N")))
    
    # remove extraneous columns, fix field names
    flag.data <- flag.data %>%
      dplyr::select(-c("Char.Upper", "Media.Upper", "Unit.Upper", "Value.Unit",
                       "Minimum", "Conversion.Factor", "Converted.Value"))
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add PotentialDupRowID column to the list
    col.order <- append(col.order, "BelowWQXUpperThreshold")
    # reorder columns in flag.data
    flag.data <- flag.data[, col.order]
    
    if(clean == FALSE) {
      return(flag.data)
    }
    if(clean == TRUE) {
      # filter out rows where BelowWQXUpperThreshold = Y; remove BelowWQXUpperThreshold column
      clean.data <- flag.data %>%
        dplyr::filter(!(BelowWQXUpperThreshold %in% "Y")) %>%
        dplyr::select(-BelowWQXUpperThreshold)
      
      return(clean.data)
    } else {
      stop("clean argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Flag or remove result without an approved QAPP
#' 
#' Organizations that submit data to the Water Quality Portal (WQP) sometimes 
#' include metadata with their results to indicate if the data produced has an
#' approved Quality Assurance Project Plan (QAPP). This check reviews data
#' submitted under the column "QAPPApprovedIndicator". When clean=false, 
#' a column will be appended to flag results that do not have an approved QAPP.
#' When clean = TRUE, rows with values that do not have an approved QAPP are 
#' removed from the dataset and no column will be appended.This function should
#' only be used to remove data if an approved QAPP is required to use data in 
#' water quality assessments
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes data without an Approved QAPP from 
#' the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column is appended to the input data set that
#' flags rows without an Approved QAPP. When clean = TRUE, data without an  
#' Approved QAPP is removed from the dataset.
#' 
#' @export

QAPPapproved <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("QAPPApprovedIndicator") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("QAPPApprovedIndicator") %in% colnames(.data)) == TRUE) {
    
    # flag data where QAPP was not approved
    # make QAPPapproved.data data frame
    QAPPapproved.data <- dplyr::filter(.data, 
                              QAPPApprovedIndicator == "N")
    
    # if there is data without an approved QAPP in the data set
    if(nrow(QAPPapproved.data) != 0){
      # append flag column
      QAPPapproved.data$QAPPapproved <- "N"
      # join QAPPapproved.data to flag.data
      flag.data <- merge(.data, QAPPapproved.data, all.x = TRUE)
      
      # flagged output
      if(clean == FALSE) {
        return(flag.data)
      }
      
      # clean output
      if(clean == TRUE) {
        # remove data without an approved QAPP
        clean.data <- dplyr::filter(flag.data, !(QAPPapproved %in% "N"))
        
        # remove QAPPapproved column
        clean.data <- dplyr::select(clean.data, -QAPPapproved)
        
        return(clean.data)
      } else {
        stop("clean argument must be Boolean (TRUE or FALSE)")
      }
    }  
    
    # if no QAPP approved indicator data is in the data set
    if(nrow(QAPPapproved.data) == 0){
      warning("The dataset does not contain QAPP approval indicator data.")
      
      return(.data)
    }   
  }
}


#' Flag or remove result without an approved QAPP
#' 
#' Organizations that submit data to the Water Quality Portal (WQP) sometimes 
#' include metadata with their results to indicate if the data produced has an
#' associated Quality Assurance Project Plan (QAPP). This check reviews data
#' submitted under the column "ProjectFileUrl" to determine if
#' a QAPP document is available to review. When clean=false, 
#' a column will be appended to flag results that do not have an associated 
#' QAPP document url provided. When clean = TRUE, rows with values that do not
#' have an associated QAPP document are removed from the dataset and no column
#' will be appended. This function should only be used to remove data if an 
#' accompanying QAPP document is required to use data in assessments
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes data without an associated QAPP
#' document from the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column is appended to the input data set that
#' flags rows without an associated QAPP document. When clean = TRUE, 
#' data without an associated QAPP document is removed from the dataset.
#' 
#' @export
#' 
QAPPDocAvailable <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("ProjectFileUrl") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("ProjectFileUrl") %in% colnames(.data)) == TRUE) {
    
    # flag data where QAPP document url is not provided
    # make QAPPdoc.data data frame
    QAPPdoc.data <- dplyr::filter(.data, ProjectFileUrl == c("null", "NA"))
    
    # if there is data without an associated QAPP url in the data set
    if(nrow(QAPPdoc.data) != 0){
      # append flag column
      QAPPdoc.data$QAPPDocAvailable <- "N"
      # join QAPPdoc.data to flag.data
      flag.data <- merge(.data, QAPPdoc.data, all.x = TRUE)
      
      # flagged output
      if(clean == FALSE) {
        return(flag.data)
      }
      
      # clean output
      if(clean == TRUE) {
        # remove data without an associated QAPP url
        clean.data <- dplyr::filter(flag.data, !(QAPPDocAvailable %in% "N"))
        
        # remove QAPPDocAvailable column
        clean.data <- dplyr::select(clean.data, -QAPPDocAvailable)
        
        return(clean.data)
      } else {
        stop("clean argument must be Boolean (TRUE or FALSE)")
      }
    }  
    
    # if no associated QAPP url data is in the data set
    if(nrow(QAPPdoc.data) == 0){
      warning("The dataset does not contain QAPP document url data.")
      
      return(.data)
    }   
  }
}
