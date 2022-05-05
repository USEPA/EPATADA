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
    if(("WQX.UncommonAnalyticalMethod" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.UncommonAnalyticalMethod)
    }
    # read in speciation reference table from sysdata.rda and filter
    meth.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicMethod")
    
    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(.data, meth.ref[, c("Characteristic", "Source", "Status", "Value")],
                        by.x = c("CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier", 
                                 "ResultAnalyticalMethod.MethodIdentifierContext"),
                        by.y = c("Characteristic", "Value", "Source"), all.x = TRUE)
    
    # rename Status column to WQX.UncommonAnalyticalMethod
    check.data <- check.data %>%
      dplyr::rename(WQX.UncommonAnalyticalMethod = Status)
    # rename values in WQX.UncommonAnalyticalMethod
    check.data["WQX.UncommonAnalyticalMethod"][check.data["WQX.UncommonAnalyticalMethod"] == "Valid"] <- "N"
    check.data["WQX.UncommonAnalyticalMethod"][check.data["WQX.UncommonAnalyticalMethod"] == "Invalid"] <- "Y"
    # rename NA values to Unknown in WQX.UncommonAnalyticalMethod column 
    check.data["WQX.UncommonAnalyticalMethod"][is.na(check.data["WQX.UncommonAnalyticalMethod"])] <- "Unknown"
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add WQX.UncommonAnalyticalMethod column to the list
    col.order <- append(col.order, "WQX.UncommonAnalyticalMethod")
      # reorder columns in flag.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
      check.data <- check.data %>%
        dplyr::relocate("WQX.UncommonAnalyticalMethod", 
                        .after = "ResultAnalyticalMethod.MethodName")
    # flagged output
    if(clean == FALSE) {
      return(check.data)
    }
    
    # clean output
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, WQX.UncommonAnalyticalMethod != "Y")
      
      # remove WQX.UncommonAnalyticalMethod column
      clean.data <- dplyr::select(clean.data, -WQX.UncommonAnalyticalMethod)
      
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
    
    # join unit.ref to raw.data
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source",
                                            "Value", "Maximum")],
                        by.x = c("CharacteristicName", "ActivityMediaName", 
                                 "ResultMeasure.MeasureUnitCode"),
                        by.y = c("Characteristic", "Source", "Value"), all.x = TRUE)
      # Run WQXTargetUnits function to convert ResultMeasureValue class to numeric
      check.data <- WQXTargetUnits(check.data, convert = TRUE)
    
    # Create flag column, flag rows where ResultMeasureValue > Maximum
    flag.data <- check.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(AboveWQXUpperThreshold = dplyr::case_when(
        ResultMeasureValue >= Maximum ~ as.character("Y"),
        ResultMeasureValue < Maximum ~ as.character("N")))
    
    # remove extraneous columns, fix field names
      flag.data <- flag.data %>%
        dplyr::select(-"Maximum")
      
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
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == TRUE) {
    
    # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
    unit.ref <- TADA::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" & 
                      Status == "Valid")
    
    # join unit.ref to raw.data
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source",
                                               "Value", "Minimum")],
                        by.x = c("CharacteristicName", "ActivityMediaName", 
                                 "ResultMeasure.MeasureUnitCode"),
                        by.y = c("Characteristic", "Source", "Value"), all.x = TRUE)
    # Run WQXTargetUnits function to convert ResultMeasureValue class to numeric
    check.data <- WQXTargetUnits(check.data, convert = TRUE)
    
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
      dplyr::select(-"Minimum")
    
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
#' This check reviews data submitted under the column "QAPPApprovedIndicator".
#' Some organizations submit data for this field to indicate if the data 
#' produced has an approved Quality Assurance Project Plan (QAPP) or not. 
#' Y indicates yes, N indicates no.  This function 
#' has two default inputs: clean = TRUE, cleanNA = FALSE. The default will
#' remove rows of data where the QAPPApprovedIndicator equals "N". Users could
#' alternatively remove both N's and NA's using the inputs: 
#' clean = TRUE, cleanNA = TRUE. If both clean = FALSE and cleanNA = FALSE, 
#' the function will not make any changes to the dataset.
#' 
#' Note: This is not a required field, so it is often left blank (NA) even if 
#' the data has an associated QAPP. All states and tribes that collect 
#' monitoring data using 106 funding (almost all state and tribal data in WQX) 
#' are required to have an EPA approved QAPP to receive 106 funding. Therefore,
#' most of these organizations data has an approved QAPP even if the data 
#' submitted to WQP is NA.
#'
#' @param .data TADA dataset
#' @param clean Two boolean arguments: 1) remove data where the 
#' QAPPApprovedIndicator = "N"  when clean = TRUE; 2) remove data where the 
#' QAPPApprovedIndicator = NA  when cleanNA = TRUE. Default is clean = TRUE and 
#' cleanNA = FALSE.
#' @param cleanNA 
#'
#' @return When clean = FALSE and cleanNA = FALSE, no data is removed from the 
#' dataset.
#' 
#' @export
#' 

QAPPapproved <- function(.data, clean = TRUE, cleanNA = FALSE) {
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("QAPPApprovedIndicator") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("QAPPApprovedIndicator") %in% colnames(.data)) == TRUE) {
      
        if (clean == TRUE) {
          .data = dplyr::filter(.data, is.na(QAPPApprovedIndicator)==TRUE | QAPPApprovedIndicator=="Y")
          
          if(nrow(.data)==0){
            warning("All QAPPApprovedIndicator data is N")
          }
        }  
        if (cleanNA == TRUE){ 
          .data = dplyr::filter(.data, is.na(QAPPApprovedIndicator)==FALSE)
        
          if(nrow(.data)==0 & clean==TRUE){
            warning("All QAPPApprovedIndicator data is NA or N")
          } else if (nrow(.data)==0 & clean==FALSE){
            warning("All QAPPApprovedIndicator data is NA")
          }
        } 
     return(.data)
  }
}




#' Flag or remove results without an approved QAPP document URL provided
#' 
#' This check reviews data submitted under the  "ProjectFileUrl" column
#' to determine if a QAPP document is available to review. When clean=FALSE, 
#' a column will be appended to flag results that have an associated 
#' QAPP document url provided. When clean = TRUE, rows that do not
#' have an associated QAPP document are removed from the dataset and no column
#' will be appended. This function should only be used to remove data if an 
#' accompanying QAPP document is required to use data in assessments.
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes data without an associated QAPP
#' document from the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column is appended to the input data set that
#' flags rows with an associated QAPP document. When clean = TRUE, 
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
    
    # flag data where QAPP document url is provided
    # make QAPPdoc.data data frame
    QAPPdoc.data <- dplyr::filter(.data, grepl('/', ProjectFileUrl))
    
    # if there is data without an associated QAPP url in the data set
    if(nrow(QAPPdoc.data) != 0){
      
      # append flag column
      QAPPdoc.data$QAPPDocAvailable <- "Y_ProjectFileUrlProvided"
      
      # join QAPPdoc.data to flag.data
      flag.data <- merge(.data, QAPPdoc.data, all.x = TRUE)
      
      # flagged output
      if(clean == FALSE) {
        return(flag.data)
      }
      
      # clean output
      if(clean == TRUE) {
        # remove data without an associated QAPP url
        clean.data <- dplyr::filter(flag.data, grepl('/', ProjectFileUrl))
        
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


#' decimalplaces
#' 
#' for numeric data type
#'
#' @param .data TADA dataset
#' 
#' @return 
#' 
#'  

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' decimalnumcount
#' 
#' for character data type
#'
#' @param .data TADA dataset
#' 
#' @return 
#' 
#'  

decimalnumcount<-function(x){stopifnot(class(x)=="character")
  x<-gsub("(.*)(\\.)|(*$)","",x)
  nchar(x)}

#' Invalid coordinates
#' 
#' When clean = FALSE, a column will be appended titled "InvalidCoordinates"
#' with the following flags (if relevant to dataset): 
#' 
#' BELOW TBD 
#' 
#' 1) If the LONG has a + sign, the function assumes this was a data
#' submission error because it is outside of the US. + LONGS will be 
#' changed to - 
#' 2) If the LAT or LONG includes the specific strings,
#' 000 or 999, or if the LAT is outside of the -90 to 90 range and LONG
#' is outside of the -180 to 180 range, the row is flagged as "Invalid";
#' 3) Precision can be measured by the number of decimal places in the LAT 
#' and LONG provided. If the LAT or LONG does not have any numbers to the right of the 
#' decimal point, the row will be flagged as "Imprecise". 
#'
#' @param .data TADA dataset
#' @param clean_outsideUSA Two boolean arguments. Default is clean = FALSE and 
#' imprecise = FALSE. When clean = TRUE and imprecise = TRUE, data transformations
#' occur and data is removed from the dataset.
#' @param clean_imprecise Two boolean arguments. Default is clean = FALSE and 
#' imprecise = FALSE. When clean = TRUE and imprecise = TRUE, data transformations
#' occur and data is removed from the dataset.
#'
#' @return When clean = FALSE and imprecise = FALSE, data is flagged but
#' not removed from the dataset.
#' 
#' @export
#' 

InvalidCoordinates <- function(.data, clean_outsideUSA = FALSE, clean_imprecise = FALSE) {
  # check that .data object is compatible with InvalidCoordinates function
  # check .data is of class data.frame
  
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("LatitudeMeasure", "LongitudeMeasure") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("LatitudeMeasure", "LongitudeMeasure") %in% colnames(.data)) == TRUE) {
    
    .data = .data %>%
      dplyr::mutate(InvalidCoordinates = dplyr::case_when(
        LatitudeMeasure < 0 ~ "LAT_OutsideUSA",
        LongitudeMeasure > 0 & LongitudeMeasure < 145 ~ "LONG_OutsideUSA",
        grepl('999', LatitudeMeasure) ~ "Imprecise",
        grepl('999', LongitudeMeasure) ~ "Imprecise",
        sapply(.data$LatitudeMeasure,decimalnumcount)<4 | sapply(.data$LongitudeMeasure,decimalnumcount)<4 ~ "Imprecise"
      ))
      
    #clean output, remove all data for stations outside of the US
    if((clean_outsideUSA==TRUE) & (clean_imprecise==TRUE))
      {.data = dplyr::filter(.data, is.na(InvalidCoordinates)== TRUE)}
    
    if((clean_outsideUSA==FALSE) & (clean_imprecise==TRUE)) 
      {.data = dplyr::filter(.data, InvalidCoordinates != "Imprecise" | is.na(InvalidCoordinates)== TRUE)}
    
    if((clean_outsideUSA==TRUE) & (clean_imprecise==FALSE))
      {.data = dplyr::filter(.data, InvalidCoordinates == "Imprecise" | is.na(InvalidCoordinates)== TRUE)}

    if (all(is.na(.data$InvalidCoordinates)==TRUE)) {
      print("All invalid coordinates were removed or your dataset does not contain monitoring stations with invalid coordinates")
      return (dplyr::select(.data, -InvalidCoordinates))
      } else {
        return (.data)}

    }
  else {
        stop("clean argument must be Boolean (TRUE or FALSE)")
  }
}
