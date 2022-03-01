#' Check Sample Fraction Validity
#' 
#' Function that checks the validity of each characteristic-fraction combination
#' in the dataset. When clean = TRUE, rows with invalid characteristic-fraction
#' combinations are removed. 
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes "Invalid" characteristic-fraction 
#' combinations from the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column indicating the validity of the 
#' combination of CharacteristicName and ResultSampleFractionText values is 
#' appended to the input data set. When clean = FALSE, the column is appended, 
#' but "Invalid" rows will be removed from the dataset.
#' 
#' @export
#' 


InvalidFraction <- function(.data, clean = FALSE){

  # check that .data object is compatible with TADA
    # check .data is of class data.frame
  if("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
    # check .data has required columns
  if(all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == TRUE) {
    
    if(("SampleFractionValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -SampleFractionValidity)
    }
    # read in sample fraction reference table from sysdata.rda and filter
    frac.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicFraction")
    # duplicate and capitalize CharName and Fraction columns in .data
    .data$Char.Upper <- toupper(.data$CharacteristicName)
    .data$Frac.Upper <- toupper(.data$ResultSampleFractionText)
    # join "Status" column to .data by CharacteristicName and Value (SampleFraction)
    check.data <- merge(.data, frac.ref[, c("Characteristic", "Status", "Value")],
                                        by.x = c("Char.Upper", "Frac.Upper"),
                                        by.y = c("Characteristic", "Value"), all.x = TRUE)
    
    # remove Char.Upper and Frac.Upper columns and rename Status column
    check.data <- check.data %>%
      select(-c("Char.Upper", "Frac.Upper")) %>%
      dplyr::rename(SampleFractionValidity = Status)
    # rename NA values to Unknown in SampleFractionValidity column
    check.data["SampleFractionValidity"][is.na(check.data["SampleFractionValidity"])] <- "Unknown"
    
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' fraction. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }
    if(clean == TRUE) {
     # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, SampleFractionValidity != "Invalid")
      
      return(clean.data)
    }
  }
  
}


#' Check Method Speciation Validity
#' 
#' Function that checks the validity of each characteristic-method 
#' speciation combination in the dataset. When clean = TRUE, rows with invalid 
#' characteristic-method speciation combinations are removed. 
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes "Invalid" characteristic-method 
#' speciation combinations from the dataset when clean = TRUE. Default is 
#' clean = FALSE.
#'
#' @return When clean = FALSE, a column indicating the validity of the 
#' combination of CharacteristicName and MethodSpeciation values is 
#' appended to the input data set. When clean = FALSE, the column is appended, 
#' but "Invalid" rows will be removed from the dataset.
#' 
#' @export
#' 


InvalidSpeciation <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == TRUE) {
    
    if(("MethodSpeciationValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -MethodSpeciationValidity)
    }
    # read in speciation reference table from sysdata.rda and filter
    spec.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicSpeciation")
    # duplicate and capitalize CharName and Speciation columns in .data
    .data$Char.Upper <- toupper(.data$CharacteristicName)
    .data$Spec.Upper <- toupper(.data$MethodSpecificationName)
    # join "Status" column to .data by CharacteristicName and Value (Speciation)
    check.data <- merge(.data, spec.ref[, c("Characteristic", "Status", "Value")],
                        by.x = c("Char.Upper", "Spec.Upper"),
                        by.y = c("Characteristic", "Value"), all.x = TRUE)
    
    # remove Char.Upper and Spec.Upper columns and rename Status column
    check.data <- check.data %>%
      select(-c("Char.Upper", "Spec.Upper")) %>%
      dplyr::rename(MethodSpeciationValidity = Status)
    # rename NA values to Unknown in MethodSpeciationValidity column 
    check.data["MethodSpeciationValidity"][is.na(check.data["MethodSpeciationValidity"])] <- "Unknown"
    
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' speciation. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }
    if(clean == TRUE) {
      # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, MethodSpeciationValidity != "Invalid")
      
      return(clean.data)
    }
  }
  
}


#' Check Result Unit Validity
#' 
#' Function that checks the validity of each characteristic-media-result unit 
#' combination in the dataset. When clean = TRUE, rows with invalid 
#' characteristic-media-result unit combinations are removed. 
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes "Invalid" characteristic-media-result 
#' unit combinations from the dataset when clean = TRUE. Default is 
#' clean = FALSE.
#'
#' @return When clean = FALSE, a column indicating the validity of the 
#' combination of CharacteristicName, ActivityMediaType, and
#' ResultMeasure/MeasureUnitCode values is appended to the input data set. When
#' clean = FALSE, the column is appended, but "Invalid" rows will be removed 
#' from the dataset.
#' 
#' @export
#' 


InvalidResultUnit <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == TRUE) {
    
    if(("ResultUnitValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -ResultUnitValidity)
    }
    # read in speciation reference table from sysdata.rda and filter
    unit.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit")
    # duplicate and capitalize CharName and result unit columns in .data
    .data$Char.Upper <- toupper(.data$CharacteristicName)
    .data$Unit.Upper <- toupper(.data$ResultMeasure.MeasureUnitCode)
    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
                        by.x = c("Char.Upper", "Unit.Upper", "ActivityMediaName"),
                        by.y = c("Characteristic", "Value", "Source"), all.x = TRUE)
    
    # remove Char.Upper and Unit.Upper columns and rename Status column
    check.data <- check.data %>%
      select(-c("Char.Upper", "Unit.Upper")) %>%
      dplyr::rename(ResultUnitValidity = Status)
    # rename NA values to Unknown in ResultUnitValidity column 
    check.data["ResultUnitValidity"][is.na(check.data["ResultUnitValidity"])] <- "Unknown"
    
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' result units. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, ResultUnitValidity != "Invalid")
      
      return(clean.data)
    }
  }
}

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
#' input data set. When clean = FALSE, the column is appended, but "Invalid" 
#' rows will be removed from the dataset.
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
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(all(c("CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier", 
           "ResultAnalyticalMethod.MethodIdentifierContext") %in% colnames(.data)) == TRUE) {
    
    if(("UncommonAnalyticalMethod" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -UncommonAnalyticalMethod)
    }
    # read in speciation reference table from sysdata.rda and filter
    meth.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicMethod")
    # duplicate and capitalize CharName and result unit columns in .data
    .data$Char.Upper <- toupper(.data$CharacteristicName)
    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
                        by.x = c("Char.Upper", "ResultAnalyticalMethod.MethodIdentifier", 
                                 "ResultAnalyticalMethod.MethodIdentifierContext"),
                        by.y = c("Characteristic", "Value", "Source"), all.x = TRUE)
    
    # remove Char.Upper column and rename Status column
    check.data <- check.data %>%
      select(-c("Char.Upper", "Unit.Upper")) %>%
      dplyr::rename(UncommonAnalyticalMethod = Status)
    # rename values in UncommonAnalyticalMethod
    check.data["UncommonAnalyticalMethod"][check.data["UncommonAnalyticalMethod"] == "Valid"] <- "N"
    check.data["UncommonAnalyticalMethod"][check.data["UncommonAnalyticalMethod"] == "Invalid"] <- "Y"
    # rename NA values to Unknown in UncommonAnalyticalMethod column 
    check.data["UncommonAnalyticalMethod"][is.na(check.data["UncommonAnalyticalMethod"])] <- "Unknown"
    
    if(clean == FALSE) {
      return(check.data)
    }
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, UncommonAnalyticalMethod != "Y")
      
      return(clean.data)
    }
  }
}