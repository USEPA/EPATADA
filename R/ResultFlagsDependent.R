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
    stop("The dataframe does not contain the required fields to use TADA. Use
         either the full physical/chemical profile downloaded from WQP or
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == TRUE) {
    
    if(("SampleFractionValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -SampleFractionValidity)
    }
    # read in sample fraction reference table from sysdata.rda and filter
    frac.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicFraction")
    
    # define raw.data
    raw.data <- .data
      # duplicate and capitalize CharName and Fraction columns in raw.data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    raw.data$Frac.Upper <- toupper(raw.data$ResultSampleFractionText)
    
    # join "Status" column to .data by CharacteristicName and Value (SampleFraction)
    check.data <- merge(raw.data, frac.ref[, c("Characteristic", "Status", "Value")],
                                        by.x = c("Char.Upper", "Frac.Upper"),
                                        by.y = c("Characteristic", "Value"), all.x = TRUE)
    
    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(SampleFractionValidity = Status)
    # rename NA values to Unknown in SampleFractionValidity column
    check.data["SampleFractionValidity"][is.na(check.data["SampleFractionValidity"])] <- "Unknown"
    
    # remove extraneous columns
    check.data <- check.data %>%
      dplyr::select(-c("Char.Upper", "Frac.Upper"))
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add SampleFractionValidity column to the list
    col.order <- append(col.order, "SampleFractionValidity")
      # reorder columns in flag.data
    check.data <- check.data[, col.order]
    
    # flagged output
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing
              to retain 'Invalid' fraction. In order to ensure transformation
              functions will run properly, set clean = TRUE.")
    }
    
    # clean output
    if(clean == TRUE) {
     # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, SampleFractionValidity != "Invalid")
      
      # remove SampleFractionValidity column
      clean.data <- dplyr::select(clean.data, -SampleFractionValidity)
      
      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
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
    stop("The dataframe does not contain the required fields to use TADA. Use
         either the full physical/chemical profile downloaded from WQP or
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == TRUE) {
    
    if(("MethodSpeciationValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -MethodSpeciationValidity)
    }
    # read in speciation reference table from sysdata.rda and filter
    spec.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicSpeciation")
    
    # define raw.data
    raw.data <- .data
      # duplicate and capitalize CharName and Speciation columns in .data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    raw.data$Spec.Upper <- toupper(raw.data$MethodSpecificationName)
    
    # join "Status" column to .data by CharacteristicName and Value (Speciation)
    check.data <- merge(raw.data, spec.ref[, c("Characteristic", "Status", "Value")],
                        by.x = c("Char.Upper", "Spec.Upper"),
                        by.y = c("Characteristic", "Value"), all.x = TRUE)
    
    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(MethodSpeciationValidity = Status)
    # rename NA values to Unknown in MethodSpeciationValidity column 
    check.data["MethodSpeciationValidity"][is.na(check.data["MethodSpeciationValidity"])] <- "Unknown"
    
    # remove extraneous columns
    check.data <- check.data %>%
      dplyr::select(-c("Char.Upper", "Spec.Upper"))
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add MethodSpeciationValidity column to the list
    col.order <- append(col.order, "MethodSpeciationValidity")
      # reorder columns in flag.data
    check.data <- check.data[, col.order]
    
    # flagged output
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to
              retain 'Invalid' speciation. In order to ensure transformation
              functions will run properly, set clean = TRUE.")
    }
    
    # clean output
    if(clean == TRUE) {
      # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, MethodSpeciationValidity != "Invalid")
      
      # remove MethodSpeciationValidity column
      clean.data <- dplyr::select(clean.data, -MethodSpeciationValidity)
      
      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
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
    stop("The dataframe does not contain the required fields to use TADA. Use
         either the full physical/chemical profile downloaded from WQP or
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == TRUE) {
    
    if(("ResultUnitValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -ResultUnitValidity)
    }
    # read in unit reference table from sysdata.rda and filter
    unit.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit")
    
    # define raw.data
    raw.data <- .data
    # duplicate and capitalize CharName and result unit columns in .data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    raw.data$Unit.Upper <- toupper(raw.data$ResultMeasure.MeasureUnitCode)
    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(raw.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
                        by.x = c("Char.Upper", "Unit.Upper", "ActivityMediaName"),
                        by.y = c("Characteristic", "Value", "Source"), all.x = TRUE)
    
    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(ResultUnitValidity = Status)
    # rename NA values to Unknown in ResultUnitValidity column 
    check.data["ResultUnitValidity"][is.na(check.data["ResultUnitValidity"])] <- "Unknown"
    
    # remove extraneous columns
    check.data <- check.data %>%
      dplyr::select(-c("Char.Upper", "Unit.Upper"))
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add ResultUnitValidity column to the list
    col.order <- append(col.order, "ResultUnitValidity")
    # reorder columns in flag.data
    check.data <- check.data[, col.order]
    
    # flagged output
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to
              retain 'Invalid' result units. In order to ensure transformation
              functions will run properly, set clean = TRUE.")
    }
    
    # clean output
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, ResultUnitValidity != "Invalid")
      
      # remove ResultUnitValidity column
      clean.data <- dplyr::select(clean.data, -ResultUnitValidity)
      
      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Depth Profile Flag & Unit Conversion
#' 
#' **placeholder text for function description
#'
#' @param .data TADA dataset
#' @param convert Boolean input indicating if conversions should be executed. 
#' The default is FALSE, where the output is .data with a TargetUnit and a 
#' ConversionFactor column appended. If convert = TRUE, the output does not 
#' append additional columns and applies the conversion to the 
#' ActivityDepthHeightMeasure.MeasureUnitCode column.
#'
#' @return Full dataset with column indicating presence of depth data. When 
#' convert = TRUE, the output is the full dataset with uniform depth units. 
#' @export
#' 


DepthProfileData <- function(.data, convert = FALSE){
  
  # check that .data object is compatible with TADA
  if(TADAprofileCheck(.data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use 
         either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(TADAprofileCheck(.data) == TRUE) {
    # read in unit conversion reference table from sysdata.rda
    unit.ref <- TADA:::unit.ref
    # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
    TADA.data <- merge(.data, unit.ref[, c("Original.Unit", "Target.Unit", "Conversion.Factor")],
                       by.x = "ActivityDepthHeightMeasure.MeasureUnitCode", 
                       by.y = "Original.Unit", 
                       all.x = TRUE)
    # if execute = FALSE, output data
    if(convert == FALSE) {
      return(TADA.data)
    }
    # if execute = TRUE, apply conversion
    if(convert == TRUE) {
      # divide ActivityDepthHeightMeasure.MeasureValue by ConversionFactor
      TADA.data$ActivityDepthHeightMeasure.MeasureValue = 
        TADA.data$ActivityDepthHeightMeasure.MeasureValue/TADA.data$Conversion.Factor
      # replace ActivityDepthHeightMeasure.MeasureUnitCode values with TargetUnit values
      TADA.data$ActivityDepthHeightMeasure.MeasureUnitCode =
        TADA.data$Target.Unit
      # remove appended columns (target unit and conversion factor)
      TADA.data <- dplyr::select(TADA.data, -c("Target.Unit", "Conversion.Factor"))
      
      return(TADA.data)
    } else {
      stop("'convert' argument must be Boolean (TRUE or FALSE)")
    }
  }
}




#' MediaNotWater
#' 
#' **Placeholder text for function description
#'
#' @param data TADA dataset
#' @param clean Clean argument indicates whether flag columns should be appended 
#' to the data (clean = FALSE), or flagged data is transformed/filtered from the 
#' dataset and no columns are appended (clean = TRUE).
#'
#' @return Full TADA dataset with flags or data removed
#' @export

MediaNotWater <- function(data, clean = TRUE){
  
  field.names <- colnames(data)
  
  if(TADAprofileCheck(data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(TADAprofileCheck(data) == TRUE) {
    
    if(clean == TRUE) {
      # Remove all data where media name does NOT equal WATER (ignore punctuation)
      cleandata <- dplyr::filter(data, ActivityMediaName == "Water")
      
      return(cleandata)
    }
    
    if(clean == FALSE) {
      # NEED TO EDIT TO ADD FLAGS, currently removes other water Water
      flagdata <- dplyr::filter(data, ActivityMediaName == "water")   
      
      return(flagdata)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Check for Special Characters in the Result Measure Value Field
#' 
#' **placeholder text for function description
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes special characters from the 
#' ResultMeasureValue field when clean = TRUE. Appends "ResultValueSpecialChar"
#' column which flags rows where ResultMeasureValue has special characters in
#' the value when clean = FALSE. Default is clean = FALSE.
#'
#' @return Full dataset with column indicating presence of special characters in
#' the ResultMeasureValue field. When clean = TRUE, the output is the full 
#' dataset with special characters removed from the ResultMeasureValueField. 
#' @export
#' 


ResultValueSpecialCharacters <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(("ResultMeasureValue" %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use 
         either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  # execute function after checks are passed
  if(("ResultMeasureValue" %in% colnames(.data)) == TRUE) {
    
    if(("ResultValueSpecialChar" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -ResultValueSpecialChar)
    }
    
    # flagged output
    if(clean == FALSE) {
      
      # define flag.data
      flag.data <- .data
      # ID rows with special characters
      lengths(sapply(strsplit(flag.data$ResultMeasureValue, ",\\s*"), 
                     setdiff, c(matchvector1, matchvector2))) > 0
      # append flag column
      
      return(flag.data)
      warning("Data summaries and calculations may be affected by choosing to
              retain special characters in the ResultValue field. In order to 
              ensure transformation functions will run properly, set clean = 
              TRUE.")
    }
    
    # clean output
    if(clean == TRUE) {
      
      #remove an special characters from ResultValue field
      clean.data <- .data %>%
        is.numeric(ResultMeasureValue)
      
      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}