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
#' Function that checks a WQP or TADA dataset for depth profile data. Where depth 
#' profile columns are populated, the function appends 'Conversion Factor' columns
#' and populates those columns based on the original unit (MeasureUnitCode 
#' columns) and the target unit, which is defined in the 'unit' argument. A 
#' 'Depth Target Unit' column is also appended, indicating the unit all selected 
#' depth data is converted to. When convert = FALSE, the output includes all
#' 'Conversion Factor' columns and the 'Depth Target Unit' column. When convert
#' = TRUE, the output includes converted depth data and the 'Depth Target
#' Unit' column, which acts as a flag indicating which rows have been converted.
#'
#' @param .data TADA dataset
#' @param unit Character string input indicating the uniform unit depth data is
#' converted to. Allowable values for 'unit' are either 'm' (meter), 'ft' (feet),
#'  or 'in' (inch). 'unit' accepts only one allowable value as an input. Default
#'  is unit = "m".
#' @param fields Character string input indicating the depth fields that will be
#' checked for data.  Allowable values for 'fields' are
#' 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
#' 'ActivityBottomDepthHeightMeasure,' and 'ResultDepthHeightMeasure.'. Default
#'  is to include all allowable values.
#' @param convert Boolean input indicating if conversions should be executed. 
#' When convert = FALSE, the output includes all Conversion Factor' columns and 
#' the 'Depth Target Unit' column. When convert = TRUE, the output includes 
#' converted depth data and the 'Depth Target Unit' column, which acts as a flag
#' indicating which rows have been converted.
#'
#' @return Full dataset with 'Conversion Factor' columns and a 'Depth Target Unit'
#' column. When convert = TRUE, the output is the full dataset with converted
#' uniform depth units and a 'Depth Target Unit' column, which acts as a flag indicating which
#' rows have been converted. 
#' @export
#' 


DepthProfileData <- function(.data, 
                             unit = "m",
                             fields = c("ActivityDepthHeightMeasure",
                                               "ActivityTopDepthHeightMeasure",
                                               "ActivityBottomDepthHeightMeasure",
                                               "ResultDepthHeightMeasure"), 
                             convert = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode",
           "ActivityTopDepthHeightMeasure.MeasureValue", "ActivityTopDepthHeightMeasure.MeasureUnitCode",
           "ActivityBottomDepthHeightMeasure.MeasureValue", "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
           "ResultDepthHeightMeasure.MeasureValue", "ResultDepthHeightMeasure.MeasureUnitCode") 
         %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use
         either the full physical/chemical profile downloaded from WQP or
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # check unit argument for valid number of inputs
  if(length(unit) != 1) {
    stop("Invalid 'unit' argument. 'unit' accepts only one allowable value as an
    input.'unit' must be one of either 'm' (meter), 'ft' (feet), or 'in' (inch).")
  }
  # check unit argument for valid inputs
  if(all(is.na(match(c("m", "ft", "in"), unit))) == TRUE) {
    stop("Invalid 'unit' argument. 'unit' must be either 'm' (meter), 'ft' (feet),
         or 'in' (inch).")
  } 
  # check fields argument for valid inputs
  if(all(is.na(match(c("ActivityDepthHeightMeasure",
        "ActivityTopDepthHeightMeasure",
        "ActivityBottomDepthHeightMeasure",
        "ResultDepthHeightMeasure"), fields))) == TRUE) {
    stop("Invalid 'fields' argument. 'fields' must include one or many of the
    following: 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
    'ActivityBottomDepthHeightMeasure,' and/or 'ResultDepthHeightMeasure.'")
  }
  
  # execute function after checks are passed
  if(all(c("ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode",
           "ActivityTopDepthHeightMeasure.MeasureValue", "ActivityTopDepthHeightMeasure.MeasureUnitCode",
           "ActivityBottomDepthHeightMeasure.MeasureValue", "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
           "ResultDepthHeightMeasure.MeasureValue", "ResultDepthHeightMeasure.MeasureUnitCode") 
         %in% colnames(.data)) == TRUE) {
    
    # read in unit conversion reference table from sysdata.rda
    unit.ref <- TADA:::unit.ref
      #subset to include only "Length Distance" units; filter by target unit defined in 'unit' argument
    unit.ref <- unit.ref %>%
      dplyr::filter(stringr::str_detect(Description, 
                                        stringr::regex("\\bLength Distance"))) %>%
      dplyr::filter(Target.Unit == unit) %>%
      dplyr::rename(Depth.TargetUnit = Target.Unit)
    
    # define check.data (to preserve .data and avoid mistakes with if statements below)
    check.data <- .data
      # add Depth.TargetUnit column
    
    # append data based on fields argument input
      if(("ActivityDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ActivityDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "Depth.TargetUnit", "Conversion.Factor")],
                       by.x = "ActivityDepthHeightMeasure.MeasureUnitCode", 
                       by.y = "Code", 
                       all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(ActDepth.Conversion.Factor = Conversion.Factor)
        }
      }
    
      if(("ActivityTopDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ActivityTopDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "Depth.TargetUnit", "Conversion.Factor")],
                              by.x = "ActivityTopDepthHeightMeasure.MeasureUnitCode", 
                              by.y = "Code", 
                              all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(ActTopDepth.Conversion.Factor = Conversion.Factor)
          # check if Target Unit column already exists, if it does, combine columns
          if(all(c("Depth.TargetUnit.x", "Depth.TargetUnit.y") %in% colnames(check.data)) == TRUE) {
            # coalesce Depth.TargetUnit columns
            check.data$Depth.TargetUnit <- dplyr::coalesce(check.data$Depth.TargetUnit.x, 
                                                      check.data$Depth.TargetUnit.y)
            # remove extra columns
            check.data <- dplyr::select(check.data, -c("Depth.TargetUnit.x",
                                                       "Depth.TargetUnit.y"))
          }
        }
      }
    
      if(("ActivityBottomDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "Depth.TargetUnit", "Conversion.Factor")],
                            by.x = "ActivityBottomDepthHeightMeasure.MeasureUnitCode", 
                            by.y = "Code", 
                            all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(ActBottomDepth.Conversion.Factor = Conversion.Factor)
          # check if Target Unit column already exists, if it does, combine columns
          if(all(c("Depth.TargetUnit.x", "Depth.TargetUnit.y") %in% colnames(check.data)) == TRUE) {
            # coalesce Depth.TargetUnit columns
            check.data$Depth.TargetUnit <- dplyr::coalesce(check.data$Depth.TargetUnit.x, 
                                                      check.data$Depth.TargetUnit.y)
            # remove extra columns
            check.data <- dplyr::select(check.data, -c("Depth.TargetUnit.x",
                                                       "Depth.TargetUnit.y"))
          }
        }
      }
    
      if(("ResultDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ResultDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "Depth.TargetUnit", "Conversion.Factor")],
                              by.x = "ResultDepthHeightMeasure.MeasureUnitCode", 
                              by.y = "Code", 
                              all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(ResultDepth.Conversion.Factor = Conversion.Factor)
          # check if Target Unit column already exists, if it does, combine columns
          if(all(c("Depth.TargetUnit.x", "Depth.TargetUnit.y") %in% colnames(check.data)) == TRUE) {
            # coalesce Depth.TargetUnit columns
            check.data$Depth.TargetUnit <- dplyr::coalesce(check.data$Depth.TargetUnit.x, 
                                                      check.data$Depth.TargetUnit.y)
            # remove extra columns
            check.data <- dplyr::select(check.data, -c("Depth.TargetUnit.x",
                                                       "Depth.TargetUnit.y"))
          }
        }
      }
    if(all(is.na(match(c("ActDepth.Conversion.Factor", 
                         "ActTopDepth.Conversion.Factor",
                         "ActBottomDepth.Conversion.Factor", 
                         "ResultDepth.Conversion.Factor"), colnames(check.data)))) == TRUE) {
      stop("The dataset does not have any depth data.")
    }
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add appended columns to the list
    if(("ActivityDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "ActDepth.Conversion.Factor")
    }
    if(("ActivityTopDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "ActTopDepth.Conversion.Factor")
    }
    if(("ActivityBottomDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "ActBottomDepth.Conversion.Factor")
    }
    if(("ResultDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "ResultDepth.Conversion.Factor")
    }
    if(("Depth.TargetUnit" %in% colnames(check.data)) == TRUE) {
      col.order <- append(col.order, "Depth.TargetUnit")
    }
    # reorder columns in flag.data
    flag.data <- check.data[, col.order]
    # if convert = FALSE, output data
    if(convert == FALSE) {
      
      return(flag.data)
    }
    # if convert = TRUE, apply conversion
    if(convert == TRUE) {
      # define clean.data
      clean.data <- flag.data
      
      # if ActDepth.Conversion.Factor exists...
      if(("ActDepth.Conversion.Factor" %in% colnames(clean.data)) == TRUE) {
        # multiply ActivityDepthHeightMeasure.MeasureValue by ActDepth.Conversion.Factor
        suppressWarnings(
          clean.data$ActivityDepthHeightMeasure.MeasureValue == 
          as.numeric(clean.data$ActivityDepthHeightMeasure.MeasureValue)*
            clean.data$ActDepth.Conversion.Factor)
        # replace ActivityDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ActivityDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ActivityDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ActDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"ActDepth.Conversion.Factor")
      }
      
      # if ActTopDepth.Conversion.Factor exists...
      if(("ActTopDepth.Conversion.Factor" %in% colnames(clean.data)) == TRUE) {
        # multiply ActivityTopDepthHeightMeasure.MeasureValue by ActTopDepth.Conversion.Factor
        suppressWarnings(
        clean.data$ActivityTopDepthHeightMeasure.MeasureValue == 
          as.numeric(clean.data$ActivityTopDepthHeightMeasure.MeasureValue)*
          clean.data$ActTopDepth.Conversion.Factor)
        # replace ActivityTopDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ActDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"ActTopDepth.Conversion.Factor")
      }
      
      # if ActBottomDepth.Conversion.Factor exists...
      if(("ActBottomDepth.Conversion.Factor" %in% colnames(clean.data)) == TRUE) {
        # multiply ActivityBottomDepthHeightMeasure.MeasureValue by ActBottomDepth.Conversion.Factor
        suppressWarnings(
          clean.data$ActivityBottomDepthHeightMeasure.MeasureValue == 
            as.numeric(clean.data$ActivityBottomDepthHeightMeasure.MeasureValue)*
            clean.data$ActBottomDepth.Conversion.Factor)
        # replace ActivityBottomDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ActBottomDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"ActBottomDepth.Conversion.Factor")
      }
      
      # if ResultDepth.Conversion.Factor exists...
      if(("ResultDepth.Conversion.Factor" %in% colnames(clean.data)) == TRUE) {
        # multiply ResultDepthHeightMeasure.MeasureValue by ResultDepth.Conversion.Factor
        suppressWarnings(
          clean.data$ResultDepthHeightMeasure.MeasureValue == 
            as.numeric(clean.data$ResultDepthHeightMeasure.MeasureValue)*
            clean.data$ResultDepth.Conversion.Factor)
        # replace ResultDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ResultDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ResultDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ResultDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"ResultDepth.Conversion.Factor")
      }
      
      return(clean.data)
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
      
      # # define flag.data
      # flag.data <- .data
      # # ID rows with special characters
      # lengths(sapply(strsplit(flag.data$ResultMeasureValue, ",\\s*"), 
      #                setdiff, c(matchvector1, matchvector2))) > 0
      # # append flag column
      
      # placeholder output until we incorporate proper logic
      
      warning("'clean = FALSE' portion of the function is still under 
              development. The input dataset was not changed.")
      return(.data)
    }
    
    # clean output
    if(clean == TRUE) {
      
      # define clean.data
      clean.data <- .data
      #remove an special characters from ResultValue field
      clean.data$ResultMeasureValue <- as.numeric(.data$ResultMeasureValue)
      
      return(clean.data)
      
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}