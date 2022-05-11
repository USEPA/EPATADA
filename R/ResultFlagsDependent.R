#' Check Sample Fraction Validity
#' 
#' Function checks the validity of each characteristic-fraction combination
#' in the dataset. When clean = TRUE, rows with invalid characteristic-fraction
#' combinations are removed. Default is clean = FALSE.
#'
#' @param .data TADA dataframe
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
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == TRUE) {
    
    if(("WQX.SampleFractionValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.SampleFractionValidity)
    }
    # read in sample fraction reference table from sysdata.rda and filter
    frac.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicFraction")
    
    # join "Status" column to .data by CharacteristicName and Value (SampleFraction)
    check.data <- merge(.data, frac.ref[, c("Characteristic", "Status", "Value")],
                                        by.x = c("CharacteristicName", 
                                                 "ResultSampleFractionText"),
                                        by.y = c("Characteristic", "Value"), all.x = TRUE)
    
    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.SampleFractionValidity = Status)
    # rename NA values to Unknown in WQX.SampleFractionValidity column
    check.data["WQX.SampleFractionValidity"][is.na(check.data["WQX.SampleFractionValidity"])] <- "Unknown"
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add WQX.SampleFractionValidity column to the list
    col.order <- append(col.order, "WQX.SampleFractionValidity")
      # reorder columns in flag.data
    check.data <- check.data[, col.order]
      # place flag column next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.SampleFractionValidity", 
                      .after = "ResultSampleFractionText")
    
    # flagged output
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' fraction. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }
    
    # clean output
    if(clean == TRUE) {
     # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, WQX.SampleFractionValidity != "Invalid")
      
      # remove WQX.SampleFractionValidity column
      clean.data <- dplyr::select(clean.data, -WQX.SampleFractionValidity)
      
      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
  
}


#' Check Method Speciation Validity
#' 
#' Function checks the validity of each characteristic-method 
#' speciation combination in the dataset. When clean = TRUE, rows with invalid 
#' characteristic-method speciation combinations are removed. Default is 
#' clean = FALSE.
#'
#' @param .data TADA dataframe
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
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == TRUE) {
    
    if(("WQX.MethodSpeciationValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.MethodSpeciationValidity)
    }
    # read in speciation reference table from sysdata.rda and filter
    spec.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicSpeciation")
    
    # join "Status" column to .data by CharacteristicName and Value (Speciation)
    check.data <- merge(.data, spec.ref[, c("Characteristic", "Status", "Value")],
                        by.x = c("CharacteristicName", "MethodSpecificationName"),
                        by.y = c("Characteristic", "Value"), all.x = TRUE)
    
    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.MethodSpeciationValidity = Status)
    # rename NA values to Unknown in WQX.MethodSpeciationValidity column 
    check.data["WQX.MethodSpeciationValidity"][is.na(check.data["WQX.MethodSpeciationValidity"])] <- "Unknown"
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add WQX.MethodSpeciationValidity column to the list
    col.order <- append(col.order, "WQX.MethodSpeciationValidity")
      # reorder columns in flag.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.MethodSpeciationValidity", 
                      .after = "MethodSpecificationName")
    
    # flagged output
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' speciation. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }
    
    # clean output
    if(clean == TRUE) {
      # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, WQX.MethodSpeciationValidity != "Invalid")
      
      # remove WQX.MethodSpeciationValidity column
      clean.data <- dplyr::select(clean.data, -WQX.MethodSpeciationValidity)
      
      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
  
}


#' Check Result Unit Validity
#' 
#' Function checks the validity of each characteristic-media-result unit 
#' combination in the dataset. When clean = TRUE, rows with invalid 
#' characteristic-media-result unit combinations are removed. Default is 
#' clean = FALSE.
#'
#' @param .data TADA dataframe
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
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == TRUE) {
    
    if(("WQX.ResultUnitValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.ResultUnitValidity)
    }
    # read in unit reference table from sysdata.rda and filter
    unit.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit")
    
    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
                        by.x = c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName"),
                        by.y = c("Characteristic", "Value", "Source"), all.x = TRUE)
    
    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.ResultUnitValidity = Status)
    # rename NA values to Unknown in WQX.ResultUnitValidity column 
    check.data["WQX.ResultUnitValidity"][is.na(check.data["WQX.ResultUnitValidity"])] <- "Unknown"
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add WQX.ResultUnitValidity column to the list
    col.order <- append(col.order, "WQX.ResultUnitValidity")
    # reorder columns in flag.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.ResultUnitValidity", 
                      .after = "ResultMeasure.MeasureUnitCode")
    
    # flagged output
    if(clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' result units. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }
    
    # clean output
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, WQX.ResultUnitValidity != "Invalid")
      
      # remove WQX.ResultUnitValidity column
      clean.data <- dplyr::select(clean.data, -WQX.ResultUnitValidity)
      
      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Depth Profile Flag & Unit Conversion
#' 
#' Function checks dataset for depth profile data. Where depth profile columns
#' are populated, the function appends 'Conversion Factor' columns
#' and populates those columns based on the original unit (MeasureUnitCode 
#' columns) and the target unit, which is defined in the 'unit' argument. A 
#' 'Depth Target Unit' column is also appended, indicating the unit all selected 
#' depth data is converted to. When convert = FALSE, the output includes all
#' 'Conversion Factor' columns and the 'Depth Target Unit' column. When convert
#' = TRUE, the output includes converted depth data and the 'Depth Target
#' Unit' column, which acts as a flag indicating which rows have been converted.
#'
#' @param .data TADA dataframe
#' @param unit Character string input indicating the uniform unit depth data is
#' converted to. Allowable values for 'unit' are either 'm' (meter), 'ft' (feet),
#'  or 'in' (inch). 'unit' accepts only one allowable value as an input. Default
#'  is unit = "m".
#' @param fields Character string input indicating the depth fields that will be
#' checked for data.  Allowable values for 'fields' are
#' 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
#' 'ActivityBottomDepthHeightMeasure,' and 'ResultDepthHeightMeasure.'. Default
#'  is to include all allowable values.
#' @param convert Boolean argument; When convert = FALSE, the output includes 
#' all Conversion Factor' columns and the 'Depth Target Unit' column. When 
#' convert = TRUE, the output includes converted depth data and the 'Depth 
#' Target Unit' column, which acts as a flag indicating which rows have been 
#' converted. Default is convert = FALSE.
#'
#' @return Full dataset with 'Conversion Factor' columns and a 'Depth Target Unit'
#' column. When convert = TRUE, the output is the full dataset with converted
#' uniform depth units and a 'Depth Target Unit' column, which acts as a flag indicating which
#' rows have been converted. 
#' 
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
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # check unit argument for valid number of inputs
  if(length(unit) != 1) {
    stop("Invalid 'unit' argument. 'unit' accepts only one allowable value as an input.'unit' must be one of either 'm' (meter), 'ft' (feet), or 'in' (inch).")
  }
  # check unit argument for valid inputs
  if(all(is.na(match(c("m", "ft", "in"), unit))) == TRUE) {
    stop("Invalid 'unit' argument. 'unit' must be either 'm' (meter), 'ft' (feet), or 'in' (inch).")
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
      dplyr::rename(WQX.Depth.TargetUnit = Target.Unit)
    
    # define check.data (to preserve .data and avoid mistakes with if statements below)
    check.data <- .data
      # add WQX.Depth.TargetUnit column
    
    # append data based on fields argument input
      if(("ActivityDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ActivityDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "WQX.Depth.TargetUnit", "Conversion.Factor")],
                       by.x = "ActivityDepthHeightMeasure.MeasureUnitCode", 
                       by.y = "Code", 
                       all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(WQX.ActDepth.ConversionFactor = Conversion.Factor)
        }
      }
    
      if(("ActivityTopDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ActivityTopDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "WQX.Depth.TargetUnit", "Conversion.Factor")],
                              by.x = "ActivityTopDepthHeightMeasure.MeasureUnitCode", 
                              by.y = "Code", 
                              all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(WQX.ActTopDepth.ConversionFactor = Conversion.Factor)
          # check if Target Unit column already exists, if it does, combine columns
          if(all(c("WQX.Depth.TargetUnit.x", "WQX.Depth.TargetUnit.y") %in% colnames(check.data)) == TRUE) {
            # coalesce WQX.Depth.TargetUnit columns
            check.data$WQX.Depth.TargetUnit <- dplyr::coalesce(check.data$WQX.Depth.TargetUnit.x, 
                                                      check.data$WQX.Depth.TargetUnit.y)
            # remove extra columns
            check.data <- dplyr::select(check.data, -c("WQX.Depth.TargetUnit.x",
                                                       "WQX.Depth.TargetUnit.y"))
          }
        }
      }
    
      if(("ActivityBottomDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "WQX.Depth.TargetUnit", "Conversion.Factor")],
                            by.x = "ActivityBottomDepthHeightMeasure.MeasureUnitCode", 
                            by.y = "Code", 
                            all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(WQX.ActBottomDepth.ConversionFactor = Conversion.Factor)
          # check if Target Unit column already exists, if it does, combine columns
          if(all(c("WQX.Depth.TargetUnit.x", "WQX.Depth.TargetUnit.y") %in% colnames(check.data)) == TRUE) {
            # coalesce WQX.Depth.TargetUnit columns
            check.data$WQX.Depth.TargetUnit <- dplyr::coalesce(check.data$WQX.Depth.TargetUnit.x, 
                                                      check.data$WQX.Depth.TargetUnit.y)
            # remove extra columns
            check.data <- dplyr::select(check.data, -c("WQX.Depth.TargetUnit.x",
                                                       "WQX.Depth.TargetUnit.y"))
          }
        }
      }
    
      if(("ResultDepthHeightMeasure" %in% fields) == TRUE) {
        # proceed only if ActivityDepthHeightMeasure.MeasureUnitCode has values other than NA
        if(sum(is.na(check.data$ResultDepthHeightMeasure.MeasureUnitCode)) > 0) {
          # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
          check.data <- merge(check.data, unit.ref[, c("Code", "WQX.Depth.TargetUnit", "Conversion.Factor")],
                              by.x = "ResultDepthHeightMeasure.MeasureUnitCode", 
                              by.y = "Code", 
                              all.x = TRUE)
          # rename new columns
          check.data <- check.data %>%
            dplyr::rename(WQX.ResultDepth.ConversionFactor = Conversion.Factor)
          # check if Target Unit column already exists, if it does, combine columns
          if(all(c("WQX.Depth.TargetUnit.x", "WQX.Depth.TargetUnit.y") %in% colnames(check.data)) == TRUE) {
            # coalesce WQX.Depth.TargetUnit columns
            check.data$WQX.Depth.TargetUnit <- dplyr::coalesce(check.data$WQX.Depth.TargetUnit.x, 
                                                      check.data$WQX.Depth.TargetUnit.y)
            # remove extra columns
            check.data <- dplyr::select(check.data, -c("WQX.Depth.TargetUnit.x",
                                                       "WQX.Depth.TargetUnit.y"))
          }
        }
      }
    # check if any Conversion Factor columns were appended
    if(all(is.na(match(c("WQX.ActDepth.ConversionFactor", 
                         "WQX.ActTopDepth.ConversionFactor",
                         "WQX.ActBottomDepth.ConversionFactor", 
                         "WQX.ResultDepth.ConversionFactor"), colnames(check.data)))) == TRUE) {
      stop("The dataset does not have any depth data.")
    }
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add appended columns to the list
    if(("ActivityDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "WQX.ActDepth.ConversionFactor")
    }
    if(("ActivityTopDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "WQX.ActTopDepth.ConversionFactor")
    }
    if(("ActivityBottomDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "WQX.ActBottomDepth.ConversionFactor")
    }
    if(("ResultDepthHeightMeasure" %in% fields) == TRUE) {
      col.order <- append(col.order, "WQX.ResultDepth.ConversionFactor")
    }
    if(("WQX.Depth.TargetUnit" %in% colnames(check.data)) == TRUE) {
      col.order <- append(col.order, "WQX.Depth.TargetUnit")
    }
    # reorder columns in flag.data
    flag.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    if(("ActivityDepthHeightMeasure" %in% fields) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate("WQX.ActDepth.ConversionFactor", 
                        .after = "ActivityDepthHeightMeasure.MeasureValue")
    }
    if(("ActivityTopDepthHeightMeasure" %in% fields) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate("WQX.ActTopDepth.ConversionFactor", 
                        .after = "ActivityTopDepthHeightMeasure.MeasureValue")
    }
    if(("ActivityBottomDepthHeightMeasure" %in% fields) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate("WQX.ActBottomDepth.ConversionFactor", 
                        .after = "ActivityBottomDepthHeightMeasure.MeasureValue")
    }
    if(("ResultDepthHeightMeasure" %in% fields) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate("WQX.ResultDepth.ConversionFactor", 
                        .after = "ResultDepthHeightMeasure.MeasureValue")
    }
    if(("WQX.Depth.TargetUnit" %in% colnames(check.data)) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate("WQX.Depth.TargetUnit", 
                        .after = "ActivityEndTime.TimeZoneCode")
    }
    # if convert = FALSE, output data
    if(convert == FALSE) {
      
      return(flag.data)
    }
    # if convert = TRUE, apply conversion
    if(convert == TRUE) {
      # define clean.data
      clean.data <- flag.data
      
      # if WQX.ActDepth.ConversionFactor exists...
      if(("WQX.ActDepth.ConversionFactor" %in% colnames(clean.data)) == TRUE) {
        # multiply ActivityDepthHeightMeasure.MeasureValue by WQX.ActDepth.ConversionFactor
        suppressWarnings(
          clean.data$ActivityDepthHeightMeasure.MeasureValue == 
          as.numeric(clean.data$ActivityDepthHeightMeasure.MeasureValue)*
            clean.data$WQX.ActDepth.ConversionFactor)
        # replace ActivityDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ActivityDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ActivityDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ActDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"WQX.ActDepth.ConversionFactor")
      }
      
      # if WQX.ActTopDepth.ConversionFactor exists...
      if(("WQX.ActTopDepth.ConversionFactor" %in% colnames(clean.data)) == TRUE) {
        # multiply ActivityTopDepthHeightMeasure.MeasureValue by WQX.ActTopDepth.ConversionFactor
        suppressWarnings(
        clean.data$ActivityTopDepthHeightMeasure.MeasureValue == 
          as.numeric(clean.data$ActivityTopDepthHeightMeasure.MeasureValue)*
          clean.data$WQX.ActTopDepth.ConversionFactor)
        # replace ActivityTopDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ActDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"WQX.ActTopDepth.ConversionFactor")
      }
      
      # if WQX.ActBottomDepth.ConversionFactor exists...
      if(("WQX.ActBottomDepth.ConversionFactor" %in% colnames(clean.data)) == TRUE) {
        # multiply ActivityBottomDepthHeightMeasure.MeasureValue by WQX.ActBottomDepth.ConversionFactor
        suppressWarnings(
          clean.data$ActivityBottomDepthHeightMeasure.MeasureValue == 
            as.numeric(clean.data$ActivityBottomDepthHeightMeasure.MeasureValue)*
            clean.data$WQX.ActBottomDepth.ConversionFactor)
        # replace ActivityBottomDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ActBottomDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"WQX.ActBottomDepth.ConversionFactor")
      }
      
      # if WQX.ResultDepth.ConversionFactor exists...
      if(("WQX.ResultDepth.ConversionFactor" %in% colnames(clean.data)) == TRUE) {
        # multiply ResultDepthHeightMeasure.MeasureValue by WQX.ResultDepth.ConversionFactor
        suppressWarnings(
          clean.data$ResultDepthHeightMeasure.MeasureValue == 
            as.numeric(clean.data$ResultDepthHeightMeasure.MeasureValue)*
            clean.data$WQX.ResultDepth.ConversionFactor)
        # replace ResultDepthHeightMeasure.MeasureUnitCode values with unit argument
          clean.data$ResultDepthHeightMeasure.MeasureUnitCode[which(
            !is.na(clean.data$ResultDepthHeightMeasure.MeasureUnitCode))] <- unit
        # delete ResultDepth.Conversion.Unit column
        clean.data <- dplyr::select(clean.data, -"WQX.ResultDepth.ConversionFactor")
      }
      
      return(clean.data)
    } else {
      stop("'convert' argument must be Boolean (TRUE or FALSE)")
    }
  }
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
#' @return Full dataset with column indicating presence of special characters in
#' the ResultMeasureValue and DetectionQuantitationLimitMeasure.MeasureValue 
#' fields. Additionally, the ResultMeasureValue and 
#' DetectionQuantitationLimitMeasure.MeasureValue fields are converted to class
#' numeric, and copies of each column are created to preserve original 
#' character values.
#'  
#' @export
#' 


MeasureValueSpecialCharacters <- function(.data){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("ResultMeasureValue", "DetectionQuantitationLimitMeasure.MeasureValue")
          %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  # execute function after checks are passed
  if(all(c("ResultMeasureValue", "DetectionQuantitationLimitMeasure.MeasureValue")
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
        TRUE ~ "Coerced to NA"))
    
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
        TRUE ~ "Coerced to NA"))
      
    # remove special characters before converting to numeric
    flag.data$ResultMeasureValue <- stringr::str_replace_all(flag.data$ResultMeasureValue, 
                                          c("<" = "", ">" = "", "~" = "", "," = ""))
    flag.data$DetectionQuantitationLimitMeasure.MeasureValue <- stringr::str_replace_all(
      flag.data$DetectionQuantitationLimitMeasure.MeasureValue, 
      c("<" = "", ">" = "", "~" = "", "," = ""))
    
    # change measure value columns to numeric
      # rename df
      clean.data <- flag.data
      # ResultMeasureValue
      clean.data$ResultMeasureValue <- suppressWarnings(
        as.numeric(clean.data$ResultMeasureValue))
      # DetectionQuantitationLimitMeasure.MeasureValue
      clean.data$DetectionQuantitationLimitMeasure.MeasureValue <- 
        suppressWarnings(as.numeric(clean.data$DetectionQuantitationLimitMeasure.MeasureValue))
    
    # reorder columns
      # place flag column next to relevant fields
      clean.data <- clean.data %>%
        dplyr::relocate("ResultMeasureValue.Original", 
                        .after = "ResultMeasureValue") %>%
        dplyr::relocate("TADA.ResultMeasureValue.Flag", 
                        .after = "ResultMeasureValue.Original") %>%
        dplyr::relocate("DetectionLimitMeasureValue.Original", 
                        .after = "DetectionQuantitationLimitMeasure.MeasureValue") %>%
        dplyr::relocate("TADA.DetectionLimitMeasureValue.Flag", 
                        .after = "DetectionLimitMeasureValue.Original")
    return(clean.data)
  }
}

