#' Check Sample Fraction Validity
#'
#' Function checks the validity of each characteristic-fraction combination
#' in the dataset. When clean = TRUE, rows with invalid characteristic-fraction
#' combinations are removed. Default is clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-fraction
#' combinations from the dataset when clean = TRUE. Default is clean = TRUE.
#'
#' @return When clean = FALSE, this function adds the following column to your
#' dataframe: WQX.SampleFractionValidity. This column flags each 
#' CharacteristicName and ResultSampleFractionText combination in your dataset 
#' as either "Nonstandardized", "Invalid", or "Valid". When clean = TRUE, 
#' "Invalid" rows are removed from the dataset and no column will be appended.
#'
#' @export
#'


InvalidFraction <- function(.data, clean = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if ("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }

  # execute function after checks are passed
  if (all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == TRUE) {
    if (("WQX.SampleFractionValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.SampleFractionValidity)
    }
    # read in sample fraction reference table from extdata and filter
    frac.ref <- GetWQXCharValRef() %>%
      dplyr::filter(Type == "CharacteristicFraction")

    # join "Status" column to .data by CharacteristicName and Value (SampleFraction)
    check.data <- merge(.data, frac.ref[, c("Characteristic", "Status", "Value")],
      by.x = c(
        "CharacteristicName",
        "ResultSampleFractionText"
      ),
      by.y = c("Characteristic", "Value"), all.x = TRUE
    )

    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.SampleFractionValidity = Status)
    # rename NA values to Nonstandardized in WQX.SampleFractionValidity column
    check.data["WQX.SampleFractionValidity"][is.na(check.data["WQX.SampleFractionValidity"])] <- "Nonstandardized"

    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add WQX.SampleFractionValidity column to the list
    col.order <- append(col.order, "WQX.SampleFractionValidity")
    # reorder columns in check.data
    check.data <- check.data[, col.order]
    # place flag column next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.SampleFractionValidity",
        .after = "ResultSampleFractionText"
      )

    # if all rows are "Valid", return input unchanged
    if (any(c("Nonstandardized", "Invalid") %in%
      unique(check.data$WQX.SampleFractionValidity)) == FALSE) {
      print("All data is valid, therefore the function cannot be applied.")
      return(.data)
    }

    # flagged output
    if (clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' fraction. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }

    # clean output
    if (clean == TRUE) {
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
#' clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-method
#' speciation combinations from the dataset when clean = TRUE. Default is
#' clean = TRUE.
#'
#' @return #'When clean = FALSE, this function adds the following column to your
#' dataframe: WQX.MethodSpeciationValidity. This column flags each 
#' CharacteristicName and MethodSpecificationName combination in your dataset as 
#' either "Nonstandardized", "Invalid", or "Valid". When clean = TRUE, "Invalid" 
#' rows are removed from the dataset and no column will be appended.
#'
#' @export
#'


InvalidSpeciation <- function(.data, clean = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }

  # execute function after checks are passed
  if (all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == TRUE) {
    if (("WQX.MethodSpeciationValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.MethodSpeciationValidity)
    }
    # read in speciation reference table from extdata and filter
    spec.ref <- GetWQXCharValRef() %>%
      dplyr::filter(Type == "CharacteristicSpeciation")

    # join "Status" column to .data by CharacteristicName and Value (Speciation)
    check.data <- merge(.data, spec.ref[, c("Characteristic", "Status", "Value")],
      by.x = c("CharacteristicName", "MethodSpecificationName"),
      by.y = c("Characteristic", "Value"), all.x = TRUE
    )

    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.MethodSpeciationValidity = Status)
    # rename NA values to Nonstandardized in WQX.MethodSpeciationValidity column
    check.data["WQX.MethodSpeciationValidity"][is.na(check.data["WQX.MethodSpeciationValidity"])] <- "Nonstandardized"

    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add WQX.MethodSpeciationValidity column to the list
    col.order <- append(col.order, "WQX.MethodSpeciationValidity")
    # reorder columns in check.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.MethodSpeciationValidity",
        .after = "MethodSpecificationName"
      )

    # if all rows are "Valid", return input unchanged
    if (any(c("Nonstandardized", "Invalid") %in%
      unique(check.data$WQX.MethodSpeciationValidity)) == FALSE) {
      print("All data is valid, therefore the function cannot be applied.")
      return(.data)
    }

    # flagged output
    if (clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' speciation. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }

    # clean output
    if (clean == TRUE) {
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
#' clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-media-result
#' unit combinations from the dataset when clean = TRUE. Default is
#' clean = TRUE.
#'
#' @return When clean = FALSE, the following column will be added to your dataset: 
#' WQX.ResultUnitValidity. This column flags each CharacteristicName, 
#' ActivityMediaName, and ResultMeasure/MeasureUnitCode combination in your 
#' dataset as either "Nonstandardized", "Invalid", or "Valid". When
#' clean = TRUE, "Invalid" rows are removed from the dataset and no column will
#' be appended.
#'
#' @export
#'


InvalidResultUnit <- function(.data, clean = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }

  # execute function after checks are passed
  if (all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == TRUE) {
    if (("WQX.ResultUnitValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.ResultUnitValidity)
    }
    # read in unit reference table from extdata and filter
    unit.ref <- GetWQXCharValRef() %>%
      dplyr::filter(Type == "CharacteristicUnit")

    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
      by.x = c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName"),
      by.y = c("Characteristic", "Value", "Source"), all.x = TRUE
    )

    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.ResultUnitValidity = Status)
    # rename NA values to Nonstandardized in WQX.ResultUnitValidity column
    check.data["WQX.ResultUnitValidity"][is.na(check.data["WQX.ResultUnitValidity"])] <- "Nonstandardized"

    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add WQX.ResultUnitValidity column to the list
    col.order <- append(col.order, "WQX.ResultUnitValidity")
    # reorder columns in check.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.ResultUnitValidity",
        .after = "ResultMeasure.MeasureUnitCode"
      )

    # if all rows are "Valid", return input unchanged
    if (any(c("Nonstandardized", "Invalid") %in%
      unique(check.data$WQX.ResultUnitValidity)) == FALSE) {
      print("All data is valid, therefore the function cannot be applied.")
      return(.data)
    }

    # flagged output
    if (clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' result units. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }

    # clean output
    if (clean == TRUE) {
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
#' are populated, the function appends 'Conversion.Factor' columns
#' and populates those columns based on the original unit (MeasureUnitCode
#' columns) and the target unit, which is defined in the 'unit' argument. A
#' 'WQX.Depth.TargetUnit' column is also appended, indicating the unit all selected
#' depth data is converted to. When transform = FALSE, the output includes all
#' 'Conversion.Factor' columns and the 'WQX.Depth.TargetUnit' column. When transform
#' = TRUE, the output includes converted depth data and the 'WQX.Depth.TargetUnit'
#' column, which acts as a flag indicating which rows have been converted.
#' Default is transform = TRUE.
#'
#' @param .data TADA dataframe
#' @param unit Character string input indicating the uniform unit depth data is
#' converted to. Allowable values for 'unit' are either 'm' (meter), 'ft' (feet),
#'  or 'in' (inch). 'unit' accepts only one allowable value as an input. Default
#'  is unit = "m".
#' @param fields Character string input indicating the depth fields that will be
#' checked for data. Allowable values for 'fields' are
#' 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
#' 'ActivityBottomDepthHeightMeasure,' and 'ResultDepthHeightMeasure.'. Default
#'  is to include all allowable values.
#' @param transform Boolean argument; When transform = FALSE, the output includes
#' all 'Conversion.Factor' columns and the 'WQX.Depth.TargetUnit' column. When
#' transform = TRUE, the output includes converted depth data and the 'Depth
#' Target Unit' column, which acts as a flag indicating which rows have been
#' converted. Default is transform = TRUE.
#'
#' @return Full dataset with converted uniform depth units and a 'WQX.Depth.TargetUnit'
#' column, which acts as a flag indicating which rows have been converted.
#' When transform = FALSE, the output is the full dataset with 'Conversion.Factor'
#' columns and a 'WQX.Depth.TargetUnit' column.
#'
#' @export
#'


DepthProfileData <- function(.data,
                             unit = "m",
                             fields = c(
                               "ActivityDepthHeightMeasure",
                               "ActivityTopDepthHeightMeasure",
                               "ActivityBottomDepthHeightMeasure",
                               "ResultDepthHeightMeasure"
                             ),
                             transform = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c(
    "ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode",
    "ActivityTopDepthHeightMeasure.MeasureValue", "ActivityTopDepthHeightMeasure.MeasureUnitCode",
    "ActivityBottomDepthHeightMeasure.MeasureValue", "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
    "ResultDepthHeightMeasure.MeasureValue", "ResultDepthHeightMeasure.MeasureUnitCode", "ActivityEndTime.TimeZoneCode"
  )
  %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # check unit argument for valid number of inputs
  if (length(unit) != 1) {
    stop("Invalid 'unit' argument. 'unit' accepts only one allowable value as an input. 'unit' must be one of either 'm' (meter), 'ft' (feet), or 'in' (inch).")
  }
  # check unit argument for valid inputs
  if (all(is.na(match(c("m", "ft", "in"), unit))) == TRUE) {
    stop("Invalid 'unit' argument. 'unit' must be either 'm' (meter), 'ft' (feet), or 'in' (inch).")
  }
  # check fields argument for valid inputs
  validFields <- c("ActivityDepthHeightMeasure",
                   "ActivityTopDepthHeightMeasure",
                   "ActivityBottomDepthHeightMeasure",
                   "ResultDepthHeightMeasure")
  if (all(is.na(match(validFields, fields))) == TRUE) {
    stop("Invalid 'fields' argument. 'fields' must include one or many of the
    following: 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
    'ActivityBottomDepthHeightMeasure,' and/or 'ResultDepthHeightMeasure.'")
  }

  # execute function after checks are passed
  if (all(c(
    "ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode",
    "ActivityTopDepthHeightMeasure.MeasureValue", "ActivityTopDepthHeightMeasure.MeasureUnitCode",
    "ActivityBottomDepthHeightMeasure.MeasureValue", "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
    "ResultDepthHeightMeasure.MeasureValue", "ResultDepthHeightMeasure.MeasureUnitCode"
  )
  %in% colnames(.data)) == TRUE) {

    # define check.data (to preserve .data and avoid mistakes with if statements below)
    check.data <- .data
    
    appCols <- c("WQXConversionFactor.ActivityDepthHeightMeasure",
                 "WQXConversionFactor.ActivityTopDepthHeightMeasure",
                 "WQXConversionFactor.ActivityBottomDepthHeightMeasure",
                 "WQXConversionFactor.ResultDepthHeightMeasure")
    
    # read in unit conversion reference table from extdata
    unit.ref <- GetMeasureUnitRef()
    
    # subset to include only "Length Distance" units; filter by target unit defined in 'unit' argument
    unit.ref <- unit.ref %>%
      dplyr::filter(stringr::str_detect(
        Description,
        stringr::regex("\\bLength Distance")
      )) %>%
      dplyr::filter(Target.Unit == unit) #%>%
      #dplyr::rename(WQX.Depth.TargetUnit = Target.Unit)

    for (i in seq(length(validFields)+1)) {
      field <- validFields[i]
      if ((field %in% fields) == TRUE) {
        # Old unit column
        unitCol <- paste(field, ".MeasureUnitCode", sep="")
       
        # proceed only if unitCol has values other than NA
        if (sum(!is.na(check.data[unitCol])) > 0) {
          
          # Join conversion factor from unit.ref to .data by unitCol
          check.data <- merge(check.data, unit.ref[, c("Code", "Conversion.Factor")],
                              by.x = unitCol,
                              by.y = "Code",
                              all.x = TRUE
          )
        
        # rename new columns
          names(check.data)[names(check.data) == "Conversion.Factor"] <- paste('WQXConversionFactor.', field,  sep="")
          
          }
        }
      }
    
    # check if any Conversion Factor columns were appended
    if (all(is.na(match(appCols, colnames(check.data)))) == TRUE) {
      stop("The dataset does not have any depth data.")
    }

    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add appended columns to the list
    for (appCol in appCols) {
      if ((appCol %in% colnames(check.data)) == TRUE) {
        col.order <- append(col.order, appCol)
      }
    }
    # reorder columns in check.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    if ((appCols[1] %in% colnames(check.data)) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate(appCols[1],
          .after = "ActivityDepthHeightMeasure.MeasureValue"
        )
    }
    if ((appCols[2] %in% colnames(check.data)) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate(appCols[2],
          .after = "ActivityTopDepthHeightMeasure.MeasureValue"
        )
    }
    if ((appCols[3] %in% colnames(check.data)) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate(appCols[3],
          .after = "ActivityBottomDepthHeightMeasure.MeasureValue"
        )
    }
    if ((appCols[4] %in% colnames(check.data)) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate(appCols[4],
          .after = "ResultDepthHeightMeasure.MeasureValue"
        )
    }
    if ((appCols[5] %in% colnames(check.data)) == TRUE) {
      check.data <- check.data %>%
        dplyr::relocate(appCols[5],
          .after = "ActivityEndTime.TimeZoneCode"
        )
    }
    
    #function should always run all code above
  
  # if transform = FALSE, output data
  if (transform == FALSE) {
    # add WQX.Depth.TargetUnit column
    check.data[ , 'WQX.Depth.TargetUnit'] <- unit
    return(check.data)
  }
    
    # if transform = TRUE, apply conversion
  if (transform == TRUE) {
    # define clean.data
    # add WQX.Depth.TargetUnit column
    check.data[ , 'WQX.Depth.TargetUnit'] <- unit
    
    clean.data <- check.data

      # if WQXConversionFactor.ActivityDepthHeightMeasure exists...
      if (("WQXConversionFactor.ActivityDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
        # multiply ActivityDepthHeightMeasure.MeasureValue by WQXConversionFactor.ActivityDepthHeightMeasure
        clean.data$ActivityDepthHeightMeasure.MeasureValue <- ((clean.data$ActivityDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ActivityDepthHeightMeasure))

        # replace ActivityDepthHeightMeasure.MeasureUnitCode values with unit argument
        clean.data$ActivityDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ActivityDepthHeightMeasure.MeasureUnitCode
        clean.data <- clean.data %>%
          dplyr::relocate("ActivityDepthHeightMeasure.MeasureUnitCode.Original",
                          .after = "WQXConversionFactor.ActivityDepthHeightMeasure"
          )
        
        clean.data$ActivityDepthHeightMeasure.MeasureUnitCode[which(
          !is.na(clean.data$ActivityDepthHeightMeasure.MeasureUnitCode)
        )] <- unit
        
        clean.data <- clean.data %>%
          dplyr::relocate("ActivityDepthHeightMeasure.MeasureUnitCode",
                          .after = "ActivityDepthHeightMeasure.MeasureValue"
          )
        # uncomment below to delete ActDepth.Conversion.Unit column
        #clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ActivityDepthHeightMeasure")
      }

     #WQXConversionFactor.ActivityTopDepthHeightMeasure exists...
     if (("WQXConversionFactor.ActivityTopDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
       # multiply ActivityTopDepthHeightMeasure.MeasureValue by WQXConversionFactor.ActivityTopDepthHeightMeasure
       clean.data$ActivityTopDepthHeightMeasure.MeasureValue <- ((clean.data$ActivityTopDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ActivityTopDepthHeightMeasure))
       
       # replace ActivityTopDepthHeightMeasure.MeasureUnitCode values with unit argument
       clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode
       clean.data <- clean.data %>%
         dplyr::relocate("ActivityTopDepthHeightMeasure.MeasureUnitCode.Original",
                         .after = "WQXConversionFactor.ActivityTopDepthHeightMeasure"
         )
       
       clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode[which(
         !is.na(clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode)
       )] <- unit
       
       clean.data <- clean.data %>%
         dplyr::relocate("ActivityTopDepthHeightMeasure.MeasureUnitCode",
                         .after = "ActivityTopDepthHeightMeasure.MeasureValue"
         )
       # uncomment below to delete ActTopDepth.Conversion.Unit column
       #clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ActivityTopDepthHeightMeasure")
     }

     #WQXConversionFactor.ActivityBottomDepthHeightMeasure exists...
     if (("WQXConversionFactor.ActivityBottomDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
       # multiply ActivityBottomDepthHeightMeasure.MeasureValue by WQXConversionFactor.ActivityBottomDepthHeightMeasure
       clean.data$ActivityBottomDepthHeightMeasure.MeasureValue <- ((clean.data$ActivityBottomDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ActivityBottomDepthHeightMeasure))
       
       # replace ActivityTopDepthHeightMeasure.MeasureUnitCode values with unit argument
       clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode
       clean.data <- clean.data %>%
         dplyr::relocate("ActivityBottomDepthHeightMeasure.MeasureUnitCode.Original",
                         .after = "WQXConversionFactor.ActivityBottomDepthHeightMeasure"
         )
       
       clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode[which(
         !is.na(clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode)
       )] <- unit
       
       clean.data <- clean.data %>%
         dplyr::relocate("ActivityBottomDepthHeightMeasure.MeasureUnitCode",
                         .after = "ActivityBottomDepthHeightMeasure.MeasureValue"
         )
       # uncomment below to delete ActBottomDepth.Conversion.Unit column
       #clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ActivityBottomDepthHeightMeasure")
     }
    
     #WQXConversionFactor.ResultDepthHeightMeasure exists...
     if (("WQXConversionFactor.ResultDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
       # multiply ResultDepthHeightMeasure.MeasureValue by WQXConversionFactor.ResultDepthHeightMeasure
       clean.data$ResultDepthHeightMeasure.MeasureValue <- ((clean.data$ResultDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ResultDepthHeightMeasure))
       
       # replace ResultDepthHeightMeasure.MeasureUnitCode values with unit argument
       clean.data$ResultDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ResultDepthHeightMeasure.MeasureUnitCode
       clean.data <- clean.data %>%
         dplyr::relocate("ResultDepthHeightMeasure.MeasureUnitCode.Original",
                         .after = "WQXConversionFactor.ResultDepthHeightMeasure"
         )
       
       clean.data$ResultDepthHeightMeasure.MeasureUnitCode[which(
         !is.na(clean.data$ResultDepthHeightMeasure.MeasureUnitCode)
       )] <- unit
       
       clean.data <- clean.data %>%
         dplyr::relocate("ResultDepthHeightMeasure.MeasureUnitCode",
                         .after = "ResultDepthHeightMeasure.MeasureUnitCode"
         )
       # uncomment below to delete WQXConversionFactor.ResultDepthHeightMeasure column
       #clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ResultDepthHeightMeasure")
     }

      # uncomment below to delete WQX.Depth.TargetUnit column
      # clean.data <- dplyr::select(clean.data, -"WQX.Depth.TargetUnit")

      return(clean.data)
    } else {
      stop("'transform' argument must be Boolean (TRUE or FALSE)")
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
