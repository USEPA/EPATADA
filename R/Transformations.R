#' Transform Units to WQX Target Units
#'
#' This function compares measure units in the input data to the Water Quality
#' Exchange (WQX) 3.0 QAQC Characteristic Validation table.
#'
#' This function will ALWAYS add the following two columns to the input dataframe:
#' 1) "WQX.ResultMeasureValue.UnitConversion",
#' 2) "WQX.DetectionLimitMeasureValue.UnitConversion"
#' These two fields indicate if data can be converted."NoResultValue" means data
#' cannot be converted because there is no ResultMeasureValue, and "NoTargetUnit"
#' means data cannot be converted because the original unit is not associated with a target unit in WQX.
#' "Convert" means the data can be transformed, and "Converted" means that this function
#' has been run with the input transform = TRUE, and the values were already converted.
#'
#' It also uses the following six fields from the input dataframe:
#' 1) "CharacteristicName",
#' 2) "ActivityMediaName",
#' 3) "ResultMeasureValue",
#' 4) "ResultMeasure.MeasureUnitCode",
#' 5) "DetectionQuantitationLimitMeasure.MeasureValue",
#' 6) "DetectionQuantitationLimitMeasure.MeasureUnitCode"
#'
#' This function adds the following two fields and transforms values within the
#' following four fields ONLY when transform=TRUE:
#' Adds: "ResultMeasureUnitCode.Original", and "DetectionLimitMeasureUnitCode.Original".
#' Transforms: "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "DetectionQuantitationLimitMeasure.MeasureValue", and "DetectionQuantitationLimitMeasure.MeasureUnitCode".
#'
#' This function adds the following two fields ONLY when transform=FALSE:
#' Adds: "WQX.ConversionFactor" and "WQX.TargetUnit".
#'
#' @param .data TADA dataframe
#'
#' @param transform Boolean argument with two possible values, “TRUE” and “FALSE”.
#' Default is transform = TRUE.
#'
#' @return When transform=TRUE, result values and units are converted to WQX target units.
#' This function changes the values within the "ResultMeasure.MeasureUnitCode"
#' and "DetectionQuantitationLimitMeasure.MeasureUnitCode" to the WQX target units and
#' converts respective values within the "ResultMeasureValue" and "DetectionQuantitationLimitMeasure.MeasureValue"
#' fields. In addition to "WQX.ResultMeasureValue.UnitConversion"
#' and "WQX.DetectionLimitMeasureValue.UnitConversion", transform=TRUE will add
#' the following two fields to the input dataframe, "ResultMeasureUnitCode.Original",
#' and "DetectionLimitMeasureUnitCode.Original", to retain the original result
#' and unit values.
#'
#' When transform = FALSE, result values and units are NOT converted to WQX target units,
#' but columns are appended to indicate what the target units and conversion factors are,
#' and if the data can be converted. In addition to "WQX.ResultMeasureValue.UnitConversion"
#' and "WQX.DetectionLimitMeasureValue.UnitConversion",  transform=FALSE will add the
#' following two fields to the input dataframe: "WQX.ConversionFactor" and "WQX.TargetUnit".
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Convert result and detection limit values and units to WQX target units and 
#' # add two new columns titled "ResultMeasureUnitCode.Original" and 
#' # "DetectionLimitMeasureUnitCode.Original" to retain the original result and unit values:
#' ResultUnitsConverted <- ConvertResultUnits(Nutrients_Utah)
#' 
#' # Do not convert result values and units, but add two new columns titled
#' # "WQX.ConversionFactor" and "WQX.TargetUnit":
#' ResultUnitsNotConverted <- ConvertResultUnits(Nutrients_Utah, transform = FALSE)

ConvertResultUnits <- function(.data, transform = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check transform is boolean
  checkType(transform, "logical")
  # check .data has all of the required columns
  expected_cols <- c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode",
    "DetectionQuantitationLimitMeasure.MeasureValue",
    "DetectionQuantitationLimitMeasure.MeasureUnitCode"
    )
  checkColumns(.data, expected_cols)

  # execute function after checks are passed

  # if class(ResultMeasureValue) != numeric, run special char function
  if (!is.numeric(.data$ResultMeasureValue)) {
    .data <- MeasureValueSpecialCharacters(.data)
  }
  
  # filter WQXcharValRef to include only valid CharacteristicUnit in water media
  unit.ref <- GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" &
                    Status == "Valid")
  # join unit.ref to .data
  check.data <- merge(.data, unit.ref[, c(
    "Characteristic", "Source",
    "Value", "Value.Unit",
    "Conversion.Factor"
  )],
  by.x = c("CharacteristicName", "ActivityMediaName", "ResultMeasure.MeasureUnitCode"),
  by.y = c("Characteristic", "Source", "Value"), all.x = TRUE
  )
  # rename columns
  flag.data <- check.data %>%
    dplyr::rename(WQX.TargetUnit = Value.Unit) %>%
    dplyr::rename(WQX.ConversionFactor = Conversion.Factor)
  
  # if temp data exists, calculate conversion factor
  if (all(is.na(match(
    c("deg F", "deg K"),
    flag.data$ResultMeasure.MeasureUnitCode
  ))) == FALSE) {
    
    # Calculate deg F and deg C, replace Conversion factor values
    flag.data <- flag.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(WQX.ConversionFactor = dplyr::case_when(
        ResultMeasure.MeasureUnitCode == "deg F" ~
          as.numeric(((ResultMeasureValue - 32) * (5 / 9)) / ResultMeasureValue),
        ResultMeasure.MeasureUnitCode == "deg K" ~
          as.numeric((ResultMeasureValue - 273.15) / ResultMeasureValue),
        TRUE ~ WQX.ConversionFactor
      ))
  }
  
  
  # add WQX.ResultMeasureValue.UnitConversion column
  flag.data <- flag.data %>%
    # apply function row by row
    dplyr::rowwise() %>%
    # create flag column
    dplyr::mutate(WQX.ResultMeasureValue.UnitConversion = dplyr::case_when(
      (!is.na(ResultMeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Convert"),
      is.na(ResultMeasureValue) ~ as.character("NoResultValue"),
      is.na(WQX.TargetUnit) ~ as.character("NoTargetUnit")
    ))
  
  # add WQX.DetectionLimitMeasureValue.UnitConversion column
  flag.data <- flag.data %>%
    # apply function row by row
    dplyr::rowwise() %>%
    # create flag column
    dplyr::mutate(WQX.DetectionLimitMeasureValue.UnitConversion = dplyr::case_when(
      (!is.na(DetectionQuantitationLimitMeasure.MeasureValue) &
         !is.na(WQX.TargetUnit)) ~ as.character("Convert"),
      is.na(DetectionQuantitationLimitMeasure.MeasureValue) ~ as.character("NoDetectionLimitValue"),
      is.na(WQX.TargetUnit) ~ as.character("NoTargetUnit")
    ))
  
  if (transform == FALSE) {
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add flag columns to the list
    col.order <- append(col.order, c(
      "WQX.TargetUnit",
      "WQX.ConversionFactor",
      "WQX.ResultMeasureValue.UnitConversion",
      "WQX.DetectionLimitMeasureValue.UnitConversion"
    ))
    # reorder columns in flag.data
    flag.data <- flag.data[, col.order]
    # place flag columns next to relevant fields
    flag.data <- flag.data %>%
      dplyr::relocate(c(
        "WQX.TargetUnit",
        "WQX.ConversionFactor",
        "WQX.ResultMeasureValue.UnitConversion"
      ),
      .after = "ResultMeasure.MeasureUnitCode"
      ) %>%
      dplyr::relocate("WQX.DetectionLimitMeasureValue.UnitConversion",
                      .after = "DetectionQuantitationLimitMeasure.MeasureUnitCode"
      )
    
    
    print("Conversions required for range checks and TADATargetUnit conversions -- Unit conversions, data summaries, and data calculations may be affected.")
    return(flag.data)
  }
  
  if (transform == TRUE) {
    
    # Duplicate unit columns, rename with .Original suffix
    if (("ResultMeasureUnitCode.Original" %in% colnames(flag.data)) == FALSE) {
      flag.data$ResultMeasureUnitCode.Original <- flag.data$ResultMeasure.MeasureUnitCode
    }
    if (("DetectionLimitMeasureUnitCode.Original" %in% colnames(flag.data)) == FALSE) {
      flag.data$DetectionLimitMeasureUnitCode.Original <-
        flag.data$DetectionQuantitationLimitMeasure.MeasureUnitCode
    }
    # Transform result measure value to Target Unit only if target unit exists
    clean.data <- flag.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(ResultMeasureValue = dplyr::case_when(
        !is.na(WQX.TargetUnit) ~
          (ResultMeasureValue * WQX.ConversionFactor),
        is.na(WQX.TargetUnit) ~ ResultMeasureValue
      ))
    
    # populate ResultMeasure.MeasureUnitCode
    clean.data <- clean.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # use target unit where there is a target unit, use original unit if no target unit
      dplyr::mutate(ResultMeasure.MeasureUnitCode = dplyr::case_when(
        !is.na(WQX.TargetUnit) ~ WQX.TargetUnit,
        is.na(WQX.TargetUnit) ~ ResultMeasure.MeasureUnitCode
      ))
    
    # Transform detection limit measure value to Target Unit only if target unit exists
    clean.data <- clean.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureValue = dplyr::case_when(
        !is.na(WQX.TargetUnit) ~
          (DetectionQuantitationLimitMeasure.MeasureValue * WQX.ConversionFactor),
        is.na(WQX.TargetUnit) ~ DetectionQuantitationLimitMeasure.MeasureValue
      ))
    
    # populate DetectionQuantitationLimitMeasure.MeasureUnitCode
    clean.data <- clean.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # use target unit where there is a target unit, use original unit if no target unit
      dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureUnitCode = dplyr::case_when(
        !is.na(WQX.TargetUnit) ~ WQX.TargetUnit,
        is.na(WQX.TargetUnit) ~ DetectionQuantitationLimitMeasure.MeasureUnitCode
      ))
    
    
    # edit WQX.ResultMeasureValue.UnitConversion column
    clean.data <- clean.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(WQX.ResultMeasureValue.UnitConversion = dplyr::case_when(
        (!is.na(ResultMeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Converted"),
        TRUE ~ WQX.ResultMeasureValue.UnitConversion
      ))
    
    # edit WQX.DetectionLimitMeasureValue.UnitConversion column
    clean.data <- clean.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(WQX.DetectionLimitMeasureValue.UnitConversion = dplyr::case_when(
        (!is.na(DetectionQuantitationLimitMeasure.MeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Converted"),
        TRUE ~ WQX.DetectionLimitMeasureValue.UnitConversion
      ))
    
    
    # remove extraneous columns, fix field names
    clean.data <- clean.data %>%
      dplyr::select(-c("WQX.ConversionFactor", "WQX.TargetUnit"))
    
    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add ResultUnitConversion column to the list if flag = TRUE
    
    col.order <- append(col.order, c(
      "WQX.ResultMeasureValue.UnitConversion",
      "WQX.DetectionLimitMeasureValue.UnitConversion"
    ))
    
    # add original units to list if transform = TRUE
    if (transform == TRUE) {
      col.order <- append(col.order, c(
        "ResultMeasureUnitCode.Original",
        "DetectionLimitMeasureUnitCode.Original"
      ))
      
      # reorder columns in clean.data
      clean.data <- clean.data[, col.order]
      
      # place flag columns next to relevant fields
      clean.data <- clean.data %>%
        dplyr::relocate("ResultMeasureUnitCode.Original",
                        .after = "ResultMeasure.MeasureUnitCode"
        ) %>%
        dplyr::relocate("WQX.DetectionLimitMeasureValue.UnitConversion",
                        .after = "DetectionQuantitationLimitMeasure.MeasureUnitCode"
        )
      
      # Place original unit columns next to original columns
      
      clean.data <- clean.data %>%
        dplyr::relocate("WQX.ResultMeasureValue.UnitConversion",
                        .after = "ResultMeasure.MeasureUnitCode"
        ) %>%
        dplyr::relocate("DetectionLimitMeasureUnitCode.Original",
                        .after = "DetectionQuantitationLimitMeasure.MeasureUnitCode"
        )
    }
    
    return(clean.data)
  }
}




#' Convert Depth Units
#'
#' Default is transform = TRUE. When transform = TRUE, all depth data is converted
#' to a consistent unit; and no additional columns are added. 
#' The default unit is "m" (meters), but a user can also choose 
#' "ft" or "in". When transform = FALSE, where depth data is populated, the 
#' function appends columns indicating the target unit and conversion factor for
#' each row; and for each depth field. No conversions are done at this time. 
#' A user can review the conversion factor information if desired by using this
#' feature. 
#' 
#' @param .data TADA dataframe
#' @param unit Character string input indicating the target depth unit to use for 
#' conversions. Allowable values for 'unit' are either "m" (meters), "ft" (feet),
#' or "in" (inches). 'unit' accepts only one allowable value as an input. The
#' default unit = "m".
#' @param fields Character string input indicating the relevant depth data fields
#' that will be converted to the desired target unit. Allowable values for 'fields'
#' are "ActivityDepthHeightMeasure", "ActivityTopDepthHeightMeasure", 
#' "ActivityBottomDepthHeightMeasure", and "ResultDepthHeightMeasure". The default
#' is to include all depth fields.
#' @param transform Boolean argument; The default is transform = TRUE. 
#' When transform = TRUE, all depth data is converted to the target unit; and
#' no additional columns are added. When transform = FALSE, where depth data 
#' is populated, the function appends columns indicating the target unit and 
#' conversion factor for each row; and for each depth field. No conversions are
#' done at this time. A user can review the conversion factor information if 
#' desired by using this feature. 
#' 
#' @return When transform = TRUE, the input dataframe is returned with all depth
#' data converted to the target unit; no additional columns are added.
#' When transform = FALSE, the input dataframe is returned with additional
#' columns including... be specific here ... 
#' 
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Convert all depth units to meters:
#' DepthUnitsConverted_m <- ConvertDepthUnits(Nutrients_Utah)
#' 
#' # Convert all depth units to feet:
#' DepthUnitsConverted_ft <- ConvertDepthUnits(Nutrients_Utah, unit = "ft")
#' 
#' # Convert only the "ActivityTopDepthHeightMeasure" field to inches:
#' TopDepthUnitsConverted_in <- ConvertDepthUnits(Nutrients_Utah, 
#' unit = "in", fields = "ActivityTopDepthHeightMeasure")
#' 
#' # Do not convert any depth units, but add columns for target units and 
#' # conversion factors for each depth measure:
#' DepthUnitsNotConverted <- ConvertDepthUnits(Nutrients_Utah, transform = FALSE)

ConvertDepthUnits <- function(.data,
                              unit = "m",
                              fields = c("ActivityDepthHeightMeasure",
                                         "ActivityTopDepthHeightMeasure",
                                         "ActivityBottomDepthHeightMeasure",
                                         "ResultDepthHeightMeasure"),
                              transform = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check unit is character
  checkType(unit, "character")
  # check unit argument for valid number of inputs (e.g., vector of character)
  if (length(unit) != 1) {
    stop("Invalid 'unit' argument. 'unit' accepts only one allowable value as an
         input. 'unit' must be one of either 'm' (meter), 'ft' (feet), or 'in'
         (inch).")
  }
  # check unit argument for valid inputs
  if (all(is.na(match(c("m", "ft", "in"), unit))) == TRUE) {
    stop("Invalid 'unit' argument. 'unit' must be either 'm' (meter), 'ft' (feet), or 'in' (inch).")
  }
  # check fields argument for valid inputs
  valid_fields <- c("ActivityDepthHeightMeasure",
                   "ActivityTopDepthHeightMeasure",
                   "ActivityBottomDepthHeightMeasure",
                   "ResultDepthHeightMeasure")
  if (all(is.na(match(valid_fields, fields))) == TRUE) {
    stop("Invalid 'fields' argument. 'fields' must include one or many of the
    following: 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
    'ActivityBottomDepthHeightMeasure,' and/or 'ResultDepthHeightMeasure.'")
  }
  # check transform is boolean
  checkType(transform, "logical")
  
  # .data required columns
  expected_cols <-c(
    "ActivityDepthHeightMeasure.MeasureValue",
    "ActivityDepthHeightMeasure.MeasureUnitCode",
    "ActivityTopDepthHeightMeasure.MeasureValue",
    "ActivityTopDepthHeightMeasure.MeasureUnitCode",
    "ActivityBottomDepthHeightMeasure.MeasureValue",
    "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
    "ResultDepthHeightMeasure.MeasureValue",
    "ResultDepthHeightMeasure.MeasureUnitCode",
    "ActivityEndTime.TimeZoneCode"
    )
  checkColumns(.data, expected_cols)
  
  # execute function after checks are passed
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
  
  for (i in seq(length(valid_fields)+1)) {
    field <- valid_fields[i]
    if ((field %in% fields) == TRUE) {
      # Old unit column
      unitCol <- paste(field, ".MeasureUnitCode", sep="")
      
      # proceed only if unitCol has values other than NA
      if (sum(!is.na(check.data[unitCol])) > 0) {
        
        # Join conversion factor from unit.ref to .data by unitCol
        check.data <- merge(check.data, unit.ref[, c("Code", "Conversion.Factor")],
                            by.x = unitCol,
                            by.y = "Code",
                            all.x = TRUE, 
                            sort = FALSE
        )
        
        # rename new columns
        names(check.data)[names(check.data) == "Conversion.Factor"] <- paste('WQXConversionFactor.', field,  sep="")
        
      }
    }
  }
  
  # check if any Conversion Factor columns were appended
  if (all(is.na(match(appCols, colnames(check.data)))) == TRUE) {
    stop("The dataframe does not have any depth data.")
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
  
  # if transform = TRUE, apply conversions and remove extra columns
  if (transform == TRUE) {
    # define clean.data
    clean.data <- check.data
    
    # add WQX.Depth.TargetUnit column
    #clean.data[ , 'WQX.Depth.TargetUnit'] <- unit
    
    # if WQXConversionFactor.ActivityDepthHeightMeasure exists...
    if (("WQXConversionFactor.ActivityDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
      # multiply ActivityDepthHeightMeasure.MeasureValue by WQXConversionFactor.ActivityDepthHeightMeasure
      clean.data$ActivityDepthHeightMeasure.MeasureValue <- ((clean.data$ActivityDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ActivityDepthHeightMeasure))
      
      # uncomment out below to keep a copy of the original unit field
      #clean.data$ActivityDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ActivityDepthHeightMeasure.MeasureUnitCode
      #clean.data <- clean.data %>%
      #  dplyr::relocate("ActivityDepthHeightMeasure.MeasureUnitCode.Original",
      #                  .after = "WQXConversionFactor.ActivityDepthHeightMeasure"
      #  )
      
      # then replace ActivityDepthHeightMeasure.MeasureUnitCode values with the new unit argument
      clean.data$ActivityDepthHeightMeasure.MeasureUnitCode[which(
        !is.na(clean.data$ActivityDepthHeightMeasure.MeasureUnitCode)
      )] <- unit
      
      # move the relevant data and unit fields next to each other in the dataframe
      clean.data <- clean.data %>%
        dplyr::relocate("ActivityDepthHeightMeasure.MeasureUnitCode",
                        .after = "ActivityDepthHeightMeasure.MeasureValue"
        )
      
      # comment out below to keep ActDepth.Conversion.Unit column
      clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ActivityDepthHeightMeasure")
    }
    
    # if WQXConversionFactor.ActivityTopDepthHeightMeasure exists...
    if (("WQXConversionFactor.ActivityTopDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
      # multiply ActivityTopDepthHeightMeasure.MeasureValue by WQXConversionFactor.ActivityTopDepthHeightMeasure
      clean.data$ActivityTopDepthHeightMeasure.MeasureValue <- ((clean.data$ActivityTopDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ActivityTopDepthHeightMeasure))
      
      # uncomment out below to keep a copy of the original unit field
      #clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode
      #clean.data <- clean.data %>%
      #  dplyr::relocate("ActivityTopDepthHeightMeasure.MeasureUnitCode.Original",
      #                  .after = "WQXConversionFactor.ActivityTopDepthHeightMeasure"
      #  )
      
      # replace ActivityTopDepthHeightMeasure.MeasureUnitCode values with unit argument
      clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode[which(
        !is.na(clean.data$ActivityTopDepthHeightMeasure.MeasureUnitCode)
      )] <- unit
      
      clean.data <- clean.data %>%
        dplyr::relocate("ActivityTopDepthHeightMeasure.MeasureUnitCode",
                        .after = "ActivityTopDepthHeightMeasure.MeasureValue"
        )
      # comment out below to keep ActTopDepth.Conversion.Unit column
      clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ActivityTopDepthHeightMeasure")
    }
    
    # if WQXConversionFactor.ActivityBottomDepthHeightMeasure exists...
    if (("WQXConversionFactor.ActivityBottomDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
      # multiply ActivityBottomDepthHeightMeasure.MeasureValue by WQXConversionFactor.ActivityBottomDepthHeightMeasure
      clean.data$ActivityBottomDepthHeightMeasure.MeasureValue <- ((clean.data$ActivityBottomDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ActivityBottomDepthHeightMeasure))
      
      # uncomment out below to keep a copy of the original unit field
      #clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode
      #clean.data <- clean.data %>%
      #  dplyr::relocate("ActivityBottomDepthHeightMeasure.MeasureUnitCode.Original",
      #                  .after = "WQXConversionFactor.ActivityBottomDepthHeightMeasure"
      #  )
      
      # replace ActivityTopDepthHeightMeasure.MeasureUnitCode values with unit argument
      clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode[which(
        !is.na(clean.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode)
      )] <- unit
      
      clean.data <- clean.data %>%
        dplyr::relocate("ActivityBottomDepthHeightMeasure.MeasureUnitCode",
                        .after = "ActivityBottomDepthHeightMeasure.MeasureValue"
        )
      # comment out below to keep ActBottomDepth.Conversion.Unit column
      clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ActivityBottomDepthHeightMeasure")
    }
    
    # if WQXConversionFactor.ResultDepthHeightMeasure exists...
    if (("WQXConversionFactor.ResultDepthHeightMeasure" %in% colnames(clean.data)) == TRUE) {
      # multiply ResultDepthHeightMeasure.MeasureValue by WQXConversionFactor.ResultDepthHeightMeasure
      clean.data$ResultDepthHeightMeasure.MeasureValue <- ((clean.data$ResultDepthHeightMeasure.MeasureValue) * (clean.data$WQXConversionFactor.ResultDepthHeightMeasure))
      
      # uncomment out below to keep a copy of the original unit field
      #clean.data$ResultDepthHeightMeasure.MeasureUnitCode.Original <- clean.data$ResultDepthHeightMeasure.MeasureUnitCode
      #clean.data <- clean.data %>%
      #  dplyr::relocate("ResultDepthHeightMeasure.MeasureUnitCode.Original",
      #                  .after = "WQXConversionFactor.ResultDepthHeightMeasure"
      #  )
      
      # replace ResultDepthHeightMeasure.MeasureUnitCode values with unit argument
      clean.data$ResultDepthHeightMeasure.MeasureUnitCode[which(
        !is.na(clean.data$ResultDepthHeightMeasure.MeasureUnitCode)
      )] <- unit
      
      clean.data <- clean.data %>%
        dplyr::relocate("ResultDepthHeightMeasure.MeasureUnitCode",
                        .after = "ResultDepthHeightMeasure.MeasureUnitCode"
        )
      # comment out below to keep WQXConversionFactor.ResultDepthHeightMeasure column
      clean.data <- dplyr::select(clean.data, -"WQXConversionFactor.ResultDepthHeightMeasure")
    }
    
    # MAY BE ABLE TO DELETE BELOW NO LONGER NEEDED? uncomment below to delete WQX.Depth.TargetUnit column
    # clean.data <- dplyr::select(clean.data, -"WQX.Depth.TargetUnit")
    
    return(clean.data)
  }
}




#' Generate Unique Harmonization Reference Table
#' 
#' Function generates a harmonization reference table that is specific to
#' the input dataframe. Users can review how their input data relates to standard
#' TADA values for CharacteristicName, ResultSampleFractionText,
#' MethodSpecificationName, and ResultMeasure.MeasureUnitCode and they can optionally
#' edit the reference file to meet their needs.
#'
#' @param .data TADA dataframe
#' 
#' @param download Boolean argument; when download = TRUE, the output is
#' downloaded to the current working directory.
#'
#' @return Harmonization Reference Table unique to the input dataframe
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create a harmonization reference table for dataframe:
#' CreateRefTable <- HarmonizationRefTable(Nutrients_Utah)
#' 
#' # Create and download (to your working directory) a harmonization reference
#' # table for dataframe: 
#' \dontrun{
#' DownloadRefTable <- HarmonizationRefTable(Nutrients_Utah, download = TRUE)
#' }
#' 

HarmonizationRefTable <- function(.data, download = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check download is boolean
  checkType(download, "logical")
  
  # check .data has the required columns
  expected_cols <- c(
    "CharacteristicName", "ResultSampleFractionText",
    "MethodSpecificationName",
    "ResultMeasure.MeasureUnitCode"
    )
  checkColumns(.data, expected_cols)
  
  # execute function after checks are passed
  # define raw harmonization table as an object
  harm.raw <- utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv", package = "TADA"))
  
  # join harmonization table to .data
  harmonization_cols <- c(
    "WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
    "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity"
  )
  # if WQX QA Char Val flags are in .data, include them in the join
  if (all(harmonization_cols %in% colnames(.data)) == TRUE) {
    join.data <- merge(.data[, c(expected_cols, harmonization_cols)],
                       harm.raw,
                       by.x = expected_cols,
                       by.y = harmonization_cols,
                       all.x = TRUE
    )
    # otherwise, execute the join with no additional columns
  } else {
    join.data <- merge(.data[, expected_cols],
                       harm.raw,
                       by.x = expected_cols,
                       by.y = expected_cols,
                       all.x = TRUE
    )
  }
  
  # trim join.data to include only unique combos of char-frac-spec-unit
  unique.data <- join.data %>%
    dplyr::filter(!duplicated(join.data[, expected_cols]))
  
  # reorder columns to match harm.raw
  # include WQX QA flag columns, if they exist
  if (all(harmonization_cols %in% colnames(.data)) == TRUE) {
    # get .data column names
    col.order <- colnames(harm.raw)
    # add WQX.SampleFractionValidity column to the list
    col.order <- append(col.order, harmonization_cols)
    # reorder columns in flag.data
    unique.data <- unique.data[, col.order]
  } else {
    unique.data <- unique.data[, colnames(harm.raw)]
  }
  
  # remove extraneous characters in first column
  colnames(unique.data)[1] <- gsub("^", "", colnames(unique.data)[1])
  
  # if download = TRUE, download unique.data as a csv to the working directory
  if (download == TRUE) {
    utils::write.csv(unique.data, "HarmonizationRefTable.csv", row.names = FALSE)
  }
  
  # return unique.data
  return(unique.data)
}



#' Transform CharacteristicName, ResultSampleFractionText, MethodSpecificationName, 
#' and ResultMeasure.MeasureUnitCode values to TADA standards.
#'
#' Function compares input dataframe to the TADA Harmonization Reference Table, and makes
#' synonymous data consistent.
#' Optional outputs include: 1) the dataframe with
#' Harmonization columns appended, 2) the dataframe with CharacteristicName,
#' ResultSampleFractionText, MethodSpecificationName, and
#' ResultMeasure.MeasureUnitCode converted to TADA standards or 3) the four fields
#' converted with most Harmonization Reference Table columns appended. Default is
#' transform = TRUE and flag = TRUE.
#'
#' @param .data TADA dataframe
#' @param ref Optional argument to specify which dataframe to use as a reference
#' file. The primary use for this argument is when a user has generated a
#' harmonization reference file unique to their data, and they made changes to
#' that file.
#' @param transform Boolean argument; transforms and/or converts original values
#' in the dataframe to the TADA Harmonization Reference Table values for the
#' following fields: CharacteristicName, ResultSampleFractionText,
#' MethodSpecificationName, and ResultMeasure.MeasureUnitCode. Default is
#' transform = TRUE.
#' @param flag Boolean argument; appends all columns from the TADA Harmonization
#' Reference Table to the dataframe. Default is flag = TRUE.
#'
#' @return When transform = FALSE and flag = TRUE, Harmonization Reference Table
#' columns are appended to the dataframe only. When transform = TRUE and flag = TRUE,
#' Harmonization columns are appended to the dataframe and transformations are
#' executed. When transform = TRUE and flag = FALSE, transformations are executed
#' only. When transform = FALSE and flag = FALSE, an error is returned (function
#' would return the input dataframe unchanged if input was allowed).
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Append harmonization reference table columns to dataframe and transform/convert
#' # data to the reference table values:
#' Nutrients_Harmonized <- HarmonizeData(Nutrients_Utah)
#' 
#' # Transform/convert data to the harmonization reference table values, but
#' # do not append any columns to dataframe:
#' Nutrients_Harmonized_noflags <- HarmonizeData(Nutrients_Utah, flag = FALSE)
#' 
#' # Append harmonization reference table columns to dataframe, but do not
#' # transform/convert data to the reference table values:
#' Nutrients_NotHarmonized <- HarmonizeData(Nutrients_Utah, transform = FALSE)

HarmonizeData <- function(.data, ref, transform = TRUE, flag = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check transform is boolean
  checkType(transform, "logical")
  # check flag is boolean
  checkType(flag, "logical")

  # check .data has the required columns
  expected_cols <- c(
    "CharacteristicName", "ResultSampleFractionText",
    "MethodSpecificationName", "ResultMeasure.MeasureUnitCode"
    )
  checkColumns(.data, expected_cols)

  # check that both transform and flag do NOT equal FALSE
  if (transform == FALSE & flag == FALSE) {
    stop("Both 'transform' and 'flag' arguments equal FALSE, which would return
         the input dataframe unchanged. One or both arguments must be equal to
         TRUE.")
  }

  #define which columns are expected in ref
  expected_ref_cols <- c(
    "TADA.SuggestedCharacteristicName",
    "TADA.SuggestedSampleFraction",
    "TADA.SuggestedSpeciation",
    "TADA.SuggestedResultUnit"
  )
  
  # execute function after checks are passed
  if (all(c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode"
  ) %in% colnames(.data)) == TRUE) {

    # if class(ResultMeasureValue) != numeric, run special char function
    if (!is.numeric(.data$ResultMeasureValue) ) {
      .data <- MeasureValueSpecialCharacters(.data)
    }

    # define harm.ref
    # if input for ref exists, use that data
    if (!missing(ref)) {
      
      # check ref is data.frame
      checkType(ref, "data.frame")
      
      # check ref has all of the required columns      
      checkColumns(ref, expected_ref_cols)      
      
      harm.ref <- ref
      
    }
    
    # if input for ref does not exist, use raw harmonization template
    if (missing(ref)) {
      # use output of HarmonizationRefTable which uses the TADA HarmonizationTemplate.csv in the extdata folder
      harm.ref <- HarmonizationRefTable(.data, download=FALSE)
    }

    # join harm.ref to .data
    flag.data <- merge(.data, harm.ref,
                       by.x = expected_cols,
                       by.y = expected_cols,
                       all.x = TRUE
                       )

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
    # add flag columns to the list
    col.order <- append(col.order, c(
      "TADACharacteristicGroup",
      "CharacteristicNameUserSupplied",
      "TADA.SuggestedCharacteristicName",
      "TADA.CharacteristicNameAssumptions",
      "TADA.SuggestedSampleFraction",
      "TADA.FractionAssumptions",
      "TADA.SuggestedSpeciation",
      "TADA.SpeciationAssumptions",
      "TADA.SpeciationConversionFactor",
      "TADA.SuggestedResultUnit",
      "TADA.UnitConversionFactor",
      "TADA.UnitConversionCoefficient",
      "CombinationValidity",
      "TADA.ComparableDataIdentifier",
      "TADA.TotalN_TotalP_CharacteristicNames_AfterSummation",
      "TADA.TotalN_TotalP_Summation_Identifier",
      "TADA.TotalN_TotalP_ComboLogic"
    ))
    # reorder columns in flag.data
    flag.data <- flag.data[, col.order]
    # place flag columns next to relevant fields
    flag.data <- flag.data %>%
      dplyr::relocate("TADACharacteristicGroup",
        .before = "CharacteristicName"
      ) %>%
      dplyr::relocate(c(
        "CharacteristicNameUserSupplied",
        "TADA.SuggestedCharacteristicName",
        "TADA.CharacteristicNameAssumptions"
      ),
      .after = "CharacteristicName"
      ) %>%
      dplyr::relocate(c(
        "TADA.SuggestedSampleFraction",
        "TADA.FractionAssumptions"
      ),
      .after = "ResultSampleFractionText"
      ) %>%
      dplyr::relocate(c(
        "TADA.SuggestedSpeciation",
        "TADA.SpeciationAssumptions",
        "TADA.SpeciationConversionFactor"
      ),
      .after = "MethodSpecificationName"
      ) %>%
      dplyr::relocate(c(
        "TADA.SuggestedResultUnit",
        "TADA.UnitConversionFactor",
        "TADA.UnitConversionCoefficient"
      ),
      .after = "ResultMeasure.MeasureUnitCode"
      )

    # if transform = FALSE and flag = TRUE, return flag.data
    if ((transform == FALSE) & (flag == TRUE)) {
      print("Be aware that you must run this function with transform = TRUE to use subsequent TADA functions.")
      return(flag.data)
    }

    # if transform = TRUE, transform data
    if (transform == TRUE) {

      # CharacteristicName
      # replace CharacteristicName with TADA.SuggestedCharacteristicName
      clean.data <- flag.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use TADA suggested name where there is a suggested name, use original name if no suggested name
        dplyr::mutate(CharacteristicName = dplyr::case_when(
          !is.na(TADA.SuggestedCharacteristicName) ~ TADA.SuggestedCharacteristicName,
          is.na(TADA.SuggestedCharacteristicName) ~ CharacteristicName
        ))

      # ResultSampleFractionText
      # replace ResultSampleFractionText with TADA.SuggestedSampleFraction
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use TADA suggested frac where there is a suggested frac, use original frac if no suggested frac
        dplyr::mutate(ResultSampleFractionText = dplyr::case_when(
          !is.na(TADA.SuggestedSampleFraction) ~ TADA.SuggestedSampleFraction,
          is.na(TADA.SuggestedSampleFraction) ~ ResultSampleFractionText
        ))


      # ResultMeasure.MeasureUnitCode
      # replace ResultMeasure.MeasureUnitCode with TADA.SuggestedResultUnit
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use TADA suggested unit where there is a suggested unit, use original unit if no suggested unit
        dplyr::mutate(ResultMeasure.MeasureUnitCode = dplyr::case_when(
          !is.na(TADA.SuggestedResultUnit) ~ TADA.SuggestedResultUnit,
          is.na(TADA.SuggestedResultUnit) ~ ResultMeasure.MeasureUnitCode
        )) %>%
        # if conversion factor exists, multiply by ResultMeasureValue
        dplyr::rowwise() %>%
        dplyr::mutate(ResultMeasureValue = dplyr::case_when(
          !is.na(TADA.UnitConversionFactor) ~
            (TADA.UnitConversionFactor * ResultMeasureValue),
          is.na(TADA.UnitConversionFactor) ~ ResultMeasureValue
        ))

      # MethodSpecificationName
      # replace MethodSpecificationName with TADA.SuggestedSpeciation
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
        dplyr::mutate(MethodSpecificationName = dplyr::case_when(
          !is.na(TADA.SuggestedSpeciation) ~ TADA.SuggestedSpeciation,
          is.na(TADA.SuggestedSpeciation) ~ MethodSpecificationName
        )) %>%
        # if conversion factor exists, multiply by ResultMeasureValue
        dplyr::rowwise() %>%
        dplyr::mutate(ResultMeasureValue = dplyr::case_when(
          !is.na(TADA.SpeciationConversionFactor) ~
            (TADA.SpeciationConversionFactor * ResultMeasureValue),
          is.na(TADA.SpeciationConversionFactor) ~ ResultMeasureValue
        ))

      # remove conversion columns
      clean.data <- clean.data %>%
        dplyr::select(-c(
          expected_ref_cols,
          "TADA.SpeciationConversionFactor",
          "TADA.UnitConversionFactor"
        ))

      # if flag = TRUE, return clean.data
      if (flag == TRUE) {
        return(clean.data)
      }

      # remove all appended columns if flag = FALSE
      if (flag == FALSE) {
        # remove all appended columns
        clean.data <- clean.data %>%
          dplyr::select(-c(
            "TADACharacteristicGroup",
            "CharacteristicNameUserSupplied",
            "CombinationValidity",
            "TADA.CharacteristicNameAssumptions",
            "TADA.FractionAssumptions",
            "TADA.SpeciationAssumptions",
            "TADA.UnitConversionCoefficient",
            "TADA.ComparableDataIdentifier",
            "TADA.TotalN_TotalP_CharacteristicNames_AfterSummation",
            "TADA.TotalN_TotalP_Summation_Identifier",
            "TADA.TotalN_TotalP_ComboLogic"
          ))

        # return clean.data
        return(clean.data)
      }
    }
  }
}



#' Harmonize censored data
#' 
#' Function substitutes monitoring device/method detection limits (if available) as result values when applicable.
#'
#' @param transform Boolean argument with two possible values, “TRUE” and “FALSE”.
#' Default is transform = TRUE.
#' 
#' @param .data Optional argument; TADA dataframe
#'
#' @return When transform=TRUE, monitoring device/method detection limits (if available) are substituted as result values and units.
#' When transform = FALSE, monitoring device/method detection limits (if available) are NOT substituted as result values and units - 
#' Instead, columns are appended to rows that may include censored data. The flag indicates 1) if the row contains censored data, and 2)
#' if monitoring device/method detection limits are available.  
#' 

HarmonizeCensoredData <- function(transform, .data) {
  # if .data specified, check .data is data.frame
  if (!missing(.data)) {
    checkType(.data, "data.frame", "Input object")
  }
  # execute function after checks are passed 
  {
    
  }
}


#' Generates Censored Data Statictics
#' 
#' Function summarizes censored data in dataframe, including any substitutions made. 
#' 
#' @param .data Optional argument; TADA dataframe
#'
#' @return Summary table
#' 

CensoredDataStats <- function(.data) {
  # if .data specified, check .data is data.frame
  if (!missing(.data)) {
    checkType(.data, "data.frame", "Input object")
  }
  # execute function after checks are passed 
  {
    
  }
}
