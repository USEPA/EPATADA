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
#' It also uses the following six fields from an autocleaned input dataframe:
#' 1) "TADA.CharacteristicName",
#' 2) "TADA.ActivityMediaName",
#' 3) "TADA.ResultMeasureValue",
#' 4) "TADA.ResultMeasure.MeasureUnitCode",
#' 5) "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
#' 6) "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
#'
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
#' This function changes the values within the "TADA.ResultMeasure.MeasureUnitCode"
#' and "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode" to the WQX target units and
#' converts respective values within the "TADA.ResultMeasureValue" and "TADA.DetectionQuantitationLimitMeasure.MeasureValue"
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
#' ResultUnitsConverted <- (Nutrients_Utah)
#' 
#' # Do not convert result values and units, but add two new columns titled
#' # "WQX.ConversionFactor" and "WQX.TargetUnit":
#' ResultUnitsNotConverted <- TADA_ConvertResultUnits(Nutrients_Utah, transform = FALSE)
#' 

TADA_ConvertResultUnits <- function(.data, transform = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check transform is boolean
  checkType(transform, "logical")
  # check .data has all of the required columns
  
  expected_cols <- c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
    "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )
  
  checkColumns(.data, expected_cols)
  
  # execute function after checks are passed
  
  # filter WQXcharValRef to include only valid CharacteristicUnit in water media
  unit.ref <- TADA_GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" &
                    Status == "Valid")
  # join unit.ref to .data
  check.data <- merge(.data, unit.ref[, c(
    "Characteristic", "Source",
    "Value", "Value.Unit",
    "Conversion.Factor"
  )],
  by.x = c("TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasure.MeasureUnitCode"), # note: only joins on ResultMeasure.MeasureUnitCode, so will not join to detection limit units
  by.y = c("Characteristic", "Source", "Value"), all.x = TRUE
  )
  # rename columns
  flag.data <- check.data %>%
    dplyr::rename(WQX.TargetUnit = Value.Unit) %>%
    dplyr::rename(WQX.ConversionFactor = Conversion.Factor)
  
  # if temp data exists, calculate conversion factor
  # EDH I THINK THIS RUNS IF THERE IS ONE OR MORE NA'S IN THE DATASET
  if (all(is.na(match(
    c("deg F", "deg K"),
    flag.data$TADA.ResultMeasure.MeasureUnitCode
  ))) == FALSE) {
    
    # Calculate deg F and deg C, replace Conversion factor values
    flag.data <- flag.data %>%
      # create flag column
      dplyr::mutate(WQX.ConversionFactor = dplyr::case_when(
        TADA.ResultMeasure.MeasureUnitCode == "deg F" ~
          as.numeric(((TADA.ResultMeasureValue - 32) * (5 / 9)) / TADA.ResultMeasureValue),
        TADA.ResultMeasure.MeasureUnitCode == "deg K" ~
          as.numeric((TADA.ResultMeasureValue - 273.15) / TADA.ResultMeasureValue),
        TRUE ~ WQX.ConversionFactor
      ))
  }
  
  # add WQX.ResultMeasureValue.UnitConversion column
  flag.data <- flag.data %>%
    # create flag column
    dplyr::mutate(WQX.ResultMeasureValue.UnitConversion = dplyr::case_when(
      (!is.na(TADA.ResultMeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Convert"),
      is.na(TADA.ResultMeasureValue) ~ as.character("No Result Value"),
      is.na(WQX.TargetUnit) ~ as.character("No Target Unit")
    ))
  
  if (transform == FALSE) {
    
    print("Conversions required for range checks and TADATargetUnit conversions -- Unit conversions, data summaries, and data calculations may be affected.")
    # reorder columns
    flag.data <- TADA_OrderCols(flag.data)
    return(flag.data)
  }
  
  if (transform == TRUE) {
    # Transform result measure value to Target Unit only if target unit exists
    clean.data <- flag.data %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
        is.na(TADA.ResultMeasureValue) ~ TADA.ResultMeasureValue,
        !is.na(WQX.TargetUnit) ~
          (TADA.ResultMeasureValue * WQX.ConversionFactor),
        is.na(WQX.TargetUnit) ~ TADA.ResultMeasureValue
      ))
    
    # populate ResultMeasure.MeasureUnitCode
    clean.data <- clean.data %>%
      # use target unit where there is a target unit, use original unit if no target unit
      dplyr::mutate(TADA.ResultMeasure.MeasureUnitCode = dplyr::case_when(
        !is.na(WQX.TargetUnit) ~ WQX.TargetUnit,
        is.na(WQX.TargetUnit) ~ TADA.ResultMeasure.MeasureUnitCode
      ))
   
    # edit WQX.ResultMeasureValue.UnitConversion column
    clean.data <- clean.data %>%
      # apply function row by row- EDH - I don't think this is needed (I think default behavior of case_when is row by row)?
      # dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(WQX.ResultMeasureValue.UnitConversion = dplyr::case_when(
        (WQX.ConversionFactor == 1) ~ as.character("No Conversion Needed"),
        (!is.na(TADA.ResultMeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Converted"),
        TRUE ~ WQX.ResultMeasureValue.UnitConversion
      ))
    
    # remove extraneous columns, fix field names
    clean.data <- clean.data %>%
      dplyr::select(-c("WQX.ConversionFactor", "WQX.TargetUnit"))
    
    # create new comparable data identifier column following conversion
    clean.data = createComparableId(clean.data)

  }
  # reorder cols
  clean.data <- TADA_OrderCols(clean.data)
  
  return(clean.data)
}



#' Convert Depth Units
#'
#' #'The **TADA_ConvertDepthUnits** function converts depth units to a consistent
#' unit. Depth values and units are most commonly associated with lake
#' data, and are populated in the *ActivityDepthHeightMeasure*,
#' *ActivityTopDepthHeightMeasure*, *ActivityBottomDepthHeightMeasure*, and
#' *ResultDepthHeightMeasure* Result Value/Unit columns.
#' 
#' This function first checks the dataframe for depth profile data. Where depth
#' profile columns are populated, the function appends 'Conversion Factor'
#' columns and populates those columns based on the original unit and the target
#' unit, which is defined in the 'unit' argument. A 'Depth Target Unit' column
#' is also appended, indicating the unit all selected depth data is converted
#' to. When transform = FALSE, the output includes all 'Conversion Factor'
#' columns and the 'Depth Target Unit' column. When transform = TRUE, the output
#' includes converted depth data in a column with the same name as the original,
#' plus the prefix "TADA." and a 'Depth Target Unit' column, which acts as a
#' flag indicating which rows have been converted. New columns are only created
#' if there are values/units in the column(s) of interest. Default is transform
#' = TRUE.
#' 
#' The depth profile function can harmonize the depth units across all the
#' following fields (or only a specific one): "ActivityDepthHeightMeasure",
#' "ActivityTopDepthHeightMeasure", "ActivityBottomDepthHeightMeasure",
#' "ResultDepthHeightMeasure"). It creates new result value/unit columns
#' with the prefix "TADA." to all converted columns. The default is to
#' check all four Depth Height columns.
#' 
#' Allowable values for 'unit' are either 'm' (meter), 'ft' (feet), or 'in'
#' (inch). 'unit' accepts only one allowable value as an input. Default is
#' unit = "m".
#' 
#' @param .data TADA dataframe
#' @param unit Character string input indicating the target depth unit to use for 
#' conversions. Allowable values for 'unit' are either "m" (meters), "ft" (feet),
#' or "in" (inches). 'unit' accepts only one allowable value as an input. The
#' default unit = "m".
#' @param fields Character string input indicating the relevant depth data fields
#' that will be converted to the desired target unit in new TADA column. Allowable 
#' values for 'fields' are "ActivityDepthHeightMeasure", "ActivityTopDepthHeightMeasure",
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
#' data converted to the target unit in new TADA-specific columns.
#' When transform = FALSE, the input dataframe is returned with additional
#' columns showing how the data would be handled when converted.
#' 
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Convert all depth units to meters:
#' DepthUnitsConverted_m <- TADA_ConvertDepthUnits(Nutrients_Utah)
#' 
#' # Convert all depth units to feet:
#' DepthUnitsConverted_ft <- TADA_ConvertDepthUnits(Nutrients_Utah, unit = "ft")
#' 
#' # Convert only the "TADA.ActivityTopDepthHeightMeasure" field to inches:
#' TopDepthUnitsConverted_in <- TADA_ConvertDepthUnits(Nutrients_Utah, 
#' unit = "in", fields = "ActivityTopDepthHeightMeasure")
#' 
#' # Do not convert any depth units, but add columns for target units and 
#' # conversion factors for each depth measure:
#' DepthUnitsNotConverted <- TADA_ConvertDepthUnits(Nutrients_Utah, transform = FALSE)
#' 

TADA_ConvertDepthUnits <- function(.data,
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
    "ResultDepthHeightMeasure.MeasureUnitCode"
  )
  checkColumns(.data, expected_cols)
  
  tadacols = c("TADA.ActivityDepthHeightMeasure.MeasureValue",
               "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
               "TADA.ActivityDepthHeightMeasure.MeasureValueDataTypes.Flag",
               "TADA.ActivityTopDepthHeightMeasure.MeasureValue",
               "TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode",
               "TADA.ActivityTopDepthHeightMeasure.MeasureValueDataTypes.Flag",
               "TADA.ActivityBottomDepthHeightMeasure.MeasureValue",
               "TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode",
               "TADA.ActivityBottomDepthHeightMeasure.MeasureValueDataTypes.Flag",
               "TADA.ResultDepthHeightMeasure.MeasureValue",
               "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
               "TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag")
  
  # Remove TADA cols if already run on a single dataset - this might occur if someone chooses initially one unit but changes mind and wants other unit.
  .data = .data[,!names(.data)%in%tadacols]
  
  # execute function after checks are passed
  # define check.data (to preserve .data and avoid mistakes with if statements below)
  check.data <- .data
  
  # conversion column names
  appCols <- c("WQXConversionFactor.ActivityDepthHeightMeasure",
               "WQXConversionFactor.ActivityTopDepthHeightMeasure",
               "WQXConversionFactor.ActivityBottomDepthHeightMeasure",
               "WQXConversionFactor.ResultDepthHeightMeasure")
  
  # read in unit conversion reference table from extdata
  unit.ref <- TADA_GetMeasureUnitRef()
  
  # subset to include only "Length Distance" units; filter by target unit defined in 'unit' argument
  unit.ref <- unit.ref %>%
    dplyr::filter(stringr::str_detect(
      Description,
      stringr::regex("\\bLength Distance")
    )) %>%
    dplyr::filter(Target.Unit == unit) #%>%
  
  # Loop over all supplied depth columns, create TADA columns, then join conversion table 
  for (i in 1:length(valid_fields)) {
    field <- valid_fields[i]
    if ((field %in% fields) == TRUE) {
      
      # OG unit column
      unitCol <- paste(field, ".MeasureUnitCode", sep="")
      valCol = paste0(field,".MeasureValue")
      # proceed only if unitCol has values other than NA
      if (sum(!is.na(check.data[unitCol])) > 0) {
        
        # new TADA column
        unitCol2 = paste0("TADA.",unitCol)
        
        # deal with any units that are "meters" and change to "m" (USGS convention)
        check.data$new = check.data[,unitCol]
        check.data$new[check.data$new=="meters"] = "m"
        names(check.data)[names(check.data)=="new"] = unitCol2
        
        # Join conversion factor from unit.ref to .data by unitCol
        check.data <- merge(check.data, unit.ref[, c("Code", "Conversion.Factor")],
                            by.x = unitCol2,
                            by.y = "Code",
                            all.x = TRUE, 
                            sort = FALSE
        )
        
        # rename new columns
        names(check.data)[names(check.data) == "Conversion.Factor"] <- paste('WQXConversionFactor.', field,  sep="")
        check.data = TADA_ConvertSpecialChars(check.data, valCol)
        
      }
    }
  }
  
  # check if any Conversion Factor columns were appended
  if (all(is.na(match(appCols, colnames(check.data)))) == TRUE) {
    warning("No action taken: the dataframe does not have any depth data in ActivityTop/BottomDepthHeight or ResultDepthHeight columns.")
    check.data <- TADA_OrderCols(check.data)
    return(check.data)
  }
  
  #function should always run all code above
  
  # if transform = FALSE, output data
  if (transform == FALSE) {
    # add WQX.Depth.TargetUnit column
    check.data[ , 'WQX.Depth.TargetUnit'] <- unit
    #reorder cols
    check.data <- TADA_OrderCols(check.data)
    return(check.data)
  }
  
  # if transform = TRUE, apply conversions and remove extra columns
  if (transform == TRUE) {
    # define clean.data
    clean.data <- check.data
    
    # function to run through each depth column
    conv_unit <- function(.data, coln){
      if(coln %in% colnames(.data)){
        .data$cf = .data[,coln]
        colnv = paste0(gsub("WQXConversionFactor","TADA",coln),".MeasureValue")
        .data$val = .data[,colnv]
        colnu = paste0(gsub("WQXConversionFactor","TADA",coln),".MeasureUnitCode")
        .data$unit = .data[,colnu]
        
        # multiply .MeasureValue by WQXConversionFactor.
        # if else added to deal with NA's in RV column, which throws error when NA multiplied by number.
        .data$val <- ifelse(!is.na(.data$val),.data$val * .data$cf,.data$val)
        
        # then replace unit values with the new unit argument
        .data$unit[which(
          !is.na(.data$unit)
        )] <- unit
        
        # replace TADA depth height columns and remove WQX conversion column
        .data <- dplyr::select(.data, -cf,-dplyr::all_of(coln),-dplyr::all_of(colnv),-dplyr::all_of(colnu))
        names(.data)[names(.data)=="val"] = colnv
        names(.data)[names(.data)=="unit"] = colnu
        
        return(.data)
      }else{return(.data)}
    }
    
    clean.data = conv_unit(clean.data,"WQXConversionFactor.ActivityDepthHeightMeasure")
    clean.data = conv_unit(clean.data,"WQXConversionFactor.ActivityBottomDepthHeightMeasure")
    clean.data = conv_unit(clean.data,"WQXConversionFactor.ResultDepthHeightMeasure")
    clean.data = conv_unit(clean.data,"WQXConversionFactor.ActivityTopDepthHeightMeasure")
    
    # order columns
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }


}
