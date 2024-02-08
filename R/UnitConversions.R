#' Transform Units to WQX Target Units
#'
#' This function compares measure units in the input data to the Water Quality
#' Exchange (WQX) 3.0 Measure Unit domain table. It also takes common USGS units
#' that include speciation information and transfers the speciation information
#' to the TADA.MethodSpeciationName field.
#'
#' It also uses the"TADA.ResultMeasureValue" and
#' "TADA.ResultMeasure.MeasureUnitCode" fields from an autocleaned input
#' dataframe to perform conversions as necessary when transform = TRUE.
#'
#' @param .data TADA dataframe
#'
#' @param transform Boolean argument with two possible values, “TRUE” and “FALSE”.
#' Default is transform = TRUE.
#'
#' @param detlimit Boolean argument with two possible values, "TRUE" and "FALSE".
#' Default is detlimit = TRUE.
#'
#' @return When transform = TRUE, result values and units are converted to WQX
#'   target units. This function changes the values within the
#'   "TADA.ResultMeasure.MeasureUnitCode" to the WQX target units and converts
#'   respective values within the "TADA.ResultMeasureValue" field.
#'   When detlimit = TRUE, detection limit values and units are converted to WQX
#'   target units. This function changes the
#'   "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode" to the WQX target
#'   units and converts respective values within the
#'   "TADA.DetectionQuantitationLimitMeasure.MeasureValue" field.
#'
#' When transform = FALSE, result values and units are NOT converted to WQX target units,
#' but columns are appended to indicate what the target units and conversion factors are,
#' and if the data can be converted. This function adds the following four fields ONLY
#' when transform = FALSE: "TADA.WQXUnitConversionFactor", "TADA.WQXTargetUnit",
#' "TADA.SpeciationUnitConversion", and "TADA.WQXResultUnitConversion.
#'
#' "TADA.WQXResultUnitConversion" indicates if data can be converted."NoResultValue" means
#' data cannot be converted because there is no ResultMeasureValue, and "NoTargetUnit"
#' means data cannot be converted because the original unit is not associated with
#' a target unit in WQX. "Convert" means the data can be transformed.
#'
#' When detlimit = FALSE, values and units for detection limit are not converted to WQX
#' target units and no additional fields related to detection limit values or units
#'  are added to the input dataframe.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' ResultUnitsConverted <- (Data_Nutrients_UT)
#'
#' # Do not convert result values and units, but add three new columns titled
#' # "TADA.WQXUnitConversionFactor", "TADA.WQXTargetUnit", and "TADA.SpeciationUnitConversion":
#' ResultUnitsNotConverted <- TADA_ConvertResultUnits(Data_Nutrients_UT, transform = FALSE, detlimit = FALSE)
#'
#' #' # Convert values and units for results and detection limits:
#' ResultUnitsNotConverted <- TADA_ConvertResultUnits(Data_Nutrients_UT, transform = TRUE, detlimit = TRUE)
#'
TADA_ConvertResultUnits <- function(.data, transform = TRUE, detlimit = TRUE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check transform is boolean
  TADA_CheckType(transform, "logical")
  # check .data has all of the required columns

  expected_cols <- c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
    "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )

  TADA_CheckColumns(.data, expected_cols)

  # execute function after checks are passed

  # filter WQXcharValRef to include only valid CharacteristicUnit in water media
  unit.ref <- utils::read.csv(system.file("extdata", "WQXunitRef.csv", package = "TADA"))

  # add usgs unit/speciations - this table was created by Elise Hinman and Cristina Mullin in 07/2023 using the pcodes domain table from NWIS and copying units with speciations in them into the same format as the measure unit domain table for WQX.
  # https://help.waterdata.usgs.gov/codes-and-parameters/parameters Downloaded the .txt file of ALL parameters and then open in Excel using the delimiter utility.
  usgs.ref <- TADA_GetUSGSSynonymRef()

  unit.ref <- plyr::rbind.fill(unit.ref, usgs.ref)

  unit.ref$TADA.ResultMeasure.MeasureUnitCode <- toupper(unit.ref$Code)
  unit.ref$Target.Unit <- toupper(unit.ref$Target.Unit)
  unit.ref$Target.Speciation <- toupper(unit.ref$Target.Speciation)

  unit.ref <- unique(unit.ref[, names(unit.ref) %in% c(
    "TADA.ResultMeasure.MeasureUnitCode",
    "Target.Unit",
    "Conversion.Factor",
    "Target.Speciation"
  )])

  # join unit.ref to .data
  check.data <- merge(.data, unit.ref, all.x = TRUE)
  
  # rename columns
  flag.data <- check.data %>%
    dplyr::rename(TADA.WQXTargetUnit = Target.Unit) %>%
    dplyr::rename(TADA.WQXUnitConversionFactor = Conversion.Factor) %>%
    dplyr::rename(TADA.SpeciationUnitConversion = Target.Speciation)

  # if temp data exists, calculate conversion factor
  # EDH I THINK THIS RUNS IF THERE IS ONE OR MORE NA'S IN THE DATASET
  if (all(is.na(match(
    c("deg F", "deg K"),
    flag.data$TADA.ResultMeasure.MeasureUnitCode
  ))) == FALSE) {
    # Calculate deg F and deg C, replace Conversion factor values
    flag.data <- flag.data %>%
      # create flag column
      dplyr::mutate(TADA.WQXUnitConversionFactor = dplyr::case_when(
        TADA.ResultMeasure.MeasureUnitCode == "deg F" ~
          as.numeric(((TADA.ResultMeasureValue - 32) * (5 / 9)) / TADA.ResultMeasureValue),
        TADA.ResultMeasure.MeasureUnitCode == "deg K" ~
          as.numeric((TADA.ResultMeasureValue - 273.15) / TADA.ResultMeasureValue),
        TRUE ~ TADA.WQXUnitConversionFactor
      ))
  }

  # add TADA.WQXResultUnitConversion column
  flag.data <- flag.data %>%
    # create flag column
    dplyr::mutate(TADA.WQXResultUnitConversion = dplyr::case_when(
        (!is.na(TADA.ResultMeasureValue) & !is.na(TADA.WQXTargetUnit)) ~ as.character("Convert"),
        is.na(TADA.ResultMeasureValue) ~ as.character("No Result Value"),
        is.na(TADA.WQXTargetUnit) ~ as.character("No Target Unit")
    ))

  if (transform == FALSE) {
    print("Conversions required for range checks and TADATargetUnit conversions -- Unit conversions, data summaries, and data calculations may be affected.")
    # reorder columns
    clean.data <- TADA_OrderCols(flag.data)
  }

  if (transform == TRUE) {
    # Transform result measure value to Target Unit only if target unit exists
    clean.data <- flag.data %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
        is.na(TADA.ResultMeasureValue) ~ TADA.ResultMeasureValue,
        !is.na(TADA.WQXTargetUnit) ~
          (TADA.ResultMeasureValue * TADA.WQXUnitConversionFactor),
        is.na(TADA.WQXTargetUnit) ~ TADA.ResultMeasureValue
      ))

    # populate ResultMeasure.MeasureUnitCode
    clean.data <- clean.data %>%
      # use target unit where there is a target unit, use original unit if no target unit
      dplyr::mutate(TADA.ResultMeasure.MeasureUnitCode = dplyr::case_when(
        !is.na(TADA.WQXTargetUnit) ~ TADA.WQXTargetUnit,
        is.na(TADA.WQXTargetUnit) ~ TADA.ResultMeasure.MeasureUnitCode
      ))

    # Convert method speciation column for USGS data
    check <- subset(flag.data, !is.na(flag.data$TADA.SpeciationUnitConversion) & !is.na(flag.data$TADA.MethodSpeciationName))
    if (dim(check)[1] > 0) {
      print(paste0("NOTE: Dataset contains ", dim(check)[1], " USGS results with speciation information in both the result unit and method speciation columns. This function overwrites the TADA method speciation column with the speciation provided in the result unit column."))
    }

    clean.data$TADA.MethodSpeciationName <- ifelse(!is.na(clean.data$TADA.SpeciationUnitConversion), clean.data$TADA.SpeciationUnitConversion, clean.data$TADA.MethodSpeciationName)

    # remove extraneous columns, fix field names
    clean.data <- clean.data %>%
      dplyr::select(-c("TADA.WQXUnitConversionFactor", "TADA.WQXTargetUnit", "TADA.SpeciationUnitConversion"))

    # create new comparable data identifier column following conversion
    clean.data <- TADA_CreateComparableID(clean.data)

    clean.data <- TADA_OrderCols(clean.data)
  }

  # reorder cols
  if (detlimit == FALSE) {
    return(clean.data)
  }

  if (detlimit == TRUE) {
    det.ref <- unit.ref %>%
      dplyr::select(TADA.ResultMeasure.MeasureUnitCode, Target.Unit, Conversion.Factor) %>%
      dplyr::rename(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = TADA.ResultMeasure.MeasureUnitCode)

    # if temp data exists, calculate conversion factor for TADA.DetectionQuantitationLimitMeasure.MeasureValue
    # EDH I THINK THIS RUNS IF THERE IS ONE OR MORE NA'S IN THE DATASET
    if (all(is.na(match(
      c("deg F", "deg K"),
      clean.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
    ))) == FALSE) {
      # Calculate deg F and deg C, replace Conversion factor values
      flag.det <- det.data %>%
        # create flag column
        dplyr::mutate(Conversion.Factor = dplyr::case_when(
          TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode == "deg F" ~
            as.numeric(
              ((TADA.DetectionQuantitationLimitMeasure.MeasureValue - 32) * (5 / 9)) / TADA.DetectionQuantitationLimitMeasure.MeasureValue,
              TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode == "deg K" ~
                as.numeric((TADA.DetectionQuantitationLimitMeasure.MeasureValue - 273.15) / TADA.DetectionQuantitationLimitMeasure.MeasureValue),
              TRUE ~ Conversion.Factor
            )
        ))
    }


    # Transform TADA.DetectionQuantitationLimitMeasure.MeasureValue value to Target Unit only if target unit exists
    det.data <- clean.data %>%
      merge(det.ref, all.x = TRUE) %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(TADA.DetectionQuantitationLimitMeasure.MeasureValue = dplyr::case_when(
        is.na(TADA.DetectionQuantitationLimitMeasure.MeasureValue) ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue,
        !is.na(Target.Unit) ~
          (TADA.DetectionQuantitationLimitMeasure.MeasureValue * Conversion.Factor),
        is.na(Target.Unit) ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue
      ))

    # populate TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
    convert.data <- det.data %>%
      # use target unit where there is a target unit, use original unit if no target unit
      dplyr::mutate(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = dplyr::case_when(
        !is.na(Target.Unit) ~ Target.Unit,
        is.na(Target.Unit) ~ TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
      )) %>%
      dplyr::select(-Target.Unit, -Conversion.Factor) %>%
      TADA_OrderCols()

    return(convert.data)
  }
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
#' flag indicating which rows have been converted. New TADA versions of all
#' depth columns are always created. Default is transform = TRUE.
#'
#' The depth profile function can harmonize depth units across all the
#' following fields: "ActivityDepthHeightMeasure",
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
#  Removed 9/1 buggy as is, not necessary?, CM
#  @param fields Character string input indicating the relevant depth data fields
#  that will be converted to the desired target unit in new TADA column. Allowable
#  values for 'fields' are "ActivityDepthHeightMeasure", "ActivityTopDepthHeightMeasure",
#  "ActivityBottomDepthHeightMeasure", and "ResultDepthHeightMeasure". The default
#  is to include all depth fields.
#' @param transform Boolean argument; The default is transform = TRUE.
#' @return When transform = TRUE, the input dataframe is returned with all depth
#' data converted to the target unit in new TADA-specific columns.
#' When transform = FALSE, the input dataframe is returned with additional
#' columns showing how the data would be handled if converted. A user can review
#' the conversion factor information if desired by using this feature.
#' No conversions are made, but new TADA-specific columns are added as copies
#' of original columns, with unit synonyms addressed (e.g. "meters" is converted
#' automatically to "m" for national consistency).
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Convert all depth units to meters and review unit harmonization:
#' # "ActivityDepthHeightMeasure.MeasureUnitCode" and "ActivityDepthHeightMeasure.MeasureValue"
#' # are harmonized to "TADA.ActivityDepthHeightMeasure.MeasureUnitCode" and "TADA.ActivityDepthHeightMeasure.MeasureValue"
#' DepthUnitsConverted_m <- TADA_ConvertDepthUnits(Data_Nutrients_UT)
#'
#' # Convert all depth units to feet:
#' DepthUnitsConverted_ft <- TADA_ConvertDepthUnits(Data_Nutrients_UT, unit = "ft")
#'
#' # Do not convert any depth units, but address depth unit synonyms, add TADA depth columns (copies),
#' # Add columns with target units and conversion factors for each depth measure unit:
#' DepthUnitsNotConverted <- TADA_ConvertDepthUnits(Data_Nutrients_UT, transform = FALSE)
#' # Compare columns before and after transformation to see example transformation from "meters" to "m"
#' unique(DepthUnitsNotConverted$TADA.ActivityDepthHeightMeasure.MeasureUnitCode)
#' unique(DepthUnitsNotConverted$ActivityDepthHeightMeasure.MeasureUnitCode)
#'
TADA_ConvertDepthUnits <- function(.data,
                                   unit = "m",
                                   # fields = c(
                                   #  "ActivityDepthHeightMeasure",
                                   #   "ActivityTopDepthHeightMeasure",
                                   #  "ActivityBottomDepthHeightMeasure",
                                   #   "ResultDepthHeightMeasure"
                                   # ),
                                   transform = TRUE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check unit is character
  TADA_CheckType(unit, "character")
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
  valid_fields <- c(
    "ActivityDepthHeightMeasure",
    "ActivityTopDepthHeightMeasure",
    "ActivityBottomDepthHeightMeasure",
    "ResultDepthHeightMeasure"
  )

  # removed fields input
  # if (all(is.na(match(valid_fields, fields))) == TRUE) {
  #  stop("Invalid 'fields' argument. 'fields' must include one or many of the
  #  following: 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
  #  'ActivityBottomDepthHeightMeasure,' and/or 'ResultDepthHeightMeasure.'")
  # }

  # check transform is boolean
  TADA_CheckType(transform, "logical")

  # .data required columns
  expected_cols <- c(
    "ActivityDepthHeightMeasure.MeasureValue",
    "ActivityDepthHeightMeasure.MeasureUnitCode",
    "ActivityTopDepthHeightMeasure.MeasureValue",
    "ActivityTopDepthHeightMeasure.MeasureUnitCode",
    "ActivityBottomDepthHeightMeasure.MeasureValue",
    "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
    "ResultDepthHeightMeasure.MeasureValue",
    "ResultDepthHeightMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)

  tadacols <- c(
    "TADA.ActivityDepthHeightMeasure.MeasureValue",
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
    "TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag"
  )

  # Remove TADA cols if already run on a single dataset - this might occur if someone chooses initially one unit but changes mind and wants other unit.
  .data <- .data[, !names(.data) %in% tadacols]

  # execute function after checks are passed
  # define check.data (to preserve .data and avoid mistakes with if statements below)
  check.data <- .data

  # conversion column names
  appCols <- c(
    "TADA.WQXConversionFactor.ActivityDepthHeightMeasure",
    "TADA.WQXConversionFactor.ActivityTopDepthHeightMeasure",
    "TADA.WQXConversionFactor.ActivityBottomDepthHeightMeasure",
    "TADA.WQXConversionFactor.ResultDepthHeightMeasure"
  )

  # read in unit conversion reference table from extdata
  unit.ref <- utils::read.csv(system.file("extdata", "WQXunitRef.csv", package = "TADA"))

  # subset to include only "Length Distance" units; filter by target unit defined in 'unit' argument
  unit.ref <- unit.ref %>%
    dplyr::filter(stringr::str_detect(
      Description,
      stringr::regex("\\bLength Distance")
    )) %>%
    dplyr::filter(Target.Unit == unit) # %>%

  # Loop over all supplied depth columns, create TADA columns, then join conversion table
  for (i in 1:length(valid_fields)) {
    field <- valid_fields[i]
    # if ((field %in% fields) == TRUE) {
    # OG unit column
    unitCol <- paste(field, ".MeasureUnitCode", sep = "")
    valCol <- paste0(field, ".MeasureValue")
    # proceed only if unitCol has values other than NA
    # requirement to proceed only if not all NA removed on 9/1/23, so that TADA depth cols are ALWAYS created
    # this avoid downstream conflicts with figure and analysis functions that req TADA depth columns
    # if (sum(!is.na(check.data[unitCol])) > 0) {
    # new TADA column
    unitCol2 <- paste0("TADA.", unitCol)

    # deal with any units that are "meters" and change to "m" (USGS convention)
    check.data$new <- check.data[, unitCol]
    check.data$new[check.data$new == "meters"] <- "m"
    names(check.data)[names(check.data) == "new"] <- unitCol2

    # Join conversion factor from unit.ref to .data by unitCol
    check.data <- merge(check.data, unit.ref[, c("Code", "Conversion.Factor")],
      by.x = unitCol2,
      by.y = "Code",
      all.x = TRUE,
      sort = FALSE
    )

    # rename new columns
    names(check.data)[names(check.data) == "Conversion.Factor"] <- paste("TADA.WQXConversionFactor.", field, sep = "")
    check.data <- TADA_ConvertSpecialChars(check.data, valCol)
    # }
    # }
  }

  # function should always run all code above

  # check if any Conversion Factor columns were appended. CM removed 9/1. Buggy now, not needed?
  # if (all(is.na(match(appCols, colnames(check.data)))) == TRUE) {
  #  print("Note: the dataframe does not have any depth data in ActivityTop/BottomDepthHeight or ResultDepthHeight columns.")
  #  check.data <- TADA_OrderCols(check.data)
  #  return(check.data)
  # }

  # if transform = FALSE, output data
  if (transform == FALSE) {
    # add WQX.Depth.TargetUnit column
    check.data[, "WQX.Depth.TargetUnit"] <- unit
    # reorder cols
    check.data <- TADA_OrderCols(check.data)
    return(check.data)
  }

  # if transform = TRUE, apply conversions and remove extra columns
  if (transform == TRUE) {
    # define clean.data
    clean.data <- check.data

    # function to run through each depth column
    conv_unit <- function(.data, coln) {
      if (coln %in% colnames(.data)) {
        .data$cf <- .data[, coln]
        colnv <- paste0(gsub("TADA.WQXConversionFactor", "TADA", coln), ".MeasureValue")
        .data$val <- .data[, colnv]
        colnu <- paste0(gsub("TADA.WQXConversionFactor", "TADA", coln), ".MeasureUnitCode")
        .data$unit <- .data[, colnu]

        # multiply .MeasureValue by TADA.WQXConversionFactor.
        # if else added to deal with NA's in RV column, which throws error when NA multiplied by number.
        .data$val <- ifelse(!is.na(.data$val), .data$val * .data$cf, .data$val)

        # then replace unit values with the new unit argument
        .data$unit[which(
          !is.na(.data$unit)
        )] <- unit

        # replace TADA depth height columns and remove WQX conversion column
        .data <- dplyr::select(.data, -cf, -dplyr::all_of(coln), -dplyr::all_of(colnv), -dplyr::all_of(colnu))
        names(.data)[names(.data) == "val"] <- colnv
        names(.data)[names(.data) == "unit"] <- colnu

        return(.data)
      } else {
        return(.data)
      }
    }

    clean.data <- conv_unit(clean.data, "TADA.WQXConversionFactor.ActivityDepthHeightMeasure")
    clean.data <- conv_unit(clean.data, "TADA.WQXConversionFactor.ActivityBottomDepthHeightMeasure")
    clean.data <- conv_unit(clean.data, "TADA.WQXConversionFactor.ResultDepthHeightMeasure")
    clean.data <- conv_unit(clean.data, "TADA.WQXConversionFactor.ActivityTopDepthHeightMeasure")

    # order columns
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }
}
