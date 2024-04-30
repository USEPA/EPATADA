#' Generate A Data Frame of Units by Characteristic Name
#' 
#' This function generates a data frame listing all unique characteristic (by CharacteristicName)
#' and unit (by ResultMeasure.MeasureUnitCode) pairs present in the dataset. Where
#' possible, the function fills in the columns for target unit, and conversion factor.
#' Users can edit it and use it as an input for TADA_ConvertResultUnits to 
#' customize standardization of units by characteristic.
#' 
#' @param .data TADA dataframe
#
#' @return A dataframe with seven columns: CharacteristicName, 
#' Code, Target.Unit, ConversionFactor, and 
#' ConversionCoefficient.The number of rows will vary based on the number of
#' unique CharacteristicName/ResultMeasure.MeasureUnitCode combinations in the
#' initial TADA dataframe.
#' @export
#' 
#' @examples
#' 
#' #' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Create a unit reference data frame
#' UT_UnitRef <- TADA_CreateUnitRef(Data_Nutrients_UT)
#' 
TADA_CreateUnitRef <- function(.data){
  
  # .data required columns
  required_cols <- c(
    "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.ActivityMediaName", "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )
  
  # Check to see if TADA_Autoclean has been run
  if (all(required_cols %in% colnames(.data)) == FALSE) {
    
    print("The dataframe does not contain the required fields to use TADA. Running TADA_AutoClean to create required columns.")
    
    .data <- TADA_AutoClean(.data)
    
    if(all(required_cols %in% colnames(.data)) == TRUE) {
      
      .data <- .data
    }
  }
  
  # Create df of unique CharactersticName and Unit in TADA data frame 
  data.units.result <- .data %>%
    dplyr::select(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode) %>%
    dplyr::distinct() %>%
    dplyr::rename("Code" = "TADA.ResultMeasure.MeasureUnitCode",
                  "CharacteristicName" = "TADA.CharacteristicName")
  
  data.units.det <- .data %>%
    dplyr::select(TADA.CharacteristicName, TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode) %>%
    dplyr::distinct() %>%
    dplyr::rename("Code" = "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode",
                  "CharacteristicName" = "TADA.CharacteristicName")
  
  data.units <- data.units.result %>%
    dplyr::full_join(data.units.det, by = c("CharacteristicName", "Code")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(CharacteristicName) %>%
    dplyr::mutate(NCode = length(unique(Code))) %>%
    dplyr::filter(!is.na(Code) |
                    is.na(Code) & NCode == 1) %>%
    dplyr::select(-NCode)
  
  rm(data.units.result, data.units.det)
  
 # Import USGS default unit ref
 usgs.ref <- TADA_GetUSGSSynonymRef()
 usgs.ref$Target.Unit <- toupper(usgs.ref$Target.Unit)
 usgs.ref$Code <- toupper(usgs.ref$Code)
 
 # Import WQX default unit ref
 wqx.ref <- TADA_GetMeasureUnitRef()
 wqx.ref$Target.Unit <- toupper(wqx.ref$Target.Unit)
 wqx.ref$Code <- toupper(wqx.ref$Code)
 
 # Combine for USGS and WQX unit ref
 unit.ref <- usgs.ref %>%
   dplyr::full_join(wqx.ref, by = c("Domain", "Unique.Identifier", "Code",
                                    "Description", "Last.Change.Date", 
                                    "Target.Unit", "Conversion.Factor")) %>%
   dplyr::mutate(Conversion.Coefficient = ifelse(is.na(Conversion.Coefficient), 0, Conversion.Coefficient))
 
 # Remove intermediate objects
 rm(usgs.ref, wqx.ref)
 
 # Import TADA unit reference for priority characteristics (characteristic specific)
 tada.char.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "TADA")) 
 # Make all target units and characteristic names uppercase
 tada.char.ref$Target.Unit <- toupper(tada.char.ref$Target.Unit)
 tada.char.ref$CharacteristicName <- toupper(tada.char.ref$CharacteristicName)
 
 # Import TADA specific conversion reference, created by HRM on 4/30/2024
  tada.unit.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharConvertRef.csv", package = "TADA"))
  # Make all units and target units uppercase
  tada.unit.ref$Target.Unit <- toupper(tada.unit.ref$Target.Unit)
  tada.unit.ref$Code <- toupper(tada.unit.ref$Code)
  
 # Add all possible TADA specific target units
  tada.targets <- data.units %>%
    dplyr::left_join(tada.char.ref, by = "CharacteristicName") %>%
    dplyr::filter(!is.na(Target.Unit)) %>%
    dplyr::left_join(tada.unit.ref,  by = c("Code", "Target.Unit"),
                                            relationship = "many-to-many") %>%
    dplyr::mutate(CharUnit = paste(CharacteristicName, "_", Code, sep = "")) %>%
    dplyr::filter(!is.na(Conversion.Factor)) %>%
    dplyr::select("CharacteristicName", "Code",
                  "Target.Unit", "Last.Change.Date",
                  "Conversion.Factor", "Conversion.Coefficient",
                  "CharUnit")
  
  tada.wqx <- data.units %>%
    dplyr::left_join(tada.char.ref, by = "CharacteristicName") %>%
    dplyr::mutate(CharUnit = paste(CharacteristicName, "_", Code, sep = "")) %>%
    dplyr::filter(!CharUnit %in% tada.targets$CharUnit &
                  !is.na(Target.Unit)) %>%
    dplyr::left_join(unit.ref, by = c("Code", "Target.Unit"),
                                      relationship = "many-to-many") %>%
    dplyr::select("CharacteristicName", "Code",
                  "Target.Unit", "Last.Change.Date",
                  "Conversion.Factor", "Conversion.Coefficient",
                  "Target.Speciation", "CharUnit")
    
    
  
  # Remove intermediate objects
  rm(tada.char.ref, tada.unit.ref)
  
  # Assign all other target units
  other.targets <- data.units %>%
    dplyr::mutate(CharUnit = paste(CharacteristicName, "_", Code, sep = "")) %>%
    dplyr::filter(!CharUnit %in% tada.targets$CharUnit &
                  !CharUnit %in% tada.wqx$CharUnit) %>%
    dplyr::left_join(unit.ref, by = "Code", relationship = "many-to-many") %>%
    dplyr::select("CharacteristicName", "Code",
                  "Target.Unit", "Last.Change.Date",
                  "Conversion.Factor", "Conversion.Coefficient",
                  "Target.Speciation", "CharUnit")
  
  # Join priority units and other units to create comprehensive unit ref table
  comb.convert <- tada.targets %>%
    dplyr::full_join(tada.wqx, by = c("CharacteristicName", "Code",
                            "Target.Unit", "Last.Change.Date",
                            "Conversion.Factor", "Conversion.Coefficient",
                            "CharUnit")) %>%
    dplyr::full_join(other.targets, by = c("CharacteristicName", "Code",
                                           "Target.Unit", "Last.Change.Date",
                                           "Conversion.Factor", "Conversion.Coefficient",
                                           "Target.Speciation", "CharUnit")) %>%
    dplyr::group_by(CharacteristicName) %>%
    dplyr::mutate(NConvert = length(unique(Target.Unit))) %>%
    dplyr::ungroup()
  
  # Remove intermediate objects
  rm(other.targets, tada.targets, tada.wqx, data.units)
  
  # Identify characteristics with multiple target units identified
  mult.target.chars <- comb.convert %>%
    dplyr::filter(NConvert > 1)
  
  if(nrow(mult.target.chars) > 0){
    
    mult.target.list <- mult.target.chars %>%
      dplyr::select(CharacteristicName, Target.Unit) %>%
      dplyr::group_by(CharacteristicName) %>%
      dplyr::mutate(MultUnits = paste(Target.Unit, collapse = ", "),
                    MultUnits = stringi::stri_replace_last(MultUnits,replacement = " and ", fixed = ", "),
                    MultUnits = paste(CharacteristicName, " (", MultUnits, ")", sep = "")) %>%
      dplyr::ungroup() %>%
      dplyr::select(-Target.Unit, -CharacteristicName) %>%
      dplyr::distinct() %>%
      dplyr::mutate(CharList = paste(MultUnits, collapse = "; ")) %>%
      dplyr::select(CharList) %>%
      dplyr::distinct() %>%
      stringi::stri_replace_last(replacement = " and ", fixed = "; ")
    
    print(paste("TADA.CreateUnitRef: The following characteristics have more than one listed target unit: ", mult.target.list, ". This may be due to units of different types that cannot be converted to match each other. You may wish to review the output of TADA.CreateUnitRef and edit it.", sep = ""))
  
    comb.convert <- comb.convert %>%
      dplyr::select(-NConvert, -CharUnit, -Last.Change.Date)
    }
  
   # Return reference table for use in unit conversion functions or for more editing by user
   return(comb.convert)
}

#' Transform Units to TADA Target Units, WQX Target Units or User Specified Units
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
#' @param ref Optional character argument in which a user can specify a data frame
#' by name. Data frame must contain the columns CharacteristicName, Unit, and TargetUnit.
#' TADA_CreateUnitRef() can be used to help create this data frame. There are two
#' options that do not require the user to supply a data frame, "tada" and "wqx". 
#' When ref = "wqx" all unit conversion will be based on WQX unit reference.
#' When ref = "tada" all unit conversion will be based on TADA priority characteristic 
#' units (where appplicable), with any other units assigned by WQX unit reference.
#' The default is ref = "tada". 
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
#'  When
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Do not convert result values and units, but add four new columns titled
#' # "TADA.WQXUnitConversionFactor", "TADA.WQXUnitConversionCoefficient", "TADA.WQXTargetUnit", and "TADA.SpeciationUnitConversion":
#' ResultUnitsNotConverted <- TADA_ConvertResultUnits(Data_Nutrients_UT, transform = FALSE, detlimit = FALSE)
#'
#' #' # Convert values and units for results and detection limits:
#' ResultUnitsConverted <- TADA_ConvertResultUnits(Data_Nutrients_UT, transform = TRUE, detlimit = TRUE)
#'
TADA_ConvertResultUnits <- function(.data, ref = "tada", transform = TRUE, detlimit = TRUE) {
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
  
  # if user supplied unit reference was provided
  if (is.data.frame(ref)) {
    
    # required columns
    expected_ref_cols <- c(
      "CharacteristicName", "Code", "Target.Unit",
      "Conversion.Factor", "Conversion.Coefficient", "Target.Speciation"
    )
    
    # check ref has all of the required columns
    TADA_CheckColumns(ref, expected_ref_cols)
      
    # create message to inform users if user-supplied unit reference contains all combinations present in TADA data frame
      unit.ref <- ref
      
      # create list of unique characteristic and unit combinations in data 
      check.units <- .data %>%
        dplyr::select(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode) %>%
        dplyr::distinct() %>%
        dplyr::rename(Code = TADA.ResultMeasure.MeasureUnitCode,
                      CharacteristicName = TADA.CharacteristicName) %>%
        dplyr::mutate(Code = toupper(Code)) %>%
        dplyr::group_by(CharacteristicName) %>%
        dplyr::mutate(NConvert = length(Code)) %>%
        dplyr::filter(NConvert == 1 |
                        (NConvert > 1 & is.na(Code))) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(CharacteristicName, Code) %>%
        dplyr::slice_head() %>%
        dplyr::select(-NConvert)
      
      # create list of unique characteristic and unit combinations in unit ref
      check.ref <- unit.ref %>%
        dplyr::select(CharacteristicName, Code) %>%
        dplyr::distinct()
      
      # compare the unique characteristic/unit combinations in data nd unit ref
      compare.ref <- check.units %>%
        dplyr::anti_join(check.ref)

      
      # if no difference between the two, print message that all combinations are present in unit ref
      if(nrow(compare.ref) == 0){
        print("All CharacteristicName/Unit combinations in the TADA dataframe are represented in user-supplied unit reference.")
        # if there are characteristic/unit combinations in the data that are not in the unit ref, print a warning message listing them
        } else{
          compare.list <- compare.ref %>%
            dplyr::mutate(Comb = paste(CharacteristicName, " (", Code, ")", sep = "")) %>%
            dplyr::mutate(CombList = paste(Comb, collapse = ", ")) %>%
            dplyr::select(CombList) %>%
            dplyr::distinct() %>%
            stringi::stri_replace_last(fixed = ",", " and")
          
          print(paste("TADA_ConvertResultUnits: The following CharacteristicName and ResultMeasure.MeasureUnitCode combinations are not included in the user-supplied unit reference data frame: ", 
                      compare.list, 
                      ". Consider revising the user-supplied unit reference data frame and running TADA_ConvertResultUnits again.", sep = ""))
    }
  }
  # if no unit reference df was provided by user or user input was "tada"
  if (!is.data.frame(ref)) {
    if (ref == "tada") {
    unit.ref <- TADA_CreateUnitRef(.data)
    
    print("TADA_ConvertResultUnits: No unit reference data frame was supplied. Characteristic units will be converted to TADA-specified units for priority characteristics and WQX target units for other characteristics.")
  }
  
  if (ref == "wqx") {
    
    # Import USGS unit ref
    usgs.ref <- TADA_GetUSGSSynonymRef()
    usgs.ref$Target.Unit <- toupper(usgs.ref$Target.Unit)
    usgs.ref$Code <- toupper(usgs.ref$Code)
    
    # Import WQX unit ref
    wqx.ref <- TADA_GetMeasureUnitRef()
    wqx.ref$Target.Unit <- toupper(wqx.ref$Target.Unit)
    wqx.ref$Code <- toupper(wqx.ref$Code)
    
    # Combine for USGS and WQX unit ref
    unit.ref <- usgs.ref %>%
      dplyr::full_join(wqx.ref, by = c("Domain", "Unique.Identifier", "Code",
                                       "Description", "Last.Change.Date", 
                                       "Target.Unit", "Conversion.Factor",
                                       "Conversion.Coefficient"))
    
    print("TADA_ConvertResultUnits: No unit reference data frame was supplied. Characteristic units will be converted to WQX target units.")
    }
  }
  
  # rename unit.ref columns
  unit.ref <- unit.ref %>%
    dplyr::rename(TADA.CharacteristicName = CharacteristicName,
                  TADA.ResultMeasure.MeasureUnitCode = Code,
                  TADA.SpeciationUnitConversion = Target.Speciation)
  
  # join unit.ref to .data
  check.data <- .data %>%
    dplyr::left_join(unit.ref, by = c("TADA.CharacteristicName",
                                      "TADA.ResultMeasure.MeasureUnitCode"),
                     relationship = "many-to-many")

  # rename columns
  flag.data <- check.data %>%
    dplyr::rename(TADA.WQXTargetUnit = Target.Unit,
                  TADA.WQXUnitConversionFactor = Conversion.Factor,
                  TADA.WQXUnitConversionCoefficient = Conversion.Coefficient)


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
                      !is.na(TADA.WQXTargetUnit) ~ ((TADA.ResultMeasureValue + TADA.WQXUnitConversionCoefficient) * TADA.WQXUnitConversionFactor),
        is.na(TADA.WQXTargetUnit) ~ TADA.ResultMeasureValue
      ))
    
    # Format TADA.ResultMeasureValue
    #clean.data$TADA.ResultMeasureValue <- format(clean.data$TADA.ResultMeasureValue, scientific = FALSE)

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
      dplyr::select(-c("TADA.WQXUnitConversionFactor", "TADA.WQXTargetUnit", 
                       "TADA.SpeciationUnitConversion", "TADA.WQXUnitConversionCoefficient"))

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
      dplyr::rename(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = TADA.ResultMeasure.MeasureUnitCode,
                    TADA.WQXUnitConversionCoefficient = Conversion.Coefficient)

    # Transform TADA.DetectionQuantitationLimitMeasure.MeasureValue value to Target Unit only if target unit exists
    det.data <- clean.data %>%
      merge(det.ref, all.x = TRUE) %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(TADA.DetectionQuantitationLimitMeasure.MeasureValue = dplyr::case_when(
                    is.na(TADA.DetectionQuantitationLimitMeasure.MeasureValue) ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue,
                    !is.na(Target.Unit) ~ ((TADA.DetectionQuantitationLimitMeasure.MeasureValue - TADA.WQXUnitConversionCoefficient) * Conversion.Factor),
                    is.na(Target.Unit) ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue))
    
    # Format TADA.DetectionQuantitationLimitMeasure.MeasureValue
    #det.data$TADA.ResultMeasureValue <- format(clean.data$TADA.ResultMeasureValue, scientific = FALSE)

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
