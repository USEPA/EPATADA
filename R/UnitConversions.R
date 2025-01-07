#' Generate A Data Frame of Units and Target by Characteristic Name
#'
#' This function generates a data frame listing all unique characteristic (by CharacteristicName)
#' and unit (by TADA.ResultMeasure.MeasureUnitCode) pairs present in the dataset. Where
#' possible, the function fills in the columns for target unit, and conversion factor.
#' Users can edit it and use it as an input for TADA_ConvertResultUnits to
#' customize standardization of units by characteristic. The TADA.ResultMeasure.MeasureUnitCode
#' column automatically incorporates any additional unique unit codes from
#' TADA.DetectionQuantitationLimiMeasure.MeasureUnitCode that were not observed in
#' TADA.ResultMeasure.MeasureUnitCode. This is done to facilitate estimating
#' censored data later in the workflow. All variants of TADA.ResultMeasure.MeasureUnitCode
#' and ResultMeasure.MeasureUnitCode, including USGS results where speciation is listed
#' in the units are included. This facilitates moving speciation from units to
#' TADA.MethodSpeciationName in TADA_ConvertResultUnits.
#'
#' The columns created by TADA_AutoClean are required to run this function. If
#' they are not present in the data frame, TADA_AutoClean is automatically run
#' before the unit reference data frame is created.
#'
#' @param .data TADA dataframe
#'
#' @param print.message Boolean argument with two possible arguments, TRUE and
#' FALSE. When print.message = TRUE, a message is printed that lists any
#' characteristics (TADA.CharacteristicName) that have been assigned more than
#' one target unit. When print.message = FALSE, no message is printed. The default
#' is print.message = TRUE.
#
#' @return A dataframe with seven columns: TADA.CharacteristicName,
#' TADA.ResultMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode,
#' TADA.Target.ResultMeasureUnit, TADA.MethodSpeciationName,
#'  ConversionFactor, and ConversionCoefficient.
#' The number of rows will vary based on the number of unique
#' TADA.CharacteristicName/ResultMeasure.MeasureUnitCode combinations in the
#' initial TADA dataframe.
#' @export
#'
#' @examples
#'
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Create a unit reference data frame
#' UT_UnitRef <- TADA_CreateUnitRef(Data_Nutrients_UT)
#'
TADA_CreateUnitRef <- function(.data, print.message = TRUE) {
  # create data frame of unique combinations
  data.units <- TADA_UniqueCharUnitSpeciation(.data)

  # remove method speciation from data.units
  data.units <- data.units %>%
    dplyr::select(-TADA.MethodSpeciationName)

  # import USGS default unit ref
  usgs.ref <- TADA_GetUSGSSynonymRef()
  # make Target.Unit and Code uppercase
  usgs.ref$Target.Unit <- toupper(usgs.ref$Target.Unit)

  # create ref for unit for usgs results
  usgs.unit <- usgs.ref %>%
    dplyr::select(Code, CodeNoSpeciation) %>%
    dplyr::mutate(
      TADA.ResultMeasure.MeasureUnitCode = toupper(Code),
      CodeNoSpeciation = toupper(CodeNoSpeciation)
    ) %>%
    dplyr::select(-Code)

  # add unit for usgs results to data units df
  data.units <- data.units %>%
    dplyr::left_join(usgs.unit, by = "TADA.ResultMeasure.MeasureUnitCode") %>%
    dplyr::mutate(
      MeasureUnitCode.match = ifelse(!is.na(CodeNoSpeciation),
        CodeNoSpeciation,
        TADA.ResultMeasure.MeasureUnitCode
      )
    ) %>%
    dplyr::select(-CodeNoSpeciation)

  # remove intermediate objects
  rm(usgs.ref, usgs.unit)

  # import WQX default unit ref
  wqx.ref <- TADA_GetMeasureUnitRef()

  # use WQX unit ref to create unit ref
  wqx.ref <- wqx.ref %>%
    dplyr::select(Code, Target.Unit, Conversion.Factor, Conversion.Coefficient) %>%
    dplyr::mutate(
      Target.Unit = toupper(Target.Unit),
      Conversion.Coefficient = ifelse(is.na(Conversion.Coefficient), 0, Conversion.Coefficient),
      MeasureUnitCode.match = toupper(Code)
    ) %>%
    dplyr::rename(TADA.Target.ResultMeasure.MeasureUnitCode = Target.Unit) %>%
    dplyr::select(
      MeasureUnitCode.match,
      TADA.Target.ResultMeasure.MeasureUnitCode,
      Conversion.Factor, Conversion.Coefficient
    )

  # import TADA unit reference for priority characteristics (characteristic specific)
  tada.char.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))
  # make all target units and characteristic names uppercase
  tada.char.ref$TADA.Target.ResultMeasure.MeasureUnitCode <- toupper(tada.char.ref$TADA.Target.ResultMeasure.MeasureUnitCode)
  tada.char.ref$TADA.CharacteristicName <- toupper(tada.char.ref$TADA.CharacteristicName)

  # import TADA specific conversion reference, created by HRM on 4/30/2024
  tada.unit.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharConvertRef.csv", package = "EPATADA"))
  # make all codes and target units uppercase
  tada.unit.ref <- tada.unit.ref %>%
    dplyr::mutate(
      TADA.Target.ResultMeasure.MeasureUnitCode = toupper(Target.Unit),
      TADA.ResultMeasure.MeasureUnitCode = toupper(tada.unit.ref$Code)
    ) %>%
    dplyr::rename(MeasureUnitCode.match = TADA.ResultMeasure.MeasureUnitCode) %>%
    dplyr::select(-Target.Unit, -Code, -Last.Change.Date)

  # create df of TADA priority targets with TADA specific conversions
  tada.targets <- data.units %>%
    # add TADA priority target units
    dplyr::left_join(tada.char.ref, by = "TADA.CharacteristicName", relationship = "many-to-many") %>%
    # filter out any rows without a TADA priority target unit
    dplyr::filter(!is.na(TADA.Target.ResultMeasure.MeasureUnitCode)) %>%
    # join TADA specific conversion factor/coefficient (not included in unit.ref because they disagree with WQX target units)
    dplyr::left_join(tada.unit.ref,
      by = c("MeasureUnitCode.match", "TADA.Target.ResultMeasure.MeasureUnitCode"),
      relationship = "many-to-many"
    ) %>%
    # create a CharUnit column by concatenating characteristic name and code
    dplyr::mutate(CharUnit = paste(TADA.CharacteristicName, "_", TADA.ResultMeasure.MeasureUnitCode, sep = "")) %>%
    # filter out any results without a conversion factor
    dplyr::filter(!is.na(Conversion.Factor)) %>%
    # select columns needed for final unit ref
    dplyr::select(
      "TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode",
      "TADA.Target.ResultMeasure.MeasureUnitCode", "ResultMeasure.MeasureUnitCode",
      "Conversion.Factor", "Conversion.Coefficient", "CharUnit"
    )

  # remove intermediate objects
  rm(tada.char.ref, tada.unit.ref)

  # create df of TADA priority targets with WQX/USGS conversions
  tada.wqx <- data.units %>%
    # create a CharUnit column by concatenating characteristic name and code
    dplyr::mutate(CharUnit = paste(TADA.CharacteristicName, "_", TADA.ResultMeasure.MeasureUnitCode, sep = "")) %>%
    # filter out any charunits that were addressed in tada.targets
    dplyr::filter(!CharUnit %in% tada.targets$CharUnit) %>%
    # join units from wqx.ref
    dplyr::left_join(wqx.ref,
      by = c("MeasureUnitCode.match"),
      relationship = "many-to-many"
    ) %>%
    # filter out any results without a conversion factor
    dplyr::filter(!is.na(Conversion.Factor)) %>%
    # select columns needed for final unit ref
    dplyr::select(
      "TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode",
      "TADA.Target.ResultMeasure.MeasureUnitCode", "ResultMeasure.MeasureUnitCode",
      "Conversion.Factor", "Conversion.Coefficient", "CharUnit"
    )


  # select columns needed for final unit ref and combine dfs
  tada.all <- tada.targets %>%
    dplyr::full_join(tada.wqx, by = names(tada.targets)) %>%
    dplyr::rename(
      TADA.WQXUnitConversionFactor = Conversion.Factor,
      TADA.WQXUnitConversionCoefficient = Conversion.Coefficient
    ) %>%
    dplyr::select(
      TADA.CharacteristicName, ResultMeasure.MeasureUnitCode,
      TADA.ResultMeasure.MeasureUnitCode, TADA.Target.ResultMeasure.MeasureUnitCode,
      TADA.WQXUnitConversionFactor, TADA.WQXUnitConversionCoefficient, CharUnit
    )

  # remove intermediate objects
  rm(tada.targets, tada.wqx)

  # identify characteristics with multiple target units
  mult.target.chars <- tada.all %>%
    # remove duplicates due to multiple ResultMeasure.MeasureUnitCodes
    dplyr::select(TADA.CharacteristicName, TADA.Target.ResultMeasure.MeasureUnitCode) %>%
    dplyr::distinct() %>%
    dplyr::group_by(TADA.CharacteristicName) %>%
    # count number of target units per characteristic
    dplyr::mutate(NConvert = length(unique(TADA.Target.ResultMeasure.MeasureUnitCode))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(NConvert > 1)

  # if there are characteristics with more than one target unit:
  if (nrow(mult.target.chars) > 1) {
    # create list of characteristics/target units when a characteristic has more than one target unit
    mult.target.list <- mult.target.chars %>%
      dplyr::group_by(TADA.CharacteristicName) %>%
      dplyr::mutate(
        MultUnits = paste(unique(TADA.Target.ResultMeasure.MeasureUnitCode), collapse = ", "),
        MultUnits = stringi::stri_replace_last(MultUnits, replacement = " and ", fixed = ", "),
        MultUnits = paste(TADA.CharacteristicName, " (", MultUnits, ")", sep = "")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-TADA.Target.ResultMeasure.MeasureUnitCode, -TADA.CharacteristicName) %>%
      dplyr::distinct() %>%
      dplyr::mutate(CharList = paste(MultUnits, collapse = "; ")) %>%
      dplyr::select(CharList) %>%
      dplyr::distinct() %>%
      stringi::stri_replace_last(replacement = " and ", fixed = "; ")

    # print message listing characteristics/target units
    if (print.message == TRUE) {
      print(paste("TADA.CreateUnitRef: The following characteristics have more than one listed target unit: ", mult.target.list, ". This may be due to units of different types that cannot be converted to match each other. You may wish to review the output of TADA.CreateUnitRef and edit it.", sep = ""))

      # remove intermediate object
      rm(mult.target.list)
    }
  }

  # remove intermediate object
  rm(mult.target.chars)

  # remove intermediate columns tada.all df
  tada.all <- tada.all %>%
    dplyr::select(-CharUnit) %>%
    dplyr::distinct()

  # return reference table for use in unit conversion functions or for more editing by user
  return(tada.all)
}


#' Transform Units to TADA Target Units, WQX Target Units or User Specified Units
#'
#' This function converts result and detection limit measure values and units in the
#' input dataframe to TADA target units. It also automatically wrangles common
#' USGS units which include speciation in the unit value, such as "mg/l as N"
#' and "mg/l asNO3", by transferring the speciation information to the
#' TADA.MethodSpeciationName field. No speciation conversions occur in this
#' function.
#'
#' The function uses the "TADA.ResultMeasureValue"
#' and TADA.ResultMeasure.MeasureUnitCode" fields from an autocleaned input
#' dataframe to perform conversions when transform = TRUE.
#'
#' Speciation in USGS result units
#' (ex: "mg/L as N") are addressed in this function by creating additional rows to
#' accommodate all possible combinations from the input TADA dataframe, including
#' those with speciation in units.
#'
#' @param .data TADA dataframe
#'
#' @param transform Boolean argument with two possible values, “TRUE” and “FALSE”.
#' Default is transform = TRUE. When transform = TRUE, result values and units,
#' and detection quantitation limit value and units are converted to TADA target units.
#' This function changes the values within "TADA.ResultMeasure.MeasureUnitCode" and
#' "TADA.DetectionQuantitationLimitMeasure.MeasureValue" to the TADA target units
#' and converts  respective values within the "TADA.ResultMeasureValue" and
#' "TADA.DetectionQuantitationLimitMeasure.MeasureValue" fields. When
#' "TADA.ResultMeasure.MeasureUnitCode" is NA, the unit is taken from
#' "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode" if it not NA. This
#' facilitates estimation of censored data later in the workflow.
#'
#' When transform = FALSE, result values and units, and detection quantitation limit
#' values are units are NOT converted to TADA target units,but columns are appended
#' to indicate what the target units and conversion factors are, and if the data can
#' be converted. This function adds the following four fields ONLY when transform =
#' FALSE: "TADA.WQXUnitConversionFactor", "TADA.Target.ResultMeasure.MeasureUnitCode",
#' "TADA.SpeciationUnitConversion", and "TADA.WQXResultUnitConversion.
#'
#' @param ref Optional character argument in which a user can specify a data frame
#' by name. Data frame must contain the columns CharacteristicName, Unit, and TargetUnit.
#' TADA_CreateUnitRef() can be used to help create this data frame. There are two
#' options that do not require the user to supply a data frame, "tada" and "wqx".
#' When ref = "wqx" all unit conversion will be based on the WQX unit reference
#' which applies targets at the unit level.
#' When ref = "tada" all unit conversion will be based on TADA priority characteristic
#' units (where applicable) which applies targets at the characteristic level,
#' with any other units assigned by WQX unit reference (at the unit level).
#' The default is ref = "tada".
#'
#' @return A TADA dataframe. Depending on the arguments entered by the user, the
#' number of columns returned may differ. When transform = TRUE, TADA.ResultMeasureValue
#' and TADA.ResultMeasure.MeasureUnitCode are converted to target units. When transform =
#' FALSE, "TADA.WQXUnitConversionFactor", "TADA.Target.ResultMeasure.MeasureUnitCode",
#' "TADA.SpeciationUnitConversion" are added. With either transform argument,
#' "TADA.WQXResultUnitConversion" is added. It indicates if data can be converted.
#' "NoResultValue" means data cannot be converted because there is no
#' ResultMeasureValue, and "NoTargetUnit"means data cannot be converted because
#' the original unit is not associated with a target unit. "Convert" means the data
#' can be transformed.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Do not convert result values and units, but add four new columns titled
#' # "TADA.WQXUnitConversionFactor", "TADA.WQXUnitConversionCoefficient", "TADA.Target.ResultMeasure.MeasureUnitCode", and "TADA.SpeciationUnitConversion":
#' ResultUnitsNotConverted <- TADA_ConvertResultUnits(Data_Nutrients_UT, transform = FALSE)
#'
#' # Convert values and units for results and detection limits:
#' ResultUnitsConverted <- TADA_ConvertResultUnits(Data_Nutrients_UT, transform = TRUE)
#'
TADA_ConvertResultUnits <- function(.data, ref = "tada", transform = TRUE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check transform is boolean
  TADA_CheckType(transform, "logical")
  # check .data has all of the required columns

  if ("TADA.CharacteristicName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original CharacteristicName
    .data$TADA.CharacteristicName <- toupper(.data$CharacteristicName)
  }

  if ("TADA.ActivityMediaName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original ActivityMediaName
    .data$TADA.ActivityMediaName <- toupper(.data$ActivityMediaName)
  }

  if ("TADA.MethodSpeciationName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original MethodSpeciationName
    .data$TADA.MethodSpeciationName <- toupper(.data$MethodSpeciationName)
  }

  if ("TADA.ResultSampleFractionText" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original ResultSampleFractionText
    .data$TADA.ResultSampleFractionText <- toupper(.data$ResultSampleFractionText)
  }

  if ("TADA.ResultMeasureValue" %in% colnames(.data)) {
    .data <- .data
  } else {
    # run TADA_ConvertSpecialChars function
    .data <- TADA_ConvertSpecialChars(.data, "ResultMeasureValue")
  }

  if ("TADA.ResultMeasure.MeasureUnitCode" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original ResultMeasure.MeasureUnitCode
    .data$TADA.ResultMeasure.MeasureUnitCode <- toupper(.data$ResultMeasure.MeasureUnitCode)
  }

  if ("TADA.DetectionQuantitationLimitMeasure.MeasureValue" %in% colnames(.data)) {
    .data <- .data
  } else {
    # run TADA_ConvertSpecialChars function
    .data <- TADA_ConvertSpecialChars(.data, "DetectionQuantitationLimitMeasure.MeasureValue")
  }

  if ("TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original DetectionQuantitationLimitMeasure.MeasureUnitCode
    .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode <-
      toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  }

  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ActivityMediaName",
    "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
    "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )

  TADA_CheckColumns(.data, expected_cols)

  # execute function after checks are passed

  # import USGS ref for method speciation
  usgs.ref <- TADA_GetUSGSSynonymRef()

  usgs.spec <- usgs.ref %>%
    dplyr::rename(
      ResultMeasure.MeasureUnitCode = Code,
      TADA.Target.MethodSpeciationName = Target.Speciation,
      TADA.WQXUnitConversionFactor = Conversion.Factor
    ) %>%
    dplyr::mutate(
      ResultMeasure.MeasureUnitCode.Upper = toupper(ResultMeasure.MeasureUnitCode),
      TADA.Target.MethodSpeciationName = toupper(TADA.Target.MethodSpeciationName)
    ) %>%
    dplyr::select(ResultMeasure.MeasureUnitCode, TADA.Target.MethodSpeciationName)

  usgs.unit <- usgs.ref %>%
    dplyr::select(Code, CodeNoSpeciation) %>%
    dplyr::mutate(
      TADA.ResultMeasure.MeasureUnitCode = toupper(Code),
      CodeNoSpeciation = toupper(CodeNoSpeciation)
    ) %>%
    dplyr::select(-Code) %>%
    dplyr::rename(MeasureUnitCode.match = CodeNoSpeciation)


  # if user supplied unit reference was provided
  if (is.data.frame(ref)) {
    # required columns
    expected_ref_cols <- c(
      "TADA.CharacteristicName",
      "TADA.ResultMeasure.MeasureUnitCode",
      "TADA.Target.ResultMeasure.MeasureUnitCode",
      "TADA.WQXUnitConversionFactor",
      "TADA.WQXUnitConversionCoefficient"
    )

    if (all(expected_ref_cols %in% colnames(ref)) == FALSE) {
      stop("The reference data frame does not contain all fields required for TADA_ConvertResultUnits. Use TADA_CreateUnitRef with the TADA data frame to create an editable unit reference table with all required columns.")

      if (all(expected_ref_cols %in% colnames(ref)) == TRUE) {
        print("The reference data frame contains all fields required for TADA_ConvertResultUnits.")
      }
    }

    # join USGS ref for method speciation name information
    unit.ref <- ref %>%
      dplyr::left_join(usgs.spec, by = dplyr::join_by(ResultMeasure.MeasureUnitCode)) %>%
      dplyr::distinct()

    # create message to inform users if user-supplied unit reference contains all combinations present in TADA data frame
    # create list of unique characteristic and unit combinations in data
    tada.list <- TADA_CreateUnitRef(.data) %>%
      dplyr::select(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode) %>%
      dplyr::distinct()

    # create list of unique characteristic and unit combinations in user-supplied unit ref
    user.list <- unit.ref %>%
      dplyr::select(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode) %>%
      dplyr::distinct()

    # compare the unique characteristic/unit combinations in data nd unit ref
    compare.ref <- tada.list %>%
      dplyr::anti_join(user.list, by = c("TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode"))

    # if no difference between the two, print message that all combinations are present in unit ref
    if (nrow(compare.ref) == 0) {
      print("All CharacteristicName/Unit combinations in the TADA dataframe are represented in user-supplied unit reference.")
      # if there are characteristic/unit combinations in the data that are not in the unit ref, print a warning message listing them
    } else {
      compare.list <- compare.ref %>%
        dplyr::mutate(Comb = paste(CharacteristicName, " (", Code, ")", sep = "")) %>%
        dplyr::mutate(CombList = paste(Comb, collapse = ", ")) %>%
        dplyr::select(CombList) %>%
        dplyr::distinct() %>%
        stringi::stri_replace_last(fixed = ",", " and")

      print(paste("TADA_ConvertResultUnits: The following CharacteristicName and ResultMeasure.MeasureUnitCode combinations are not included in the user-supplied unit reference data frame: ",
        compare.list,
        ". Consider revising the user-supplied unit reference data frame and running TADA_ConvertResultUnits again.",
        sep = ""
      ))
    }
  }

  # if user did not provide a data frame
  if (!is.data.frame(ref)) {
    # if no unit reference df was provided by user or user input was "tada"
    if (ref == "tada") {
      unit.ref <- TADA_CreateUnitRef(.data, print.message = FALSE)

      unit.ref <- unit.ref %>%
        dplyr::left_join(usgs.spec, by = dplyr::join_by(ResultMeasure.MeasureUnitCode)) %>%
        dplyr::distinct()
    }

    if (ref == "wqx") {
      # import WQX unit ref
      wqx.ref <- TADA_GetMeasureUnitRef()

      wqx.ref <- wqx.ref %>%
        dplyr::mutate(
          Target.Unit = toupper(Target.Unit),
          Code = toupper(Code)
        ) %>%
        dplyr::rename(
          MeasureUnitCode.match = Code,
          TADA.Target.ResultMeasure.MeasureUnitCode = Target.Unit,
          TADA.WQXUnitConversionFactor = Conversion.Factor,
          TADA.WQXUnitConversionCoefficient = Conversion.Coefficient
        ) %>%
        dplyr::select(
          MeasureUnitCode.match, TADA.Target.ResultMeasure.MeasureUnitCode,
          TADA.WQXUnitConversionFactor, TADA.WQXUnitConversionCoefficient
        ) %>%
        dplyr::distinct()

      # create unit ref
      unit.ref <- TADA_UniqueCharUnitSpeciation(.data) %>%
        dplyr::left_join(usgs.unit, by = dplyr::join_by(TADA.ResultMeasure.MeasureUnitCode)) %>%
        dplyr::mutate(MeasureUnitCode.match = ifelse(is.na(MeasureUnitCode.match),
          TADA.ResultMeasure.MeasureUnitCode,
          MeasureUnitCode.match
        )) %>%
        dplyr::left_join(wqx.ref, by = dplyr::join_by(MeasureUnitCode.match)) %>%
        dplyr::select(-MeasureUnitCode.match) %>%
        dplyr::distinct()

      unit.ref <- unit.ref %>%
        dplyr::left_join(usgs.spec, by = dplyr::join_by(ResultMeasure.MeasureUnitCode)) %>%
        dplyr::select(-TADA.MethodSpeciationName) %>%
        dplyr::distinct()
    }
  }

  # list of conversion columns

  conversion.cols <- c(
    "TADA.SpeciationUnitConversion",
    "TADA.WQXUnitConversionFactor", "TADA.WQXUnitConversionCoefficient",
    "TADA.Target.MethodSpeciationName", "TADA.Target.ResultMeasure.MeasureUnitCode"
  )

  # ref join

  ref.join <- c(
    "TADA.CharacteristicName",
    "ResultMeasure.MeasureUnitCode",
    "TADA.ResultMeasure.MeasureUnitCode"
  )


  # join unit.ref to .data
  check.data <- .data %>%
    # remove existing conversion columns
    dplyr::select(-tidyselect::any_of(c(conversion.cols))) %>%
    # add new conversion columns
    dplyr::left_join(unit.ref,
      by =
        c(
          ref.join
        ),
      relationship = "many-to-many"
    )

  # add TADA.WQXResultUnitConversion column
  flag.data <- check.data %>%
    # create flag column
    dplyr::mutate(TADA.WQXResultUnitConversion = dplyr::case_when(
      (!is.na(TADA.ResultMeasureValue) & !is.na(TADA.Target.ResultMeasure.MeasureUnitCode)) ~ as.character("Convert"),
      is.na(TADA.ResultMeasureValue) ~ as.character("No Result Value"),
      is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ as.character("No Target Unit")
    ))

  if (transform == FALSE) {
    print("TADA_ConvertResultUnits: When Transform = FALSE, result values and units are NOT converted. Conversions are required for many other TADA functions to work properly (such as result value range checks).")
    # reorder columns
    clean.data <- TADA_OrderCols(flag.data) %>%
      TADA_CreateComparableID()

    # Update ID and column ordering
    clean.data <- TADA_CreateComparableID(clean.data)
    clean.data <- TADA_OrderCols(clean.data)

    return(clean.data)
  }

  if (transform == TRUE) {
    # Transform result measure value to Target Unit only if target unit exists
    clean.data <- flag.data %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
        is.na(TADA.ResultMeasureValue) ~ TADA.ResultMeasureValue,
        !is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ ((TADA.ResultMeasureValue + TADA.WQXUnitConversionCoefficient) * TADA.WQXUnitConversionFactor),
        is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ TADA.ResultMeasureValue
      ))

    rm(flag.data)

    # populate TADA.ResultMeasure.MeasureUnitCode
    clean.data <- clean.data %>%
      # use target unit where there is a target unit, use original unit if no target unit
      dplyr::mutate(TADA.ResultMeasure.MeasureUnitCode = dplyr::case_when(
        !is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ TADA.Target.ResultMeasure.MeasureUnitCode,
        is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ TADA.ResultMeasure.MeasureUnitCode
      ))

    # Convert method speciation column for USGS data
    check <- clean.data %>%
      dplyr::filter(ResultMeasure.MeasureUnitCode %in% usgs.ref$ResultMeasure.MeasureUnitCode)

    if (dim(check)[1] > 0) {
      print(paste0("NOTE: Dataset contains ", dim(check)[1], " USGS results with speciation information in both the result unit and method speciation columns. This function overwrites the TADA method speciation column with the speciation provided in the result unit column."))
    }

    # add target method speciation name when needed
    clean.data <- clean.data %>%
      dplyr::mutate(
        TADA.MethodSpeciationName = ifelse(!is.na(TADA.Target.MethodSpeciationName), TADA.Target.MethodSpeciationName, toupper(TADA.MethodSpeciationName)),
        # replace UNKNOWN or NONE method speciation name with NA
        TADA.MethodSpeciationName = ifelse(TADA.MethodSpeciationName %in% c("UNKNOWN", "NONE"), NA, TADA.MethodSpeciationName)
      )

    # create detection unit ref
    det.ref <- unit.ref %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        DetectionQuantitationLimitMeasure.MeasureUnitCode = ResultMeasure.MeasureUnitCode,
        TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = TADA.ResultMeasure.MeasureUnitCode
      ) %>%
      dplyr::distinct()

    det.join <- c(
      "TADA.CharacteristicName",
      "DetectionQuantitationLimitMeasure.MeasureUnitCode",
      "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
    )

    # Transform TADA.DetectionQuantitationLimitMeasure.MeasureValue value to target value only if target unit exists
    det.data <- clean.data %>%
      dplyr::select(-tidyselect::any_of(conversion.cols)) %>%
      dplyr::left_join(det.ref, by = det.join) %>%
      # apply conversions where there is a target unit, use original value if no target unit
      dplyr::mutate(TADA.DetectionQuantitationLimitMeasure.MeasureValue = dplyr::case_when(
        is.na(TADA.DetectionQuantitationLimitMeasure.MeasureValue) ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue,
        !is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ ((TADA.DetectionQuantitationLimitMeasure.MeasureValue + TADA.WQXUnitConversionCoefficient) * TADA.WQXUnitConversionFactor),
        is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue
      ))
    rm(clean.data)

    # populate TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
    convert.data <- det.data %>%
      # use target unit where there is a target unit, use original unit if no target unit
      dplyr::mutate(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = dplyr::case_when(
        !is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ TADA.Target.ResultMeasure.MeasureUnitCode,
        is.na(TADA.Target.ResultMeasure.MeasureUnitCode) ~ TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
      ))

    # Remove unneccessary conversion columns
    convert.data <- convert.data %>%
      dplyr::select(-tidyselect::any_of(conversion.cols))


    # Transform pH units to "STD UNITS"
    if(any(convert.data$CharacteristicName == "pH")) {
      print("TADA_ConvertResultUnits: harmonizing pH units to STD UNITS.")

      convert.data <- convert.data %>%
        dplyr::mutate(TADA.ResultMeasure.MeasureUnitCode = dplyr::case_when(
          TADA.CharacteristicName == "PH" & TADA.ResultMeasure.MeasureUnitCode %in% NA ~ as.character("STD UNITS"),
          TADA.CharacteristicName == "PH" & ResultMeasure.MeasureUnitCode %in% NA ~ as.character("STD UNITS"),
          TADA.CharacteristicName == "PH" & ResultMeasure.MeasureUnitCode == "None" ~ as.character("STD UNITS"),
          TADA.CharacteristicName == "PH" & ResultMeasure.MeasureUnitCode == "std units" ~ as.character("STD UNITS")
        ))
    }
    
    # Update ID and column ordering
    convert.data <- TADA_CreateComparableID(convert.data)
    convert.data <- TADA_OrderCols(convert.data)

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

  # read in unit conversion reference table from extdata, created by HRM on 4/30/2024
  length.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharConvertRef.csv", package = "EPATADA"))

  # subset to include only "Length Distance" units; filter by target unit defined in 'unit' argument
  length.ref <- length.ref %>%
    dplyr::filter(Code %in% c(
      "Angst", "cm", "dm", "feet", "ft", "in", "km", "m",
      "mi", "mm", "nm", "nmi", "yd"
    )) %>%
    dplyr::filter(Target.Unit == unit)




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
    check.data <- merge(check.data, length.ref[, c("Code", "Conversion.Factor")],
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
