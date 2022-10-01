#' Transform Units to WQX Target Units
#'
#' This function compares measure units in the input data to the Water Quality
#' Exchange (WQX) 3.0 QAQC Characteristic Validation table.
#'
#' This function will ALWAYS add the following two columns to the input dataset:
#' 1) "WQX.ResultMeasureValue.UnitConversion",
#' 2) "WQX.DetectionLimitMeasureValue.UnitConversion"
#' These two fields indicate if data can be converted."NoResultValue" means data
#' cannot be converted because there is no ResultMeasureValue, and "NoTargetUnit"
#' means data cannot be converted because the original unit is not associated with a target unit in WQX.
#' "Convert" means the data can be transformed, and "Converted" means that this function
#' has been run with the input transform = TRUE, and the values were already converted.
#'
#' It also uses the following six fields from the input dataset:
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
#' @param .data TADA dataset
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
#' the following two fields to the input dataset, "ResultMeasureUnitCode.Original",
#' and "DetectionLimitMeasureUnitCode.Original", to retain the original result
#' and unit values.
#'
#' When transform = FALSE, result values and units are NOT converted to WQX target units,
#' but columns are appended to indicate what the target units and conversion factors are,
#' and if the data can be converted. In addition to "WQX.ResultMeasureValue.UnitConversion"
#' and "WQX.DetectionLimitMeasureValue.UnitConversion",  transform=FALSE will add the
#' following two fields to the input dataset: "WQX.ConversionFactor" and "WQX.TargetUnit".
#'
#' @export

WQXTargetUnits <- function(.data, transform = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has all of the required columns
  if (all(c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode",
    "DetectionQuantitationLimitMeasure.MeasureValue",
    "DetectionQuantitationLimitMeasure.MeasureUnitCode"
  ) %in%
    colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # check if transform is boolean
  if (is.logical(transform) == FALSE) {
    stop("transform argument must be Boolean (TRUE or FALSE)")
  }

  # execute function after checks are passed
  if (all(c(
    "CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
    "ResultMeasure.MeasureUnitCode",
    "DetectionQuantitationLimitMeasure.MeasureValue",
    "DetectionQuantitationLimitMeasure.MeasureUnitCode"
  ) %in%
    colnames(.data)) == TRUE) {

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
}


#' Transform Characteristic, Fraction, Speciation, and Unit values to TADA Standards
#'
#' Function compares input dataset to the TADA Harmonization Reference Table. The
#' purpose of the function is to make similar data consistent and therefore
#' easier to compare and analyze. Optional outputs include: 1) the dataset with
#' Harmonization columns appended, 2) the datset with CharacteristicName,
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
#' in the dataset to the TADA Harmonization Reference Table values for the
#' following fields: CharacteristicName, ResultSampleFractionText,
#' MethodSpecificationName, and ResultMeasure.MeasureUnitCode. Default is
#' transform = TRUE.
#' @param flag Boolean argument; appends all columns from the TADA Harmonization
#' Reference Table to the dataframe. Default is flag = TRUE.
#'
#' @return When transform = FALSE and flag = TRUE, Harmonization Reference Table
#' columns are appended to the dataset only. When transform = TRUE and flag = TRUE,
#' Harmonization columns are appended to the dataset and transformations are
#' executed. When transform = TRUE and flag = FALSE, transformations are executed
#' only. When transform = FALSE and flag = FALSE, an error is returned (function
#' would return the input dataframe unchanged if input was allowed).
#'
#' @export

HarmonizeData <- function(.data, ref, transform = TRUE, flag = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop(".data must be of class 'data.frame'")
  }
  # check ref is of class data.frame, if it exists
  if (!missing(ref)) {
    if (("data.frame" %in% class(ref)) == FALSE) {
      stop("ref must be of class 'data.frame'")
    }
  }
  # check .data has all of the required columns
  if (all(c(
    "CharacteristicName", "ResultSampleFractionText",
    "MethodSpecificationName",
    "ResultMeasure.MeasureUnitCode"
  ) %in%
    colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # check ref has all of the required columns
  if (!missing(ref)) {
    if ((all(c(
      "TADA.SuggestedCharacteristicName",
      "TADA.SuggestedSampleFraction",
      "TADA.SuggestedSpeciation",
      "TADA.SuggestedResultUnit"
    ) %in%
      colnames(ref))) == FALSE) {
      stop("The 'ref' input dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
    }
  }
  # check if transform is boolean
  if (is.logical(transform) == FALSE) {
    stop("transform argument must be Boolean (TRUE or FALSE)")
  }
  # check if flag is boolean
  if (is.logical(flag) == FALSE) {
    stop("flag argument must be Boolean (TRUE or FALSE)")
  }
  # check that both transform and flag do NOT equal FALSE
  if (transform == FALSE & flag == FALSE) {
    stop("Both 'transform' and 'flag' arguments equal FALSE, which would return the input dataset unchanged. One or both arguments must be equal to TRUE.")
  }

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
      harm.ref <- ref
    }
    # if input for ref does not exist, use raw harmonization template
    if (missing(ref)) {
      harm.ref <- utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv",
        package = "TADA"
      ))
      # remove extraneous characters in first column
      colnames(harm.ref)[1] <- gsub("^...", "", colnames(harm.ref)[1])
    }

    # join harm.ref to .data
    flag.data <- merge(.data, harm.ref,
      by.x = c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      ),
      by.y = c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      ),
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
      warning("Transformations required to use subsequent TADA tools.")
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
          "TADA.SuggestedCharacteristicName",
          "TADA.SuggestedSampleFraction",
          "TADA.SuggestedSpeciation",
          "TADA.SpeciationConversionFactor",
          "TADA.SuggestedResultUnit",
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

#' Generate Unique Harmonization Reference Table
#' 
#' #' Function generates a harmonization reference table that is specific to
#' the input dataset. Users can review how their input data relates to standard
#' TADA values for CharacteristicName, ResultSampleFractionText,
#' MethodSpecicationName, and ResultMeasure.MeasureUnitCode and they can optionally
#' edit the reference file to meet their needs.
#'
#' @param .data TADA dataframe
#' @param download Boolean argument; when download = TRUE, the output is
#' downloaded to the current working directory.
#'
#' @return Harmonization Reference Table unique to the input dataset
#'
#' @export

HarmonizationRefTable <- function(.data, download = FALSE) {
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has any of the required columns
  if (all(c(
    "CharacteristicName", "ResultSampleFractionText",
    "MethodSpecificationName",
    "ResultMeasure.MeasureUnitCode"
  ) %in%
  colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # check if download is boolean
  if (is.logical(download) == FALSE) {
    stop("download argument must be Boolean (TRUE or FALSE)")
  }
  # execute function after checks are passed
  if (all(c(
    "CharacteristicName", "ResultSampleFractionText",
    "MethodSpecificationName",
    "ResultMeasure.MeasureUnitCode"
  ) %in%
  colnames(.data)) == TRUE) {
    
    # define raw harmonization table as an object
    harm.raw <- utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv",
                                     package = "TADA"
    ))
    
    # join harmonization table to .data
    # if WQX QA Char Val flags are in .data, include them in the join
    if (all(c(
      "WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
      "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity"
    ) %in%
    colnames(.data)) == TRUE) {
      join.data <- merge(.data[, c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode",
        "WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
        "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity"
      )],
      harm.raw,
      by.x = c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      ),
      by.y = c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      ),
      all.x = TRUE
      )
      # otherwise, execute the join with no additional columns
    } else {
      join.data <- merge(.data[, c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      )],
      harm.raw,
      by.x = c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      ),
      by.y = c(
        "CharacteristicName", "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      ),
      all.x = TRUE
      )
    }
    
    # trim join.data to include only unique combos of char-frac-spec-unit
    unique.data <- join.data %>%
      dplyr::filter(!duplicated(join.data[, c(
        "CharacteristicName",
        "ResultSampleFractionText",
        "MethodSpecificationName",
        "ResultMeasure.MeasureUnitCode"
      )]))
    
    # reorder columns to match harm.raw
    # include WQX QA flag columns, if they exist
    if (all(c(
      "WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
      "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity"
    ) %in%
    colnames(.data)) == TRUE) {
      # get .data column names
      col.order <- colnames(harm.raw)
      # add WQX.SampleFractionValidity column to the list
      col.order <- append(col.order, c(
        "WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
        "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity"
      ))
      # reorder columns in flag.data
      unique.data <- unique.data[, col.order]
    } else {
      unique.data <- unique.data[, colnames(harm.raw)]
    }
    
    # remove extraneous characters in first column
    colnames(unique.data)[1] <- gsub("^", "", colnames(unique.data)[1])
    
    # flag potential duplicates
    unique.data$TADA.ComparableDataID <- as.integer(seq_len(nrow(unique.data)))
    
    # if download = TRUE, download unique.data as a csv to the working directory
    if (download == TRUE) {
      utils::write.csv(unique.data, "HarmonizationRefTable.csv")
    }
    
    # return unique.data
    return(unique.data)
  }
}
