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
    "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
    "TADA.MethodSpecificationName",
    "TADA.ResultMeasure.MeasureUnitCode"
    )
  checkColumns(.data, expected_cols)
  
  # execute function after checks are passed
  # define raw harmonization table as an object
  harm.raw <- utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv", package = "TADA"))
  
  # columns to keep from .data if exist
  harmonization_cols <- c(
    "TADA.SampleFraction.Flag", "TADA.MethodSpeciation.Flag",
    "TADA.ResultUnit.Flag", "TADA.AnalyticalMethod.Flag"
  )
  
  # join to harmonization table
  # if WQX QA Char Val flags are in .data, include them in the join
  if (all(harmonization_cols %in% colnames(.data)) == TRUE) {
    datcols = unique(.data[, c(expected_cols, harmonization_cols)])
    # otherwise, execute the join with no additional columns
  } else {
    datcols = unique(.data[, expected_cols])
  }
  
  join.data <- merge(datcols,
                     harm.raw,
                     # by.x = expected_cols,
                     # by.y = expected_cols, EDH - are these needed?
                     all.x = TRUE)
  
  # trim join.data to include only unique combos of char-frac-spec-unit
  unique.data <- join.data %>% dplyr::distinct()
  
  unique.data$TADA.ComparableDataIdentifier = ifelse(is.na(unique.data$TADA.ComparableDataIdentifier),paste(unique.data$TADA.CharacteristicName,unique.data$TADA.ResultSampleFractionText, unique.data$TADA.MethodSpecificationName, unique.data$TADA.ResultMeasure.MeasureUnitCode,sep = "_"),unique.data$TADA.ComparableDataIdentifier)
  
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
#' 
#' # Append harmonization reference table columns to dataframe and transform/convert
#' # data to the USER SUPPLIED reference table values:
#' UniqueHarmonizationRef <- HarmonizationRefTable(Nutrients_Utah, download = FALSE)
#' Nutrients_Harmonized_UserSuppliedRef <- HarmonizeData(Nutrients_Utah, ref = UniqueHarmonizationRef)

HarmonizeData <- function(.data, ref, transform = TRUE, flag = TRUE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check transform is boolean
  checkType(transform, "logical")
  # check flag is boolean
  checkType(flag, "logical")

  # check .data has the required columns
  expected_cols <- c(
    "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
    "TADA.MethodSpecificationName", "TADA.ResultMeasure.MeasureUnitCode"
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
    "TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  ) %in% colnames(.data)) == TRUE) {

    # if class(ResultMeasureValue) != numeric, run special char function
    if (!is.numeric(.data$TADA.ResultMeasureValue) ) {
      stop("TADA.ResultMeasureValue is not numeric. This column must be numeric before proceeding.")
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
    
    .data = .data[,!names(.data)%in%c("TADA.ComparableDataIdentifier")]

    # join harm.ref to .data
    flag.data <- merge(.data, harm.ref,
                       # by.x = expected_cols, EDH - this is problematic because harm.ref might share expected and flag columns with data - this join is potentially many to many, leading to duplicate results.
                       # by.y = expected_cols,
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

    # if transform = FALSE and flag = TRUE, return flag.data
    if ((transform == FALSE) & (flag == TRUE)) {
      print("Be aware that you must run this function with transform = TRUE to use subsequent TADA functions.")
      flag.data = OrderTADACols(flag.data)
      return(flag.data)
    }

    # if transform = TRUE, transform data
    if (transform == TRUE) {

      # TADA.CharacteristicName
      # replace TADA.CharacteristicName with TADA.SuggestedCharacteristicName
      clean.data <- flag.data %>%
        # apply function row by row
        # dplyr::rowwise() %>%
        # use TADA suggested name where there is a suggested name, use original name if no suggested name
        dplyr::mutate(TADA.CharacteristicName = dplyr::case_when(
          !is.na(TADA.SuggestedCharacteristicName) ~ TADA.SuggestedCharacteristicName,
          is.na(TADA.SuggestedCharacteristicName) ~ TADA.CharacteristicName
        ))

      # TADA.ResultSampleFractionText
      # replace ResultSampleFractionText with TADA.SuggestedSampleFraction
      clean.data <- clean.data %>%
        # # apply function row by row
        # dplyr::rowwise() %>%
        # use TADA suggested frac where there is a suggested frac, use original frac if no suggested frac
        dplyr::mutate(TADA.ResultSampleFractionText = dplyr::case_when(
          !is.na(TADA.SuggestedSampleFraction) ~ TADA.SuggestedSampleFraction,
          is.na(TADA.SuggestedSampleFraction) ~ TADA.ResultSampleFractionText
        ))


      # ResultMeasure.MeasureUnitCode
      # replace ResultMeasure.MeasureUnitCode with TADA.SuggestedResultUnit
      clean.data <- clean.data %>%
        # apply function row by row
        # dplyr::rowwise() %>%
        # use TADA suggested unit where there is a suggested unit, use original unit if no suggested unit
        dplyr::mutate(TADA.ResultMeasure.MeasureUnitCode = dplyr::case_when(
          !is.na(TADA.SuggestedResultUnit) ~ TADA.SuggestedResultUnit,
          is.na(TADA.SuggestedResultUnit) ~ TADA.ResultMeasure.MeasureUnitCode
        )) %>%
        # if conversion factor exists, multiply by ResultMeasureValue
        dplyr::rowwise() %>%
        dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
          !is.na(TADA.UnitConversionFactor) ~
            (TADA.UnitConversionFactor * TADA.ResultMeasureValue),
          is.na(TADA.UnitConversionFactor) ~ TADA.ResultMeasureValue
        ))

      # TADA.MethodSpecificationName
      # replace MethodSpecificationName with TADA.SuggestedSpeciation
      clean.data <- clean.data %>%
        # apply function row by row
        # dplyr::rowwise() %>%
        # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
        dplyr::mutate(TADA.MethodSpecificationName = dplyr::case_when(
          !is.na(TADA.SuggestedSpeciation) ~ TADA.SuggestedSpeciation,
          is.na(TADA.SuggestedSpeciation) ~ TADA.MethodSpecificationName
        )) %>%
        # if conversion factor exists, multiply by ResultMeasureValue
        dplyr::rowwise() %>%
        dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
          !is.na(TADA.SpeciationConversionFactor) ~
            (TADA.SpeciationConversionFactor * TADA.ResultMeasureValue),
          is.na(TADA.SpeciationConversionFactor) ~ TADA.ResultMeasureValue
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
        clean.data = OrderTADACols(clean.data)
        return(clean.data)
      }

      # remove all appended columns if flag = FALSE
      if (flag == FALSE) {
        # remove all appended columns
        clean.data <- clean.data %>%
          dplyr::select(-c(
            "TADA.CharacteristicGroup",
            "CharacteristicNameUserSupplied",
            "CombinationValidity",
            "TADA.CharacteristicNameAssumptions",
            "TADA.FractionAssumptions",
            "TADA.SpeciationAssumptions",
            "TADA.UnitConversionCoefficient",
            "TADA.TotalN_TotalP_CharacteristicNames_AfterSummation",
            "TADA.TotalN_TotalP_Summation_Identifier",
            "TADA.TotalN_TotalP_ComboLogic"
          ))

        # return clean.data
        clean.data = OrderTADACols(clean.data)
        return(clean.data)
      }
    }
  }
}
