#' Check Sample Fraction Validity
#'
#' Function checks the validity of each characteristic-fraction combination
#' in the dataframe. When clean = TRUE, rows with Suspect characteristic-fraction
#' combinations are removed. Default is clean = TRUE. When flaggedonly = TRUE, only
#' Suspect characteristic-fraction combinations are returned. Default is flaggedonly = FALSE.
#'
#' The “Not Reviewed” value within "TADA.SampleFraction.Flag" means
#' that the EPA WQX team has not yet reviewed the combinations
#' (see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
#' The WQX team plans to review and update these new combinations quarterly.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Suspect" characteristic-fraction
#' combinations from the dataframe when clean = TRUE. Default is clean = TRUE.
#' @param flaggedonly Boolean argument; filters to show only the "Suspect"
#' characteristic-fraction combinations in the dataframe when flaggedonly = TRUE.
#' Default is flaggedonly = FALSE.
#'
#' @return This function adds the following column to your dataframe:
#'   TADA.SampleFraction.Flag, which flags each CharacteristicName and
#'   ResultSampleFractionText combination in your dataframe as either
#'   "NonStandardized", "Suspect", "Pass", or "Not Reviewed". When clean = FALSE and flaggedonly
#'   = TRUE, the data are filtered to show the "Suspect" rows only. When clean =
#'   TRUE and flaggedonly = FALSE, "Suspect" rows are removed from the dataframe
#'   and no column will be appended. When clean = TRUE and flaggedonly = TRUE,
#'   the function does not execute and an error message is returned.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#'
#' # Remove data with Suspect characteristic-fraction combinations:
#' SuspectFraction_clean <- TADA_FlagFraction(Data_Nutrients_UT)
#'
#' # Flag, but do not remove, data with Suspect characteristic-fraction
#' # combinations
#' # in new column titled "TADA.SampleFraction.Flag":
#' SuspectFraction_flags <- TADA_FlagFraction(Data_Nutrients_UT, clean = FALSE)
#'
#' # Show only Suspect characteristic-fraction combinations:
#' SuspectFraction_flaggedonly <- TADA_FlagFraction(Data_Nutrients_UT,
#'   clean = FALSE, flaggedonly = TRUE
#' )
#'
TADA_FlagFraction <- function(.data, clean = TRUE, flaggedonly = FALSE) {
  # check .data is data.frame and has required columns
  TADA_CheckColumns(
    .data,
    c("TADA.CharacteristicName", "TADA.ResultSampleFractionText")
  )
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check that both clean and flaggedonly are not TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop(
      "Function not executed because clean and flaggedonly cannot both be TRUE"
    )
  }

  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.SampleFraction.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.SampleFraction.Flag)
  }

  # read in fraction reference table from extdata and filter
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)
  frac.ref <- dplyr::filter(WQXcharValRef, Type == "CharacteristicFraction")
  rm(WQXcharValRef)

  # join "TADA.WQXVal.Flag" column to .data by CharacteristicName and Value (SampleFraction)
  check.data <- merge(
    .data,
    frac.ref[, c("Characteristic", "TADA.WQXVal.Flag", "Value")],
    by.x = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText"),
    by.y = c("Characteristic", "Value"),
    all.x = TRUE
  )

  # rename TADA.WQXVal.Flag column
  check.data <- check.data |>
    dplyr::rename(TADA.SampleFraction.Flag = TADA.WQXVal.Flag) |>
    dplyr::distinct()
  # rename NA values to "Not Reviewed" in TADA.SampleFraction.Flag column
  check.data["TADA.SampleFraction.Flag"][is.na(check.data[
    "TADA.SampleFraction.Flag"
  ])] <- "Not Reviewed"

  # if all rows are "Pass", return input unchanged
  if (
    any(
      c("NonStandardized", "Suspect", "Not Reviewed") %in%
        unique(check.data$TADA.SampleFraction.Flag)
    ) ==
      FALSE
  ) {
    if (flaggedonly == FALSE) {
      message(
        "All characteristic/fraction combinations are valid in your dataframe. Returning input dataframe with TADA.SampleFraction.Flag column for tracking."
      )
      check.data <- TADA_OrderCols(check.data)
      return(check.data)
    }
    if (flaggedonly == TRUE) {
      message(
        "This dataframe is empty because we did not find any Suspect fraction/characteristic combinations in your dataframe"
      )
      empty.data <- dplyr::filter(
        check.data,
        TADA.SampleFraction.Flag == "Suspect"
      )
      empty.data <- TADA_OrderCols(empty.data)
      return(empty.data)
    }
  }

  # flagged output, all data
  if (clean == FALSE & flaggedonly == FALSE) {
    message(
      "TADA_FlagFraction: Rows with Suspect sample fractions have been flagged but retained. Review these rows using the TADA.SampleFraction.Flag column before proceeding and/or set clean = TRUE."
    )
    check.data <- TADA_OrderCols(check.data)
    return(check.data)
  }

  # clean output
  if (clean == TRUE & flaggedonly == FALSE) {
    # filter out Suspect characteristic-fraction combinations
    clean.data <- dplyr::filter(
      check.data,
      TADA.SampleFraction.Flag != "Suspect"
    )
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }

  # flagged output, errors only
  if (clean == FALSE & flaggedonly == TRUE) {
    # filter out valid characteristic-fraction combinations
    Suspect.data <- dplyr::filter(
      check.data,
      TADA.SampleFraction.Flag == "Suspect"
    )
    Suspect.data <- TADA_OrderCols(Suspect.data)
    return(Suspect.data)
  }
}

#' Check Method Speciation Validity
#'
#' Function checks the validity of each characteristic-method
#' speciation combination in the dataframe. When clean = "suspect_only", rows with Suspect
#' characteristic-method speciation combinations are removed. Default is
#' clean = "suspect_only". When flaggedonly = TRUE, dataframe is filtered to show only
#' rows with "Suspect" or "NonStandardized" characteristic-method speciation combinations.
#' Default is flaggedonly = FALSE.
#'
#' The “Not Reviewed” value within "TADA.MethodSpeciation.Flag" means
#' that the EPA WQX team has not yet reviewed the combinations
#' (see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
#' The WQX team plans to review and update these new combinations quarterly.
#'
#' @param .data TADA dataframe
#' @param clean Character argument with options "suspect_only", "nonstandardized_only",
#' "both", or "none." The default is clean = "suspect_only" which removes rows of
#' data flagged as having "Suspect" characteristic-method speciation combinations. When
#' clean = "nonstandardized_only", the function removes rows of data flagged as
#' having "NonStandardized" characteristic-method speciation combinations. When
#' clean = "both", the function removes rows of data flagged as either "Suspect" or
#' "NonStandardized". And when clean = "none", the function does not remove any "Suspect"
#' or "NonStandardized" rows of data.
#' @param flaggedonly Boolean argument; filters to show only the "Suspect"
#' characteristic-method speciation combinations from the dataframe when
#' flaggedonly = TRUE. Default is flaggedonly = FALSE.
#'
#' @return This function adds TADA.MethodSpeciation.Flag to the dataframe. This column
#' flags each TADA.CharacteristicName and MethodSpeciationName combination in your
#' dataframe as either "NonStandardized", "Suspect", "Pass", or "Not Reviewed". When clean = "none"
#' and flaggedonly = TRUE, the dataframe is filtered to show only the "Suspect" and
#' "NonStandardized data; the column TADA.MethodSpeciation.Flag is still appended.
#' When clean = "suspect_only" and flaggedonly = FALSE, "Suspect" rows are removed
#' from the dataframe, but "NonStandardized" rows are retained. When
#' clean = "nonstandardized_only" and flaggedonly = FALSE, "NonStandardized" rows
#' are removed, but "Suspect" rows are retained. The default is clean = "suspect_only"
#' and flaggedonly = FALSE.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#'
#' # Remove data with Suspect characteristic-method speciation combinations
#' # from dataframe,
#' # but retain "NonStandardized" combinations flagged in new column
#' # 'TADA.MethodSpeciation.Flag':
#' SuspectSpeciation_clean <- TADA_FlagSpeciation(Data_Nutrients_UT)
#'
#' # Remove data with "NonStandardized" characteristic-method speciation
#' # combinations
#' # from dataframe but retain Suspect combinations flagged in new column
#' # 'TADA.MethodSpeciation.Flag':
#' NonstandardSpeciation_clean <- TADA_FlagSpeciation(Data_Nutrients_UT,
#'   clean = "nonstandardized_only"
#' )
#'
#' # Remove both "Suspect" and "NonStandardized" characteristic-method
#' # speciation combinations
#' # from dataframe:
#' Speciation_clean <- TADA_FlagSpeciation(Data_Nutrients_UT, clean = "both")
#'
#' # Flag, but do not remove, data with "Suspect" or "NonStandardized"
#' # characteristic-method speciation
#' # combinations in new column titled "TADA.MethodSpeciation.Flag":
#' SuspectSpeciation_flags <- TADA_FlagSpeciation(Data_Nutrients_UT,
#'   clean = "none"
#' )
#'
#' # Show only Suspect characteristic-method speciation combinations:
#' SuspectSpeciation_flaggedonly <- TADA_FlagSpeciation(Data_Nutrients_UT,
#'   clean = "nonstandardized_only", flaggedonly = TRUE
#' )
#'
#' # Show only "NonStandardized" characteristic-method speciation combinations:
#' NonstandardSpeciation_flaggedonly <- TADA_FlagSpeciation(Data_Nutrients_UT,
#'   clean = "suspect_only", flaggedonly = TRUE
#' )
#'
TADA_FlagSpeciation <- function(
  .data,
  clean = c("suspect_only", "nonstandardized_only", "both", "none"),
  flaggedonly = FALSE
) {
  # check .data is data.frame and has required columns
  TADA_CheckColumns(
    .data,
    c("TADA.CharacteristicName", "TADA.MethodSpeciationName")
  )
  # check clean is boolean
  TADA_CheckType(clean, "character")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check that clean is either "suspect_only", "nonstandardized_only", "both", or "none"
  clean <- match.arg(clean)

  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.MethodSpeciation.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.MethodSpeciation.Flag)
  }

  # read in speciation reference table from extdata and filter
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)
  spec.ref <- dplyr::filter(WQXcharValRef, Type == "CharacteristicSpeciation")
  rm(WQXcharValRef)

  # join "TADA.WQXVal.Flag" column to .data by CharacteristicName and Value (Speciation)
  check.data <- merge(
    .data,
    spec.ref[, c("Characteristic", "TADA.WQXVal.Flag", "Value")],
    by.x = c("TADA.CharacteristicName", "TADA.MethodSpeciationName"),
    by.y = c("Characteristic", "Value"),
    all.x = TRUE
  )

  # rename TADA.WQXVal.Flag column
  check.data <- check.data |>
    dplyr::rename(TADA.MethodSpeciation.Flag = TADA.WQXVal.Flag) |>
    dplyr::distinct()

  # rename NA values to Not Reviewed in TADA.MethodSpeciation.Flag column
  check.data["TADA.MethodSpeciation.Flag"][is.na(check.data[
    "TADA.MethodSpeciation.Flag"
  ])] <- "Not Reviewed"

  # if all rows are "Pass", return input with flag column
  if (
    any(
      c("Not Reviewed", "Suspect", "NonStandardized") %in%
        unique(check.data$TADA.MethodSpeciation.Flag)
    ) ==
      FALSE
  ) {
    message(
      "TADA_FlagSpeciation: All characteristic/method speciation combinations are valid in your dataframe. Returning input dataframe with TADA.MethodSpeciation.Flag column for tracking."
    )
    check.data <- TADA_OrderCols(check.data)
    return(check.data)
  }

  # flagged output, all data
  if (clean == "none" & flaggedonly == FALSE) {
    message(
      "TADA_FlagSpeciation: Rows with Suspect speciations have been flagged but retained. Review these rows using the new TADA.MethodSpeciation.Flag column before proceeding and/or set clean = 'suspect_only' or 'both'."
    )
  }

  # when clean = "suspect_only"
  if (clean == "suspect_only") {
    # filter out rejected characteristic-method speciation combinations
    clean.data <- dplyr::filter(
      check.data,
      TADA.MethodSpeciation.Flag != "Suspect"
    )
  }

  # when clean = "nonstandardized_only"
  if (clean == "nonstandardized_only") {
    # filter out only "NonStandardized" characteristic-method speciation combinations
    clean.data <- dplyr::filter(
      check.data,
      !TADA.MethodSpeciation.Flag %in% c("NonStandardized")
    )
  }

  # when clean = "both"
  if (clean == "both") {
    # filter out both "Suspect" and "NonStandardized" characteristic-method speciation combinations
    # clean.data <- dplyr::filter(check.data, TADA.MethodSpeciation.Flag != "NonStandardized" & TADA.MethodSpeciation.Flag != "Suspect")
    clean.data <- dplyr::filter(
      check.data,
      TADA.MethodSpeciation.Flag %in% c("Not Reviewed", "Pass")
    )
  }

  # when clean = "none"
  if (clean == "none") {
    # retain all data
    clean.data <- check.data
  }

  # when flaggedonly = FALSE
  if (flaggedonly == FALSE) {
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }

  # when flaggedonly = TRUE
  if (flaggedonly == TRUE) {
    # filter to show only Suspect and/or NonStandardized characteristic-method speciation combinations
    # error.data <- dplyr::filter(clean.data, TADA.MethodSpeciation.Flag == "Suspect" | TADA.MethodSpeciation.Flag == "NonStandardized")
    error.data <- dplyr::filter(
      clean.data,
      !TADA.MethodSpeciation.Flag %in% c("Not Reviewed", "Pass")
    )

    # if there are no errors
    if (nrow(error.data) == 0) {
      message(
        "This dataframe is empty because either we did not find any Suspect/NonStandardized characteristic-method speciation combinations or they were all filtered out"
      )
      # error.data <- dplyr::select(error.data, -TADA.MethodSpeciation.Flag)
    }
    error.data <- TADA_OrderCols(error.data)
    return(error.data)
  }
}


#' Check Result Unit Validity
#'
#' This function checks the validity of each characteristic and result unit
#' combination in the input data frame. By default, rows are flagged but not removed.
#' The full input data frame is returned along with an additional flag column,
#' 'TADA.ResultUnit.Flag', unless `clean` is set to 'both', in which case the flag
#' column is excluded.
#'
#' Users can choose to filter and review only the flagged rows by setting
#' `flaggedonly` to `TRUE`. After review, users can choose to remove any rows flagged
#' as 'Suspect' or 'NonStandardized' in the 'TADA.ResultUnit.Flag' column by setting
#' `clean` to 'suspect_only', 'nonstandardized_only', 'both', or 'none'.
#' Note: The 'Not Reviewed' value in the 'TADA.ResultUnit.Flag' means that the
#' EPA WQX team has not yet reviewed the combination for validity.
#'
#' @param .data A data frame containing the TADA dataset.
#' @param clean A character argument with options 'suspect_only', 'nonstandardized_only',
#' 'both', or 'none'. The default is 'none', which retains all rows but flags them.
#' @param flaggedonly A boolean argument; filters the data frame to show only 'Suspect'
#' and 'NonStandardized' characteristic-media-result unit combinations when `TRUE`.
#' Default is `FALSE`. This can only be `TRUE` if `clean` is set to 'none'.
#'
#' @return The function returns the input data frame with an added 'TADA.ResultUnit.Flag'
#' column unless `clean` is 'both', in which case the column is excluded. This column
#' flags each 'TADA.CharacteristicName' and TADA.ResultMeasure.MeasureUnitCode'
#' combination as 'NonStandardized', 'Suspect', 'Pass', or 'Not Reviewed'.
#' When `clean = 'none'` and `flaggedonly = TRUE`, the data frame is filtered to show only
#' the 'Suspect' and 'NonStandardized' data.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Flag, but do not remove, data with 'Suspect' or 'NonStandardized'
#' # characteristic and unit combinations in a new column titled
#' # 'TADA.ResultUnit.Flag':
#' SuspectUnit_flags <- TADA_FlagResultUnit(Data_R5_TADAPackageDemo)
#'
#' # Show only 'Suspect' or 'NonStandardized' characteristic and unit combinations:
#' SuspectUnit_flaggedonly <- TADA_FlagResultUnit(Data_R5_TADAPackageDemo,
#'   clean = "none", flaggedonly = TRUE
#' )
#' SuspectUnit_flaggedonly_selectcols <- dplyr::select(
#'   SuspectUnit_flaggedonly,
#'   TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.ResultUnit.Flag
#' )
#'
#' # Remove both 'Suspect' and 'NonStandardized' characteristic and result
#' # combinations, and exclude the flag column:
#' ResultUnit_clean <- TADA_FlagResultUnit(Data_R5_TADAPackageDemo, clean = "both")
#'
TADA_FlagResultUnit <- function(.data, clean = "none", flaggedonly = FALSE) {
  # Check if the input dataframe is blank (empty)
  if (nrow(.data) == 0) {
    stop("The input dataframe is blank. Please provide a dataframe with data.")
  }

  # Ensure .data is a data.frame and contains required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.ActivityMediaName"
  )
  TADA_CheckColumns(.data, expected_cols) # Check for required columns

  # Validate types of clean and flaggedonly
  TADA_CheckType(clean, "character")
  TADA_CheckType(flaggedonly, "logical")

  # Ensure clean is one of the valid options
  clean <- match.arg(
    clean,
    choices = c("suspect_only", "nonstandardized_only", "both", "none")
  )

  # Check if flaggedonly is TRUE when clean is not 'none'
  if (flaggedonly && clean != "none") {
    stop(
      'The "flaggedonly" parameter can only be set to TRUE if "clean" is set to "none".'
    )
  }

  # Convert necessary columns to uppercase if they don't exist
  for (col in expected_cols) {
    if (!(col %in% colnames(.data))) {
      original_col <- sub("TADA.", "", col)
      .data[[col]] <- toupper(.data[[original_col]])
    }
  }

  # Remove existing TADA.ResultUnit.Flag column if present
  if ("TADA.ResultUnit.Flag" %in% colnames(.data)) {
    .data <- dplyr::select(.data, -TADA.ResultUnit.Flag)
  }

  # Load unit reference table for validation
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  unit.ref <- dplyr::filter(WQXcharValRef, Type == "CharacteristicUnit")

  # Merge data with reference table to flag results
  check.data <- merge(
    .data,
    unit.ref[, c("Characteristic", "Source", "TADA.WQXVal.Flag", "Value")],
    by.x = c(
      "TADA.CharacteristicName",
      "TADA.ResultMeasure.MeasureUnitCode",
      "TADA.ActivityMediaName"
    ),
    by.y = c("Characteristic", "Value", "Source"),
    all.x = TRUE
  )

  # Rename flag column and handle NA values
  check.data <- check.data |>
    dplyr::rename(TADA.ResultUnit.Flag = TADA.WQXVal.Flag) |>
    dplyr::mutate(
      TADA.ResultUnit.Flag = ifelse(
        is.na(TADA.ResultUnit.Flag),
        "Not Reviewed",
        TADA.ResultUnit.Flag
      )
    )

  # Additional flagging for specific cases (e.g., pH with certain units)
  check.data <- check.data |>
    dplyr::mutate(
      TADA.ResultUnit.Flag = ifelse(
        TADA.CharacteristicName == "PH" &
          is.na(TADA.ActivityMediaName) &
          TADA.ResultMeasure.MeasureUnitCode %in% c("MOLE/L", "MMOL/L"),
        "Suspect",
        TADA.ResultUnit.Flag
      )
    )

  # Apply cleaning logic based on the 'clean' parameter
  if (clean != "none") {
    if (clean == "suspect_only") {
      check.data <- dplyr::filter(check.data, TADA.ResultUnit.Flag != "Suspect")
    } else if (clean == "nonstandardized_only") {
      check.data <- dplyr::filter(
        check.data,
        TADA.ResultUnit.Flag != "NonStandardized"
      )
    } else if (clean == "both") {
      check.data <- dplyr::filter(
        check.data,
        !TADA.ResultUnit.Flag %in% c("Suspect", "NonStandardized")
      )
      # Remove the flag column when clean is 'both'
      check.data <- dplyr::select(check.data, -TADA.ResultUnit.Flag)
    }
  }

  # Apply flaggedonly logic to show only flagged rows
  if (flaggedonly) {
    check.data <- dplyr::filter(
      check.data,
      TADA.ResultUnit.Flag %in% c("Suspect", "NonStandardized")
    )
    if (nrow(check.data) == 0) {
      message(
        "No Suspect/NonStandardized characteristic-media-result unit combinations found."
      )
    }
  }

  # Order columns as per the required format
  check.data <- TADA_OrderCols(check.data)
  return(check.data)
}


#' Check for Quality Control Samples
#'
#' This function checks for and flags or removes samples denoted as quality control
#' activities based on the 'ActivityTypeCode' column. The function will flag
#' duplicate samples as "QC_duplicate", blank samples as "QC_blank", calibration
#' or spiked samples as "QC_calibration", and other QC samples as "QC_other".
#' All other samples are flagged as "Non_QC".
#'
#' @param .data TADA dataframe which must include the column 'ActivityTypeCode'
#' @param clean Character argument with options "none", "all",
#' "duplicates", or "blanks", "calibrations", or "other". The default is
#' clean = "none" which does not remove any rows of data. When clean = "all",
#' any rows of data flagged as a Quality Control sample will be removed. When
#' clean = "duplicates", any rows of data flagged as a duplicate Quality Control
#' sample will be removed. When clean = "blanks", any rows of data flagged as a
#' blank Quality Control sample will be removed. When clean = "calibrations", any
#' rows of data flagged as a calibration check or spiked Quality Control sample
#' will be removed. And when clean = "other", any rows of data flagged as some
#' other type of Quality Control sample will be removed.
#' @param flaggedonly Boolean argument; the default is flaggedonly = FALSE. When
#' flaggedonly = TRUE, the function will filter the dataframe to show only the
#' rows of data flagged as Quality Control samples.
#'
#' @return This function adds the column "TADA.ActivityType.Flag" to the dataframe
#' which flags quality control samples based on the "ActivityTypeCode" column. When
#' clean = "none", all flagged data are kept in the dataframe. When clean = "all",
#' all flagged data are removed from the dataframe. When clean = "duplicates",
#' data flagged as QC duplicates are removed from the dataframe. When clean =
#' "blanks", data flagged as QC blanks are removed from the dataframe. When
#' clean = "calibrations", data flagged as QC calibration checks or spikes are
#' removed from the dataframe. When clean = "other", data flagged as other QC
#' samples are removed from the dataframe. When flaggedonly = TRUE, the dataframe
#' is filtered to show only the flagged data. When flaggedonly = FALSE, the full,
#' cleaned dataframe is returned. The default is clean = "none" and flaggedonly = FALSE.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#'
#' # Flag and keep all QC samples:
#' QC_flagged <- TADA_FindQCActivities(Data_Nutrients_UT)
#'
#' # Flag QC samples and filter to flagged data only:
#' QC_flags_only <- TADA_FindQCActivities(Data_Nutrients_UT, flaggedonly = TRUE)
#'
#' # Remove all QC samples:
#' QC_clean <- TADA_FindQCActivities(Data_Nutrients_UT, clean = TRUE)
#'
TADA_FindQCActivities <- function(.data, clean = FALSE, flaggedonly = FALSE) {
  # Check if the input .data is NULL
  if (is.null(.data)) {
    warning(
      "TADA_FindQCActivities: The input dataframe is NULL. Please provide a valid dataframe."
    )
    return(NULL)
  }
  # check .data is data.frame and has required columns
  TADA_CheckColumns(.data, c("ActivityTypeCode"))
  # check that clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")

  # execute function after checks are passed
  # delete existing flag column
  if (("TADA.ActivityType.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ActivityType.Flag)
  }

  # load in ActivityTypeRef Table
  qc.ref <- TADA_GetActivityTypeRef() |>
    dplyr::rename(ActivityTypeCode = Code) |>
    dplyr::select(ActivityTypeCode, TADA.ActivityType.Flag)

  # identify any Activity Type Codes not in reference table
  # these are likely USGS only values
  codes <- unique(.data$ActivityTypeCode)
  if (any(!codes %in% qc.ref$ActivityTypeCode)) {
    missing_codes <- codes[!codes %in% qc.ref$ActivityTypeCode]
    missing_codes_df <- data.frame(
      ActivityTypeCode = missing_codes,
      TADA.ActivityType.Flag = "Not Reviewed"
    )
    qc.ref <- rbind(qc.ref, missing_codes_df)
    missing_codes <- paste(missing_codes, collapse = ", ")
    message(paste0(
      "TADA_FindQCActivities: ActivityTypeCode column in dataset contains value(s) ",
      missing_codes,
      " which is/are not in the ActivityType WQX domain table. These data records are USGS only values and placed under the TADA.ActivityType.Flag: 'Not Reviewed'. Please review these carefully to detemine data usability."
    ))
  }

  # populate flag column in data
  flag.data <- dplyr::left_join(.data, qc.ref, by = "ActivityTypeCode")
  flag.data <- flag.data |> dplyr::distinct()

  # clean dataframe
  # if clean = FALSE, return full dataframe
  if (clean == FALSE) {
    clean.data <- flag.data
  }
  # if clean = TRUE, remove flagged data
  if (clean == TRUE) {
    clean.data <- dplyr::filter(flag.data, TADA.ActivityType.Flag == "Non_QC")
  }

  # if flaggedonly = FALSE, return full clean dataframe
  if (flaggedonly == FALSE) {
    final.data <- clean.data
    # if the dataframe is empty, print message
    if (nrow(final.data) == 0) {
      message(
        "TADA_FindQCActivities: This dataframe is empty because all rows contained QC samples and were removed"
      )
    }
    # if there are no flags, print message
    if (sum(final.data$TADA.ActivityType.Flag != "Non_QC") == 0) {
      message(
        "TADA_FindQCActivities: Quality control samples have been removed or were not present in the input dataframe. Returning dataframe with TADA.ActivityType.Flag column for tracking."
      )
    }
  }

  # if flaggedonly = TRUE, return clean dataframe filtered to only the flagged rows
  if (flaggedonly == TRUE) {
    # filter to review only samples that are not Non_QC
    final.data <- dplyr::filter(clean.data, TADA.ActivityType.Flag != "Non_QC")

    # if the dataframe is empty, print message
    if (nrow(final.data) == 0) {
      message(
        "TADA_FindQCActivities: This dataframe is empty because either we did not find any QC samples or because they were all removed"
      )
    }
  }

  # order and return final dataframe
  final.data <- TADA_CreateComparableID(final.data)
  final.data <- TADA_OrderCols(final.data)
  return(final.data)
}


#' Pair Replicates with Original Samples
#'
#' This function looks for replicate samples and pairs them to their original or
#' parent sample for further analysis. Replicate samples without an associated original
#' sample are flagged as orphan samples.
#'
#' @param .data TADA dataframe which must include the columns 'OrganizationIdentifier',
#' 'ActivityTypeCode', 'ActivityStartDate', 'ActivityStartDateTime', 'ResultIdentifier',
#' 'ActivityRelativeDepthName', 'TADA.LatitudeMeasure', 'TADA.LongitudeMeasure',
#' 'TADA.ResultMeasureValue', 'TADA.ComparableDataIdentifier', 'TADA.ActivityType.Flag',
#' 'TADA.ActivityDepthHeightMeasure.MeasureValue', 'TADA.ResultDepthHeightMeasure.MeasureValue',
#' 'TADA.ActivityTopDepthHeightMeasure.MeasureValue', and
#' 'TADA.ActivityBottomDepthHeightMeasure.MeasureValue'.
#'
#' @param type Character argument identifying which Activity Types to look for while pairing replicates
#' to their parent samples. The default type is "QC_replicate", which includes Activity Type Codes:
#' "Quality Control Field Replicate Habitat Assessment",
#' "Quality Control Field Replicate Msr/Obs",
#' "Quality Control Field Replicate Portable Data Logger",
#' "Quality Control Field Replicate Sample-Composite", and
#' "Quality Control Sample-Field Replicate".
#'
#' @param time_difference Numeric argument defining the maximum time difference in seconds
#' to search for parent samples. The default time window is 600 seconds or 10 minutes.
#' The time_difference can be as large as the user would like, but parent-replicate pairs will only be
#' identified if they were collected on the same date.
#'
#' @return This function adds one column to the original dataframe: 'TADA.ReplicateSampleID'.
#' 'TADA.ReplicateSampleID' contains the 'ResultIdentifier' value from the replicate sample
#' if a parent sample match is identified. Both the replicate sample and the parent sample
#' will have the same 'ResultIdentifier' code in this column, marking them as a pair.
#' If a sample was identified as a replicate sample in the 'TADA.ActivityType.Flag'
#' column but does not have an associated parent sample in the dataframe, the 'TADA.ReplicateSampleID'
#' column will contain the flag 'Orphan'. If more than one parent or replicate sample is identified
#' in the dataframe, the 'TADA.ReplicateSampleID' column for all samples will contain the
#' 'ResultIdentifier' value from one of the replicate samples marking them as a grouping.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#'
#' # Run TADA_FindQCActivities to add TADA.ActivityType.Flag column:
#' df <- TADA_FindQCActivities(Data_Nutrients_UT)
#'
#' # Find pairs for all data flagged as "QC_replicate" in the TADA.ActivityType.Flag column:
#' df_all_pairs <- TADA_PairReplicates(df)
#'
#' # Find pairs for only data with ActivityTypeCode "Quality Control Sample-Field Replicate":
#' df_fieldrep_pairs <- TADA_PairReplicates(df, type = "Quality Control Sample-Field Replicate")
#'
#' # Find pairs for all data flagged as "QC_replicate" within a 5-minute time window:
#' df_all_pairs_5min <- TADA_PairReplicates(df, time_difference = 300)
TADA_PairReplicates <- function(
  .data,
  type = c("QC_replicate"),
  time_difference = 600
) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "OrganizationIdentifier",
    "ActivityTypeCode",
    "ActivityStartDate",
    "ActivityStartDateTime",
    "ResultIdentifier",
    "ActivityRelativeDepthName",
    "TADA.LatitudeMeasure",
    "TADA.LongitudeMeasure",
    "TADA.ResultMeasureValue",
    "TADA.ComparableDataIdentifier",
    "TADA.ActivityDepthHeightMeasure.MeasureValue",
    "TADA.ResultDepthHeightMeasure.MeasureValue",
    "TADA.ActivityTopDepthHeightMeasure.MeasureValue",
    "TADA.ActivityBottomDepthHeightMeasure.MeasureValue"
  )
  TADA_CheckColumns(.data, expected_cols)

  # run TADA_FindQCActivities if needed
  if (("TADA.ActivityType.Flag" %in% colnames(.data)) == TRUE) {
    .data <- .data
  }

  if (("TADA.ActivityType.Flag" %in% colnames(.data)) == FALSE) {
    .data <- TADA_FindQCActivities(.data, clean = FALSE, flaggedonly = FALSE)
  }

  # execute function after checks are passed
  if ("QC_replicate" %in% type) {
    if (
      nrow(dplyr::filter(
        .data,
        .data$TADA.ActivityType.Flag == "QC_replicate"
      )) ==
        0
    ) {
      stop("Function not executed because no replicates found in dataframe.")
    }
  } else {
    if (nrow(dplyr::filter(.data, .data$ActivityTypeCode %in% type)) == 0) {
      stop(paste0(
        "Function not executed because no replicates of type '",
        type,
        "' found in dataframe."
      ))
    }
  }
  # check type is character
  TADA_CheckType(type, "character")
  # check time_window is numeric
  TADA_CheckType(time_difference, "numeric")

  # execute function after checks are passed

  # create column for matching replicates to their parent sample
  .data$TADA.ReplicateSampleID <- NA

  # loop through rows flagged as "QC_replicate" to find Non_QC samples taken within 5 minutes of the replicate sample
  for (i in 1:nrow(.data)) {
    if ("QC_replicate" %in% type) {
      x <- .data$TADA.ActivityType.Flag[i]
    } else {
      x <- .data$ActivityTypeCode[i]
    }
    if (x %in% type) {
      # find samples with the same date, lat/long, organization name, comparable data identifier, and depth
      info_match <- which(
        .data$ActivityStartDate == .data$ActivityStartDate[i] &
          .data$TADA.LatitudeMeasure == .data$TADA.LatitudeMeasure[i] &
          .data$TADA.LongitudeMeasure == .data$TADA.LongitudeMeasure[i] &
          .data$OrganizationIdentifier == .data$OrganizationIdentifier[i] &
          .data$TADA.ComparableDataIdentifier ==
            .data$TADA.ComparableDataIdentifier[i] &
          ((.data$TADA.ActivityDepthHeightMeasure.MeasureValue ==
            .data$TADA.ActivityDepthHeightMeasure.MeasureValue[i]) |
            (is.na(.data$TADA.ActivityDepthHeightMeasure.MeasureValue) &
              is.na(.data$TADA.ActivityDepthHeightMeasure.MeasureValue[i]))) &
          ((.data$TADA.ResultDepthHeightMeasure.MeasureValue ==
            .data$TADA.ResultDepthHeightMeasure.MeasureValue[i]) |
            (is.na(.data$TADA.ResultDepthHeightMeasure.MeasureValue) &
              is.na(.data$TADA.ResultDepthHeightMeasure.MeasureValue[i]))) &
          ((.data$TADA.ActivityTopDepthHeightMeasure.MeasureValue ==
            .data$TADA.ActivityTopDepthHeightMeasure.MeasureValue[i]) |
            (is.na(.data$TADA.ActivityTopDepthHeightMeasure.MeasureValue) &
              is.na(.data$TADA.ActivityTopDepthHeightMeasure.MeasureValue[
                i
              ]))) &
          ((.data$TADA.ActivityBottomDepthHeightMeasure.MeasureValue ==
            .data$TADA.ActivityBottomDepthHeightMeasure.MeasureValue[i]) |
            (is.na(.data$TADA.ActivityBottomDepthHeightMeasure.MeasureValue) &
              is.na(.data$TADA.ActivityBottomDepthHeightMeasure.MeasureValue[
                i
              ]))) &
          ((.data$ActivityRelativeDepthName ==
            .data$ActivityRelativeDepthName[i]) |
            (is.na(.data$ActivityRelativeDepthName) &
              is.na(.data$ActivityRelativeDepthName[i])))
      )

      # if time field is not NA, find time difference between current sample and info_match samples
      if (!is.na(.data$ActivityStartDateTime[i])) {
        time_diff <- abs(difftime(
          .data$ActivityStartDateTime[i],
          .data$ActivityStartDateTime[info_match]
        ))

        # samples where the time differences is <= time_difference (default is 10 minutes)
        within_window <- info_match[time_diff <= time_difference]

        # keep the samples with times within the window
        info_match <- intersect(info_match, within_window)
      }

      # if 2 or more matches are identified (2 includes the replicate and the parent sample),
      # give all matches the result identifier of the replicate
      if (length(info_match) >= 2) {
        .data$TADA.ReplicateSampleID[info_match] <- .data$ResultIdentifier[i]
      }

      # if less than 2 matches are identified (i.e. just the replicate, no parent sample),
      # label the replicate as an orphan
      if (length(info_match) <= 1) {
        .data$TADA.ReplicateSampleID[i] <- "Orphan"
      }
    }
  }
  .data <- TADA_OrderCols(.data)
  return(.data)
}

#' Check for results with suspect Measure Qualifier Codes
#'
#' This function inspects the 'MeasureQualifierCode' column and flags results based
#' on a reference table of qualifier codes. Results are flagged as:
#' - "Suspect" for suspect qualifiers
#' - "Non-Detect" or "Over-Detect" for censored-data qualifiers (for later use in
#'   TADA_SimpleCensoredMethods)
#' - "Pass" when qualifiers indicate a passing result (or when no qualifier is provided)
#' - "Not Reviewed" for qualifiers not present in the WQX domain table
#'
#' Multiple qualifiers in a single record are supported when delimited by semicolons (;).
#' Unknown qualifiers are assigned "Not Reviewed" and a message is emitted.
#'
#' @details
#' - Requirements:
#'   - The input must contain a 'MeasureQualifierCode' column.
#'   - If `define = TRUE`, the input must also contain 'ResultIdentifier' to support
#'     building a definition column.
#' - Multiple codes:
#'   - When multiple qualifier codes are present in a single record (semicolon-separated),
#'     the following precedence is used when assigning the flag:
#'     Suspect > Non-Detect > Over-Detect > Pass > Not Reviewed.
#' - Unknown/missing/blank codes:
#'   - Any qualifier code not found in the WQX domain reference is labeled "Not Reviewed"
#'     and a message is issued.
#' - Definitions:
#'   - When `define = TRUE`, an additional column 'TADA.MeasureQualifierCode.Def' is added,
#'     containing concatenated "Code - Description" definitions (for all codes present in
#'     a record).
#' - Cleaning vs filtering:
#'   - `clean = TRUE` removes only rows flagged as "Suspect". Other flags (e.g., "Non-Detect",
#'     "Over-Detect", "Not Reviewed") are retained.
#'   - `flaggedonly = TRUE` overrides `clean` and returns only rows flagged as "Suspect".
#'
#' @param .data A TADA dataframe which must include the column 'MeasureQualifierCode'.
#' @param clean Logical; default `FALSE`. When `TRUE`, rows flagged as "Suspect" are removed.
#'   Ignored when `flaggedonly = TRUE`.
#' @param flaggedonly Logical; default `FALSE`. When `TRUE`, returns only rows flagged as
#'   "Suspect" and overrides the `clean` argument.
#' @param define Logical; default `TRUE`. When `TRUE`, adds a column
#'   'TADA.MeasureQualifierCode.Def' with concatenated definitions for any qualifier codes
#'   present in each result. When `FALSE`, no definition column is added.
#'
#' @return
#' The input dataframe with:
#' - A new column 'TADA.MeasureQualifierCode.Flag' indicating the flag for each result:
#'   "Suspect", "Non-Detect", "Over-Detect", "Pass", or "Not Reviewed".
#' - Optionally, a new column 'TADA.MeasureQualifierCode.Def' (when `define = TRUE`),
#'   containing concatenated "Code - Description" strings for any qualifiers present.
#'
#' Output shape depends on arguments:
#' - `clean = FALSE`, `flaggedonly = FALSE` (default): return all rows with flags (no removal).
#' - `clean = TRUE`, `flaggedonly = FALSE`: return all rows except those flagged "Suspect".
#' - `flaggedonly = TRUE` (regardless of `clean`): return only rows flagged "Suspect".
#'
#' Messages:
#' - A message is printed when unknown qualifier codes are encountered; those rows are labeled
#'   "Not Reviewed". Blank tokens are ignored and will not produce messages.
#' - A message is printed if the entire 'MeasureQualifierCode' column is NA.
#'
#' @seealso [TADA_GetMeasureQualifierCodeRef()]
#' @seealso [TADA_SimpleCensoredMethods()]
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#'
#' # 1) Flag all records and keep everything:
#' mq_flagged <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT)
#'
#' # 2) Return only suspect records (flaggedonly overrides clean):
#' mq_suspect_only <- TADA_FlagMeasureQualifierCode(
#'   Data_Nutrients_UT,
#'   flaggedonly = TRUE
#' )
#'
#' # 3) Remove suspect records but retain Non-Detect, Over-Detect, Not Reviewed, and Pass:
#' mq_clean <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT, clean = TRUE)
#'
#' # 4) Remove suspect records and skip adding the definition column:
#' mq_clean_nodefs <- TADA_FlagMeasureQualifierCode(
#'   Data_Nutrients_UT,
#'   clean = TRUE, define = FALSE
#' )
TADA_FlagMeasureQualifierCode <- function(
  .data,
  clean = FALSE,
  flaggedonly = FALSE,
  define = TRUE
) {
  # Check if the input .data is NULL
  if (is.null(.data)) {
    warning(
      "TADA_FlagMeasureQualifierCode: The input dataframe is NULL. Please provide a valid dataframe."
    )
    return(NULL)
  }

  # check .data is data.frame and has required columns
  TADA_CheckColumns(.data, "MeasureQualifierCode")
  if (isTRUE(define)) {
    TADA_CheckColumns(.data, "ResultIdentifier")
  }

  # check argument types
  TADA_CheckType(clean, "logical")
  TADA_CheckType(flaggedonly, "logical")
  TADA_CheckType(define, "logical")

  # coerce MeasureQualifierCode to character (protect against factor inputs)
  if (
    !is.character(.data$MeasureQualifierCode) &&
      !all(is.na(.data$MeasureQualifierCode))
  ) {
    .data$MeasureQualifierCode <- as.character(.data$MeasureQualifierCode)
  }

  # If MeasureQualifierCode is entirely NA, create flags and optionally defs, then handle flaggedonly
  if (all(is.na(.data$MeasureQualifierCode))) {
    message(
      "TADA_FlagMeasureQualifierCode: Dataframe does not include any information (all NAs) in MeasureQualifierCode."
    )

    .data <- .data |> dplyr::mutate(TADA.MeasureQualifierCode.Flag = "Pass")

    if (isTRUE(define)) {
      .data <- .data |>
        dplyr::mutate(TADA.MeasureQualifierCode.Def = "NA - Not Applicable")
    }

    if (isTRUE(flaggedonly)) {
      # flaggedonly overrides clean and returns only Suspect -> none here
      final.data <- .data |>
        dplyr::filter(TADA.MeasureQualifierCode.Flag == "Suspect")
      if (nrow(final.data) == 0) {
        message("TADA_FlagMeasureQualifierCode: No Suspect results found.")
      }
      final.data <- TADA_OrderCols(final.data)
      return(final.data)
    }

    .data <- TADA_OrderCols(.data)
    return(.data)
  }

  # delete existing flag column
  if ("TADA.MeasureQualifierCode.Flag" %in% colnames(.data)) {
    .data <- dplyr::select(.data, -TADA.MeasureQualifierCode.Flag)
  }

  # load reference table
  qc.ref <- TADA_GetMeasureQualifierCodeRef() |>
    dplyr::rename(MeasureQualifierCode = Code) |>
    dplyr::select(
      MeasureQualifierCode,
      TADA.MeasureQualifierCode.Flag,
      Description
    )

  # Add any missing codes from data to reference BEFORE computing flags
  # (ignore blank tokens)
  codes <- stringr::str_split(unique(.data$MeasureQualifierCode), ";") |>
    unlist() |>
    stringr::str_trim()
  codes <- unique(codes[!is.na(codes) & nzchar(codes)])

  if (length(codes) > 0 && any(!codes %in% qc.ref$MeasureQualifierCode)) {
    missing_codes <- codes[!codes %in% qc.ref$MeasureQualifierCode]
    if (length(missing_codes) > 0) {
      missing_codes_df <- data.frame(
        MeasureQualifierCode = missing_codes,
        TADA.MeasureQualifierCode.Flag = "Not Reviewed",
        Description = ""
      )
      qc.ref <- dplyr::bind_rows(qc.ref, missing_codes_df)
      message(paste0(
        "TADA_FlagMeasureQualifierCode: MeasureQualifierCode column in dataset contains value(s) ",
        paste(missing_codes, collapse = ", "),
        " which are not represented in the MeasureQualifierCode WQX domain table. ",
        "These data records are placed under the TADA.MeasureQualifierCode.Flag: 'Not Reviewed'. ",
        "Please contact TADA administrators to resolve."
      ))
    }
  }

  # add qualifier code definitions
  if (isTRUE(define)) {
    mqc.ref <- qc.ref |>
      dplyr::select(MeasureQualifierCode, Description) |>
      dplyr::distinct() |>
      dplyr::mutate(
        Concat = dplyr::if_else(
          nzchar(Description),
          paste0(MeasureQualifierCode, " - ", Description),
          MeasureQualifierCode
        )
      ) |>
      dplyr::select(MeasureQualifierCode, Concat)

    mqc.TADA <- .data |>
      dplyr::mutate(
        MeasureQualifierCode = stringr::str_split(MeasureQualifierCode, ";")
      ) |>
      tidyr::unnest(MeasureQualifierCode) |>
      dplyr::mutate(
        MeasureQualifierCode = stringr::str_trim(MeasureQualifierCode)
      ) |>
      dplyr::filter(
        !is.na(MeasureQualifierCode) & nzchar(MeasureQualifierCode)
      ) |>
      dplyr::left_join(mqc.ref, by = "MeasureQualifierCode") |>
      dplyr::group_by(ResultIdentifier) |>
      dplyr::summarize(
        TADA.MeasureQualifierCode.Def = {
          vals <- stats::na.omit(Concat)
          if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
        },
        .groups = "drop"
      )

    .data$TADA.MeasureQualifierCode.Def <- mqc.TADA$TADA.MeasureQualifierCode.Def[match(
      .data$ResultIdentifier,
      mqc.TADA$ResultIdentifier
    )]
  }

  # Build flag lists from reference
  flag.lists <- split(
    qc.ref$MeasureQualifierCode,
    qc.ref$TADA.MeasureQualifierCode.Flag
  )
  names(flag.lists) <- names(flag.lists) |>
    tolower() |>
    stringr::str_remove_all("-") |>
    stringr::str_remove_all(" ")

  # Compute flags (dropping empty tokens; blank-only rows => Pass)
  flag.data <- .data |>
    dplyr::mutate(
      MeasureQualifierCode.Split = stringr::str_split(
        MeasureQualifierCode,
        ";"
      ),
      MeasureQualifierCode.Split = purrr::map(
        MeasureQualifierCode.Split,
        ~ {
          y <- stringr::str_trim(.x)
          y <- y[!is.na(y) & nzchar(y)] # drop empty tokens
          if (length(y) == 0) NA_character_ else y
        }
      ),
      TADA.MeasureQualifierCode.Flag = purrr::map_chr(
        MeasureQualifierCode.Split,
        ~ dplyr::case_when(
          # Treat per-row NA/blank-only as Pass
          length(.x) == 1 && is.na(.x[1]) ~ "Pass",
          any(.x %in% flag.lists$suspect, na.rm = TRUE) ~ "Suspect",
          any(.x %in% flag.lists$nondetect, na.rm = TRUE) ~ "Non-Detect",
          any(.x %in% flag.lists$overdetect, na.rm = TRUE) ~ "Over-Detect",
          any(.x %in% flag.lists$pass, na.rm = TRUE) ~ "Pass",
          any(.x %in% flag.lists$notreviewed, na.rm = TRUE) ~ "Not Reviewed",
          TRUE ~ "Not Reviewed"
        )
      )
    ) |>
    dplyr::select(-MeasureQualifierCode.Split)

  # flaggedonly overrides clean and returns only Suspect
  if (isTRUE(flaggedonly)) {
    final.data <- dplyr::filter(
      flag.data,
      TADA.MeasureQualifierCode.Flag == "Suspect"
    )
    if (nrow(final.data) == 0) {
      message("TADA_FlagMeasureQualifierCode: No Suspect results found.")
    }
    final.data <- TADA_OrderCols(final.data)
    return(final.data)
  }

  # Otherwise, apply cleaning if requested (removes Suspect only)
  if (!isTRUE(clean)) {
    clean.data <- flag.data
  } else {
    clean.data <- dplyr::filter(
      flag.data,
      TADA.MeasureQualifierCode.Flag != "Suspect"
    )
    if (nrow(clean.data) == 0) {
      message(
        "TADA_FlagMeasureQualifierCode: This dataframe is empty because all rows contained Suspect samples that were removed."
      )
    }
  }

  final.data <- clean.data

  if (
    sum(final.data$TADA.MeasureQualifierCode.Flag != "Suspect", na.rm = TRUE) ==
      0
  ) {
    message(
      "TADA_FlagMeasureQualifierCode: Suspect samples have been removed or were not present in the input dataframe. Returning dataframe with TADA.MeasureQualifierCode.Flag column for tracking."
    )
  }

  final.data <- TADA_OrderCols(final.data)
  return(final.data)
}
