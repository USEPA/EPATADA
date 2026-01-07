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
      print(
        "All characteristic/fraction combinations are valid in your dataframe. Returning input dataframe with TADA.SampleFraction.Flag column for tracking."
      )
      check.data <- TADA_OrderCols(check.data)
      return(check.data)
    }
    if (flaggedonly == TRUE) {
      print(
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
    print(
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
    print(
      "TADA_FlagSpeciation: All characteristic/method speciation combinations are valid in your dataframe. Returning input dataframe with TADA.MethodSpeciation.Flag column for tracking."
    )
    check.data <- TADA_OrderCols(check.data)
    return(check.data)
  }

  # flagged output, all data
  if (clean == "none" & flaggedonly == FALSE) {
    print(
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
      print(
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
  # If input is NULL, return a zero-row tibble with minimal expected columns
  if (is.null(.data)) {
    warning(
      "TADA_FindQCActivities: The input dataframe is NULL. Returning an empty dataframe."
    )
    empty <- tibble::tibble(
      ActivityTypeCode = character(0),
      TADA.ActivityType.Flag = character(0)
    )
    return(empty)
  }
  # Check .data is data.frame and has required columns
  TADA_CheckColumns(.data, c("ActivityTypeCode"))
  # Normalize and validate 'clean'
  # Backward compatibility: logical TRUE -> "all", FALSE -> "none"
  if (is.logical(clean)) {
    clean <- ifelse(isTRUE(clean), "all", "none")
  }
  allowed_clean <- c(
    "none",
    "all",
    "duplicates",
    "blanks",
    "calibrations",
    "other"
  )
  if (is.na(clean) || !(clean %in% allowed_clean)) {
    stop(
      "TADA_FindQCActivities: 'clean' must be one of: ",
      paste(allowed_clean, collapse = ", "),
      " (or a logical TRUE/FALSE for backward compatibility)."
    )
  }
  # Check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # Delete existing flag column if present
  if ("TADA.ActivityType.Flag" %in% colnames(.data)) {
    .data <- dplyr::select(.data, -TADA.ActivityType.Flag)
  }
  # Load ActivityType reference table
  qc.ref <- utils::read.csv(system.file(
    "extdata",
    "WQXActivityTypeRef.csv",
    package = "EPATADA"
  )) |>
    dplyr::rename(ActivityTypeCode = Code) |>
    dplyr::select(ActivityTypeCode, TADA.ActivityType.Flag)
  # Identify any Activity Type Codes not in reference table (likely USGS-only values)
  codes <- unique(.data$ActivityTypeCode)
  if (any(!codes %in% qc.ref$ActivityTypeCode)) {
    missing_codes <- codes[!codes %in% qc.ref$ActivityTypeCode]
    missing_codes_df <- data.frame(
      ActivityTypeCode = missing_codes,
      TADA.ActivityType.Flag = "Not Reviewed"
    )
    qc.ref <- rbind(qc.ref, missing_codes_df)
    missing_codes_str <- paste(missing_codes, collapse = ", ")
    message(paste0(
      "TADA_FindQCActivities: ActivityTypeCode column in dataset contains value(s) ",
      missing_codes_str,
      " which is/are not in the ActivityType WQX domain table. These data records ",
      "are USGS-only values and placed under the TADA.ActivityType.Flag: 'Not Reviewed'. ",
      "Please review these carefully to determine data usability."
    ))
  }
  # Populate flag column in data
  flag.data <- dplyr::left_join(.data, qc.ref, by = "ActivityTypeCode") |>
    dplyr::distinct()
  # Treat missing flags (e.g., NA ActivityTypeCode) as "Not Reviewed"
  flag.data$TADA.ActivityType.Flag[is.na(
    flag.data$TADA.ActivityType.Flag
  )] <- "Not Reviewed"

  # Clean dataframe according to 'clean'
  if (clean == "none") {
    clean.data <- flag.data
  } else if (clean == "all") {
    # Keep only Non_QC rows
    clean.data <- dplyr::filter(flag.data, TADA.ActivityType.Flag == "Non_QC")
  } else {
    # Remove only specified QC category
    remove_flags <- switch(
      clean,
      duplicates = "QC_duplicate",
      blanks = "QC_blank",
      calibrations = "QC_calibration",
      other = "QC_other"
    )
    clean.data <- dplyr::filter(
      flag.data,
      !(TADA.ActivityType.Flag %in% remove_flags)
    )
  }
  # Determine final data based on flaggedonly
  if (isTRUE(flaggedonly)) {
    # Show only QC (non-Non_QC) rows
    final.data <- dplyr::filter(clean.data, TADA.ActivityType.Flag != "Non_QC")
    if (nrow(final.data) == 0) {
      message(
        "TADA_FindQCActivities: This dataframe is empty because either we did not find any ",
        "QC samples or because they were all removed."
      )
      # Return a zero-row dataframe with the same schema as the flagged data
      final.data <- dplyr::slice(flag.data, 0)
    }
  } else {
    final.data <- clean.data
    # Informative messages
    if (nrow(final.data) == 0) {
      message(
        "TADA_FindQCActivities: This dataframe is empty because all rows contained QC samples and were removed."
      )
    } else if (
      sum(final.data$TADA.ActivityType.Flag != "Non_QC", na.rm = TRUE) == 0
    ) {
      message(
        "TADA_FindQCActivities: Quality control samples have been removed or were not present ",
        "in the input dataframe. Returning dataframe with TADA.ActivityType.Flag column for tracking."
      )
    }
  }
  # If final.data is empty, return it after attempting to order columns
  if (nrow(final.data) == 0) {
    final.data <- tryCatch(TADA_OrderCols(final.data), error = function(e) {
      final.data
    })
    return(final.data)
  }
  # For non-empty data, proceed with ID creation and column ordering
  final.data <- TADA_CreateComparableID(final.data)
  # Guard in case downstream function returns NULL unexpectedly
  if (is.null(final.data)) {
    warning(
      "TADA_FindQCActivities: TADA_CreateComparableID returned NULL. Returning an empty dataframe."
    )
    final.data <- dplyr::slice(clean.data, 0)
    final.data <- tryCatch(TADA_OrderCols(final.data), error = function(e) {
      final.data
    })
    return(final.data)
  }
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
#' 'TADA.ResultMeasureValue', 'TADA.ComparableDataIdentifier',
#' 'TADA.ActivityDepthHeightMeasure.MeasureValue', 'TADA.ResultDepthHeightMeasure.MeasureValue',
#' 'TADA.ActivityTopDepthHeightMeasure.MeasureValue', and
#' 'TADA.ActivityBottomDepthHeightMeasure.MeasureValue'.
#' The column 'TADA.ActivityType.Flag' is optional; if missing, it will be created
#' internally using TADA_FindQCActivities(.data, clean = FALSE, flaggedonly = FALSE).
#'
#' @param type Character argument identifying which Activity Types to look for while pairing replicates
#' to their parent samples. The default type is "QC_replicate", which includes Activity Type Codes:
#' "Quality Control Field Replicate Habitat Assessment",
#' "Quality Control Field Replicate Msr/Obs",
#' "Quality Control Field Replicate Portable Data Logger",
#' "Quality Control Field Replicate Sample-Composite", and
#' "Quality Control Sample-Field Replicate".
#'
#' @param time_difference Numeric scalar defining the maximum time difference in seconds
#' to search for parent samples. The default time window is 600 seconds (10 minutes).
#' The time_difference can be as large as the user would like, but parent–replicate pairs will only be
#' identified if they were collected on the same date.
#'
#' @return If replicates of the requested type are present, this function adds one column
#' to the dataframe: 'TADA.ReplicateSampleID'. When added, it is initialized to the string
#' "NA - Not Available", matched pairs receive the replicate sample's 'ResultIdentifier',
#' and unmatched replicate rows are assigned "Orphan". If no replicates of the requested type
#' are found, the function emits a message and returns the input unchanged (no column added).
#'
#' @details
#' - Column creation is conditional: 'TADA.ReplicateSampleID' is added only when replicates
#'   of the requested type exist; if present already, it will be overwritten.
#' - If 'ActivityStartDateTime' is not POSIXct/POSIXlt, the function will attempt to parse it
#'   internally (format "%Y-%m-%d %H:%M:%S", tz = "UTC") for time-window filtering.
#'   If parsing fails (all values NA), time filtering is skipped.
#' - Time comparisons are performed in seconds using difftime(..., units = "secs").
#' - Parent–replicate pairing is only considered when 'ActivityStartDate' matches.
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

  # ensure QC flags exist
  if (!("TADA.ActivityType.Flag" %in% colnames(.data))) {
    .data <- TADA_FindQCActivities(.data, clean = FALSE, flaggedonly = FALSE)
  }

  # type checks
  TADA_CheckType(type, "character")
  TADA_CheckType(time_difference, "numeric")

  # validate time_difference
  if (length(time_difference) != 1 || is.na(time_difference)) {
    stop("time_difference must be a single non-NA numeric value (seconds).")
  }
  if (time_difference < 0) {
    stop("time_difference must be non-negative (seconds).")
  }

  # Prepare datetime vector for time filtering:
  # Use as-is if POSIXt; otherwise attempt to parse character datetimes (UTC).
  dt_vec <- .data$ActivityStartDateTime
  posix_ok <- inherits(dt_vec, "POSIXt")
  if (!posix_ok) {
    dt_parsed <- suppressWarnings(as.POSIXct(
      dt_vec,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ))
    if (any(!is.na(dt_parsed))) {
      dt_vec <- dt_parsed
      posix_ok <- TRUE
    } else {
      warning(
        "ActivityStartDateTime is not POSIXct/POSIXlt and could not be parsed; time filtering will be skipped."
      )
    }
  }

  # detect presence of replicates for the requested type
  has_reps <- if ("QC_replicate" %in% type) {
    any(.data$TADA.ActivityType.Flag == "QC_replicate", na.rm = TRUE)
  } else {
    any(.data$ActivityTypeCode %in% type, na.rm = TRUE)
  }

  # If no replicates, message and return input unchanged (no new column added)
  if (!has_reps) {
    if ("QC_replicate" %in% type) {
      message("No replicates found in dataframe. Returning input unchanged.")
    } else {
      message(paste0(
        "No replicates of type '",
        paste(type, collapse = "', '"),
        "' found in dataframe. Returning input unchanged."
      ))
    }
    return(.data)
  }

  # When replicates are present, create/initialize output column
  .data$TADA.ReplicateSampleID <- rep("NA - Not Available", nrow(.data))

  # loop through rows to find matches
  for (i in seq_len(nrow(.data))) {
    x <- if ("QC_replicate" %in% type) {
      .data$TADA.ActivityType.Flag[i]
    } else {
      .data$ActivityTypeCode[i]
    }
    if (is.na(x) || !(x %in% type)) {
      next
    }

    # match on date, location, org, comparable ID, and depths
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
            is.na(.data$TADA.ActivityTopDepthHeightMeasure.MeasureValue[i]))) &
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

    # time window filter (in seconds), only if parsed timestamps available and current row’s timestamp is non-NA
    if (posix_ok && !is.na(dt_vec[i]) && length(info_match) > 0) {
      td <- abs(difftime(dt_vec[info_match], dt_vec[i], units = "secs"))
      info_match <- info_match[as.numeric(td) <= time_difference]
    }

    # assign replicate grouping or orphan flag
    if (length(info_match) >= 2) {
      .data$TADA.ReplicateSampleID[info_match] <- .data$ResultIdentifier[i]
    } else {
      .data$TADA.ReplicateSampleID[i] <- "Orphan"
    }
  }

  .data <- TADA_OrderCols(.data)
  return(.data)
}


#' Check for results with suspect result Measure Qualifier Codes
#'
#' This function checks for and flags or removes samples denoted as suspect
#' based on the 'MeasureQualifierCode' column. The function will flag
#' suspect samples as "Suspect" and passing samples as "Pass". This function also
#' flags censored data as "Over-Detect" or "Non-Detect" for later use in the
#' censored data function, TADA_SimpleCensoredMethods.
#'
#' @param .data TADA dataframe which must include the column 'MeasureQualifierCode'
#' @param clean Boolean argument with options "TRUE" or "FALSE". The default is
#' clean = "FALSE" which does not remove any rows of data. When clean = "TRUE",
#' any rows of data flagged as "Suspect" based on the MeasureQualifierCode
#' will be removed.
#'
#' @param flaggedonly Boolean argument; the default is flaggedonly = FALSE. When
#' flaggedonly = TRUE, the function will filter the dataframe to show only the
#' rows of data flagged as Suspect.
#'
#' @param define Boolean argument; the default is define = TRUE. When define = TRUE,
#' the function will add an additional column (TADA.MeasureQualifierCode.Def) providing
#' all available definitions for the MethodQualifierCodes for each result. When
#' define = FALSE, no additional column is added.
#'
#' @return This function adds the column "TADA.MeasureQualifierCode.Flag" to the dataframe
#' which flags suspect samples based on the "MeasureQualifierCode" column. When
#' clean = "FALSE", all suspect data are kept in the dataframe. When clean = "TRUE",
#' all suspect data are removed from the dataframe. When flaggedonly = TRUE, the dataframe
#' is filtered to show only the suspect data. When flaggedonly = FALSE, the full,
#' cleaned dataframe is returned. The default is clean = FALSE and flaggedonly = FALSE.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#'
#' # Flag and keep all suspect samples:
#' MeasureQualifierCode_flagged <-
#'   TADA_FlagMeasureQualifierCode(Data_Nutrients_UT)
#'
#' # Flag suspect samples and filter to suspect data only:
#' MeasureQualifierCode_flags_only <- TADA_FlagMeasureQualifierCode(
#'   Data_Nutrients_UT,
#'   flaggedonly = TRUE
#' )
#'
#' # Remove all suspect samples:
#' MeasureQualifierCode_clean <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT,
#'   clean = TRUE
#' )
#'
#' # Remove all suspect samples and DO NOT include a new column with
#' # qualifier definitions (TADA.MeasureQualifierCode.Def):
#' MeasureQualifierCode_clean_nodefs <- TADA_FlagMeasureQualifierCode(
#'   Data_Nutrients_UT,
#'   clean = TRUE, define = FALSE
#' )
#'
TADA_FlagMeasureQualifierCode <- function(
  .data,
  clean = FALSE,
  flaggedonly = FALSE,
  define = TRUE
) {
  # Check if the input .data is NULL
  if (is.null(.data)) {
    warning(
      "TADA_FindQCActivities: The input dataframe is NULL. Please provide a valid dataframe."
    )
    return(NULL)
  }
  # check .data is data.frame and has required columns
  TADA_CheckColumns(.data, "MeasureQualifierCode")
  # check that clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check .data MeasureQualifierCode is not all NA. If it is, don't run function and return .data
  if (all(is.na(.data$MeasureQualifierCode))) {
    print(
      "TADA_FlagMeasureQualifierCode: Dataframe does not include any information (all NA's) in MeasureQualifierCode."
    )

    .data <- .data |>
      dplyr::mutate(TADA.MeasureQualifierCode.Flag = "Pass") |>
      dplyr::mutate(TADA.MeasureQualifierCode.Def = "NA - Not Applicable")

    .data <- TADA_OrderCols(.data)

    return(.data)
  }

  # execute function after checks are passed
  # delete existing flag column
  if (("TADA.MeasureQualifierCode.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.MeasureQualifierCode.Flag)
  }

  # load in ResultMeasureQualifier Flag Table
  qc.ref <- utils::read.csv(system.file(
    "extdata",
    "WQXMeasureQualifierCodeRef.csv",
    package = "EPATADA"
  )) |>
    dplyr::rename(MeasureQualifierCode = Code) |>
    dplyr::select(
      MeasureQualifierCode,
      TADA.MeasureQualifierCode.Flag,
      Description
    )

  # add TADA.MeasureQualifierCode, qualifier code definitions
  # Create TADA.MeasureQualifierCode by concatenating MeasureQualifierCode with description from MeasureQualifierCodeRef.
  if (define == FALSE) {
    .data <- .data
  }

  if (define == TRUE) {
    mqc.ref <- qc.ref |>
      dplyr::select(MeasureQualifierCode, Description) |>
      dplyr::group_by(MeasureQualifierCode) |>
      dplyr::mutate(
        Concat = paste(MeasureQualifierCode, "-", Description, collapse = "")
      ) |>
      dplyr::select(MeasureQualifierCode, Concat)

    mqc.TADA <- .data |>
      dplyr::mutate(
        MeasureQualifierCode = stringr::str_split(MeasureQualifierCode, ";")
      ) |>
      tidyr::unnest(MeasureQualifierCode) |>
      merge(mqc.ref) |>
      dplyr::group_by(ResultIdentifier) |>
      dplyr::summarize(
        TADA.MeasureQualifierCode.Def = paste(Concat, collapse = "; ")
      )

    .data$TADA.MeasureQualifierCode.Def <- mqc.TADA$TADA.MeasureQualifierCode.Def[match(
      .data$ResultIdentifier,
      mqc.TADA$ResultIdentifier
    )]

    rm(mqc.ref, mqc.TADA)
  }

  # Populate flag column in data
  flag.lists <- split(
    qc.ref$MeasureQualifierCode,
    qc.ref$TADA.MeasureQualifierCode.Flag
  )

  # Set names with transformations
  names(flag.lists) <- tolower(names(flag.lists))
  names(flag.lists) <- stringr::str_remove_all(names(flag.lists), "-")
  names(flag.lists) <- stringr::str_remove_all(names(flag.lists), " ")

  flag.data <- .data |>
    dplyr::mutate(
      MeasureQualifierCode.Split = strsplit(MeasureQualifierCode, ";"),
      TADA.MeasureQualifierCode.Flag = purrr::map_chr(
        MeasureQualifierCode.Split,
        ~ dplyr::case_when(
          any(.x %in% flag.lists$suspect) ~ "Suspect",
          any(.x %in% flag.lists$nondetect) ~ "Non-Detect",
          any(.x %in% flag.lists$overdetect) ~ "Over-Detect",
          any(.x %in% flag.lists$pass) ~ "Pass",
          any(.x %in% flag.lists$notreviewed) ~ "Not Reviewed",
          TRUE ~ NA_character_
        )
      )
    ) |>
    dplyr::select(-MeasureQualifierCode.Split)

  flag.data <- flag.data |> dplyr::distinct()

  # identify any ResultMeasureQualifier Codes not in reference table
  codes <- stringr::str_split(unique(.data$MeasureQualifierCode), ";") |>
    unlist() |>
    unique()

  if (any(!codes %in% qc.ref$MeasureQualifierCode)) {
    missing_codes <- codes[!codes %in% qc.ref$MeasureQualifierCode]
    missing_codes_df <- data.frame(
      MeasureQualifierCode = missing_codes,
      TADA.MeasureQualifierCode.Flag = "Not Reviewed",
      Description = ""
    )
    qc.ref <- rbind(qc.ref, missing_codes_df)
    missing_codes <- paste(missing_codes, collapse = ", ")
    print(paste0(
      "TADA_FlagMeasureQualifierCode: MeasureQualifierCode column in dataset contains value(s) ",
      missing_codes,
      " which is/are not represented in the MeasureQualifierCode WQX domain table. These data records are placed under the TADA.MeasureQualifierCode.Flag: 'uncategorized'. Please contact TADA administrators to resolve."
    ))
  }

  ## rename ResultMeasureQualifier NA values to Pass in TADA.MeasureQualifierCode.Flag column (no longer needed cm 1/4/24)
  # flag.data["TADA.MeasureQualifierCode.Flag"][is.na(flag.data["MeasureQualifierCode"])] <- "Pass"

  # clean dataframe
  # if clean = FALSE, return full dataframe
  if (clean == FALSE) {
    clean.data <- flag.data
  }
  # if clean = TRUE, remove flagged data
  if (clean == TRUE) {
    clean.data <- dplyr::filter(
      flag.data,
      flag.data$TADA.MeasureQualifierCode.Flag != "Suspect"
    )
  }

  # if flaggedonly = FALSE, return full clean dataframe
  if (flaggedonly == FALSE) {
    final.data <- clean.data
    # if the dataframe is empty, print message
    if (nrow(final.data) == 0) {
      print(
        "TADA_FlagMeasureQualifierCode: This dataframe is empty because all rows contained Suspect samples that were removed"
      )
    }
    # if there are no flags, print message
    if (sum(final.data$TADA.MeasureQualifierCode.Flag != "Suspect") == 0) {
      print(
        "TADA_FlagMeasureQualifierCode: Suspect samples have been removed or were not present in the input dataframe. Returning dataframe with TADA.MeasureQualifierCode.Flag column for tracking."
      )
    }
  }

  # if flaggedonly = TRUE, return clean dataframe filtered to only the flagged rows
  if (flaggedonly == TRUE) {
    final.data <- dplyr::filter(
      clean.data,
      TADA.MeasureQualifierCode.Flag == "Suspect"
    )

    # if the dataframe is empty, print message
    if (nrow(final.data) == 0) {
      print(
        "TADA_FlagMeasureQualifierCode: This dataframe is empty because either we did not find any Suspect samples or because they were all removed"
      )
    }
  }

  final.data <- TADA_OrderCols(final.data)
  # return final dataframe
  return(final.data)
}
