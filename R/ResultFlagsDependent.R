#' Check Sample Fraction Validity
#'
#' Function checks the validity of each characteristic-fraction combination
#' in the dataframe. When clean = TRUE, rows with invalid characteristic-fraction
#' combinations are removed. Default is clean = TRUE. When errorsonly = TRUE, only
#' invalid characteristic-fraction combinations are returned. Default is errorsonly = FALSE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-fraction
#' combinations from the dataframe when clean = TRUE. Default is clean = TRUE.
#' @param errorsonly Boolean argument; filters to show only the "Invalid"
#' characteristic-fraction combinations in the dataframe when errorsonly = TRUE.
#' Default is errorsonly = FALSE.
#'
#' @return This function adds the following column to your dataframe:
#'   TADA.SampleFraction.Flag, which flags each CharacteristicName and
#'   ResultSampleFractionText combination in your dataframe as either
#'   "Nonstandardized", "Invalid", or "Valid". When clean = FALSE and errorsonly
#'   = TRUE, the data are filtered to show the "Invalid" rows only. When clean =
#'   TRUE and errorsonly = FALSE, "Invalid" rows are removed from the dataframe
#'   and no column will be appended. When clean = TRUE and errorsonly = TRUE,
#'   the function does not execute and an error message is returned.
#'
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Remove data with invalid characteristic-fraction combinations:
#' InvalidFraction_clean <- TADA_InvalidFraction(Nutrients_Utah)
#' 
#' # Flag, but do not remove, data with invalid characteristic-fraction combinations
#' # in new column titled "TADA.SampleFraction.Flag":
#' InvalidFraction_flags <- TADA_InvalidFraction(Nutrients_Utah, clean = FALSE)
#' 
#' # Show only invalid characteristic-fraction combinations:
#' InvalidFraction_errorsonly <- TADA_InvalidFraction(Nutrients_Utah, clean = FALSE, errorsonly = TRUE)
#' 

TADA_InvalidFraction <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText"))
  # check that both clean and errorsonly are not TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }
  
  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.SampleFraction.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.SampleFraction.Flag)
  }
  # read in sample fraction reference table from extdata and filter
  frac.ref <- TADA_GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicFraction")
  
  # join "Status" column to .data by CharacteristicName and Value (SampleFraction)
  check.data <- merge(.data, frac.ref[, c("Characteristic", "Status", "Value")],
                      by.x = c(
                        "TADA.CharacteristicName",
                        "TADA.ResultSampleFractionText"
                      ),
                      by.y = c("Characteristic", "Value"), all.x = TRUE
  )
  
  # rename Status column
  check.data <- check.data %>%
    dplyr::rename(TADA.SampleFraction.Flag = Status) %>% dplyr::distinct()
  # rename NA values to Nonstandardized in TADA.SampleFraction.Flag column
  check.data["TADA.SampleFraction.Flag"][is.na(check.data["TADA.SampleFraction.Flag"])] <- "Nonstandardized"
  
  # if all rows are "Valid", return input unchanged
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.SampleFraction.Flag)) == FALSE) {
    if (errorsonly == FALSE) {
      print("All characteristic/fraction combinations are valid in your dataframe. Returning input dataframe with TADA.SampleFraction.Flag column for tracking.")
      check.data = TADA_OrderCols(check.data)
      return(check.data)
    }
    if (errorsonly == TRUE) {
      print("This dataframe is empty because we did not find any invalid fraction/characteristic combinations in your dataframe")
      empty.data <- dplyr::filter(check.data, TADA.SampleFraction.Flag == "Invalid")
      # empty.data <- dplyr::select(empty.data, -TADA.SampleFraction.Flag)
      empty.data = TADA_OrderCols(empty.data)
      return(empty.data)
    }
  }
  
  # flagged output, all data
  if (clean == FALSE & errorsonly == FALSE) {
    print("Rows with invalid sample fractions have been flagged but retained. Review these rows before proceeding and/or set clean = TRUE.")
    check.data = TADA_OrderCols(check.data)
    return(check.data)
      }
  
  # clean output
  if (clean == TRUE & errorsonly == FALSE) {
    # filter out invalid characteristic-fraction combinations
    clean.data <- dplyr::filter(check.data, TADA.SampleFraction.Flag != "Invalid")
    
    # remove WQX.SampleFractionValidity column
    # clean.data <- dplyr::select(clean.data, -TADA.SampleFraction.Flag)
    clean.data = TADA_OrderCols(clean.data)
    return(clean.data)
  }
  
  # flagged output, errors only
  if (clean == FALSE & errorsonly == TRUE) {
    # filter out valid characteristic-fraction combinations
    invalid.data <- dplyr::filter(check.data, TADA.SampleFraction.Flag == "Invalid")
    invalid.data = TADA_OrderCols(invalid.data)
    return(invalid.data)
  }
}

#' Check Method Speciation Validity
#'
#' Function checks the validity of each characteristic-method
#' speciation combination in the dataframe. When clean = "invalid_only", rows with invalid
#' characteristic-method speciation combinations are removed. Default is
#' clean = "invalid_only". When errorsonly = TRUE, dataframe is filtered to show only
#' rows with invalid or nonstandardized characteristic-method speciation combinations. 
#' Default is errorsonly = FALSE.
#'
#' @param .data TADA dataframe
#' @param clean Character argument with options "invalid_only", "nonstandardized_only", 
#' "both", or "none." The default is clean = "invalid_only" which removes rows of
#' data flagged as having "Invalid" characteristic-method speciation combinations. When
#' clean = "nonstandardized_only", the function removes rows of data flagged as
#' having "Nonstandardized" characteristic-method speciation combinations. When 
#' clean = "both", the function removes rows of data flagged as either "Invalid" or
#' "Nonstandardized". And when clean = "none", the function does not remove any "Invalid"
#' or "Nonstandardized" rows of data.
#' @param errorsonly Boolean argument; filters to show only the "Invalid" 
#' characteristic-method speciation combinations from the dataframe when 
#' errorsonly = TRUE. Default is errorsonly = FALSE.
#'
#' @return This function adds TADA.MethodSpeciation.Flag to the dataframe. This column 
#' flags each TADA.CharacteristicName and MethodSpecificationName combination in your 
#' dataframe as either "Nonstandardized", "Invalid", or "Valid". When clean = "none" 
#' and errorsonly = TRUE, the dataframe is filtered to show only the "Invalid" and
#' "Nonstandardized data; the column TADA.MethodSpeciation.Flag is still appended. 
#' When clean = "invalid_only" and errorsonly = FALSE, "Invalid" rows are removed 
#' from the dataframe, but "Nonstandardized" rows are retained. When
#' clean = "nonstandardized_only" and errorsonly = FALSE, "Nonstandardized" rows
#' are removed, but "Invalid" rows are retained. The default is clean = "invalid_only"
#' and errorsonly = FALSE.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Remove data with invalid characteristic-method speciation combinations from dataframe, 
#' # but retain nonstandardized combinations flagged in new column 'TADA.MethodSpeciation.Flag':
#' InvalidSpeciation_clean <- TADA_InvalidSpeciation(Nutrients_Utah)
#' 
#' # Remove data with nonstandardized characteristic-method speciation combinations 
#' # from dataframe but retain invalid combinations flagged in new column 'TADA.MethodSpeciation.Flag':
#' NonstandardSpeciation_clean <- TADA_InvalidSpeciation(Nutrients_Utah, clean = "nonstandardized_only")
#' 
#' # Remove both invalid and nonstandardized characteristic-method speciation combinations
#' # from dataframe:
#' Speciation_clean <- TADA_InvalidSpeciation(Nutrients_Utah, clean = "both")
#' 
#' # Flag, but do not remove, data with invalid or nonstandardized characteristic-method speciation
#' # combinations in new column titled "TADA.MethodSpeciation.Flag":
#' InvalidSpeciation_flags <- TADA_InvalidSpeciation(Nutrients_Utah, clean = "none")
#' 
#' # Show only invalid characteristic-method speciation combinations:
#' InvalidSpeciation_errorsonly <- TADA_InvalidSpeciation(Nutrients_Utah, clean = "nonstandardized_only", errorsonly = TRUE)
#' 
#' # Show only nonstandardized characteristic-method speciation combinations:
#' NonstandardSpeciation_errorsonly <- TADA_InvalidSpeciation(Nutrients_Utah, clean = "invalid_only", errorsonly = TRUE)
#' 

TADA_InvalidSpeciation <- function(.data, clean = c("invalid_only", "nonstandardized_only", "both", "none"), errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "character")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, c("TADA.CharacteristicName", "TADA.MethodSpecificationName"))
  # check that clean is either "invalid_only", "nonstandardized_only", "both", or "none"
  clean <- match.arg(clean)

  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.MethodSpeciation.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.MethodSpeciation.Flag)
  }
  
  # read in speciation reference table from extdata and filter
  spec.ref <- TADA_GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicSpeciation")
  
  # join "Status" column to .data by CharacteristicName and Value (Speciation)
  check.data <- merge(.data, spec.ref[, c("Characteristic", "Status", "Value")],
                      by.x = c("TADA.CharacteristicName", "TADA.MethodSpecificationName"),
                      by.y = c("Characteristic", "Value"), all.x = TRUE
  )
  
  # rename Status column
  check.data <- check.data %>%
    dplyr::rename(TADA.MethodSpeciation.Flag = Status) %>% dplyr::distinct()
  # rename NA values to Nonstandardized in TADA.MethodSpeciation.Flag column
  check.data["TADA.MethodSpeciation.Flag"][is.na(check.data["TADA.MethodSpeciation.Flag"])] <- "Nonstandardized"
  
  # if all rows are "Valid", return input with flag column
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.MethodSpeciation.Flag)) == FALSE) {
    print("All characteristic/method speciation combinations are valid in your dataframe. Returning input dataframe with TADA.MethodSpeciation.Flag column for tracking.")
    check.data = TADA_OrderCols(check.data)
    return(check.data)
  }
  
  # flagged output, all data
  if (clean == "none" & errorsonly == FALSE) {
    print("Rows with invalid speciations have been flagged but retained. Review these rows before proceeding and/or set clean = 'invalid_only' or 'both'.")
  }
  
  # when clean = "invalid_only"
  if (clean == "invalid_only") {
    # filter out only "Invalid" characteristic-method speciation combinations
    clean.data <- dplyr::filter(check.data, TADA.MethodSpeciation.Flag != "Invalid")
  }
  
  # when clean = "nonstandardized_only"
  if (clean == "nonstandardized_only") {
    # filter out only "Nonstandardized" characteristic-method speciation combinations
    clean.data <- dplyr::filter(check.data, TADA.MethodSpeciation.Flag != "Nonstandardized")
  }
  
  # when clean = "both"
  if (clean == "both") {
    # filter out both "Invalid" and "Nonstandardized" characteristic-method speciation combinations
    clean.data <- dplyr::filter(check.data, TADA.MethodSpeciation.Flag != "Nonstandardized" & TADA.MethodSpeciation.Flag != "Invalid")
  }
  
  # when clean = "none"
  if (clean == "none") {
    # retain all data
    clean.data <- check.data
  }
  
  # when errorsonly = FALSE
  if (errorsonly == FALSE) {
    clean.data = TADA_OrderCols(clean.data)
    return(clean.data)
  }
  
  # when errorsonly = TRUE
  if (errorsonly == TRUE) {
    # filter to show only invalid and/or nonstandardized characteristic-method speciation combinations
    error.data <- dplyr::filter(clean.data, TADA.MethodSpeciation.Flag == "Invalid" | TADA.MethodSpeciation.Flag == "Nonstandardized")
    # if there are no errors
    if (nrow(error.data) == 0) {
      print("This dataframe is empty because either we did not find any invalid/nonstandardized characteristic-method speciation combinations or they were all filtered out")
      # error.data <- dplyr::select(error.data, -TADA.MethodSpeciation.Flag)
    }
    error.data = TADA_OrderCols(error.data)
    return(error.data)
  }
}



#' Check Result Unit Validity
#'
#' Function checks the validity of each characteristic-media-result unit
#' combination in the dataframe. When clean = "invalid_only", rows with invalid
#' characteristic-media-result unit combinations are removed. Default is
#' clean = "invalid_only". When errorsonly = TRUE, dataframe is filtered to show only
#' rows with invalid or nonstandardized characteristic-media-result unit combinations. 
#' Default is errorsonly = FALSE.
#'
#' @param .data TADA dataframe
#' @param clean Character argument with options "invalid_only", "nonstandardized_only", 
#' "both", or "none." The default is clean = "invalid_only" which removes rows of
#' data flagged as having "Invalid" characteristic-media-result unit combinations. When
#' clean = "nonstandardized_only", the function removes rows of data flagged as
#' having "Nonstandardized" characteristic-media-result unit combinations. When 
#' clean = "both", the function removes rows of data flagged as either "Invalid" or
#' "Nonstandardized". And when clean = "none", the function does not remove any "Invalid"
#' or "Nonstandardized" rows of data.
#' @param errorsonly Boolean argument; filters dataframe to show only "Invalid"
#' characteristic-media-result unit combinations when errorsonly = TRUE. Default
#' is errorsonly = FALSE.
#'
#' @return This function adds the TADA.ResultUnit.Flag to a TADA dataframe. This column 
#' flags each CharacteristicName, ActivityMediaName, and ResultMeasure/MeasureUnitCode
#' combination in your dataframe as either "Nonstandardized", "Invalid", or "Valid". 
#' When clean = "none" and errorsonly = TRUE, the dataframe is filtered to show only 
#' the "Invalid" and "Nonstandardized data; the column TADA.ResultUnit.Flag is 
#' still appended. When clean = "invalid_only" and errorsonly = FALSE, "Invalid" 
#' rows are removed from the dataframe, but "Nonstandardized" rows are retained. When
#' clean = "nonstandardized_only" and errorsonly = FALSE, "Nonstandardized" rows
#' are removed, but "Invalid" rows are retained. The default is clean = "invalid_only"
#' and errorsonly = FALSE.
#'
#' @export
#'
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Remove data with invalid characteristic-media-result unit combinations from dataframe, 
#' # but retain nonstandardized combinations flagged in new column 'TADA.ResultUnit.Flag':
#' InvalidUnit_clean <- TADA_InvalidResultUnit(Nutrients_Utah)
#' 
#' # Remove data with nonstandardized characteristic-media-result unit combinations 
#' # from dataframe but retain invalid combinations flagged in new column 'TADA.ResultUnit.Flag:
#' NonstandardUnit_clean <- TADA_InvalidResultUnit(Nutrients_Utah, clean = "nonstandardized_only")
#' 
#' # Remove both invalid and nonstandardized characteristic-media-result unit combinations
#' # from dataframe:
#' ResultUnit_clean <- TADA_InvalidResultUnit(Nutrients_Utah, clean = "both")
#' 
#' # Flag, but do not remove, data with invalid or nonstandardized characteristic-media-result unit
#' # combinations in new column titled "TADA.ResultUnit.Flag":
#' InvalidUnit_flags <- TADA_InvalidResultUnit(Nutrients_Utah, clean = "none")
#' 
#' # Show only invalid characteristic-media-result unit combinations:
#' InvalidUnit_errorsonly <- TADA_InvalidResultUnit(Nutrients_Utah, clean = "nonstandardized_only", errorsonly = TRUE)
#' 
#' # Show only nonstandardized characteristic-media-result unit combinations:
#' NonstandardUnit_errorsonly <- TADA_InvalidResultUnit(Nutrients_Utah, clean = "invalid_only", errorsonly = TRUE)

TADA_InvalidResultUnit <- function(.data, clean = c("invalid_only", "nonstandardized_only", "both", "none"), errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is character
  TADA_CheckType(clean, "character")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, c("TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode", "TADA.ActivityMediaName"))
  # check that clean is either "invalid_only", "nonstandardized_only", "both", or "none"
  clean <- match.arg(clean)

  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.ResultUnit.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ResultUnit.Flag)
  }
  
  # read in unit reference table from extdata and filter
  unit.ref <- TADA_GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit")
  
  # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
  check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
                      by.x = c("TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode", "TADA.ActivityMediaName"),
                      by.y = c("Characteristic", "Value", "Source"), all.x = TRUE
  )
  
  # rename Status column
  check.data <- check.data %>%
    dplyr::rename(TADA.ResultUnit.Flag = Status) %>% dplyr::distinct()
  # rename NA values to Nonstandardized in WQX.ResultUnitValidity column
  check.data["TADA.ResultUnit.Flag"][is.na(check.data["TADA.ResultUnit.Flag"])] <- "Nonstandardized"
  
  # if all rows are "Valid", return input with flag column
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.ResultUnit.Flag)) == FALSE) {
    print("All characteristic/unit combinations are valid in your dataframe. Returning input dataframe with TADA.ResultUnit.Flag column for tracking.")
    check.data = TADA_OrderCols(check.data)
    return(check.data)
  }
  
  # flagged output, all data
  if (clean == "none" & errorsonly == FALSE) {
    print("Rows with invalid result value units have been flagged but retained. Review these rows before proceeding and/or set clean = 'invalid_only' or 'both'.")
  }
  
  # when clean = "invalid_only"
  if (clean == "invalid_only") {
    # filter out only "Invalid" characteristic-method speciation combinations
    clean.data <- dplyr::filter(check.data, TADA.ResultUnit.Flag != "Invalid")
  }
  
  # when clean = "nonstandardized_only"
  if (clean == "nonstandardized_only") {
    # filter out only "Nonstandardized" characteristic-method speciation combinations
    clean.data <- dplyr::filter(check.data, TADA.ResultUnit.Flag != "Nonstandardized")
  }
  
  # when clean = "both"
  if (clean == "both") {
    # filter out both "Invalid" and "Nonstandardized" characteristic-method speciation combinations
    clean.data <- dplyr::filter(check.data, TADA.ResultUnit.Flag != "Nonstandardized" & TADA.ResultUnit.Flag != "Invalid")
  }
  
  # when clean = "none"
  if (clean == "none") {
    # retain all data
    clean.data <- check.data
  }
  
  # when errorsonly = FALSE
  if (errorsonly == FALSE) {
    clean.data = TADA_OrderCols(clean.data)
    return(clean.data)
  }
  
  # when errorsonly = TRUE
  if (errorsonly == TRUE) {
    # filter to show only invalid and/or nonstandardized characteristic-method speciation combinations
    error.data <- dplyr::filter(clean.data, TADA.ResultUnit.Flag == "Invalid" | TADA.ResultUnit.Flag == "Nonstandardized")
    # if there are no errors
    if (nrow(error.data) == 0) {
      print("This dataframe is empty because either we did not find any invalid/nonstandardized characteristic-media-result unit combinations or they were all filtered out")
      # error.data <- dplyr::select(error.data, -TADA.ResultUnit.Flag)
    }
    error.data = TADA_OrderCols(error.data)
    return(error.data)
  }
}


#' Check for Quality Control Samples
#' 
#' This function checks for and flags or removes samples denoted as quality control 
#' activities based on the 'ActivityTypeCode' column. The function will flag 
#' duplicate samples as "QC_duplicate", blank samples as "QC_blank", calibration
#' or spiked samples as "QC_calibration", and other QC samples as "QC_other". 
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
#' @param errorsonly Boolean argument; the default is errorsonly = FALSE. When
#' errorsonly = TRUE, the function will filter the dataframe to show only the
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
#' samples are removed from the dataframe. When errorsonly = TRUE, the dataframe 
#' is filtered to show only the flagged data. When errorsonly = FALSE, the full,
#' cleaned dataframe is returned. The default is clean = "none" and errorsonly = FALSE.
#' 
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Flag and keep all QC samples:
#' QC_flagged <- TADA_QualityControlActivity(Nutrients_Utah)
#' 
#' # Flag QC samples and filter to flagged data only:
#' QC_flags_only <- TADA_QualityControlActivity(Nutrients_Utah, errorsonly = TRUE)
#' 
#' # Remove all QC samples:
#' QC_clean <- TADA_QualityControlActivity(Nutrients_Utah, clean = TRUE)

TADA_QualityControlActivity <- function(.data, clean = FALSE, errorsonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check that clean is boolean
  TADA_CheckType(clean, "logical")
  # check errorsonly is boolean
  TADA_CheckType(errorsonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, c("ActivityTypeCode"))

  # execute function after checks are passed
  # delete existing flag column
  if (("TADA.ActivityType.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ActivityType.Flag)
  }
  
  # load in ActivityTypeRef Table
  qc.ref <- TADA_GetActivityTypeRef() %>%
    dplyr::rename(ActivityTypeCode = Code) %>%
    dplyr::select(ActivityTypeCode, TADA.ActivityType.Flag)
  
  # identify any Activity Type Codes not in reference table
  codes = unique(.data$ActivityTypeCode)
  if(any(!codes %in% qc.ref$ActivityTypeCode)){
    missing_codes = codes[!codes %in% qc.ref$ActivityTypeCode]
    missing_codes_df <- data.frame(ActivityTypeCode = missing_codes,
                                   TADA.ActivityType.Flag = "QC_uncategorized")
    qc.ref <- rbind(qc.ref, missing_codes_df)
    missing_codes = paste(missing_codes, collapse = ", ")
    print(paste0("ActivityTypeCode column in dataset contains value(s) ",missing_codes, " which is/are not represented in the ActivityType WQX domain table. These data records are placed under the TADA.ActivityType.Flag: 'QC_uncategorized'. Please contact TADA administrators to resolve."))
  }
  
  # populate flag column in data
  flag.data <- dplyr::left_join(.data, qc.ref, by = "ActivityTypeCode")
  flag.data = flag.data %>% dplyr::distinct()
  
  # clean dataframe
  # if clean = FALSE, return full dataframe
  if(clean == FALSE) {
    clean.data <- flag.data
  } 
  # if clean = TRUE, remove flagged data
  if(clean == TRUE) {
    clean.data <- dplyr::filter(flag.data, flag.data$TADA.ActivityType.Flag == "Non_QC")
  }

  # if errorsonly = FALSE, return full clean dataframe
  if(errorsonly == FALSE) {
    final.data <- clean.data
    # if the dataframe is empty, print message
    if(nrow(final.data) == 0) {
      print("This dataframe is empty because all rows contained QC samples and were removed")
    }
    # if there are no flags, print message
    if(sum(final.data$TADA.ActivityType.Flag != "Non_QC") == 0) {
      print("Quality control samples have been removed or were not present in the input dataframe. Returning dataframe with TADA.ActivityType.Flag column for tracking.")
    }
  }
  
  # if errorsonly = TRUE, return clean dataframe filtered to only the flagged rows
  if(errorsonly == TRUE) {
    final.data <- clean.data[!is.na(clean.data$TADA.ActivityType.Flag),]
    # if the dataframe is empty, print message
    if(nrow(final.data) == 0) {
      print("This dataframe is empty because either we did not find any QC samples or because they were all removed")
    }
  }
  
  # return final dataframe
  return(final.data)
}