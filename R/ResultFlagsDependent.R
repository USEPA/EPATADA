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
#' @return When clean = FALSE and errorsonly = FALSE, this function adds the
#' following column to your dataframe: TADA.SampleFraction.Flag. This column flags each 
#' CharacteristicName and ResultSampleFractionText combination in your dataframe 
#' as either "Nonstandardized", "Invalid", or "Valid". When clean = FALSE and 
#' errorsonly = TRUE, the TADA.SampleFraction.Flag is still added and the data
#' is filtered to only show the "Invalid" rows only. When clean = TRUE and
#' errorsonly = FALSE, "Invalid" rows are removed from the dataframe and no 
#' column will be appended. When clean = TRUE and errorsonly = TRUE, the function
#' does not execute and an error message is returned.
#'
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Remove data with invalid characteristic-fraction combinations:
#' InvalidFraction_clean <- InvalidFraction(Nutrients_Utah)
#' 
#' # Flag, but do not remove, data with invalid characteristic-fraction combinations
#' # in new column titled "TADA.SampleFraction.Flag":
#' InvalidFraction_flags <- InvalidFraction(Nutrients_Utah, clean = FALSE)
#' 
#' # Show only invalid characteristic-fraction combinations:
#' InvalidFraction_errorsonly <- InvalidFraction(Nutrients_Utah, clean = FALSE, errorsonly = TRUE)
#' 

InvalidFraction <- function(.data, clean = TRUE, errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "logical")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  checkColumns(.data, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText"))
  # check that both clean and errorsonly are not TRUE
  if (clean == TRUE & errorsonly == TRUE) {
    stop("Function not executed because clean and errorsonly cannot both be TRUE")
  }
  
  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.SampleFraction.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.SampleFraction.Flag)
  }
  # read in sample fraction reference table from extdata and filter
  frac.ref <- GetWQXCharValRef() %>%
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
    dplyr::rename(TADA.SampleFraction.Flag = Status)
  # rename NA values to Nonstandardized in TADA.SampleFraction.Flag column
  check.data["TADA.SampleFraction.Flag"][is.na(check.data["TADA.SampleFraction.Flag"])] <- "Nonstandardized"
  
  # if all rows are "Valid", return input unchanged
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.SampleFraction.Flag)) == FALSE) {
    if (errorsonly == FALSE) {
      print("All characteristic/fraction combinations are valid in your dataframe. Returning input dataframe with TADA.SampleFraction.Flag column for tracking.")
      check.data = OrderTADACols(check.data)
      return(check.data)
    }
    if (errorsonly == TRUE) {
      print("This dataframe is empty because we did not find any invalid fraction/characteristic combinations in your dataframe")
      empty.data <- dplyr::filter(check.data, TADA.SampleFraction.Flag == "Invalid")
      # empty.data <- dplyr::select(empty.data, -TADA.SampleFraction.Flag)
      empty.data = OrderTADACols(empty.data)
      return(empty.data)
    }
  }
  
  # flagged output, all data
  if (clean == FALSE & errorsonly == FALSE) {
    warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' fraction. Check these records before proceeding with transformations and/or set clean = TRUE.")
    check.data = OrderTADACols(check.data)
    return(check.data)
      }
  
  # clean output
  if (clean == TRUE & errorsonly == FALSE) {
    # filter out invalid characteristic-fraction combinations
    clean.data <- dplyr::filter(check.data, TADA.SampleFraction.Flag != "Invalid")
    
    # remove WQX.SampleFractionValidity column
    # clean.data <- dplyr::select(clean.data, -TADA.SampleFraction.Flag)
    clean.data = OrderTADACols(clean.data)
    return(clean.data)
  }
  
  # flagged output, errors only
  if (clean == FALSE & errorsonly == TRUE) {
    # filter out valid characteristic-fraction combinations
    invalid.data <- dplyr::filter(check.data, TADA.SampleFraction.Flag == "Invalid")
    invalid.data = OrderTADACols(invalid.data)
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
#' @return When clean = "none" and errorsonly = FALSE, this function adds the 
#' following column to your dataframe: TADA.MethodSpeciation.Flag. This column 
#' flags each CharacteristicName and MethodSpecificationName combination in your 
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
#' InvalidSpeciation_clean <- InvalidSpeciation(Nutrients_Utah)
#' 
#' # Remove data with nonstandardized characteristic-method speciation combinations 
#' # from dataframe but retain invalid combinations flagged in new column 'TADA.MethodSpeciation.Flag':
#' NonstandardSpeciation_clean <- InvalidSpeciation(Nutrients_Utah, clean = "nonstandardized_only")
#' 
#' # Remove both invalid and nonstandardized characteristic-method speciation combinations
#' # from dataframe:
#' Speciation_clean <- InvalidSpeciation(Nutrients_Utah, clean = "both")
#' 
#' # Flag, but do not remove, data with invalid or nonstandardized characteristic-method speciation
#' # combinations in new column titled "TADA.MethodSpeciation.Flag":
#' InvalidSpeciation_flags <- InvalidSpeciation(Nutrients_Utah, clean = "none")
#' 
#' # Show only invalid characteristic-method speciation combinations:
#' InvalidSpeciation_errorsonly <- InvalidSpeciation(Nutrients_Utah, clean = "nonstandardized_only", errorsonly = TRUE)
#' 
#' # Show only nonstandardized characteristic-method speciation combinations:
#' NonstandardSpeciation_errorsonly <- InvalidSpeciation(Nutrients_Utah, clean = "invalid_only", errorsonly = TRUE)
#' 

InvalidSpeciation <- function(.data, clean = c("invalid_only", "nonstandardized_only", "both", "none"), errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is boolean
  checkType(clean, "character")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  checkColumns(.data, c("TADA.CharacteristicName", "TADA.MethodSpecificationName"))
  # check that clean is either "invalid_only", "nonstandardized_only", "both", or "none"
  clean <- match.arg(clean)

  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.MethodSpeciation.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.MethodSpeciation.Flag)
  }
  
  # read in speciation reference table from extdata and filter
  spec.ref <- GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicSpeciation")
  
  # join "Status" column to .data by CharacteristicName and Value (Speciation)
  check.data <- merge(.data, spec.ref[, c("Characteristic", "Status", "Value")],
                      by.x = c("TADA.CharacteristicName", "TADA.MethodSpecificationName"),
                      by.y = c("Characteristic", "Value"), all.x = TRUE
  )
  
  # rename Status column
  check.data <- check.data %>%
    dplyr::rename(TADA.MethodSpeciation.Flag = Status)
  # rename NA values to Nonstandardized in TADA.MethodSpeciation.Flag column
  check.data["TADA.MethodSpeciation.Flag"][is.na(check.data["TADA.MethodSpeciation.Flag"])] <- "Nonstandardized"
  
  # if all rows are "Valid", return input with flag column
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.MethodSpeciation.Flag)) == FALSE) {
    print("All characteristic/method speciation combinations are valid in your dataframe. Returning input dataframe with TADA.MethodSpeciation.Flag column for tracking.")
    check.data = OrderTADACols(check.data)
    return(.data)
  }
  
  # flagged output, all data
  if (clean == "none" & errorsonly == FALSE) {
    warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' speciation. Check these records before proceeding with transformations and/or set clean = 'invalid_only' or 'both'.")
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
    clean.data = OrderTADACols(clean.data)
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
    error.data = OrderTADACols(error.data)
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
#' @return When clean = "none" and errorsonly = FALSE, this function adds the 
#' following column to your dataframe: TADA.ResultUnit.Flag. This column 
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
#' InvalidUnit_clean <- InvalidResultUnit(Nutrients_Utah)
#' 
#' # Remove data with nonstandardized characteristic-media-result unit combinations 
#' # from dataframe but retain invalid combinations flagged in new column 'TADA.ResultUnit.Flag:
#' NonstandardUnit_clean <- InvalidResultUnit(Nutrients_Utah, clean = "nonstandardized_only")
#' 
#' # Remove both invalid and nonstandardized characteristic-media-result unit combinations
#' # from dataframe:
#' ResultUnit_clean <- InvalidResultUnit(Nutrients_Utah, clean = "both")
#' 
#' # Flag, but do not remove, data with invalid or nonstandardized characteristic-media-result unit
#' # combinations in new column titled "TADA.ResultUnit.Flag":
#' InvalidUnit_flags <- InvalidResultUnit(Nutrients_Utah, clean = "none")
#' 
#' # Show only invalid characteristic-media-result unit combinations:
#' InvalidUnit_errorsonly <- InvalidResultUnit(Nutrients_Utah, clean = "nonstandardized_only", errorsonly = TRUE)
#' 
#' # Show only nonstandardized characteristic-media-result unit combinations:
#' NonstandardUnit_errorsonly <- InvalidResultUnit(Nutrients_Utah, clean = "invalid_only", errorsonly = TRUE)

InvalidResultUnit <- function(.data, clean = c("invalid_only", "nonstandardized_only", "both", "none"), errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check clean is character
  checkType(clean, "character")
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  checkColumns(.data, c("TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode", "TADA.ActivityMediaName"))
  # check that clean is either "invalid_only", "nonstandardized_only", "both", or "none"
  clean <- match.arg(clean)

  # execute function after checks are passed - removes flag column in case reference table has changed.
  if (("TADA.ResultUnit.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ResultUnit.Flag)
  }
  
  # read in unit reference table from extdata and filter
  unit.ref <- GetWQXCharValRef() %>%
    dplyr::filter(Type == "CharacteristicUnit")
  
  # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
  check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
                      by.x = c("TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode", "TADA.ActivityMediaName"),
                      by.y = c("Characteristic", "Value", "Source"), all.x = TRUE
  )
  
  # rename Status column
  check.data <- check.data %>%
    dplyr::rename(TADA.ResultUnit.Flag = Status)
  # rename NA values to Nonstandardized in WQX.ResultUnitValidity column
  check.data["TADA.ResultUnit.Flag"][is.na(check.data["TADA.ResultUnit.Flag"])] <- "Nonstandardized"
  
  # if all rows are "Valid", return input with flag column
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.ResultUnit.Flag)) == FALSE) {
    print("All characteristic/unit combinations are valid in your dataframe. Returning input dataframe with TADA.ResultUnit.Flag column for tracking.")
    check.data = OrderTADACols(.data)
    return(check.data)
  }
  
  # flagged output, all data
  if (clean == "none" & errorsonly == FALSE) {
    warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' result units. Check these records before proceeding with transformations and/or set clean = 'invalid_only' or 'both'.")
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
    clean.data = OrderTADACols(clean.data)
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
    error.data = OrderTADACols(error.data)
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
#' QualityControlActivity(Nutrients_Utah)
#' 
#' # Flag QC samples and filter to flagged data only:
#' QualityControlActivity(Nutrients_Utah, errorsonly = TRUE)
#' 
#' # Remove all QC samples:
#' QualityControlActivity(Nutrients_Utah, clean = "all")
#' 
#' # Remove only duplicate QC samples:
#' QualityControlActivity(Nutrients_Utah, clean = "duplicates")
#' 
#' # Remove only blank QC samples:
#' QualityControlActivity(Nutrients_Utah, clean = "blanks")
#' 
#' # Remove only calibration checks or spiked QC samples:
#' QualityControlActivity(Nutrients_Utah, clean = "calibrations")
#' 
#' # Remove only other QC samples:
#' QualityControlActivity(Nutrients_Utah, clean = "other")
#' 
#' # Remove blank QC samples and filter to flagged data only:
#' QualityControlActivity(Nutrients_Utah, clean = "blanks", errorsonly = TRUE)

QualityControlActivity <- function(.data, clean = c("none", "all", "duplicates", "blanks", "calibrations", "other"), errorsonly = FALSE) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check that clean is either "all", "none", "duplicates", "blanks", "calibrations", or "other"
  clean <- match.arg(clean)
  # check errorsonly is boolean
  checkType(errorsonly, "logical")
  # check .data has required columns
  checkColumns(.data, c("ActivityTypeCode"))

  
  # execute function after checks are passed
  # delete existing flag column
  if (("TADA.ActivityType.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ActivityType.Flag)
  }
  
  # designate replicate/duplicate Activity Types to flag
  dup <- c("Quality Control Alternative Measurement Sensitivity",
           "Quality Control Alternative Measurement Sensitivity Plus",
           "Quality Control Field Replicate Habitat Assessment",
           "Quality Control Field Replicate Msr/Obs",
           "Quality Control Field Replicate Portable Data Logger",
           "Quality Control Field Replicate Sample-Composite",
           "Quality Control Sample-Blind Duplicate",
           "Quality Control Sample-Field Replicate",
           "Quality Control Sample-Inter-lab Split",
           "Quality Control Sample-Lab Duplicate",
           "Quality Control Sample-Lab Duplicate 2",
           "Quality Control Sample-Lab Re-Analysis",
           "Quality Control Sample-Lab Split",
           "Quality Control-Meter Lab Duplicate",
           "Quality Control-Meter Lab Duplicate 2",
           "Sample-Routine Resample")
  
  # designate blank Activity Types to flag
  blank <- c("Quality Control Field Sample Equipment Rinsate Blank",
             "Quality Control Lab Sample Equipment Rinsate Blank",
             "Quality Control Sample-Equipment Blank",
             "Quality Control Sample-Field Ambient Conditions Blank",
             "Quality Control Sample-Field Blank",
             "Quality Control Sample-Lab Blank",
             "Quality Control Sample-Post-preservative Blank",
             "Quality Control Sample-Pre-preservative Blank",
             "Quality Control Sample-Reagent Blank",
             "Quality Control Sample-Trip Blank",
             "Quality Control-Meter Lab Blank",
             "Quality Control-Negative Control",
             "Sample-Depletion Replicate",
             "Sample-Negative Control")
  
  # designate calibration/spike/standard Activity Types to flag
  cal <- c("Quality Control Field Calibration Check",
           "Quality Control Field Msr/Obs Post-Calibration",
           "Quality Control Field Msr/Obs Pre-Calibration",
           "Quality Control Sample-Field Spike",
           "Quality Control Sample-Field Surrogate Spike",
           "Quality Control Sample-Lab Continuing Calibration Verification",
           "Quality Control Sample-Lab Control Sample/Blank Spike",
           "Quality Control Sample-Lab Control Sample/Blank Spike Duplicate",
           "Quality Control Sample-Lab Control Standard",
           "Quality Control Sample-Lab Control Standard Duplicate",
           "Quality Control Sample-Lab Initial Calib Certified Reference Material",
           "Quality Control Sample-Lab Initial Calibration Verification",
           "Quality Control Sample-Lab Matrix Spike",
           "Quality Control Sample-Lab Matrix Spike Duplicate",
           "Quality Control Sample-Lab Spike",
           "Quality Control Sample-Lab Spike Duplicate",
           "Quality Control Sample-Lab Spike Target",
           "Quality Control Sample-Lab Spike of a Lab Blank",
           "Quality Control Sample-Lab Surrogate Control Standard",
           "Quality Control Sample-Lab Surrogate Control Standard Duplicate",
           "Quality Control Sample-Lab Surrogate Method Blank",
           "Quality Control Sample-Measurement Precision Sample",
           "Quality Control Sample-Reference Sample",
           "Quality Control-Calibration Check",
           "Quality Control-Calibration Check Buffer",
           "Sample-Positive Control")
  
  # designate "other" QC Activity types to flag
  other <- c("Quality Control Sample-Other")
  
  # populate flag column
  flag.data <- .data %>%
    dplyr::mutate(TADA.ActivityType.Flag = dplyr::case_when(
      ActivityTypeCode %in% dup ~ "QC_duplicate",
      ActivityTypeCode %in% blank ~ "QC_blank",
      ActivityTypeCode %in% cal ~ "QC_calibration",
      ActivityTypeCode %in% other ~ "QC_other"
    ))
  
  # clean dataframe
  # if clean = "none", return full dataframe
  if(clean == "none") {
    clean.data <- flag.data
  } 
  # if clean = "all", remove flagged data
  if(clean == "all") {
    clean.data <- flag.data[is.na(flag.data$TADA.ActivityType.Flag),]
  }
  # if clean = "duplicates", remove data flagged as duplicate samples
  if(clean == "duplicates") {
    clean.data <- dplyr::filter(flag.data, flag.data$TADA.ActivityType.Flag != "QC_duplicate"|is.na(flag.data$TADA.ActivityType.Flag))
  }
  # if clean = "blanks", remove data flagged as blank samples
  if(clean == "blanks") {
    clean.data <- dplyr::filter(flag.data, flag.data$TADA.ActivityType.Flag != "QC_blank"|is.na(flag.data$TADA.ActivityType.Flag))
  }
  # if clean = "calibrations", remove data flagged as calibration samples
  if(clean == "calibrations") {
    clean.data <- dplyr::filter(flag.data, flag.data$TADA.ActivityType.Flag != "QC_calibration"|is.na(flag.data$TADA.ActivityType.Flag))
  }
  # if clean = "other", remove data flagged as other QC samples
  if(clean == "other") {
    clean.data <- dplyr::filter(flag.data, flag.data$TADA.ActivityType.Flag != "QC_other"|is.na(flag.data$TADA.ActivityType.Flag))
  }
  
  # if errorsonly = FALSE, return full clean dataframe
  if(errorsonly == FALSE) {
    final.data <- clean.data
    # if the dataframe is empty, print message
    if(nrow(final.data) == 0) {
      print("This dataframe is empty because all rows contained QC samples and were removed")
    }
    # if there are no flags, remove flag column and print message
    if(sum(!is.na(final.data$TADA.ActivityType.Flag)) == 0) {
      print("The column TADA.ActivityType.Flag was not added to this dataframe because either no QC samples were found or all flagged QC samples were removed")
      final.data <- dplyr::select(final.data, -TADA.ActivityType.Flag)
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