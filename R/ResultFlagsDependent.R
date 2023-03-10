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
  
  # EDH replaced with function OrderTADACols at end
  # # reorder column names to match .data
  # # get .data column names
  # col.order <- colnames(.data)
  # # add WQX.SampleFractionValidity column to the list
  # col.order <- append(col.order, "TADA.SampleFraction.Flag")
  # # reorder columns in check.data
  # check.data <- check.data[, col.order]
  # # place flag column next to relevant fields
  # check.data <- check.data %>%
  #   dplyr::relocate("TADA.SampleFraction.Flag",
  #                   .after = "ResultSampleFractionText"
  #   )
  
  # if all rows are "Valid", return input unchanged
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.SampleFraction.Flag)) == FALSE) {
    if (errorsonly == FALSE) {
      print("All characteristic/fraction combinations are valid in your dataframe. Returning input dataframe with TADA.SampleFraction.Flag column.")
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
  
  # EDH replaced with OrderTADACols at end of function
  # # reorder column names to match .data
  # # get .data column names
  # col.order <- colnames(.data)
  # # add TADA.MethodSpeciation.Flag column to the list
  # col.order <- append(col.order, "TADA.MethodSpeciation.Flag")
  # # reorder columns in check.data
  # check.data <- check.data[, col.order]
  # # place flag columns next to relevant fields
  # check.data <- check.data %>%
  #   dplyr::relocate("TADA.MethodSpeciation.Flag",
  #                   .after = "MethodSpecificationName"
  #   )
  
  # if all rows are "Valid", return input with flag column
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.MethodSpeciation.Flag)) == FALSE) {
    print("All characteristic/method speciation combinations are valid in your dataframe. Returning input dataframe with TADA.MethodSpeciation.Flag column.")
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
  
  # EDH replaced with OrderTADACols at end of function
  # # reorder column names to match .data
  # # get .data column names
  # col.order <- colnames(.data)
  # # add TADA.ResultUnit.Flag column to the list
  # col.order <- append(col.order, "TADA.ResultUnit.Flag")
  # # reorder columns in check.data
  # check.data <- check.data[, col.order]
  # # place flag columns next to relevant fields
  # check.data <- check.data %>%
  #   dplyr::relocate("TADA.ResultUnit.Flag",
  #                   .after = "ResultMeasure.MeasureUnitCode"
  #   )
  
  # if all rows are "Valid", return input with flag column
  if (any(c("Nonstandardized", "Invalid") %in%
          unique(check.data$TADA.ResultUnit.Flag)) == FALSE) {
    print("All characteristic/unit combinations are valid in your dataframe. Returning input dataframe with TADA.ResultUnit.Flag column.")
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
