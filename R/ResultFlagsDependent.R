#' Check Sample Fraction Validity
#'
#' Function checks the validity of each characteristic-fraction combination
#' in the dataframe. When clean = TRUE, rows with invalid characteristic-fraction
#' combinations are removed. Default is clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-fraction
#' combinations from the dataframe when clean = TRUE. Default is clean = TRUE.
#'
#' @return When clean = FALSE, this function adds the following column to your
#' dataframe: WQX.SampleFractionValidity. This column flags each 
#' CharacteristicName and ResultSampleFractionText combination in your dataframe 
#' as either "Nonstandardized", "Invalid", or "Valid". When clean = TRUE, 
#' "Invalid" rows are removed from the dataframe and no column will be appended.
#'
#' @export
#'


InvalidFraction <- function(.data, clean = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if ("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }

  # execute function after checks are passed
  if (all(c("CharacteristicName", "ResultSampleFractionText") %in% colnames(.data)) == TRUE) {
    if (("WQX.SampleFractionValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.SampleFractionValidity)
    }
    # read in sample fraction reference table from extdata and filter
    frac.ref <- GetWQXCharValRef() %>%
      dplyr::filter(Type == "CharacteristicFraction")

    # join "Status" column to .data by CharacteristicName and Value (SampleFraction)
    check.data <- merge(.data, frac.ref[, c("Characteristic", "Status", "Value")],
      by.x = c(
        "CharacteristicName",
        "ResultSampleFractionText"
      ),
      by.y = c("Characteristic", "Value"), all.x = TRUE
    )

    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.SampleFractionValidity = Status)
    # rename NA values to Nonstandardized in WQX.SampleFractionValidity column
    check.data["WQX.SampleFractionValidity"][is.na(check.data["WQX.SampleFractionValidity"])] <- "Nonstandardized"

    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add WQX.SampleFractionValidity column to the list
    col.order <- append(col.order, "WQX.SampleFractionValidity")
    # reorder columns in check.data
    check.data <- check.data[, col.order]
    # place flag column next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.SampleFractionValidity",
        .after = "ResultSampleFractionText"
      )

    # if all rows are "Valid", return input unchanged
    if (any(c("Nonstandardized", "Invalid") %in%
      unique(check.data$WQX.SampleFractionValidity)) == FALSE) {
      print("No changes were made, because we did not find any invalid fraction/characteristic combinations in your dataframe")
      return(.data)
    }

    # flagged output
    if (clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' fraction. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }

    # clean output
    if (clean == TRUE) {
      # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, WQX.SampleFractionValidity != "Invalid")

      # remove WQX.SampleFractionValidity column
      clean.data <- dplyr::select(clean.data, -WQX.SampleFractionValidity)

      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Check Method Speciation Validity
#'
#' Function checks the validity of each characteristic-method
#' speciation combination in the dataframe. When clean = TRUE, rows with invalid
#' characteristic-method speciation combinations are removed. Default is
#' clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-method
#' speciation combinations from the dataframe when clean = TRUE. Default is
#' clean = TRUE.
#'
#' @return #'When clean = FALSE, this function adds the following column to your
#' dataframe: WQX.MethodSpeciationValidity. This column flags each 
#' CharacteristicName and MethodSpecificationName combination in your dataframe as 
#' either "Nonstandardized", "Invalid", or "Valid". When clean = TRUE, "Invalid" 
#' rows are removed from the dataframe and no column will be appended.
#'
#' @export
#'


InvalidSpeciation <- function(.data, clean = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }

  # execute function after checks are passed
  if (all(c("CharacteristicName", "MethodSpecificationName") %in% colnames(.data)) == TRUE) {
    if (("WQX.MethodSpeciationValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.MethodSpeciationValidity)
    }
    # read in speciation reference table from extdata and filter
    spec.ref <- GetWQXCharValRef() %>%
      dplyr::filter(Type == "CharacteristicSpeciation")

    # join "Status" column to .data by CharacteristicName and Value (Speciation)
    check.data <- merge(.data, spec.ref[, c("Characteristic", "Status", "Value")],
      by.x = c("CharacteristicName", "MethodSpecificationName"),
      by.y = c("Characteristic", "Value"), all.x = TRUE
    )

    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.MethodSpeciationValidity = Status)
    # rename NA values to Nonstandardized in WQX.MethodSpeciationValidity column
    check.data["WQX.MethodSpeciationValidity"][is.na(check.data["WQX.MethodSpeciationValidity"])] <- "Nonstandardized"

    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add WQX.MethodSpeciationValidity column to the list
    col.order <- append(col.order, "WQX.MethodSpeciationValidity")
    # reorder columns in check.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.MethodSpeciationValidity",
        .after = "MethodSpecificationName"
      )

    # if all rows are "Valid", return input unchanged
    if (any(c("Nonstandardized", "Invalid") %in%
      unique(check.data$WQX.MethodSpeciationValidity)) == FALSE) {
      print("All data is valid, therefore the function cannot be applied.")
      return(.data)
    }

    # flagged output
    if (clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' speciation. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }

    # clean output
    if (clean == TRUE) {
      # filter out invalid characteristic-fraction combinations
      clean.data <- dplyr::filter(check.data, WQX.MethodSpeciationValidity != "Invalid")

      # remove WQX.MethodSpeciationValidity column
      clean.data <- dplyr::select(clean.data, -WQX.MethodSpeciationValidity)

      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Check Result Unit Validity
#'
#' Function checks the validity of each characteristic-media-result unit
#' combination in the dataframe. When clean = TRUE, rows with invalid
#' characteristic-media-result unit combinations are removed. Default is
#' clean = TRUE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-media-result
#' unit combinations from the dataframe when clean = TRUE. Default is
#' clean = TRUE.
#'
#' @return When clean = FALSE, the following column will be added to your dataframe: 
#' WQX.ResultUnitValidity. This column flags each CharacteristicName, 
#' ActivityMediaName, and ResultMeasure/MeasureUnitCode combination in your 
#' dataframe as either "Nonstandardized", "Invalid", or "Valid". When
#' clean = TRUE, "Invalid" rows are removed from the dataframe and no column will
#' be appended.
#'
#' @export
#'


InvalidResultUnit <- function(.data, clean = TRUE) {

  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }

  # execute function after checks are passed
  if (all(c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName") %in% colnames(.data)) == TRUE) {
    if (("WQX.ResultUnitValidity" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -WQX.ResultUnitValidity)
    }
    # read in unit reference table from extdata and filter
    unit.ref <- GetWQXCharValRef() %>%
      dplyr::filter(Type == "CharacteristicUnit")

    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
      by.x = c("CharacteristicName", "ResultMeasure.MeasureUnitCode", "ActivityMediaName"),
      by.y = c("Characteristic", "Value", "Source"), all.x = TRUE
    )

    # rename Status column
    check.data <- check.data %>%
      dplyr::rename(WQX.ResultUnitValidity = Status)
    # rename NA values to Nonstandardized in WQX.ResultUnitValidity column
    check.data["WQX.ResultUnitValidity"][is.na(check.data["WQX.ResultUnitValidity"])] <- "Nonstandardized"

    # reorder column names to match .data
    # get .data column names
    col.order <- colnames(.data)
    # add WQX.ResultUnitValidity column to the list
    col.order <- append(col.order, "WQX.ResultUnitValidity")
    # reorder columns in check.data
    check.data <- check.data[, col.order]
    # place flag columns next to relevant fields
    check.data <- check.data %>%
      dplyr::relocate("WQX.ResultUnitValidity",
        .after = "ResultMeasure.MeasureUnitCode"
      )

    # if all rows are "Valid", return input unchanged
    if (any(c("Nonstandardized", "Invalid") %in%
      unique(check.data$WQX.ResultUnitValidity)) == FALSE) {
      print("All data is valid, therefore the function cannot be applied.")
      return(.data)
    }

    # flagged output
    if (clean == FALSE) {
      return(check.data)
      warning("Metadata transformations may be adversely affected by choosing to retain 'Invalid' result units. In order to ensure transformation functions will run properly, set clean = TRUE.")
    }

    # clean output
    if (clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, WQX.ResultUnitValidity != "Invalid")

      # remove WQX.ResultUnitValidity column
      clean.data <- dplyr::select(clean.data, -WQX.ResultUnitValidity)

      return(clean.data)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}

