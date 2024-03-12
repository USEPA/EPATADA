#' Check for Invalid Analytical Methods
#'
#' Function checks the validity of each characteristic-analytical method
#' combination in the dataframe. When clean = TRUE, rows with invalid
#' characteristic-analytical method combinations are removed. Default is
#' clean = TRUE. When flaggedonly = TRUE, dataframe is filtered to show only
#' invalid characteristic-analytical method combinations. Default is
#' flaggedonly = FALSE.
#'
#' The “Not Reviewed” value within "TADA.ResultAboveUpperThreshold.Flag" means
#' that the EPA WQX team has not yet reviewed the combinations
#' (see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
#' The WQX team plans to review and update these new combinations quarterly.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Invalid" characteristic-analytical
#' method combinations from the dataframe when clean = TRUE. Default is
#' clean = TRUE.
#' @param flaggedonly Boolean argument; filters dataframe to show only "Invalid"
#' characteristic-analytical method combinations when flaggedonly = TRUE. Default
#' is flaggedonly = FALSE.
#'
#' @return This function adds the TADA.AnalyticalMethod.Flag to a TADA dataframe. This column
#' flags invalid CharacteristicName, ResultAnalyticalMethod/MethodIdentifier,
#' and ResultAnalyticalMethod/MethodIdentifierContext combinations in your dataframe
#' as either "Not Reviewed", "Invalid", or "Valid". When clean = FALSE and
#' flaggedonly = TRUE, the dataframe is filtered to show only "Invalid"
#' characteristic-analytical method combinations; the column TADA.AnalyticalMethod.Flag
#' is still appended. When clean = TRUE and flaggedonly = FALSE, "Invalid" rows
#' are removed from the dataframe and no column will be appended.
#'
#' @export
#'
#' @examples
#' # Load example dataset
#' data(Data_NCTCShepherdstown_HUC12)
#'
#' # Remove invalid characteristic-analytical method combinations from dataframe:
#' InvalidMethod_clean <- TADA_FlagMethod(Data_NCTCShepherdstown_HUC12, clean = TRUE)
#'
#' # Flag, but do not remove, invalid characteristic-analytical method combinations
#' # in new column titled "TADA.AnalyticalMethod.Flag":
#' InvalidMethod_flags <- TADA_FlagMethod(Data_NCTCShepherdstown_HUC12, clean = FALSE)
#'
#' # Show only invalid characteristic-analytical method combinations:
#' InvalidMethod_flaggedonly <- TADA_FlagMethod(Data_NCTCShepherdstown_HUC12, clean = FALSE, flaggedonly = TRUE)
#'
TADA_FlagMethod <- function(.data, clean = TRUE, flaggedonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "TADA.CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext"
  )
  TADA_CheckColumns(.data, required_cols)
  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop("Function not executed because clean and flaggedonly cannot both be TRUE")
  }

  # execute function after checks are passed - removes flag column in case reference table has changed.
  # delete existing flag column
  if (("TADA.AnalyticalMethod.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.AnalyticalMethod.Flag)
  }
  # read in WQX val reference table and filter
  meth.ref <- utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "TADA")) %>%
    dplyr::filter(Type == "CharacteristicMethod")

  # join "TADA.WQXVal.Flag" column to .data by CharacteristicName, Source (Media), and Value (unit)
  check.data <- merge(.data, meth.ref[, c("Characteristic", "Source", "Value", "TADA.WQXVal.Flag")],
    by.x = c(
      "TADA.CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier",
      "ResultAnalyticalMethod.MethodIdentifierContext"
    ),
    by.y = c("Characteristic", "Value", "Source"), all.x = TRUE
  )

  # rename TADA.WQXVal.Flag column to WQX.AnalyticalMethodValidity
  check.data <- check.data %>%
    dplyr::rename(TADA.AnalyticalMethod.Flag = TADA.WQXVal.Flag) %>%
    dplyr::distinct()
  # rename NA values to NonStandardized in WQX.AnalyticalMethodValidity column
  check.data["TADA.AnalyticalMethod.Flag"][is.na(check.data["TADA.AnalyticalMethod.Flag"])] <- "Not Reviewed"

  if (flaggedonly == FALSE) {
    # if all rows are "Valid" or NA "Not Reviewed", return input unchanged
    ## note: Cristina edited this on 9/19/22 to keep Not Reviewed/NA data when clean = TRUE. Now only Invalid data is removed.
    if (any("Invalid" %in%
      unique(check.data$TADA.AnalyticalMethod.Flag)) == FALSE) {
      print("No invalid method/characteristic combinations in your dataframe. Returning the input dataframe with TADA.AnalyticalMethod.Flag column for tracking.")
      check.data <- TADA_OrderCols(check.data)
      return(check.data)
    }

    # flagged output, all data
    if (clean == FALSE) {
      check.data <- TADA_OrderCols(check.data)
      return(check.data)
    }

    # clean output
    if (clean == TRUE) {
      # filter out invalid characteristic-unit-method combinations
      clean.data <- dplyr::filter(check.data, TADA.AnalyticalMethod.Flag != "Invalid")

      # remove WQX.AnalyticalMethodValidity column
      # clean.data <- dplyr::select(clean.data, -TADA.AnalyticalMethod.Flag)
      clean.data <- TADA_OrderCols(clean.data)
      return(clean.data)
    }
  }

  # flagged output, errors only
  if (clean == FALSE & flaggedonly == TRUE) {
    # filter to show only invalid characteristic-unit-method combinations
    invalid.data <- dplyr::filter(check.data, TADA.AnalyticalMethod.Flag == "Invalid")
    if (nrow(invalid.data) == 0) {
      # invalid.data <- dplyr::select(invalid.data, -TADA.AnalyticalMethod.Flag)
      print("This dataframe is empty because we did not find any invalid method/characteristic combinations in your dataframe")
    }
    invalid.data <- TADA_OrderCols(invalid.data)
    return(invalid.data)
  }
}



#' Check for Aggregated Continuous Data
#'
#' The Water Quality Portal (WQP) does not currently include a lot of high-frequency
#' sensor data. However, sometimes data providers choose to aggregate their
#' continuous data to a daily avg, max, or min value, and then submit that
#' aggregated data to the WQP through WQX. Alternatively, some organizations
#' aggregate their high frequency data (15 min or 1 hour data)
#' to 2 or 4 hour interval averages, and they also submit that data to the WQP through WQX.
#' The raw continuous time series data is made available through a text file
#' attachment at the activity level. This type of high frequency data may
#' (or may not) be suitable for integration with discrete
#' water quality data for assessments. Therefore, this function uses metadata
#' submitted by data providers to flag rows with aggregated continuous data.
#' This is done by flagging results where the ResultDetectionConditionText =
#' "Reported in Raw Data (attached)". When clean = FALSE and flaggedonly = FALSE, a column titled
#' "TADA.AggregatedContinuousData.Flag" is added to the dataframe to indicate if the row
#' includes aggregated continuous data, "Continuous", or not,  "Discrete". When clean = FALSE and
#' flaggedonly = TRUE, the dataframe will be filtered to show only the rows flagged
#' "Continuous" for aggregated continuous data. When clean = TRUE and flaggedonly = FALSE,
#' rows with aggregated continuous data are removed from the dataframe and no
#' column will be appended. When clean = TRUE and flaggedonly = TRUE, the function
#' does not execute and an error message is returned. The default is clean = TRUE
#' and flaggedonly = FALSE.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes aggregated continuous data from
#' the dataframe when clean = TRUE. Default is clean = TRUE.
#' @param flaggedonly Boolean argument; filters dataframe to show only aggregated
#' continuous data when flaggedonly = TRUE. Default is flaggedonly = FALSE.
#'
#' @return When clean = FALSE and flaggedonly = FALSE, a column flagging rows with
#' aggregated continuous data is appended to the input data set. When clean = FALSE
#' and flaggedonly = TRUE, the dataframe is filtered to show only the flagged
#' aggregated continuous data and flag column is still appended. When clean = TRUE
#' and flaggedonly = FALSE, aggregated continuous data is removed from the dataframe
#' and no column is appended. The default is clean = FALSE and flaggedonly = FALSE.
#'
#' @export
#'
#' @examples
#' # Load example dataset
#' data(Data_Nutrients_UT)
#'
#' # Remove aggregated continuous data in dataframe:
#' AggContinuous_clean <- TADA_FindContinuousData(Data_Nutrients_UT, clean = TRUE)
#'
#' # Flag, but do not remove, aggregated continuous data in new column
#' # titled "TADA.AggregatedContinuousData.Flag":
#' AggContinuous_flags <- TADA_FindContinuousData(Data_Nutrients_UT, clean = FALSE)
#'
#' # Show only rows flagged for aggregated continuous data:
#' AggContinuous_flaggedonly <- TADA_FindContinuousData(Data_Nutrients_UT,
#'   clean = FALSE, flaggedonly = TRUE
#' )
#'
TADA_FindContinuousData <- function(.data, clean = FALSE, flaggedonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, "ResultDetectionConditionText")
  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop("Function not executed because clean and flaggedonly cannot both be TRUE")
  }

  # set default flag to "unknown"
  .data$TADA.AggregatedContinuousData.Flag <- "Discrete"

  # execute function after checks are passed
  # flag continuous data
  # make cont.data data frame
  # with new profiles might want to check for zip files? Do these columns show up in TADA_DataRetrieval?
  cont.data <- .data %>% dplyr::filter((ActivityTypeCode == "Field Msr/Obs" &
    ResultDetectionConditionText == "Reported in Raw Data (attached)") |
    (ActivityTypeCode == "Field Msr/Obs" &
      SampleCollectionEquipmentName == "Probe/Sensor" &
      !is.na(ResultTimeBasisText) &
      !is.na(StatisticalBaseCode) &
      ResultValueTypeName == "Calculated"))

  # everything not in cont dataframe
  noncont.data <- subset(.data, !.data$ResultIdentifier %in% cont.data$ResultIdentifier)

  # if there is aggregated continuous data is in the data set
  if (nrow(cont.data) != 0) {
    # change contents of ContDataFlag column
    cont.data$TADA.AggregatedContinuousData.Flag <- "Continuous"
    # join cont.data to flag.data
    flag.data <- plyr::rbind.fill(cont.data, noncont.data)

    # flagged output, all data
    if (clean == FALSE & flaggedonly == FALSE) {
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }

    # clean output
    if (clean == TRUE & flaggedonly == FALSE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(flag.data, !(TADA.AggregatedContinuousData.Flag %in% "Continuous"))

      # remove TADA.AggregatedContinuousData column
      # clean.data <- dplyr::select(clean.data, -TADA.AggregatedContinuousData.Flag)
      clean.data <- TADA_OrderCols(clean.data)
      return(clean.data)
    }

    # flagged output, only aggregated continuous data
    if (clean == FALSE & flaggedonly == TRUE) {
      # filter to show only invalid characteristic-unit-media combinations
      aggcont.data <- dplyr::filter(flag.data, TADA.AggregatedContinuousData.Flag == "Continuous")
      aggcont.data <- TADA_OrderCols(aggcont.data)
      return(aggcont.data)
    }
  }

  # if no aggregated continuous data is in the data set
  if (nrow(cont.data) == 0) {
    if (flaggedonly == FALSE) {
      print("No evidence of aggregated continuous data in your dataframe. Returning the input dataframe with TADA.AggregatedContinuousData.Flag column for tracking.")
      .data <- TADA_OrderCols(.data)
      return(.data)
    }

    if (flaggedonly == TRUE) {
      print("This dataframe is empty because we did not find any aggregated continuous data in your dataframe")
      cont.data <- TADA_OrderCols(cont.data)
      return(cont.data)
    }
  }
}

#' Check Result Value Against WQX Upper Threshold
#'
#' EPA's Water Quality Exchange (WQX) has generated maximum and minimum thresholds
#' for each parameter and unit combination from millions of water quality data
#' points around the country. This function leverages the WQX QAQC Validation Table
#' to flag any data that is above the upper threshold of result values submitted
#' to WQX for a given characteristic.
#'
#' When clean = FALSE and flaggedonly = FALSE, a column which flags data above
#' the upper WQX threshold is appended to the dataframe. When clean = FALSE and
#' flaggedonly = TRUE, the dataframe is filtered to show only data found above
#' the WQX threshold. When clean = TRUE and flaggedonly = FALSE, rows with values
#' that are above the upper WQX threshold are removed from the dataframe and no
#' column is appended. When clean = TRUE and and flaggedonly = TRUE, the function
#' is not executed and an error message is returned. Defaults are clean = FALSE
#' and flaggedonly = FALSE.
#'
#' This function will add the column "TADA.ResultAboveUpperThreshold.Flag" which
#' will be populated with the values: "Pass", "Suspect", "Not Reviewed", or
#' "NA - Not Available". The “Not Reviewed” value means that the EPA WQX team
#' has not yet reviewed the range yet for the characteristic and unit combination combination
#' in that row (see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
#' The WQX team plans to review and update these new combinations quarterly. The
#' "NA - Not Available" flag means that the characteristic, media, and/or unit combination
#' for that row is not fully populated (is NA or does not match the WQX data standard)
#' or the result value is NA.
#'
#' If this function is run more than once on the same dataframe,
#' the flag column will be deleted and regenerated.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data that is above the upper WQX
#' threshold from the dataframe when clean = TRUE. Default is clean = FALSE.
#' @param flaggedonly Boolean argument; filters dataframe to show only the data
#' flagged as above the upper WQX threshold. Default is flaggedonly = FALSE.
#' @return The input TADA dataset with the added "TADA.ResultAboveUpperThreshold.Flag"
#' column which is populated with the values: "Pass", "Suspect", "Not Reviewed", or
#' "NA - Not Available". Defaults are clean = FALSE and flaggedonly = FALSE.
#' When clean = FALSE and flaggedonly = TRUE, the dataframe
#' is filtered to show only data found above the WQX threshold. When clean = TRUE
#' and flaggedonly = FALSE, rows with values that are above the upper WQX threshold
#' are removed from the dataframe. When clean = TRUE and and flaggedonly = TRUE,
#' the function is not executed and an error message is returned.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Remove data that is above the upper WQX threshold from dataframe:
#' WQXUpperThreshold_clean <- TADA_FlagAboveThreshold(Data_Nutrients_UT, clean = TRUE)
#'
#' # Flag, but do not remove, data that is above the upper WQX threshold in
#' # new column titled "TADA.ResultValueAboveUpperThreshold.Flag":
#' WQXUpperThreshold_flags <- TADA_FlagAboveThreshold(Data_Nutrients_UT, clean = FALSE)
#'
#' # Show only data flagged as above the upper WQX threshold:
#' WQXUpperThreshold_flagsonly <- TADA_FlagAboveThreshold(Data_Nutrients_UT, clean = FALSE, flaggedonly = TRUE)
#'
TADA_FlagAboveThreshold <- function(.data, clean = FALSE, flaggedonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, required_cols)
  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop("Function not executed because clean and flaggedonly cannot both be TRUE")
  }

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }

  # execute function after checks are passed

  # delete existing flag column - removes flag column in case reference table has changed.
  if (("TADA.ResultValueAboveUpperThreshold.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ResultValueAboveUpperThreshold.Flag)
  }

  # get WQXcharVal.ref and filter to include only CharacteristicUnit.
  # Note that status is not applicable to ranges.
  # Instead, we generate a validation flag later in this function
  unit.ref <- utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "TADA")) %>%
    dplyr::filter(Type == "CharacteristicUnit")

  # update ref table names to prepare for left join with df
  names(unit.ref)[names(unit.ref) == "Characteristic"] <- "TADA.CharacteristicName"
  names(unit.ref)[names(unit.ref) == "Source"] <- "TADA.ActivityMediaName"
  names(unit.ref)[names(unit.ref) == "Value.Unit"] <- "TADA.ResultMeasure.MeasureUnitCode"

  # remove extraneous columns from unit.ref
  unit.ref <- dplyr::select(unit.ref, c(
    -Type, -Unique.Identifier, -Domain,
    -Status, -TADA.WQXVal.Flag,
    -Note.Recommendation,
    -Conversion.Factor,
    -Conversion.Coefficient,
    -Last.Change.Date,
    -Value,
    -Minimum
  ))
  unit.ref <- unique(unit.ref)

  check.data <- dplyr::left_join(.data,
    unit.ref,
    by = c(
      "TADA.CharacteristicName",
      "TADA.ActivityMediaName",
      "TADA.ResultMeasure.MeasureUnitCode"
    ),
    multiple = "any", # this should be "all" but the validation table has issues
    relationship = "many-to-many" # this should be "one-to-one" but the validation table has issues
  )

  # Create flag column, flag rows where ResultMeasureValue > Maximum
  flag.data <- check.data %>%
    # create flag column
    dplyr::mutate(TADA.ResultValueAboveUpperThreshold.Flag = dplyr::case_when(
      TADA.ResultMeasureValue >= Maximum ~ as.character("Suspect"),
      TADA.ResultMeasureValue < Maximum ~ as.character("Pass"),
      is.na(Maximum) ~ as.character("Not Reviewed"), # in QAQC table, but not yet reviewed
      TRUE ~ as.character("NA - Not Available") # this occurs when the char/unit/media combo is not in the WQX QAQC table at all. USGS data may not be in QAQC table because it does not adhere to the WQX domain tables.
    ))

  # remove Maximum column
  flag.data <- flag.data %>%
    dplyr::select(-"Maximum")

  # if no data above WQX threshold is found
  if (any("Suspect" %in%
    unique(flag.data$TADA.ResultValueAboveUpperThreshold.Flag)) == FALSE) {
    if (flaggedonly == FALSE) {
      print("No data above the WQX Upper Threshold was found in your dataframe. Returning the input dataframe with TADA.ResultAboveUpperThreshold.Flag column for tracking.")
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    if (flaggedonly == TRUE) {
      print("This dataframe is empty because no data above the WQX Upper Threshold was found in your dataframe")
      emptyflag.data <- dplyr::filter(flag.data, TADA.ResultValueAboveUpperThreshold.Flag %in% "Suspect")
      emptyflag.data <- TADA_OrderCols(emptyflag.data)
      return(emptyflag.data)
    }
  }

  # flagged, all data
  if (clean == FALSE & flaggedonly == FALSE) {
    flag.data <- TADA_OrderCols(flag.data)
    return(flag.data)
  }

  # clean data
  if (clean == TRUE & flaggedonly == FALSE) {
    # filter out rows where TADA.ResultValueAboveUpperThreshold.Flag = Suspect; remove TADA.ResultValueAboveUpperThreshold.Flag column
    clean.data <- flag.data %>%
      dplyr::filter(!(TADA.ResultValueAboveUpperThreshold.Flag %in% "Suspect")) # %>%
    # dplyr::select(-TADA.ResultValueAboveUpperThreshold.Flag)
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }

  # flagged, errors only
  if (clean == FALSE & flaggedonly == TRUE) {
    # filter to show only rows above WQX upper threshold
    flagsonly.data <- flag.data %>%
      dplyr::filter(TADA.ResultValueAboveUpperThreshold.Flag %in% "Suspect")
    flagsonly.data <- TADA_OrderCols(flagsonly.data)
    return(flagsonly.data)
  }
}



#' Check Result Value Against WQX Lower Threshold
#'
#' EPA's Water Quality Exchange (WQX) has generated maximum and minimum thresholds
#' for each parameter and unit combination from millions of water quality data
#' points around the country. This function leverages the WQX QAQC Validation Table
#' to flag any data that is below the lower threshold of result values submitted
#' to WQX for a given characteristic.
#'
#' When clean = FALSE and flaggedonly = FALSE, a column which flags data below
#' the lower WQX threshold is appended to the dataframe. When clean = FALSE and
#' flaggedonly = TRUE, the dataframe is filtered to show only data found below
#' the WQX threshold. When clean = TRUE and flaggedonly = FALSE, rows with values
#' that are below the upper WQX threshold are removed from the dataframe and no
#' column is appended. When clean = TRUE and and flaggedonly = TRUE, the function
#' is not executed and an error message is returned. Defaults are clean = FALSE
#' and flaggedonly = FALSE.
#'
#' This function will add the column "TADA.ResultValueBelowLowerThreshold.Flag" which
#' will be populated with the values: "Pass", "Suspect", "Not Reviewed", or
#' "NA - Not Available". The “Not Reviewed” value means that the EPA WQX team
#' has not yet reviewed the range yet for the characteristic and unit combination combination
#' in that row (see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
#' The WQX team plans to review and update these new combinations quarterly. The
#' "NA - Not Available" flag means that the characteristic, media, and/or unit combination
#' for that row is not fully populated (is NA or does not match the WQX data standard)
#' or the result value is NA.
#'
#' If this function is run more than once on the same dataframe,
#' the flag column will be deleted and regenerated.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data that is below the lower WQX
#' threshold from the dataframe when clean = TRUE. Default is clean = FALSE.
#' @param flaggedonly Boolean argument; filters dataframe to show only the data
#' flagged as below the lower WQX threshold. Default is flaggedonly = FALSE.
#' @return The input TADA dataset with the added "TADA.ResultValueBelowLowerThreshold.Flag"
#' column which is populated with the values: "Pass", "Suspect", "Not Reviewed", or
#' "NA - Not Available". Defaults are clean = FALSE and flaggedonly = FALSE.
#' When clean = FALSE and flaggedonly = TRUE, the dataframe
#' is filtered to show only data found below the WQX threshold. When clean = TRUE
#' and flaggedonly = FALSE, rows with values that are below the lower WQX threshold
#' are removed from the dataframe. When clean = TRUE and and flaggedonly = TRUE,
#' the function is not executed and an error message is returned.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Remove data that is below the lower WQX threshold from the dataframe:
#' WQXLowerThreshold_clean <- TADA_FlagBelowThreshold(Data_Nutrients_UT, clean = TRUE)
#'
#' # Flag, but do not remove, data that is below the lower WQX threshold in
#' # new column titled "TADA.ResultValueBelowLowerThreshold.Flag":
#' WQXLowerThreshold_flags <- TADA_FlagBelowThreshold(Data_Nutrients_UT, clean = FALSE)
#'
#' # Show only data that is below the lower WQX threshold:
#' WQXLowerThreshold_flagsonly <- TADA_FlagBelowThreshold(Data_Nutrients_UT, clean = FALSE, flaggedonly = TRUE)
#'
TADA_FlagBelowThreshold <- function(.data, clean = FALSE, flaggedonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check .data has required columns
  required_cols <- c(
    "TADA.CharacteristicName", "TADA.ActivityMediaName", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, required_cols)
  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop("Function not executed because clean and flaggedonly cannot both be TRUE")
  }

  # check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop("The ResultMeasureValue column must be of class 'numeric'.")
  }

  # execute function after checks are passed - removes flag column in case reference table has changed.
  # delete existing flag column
  if (("TADA.ResultValueBelowLowerThreshold.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.ResultValueBelowLowerThreshold.Flag)
  }

  # get WQXcharVal.ref and filter to include only CharacteristicUnit
  # Note that status is not applicable to ranges.
  # Instead, we generate a validation flag later in this function
  unit.ref <- utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "TADA")) %>%
    dplyr::filter(Type == "CharacteristicUnit")

  # update ref table names to prepare for left join with df
  names(unit.ref)[names(unit.ref) == "Characteristic"] <- "TADA.CharacteristicName"
  names(unit.ref)[names(unit.ref) == "Source"] <- "TADA.ActivityMediaName"
  names(unit.ref)[names(unit.ref) == "Value.Unit"] <- "TADA.ResultMeasure.MeasureUnitCode"

  # remove extraneous columns from unit.ref
  unit.ref <- dplyr::select(unit.ref, c(
    -Type, -Unique.Identifier, -Domain,
    -Status, -TADA.WQXVal.Flag,
    -Note.Recommendation,
    -Conversion.Factor,
    -Conversion.Coefficient,
    -Last.Change.Date,
    -Value,
    -Maximum
  ))

  unit.ref <- unique(unit.ref)

  check.data <- dplyr::left_join(.data,
    unit.ref,
    by = c(
      "TADA.CharacteristicName",
      "TADA.ActivityMediaName",
      "TADA.ResultMeasure.MeasureUnitCode"
    ),
    multiple = "any", # this should be "all" but the validation table has issues
    relationship = "many-to-many" # this should be "one-to-one" but the validation table has issues
  )

  # Create flag column, flag rows where TADA.ResultMeasureValue < Minimum
  flag.data <- check.data %>%
    # create flag column
    dplyr::mutate(TADA.ResultValueBelowLowerThreshold.Flag = dplyr::case_when(
      TADA.ResultMeasureValue < Minimum ~ as.character("Suspect"),
      TADA.ResultMeasureValue >= Minimum ~ as.character("Pass"),
      is.na(Minimum) ~ as.character("Not Reviewed"), # in QAQC table but not reviewed
      TRUE ~ as.character("NA - Not Available") # this occurs when the char/unit/media combo is not in the WQX QAQC table at all. USGS data may not be in QAQC table because it does not adhere to the WQX domain tables.
    ))

  # remove Min column
  flag.data <- flag.data %>%
    dplyr::select(-"Minimum")

  # if no data below WQX lower threshold is found
  if (any("Suspect" %in%
    unique(flag.data$TADA.ResultValueBelowLowerThreshold.Flag)) == FALSE) {
    if (flaggedonly == FALSE) {
      print("No data below the WQX Lower Threshold were found in your dataframe. Returning the input dataframe with TADA.ResultValueBelowLowerThreshold.Flag column for tracking.")
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    if (flaggedonly == TRUE) {
      print("This dataframe is empty because no data below the WQX Lower Threshold was found in your dataframe")
      emptyflag.data <- dplyr::filter(flag.data, TADA.ResultValueBelowLowerThreshold.Flag %in% "Suspect")
      emptyflag.data <- TADA_OrderCols(emptyflag.data)
      return(emptyflag.data)
    }
  }

  # flagged, all data
  if (clean == FALSE & flaggedonly == FALSE) {
    flag.data <- TADA_OrderCols(flag.data)
    return(flag.data)
  }

  # clean data
  if (clean == TRUE & flaggedonly == FALSE) {
    # filter out rows where TADA.ResultValueBelowLowerThreshold.Flag = Suspect; remove TADA.ResultValueBelowLowerThreshold.Flag column
    clean.data <- flag.data %>%
      dplyr::filter(!(TADA.ResultValueBelowLowerThreshold.Flag %in% "Suspect"))
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }

  # only flagged data
  if (clean == FALSE & flaggedonly == TRUE) {
    # filter to show only rows where TADA.ResultValueBelowLowerThreshold.Flag = Suspect
    flagsonly.data <- flag.data %>%
      dplyr::filter(TADA.ResultValueBelowLowerThreshold.Flag %in% "Suspect")
    flagsonly.data <- TADA_OrderCols(flagsonly.data)
    return(flagsonly.data)
  }
}




#' Check data for an approved QAPP
#'
#' Function checks data submitted under the column "QAPPApprovedIndicator".
#' Some organizations submit data for this field to indicate if the data
#' produced has an approved Quality Assurance Project Plan (QAPP) or not.
#' Y indicates yes, N indicates no.  This function has three default inputs:
#' clean = TRUE, cleanNA = FALSE, and flaggedonly == FALSE. The default flags
#' rows of data where the QAPPApprovedIndicator equals "N". Users could
#' remove NA's in addition to N's using the inputs clean = TRUE, cleanNA = TRUE,
#' and flaggedonly = FALSE. If flaggedonly = TRUE, the function will filter out all
#' rows where the QAPPApprovedIndicator is 'Y'. If clean = FALSE, cleanNA = FALSE,
#' and flaggedonly = FALSE, the function will not make any changes to the data.
#'
#' Note: This is not a required field, so it is often left blank (NA) even if
#' the data has an associated QAPP. All states and tribes that collect
#' monitoring data using 106 funding (almost all state and tribal data in WQX)
#' are required to have an EPA approved QAPP to receive 106 funding. Therefore,
#' most of these organizations data has an approved QAPP even if the data
#' submitted to WQP is NA.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument with two possible values called "TRUE" and
#' "FALSE". When clean=TRUE, rows of data where the QAPPApprovedIndicator equals
#' "N" will be removed. When, clean=FALSE, rows of data where the
#' QAPPApprovedIndicator equals "N" will be retained.
#' @param cleanNA Boolean argument with two possible values called "TRUE" and
#' "FALSE". When cleanNA=TRUE, rows of data where the QAPPApprovedIndicator
#' equals "NA" will be removed. When, cleanNA=FALSE, rows of data where the
#' the QAPPApprovedIndicator equals "NA" will be retained.
#' @param flaggedonly Boolean argument; when flaggedonly = TRUE, the dataframe will
#' be filtered to remove any rows where the QAPPApprovedIndicator equals "Y".
#'
#' @return Several combinations of inputs are possible:
#' When clean = TRUE, cleanNA = FALSE, and flaggedonly = FALSE, the dataframe will
#' be filtered to show only rows where QAPPAprrovedIndicator is "Y" or "NA";
#' When clean = TRUE, cleanNA = TRUE, and flaggedonly = FALSE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "Y";
#' When clean = FALSE, cleanNA = TRUE, and flaggedonly = FALSE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "Y" or "N";
#' When clean = FALSE, cleanNA = FALSE, and flaggedonly = FALSE, no rows are
#' removed from the dataframe;
#' When clean = TRUE, cleanNA = TRUE, and flaggedonly = TRUE, the function will
#' not execute and an error message will be returned;
#' When clean = TRUE, cleanNA = FALSE, and flaggedonly = TRUE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "NA";
#' When clean = FALSE, cleanNA = TRUE, and flaggedonly = TRUE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "N";
#' When clean = FALSE, cleanNA = FALSE, and flaggedonly = TRUE, the dataframe will
#' be filtered to show only rows where QAPPApprovedIndicator is "N" or "NA"
#'
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Show data where the QAPPApprovedIndicator equals "Y" or "NA":
#' QAPPapproved_clean <- TADA_FindQAPPApproval(Data_Nutrients_UT)
#'
#' # Show only data where the QAPPApprovedIndicator equals "Y":
#' QAPPapproved_cleanNAs <- TADA_FindQAPPApproval(Data_Nutrients_UT, cleanNA = TRUE)
#'
#' # Show data where the QAPPApprovedIndicator equals "N" or "NA":
#' QAPPIndicator_N_NA <- TADA_FindQAPPApproval(Data_Nutrients_UT,
#'   clean = FALSE,
#'   cleanNA = FALSE, flaggedonly = TRUE
#' )
#'
#' # Show data where the QAPPApprovedIndicator equals "N":
#' QAPPIndicator_N <- TADA_FindQAPPApproval(Data_Nutrients_UT,
#'   clean = FALSE,
#'   cleanNA = TRUE, flaggedonly = TRUE
#' )
#'
#' # Note: When clean = FALSE, cleanNA = FALSE, and flaggedonly = FALSE, no data is removed
#' # Note: When clean = TRUE, cleanNA = TRUE, and flaggedonly = TRUE, an error message is returned
#'
TADA_FindQAPPApproval <- function(.data, clean = FALSE, cleanNA = FALSE, flaggedonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check cleanNA is boolean
  TADA_CheckType(cleanNA, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, "QAPPApprovedIndicator")
  # check that clean, cleanNA and flaggedonly are not all TRUE
  if (clean == TRUE & cleanNA == TRUE & flaggedonly == TRUE) {
    stop("Function not executed because clean, cleanNA, and flaggedonly cannot all be TRUE")
  }

  # execute function after checks are passed

  # if flaggedonly = FALSE
  if (flaggedonly == FALSE) {
    if (clean == TRUE) {
      .data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE | QAPPApprovedIndicator == "Y")

      if (nrow(.data) == 0) {
        print("All QAPPApprovedIndicator data is N")
      }
    }
    if (cleanNA == TRUE) {
      .data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == FALSE)

      if (nrow(.data) == 0 & clean == TRUE) {
        print("All QAPPApprovedIndicator data is NA or N")
      } else if (nrow(.data) == 0 & clean == FALSE) {
        print("All QAPPApprovedIndicator data is NA")
      }
    }
    if (clean == FALSE & cleanNA == FALSE) {
      print("Data is flagged but not removed because clean and cleanNA were FALSE")
    }
    .data <- TADA_OrderCols(.data)
    return(.data)
  }

  # if flaggedonly = TRUE
  if (flaggedonly == TRUE & clean == TRUE & cleanNA == FALSE) {
    NA.data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE)
    if (nrow(NA.data) == 0) {
      warning("All QAPPApprovedIndicator data is 'Y' or 'N'")
    }
    NA.data <- TADA_OrderCols(NA.data)
    return(NA.data)
  }
  if (flaggedonly == TRUE & clean == FALSE & cleanNA == TRUE) {
    N.data <- dplyr::filter(.data, QAPPApprovedIndicator == "N")
    if (nrow(N.data) == 0) {
      warning("All QAPPApprovedIndicator data is NA or 'Y'")
    }
    N.data <- TADA_OrderCols(N.data)
    return(N.data)
  }
  if (flaggedonly == TRUE & clean == FALSE & cleanNA == FALSE) {
    NAorN.data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == TRUE | QAPPApprovedIndicator == "N")
    if (nrow(NAorN.data) == 0) {
      warning("All QAPPApprovedIndicator data is 'Y'")
    }
    NAorN.data <- TADA_OrderCols(NAorN.data)
    return(NAorN.data)
  }
}



#' Check if an approved QAPP document URL is provided
#'
#' Function checks data submitted under the "ProjectFileUrl" column
#' to determine if a QAPP document is available to review. When clean = FALSE,
#' a column will be appended to flag results that have an associated
#' QAPP document URL provided. When clean = TRUE, rows that do not
#' have an associated QAPP document are removed from the dataframe and no column
#' will be appended. This function should only be used to remove data if an
#' accompanying QAPP document is required to use data in assessments.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes data without an associated QAPP
#' document from the dataframe when clean = TRUE. Default is clean = FALSE.
#'
#' @return Returns input dataframe with the added "TADA.QAPPDocAvailable" column.
#' When clean = FALSE, no data are removed and the TADA.QAPPDocAvailable column
#' flags rows with an associated QAPP document. When clean = TRUE,
#' data without an associated QAPP document are removed from the dataframe.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Flag, but do not remove, data without an associated QAPP document in
#' # new column titled "TADA.QAPPDocAvailable":
#' FlagData_MissingQAPPDocURLs <- TADA_FindQAPPDoc(Data_Nutrients_UT)
#'
#' # Remove data without an associated QAPP document available:
#' RemoveData_MissingQAPPDocURLs <- TADA_FindQAPPDoc(Data_Nutrients_UT, clean = TRUE)
#'
TADA_FindQAPPDoc <- function(.data, clean = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check .data has required columns

  # generate required column if it does not exist (there is no project data)
  if ("ProjectFileUrl" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create empty ProjectFileUrl column
    .data[, "ProjectFileUrl"] <- NA
  }

  # check .data has required columns
  TADA_CheckColumns(.data, "ProjectFileUrl")

  # default flag column
  .data$TADA.QAPPDocAvailable <- "N"

  # execute function after checks are passed
  # flag data where QAPP document url is provided
  # make QAPPdoc.data data frame
  QAPPdoc.data <- dplyr::filter(.data, grepl("/", ProjectFileUrl))
  NQAPPdoc.data <- subset(.data, !.data$ResultIdentifier %in% QAPPdoc.data$ResultIdentifier)

  # if there is data without an associated QAPP url in the data set
  if (nrow(QAPPdoc.data) != 0) {
    # change flag column
    QAPPdoc.data$TADA.QAPPDocAvailable <- "Y_ProjectFileUrlProvided"

    # join QAPPdoc.data to flag.data
    flag.data <- plyr::rbind.fill(QAPPdoc.data, NQAPPdoc.data)

    # flagged output
    if (clean == FALSE) {
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }

    # clean output
    if (clean == TRUE) {
      # remove data without an associated QAPP url
      clean.data <- dplyr::filter(flag.data, grepl("/", ProjectFileUrl))

      # remove TADA.QAPPDocAvailable column
      # clean.data <- dplyr::select(clean.data, -TADA.QAPPDocAvailable)
      clean.data <- TADA_OrderCols(clean.data)
      return(clean.data)
    }
  }

  # if no associated QAPP url data is in the data set
  if (nrow(QAPPdoc.data) == 0) {
    if (clean == FALSE) {
      print("No QAPP document url data found in your dataframe. Returning input dataframe with TADA.QAPPDocAvailable column for tracking.")
      .data <- TADA_OrderCols(.data)
      return(.data)
    }
    if (clean == TRUE) {
      print("This dataframe is empty because we did not find any QAPP document url data in your dataframe")
      QAPPdoc.data <- TADA_OrderCols(QAPPdoc.data)
      return(QAPPdoc.data)
    }
  }
}



#' Invalid coordinates
#'
#' This function identifies and flags invalid coordinate data. When
#' clean_outsideUSA = "no" and clean_imprecise = FALSE,
#' a column will be appended titled "TADA.InvalidCoordinates.Flag" with the following
#' flags: 1) If the latitude is less than zero, the row will be
#' flagged with "LAT_OutsideUSA" (with the exception of American Samoa,
#' Northern Mariana Islands, and Guam), 2) If the longitude is greater than zero AND less than 145,
#' the row will be flagged as "LONG_OutsideUSA" (with the exception of
#' American Samoa, Northern Mariana Islands, and Guam), and 3) Finally,
#' precision can be measured by the number of decimal places in the latitude and longitude
#' provided. If either the latitude or longitude does not have at least three numbers to the
#' right of the decimal point, the row will be flagged as "Imprecise". Occasionally
#' latitude and longitude measurements are flagged as outside of the United States
#' because the data was entered as negative when it should be positive or vice versa.
#' This function offers the option of clean_outsideUSA = "change sign" to fix this
#' issue. However, data owners should fix the raw data through WQX. For assistance
#' with changing raw data, email the WQX help desk: \email{WQX@@epa.gov}
#'
#' @param .data TADA dataframe
#' @param clean_outsideUSA Character argument with options "no", "remove", and "change sign";
#' flags coordinates as outside the USA when clean_outsideUSA = "no";
#' removes data with coordinates outside of the United States when clean_outsideUSA = "remove";
#' changes sign of lat/long coordinates flagged as outside the USA when
#' clean_outside = "change sign"; Default is clean_outsideUSA = "no".
#' @param clean_imprecise Boolean argument; removes imprecise data when
#' clean_imprecise = TRUE. Default is clean_imprecise = FALSE.
#' @param flaggedonly Boolean argument; Return only flagged data when flaggedonly = TRUE;
#' default is flaggedonly = FALSE.
#'
#' @return Returns input TADA dataset with the added "TADA.InvalidCoordinates.Flag" column.
#' When clean_outsideUSA is "no", "change sign", or clean_imprecise argument is FALSE,
#' a column flagging rows with the respective QA check is appended to the input
#' dataframe. When clean_outsideUSA is "remove" or clean_imprecise is TRUE,
#' "invalid" or "imprecise" data is removed, respectively. When flaggedonly is TRUE,
#' the dataframe will be filtered to show only the data flagged as invalid, imprecise,
#' or out of the United States. Defaults are clean_outsideUSA = "no",
#' clean_imprecise = FALSE, and flaggedonly = FALSE.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Flag, but do not remove, data with invalid coordinates in new column
#' # titled "TADA.InvalidCoordinates.Flag":
#' # Return ALL data:
#' InvalidCoord_flags <- TADA_FlagCoordinates(Data_Nutrients_UT)
#'
#' # Flag, but do not remove, data with invalid coordinates in new column
#' # titled "TADA.InvalidCoordinates.Flag"
#' # Return ONLY the flagged data:
#' InvalidCoord_flags_flaggedonly <- TADA_FlagCoordinates(Data_Nutrients_UT, flaggedonly = TRUE)
#'
#' # Remove data with coordinates outside the USA, but keep flagged data with
#' # imprecise coordinates:
#' OutsideUSACoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove")
#'
#' # Change the sign of coordinates flagged as outside the USA and keep all
#' # flagged data:
#' OutsideUSACoord_changed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "change sign")
#'
#' # Remove data with imprecise coordinates, but keep flagged data with coordinates outside the USA;
#' # imprecise data may have less than 3 significant figures to the right
#' # of the decimal point:
#' ImpreciseCoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_imprecise = TRUE)
#'
#' # Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
#' InvalidCoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove", clean_imprecise = TRUE)
#'
TADA_FlagCoordinates <- function(.data,
                                 clean_outsideUSA = c("no", "remove", "change sign"),
                                 clean_imprecise = FALSE,
                                 flaggedonly = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check clean_outsideUSA is character
  TADA_CheckType(clean_outsideUSA, "character")
  # check clean_imprecise is boolean
  TADA_CheckType(clean_imprecise, "logical")
  # check .data has required columns
  TADA_CheckColumns(.data, c("TADA.LatitudeMeasure", "TADA.LongitudeMeasure"))
  # check lat and long are "numeric"
  if (!is.numeric(.data$TADA.LongitudeMeasure)) {
    warning("TADA.LongitudeMeasure field must be numeric")
  }

  if (!is.numeric(.data$TADA.LatitudeMeasure)) {
    warning("TADA.LatitudeMeasure field must be numeric")
  }
  # check that clean_outsideUSA is either "no", "remove", or "change sign"
  clean_outsideUSA <- match.arg(clean_outsideUSA)

  orig_dim <- dim(.data)[1]

  # execute function after checks are passed
  .data <- .data %>%
    dplyr::mutate(TADA.InvalidCoordinates.Flag = dplyr::case_when(
      TADA.LatitudeMeasure < -11.046934 & TADA.LatitudeMeasure > -14.548699 & TADA.LongitudeMeasure < -168.1433 & TADA.LongitudeMeasure > -171.089874 ~ NA_character_, # American Samoa
      TADA.LatitudeMeasure < 20.553802 & TADA.LatitudeMeasure > 14.110472 & TADA.LongitudeMeasure < 146.064818 & TADA.LongitudeMeasure > 144.886331 ~ NA_character_, # Northern Mariana Islands
      TADA.LatitudeMeasure < 13.654383 & TADA.LatitudeMeasure > 13.234189 & TADA.LongitudeMeasure < 144.956712 & TADA.LongitudeMeasure > 144.618068 ~ NA_character_, # Guam
      TADA.LatitudeMeasure < 0 ~ "LAT_OutsideUSA",
      TADA.LongitudeMeasure > 0 & TADA.LongitudeMeasure < 145 ~ "LONG_OutsideUSA",
      # for below, lat and long fields must be numeric
      # this checks if there are at least 3 significant figures to the
      # right of the decimal point
      sapply(.data$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 3 |
        sapply(.data$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 3 ~ "Imprecise_lessthan3decimaldigits"
    ))

  # Fill in flag for coordinates that appear OK/PASS tests
  .data$TADA.InvalidCoordinates.Flag[is.na(.data$TADA.InvalidCoordinates.Flag)] <- "OK"

  # if clean_imprecise is TRUE, remove imprecise station metadata
  if (clean_imprecise == TRUE) {
    .data <- dplyr::filter(
      .data,
      !TADA.InvalidCoordinates.Flag %in% "Imprecise_lessthan3decimaldigits"
    )
  }

  # if clean_outsideUSA is "remove", remove stations flagged as outside the USA
  if (clean_outsideUSA == "remove") {
    .data <- dplyr::filter(
      .data,
      !TADA.InvalidCoordinates.Flag %in% c("LAT_OutsideUSA", "LONG_OutsideUSA")
    )
  }

  # if clean_outsideUSA is "change sign", change the sign of lat/long coordinates outside of USA
  if (clean_outsideUSA == "change sign") {
    print("When clean_outsideUSA == change sign, the sign for any lat/long coordinates flagged as outside of USA are switched. This is a temporary solution. Data owners should fix the raw data to address invalid coordinates through WQX. For assistance fixing data errors you see in the WQP, email the WQX helpdesk (WQX@epa.gov).")
    .data <- .data %>%
      dplyr::mutate(
        TADA.LatitudeMeasure = dplyr::case_when(
          TADA.InvalidCoordinates.Flag == "LAT_OutsideUSA" ~ TADA.LatitudeMeasure * (-1),
          TRUE ~ TADA.LatitudeMeasure
        ),
        TADA.LongitudeMeasure = dplyr::case_when(
          TADA.InvalidCoordinates.Flag == "LONG_OutsideUSA" ~ TADA.LongitudeMeasure * (-1),
          TRUE ~ TADA.LongitudeMeasure
        )
      )
  }

  # return only flagged data if flaggedonly = true
  if ((flaggedonly == TRUE)) {
    .data <- dplyr::filter(.data, !TADA.InvalidCoordinates.Flag %in% c("OK"))
  }

  if (all(.data$TADA.InvalidCoordinates.Flag %in% c("OK")) == TRUE) {
    if (orig_dim == dim(.data)[1]) {
      print("Your dataframe does not contain monitoring stations with invalid coordinates. Returning input dataframe with TADA.InvalidCoordinates.Flag column for tracking.")
    } else {
      print("All invalid coordinates were removed. Returning input dataframe with TADA.InvalidCoordinates.Flag column for tracking.")
    }
  }
  .data <- TADA_OrderCols(.data)
  return(.data)
}

#' Identify Potentially Duplicated Data Uploads by Multiple Organizations
#'
#' Identifies data records uploaded by different organizations with the same date,
#' time, characteristic name, and result value within X meters of each other and
#' flags as potential duplicates. However, it is at the discretion of the data user
#' to determine if the data records are unique or represent overlap that could cause
#' issues in the data analysis.
#'
#' This function runs TADA_FindNearbySites within it which adds the
#' TADA.NearbySiteGroups field. Duplicates are only flagged as duplicates if
#' the distance between sites is less than the function input dist_buffer
#' (default is 100m). Each group in the TADA.NearbySiteGroups field indicates
#' that the sites within each group are within the specified distance from each other.
#'
#' @param .data TADA dataframe
#'
#' @param dist_buffer Numeric. The distance in meters below which two sites with
#' measurements at the same time on the same day of the same parameter will
#' be flagged as potential duplicates.
#'
#' @param org_hierarchy Vector of organization identifiers that acts as the
#'   order in which the function should select a result as the representative
#'   duplicate, based on the organization that collected the data. If left
#'   blank, the function chooses the representative duplicate result at random.
#'
#' @return The same input TADA dataframe with additional columns: a
#'   TADA.MultipleOrgDuplicate column indicating if there is evidence that
#'   results are likely duplicated due to submission of the same dataset by two
#'   or more different organizations, a TADA.MultipleOrgDupGroupID column
#'   containing a number unique to results that may represent duplicated
#'   measurement events, a TADA.ResultSelectedMultipleOrgs column indicating
#'   which rows are selected to keep (Y) and remove (N) based on the
#'   org hierarchy, and a TADA.NearbySiteGroups column indicating which
#'   monitoring locations are within the distance buffer from each other.
#'
#' @export
#'
#' @examples
#' # Load dataset
#' dat <- TADA_DataRetrieval(startDate = "2022-09-01", endDate = "2023-05-01", statecode = "PA", sampleMedia = "Water")
#' unique(dat$OrganizationIdentifier)
#' # If duplicates across organizations exist, pick the result belonging to "21PA_WQX" if available.
#' dat1 <- TADA_FindPotentialDuplicatesMultipleOrgs(dat, dist_buffer = 100, org_hierarchy = c("21PA_WQX"))
#' table(dat1$TADA.ResultSelectedMultipleOrgs)
#'
TADA_FindPotentialDuplicatesMultipleOrgs <- function(.data, dist_buffer = 100, org_hierarchy = "none") {
  # get all data that are not NA and round to 2 digits
  dupsprep <- .data %>%
    dplyr::select(OrganizationIdentifier, ResultIdentifier, ActivityStartDate, ActivityStartTime.Time, TADA.CharacteristicName, ActivityTypeCode, TADA.ResultMeasureValue) %>%
    dplyr::filter(!is.na(TADA.ResultMeasureValue)) %>%
    dplyr::mutate(roundRV = round(TADA.ResultMeasureValue, digits = 2))
  # group by date, time, characteristic, and rounded result value and summarise the number of organizations that have those same row values, and filter to those summary rows with more than one organization
  dups_sum <- dupsprep %>%
    dplyr::group_by(ActivityStartDate, ActivityStartTime.Time, TADA.CharacteristicName, ActivityTypeCode, roundRV) %>%
    dplyr::summarise(numorgs = length(unique(OrganizationIdentifier))) %>%
    dplyr::filter(numorgs > 1)

  # if there are potential duplicates based on grouping above, check if sites are nearby
  if (dim(dups_sum)[1] > 0) {
    # give potential duplicates a grouping ID
    dups_sum$TADA.MultipleOrgDupGroupID <- seq(1:dim(dups_sum)[1])
    # merge to narrow dataset to match to true result value
    dupsdat <- merge(dups_sum, dupsprep, all.x = TRUE)
    # make sure potential dupes are within 10% of the max value in the group
    dupsdat <- dupsdat %>%
      dplyr::group_by(TADA.MultipleOrgDupGroupID) %>%
      dplyr::mutate(maxRV = max(TADA.ResultMeasureValue)) %>%
      dplyr::mutate(within10 = ifelse(min(TADA.ResultMeasureValue) >= 0.9 * maxRV, "Y", "N")) %>%
      dplyr::filter(within10 == "Y") %>%
      dplyr::select(!c(roundRV, maxRV, within10, numorgs)) %>%
      dplyr::ungroup()

    # merge to data
    dupsdat <- dplyr::left_join(dupsdat, .data, by = c(
      "ActivityStartDate",
      "ActivityStartTime.Time",
      "TADA.CharacteristicName",
      "ActivityTypeCode",
      "OrganizationIdentifier",
      "ResultIdentifier",
      "TADA.ResultMeasureValue"
    ))

    rm(dupsprep)

    # from those datapoints, determine which are in adjacent sites
    if (!"TADA.NearbySiteGroups" %in% names(.data)) {
      .data <- TADA_FindNearbySites(.data, dist_buffer = dist_buffer)
    }

    dupsites <- unique(.data[, c("MonitoringLocationIdentifier", "TADA.LatitudeMeasure", "TADA.LongitudeMeasure", "TADA.NearbySiteGroups")])

    # get rid of results with no site group added - not duplicated spatially
    dupsites <- subset(dupsites, !dupsites$TADA.NearbySiteGroups %in% c("No nearby sites"))

    if (dim(dupsites)[1] > 0) { # if results are potentially duplicated temporally and spatially, determine if they share site group membership
      # narrow down dataset more
      dupsdat <- subset(dupsdat, dupsdat$MonitoringLocationIdentifier %in% dupsites$MonitoringLocationIdentifier)

      if (dim(dupsdat)[1] > 0) { # nearby sites exist but do not overlap with potentially duplicated result values
        colnum <- max(stringr::str_count(dupsites$TADA.NearbySiteGroups, ","), na.rm = TRUE) + 1
        colnam <- paste0("TADA.SiteGroup", 1:colnum)
        sitecomp <- suppressWarnings(dupsites %>% dplyr::select(MonitoringLocationIdentifier, TADA.NearbySiteGroups) %>%
          tidyr::separate(TADA.NearbySiteGroups, into = colnam, sep = ", ") %>%
          tidyr::pivot_longer(dplyr::all_of(colnam), values_to = "AllGroups") %>%
          dplyr::filter(!is.na(AllGroups)) %>%
          dplyr::distinct()) # get unique groups in dataset

        dupids <- unique(dupsdat$TADA.MultipleOrgDupGroupID) # will loop by ID

        # create empty dataframe to hold result of spatial grouping tests
        dupdata <- data.frame()

        for (i in 1:length(dupids)) { # loop through each duplicate group
          dat <- subset(dupsdat, dupsdat$TADA.MultipleOrgDupGroupID == dupids[i])
          if (dim(dat)[1] > 1 & length(unique(dat$OrganizationIdentifier)) > 1) { # if more than one result in dataset and multiple organizations
            sitecompj <- subset(sitecomp, sitecomp$MonitoringLocationIdentifier %in% dat$MonitoringLocationIdentifier) # grab all sites in data subset and the nearby site groups in which they belong
            for (j in 1:length(unique(sitecompj$AllGroups))) { # loop through each group
              sitegp <- subset(sitecompj, sitecompj$AllGroups == unique(sitecompj$AllGroups)[j]) # grab all sites within each group
              if (dim(sitegp)[1] > 1) { # if there's more than one site in this group (indicating that the potential duplicates are from nearby sites that share the same nearby group)
                dat$TADA.MultipleOrgDuplicate <- ifelse(dat$MonitoringLocationIdentifier %in% sitegp$MonitoringLocationIdentifier, "Y", "N") # mark as "Y" probable duplicate if sites in dup id subset are in group id subset.
                dupdata <- plyr::rbind.fill(dupdata, dat) # bind to all other potential duplicate results - note that this will result in duplicated result identifiers if the potential duplicate result sites belong to multiple nearby site groups.
              }
            }
          }
        }

        if (dim(dupdata)[1] > 0) {
          dupdata <- subset(dupdata, dupdata$TADA.MultipleOrgDuplicate == "Y") %>% dplyr::distinct() # subset to those potential duplicate results that are at nearby sites and remove duplicated result identifiers that may appear in this df multiple times due to more than 1 nearby group membership.
          # make a selection of a representative result
          if (!any(org_hierarchy == "none")) { # if there is an org hierarchy, use that to pick result with lowest rank in hierarchy
            data_orgs <- unique(.data$OrganizationIdentifier)
            if (any(!org_hierarchy %in% data_orgs)) {
              print("One or more organizations in input hierarchy are not present in the input dataset.")
            }
            hierarchy_df <- data.frame("OrganizationIdentifier" = org_hierarchy, "rank" = 1:length(org_hierarchy))
            dupranks <- dupdata %>%
              dplyr::select(ResultIdentifier, OrganizationIdentifier, TADA.MultipleOrgDupGroupID) %>%
              dplyr::left_join(hierarchy_df)
          } else {
            dupranks <- dupdata %>%
              dplyr::select(ResultIdentifier, TADA.MultipleOrgDupGroupID) %>%
              dplyr::mutate(rank = 99)
          }

          dupranks$rank[is.na(dupranks$rank)] <- 99
          duppicks <- dupranks %>%
            dplyr::select(ResultIdentifier, TADA.MultipleOrgDupGroupID, rank) %>%
            dplyr::group_by(TADA.MultipleOrgDupGroupID) %>%
            dplyr::slice_min(rank) %>%
            dplyr::slice_sample(n = 1)

          dupdata$TADA.ResultSelectedMultipleOrgs <- "N"
          dupdata$TADA.ResultSelectedMultipleOrgs <- ifelse(dupdata$ResultIdentifier %in% duppicks$ResultIdentifier, "Y", dupdata$TADA.ResultSelectedMultipleOrgs)

          # connect back to original dataset
          .data <- merge(.data, dupdata[, c("ResultIdentifier", "TADA.MultipleOrgDupGroupID", "TADA.MultipleOrgDuplicate", "TADA.ResultSelectedMultipleOrgs")], all.x = TRUE)
          .data$TADA.MultipleOrgDuplicate[is.na(.data$TADA.MultipleOrgDuplicate)] <- "N"
          .data$TADA.MultipleOrgDupGroupID[.data$TADA.MultipleOrgDuplicate == "N"] <- "Not a duplicate"
          .data$TADA.ResultSelectedMultipleOrgs[is.na(.data$TADA.ResultSelectedMultipleOrgs)] <- "Y"

          print(paste0(length(dupdata$TADA.MultipleOrgDuplicate[dupdata$TADA.MultipleOrgDuplicate %in% c("Y")]), " potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.MultipleOrgDupGroupID column and the TADA.MultipleOrgDuplicate column is set to 'Y' (yes). If you provided an organization hierarchy, the result with the lowest ranked organization identifier was selected as the representative result in the TADA.ResultSelectedMultipleOrgs (this column is set to 'Y' for all results either selected or not considered duplicates)."))
        } else { # potential dup results at nearby sites but no actual duplicates at nearby sites comparing on day, time, characteristic, etc.
          .data$TADA.MultipleOrgDupGroupID <- "Not a duplicate"
          .data$TADA.MultipleOrgDuplicate <- "N"
          .data$TADA.ResultSelectedMultipleOrgs <- "Y"
          print("No duplicate results detected. Returning input dataframe with duplicate flagging columns set to 'N'.")
        }
      } else { # no overlap between dup results and dup sites
        .data$TADA.MultipleOrgDupGroupID <- "Not a duplicate"
        .data$TADA.MultipleOrgDuplicate <- "N"
        .data$TADA.ResultSelectedMultipleOrgs <- "Y"
        print("No duplicate results detected. Returning input dataframe with duplicate flagging columns set to 'N'.")
      }
    } else { # if no site duplicates detected
      .data$TADA.MultipleOrgDupGroupID <- "Not a duplicate"
      .data$TADA.MultipleOrgDuplicate <- "N"
      .data$TADA.ResultSelectedMultipleOrgs <- "Y"
      print("No duplicate results detected. Returning input dataframe with duplicate flagging columns set to 'N'.")
    }
  } else { # if no result/org duplicates detected
    .data$TADA.MultipleOrgDupGroupID <- "Not a duplicate"
    .data$TADA.MultipleOrgDuplicate <- "N"
    .data$TADA.ResultSelectedMultipleOrgs <- "Y"
    print("No duplicate results detected. Returning input dataframe with duplicate flagging columns set to 'N'.")
  }

  .data <- TADA_OrderCols(.data)

  return(.data)
}

#' Identify Potentially Duplicated Data Uploads by a Single Organization
#'
#' Identifies data records uploaded by the same organization with the same date,
#' time, monitoring location, activity type, characteristic name, fraction,
#' taxonomic name, depth columns, and result value and flags as potential
#' duplicates. However, it is at the discretion of the data user to determine if
#' the data records are unique or represent overlap that could cause issues in
#' the data analysis. Note, the dataset may contain data from multiple
#' organizations: the function performs the same analysis on data from each
#' organization.
#'
#' @param .data TADA dataframe
#' @return The same input TADA dataframe with additional columns: a
#'   TADA.SingleOrgDupGroupID column indicating whether a result is part of a
#'   group that shares the same date, time, location, characteristic, etc. If
#'   multiple rows include duplicates within a single organization, the rows will
#'   have the same number identifier in the TADA.SingleOrgDupGroupID column.
#'   In addition, the column TADA.SingleOrgDup.Flag is added, which randomly
#'   flags rows within each TADA.SingleOrgDupGroupID group for removal. Rows
#'   randomly selected for potential removal within a duplicate group will have the
#'   TADA.SingleOrgDup.Flag = 'Duplicate' and  all other results in the group will have
#'   the value TADA.SingleOrgDup.Flag = 'Unique'.
#'
#' @export
#'
#' @examples
#' # Load dataset
#' data(Data_6Tribes_5y)
#' # If duplicates exist, identify and flag them for removal
#' Data_6Tribes_5y_dups <- TADA_FindPotentialDuplicatesSingleOrg(Data_6Tribes_5y)
#' table(Data_6Tribes_5y_dups$TADA.SingleOrgDup.Flag)
#'
TADA_FindPotentialDuplicatesSingleOrg <- function(.data) {
  # find the depth columns in the dataset
  depthcols <- names(.data)[grepl("^TADA.*DepthHeightMeasure.MeasureValue$", names(.data))]

  # tack depth columns onto additional grouping columns
  colss <- c("OrganizationIdentifier", "MonitoringLocationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "ActivityTypeCode", "TADA.CharacteristicName", "SubjectTaxonomicName", "TADA.ResultSampleFractionText", "TADA.ResultMeasureValue", depthcols)

  # find where the grouping using the columns above results in more than one result identifier
  dups_sum_org <- .data %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(colss))) %>%
    dplyr::summarise(numres = length(unique(ResultIdentifier))) %>%
    dplyr::filter(numres > 1) %>%
    dplyr::mutate(TADA.SingleOrgDupGroupID = dplyr::cur_group_id())

  if (dim(dups_sum_org)[1] > 0) {
    # apply to .data and remove numbers column
    .data <- merge(.data, dups_sum_org, all.x = TRUE)
    .data <- .data %>% dplyr::select(-numres)
    .data$TADA.SingleOrgDupGroupID[is.na(.data$TADA.SingleOrgDupGroupID)] <- "Not a duplicate"

    # flag rows randomly within a duplicate group for potential removal
    dup_rids <- subset(.data, !is.na(.data$TADA.SingleOrgDupGroupID))$ResultIdentifier
    picks <- .data %>%
      dplyr::filter(!is.na(TADA.SingleOrgDupGroupID)) %>%
      dplyr::group_by(TADA.SingleOrgDupGroupID) %>%
      dplyr::slice_sample(n = 1)
    .data$TADA.SingleOrgDup.Flag <- "Duplicate"
    # flags potential duplicates as "Duplicate" for easy filtering
    .data$TADA.SingleOrgDup.Flag <- ifelse(.data$ResultIdentifier %in% picks$ResultIdentifier, "Unique", .data$TADA.SingleOrgDup.Flag)
    # flags non-duplicates as passing
    .data$TADA.SingleOrgDup.Flag <- ifelse(.data$TADA.SingleOrgDupGroupID == "Not a duplicate", "Unique", .data$TADA.SingleOrgDup.Flag)
    print(paste0(dim(dups_sum_org)[1], " groups of potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.SingleOrgDupGroupID column and the function randomly selected one result from each group to represent a single, unduplicated value. Selected values are indicated in the TADA.SingleOrgDup.Flag as 'Unique', while duplicates are flagged as 'Duplicate' for easy filtering."))
  }

  .data <- TADA_OrderCols(.data)
  return(.data)
}