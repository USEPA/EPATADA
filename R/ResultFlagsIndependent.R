#' Check for Suspect Analytical Methods
#'
#' Function checks the validity of each characteristic-analytical method
#' combination in the dataframe. When clean = TRUE, rows with Suspect
#' characteristic-analytical method combinations are removed. Default is
#' clean = FALSE. When flaggedonly = TRUE, dataframe is filtered to show only
#' Suspect characteristic-analytical method combinations. Default is
#' flaggedonly = FALSE.
#'
#' The “Not Reviewed” value within "TADA.AnalyticalMethod.Flag" means
#' that the EPA WQX team has not yet reviewed the combinations
#' (see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
#' The WQX team plans to review and update these new combinations quarterly.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument; removes "Suspect" characteristic-analytical
#' method combinations from the dataframe when clean = TRUE. Default is
#' clean = FALSE.
#' @param flaggedonly Boolean argument; filters dataframe to show only "Suspect"
#' characteristic-analytical method combinations when flaggedonly = TRUE. Default
#' is flaggedonly = FALSE.
#'
#' @return This function adds the TADA.AnalyticalMethod.Flag to a TADA dataframe. This column
#' flags Suspect CharacteristicName, ResultAnalyticalMethod/MethodIdentifier,
#' and ResultAnalyticalMethod/MethodIdentifierContext combinations in your dataframe
#' as either "Not Reviewed", "Suspect", or "Pass". When clean = FALSE and
#' flaggedonly = TRUE, the dataframe is filtered to show only "Suspect"
#' characteristic-analytical method combinations; the column TADA.AnalyticalMethod.Flag
#' is still appended. When clean = TRUE and flaggedonly = FALSE, "Suspect" rows
#' are removed from the dataframe and no column will be appended.
#'
#' @export
#'
#' @examples
#' # Load example dataset
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Remove Suspect characteristic-analytical method combinations from
#' # data frame:
#' SuspectMethod_clean <- TADA_FlagMethod(Data_R5_TADAPackageDemo,
#'   clean = TRUE
#' )
#'
#' # Flag, but do not remove, Suspect characteristic-analytical method
#' # combinations
#' # in new column titled "TADA.AnalyticalMethod.Flag":
#' SuspectMethod_flags <- TADA_FlagMethod(Data_R5_TADAPackageDemo,
#'   clean = FALSE
#' )
#'
#' # Show only Suspect characteristic-analytical method combinations:
#' SuspectMethod_flaggedonly <- TADA_FlagMethod(Data_R5_TADAPackageDemo,
#'   clean = FALSE, flaggedonly = TRUE
#' )
#'
TADA_FlagMethod <- function(.data, clean = FALSE, flaggedonly = FALSE) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext"
  )
  TADA_CheckColumns(.data, expected_cols)
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop(
      "Function not executed because clean and flaggedonly cannot both be TRUE"
    )
  }

  # execute function after checks are passed - removes flag column in case reference table has changed.
  # delete existing flag column
  if (("TADA.AnalyticalMethod.Flag" %in% colnames(.data)) == TRUE) {
    .data <- dplyr::select(.data, -TADA.AnalyticalMethod.Flag)
  }
  # read in WQX val reference table and filter
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)
  meth.ref <- dplyr::filter(WQXcharValRef, Type == "CharacteristicMethod")
  rm(WQXcharValRef)

  # join "TADA.WQXVal.Flag" column to .data by CharacteristicName, Source (Media), and Value (unit)
  check.data <- merge(
    .data,
    meth.ref[, c("Characteristic", "Source", "Value", "TADA.WQXVal.Flag")],
    by.x = c(
      "TADA.CharacteristicName",
      "ResultAnalyticalMethod.MethodIdentifier",
      "ResultAnalyticalMethod.MethodIdentifierContext"
    ),
    by.y = c("Characteristic", "Value", "Source"),
    all.x = TRUE
  )

  # rename TADA.WQXVal.Flag column to WQX.AnalyticalMethodValidity
  check.data <- check.data |>
    dplyr::rename(TADA.AnalyticalMethod.Flag = TADA.WQXVal.Flag) |>
    dplyr::distinct()
  # rename NA values to NonStandardized in WQX.AnalyticalMethodValidity column
  check.data["TADA.AnalyticalMethod.Flag"][is.na(check.data[
    "TADA.AnalyticalMethod.Flag"
  ])] <- "Not Reviewed"

  if (flaggedonly == FALSE) {
    # if all rows are "Pass" or NA "Not Reviewed", return input unchanged
    ## note: Cristina edited this on 9/19/22 to keep Not Reviewed/NA data when clean = TRUE. Now only Suspect data is removed.
    if (
      any("Suspect" %in% unique(check.data$TADA.AnalyticalMethod.Flag)) == FALSE
    ) {
      message(
        "No Suspect method/characteristic combinations in your dataframe. Returning the input dataframe with TADA.AnalyticalMethod.Flag column for tracking."
      )
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
      # filter out Suspect characteristic-unit-method combinations
      clean.data <- dplyr::filter(
        check.data,
        TADA.AnalyticalMethod.Flag != "Suspect"
      )

      # remove WQX.AnalyticalMethodValidity column
      # clean.data <- dplyr::select(clean.data, -TADA.AnalyticalMethod.Flag)
      clean.data <- TADA_OrderCols(clean.data)
      return(clean.data)
    }
  }

  # flagged output, errors only
  if (clean == FALSE & flaggedonly == TRUE) {
    # filter to show only Suspect characteristic-unit-method combinations
    Suspect.data <- dplyr::filter(
      check.data,
      TADA.AnalyticalMethod.Flag == "Suspect"
    )
    if (nrow(Suspect.data) == 0) {
      # Suspect.data <- dplyr::select(Suspect.data, -TADA.AnalyticalMethod.Flag)
      message(
        "This dataframe is empty because we did not find any Suspect method/characteristic combinations in your dataframe"
      )
    }
    Suspect.data <- TADA_OrderCols(Suspect.data)
    return(Suspect.data)
  }
}


#' Flag Continuous Data
#'
#' Continuous data may (or may not) be suitable for integration with discrete
#' water quality data for analyses. Therefore, this function uses metadata
#' submitted by data providers to flag rows with continuous data.
#'
#' Continuous data is often aggregated to a daily avg, max, and min value,
#' or another statistic of interest to the data submitter. Alternatively, some
#' organizations aggregate their high frequency data (15 min or 1 hour data)
#' to 2 or 4 hour interval averages. In all of these scenarios, the data provider
#' may have also included the raw data (full continuous time series) as a text file
#' attachment at the activity level.
#'
#' @param .data TADA dataframe
#' @param clean Boolean argument: When clean = FALSE (default), a column titled
#' "TADA.ContinuousData.Flag" is added to the dataframe to indicate if
#' each row includes "Continuous" or "Discrete" data. When clean = TRUE, rows
#' with "Continuous" data are removed from the dataframe and no column is appended.
#' @param flaggedonly Boolean argument: When flaggedonly = FALSE (default), all
#' results are included in the output. When flaggedonly = TRUE, the dataframe
#' will be filtered to include only the rows flagged as "Continuous" results.
#' @param time_difference Numeric argument defining the maximum time difference
#' in hours between measurements of the same TADA.ComparableDataIdentifier taken at the same
#' latitude, longitude, and depth. This is used to search for
#' continuous time series data (i.e., if there are multiple measurements within the selected
#' time_difference, then the row will be flagged as continuous). The default time window is 4 hours.
#' The time_difference can be adjusted by the user.
#' @return The default is clean = FALSE and flaggedonly = FALSE.
#' When clean = FALSE and flaggedonly = FALSE (default), a new column,
#' "TADA.ContinuousData.Flag", is appended to the input data set which
#' flags each row as "Continuous" or "Discrete".
#' When clean = FALSE and flaggedonly = TRUE, the dataframe is filtered to show
#' only the flagged continuous data and the flag column is still appended.
#' When clean = TRUE and flaggedonly = FALSE, continuous data is
#' removed from the dataframe and no column is appended.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' all_data <- TADA_DataRetrieval(project = c(
#'   "Continuous LC1",
#'   "MA_Continuous", "Anchorage Bacteria 20-21"
#' ), ask = FALSE)
#'
#' # Flag continuous data in new column titled "TADA.ContinuousData.Flag"
#' all_data_flags <- TADA_FlagContinuousData(all_data, clean = FALSE)
#'
#' # Show only rows flagged as continuous data (note that all results are
#' # flagged in the example)
#' all_data_flaggedonly <- TADA_FlagContinuousData(all_data,
#'   clean = FALSE, flaggedonly = TRUE
#' )
#'
#' # Remove continuous data in dataframe (note that this dataframe will
#' # have 0 results because all are flagged in the example)
#' all_data_clean <- TADA_FlagContinuousData(all_data, clean = TRUE)
#'
#' utils::data(Data_Nutrients_UT)
#'
#' # Flag continuous data in new column titled "TADA.ContinuousData.Flag"
#' Data_Nutrients_UT_flags <- TADA_FlagContinuousData(Data_Nutrients_UT,
#'   clean = FALSE
#' )
#' unique(Data_Nutrients_UT_flags$TADA.ContinuousData.Flag)
#'
#' # Show only rows flagged as continuous data
#' Data_Nutrients_UT_flaggedonly <- TADA_FlagContinuousData(Data_Nutrients_UT,
#'   clean = FALSE, flaggedonly = TRUE
#' )
#'
#' # Remove continuous data in dataframe
#' Data_Nutrients_UT_clean <- TADA_FlagContinuousData(Data_Nutrients_UT,
#'   clean = TRUE
#' )
#' unique(Data_Nutrients_UT_clean$TADA.ContinuousData.Flag)
#' }
#'
TADA_FlagContinuousData <- function(
  .data,
  clean = FALSE,
  flaggedonly = FALSE,
  time_difference = 4
) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "ActivityTypeCode",
    "SampleCollectionEquipmentName",
    "ResultDetectionConditionText",
    "ResultTimeBasisText",
    "StatisticalBaseCode",
    "ResultValueTypeName",
    "ResultIdentifier",
    "OrganizationIdentifier",
    "ActivityRelativeDepthName"
  )
  TADA_CheckColumns(.data, expected_cols)
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # Ensure 'clean' and 'flaggedonly' are not both TRUE, as this is not a valid state
  if (clean == TRUE & flaggedonly == TRUE) {
    stop(
      "Function not executed because clean and flaggedonly cannot both be TRUE"
    )
  }

  # Run autoclean if necessary to prepare the data
  if (!"TADA.ActivityMediaName" %in% colnames(.data)) {
    .data <- TADA_AutoClean(.data)
  }

  if (!"TADA.LatitudeMeasure" %in% colnames(.data)) {
    .data <- TADA_AutoClean(.data)
  }

  if (!"TADA.LongitudeMeasure" %in% colnames(.data)) {
    .data <- TADA_AutoClean(.data)
  }

  # Run quality control check if necessary
  if (!"TADA.ActivityType.Flag" %in% colnames(.data)) {
    .data <- TADA_FindQCActivities(.data)
  }

  # Create comparable data identifier if necessary
  if (!"TADA.ComparableDataIdentifier" %in% colnames(.data)) {
    .data <- TADA_CreateComparableID(.data)
  }

  # Initialize all data as "Discrete"
  .data$TADA.ContinuousData.Flag <- "Discrete"

  # Identify continuous data based on various criteria
  cont.data <- .data |>
    dplyr::filter(TADA.ActivityType.Flag == "Non_QC") |>
    dplyr::filter(
      ActivityTypeCode == "Field Msr/Obs-Continuous Time Series" |
        grepl("Continuous", ProjectIdentifier) |
        grepl("CONTINUOUS", ProjectIdentifier) |
        (ActivityTypeCode == "Sample-Integrated Time Series" &
          SampleCollectionEquipmentName == "Probe/Sensor") |
        (ActivityTypeCode == "Field Msr/Obs-Portable Data Logger" &
          !is.na(ResultTimeBasisText)) |
        (ActivityTypeCode == "Field Msr/Obs-Portable Data Logger" &
          !is.na(StatisticalBaseCode)) |
        (ActivityTypeCode == "Field Msr/Obs-Portable Data Logger" &
          ResultValueTypeName == "Calculated") |
        (ActivityTypeCode == "Field Msr/Obs-Portable Data Logger" &
          ResultValueTypeName == "Estimated") |
        (SampleCollectionEquipmentName == "Probe/Sensor" &
          !is.na(ResultTimeBasisText)) |
        (SampleCollectionEquipmentName == "Probe/Sensor" &
          !is.na(StatisticalBaseCode)) |
        (SampleCollectionEquipmentName == "Probe/Sensor" &
          ResultValueTypeName == "Calculated") |
        (SampleCollectionEquipmentName == "Probe/Sensor" &
          ResultValueTypeName == "Estimated")
    ) |>
    dplyr::mutate(TADA.ContinuousData.Flag = "Continuous")

  # Identify non-continuous data
  noncont.data <- subset(
    .data,
    !.data$ResultIdentifier %in% cont.data$ResultIdentifier
  )

  # If there are non-continuous data, further check for time-based continuity
  if (length(noncont.data) >= 1) {
    info_match <- noncont.data |>
      dplyr::filter(TADA.ActivityType.Flag == "Non_QC") |>
      dplyr::group_by(
        TADA.LatitudeMeasure,
        TADA.LongitudeMeasure,
        OrganizationIdentifier,
        TADA.ComparableDataIdentifier,
        TADA.ActivityDepthHeightMeasure.MeasureValue,
        TADA.ResultDepthHeightMeasure.MeasureValue,
        TADA.ActivityBottomDepthHeightMeasure.MeasureValue,
        TADA.ActivityTopDepthHeightMeasure.MeasureValue,
        ActivityRelativeDepthName
      ) |>
      dplyr::mutate(n_records = length(TADA.ResultMeasureValue)) |>
      dplyr::mutate(group_id = dplyr::cur_group_id()) |>
      dplyr::filter(n_records > 1) |>
      dplyr::ungroup() |>
      dplyr::group_by(group_id) |>
      dplyr::arrange(ActivityStartDateTime, .by_group = TRUE) |>
      dplyr::mutate(
        time_diff_lag = abs(difftime(
          ActivityStartDateTime,
          dplyr::lag(ActivityStartDateTime),
          units = "hours"
        )),
        time_diff_lead = abs(difftime(
          ActivityStartDateTime,
          dplyr::lead(ActivityStartDateTime),
          units = "hours"
        ))
      ) |>
      dplyr::ungroup()

    # Flag as continuous if time differences are within the specified window
    within_window <- info_match |>
      dplyr::filter(
        time_diff_lead <= time_difference | time_diff_lag <= time_difference
      )

    rm(info_match)

    # Update flag for continuous data
    noncont.data <- noncont.data |>
      dplyr::mutate(
        TADA.ContinuousData.Flag = ifelse(
          ResultIdentifier %in% within_window$ResultIdentifier,
          "Continuous",
          TADA.ContinuousData.Flag
        )
      )

    rm(within_window)
  }

  # Combine continuous and non-continuous data
  if (nrow(noncont.data) == 0) {
    message(
      "All data is flagged as continuous in TADA.ContinuousData.Flag column."
    )
    flag.data <- cont.data
  } else {
    flag.data <- cont.data |>
      dplyr::full_join(noncont.data, by = c(names(cont.data)))
  }

  # Return data based on the 'clean' and 'flaggedonly' parameters
  if (clean == FALSE & flaggedonly == FALSE) {
    flag.data <- TADA_OrderCols(flag.data)
    return(flag.data)
  }

  if (clean == TRUE & flaggedonly == FALSE) {
    clean.data <- flag.data |>
      dplyr::filter(!(TADA.ContinuousData.Flag %in% "Continuous")) |>
      dplyr::select(-TADA.ContinuousData.Flag) |>
      TADA_OrderCols()
    return(clean.data)
  }

  if (clean == FALSE & flaggedonly == TRUE) {
    onlycont.data <- flag.data |>
      dplyr::filter(TADA.ContinuousData.Flag == "Continuous") |>
      TADA_OrderCols()
    return(onlycont.data)
  }

  if (
    nrow(flag.data[flag.data$TADA.ContinuousData.Flag == "Continuous", ]) == 0
  ) {
    if (flaggedonly == FALSE) {
      message(
        "No evidence of aggregated continuous data in your dataframe. Returning the input dataframe with TADA.ContinuousData.Flag column for tracking."
      )
      .data <- TADA_OrderCols(.data)
      return(.data)
    }

    if (flaggedonly == TRUE) {
      message(
        "This dataframe is empty because we did not find any aggregated continuous data in your dataframe"
      )
      all.cont.data <- flag.data |>
        dplyr::filter(TADA.ContinuousData.Flag == "Continuous")
      return(all.cont.data)
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
#' This function will add the column "TADA.ResultValueAboveUpperThreshold.Flag" which
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
#' @return The input TADA dataset with the added "TADA.ResultValueAboveUpperThreshold.Flag"
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
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Remove data that is above the upper WQX threshold from dataframe:
#' WQXUpperThreshold_clean <- TADA_FlagAboveThreshold(
#'   Data_R5_TADAPackageDemo,
#'   clean = TRUE
#' )
#'
#' # Flag, but do not remove, data that is above the upper WQX threshold in
#' # new column titled "TADA.ResultValueAboveUpperThreshold.Flag":
#' WQXUpperThreshold_flags <- TADA_FlagAboveThreshold(
#'   Data_R5_TADAPackageDemo,
#'   clean = FALSE
#' )
#'
#' # Show only data flagged as above the upper WQX threshold:
#' WQXUpperThreshold_flagsonly <- TADA_FlagAboveThreshold(
#'   Data_R5_TADAPackageDemo,
#'   clean = FALSE, flaggedonly = TRUE
#' )
#'
TADA_FlagAboveThreshold <- function(.data, clean = FALSE, flaggedonly = FALSE) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ActivityMediaName",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # Check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check .data has required columns
  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop(
      "TADA_FlagAboveThreshold: Function not executed because clean and flaggedonly cannot both be TRUE"
    )
  }

  # Check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop(
      "TADA_FlagAboveThreshold: The ResultMeasureValue column must be of class 'numeric'."
    )
  }

  # Execute function after checks are passed

  # Delete existing flag column - removes flag column in case reference table has changed.
  if (
    ("TADA.ResultValueAboveUpperThreshold.Flag" %in% colnames(.data)) == TRUE
  ) {
    .data <- dplyr::select(.data, -TADA.ResultValueAboveUpperThreshold.Flag)
  }

  # Load WQXcharValRef data
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)

  # Filter to include only the units Type
  unit.ref <- dplyr::filter(WQXcharValRef, Type == "CharacteristicUnit")
  rm(WQXcharValRef)

  # Update ref table names to prepare for left join with df
  unit.ref <- unit.ref |>
    dplyr::rename(
      TADA.CharacteristicName = Characteristic,
      TADA.ActivityMediaName = Source,
      TADA.ResultMeasure.MeasureUnitCode = Value.Unit
    )

  # Remove rows where TADA.ResultMeasure.MeasureUnitCode is NA
  unit.ref <- dplyr::filter(
    unit.ref,
    !is.na(TADA.ResultMeasure.MeasureUnitCode) &
      TADA.ResultMeasure.MeasureUnitCode != ""
  )

  # Change NonStandardized to Pass for this function (same)
  unit.ref <- unit.ref |>
    dplyr::mutate(
      TADA.WQXVal.Flag = dplyr::case_when(
        TADA.WQXVal.Flag == "NonStandardized" ~ "Pass",
        TRUE ~ TADA.WQXVal.Flag
      )
    )

  # Identify inconsistent flag groups
  inconsistent_flags <- unit.ref |>
    dplyr::group_by(
      TADA.CharacteristicName,
      TADA.ActivityMediaName,
      TADA.ResultMeasure.MeasureUnitCode
    ) |>
    dplyr::filter(dplyr::n_distinct(TADA.WQXVal.Flag) > 1) |>
    dplyr::ungroup()

  # Keep only rows where TADA.WQXVal.Flag == "Pass" for inconsistent groups & keep all others outside the inconsistent groups
  unit.ref <- unit.ref |>
    dplyr::filter(
      !(TADA.CharacteristicName %in%
        inconsistent_flags$TADA.CharacteristicName &
        TADA.ActivityMediaName %in% inconsistent_flags$TADA.ActivityMediaName &
        TADA.ResultMeasure.MeasureUnitCode %in%
          inconsistent_flags$TADA.ResultMeasure.MeasureUnitCode) |
        (TADA.WQXVal.Flag == "Pass")
    )

  # Remove extraneous columns from unit.ref
  unit.ref <- unit.ref |>
    dplyr::select(
      -c(
        Domain,
        Status,
        Type,
        Unique.Identifier,
        Note.Recommendation,
        Conversion.Factor,
        Conversion.Coefficient,
        Last.Change.Date,
        Value,
        Minimum
      )
    ) |>
    dplyr::distinct()

  # Join with the input data
  check.data <- dplyr::left_join(
    .data,
    unit.ref,
    by = c(
      "TADA.CharacteristicName",
      "TADA.ActivityMediaName",
      "TADA.ResultMeasure.MeasureUnitCode"
    )
  )

  # Create flag column
  flag.data <- check.data |>
    dplyr::mutate(
      TADA.ResultValueAboveUpperThreshold.Flag = dplyr::case_when(
        TADA.ResultMeasureValue > Maximum ~ as.character("Suspect"),
        TADA.WQXVal.Flag == "Suspect" ~ as.character("Suspect"),
        (TADA.WQXVal.Flag == "Pass" &
          TADA.ResultMeasureValue <= Maximum) ~ as.character("Pass"),
        TADA.WQXVal.Flag == "Not Reviewed" ~ as.character("Not Reviewed"),
        TRUE ~ "NA - Not Available"
      )
    )

  # Count occurrences of each flag
  flag_counts <- table(flag.data$TADA.ResultValueAboveUpperThreshold.Flag)

  # Format the counts for display
  formatted_counts <- paste(
    names(flag_counts),
    flag_counts,
    sep = ": ",
    collapse = ", "
  )

  # Remove Maximum and TADA.WQXVal.Flag column from flag.data
  flag.data <- flag.data |> dplyr::select(-c(Maximum, TADA.WQXVal.Flag))

  # Handle different scenarios based on clean and flaggedonly parameters
  if (
    any(
      "Suspect" %in% unique(flag.data$TADA.ResultValueAboveUpperThreshold.Flag)
    ) ==
      FALSE
  ) {
    if (flaggedonly == FALSE) {
      message(paste(
        "TADA_FlagAboveThreshold: No data above the WQX Upper Threshold was found in your dataframe. Returning the input dataframe with TADA.ResultValueAboveUpperThreshold.Flag column for tracking. Counts: ",
        formatted_counts
      ))
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    if (flaggedonly == TRUE) {
      message(paste(
        "TADA_FlagAboveThreshold: No data above the WQX Upper Threshold was found in your dataframe. Returning an empty dataframe. Counts: ",
        formatted_counts
      ))
      emptyflag.data <- dplyr::filter(
        flag.data,
        TADA.ResultValueAboveUpperThreshold.Flag %in% "Suspect"
      )
      emptyflag.data <- TADA_OrderCols(emptyflag.data)
      return(emptyflag.data)
    }
  }

  # flagged and not cleaned
  if (clean == FALSE & flaggedonly == FALSE) {
    message(paste(
      "TADA_FlagAboveThreshold: Returning the dataframe with flags. Counts: ",
      formatted_counts
    ))
    flag.data <- TADA_OrderCols(flag.data)
    return(flag.data)
  }

  # clean = TRUE and flaggedonly = FALSE
  if (clean == TRUE & flaggedonly == FALSE) {
    # filter out rows where TADA.ResultValueAboveUpperThreshold.Flag = Suspect; remove TADA.ResultValueAboveUpperThreshold.Flag column
    clean.data <- flag.data |>
      dplyr::filter(TADA.ResultValueAboveUpperThreshold.Flag != "Suspect") |>
      dplyr::select(-TADA.ResultValueAboveUpperThreshold.Flag)
    message(paste(
      "TADA_FlagAboveThreshold: Returning cleaned dataframe with 'Suspect' rows removed. Counts: ",
      formatted_counts
    ))
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }

  # flagged, errors only
  if (clean == FALSE & flaggedonly == TRUE) {
    # filter to show only rows above WQX upper threshold
    flagsonly.data <- dplyr::filter(
      flag.data,
      TADA.ResultValueAboveUpperThreshold.Flag %in% "Suspect"
    )
    message(paste(
      "TADA_FlagAboveThreshold: Returning dataframe with only 'Suspect' rows. Counts: ",
      formatted_counts
    ))
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
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Remove data that is below the lower WQX threshold from the dataframe:
#' WQXLowerThreshold_clean <- TADA_FlagBelowThreshold(
#'   Data_R5_TADAPackageDemo,
#'   clean = TRUE
#' )
#'
#' # Flag, but do not remove, data that is below the lower WQX threshold in
#' # new column titled "TADA.ResultValueBelowLowerThreshold.Flag":
#' WQXLowerThreshold_flags <- TADA_FlagBelowThreshold(
#'   Data_R5_TADAPackageDemo,
#'   clean = FALSE
#' )
#'
#' # Show only data that is below the lower WQX threshold:
#' WQXLowerThreshold_flagsonly <- TADA_FlagBelowThreshold(
#'   Data_R5_TADAPackageDemo,
#'   clean = FALSE, flaggedonly = TRUE
#' )
#'
TADA_FlagBelowThreshold <- function(.data, clean = FALSE, flaggedonly = FALSE) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ActivityMediaName",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # Check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop(
      "TADA_FlagBelowThreshold: Function not executed because clean and flaggedonly cannot both be TRUE"
    )
  }

  # Check ResultMeasureValue column is of class numeric
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop(
      "TADA_FlagBelowThreshold: The ResultMeasureValue column must be of class 'numeric'."
    )
  }

  # Execute function after checks are passed

  # Delete existing flag column - removes flag column in case reference table has changed.
  if (
    ("TADA.ResultValueBelowLowerThreshold.Flag" %in% colnames(.data)) == TRUE
  ) {
    .data <- dplyr::select(.data, -TADA.ResultValueBelowLowerThreshold.Flag)
  }

  # Load WQXcharValRef data
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)

  # Filter to include only the units Type
  unit.ref <- dplyr::filter(WQXcharValRef, Type == "CharacteristicUnit")
  rm(WQXcharValRef)

  # Update ref table names to prepare for left join with df
  unit.ref <- unit.ref |>
    dplyr::rename(
      TADA.CharacteristicName = Characteristic,
      TADA.ActivityMediaName = Source,
      TADA.ResultMeasure.MeasureUnitCode = Value.Unit
    )

  # Remove rows where TADA.ResultMeasure.MeasureUnitCode is NA
  unit.ref <- dplyr::filter(
    unit.ref,
    !is.na(TADA.ResultMeasure.MeasureUnitCode) &
      TADA.ResultMeasure.MeasureUnitCode != ""
  )

  # Change NonStandardized to Pass for this function (same)
  unit.ref <- unit.ref |>
    dplyr::mutate(
      TADA.WQXVal.Flag = dplyr::case_when(
        TADA.WQXVal.Flag == "NonStandardized" ~ "Pass",
        TRUE ~ TADA.WQXVal.Flag
      )
    )

  # Identify inconsistent flag groups
  inconsistent_flags <- unit.ref |>
    dplyr::group_by(
      TADA.CharacteristicName,
      TADA.ActivityMediaName,
      TADA.ResultMeasure.MeasureUnitCode
    ) |>
    dplyr::filter(dplyr::n_distinct(TADA.WQXVal.Flag) > 1) |>
    dplyr::ungroup()

  # Keep only rows where TADA.WQXVal.Flag == "Pass" for inconsistent groups & keep all others outside the inconsistent groups
  unit.ref <- unit.ref |>
    dplyr::filter(
      !(TADA.CharacteristicName %in%
        inconsistent_flags$TADA.CharacteristicName &
        TADA.ActivityMediaName %in% inconsistent_flags$TADA.ActivityMediaName &
        TADA.ResultMeasure.MeasureUnitCode %in%
          inconsistent_flags$TADA.ResultMeasure.MeasureUnitCode) |
        (TADA.WQXVal.Flag == "Pass")
    )

  # Remove extraneous columns from unit.ref
  unit.ref <- unit.ref |>
    dplyr::select(
      -c(
        Domain,
        Status,
        Type,
        Unique.Identifier,
        Note.Recommendation,
        Conversion.Factor,
        Conversion.Coefficient,
        Last.Change.Date,
        Value,
        Maximum
      )
    ) |>
    dplyr::distinct()

  # Join with the input data
  check.data <- dplyr::left_join(
    .data,
    unit.ref,
    by = c(
      "TADA.CharacteristicName",
      "TADA.ActivityMediaName",
      "TADA.ResultMeasure.MeasureUnitCode"
    )
  )

  # Create flag column
  flag.data <- check.data |>
    dplyr::mutate(
      TADA.ResultValueBelowLowerThreshold.Flag = dplyr::case_when(
        TADA.ResultMeasureValue < Minimum ~ as.character("Suspect"),
        TADA.WQXVal.Flag == "Suspect" ~ as.character("Suspect"),
        (TADA.WQXVal.Flag == "Pass" &
          TADA.ResultMeasureValue >= Minimum) ~ as.character("Pass"),
        TADA.WQXVal.Flag == "Not Reviewed" ~ as.character("Not Reviewed"),
        TRUE ~ "NA - Not Available"
      )
    )

  # Count occurrences of each flag
  flag_counts <- table(flag.data$TADA.ResultValueBelowLowerThreshold.Flag)

  # Format the counts for display
  formatted_counts <- paste(
    names(flag_counts),
    flag_counts,
    sep = ": ",
    collapse = ", "
  )

  # Remove Minimum and TADA.WQXVal.Flag column from flag.data
  flag.data <- flag.data |> dplyr::select(-c(Minimum, TADA.WQXVal.Flag))

  # Handle different scenarios based on clean and flaggedonly parameters
  if (
    any(
      "Suspect" %in% unique(flag.data$TADA.ResultValueBelowLowerThreshold.Flag)
    ) ==
      FALSE
  ) {
    if (flaggedonly == FALSE) {
      message(paste(
        "TADA_FlagBelowThreshold: No data below the WQX Lower Threshold was found in your dataframe. Returning the input dataframe with TADA.ResultValueBelowLowerThreshold.Flag column for tracking. Counts: ",
        formatted_counts
      ))
      flag.data <- TADA_OrderCols(flag.data)
      return(flag.data)
    }
    if (flaggedonly == TRUE) {
      message(paste(
        "TADA_FlagBelowThreshold: No data below the WQX Lower Threshold was found in your dataframe. Returning an empty dataframe. Counts: ",
        formatted_counts
      ))
      emptyflag.data <- dplyr::filter(
        flag.data,
        TADA.ResultValueBelowLowerThreshold.Flag %in% "Suspect"
      )
      emptyflag.data <- TADA_OrderCols(emptyflag.data)
      return(emptyflag.data)
    }
  }

  # flagged and not cleaned
  if (clean == FALSE & flaggedonly == FALSE) {
    message(paste(
      "TADA_FlagBelowThreshold: Returning the dataframe with flags. Counts: ",
      formatted_counts
    ))
    flag.data <- TADA_OrderCols(flag.data)
    return(flag.data)
  }

  # clean = TRUE and flaggedonly = FALSE
  if (clean == TRUE & flaggedonly == FALSE) {
    # filter out rows where TADA.ResultValueBelowLowerThreshold.Flag = Suspect; remove TADA.ResultValueBelowLowerThreshold.Flag column
    clean.data <- flag.data |>
      dplyr::filter(TADA.ResultValueBelowLowerThreshold.Flag != "Suspect") |>
      dplyr::select(-TADA.ResultValueBelowLowerThreshold.Flag)
    message(paste(
      "TADA_FlagBelowThreshold: Returning cleaned dataframe with 'Suspect' rows removed. Counts: ",
      formatted_counts
    ))
    clean.data <- TADA_OrderCols(clean.data)
    return(clean.data)
  }

  # flagged, errors only
  if (clean == FALSE & flaggedonly == TRUE) {
    # filter to show only rows below WQX lower threshold
    flagsonly.data <- dplyr::filter(
      flag.data,
      TADA.ResultValueBelowLowerThreshold.Flag %in% "Suspect"
    )
    message(paste(
      "TADA_FlagBelowThreshold: Returning dataframe with only 'Suspect' rows. Counts: ",
      formatted_counts
    ))
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
#' remove NAs in addition to N's using the inputs clean = TRUE, cleanNA = TRUE,
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
#' be filtered to show only rows where QAPPApprovedIndicator is "Y" or "NA";
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
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Show data where the QAPPApprovedIndicator equals "Y" or "NA":
#' QAPPapproved_clean <- TADA_FindQAPPApproval(Data_R5_TADAPackageDemo)
#'
#' # Show only data where the QAPPApprovedIndicator equals "Y":
#' QAPPapproved_cleanNAs <- TADA_FindQAPPApproval(Data_R5_TADAPackageDemo, cleanNA = TRUE)
#'
#' # Show data where the QAPPApprovedIndicator equals "N" or "NA":
#' QAPPIndicator_N_NA <- TADA_FindQAPPApproval(Data_R5_TADAPackageDemo,
#'   clean = FALSE,
#'   cleanNA = FALSE, flaggedonly = TRUE
#' )
#'
#' # Show data where the QAPPApprovedIndicator equals "N":
#' QAPPIndicator_N <- TADA_FindQAPPApproval(Data_R5_TADAPackageDemo,
#'   clean = FALSE,
#'   cleanNA = TRUE, flaggedonly = TRUE
#' )
#'
#' # Note: When clean = FALSE, cleanNA = FALSE, and flaggedonly = FALSE, no data is removed
#' # Note: When clean = TRUE, cleanNA = TRUE, and flaggedonly = TRUE, an error message is returned
#'
TADA_FindQAPPApproval <- function(
  .data,
  clean = FALSE,
  cleanNA = FALSE,
  flaggedonly = FALSE
) {
  # check .data is data.frame and has required columns
  TADA_CheckColumns(.data, "QAPPApprovedIndicator")
  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # check cleanNA is boolean
  TADA_CheckType(cleanNA, "logical")
  # check flaggedonly is boolean
  TADA_CheckType(flaggedonly, "logical")
  # check that clean, cleanNA and flaggedonly are not all TRUE
  if (clean == TRUE & cleanNA == TRUE & flaggedonly == TRUE) {
    stop(
      "Function not executed because clean, cleanNA, and flaggedonly cannot all be TRUE"
    )
  }

  # execute function after checks are passed

  # if flaggedonly = FALSE
  if (flaggedonly == FALSE) {
    if (clean == TRUE) {
      .data <- dplyr::filter(
        .data,
        is.na(QAPPApprovedIndicator) == TRUE | QAPPApprovedIndicator == "Y"
      )

      if (nrow(.data) == 0) {
        message("All QAPPApprovedIndicator data is N")
      }
    }
    if (cleanNA == TRUE) {
      .data <- dplyr::filter(.data, is.na(QAPPApprovedIndicator) == FALSE)

      if (nrow(.data) == 0 & clean == TRUE) {
        message("All QAPPApprovedIndicator data is NA or N")
      } else if (nrow(.data) == 0 & clean == FALSE) {
        message("All QAPPApprovedIndicator data is NA")
      }
    }
    if (clean == FALSE & cleanNA == FALSE) {
      message(
        "Data is flagged but not removed because clean and cleanNA were FALSE"
      )
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
    NAorN.data <- dplyr::filter(
      .data,
      is.na(QAPPApprovedIndicator) == TRUE | QAPPApprovedIndicator == "N"
    )
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
#' utils::data(Data_R5_TADAPackageDemo)
#'
#' # Flag, but do not remove, data without an associated QAPP document in
#' # new column titled "TADA.QAPPDocAvailable":
#' FlagData_MissingQAPPDocURLs <- TADA_FindQAPPDoc(Data_R5_TADAPackageDemo)
#'
#' # Remove data without an associated QAPP document available:
#' RemoveData_MissingQAPPDocURLs <- TADA_FindQAPPDoc(Data_R5_TADAPackageDemo, clean = TRUE)
#'
TADA_FindQAPPDoc <- function(.data, clean = FALSE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # generate required column if it does not exist (there is no project data)
  if ("ProjectFileUrl" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create empty ProjectFileUrl column
    .data[, "ProjectFileUrl"] <- NA
  }
  # check .data has required columns
  TADA_CheckColumns(.data, "ProjectFileUrl")

  # check clean is boolean
  TADA_CheckType(clean, "logical")
  # default flag column
  .data$TADA.QAPPDocAvailable <- "N"

  # execute function after checks are passed
  # flag data where QAPP document url is provided
  # make QAPPdoc.data dataframe
  QAPPdoc.data <- dplyr::filter(.data, grepl("/", ProjectFileUrl))
  NQAPPdoc.data <- subset(
    .data,
    !.data$ResultIdentifier %in% QAPPdoc.data$ResultIdentifier
  )

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
      message(
        "No QAPP document url data found in your dataframe. Returning input dataframe with TADA.QAPPDocAvailable column for tracking."
      )
      .data <- TADA_OrderCols(.data)
      return(.data)
    }
    if (clean == TRUE) {
      message(
        "This dataframe is empty because we did not find any QAPP document url data in your dataframe"
      )
      QAPPdoc.data <- TADA_OrderCols(QAPPdoc.data)
      return(QAPPdoc.data)
    }
  }
}


#' Suspect coordinates
#'
#' This function identifies and flags Suspect coordinate data. When
#' clean_outsideUSA = "no" and clean_imprecise = FALSE,
#' a column will be appended titled "TADA.SuspectCoordinates.Flag" with the following
#' flags: 1) If the latitude is less than zero, the row will be
#' flagged with "LAT_OutsideUSA" (with the exception of American Samoa,
#' Northern Mariana Islands, and Guam), 2) If the longitude is greater than zero AND less than 145,
#' the row will be flagged as "LONG_OutsideUSA" (with the exception of
#' American Samoa, Northern Mariana Islands, and Guam), and 3) Finally,
#' precision can be measured by the number of decimal places in the latitude and longitude
#' provided. If either the latitude or longitude does not have at least three numbers to the
#' right of the decimal point, the row will be flagged as "Imprecise_lessthan3decimaldigits". Occasionally
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
#' @param check_location_metadata Boolean argument; Flags coordinates if location metadata
#' does not match with coordinate location; default is check_location_metadata = FALSE.
#'
#' @return Returns input TADA dataset with the added "TADA.SuspectCoordinates.Flag" column.
#' When clean_outsideUSA is "no", "change sign", or clean_imprecise argument is FALSE,
#' a column flagging rows with the respective QA check is appended to the input
#' dataframe. When clean_outsideUSA is "remove" or clean_imprecise is TRUE,
#' "Suspect" or "imprecise" data is removed, respectively. When flaggedonly is TRUE,
#' the dataframe will be filtered to show only the data flagged as Suspect, imprecise,
#' or out of the United States. Defaults are clean_outsideUSA = "no",
#' clean_imprecise = FALSE, and flaggedonly = FALSE.
#'
#' @export
#'
#' @examples
#' # Create a small mock dataset with minimal required columns.
#' # Rows cover: Pass, LAT_OutsideUSA, LONG_OutsideUSA, American Samoa,
#' # Northern Mariana Islands, Guam, and an imprecise coordinate.
#' mock_coords <- data.frame(
#'   ID = c(
#'     "Pass_US_mainland",
#'     "Lat_outside",
#'     "Long_outside",
#'     "American_Samoa",
#'     "Northern_Mariana_Islands",
#'     "Guam",
#'     "Imprecise"
#'   ),
#'   TADA.LatitudeMeasure = c(
#'     38.8977, # Pass (USA mainland-like)
#'     -5.0000, # LAT_OutsideUSA
#'     40.0000, # LONG_OutsideUSA (long between 0 and 145)
#'     -13.5000, # American Samoa (excluded from outside flags)
#'     15.0000, # Northern Mariana Islands (excluded)
#'     13.4000, # Guam (excluded)
#'     35.12 # Imprecise (< 3 decimal places)
#'   ),
#'   TADA.LongitudeMeasure = c(
#'     -77.0365, # Pass
#'     -120.0000, # LAT_OutsideUSA
#'     10.0000, # LONG_OutsideUSA
#'     -170.0000, # American Samoa
#'     145.5000, # Northern Mariana Islands
#'     144.8500, # Guam
#'     -120.0 # Imprecise (<= 1 decimal place)
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Flag, but do not remove, data with Suspect coordinates in new column
#' # titled "TADA.SuspectCoordinates.Flag":
#' # Return ALL data:
#' SuspectCoord_flags <- TADA_FlagCoordinates(mock_coords)
#'
#' # Flag, but do not remove, data with Suspect coordinates in new column
#' # titled "TADA.SuspectCoordinates.Flag"
#' # Return ONLY the flagged data:
#' SuspectCoord_flags_flaggedonly <- TADA_FlagCoordinates(
#'   mock_coords,
#'   flaggedonly = TRUE
#' )
#'
#' # Remove data with coordinates outside the USA, but keep flagged data with
#' # imprecise coordinates:
#' OutsideUSACoord_removed <- TADA_FlagCoordinates(
#'   mock_coords,
#'   clean_outsideUSA = "remove"
#' )
#'
#' # Change the sign of coordinates flagged as outside the USA and keep all
#' # flagged data:
#' OutsideUSACoord_changed <- TADA_FlagCoordinates(
#'   mock_coords,
#'   clean_outsideUSA = "change sign"
#' )
#'
#' # Remove data with imprecise coordinates, but keep flagged data with
#' # coordinates outside the USA;
#' # imprecise data may have less than 3 significant figures to the right
#' # of the decimal point:
#' ImpreciseCoord_removed <- TADA_FlagCoordinates(
#'   mock_coords,
#'   clean_imprecise = TRUE
#' )
#'
#' # Remove data with imprecise coordinates or coordinates outside the USA
#' # from the dataframe:
#' SuspectCoord_removed <- TADA_FlagCoordinates(
#'   mock_coords,
#'   clean_outsideUSA = "remove",
#'   clean_imprecise = TRUE
#' )
#'
#' # Flag data with coordinates not matching metadata
#' # from the dataframe:
#' SuspectCoord_removed <- TADA_FlagCoordinates(
#'   mock_coords,
#'   check_location_metadata = TRUE
#' )
#'
TADA_FlagCoordinates <- function(
  .data,
  clean_outsideUSA = c("no", "remove", "change sign"),
  clean_imprecise = FALSE,
  flaggedonly = FALSE,
  check_location_metadata = FALSE
) {
  # check .data is data.frame and has required columns
  TADA_CheckColumns(.data, c("TADA.LatitudeMeasure", "TADA.LongitudeMeasure"))
  # check clean_outsideUSA is character
  TADA_CheckType(clean_outsideUSA, "character")
  # check clean_imprecise is boolean
  TADA_CheckType(clean_imprecise, "logical")
  # check check_location_metadata is boolean
  TADA_CheckType(check_location_metadata, "logical")
  # check lat and long are "numeric"
  if (!is.numeric(.data$TADA.LongitudeMeasure)) {
    warning("TADA.LongitudeMeasure field must be numeric")
  }

  if (!is.numeric(.data$TADA.LatitudeMeasure)) {
    warning("TADA.LatitudeMeasure field must be numeric")
  }

  # check for required columns
  if (check_location_metadata == TRUE) {
    TADA_CheckColumns(
      .data,
      c(
        "TADA.LatitudeMeasure",
        "TADA.LongitudeMeasure",
        "StateCode",
        "CountyCode"
      )
    )
  }

  # check that clean_outsideUSA is either "no", "remove", or "change sign"
  clean_outsideUSA <- match.arg(clean_outsideUSA)

  orig_dim <- dim(.data)[1]

  # execute function after checks are passed
  .data <- .data |>
    dplyr::mutate(
      TADA.SuspectCoordinates.Flag = dplyr::case_when(
        TADA.LatitudeMeasure < -11.046934 &
          TADA.LatitudeMeasure > -14.548699 &
          TADA.LongitudeMeasure < -168.1433 &
          TADA.LongitudeMeasure > -171.089874 ~ NA_character_, # American Samoa
        TADA.LatitudeMeasure < 20.553802 &
          TADA.LatitudeMeasure > 14.110472 &
          TADA.LongitudeMeasure < 146.064818 &
          TADA.LongitudeMeasure > 144.886331 ~ NA_character_, # Northern Mariana Islands
        TADA.LatitudeMeasure < 13.654383 &
          TADA.LatitudeMeasure > 13.234189 &
          TADA.LongitudeMeasure < 144.956712 &
          TADA.LongitudeMeasure > 144.618068 ~ NA_character_, # Guam
        TADA.LatitudeMeasure < 0 ~ "LAT_OutsideUSA",
        TADA.LongitudeMeasure > 0 &
          TADA.LongitudeMeasure < 145 ~ "LONG_OutsideUSA",
        # for below, lat and long fields must be numeric
        # this checks if there are at least 3 significant figures to the
        # right of the decimal point
        sapply(.data$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 3 |
          sapply(.data$TADA.LongitudeMeasure, TADA_DecimalPlaces) <
            3 ~ "Imprecise_lessthan3decimaldigits"
      )
    )

  # Fill in flag for coordinates that appear OK/PASS tests
  .data$TADA.SuspectCoordinates.Flag[is.na(
    .data$TADA.SuspectCoordinates.Flag
  )] <- "Pass"

  # Optional StateCode / CountyCode coordinate consistency check
  if (check_location_metadata == TRUE) {
    counties <- tigris::counties(cb = TRUE, year = 2023, class = "sf") |>
      dplyr::mutate(
        CoordinateStateCode = STATEFP,
        CoordinateCountyCode = COUNTYFP
      ) |>
      dplyr::select(CoordinateStateCode, CoordinateCountyCode, geometry) |>
      sf::st_transform(4326)

    pts <- .data |>
      dplyr::mutate(.row_id = dplyr::row_number()) |>
      sf::st_as_sf(
        coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
        crs = 4326,
        remove = FALSE
      )

    coord_metadata <- sf::st_join(pts, counties, join = sf::st_within) |>
      sf::st_drop_geometry() |>
      dplyr::select(.row_id, CoordinateStateCode, CoordinateCountyCode)

    .data <- .data |>
      dplyr::mutate(
        .row_id = dplyr::row_number(),
        StateCode = stringr::str_pad(as.character(StateCode), 2, pad = "0"),
        CountyCode = stringr::str_pad(as.character(CountyCode), 3, pad = "0")
      ) |>
      dplyr::left_join(coord_metadata, by = ".row_id") |>
      dplyr::mutate(
        TADA.SuspectCoordinates.Flag = dplyr::case_when(
          !is.na(StateCode) &
            !is.na(CoordinateStateCode) &
            StateCode != CoordinateStateCode ~ dplyr::if_else(
            TADA.SuspectCoordinates.Flag == "Pass",
            "Coordinate_StateMismatch",
            paste(
              TADA.SuspectCoordinates.Flag,
              "Coordinate_StateMismatch",
              sep = "; "
            )
          ),

          !is.na(CountyCode) &
            !is.na(CoordinateCountyCode) &
            CountyCode != CoordinateCountyCode ~ dplyr::if_else(
            TADA.SuspectCoordinates.Flag == "Pass",
            "Coordinate_CountyMismatch",
            paste(
              TADA.SuspectCoordinates.Flag,
              "Coordinate_CountyMismatch",
              sep = "; "
            )
          ),

          TRUE ~ TADA.SuspectCoordinates.Flag
        )
      ) |>
      dplyr::select(-.row_id, -CoordinateStateCode, -CoordinateCountyCode)
  }

  # if clean_imprecise is TRUE, remove imprecise station metadata
  if (clean_imprecise == TRUE) {
    .data <- dplyr::filter(
      .data,
      !TADA.SuspectCoordinates.Flag %in% "Imprecise_lessthan3decimaldigits"
    )
  }

  # if clean_outsideUSA is "remove", remove stations flagged as outside the USA
  if (clean_outsideUSA == "remove") {
    .data <- dplyr::filter(
      .data,
      !TADA.SuspectCoordinates.Flag %in% c("LAT_OutsideUSA", "LONG_OutsideUSA")
    )
  }

  # if clean_outsideUSA is "change sign", change the sign of lat/long coordinates outside of USA
  if (clean_outsideUSA == "change sign") {
    message(
      "When clean_outsideUSA == change sign, the sign for any lat/long coordinates flagged as outside of USA are switched. This is a temporary solution. Data owners should fix the raw data to address Suspect coordinates through WQX. For assistance fixing data errors you see in the WQP, email the WQX helpdesk (WQX@epa.gov)."
    )
    .data <- .data |>
      dplyr::mutate(
        TADA.LatitudeMeasure = dplyr::case_when(
          TADA.SuspectCoordinates.Flag ==
            "LAT_OutsideUSA" ~ TADA.LatitudeMeasure * (-1),
          TRUE ~ TADA.LatitudeMeasure
        ),
        TADA.LongitudeMeasure = dplyr::case_when(
          TADA.SuspectCoordinates.Flag ==
            "LONG_OutsideUSA" ~ TADA.LongitudeMeasure * (-1),
          TRUE ~ TADA.LongitudeMeasure
        )
      )
  }

  # return only flagged data if flaggedonly = true
  if ((flaggedonly == TRUE)) {
    .data <- dplyr::filter(.data, TADA.SuspectCoordinates.Flag != "Pass")
  }

  if (all(.data$TADA.SuspectCoordinates.Flag %in% c("OK")) == TRUE) {
    if (orig_dim == dim(.data)[1]) {
      message(
        "Your dataframe does not contain monitoring stations with Suspect coordinates. Returning input dataframe with TADA.SuspectCoordinates.Flag column for tracking."
      )
    } else {
      message(
        "All Suspect coordinates were removed. Returning input dataframe with TADA.SuspectCoordinates.Flag column for tracking."
      )
    }
  }
  .data <- TADA_OrderCols(.data)
  return(.data)
}

#' Identify Potentially Duplicated Data by Multiple Organizations
#'
#' Identifies records that may be duplicated across different organizations.
#' Records are flagged as potential duplicates when they share the same date,
#' time, characteristic name, activity type, and exact result value across
#' organizations, and the associated monitoring locations are within a specified
#' distance.
#'
#' This function calls `TADA_FindNearbySites()` internally to identify nearby
#' monitoring locations. Records are then compared across organizations within
#' each nearby-site group. Because this is a heuristic, users should review
#' flagged records to determine whether they are true duplicates or legitimate
#' separate observations.
#'
#' It is recommended to run `TADA_FindPotentialDuplicatesSingleOrg()` first to
#' address within-organization duplicates before checking for duplicates across
#' organizations.
#'
#' @param .data A TADA dataframe. If needed, this function may internally
#'   convert the input to an sf object for spatial processing.
#' @param dist_buffer Numeric. Distance in meters used to define nearby sites.
#'   Records from different organizations are considered for duplicate checking
#'   only when their monitoring locations are within this distance. Default is
#'   100.
#' @param org_hierarchy A character vector of organization identifiers used to
#'   prioritize which record is selected within each duplicate group. If set to
#'   "none" or not provided, one record is selected at random from each group.
#'   Otherwise, the first organization in the vector that appears in a group is
#'   selected as the representative row.
#' @param clean Logical. If TRUE, rows flagged as "Duplicate Not Selected"
#'   are removed before returning the result. Default is FALSE.
#'
#' @return The input dataframe with two additional columns:
#' TADA.MultipleOrgDup.Flag and TADA.MultipleOrgDupGroupID.
#'
#' TADA.MultipleOrgDup.Flag indicates duplicate status for each row and will be
#' one of:
#' - "Not a Duplicate"
#' - "Duplicate Selected"
#' - "Duplicate Not Selected"
#'
#' TADA.MultipleOrgDupGroupID identifies potential multi-organization duplicate
#' groups. Rows not assigned to a duplicate group are labeled
#' "Not a Duplicate". Rows with the same group ID belong to the same potential
#' duplicate set.
#'
#' When clean = TRUE, rows flagged as "Duplicate Not Selected" are removed,
#' while rows flagged as "Duplicate Selected" and "Not a Duplicate" are retained.
#' If the duplicate flag columns already exist and clean = TRUE, the function
#' assumes those columns were created by this function and are current; in that
#' case, it skips recomputation and filters the existing flags.
#'
#' @examples
#' \dontrun{
#' # Load example dataset with known multiple-organization duplicates
#' dat <- TADA_DataRetrieval(
#'   startDate = "2023-04-01",
#'   endDate = "2023-04-30",
#'   statecode = "PA",
#'   organization = c("21PA_WQX", "USGS-PA"),
#'   ask = FALSE
#' )
#'
#' # Review unique organizations and consider prioritizing for
#' # duplicate selection (using optional org_hierarchy input)
#' unique(dat$OrganizationIdentifier)
#'
#' # Identify potential duplicates and prioritize "21PA_WQX" over "USGS-PA"
#' dat1 <- TADA_FindPotentialDuplicatesMultipleOrgs(
#'   dat,
#'   dist_buffer = 100,
#'   org_hierarchy = c("21PA_WQX", "USGS-PA")
#' )
#' table(dat1$TADA.MultipleOrgDup.Flag)
#'
#' # Review duplicate groupings
#' dat_review <- dat1 |>
#'   dplyr::select(
#'     OrganizationIdentifier,
#'     MonitoringLocationIdentifier,
#'     ActivityTypeCode,
#'     ActivityStartDate,
#'     ActivityStartTime.Time,
#'     TADA.ComparableDataIdentifier,
#'     SubjectTaxonomicName,
#'     TADA.ResultMeasureValue,
#'     TADA.ResultDepthHeightMeasure.MeasureValue,
#'     TADA.ResultDepthHeightMeasure.MeasureUnitCode,
#'     TADA.MultipleOrgDupGroupID,
#'     TADA.MultipleOrgDup.Flag
#'   ) |>
#'   dplyr::arrange(TADA.MultipleOrgDupGroupID)
#'
#' # Re-run and keep only non-duplicate / representative rows
#' # Relies on existing duplicate flag columns to avoid re-computation
#' dat_clean <- TADA_FindPotentialDuplicatesMultipleOrgs(dat1, clean = TRUE)
#' }
#'
#' @export
TADA_FindPotentialDuplicatesMultipleOrgs <- function(
  .data,
  dist_buffer = 100,
  org_hierarchy = "none",
  clean = FALSE
) {
  if (nrow(.data) == 0) {
    message("The input dataframe is empty. Returning the dataframe unchanged.")
    return(.data)
  }

  TADA_CheckType(clean, "logical")

  already_processed <- all(
    c("TADA.MultipleOrgDup.Flag", "TADA.MultipleOrgDupGroupID") %in%
      names(.data)
  )

  if (clean == TRUE && already_processed) {
    .data <- .data |>
      dplyr::filter(TADA.MultipleOrgDup.Flag != "Duplicate Not Selected") |>
      dplyr::mutate(
        TADA.MultipleOrgDupGroupID = ifelse(
          is.na(TADA.MultipleOrgDupGroupID),
          "Not a Duplicate",
          as.character(TADA.MultipleOrgDupGroupID)
        ),
        TADA.MultipleOrgDup.Flag = ifelse(
          is.na(TADA.MultipleOrgDup.Flag),
          "Not a Duplicate",
          TADA.MultipleOrgDup.Flag
        )
      )

    .data <- TADA_OrderCols(.data)
    message(
      "Existing duplicate-flag columns detected; clean = TRUE, so only filtering duplicate rows and returning the result."
    )
    return(.data)
  }

  if (!"TADA.NearbySites.Flag" %in% names(.data)) {
    .data <- TADA_FindNearbySites(
      .data,
      dist_buffer = dist_buffer,
      org_hierarchy = org_hierarchy
    )
  }

  required_nearby_cols <- c(
    "TADA.NearbySiteGroup",
    "TADA.MonitoringLocationIdentifier"
  )
  missing_nearby_cols <- required_nearby_cols[
    !required_nearby_cols %in% names(.data)
  ]
  if (length(missing_nearby_cols) > 0) {
    stop(
      "TADA_FindNearbySites() did not return required columns: ",
      paste(missing_nearby_cols, collapse = ", ")
    )
  }

  dupsites <- unique(.data[, c(
    "MonitoringLocationIdentifier",
    "TADA.NearbySiteGroup"
  )])

  dupsites <- dupsites |> dplyr::filter(!is.na(TADA.NearbySiteGroup))

  dupsprep <- .data |>
    dplyr::filter(
      MonitoringLocationIdentifier %in% dupsites$MonitoringLocationIdentifier
    ) |>
    dplyr::select(
      OrganizationIdentifier,
      ResultIdentifier,
      ActivityStartDate,
      ActivityStartTime.Time,
      TADA.ComparableDataIdentifier,
      ActivityTypeCode,
      TADA.ResultMeasureValue,
      TADA.MonitoringLocationIdentifier,
      TADA.NearbySiteGroup
    ) |>
    dplyr::filter(!is.na(TADA.ResultMeasureValue))

  rm(dupsites)

  dups_sum <- dupsprep |>
    dplyr::group_by(
      ActivityStartDate,
      ActivityStartTime.Time,
      TADA.ComparableDataIdentifier,
      ActivityTypeCode,
      TADA.ResultMeasureValue,
      TADA.MonitoringLocationIdentifier,
      TADA.NearbySiteGroup
    ) |>
    dplyr::mutate(numorgs = dplyr::n_distinct(OrganizationIdentifier)) |>
    dplyr::filter(numorgs > 1) |>
    dplyr::mutate(TADA.MultipleOrgDupGroupID = dplyr::cur_group_id()) |>
    dplyr::select(-numorgs) |>
    dplyr::ungroup()

  dupsdat <- dplyr::left_join(
    .data,
    dups_sum |>
      dplyr::select(
        ActivityStartDate,
        ActivityStartTime.Time,
        TADA.ComparableDataIdentifier,
        ActivityTypeCode,
        TADA.ResultMeasureValue,
        OrganizationIdentifier,
        ResultIdentifier,
        TADA.MonitoringLocationIdentifier,
        TADA.NearbySiteGroup,
        TADA.MultipleOrgDupGroupID
      ),
    by = c(
      "ActivityStartDate",
      "ActivityStartTime.Time",
      "TADA.ComparableDataIdentifier",
      "ActivityTypeCode",
      "TADA.ResultMeasureValue",
      "OrganizationIdentifier",
      "ResultIdentifier",
      "TADA.MonitoringLocationIdentifier",
      "TADA.NearbySiteGroup"
    )
  ) |>
    dplyr::mutate(
      TADA.MultipleOrgDupGroupID = ifelse(
        is.na(TADA.MultipleOrgDupGroupID),
        "Not a Duplicate",
        as.character(TADA.MultipleOrgDupGroupID)
      ),
      TADA.MultipleOrgDup.Flag = "Not a Duplicate"
    )

  dup_groups <- unique(dupsdat$TADA.MultipleOrgDupGroupID)
  dup_groups <- dup_groups[dup_groups != "Not a Duplicate"]

  if (length(dup_groups) > 0) {
    if (identical(org_hierarchy, "none")) {
      selected_rows <- dupsdat |>
        dplyr::filter(TADA.MultipleOrgDupGroupID != "Not a Duplicate") |>
        dplyr::group_by(TADA.MultipleOrgDupGroupID) |>
        dplyr::slice_sample(n = 1) |>
        dplyr::ungroup() |>
        dplyr::select(ResultIdentifier)

      dupsdat <- dupsdat |>
        dplyr::mutate(
          TADA.MultipleOrgDup.Flag = dplyr::case_when(
            TADA.MultipleOrgDupGroupID == "Not a Duplicate" ~ "Not a Duplicate",
            ResultIdentifier %in%
              selected_rows$ResultIdentifier ~ "Duplicate Selected",
            TRUE ~ "Duplicate Not Selected"
          )
        )
    } else {
      dupsdat <- dupsdat |>
        dplyr::group_by(TADA.MultipleOrgDupGroupID) |>
        dplyr::mutate(
          .rank = match(OrganizationIdentifier, org_hierarchy),
          .rank = ifelse(is.na(.rank), 9999, .rank),
          .selected = .rank == min(.rank, na.rm = TRUE),
          TADA.MultipleOrgDup.Flag = dplyr::case_when(
            TADA.MultipleOrgDupGroupID == "Not a Duplicate" ~ "Not a Duplicate",
            .selected ~ "Duplicate Selected",
            TRUE ~ "Duplicate Not Selected"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-.rank, -.selected)
    }

    message(paste0(
      sum(
        dupsdat$TADA.MultipleOrgDup.Flag == "Duplicate Not Selected",
        na.rm = TRUE
      ),
      " potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.MultipleOrgDupGroupID column. ",
      "Within each duplicate group, one row is marked 'Duplicate Selected' and the other rows are marked 'Duplicate Not Selected'."
    ))
  } else {
    message(
      "No duplicate results detected. Returning input dataframe with duplicate flagging columns set to 'Not a Duplicate'."
    )
  }

  if (clean == TRUE) {
    dupsdat <- dplyr::filter(
      dupsdat,
      TADA.MultipleOrgDup.Flag != "Duplicate Not Selected"
    )
  }

  dupsdat <- TADA_OrderCols(dupsdat)
  return(dupsdat)
}

#' Identify Potentially Duplicated Data Uploads by a Single Organization
#'
#' Identifies records that may be duplicated within the same organization.
#' Records are flagged when they share the same organization, monitoring
#' location, date, time, activity type, characteristic name, taxonomic name,
#' sample fraction, depth/height measure columns, and exact result value.
#'
#' The duplicate detection is performed independently within each organization.
#' This is a heuristic intended to identify likely duplicate uploads, not to
#' determine with certainty whether records are truly duplicated. Users should
#' review flagged records before removing them.
#'
#' If the input data already contains the columns
#' `TADA.SingleOrgDupGroupID` and `TADA.SingleOrgDup.Flag`, then `clean = TRUE`
#' will reuse those columns and simply remove rows flagged as
#' `"Duplicate Not Selected"` without recomputing duplicate groups. If
#' `clean = FALSE`, duplicate flags are recomputed and any existing values in
#' those columns are overwritten.
#'
#' @param .data A TADA dataframe.
#' @param clean Logical. If TRUE, rows flagged as `"Duplicate Not Selected"`
#'   are removed before returning the result. If the duplicate flag columns
#'   already exist, they are reused when `clean = TRUE`; otherwise they are
#'   computed first. If `clean = FALSE`, duplicate flags are always recomputed.
#'   Default is FALSE.
#'
#' @return The input TADA dataframe with these additional columns:
#' `TADA.SingleOrgDupGroupID` and `TADA.SingleOrgDup.Flag`.
#'
#' `TADA.SingleOrgDupGroupID`: Identifier for a group of potentially duplicated
#' results within a single organization. Rows not in a duplicate group are
#' labeled `"Not a Duplicate"`.
#'
#' `TADA.SingleOrgDup.Flag`: Duplicate status for each row.
#' For each duplicate group, one row is randomly selected to represent the
#' group and is marked `"Duplicate Selected"`. The remaining rows in that group
#' are marked `"Duplicate Not Selected"`. Rows outside duplicate groups are
#' marked `"Not a Duplicate"`. When `clean = TRUE`, rows flagged as
#' `"Duplicate Not Selected"` are removed and only representative rows plus
#' non-duplicate rows are returned.
#'
#' @examples
#' # Load example dataset
#' utils::data(Data_TribalNations)
#'
#' # Identify duplicates and flag them for review
#' Data_TribalNations_dups <- TADA_FindPotentialDuplicatesSingleOrg(Data_TribalNations)
#' table(Data_TribalNations_dups$TADA.SingleOrgDup.Flag)
#'
#' # Review duplicate rows
#' Data_TribalNations_review <- Data_TribalNations_dups |>
#'   dplyr::select(
#'     OrganizationIdentifier,
#'     MonitoringLocationIdentifier,
#'     ActivityTypeCode,
#'     ActivityStartDate,
#'     ActivityStartTime.Time,
#'     TADA.ComparableDataIdentifier,
#'     SubjectTaxonomicName,
#'     TADA.ResultMeasureValue,
#'     TADA.ResultDepthHeightMeasure.MeasureValue,
#'     TADA.ResultDepthHeightMeasure.MeasureUnitCode,
#'     TADA.SingleOrgDupGroupID,
#'     TADA.SingleOrgDup.Flag
#'   ) |>
#'   dplyr::arrange(TADA.SingleOrgDupGroupID)
#'
#' # Keep only representative rows
#' Data_TribalNations_clean <- TADA_FindPotentialDuplicatesSingleOrg(
#'   Data_TribalNations,
#'   clean = TRUE
#' )
#'
#' # Clean an already-flagged dataset without recomputing flags
#' Data_TribalNations_clean2 <- TADA_FindPotentialDuplicatesSingleOrg(
#'   Data_TribalNations_dups,
#'   clean = TRUE
#' )
#'
#' @export
TADA_FindPotentialDuplicatesSingleOrg <- function(.data, clean = FALSE) {
  expected_cols <- c(
    "OrganizationIdentifier",
    "MonitoringLocationIdentifier",
    "ActivityStartDate",
    "ActivityStartTime.Time",
    "ActivityTypeCode",
    "TADA.CharacteristicName",
    "SubjectTaxonomicName",
    "TADA.ResultSampleFractionText",
    "TADA.ResultMeasureValue",
    "ResultIdentifier"
  )

  TADA_CheckColumns(.data, expected_cols)
  TADA_CheckType(clean, "logical")

  dup_cols <- c("TADA.SingleOrgDupGroupID", "TADA.SingleOrgDup.Flag")
  has_dup_cols <- all(dup_cols %in% names(.data))

  # Recompute duplicate flags unless clean = TRUE and the duplicate
  # columns already exist. In that case, preserve existing flags and
  # only apply the clean filter below.
  if (!(clean && has_dup_cols)) {
    depthcols <- names(.data)[grepl(
      "^TADA.*DepthHeightMeasure.MeasureValue$",
      names(.data)
    )]

    cols <- c(
      "OrganizationIdentifier",
      "MonitoringLocationIdentifier",
      "ActivityStartDate",
      "ActivityStartTime.Time",
      "ActivityTypeCode",
      "TADA.CharacteristicName",
      "SubjectTaxonomicName",
      "TADA.ResultSampleFractionText",
      "TADA.ResultMeasureValue",
      depthcols
    )

    dups_sum_org <- .data |>
      dplyr::group_by(dplyr::across(tidyselect::any_of(cols))) |>
      dplyr::summarise(
        numres = dplyr::n_distinct(ResultIdentifier),
        .groups = "drop"
      ) |>
      dplyr::filter(numres > 1) |>
      dplyr::mutate(TADA.SingleOrgDupGroupID = dplyr::row_number())

    .data <- dplyr::left_join(
      .data,
      dups_sum_org |> dplyr::select(-numres),
      by = cols
    )

    .data <- .data |>
      dplyr::mutate(
        TADA.SingleOrgDupGroupID = ifelse(
          is.na(TADA.SingleOrgDupGroupID),
          "Not a Duplicate",
          as.character(TADA.SingleOrgDupGroupID)
        ),
        TADA.SingleOrgDup.Flag = "Not a Duplicate"
      )

    if (nrow(dups_sum_org) > 0) {
      picks <- .data |>
        dplyr::filter(TADA.SingleOrgDupGroupID != "Not a Duplicate") |>
        dplyr::group_by(TADA.SingleOrgDupGroupID) |>
        dplyr::slice_sample(n = 1) |>
        dplyr::ungroup()

      .data <- .data |>
        dplyr::mutate(
          TADA.SingleOrgDup.Flag = ifelse(
            ResultIdentifier %in% picks$ResultIdentifier,
            "Duplicate Selected",
            ifelse(
              TADA.SingleOrgDupGroupID == "Not a Duplicate",
              "Not a Duplicate",
              "Duplicate Not Selected"
            )
          )
        )

      message(paste0(
        "TADA_FindPotentialDuplicatesSingleOrg: ",
        nrow(dups_sum_org),
        " groups of potentially duplicated results found in dataset. ",
        "These have been placed into duplicate groups in the TADA.SingleOrgDupGroupID column ",
        "and one result from each group was randomly selected to represent a single, ",
        "unduplicated value. Selected values are indicated in the TADA.SingleOrgDup.Flag ",
        "as 'Duplicate Selected', while duplicates are flagged as 'Duplicate Not Selected' ",
        "for easy filtering."
      ))
    } else {
      message(
        "No duplicate results detected. Returning input dataframe with TADA.SingleOrgDup.Flag set to 'Not a Duplicate'."
      )
    }
  }

  if (clean == TRUE) {
    if (!"TADA.SingleOrgDup.Flag" %in% names(.data)) {
      stop("TADA.SingleOrgDup.Flag not found and could not be created.")
    }
    .data <- dplyr::filter(
      .data,
      TADA.SingleOrgDup.Flag != "Duplicate Not Selected"
    )
  }

  .data <- TADA_OrderCols(.data)
  return(.data)
}
