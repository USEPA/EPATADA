#' Harmonize Synonyms
#'
#' This function joins a synonym reference table to the dataset to convert
#' synonymous data to a unified naming format for easier aggregation, analysis,
#' and visualization. Users may populate the function with a dataset-specific
#' synonym table created from TADA_GetSynonymRef and reviewed/customized by the
#' user (recommended), or the default TADA-provided synonym table, containing
#' suggested synonym naming for some priority characteristics. Where a suggested
#' characteristic name, fraction, or speciation is present, the function
#' will convert the TADA.CharacteristicName, TADA.ResultSampleFractionText,
#' and TADA.MethodSpeciationName to the target format. In cases where a target
#' speciation differs from the existing speciation, the reference table will
#' also apply multiplication conversion factors to the TADA.ResultMeasureValue.
#'
#' @param .data TADA dataframe
#' @param ref Optional argument to specify which dataframe to use as a reference
#' file. The primary use for this argument is when a user has generated a
#' synonym reference file unique to their data, and they made changes to
#' that file.
#' @param np_speciation Boolean. Determines whether the user wants to convert
#'   nitrogen and phosphorus subspecies to speciation 'as N' and 'as P', where
#'   speciation conversions are provided. Defaults to TRUE. For example, if
#'   np_speciation is TRUE, all Nitrate with TADA.MethodSpeciationName = as
#'   NO3 will be converted to as N using molecular weight conversion factors.
#'
#' @return The input TADA dataframe with the TADA.CharacteristicName,
#'   TADA.ResultSampleFractionText, and TADA.MethodSpeciationName columns
#'   converted to the target values,
#'   if supplied. Also includes additional columns
#'   TADA.CharacteristicNameAssumptions, TADA.FractionAssumptions, and
#'   TADA.SpeciationAssumptions populated with additional notes about the conversion
#'   logic, and a TADA.Harmonized.Flag, indicating whether TADA columns were
#'   changed in this function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example dataset:
#' utils::data(Data_6Tribes_5y)
#'
#' # Create a synonym reference table for flagged, cleaned dataframe:
#' Data_6Tribes_5yClean <- subset(
#'   Data_6Tribes_5y,
#'   !is.na(Data_6Tribes_5y$TADA.ResultMeasureValue)
#' )
#' Data_6Tribes_5yClean <- TADA_FlagFraction(Data_6Tribes_5yClean,
#'   clean = TRUE
#' )
#' Data_6Tribes_5yClean <- TADA_FlagResultUnit(Data_6Tribes_5yClean,
#'   clean = "suspect_only"
#' )
#' Data_6Tribes_5yClean <- TADA_FlagSpeciation(Data_6Tribes_5yClean,
#'   clean = "suspect_only"
#' )
#' Data_6Tribes_5yClean <- TADA_FlagMethod(Data_6Tribes_5yClean, clean = TRUE)
#' CreateRefTable <- TADA_GetSynonymRef(Data_6Tribes_5yClean)
#'
#' # Append synonym reference table columns to dataframe and transform/convert
#' # data to the USER SUPPLIED reference table values:
#' Data_6Tribes_5yClean_Harmonized <-
#'   TADA_HarmonizeSynonyms(Data_6Tribes_5yClean, ref = CreateRefTable)
#' }
#'
TADA_HarmonizeSynonyms <- function(.data, ref, np_speciation = TRUE) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  TADA_CheckColumns(.data, expected_cols)

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  # if class(ResultMeasureValue) != numeric, run special char function - EDH - should not be needed at this point but doesn't hurt.
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop(
      "TADA.ResultMeasureValue is not numeric. This column must be numeric before proceeding."
    )
  }

  # Changes NONE in fraction and speciation to NA for better harmonization
  # Should this be specified in the template instead? 7/25/25 cm
  .data <- .data |>
    dplyr::mutate(
      TADA.ResultSampleFractionText = replace(
        TADA.ResultSampleFractionText,
        TADA.ResultSampleFractionText %in% c("NONE"),
        NA
      ),
      TADA.MethodSpeciationName = replace(
        TADA.MethodSpeciationName,
        TADA.MethodSpeciationName %in% c("NONE"),
        NA
      )
    )

  # define harm.ref
  # if input for ref exists, use that data
  if (!missing(ref)) {
    # check ref has all of the required columns
    expected_ref_cols <- c(
      expected_cols,
      c(
        "Target.TADA.CharacteristicName",
        "Target.TADA.ResultSampleFractionText",
        "Target.TADA.MethodSpeciationName",
        "TADA.CharacteristicNameAssumptions",
        "TADA.FractionAssumptions",
        "TADA.SpeciationAssumptions",
        "Target.TADA.SpeciationConversionFactor",
        "HarmonizationGroup"
      )
    )
    TADA_CheckColumns(ref, expected_ref_cols)

    harm.ref <- ref
  }

  # if input for ref does not exist, use raw harmonization template
  if (missing(ref)) {
    # use output of TADA_GetSynonymRef which uses the TADA HarmonizationTemplate.csv in the extdata folder
    harm.ref <- TADA_GetSynonymRef(.data) |> dplyr::distinct()
  }

  # find places where metadata will be changed and add targets
  harm.ref$TADA.Harmonized.Flag <- ifelse(
    !is.na(harm.ref$Target.TADA.CharacteristicName) |
      !is.na(harm.ref$Target.TADA.ResultSampleFractionText) |
      !is.na(harm.ref$Target.TADA.MethodSpeciationName),
    TRUE,
    FALSE
  )

  .data <- .data[, !names(.data) %in% c("TADA.ComparableDataIdentifier")]

  # join harm.ref to .data
  flag.data <- .data |>
    dplyr::left_join(
      harm.ref,
      by = c(
        "TADA.CharacteristicName",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName"
      )
    )

  # TADA.CharacteristicName
  # replace TADA.CharacteristicName with Target.TADA.CharacteristicName
  clean.data <- flag.data |>
    # use TADA suggested name where there is a suggested name, use original name if no suggested name
    dplyr::mutate(
      TADA.CharacteristicName = dplyr::case_when(
        !is.na(Target.TADA.CharacteristicName) ~ Target.TADA.CharacteristicName,
        # is.na(Target.TADA.CharacteristicName) ~ TADA.CharacteristicName,
        .default = TADA.CharacteristicName
      )
    )

  # TADA.ResultSampleFractionText
  # replace ResultSampleFractionText with Target.TADA.ResultSampleFractionText
  clean.data <- clean.data |>
    # use TADA suggested frac where there is a suggested frac, use original frac if no suggested frac
    dplyr::mutate(
      TADA.ResultSampleFractionText = dplyr::case_when(
        !is.na(
          Target.TADA.ResultSampleFractionText
        ) ~ Target.TADA.ResultSampleFractionText,
        !is.na(TADA.ResultSampleFractionText) &
          is.na(Target.TADA.ResultSampleFractionText) &
          !is.na(
            TADA.FractionAssumptions
          ) ~ Target.TADA.ResultSampleFractionText,
        # is.na(Target.TADA.ResultSampleFractionText) ~ TADA.ResultSampleFractionText
        .default = TADA.ResultSampleFractionText
      )
    )

  # Handle instances with DO where the speciation is listed "AS O2" but it should be NA
  clean.data$TADA.MethodSpeciationName <- ifelse(
    !is.na(clean.data$TADA.MethodSpeciationName) &
      is.na(clean.data$Target.TADA.MethodSpeciationName) &
      !is.na(clean.data$TADA.SpeciationAssumptions),
    clean.data$Target.TADA.MethodSpeciationName,
    clean.data$TADA.MethodSpeciationName
  )

  # TADA.MethodSpeciationName
  # replace MethodSpeciationName with Target.TADA.MethodSpeciationName

  if (np_speciation == TRUE) {
    clean.data <- clean.data |>
      # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
      dplyr::mutate(
        TADA.MethodSpeciationName = dplyr::case_when(
          !is.na(
            Target.TADA.MethodSpeciationName
          ) ~ Target.TADA.MethodSpeciationName,
          # is.na(Target.TADA.MethodSpeciationName) ~ TADA.MethodSpeciationName
          .default = TADA.MethodSpeciationName
        )
      ) |>
      # if conversion factor exists, multiply by ResultMeasureValue
      dplyr::rowwise() |>
      dplyr::mutate(
        TADA.ResultMeasureValue = dplyr::case_when(
          !is.na(
            Target.TADA.SpeciationConversionFactor
          ) ~ (Target.TADA.SpeciationConversionFactor *
            TADA.ResultMeasureValue),
          # is.na(Target.TADA.SpeciationConversionFactor) ~ TADA.ResultMeasureValue
          .default = TADA.ResultMeasureValue
        )
      )
  } else {
    clean.data <- clean.data |>
      # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
      dplyr::mutate(
        TADA.MethodSpeciationName = dplyr::case_when(
          !is.na(Target.TADA.MethodSpeciationName) &
            is.na(
              Target.TADA.SpeciationConversionFactor
            ) ~ Target.TADA.MethodSpeciationName,
          # is.na(Target.TADA.MethodSpeciationName) ~ TADA.MethodSpeciationName
          .default = TADA.MethodSpeciationName
        )
      )
  }

  # remove conversion columns
  clean.data <- clean.data |>
    dplyr::select(
      -c(
        "Target.TADA.CharacteristicName",
        "Target.TADA.ResultSampleFractionText",
        "Target.TADA.MethodSpeciationName",
        "Target.TADA.SpeciationConversionFactor",
        "HarmonizationGroup"
      )
    )

  clean.data$TADA.Harmonized.Flag <- ifelse(
    is.na(clean.data$TADA.Harmonized.Flag),
    FALSE,
    clean.data$TADA.Harmonized.Flag
  )

  # return clean.data
  clean.data <- TADA_CreateComparableID(clean.data)
  clean.data <- TADA_OrderCols(clean.data)
  return(clean.data)
}


#' Calculate Total Nitrogen and Phosphorus
#'
#' This function applies the [Nutrient Aggregation logic](https://echo.epa.gov/trends/loading-tool/resources/technical-background-methodology/nutrient-aggregation#nitrogen)
#' from ECHO's Water Pollutant Loading Tool to add nitrogen subspecies together
#' to approximate a total nitrogen value on a single day at a single site. Additional
#' rows are added with the total nitrogen and total phosphorous estimations.
#' Before summing subspecies, this function runs TADA_AggregateMeasurements to
#' obtain the max value of a characteristic-fraction-speciation-unit-media
#' combination at a given site
#' and date. Where necessary, it uses conversion factors to convert
#' nitrogen subspecies expressed as nitrate, nitrite, ammonia, ammonium, etc. to
#' as nitrogen based on the atomic weights of the different elements in the
#' compound. Similarly, phosphate and other phosphorus forms are converted to
#' AS P where applicable. The reference table is contained within the package but may be
#' edited/customized by users.
#'
#' Nutrient equations are as follows:
#'
#' NITROGEN:
#' 1. TOTAL N (UNFILTERED)
#' 2. TOTAL N (FILTERED) + TOTAL N (PARTICULATE)
#' 3. TOTAL KJELDAHL NITROGEN + NITRATE + NITRITE
#' 4. ORGANIC N + AMMONIA + NITRATE + NITRITE
#' 5. OTHER NITROGEN FORMS
#'
#' PHOSPHORUS:
#' 1. TOTAL PHOSPHORUS
#' 2. PHOSPHATE
#' 3. OTHER PHOSPHORUS FORMS
#'
#' Equations are applied in the order above. The function looks for groups of
#' nutrients that exactly match each equation before looking for every
#' combination within each equation (for example, a group of nitrogen subspecies
#' including AMMONIA and NITRATE will be passed over in an initial sweep of
#' groups of subspecies containing ORGANIC N, AMMONIA, NITRATE, and NITRITE, but
#' will be caught as the function moves down the hierarchy of equations to fewer
#' and fewer subspecies). Eventually, even groups with only one subspecies will
#' be used to represent a TOTAL N value for that site/day/depth.
#'
#' @param .data TADA dataframe. TADA_AutoClean() must have been run (and the
#'   TADA.ResultMeasureValueDataTypes.Flag added)
#'   before this TM/TP summation function can be used.
#'   This function runs required flag functions
#'   (TADA_FindQCActivities, TADA_FlagResultUnit, TADA_FlagFraction, TADA_FlagSpeciation) if
#'   they have not yet been run. Suspect and invalid combinations are not included in
#'   TN and TP summation. While not required for this function to run, ideally,
#'   users should also run TADA_SimpleCensoredMethods
#'   and TADA_HarmonizeSynonyms before this function.
#'   If user wants to consider grouping N or P subspecies across multiple
#'   organizations, user should have also run TADA_FindNearbySites and grouped all
#'   nearby sites to one common TADA.MonitoringLocationIdentifier,
#'   TADA.LatitudeMeasure, TADA.LongitudeMeasure, etc.
#' @param sum_ref Optional. A custom summation reference dataframe the user has
#'   loaded into the R environment. Dataframe must have same columns as default
#'   TADA.summation reference table.
#' @param daily_agg If there are multiple measurements for the same
#'   characteristic-unit-fraction-speciation-media combination at the same location
#'   (TADA.MonitoringLocationIdentifier) on the same day (ActivityStartDate),
#'   this will select a single measurement to use in the
#'   Total N or Total P summation. Defaults to 'max', but can be set
#'   to 'min' or 'mean'.
#'
#' @return TADA dataframe with additional rows representing total N and P
#'   summation values from adding up subspecies. Note that for total phosphorus,
#'   these additional rows are simply a re-classification of phosphorus or
#'   phosphate into the total phosphorus as P format. These new rows share the
#'   same date and monitoring location as the subspecies, but an additional note
#'   is added in the TADA.NutrientSummation.Flag column describing how the total
#'   was derived. Also adds TADA.NutrientSummationGroup and
#'   TADA.NutrientSummationEquation columns, which can be used to trace how the
#'   total was calculated and from which subspecies.
#'
#' @seealso [TADA_AggregateMeasurements()]
#' @seealso [TADA_FlagDepthCategory()]
#' @seealso [TADA_SimpleCensoredMethods()]
#' @seealso [TADA_HarmonizeSynonyms()]
#' @seealso [TADA_FindNearbySites()]
#' @seealso [TADA_FindQCActivities()]
#' @seealso [TADA_FlagResultUnit()]
#' @seealso [TADA_FlagFraction()]
#' @seealso [TADA_FlagSpeciation()]
#' @seealso [TADA_AutoClean()]
#'
#' @export
#'
#' @examples
#' df <- TADA_DataRetrieval(
#'   statecode = "UT", startDate = "2024-06-01",
#'   endDate = "2024-07-01", characteristicType = "Nutrient", applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' df2 <- TADA_SimpleCensoredMethods(df,
#'   nd_method = "multiplier",
#'   nd_multiplier = 0.5, od_method = "as-is", od_multiplier = "null"
#' )
#'
#' df2 <- TADA_RunKeyFlagFunctions(df2, clean = TRUE)
#'
#' df2 <- TADA_HarmonizeSynonyms(df2)
#'
#' df3 <- TADA_CalculateTotalNP(df2, daily_agg = "max")
#'
TADA_CalculateTotalNP <- function(.data, sum_ref, daily_agg = "max") {
  # check .data is data.frame and has required columns
  req_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.ResultMeasureValue",
    "ActivityStartDate",
    "TADA.MonitoringLocationIdentifier",
    "ActivityTypeCode",
    "OrganizationIdentifier",
    "OrganizationFormalName",
    "CountryCode",
    "StateCode",
    "CountyCode",
    "HUCEightDigitCode",
    "MonitoringLocationTypeName",
    "MonitoringLocationDescriptionText",
    "TADA.MonitoringLocationName",
    "TADA.LongitudeMeasure",
    "TADA.LatitudeMeasure",
    "ActivityRelativeDepthName",
    "ActivityMediaSubdivisionName",
    "TADA.ActivityMediaName",
    "TADA.ComparableDataIdentifier",
    "TADA.ResultMeasureValueDataTypes.Flag"
  )
  TADA_CheckColumns(.data, req_cols)
  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  # check to make sure daily_agg is populated with allowable value
  daily_agg <- match.arg(daily_agg)

  # check if QC flag function ran and message warning if not
  if (!"TADA.ActivityType.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Your input dataset was missing the TADA.ActivityType.Flag column, suggesting that QC replicates have not been addressed or reviewed. Running the TADA_FindQCActivities function with the clean = FALSE option before executing this function. This function will not include QC results when aggregating to a daily maximum and total nutrient value."
    )
    .data <- TADA_FindQCActivities(.data, clean = FALSE)
  }

  # check if unit flag function ran and message warning if not
  if (!"TADA.ResultUnit.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Your input dataset was missing the TADA.ResultUnit.Flag column, suggesting that unit and characteristic combinations have not been addressed or reviewed. Running the TADA_FlagResultUnit function with the clean = FALSE option before executing this function. This function will not include results with invalid or suspect units when aggregating to a daily maximum and total nutrient value."
    )
    .data <- TADA_FlagResultUnit(.data, clean = "none")
  }

  # check if fraction flag function ran and message if not
  if (!"TADA.SampleFraction.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Your input dataset was missing the TADA.SampleFraction.Flag column, suggesting that fraction and characteristic combinations have not been addressed or reviewed. Running the TADA_FlagFraction function with the clean = FALSE option before executing this function. This function will not include results with invalid or suspect fractions when aggregating to a daily maximum and total nutrient value."
    )
    .data <- TADA_FlagFraction(.data, clean = FALSE)
  }

  # check if speciation flag function ran and message warning if not
  if (!"TADA.MethodSpeciation.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Your input dataset was missing the TADA.MethodSpeciation.Flag column, suggesting that speciation and characteristic combinations have not been addressed or reviewed. Running the TADA_FlagSpeciation function with the clean = FALSE option before executing this function. This function will not include results with invalid or suspect speciations when aggregating to a daily maximum and total nutrient value."
    )
    .data <- TADA_FlagSpeciation(.data, clean = "none")
  }

  # Check if the specified values are present in the TADA.ResultMeasureValueDataTypes.Flag column
  if (
    any(
      .data$TADA.ResultMeasureValueDataTypes.Flag %in%
        c(
          "TP estimated from one or more subspecies.",
          "TN estimated from one or more subspecies."
        )
    )
  ) {
    # Notify the user that execution is halted
    message(paste(
      "TADA_CalculateTotalNP has already been run. Returning data unchanged. See TADA.ResultMeasureValueDataTypes.Flag column."
    ))
    return(.data)
  }

  # Create the include and exclude data frames
  include_df <- .data[
    .data$TADA.ActivityType.Flag == "Non_QC" &
      (.data$TADA.ResultMeasureValueDataTypes.Flag %in%
        c(
          "Numeric",
          "Result Value/Unit Estimated from Detection Limit",
          "Less Than",
          "Percentage",
          "Approximate Value",
          "Greater Than",
          "Comma-Separated Numeric",
          "Numeric Range - Averaged",
          "Percentage Range - Averaged",
          "Approximate Value",
          "Result Value/Unit Copied from Detection Limit"
        )) &
      (.data$TADA.ResultUnit.Flag %in% c("Pass", "Not Reviewed")) &
      (.data$TADA.SampleFraction.Flag %in% c("Pass", "Not Reviewed")) &
      (.data$TADA.MethodSpeciation.Flag %in% c("Pass", "Not Reviewed")),
  ]

  exclude_df <- .data[
    .data$TADA.ActivityType.Flag != "Non_QC" |
      is.na(.data$TADA.ResultMeasureValueDataTypes.Flag) |
      (.data$TADA.ResultMeasureValueDataTypes.Flag %in%
        c(
          "NA - Not Available",
          "Text",
          "Non-ASCII Character(s)",
          "Result Value/Unit Cannot Be Estimated From Detection Limit",
          "Coerced to NA"
        )) |
      !(.data$TADA.ResultUnit.Flag %in% c("Pass", "Not Reviewed")) |
      !(.data$TADA.SampleFraction.Flag %in% c("Pass", "Not Reviewed")) |
      !(.data$TADA.MethodSpeciation.Flag %in% c("Pass", "Not Reviewed")),
  ]

  # add flags noting these are not used in TN/TP summation
  exclude_df <- exclude_df |>
    dplyr::mutate(
      TADA.NutrientSummation.Flag = "Not used to calculate Total N or P."
    ) |>
    dplyr::mutate(
      TADA.ResultValueAggregation.Flag = "Not considered in max aggregation function"
    )

  # # For function testing only
  # # Calculate the number of rows in each data frame
  # total_rows_data <- nrow(.data)
  # total_rows_include <- nrow(include_df)
  # total_rows_exclude <- nrow(exclude_df)
  #
  # # Check if the sum of rows in include_df and exclude_df equals the total rows in .data
  # test_result <- total_rows_include + total_rows_exclude == total_rows_data
  #
  # # Print the test result
  # if (test_result) {
  #   print("Test passed: The sum of rows in include_df and exclude_df equals the total rows in .data.")
  # } else {
  #   print("Test failed: The sum of rows in include_df and exclude_df does not equal the total rows in .data.")
  # }

  # bring in custom reference df if provided
  if (!missing(sum_ref)) {
    TADA_CheckColumns(sum_ref, names(TADA_GetNutrientSummationRef()))
  } else {
    sum_ref <- TADA_GetNutrientSummationRef()
  }

  # Get grouping cols for daily aggregation
  # create nutrient groups by site and date

  # # used to include depth as well. cm removed 8/6/25
  # depths <- names(include_df)[grepl("DepthHeightMeasure", names(include_df))]
  # depths <- depths[grepl("TADA.", depths)]
  grpcols <- c(
    "ActivityStartDate",
    "TADA.MonitoringLocationIdentifier",
    "TADA.MonitoringLocationName",
    "TADA.LongitudeMeasure",
    "TADA.LatitudeMeasure",
    "TADA.ActivityMediaName",
    "TADA.ComparableDataIdentifier",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.CharacteristicName",
    "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText",
    "OrganizationIdentifier",
    "OrganizationFormalName",
    "CountryCode",
    "StateCode",
    "CountyCode",
    "HUCEightDigitCode",
    "MonitoringLocationTypeName",
    "MonitoringLocationDescriptionText"
    # "ActivityRelativeDepthName"
    # depths # does not make sense for daily aggregation of a max value. Use max value for day regardless of depth
  )

  dat <- suppressMessages(TADA_AggregateMeasurements(
    include_df,
    grouping_cols = grpcols,
    agg_fun = daily_agg,
    clean = FALSE
  ))
  # # for function review only
  # dat_subset <- dat |>
  #   select(all_of(c(
  #     "ActivityStartDate",
  #     "TADA.MonitoringLocationIdentifier",
  #     "TADA.ActivityMediaName",
  #     "TADA.ComparableDataIdentifier",
  #     "TADA.ResultValueAggregation.Flag")))

  # Add rows not selected back at end but do not include in TN/TP summation
  # Define the condition for rows to be added back
  condition <- paste0(
    "Considered in ",
    daily_agg,
    " aggregation function but not selected"
  )
  # Initialize dat_addback as an empty data frame. This handles cases where no rows meet the condition
  dat_addback <- data.frame()
  # Check if the column exists and subset the rows to add back
  if ("TADA.ResultValueAggregation.Flag" %in% names(dat)) {
    dat_addback <- dat[dat$TADA.ResultValueAggregation.Flag %in% condition, ]
  }
  # Add flag to specify these are not used in TN and TP summation
  if (dim(dat_addback)[1] > 0) {
    dat_addback$TADA.NutrientSummation.Flag <- "Not used to calculate Total N or P."
  }

  # move forward with only max values selected for each grouping (dat_TNTP)
  # TADA.ResultValueAggregation.Flag should be "No aggregation needed" OR "Selected as max aggregate value"
  # no longer need "Considered in max aggregation function but not selected"
  # Define the condition for filtering

  valid_flags <- c(
    "No aggregation needed",
    paste0("Selected as ", daily_agg, " aggregate value")
  )

  # Try both original and cleaned column names
  flag_candidates <- "TADA.ResultValueAggregation.Flag"
  flag_col <- flag_candidates[flag_candidates %in% names(dat)]

  # Column 'TADA.ResultValueAggregation.Flag' not found
  if (length(flag_col) == 0) {
    message(
      "There is no applicable data to calculate TN or TP. Returning data unchanged."
    )
    return(.data)
  }
  flag_col <- flag_col[1]

  # Build logical index, dropping NAs
  matching_rows <- !is.na(dat[[flag_col]]) & dat[[flag_col]] %in% valid_flags

  dat_TNTP <- dat[matching_rows, , drop = FALSE]

  # join data to summation table and keep only those that match for summations
  sum_dat <- merge(dat_TNTP, sum_ref, all.x = TRUE)
  sum_dat <- subset(sum_dat, !is.na(sum_dat$NutrientGroup))

  # # REMINDER FOR TADA TEAM: NEED TO ENSURE ALL COMBOS PRESENT IN TABLE
  # # for review only: what is not matching?
  # sum_dat_review <- sum_dat |>
  #   dplyr::select(all_of(c(
  #     "ActivityStartDate",
  #     "TADA.MonitoringLocationIdentifier",
  #     "TADA.ActivityMediaName",
  #     "TADA.ComparableDataIdentifier",
  #     "TADA.ResultValueAggregation.Flag",
  #     "NutrientGroup"))) |>
  #   dplyr::filter(is.na(NutrientGroup))

  # If the join results in matching rows
  if (dim(sum_dat)[1] > 0) {
    thecols <- grpcols[
      !grpcols %in%
        c(
          "TADA.ComparableDataIdentifier",
          "TADA.ResultMeasure.MeasureUnitCode",
          "TADA.CharacteristicName",
          "TADA.MethodSpeciationName",
          "TADA.ResultSampleFractionText"
        )
    ]

    # create nutrient group ID's.
    sum_dat <- sum_dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(thecols))) |>
      dplyr::mutate(TADA.NutrientSummationGroup = dplyr::cur_group_id())

    # bring in equations
    eqns <- utils::read.csv(system.file(
      "extdata",
      "NP_equations.csv",
      package = "EPATADA"
    ))

    # dataframe to hold results
    summeddata <- data.frame()
    grps <- vector()

    for (i in 1:length(unique(eqns$Nutrient))) {
      nut <- unique(eqns$Nutrient)[i]
      nutqns <- subset(eqns, eqns$Nutrient == nut)
      for (j in 1:length(unique(nutqns$EQN))) {
        eqnum <- unique(nutqns$EQN)[j]
        eqn <- subset(nutqns, nutqns$EQN == eqnum)$SummationName
        nutrient <- ifelse(
          nut == "N",
          "Total Nitrogen as N",
          "Total Phosphorus as P"
        )
        # for each equation, see if any groups contain all required subspecies,
        # and for each pick the variant with the lowest rank.
        # combine group with other groups and remove group ID from consideration
        # for the next equation
        out <- sum_dat |>
          dplyr::filter(!TADA.NutrientSummationGroup %in% grps) |>
          dplyr::group_by(TADA.NutrientSummationGroup) |>
          dplyr::filter(all(eqn %in% SummationName)) |> # this line ensures that ALL subspecies are present within an equation group, not just one or more
          dplyr::filter(SummationName %in% eqn) |>
          dplyr::mutate(
            TADA.NutrientSummationEquation = paste0(
              unique(SummationName),
              collapse = " + "
            )
          )

        out <- out |>
          dplyr::group_by(TADA.NutrientSummationGroup, SummationName) |>
          dplyr::slice_min(SummationRank, with_ties = FALSE)
        out$TADA.NutrientSummation.Flag <- paste0(
          "Used to calculate ",
          nutrient,
          "."
        )
        out$nutrient <- nutrient
        summeddata <- plyr::rbind.fill(summeddata, out)
        grps <- c(grps, unique(out$TADA.NutrientSummationGroup))
      }
    }

    # Convert speciation if needed
    summeddata$TADA.ResultMeasureValue <- ifelse(
      !is.na(summeddata$SummationSpeciationConversionFactor),
      summeddata$TADA.ResultMeasureValue *
        summeddata$SummationSpeciationConversionFactor,
      summeddata$TADA.ResultMeasureValue
    )
    summeddata$TADA.MethodSpeciationName <- ifelse(
      !is.na(summeddata$SummationSpeciationConversionFactor) &
        summeddata$nutrient == "Total Nitrogen as N",
      "AS N",
      summeddata$TADA.MethodSpeciationName
    )
    summeddata$TADA.MethodSpeciationName <- ifelse(
      !is.na(summeddata$SummationSpeciationConversionFactor) &
        summeddata$nutrient == "Total Phosphorus as P",
      "AS P",
      summeddata$TADA.MethodSpeciationName
    )

    # Get to total N or P
    totncols <- c(
      thecols,
      "TADA.NutrientSummationGroup",
      "TADA.NutrientSummationEquation"
    )
    TotalN <- summeddata |>
      dplyr::filter(nutrient == "Total Nitrogen as N") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(totncols))) |>
      dplyr::summarise(
        TADA.ResultMeasureValue = sum(TADA.ResultMeasureValue)
      ) |>
      dplyr::mutate(
        TADA.CharacteristicName = "TOTAL NITROGEN, MIXED FORMS",
        TADA.ResultSampleFractionText = "UNFILTERED",
        TADA.MethodSpeciationName = "AS N",
        TADA.ResultMeasure.MeasureUnitCode = "MG/L",
        TADA.ComparableDataIdentifier = "TOTAL NITROGEN, MIXED FORMS_UNFILTERED_AS N_MG/L",
        TADA.NutrientSummation.Flag = "New row added: Nutrient summation from one or more subspecies.",
        TADA.ResultMeasureValueDataTypes.Flag = "TN estimated from one or more subspecies.",
        TADA.ResultValueAggregation.Flag = "Nutrient summation from selected aggregate values and values where no aggregation was needed."
      )
    TotalP <- summeddata |>
      dplyr::filter(nutrient == "Total Phosphorus as P") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(totncols))) |>
      dplyr::summarise(
        TADA.ResultMeasureValue = sum(TADA.ResultMeasureValue)
      ) |>
      dplyr::mutate(
        TADA.CharacteristicName = "TOTAL PHOSPHORUS, MIXED FORMS",
        TADA.ResultSampleFractionText = "UNFILTERED",
        TADA.MethodSpeciationName = "AS P",
        TADA.ResultMeasure.MeasureUnitCode = "UG/L",
        TADA.ComparableDataIdentifier = "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L",
        TADA.NutrientSummation.Flag = "New row added: Nutrient summation from one subspecies.",
        TADA.ResultMeasureValueDataTypes.Flag = "TP estimated from one or more subspecies.",
        TADA.ResultValueAggregation.Flag = "Nutrient summation from selected aggregate values and values where no aggregation was needed."
      )

    # If summation is zero....include anyway
    # Generate unique ResultIdentifier
    Totals <- plyr::rbind.fill(TotalN, TotalP) |>
      dplyr::mutate(
        ResultIdentifier = paste0(
          "TADA-",
          sample(seq_len(1000000000), dplyr::n())
        )
      )

    # Combine all data back into dat_TNTP and get rid of unneeded columns
    dat_TNTP_combined <- dat_TNTP |>
      base::merge(summeddata, all.x = TRUE) |>
      plyr::rbind.fill(Totals) |>
      dplyr::select(
        -SummationFractionNotes,
        -SummationSpeciationNotes,
        -SummationSpeciationConversionFactor,
        -SummationName,
        -SummationRank,
        -SummationNote,
        -nutrient,
        -NutrientGroup
      ) |>
      dplyr::mutate(
        TADA.NutrientSummation.Flag = dplyr::if_else(
          is.na(TADA.NutrientSummation.Flag),
          "Not used to calculate Total N or P.",
          TADA.NutrientSummation.Flag
        )
      )
    # At end... summation complete at this point
    # Check if each data frame is not empty
    dat_TNTP_combined_non_empty <- if (nrow(dat_TNTP_combined) > 0) {
      dat_TNTP_combined
    } else {
      NULL
    }
    exclude_df_non_empty <- if (nrow(exclude_df) > 0) exclude_df else NULL
    dat_addback_non_empty <- if (nrow(dat_addback) > 0) dat_addback else NULL
    # Bind rows only if the data frames are not NULL
    final_TNTP <- dplyr::bind_rows(
      dat_TNTP_combined_non_empty,
      exclude_df_non_empty,
      dat_addback_non_empty
    )

    # Filter rows based on specific conditions
    duplicates <- final_TNTP |>
      dplyr::group_by(TADA.NutrientSummationGroup) |>
      dplyr::filter(
        dplyr::n() == 2 &
          TADA.ResultMeasureValue[1] == TADA.ResultMeasureValue[2]
      ) |>
      dplyr::filter(
        TADA.NutrientSummation.Flag ==
          "New row added: Nutrient summation from one or more subspecies."
      )

    remove_list <- unique(duplicates$ResultIdentifier)

    # Filter the data frame
    complete_df <- final_TNTP |>
      dplyr::filter(!ResultIdentifier %in% remove_list)
  } else {
    # Check if each data frame is not empty
    dat_TNTP_non_empty <- if (nrow(dat_TNTP) > 0) dat_TNTP else NULL
    exclude_df_non_empty <- if (nrow(exclude_df) > 0) exclude_df else NULL
    dat_addback_non_empty <- if (nrow(dat_addback) > 0) dat_addback else NULL

    # Bind rows only if the data frames are not NULL
    complete_df <- dplyr::bind_rows(
      dat_TNTP_non_empty,
      exclude_df_non_empty,
      dat_addback_non_empty
    )

    # if there are no data to sum
    complete_df$TADA.NutrientSummation.Flag <- "Not used to calculate Total N or P."
    message(
      "No Total N or P subspecies exist in dataset. Returning input dataset with TADA.NutrientSummation.Flag set to 'Not used to calculate Total N or P'"
    )
  }

  # order columns and return complete_df
  complete_df <- TADA_CreateComparableID(complete_df)
  complete_df <- TADA_OrderCols(complete_df)
  return(complete_df)
}


#' Aggregate multiple result values to a min, max, or mean
#'
#' This function groups TADA data by user-defined columns and aggregates the
#' TADA.ResultMeasureValue to a minimum, maximum, or mean value.
#'
#' @param .data A TADA dataframe
#'
#' @param grouping_cols The column names used to group the data
#'
#' @param agg_fun The aggregation function used on the grouped data. This can
#'   either be 'min', 'max', or 'mean'.
#'
#' @param clean Boolean. Determines whether other measurements from the group
#'   aggregation should be removed or kept in the dataframe. If clean = FALSE,
#'   additional measurements that were considered are indicated in the
#'   TADA.ResultValueAggregation.Flag. The default is clean = FALSE.
#'
#' @return A TADA dataframe with aggregated values combined into one row. If the
#'   agg_fun is 'min' or 'max', the function will select the row matching the
#'   aggregation condition and flag it as the selected measurement. If the
#'   agg_fun is 'mean', the function will select a random row from the
#'   aggregated rows to represent the metadata associated with the mean value,
#'   and gives the row a unique ResultIdentifier: the original ResultIdentifier
#'   with the prefix "TADA-". Function adds a TADA.ResultValueAggregation.Flag
#'   to indicate which rows have been aggregated.
#'
#' @export
#'
#' @examples
#' # Load example dataset
#' utils::data(Data_6Tribes_5y)
#' # Select maximum value per day, site, comparable data identifier,
#' # unit, result detection condition,
#' # and activity type code. Clean all non-maximum measurements from grouped data.
#' Data_6Tribes_5y_max <- TADA_AggregateMeasurements(Data_6Tribes_5y,
#'   grouping_cols = c(
#'     "ActivityStartDate",
#'     "TADA.MonitoringLocationIdentifier",
#'     "TADA.ComparableDataIdentifier",
#'     "ResultDetectionConditionText",
#'     "ActivityTypeCode",
#'     "TADA.ResultMeasure.MeasureUnitCode"
#'   ),
#'   agg_fun = "max",
#'   clean = TRUE
#' )
#'
#' # Calculate a mean value per day, site, comparable data identifier, unit,
#' # result detection condition,
#' # and activity type code. Keep all measurements used to calculate mean measurement.
#' Data_6Tribes_5y_mean <- TADA_AggregateMeasurements(Data_6Tribes_5y,
#'   grouping_cols = c(
#'     "ActivityStartDate", "TADA.MonitoringLocationIdentifier",
#'     "TADA.ComparableDataIdentifier", "ResultDetectionConditionText",
#'     "ActivityTypeCode", "TADA.ResultMeasure.MeasureUnitCode"
#'   ),
#'   agg_fun = "mean",
#'   clean = FALSE
#' )
#'
TADA_AggregateMeasurements <- function(
  .data,
  grouping_cols = c(
    "ActivityStartDate",
    "TADA.MonitoringLocationIdentifier",
    "TADA.ComparableDataIdentifier",
    "ResultDetectionConditionText",
    "ActivityTypeCode",
    "TADA.ResultMeasure.MeasureUnitCode"
  ),
  agg_fun = c("max", "min", "mean"),
  clean = FALSE
) {
  # check .data is data.frame and has required columns
  TADA_CheckColumns(.data, grouping_cols)
  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  agg_fun <- match.arg(agg_fun)

  # Find multiple values in groups
  ncount <- .data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
    dplyr::summarise(ncount = length(ResultIdentifier))

  if (max(ncount$ncount) < 2) {
    message("TADA_AggregateMeasurements: No rows to aggregate.")
    return(.data)
  } else {
    dat <- merge(.data, ncount, all.x = TRUE)

    if (any(is.na(dat$TADA.ResultMeasureValue))) {
      "TADA_AggregateMeasurements: Your dataset contains one or more rows where TADA.ResultMeasureValue = NA. Recommend removing these rows before proceeding. Otherwise, the function will not consider NAs in its calculations."
    }

    dat$TADA.ResultValueAggregation.Flag <- ifelse(
      dat$ncount == 1,
      "No aggregation needed",
      paste0(
        "Considered in ",
        agg_fun,
        " aggregation function but not selected"
      )
    )
    multiples <- dat |> dplyr::filter(ncount > 1)

    dat <- dat |> dplyr::select(-ncount)

    if (agg_fun == "max") {
      out <- multiples |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
        dplyr::slice_max(
          order_by = TADA.ResultMeasureValue,
          n = 1,
          with_ties = FALSE
        )
      dat$TADA.ResultValueAggregation.Flag <- ifelse(
        dat$ResultIdentifier %in% out$ResultIdentifier,
        paste0("Selected as ", agg_fun, " aggregate value"),
        dat$TADA.ResultValueAggregation.Flag
      )
    }
    if (agg_fun == "min") {
      out <- multiples |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
        dplyr::slice_min(
          order_by = TADA.ResultMeasureValue,
          n = 1,
          with_ties = FALSE
        )
      dat$TADA.ResultValueAggregation.Flag <- ifelse(
        dat$ResultIdentifier %in% out$ResultIdentifier,
        paste0("Selected as ", agg_fun, " aggregate value"),
        dat$TADA.ResultValueAggregation.Flag
      )
    }
    if (agg_fun == "mean") {
      out <- multiples |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
        dplyr::mutate(
          TADA.ResultMeasureValue1 = mean(TADA.ResultMeasureValue, na.rm = TRUE)
        ) |>
        dplyr::slice_sample(n = 1) |>
        dplyr::mutate(
          TADA.ResultValueAggregation.Flag = paste0(
            "Selected as ",
            agg_fun,
            " aggregate value, with randomly selected metadata from a row in the aggregate group"
          )
        )
      out <- out |>
        dplyr::select(-TADA.ResultMeasureValue) |>
        dplyr::rename(TADA.ResultMeasureValue = TADA.ResultMeasureValue1) |>
        dplyr::mutate(ResultIdentifier = paste0("TADA-", ResultIdentifier))
      dat <- plyr::rbind.fill(dat, out)
    }

    if (clean == TRUE) {
      dat <- subset(
        dat,
        !dat$TADA.ResultValueAggregation.Flag %in%
          c(paste0(
            "Considered in ",
            agg_fun,
            " aggregation function but not selected"
          ))
      )
    }

    dat <- TADA_CreateComparableID(dat)
    dat <- TADA_OrderCols(dat)
    message("Aggregation results:")
    message(table(dat$TADA.ResultValueAggregation.Flag))
    return(dat)
  }
}
