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
#'   file. The primary use for this argument is when a user has generated a
#'   synonym reference file unique to their data, and they made changes to
#'   that file.
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
#' Data_6Tribes_5yClean <- TADA_FlagFraction(Data_6Tribes_5yClean, clean = TRUE)
#' Data_6Tribes_5yClean <- TADA_FlagResultUnit(Data_6Tribes_5yClean, clean = "suspect_only")
#' Data_6Tribes_5yClean <- TADA_FlagSpeciation(Data_6Tribes_5yClean, clean = "suspect_only")
#' Data_6Tribes_5yClean <- TADA_FlagMethod(Data_6Tribes_5yClean, clean = TRUE)
#' CreateRefTable <- TADA_GetSynonymRef(Data_6Tribes_5yClean)
#'
#' # Append synonym reference table columns to dataframe and transform/convert
#' # data to the USER SUPPLIED reference table values:
#' Data_6Tribes_5yClean_Harmonized <-
#'   TADA_HarmonizeSynonyms(Data_6Tribes_5yClean, ref = CreateRefTable)
#' }
#'
TADA_HarmonizeSynonyms <- function(.data, ref = NULL, np_speciation = TRUE) {
  # Required columns in .data
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasureValue"
  )
  TADA_CheckColumns(.data, expected_cols)

  # Empty input: keep pipeline continuity
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. Returning input unchanged.")
    return(.data)
  }

  # Ensure numeric result value
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop(
      "TADA.ResultMeasureValue is not numeric. This column must be numeric before proceeding."
    )
  }

  # Helpers
  key_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  target_cols <- c(
    "Target.TADA.CharacteristicName",
    "Target.TADA.ResultSampleFractionText",
    "Target.TADA.MethodSpeciationName"
  )

  normalize_keys <- function(x) {
    if (is.character(x)) {
      y <- trimws(x)
      y[y == ""] <- NA_character_
      y[toupper(y) == "NONE"] <- NA_character_
      y
    } else {
      x
    }
  }
  to_na_trim <- function(x) {
    if (is.character(x)) {
      y <- trimws(x)
      y[y == ""] <- NA_character_
      y
    } else {
      x
    }
  }

  # Normalize .data keys (trim, convert "" and "NONE" -> NA)
  .data <- .data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(key_cols), normalize_keys))

  # Build harm.ref (either from user-supplied ref or default template)
  if (!is.null(ref)) {
    required_ref_cols <- c(
      "TADA.CharacteristicName",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName",
      "Target.TADA.CharacteristicName",
      "Target.TADA.ResultSampleFractionText",
      "Target.TADA.MethodSpeciationName",
      "Target.TADA.SpeciationConversionFactor"
    )
    TADA_CheckColumns(ref, required_ref_cols)

    # Add optional columns if missing
    add_ref_cols <- c(
      "TADA.CharacteristicNameAssumptions",
      "TADA.FractionAssumptions",
      "TADA.SpeciationAssumptions",
      "HarmonizationGroup"
    )
    for (col in setdiff(add_ref_cols, names(ref))) {
      ref[[col]] <- NA_character_
    }

    harm.ref <- ref
  } else {
    harm.ref <- TADA_GetSynonymRef(.data)
    # Ensure optional columns exist (if the internal template ever changes)
    add_ref_cols <- c(
      "TADA.CharacteristicNameAssumptions",
      "TADA.FractionAssumptions",
      "TADA.SpeciationAssumptions",
      "HarmonizationGroup"
    )
    for (col in setdiff(add_ref_cols, names(harm.ref))) {
      harm.ref[[col]] <- NA_character_
    }
  }

  # Normalize ref keys and targets (trim, "" -> NA; treat "NONE" -> NA for keys)
  harm.ref <- harm.ref |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(key_cols), normalize_keys),
      dplyr::across(dplyr::any_of(target_cols), to_na_trim)
    )

  # Conversion factor numeric (protect against character input)
  if ("Target.TADA.SpeciationConversionFactor" %in% names(harm.ref)) {
    harm.ref$Target.TADA.SpeciationConversionFactor <- suppressWarnings(as.numeric(
      harm.ref$Target.TADA.SpeciationConversionFactor
    ))
  }

  # Warn if duplicate key rows exist (could create many-to-many join duplicates)
  dups <- harm.ref |>
    dplyr::count(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dups) > 0) {
    warning(
      "Reference table contains duplicate key combinations. Results may be duplicated."
    )
  }

  # Warn if same key maps to multiple distinct target sets
  if (all(target_cols %in% names(harm.ref))) {
    tgt_sets <- harm.ref |>
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
      dplyr::summarise(
        n_target_sets = dplyr::n_distinct(paste(
          `Target.TADA.CharacteristicName`,
          `Target.TADA.ResultSampleFractionText`,
          `Target.TADA.MethodSpeciationName`,
          `Target.TADA.SpeciationConversionFactor`
        )),
        .groups = "drop"
      ) |>
      dplyr::filter(.data$n_target_sets > 1)
    if (nrow(tgt_sets) > 0) {
      warning(
        "Reference table contains keys with conflicting target assignments. Review your synonym reference."
      )
    }
  }

  # Deduplicate ref
  harm.ref <- harm.ref |> dplyr::distinct()

  # Drop old comparable ID (will be recreated later)
  .data <- .data[, !names(.data) %in% c("TADA.ComparableDataIdentifier")]

  # Join harm.ref to .data (NA-aware join supported in dplyr >= 1.1.0)
  flag.data <- .data |>
    dplyr::left_join(
      harm.ref,
      by = c(
        "TADA.CharacteristicName",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName"
      ),
      na_matches = "na"
    )

  # Update TADA.CharacteristicName to target when present
  clean.data <- flag.data |>
    dplyr::mutate(
      TADA.CharacteristicName = dplyr::case_when(
        !is.na(Target.TADA.CharacteristicName) ~ Target.TADA.CharacteristicName,
        TRUE ~ TADA.CharacteristicName
      )
    )

  # Update TADA.ResultSampleFractionText to target when present
  clean.data <- clean.data |>
    dplyr::mutate(
      TADA.ResultSampleFractionText = dplyr::case_when(
        !is.na(
          Target.TADA.ResultSampleFractionText
        ) ~ Target.TADA.ResultSampleFractionText,
        TRUE ~ TADA.ResultSampleFractionText
      )
    )

  # Compute whether speciation actually changes (to guard conversion)
  clean.data <- clean.data |>
    dplyr::mutate(
      .spec_changed = (is.na(TADA.MethodSpeciationName) &
        !is.na(Target.TADA.MethodSpeciationName)) |
        (!is.na(TADA.MethodSpeciationName) &
          is.na(Target.TADA.MethodSpeciationName)) |
        (!is.na(TADA.MethodSpeciationName) &
          !is.na(Target.TADA.MethodSpeciationName) &
          TADA.MethodSpeciationName != Target.TADA.MethodSpeciationName)
    )

  # Update speciation and convert measure values if requested
  if (isTRUE(np_speciation)) {
    clean.data <- clean.data |>
      dplyr::mutate(
        TADA.MethodSpeciationName = dplyr::case_when(
          !is.na(
            Target.TADA.MethodSpeciationName
          ) ~ Target.TADA.MethodSpeciationName,
          TRUE ~ TADA.MethodSpeciationName
        ),
        # Apply conversion only if a factor is provided AND speciation actually changes
        TADA.ResultMeasureValue = dplyr::case_when(
          !is.na(Target.TADA.SpeciationConversionFactor) &
            .spec_changed ~ Target.TADA.SpeciationConversionFactor *
            TADA.ResultMeasureValue,
          TRUE ~ TADA.ResultMeasureValue
        )
      )
  } else {
    clean.data <- clean.data |>
      dplyr::mutate(
        TADA.MethodSpeciationName = dplyr::case_when(
          !is.na(Target.TADA.MethodSpeciationName) &
            is.na(
              Target.TADA.SpeciationConversionFactor
            ) ~ Target.TADA.MethodSpeciationName,
          TRUE ~ TADA.MethodSpeciationName
        )
      )
  }

  # Create Comparable ID and normalize fraction/speciation/unit ("NONE" for missing)
  clean.data <- TADA_CreateComparableID(clean.data)

  # Compute harmonization flag AFTER Comparable ID creation so NA/blank -> "NONE" changes are included
  diff_chr <- function(old, new) {
    (is.na(old) & !is.na(new)) |
      (!is.na(old) & is.na(new)) |
      (!is.na(old) & !is.na(new) & old != new)
  }
  diff_num <- function(old, new, tol = 0) {
    (is.na(old) & !is.na(new)) |
      (!is.na(old) & is.na(new)) |
      (!is.na(old) & !is.na(new) & abs(new - old) > tol)
  }

  # Guard for unit column if not present
  old_unit <- if ("TADA.ResultMeasure.MeasureUnitCode" %in% names(flag.data)) {
    flag.data$TADA.ResultMeasure.MeasureUnitCode
  } else {
    rep(NA_character_, nrow(flag.data))
  }
  new_unit <- if ("TADA.ResultMeasure.MeasureUnitCode" %in% names(clean.data)) {
    clean.data$TADA.ResultMeasure.MeasureUnitCode
  } else {
    rep(NA_character_, nrow(clean.data))
  }

  clean.data$TADA.Harmonized.Flag <- diff_chr(
    flag.data$TADA.CharacteristicName,
    clean.data$TADA.CharacteristicName
  ) |
    diff_chr(
      flag.data$TADA.ResultSampleFractionText,
      clean.data$TADA.ResultSampleFractionText
    ) |
    diff_chr(
      flag.data$TADA.MethodSpeciationName,
      clean.data$TADA.MethodSpeciationName
    ) |
    diff_chr(old_unit, new_unit) |
    diff_num(
      flag.data$TADA.ResultMeasureValue,
      clean.data$TADA.ResultMeasureValue,
      tol = 0
    )

  # Drop conversion/reference-only columns (use any_of for resilience)
  clean.data <- clean.data |>
    dplyr::select(
      -dplyr::any_of(c(
        "Target.TADA.CharacteristicName",
        "Target.TADA.ResultSampleFractionText",
        "Target.TADA.MethodSpeciationName",
        "Target.TADA.SpeciationConversionFactor",
        "HarmonizationGroup",
        ".spec_changed"
      ))
    )

  # Finalize
  clean.data <- TADA_OrderCols(clean.data)
  return(clean.data)
}

#' Calculate Total Nitrogen and Phosphorus
#'
#' Apply nutrient aggregation logic from EPA's Enforcement and Compliance History
#' Online (ECHO) Water Pollutant Loading Tool
#' to add nitrogen subspecies and phosphorus forms together to approximate
#' total nitrogen (TN) and total phosphorus (TP) by site and day.
#'
#' Before summing, the function aggregates measurements to a single daily value
#' per characteristic–unit–fraction–speciation–media combination using the chosen
#' `daily_agg`. Where needed, it converts nitrogen subspecies to "AS N" and
#' phosphorus forms to "AS P" using conversion factors in the summation reference.
#' The internal summation reference can be customized and supplied via `sum_ref`.
#'
#' @details
#' - If required QA/QC flagging columns are absent, the function runs
#'   [TADA_FindQCActivities()] (clean = FALSE), [TADA_FlagResultUnit()] (clean = "none"),
#'   [TADA_FlagFraction()] (clean = FALSE), and [TADA_FlagSpeciation()] (clean = "none"),
#'   and excludes invalid or suspect combinations from TN/TP summations.
#' - The function will not run a second time on the same data: if
#'   TADA.ResultMeasureValueDataTypes.Flag already contains "TN/TP estimated...",
#'   the input is returned unchanged.
#' - Daily aggregation uses `daily_agg` ("max", "min", or "mean") per site/day
#'   and metadata combination. Rows considered but not selected are preserved
#'   with explanatory flags.
#' - Keys are normalized (trim whitespace; `""` and `"NONE"` become `NA`) before
#'   matching to the summation reference; the join is NA-aware (dplyr >= 1.1.0).
#' - Speciation conversions are applied where a conversion factor is provided.
#' - New rows for TN/TP totals are added with deterministic ResultIdentifier values
#'   based on site/date/group to ease testing and traceability.
#'
#' @param .data TADA dataframe. [TADA_AutoClean()] should have been run and
#'   TADA.ResultMeasureValueDataTypes.Flag must be present. The function will run
#'   required flag functions if needed. Suspect/invalid combinations are excluded.
#' @param sum_ref Optional custom summation reference dataframe with the same
#'   columns as the internal reference; if omitted, the internal reference is used.
#' @param daily_agg Aggregation function used to collapse multiple measurements
#'   per day/site/metadata combination. One of "max", "min", or "mean". Defaults to "max".
#'
#' @return The input dataframe plus additional rows representing TN and TP totals,
#'   with explanatory flags: TADA.NutrientSummation.Flag, TADA.NutrientSummationGroup,
#'   and TADA.NutrientSummationEquation. Original rows not used in summations are preserved.
#'
#' @seealso [TADA_AggregateMeasurements()], [TADA_FlagResultUnit()],
#'   [TADA_FlagFraction()], [TADA_FlagSpeciation()], [TADA_HarmonizeSynonyms()],
#'   [TADA_GetNutrientSummationRef()]
#'
#' @note Requires dplyr >= 1.1.0 for NA-aware joins when matching to the summation ref.
#'
#' @examples
#' \dontrun{
#' df <- TADA_DataRetrieval(
#'   statecode = "UT",
#'   startDate = "2024-06-01",
#'   endDate = "2024-07-01",
#'   characteristicType = "Nutrient",
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' df2 <- TADA_SimpleCensoredMethods(
#'   df, nd_method = "multiplier", nd_multiplier = 0.5,
#'   od_method = "as-is", od_multiplier = "null"
#' )
#'
#' df2 <- TADA_RunKeyFlagFunctions(df2, clean = TRUE)
#' df2 <- TADA_HarmonizeSynonyms(df2)
#'
#' out <- TADA_CalculateTotalNP(df2, daily_agg = "max")
#' }
TADA_CalculateTotalNP <- function(
  .data,
  sum_ref = NULL,
  daily_agg = c("max", "min", "mean")
) {
  # Validate daily_agg
  daily_agg <- match.arg(daily_agg)

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
    return(NULL)
  }

  # Ensure QC/flag functions ran
  if (!"TADA.ActivityType.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Missing TADA.ActivityType.Flag. Running TADA_FindQCActivities(clean = FALSE)."
    )
    .data <- TADA_FindQCActivities(.data, clean = FALSE)
  }
  if (!"TADA.ResultUnit.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Missing TADA.ResultUnit.Flag. Running TADA_FlagResultUnit(clean = 'none')."
    )
    .data <- TADA_FlagResultUnit(.data, clean = "none")
  }
  if (!"TADA.SampleFraction.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Missing TADA.SampleFraction.Flag. Running TADA_FlagFraction(clean = FALSE)."
    )
    .data <- TADA_FlagFraction(.data, clean = FALSE)
  }
  if (!"TADA.MethodSpeciation.Flag" %in% names(.data)) {
    message(
      "TADA_CalculateTotalNP: Missing TADA.MethodSpeciation.Flag. Running TADA_FlagSpeciation(clean = 'none')."
    )
    .data <- TADA_FlagSpeciation(.data, clean = "none")
  }

  # Prevent running twice
  if (
    any(
      .data$TADA.ResultMeasureValueDataTypes.Flag %in%
        c(
          "TP estimated from one or more subspecies.",
          "TN estimated from one or more subspecies."
        )
    )
  ) {
    message(
      "TADA_CalculateTotalNP has already been run. Returning data unchanged. See TADA.ResultMeasureValueDataTypes.Flag."
    )
    return(.data)
  }

  # Include/exclude subsets
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

  exclude_df <- exclude_df |>
    dplyr::mutate(
      TADA.NutrientSummation.Flag = "Not used to calculate Total N or P.",
      TADA.ResultValueAggregation.Flag = "Not considered in max aggregation function"
    )

  # Summation reference: use default if not provided; normalize and ensure numeric
  if (is.null(sum_ref)) {
    sum_ref <- TADA_GetNutrientSummationRef()
  } else {
    TADA_CheckColumns(sum_ref, names(TADA_GetNutrientSummationRef()))
  }

  key_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )

  normalize_keys_vec <- function(x) {
    if (is.character(x)) {
      y <- trimws(x)
      y[y == ""] <- NA_character_
      y[toupper(y) == "NONE"] <- NA_character_
      y
    } else {
      x
    }
  }

  # Normalize sum_ref keys and ensure numeric
  sum_ref <- sum_ref |>
    dplyr::mutate(dplyr::across(dplyr::all_of(key_cols), normalize_keys_vec))
  if ("SummationSpeciationConversionFactor" %in% names(sum_ref)) {
    sum_ref$SummationSpeciationConversionFactor <- suppressWarnings(as.numeric(
      sum_ref$SummationSpeciationConversionFactor
    ))
  }

  # Daily aggregation (max/min/mean)
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
  )

  dat <- suppressMessages(TADA_AggregateMeasurements(
    include_df,
    grouping_cols = grpcols,
    agg_fun = daily_agg,
    clean = FALSE
  ))

  # Add back not selected rows for info
  condition <- paste0(
    "Considered in ",
    daily_agg,
    " aggregation function but not selected"
  )
  dat_addback <- if ("TADA.ResultValueAggregation.Flag" %in% names(dat)) {
    dat[dat$TADA.ResultValueAggregation.Flag %in% condition, ]
  } else {
    data.frame()
  }
  if (nrow(dat_addback) > 0) {
    dat_addback$TADA.NutrientSummation.Flag <- "Not used to calculate Total N or P."
  }

  # Filter to selected/no-agg-needed rows
  valid_flags <- c(
    "No aggregation needed",
    paste0("Selected as ", daily_agg, " aggregate value")
  )
  flag_col <- "TADA.ResultValueAggregation.Flag"
  if (!flag_col %in% names(dat)) {
    message(
      "There is no applicable data to calculate TN or TP. Returning data unchanged."
    )
    return(.data)
  }
  dat_TNTP <- dat[
    !is.na(dat[[flag_col]]) & dat[[flag_col]] %in% valid_flags,
    ,
    drop = FALSE
  ]

  # Normalize dat_TNTP keys
  dat_TNTP <- dat_TNTP |>
    dplyr::mutate(dplyr::across(dplyr::all_of(key_cols), normalize_keys_vec))

  # NA-aware left join to summation ref (pull SummationName/Rank/Group, etc.)
  sum_dat <- dplyr::left_join(
    dat_TNTP,
    sum_ref,
    by = key_cols,
    na_matches = "na"
  )

  # Keep only rows participating in summations
  sum_dat <- subset(sum_dat, !is.na(sum_dat$NutrientGroup))

  if (nrow(sum_dat) > 0) {
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

    sum_dat <- sum_dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(thecols))) |>
      dplyr::mutate(TADA.NutrientSummationGroup = dplyr::cur_group_id())

    eqns <- utils::read.csv(system.file(
      "extdata",
      "NP_equations.csv",
      package = "EPATADA"
    ))

    summeddata <- dplyr::tibble()
    grps <- character()

    for (nut in unique(eqns$Nutrient)) {
      nutqns <- eqns[eqns$Nutrient == nut, , drop = FALSE]
      for (eqnum in unique(nutqns$EQN)) {
        eqn_names <- nutqns$SummationName[nutqns$EQN == eqnum]
        nutrient <- ifelse(
          nut == "N",
          "Total Nitrogen as N",
          "Total Phosphorus as P"
        )

        out <- sum_dat |>
          dplyr::filter(!.data$TADA.NutrientSummationGroup %in% grps) |>
          dplyr::group_by(.data$TADA.NutrientSummationGroup) |>
          dplyr::filter(all(eqn_names %in% .data$SummationName)) |>
          dplyr::filter(.data$SummationName %in% eqn_names) |>
          dplyr::mutate(
            TADA.NutrientSummationEquation = paste0(
              unique(.data$SummationName),
              collapse = " + "
            )
          ) |>
          dplyr::group_by(
            .data$TADA.NutrientSummationGroup,
            .data$SummationName
          ) |>
          dplyr::slice_min(.data$SummationRank, with_ties = FALSE) |>
          dplyr::ungroup()

        out$TADA.NutrientSummation.Flag <- paste0(
          "Used to calculate ",
          nutrient,
          "."
        )
        out$nutrient <- nutrient

        summeddata <- dplyr::bind_rows(summeddata, out)
        grps <- c(grps, unique(out$TADA.NutrientSummationGroup))
      }
    }

    # Convert speciation if needed
    if ("SummationSpeciationConversionFactor" %in% names(summeddata)) {
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
    }

    # Unit normalization to match reference assumptions:
    # - Nitrogen to MG/L
    # - Phosphorus to UG/L
    to_mgL <- function(value, unit) {
      ifelse(!is.na(unit) & toupper(unit) == "UG/L", value / 1000, value)
    }
    to_ugL <- function(value, unit) {
      ifelse(!is.na(unit) & toupper(unit) == "MG/L", value * 1000, value)
    }
    summeddata <- summeddata |>
      dplyr::mutate(
        # Normalize nitrogen rows to MG/L
        TADA.ResultMeasureValue = dplyr::if_else(
          .data$nutrient == "Total Nitrogen as N",
          to_mgL(
            .data$TADA.ResultMeasureValue,
            .data$TADA.ResultMeasure.MeasureUnitCode
          ),
          .data$TADA.ResultMeasureValue
        ),
        TADA.ResultMeasure.MeasureUnitCode = dplyr::if_else(
          .data$nutrient == "Total Nitrogen as N" &
            !is.na(.data$TADA.ResultMeasure.MeasureUnitCode) &
            toupper(.data$TADA.ResultMeasure.MeasureUnitCode) == "UG/L",
          "MG/L",
          .data$TADA.ResultMeasure.MeasureUnitCode
        ),
        # Normalize phosphorus rows to UG/L
        TADA.ResultMeasureValue = dplyr::if_else(
          .data$nutrient == "Total Phosphorus as P",
          to_ugL(
            .data$TADA.ResultMeasureValue,
            .data$TADA.ResultMeasure.MeasureUnitCode
          ),
          .data$TADA.ResultMeasureValue
        ),
        TADA.ResultMeasure.MeasureUnitCode = dplyr::if_else(
          .data$nutrient == "Total Phosphorus as P" &
            !is.na(.data$TADA.ResultMeasure.MeasureUnitCode) &
            toupper(.data$TADA.ResultMeasure.MeasureUnitCode) == "MG/L",
          "UG/L",
          .data$TADA.ResultMeasure.MeasureUnitCode
        )
      )

    # Compute totals
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
    totncols <- c(
      thecols,
      "TADA.NutrientSummationGroup",
      "TADA.NutrientSummationEquation"
    )

    TotalN <- summeddata |>
      dplyr::filter(.data$nutrient == "Total Nitrogen as N") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(totncols))) |>
      dplyr::summarise(
        TADA.ResultMeasureValue = sum(.data$TADA.ResultMeasureValue),
        .groups = "drop"
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
      dplyr::filter(.data$nutrient == "Total Phosphorus as P") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(totncols))) |>
      dplyr::summarise(
        TADA.ResultMeasureValue = sum(.data$TADA.ResultMeasureValue),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        TADA.CharacteristicName = "TOTAL PHOSPHORUS, MIXED FORMS",
        TADA.ResultSampleFractionText = "UNFILTERED",
        TADA.MethodSpeciationName = "AS P",
        TADA.ResultMeasure.MeasureUnitCode = "UG/L",
        TADA.ComparableDataIdentifier = "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L",
        TADA.NutrientSummation.Flag = "New row added: Nutrient summation from one or more subspecies.",
        TADA.ResultMeasureValueDataTypes.Flag = "TP estimated from one or more subspecies.",
        TADA.ResultValueAggregation.Flag = "Nutrient summation from selected aggregate values and values where no aggregation was needed."
      )

    # Deterministic ResultIdentifier for totals
    make_id <- function(df, suffix) {
      paste0(
        "TADA-",
        df$TADA.MonitoringLocationIdentifier,
        "_",
        df$ActivityStartDate,
        "_",
        df$TADA.NutrientSummationGroup,
        "_",
        suffix
      )
    }
    if (nrow(TotalN) > 0) {
      TotalN$ResultIdentifier <- make_id(TotalN, "TN")
    }
    if (nrow(TotalP) > 0) {
      TotalP$ResultIdentifier <- make_id(TotalP, "TP")
    }

    Totals <- dplyr::bind_rows(TotalN, TotalP)

    # Join back the "used" flags to the selected daily rows (NA-safe join)
    back_keys <- c(
      "ActivityStartDate",
      "TADA.MonitoringLocationIdentifier",
      "TADA.MonitoringLocationName",
      "TADA.LongitudeMeasure",
      "TADA.LatitudeMeasure",
      "TADA.ActivityMediaName",
      # "TADA.ResultMeasure.MeasureUnitCode",  # REMOVE from join keys
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
    )

    used_cols <- c(
      back_keys,
      "TADA.ResultMeasure.MeasureUnitCode", # keep in payload if you want
      "TADA.NutrientSummation.Flag",
      "TADA.NutrientSummationEquation"
    )

    used_subset <- dplyr::distinct(summeddata[, used_cols, drop = FALSE])

    dat_TNTP_combined <- dplyr::left_join(
      dat_TNTP,
      used_subset,
      by = back_keys,
      na_matches = "na"
    ) |>
      dplyr::bind_rows(Totals) |>
      dplyr::select(
        -dplyr::any_of(c(
          "SummationFractionNotes",
          "SummationSpeciationNotes",
          "SummationSpeciationConversionFactor",
          "SummationName",
          "SummationRank",
          "SummationNote"
        ))
      ) |>
      dplyr::mutate(
        TADA.NutrientSummation.Flag = dplyr::if_else(
          is.na(.data$TADA.NutrientSummation.Flag),
          "Not used to calculate Total N or P.",
          .data$TADA.NutrientSummation.Flag
        )
      )

    final_TNTP <- dat_TNTP_combined
    # Duplicate removal (retain original behavior)
    duplicates <- final_TNTP |>
      dplyr::group_by(.data$TADA.NutrientSummationGroup) |>
      dplyr::filter(
        dplyr::n() == 2 &
          .data$TADA.ResultMeasureValue[1] == .data$TADA.ResultMeasureValue[2]
      ) |>
      dplyr::filter(
        .data$TADA.NutrientSummation.Flag ==
          "New row added: Nutrient summation from one or more subspecies."
      )
    remove_list <- unique(duplicates$ResultIdentifier)
    complete_df <- final_TNTP |>
      dplyr::filter(!.data$ResultIdentifier %in% remove_list)
  } else {
    # No data to sum
    dat_TNTP_non_empty <- if (nrow(dat_TNTP) > 0) dat_TNTP else NULL
    exclude_df_non_empty <- if (nrow(exclude_df) > 0) exclude_df else NULL
    dat_addback_non_empty <- if (nrow(dat_addback) > 0) dat_addback else NULL

    complete_df <- dplyr::bind_rows(
      dat_TNTP_non_empty,
      exclude_df_non_empty,
      dat_addback_non_empty
    )

    complete_df$TADA.NutrientSummation.Flag <- "Not used to calculate Total N or P."
    message(
      "No Total N or P subspecies exist in dataset. Returning input dataset with TADA.NutrientSummation.Flag set to 'Not used to calculate Total N or P'"
    )
  }

  # Order columns and return complete_df
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
      warning(
        "TADA_AggregateMeasurements: One or more rows have TADA.ResultMeasureValue = NA. These NAs are ignored in aggregation."
      )
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
