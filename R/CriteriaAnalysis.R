#' Join WQP data to criteria (TADA.ComparableDataIdentifier preferred, else fallbacks)
#'
#' @description
#' Join WQP results to a criteria table by the best available key:
#' 1) TADA.ComparableDataIdentifier (if present in both and non-NA in criteria)
#' 2) TADA.CharacteristicName + TADA.ResultSampleFractionText + TADA.MethodSpeciationName
#' 3) TADA.CharacteristicName + TADA.ResultSampleFractionText
#' 4) TADA.CharacteristicName + TADA.MethodSpeciationName
#' 5) TADA.CharacteristicName (or when byChar = TRUE)
#'
#' For each fallback pass, rows with NA in any of the pass keys are dropped
#' from both inputs for that pass. Left-join semantics are preserved overall.
#'
#' @param .data data.frame/tibble of WQP results.
#' @param criteria data.frame/tibble of criteria rows.
#'
#' @return data.frame with WQP rows and matching criteria columns.
#'
#' @examples
#' # load example data.frame
#' utils::data("Data_MT_MissoulaCounty", package = "EPATADA")
#' MT_data <- Data_MT_MissoulaCounty
#'
#' # load example criteria table from community hub
#' criteria_MT <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")
#'
#' # join the table by best match from what is filled out from the criteria table
#' MT_data_criteria <- TADA_Analysis_Join_WQP_Criteria(MT_data, criteria_MT)
#'
#' @noRd
TADA_Analysis_Join_WQP_Criteria <- function(
    .data,
    criteria,
    byChar = FALSE,
    AUMLRef = NULL, # has not been handled yet
    AU_UsesRef = NULL # has not been handled yet
    ) {
  stopifnot(is.data.frame(.data), is.data.frame(criteria))
  
  # Harmonize case on selected keys only
  upper_keys <- c(
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  criteria <- criteria |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(upper_keys),
      ~ toupper(as.character(.x))
    ))
  for (nm in upper_keys) {
    if (nm %in% names(.data)) {
      .data[[nm]] <- toupper(as.character(.data[[nm]]))
    }
  }
  
  # Join keys
  id_col1 <- "TADA.ComparableDataIdentifier"
  id_col2 <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  id_col3 <- c("TADA.CharacteristicName", "TADA.ResultSampleFractionText")
  id_col4 <- c("TADA.CharacteristicName", "TADA.MethodSpeciationName")
  id_col5 <- c("TADA.CharacteristicName")
  
  # Optional use/org keys (only if present in BOTH)
  use_org <- c("ATTAINS.UseName", "ATTAINS.OrganizationIdentifier")
  use_org_in_both <- intersect(use_org, intersect(names(.data), names(criteria)))
  with_use_org <- function(keys) unique(c(keys, use_org_in_both))
  
  # Split criteria into disjoint sets (NO de-duplication)
  criteria1 <- dplyr::filter(criteria, !is.na(.data$`TADA.ComparableDataIdentifier`)) |>
    dplyr::select(
      -dplyr::any_of(c(
        "TADA.CharacteristicName",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName"
      ))
    )
  
  criteria2 <- dplyr::filter(
    criteria,
    is.na(.data$`TADA.ComparableDataIdentifier`),
    !is.na(.data$`TADA.ResultSampleFractionText`),
    !is.na(.data$`TADA.MethodSpeciationName`)
  ) |>
    dplyr::select(-dplyr::any_of("TADA.ComparableDataIdentifier"))
  
  criteria3 <- dplyr::filter(
    criteria,
    is.na(.data$`TADA.ComparableDataIdentifier`),
    !is.na(.data$`TADA.ResultSampleFractionText`),
    is.na(.data$`TADA.MethodSpeciationName`)
  ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "TADA.ComparableDataIdentifier",
        "TADA.MethodSpeciationName"
      ))
    )
  
  criteria4 <- dplyr::filter(
    criteria,
    is.na(.data$`TADA.ComparableDataIdentifier`),
    is.na(.data$`TADA.ResultSampleFractionText`),
    !is.na(.data$`TADA.MethodSpeciationName`)
  ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "TADA.ComparableDataIdentifier",
        "TADA.ResultSampleFractionText"
      ))
    )
  
  criteria5 <- dplyr::filter(
    criteria,
    is.na(.data$`TADA.ComparableDataIdentifier`),
    is.na(.data$`TADA.ResultSampleFractionText`),
    is.na(.data$`TADA.MethodSpeciationName`)
  ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "TADA.ComparableDataIdentifier",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName"
      ))
    )
  
  # Expected characteristics (only those present in criteria)
  expected_chars <- if ("TADA.CharacteristicName" %in% names(criteria)) {
    unique(stats::na.omit(criteria$TADA.CharacteristicName))
  } else {
    character(0)
  }
  
  if (isTRUE(byChar)) {
    # Char-only join (+ optional Use/Org) – keep all criteria rows
    keys <- with_use_org("TADA.CharacteristicName")
    drop_cols <- c(
      "TADA.ComparableDataIdentifier",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName"
    )
    
    crit_char <- criteria |>
      dplyr::select(-dplyr::any_of(drop_cols)) |>
      dplyr::filter(!is.na(.data$`TADA.CharacteristicName`))
    
    wqp_criteria <- dplyr::left_join(
      .data, crit_char, by = keys, relationship = "many-to-many"
    )
    
    # Warn only for criteria characteristics that had WQP rows but did not match
    if ("TADA.CharacteristicName" %in% names(.data) && length(expected_chars) > 0) {
      .data_chars_in_expected <- intersect(
        unique(stats::na.omit(.data$TADA.CharacteristicName)),
        expected_chars
      )
      matched_chars <- intersect(
        unique(stats::na.omit(wqp_criteria$TADA.CharacteristicName)),
        expected_chars
      )
      unmatched_chars <- setdiff(.data_chars_in_expected, matched_chars)
      if (length(unmatched_chars) > 0) {
        warning(
          sprintf(
            "WQP contains %d characteristic(s) present in criteria that did not match (up to 10 shown): %s",
            length(unmatched_chars),
            paste(utils::head(unmatched_chars, 10), collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
    return(wqp_criteria)
  }
  
  # Cross-pass: allow the same WQP row to match in multiple passes
  results <- list()
  
  # Pass 1: ID (+ optional Use/Org)
  if (nrow(criteria1) > 0 && all(with_use_org(id_col1) %in% names(.data))) {
    keys <- with_use_org(id_col1)
    j1 <- dplyr::left_join(.data, criteria1, by = keys, relationship = "many-to-many")
    if (nrow(j1) > 0) results[[length(results) + 1]] <- j1
  }
  
  # Pass 2: Char + Fraction + Speciation (+ optional Use/Org)
  if (nrow(criteria2) > 0 && all(with_use_org(id_col2) %in% names(.data))) {
    keys <- with_use_org(id_col2)
    j2 <- dplyr::left_join(.data, criteria2, by = keys, relationship = "many-to-many")
    if (nrow(j2) > 0) results[[length(results) + 1]] <- j2
  }
  
  # Pass 3: Char + Fraction (+ optional Use/Org)
  if (nrow(criteria3) > 0 && all(with_use_org(id_col3) %in% names(.data))) {
    keys <- with_use_org(id_col3)
    j3 <- dplyr::left_join(.data, criteria3, by = keys, relationship = "many-to-many")
    if (nrow(j3) > 0) results[[length(results) + 1]] <- j3
  }
  
  # Pass 4: Char + Speciation (+ optional Use/Org)
  if (nrow(criteria4) > 0 && all(with_use_org(id_col4) %in% names(.data))) {
    keys <- with_use_org(id_col4)
    j4 <- dplyr::left_join(.data, criteria4, by = keys, relationship = "many-to-many")
    if (nrow(j4) > 0) results[[length(results) + 1]] <- j4
  }
  
  # Pass 5: Char only (+ optional Use/Org)
  if (nrow(criteria5) > 0 && all(with_use_org(id_col5) %in% names(.data))) {
    keys <- with_use_org(id_col5)
    j5 <- dplyr::left_join(.data, criteria5, by = keys, relationship = "many-to-many")
    if (nrow(j5) > 0) results[[length(results) + 1]] <- j5
  }
  
  wqp_criteria <- if (length(results) > 0) {
    dplyr::bind_rows(results)
  } else {
    # No criteria matched in any pass
    .data
  }
  
  # Warn only for expected (criteria) characteristics that had unmatched WQP rows overall
  if ("TADA.CharacteristicName" %in% names(.data) && length(expected_chars) > 0) {
    matched_chars <- intersect(
      unique(stats::na.omit(wqp_criteria$TADA.CharacteristicName)),
      expected_chars
    )
    .data_chars_in_expected <- intersect(
      unique(stats::na.omit(.data$TADA.CharacteristicName)),
      expected_chars
    )
    unmatched_chars <- setdiff(.data_chars_in_expected, matched_chars)
    if (length(unmatched_chars) > 0) {
      warning(
        sprintf(
          "WQP contains %d characteristic(s) present in criteria that did not match: %s \nPlease ensure the fraction and/or speciation in your criteria table match those in your WQP data.",
          length(unmatched_chars),
          paste(utils::head(unmatched_chars, 10), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  return(wqp_criteria)
}


# test_analysis <- TADA_Analysis_Custom(
#       Data_MT_MissoulaCounty, ATTAINS.OrganizationIdentifier = "MTDEQ", ATTAINS.ParameterName = "ESCHERICHIA COLI (E. COLI)", TADA.CharacteristicName  = "ESCHERICHIA COLI", MagnitudeValueLower = 1, DurationValue = 1, DurationUnit = "n-day", DurationMethod = "geometric mean")
TADA_Analysis_Custom <- function(
    .data,
    ATTAINS.OrganizationIdentifier = NULL,
    ATTAINS.ParameterName          = NULL,
    ATTAINS.UseName                = NULL,
    TADA.ComparableDataIdentifier  = NULL,
    TADA.CharacteristicName        = NULL,
    TADA.ResultSampleFractionText  = NULL,
    TADA.MethodSpeciationName      = NULL,
    ATTAINS.WaterType              = NULL,          
    SaltFresh                      = NULL,
    DepthCategory                  = NULL,
    UniqueSpatialCriteria          = NULL,
    AcuteChronic                   = NULL,
    EquationBased                  = NULL,
    MagnitudeValueLower            = NULL,
    MagnitudeValueUpper            = NULL,
    MagnitudeUnit                  = NULL,                 
    DurationValue                  = NULL,
    DurationUnit                   = NULL,
    DurationMethod                 = NULL,
    FreqValue                      = NULL,
    FreqMethod                     = NULL,
    AssessPeriod                   = NULL,
    AssessPeriodStartDate          = NULL,
    AssessPeriodEndDate            = NULL,
    Season                         = NULL,
    SeasonStartDate                = NULL,
    SeasonEndDate                  = NULL,
    DistrCount                     = NULL,
    DistrPeriod                    = NULL,
    DistrMinSample                 = NULL
) {
  # Helper: keep columns even when args are NULL
  to_na <- function(x) if (is.null(x)) NA else x
  
  criteria <- data.frame(
    ATTAINS.OrganizationIdentifier = to_na(ATTAINS.OrganizationIdentifier),
    ATTAINS.ParameterName          = to_na(ATTAINS.ParameterName),
    ATTAINS.UseName                = to_na(ATTAINS.UseName),
    TADA.ComparableDataIdentifier  = to_na(TADA.ComparableDataIdentifier),
    TADA.CharacteristicName        = to_na(TADA.CharacteristicName),
    TADA.ResultSampleFractionText  = to_na(TADA.ResultSampleFractionText),
    TADA.MethodSpeciationName      = to_na(TADA.MethodSpeciationName),
    ATTAINS.WaterType              = to_na(ATTAINS.WaterType),          
    SaltFresh                      = to_na(SaltFresh),
    DepthCategory                  = to_na(DepthCategory),
    UniqueSpatialCriteria          = to_na(UniqueSpatialCriteria),
    AcuteChronic                   = to_na(AcuteChronic),
    EquationBased                  = to_na(EquationBased),
    MagnitudeValueLower            = to_na(MagnitudeValueLower),
    MagnitudeValueUpper            = to_na(MagnitudeValueUpper),
    MagnitudeUnit                  = to_na(MagnitudeUnit),                 
    DurationValue                  = to_na(DurationValue),
    DurationUnit                   = to_na(DurationUnit),
    DurationMethod                 = to_na(DurationMethod),
    FreqValue                      = to_na(FreqValue),
    FreqMethod                     = to_na(FreqMethod),
    AssessPeriod                   = to_na(AssessPeriod),
    AssessPeriodStartDate          = to_na(AssessPeriodStartDate),
    AssessPeriodEndDate            = to_na(AssessPeriodEndDate),
    Season                         = to_na(Season),
    SeasonStartDate                = to_na(SeasonStartDate),
    SeasonEndDate                  = to_na(SeasonEndDate),
    DistrCount                     = to_na(DistrCount),
    DistrPeriod                    = to_na(DistrPeriod),
    DistrMinSample                 = to_na(DistrMinSample),
    stringsAsFactors               = FALSE
  )
  
  # filter data to just TADA.ComparableDataIdentifier or char (or ATTAINS.Parameter/frac/spec)
  .data <- dplyr::filter(.data, TADA.CharacteristicName == unique(criteria$TADA.CharacteristicName))
  
  criteria <- criteria |>
    TADA_CorrectColType() |>
    dplyr::left_join(
      spsUtil::quiet(
        TADA_DefineCriteriaMethodology()
        ),
      by = names(criteria)
      )
    
  criteria2 <- TADA_DefineCriteriaMethodology(.data = .data, org_id = ATTAINS.OrganizationIdentifier, criteriaMethods = criteria)
  
  message(TADACommunityHub::runAllValidations(criteria2)$overall_status)
  
  # join the wqp data with the criteria table (joins be best matches of what is filled in from the criteria table)
  data_w_criteria <- TADA_Analysis_Join_WQP_Criteria(.data, criteria = criteria2)
  
  data_w_criteria_windows <- TADA_Analysis_Join_Windows(data_w_criteria)
  
  # start of with just geo mean function
  geo_mean <- function(x) {
    x <- as.numeric(x)
    # Replace zeros with 1 if censored data exists
    x[x == 0] <- 1
    # Return NA if negative numbers exist
    if (any(x < 0, na.rm = TRUE)) return(NA_real_)
    
    exp(mean(log(x), na.rm = TRUE))
  }
  
  final_analysis_test <- data_w_criteria_windows |>
    dplyr::group_by(window_start, window_end,
                    TADA.ComparableDataIdentifier, TADA.CharacteristicName,
                    TADA.ResultSampleFractionText, TADA.MethodSpeciationName,
                    AcuteChronic, SaltFresh,
                    MagnitudeValueLower, MagnitudeValueUpper, MagnitudeUnit,
                    DurationValue, DurationUnit, DurationMethod,
                    FreqValue, FreqMethod, AssessPeriod, AssessPeriodStartDate,
                    AssessPeriodEndDate, Season, SeasonStartDate, SeasonEndDate,
                    DistrCount, DistrPeriod, DistrMinSample
                    ) |>
    dplyr::summarise(
      summary_value = {
        method <- tolower(coalesce(dplyr::first(DurationMethod), ""))
        vals <- as.numeric(TADA.ResultMeasureValue)
        
        if (grepl("rolling geometric mean", method)) {
          geo_mean(vals)
          
        } else if (grepl("rolling arithmetic mean", method)) {
          mean(vals, na.rm = TRUE)
          
        } else if (grepl("geometric mean", method)) {
          geo_mean(vals)
          
        } else if (grepl("arithmetic mean", method)) {
          mean(vals, na.rm = TRUE)
          
        } else if (grepl("arithmetic median", method)) {
          median(vals, na.rm = TRUE)
          
        } else if (grepl("arithmetic max", method)) {
          max(vals, na.rm = TRUE)
          
        } else if (grepl("arithmetic min", method)) {
          min(vals, na.rm = TRUE)
          
        } else if (grepl("arithmetic extremes", method)) {
          upper_ok <- all(vals <= TADA.UpperLimit, na.rm = TRUE)
          lower_ok <- all(vals >= TADA.LowerLimit, na.rm = TRUE)
          
          if (upper_ok && lower_ok) {
            mean(vals, na.rm = TRUE)
          } else {
            NA_real_
          }
          
        } else if (grepl("mean of daily minima", method)) {
          mean(vals, na.rm = TRUE)
          
        } else if (grepl("mean of daily maxima", method)) {
          mean(vals, na.rm = TRUE)
          
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      exceedance = dplyr::case_when(
        is.na(summary_value) | (is.na(MagnitudeValueLower) & is.na(MagnitudeValueUpper) )~ NA_character_,
        summary_value < MagnitudeValueLower | summary_value > MagnitudeValueUpper ~ "yes",
        TRUE ~ "no"
      )
    )
  
  exceedance_percent <- final_analysis_test |>
    dplyr::group_by(dplyr::across(-c(window_start, window_end, summary_value, exceedance))) |>dplyr::summarise(
      n_exceed = sum(exceedance == "yes", na.rm = TRUE),
      n_eval = sum(!is.na(exceedance)),
      percent_exceed = if (n_eval > 0) 100 * n_exceed / n_eval else NA_real_,
      .groups = "drop"
    )
  
  final_summary <- list(
    data_windows_summary <- final_analysis_test,
    exceedance_summary <- exceedance_percent
  )
  
  return(final_summary)
}
