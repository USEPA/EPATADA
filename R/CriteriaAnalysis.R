#' Join WQP data to criteria (ID preferred, with smart fallbacks)
#'
#' @description
#' Join WQP results to a criteria table by the best available key:
#' 1) TADA.ComparableDataIdentifier (if present in both and non-NA in criteria)
#' 2) TADA.CharacteristicName + TADA.ResultSampleFractionText + TADA.MethodSpeciationName
#' 3) TADA.CharacteristicName + TADA.ResultSampleFractionText
#' 4) TADA.CharacteristicName + TADA.MethodSpeciationName
#' 5) TADA.CharacteristicName (byChar = TRUE)
#'
#' For each fallback pass, rows with NA in any of the pass keys are dropped
#' from both inputs for that pass. Left-join semantics are preserved overall.
#'
#' @param wqp data.frame/tibble of WQP results.
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
#' MT_data_criteria <- join_wqp_criteria(MT_data, criteria_MT)
#'
#' @noRd
join_wqp_criteria <- function(wqp, criteria, byChar = FALSE, AUMLRef = NULL, AU_UsesRef = NULL) {
  stopifnot(is.data.frame(wqp), is.data.frame(criteria))
  
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
    if (nm %in% names(wqp)) {
      wqp[[nm]] <- toupper(as.character(wqp[[nm]]))
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
  use_org_in_both <- intersect(use_org, intersect(names(wqp), names(criteria)))
  with_use_org <- function(keys) unique(c(keys, use_org_in_both))
  
  # Split criteria into disjoint sets and deduplicate by keys (+use/org)
  criteria1 <- dplyr::filter(
    criteria,
    !is.na(.data$`TADA.ComparableDataIdentifier`)
  ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "TADA.CharacteristicName",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName"
      ))
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(with_use_org(id_col1))),
      .keep_all = TRUE
    )
  
  criteria2 <- dplyr::filter(
    criteria,
    is.na(.data$`TADA.ComparableDataIdentifier`),
    !is.na(.data$`TADA.ResultSampleFractionText`),
    !is.na(.data$`TADA.MethodSpeciationName`)
  ) |>
    dplyr::select(-dplyr::any_of("TADA.ComparableDataIdentifier")) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(with_use_org(id_col2))),
      .keep_all = TRUE
    )
  
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
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(with_use_org(id_col3))),
      .keep_all = TRUE
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
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(with_use_org(id_col4))),
      .keep_all = TRUE
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
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(with_use_org(id_col5))),
      .keep_all = TRUE
    )
  
  # Expected characteristics (only those present in criteria)
  expected_chars <- if ("TADA.CharacteristicName" %in% names(criteria)) {
    unique(stats::na.omit(criteria$TADA.CharacteristicName))
  } else {
    character(0)
  }
  
  if (isTRUE(byChar)) {
    # Char-only join (+ optional Use/Org)
    keys <- with_use_org("TADA.CharacteristicName")
    drop_cols <- c(
      "TADA.ComparableDataIdentifier",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName"
    )
    
    crit_char <- criteria |>
      dplyr::select(-dplyr::any_of(drop_cols)) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
    
    wqp_criteria <- dplyr::left_join(wqp, crit_char, by = keys)
    
    # Warn only for criteria characteristics that had WQP rows but did not match
    if (
      "TADA.CharacteristicName" %in% names(wqp) && length(expected_chars) > 0
    ) {
      wqp_chars_in_expected <- intersect(
        unique(stats::na.omit(wqp$TADA.CharacteristicName)),
        expected_chars
      )
      matched_chars <- intersect(
        unique(stats::na.omit(wqp_criteria$TADA.CharacteristicName)),
        expected_chars
      )
      unmatched_chars <- setdiff(wqp_chars_in_expected, matched_chars)
      if (length(unmatched_chars) > 0) {
        warning(
          sprintf(
            "WQP contains %d characteristic(s) present in criteria that did not match (showing up to 10): %s",
            length(unmatched_chars),
            paste(utils::head(unmatched_chars, 10), collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
    return(wqp_criteria)
  }
  
  # Non-char-only path: sequential passes to avoid duplicates and preserve left-join semantics
  results <- list()
  wqp_rem <- wqp # define unconditionally so it's always available
  
  # Pass 1: ID (+ optional Use/Org)
  if (nrow(criteria1) > 0 && all(with_use_org(id_col1) %in% names(wqp_rem))) {
    keys <- with_use_org(id_col1)
    tmp <- dplyr::semi_join(wqp_rem, criteria1, by = keys)
    if (nrow(tmp) > 0) {
      results[[length(results) + 1]] <- dplyr::left_join(
        tmp,
        criteria1,
        by = keys
      )
      wqp_rem <- dplyr::anti_join(wqp_rem, criteria1, by = keys)
    }
  }
  
  # Pass 2: Char + Fraction + Speciation (+ optional Use/Org)
  if (nrow(criteria2) > 0 && all(with_use_org(id_col2) %in% names(wqp_rem))) {
    keys <- with_use_org(id_col2)
    tmp <- dplyr::semi_join(wqp_rem, criteria2, by = keys)
    if (nrow(tmp) > 0) {
      results[[length(results) + 1]] <- dplyr::left_join(
        tmp,
        criteria2,
        by = keys
      )
      wqp_rem <- dplyr::anti_join(wqp_rem, criteria2, by = keys)
    }
  }
  
  # Pass 3: Char + Fraction (+ optional Use/Org)
  if (nrow(criteria3) > 0 && all(with_use_org(id_col3) %in% names(wqp_rem))) {
    keys <- with_use_org(id_col3)
    tmp <- dplyr::semi_join(wqp_rem, criteria3, by = keys)
    if (nrow(tmp) > 0) {
      results[[length(results) + 1]] <- dplyr::left_join(
        tmp,
        criteria3,
        by = keys
      )
      wqp_rem <- dplyr::anti_join(wqp_rem, criteria3, by = keys)
    }
  }
  
  # Pass 4: Char + Speciation (+ optional Use/Org)
  if (nrow(criteria4) > 0 && all(with_use_org(id_col4) %in% names(wqp_rem))) {
    keys <- with_use_org(id_col4)
    tmp <- dplyr::semi_join(wqp_rem, criteria4, by = keys)
    if (nrow(tmp) > 0) {
      results[[length(results) + 1]] <- dplyr::left_join(
        tmp,
        criteria4,
        by = keys
      )
      wqp_rem <- dplyr::anti_join(wqp_rem, criteria4, by = keys)
    }
  }
  
  # Pass 5: Char only (+ optional Use/Org)
  if (nrow(criteria5) > 0 && all(with_use_org(id_col5) %in% names(wqp_rem))) {
    keys <- with_use_org(id_col5)
    tmp <- dplyr::semi_join(wqp_rem, criteria5, by = keys)
    if (nrow(tmp) > 0) {
      results[[length(results) + 1]] <- dplyr::left_join(
        tmp,
        criteria5,
        by = keys
      )
      wqp_rem <- dplyr::anti_join(wqp_rem, criteria5, by = keys)
    }
  }
  
  wqp_criteria <- if (length(results) > 0) {
    dplyr::bind_rows(dplyr::bind_rows(results), wqp_rem)
  } else {
    wqp_rem
  }
  
  # Warn only for expected (criteria) characteristics that had unmatched WQP rows
  if (
    "TADA.CharacteristicName" %in% names(wqp_rem) && length(expected_chars) > 0
  ) {
    unmatched_chars <- intersect(
      unique(stats::na.omit(wqp_rem$TADA.CharacteristicName)),
      expected_chars
    )
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
