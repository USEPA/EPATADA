#' Join WQP data to criteria, AUMLRef and AU_UsesRef
#' (UNDER ACTIVE DEVELOPMENT)
#'
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
#' When MLSummaryRef is provided (optional), this function first joins the WQP
#' .data to the MLSummaryRef by MonitoringLocationIdentifier.
#' NOTE: MLSummaryRef is in active development and joins the ref tables
#' of the spatial summary, parameters and uses for analysis.
#'
#' @param .data A TADA data frame.
#' @param criteria data.frame of TADA compatible criteria table for any
#' of either TADA.ComparableDataIdentifier and a combination of TADA.CharacteristicName,
#' TADA.ResultSampleFractionText, and TADA.MethodSpeciationName
#' @param MLSummaryRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit. This table is populated based on the inputs
#' from the users and their desired level of analysis.
#' If provided the data frame must contain these columns:
#' "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
#' "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
#' "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
#' "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "LongitudeMeasure",
#' "LatitudeMeasure", "IncludeOrExclude" and "UniqueSpatialCriteria".
#' @param byChar A boolean value. If byChar = TRUE, this function will join the
#' WQP data frame with the criteria table by only CharacteristicName, regardless
#' of what has been filled out in the criteria table.
#'
#' @return data.frame with WQP rows and matching criteria columns.
#' @export
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
#' # create the MLSummaryRef (ML only - no AU or other spatial columns)
#' params <- TADA_ParametersForAnalysis(
#'   Data_MT_MissoulaCounty, org_id = "MTDEQ", auto_assign = "Org")
#'
#' uses <- TADA_UsesForAnalysis(Data_MT_MissoulaCounty,
#'  org_id = "MTDEQ", paramRef = params, auto_assign = TRUE)
#'
#' mlsummary <- TADA_MLSummary(
#'   Data_MT_MissoulaCounty,
#'   org_id = "MTDEQ",
#'   usesRef = uses)
#'
#' # join the table by best match, along with the MLSummaryRef
#' MT_data_criteria2 <- TADA_Analysis_Join_WQP_Criteria(
#'   MT_data,
#'   criteria_MT,
#'   MLSummaryRef = mlsummary)
#'
TADA_Analysis_Join_WQP_Criteria <- function(
  .data,
  criteria,
  byChar = FALSE,
  MLSummaryRef = NULL
) {
  stopifnot(is.data.frame(.data), is.data.frame(criteria))

  upper_keys <- c(
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.MonitoringLocationIdentifier",
    "ATTAINS.OrganizationIdentifier",
    "ATTAINS.AssessmentUnitIdentifier",
    "MonitoringLocationIdentifier",
    "MonitoringLocationTypeName",
    "TADA.ParameterName",
    "ATTAINS.ParameterName",
    "ATTAINS.UseName",
    "ATTAINS.WaterType",
    "SaltFresh",
    "DepthCategory",
    "LongitudeMeasure",
    "LatitudeMeasure",
    "IncludeOrExclude",
    "UniqueSpatialCriteria"
  )

  upperize <- function(df) {
    for (nm in intersect(names(df), upper_keys)) {
      if (is.character(df[[nm]]) || is.factor(df[[nm]])) {
        df[[nm]] <- toupper(as.character(df[[nm]]))
      }
    }
    df
  }

  .data <- upperize(.data)
  criteria <- upperize(criteria)

  if (!is.null(MLSummaryRef) && is.data.frame(MLSummaryRef)) {
    MLSummaryRef <- upperize(MLSummaryRef)
  }

  # ------------------------------------------------------------
  # Join MLSummaryRef first (if provided)
  # ------------------------------------------------------------
  if (!is.null(MLSummaryRef) && nrow(MLSummaryRef) > 0) {
    needed <- c("MonitoringLocationIdentifier", "TADA.ComparableDataIdentifier")

    if (all(needed %in% names(.data)) && all(needed %in% names(MLSummaryRef))) {
      .data <- dplyr::left_join(
        .data,
        MLSummaryRef,
        by = needed,
        relationship = "many-to-many"
      )
    } else {
      warning(
        "MLSummaryRef could not be joined because required columns are missing.",
        call. = FALSE
      )
    }
  }

  # ------------------------------------------------------------
  # Criteria join logic
  # ------------------------------------------------------------
  if (isTRUE(byChar)) {
    crit_char <- criteria |>
      dplyr::filter(!is.na(.data$`TADA.CharacteristicName`))

    wqp_criteria <- dplyr::left_join(
      .data,
      crit_char,
      by = "TADA.CharacteristicName",
      relationship = "many-to-many"
    )

    return(wqp_criteria)
  }

  # Join keys if MLSummaryRef is supplied
  ML_id_col <- c(
    "ATTAINS.OrganizationIdentifier",
    "ATTAINS.ParameterName",
    "ATTAINS.UseName",
    "ATTAINS.WaterType",
    "SaltFresh",
    "DepthCategory",
    "UniqueSpatialCriteria"
  )

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

  # If MLSummaryRef is provided, append ML_id_col to all join key sets
  if (!is.null(MLSummaryRef)) {
    id_col1 <- c(id_col1, ML_id_col)
    id_col2 <- c(id_col2, ML_id_col)
    id_col3 <- c(id_col3, ML_id_col)
    id_col4 <- c(id_col4, ML_id_col)
    id_col5 <- c(id_col5, ML_id_col)
  }

  # Split criteria into disjoint sets (NO de-duplication)
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

  results <- list()

  do_join <- function(df, crit, keys) {
    df <- TADA_CorrectColType(df)
    crit <- TADA_CorrectColType(crit)

    if (nrow(crit) == 0) {
      return(NULL)
    }
    if (!all(keys %in% names(df))) {
      return(NULL)
    }
    if (!all(keys %in% names(crit))) {
      return(NULL)
    }

    dplyr::left_join(df, crit, by = keys, relationship = "many-to-many")
  }

  j1 <- do_join(.data, criteria1, id_col1)
  if (!is.null(j1)) {
    results[[length(results) + 1]] <- j1
  }

  j2 <- do_join(.data, criteria2, id_col2)
  if (!is.null(j2)) {
    results[[length(results) + 1]] <- j2
  }

  j3 <- do_join(.data, criteria3, id_col3)
  if (!is.null(j3)) {
    results[[length(results) + 1]] <- j3
  }

  j4 <- do_join(.data, criteria4, id_col4)
  if (!is.null(j4)) {
    results[[length(results) + 1]] <- j4
  }

  j5 <- do_join(.data, criteria5, id_col5)
  if (!is.null(j5)) {
    results[[length(results) + 1]] <- j5
  }

  wqp_criteria <- if (length(results) > 0) {
    dplyr::bind_rows(results)
  } else {
    .data
  }

  wqp_criteria <- TADA_CorrectColType(wqp_criteria)

  cols <- spsUtil::quiet(names(TADA_DefineCriteriaMethodology()[[1]])[
    -seq_len(8)
  ])
  existing_cols <- intersect(cols, names(wqp_criteria))
  
  return(wqp_criteria)
}

# checks for mismatching combinations between .data, refs, and criteria table
TADA_Analysis_Validate_Ref2 <- function(
  .data,
  criteria,
  AUMLRef = NULL,
  AU_UsesRef = NULL
) {
  if (!is.null(AUMLRef) || !is.null(AU_UsesRef)) {
    upperize <- function(df) {
      cols <- intersect(
        names(df),
        c(
          "TADA.ComparableDataIdentifier",
          "TADA.CharacteristicName",
          "TADA.ResultSampleFractionText",
          "TADA.MethodSpeciationName",
          "ATTAINS.UseName",
          "ATTAINS.WaterType",
          "ATTAINS.ParameterName"
        )
      )

      for (nm in cols) {
        df[[nm]] <- toupper(as.character(df[[nm]]))
      }
      df
    }

    wrap_vals <- function(x) {
      vals <- unique(stats::na.omit(trimws(as.character(x))))
      if (!length(vals)) {
        return("")
      }
      paste0("\n\n  ", paste(vals, collapse = "\n  "))
    }

    .data <- upperize(.data)
    criteria <- upperize(criteria)
    if (!is.null(AUMLRef)) {
      AUMLRef <- upperize(AUMLRef)
    }
    if (!is.null(AU_UsesRef)) {
      AU_UsesRef <- upperize(AU_UsesRef)
    }

    cmp_vals <- function(
      x,
      y,
      cols,
      value_col,
      direction = c("x_not_in_y", "y_not_in_x")
    ) {
      direction <- match.arg(direction)
      cols <- intersect(cols, intersect(names(x), names(y)))
      if (!length(cols)) {
        return(NULL)
      }

      if (direction == "x_not_in_y") {
        out <- dplyr::anti_join(
          dplyr::distinct(dplyr::select(x, dplyr::all_of(cols))),
          dplyr::distinct(dplyr::select(y, dplyr::all_of(cols))),
          by = cols
        )
      } else {
        out <- dplyr::anti_join(
          dplyr::distinct(dplyr::select(y, dplyr::all_of(cols))),
          dplyr::distinct(dplyr::select(x, dplyr::all_of(cols))),
          by = cols
        )
      }

      vals <- unique(stats::na.omit(trimws(as.character(out[[value_col]]))))
      if (!length(vals)) {
        return(NULL)
      }
      vals
    }

    # AU_UsesRef checks
    if (!is.null(AU_UsesRef)) {
      vals1 <- cmp_vals(
        criteria,
        AU_UsesRef,
        c("TADA.CharacteristicName", "ATTAINS.UseName"),
        "ATTAINS.UseName",
        direction = "x_not_in_y"
      )

      vals2 <- cmp_vals(
        criteria,
        AU_UsesRef,
        c("TADA.CharacteristicName", "ATTAINS.UseName"),
        "ATTAINS.UseName",
        direction = "y_not_in_x"
      )

      if (!is.null(vals1) || !is.null(vals2)) {
        msg <- character()
        if (!is.null(vals1)) {
          msg <- c(
            msg,
            paste0(
              "1: Your final criteria table output contains values not found in your AU_UsesRef for these ATTAINS.UseName(s):",
              "\n\n  ",
              paste(vals1, collapse = "\n  ")
            )
          )
        }
        if (!is.null(vals2)) {
          msg <- c(
            msg,
            paste0(
              "2: Your AU_UsesRef contains values not found in criteria for these ATTAINS.UseName(s):",
              "\n\n  ",
              paste(vals2, collapse = "\n  ")
            )
          )
        }
        warning(paste(msg, collapse = "\n\n"), call. = FALSE)
      }
    }

    # AUMLRef checks
    if (!is.null(AUMLRef)) {
      vals1 <- cmp_vals(
        criteria,
        AUMLRef,
        c("ATTAINS.WaterType"),
        "ATTAINS.WaterType",
        direction = "x_not_in_y"
      )

      vals2 <- cmp_vals(
        criteria,
        AUMLRef,
        c("ATTAINS.WaterType"),
        "ATTAINS.WaterType",
        direction = "y_not_in_x"
      )

      if (!is.null(vals1) || !is.null(vals2)) {
        msg <- character()
        if (!is.null(vals1)) {
          msg <- c(
            msg,
            paste0(
              "1: Your final criteria table output contains values not found in your AUMLRef for these ATTAINS.WaterType(s):",
              "\n\n  ",
              paste(vals1, collapse = "\n  ")
            )
          )
        }
        if (!is.null(vals2)) {
          msg <- c(
            msg,
            paste0(
              "2: Your AUMLRef contains values not found in criteria for these ATTAINS.WaterType(s):",
              "\n\n  ",
              paste(vals2, collapse = "\n  ")
            )
          )
        }
        warning(paste(msg, collapse = "\n\n"), call. = FALSE)
      }
    }

    spatial_cols <- c(
      "ATTAINS.WaterType",
      "SaltFresh",
      "UniqueSpatialCriteria",
      "DepthCategory"
    )
    spatial_cols <- intersect(spatial_cols, names(.data))

    if (length(spatial_cols) > 0) {
      df_combo <- TADA_CorrectColType(
        .data |> dplyr::select(dplyr::all_of(spatial_cols)) |> dplyr::distinct()
      )

      crit_combo <- TADA_CorrectColType(
        criteria |>
          dplyr::filter(
            TADA.CharacteristicName %in% .data$TADA.CharacteristicName
          ) |>
          dplyr::select(dplyr::all_of(spatial_cols)) |>
          dplyr::distinct()
      )

      missing_combos <- dplyr::anti_join(
        crit_combo,
        df_combo,
        by = spatial_cols
      )

      if (nrow(missing_combos) > 0) {
        warning(
          paste0(
            "These spatial combinations exist in criteria but not in your WQP .data for your TADA.CharacteristicName(s):\n",
            "Please ensure these entries are correct or these values cannot be joined due to a mismatch.\n",
            paste(capture.output(print(missing_combos)), collapse = "\n")
          ),
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}
