#' Join WQP data to criteria and spatial MLSummaryRef
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
#' # create the MLSummaryRef
#' params <- TADA_ParametersForAnalysis(
#'   Data_MT_MissoulaCounty, org_id = "MTDEQ", auto_assign = "Org")
#'
#' uses <- TADA_UsesForAnalysis(Data_MT_MissoulaCounty,
#'  org_id = "MTDEQ", paramRef = params, auto_assign = TRUE)
#'
#' mlsummary <- TADA_MLSummary(
#'   Data_MT_MissoulaCounty,
#'   org_id = "MTDEQ",
#'   AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
#'   AU_UsesRef = Data_MT_AU_UsesRef_Water,
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
  criteria_out <- TADA_DefineCriteriaMethodology(
    .data = .data,
    org_id = unique(criteria$ATTAINS.OrganizationIdentifier),
    criteriaMethods = criteria,
    displayUniqueId = TRUE
  )

  criteria <- upperize(criteria_out[[1]])

  if (!is.null(MLSummaryRef) && is.data.frame(MLSummaryRef)) {
    MLSummaryRef <- upperize(MLSummaryRef)
  }

  # ------------------------------------------------------------
  # Join MLSummaryRef first (if provided)
  # ------------------------------------------------------------
  if (!is.null(MLSummaryRef) && nrow(MLSummaryRef) > 0) {
    compare_keys <- intersect(
      c(
        "TADA.ComparableDataIdentifier",
        "ATTAINS.ParameterName",
        "ATTAINS.UseName",
        "ATTAINS.AssessmentUnitIdentifier",
        "ATTAINS.WaterType",
        "MonitoringLocationIdentifier",
        "SaltFresh",
        "UniqueSpatialCriteria",
        "DepthCategory"
      ),
      intersect(names(MLSummaryRef), names(.data))
    )
      
      .data <- dplyr::left_join(
        .data,
        MLSummaryRef,
        by = compare_keys,
        relationship = "many-to-many"
      )
    } else {
      warning(
        "MLSummaryRef could not be joined because required columns are missing.",
        call. = FALSE
      )
    }
  
  # ------------------------------------------------------------
  # Criteria join logic
  # ------------------------------------------------------------

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

  if (isTRUE(byChar)) {
    criteria <- criteria |>
      dplyr::mutate(
        TADA.ComparableDataIdentifier = NA_character_,
        TADA.ResultSampleFractionText = NA_character_,
        TADA.MethodSpeciationName = NA_character_
      ) |>
      dplyr::distinct()
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

  # ------------------------------------------------------------
  # Identify all mismatching criteria table rows that could not be matched to .data
  # ------------------------------------------------------------
  
  do_anti_join <- function(crit, df, keys) {
    if (is.null(crit) || is.null(df)) return(NULL)
    if (!is.data.frame(crit) || !is.data.frame(df)) return(NULL)
    
    crit <- TADA_CorrectColType(crit)
    df <- TADA_CorrectColType(df)
    
    if (is.null(crit) || is.null(df)) return(NULL)
    if (nrow(crit) == 0 || nrow(df) == 0) return(NULL)
    if (length(keys) == 0) return(NULL)
    if (!all(keys %in% names(crit)) || !all(keys %in% names(df))) return(NULL)
    
    dplyr::anti_join(crit, df, by = keys)
  }
  
  summarize_missing_causes <- function(unmatched_df, ref_df, join_cols) {
    if (is.null(unmatched_df) || !is.data.frame(unmatched_df) || nrow(unmatched_df) == 0) {
      return(NULL)
    }
    if (is.null(ref_df) || !is.data.frame(ref_df) || nrow(ref_df) == 0) {
      return(NULL)
    }
    
    join_cols <- intersect(join_cols, intersect(names(unmatched_df), names(ref_df)))
    if (!length(join_cols)) return(NULL)
    
    main_col <- join_cols[1]
    other_cols <- setdiff(join_cols, main_col)
    
    main_vals <- unique(unmatched_df[[main_col]])
    main_vals <- main_vals[!is.na(main_vals)]
    if (!length(main_vals)) return(NULL)
    
    msgs <- lapply(main_vals, function(main_val) {
      row_match <- unmatched_df[unmatched_df[[main_col]] == main_val, , drop = FALSE]
      
      cause_msgs <- lapply(other_cols, function(col) {
        vals <- unique(row_match[[col]])
        vals <- vals[!is.na(vals)]
        
        if (!length(vals)) return(NULL)
        
        ref_vals <- unique(ref_df[[col]])
        ref_vals <- ref_vals[!is.na(ref_vals)]
        
        bad_vals <- setdiff(vals, ref_vals)
        
        if (!length(bad_vals)) return(NULL)
        
        paste0(
          "\n",
          paste0("  * ", bad_vals, " not found in column: '", col, "'", collapse = "\n")
        )
      })
      
      cause_msgs <- unlist(cause_msgs)
      if (!length(cause_msgs)) return(NULL)
      
      paste0(
        main_val, " for ",
        paste(cause_msgs, collapse = "")
      )
    })
    
    msgs <- Filter(Negate(is.null), msgs)
    
    if (!length(msgs)) return(NULL)
    
    paste0(
      "Row(s) for these TADA.CharacteristicName from your criteria table input or MLSummaryRef could not be matched to your WQP data due to a mismatch. Please correct these values found within each defined column in your criteria table or MLSummaryRef if you would like to perform analysis for them:\n\n",
      paste0("- ", msgs, collapse = "\n")
    )
  }
  
  # Run anti-joins and preserve the join columns used for each set
  unmatched_sets <- list(
    list(df = do_anti_join(criteria1, .data, id_col1), keys = id_col1, name = "criteria1"),
    list(df = do_anti_join(criteria2, .data, id_col2), keys = id_col2, name = "criteria2"),
    list(df = do_anti_join(criteria3, .data, id_col3), keys = id_col3, name = "criteria3"),
    list(df = do_anti_join(criteria4, .data, id_col4), keys = id_col4, name = "criteria4"),
    list(df = do_anti_join(criteria5, .data, id_col5), keys = id_col5, name = "criteria5")
  )
  
  # Remove NULL results
  unmatched_sets <- Filter(function(x) !is.null(x$df), unmatched_sets)
  
  # Print warnings for each unmatched set
  if (length(unmatched_sets) > 0) {
    mismatch_msgs <- lapply(unmatched_sets, function(x) {
      summarize_missing_causes(x$df, .data, x$keys)
    })
    
    mismatch_msgs <- Filter(Negate(is.null), mismatch_msgs)
    
    if (length(mismatch_msgs) > 0) {
      message(
          paste0("- ", unlist(mismatch_msgs), collapse = "\n")
      )
    }
  }

  cols <- spsUtil::quiet(names(TADA_DefineCriteriaMethodology()[[1]])[
    -seq_len(8)
  ])
  existing_cols <- intersect(cols, names(wqp_criteria))

  return(wqp_criteria)
}

#' Validate Reference Tables Against WQP Data and Criteria
#'
#' Checks for mismatching combinations between a WQP data frame, a criteria
#' table, and optional spatial reference tables. When reference tables are
#' supplied, the function compares key identifying fields and issues warnings
#' if values present in one table are not found in another.
#'
#' This function is primarily used as a pre-check before joining criteria and
#' reference tables to WQP data for analysis.
#'
#' @param .data A data frame containing WQP data. Must include
#'   `TADA.CharacteristicName` and, if applicable, spatial columns such as
#'   `ATTAINS.WaterType`, `SaltFresh`, `UniqueSpatialCriteria`, and/or
#'   `DepthCategory`.
#' @param criteria A data frame containing the final criteria table. Must include
#'   `TADA.CharacteristicName` and any other fields needed for matching and
#'   validation.
#' @param AUMLRef Optional. A reference table for assessment unit–level water
#'   type mappings. If provided, the function checks for mismatches in
#'   `ATTAINS.WaterType`.
#' @param AU_UsesRef Optional. A reference table for assessment unit use
#'   mappings. If provided, the function checks for mismatches in
#'   `ATTAINS.UseName`.
#'
#' @return Invisibly returns `NULL`. The function is called for its side effect
#'   of issuing warnings when mismatches are detected.
#'
#' @details
#' The function performs the following checks:
#' \enumerate{
#'   \item Compares `criteria` and `AU_UsesRef` on
#'   `TADA.CharacteristicName` and `ATTAINS.UseName` when `AU_UsesRef` is
#'   provided.
#'   \item Compares `criteria` and `AUMLRef` on `ATTAINS.WaterType` when
#'   `AUMLRef` is provided.
#'   \item Checks whether spatial combinations present in `criteria` also exist
#'   in `.data` for the overlapping characteristic names.
#' }
#'
#' Character columns used in comparison are converted to uppercase and trimmed
#' before matching to reduce false mismatches due to case differences or extra
#' whitespace.
#'
#' @note This function does not modify the input objects. It only validates
#' them and generates warnings when inconsistencies are found.
#'
#' @examples
#' \dontrun{
#' # load example data.frame
#' utils::data("Data_MT_MissoulaCounty", package = "EPATADA")
#' MT_data <- Data_MT_MissoulaCounty
#'
#' # load example criteria table from community hub
#' criteria_MT <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")
#'
#' TADA_Analysis_Validate_Ref(
#'  Data_MT_MissoulaCounty,
#'  criteria = criteria_MT,
#'  AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
#'  AU_UsesRef = Data_MT_AU_UsesRef_Water)
#' }
#'
#' @export
TADA_Analysis_Validate_Ref <- function(
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
              "Your final criteria table output contains values not found in your AU_UsesRef for these ATTAINS.UseName(s), analysis cannot be done for these rows without defining them in your AU_UsesRef reference table:",
              "\n\n  ",
              paste(vals1, collapse = "\n  ")
            )
          )
        }
        if (!is.null(vals2)) {
          msg <- c(
            msg,
            paste0(
              "Your AU_UsesRef contains values not found in criteria for these ATTAINS.UseName(s), please ensure you have defined all criteria relevant for analysis:",
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
              "Your final criteria table contains values not found in your AUMLRef for these ATTAINS.WaterType(s), analysis cannot be done for these rows without defining them in your AUMLRef reference table:",
              "\n\n  ",
              paste(vals1, collapse = "\n  ")
            )
          )
        }
        if (!is.null(vals2)) {
          msg <- c(
            msg,
            paste0(
              "Your AUMLRef contains values not found in criteria for these ATTAINS.WaterType(s), please ensure you have defined all criteria relevant for analysis:",
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
            "These spatial combinations exist in your criteria table, but not in your WQP .data for your TADA.CharacteristicName(s):\n",
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
