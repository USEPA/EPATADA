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
#' NOTE: MLSummaryRef is in active development and joins the ref tables of AUMLRef,
#' AU_UsesRef, and usesParamRef of the spatial summary, parameters and uses for
#' analysis.
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
#' # join the table by best match, along with the AUMLRef
#' MT_data_criteria2 <- TADA_Analysis_Join_WQP_Criteria(
#'   MT_data,
#'   criteria_MT,
#'   AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk)
#'
#' # join the table by best match, with both the AUMLRef and AU_UsesRef
#' MT_data_criteria3 <- TADA_Analysis_Join_WQP_Criteria(
#'   MT_data,
#'   criteria_MT,
#'   AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
#'   AU_UsesRef = Data_MT_AU_UsesRef_Water)
#'
#' # only return rows that will be used for analysis
#' MT_data_criteria3_cleaned <- TADA_Analysis_Join_WQP_Criteria(
#'   MT_data,
#'   criteria_MT,
#'   AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
#'   AU_UsesRef = Data_MT_AU_UsesRef_Water,
#'   clean = TRUE)
#'
TADA_Analysis_Join_WQP_Criteria <- function(
  .data,
  criteria,
  byChar = FALSE,
  MLSummaryRef = NULL,
  clean = FALSE
) {
  stopifnot(is.data.frame(.data), is.data.frame(criteria))

  upper_keys <- c(
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.MonitoringLocationIdentifier",
    "ATTAINS.OrganizationIdentifier",
    "ATTAINS.UseName",
    "ATTAINS.AssessmentUnitIdentifier",
    "ATTAINS.WaterType"
  )

  upperize <- function(df) {
    for (nm in intersect(names(df), upper_keys)) {
      df[[nm]] <- toupper(as.character(df[[nm]]))
    }
    df
  }

  .data <- upperize(.data)

  criteria <- upperize(criteria)
  if (!is.null(AUMLRef) && is.data.frame(AUMLRef)) {
    AUMLRef <- upperize(AUMLRef)
  }
  if (!is.null(AU_UsesRef) && is.data.frame(AU_UsesRef)) {
    AU_UsesRef <- upperize(AU_UsesRef)
  }

  has_AUMLRef <- !is.null(AUMLRef) && nrow(AUMLRef) > 0
  has_AUUsesRef <- !is.null(AU_UsesRef) && nrow(AU_UsesRef) > 0

  # ---- join MLSummaryRef into .data if needed ----
  if (has_AUMLRef) {
    if ("ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {
      message(
        "ATTAINS.AssessmentUnitIdentifier already exists in .data; no join of AUMLRef will be done."
      )
    } else {
      required_cols <- c(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
        "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
        "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "LongitudeMeasure",
        "LatitudeMeasure", "IncludeOrExclude" , "UniqueSpatialCriteria"
      )

        .data <- .data |>
          dplyr::left_join(
            AUMLRef,
            by = required_cols,
            relationship = "many-to-many"
          )
      } else {
        warning(
          "AUMLRef could not be joined because required columns are missing.",
          call. = FALSE
        )
      }
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

  # Expected characteristics (only those present in criteria)
  expected_chars <- if ("TADA.CharacteristicName" %in% names(criteria)) {
    unique(stats::na.omit(criteria$TADA.CharacteristicName))
  } else {
    character(0)
  }

  if (isTRUE(byChar)) {
    keys <- unique(c("TADA.CharacteristicName"))
    drop_cols <- c(
      "TADA.ComparableDataIdentifier",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName"
    )

    crit_char <- criteria |>
      dplyr::select(-dplyr::any_of(drop_cols)) |>
      dplyr::filter(!is.na(.data$`TADA.CharacteristicName`))

    wqp_criteria <- dplyr::left_join(
      .data,
      crit_char,
      by = keys,
      relationship = "many-to-many"
    )

    return(wqp_criteria)
  }

  # Cross-pass: allow the same WQP row to match in multiple passes
  results <- list()

  # If ATTAINS.UseName is populated from the AU_UsesRef, use the criteria table to determine which ATTAINS.Parameter it is associated with
  join_keys <- function(base_keys) {
    spatial_extra <- c("SaltFresh", "UniqueSpatialCriteria", "DepthCategory")

    # if AU_UsesRef exist, join by ATTAINS.UseName too
    if (has_AUUsesRef) {
      base_keys <- unique(c(base_keys, "ATTAINS.UseName"))
    }

    # if AUMLRef exist, checks what spatial columns currently exists
    if (has_AUMLRef) {
      base_keys <- unique(c(
        base_keys,
        "ATTAINS.WaterType",
        "ATTAINS.OrganizationIdentifier"
      ))

      # add spatial extras only if they exist in AUMLRef and are not all NA
      spatial_extra_present <- spatial_extra[spatial_extra %in% names(AUMLRef)]
      spatial_extra_present <- spatial_extra_present[
        !vapply(
          AUMLRef[spatial_extra_present],
          function(x) all(is.na(x)),
          logical(1)
        )
      ]

      base_keys <- unique(c(base_keys, spatial_extra_present))
    }

    if (isFALSE(has_AUMLRef) && isFALSE(has_AUUsesRef)) {
      base_keys
    }

    return(base_keys)
  }

  do_join <- function(df, crit, keys) {
    spatial_cols <- c(
      "ATTAINS.WaterType",
      "SaltFresh",
      "UniqueSpatialCriteria",
      "DepthCategory"
    )

    if (nrow(crit) == 0) {
      return(NULL)
    }
    if (!all(keys %in% names(df))) {
      return(NULL)
    }
    if (!all(keys %in% names(crit))) {
      return(NULL)
    }

    if (has_AUMLRef) {
      # if none exist, do a normal join
      if (length(spatial_cols) == 0) {
        return(dplyr::left_join(
          df,
          crit,
          by = keys,
          relationship = "many-to-many"
        ))
      }

      # create a pattern describing which spatial columns are NA
      crit2 <- crit |>
        dplyr::mutate(
          .spatial_pattern = apply(
            dplyr::across(dplyr::all_of(spatial_cols), is.na),
            1,
            function(x) paste(names(x)[x], collapse = "|")
          )
        )

      # split by pattern of NA spatial columns
      crit_split <- split(crit2, crit2$.spatial_pattern)

      joins <- lapply(crit_split, function(x) {
        na_spatial <- spatial_cols[is.na(x[1, spatial_cols])]

        by_use <- setdiff(keys, na_spatial)

        # only keep rows with non-NA values in the spatial columns that are used
        # (otherwise join by NA is not meaningful)
        x2 <- x |>
          dplyr::select(-.spatial_pattern) |>
          dplyr::mutate(dplyr::across(
            dplyr::any_of(c("SeasonStartDate", "SeasonEndDate")),
            as.character
          ))

        out <- dplyr::left_join(
          df,
          x2,
          by = by_use,
          relationship = "many-to-many"
        ) |>
          dplyr::mutate(dplyr::across(
            dplyr::any_of(c("SeasonStartDate", "SeasonEndDate")),
            as.character
          ))

        out
      })

      return(dplyr::bind_rows(joins) |> dplyr::distinct())
    }

    if (isFALSE(has_AUMLRef)) {
      for (nm in c("SeasonStartDate", "SeasonEndDate")) {
        if (nm %in% names(df)) {
          df[[nm]] <- as.character(df[[nm]])
        }
        if (nm %in% names(crit)) {
          crit[[nm]] <- as.character(crit[[nm]])
        }
      }

      return(dplyr::left_join(
        df,
        crit,
        by = keys,
        relationship = "many-to-many"
      ))
    }
    ######## NOTE: ADD BACK TO CODE ONCE ADDITIONAL SPATIAL COLUMNS ARE INCLUDED IN AUMLREF
    # spatial_cols <- c(
    #   "ATTAINS.WaterType",
    #   "SaltFresh",
    #   "UniqueSpatialCriteria",
    #   "DepthCategory"
    # )
    #
    # if (nrow(crit) == 0) {
    #   return(NULL)
    # }
    # if (!all(keys %in% names(df))) {
    #   return(NULL)
    # }
    # if (!all(keys %in% names(crit))) {
    #   return(NULL)
    # }
    #
    # # only keep spatial cols that actually exist in crit (should be all)
    # spatial_cols <- intersect(spatial_cols, names(crit))
    #
    # if (length(spatial_cols) > 0) {
    #   df_combo <- TADA_CorrectColType(
    #     dplyr::distinct(df[, spatial_cols, drop = FALSE])
    #   )
    #
    #   crit_combo <- TADA_CorrectColType(
    #     crit |>
    #       dplyr::filter(TADA.CharacteristicName %in% df$TADA.CharacteristicName) |>
    #       dplyr::select(dplyr::all_of(spatial_cols)) |>
    #       dplyr::distinct()
    #   )
    #
    #   missing_combos <- dplyr::anti_join(
    #     crit_combo,
    #     df_combo,
    #     by = spatial_cols
    #   )
    #
    #   if (nrow(missing_combos) > 0) {
    #     warning(
    #       paste0(
    #         "These spatial combinations exist in criteria but not in your WQP .data for your TADA.CharacteristicName(s):\n",
    #         "Please ensure these entries are correct or these values cannot be joined due to a mismatch.\n",
    #         paste(capture.output(print(missing_combos)), collapse = "\n")
    #       )
    #     )
    #   }
    #}
    # # if none exist, do a normal join
    # if (length(spatial_cols) == 0) {
    #   return(dplyr::left_join(
    #     df,
    #     crit,
    #     by = keys,
    #     relationship = "many-to-many"
    #   ))
    # }
    #
    # # create a pattern describing which spatial columns are NA
    # crit2 <- crit |>
    #   dplyr::mutate(
    #     .spatial_pattern = apply(
    #       dplyr::across(dplyr::all_of(spatial_cols), is.na),
    #       1,
    #       function(x) paste(names(x)[x], collapse = "|")
    #     )
    #   )
    #
    # # split by pattern of NA spatial columns
    # crit_split <- split(crit2, crit2$.spatial_pattern)
    #
    # joins <- lapply(crit_split, function(x) {
    #   na_spatial <- spatial_cols[is.na(x[1, spatial_cols])]
    #
    #   by_use <- setdiff(keys, na_spatial)
    #
    #   # only keep rows with non-NA values in the spatial columns that are used
    #   # (otherwise join by NA is not meaningful)
    #   x2 <- x |> dplyr::select(-.spatial_pattern) |> TADA_CorrectColType()
    #
    #   out <- dplyr::left_join(df, x2, by = by_use, relationship = "many-to-many") |>
    #     TADA_CorrectColType()
    #
    #   out
    #})
    #
    #dplyr::bind_rows(joins) |> dplyr::distinct()
  }
  # Pass 1: ID (+ optional keys)
  j1 <- do_join(.data, criteria1, join_keys(id_col1))
  if (!is.null(j1)) {
    results[[length(results) + 1]] <- j1
  }

  # Pass 2: Char + Fraction + Speciation (+ optional keys)
  j2 <- do_join(.data, criteria2, join_keys(id_col2))
  if (!is.null(j2)) {
    results[[length(results) + 1]] <- j2
  }

  # Pass 3: Char + Fraction (+ optional keys)
  j3 <- do_join(.data, criteria3, join_keys(id_col3))
  if (!is.null(j3)) {
    results[[length(results) + 1]] <- j3
  }

  # Pass 4: Char + Speciation (+ optional keys)
  j4 <- do_join(.data, criteria4, join_keys(id_col4))
  if (!is.null(j4)) {
    results[[length(results) + 1]] <- j4
  }

  # Pass 5: Char only (+ optional keys)
  j5 <- do_join(.data, criteria5, join_keys(id_col5))
  if (!is.null(j5)) {
    results[[length(results) + 1]] <- j5
  }

  wqp_criteria <- if (length(results) > 0) {
    dplyr::bind_rows(results)
  } else {
    .data
  }

  wqp_criteria <- TADA_CorrectColType(wqp_criteria)

  # if TRUE, only displays returning matches (those filled in from criteria table) that will be used for analysis
  cols <- spsUtil::quiet(names(TADA_DefineCriteriaMethodology()[[1]])[
    -seq_len(8)
  ])
  existing_cols <- intersect(cols, names(wqp_criteria))

  if (clean) {
    wqp_criteria <- wqp_criteria |>
      dplyr::filter(!dplyr::if_all(existing_cols, is.na))
  }

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
