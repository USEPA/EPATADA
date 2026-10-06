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
  # Warn if spatial columns are present in criteria table but MLSummaryRef is missing
  # ------------------------------------------------------------
  spatial_cols <- c(
    "ATTAINS.WaterType",
    "SaltFresh",
    "UniqueSpatialCriteria",
    "DepthCategory"
  )
  
  spatial_in_criteria <- intersect(spatial_cols, names(criteria))
  spatial_filled <- spatial_in_criteria[
    vapply(spatial_in_criteria, function(nm) {
      x <- criteria[[nm]]
      if (is.factor(x)) x <- as.character(x)
      any(!is.na(x) & nzchar(trimws(as.character(x))))
    }, logical(1))
  ]
  
  if (is.null(MLSummaryRef) && length(spatial_filled) > 0) {
    warning(
      paste0(
        "No MLSummaryRef was provided, but spatial columns contain values in the criteria table: ",
        paste(spatial_filled, collapse = ", "),
        ". Cannot differentiate which monitoring location sites belong to any of these spatial columns. ",
        "Please create the MLSummaryRef to define the sites that are applicable to these spatial columns."
      ),
      call. = FALSE
    )
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
        "DepthCategory",
        "LongitudeMeasure",
        "LatitudeMeasure"
      ),
      intersect(names(MLSummaryRef), names(.data))
    )

    if (length(compare_keys) == 0) {
      warning(
        "MLSummaryRef could not be joined because required columns are missing.",
        call. = FALSE
      )
    } else {
      .data <- dplyr::left_join(
        .data,
        MLSummaryRef,
        by = compare_keys,
        relationship = "many-to-many"
      )
    }
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
    if (is.null(crit) || is.null(df)) {
      return(NULL)
    }
    if (!is.data.frame(crit) || !is.data.frame(df)) {
      return(NULL)
    }

    crit <- TADA_CorrectColType(crit)
    df <- TADA_CorrectColType(df)

    if (is.null(crit) || is.null(df)) {
      return(NULL)
    }
    if (nrow(crit) == 0 || nrow(df) == 0) {
      return(NULL)
    }
    if (length(keys) == 0) {
      return(NULL)
    }
    if (!all(keys %in% names(crit)) || !all(keys %in% names(df))) {
      return(NULL)
    }

    dplyr::anti_join(crit, df, by = keys)
  }

  summarize_missing_causes <- function(unmatched_df, ref_df, join_cols) {
    if (
      is.null(unmatched_df) ||
        !is.data.frame(unmatched_df) ||
        nrow(unmatched_df) == 0
    ) {
      return(NULL)
    }
    if (is.null(ref_df) || !is.data.frame(ref_df) || nrow(ref_df) == 0) {
      return(NULL)
    }

    join_cols <- intersect(
      join_cols,
      intersect(names(unmatched_df), names(ref_df))
    )
    if (!length(join_cols)) {
      return(NULL)
    }

    main_col <- join_cols[1]
    other_cols <- setdiff(join_cols, main_col)

    main_vals <- unique(unmatched_df[[main_col]])
    main_vals <- main_vals[!is.na(main_vals)]
    if (!length(main_vals)) {
      return(NULL)
    }

    msgs <- lapply(main_vals, function(main_val) {
      row_match <- unmatched_df[
        unmatched_df[[main_col]] == main_val,
        ,
        drop = FALSE
      ]

      cause_msgs <- lapply(other_cols, function(col) {
        vals <- unique(row_match[[col]])
        vals <- vals[!is.na(vals)]

        if (!length(vals)) {
          return(NULL)
        }

        ref_vals <- unique(ref_df[[col]])
        ref_vals <- ref_vals[!is.na(ref_vals)]

        bad_vals <- setdiff(vals, ref_vals)

        if (!length(bad_vals)) {
          return(NULL)
        }

        paste0(
          "\n",
          paste0(
            "  * ",
            bad_vals,
            " not found in column: '",
            col,
            "'",
            collapse = "\n"
          )
        )
      })

      cause_msgs <- unlist(cause_msgs)
      if (!length(cause_msgs)) {
        return(NULL)
      }

      paste0(main_val, " for ", paste(cause_msgs, collapse = ""))
    })

    msgs <- Filter(Negate(is.null), msgs)

    if (!length(msgs)) {
      return(NULL)
    }

    paste0(
      "Row(s) for these TADA.CharacteristicName or TADA.ComparableDataIdentifier from your criteria table input could not be matched ",
      "to your WQP data or MLSummaryRef due to a mismatch. To help ensure the tables can be joined, ",
      "please correct the values in each defined column by adding any missing values to your MLSummaryRef ",
      "or by making sure values are spelled correctly and match exactly between your criteria table and MLSummaryRef. ",
      "Only matching values can be analyzed:\n\n",
      paste0("- ", msgs, collapse = "\n")
    )
  }

  # Run anti-joins and preserve the join columns used for each set
  unmatched_sets <- list(
    list(
      df = do_anti_join(criteria1, .data, id_col1),
      keys = id_col1,
      name = "criteria1"
    ),
    list(
      df = do_anti_join(criteria2, .data, id_col2),
      keys = id_col2,
      name = "criteria2"
    ),
    list(
      df = do_anti_join(criteria3, .data, id_col3),
      keys = id_col3,
      name = "criteria3"
    ),
    list(
      df = do_anti_join(criteria4, .data, id_col4),
      keys = id_col4,
      name = "criteria4"
    ),
    list(
      df = do_anti_join(criteria5, .data, id_col5),
      keys = id_col5,
      name = "criteria5"
    )
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
      message(paste0("- ", unlist(mismatch_msgs), collapse = "\n"))
    }
  }

  cols <- spsUtil::quiet(names(TADA_DefineCriteriaMethodology()[[1]])[
    -seq_len(8)
  ])
  existing_cols <- intersect(cols, names(wqp_criteria))

  return(wqp_criteria)
}
