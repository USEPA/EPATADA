#' Join WQP data to criteria
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
#' @param .data A TADA data frame.
#' @param criteria data.frame of TADA compatible criteria table for any
#' of either TADA.ComparableDataIdentifier and a combination of TADA.CharacteristicName,
#' TADA.ResultSampleFractionText, and TADA.MethodSpeciationName
#' @param AU_UsesRef An optional data frame input. If provided, the ATTAINS.UseName
#' will be filtered by the ATTAINS.UseName found in this data frame. It will also
#' assign the uses to each assessment unit defined in this table if an AUMLRef
#' is also provided. This data frame must contain the following column names
#' which can be generated from the output of TADA_AssignUsesToAU:
#' ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName,
#' and ATTAINS.WaterType.
#' @param AUMLRef An optional data frame input containing a completed crosswalk
#' of monitoring location sites associated with an assessment unit. If provided,
#' each Monitoring location site will get assigned to an ATTAINS.AssessmentUnitIdentifier
#' to allow users to analyze by either assessment units or by monitoring location.
#' This data frame must contain the following column names which can be generated
#' from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
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
#' MT_data_criteria3 <- TADA_Analysis_Join_WQP_Criteria(
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
  AUMLRef = NULL,
  AU_UsesRef = NULL,
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

  # ---- join AUMLRef into .data if needed ----
  if (has_AUMLRef) {
    if ("ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {
      message(
        "ATTAINS.AssessmentUnitIdentifier already exists in .data; no join of AUMLRef will be done."
      )
    } else {
      if (
        all(
          c("TADA.MonitoringLocationIdentifier", "OrganizationIdentifier") %in%
            names(.data)
        ) &&
          all(
            c(
              "TADA.MonitoringLocationIdentifier",
              "OrganizationIdentifier"
            ) %in%
              names(AUMLRef)
          )
      ) {
        .data <- .data |>
          # NOTE: CAN REMOVE THIS MUTATE CHUNK ONCE THEY ARE INCORPORATED INTO AUMLREF
          dplyr::mutate(
            ATTAINS.SaltFresh = ifelse(!"ATTAINS.SaltFresh" %in% names(.data), NA_character_, ATTAINS.SaltFresh),
            ATTAINS.UniqueSpatialCriteria = ifelse(!"ATTAINS.UniqueSpatialCriteria" %in% names(.data), NA_character_, ATTAINS.UniqueSpatialCriteria),
            ATTAINS.DepthCategory = ifelse(!"ATTAINS.DepthCategory" %in% names(.data), NA_character_, ATTAINS.DepthCategory)
          ) |>
          dplyr::left_join(
          AUMLRef,
          by = c("TADA.MonitoringLocationIdentifier", "OrganizationIdentifier"),
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

  # ---- join AU_UsesRef into .data if needed ----
  if (has_AUUsesRef && has_AUMLRef) {
    if ("ATTAINS.UseName" %in% names(.data)) {
      message(
        "ATTAINS.UseName already exists in .data; no join of AU_UsesRef will be done."
      )
    } else {
      if (
        all(
          c(
            "ATTAINS.OrganizationIdentifier",
            "ATTAINS.AssessmentUnitIdentifier",
            "ATTAINS.WaterType"
          ) %in%
            names(.data)
        ) &&
          all(
            c(
              "ATTAINS.OrganizationIdentifier",
              "ATTAINS.AssessmentUnitIdentifier",
              "ATTAINS.WaterType"
            ) %in%
              names(AU_UsesRef)
          )
      ) {
        .data <- dplyr::left_join(
          .data,
          AU_UsesRef,
          by = c(
            "ATTAINS.OrganizationIdentifier",
            "ATTAINS.AssessmentUnitIdentifier",
            "ATTAINS.WaterType"
          ),
          relationship = "many-to-many"
        )
      } else {
        warning(
          "AU_UsesRef could not be joined because required columns are missing.",
          call. = FALSE
        )
      }
    }
  }

  if (has_AUUsesRef) {
    allowed_uses <- unique(stats::na.omit(AU_UsesRef$`ATTAINS.UseName`))

    if (length(allowed_uses) == 0) {
      warning(
        "No ATTAINS.UseName matches were found. No criteria table can be joined, please ensure your uses match those found in your criteria table."
      )
    }
    criteria <- dplyr::filter(criteria, ATTAINS.UseName %in% allowed_uses)
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
    if (has_AUUsesRef) {
      unique(c(base_keys, "ATTAINS.UseName"))
    }
    
    if (has_AUUsesRef && has_AUMLRef) {
      unique(c(base_keys, "ATTAINS.WaterType", "ATTAINS.OrganizationIdentifier", "ATTAINS.UseName")) # add SaltFresh, UniqueSpatialCriteria and DepthCategory in the future
    } 
    
    if (has_AUMLRef && isFALSE(has_AUUsesRef)) {
      unique(c(base_keys, "ATTAINS.WaterType", "ATTAINS.OrganizationIdentifier"))
    }
    
    if (isFALSE(has_AUMLRef) && isFALSE(has_AUUsesRef)) {
      base_keys
    }
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
    
    # only keep spatial cols that actually exist in crit
    spatial_cols <- intersect(spatial_cols, names(crit))
    
    # if none exist, do a normal join
    if (length(spatial_cols) == 0) {
      return(dplyr::left_join(df, crit, by = keys, relationship = "many-to-many"))
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
        dplyr::select(-.spatial_pattern)
      
      dplyr::left_join(df, x2, by = by_use, relationship = "many-to-many")
    })
    
    dplyr::bind_rows(joins) |>
      dplyr::distinct()
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
  
  wqp_criteria <- TADA_CorrectColType(wqp_criteria) |>
    dplyr::mutate(SaltFresh = as.character(SaltFresh))
  
  # if TRUE, only displays returning matches (those filled in from criteria table) that will be used for analysis
  cols <- names(TADA_DefineCriteriaMethodology()[[1]])[-seq_len(8)]
  existing_cols <- intersect(cols, names(wqp_criteria))

  if (clean) {
    wqp_criteria <- wqp_criteria |>
      dplyr::filter(!dplyr::if_all(existing_cols, is.na))
  }

  return(wqp_criteria)
}
