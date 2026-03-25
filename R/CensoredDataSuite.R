#' Prepare and Flag Censored Data
#'
#' Identify and categorize censored data records using detection condition text,
#' detection limit type, and (if available) user-supplied measure qualifier codes.
#' Records are flagged as:
#' - "Non-Detect"
#' - "Over-Detect"
#' - "Other Condition/Limit Populated"
#' - "Conflict between Condition and Limit"
#' - "Detection condition is missing and required for censored data ID."
#' - "Detection condition or detection limit is not documented in TADA reference tables."
#' Uncensored records are flagged as "Uncensored".
#'
#' The function also copies detection limit values and units to
#' TADA.ResultMeasureValue and TADA.ResultMeasure.MeasureUnitCode when:
#' - ResultMeasureValue is the text "BPQL", "BDL", or "ND" and detection limit
#'   value/unit are present, or
#' - TADA.ResultMeasureValueDataTypes.Flag indicates "NA - Not Available" and
#'   detection limit value/unit are present (copy occurs only if the TADA
#'   result value/unit are actually NA).
#'
#' When a conflict between detection condition and detection limit type is found,
#' the function sets TADA.ResultMeasureValue and TADA.ResultMeasure.MeasureUnitCode to NA and
#' updates TADA.ResultMeasureValueDataTypes.Flag to
#' "Result Value/Unit Cannot Be Estimated From Detection Limit".
#'
#' If TADA.MeasureQualifierCode.Flag is not already present in the input,
#' the function calls TADA_FlagMeasureQualifierCode(clean = FALSE) internally
#' to help identify censored results via measure qualifier codes. Note that
#' the internal flag column may be removed before returning the final result; if
#' you need to retain qualifier flags for auditing, run TADA_FlagMeasureQualifierCode
#' separately prior to calling this function.
#'
#' @param .data A TADA dataframe. It must include the following columns:
#' - ResultIdentifier
#' - ResultMeasureValue
#' - ResultDetectionConditionText
#' - DetectionQuantitationLimitTypeName
#' - TADA.ResultMeasureValueDataTypes.Flag
#' - TADA.ResultMeasureValue
#' - TADA.ResultMeasure.MeasureUnitCode
#' - TADA.DetectionQuantitationLimitMeasure.MeasureValue
#' - TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
#'
#' @return A TADA dataframe with:
#' - Added column TADA.CensoredData.Flag describing censored data category, and
#' - Possible updates to TADA.ResultMeasureValue,
#'   TADA.ResultMeasure.MeasureUnitCode, and
#'   TADA.ResultMeasureValueDataTypes.Flag, when detection limits are copied
#'   or conflicts are detected.
#'
#' @details
#' This function uses TADA reference tables from
#' TADA_GetDetCondRef() (detection condition domain) and
#' TADA_GetDetLimitRef() (detection limit type domain). If values found in the
#' input dataframe are not present in the reference tables, the function flags
#' those records as "Detection condition or detection limit is not documented
#' in TADA reference tables." and prints an informational message. Such records
#' are excluded from downstream censored data handling methods.
#'
#' When ResultMeasureValue is a special text indicating censored data
#' ("BPQL", "BDL", "ND"), and detection limit value/unit are present, the
#' detection limit value and unit are copied to TADA result value/unit and
#' TADA.ResultMeasureValueDataTypes.Flag is set to "Result Value/Unit Copied from Detection Limit".
#'
#' @seealso
#' - TADA_SimpleCensoredMethods for simple censor handling
#' - TADA_FlagMeasureQualifierCode for measure qualifier flagging
#' - TADA_GetDetCondRef and TADA_GetDetLimitRef for domain reference tables
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Typical workflow with package dataset
#' utils::data(Data_Nutrients_UT)
#'
#' # Flag censored data and inspect categories
#' dat_flagged <- TADA_IDCensoredData(Data_Nutrients_UT)
#'
#' # Count records per censored category
#' table(dat_flagged$TADA.CensoredData.Flag)
#'
#' # Review Non-Detects and confirm the detection limit value was copied
#' nd <- subset(dat_flagged, TADA.CensoredData.Flag == "Non-Detect")
#' head(nd[, c("ResultIdentifier",
#'   "ResultDetectionConditionText",
#'   "DetectionQuantitationLimitTypeName",
#'   "TADA.ResultMeasureValue",
#'   "TADA.ResultMeasure.MeasureUnitCode",
#'   "TADA.ResultMeasureValueDataTypes.Flag")])
#'
#' # Review conflicts (values/units set to NA)
#' conf <- subset(dat_flagged, TADA.CensoredData.Flag == "Conflict between Condition and Limit")
#' head(conf[, c("ResultIdentifier",
#'   "ResultDetectionConditionText",
#'   "DetectionQuantitationLimitTypeName",
#'   "TADA.ResultMeasureValue",
#'   "TADA.ResultMeasure.MeasureUnitCode",
#'   "TADA.ResultMeasureValueDataTypes.Flag")])
#'
#' # Example 2: Ensure measure qualifier flags are present (optional)
#' # This can improve censored result identification via user-supplied codes.
#' dat_mq <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT, clean = FALSE)
#' dat_flagged2 <- TADA_IDCensoredData(dat_mq)
#' table(dat_flagged2$TADA.CensoredData.Flag)
#' }
#' # Prepare and Flag Censored Data (NA/blank safe)
TADA_IDCensoredData <- function(.data) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "ResultIdentifier",
    "ResultMeasureValue",
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValueDataTypes.Flag",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
    "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)
  }

  # Normalize key character fields: trim and convert "" to NA
  char_cols <- c(
    "ResultMeasureValue",
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValueDataTypes.Flag",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode",
    "TADA.MeasureQualifierCode.Flag"
  )
  for (nm in intersect(char_cols, names(.data))) {
    x <- .data[[nm]]
    if (is.factor(x)) {
      x <- as.character(x)
    }
    if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA_character_
    }
    .data[[nm]] <- x
  }

  # Case-insensitive ND tokens
  rv_upper <- toupper(trimws(as.character(.data$ResultMeasureValue)))

  # Copy det limit value/unit to TADA result when result text is BPQL/BDL/ND and BOTH limit pieces present
  cond_text_nd <- (!is.na(rv_upper)) &
    (rv_upper %in% c("BPQL", "BDL", "ND")) &
    !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue) &
    !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
  idx <- which(cond_text_nd)
  if (length(idx)) {
    .data$TADA.ResultMeasureValue[
      idx
    ] <- .data$TADA.DetectionQuantitationLimitMeasure.MeasureValue[idx]
    .data$TADA.ResultMeasure.MeasureUnitCode[
      idx
    ] <- .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode[idx]
    .data$TADA.ResultMeasureValueDataTypes.Flag[
      idx
    ] <- "Result Value/Unit Copied from Detection Limit"
  }

  # If result is BPQL/BDL/ND but BOTH limit pieces missing, flag cannot estimate
  cond_text_nd_no_limit <- (!is.na(rv_upper)) &
    (rv_upper %in% c("BPQL", "BDL", "ND")) &
    is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue) &
    is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
  idx <- which(cond_text_nd_no_limit)
  if (length(idx)) {
    .data$TADA.ResultMeasureValueDataTypes.Flag[
      idx
    ] <- "Result Value/Unit Cannot Be Estimated From Detection Limit"
  }

  # Identify rows eligible for NA-flag copy
  ok_flag <- is.na(.data$TADA.ResultMeasureValueDataTypes.Flag) |
    .data$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available"

  # Copy value if missing and limit value present
  idx_val <- which(
    ok_flag &
      is.na(.data$TADA.ResultMeasureValue) &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue)
  )
  if (length(idx_val)) {
    .data$TADA.ResultMeasureValue[
      idx_val
    ] <- .data$TADA.DetectionQuantitationLimitMeasure.MeasureValue[idx_val]
  }

  # Copy unit if missing and limit unit present
  idx_unit <- which(
    ok_flag &
      is.na(.data$TADA.ResultMeasure.MeasureUnitCode) &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
  )
  if (length(idx_unit)) {
    .data$TADA.ResultMeasure.MeasureUnitCode[
      idx_unit
    ] <- .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode[idx_unit]
  }

  # Set flag to 'Copied' for any row where either value or unit was copied
  idx_any <- sort(unique(c(idx_val, idx_unit)))
  if (length(idx_any)) {
    .data$TADA.ResultMeasureValueDataTypes.Flag[
      idx_any
    ] <- "Result Value/Unit Copied from Detection Limit"
  }

  # If user has not previously run TADA_FlagMeasureQualifierCode, run it here
  if (!"TADA.MeasureQualifierCode.Flag" %in% names(.data)) {
    data_mq_flag <- TADA_FlagMeasureQualifierCode(.data, clean = FALSE)
  } else {
    data_mq_flag <- .data
  }

  # Update the TADA.ResultMeasureValueDataTypes.Flag if flag is NA and result value is NA
  data_mq_flag <- data_mq_flag |>
    dplyr::mutate(
      TADA.ResultMeasureValueDataTypes.Flag = ifelse(
        is.na(TADA.ResultMeasureValueDataTypes.Flag) &
          is.na(TADA.ResultMeasureValue),
        "NA - Not Available",
        TADA.ResultMeasureValueDataTypes.Flag
      )
    )

  ## Identify censored data using TADA.ResultMeasureValueDataTypes.Flag and TADA.MeasureQualifierCode.Flag
  cens_rm_flag <- data_mq_flag |>
    dplyr::filter(
      TADA.ResultMeasureValueDataTypes.Flag ==
        "Result Value/Unit Copied from Detection Limit"
    )

  cens_mq_flag <- data_mq_flag |>
    dplyr::filter(
      TADA.MeasureQualifierCode.Flag %in% c("Non-Detect", "Over-Detect")
    ) |>
    dplyr::filter(!ResultIdentifier %in% cens_rm_flag$ResultIdentifier)

  # Combine censored sources
  cens <- dplyr::bind_rows(cens_rm_flag, cens_mq_flag)

  # Non-censored
  not_cens <- data_mq_flag |>
    dplyr::filter(!ResultIdentifier %in% cens$ResultIdentifier)

  # Assign Uncensored flag
  if (nrow(not_cens) > 0) {
    not_cens$TADA.CensoredData.Flag <- "Uncensored"
  } else {
    not_cens <- data_mq_flag[0, ]
    not_cens$TADA.CensoredData.Flag <- character(0)
  }

  rm(cens_rm_flag, cens_mq_flag, data_mq_flag)

  if (nrow(cens) > 0) {
    ## Bring in det cond reference table
    cond.ref <- TADA_GetDetCondRef() |>
      dplyr::rename(ResultDetectionConditionText = Name) |>
      dplyr::select(ResultDetectionConditionText, TADA.Detection_Type)

    ## Join to censored data
    cens <- dplyr::left_join(
      cens,
      cond.ref,
      by = "ResultDetectionConditionText"
    )

    ## Flag censored data that does not have det cond populated (after blank->NA normalization)
    cens$TADA.Detection_Type <- ifelse(
      is.na(cens$ResultDetectionConditionText),
      "ResultDetectionConditionText missing",
      cens$TADA.Detection_Type
    )

    ## Fill in detection type when ResultMeasureValue indicates it is a nondetect
    # Build a robust set of ND tokens: fixed ["ND","BDL","BPQL"] plus domain codes
    nd_texts <- tryCatch(
      {
        c(
          "ND",
          "BDL",
          "BPQL",
          TADA_GetMeasureQualifierCodeRef() |>
            dplyr::filter(TADA.MeasureQualifierCode.Flag == "Non-Detect") |>
            dplyr::pull(Code)
        )
      },
      error = function(e) c("ND", "BDL", "BPQL")
    )

    # Normalize
    nd_texts <- unique(toupper(trimws(as.character(nd_texts))))
    nd_texts <- nd_texts[nd_texts != ""]

    # Compare using character, trim and uppercase for safety
    rv_chr <- if (is.factor(cens$ResultMeasureValue)) {
      as.character(cens$ResultMeasureValue)
    } else {
      cens$ResultMeasureValue
    }
    rv_chr <- toupper(trimws(as.character(rv_chr)))

    idx_nd <- !is.na(rv_chr) & rv_chr %in% nd_texts
    cens$TADA.Detection_Type[idx_nd] <- "Non-Detect"

    ## Missing detection condition message
    if (
      any(
        cens$TADA.Detection_Type[!is.na(cens$TADA.Detection_Type)] ==
          "ResultDetectionConditionText missing"
      )
    ) {
      missing_detcond <- sum(
        cens$TADA.Detection_Type == "ResultDetectionConditionText missing",
        na.rm = TRUE
      )
      message(paste0(
        "TADA_IDCensoredData: There are ",
        missing_detcond,
        " results in your dataframe that are missing ResultDetectionConditionText. TADA requires BOTH ResultDetectionConditionText and DetectionQuantitationLimitTypeName fields to be populated in order to categorize censored data."
      ))
    }

    ## Warn when result detection conditions not in ref table (exclude NA)
    conds <- unique(cens$ResultDetectionConditionText[
      !is.na(cens$ResultDetectionConditionText)
    ])
    if (
      length(conds) > 0 &&
        any(!conds %in% cond.ref$ResultDetectionConditionText)
    ) {
      missing_conds <- conds[!conds %in% cond.ref$ResultDetectionConditionText]
      missing_conds <- paste(missing_conds, collapse = ", ")
      message(paste0(
        "TADA_IDCensoredData: ResultDetectionConditionText column in dataframe contains value(s) ",
        missing_conds,
        " which is/are not represented in the ResultDetectionConditionText WQX domain table. These data records are placed under the TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in censored data handling methods. Please contact TADA administrators to resolve."
      ))
    }

    ## Bring in det limit type reference table
    limtype.ref <- TADA_GetDetLimitRef() |>
      dplyr::rename(DetectionQuantitationLimitTypeName = Name) |>
      dplyr::select(DetectionQuantitationLimitTypeName, TADA.Limit_Type)

    ## Join to censored data
    cens <- dplyr::left_join(
      cens,
      limtype.ref,
      by = "DetectionQuantitationLimitTypeName"
    )

    # Warn when some limit type names not in ref table (exclude NA)
    limits <- unique(cens$DetectionQuantitationLimitTypeName[
      !is.na(cens$DetectionQuantitationLimitTypeName)
    ])
    if (
      length(limits) > 0 &&
        any(!limits %in% limtype.ref$DetectionQuantitationLimitTypeName)
    ) {
      missing_lims <- limits[
        !limits %in% limtype.ref$DetectionQuantitationLimitTypeName
      ]
      missing_lims <- paste(missing_lims, collapse = ", ")
      message(paste0(
        "TADA_IDCensoredData: DetectionQuantitationLimitTypeName column in dataframe contains value(s) ",
        missing_lims,
        " which is/are not represented in the DetectionQuantitationLimitTypeName WQX domain table. These data records are placed under the TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in censored data handling methods. Please contact TADA administrators to resolve."
      ))
    }

    ## Create flag for condition and limit type combinations
    cens$TADA.CensoredData.Flag <- "Detection condition or detection limit is not documented in TADA reference tables."
    cens$TADA.CensoredData.Flag <- ifelse(
      cens$TADA.Detection_Type %in% c("ResultDetectionConditionText missing"),
      "Detection condition is missing and required for censored data ID.",
      cens$TADA.CensoredData.Flag
    )

    cens$TADA.CensoredData.Flag <- ifelse(
      cens$TADA.Detection_Type %in%
        c("Non-Detect") &
        cens$TADA.Limit_Type %in% c("Non-Detect"),
      "Non-Detect",
      cens$TADA.CensoredData.Flag
    )
    cens$TADA.CensoredData.Flag <- ifelse(
      cens$TADA.Detection_Type %in%
        c("Over-Detect") &
        cens$TADA.Limit_Type %in% c("Over-Detect"),
      "Over-Detect",
      cens$TADA.CensoredData.Flag
    )
    cens$TADA.CensoredData.Flag <- ifelse(
      cens$TADA.Detection_Type %in%
        c("Other") &
        cens$TADA.Limit_Type %in% c("Other"),
      "Other Condition/Limit Populated",
      cens$TADA.CensoredData.Flag
    )
    # Identify conflicts between detection condition and limit type
    cens$TADA.CensoredData.Flag <- ifelse(
      cens$TADA.Detection_Type %in%
        c("Non-Detect", "Over-Detect", "Other") &
        cens$TADA.Limit_Type %in% c("Non-Detect", "Over-Detect", "Other") &
        cens$TADA.Detection_Type != cens$TADA.Limit_Type,
      "Conflict between Condition and Limit",
      cens$TADA.CensoredData.Flag
    )

    ## Warn when conflicts & revert result value back to NA and update flags
    if (
      "Conflict between Condition and Limit" %in% cens$TADA.CensoredData.Flag
    ) {
      num <- sum(
        cens$TADA.CensoredData.Flag == "Conflict between Condition and Limit",
        na.rm = TRUE
      )
      message(paste0(
        "TADA_IDCensoredData: ",
        num,
        " records in supplied dataset have conflicting detection condition and detection limit type information. These records will not be included in detection limit handling calculations."
      ))

      idx_conf <- cens$TADA.CensoredData.Flag ==
        "Conflict between Condition and Limit"
      cens$TADA.ResultMeasureValueDataTypes.Flag[
        idx_conf
      ] <- "Result Value/Unit Cannot Be Estimated From Detection Limit"
      cens$TADA.ResultMeasureValue[idx_conf] <- NA
      cens$TADA.ResultMeasure.MeasureUnitCode[idx_conf] <- NA
    }

    if (
      "Detection condition or detection limit is not documented in TADA reference tables." %in%
        cens$TADA.CensoredData.Flag
    ) {
      num <- sum(
        cens$TADA.CensoredData.Flag ==
          "Detection condition or detection limit is not documented in TADA reference tables.",
        na.rm = TRUE
      )
      message(paste0(
        "TADA_IDCensoredData: ",
        num,
        " records in the supplied dataframe have detection conditions and/or limit types that are missing from TADA reference tables. These records will not be included in detection limit handling calculations."
      ))
    }

    cens <- cens |>
      dplyr::select(
        -TADA.Detection_Type,
        -TADA.Limit_Type,
        -TADA.MeasureQualifierCode.Flag
      )

    cens.check <- plyr::rbind.fill(cens, not_cens)
  } else {
    cens.check <- not_cens
    message(
      "TADA_IDCensoredData: No censored data detected in your dataframe. Returning input dataframe with new column TADA.CensoredData.Flag set to Uncensored"
    )
  }

  # Double check that detection values are not copied when there are conflicts
  idx_conflict <- cens.check$TADA.CensoredData.Flag ==
    "Conflict between Condition and Limit"
  if (any(idx_conflict, na.rm = TRUE)) {
    cens.check$TADA.ResultMeasureValueDataTypes.Flag[
      idx_conflict
    ] <- "Result Value/Unit Cannot Be Estimated From Detection Limit"
    cens.check$TADA.ResultMeasureValue[idx_conflict] <- NA
    cens.check$TADA.ResultMeasure.MeasureUnitCode[idx_conflict] <- NA
  }

  cens.check <- TADA_OrderCols(cens.check)
  return(cens.check)
}

#' Simple Tools for Censored Data Handling
#'
#' Apply simple, non-frequency-based methods to populate values for censored
#' results categorized as "Non-Detect" (lower limit) or "Over-Detect" (upper limit).
#' This function first ensures censored data are identified and categorized by
#' calling TADA_IDCensoredData (if needed), then fills result values using one of
#' the supported methods:
#' - For Non-Detects (lower limits):
#'   - "multiplier": value = nd_multiplier × lower detection limit (default)
#'   - "randombelowlimit": value = U(0, 1) × lower detection limit (a new random
#'     uniform value per row)
#'   - "as-is": retain lower detection limit value unchanged
#' - For Over-Detects (upper limits):
#'   - "multiplier": value = od_multiplier × upper detection limit
#'   - "as-is": retain upper detection limit value unchanged (default)
#'
#' Records flagged as:
#' - "Detection condition is missing and required for censored data ID."
#' - "Detection condition or detection limit is not documented in TADA reference tables."
#' are set to NA for TADA.ResultMeasureValue and TADA.ResultMeasure.MeasureUnitCode and
#' given TADA.ResultMeasureValueDataTypes.Flag =
#' "Result Value/Unit Cannot Be Estimated From Detection Limit".
#'
#' If the dataframe contains no censored results, the function returns the input
#' unchanged and prints an informational message.
#'
#' @param .data A TADA dataframe. If TADA.CensoredData.Flag is not present, the
#' function will call TADA_IDCensoredData to generate it (and may call
#' TADA_FlagMeasureQualifierCode inside that step).
#'
#' @param nd_method Character. Method for Non-Detect handling: "multiplier"
#' (default), "randombelowlimit", or "as-is".
#'
#' @param nd_multiplier Numeric scalar. Multiplier applied to the LOWER detection
#' limit when nd_method = "multiplier". Default is 0.5 (half the detection limit).
#'
#' @param od_method Character. Method for Over-Detect handling: "multiplier" or
#' "as-is" (default).
#'
#' @param od_multiplier Numeric scalar or "null". Multiplier applied to the UPPER
#' detection limit when od_method = "multiplier". When od_method = "as-is", this
#' parameter is ignored. If od_method = "multiplier", a numeric multiplier must
#' be supplied.
#'
#' @return A TADA dataframe with:
#' - Updated TADA.ResultMeasureValue for censored records according to the chosen method(s),
#' - Updated TADA.ResultMeasureValueDataTypes.Flag documenting that values were
#'   estimated from detection limits,
#' - Added TADA.CensoredMethod indicating the method used (e.g., "Detection Limit Value
#'   Multiplied by 0.5", "Random Value Between 0 and Detection Limit Using this Multiplier: 0.317"),
#' - For problematic records (missing detection condition or undocumented types),
#'   values/units are set to NA and the data type flag indicates they cannot be
#'   estimated from detection limits.
#'
#' @details
#' This function assumes detection limits have already been copied to
#' TADA.ResultMeasureValue and TADA.ResultMeasure.MeasureUnitCode for censored
#' records by TADA_IDCensoredData when feasible. It coerces TADA.ResultMeasureValue to
#' numeric before applying multipliers or randomness.
#'
#' Required columns in .data (either pre-existing or created by TADA_IDCensoredData):
#' - ResultIdentifier
#' - ResultMeasureValue
#' - ResultDetectionConditionText
#' - DetectionQuantitationLimitTypeName
#' - TADA.ResultMeasureValueDataTypes.Flag
#' - TADA.ResultMeasureValue
#' - TADA.ResultMeasure.MeasureUnitCode
#' - TADA.CensoredData.Flag
#'
#' @seealso
#' - TADA_IDCensoredData for censored data identification and flagging
#' - TADA_FlagMeasureQualifierCode for measure qualifier flagging that can aid in censor identification
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example dataframe:
#' utils::data(Data_Nutrients_UT)
#'
#' # Example 1: Fill Non-Detects with half the lower detection limit,
#' # keep Over-Detects as-is
#' dat_simple <- TADA_SimpleCensoredMethods(
#'   Data_Nutrients_UT,
#'   nd_method = "multiplier",
#'   nd_multiplier = 0.5,
#'   od_method = "as-is",
#'   od_multiplier = "null"
#' )
#' table(dat_simple$TADA.CensoredData.Flag)
#' head(subset(dat_simple, TADA.CensoredData.Flag == "Non-Detect")[
#'   , c("ResultIdentifier", "TADA.ResultMeasureValue",
#'     "TADA.ResultMeasure.MeasureUnitCode", "TADA.CensoredMethod")
#' ])
#'
#' # Example 2: Randomize Non-Detects below the detection limit, keep Over-Detects as-is
#' set.seed(42)
#' dat_rand <- TADA_SimpleCensoredMethods(
#'   Data_Nutrients_UT,
#'   nd_method = "randombelowlimit",
#'   nd_multiplier = "null",
#'   od_method = "as-is",
#'   od_multiplier = "null"
#' )
#' head(subset(dat_rand, TADA.CensoredData.Flag == "Non-Detect")[
#'   , c("ResultIdentifier", "TADA.ResultMeasureValue", "TADA.CensoredMethod")
#' ])
#'
#' # Example 3: Apply multipliers to both Non-Detects and Over-Detects
#' dat_both <- TADA_SimpleCensoredMethods(
#'   Data_Nutrients_UT,
#'   nd_method = "multiplier",
#'   nd_multiplier = 0.2,
#'   od_method = "multiplier",
#'   od_multiplier = 0.8
#' )
#' head(subset(dat_both, TADA.CensoredData.Flag %in% c("Non-Detect", "Over-Detect"))[
#'   , c("ResultIdentifier", "TADA.ResultMeasureValue", "TADA.CensoredMethod")
#' ])
#'
#' # Example 4: If no censored data are present, the function returns the input
#' dat_unc <- subset(Data_Nutrients_UT, FALSE)  # empty subset for illustration
#' res <- TADA_SimpleCensoredMethods(dat_unc)
#' # message: "Cannot apply simple censored methods to dataframe with no censored
#' # data results. Returning input dataframe."
#' }
TADA_SimpleCensoredMethods <- function(
  .data,
  nd_method = "multiplier",
  nd_multiplier = 0.5,
  od_method = "as-is",
  od_multiplier = "null"
) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "ResultIdentifier",
    "ResultMeasureValue",
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValueDataTypes.Flag",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)
  }

  # Validate method and multiplier arguments (scalar-safe)
  if (length(nd_method) != 1L) {
    stop("nd_method must be length 1.")
  }
  if (length(od_method) != 1L) {
    stop("od_method must be length 1.")
  }

  # ND multiplier checks
  if (identical(nd_method, "multiplier") && identical(nd_multiplier, "null")) {
    stop(
      "Please provide a multiplier for the lower detection limit handling method of 'multiplier'. Typically, the multiplier value is between 0 and 1."
    )
  }
  if (
    identical(nd_method, "multiplier") &&
      (!is.numeric(nd_multiplier) || length(nd_multiplier) != 1L)
  ) {
    stop("nd_multiplier must be a single numeric value.")
  }

  # OD multiplier checks
  if (identical(od_method, "multiplier") && identical(od_multiplier, "null")) {
    stop(
      "Please provide a multiplier for the upper detection limit handling method of 'multiplier'"
    )
  }
  if (
    identical(od_method, "multiplier") &&
      (!is.numeric(od_multiplier) || length(od_multiplier) != 1L)
  ) {
    stop("od_multiplier must be a single numeric value.")
  }

  # If user has not previously run TADA_IDCensoredData, run it here
  if (!"TADA.CensoredData.Flag" %in% names(.data)) {
    cens.data <- TADA_IDCensoredData(.data)
  } else {
    cens.data <- .data
  }

  # Safely check if everything is Uncensored (avoid NA short-circuit)
  if (!any(cens.data$TADA.CensoredData.Flag != "Uncensored", na.rm = TRUE)) {
    message(
      "Cannot apply simple censored methods to dataframe with no censored data results. Returning input dataframe."
    )
    .data <- cens.data
  } else {
    # split out over detects and non detects
    nd <- subset(cens.data, cens.data$TADA.CensoredData.Flag == "Non-Detect")
    od <- subset(cens.data, cens.data$TADA.CensoredData.Flag == "Over-Detect")
    no.ref <- subset(
      cens.data,
      cens.data$TADA.CensoredData.Flag ==
        "Detection condition is missing and required for censored data ID."
    )
    missing.ref <- subset(
      cens.data,
      cens.data$TADA.CensoredData.Flag ==
        "Detection condition or detection limit is not documented in TADA reference tables."
    )

    all_others <- subset(
      cens.data,
      !cens.data$ResultIdentifier %in%
        c(
          nd$ResultIdentifier,
          od$ResultIdentifier,
          no.ref$ResultIdentifier,
          missing.ref$ResultIdentifier
        )
    )

    # ND handling
    if (dim(nd)[1] > 0) {
      # ensure numeric before multiplication/randomization
      suppressWarnings(
        nd$TADA.ResultMeasureValue <- as.numeric(nd$TADA.ResultMeasureValue)
      )

      if (nd_method == "multiplier") {
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd_multiplier
        nd$TADA.CensoredMethod <- paste0(
          "Detection Limit Value Multiplied by ",
          nd_multiplier
        )
        nd$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
      if (nd_method == "randombelowlimit") {
        nd$multiplier <- stats::runif(dim(nd)[1], 0, 1)
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd$multiplier
        nd$TADA.CensoredMethod <- paste0(
          "Random Value Between 0 and Detection Limit Using this Multiplier: ",
          round(nd$multiplier, digits = 3)
        )
        nd <- nd |> dplyr::select(-multiplier)
        nd$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
      if (nd_method == "as-is") {
        nd$TADA.CensoredMethod <- "Detection Limit Value Unchanged"
        nd$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
    }

    # OD handling
    if (dim(od)[1] > 0) {
      # ensure numeric before multiplication
      suppressWarnings(
        od$TADA.ResultMeasureValue <- as.numeric(od$TADA.ResultMeasureValue)
      )

      if (od_method == "multiplier") {
        od$TADA.ResultMeasureValue <- od$TADA.ResultMeasureValue * od_multiplier
        od$TADA.CensoredMethod <- paste0(
          "Detection Limit Value Multiplied by ",
          od_multiplier
        )
        od$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
      if (od_method == "as-is") {
        od$TADA.CensoredMethod <- "Detection Limit Value Unchanged"
        od$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
    }

    # handling for results with missing detection conditions
    if (dim(no.ref)[1] > 0) {
      no.ref$TADA.ResultMeasureValue <- NA
      no.ref$TADA.ResultMeasure.MeasureUnitCode <- NA
      no.ref$TADA.CensoredMethod <- "Result set to NA due to Missing Detection Condition"
      no.ref$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Cannot Be Estimated From Detection Limit"
    }

    # handling for results with detection condition or limit not in TADA ref table
    if (dim(missing.ref)[1] > 0) {
      missing.ref$TADA.ResultMeasureValue <- NA
      missing.ref$TADA.ResultMeasure.MeasureUnitCode <- NA
      missing.ref$TADA.CensoredMethod <- "Result set to NA as Detection Condition or Limit is not in TADA Ref Table"
      missing.ref$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Cannot Be Estimated From Detection Limit"
    }

    .data <- plyr::rbind.fill(nd, od, all_others, no.ref, missing.ref) |>
      TADA_CreateComparableID()
  }

  .data <- TADA_OrderCols(.data)
  return(.data)
}
