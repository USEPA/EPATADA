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
#' The function also identifies rows where `ResultMeasureValue` contains the text
#' "BPQL", "BDL", or "ND". These rows are treated as censored, the
#' `ResultMeasureValue` is set to `NA`, and the row is categorized using the
#' censoring reference logic. Detection limit value/unit are not copied into the
#' result value/unit fields.
#'
#' If `TADA.MeasureQualifierCode.Flag` is not already present in the input, the
#' function calls `TADA_FlagMeasureQualifierCode(clean = FALSE)` internally to
#' help identify censored results via measure qualifier codes. Note that the
#' internal flag column may be removed before returning the final result; if you
#' need to retain qualifier flags for auditing, run
#' `TADA_FlagMeasureQualifierCode()` separately prior to calling this function.
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
#' - Added column `TADA.CensoredData.Flag` describing censored data category, and
#' - Possible updates to `ResultMeasureValue` and
#'   `TADA.ResultMeasureValueDataTypes.Flag` when censored-text values are found.
#'
#' @details
#' This function uses TADA reference tables from
#' `TADA_GetDetCondRef()` (detection condition domain) and
#' `TADA_GetDetLimitRef()` (detection limit type domain). If values found in the
#' input dataframe are not present in the reference tables, the function flags
#' those records as `"Detection condition or detection limit is not documented
#' in TADA reference tables."` and prints an informational message. Such records
#' are excluded from downstream censored data handling methods.
#'
#' When `ResultMeasureValue` is a special text indicating censored data
#' (`"BPQL"`, `"BDL"`, `"ND"`), the function treats the row as censored and sets
#' `ResultMeasureValue` to `NA`. Detection limit value and unit are not copied to
#' the result fields.
#'
#' NA and blank character values are normalized before processing.
#'
#' @seealso
#' - `TADA_SimpleCensoredMethods` for simple censor handling
#' - `TADA_FlagMeasureQualifierCode` for measure qualifier flagging
#' - `TADA_GetDetCondRef` and `TADA_GetDetLimitRef` for domain reference tables
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
#' table(dat_flagged$TADA.CensoredData.Flag, useNA = "ifany")
#'
#' # Review Non-Detects
#' nd <- subset(dat_flagged, TADA.CensoredData.Flag == "Non-Detect")
#' head(nd[, c(
#'   "ResultIdentifier",
#'   "ResultDetectionConditionText",
#'   "DetectionQuantitationLimitTypeName",
#'   "ResultMeasureValue",
#'   "TADA.ResultMeasureValueDataTypes.Flag"
#' )])
#'
#' # Review conflicts
#' conf <- subset(dat_flagged, TADA.CensoredData.Flag == "Conflict between Condition and Limit")
#' head(conf[, c(
#'   "ResultIdentifier",
#'   "ResultDetectionConditionText",
#'   "DetectionQuantitationLimitTypeName",
#'   "ResultMeasureValue",
#'   "TADA.ResultMeasureValueDataTypes.Flag"
#' )])
#'
#' # Example 2: Ensure measure qualifier flags are present (optional)
#' # This can improve censored result identification via user-supplied codes.
#' dat_mq <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT, clean = FALSE)
#' dat_flagged2 <- TADA_IDCensoredData(dat_mq)
#' table(dat_flagged2$TADA.CensoredData.Flag, useNA = "ifany")
#' }
#'
TADA_IDCensoredData <- function(.data) {
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

  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)
  }

  # Normalize character fields
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
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA_character_
    }
    .data[[nm]] <- x
  }

  # Add qualifier flags if needed
  if (!"TADA.MeasureQualifierCode.Flag" %in% names(.data)) {
    data_mq_flag <- TADA_FlagMeasureQualifierCode(.data, clean = FALSE)
  } else {
    data_mq_flag <- .data
  }

  # Default missing-result flag
  data_mq_flag <- data_mq_flag |>
    dplyr::mutate(
      TADA.ResultMeasureValueDataTypes.Flag = ifelse(
        is.na(TADA.ResultMeasureValueDataTypes.Flag) & is.na(TADA.ResultMeasureValue),
        "NA - Not Available",
        TADA.ResultMeasureValueDataTypes.Flag
      )
    )

  # Censor text tokens
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
  nd_texts <- unique(toupper(trimws(as.character(nd_texts))))
  nd_texts <- nd_texts[nd_texts != ""]

  rv_chr <- toupper(trimws(as.character(data_mq_flag$ResultMeasureValue)))
  idx_censor_text <- !is.na(rv_chr) & rv_chr %in% nd_texts

  # Set censor-text result values to NA
  if (any(idx_censor_text, na.rm = TRUE)) {
    data_mq_flag$ResultMeasureValue[idx_censor_text] <- NA_character_
    data_mq_flag$TADA.ResultMeasureValueDataTypes.Flag[idx_censor_text] <-
      "Result Value/Unit Cannot Be Estimated From Detection Limit"
  }

  # Identify censored rows from qualifier flags or censor text
  idx_cens <- idx_censor_text |
    data_mq_flag$TADA.MeasureQualifierCode.Flag %in% c("Non-Detect", "Over-Detect")

  cens <- data_mq_flag[idx_cens, , drop = FALSE]
  not_cens <- data_mq_flag[!idx_cens, , drop = FALSE]

  if (nrow(not_cens) > 0) {
    not_cens$TADA.CensoredData.Flag <- "Uncensored"
  } else {
    not_cens <- data_mq_flag[0, , drop = FALSE]
    not_cens$TADA.CensoredData.Flag <- character(0)
  }

  if (nrow(cens) > 0) {
    # Reference tables
    cond.ref <- TADA_GetDetCondRef() |>
      dplyr::rename(ResultDetectionConditionText = Name) |>
      dplyr::select(ResultDetectionConditionText, TADA.Detection_Type)

    lim.ref <- TADA_GetDetLimitRef() |>
      dplyr::rename(DetectionQuantitationLimitTypeName = Name) |>
      dplyr::select(DetectionQuantitationLimitTypeName, TADA.Limit_Type)

    cens <- dplyr::left_join(cens, cond.ref, by = "ResultDetectionConditionText")
    cens <- dplyr::left_join(cens, lim.ref, by = "DetectionQuantitationLimitTypeName")

    # Track missing / undocumented detection condition and limit values
    cens$TADA.CensoredData.Flag <- NA_character_

    # missing condition and missing limit
    idx_missing_both <- is.na(cens$ResultDetectionConditionText) &
      is.na(cens$DetectionQuantitationLimitTypeName)
    cens$TADA.CensoredData.Flag[idx_missing_both] <-
      "Detection condition or detection limit is not documented in TADA reference tables."

    # missing condition only
    idx_missing_cond <- is.na(cens$ResultDetectionConditionText) &
      !is.na(cens$DetectionQuantitationLimitTypeName)
    cens$TADA.CensoredData.Flag[idx_missing_cond] <-
      "Detection condition is missing and required for censored data ID."

    # missing/ref-mismatch condition or limit not in reference tables
    idx_undoc <- (
      (!is.na(cens$ResultDetectionConditionText) & is.na(cens$TADA.Detection_Type)) |
        (!is.na(cens$DetectionQuantitationLimitTypeName) & is.na(cens$TADA.Limit_Type))
    )
    cens$TADA.CensoredData.Flag[idx_undoc] <-
      "Detection condition or detection limit is not documented in TADA reference tables."

    # valid matching types
    idx_nd <- cens$TADA.Detection_Type == "Non-Detect" &
      cens$TADA.Limit_Type == "Non-Detect"
    cens$TADA.CensoredData.Flag[idx_nd] <- "Non-Detect"

    idx_od <- cens$TADA.Detection_Type == "Over-Detect" &
      cens$TADA.Limit_Type == "Over-Detect"
    cens$TADA.CensoredData.Flag[idx_od] <- "Over-Detect"

    idx_other <- cens$TADA.Detection_Type == "Other" &
      cens$TADA.Limit_Type == "Other"
    cens$TADA.CensoredData.Flag[idx_other] <- "Other Condition/Limit Populated"

    # conflicts
    idx_conf <- cens$TADA.Detection_Type %in% c("Non-Detect", "Over-Detect", "Other") &
      cens$TADA.Limit_Type %in% c("Non-Detect", "Over-Detect", "Other") &
      cens$TADA.Detection_Type != cens$TADA.Limit_Type
    cens$TADA.CensoredData.Flag[idx_conf] <- "Conflict between Condition and Limit"

    # If conflict, null out result value/unit and mark cannot estimate
    if (any(idx_conf, na.rm = TRUE)) {
      cens$TADA.ResultMeasureValue[idx_conf] <- NA
      cens$TADA.ResultMeasure.MeasureUnitCode[idx_conf] <- NA
      cens$TADA.ResultMeasureValueDataTypes.Flag[idx_conf] <-
        "Result Value/Unit Cannot Be Estimated From Detection Limit"
    }

    # If censored text was present, keep result value NA
    if (any(idx_censor_text[idx_cens], na.rm = TRUE)) {
      cens$ResultMeasureValue[idx_censor_text[idx_cens]] <- NA
    }

    # Any remaining NA flags that are truly uncategorized
    cens$TADA.CensoredData.Flag[is.na(cens$TADA.CensoredData.Flag)] <-
      "Detection condition or detection limit is not documented in TADA reference tables."

    # Messages
    if (any(cens$TADA.CensoredData.Flag == "Detection condition is missing and required for censored data ID.", na.rm = TRUE)) {
      missing_detcond <- sum(cens$TADA.CensoredData.Flag == "Detection condition is missing and required for censored data ID.", na.rm = TRUE)
      message(paste0(
        "TADA_IDCensoredData: There are ",
        missing_detcond,
        " results in your dataframe that are missing ResultDetectionConditionText. TADA requires BOTH ResultDetectionConditionText and DetectionQuantitationLimitTypeName fields to be populated in order to categorize censored data."
      ))
    }

    conds <- unique(cens$ResultDetectionConditionText[!is.na(cens$ResultDetectionConditionText)])
    if (length(conds) > 0 && any(!conds %in% cond.ref$ResultDetectionConditionText)) {
      missing_conds <- paste(conds[!conds %in% cond.ref$ResultDetectionConditionText], collapse = ", ")
      message(paste0(
        "TADA_IDCensoredData: ResultDetectionConditionText column in dataframe contains value(s) ",
        missing_conds,
        " which is/are not represented in the ResultDetectionConditionText WQX domain table. These data records are placed under the TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in censored data handling methods. Please contact TADA administrators to resolve."
      ))
    }

    limits <- unique(cens$DetectionQuantitationLimitTypeName[!is.na(cens$DetectionQuantitationLimitTypeName)])
    if (length(limits) > 0 && any(!limits %in% lim.ref$DetectionQuantitationLimitTypeName)) {
      missing_lims <- paste(limits[!limits %in% lim.ref$DetectionQuantitationLimitTypeName], collapse = ", ")
      message(paste0(
        "TADA_IDCensoredData: DetectionQuantitationLimitTypeName column in dataframe contains value(s) ",
        missing_lims,
        " which is/are not represented in the DetectionQuantitationLimitTypeName WQX domain table. These data records are placed under the TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in censored data handling methods. Please contact TADA administrators to resolve."
      ))
    }

    cens <- cens |>
      dplyr::select(-TADA.Detection_Type, -TADA.Limit_Type, -TADA.MeasureQualifierCode.Flag)

    cens.check <- plyr::rbind.fill(cens, not_cens)
  } else {
    cens.check <- not_cens
    message("TADA_IDCensoredData: No censored data detected in your dataframe. Returning input dataframe with new column TADA.CensoredData.Flag set to Uncensored")
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
#'   , c(
#'     "ResultIdentifier", "TADA.ResultMeasureValue",
#'     "TADA.ResultMeasure.MeasureUnitCode", "TADA.CensoredMethod"
#'   )
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
#' dat_unc <- subset(Data_Nutrients_UT, FALSE) # empty subset for illustration
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

  # Safely check if everything is Uncensored
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


      if (any(is.na(nd$TADA.DetectionQuantitationLimitMeasure.MeasureValue))) {
        message("Some Non-Detect rows are missing detection limit values.")
      }

      suppressWarnings(
        nd$TADA.ResultMeasureValue <- as.numeric(nd$TADA.DetectionQuantitationLimitMeasure.MeasureValue)
      )
      nd$TADA.ResultMeasure.MeasureUnitCode <- nd$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode

      if (nd_method == "multiplier") {
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd_multiplier
        nd$TADA.CensoredMethod <- paste0(
          "Detection Limit Value Multiplied by ",
          nd_multiplier
        )
      }

      if (nd_method == "randombelowlimit") {
        nd$multiplier <- stats::runif(dim(nd)[1], 0, 1)
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd$multiplier
        nd$TADA.CensoredMethod <- paste0(
          "Random Value Between 0 and Detection Limit Using this Multiplier: ",
          round(nd$multiplier, digits = 3)
        )
        nd <- nd |> dplyr::select(-multiplier)
      }

      if (nd_method == "as-is") {
        nd$TADA.CensoredMethod <- "Detection Limit Value Unchanged"
      }

      nd$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
    }

    # OD handling
    if (dim(od)[1] > 0) {

      if (any(is.na(od$TADA.DetectionQuantitationLimitMeasure.MeasureValue))) {
        message("Some Over-Detect rows are missing detection limit values.")
      }

      suppressWarnings(
        od$TADA.ResultMeasureValue <- as.numeric(od$TADA.DetectionQuantitationLimitMeasure.MeasureValue)
      )
      od$TADA.ResultMeasure.MeasureUnitCode <- od$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode

      if (od_method == "multiplier") {
        od$TADA.ResultMeasureValue <- od$TADA.ResultMeasureValue * od_multiplier
        od$TADA.CensoredMethod <- paste0(
          "Detection Limit Value Multiplied by ",
          od_multiplier
        )
      }

      if (od_method == "as-is") {
        od$TADA.CensoredMethod <- "Detection Limit Value Unchanged"
      }

      od$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
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
