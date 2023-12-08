#' Prepare Censored Data
#'
#' This function identifies censored data records and characterizes them as
#' "Non-Detects" or "Over-Detects" based on the ResultDetectionConditionText and
#' DetectionQuantitationLimitTypeName. It also identifies records populated with
#' limits and conditions that cannot be grouped as over- or non-detects as
#' "Other Condition/Limit Populated", and flags records where there is a
#' conflict between ResultDetectionConditionText and
#' DetectionQuantitationLimitTypeName as "Conflict between Condition and Limit".
#' A "Conflict between Condition and Limit" may occur if: A) the data submitter supplied
#' multiple detection limits and the wrong one is accidentally being associated
#' with the detection condition, or B) the data submitter accidentally choose
#' the wrong detection limit or detection condition to associate with the result.
#' Detection limit results missing ResultDetectionConditionText are flagged as
#' "Detection condition is missing and required for censored data ID." unless
#' the ResultMeasureValue field is populated with "ND" (indicating non-detect).
#' In rare situations, new detection limit types are added to WQX domain tables
#' (and thus WQP data) that have not yet been classified as over- or
#' non-detects. When these appear in a dataset, they are categorized as
#' "Detection condition or detection limit is not documented in TADA reference
#' tables." In these situations, users should contact TADA administrators to
#' update the package accordingly. This function is used by default in
#' TADA_AutoClean, but can be used on its own.
#'
#' @param .data A TADA dataframe
#'
#' @return A TADA dataframe with additional column named TADA.CensoredData.Flag, which indicates if there are disagreements in ResultDetectionCondition and DetectionQuantitationLimitTypeName.
#'
#' @export
#'

TADA_IDCensoredData <- function(.data) {
  # check .data has all of the required columns
  expected_cols <- c(
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValueDataTypes.Flag"
  )
  TADA_CheckColumns(.data, expected_cols)

  ## First step: identify censored data
  cens <- .data %>% dplyr::filter(TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit")
  not_cens <- .data %>% dplyr::filter(!ResultIdentifier %in% cens$ResultIdentifier)
  not_cens$TADA.CensoredData.Flag <- "Uncensored"


  if (dim(cens)[1] > 0) {
    ## Bring in det cond reference table
    cond.ref <- TADA_GetDetCondRef() %>%
      dplyr::rename(ResultDetectionConditionText = Name) %>%
      dplyr::select(ResultDetectionConditionText, TADA.Detection_Type)

    ## Join to censored data
    cens <- dplyr::left_join(cens, cond.ref, by = "ResultDetectionConditionText")

    ## Flag censored data that does not havedet cond populated
    cens$TADA.Detection_Type <- ifelse(is.na(cens$ResultDetectionConditionText), "ResultDetectionConditionText missing", cens$TADA.Detection_Type)

    ## Fill in detection type when result measure value = "ND"
    cens$TADA.Detection_Type <- ifelse(cens$ResultMeasureValue %in% c("ND"), "Non-Detect", cens$TADA.Detection_Type)

    ## Let user know when detection condition text is missing from one or more results
    # NOTE that at this point, TADA.Detection_Type may be NA if there are detection conditions in dataset that are not present in domain table
    if (any(cens$TADA.Detection_Type[!is.na(cens$TADA.Detection_Type)] == "ResultDetectionConditionText missing")) {
      missing_detcond <- length(cens$TADA.Detection_Type[cens$TADA.Detection_Type == "ResultDetectionConditionText missing"])
      print(paste0("TADA_IDCensoredData: There are ", missing_detcond, " results in your dataset that are missing ResultDetectionConditionText. Unless the ResultMeasureValue = 'ND' (indicating non-detect), TADA requires BOTH ResultDetectionConditionText and DetectionQuantitationLimitTypeName fields to be populated in order to categorize censored data. Please contact the TADA Admins to resolve."))
    }

    ## Let user know when one or more result detection conditions are not in the ref table
    conds <- unique(cens$ResultDetectionConditionText[!is.na(cens$ResultDetectionConditionText)])
    if (any(!conds %in% cond.ref$ResultDetectionConditionText)) {
      missing_conds <- conds[!conds %in% cond.ref$ResultDetectionConditionText]
      missing_conds <- paste(missing_conds, collapse = ", ")
      print(paste0("TADA_IDCensoredData: ResultDetectionConditionText column in dataset contains value(s) ", missing_conds, " which is/are not represented in the ResultDetectionConditionText WQX domain table. These data records are placed under the TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in censored data handling methods. Please contact TADA administrators to resolve."))
    }

    ## Bring in det limit type reference table
    limtype.ref <- TADA_GetDetLimitRef() %>%
      dplyr::rename(DetectionQuantitationLimitTypeName = Name) %>%
      dplyr::select(DetectionQuantitationLimitTypeName, TADA.Limit_Type)

    ## Join to censored data
    cens <- dplyr::left_join(cens, limtype.ref, by = "DetectionQuantitationLimitTypeName")

    limits <- unique(cens$DetectionQuantitationLimitTypeName)
    if (any(!limits %in% limtype.ref$DetectionQuantitationLimitTypeName)) {
      missing_lims <- limits[!limits %in% limtype.ref$DetectionQuantitationLimitTypeName]
      missing_lims <- paste(missing_lims, collapse = ", ")
      print(paste0("TADA_IDCensoredData: DetectionQuantitationLimitTypeName column dataset contains value(s) ", missing_lims, " which is/are not represented in the DetectionQuantitationLimitTypeName WQX domain table. These data records are placed under the TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in censored data handling methods. Please contact TADA administrators to resolve."))
    }

    ## Create flag for condition and limit type combinations
    cens$TADA.CensoredData.Flag <- "Detection condition or detection limit is not documented in TADA reference tables."
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("ResultDetectionConditionText missing"), "Detection condition is missing and required for censored data ID.", cens$TADA.CensoredData.Flag)
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Non-Detect") & cens$TADA.Limit_Type %in% c("Non-Detect"), "Non-Detect", cens$TADA.CensoredData.Flag)
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Over-Detect") & cens$TADA.Limit_Type %in% c("Over-Detect"), "Over-Detect", cens$TADA.CensoredData.Flag)
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Other") & cens$TADA.Limit_Type %in% c("Other"), "Other Condition/Limit Populated", cens$TADA.CensoredData.Flag)
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Non-Detect", "Over-Detect", "Other") & cens$TADA.Limit_Type %in% c("Non-Detect", "Over-Detect", "Other") & !cens$TADA.Detection_Type == cens$TADA.Limit_Type, "Conflict between Condition and Limit", cens$TADA.CensoredData.Flag)
    

    ## warn when some limit metadata may be problematic
    if ("Conflict between Condition and Limit" %in% cens$TADA.CensoredData.Flag) {
      num <- length(cens$TADA.CensoredData.Flag[cens$TADA.CensoredData.Flag == "Conflict between Condition and Limit"])
      print(paste0("TADA_IDCensoredData: ", num, " records in supplied dataset have conflicting detection condition and detection limit type information. These records will not be included in detection limit handling calculations."))
    }

    if ("Detection condition or detection limit is not documented in TADA reference tables." %in% cens$TADA.CensoredData.Flag) {
      num <- length(cens$TADA.CensoredData.Flag[cens$TADA.CensoredData.Flag == "Detection condition or detection limit is not documented in TADA reference tables."])
      print(paste0("TADA_IDCensoredData: ", num, " records in supplied dataset have detection conditions and/or limit types that are missing from TADA reference tables . These records will not be included in detection limit handling calculations."))
    }

    cens <- cens %>% dplyr::select(-TADA.Detection_Type, -TADA.Limit_Type)

    cens.check <- plyr::rbind.fill(cens, not_cens)
  } else {
    cens.check <- not_cens
    print("TADA_IDCensoredData: No censored data detected in your dataset. Returning input dataframe with new column TADA.CensoredData.Flag set to Uncensored")
  }

  return(cens.check)
}


#' Simple Tools for Censored Data Handling
#'
#' This function determines if detection limit type and detection condition are parsimonious
#' before applying simple tools for non-detect and over-detect data handling, including filling
#' in the values as-is, X times the detection limit, or a random number between 0
#' and the LOWER detection limit. These methods do NOT depend upon censored data frequency
#' in the dataset.
#'
#' This function applies a separate method for pathogen (list CharacteristicNames?) results to 
#' prepare for calculating geometric means. For pathogen results which are non-detects, coerced 
#' to NA or equal to or less than 0, a small value (default = 1) is substituted.
#'
#' This function runs TADA_IDCensoredData within it which add the column
#' TADA.CensoredData.Flag. Enter ?TADA_IDCensoredData into the console for more
#' information.
#'
#' @param .data A TADA dataframe
#' @param nd_method A text string indicating the type of method used to populate a non-detect (lower limit) data value. Can be set to "multiplier" (default),"randombelowlimit", or "as-is".
#' @param nd_multiplier A number to be multiplied to the LOWER detection limit for each entry to obtain the censored data value. Must be supplied if nd_method = "multiplier". Defaults to 0.5, or half the detection limit.
#' @param od_method A text string indicating the type of method used to populate an over-detect (upper limit) data value. Can be set to "multiplier" or "as-is" (default).
#' @param od_multiplier A number to be multiplied to the UPPER detection limit for each entry to obtain the censored data value. Must be supplied if od_method = "multiplier". Defaults to 0.5, or half the detection limit.
#' @param pathogen Boolean. Determines whether pathogen (list of CharacteristicNames?) non-detects should be set to a specified small value for use in calculation geometric means. If pathogen = TRUE (default), the substitution is applied. If pathogen = FALSE, no substitution is applied.
#' @param pathogen_value A number to be substituted when pathogen results are non-detects. Must be supplied if pathogen = TRUE. Defaults to 1.
#'
#' @return A TADA dataframe with additional columns named TADA.CensoredData.Flag, which indicates if there are disagreements in ResultDetectionCondition and DetectionQuantitationLimitTypeName, and TADA.CensoredMethod, which documents the method used to fill censored data values.
#'
#'
#' @export
#'
#' @examples
#' #' # Load example dataset:
#' data(Data_Nutrients_UT)
#' # Check for agreement between detection condition and detection limit type,
#' # and in instances where the measurement is non-detect, set the result value
#' # to half of the detection limit value. For over-detect measurements, retain
#' # the detection limit value as the result value as-is.
#' Data_Nutrients_UT_CensoredFlag <- TADA_SimpleCensoredMethods(Data_Nutrients_UT, nd_method = "multiplier", nd_multiplier = 0.5, od_method = "as-is", od_multiplier = "null")
#'
#' # Check for agreement between detection condition and detection limit type, and in instances where the measurement is non-detect, set the result value to a random value between 0 and the detection limit value. For over-detect measurements, retain the detection limit value as the result value as-is.
#' Data_Nutrients_UT_CensoredFlag <- TADA_SimpleCensoredMethods(Data_Nutrients_UT, nd_method = "randombelowlimit", nd_multiplier = "null", od_method = "as-is", od_multiplier = "null")
#'
TADA_SimpleCensoredMethods <- function(.data, nd_method = "multiplier", nd_multiplier = 0.5, od_method = "as-is", od_multiplier = "null", pathogen = TRUE, pathogen_value = 1) {
  # check .data has all of the required columns
  expected_cols <- c(
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValueDataTypes.Flag"
  )
  TADA_CheckColumns(.data, expected_cols)

  # check that multiplier is provided if method = "multiplier"
  if (nd_method == "multiplier" & nd_multiplier == "null") {
    stop("Please provide a multiplier for the lower detection limit handling method of 'multiplier'. Typically, the multiplier value is between 0 and 1.")
  }
  if (od_method == "multiplier" & od_multiplier == "null") {
    stop("Please provide a multiplier for the upper detection limit handling method of 'multiplier'")
  }

  # If user has not previously run TADA_IDCensoredData function, run it here to get required columns
  if (!"TADA.CensoredData.Flag" %in% names(.data)) {
    cens.data <- TADA_IDCensoredData(.data)
  } else {
    cens.data <- .data
  }

  if (all(cens.data$TADA.CensoredData.Flag == "Uncensored")) {
    print("Cannot apply simple censored methods to dataset with no censored data results. Returning input dataframe.")
    .data <- cens.data
  } else {
    # split out over detects and non detects (excluding pathogens as they are handled separately).
    nd <- subset(cens.data, cens.data$TADA.CensoredData.Flag == "Non-Detect" & cens.data$TADA.CharacteristicName != "FECAL COLIFORM")
    od <- subset(cens.data, cens.data$TADA.CensoredData.Flag == "Over-Detect" & cens.data$TADA.CharacteristicName != "FECAL COLIFORM")

    # create separate subsets for pathogen non-detects, coerced to NAs, NAs, and 0 or negative values.
    path_nd <- subset(cens.data, cens.data$TADA.CensoredData.Flag == "Non-Detect" & cens.data$TADA.CharacteristicName == "FECAL COLIFORM")
    path_co_na <- subset(cens.data, cens.data$TADA.CharacteristicName == "FECAL COLIFORM" & cens.data$TADA.ResultMeasureValueDataTypes.Flag == "Coerced to NA" & cens.data$TADA.CensoredData.Flag == "Uncensored")
    path_na <- subset(cens.data, cens.data$TADA.CharacteristicName == "FECAL COLIFORM" & is.na(TADA.ResultMeasureValue) & cens.data$TADA.ResultMeasureValueDataTypes.Flag != "Coerced to NA" & cens.data$TADA.CensoredData.Flag == "Uncensored")
    path_neg <- subset(cens.data, cens.data$TADA.CharacteristicName == "FECAL COLIFORM" & cens.data$TADA.ResultMeasureValue <= 0)
    path_comb <- plyr::rbind.fill(path_nd, path_na, path_co_na, path_neg)

    rm(path_nd, path_co_na, path_na, path_neg)

    # create subset of all results not included in the non detects, over detects, or pathogen subsets.
    all_others <- subset(cens.data, !cens.data$ResultIdentifier %in% c(nd$ResultIdentifier, od$ResultIdentifier, path_comb$ResultIdentifier))



    # ND handling
    if (dim(nd)[1] > 0) {
      if (nd_method == "multiplier") {
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd_multiplier
        nd$TADA.CensoredMethod <- paste0("Detection Limit Value Multiplied by ", nd_multiplier)
        nd$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
      if (nd_method == "randombelowlimit") {
        nd$multiplier <- stats::runif(dim(nd)[1], 0, 1)
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd$multiplier
        nd$TADA.CensoredMethod <- paste0("Random Value Between 0 and Detection Limit Using this Multiplier: ", round(nd$multiplier, digits = 3))
        nd <- nd %>% dplyr::select(-multiplier)
        nd$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
      if (nd_method == "as-is") {
        nd$TADA.CensoredMethod <- "Detection Limit Value Unchanged"
      }
    }
    # OD handling
    if (dim(od)[1] > 0) {
      if (od_method == "multiplier") {
        od$TADA.ResultMeasureValue <- od$TADA.ResultMeasureValue * od_multiplier
        od$TADA.CensoredMethod <- paste0("Detection Limit Value Multiplied by ", od_multiplier)
        od$TADA.ResultMeasureValueDataTypes.Flag <- "Result Value/Unit Estimated from Detection Limit"
      }
      if (od_method == "as-is") {
        od$TADA.CensoredMethod <- "Detection Limit Value Unchanged"
      }
    }
    # pathogen ND handling
    if (dim(path_comb)[1] > 0) {
      if (pathogen == TRUE) {
        path_comb$TADA.ResultMeasureValue <- pathogen_value
        path_comb$TADA.CensoredMethod <- paste0("Non-Detect, Coerced NA, NA, or Negative Value Substituted With ", pathogen_value)
        path_comb$TADA.ResultMeasureValueDataTypes.Flag <- "Non-Detect, Coerced NA, NA, or Negative Value Defaults to Small Number"
      }
      if (pathogen == FALSE) {
        path_comb$TADA.CensoredMethod <- "Result Measure Value Unchanged"
      }
    }

    .data <- plyr::rbind.fill(nd, od, path_comb, all_others)
    .data <- TADA_OrderCols(.data)
  }
  return(.data)
}
