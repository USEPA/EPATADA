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
#' non-detects. When these appear in a data frame, they are categorized as
#' "Detection condition or detection limit is not documented in TADA reference
#' tables." In these situations, users should contact TADA administrators to
#' update the package accordingly. This function is used by default in
#' TADA_SimpleCensoredMethods, but can be used on its own.
#'
#' @param .data A TADA dataframe
#'
#' @return A TADA dataframe with additional column named TADA.CensoredData.Flag,
#' which categorizes results with the following values:
#' 1) Detection condition or detection limit is not documented in TADA reference tables.
#' 2) Detection condition is missing and required for censored data ID.
#' 3) Conflict between Condition and Limit
#' 4) Non-Detect
#' 5) Over-Detect
#' 6) Other Condition/Limit Populated
#' If user has not previously run TADA_FlagMeasureQualifierCode, this function will also run that
#' function to add the columns TADA.MeasureQualifierCode.Flag and TADA.MeasureQualifierCode.Def
#' because user-supplied Result Measure Qualifier codes are also used to ID censored results.
#'
#' @export

TADA_IDCensoredData <- function(.data) {
  # check .data has all of the required columns
  expected_cols <- c(
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValueDataTypes.Flag"
  )
  TADA_CheckColumns(.data, expected_cols)

  # this copies det lim result value and unit over to TADA result value and unit
  # when the result value is TEXT but there is a specific text value that indicates
  # the result is censored (BPQL, BDL, ND)
  # and the TADA.DetectionQuantitationLimitMeasure.MeasureValue and unit is populated
  # if more are added, they need to be included below as well

  .data$TADA.ResultMeasureValue <- ifelse(
    (.data$ResultMeasureValue == "BPQL" |
      .data$ResultMeasureValue == "BDL" |
      .data$ResultMeasureValue == "ND") &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue) &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode),
    .data$TADA.DetectionQuantitationLimitMeasure.MeasureValue,
    .data$TADA.ResultMeasureValue
  )

  # this does the same as above for the units
  .data$TADA.ResultMeasure.MeasureUnitCode <- ifelse(
    (.data$ResultMeasureValue == "BPQL" |
      .data$ResultMeasureValue == "BDL" |
      .data$ResultMeasureValue == "ND") &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue) &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode),
    .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
    .data$TADA.ResultMeasure.MeasureUnitCode
  )

  # this updates the TADA.ResultMeasureValueDataTypes.Flag
  .data$TADA.ResultMeasureValueDataTypes.Flag <- ifelse(
    (.data$ResultMeasureValue == "BPQL" |
      .data$ResultMeasureValue == "BDL" |
      .data$ResultMeasureValue == "ND") &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue) &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode),
    "Result Value/Unit Copied from Detection Limit",
    .data$TADA.ResultMeasureValueDataTypes.Flag
  )

  # Copy detection limit value and unit to TADA Result Measure Value and Unit columns
  # this first row copies all over when TADA.DetectionQuantitationLimitMeasure.MeasureValue is not 
  # NA and thw TADA.ResultMeasureValueDataTypes.Flag is "NA - Not Available"
  # Imp note: TADA result values are NA for text and other values (coerced) even though they are 
  # not NA in the original result value
  .data$TADA.ResultMeasureValue <- ifelse(
    !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue) &
      (.data$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available" |
        is.na(.data$TADA.ResultMeasureValueDataTypes.Flag)),
    .data$TADA.DetectionQuantitationLimitMeasure.MeasureValue,
    .data$TADA.ResultMeasureValue
  )

  # this does the same as above for the units
  .data$TADA.ResultMeasure.MeasureUnitCode <- ifelse(
    !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode) &
      (.data$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available" |
        is.na(.data$TADA.ResultMeasureValueDataTypes.Flag)),
    .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
    .data$TADA.ResultMeasure.MeasureUnitCode
  )

  # this updates the TADA.ResultMeasureValueDataTypes.Flag
  .data$TADA.ResultMeasureValueDataTypes.Flag <- ifelse(
    (.data$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available" |
      is.na(.data$TADA.ResultMeasureValueDataTypes.Flag)) &
      !is.na(.data$TADA.DetectionQuantitationLimitMeasure.MeasureValue),
    "Result Value/Unit Copied from Detection Limit",
    .data$TADA.ResultMeasureValueDataTypes.Flag
  )

  # If user has not previously run TADA_FlagMeasureQualifierCode, run it here
  # to add column TADA.MeasureQualifier.Flag to allow for using user-supplied
  # Result Measure Qualifier codes to identify censored samples.
  if (!"TADA.MeasureQualifierCode.Flag" %in% names(.data)) {
    data_mq_flag <- TADA_FlagMeasureQualifierCode(.data)
  } else {
    data_mq_flag <- .data
  }

  ## Identify censored data using TADA.ResultMeasureValueDataTypes.Flag and 
  # TADA.MeasureQualifierCode.Flag
  cens_rm_flag <- data_mq_flag %>% dplyr::filter(
    TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit")
  cens_mq_flag <- data_mq_flag %>%
    dplyr::filter(TADA.MeasureQualifierCode.Flag %in% c("Non-Detect", "Over-Detect")) %>%
    dplyr::filter(!ResultIdentifier %in% cens_rm_flag$ResultIdentifier)
  cens <- cens_rm_flag %>%
    rbind(cens_mq_flag)
  not_cens <- data_mq_flag %>% dplyr::filter(!ResultIdentifier %in% cens$ResultIdentifier)
  not_cens$TADA.CensoredData.Flag <- "Uncensored"

  rm(cens_rm_flag, cens_mq_flag, data_mq_flag)

  if (dim(cens)[1] > 0) {
    ## Bring in det cond reference table
    cond.ref <- utils::read.csv(system.file("extdata", "WQXResultDetectionConditionRef.csv", 
                                            package = "EPATADA")) %>%
      dplyr::rename(ResultDetectionConditionText = Name) %>%
      dplyr::select(ResultDetectionConditionText, TADA.Detection_Type)

    ## Join to censored data
    cens <- dplyr::left_join(cens, cond.ref, by = "ResultDetectionConditionText")

    ## Flag censored data that does not have det cond populated
    cens$TADA.Detection_Type <- ifelse(is.na(cens$ResultDetectionConditionText), 
                                       "ResultDetectionConditionText missing", 
                                       cens$TADA.Detection_Type)

    ## Fill in detection type when ResultMeasureValue indicates it is a nondetect
    ## should be blank in result value so that the rest of the function can work
    nd.rmv.list <- utils::read.csv(system.file("extdata", "WQXMeasureQualifierCodeRef.csv", 
                                               package = "EPATADA")) %>%
      dplyr::filter(TADA.MeasureQualifierCode.Flag == "Non-Detect") %>%
      dplyr::select(Code) %>%
      dplyr::pull()

    cens$TADA.Detection_Type <- ifelse(cens$ResultMeasureValue %in%
      nd.rmv.list,
    "Non-Detect",
    cens$TADA.Detection_Type
    )

    rm(nd.rmv.list)

    ## Let user know when detection condition text is missing from one or more results
    # NOTE that at this point, TADA.Detection_Type may be NA if there are detection conditions in 
    # data frame that are not present in domain table
    if (any(cens$TADA.Detection_Type[!is.na(cens$TADA.Detection_Type)] == 
            "ResultDetectionConditionText missing")) {
      missing_detcond <- length(cens$TADA.Detection_Type[cens$TADA.Detection_Type == 
                                                           "ResultDetectionConditionText missing"])
      print(paste0("TADA_IDCensoredData: There are ", missing_detcond, 
                   " results in your data frame that are missing ResultDetectionConditionText.", 
                   "TADA requires BOTH ResultDetectionConditionText and", 
                   "DetectionQuantitationLimitTypeName fields to be populated in order to",
                   "categorize censored data."))
    }

    ## Let user know when one or more result detection conditions are not in the ref table
    conds <- unique(cens$ResultDetectionConditionText[!is.na(cens$ResultDetectionConditionText)])
    if (any(!conds %in% cond.ref$ResultDetectionConditionText)) {
      missing_conds <- conds[!conds %in% cond.ref$ResultDetectionConditionText]
      missing_conds <- paste(missing_conds, collapse = ", ")
      print(paste0("TADA_IDCensoredData: ResultDetectionConditionText column in data frame",
                   "contains value(s) ", missing_conds, " which is/are not represented in the",
                   "ResultDetectionConditionText WQX domain table. These data records are placed",
                   "under the TADA.CensoredData.Flag: Censored but not Categorized, and will not",
                   "be used in censored data handling methods. ",
                   "Please contact TADA administrators to resolve."))
    }

    ## Bring in det limit type reference table
    limtype.ref <- utils::read.csv(system.file("extdata", 
                                               "WQXDetectionQuantitationLimitTypeRef.csv", 
                                               package = "EPATADA")) %>%
      dplyr::rename(DetectionQuantitationLimitTypeName = Name) %>%
      dplyr::select(DetectionQuantitationLimitTypeName, TADA.Limit_Type)

    ## Join to censored data
    cens <- dplyr::left_join(cens, limtype.ref, by = "DetectionQuantitationLimitTypeName")

    limits <- unique(cens$DetectionQuantitationLimitTypeName)
    if (any(!limits %in% limtype.ref$DetectionQuantitationLimitTypeName)) {
      missing_lims <- limits[!limits %in% limtype.ref$DetectionQuantitationLimitTypeName]
      missing_lims <- paste(missing_lims, collapse = ", ")
      print(paste0("TADA_IDCensoredData: DetectionQuantitationLimitTypeName column in data frame",
                   "contains value(s) ", missing_lims, 
                   " which is/are not represented in the DetectionQuantitationLimitTypeName",
                   "WQX domain table. These data records are placed under the",
                   "TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in",
                   "censored data handling methods.",
                   "Please contact TADA administrators to resolve."))
    }

    ## Create flag for condition and limit type combinations
    cens$TADA.CensoredData.Flag <- 
      "Detection condition or detection limit is not documented in TADA reference tables."
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type 
                                          %in% c("ResultDetectionConditionText missing"),
      "Detection condition is missing and required for censored data ID.",
      cens$TADA.CensoredData.Flag
    )
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Non-Detect") &
      cens$TADA.Limit_Type %in% c("Non-Detect"),
    "Non-Detect",
    cens$TADA.CensoredData.Flag
    )
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Over-Detect") &
      cens$TADA.Limit_Type %in% c("Over-Detect"),
    "Over-Detect",
    cens$TADA.CensoredData.Flag
    )
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Other") &
      cens$TADA.Limit_Type %in% c("Other"),
    "Other Condition/Limit Populated",
    cens$TADA.CensoredData.Flag
    )
    cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.Detection_Type %in% c("Non-Detect", 
                                                                          "Over-Detect", "Other") &
      cens$TADA.Limit_Type %in% c("Non-Detect", "Over-Detect", "Other") &
      !cens$TADA.Detection_Type == cens$TADA.Limit_Type,
    "Conflict between Condition and Limit",
    cens$TADA.CensoredData.Flag
    )
    # Because detection limits for ResultMeasureValue = "BDL", "BPQL", or "ND" are copied to the 
    # result value (above), the TADA.CensoredData.Flag should be
    # "Non-Detect"
    # cens$TADA.CensoredData.Flag <- ifelse(cens$TADA.ResultMeasureValueDataTypes.Flag %in% 
    #                                       c("Result Value/Unit Copied from Detection Limit") &
    #                                         cens$ResultMeasureValue %in% c("BDL", "BPQL", "ND"),
    #                                       "Non-Detect",
    #                                       cens$TADA.CensoredData.Flag)

    ## warn when some limit metadata may be problematic
    if ("Conflict between Condition and Limit" %in% cens$TADA.CensoredData.Flag) {
      num <- length(cens$TADA.CensoredData.Flag[cens$TADA.CensoredData.Flag == 
                                                  "Conflict between Condition and Limit"])
      print(paste0("TADA_IDCensoredData: ", num, 
                   " records in supplied dataset have conflicting detection condition and detection",
                   "limit type information. These records will not be included in detection limit",
                   "handling calculations."))
    }

    if ("Detection condition or detection limit is not documented in TADA reference tables."
        %in% cens$TADA.CensoredData.Flag) {
      num <- length(cens$TADA.CensoredData.Flag[cens$TADA.CensoredData.Flag == 
            "Detection condition or detection limit is not documented in TADA reference tables."])
      print(paste0("TADA_IDCensoredData: ", num, 
                   " records in the supplied data frame have detection conditions and/or limit",
                   "types that are missing from TADA reference tables. These records will not be",
                   "included in detection limit handling calculations."))
    }

    cens <- cens %>% dplyr::select(-TADA.Detection_Type, -TADA.Limit_Type, 
                                   -TADA.MeasureQualifierCode.Flag)

    cens.check <- plyr::rbind.fill(cens, not_cens)
  } else {
    cens.check <- not_cens
    print(paste0("TADA_IDCensoredData: No censored data detected in your data frame.",
                 " Returning input dataframe with new column TADA.CensoredData.Flag set",
                 " to Uncensored"))
  }

  cens.check <- TADA_OrderCols(cens.check)
  return(cens.check)
}


#' Simple Tools for Censored Data Handling
#'
#' This function determines if detection limit type and detection condition are parsimonious
#' before applying simple tools for non-detect and over-detect data handling, including filling
#' in the values as-is, X times the detection limit, or a random number between 0
#' and the LOWER detection limit. These methods do NOT depend upon censored data frequency
#' in the data frame.
#'
#' This function runs TADA_IDCensoredData within it which adds the column
#' TADA.CensoredData.Flag. Enter ?TADA_IDCensoredData into the console for more
#' information.
#'
#' @param .data A TADA dataframe
#' @param nd_method A text string indicating the type of method used to populate a non-detect
#' (lower limit) data value. Can be set to "multiplier" (default),"randombelowlimit", or "as-is".
#' @param nd_multiplier A number to be multiplied to the LOWER detection limit for each entry to 
#' obtain the censored data value. Must be supplied if nd_method = "multiplier". Defaults to 0.5, 
#' or half the detection limit.
#' @param od_method A text string indicating the type of method used to populate an over-detect 
#' (upper limit) data value. Can be set to "multiplier" or "as-is" (default).
#' @param od_multiplier A number to be multiplied to the UPPER detection limit for each entry to 
#' obtain the censored data value. Must be supplied if od_method = "multiplier". Defaults to 0.5, 
#' or half the detection limit.
#'
#' @return A TADA dataframe with the additional column TADA.CensoredMethod,
#' which documents the method used to fill censored data values.
#' If user has not previously run TADA_IDCensoredData or
#' TADA_FlagMeasureQualifierCode, this function will also run those and add the
#' columns TADA.CensoredData.Flag, TADA.MeasureQualifierCode.Flag and
#' TADA.MeasureQualifierCode.Def.
#'
#' @export
#'
#' @examples
#' # Load example data frame:
#' data(Data_Nutrients_UT)
#' # Check for agreement between detection condition and detection limit type,
#' # and in instances where the measurement is non-detect, set the result value
#' # to half of the detection limit value. For over-detect measurements, retain
#' # the detection limit value as the result value as-is.
#' Data_Nutrients_UT_CensoredFlag <- TADA_SimpleCensoredMethods(Data_Nutrients_UT,
#'   nd_method = "multiplier",
#'   nd_multiplier = 0.5,
#'   od_method = "as-is",
#'   od_multiplier = "null"
#' )
#'
#' # Check for agreement between detection condition and detection limit type, and in instances 
#' where the measurement is non-detect, set the result value to a random value between 0 and the 
#' detection limit value. For over-detect measurements, retain the detection limit value as the 
#' result value as-is.
#' Data_Nutrients_UT_CensoredFlag <- TADA_SimpleCensoredMethods(Data_Nutrients_UT,
#'   nd_method = "randombelowlimit",
#'   nd_multiplier = "null",
#'   od_method = "as-is",
#'   od_multiplier = "null"
#' )
#'
TADA_SimpleCensoredMethods <- function(.data, nd_method = "multiplier", nd_multiplier = 0.5, 
                                       od_method = "as-is", od_multiplier = "null") {
  # check .data has all of the required columns
  expected_cols <- c(
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValueDataTypes.Flag"
  )
  TADA_CheckColumns(.data, expected_cols)

  # check that multiplier is provided if method = "multiplier"
  if (nd_method == "multiplier" & nd_multiplier == "null") {
    stop(paste0("Please provide a multiplier for the lower detection limit handling method of",
                " 'multiplier'. Typically, the multiplier value is between 0 and 1."))
  }
  if (od_method == "multiplier" & od_multiplier == "null") {
    stop(paste0("Please provide a multiplier for the upper detection limit handling method of",
                " 'multiplier'"))
  }

  # If user has not previously run TADA_IDCensoredData function, run it here to get required 
  # columns and to copy
  # detection limit to result value
  if (!"TADA.CensoredData.Flag" %in% names(.data)) {
    cens.data <- TADA_IDCensoredData(.data)
  } else {
    cens.data <- .data
  }

  if (all(cens.data$TADA.CensoredData.Flag == "Uncensored")) {
    print(paste0("Cannot apply simple censored methods to data frame with no censored data",
                 " results. Returning input data frame."))
    .data <- cens.data
  } else {
    # split out over detects and non detects
    nd <- subset(cens.data, cens.data$TADA.CensoredData.Flag == "Non-Detect")
    od <- subset(cens.data, cens.data$TADA.CensoredData.Flag == "Over-Detect")
    all_others <- subset(cens.data, !cens.data$ResultIdentifier %in% 
                           c(nd$ResultIdentifier, od$ResultIdentifier))

    # ND handling
    if (dim(nd)[1] > 0) {
      if (nd_method == "multiplier") {
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd_multiplier
        nd$TADA.CensoredMethod <- paste0("Detection Limit Value Multiplied by ", nd_multiplier)
        nd$TADA.ResultMeasureValueDataTypes.Flag <- 
          "Result Value/Unit Estimated from Detection Limit"
      }
      if (nd_method == "randombelowlimit") {
        nd$multiplier <- stats::runif(dim(nd)[1], 0, 1)
        nd$TADA.ResultMeasureValue <- nd$TADA.ResultMeasureValue * nd$multiplier
        nd$TADA.CensoredMethod <- paste0("Random Value Between 0 and Detection Limit Using this",
                                         " Multiplier: ", round(nd$multiplier, digits = 3))
        nd <- nd %>% dplyr::select(-multiplier)
        nd$TADA.ResultMeasureValueDataTypes.Flag <- 
          "Result Value/Unit Estimated from Detection Limit"
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
        od$TADA.ResultMeasureValueDataTypes.Flag <- 
          "Result Value/Unit Estimated from Detection Limit"
      }
      if (od_method == "as-is") {
        od$TADA.CensoredMethod <- "Detection Limit Value Unchanged"
      }
    }

    .data <- plyr::rbind.fill(nd, od, all_others) %>%
      TADA_CreateComparableID()
  }
  .data <- TADA_OrderCols(.data)
  return(.data)
}
