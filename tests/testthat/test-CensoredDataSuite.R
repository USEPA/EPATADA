# Ensures synthetic test data include columns that downstream helpers require.
# - Adds TADA.MeasureQualifierCode.Flag to avoid calling TADA_FlagMeasureQualifierCode
# - Adds Comparable ID columns required by TADA_CreateComparableID (used by TADA_SimpleCensoredMethods)
ensure_test_cols <- function(df) {
  if (!"TADA.MeasureQualifierCode.Flag" %in% names(df)) {
    df$TADA.MeasureQualifierCode.Flag <- NA_character_
  }
  if (!"TADA.CharacteristicName" %in% names(df)) {
    df$TADA.CharacteristicName <- "DummyCharacteristic"
  }
  if (!"TADA.ResultSampleFractionText" %in% names(df)) {
    df$TADA.ResultSampleFractionText <- "Total"
  }
  if (!"TADA.MethodSpeciationName" %in% names(df)) {
    df$TADA.MethodSpeciationName <- NA_character_
  }
  df
}

test_that("TADA_IDCensoredData orphans", {
  cens.check <- TADA_RandomTestingData(choose_random_state = TRUE)
  expect_true(all(!is.na(cens.check$TADA.CensoredData.Flag)))
})

test_that("TADA_SimpleCensoredMethods doesn't drop data", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  cens.check <- TADA_SimpleCensoredMethods(testdat)
  expect_equal(nrow(testdat), nrow(cens.check))
})

test_that("TADA_IDCensoredData copies limits for BPQL/BDL/ND when present (dummy)", {
  # Dummy data exercising early copy rule for BPQL/BDL/ND
  df <- data.frame(
    ResultIdentifier = c("b1", "b2", "b3", "b4"),
    ResultMeasureValue = c("BPQL", "BDL", "ND", "ND"),
    ResultDetectionConditionText = NA_character_,
    DetectionQuantitationLimitTypeName = NA_character_,
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(
      0.11,
      0.22,
      0.33,
      0.44
    ),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c(
      "mg/L",
      "mg/L",
      "mg/L",
      NA_character_
    ), # b4 unit missing
    TADA.MeasureQualifierCode.Flag = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- TADA_IDCensoredData(df)

  # Rows with both limit value and unit present should be copied and flagged
  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "b1"],
    0.11
  )
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "b1"],
    "mg/L"
  )
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "b1"],
    "Result Value/Unit Copied from Detection Limit"
  )

  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "b2"],
    0.22
  )
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "b2"],
    "mg/L"
  )
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "b2"],
    "Result Value/Unit Copied from Detection Limit"
  )

  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "b3"],
    0.33
  )
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "b3"],
    "mg/L"
  )
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "b3"],
    "Result Value/Unit Copied from Detection Limit"
  )

  # b4: unit missing -> early copy should not fire; NA-flag copy may populate value-only
  # Our suite checks per-field NA-flag behavior elsewhere; here just confirm unit is still NA
  expect_true(is.na(out$TADA.ResultMeasure.MeasureUnitCode[
    out$ResultIdentifier == "b4"
  ]))
})

test_that("TADA_IDCensoredData does not introduce NAs in TADA.ResultMeasureValueDataTypes.Flag", {
  testdat <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 1,
    autoclean = TRUE
  )

  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Precondition: no NA flags
  expect_equal(sum(is.na(testdat$TADA.ResultMeasureValueDataTypes.Flag)), 0L)

  testdat2 <- TADA_IDCensoredData(testdat)

  # Postcondition: still no NA flags
  expect_equal(sum(is.na(testdat2$TADA.ResultMeasureValueDataTypes.Flag)), 0L)

  # Values remain numeric
  expect_true(is.numeric(testdat2$TADA.ResultMeasureValue))
})

test_that("TADA_SimpleCensoredMethods does not introduce duplicates or NAs in result or unit cols that cannot be handled in TADA_ConvertSpecialChars", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))

  # TADA_ConvertSpecialChars does not handle this yet 8/11/25
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))

  testdat2 <- TADA_SimpleCensoredMethods(
    testdat,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )

  testdat3 <- TADA_ConvertSpecialChars(
    testdat2,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat3$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat3$TADA.ResultMeasureValue)))

  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat2$TADA.ResultMeasure.MeasureUnitCode)))
})

test_that("TADA_IDCensoredData handles NA/blanks and classifies censored data correctly", {
  # Domain references
  cond_ref <- TADA_GetDetCondRef()
  lim_ref <- TADA_GetDetLimitRef()

  # Pick valid names from domain tables
  cond_nd <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Non-Detect"]
  lim_nd <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Non-Detect"]
  cond_od <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Over-Detect"]
  lim_od <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Over-Detect"]

  skip_if(length(cond_nd) == 0, "No Non-Detect condition in reference")
  skip_if(length(lim_nd) == 0, "No Non-Detect limit in reference")
  skip_if(length(cond_od) == 0, "No Over-Detect condition in reference")
  skip_if(length(lim_od) == 0, "No Over-Detect limit in reference")

  cond_nd <- cond_nd[[1]]
  lim_nd <- lim_nd[[1]]
  cond_od <- cond_od[[1]]
  lim_od <- lim_od[[1]]

  # Use an ND token recognized by early copy logic
  nd_copy_token <- "ND"

  test_df <- data.frame(
    ResultIdentifier = paste0("r", 1:9),
    ResultMeasureValue = c(
      nd_copy_token, # r1 -> copy from limit
      nd_copy_token, # r2 -> cannot estimate (both limit pieces missing)
      nd_copy_token, # r3 -> unit blank -> treated as NA, NA-flag copy of value only
      NA, # r4 -> over-detect via qualifier flag; ensure no overwrite
      nd_copy_token, # r5 -> conflict (ND cond vs Over-Detect limit type)
      nd_copy_token, # r6 -> missing detection condition (blank -> NA), ND overrides
      nd_copy_token, # r7 -> unknown detection condition, ND override => conflict with OD limit
      2, # r8 -> uncensored
      NA # r9 -> NA-flag copy from limit
    ),
    ResultDetectionConditionText = c(
      cond_nd,
      cond_nd,
      cond_nd,
      cond_od,
      cond_nd,
      "   ",
      "FooBar",
      NA,
      cond_nd
    ),
    DetectionQuantitationLimitTypeName = c(
      lim_nd,
      NA,
      lim_nd,
      lim_od,
      lim_od,
      lim_nd,
      lim_od,
      NA,
      lim_nd
    ),
    TADA.ResultMeasureValueDataTypes.Flag = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "NA - Not Available"
    ),
    TADA.ResultMeasureValue = c(NA, NA, NA, 10, NA, NA, NA, 2, NA),
    TADA.ResultMeasure.MeasureUnitCode = c(
      NA,
      NA,
      NA,
      "mg/L",
      NA,
      NA,
      NA,
      "mg/L",
      NA
    ),
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(
      0.5,
      NA,
      0.2,
      NA,
      NA,
      0.3,
      1.1,
      NA,
      0.1
    ),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c(
      "mg/L",
      NA,
      "   ",
      NA,
      NA,
      "mg/L",
      "mg/L",
      NA,
      "mg/L"
    ),
    TADA.MeasureQualifierCode.Flag = c(
      NA,
      "Non-Detect",
      "Non-Detect",
      "Over-Detect",
      "Non-Detect",
      "Non-Detect",
      "Non-Detect",
      NA,
      NA
    ),
    stringsAsFactors = FALSE
  )

  res <- TADA_IDCensoredData(test_df)
  get <- function(id, col) res[res$ResultIdentifier == id, col, drop = TRUE]

  # r1: Copy from limit for ND text; classify Non-Detect
  expect_identical(get("r1", "TADA.ResultMeasureValue"), 0.5)
  expect_identical(get("r1", "TADA.ResultMeasure.MeasureUnitCode"), "mg/L")
  expect_identical(
    get("r1", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Copied from Detection Limit"
  )
  expect_identical(get("r1", "TADA.CensoredData.Flag"), "Non-Detect")

  # r2: ND text but no limit info -> cannot estimate; categorized as not documented
  expect_identical(
    get("r2", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )
  expect_identical(
    get("r2", "TADA.CensoredData.Flag"),
    "Detection condition or detection limit is not documented in TADA reference tables."
  )

  # r3: NA-flag copy of value only; still Non-Detect
  expect_equal(get("r3", "TADA.ResultMeasureValue"), 0.2, tolerance = 1e-12)
  expect_true(is.na(get("r3", "TADA.ResultMeasure.MeasureUnitCode")))
  expect_identical(
    get("r3", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Copied from Detection Limit"
  )
  expect_identical(get("r3", "TADA.CensoredData.Flag"), "Non-Detect")

  # r4: Over-Detect via qualifier flag and consistent domain types; ensure value/unit retained
  expect_identical(get("r4", "TADA.CensoredData.Flag"), "Over-Detect")
  expect_identical(get("r4", "TADA.ResultMeasureValue"), 10)
  expect_identical(get("r4", "TADA.ResultMeasure.MeasureUnitCode"), "mg/L")

  # r5: Conflict between ND and OD types -> value/unit nulled and flag updated
  expect_identical(
    get("r5", "TADA.CensoredData.Flag"),
    "Conflict between Condition and Limit"
  )
  expect_true(is.na(get("r5", "TADA.ResultMeasureValue")))
  expect_true(is.na(get("r5", "TADA.ResultMeasure.MeasureUnitCode")))
  expect_identical(
    get("r5", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )

  # r6: Missing detection condition (blank -> NA) but ND override => Non-Detect
  expect_identical(get("r6", "TADA.CensoredData.Flag"), "Non-Detect")

  # r7: Unknown detection condition text with ND override + OD limit => Conflict and revert
  expect_identical(
    get("r7", "TADA.CensoredData.Flag"),
    "Conflict between Condition and Limit"
  )
  expect_true(is.na(get("r7", "TADA.ResultMeasureValue")))
  expect_true(is.na(get("r7", "TADA.ResultMeasure.MeasureUnitCode")))
  expect_identical(
    get("r7", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )

  # r8: Uncensored
  expect_identical(get("r8", "TADA.CensoredData.Flag"), "Uncensored")

  # r9: NA-flag copy; classify Non-Detect
  expect_identical(get("r9", "TADA.ResultMeasureValue"), 0.1)
  expect_identical(get("r9", "TADA.ResultMeasure.MeasureUnitCode"), "mg/L")
  expect_identical(
    get("r9", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Copied from Detection Limit"
  )
  expect_identical(get("r9", "TADA.CensoredData.Flag"), "Non-Detect")

  # No blank strings remain in key character fields
  no_blanks <- function(x) {
    x <- x[!is.na(x)]
    all(trimws(x) != "")
  }
  expect_true(no_blanks(res$ResultDetectionConditionText))
  expect_true(no_blanks(res$DetectionQuantitationLimitTypeName))
  expect_true(no_blanks(res$TADA.ResultMeasure.MeasureUnitCode))
  expect_true(no_blanks(
    res$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
  ))
  expect_true(no_blanks(res$TADA.ResultMeasureValueDataTypes.Flag))
})

test_that("ND override works with domain ND codes not in {ND,BDL,BPQL}", {
  mqr_ref <- TADA_GetMeasureQualifierCodeRef()
  # Find any extra ND codes in your domain
  extra_nd <- setdiff(
    mqr_ref$Code[mqr_ref$TADA.MeasureQualifierCode.Flag == "Non-Detect"],
    c("ND", "BDL", "BPQL")
  )
  skip_if(length(extra_nd) == 0, "No additional ND codes in domain")

  lim_ref <- TADA_GetDetLimitRef()
  lim_od <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Over-Detect"]
  skip_if(length(lim_od) == 0, "No Over-Detect limit in reference")
  lim_od <- lim_od[[1]]

  nd_token <- extra_nd[[1]]
  test_df <- data.frame(
    ResultIdentifier = "x1",
    ResultMeasureValue = nd_token, # domain ND token (may be lower-case, etc.)
    ResultDetectionConditionText = "FooBar", # unknown (non-blank)
    DetectionQuantitationLimitTypeName = lim_od, # OD limit type
    TADA.ResultMeasureValueDataTypes.Flag = "NA - Not Available",
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = 0.9,
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = "mg/L",
    TADA.MeasureQualifierCode.Flag = "Non-Detect",
    stringsAsFactors = FALSE
  )

  res <- TADA_IDCensoredData(test_df)
  expect_identical(
    res$TADA.CensoredData.Flag,
    "Conflict between Condition and Limit"
  )
  expect_true(is.na(res$TADA.ResultMeasureValue))
  expect_true(is.na(res$TADA.ResultMeasure.MeasureUnitCode))
  expect_identical(
    res$TADA.ResultMeasureValueDataTypes.Flag,
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )
})

test_that("Early ND copy requires both limit pieces; NA-flag copy fills value when unit missing", {
  lim_ref <- TADA_GetDetLimitRef()
  lim_nd <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Non-Detect"]
  skip_if(length(lim_nd) == 0, "No Non-Detect limit in reference")
  lim_nd <- lim_nd[[1]]

  df <- data.frame(
    ResultIdentifier = c("c1", "c2"),
    ResultMeasureValue = c("nd", "BDL"), # lower/mixed case tokens
    ResultDetectionConditionText = c(NA, NA),
    DetectionQuantitationLimitTypeName = c(lim_nd, lim_nd),
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(0.7, 1.2),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c("mg/L", NA), # c2 unit missing
    TADA.MeasureQualifierCode.Flag = NA_character_, # avoid internal flagger dependency
    stringsAsFactors = FALSE
  )

  out <- TADA_IDCensoredData(df)

  # c1: both value and unit present -> copy both and flag
  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "c1"],
    0.7
  )
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "c1"],
    "mg/L"
  )
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "c1"],
    "Result Value/Unit Copied from Detection Limit"
  )

  # c2: unit missing -> early copy does not fire, but NA-flag copy fills value; unit stays NA; flag set to Copied
  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "c2"],
    1.2
  )
  expect_true(is.na(out$TADA.ResultMeasure.MeasureUnitCode[
    out$ResultIdentifier == "c2"
  ]))
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "c2"],
    "Result Value/Unit Copied from Detection Limit"
  )
})

test_that("NA-flag copy populates value and unit independently and sets flag", {
  df <- data.frame(
    ResultIdentifier = c("v_only", "u_only", "both"),
    ResultMeasureValue = NA_real_,
    ResultDetectionConditionText = NA_character_,
    DetectionQuantitationLimitTypeName = NA_character_,
    TADA.ResultMeasureValueDataTypes.Flag = "NA - Not Available",
    TADA.ResultMeasureValue = c(NA, 5, NA),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", NA, NA),
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(1, 10, 2),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c(
      "mg/L",
      "mg/L",
      "mg/L"
    ),
    TADA.MeasureQualifierCode.Flag = NA_character_, # avoid TADA_FlagMeasureQualifierCode
    stringsAsFactors = FALSE
  )
  out <- TADA_IDCensoredData(df)

  # v_only: only value copied; unit unchanged
  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "v_only"],
    1
  )
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "v_only"],
    "mg/L"
  )
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "v_only"],
    "Result Value/Unit Copied from Detection Limit"
  )

  # u_only: only unit copied; value retained (5)
  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "u_only"],
    5
  )
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "u_only"],
    "mg/L"
  )
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "u_only"],
    "Result Value/Unit Copied from Detection Limit"
  )

  # both: both copied
  expect_identical(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "both"],
    2
  )
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "both"],
    "mg/L"
  )
  expect_identical(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "both"],
    "Result Value/Unit Copied from Detection Limit"
  )
})

test_that("TADA_SimpleCensoredMethods fills ND/OD correctly and handles problematic records", {
  # Domain lookup for consistency
  cond_ref <- TADA_GetDetCondRef()
  lim_ref <- TADA_GetDetLimitRef()

  cond_nd <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Non-Detect"]
  lim_nd <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Non-Detect"]
  cond_od <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Over-Detect"]
  lim_od <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Over-Detect"]

  skip_if(
    length(cond_nd) == 0 ||
      length(lim_nd) == 0 ||
      length(cond_od) == 0 ||
      length(lim_od) == 0,
    "Reference tables incomplete for this test"
  )

  cond_nd <- cond_nd[[1]]
  lim_nd <- lim_nd[[1]]
  cond_od <- cond_od[[1]]
  lim_od <- lim_od[[1]]

  df <- data.frame(
    ResultIdentifier = paste0("t", 1:6),
    ResultMeasureValue = c(
      "ND", # t1 ND: will copy DL, then multiply
      NA, # t2 OD: qualifier flag drives categorization
      NA, # t3 missing detection condition (blank text)
      1.23, # t4 not documented (unknown cond/limit)
      "ND", # t5 ND for "as-is"
      "ND" # t6 ND for "random"
    ),
    ResultDetectionConditionText = c(
      cond_nd,
      cond_od,
      "   ",
      "FooBar",
      cond_nd,
      cond_nd
    ),
    DetectionQuantitationLimitTypeName = c(
      lim_nd,
      lim_od,
      lim_nd,
      "FooLimit",
      lim_nd,
      lim_nd
    ),
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(
      2.0,
      10.0,
      0.5,
      NA,
      1.5,
      4.0
    ),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c(
      "mg/L",
      "mg/L",
      "mg/L",
      NA,
      "mg/L",
      "mg/L"
    ),
    TADA.MeasureQualifierCode.Flag = c(
      "Non-Detect",
      "Over-Detect",
      "Non-Detect",
      "Non-Detect",
      "Non-Detect",
      "Non-Detect"
    ),
    stringsAsFactors = FALSE
  )
  df <- ensure_test_cols(df) # ensure Comparable ID cols

  # ND multiplier and OD as-is
  out <- TADA_SimpleCensoredMethods(
    df,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )

  pick <- function(id, col) out[out$ResultIdentifier == id, col, drop = TRUE]

  # t1: ND, copied DL 2.0 -> multiplied by 0.5 => 1.0
  expect_equal(pick("t1", "TADA.ResultMeasureValue"), 1.0, tolerance = 1e-12)
  expect_identical(pick("t1", "TADA.ResultMeasure.MeasureUnitCode"), "mg/L")
  expect_identical(
    pick("t1", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Estimated from Detection Limit"
  )
  expect_identical(
    pick("t1", "TADA.CensoredMethod"),
    "Detection Limit Value Multiplied by 0.5"
  )

  # t2: OD as-is, value stays 10.0
  expect_equal(pick("t2", "TADA.ResultMeasureValue"), 10.0, tolerance = 1e-12)
  expect_identical(pick("t2", "TADA.ResultMeasure.MeasureUnitCode"), "mg/L")
  expect_identical(
    pick("t2", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Estimated from Detection Limit"
  )
  expect_identical(
    pick("t2", "TADA.CensoredMethod"),
    "Detection Limit Value Unchanged"
  )

  # t3: Missing detection condition -> set value to NA and flag cannot be estimated
  expect_true(is.na(pick("t3", "TADA.ResultMeasureValue")))
  expect_identical(
    pick("t3", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )
  expect_identical(
    pick("t3", "TADA.CensoredMethod"),
    "Result set to NA due to Missing Detection Condition"
  )

  # t4: Not documented -> set value to NA and flag cannot be estimated
  expect_true(is.na(pick("t4", "TADA.ResultMeasureValue")))
  expect_identical(
    pick("t4", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )
  expect_identical(
    pick("t4", "TADA.CensoredMethod"),
    "Result set to NA as Detection Condition or Limit is not in TADA Ref Table"
  )

  # t5: ND as-is
  out_as_is <- TADA_SimpleCensoredMethods(
    df[df$ResultIdentifier == "t5", ],
    nd_method = "as-is",
    od_method = "as-is",
    od_multiplier = "null"
  )
  expect_identical(
    out_as_is$TADA.ResultMeasureValueDataTypes.Flag,
    "Result Value/Unit Estimated from Detection Limit"
  )
  expect_identical(
    out_as_is$TADA.CensoredMethod,
    "Detection Limit Value Unchanged"
  )
  expect_equal(out_as_is$TADA.ResultMeasureValue, 1.5, tolerance = 1e-12)

  # t6: ND random below limit ∈ [0, limit]
  set.seed(42)
  out_rand <- TADA_SimpleCensoredMethods(
    df[df$ResultIdentifier == "t6", ],
    nd_method = "randombelowlimit",
    od_method = "as-is",
    od_multiplier = "null"
  )
  val <- out_rand$TADA.ResultMeasureValue
  limit <- 4.0
  expect_true(is.finite(val) && val >= 0 && val <= limit)
  expect_identical(
    out_rand$TADA.ResultMeasureValueDataTypes.Flag,
    "Result Value/Unit Estimated from Detection Limit"
  )
  expect_true(grepl(
    "^Random Value Between 0 and Detection Limit Using this Multiplier:",
    out_rand$TADA.CensoredMethod
  ))
})

test_that("TADA_IDCensoredData doesn't drop data", {
  df <- TADA_RandomTestingData(choose_random_state = TRUE)
  out <- TADA_IDCensoredData(df)
  expect_equal(nrow(df), nrow(out))
})

test_that("TADA_IDCensoredData executes without error on typical input", {
  df <- TADA_RandomTestingData(choose_random_state = TRUE)
  expect_no_error(TADA_IDCensoredData(df))
})

test_that("TADA_SimpleCensoredMethods validates parameters", {
  # Build a minimal dataset with one ND and one OD so method checks run
  cond_ref <- TADA_GetDetCondRef()
  lim_ref <- TADA_GetDetLimitRef()
  cond_nd <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Non-Detect"]
  lim_nd <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Non-Detect"]
  cond_od <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Over-Detect"]
  lim_od <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Over-Detect"]
  skip_if(
    length(cond_nd) * length(lim_nd) * length(cond_od) * length(lim_od) == 0,
    "Reference tables incomplete"
  )
  cond_nd <- cond_nd[[1]]
  lim_nd <- lim_nd[[1]]
  cond_od <- cond_od[[1]]
  lim_od <- lim_od[[1]]

  df <- data.frame(
    ResultIdentifier = c("nd1", "od1"),
    ResultMeasureValue = c("ND", NA),
    ResultDetectionConditionText = c(cond_nd, cond_od),
    DetectionQuantitationLimitTypeName = c(lim_nd, lim_od),
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(2, 10),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    TADA.MeasureQualifierCode.Flag = c("Non-Detect", "Over-Detect"),
    stringsAsFactors = FALSE
  )
  df <- ensure_test_cols(df)

  # nd_method = multiplier requires a numeric scalar
  expect_error(
    TADA_SimpleCensoredMethods(
      df,
      nd_method = "multiplier",
      nd_multiplier = "null"
    ),
    "Please provide a multiplier"
  )
  expect_error(
    TADA_SimpleCensoredMethods(
      df,
      nd_method = "multiplier",
      nd_multiplier = c(0.5, 0.2)
    ),
    "single numeric value"
  )

  # od_method = multiplier requires a numeric scalar
  expect_error(
    TADA_SimpleCensoredMethods(
      df,
      od_method = "multiplier",
      od_multiplier = "null"
    ),
    "Please provide a multiplier"
  )
  expect_error(
    TADA_SimpleCensoredMethods(
      df,
      od_method = "multiplier",
      od_multiplier = c(0.8, 0.9)
    ),
    "single numeric value"
  )
})

test_that("TADA_SimpleCensoredMethods applies OD multiplier correctly", {
  cond_ref <- TADA_GetDetCondRef()
  lim_ref <- TADA_GetDetLimitRef()
  cond_od <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Over-Detect"]
  lim_od <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Over-Detect"]
  skip_if(length(cond_od) * length(lim_od) == 0, "Reference tables incomplete")
  cond_od <- cond_od[[1]]
  lim_od <- lim_od[[1]]

  df <- data.frame(
    ResultIdentifier = c("od1", "od2"),
    ResultMeasureValue = NA,
    ResultDetectionConditionText = cond_od,
    DetectionQuantitationLimitTypeName = lim_od,
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(10, 5),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    TADA.MeasureQualifierCode.Flag = "Over-Detect",
    stringsAsFactors = FALSE
  )
  df <- ensure_test_cols(df)

  out <- TADA_SimpleCensoredMethods(
    df,
    nd_method = "multiplier",
    nd_multiplier = 0.5, # irrelevant here
    od_method = "multiplier",
    od_multiplier = 0.8
  )

  pick <- function(id, col) out[out$ResultIdentifier == id, col, drop = TRUE]
  expect_equal(pick("od1", "TADA.ResultMeasureValue"), 8.0, tolerance = 1e-12)
  expect_equal(pick("od2", "TADA.ResultMeasureValue"), 4.0, tolerance = 1e-12)
  expect_identical(
    pick("od1", "TADA.ResultMeasureValueDataTypes.Flag"),
    "Result Value/Unit Estimated from Detection Limit"
  )
  expect_identical(
    pick("od1", "TADA.CensoredMethod"),
    "Detection Limit Value Multiplied by 0.8"
  )
})

test_that("TADA_SimpleCensoredMethods ND random is per-row and within bounds", {
  cond_ref <- TADA_GetDetCondRef()
  lim_ref <- TADA_GetDetLimitRef()
  cond_nd <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Non-Detect"]
  lim_nd <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Non-Detect"]
  skip_if(length(cond_nd) * length(lim_nd) == 0, "Reference tables incomplete")
  cond_nd <- cond_nd[[1]]
  lim_nd <- lim_nd[[1]]

  df <- data.frame(
    ResultIdentifier = paste0("rnd", 1:5),
    ResultMeasureValue = "ND",
    ResultDetectionConditionText = cond_nd,
    DetectionQuantitationLimitTypeName = lim_nd,
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(1, 2, 3, 4, 5),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = "mg/L",
    TADA.MeasureQualifierCode.Flag = "Non-Detect",
    stringsAsFactors = FALSE
  )
  df <- ensure_test_cols(df)

  set.seed(123)
  out <- TADA_SimpleCensoredMethods(
    df,
    nd_method = "randombelowlimit",
    nd_multiplier = "null",
    od_method = "as-is",
    od_multiplier = "null"
  )

  vals <- out$TADA.ResultMeasureValue
  lims <- df$TADA.DetectionQuantitationLimitMeasure.MeasureValue
  expect_true(all(is.finite(vals)))
  expect_true(all(vals >= 0))
  expect_true(all(vals <= lims + .Machine$double.eps))
  # random draws should not all be equal
  expect_gt(length(unique(round(vals, 6))), 1L)
  expect_identical(
    unique(out$TADA.ResultMeasureValueDataTypes.Flag),
    "Result Value/Unit Estimated from Detection Limit"
  )
  expect_true(all(grepl(
    "^Random Value Between 0 and Detection Limit Using this Multiplier:",
    out$TADA.CensoredMethod
  )))
})

test_that("TADA_SimpleCensoredMethods returns input unchanged when no censored data", {
  # Construct a small uncensored data frame with required columns
  df <- data.frame(
    ResultIdentifier = c("u1", "u2"),
    ResultMeasureValue = c(1.1, 2.2),
    ResultDetectionConditionText = NA_character_,
    DetectionQuantitationLimitTypeName = NA_character_,
    TADA.ResultMeasureValueDataTypes.Flag = "Measured",
    TADA.ResultMeasureValue = c(1.1, 2.2),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    # Include an explicit 'Uncensored' flag to bypass re-ID
    TADA.CensoredData.Flag = "Uncensored",
    stringsAsFactors = FALSE
  )
  df <- ensure_test_cols(df) # optional but safe

  # Expect message and identical key columns after run
  expect_message(
    out <- TADA_SimpleCensoredMethods(df),
    "no censored data results",
    fixed = TRUE
  )
  expect_equal(nrow(out), nrow(df))
  expect_identical(out$TADA.ResultMeasureValue, df$TADA.ResultMeasureValue)
  expect_identical(
    out$TADA.ResultMeasure.MeasureUnitCode,
    df$TADA.ResultMeasure.MeasureUnitCode
  )
  expect_identical(out$TADA.CensoredData.Flag, df$TADA.CensoredData.Flag)
})

test_that("TADA_SimpleCensoredMethods respects existing TADA.CensoredData.Flag categories", {
  # One ND and one OD, already categorized, with copied limits
  df <- data.frame(
    ResultIdentifier = c("n1", "o1"),
    ResultMeasureValue = c(NA, NA),
    ResultDetectionConditionText = c(NA, NA),
    DetectionQuantitationLimitTypeName = c(NA, NA),
    TADA.ResultMeasureValueDataTypes.Flag = c(
      "Result Value/Unit Copied from Detection Limit",
      "Result Value/Unit Copied from Detection Limit"
    ),
    TADA.ResultMeasureValue = c(2, 10),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    TADA.CensoredData.Flag = c("Non-Detect", "Over-Detect"),
    stringsAsFactors = FALSE
  )
  df <- ensure_test_cols(df)

  out <- TADA_SimpleCensoredMethods(
    df,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "multiplier",
    od_multiplier = 0.8
  )

  expect_equal(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "n1"],
    1.0,
    tolerance = 1e-12
  )
  expect_equal(
    out$TADA.ResultMeasureValue[out$ResultIdentifier == "o1"],
    8.0,
    tolerance = 1e-12
  )
  expect_identical(
    unique(out$TADA.ResultMeasureValueDataTypes.Flag),
    "Result Value/Unit Estimated from Detection Limit"
  )
})

test_that("TADA_IDCensoredData flags missing detection condition when ND override does not apply", {
  lim_ref <- TADA_GetDetLimitRef()
  lim_nd <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Non-Detect"]
  skip_if(length(lim_nd) == 0, "No Non-Detect limit in reference")
  lim_nd <- lim_nd[[1]]

  # Detection condition blank (-> NA), value not an ND token, include via qualifier flag
  df <- data.frame(
    ResultIdentifier = "m1",
    ResultMeasureValue = 1.23, # not ND/BDL/BPQL
    ResultDetectionConditionText = "   ", # blank -> NA
    DetectionQuantitationLimitTypeName = lim_nd,
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = 0.25,
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = "mg/L",
    TADA.MeasureQualifierCode.Flag = "Non-Detect", # ensures inclusion in censored subset
    stringsAsFactors = FALSE
  )

  res <- TADA_IDCensoredData(df)
  expect_identical(
    res$TADA.CensoredData.Flag,
    "Detection condition is missing and required for censored data ID."
  )
})

test_that("TADA_IDCensoredData flags 'Other Condition/Limit Populated' when both are Other", {
  cond_ref <- TADA_GetDetCondRef()
  lim_ref <- TADA_GetDetLimitRef()

  cond_other <- cond_ref$Name[cond_ref$TADA.Detection_Type == "Other"]
  lim_other <- lim_ref$Name[lim_ref$TADA.Limit_Type == "Other"]
  skip_if(
    length(cond_other) == 0 || length(lim_other) == 0,
    "No 'Other' condition/limit in reference tables"
  )

  cond_other <- cond_other[[1]]
  lim_other <- lim_other[[1]]

  # Use a non-ND ResultMeasureValue so ND override does not force Non-Detect
  df <- data.frame(
    ResultIdentifier = "oth1",
    ResultMeasureValue = 1.11, # not ND/BDL/BPQL
    ResultDetectionConditionText = cond_other, # maps to Other
    DetectionQuantitationLimitTypeName = lim_other, # maps to Other
    TADA.ResultMeasureValueDataTypes.Flag = NA_character_,
    TADA.ResultMeasureValue = NA_real_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = NA_real_, # not needed for classification
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = NA_character_,
    TADA.MeasureQualifierCode.Flag = "Non-Detect", # ensures inclusion in censored subset
    stringsAsFactors = FALSE
  )

  res <- TADA_IDCensoredData(df)
  expect_identical(
    res$TADA.CensoredData.Flag,
    "Other Condition/Limit Populated"
  )
})
