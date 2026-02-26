# Test that new combinations of ATTAINS parameter have not been added to domain tables
test_that("Is the saved ATTAINSParamToWQPCharRef.csv up to date?", {
  # Check for any new domain values for ATTAINS Parameters

  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))

  # extract unique ATTAINS parameter names
  ref <- ATTAINS.raw[, "name"]
  old <- utils::read.csv(system.file(
    "extdata",
    "ATTAINSParamToWQPCharRef.csv",
    package = "EPATADA"
  ))[, "ATTAINS.ParameterName"]

  expect_in(ref, old)
})

test_that("errors when any tolerance exceeds 1.0", {
  expect_error(
    TADA_AdditionalCharAliasForReview(ATTAINS.WQX.tolerance = 1.2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_AdditionalCharAliasForReview(WQX.ATTAINS.tolerance = 2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_AdditionalCharAliasForReview(ATTAINS.CST.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_AdditionalCharAliasForReview(CST.ATTAINS.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
})
