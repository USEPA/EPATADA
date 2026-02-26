# Test that new combinations of ATTAINS parameter have not been added to domain tables
test_that("Is the saved TADA_GetTADACharAliasRef.csv up to date?", {
  # Check for any new domain values for ATTAINS Parameters

  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))

  # extract unique ATTAINS parameter names
  ref <- ATTAINS.raw[, "name"]
  old <- utils::read.csv(system.file(
    "extdata",
    "TADACharAliasRef.csv",
    package = "EPATADA"
  ))[, "ATTAINS.ParameterName"]

  expect_in(ref, old)
})

test_that("errors when any tolerance exceeds 1.0", {
  expect_error(
    TADA_GetTADACharAliasRef(ATTAINS.WQX.tolerance = 1.2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(WQX.ATTAINS.tolerance = 2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(ATTAINS.CST.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(CST.ATTAINS.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
})

test_that("errors when any tolerance exceeds 1.0", {
  expect_error(
    TADA_GetTADAUsesAliasRef(ATTAINS.CST.tolerance = 1.2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADAUsesAliasRef(CST.ATTAINS.tolerance = 2),
    regexp = "Tolerance cannot exceed 100%"
  )
})
