# Test that new combinations of ATTAINS parameter have not been added to domain tables
test_that("Does the current TADA_GetATTAINSParamToWQPCharRef contain all ATTAINS parameter name?", {
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
