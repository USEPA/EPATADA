# Test that new ATTAINS organization identifiers have not been added to domain tables
test_that("Is the saved ATTAINSOrgIDsRef.csv up to date?", {
  # Check for any new domain values for ATTAINS Parameters

  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))

  # extract unique ATTAINS parameter names
  ref <- ATTAINS.raw[, "name"]
  old <- utils::read.csv(system.file(
    "extdata",
    "ATTAINSOrgIDsRef.csv",
    package = "EPATADA"
  ))[, "name"]

  expect_in(ref, old)
})
