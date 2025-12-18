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

# Test that new combinations of CST pollutant or STD pollutant names have not been added to domain tables
test_that("Does the current TADA_GetCriteriaSearchToolRef contain all CST pollutant names?", {
  # Check for any new domain values for CST Pollutant Names
  # retrieve the CST
  CriteriaSearchToolRef.raw <- openxlsx::read.xlsx(
    # read raw xlsx from url
    "https://www.epa.gov/system/files/documents/2025-07/criteria-search-tool-data.xlsx",
    sheet = 3
  )
  # extract Pollutant Name from the most recent CST ref
  ref <- CriteriaSearchToolRef.raw$POLLUTANT_NAME
  file_path <- system.file(
    "extdata",
    "CriteriaSearchToolRef.rda",
    package = "EPATADA"
  )
  load(file_path)
  old <- CriteriaSearchToolRef$POLLUTANT_NAME

  expect_in(ref, old)
})
