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

test_that("errors if internal CST workbook is missing (fast)", {
  # Minimal synthetic inputs to avoid upstream I/O (only needed if function
  # does not fail fast on CST before WQX/ATTAINS).
  wqx_ref <- data.frame(
    CharacteristicName = "Nitrate as N",
    Char_Flag = NA_character_,
    Comparable.Name = "Nitrate",
    CAS.Number = "14797-55-8",
    stringsAsFactors = FALSE
  )
  atta_ref <- data.frame(
    name = "Nitrate",
    stringsAsFactors = FALSE
  )
  
  # Shadow system.file within EPATADA namespace so the function sees an empty path
  testthat::local_mocked_bindings(
    system.file = function(...) "",
    .package = "EPATADA"
  )
  
  # Avoid touching real alias CSV and upstream data
  testthat::local_mocked_bindings(
    read.csv = function(...) {
      data.frame(
        ATTAINS.ParameterName = character(),
        CharacteristicName = character(),
        stringsAsFactors = FALSE
      )
    },
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    TADA_GetCharacteristicRef = function() wqx_ref,
    .package = "EPATADA"
  )
  testthat::local_mocked_bindings(
    EQ_DomainValues = function(domain) atta_ref,
    .package = "rExpertQuery"
  )
  
  expect_error(
    TADA_AdditionalCharAliasForReview(),
    regexp = "Internal CST workbook is missing"
  )
})

test_that("errors if internal CST workbook cannot be read (fast)", {
  # Minimal synthetic inputs to avoid upstream I/O
  wqx_ref <- data.frame(
    CharacteristicName = "Nitrate as N",
    Char_Flag = NA_character_,
    Comparable.Name = "Nitrate",
    CAS.Number = "14797-55-8",
    stringsAsFactors = FALSE
  )
  atta_ref <- data.frame(
    name = "Nitrate",
    stringsAsFactors = FALSE
  )
  
  # Create a temp file to simulate an existing internal workbook path
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  file.create(tmp_xlsx)
  
  # Shadow system.file within EPATADA namespace to return our temp path
  testthat::local_mocked_bindings(
    system.file = function(...) tmp_xlsx,
    .package = "EPATADA"
  )
  
  # Force read failure from the internal workbook
  testthat::local_mocked_bindings(
    .tada_cst_read_sheet = function(workbook_path, target) NULL,
    .package = "EPATADA"
  )
  
  # Avoid touching real alias CSV and upstream data
  testthat::local_mocked_bindings(
    read.csv = function(...) {
      data.frame(
        ATTAINS.ParameterName = character(),
        CharacteristicName = character(),
        stringsAsFactors = FALSE
      )
    },
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    TADA_GetCharacteristicRef = function() wqx_ref,
    .package = "EPATADA"
  )
  testthat::local_mocked_bindings(
    EQ_DomainValues = function(domain) atta_ref,
    .package = "rExpertQuery"
  )
  
  expect_error(
    TADA_AdditionalCharAliasForReview(),
    regexp = "Failed to read 'Criteria' sheet from internal CST workbook"
  )
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
