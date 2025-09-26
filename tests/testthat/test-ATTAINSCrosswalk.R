# get random data
test_dat <- TADA_RandomTestingData()

# Test: Check for potential duplicates, all auto_assign options without a user supplied paramRef should have equal rows
test_that("TADA_CreateParamRef ", {
  param_ref_none <- TADA_CreateParamRef(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "None",
    excel = FALSE
  )
  
  param_ref_all <- TADA_CreateParamRef(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "All",
    excel = FALSE
  )
  
  param_ref_org <- TADA_CreateParamRef(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "Org",
    excel = FALSE
  )
  
  # check to make sure all rows contain equal number of rows and for equal number of unique ComparableDataIdentifiers
  expect_true(
    length(unique(test_dat$TADA.ComparableDataIdentifier)) == nrow(param_ref_none) && 
      nrow(param_ref_none) == nrow(param_ref_all) && nrow(param_ref_all) == nrow(param_ref_org)
    )
})

# Test: Auto_assign criteria table should contain all unique TADA.Characteristics/TADA.ComparableDataIdentifier
test_that("TADA_DefineCriteriaMethodology ", {
  suppressMessages(
  Criteria_autofill <- TADA_DefineCriteriaMethodology(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = TRUE,
    displayUniqueId = FALSE,
    excel = FALSE
    # uncomment to run the excel file
    #excel = TRUE, overwrite = TRUE
  )
  )
  
  suppressMessages(
  Criteria_autofill_w_uniqueID <- TADA_DefineCriteriaMethodology(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = TRUE,
    displayUniqueId = TRUE,
    # excel = FALSE
    # uncomment to run the excel file
    excel = TRUE, overwrite = TRUE
  )
  )
  # check to make sure all criteria table has same number of TADA.Characteristics/TADA.ComparableDataIdentifiers
  expect_true(
    length(unique(test_dat$TADA.ComparableDataIdentifier)) == length(unique(Criteria_autofill_w_uniqueID$TADA.ComparableDataIdentifier)) && 
    length(unique(test_dat$TADA.CharacteristicName)) == length(unique(Criteria_autofill$TADA.CharacteristicName))
  )
})
