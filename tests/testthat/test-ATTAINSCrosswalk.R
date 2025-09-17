# get MT data
test_dat <- TADA_DataRetrieval(
  startDate = "2020-01-01",
  endDate = "2022-12-31",
  statecode = "MT",
  characteristicName = c(
    "Escherichia",
    "Escherichia coli",
    "pH"
  ),
  county = "Missoula County",
  ask = FALSE
)

# Test: Check for potential duplicates, all auto_assign options without a user supplied paramRef should be equal
test_that("TADA_CreateParamRef ", {
  param_ref_none <- TADA_CreateParamRef(
    test_dat,
    org_id = "MTDEQ",
    auto_assign = "None",
    excel = FALSE
  )
  
  param_ref_all <- TADA_CreateParamRef(
    test_dat,
    org_id = "MTDEQ",
    auto_assign = "All",
    excel = FALSE
  )
  
  param_ref_org <- TADA_CreateParamRef(
    test_dat,
    org_id = "MTDEQ",
    auto_assign = "Org",
    excel = FALSE
  )
  
  # check to make sure all rows contain equal number of rows and for equal number of unique ComparableDataIdentifiers
  expect_true(
    length(unique(test_dat$TADA.ComparableDataIdentifier)) == nrow(param_ref_none) && 
      nrow(param_ref_none) == nrow(param_ref_all) && nrow(param_ref_all) == nrow(param_ref_org)
    )
})