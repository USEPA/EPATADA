# get random data
test_dat <- TADA_RandomTestingData()

# Test: Check for potential duplicates during criteria methods table generation
testthat::test_that("TADA_ParametersForAnalysis ", {
  param_ref_none <- TADA_ParametersForAnalysis(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "None",
    excel = FALSE
  )

  suppressWarnings(
    use_param_ref_none <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_none,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = FALSE,
      excel = FALSE
    )
  )
  # a user supplied table for a param_use will not populate TADA_UsesForAnalysis if paramRef crosswalk is left blank.
  user.supplied.uses.param <- data.frame(
    ATTAINS.OrganizationIdentifier = rep(
      "MTDEQ",
      length(unique(test_dat$TADA.ComparableDataIdentifier))
    ), # we have assigned to example uses to each parameter
    ATTAINS.ParameterName = unique(test_dat$TADA.ComparableDataIdentifier),
    ATTAINS.UseName = rep(
      c("example use_name1", "example use_name2"),
      length(unique(test_dat$TADA.ComparableDataIdentifier))
    )
  )

  suppressWarnings(
    use_param_ref_none2 <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_none,
      usesRef = user.supplied.uses.param,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = FALSE,
      excel = FALSE
    )
  )

  # A user supplied table when paramRef has no crosswalk should return 0 rows (empty data frame).
  # Check to make sure there are 7 columns, 0 rows
  testthat::expect_true(
    dim(use_param_ref_none)[1] == 0 &&
      dim(use_param_ref_none)[2] == 7 &&
      dim(use_param_ref_none2)[1] == 0 &&
      dim(use_param_ref_none2)[2] == 7
  )
  #################
  param_ref_all <- TADA_ParametersForAnalysis(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "All",
    excel = FALSE
  )

  suppressWarnings(
    use_param_ref_all <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_all,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = TRUE,
      excel = FALSE
    )
  )

  # a user supplied table for a param_use will not populate TADA_UsesForAnalysis if paramRef crosswalk is left blank.
  user.supplied.uses.param <- data.frame(
    ATTAINS.OrganizationIdentifier = rep(
      "MTDEQ",
      length(unique(param_ref_all$ATTAINS.ParameterName))
    ), # we have assigned to example uses to each parameter
    ATTAINS.ParameterName = unique(param_ref_all$ATTAINS.ParameterName),
    ATTAINS.UseName = rep(
      c("example use_name1", "example use_name2"),
      length(unique(param_ref_all$ATTAINS.ParameterName))
    )
  )

  suppressWarnings(
    use_param_ref_all2 <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_all,
      usesRef = user.supplied.uses.param,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = FALSE,
      excel = FALSE
    )
  )

  # A user supplied table when paramRef is filled out when auto_assign = FALSE
  # should all reflect the user supplied crosswalk. Check all unique uses to make sure.
  # Check to make sure there are 7 columns, 0 rows
  testthat::expect_true(
    all(
      unique(use_param_ref_all2$ATTAINS.UseName) %in%
        unique(user.supplied.uses.param$ATTAINS.UseName)
    ) &&
      dim(use_param_ref_all)[2] == 7 &&
      dim(use_param_ref_all2)[2] == 7
  )
  #################
  param_ref_org <- TADA_ParametersForAnalysis(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "Org",
    excel = FALSE
  )

  # check to make sure all rows contain all unique ComparableDataIdentifiers in the TADA data frame
  testthat::expect_true(
    length(unique(test_dat$TADA.ComparableDataIdentifier)) ==
      length(unique(param_ref_none$TADA.ComparableDataIdentifier)) &&
      length(unique(param_ref_all$TADA.ComparableDataIdentifier)) ==
        length(unique(param_ref_none$TADA.ComparableDataIdentifier))
  )
})

# Test: Auto_assign criteria table should contain all unique TADA.Characteristics/TADA.ComparableDataIdentifier
testthat::test_that("TADA_DefineCriteriaMethodology ", {
  suppressWarnings(
    Criteria_autofill <- TADA_DefineCriteriaMethodology(
      test_dat,
      org_id = NULL, # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = TRUE,
      displayUniqueId = FALSE,
      excel = FALSE
      # uncomment to run the excel file
      # excel = TRUE, overwrite = TRUE
    )
  )

  suppressWarnings(
    Criteria_autofill_w_uniqueID <- TADA_DefineCriteriaMethodology(
      test_dat,
      org_id = NULL, # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = TRUE,
      displayUniqueId = TRUE,
      excel = FALSE
      # uncomment to run the excel file
      # excel = TRUE, overwrite = TRUE
    )
  )
  # check to make sure all criteria table has same number of TADA.Characteristics/TADA.ComparableDataIdentifiers
  testthat::expect_true(
    length(unique(test_dat$TADA.ComparableDataIdentifier)) ==
      length(unique(
        Criteria_autofill_w_uniqueID$TADA.ComparableDataIdentifier
      )) &&
      length(unique(test_dat$TADA.CharacteristicName)) ==
        length(unique(Criteria_autofill$TADA.CharacteristicName))
  )
})
