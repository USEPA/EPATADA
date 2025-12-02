# Test that new combinations of ATTAINS parameter have not been added to domain tables
test_that("Does the current TADA_GetATTAINSParamToWQPCharRef contain all ATTAINS parameter name?", {
  # Check for any new domain values for ATTAINS Parameters

  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))

  # extract unique ATTAINS parameter names
  ref <- ATTAINS.raw[, "name"]
  old <- utils::read.csv(system.file("extdata", "ATTAINSParamToWQPCharRef.csv", package = "EPATADA"))[, "ATTAINS.ParameterName"]

  expect_in(ref, old)
})

# Test that new combinations of CST pollutant or STD pollutant names have not been added to domain tables
test_that("Does the current TADA_GetCriteriaSearchToolRef contain all CST pollutant names?", {
  CST.raw <- openxlsx::read.xlsx("https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx")

  # Find the first row that has all values populated. This will indicate the column names of the CST data frame.
  # Note: Why not use a static row number? The CST may get new entries that may change the start of the data frame's.
  first_filled_row_index <- which(rowSums(is.na(CST.raw)) == 0)[1]

  # Extract our CST column names
  CST.cols <- as.character(CST.raw[first_filled_row_index, ])

  # remove rows with "legend" info (rows 1-201)
  CST <- CST.raw[-c(1:first_filled_row_index), ]

  # assign column names to the new data frame
  names(CST) <- CST.cols

  # filter the dataframe to just the CAS and pollutant numbers for our use case.
  CST <- CST %>%
    dplyr::select(POLLUTANT_NAME, STD_POLLUTANT_NAME, CAS_NO) %>%
    dplyr::distinct()
  ref <- CST$POLLUTANT_NAME
  old <- utils::read.csv(system.file("extdata", "CriteriaSearchToolRef.csv", package = "EPATADA"))[, "POLLUTANT_NAME"]

  expect_in(ref, old)
})
