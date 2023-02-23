test_that("InvalidCoordinates works", {

  #use example dataset
  data(Nutrients_Utah)
  
  #flagonly
  InvalidCoord_flags <- InvalidCoordinates(Nutrients_Utah)
  unique(InvalidCoord_flags$TADA.InvalidCoordinates)
  reviewselectcolumns <- InvalidCoord_flags %>% dplyr::select(TADA.InvalidCoordinates, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  reviewflagsonly <- dplyr::filter(reviewselectcolumns, 
                                        is.na(TADA.InvalidCoordinates) != TRUE)
  unique(reviewflagsonly$TADA.InvalidCoordinates)

  #removeimprecise
  ImpreciseCoord_removed <- InvalidCoordinates(Nutrients_Utah, clean_imprecise = TRUE)
  unique(ImpreciseCoord_removed$TADA.InvalidCoordinates)
  
  expect_true(any(ImpreciseCoord_removed$TADA.InvalidCoordinates!="Imprecise_Latincludes999" 
                  | ImpreciseCoord_removed$TADA.InvalidCoordinates!="Imprecise_Longincludes999"
              | ImpreciseCoord_removed$TADA.InvalidCoordinates!="Imprecise_lessthan3decimaldigits"))
  
  # Remove data with coordinates outside the USA, but keep flagged data with imprecise coordinates:
  OutsideUSACoord_removed <- InvalidCoordinates(Nutrients_Utah, clean_outsideUSA = "remove")
  unique(OutsideUSACoord_removed$TADA.InvalidCoordinates)
  
  expect_true(any(OutsideUSACoord_removed$TADA.InvalidCoordinates!="LONG_OutsideUSA" 
                  | OutsideUSACoord_removed$TADA.InvalidCoordinates!="LAT_OutsideUSA"))
  
  ## Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
  Invalid_removed <- InvalidCoordinates(Nutrients_Utah, clean_outsideUSA = "remove", clean_imprecise = TRUE)
  unique(Invalid_removed$TADA.InvalidCoordinates)
  
})


test_that("Imprecise_lessthan3decimaldigits works", {
  
  #use example dataset
  data(Nutrients_Utah)
  
  #flagonly
  FLAGSONLY <- InvalidCoordinates(Nutrients_Utah)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.InvalidCoordinates, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.InvalidCoordinates == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LongitudeMeasure, decimalplaces) < 3)%>%dplyr::distinct()
  
  expect_true(all(sapply(FLAGSONLY$TADA.LongitudeMeasure, decimalplaces) < 4))
  
})

test_that("Imprecise_lessthan3decimaldigits works again", {
  
  #use example dataset
  data(Nutrients_Utah)
  
  #flagonly
  FLAGSONLY <- InvalidCoordinates(Nutrients_Utah)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.InvalidCoordinates, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.InvalidCoordinates == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LatitudeMeasure, decimalplaces) < 3) %>% dplyr::distinct()
  
  expect_true(all(sapply(FLAGSONLY$TADA.LatitudeMeasure, decimalplaces) < 4))
})

