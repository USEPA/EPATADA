test_that("PairForCriteriaCalc does not grow dataset", {
  # use example dataset
  data(Data_Nutrients_UT)
  
  # flagonly
  InvalidCoord_flags <- TADA_FlagCoordinates(Data_Nutrients_UT)
  unique(InvalidCoord_flags$TADA.InvalidCoordinates)
  reviewselectcolumns <- InvalidCoord_flags %>% dplyr::select(TADA.InvalidCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  reviewflagsonly <- dplyr::filter(
    reviewselectcolumns,
    is.na(TADA.InvalidCoordinates.Flag) != TRUE
  )
  unique(reviewflagsonly$TADA.InvalidCoordinates.Flag)
  
  # removeimprecise
  ImpreciseCoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_imprecise = TRUE)
  unique(ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag)
  
  expect_true(any(ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag != "Imprecise_lessthan3decimaldigits"))
  
  # Remove data with coordinates outside the USA, but keep flagged data with imprecise coordinates:
  OutsideUSACoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove")
  unique(OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag)
  
  expect_true(any(OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag != "LONG_OutsideUSA" |
                    OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag != "LAT_OutsideUSA"))
  
  ## Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
  Invalid_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove", clean_imprecise = TRUE)
  unique(Invalid_removed$TADA.InvalidCoordinates.Flag)
})