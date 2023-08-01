test_that("harmonization works", {
  dat = TADA_RandomTestingSet()
  dat = subset(dat, !is.na(dat$TADA.ResultMeasureValue))
  dat = TADA_FlagFraction(dat, clean = TRUE)
  dat = TADA_FlagResultUnit(dat, clean = "invalid_only")
  dat = TADA_FlagSpeciation(dat, clean = "invalid_only")
  dat = TADA_FlagMethod(dat, clean = TRUE)
  
  ref = TADA_GetSynonymRef(dat)
  dat1 = TADA_HarmonizeSynonyms(dat, ref = ref)
  
  expect_true(dim(dat)[1]==dim(dat1)[1])
})
