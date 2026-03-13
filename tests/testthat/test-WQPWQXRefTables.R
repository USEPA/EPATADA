# Test that new combinations of detection condition have not been added to domain tables
test_that("Is TADA_GetDetCondRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  file_path <- system.file(
    "extdata",
    "WQXResultDetectionConditionRef.rda",
    package = "EPATADA"
  )
  load(file_path)
  old <- WQXResultDetectionConditionRef
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetDetCondRef(download_only = TRUE, refresh = TRUE)
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))
  expect_true(old_latedate == new_latedate)
})

# Test that new combinations of detection condition have not been added to domain tables
test_that("Is TADA_GetDetLimitRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  file_path <- system.file(
    "extdata",
    "WQXDetectionQuantitationLimitTypeRef.rda",
    package = "EPATADA"
  )
  load(file_path)
  old <- WQXDetectionQuantitationLimitTypeRef
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetDetLimitRef(download_only = TRUE, refresh = TRUE)
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))
  expect_true(old_latedate == new_latedate)
})

# Test that new QC conditions have not been added to ActivityType domain table
test_that("Is TADA_GetActivityTypeRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  file_path <- system.file(
    "extdata",
    "WQXActivityTypeRef.rda",
    package = "EPATADA"
  )
  load(file_path)
  old <- WQXActivityTypeRef
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetActivityTypeRef(download_only = TRUE, refresh = TRUE)
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))
  expect_true(old_latedate == new_latedate)
})

# Test that new codes have not been added to ResultMeasureQualifier domain table
test_that("Is TADA_GetMeasureQualifierCodeRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  file_path <- system.file(
    "extdata",
    "WQXMeasureQualifierCodeRef.rda",
    package = "EPATADA"
  )
  load(file_path)
  old <- WQXMeasureQualifierCodeRef
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetMeasureQualifierCodeRef(download_only = TRUE, refresh = TRUE)
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))
  expect_true(old_latedate == new_latedate)
})

test_that("WQXcharValRef.rda contains only one row for each unique characteristic/media/unit/max/min combination for threshold functions", {
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)

  unit.ref <- dplyr::filter(WQXcharValRef, Type == "CharacteristicUnit")

  find.dups <- unit.ref |>
    dplyr::filter(Type == "CharacteristicUnit") |>
    dplyr::group_by(Characteristic, Source, Value.Unit) |>
    dplyr::mutate(
      Min_n = length(unique(Minimum)),
      Max_n = length(unique(Maximum))
    ) |>
    dplyr::filter(Min_n > 1 | Max_n > 1)

  expect_true(nrow(find.dups) == 0)
})

test_that("WQXcharValRef.rda contains only one row for each unique characteristic/media/unit/max/min combination for threshold functions", {
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)

  unit.ref <- dplyr::filter(
    WQXcharValRef,
    Type == "CharacteristicUnit",
    Status == "Accepted"
  )

  find.dups <- unit.ref |>
    dplyr::filter(Type == "CharacteristicUnit") |>
    dplyr::group_by(Characteristic, Source, Value.Unit) |>
    dplyr::mutate(
      Min_n = length(unique(Minimum)),
      Max_n = length(unique(Maximum))
    ) |>
    dplyr::filter(Min_n > 1 | Max_n > 1)

  expect_true(nrow(find.dups) == 0)
})

# Test if new alias have been added to the WQX domain table
test_that("Is TADA_GetWQXCharRef.rda up to date?", {
  old <- TADA_GetWQXCharAliasRef(download_only = FALSE, refresh = FALSE)
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetWQXCharAliasRef(download_only = TRUE, refresh = TRUE)
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))
  expect_true(old_latedate == new_latedate)
})
