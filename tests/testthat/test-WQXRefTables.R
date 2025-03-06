# Test that new combinations of detection condition have not been added to domain tables
test_that("Is TADA_GetDetCondRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old <- utils::read.csv(system.file("extdata", "WQXResultDetectionConditionRef.csv", package = "EPATADA"))
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetDetCondRef()
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))

  expect_true(old_latedate == new_latedate)
})

# Test that new combinations of detection condition have not been added to domain tables
test_that("Is TADA_GetDetLimitRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old <- utils::read.csv(system.file("extdata", "WQXDetectionQuantitationLimitTypeRef.csv", package = "EPATADA"))
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetDetLimitRef()
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))

  expect_true(old_latedate == new_latedate)
})

# Test that new QC conditions have not been added to ActivityType domain table
test_that("Is TADA_GetActivityTypeRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old <- utils::read.csv(system.file("extdata", "WQXActivityTypeRef.csv", package = "EPATADA"))
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetActivityTypeRef()
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))

  expect_true(old_latedate == new_latedate)
})



# Test that new codes have not been added to ResultMeasureQualifier domain table
test_that("Is TADA_GetMeasureQualifierCodeRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old <- utils::read.csv(system.file("extdata", "WQXMeasureQualifierCodeRef.csv", package = "EPATADA"))
  old_latedate <- max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref <- TADA_GetMeasureQualifierCodeRef()
  new_latedate <- max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))

  expect_true(old_latedate == new_latedate)
})


# MORE ROBUST TEST FOR WQX VAL TABLE, UPDATED 1/8/25
test_that("WQXcharValRef.csv contains only one row for each unique characteristic/media/unit/max/min combination for threshold functions", {
  unit.ref <- utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "EPATADA")) %>%
    dplyr::filter(
      Type == "CharacteristicUnit"
    )
  
  find.dups <- unit.ref %>%
    dplyr::filter(Type == "CharacteristicUnit") %>%
    dplyr::group_by(Characteristic, Source, Value.Unit) %>%
    dplyr::mutate(
      Min_n = length(unique(Minimum)),
      Max_n = length(unique(Maximum))
    ) %>%
    dplyr::filter(Min_n > 1 |
                    Max_n > 1)
  
  expect_true(nrow(find.dups) == 0)
})

# test_that("WQXcharValRef.csv contains only one row for each unique characteristic/media/unit/max/min combination for threshold functions", {
#   unit.ref <- utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "EPATADA")) %>%
#     dplyr::filter(
#       Type == "CharacteristicUnit",
#       Status == "Accepted"
#     )
#
#   find.dups <- unit.ref %>%
#     dplyr::filter(Type == "CharacteristicUnit") %>%
#     dplyr::group_by(Characteristic, Source, Value.Unit) %>%
#     dplyr::mutate(
#       Min_n = length(unique(Minimum)),
#       Max_n = length(unique(Maximum))
#     ) %>%
#     dplyr::filter(Min_n > 1 |
#                     Max_n > 1)
#
#   expect_true(nrow(find.dups) == 0)
# })
