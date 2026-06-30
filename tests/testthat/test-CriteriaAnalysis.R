test_that("TADA_Analysis_Join_WQP_Criteria joins by ComparableDataIdentifier when available", {
  .data <- tibble::tibble(
    row_id = 1:4,
    `TADA.ComparableDataIdentifier` = c(
      "LEAD_TOTAL_MET1_UG/L",
      "MERCURY_DISSOLVED_MET2_UG/L",
      "COPPER_DISSOLVED__UG/L",
      "ZINC___UG/L"
    ),
    `TADA.CharacteristicName` = c("Lead", "Mercury", "Copper", "Zinc"),
    `TADA.ResultSampleFractionText` = c("Total", "Dissolved", "Dissolved", NA),
    `TADA.MethodSpeciationName` = c("Met1", "Met2", NA, NA),
    `TADA.ResultMeasure.MeasureUnitCode` = c("ug/L", "ug/L", "ug/L", "ug/L")
  )

  criteria <- tibble::tibble(
    `TADA.ComparableDataIdentifier` = c(
      "LEAD_TOTAL_MET1_UG/L",
      "MERCURY_DISSOLVED_MET2_UG/L"
    ),
    `TADA.CharacteristicName` = c("Lead", "Mercury"),
    `TADA.ResultSampleFractionText` = c("Total", "Dissolved"),
    `TADA.MethodSpeciationName` = c("Met1", "Met2"),
    `TADA.ResultMeasure.MeasureUnitCode` = c("ug/L", "ug/L"),
    crit_value = c(10, 20)
  )

  out <- TADA_Analysis_Join_WQP_Criteria(.data, criteria)

  expect_true(any(out$row_id == 1 & out$crit_value == 10))
  expect_true(any(out$row_id == 2 & out$crit_value == 20))
  expect_true("crit_value" %in% names(out))
})

test_that("TADA_Analysis_Join_WQP_Criteria falls back when ComparableDataIdentifier is missing from criteria", {
  data <- tibble::tibble(
    row_id = 1,
    `TADA.ComparableDataIdentifier` = "COPPER_DISSOLVED__UG/L",
    `TADA.CharacteristicName` = "Copper",
    `TADA.ResultSampleFractionText` = "Dissolved",
    `TADA.MethodSpeciationName` = NA_character_,
    `TADA.ResultMeasure.MeasureUnitCode` = "ug/L"
  )

  criteria <- tibble::tibble(
    `TADA.ComparableDataIdentifier` = NA_character_,
    `TADA.CharacteristicName` = "Copper",
    `TADA.ResultSampleFractionText` = "Dissolved",
    `TADA.MethodSpeciationName` = NA_character_,
    `TADA.ResultMeasure.MeasureUnitCode` = "ug/L",
    crit_value = 30
  )

  out <- TADA_Analysis_Join_WQP_Criteria(data, criteria)

  expect_equal(nrow(out), 1)
  expect_equal(out$crit_value, 30)
})

test_that("TADA_Analysis_Join_WQP_Criteria left-join semantics are preserved", {
  .data <- tibble::tibble(
    row_id = 1:2,
    `TADA.ComparableDataIdentifier` = c(
      "LEAD_TOTAL_MET1_UG/L",
      "NO_MATCH_TOTAL_MET1_UG/L"
    ),
    `TADA.CharacteristicName` = c("Lead", "No Match"),
    `TADA.ResultSampleFractionText` = c("Total", "Total"),
    `TADA.MethodSpeciationName` = c("Met1", "Met1"),
    `TADA.ResultMeasure.MeasureUnitCode` = c("ug/L", "ug/L")
  )

  criteria <- tibble::tibble(
    `TADA.ComparableDataIdentifier` = "LEAD_TOTAL_MET1_UG/L",
    `TADA.CharacteristicName` = "Lead",
    `TADA.ResultSampleFractionText` = "Total",
    `TADA.MethodSpeciationName` = "Met1",
    `TADA.ResultMeasure.MeasureUnitCode` = "ug/L",
    crit_value = 10
  )

  out <- TADA_Analysis_Join_WQP_Criteria(.data, criteria)

  expect_true(any(out$row_id == 1))
  expect_true(any(out$row_id == 2))
})

test_that("TADA_Analysis_Join_WQP_Criteria byChar = TRUE joins on characteristic only", {
  .data <- tibble::tibble(
    row_id = 1:3,
    `TADA.ComparableDataIdentifier` = c(
      "LEAD_TOTAL_MET1_UG/L",
      "LEAD_DISSOLVED_MET2_UG/L",
      "MERCURY_TOTAL_MET3_UG/L"
    ),
    `TADA.CharacteristicName` = c("Lead", "Lead", "Mercury"),
    `TADA.ResultSampleFractionText` = c("Total", "Dissolved", "Total"),
    `TADA.MethodSpeciationName` = c("Met1", "Met2", "Met3"),
    `TADA.ResultMeasure.MeasureUnitCode` = c("ug/L", "ug/L", "ug/L")
  )

  criteria <- tibble::tibble(
    `TADA.ComparableDataIdentifier` = c(
      "LEAD_TOTAL_METX_UG/L",
      "LEAD_DISSOLVED_METY_UG/L",
      "MERCURY_TOTAL_METZ_UG/L"
    ),
    `TADA.CharacteristicName` = c("Lead", "Lead", "Mercury"),
    `TADA.ResultSampleFractionText` = c("Total", "Dissolved", "Total"),
    `TADA.MethodSpeciationName` = c("MetX", "MetY", "MetZ"),
    `TADA.ResultMeasure.MeasureUnitCode` = c("ug/L", "ug/L", "ug/L"),
    crit_value = c(11, 22, 33)
  )

  out <- TADA_Analysis_Join_WQP_Criteria(.data, criteria, byChar = TRUE)

  expect_equal(sum(out$`TADA.CharacteristicName` == "LEAD"), 4)
  expect_true(any(
    out$`TADA.CharacteristicName` == "MERCURY" & out$crit_value == 33
  ))
})

test_that("TADA_Analysis_Join_WQP_Criteria returns original data when no criteria match", {
  .data <- tibble::tibble(
    row_id = 1,
    `TADA.ComparableDataIdentifier` = "NOMATCH_TOTAL_MET1_UG/L",
    `TADA.CharacteristicName` = "NoMatch",
    `TADA.ResultSampleFractionText` = "Total",
    `TADA.MethodSpeciationName` = "Met1",
    `TADA.ResultMeasure.MeasureUnitCode` = "ug/L"
  )

  criteria <- tibble::tibble(
    `TADA.ComparableDataIdentifier` = "LEAD_TOTAL_MET1_UG/L",
    `TADA.CharacteristicName` = "Lead",
    `TADA.ResultSampleFractionText` = "Total",
    `TADA.MethodSpeciationName` = "Met1",
    `TADA.ResultMeasure.MeasureUnitCode` = "ug/L",
    crit_value = 1
  )

  out <- TADA_Analysis_Join_WQP_Criteria(.data, criteria)

  expect_equal(nrow(out), 1)
  expect_equal(out$row_id, 1)
})
