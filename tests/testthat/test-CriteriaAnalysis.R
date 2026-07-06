testthat::test_that("TADA_Analysis_Join_WQP_Criteria joins by ComparableDataIdentifier when available", {
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

testthat::test_that("TADA_Analysis_Join_WQP_Criteria falls back when ComparableDataIdentifier is missing from criteria", {
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

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$crit_value, 30)
})

testthat::test_that("TADA_Analysis_Join_WQP_Criteria left-join semantics are preserved", {
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

testthat::test_that("TADA_Analysis_Join_WQP_Criteria byChar = TRUE joins on characteristic only", {
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

  testthat::expect_equal(sum(out$`TADA.CharacteristicName` == "LEAD"), 4)
  expect_true(any(
    out$`TADA.CharacteristicName` == "MERCURY" & out$crit_value == 33
  ))
})

testthat::test_that("TADA_Analysis_Join_WQP_Criteria returns original data when no criteria match", {
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

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$row_id, 1)
})


testthat::test_that("TADA_Analysis_Join_WQP_Criteria returns expected row counts after joining Data_Nutrients_UT data with an example criteria table", {
  # run key flagging functions
  Data_Nutrients_UT2 <- TADA_RunKeyFlagFunctions(Data_Nutrients_UT)
  # harmonize synonyms
  Data_Nutrients_UT_Harmonized <- TADA_HarmonizeSynonyms(Data_Nutrients_UT2)
  # use auto_assign to generate pre-filled criteria table
  testthat::expect_warning(
    UTAH_Criteria <- TADA_DefineCriteriaMethodology(
      Data_Nutrients_UT_Harmonized,
      org_id = "UTAHDWQ",
      auto_assign = TRUE,
      displayUniqueId = TRUE
    )[[1]]
  )
  # join the criteria table to the data frame
  UTAH_with_criteria <- TADA_Analysis_Join_WQP_Criteria(
    Data_Nutrients_UT_Harmonized,
    UTAH_Criteria
  )
  # all criteria has been filled out with TADA.ComparableDataIdentifier, look at all unique values and see if joins worked correctly
  criteria_counts <- TADA_FieldValuesTable(
    UTAH_Criteria,
    "TADA.ComparableDataIdentifier"
  )
  data_counts <- TADA_FieldValuesTable(
    Data_Nutrients_UT_Harmonized,
    "TADA.ComparableDataIdentifier"
  )

  data_criteria_counts <- data_counts |>
    dplyr::left_join(criteria_counts, by = "Value") |>
    dplyr::mutate(
      Count.y = dplyr::if_else(is.na(Count.y), 1, Count.y),
      total_count = Count.x * Count.y
    )

  testthat::expect_equal(
    sum(data_criteria_counts$total_count),
    nrow(UTAH_with_criteria)
  )
})
