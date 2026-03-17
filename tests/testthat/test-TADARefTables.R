# # It is impossible to include all combos in the internal synonym ref
# # This can be run to add more manually as time permits
# test_that("There aren't any char-frac-spec-unit combos not present in synonym reference",{
#   test <- TADA_RandomTestingData(choose_random_state = TRUE)
#   test1 <- TADA_RunKeyFlagFunctions(test, clean = TRUE)
#   ref <- TADA_GetSynonymRef()
#   ref_chars <- unique(ref$TADA.CharacteristicName)
#   test_chars <- unique(subset(test1, test1$TADA.CharacteristicName%in%ref_chars)[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName","TADA.ResultMeasure.MeasureUnitCode")])
#   test_chars_ref <- merge(test_chars, ref, all.x = TRUE)
#   new_combos <- subset(test_chars_ref, is.na(test_chars_ref$HarmonizationGroup))[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName","TADA.ResultMeasure.MeasureUnitCode")]
#   if(dim(new_combos)[1]>0){
#     print("New combinations found in random dataset test:")
#     print(new_combos)
#   }
#   expect_true(dim(new_combos)[1]==0)
# })

test_that("No combos were missed in NP key from harmonization table", {
  keys <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )

  normalize <- function(df) {
    df |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(keys),
        ~ {
          x <- as.character(.)
          x <- trimws(x)
          x[x == ""] <- NA_character_
          x[toupper(x) == "NONE"] <- NA_character_
          x
        }
      ))
  }

  # Fraction equivalence for matching: NA <-> UNFILTERED
  expand_fraction <- function(df) {
    out <- df
    add_unf <- df[is.na(df$TADA.ResultSampleFractionText), , drop = FALSE]
    if (nrow(add_unf) > 0) {
      add_unf$TADA.ResultSampleFractionText <- "UNFILTERED"
      out <- dplyr::bind_rows(out, add_unf)
    }
    add_na <- df[
      !is.na(df$TADA.ResultSampleFractionText) &
        toupper(df$TADA.ResultSampleFractionText) == "UNFILTERED",
      ,
      drop = FALSE
    ]
    if (nrow(add_na) > 0) {
      add_na$TADA.ResultSampleFractionText <- NA_character_
      out <- dplyr::bind_rows(out, add_na)
    }
    dplyr::distinct(out)
  }

  # Speciation equivalence for matching: NA <-> AS N/P
  expand_spec <- function(df) {
    out <- df
    add_na_spec <- df[
      !is.na(df$TADA.MethodSpeciationName) &
        toupper(df$TADA.MethodSpeciationName) %in% c("AS N", "AS P"),
      ,
      drop = FALSE
    ]
    if (nrow(add_na_spec) > 0) {
      add_na_spec$TADA.MethodSpeciationName <- NA_character_
      out <- dplyr::bind_rows(out, add_na_spec)
    }
    add_as <- df[is.na(df$TADA.MethodSpeciationName), , drop = FALSE]
    if (nrow(add_as) > 0) {
      add_asN <- add_as
      add_asN$TADA.MethodSpeciationName <- "AS N"
      add_asP <- add_as
      add_asP$TADA.MethodSpeciationName <- "AS P"
      out <- dplyr::bind_rows(out, add_asN, add_asP)
    }
    dplyr::distinct(out)
  }

  # Prepare NP keys (normalized + expansions)
  np <- TADA_GetNutrientSummationRef() |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::distinct() |>
    normalize() |>
    expand_fraction() |>
    expand_spec()

  # Prepare harmonization keys
  harm <- TADA_GetSynonymRef() |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::distinct() |>
    normalize() |>
    dplyr::semi_join(
      np |> dplyr::distinct(`TADA.CharacteristicName`),
      by = "TADA.CharacteristicName"
    )

  # Orphans: harmonization keys that aren’t covered by (expanded) NP keys
  orphs <- dplyr::anti_join(harm, np, by = keys, na_matches = "na") |>
    dplyr::distinct()

  if (nrow(orphs) > 0) {
    message(
      "Missing NP combos (after NA/UNFILTERED and NA/AS N/P equivalences):"
    )
    print(orphs)
  }

  expect_equal(nrow(orphs), 0)
})

test_that("np summation keys are a subset of nitrogen/phosphorus harmonization keys", {
  keys <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )

  normalize <- function(df) {
    df |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(keys),
        ~ {
          x <- as.character(.)
          x <- trimws(x)
          x[x == ""] <- NA_character_
          x[toupper(x) == "NONE"] <- NA_character_
          x
        }
      ))
  }

  harm <- TADA_GetSynonymRef() |>
    dplyr::filter(HarmonizationGroup %in% c("Phosphorus", "Nitrogen")) |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::distinct() |>
    normalize()

  np <- TADA_GetNutrientSummationRef() |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::distinct() |>
    normalize()

  missing_in_harm <- dplyr::anti_join(np, harm, by = keys, na_matches = "na")
  expect_equal(
    nrow(missing_in_harm),
    0,
    info = paste(
      "NP summation keys not found in harmonization reference:\n",
      paste(utils::capture.output(print(missing_in_harm)), collapse = "\n")
    )
  )
})

test_that("TADA_GetSynonymRef warns when QC flags are missing and preserves template columns", {
  df_in <- data.frame(
    "TADA.CharacteristicName" = c("Nitrate", "Nitrate"),
    "TADA.ResultSampleFractionText" = c("", "UNFILTERED"),
    "TADA.MethodSpeciationName" = c("NONE", NA),
    stringsAsFactors = FALSE
  )
  tmpl <- TADA_GetSynonymRef(NULL)
  expect_true(is.data.frame(tmpl))

  expect_warning(
    out <- TADA_GetSynonymRef(df_in),
    "missing TADA QC flagging columns",
    fixed = TRUE
  )
  expect_true(is.data.frame(out))
  expect_setequal(names(out), names(tmpl))
})

test_that("TADA_GetNutrientSummationRef normalizes keys: no empty strings; 'NONE' -> NA", {
  ref <- TADA_GetNutrientSummationRef()
  keys <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  for (k in keys) {
    skip_if_not(k %in% names(ref), paste("Missing key column:", k))
    expect_false(any(ref[[k]] %in% "", na.rm = TRUE))
  }
  none_present <- any(
    toupper(stats::na.omit(ref$TADA.MethodSpeciationName)) == "NONE"
  )
  expect_false(none_present)
})

test_that("Is the saved TADACharAliasRef.csv up to date?", {
  skip_on_cran()
  skip_if_not_installed("rExpertQuery")

  ATTAINS.raw <- suppressWarnings(suppressMessages(rExpertQuery::EQ_DomainValues(
    "param_name"
  )))
  ref <- unique(ATTAINS.raw[, "name"])
  old <- utils::read.csv(
    system.file("extdata", "TADACharAliasRef.csv", package = "EPATADA"),
    stringsAsFactors = FALSE
  )[, "ATTAINS.ParameterName"]
  # All current ATTAINS param names should be present in saved CSV
  missing <- setdiff(ref, old)
  expect_equal(
    length(missing),
    0L,
    info = paste(
      "New ATTAINS parameters not in TADACharAliasRef.csv:\n",
      paste(missing, collapse = "\n")
    )
  )
})

# -------------------------
# Cache + CSV-only behavior
# -------------------------

test_that("TADA_GetTADACharAliasRef caches by tolerance", {
  skip_on_cran()
  skip_if_not_installed("rExpertQuery")

  TADA_ClearCache()
  keys_before <- TADA_ListCacheKeys()

  ref1 <- TADA_GetTADACharAliasRef(
    ATTAINS.CST.tolerance = 1.0,
    CST.ATTAINS.tolerance = 1.0,
    ATTAINS.WQX.tolerance = 1.0,
    WQX.ATTAINS.tolerance = 1.0,
    CST.WQX.tolerance = 1.0,
    WQX.CST.tolerance = 1.0
  )
  keys_after1 <- TADA_ListCacheKeys()
  new_keys1 <- setdiff(keys_after1, keys_before)
  expect_true(any(grepl("^TADACharAliasRef\\|", new_keys1)))

  ref2 <- TADA_GetTADACharAliasRef(
    ATTAINS.CST.tolerance = 1.0,
    CST.ATTAINS.tolerance = 1.0,
    ATTAINS.WQX.tolerance = 1.0,
    WQX.ATTAINS.tolerance = 1.0,
    CST.WQX.tolerance = 1.0,
    WQX.CST.tolerance = 1.0
  )
  expect_identical(ref1, ref2)

  ref3 <- TADA_GetTADACharAliasRef(
    ATTAINS.CST.tolerance = 0.9,
    CST.ATTAINS.tolerance = 0.9,
    ATTAINS.WQX.tolerance = 0.9,
    WQX.ATTAINS.tolerance = 0.9,
    CST.WQX.tolerance = 0.9,
    WQX.CST.tolerance = 0.9
  )
  expect_true(is.data.frame(ref3))
  keys_after2 <- TADA_ListCacheKeys()
  new_keys2 <- setdiff(keys_after2, keys_after1)
  expect_true(any(grepl("^TADACharAliasRef\\|", new_keys2)))
})

test_that("set.all.tolerance propagates to all tolerance notes", {
  skip_on_cran()
  skip_if_not_installed("rExpertQuery")

  ref <- TADA_GetTADACharAliasRef(set.all.tolerance = 0.5)
  expect_true(is.data.frame(ref))
  expect_true("Status.Notes" %in% names(ref))

  notes <- stats::na.omit(ref$Status.Notes)
  skip_if(length(notes) == 0, "No Status.Notes present to validate.")
  has_all <- vapply(
    notes,
    function(s) {
      all(c(
        grepl("WQX\\.ATTAINS = 0\\.5", s),
        grepl("ATTAINS\\.WQX = 0\\.5", s),
        grepl("CST\\.WQX = 0\\.5", s),
        grepl("WQX\\.CST = 0\\.5", s),
        grepl("ATTAINS\\.CST = 0\\.5", s),
        grepl("CST\\.ATTAINS = 0\\.5", s)
      ))
    },
    logical(1)
  )
  expect_true(any(has_all))
})

test_that("TADA_GetTADAUsesAliasRef caches by tolerance", {
  skip_on_cran()
  skip_if_not_installed("rExpertQuery")

  TADA_ClearCache()
  keys_before <- TADA_ListCacheKeys()

  x1 <- TADA_GetTADAUsesAliasRef(
    ATTAINS.CST.tolerance = 0.15,
    CST.ATTAINS.tolerance = 0.15
  )
  keys_after1 <- TADA_ListCacheKeys()
  new_keys1 <- setdiff(keys_after1, keys_before)
  expect_true(any(grepl("^TADAUsesAliasRef\\|", new_keys1)))

  x2 <- TADA_GetTADAUsesAliasRef(
    ATTAINS.CST.tolerance = 0.15,
    CST.ATTAINS.tolerance = 0.15
  )
  expect_identical(x1, x2)

  invisible(TADA_GetTADAUsesAliasRef(
    ATTAINS.CST.tolerance = 0.5,
    CST.ATTAINS.tolerance = 0.5
  ))
  keys_after2 <- TADA_ListCacheKeys()
  new_keys2 <- setdiff(keys_after2, keys_after1)
  expect_true(any(grepl("^TADAUsesAliasRef\\|", new_keys2)))
})

test_that("TADA_GetTADACharAliasRef emits reviewed-rows or empty-review message", {
  skip_on_cran()
  skip_if_not_installed("rExpertQuery")

  TADA_ClearCache()
  expect_message(
    try(
      TADA_GetTADACharAliasRef(
        ATTAINS.CST.tolerance = 0.987,
        CST.ATTAINS.tolerance = 0.987,
        ATTAINS.WQX.tolerance = 0.987,
        WQX.ATTAINS.tolerance = 0.987,
        CST.WQX.tolerance = 0.987,
        WQX.CST.tolerance = 0.987
      ),
      silent = TRUE
    ),
    regexp = "TADACharAliasRef\\.csv|empty review list",
    perl = TRUE
  )
})

test_that("TADA_GetTADAUsesAliasRef emits reviewed-rows or empty-review message", {
  skip_on_cran()
  skip_if_not_installed("rExpertQuery")

  TADA_ClearCache()
  expect_message(
    try(
      TADA_GetTADAUsesAliasRef(
        ATTAINS.CST.tolerance = 0.321,
        CST.ATTAINS.tolerance = 0.321
      ),
      silent = TRUE
    ),
    regexp = "TADAUsesAliasRef\\.csv|empty review list",
    perl = TRUE
  )
})

# -------------------------
# Validation of tolerance guards
# -------------------------

test_that("TADA_GetTADACharAliasRef errors when tolerance exceeds 1.0", {
  expect_error(
    TADA_GetTADACharAliasRef(ATTAINS.WQX.tolerance = 1.2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(WQX.ATTAINS.tolerance = 2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(ATTAINS.CST.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(CST.ATTAINS.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(CST.WQX.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADACharAliasRef(WQX.CST.tolerance = 1.01),
    regexp = "Tolerance cannot exceed 100%"
  )
})

test_that("TADA_GetTADACharAliasRef errors when any tolerance is less than 0.0", {
  expect_error(
    TADA_GetTADACharAliasRef(ATTAINS.WQX.tolerance = -1.2),
    regexp = "Tolerance cannot be less than 0%."
  )
  expect_error(
    TADA_GetTADACharAliasRef(WQX.ATTAINS.tolerance = -2),
    regexp = "Tolerance cannot be less than 0%."
  )
  expect_error(
    TADA_GetTADACharAliasRef(ATTAINS.CST.tolerance = -0.01),
    regexp = "Tolerance cannot be less than 0%."
  )
  expect_error(
    TADA_GetTADACharAliasRef(CST.ATTAINS.tolerance = -0.01),
    regexp = "Tolerance cannot be less than 0%."
  )
  expect_error(
    TADA_GetTADACharAliasRef(CST.WQX.tolerance = -0.01),
    regexp = "Tolerance cannot be less than 0%."
  )
  expect_error(
    TADA_GetTADACharAliasRef(WQX.CST.tolerance = -1.01),
    regexp = "Tolerance cannot be less than 0%."
  )
})

test_that("TADA_GetTADAUsesAliasRef errors when tolerance exceeds 1.0", {
  expect_error(
    TADA_GetTADAUsesAliasRef(ATTAINS.CST.tolerance = 1.2),
    regexp = "Tolerance cannot exceed 100%"
  )
  expect_error(
    TADA_GetTADAUsesAliasRef(CST.ATTAINS.tolerance = 2),
    regexp = "Tolerance cannot exceed 100%"
  )
})

test_that("TADA_ClearCache empties cache", {
  invisible(TADA_GetCharacteristicRef()) # populate some key
  keys1 <- TADA_ListCacheKeys()
  expect_true(length(keys1) >= 1)

  TADA_ClearCache()
  keys2 <- TADA_ListCacheKeys()
  expect_identical(length(keys2), 0L)
})
