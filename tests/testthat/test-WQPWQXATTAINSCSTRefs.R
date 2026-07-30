testthat::test_that("Cache helpers: set, list, clear", {
  ns <- asNamespace("EPATADA")

  # Ensure clean cache
  EPATADA::TADA_ClearCache()
  testthat::expect_equal(EPATADA::TADA_ListCacheKeys(), character(0))

  # Set two keys via internal setter
  get(".tada_cache_set", envir = ns)("k1", 1L)
  get(".tada_cache_set", envir = ns)("k2", data.frame(a = 1))
  keys <- EPATADA::TADA_ListCacheKeys()
  testthat::expect_true(all(c("k1", "k2") %in% keys))

  # Clear cache
  EPATADA::TADA_ClearCache()
  testthat::expect_equal(EPATADA::TADA_ListCacheKeys(), character(0))
})

testthat::test_that("trim converts factors to character and trims", {
  ns <- asNamespace("EPATADA")
  .trim <- get(".tada_trim_char_cols", envir = ns)
  df <- data.frame(
    a = factor(c("  X ", "Y  ")),
    b = c(" A ", "  B"),
    c = 1:2,
    stringsAsFactors = TRUE
  )
  out <- .trim(df)
  testthat::expect_true(is.character(out$a))
  testthat::expect_equal(out$a, c("X", "Y"))
  testthat::expect_equal(out$b, c("A", "B"))
  testthat::expect_identical(out$c, 1:2)
})

testthat::test_that("safe bind rows handles Date/POSIXct/factors/logicals and tzone", {
  ns <- asNamespace("EPATADA")
  .bind <- get(".tada_bind_rows", envir = ns)

  d1 <- data.frame(
    d = as.Date("2020-01-01"),
    t = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
    f = factor("a"),
    x = 1,
    stringsAsFactors = TRUE
  )
  d2 <- data.frame(
    d = NA, # will be promoted to Date NA
    t = NA, # will be promoted to POSIXct NA (UTC)
    f = "b", # factor vs char
    y = "new_col", # new column only in d2
    stringsAsFactors = FALSE
  )
  out <- .bind(d1, d2)

  testthat::expect_true(inherits(out$d, "Date"))
  testthat::expect_true(inherits(out$t, "POSIXct"))
  testthat::expect_equal(attr(out$t, "tzone"), "UTC")
  testthat::expect_true(is.character(out$f))
  testthat::expect_true(all(c("x", "y") %in% names(out)))
  testthat::expect_equal(out$y[1], NA_character_)
  testthat::expect_equal(out$y[2], "new_col")
})

testthat::test_that("df_equal ignores row order and factor levels", {
  ns <- asNamespace("EPATADA")
  .eq <- get(".tada_df_equal", envir = ns)

  a <- data.frame(x = factor(c("b", "a")), y = c(2, 1))
  b <- data.frame(x = c("a", "b"), y = c(1, 2))
  testthat::expect_true(.eq(a, b))

  c <- transform(b, y = y + 1)
  testthat::expect_false(.eq(a, c))
})

testthat::test_that("Is TADA_GetDetCondRef up to date?", {
  skip_on_cran()
  skip_if_no_live_tests()

  file_path <- system.file(
    "extdata",
    "WQXResultDetectionConditionRef.rda",
    package = "EPATADA"
  )
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)
  old <- e$WQXResultDetectionConditionRef

  # Parse m/d/Y to Date
  old_dates <- as.Date(old$Last.Change.Date, format = "%m/%d/%Y")
  ref <- EPATADA::TADA_GetDetCondRef(download_only = TRUE, refresh = TRUE)
  new_dates <- as.Date(ref$Last.Change.Date, format = "%m/%d/%Y")

  # Optional sanity checks to avoid -Inf if all parsing fails
  testthat::expect_true(any(!is.na(old_dates)))
  testthat::expect_true(any(!is.na(new_dates)))

  old_latedate <- max(old_dates, na.rm = TRUE)
  new_latedate <- max(new_dates, na.rm = TRUE)

  testthat::expect_equal(old_latedate, new_latedate)
})

testthat::test_that("Is TADA_GetDetLimitRef up to date?", {
  skip_on_cran()
  skip_if_no_live_tests()

  file_path <- system.file(
    "extdata",
    "WQXDetectionQuantitationLimitTypeRef.rda",
    package = "EPATADA"
  )
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)
  old <- e$WQXDetectionQuantitationLimitTypeRef

  # Parse m/d/Y to Date
  old_dates <- as.Date(old$Last.Change.Date, format = "%m/%d/%Y")

  ref <- EPATADA::TADA_GetDetLimitRef(download_only = TRUE, refresh = TRUE)
  new_dates <- as.Date(ref$Last.Change.Date, format = "%m/%d/%Y")

  # Optional sanity checks to avoid -Inf if parsing ever fails
  testthat::expect_true(any(!is.na(old_dates)))
  testthat::expect_true(any(!is.na(new_dates)))

  old_latedate <- max(old_dates, na.rm = TRUE)
  new_latedate <- max(new_dates, na.rm = TRUE)

  testthat::expect_equal(old_latedate, new_latedate)
})

testthat::test_that("Is TADA_GetActivityTypeRef up to date?", {
  skip_on_cran()
  skip_if_no_live_tests()

  file_path <- system.file(
    "extdata",
    "WQXActivityTypeRef.rda",
    package = "EPATADA"
  )
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)
  old <- e$WQXActivityTypeRef

  # Parse m/d/Y to Date (safe if already Date)
  old_dates <- as.Date(old$Last.Change.Date, format = "%m/%d/%Y")

  ref <- EPATADA::TADA_GetActivityTypeRef(download_only = TRUE, refresh = TRUE)
  new_dates <- as.Date(ref$Last.Change.Date, format = "%m/%d/%Y")

  # Optional sanity checks
  testthat::expect_true(any(!is.na(old_dates)))
  testthat::expect_true(any(!is.na(new_dates)))

  old_latedate <- max(old_dates, na.rm = TRUE)
  new_latedate <- max(new_dates, na.rm = TRUE)

  testthat::expect_equal(old_latedate, new_latedate)
})

testthat::test_that("Is TADA_GetMeasureQualifierCodeRef up to date?", {
  skip_on_cran()
  skip_if_no_live_tests()

  file_path <- system.file(
    "extdata",
    "WQXMeasureQualifierCodeRef.rda",
    package = "EPATADA"
  )
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)
  old <- e$WQXMeasureQualifierCodeRef

  # Parse m/d/Y to Date (safe if already Date)
  old_dates <- as.Date(old$Last.Change.Date, format = "%m/%d/%Y")

  ref <- EPATADA::TADA_GetMeasureQualifierCodeRef(
    download_only = TRUE,
    refresh = TRUE
  )
  new_dates <- as.Date(ref$Last.Change.Date, format = "%m/%d/%Y")

  # Optional sanity checks to avoid -Inf if parsing fails
  testthat::expect_true(any(!is.na(old_dates)))
  testthat::expect_true(any(!is.na(new_dates)))

  testthat::expect_equal(
    max(old_dates, na.rm = TRUE),
    max(new_dates, na.rm = TRUE)
  )
})

testthat::test_that("Is TADA_GetWQXCharAliasRef up to date?", {
  skip_on_cran()
  skip_if_no_live_tests()

  file_path <- system.file(
    "extdata",
    "WQXCharAliasRef.rda",
    package = "EPATADA"
  )
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)
  old <- e$WQXCharAliasRef

  ref <- EPATADA::TADA_GetWQXCharAliasRef(download_only = TRUE, refresh = TRUE)

  .canonicalize_alias_ref <- function(df) {
    if (is.null(df) || !is.data.frame(df)) {
      return(df)
    }

    names(df) <- trimws(names(df))
    rownames(df) <- NULL
    df <- .tada_trim_char_cols(df)

    keep_cols <- intersect(
      c(
        "Domain",
        "Unique.Identifier",
        "Alias.Name",
        "Description",
        "Characteristic.Name",
        "Alias.Type.Name"
      ),
      names(df)
    )
    df <- df[, keep_cols, drop = FALSE]

    sort_keys <- intersect(
      c(
        "Domain",
        "Unique.Identifier",
        "Alias.Name",
        "Characteristic.Name",
        "Alias.Type.Name"
      ),
      names(df)
    )
    if (length(sort_keys) > 0) {
      ord <- do.call(
        order,
        c(df[sort_keys], list(na.last = TRUE, method = "radix"))
      )
      df <- df[ord, , drop = FALSE]
      rownames(df) <- NULL
    }

    df
  }

  old2 <- .canonicalize_alias_ref(old)
  ref2 <- .canonicalize_alias_ref(ref)

  testthat::expect_equal(old2, ref2)
})

testthat::test_that("MeasureUnitRef falls back when live fails, and errors if fallback invalid", {
  ns <- asNamespace("EPATADA")

  # Mock live download to fail
  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) NULL,
    .env = ns
  )

  # Successful fallback
  fallback_df <- data.frame(Code = "", Name = "", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    .tada_load_extdata_rda = function(...) fallback_df,
    .env = ns
  )
  out <- EPATADA::TADA_GetMeasureUnitRef(refresh = TRUE)
  testthat::expect_true(all(c("Code", "Name") %in% names(out)))

  # Fallback invalid -> error
  testthat::local_mocked_bindings(
    .tada_load_extdata_rda = function(...) NULL,
    .env = ns
  )
  testthat::expect_error(
    EPATADA::TADA_GetMeasureUnitRef(refresh = TRUE),
    "Fallback extdata"
  )
})

testthat::test_that("DetCondRef required cols enforced in download_only", {
  ns <- asNamespace("EPATADA")

  # Live download returns df missing Name
  bad_df <- data.frame(NotName = "", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) bad_df,
    .env = ns
  )
  testthat::expect_error(
    EPATADA::TADA_GetDetCondRef(download_only = TRUE, refresh = TRUE),
    "missing required columns"
  )
})

# ... rest of file unchanged ...
