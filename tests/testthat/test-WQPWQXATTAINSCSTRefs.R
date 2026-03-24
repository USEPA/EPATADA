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

  file_path <- system.file(
    "extdata",
    "WQXCharAliasRef.rda",
    package = "EPATADA"
  )
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)
  old <- e$WQXCharAliasRef

  # Parse m/d/Y to Date
  old_dates <- as.Date(old$Last.Change.Date, format = "%m/%d/%Y")

  ref <- EPATADA::TADA_GetWQXCharAliasRef(download_only = TRUE, refresh = TRUE)
  new_dates <- as.Date(ref$Last.Change.Date, format = "%m/%d/%Y")

  # Optional sanity checks to avoid -Inf if parsing fails
  testthat::expect_true(any(!is.na(old_dates)))
  testthat::expect_true(any(!is.na(new_dates)))

  testthat::expect_equal(
    max(old_dates, na.rm = TRUE),
    max(new_dates, na.rm = TRUE)
  )
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

testthat::test_that("CharacteristicRef updater errors on unexpected structure", {
  ns <- asNamespace("EPATADA")
  bad_df <- data.frame(foo = "", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) bad_df,
    .env = ns
  )
  testthat::expect_error(
    EPATADA:::.TADA_UpdateCharacteristicRef(),
    "Unexpected columns"
  )
})

testthat::test_that("CharacteristicRef normalizer keeps exact columns and trims/dedups", {
  ns <- asNamespace("EPATADA")
  norm <- get(".TADA_normalize_characteristic_ref", envir = ns)

  live <- data.frame(
    Name = c("  Ch1 ", "Ch1 "),
    `Comparable.Name` = " CN ",
    `CAS.Number` = " 123 ",
    `Domain.Value.Status` = c("A", "A"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out <- norm(live)

  testthat::expect_identical(
    names(out),
    c("CharacteristicName", "Comparable.Name", "CAS.Number", "Char_Flag")
  )
  testthat::expect_equal(nrow(out), 1L) # unique() removes duplicate row
  testthat::expect_equal(out$CharacteristicName, "Ch1")
  testthat::expect_equal(out$Comparable.Name, "CN")
  testthat::expect_equal(out$CAS.Number, "123")
  testthat::expect_equal(out$Char_Flag, "A")
})

testthat::test_that("WQP Organization: column selection + fallback", {
  ns <- asNamespace("EPATADA")

  # Live returns extra columns
  live <- data.frame(
    OrganizationIdentifier = "ID",
    OrganizationFormalName = " Name ",
    ProviderName = " WQP ",
    Extra = "x",
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) live,
    .env = ns
  )
  out <- EPATADA::TADA_GetWQPOrganizationRef(refresh = TRUE)
  testthat::expect_identical(
    names(out),
    c("OrganizationIdentifier", "OrganizationFormalName", "ProviderName")
  )
  testthat::expect_equal(out$OrganizationFormalName, "Name")
  testthat::expect_equal(out$ProviderName, "WQP")

  # Live fails, fallback used
  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) NULL,
    .tada_load_extdata_rda = function(...) {
      live[, c(
        "OrganizationIdentifier",
        "OrganizationFormalName",
        "ProviderName"
      )]
    },
    .env = ns
  )
  out2 <- EPATADA::TADA_GetWQPOrganizationRef(refresh = TRUE)
  testthat::expect_true(is.data.frame(out2))
})

testthat::test_that("Detection Condition classifier assigns expected flags + legacy rows", {
  ns <- asNamespace("EPATADA")
  f <- get(".TADA_flag_DetCondRef", envir = ns)
  df <- data.frame(
    Name = c(
      "Not Detected",
      "Present Above Quantification Limit",
      "Value Decensored",
      NA
    ),
    Description = "x",
    stringsAsFactors = FALSE
  )
  out <- f(df)
  testthat::expect_setequal(
    unique(out$TADA.Detection_Type),
    c("Non-Detect", "Over-Detect", "Other", "Not Reviewed")
  )
  testthat::expect_true(all(
    c("*Non-detect", "*Present <QL", "*Present") %in% out$Name
  ))
})

testthat::test_that("Detection Limit Type classifier assigns expected flags", {
  ns <- asNamespace("EPATADA")
  f <- get(".TADA_flag_DetLimitRef", envir = ns)
  df <- data.frame(
    Name = c(
      "Method Detection Level",
      "Upper Reporting Limit",
      "Measurement Uncertainty",
      NA
    ),
    Description = "x",
    stringsAsFactors = FALSE
  )
  out <- f(df)
  testthat::expect_setequal(
    unique(out$TADA.Limit_Type),
    c("Non-Detect", "Over-Detect", "Other", "Not Reviewed")
  )
})

testthat::test_that("ActivityType classifier by Code handles QC bins and Non_QC", {
  ns <- asNamespace("EPATADA")
  f <- get(".TADA_flag_ActivityTypeRef", envir = ns)
  df <- data.frame(
    Code = c(
      "Quality Control Sample-Field Blank",
      "Quality Control Sample-Inter-lab Split",
      "Quality Control Field Calibration Check",
      "Quality Control Sample-Other",
      "Sample-Routine",
      "Unknown"
    ),
    Description = "x",
    stringsAsFactors = FALSE
  )
  out <- f(df)
  m <- setNames(out$TADA.ActivityType.Flag, out$Code)
  testthat::expect_equal(m[["Quality Control Sample-Field Blank"]], "QC_blank")
  testthat::expect_equal(
    m[["Quality Control Sample-Inter-lab Split"]],
    "QC_duplicate"
  )
  testthat::expect_equal(
    m[["Quality Control Field Calibration Check"]],
    "QC_calibration"
  )
  testthat::expect_equal(m[["Quality Control Sample-Other"]], "QC_other")
  testthat::expect_equal(m[["Sample-Routine"]], "Non_QC")
  testthat::expect_equal(m[["Unknown"]], "Not Reviewed")
})

testthat::test_that("Monitoring Location Type flag assigns Surface Water / Groundwater", {
  ns <- asNamespace("EPATADA")
  f <- get(".TADA_flag_MonLocTypeRef", envir = ns)
  df <- data.frame(
    Name = c("Lake", "Well", "Other-Surface Water", "Other-X"),
    stringsAsFactors = FALSE
  )
  out <- f(df)
  m <- setNames(out$TADA.Media.Flag, out$Name)
  testthat::expect_equal(m[["Lake"]], "Surface Water")
  testthat::expect_equal(m[["Well"]], "Groundwater")
  testthat::expect_equal(m[["Other-Surface Water"]], "Surface Water")
  testthat::expect_equal(m[["Other-X"]], "")
})

testthat::test_that("Measure Qualifier Code classifier flags Pass/Suspect/Non-Detect/Over-Detect and NA -> Pass", {
  ns <- asNamespace("EPATADA")
  f <- get(".TADA_flag_MeasureQualifierCodeRef", envir = ns)
  df <- data.frame(Code = c("P", "AR", "U", "GT", NA), stringsAsFactors = FALSE)
  out <- f(df)
  m <- setNames(
    out$TADA.MeasureQualifierCode.Flag,
    ifelse(is.na(out$Code), "<NA>", out$Code)
  )
  testthat::expect_equal(m[["P"]], "Pass")
  testthat::expect_equal(m[["AR"]], "Suspect")
  testthat::expect_equal(m[["U"]], "Non-Detect")
  testthat::expect_equal(m[["GT"]], "Over-Detect")
  testthat::expect_true(any(
    out$TADA.MeasureQualifierCode.Flag == "Pass" & is.na(out$Code)
  ))
})

# CST getters + helpers

testthat::test_that("Getters read, normalize, and cache results (classic and fuzzy names)", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }
  legend_in <- data.frame(
    A = c(" a ", "a "),
    B = c(" x ", "x "),
    stringsAsFactors = FALSE
  )
  sources_in <- data.frame(
    Src = c(" s1 ", "s1 "),
    More = c(" y  ", "y"),
    stringsAsFactors = FALSE
  )
  criteria_in <- data.frame(
    C1 = c(" val ", "val "),
    C2 = c(" z ", "z "),
    stringsAsFactors = FALSE
  )

  expected_legend <- unique(data.frame(
    A = "a",
    B = "x",
    stringsAsFactors = FALSE
  ))
  expected_sources <- unique(data.frame(
    Src = "s1",
    More = "y",
    stringsAsFactors = FALSE
  ))
  expected_criteria <- unique(data.frame(
    C1 = "val",
    C2 = "z",
    stringsAsFactors = FALSE
  ))

  # Inline make_cst_xlsx
  wb_path <- tempfile(fileext = ".xlsx")
  x <- list(legend_in, sources_in, criteria_in)
  names(x) <- c("LEGEND notes", "SourCes", "criteria_table")
  openxlsx::write.xlsx(x, file = wb_path, asTable = FALSE, overwrite = TRUE)

  # Inline mock_resolver_to_path
  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(
      download_only = FALSE,
      refresh = FALSE,
      pkg = "EPATADA",
      on_fail_message = NULL
    ) {
      if (!download_only) {
        get(".tada_cache_set", envir = ns)("CST_workbook_path", wb_path)
      }
      wb_path
    },
    .env = ns
  )

  out_legend <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  out_sources <- EPATADA::TADA_CST_GetSources(refresh = TRUE)
  out_criteria <- EPATADA::TADA_CST_GetCriteria(refresh = TRUE)

  testthat::expect_equal(out_legend, expected_legend)
  testthat::expect_equal(out_sources, expected_sources)
  testthat::expect_equal(out_criteria, expected_criteria)

  # Inline get_cache_value
  testthat::expect_true(is.data.frame(get(".tada_cache_get", envir = ns)(
    "CST_legend_df"
  )))
  testthat::expect_true(is.data.frame(get(".tada_cache_get", envir = ns)(
    "CST_sources_df"
  )))
  testthat::expect_true(is.data.frame(get(".tada_cache_get", envir = ns)(
    "CST_criteria_df"
  )))

  wp <- get(".tada_cache_get", envir = ns)("CST_workbook_path")
  testthat::expect_true(is.character(wp) && length(wp) == 1 && nzchar(wp))

  # Second call without refresh returns from cache
  out_legend2 <- EPATADA::TADA_CST_GetLegend(refresh = FALSE)
  testthat::expect_identical(out_legend2, expected_legend)
})

testthat::test_that("download_only = TRUE returns data but does not populate cache", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }

  legend_df <- data.frame(A = "  foo ", stringsAsFactors = FALSE)
  sources_df <- data.frame(Src = " bar ", stringsAsFactors = FALSE)
  criteria_df <- data.frame(C1 = " baz ", stringsAsFactors = FALSE)

  wb_path <- tempfile(fileext = ".xlsx")
  x <- list(legend_df, sources_df, criteria_df)
  names(x) <- c("Legend", "Sources", "Criteria")
  openxlsx::write.xlsx(x, file = wb_path, asTable = FALSE, overwrite = TRUE)

  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(
      download_only = FALSE,
      refresh = FALSE,
      ...
    ) {
      if (!download_only) {
        get(".tada_cache_set", envir = ns)("CST_workbook_path", wb_path)
      }
      wb_path
    },
    .env = ns
  )

  out <- EPATADA::TADA_CST_GetLegend(download_only = TRUE, refresh = FALSE)
  testthat::expect_equal(out, data.frame(A = "foo", stringsAsFactors = FALSE))
  testthat::expect_null(get(".tada_cache_get", envir = ns)("CST_legend_df"))
  testthat::expect_null(get(".tada_cache_get", envir = ns)("CST_workbook_path"))
})

testthat::test_that("refresh = TRUE bypasses cached data and updates it", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }

  wb1 <- tempfile(fileext = ".xlsx")
  x1 <- list(
    data.frame(A = "old", stringsAsFactors = FALSE),
    data.frame(Src = "old", stringsAsFactors = FALSE),
    data.frame(C1 = "old", stringsAsFactors = FALSE)
  )
  names(x1) <- c("Legend", "Sources", "Criteria")
  openxlsx::write.xlsx(x1, file = wb1, asTable = FALSE, overwrite = TRUE)

  wb2 <- tempfile(fileext = ".xlsx")
  x2 <- list(
    data.frame(A = "new", stringsAsFactors = FALSE),
    data.frame(Src = "new", stringsAsFactors = FALSE),
    data.frame(C1 = "new", stringsAsFactors = FALSE)
  )
  names(x2) <- c("Legend", "Sources", "Criteria")
  openxlsx::write.xlsx(x2, file = wb2, asTable = FALSE, overwrite = TRUE)

  current_path <- wb1
  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(
      download_only = FALSE,
      refresh = FALSE,
      ...
    ) {
      if (!download_only) {
        get(".tada_cache_set", envir = ns)("CST_workbook_path", current_path)
      }
      current_path
    },
    .env = ns
  )

  out1 <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  testthat::expect_equal(out1, data.frame(A = "old", stringsAsFactors = FALSE))

  current_path <- wb2
  out2 <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  testthat::expect_equal(out2, data.frame(A = "new", stringsAsFactors = FALSE))
  testthat::expect_equal(
    get(".tada_cache_get", envir = ns)("CST_legend_df"),
    data.frame(A = "new", stringsAsFactors = FALSE)
  )
})

testthat::test_that("Resolves new CST naming: base=(legend), (2)=(sources), (3)=(criteria)", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }

  wb <- tempfile(fileext = ".xlsx")
  x <- list(
    data.frame(Lcol = "L_new", stringsAsFactors = FALSE),
    data.frame(Scol = "S_new", stringsAsFactors = FALSE),
    data.frame(Ccol = "C_new", stringsAsFactors = FALSE)
  )
  names(x) <- c(
    "Search Tool Criteria Data",
    "Search Tool Criteria Data (2)",
    "Search Tool Criteria Data (3)"
  )
  openxlsx::write.xlsx(x, file = wb, asTable = FALSE, overwrite = TRUE)

  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(
      download_only = FALSE,
      refresh = FALSE,
      ...
    ) {
      if (!download_only) {
        get(".tada_cache_set", envir = ns)("CST_workbook_path", wb)
      }
      wb
    },
    .env = ns
  )

  l <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  s <- EPATADA::TADA_CST_GetSources(refresh = TRUE)
  c <- EPATADA::TADA_CST_GetCriteria(refresh = TRUE)

  testthat::expect_equal(
    l,
    data.frame(Lcol = "L_new", stringsAsFactors = FALSE)
  )
  testthat::expect_equal(
    s,
    data.frame(Scol = "S_new", stringsAsFactors = FALSE)
  )
  testthat::expect_equal(
    c,
    data.frame(Ccol = "C_new", stringsAsFactors = FALSE)
  )
})

testthat::test_that("Errors when sheet names are unrecognized (no index fallback)", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }

  wb <- tempfile(fileext = ".xlsx")
  x <- list(
    data.frame(L = "Legend_by_index", stringsAsFactors = FALSE),
    data.frame(S = "Sources_by_index", stringsAsFactors = FALSE),
    data.frame(C = "Criteria_by_index", stringsAsFactors = FALSE)
  )
  names(x) <- c("AAA", "BBB", "CCC")
  openxlsx::write.xlsx(x, file = wb, asTable = FALSE, overwrite = TRUE)

  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(
      download_only = FALSE,
      refresh = FALSE,
      ...
    ) {
      if (!download_only) {
        get(".tada_cache_set", envir = ns)("CST_workbook_path", wb)
      }
      wb
    },
    .env = ns
  )

  suppressWarnings(testthat::expect_error(
    EPATADA::TADA_CST_GetLegend(refresh = TRUE),
    "Failed to read Legend sheet"
  ))
  suppressWarnings(testthat::expect_error(
    EPATADA::TADA_CST_GetSources(refresh = TRUE),
    "Failed to read Sources sheet"
  ))
  suppressWarnings(testthat::expect_error(
    EPATADA::TADA_CST_GetCriteria(refresh = TRUE),
    "Failed to read Criteria sheet"
  ))
})

testthat::test_that(".TADA_CST_UpdateWorkbook delegates to write helper", {
  ns <- asNamespace("EPATADA")
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }

  fake_src <- tempfile(fileext = ".xlsx")
  file.create(fake_src)

  called <- FALSE
  captured_src <- NULL
  captured_norm <- NULL

  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(
      download_only = TRUE,
      refresh = TRUE,
      ...
    ) {
      fake_src
    },
    .tada_cst_write_ext_workbook_if_changed = function(
      src_path,
      pkg = "EPATADA",
      filename = "cst-workbook.xlsx",
      normalize_tabs = TRUE
    ) {
      called <<- TRUE
      captured_src <<- src_path
      captured_norm <<- normalize_tabs
      tempfile()
    },
    .env = ns
  )

  res <- EPATADA:::.TADA_CST_UpdateWorkbook()
  testthat::expect_true(called)
  testthat::expect_identical(captured_src, fake_src)
  testthat::expect_true(isTRUE(captured_norm))
  testthat::expect_identical(res, fake_src)
})

testthat::test_that("ReportDateTime is extracted from Legend", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")

  # Inline make_legend_with_report_dt
  legend <- data.frame(
    X1 = c("ReportDateTime", "Other"),
    X2 = c("2024-12-31 23:59:59", "ignore"),
    stringsAsFactors = FALSE
  )

  wb <- tempfile(fileext = ".xlsx")
  x <- list(legend, data.frame(S = 1), data.frame(C = 1))
  names(x) <- c("Legend", "Sources", "Criteria")
  openxlsx::write.xlsx(x, file = wb, asTable = FALSE, overwrite = TRUE)

  f <- get(".tada_cst_get_report_datetime", envir = ns)
  testthat::expect_identical(f(wb), "2024-12-31 23:59:59")
})

testthat::test_that("Normalized copy renames new CST naming to classic", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")

  wb <- tempfile(fileext = ".xlsx")
  x <- list(data.frame(L = 1), data.frame(S = 1), data.frame(C = 1))
  names(x) <- c(
    "Search Tool Criteria Data",
    "Search Tool Criteria Data (2)",
    "Search Tool Criteria Data (3)"
  )
  openxlsx::write.xlsx(x, file = wb, asTable = FALSE, overwrite = TRUE)

  g <- get(".tada_cst_make_normalized_copy", envir = ns)
  tmp <- g(wb)
  testthat::expect_true(file.exists(tmp) || is.null(tmp))
  if (!is.null(tmp)) {
    sheets <- openxlsx::getSheetNames(tmp)
    testthat::expect_true(all(c("Legend", "Sources", "Criteria") %in% sheets))
  }
})

testthat::test_that("Workbook path is cached and reused when refresh = FALSE", {
  ns <- asNamespace("EPATADA")
  testthat::skip_if_not_installed("openxlsx")
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }

  wb <- tempfile(fileext = ".xlsx")
  x <- list(data.frame(L = 1), data.frame(S = 1), data.frame(C = 1))
  names(x) <- c("Legend", "Sources", "Criteria")
  openxlsx::write.xlsx(x, file = wb, asTable = FALSE, overwrite = TRUE)

  # First resolution caches the path
  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(
      download_only = FALSE,
      refresh = FALSE,
      ...
    ) {
      get(".tada_cache_set", envir = ns)("CST_workbook_path", wb)
      wb
    },
    .env = ns
  )
  out1 <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  testthat::expect_true(is.data.frame(out1))

  # Second call with refresh = FALSE should reuse cache even if resolver would fail
  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(...) NULL, # would fail if called
    .env = ns
  )
  out2 <- EPATADA::TADA_CST_GetLegend(refresh = FALSE)
  testthat::expect_equal(out1, out2)
})

testthat::test_that("CST getters error clearly when workbook path cannot be retrieved", {
  ns <- asNamespace("EPATADA")

  testthat::local_mocked_bindings(
    .tada_cst_get_workbook_path = function(...) NULL,
    .env = ns
  )
  testthat::expect_error(
    EPATADA::TADA_CST_GetLegend(refresh = TRUE),
    "Failed to retrieve CST workbook"
  )
  testthat::expect_error(
    EPATADA::TADA_CST_GetSources(refresh = TRUE),
    "Failed to retrieve CST workbook"
  )
  testthat::expect_error(
    EPATADA::TADA_CST_GetCriteria(refresh = TRUE),
    "Failed to retrieve CST workbook"
  )
})

testthat::test_that("RDA writer saves and skips when unchanged", {
  ns <- asNamespace("EPATADA")

  tmp_pkg <- withr::local_tempdir()
  dir.create(
    file.path(tmp_pkg, "inst", "extdata"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines("Package: EPATADA", file.path(tmp_pkg, "DESCRIPTION"))
  f <- get(".tada_save_ext_rda", envir = ns)

  df1 <- data.frame(a = 1, b = "x", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    .tada_find_pkg_root = function(...) tmp_pkg,
    .env = ns
  )

  path <- f(df1, obj_name = "OBJ", pkg = "EPATADA", filename = "obj.rda")
  testthat::expect_true(file.exists(path))
  testthat::expect_message(
    f(df1, obj_name = "OBJ", pkg = "EPATADA", filename = "obj.rda"),
    "No changes detected"
  )

  df2 <- transform(df1, a = 2)
  testthat::expect_message(
    f(df2, obj_name = "OBJ", pkg = "EPATADA", filename = "obj.rda"),
    "saved to"
  )
})

# Live ATTAINS test: skip on CRAN, require internet and required packages
testthat::test_that("Is the saved ATTAINSOrgIDsRef up to date (live domain subset in fallback)?", {
  skip_on_cran()
  testthat::skip_if_not_installed("rExpertQuery")
  testthat::skip_if_not_installed("spsUtil")

  # Inline skip_if_offline
  if (!requireNamespace("curl", quietly = TRUE)) {
    testthat::skip("curl not installed")
  }
  if (!curl::has_internet()) {
    testthat::skip("No internet connection")
  }

  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))
  ref <- unique(ATTAINS.raw[, "name"])

  # Baseline from installed RDA
  file_path <- system.file(
    "extdata",
    "ATTAINSOrgIDsRef.rda",
    package = "EPATADA"
  )
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)
  old <- unique(e$ATTAINSOrgIDsRef[, "name"])

  # Ensure all live domain names are present in fallback (subset check)
  testthat::expect_true(all(ref %in% old))
})

# Requires dplyr for concise checks
testthat::test_that("WQXcharValRef has unique characteristic/media/unit/max/min rows for threshold functions", {
  testthat::skip_if_not_installed("dplyr")
  skip_on_cran()

  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  e <- new.env(parent = emptyenv())
  load(file_path, envir = e)

  unit.ref <- dplyr::filter(
    e$WQXcharValRef,
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

  testthat::expect_true(nrow(find.dups) == 0)
})

testthat::test_that(".tada_norm_colnames strips BOM and makes unique names", {
  ns <- asNamespace("EPATADA")
  f <- get(".tada_norm_colnames", envir = ns)
  df <- data.frame("\ufeffCol" = 1, "Col" = 2, check.names = FALSE)
  out <- f(df)
  testthat::expect_identical(names(out), c("Col", "Col.1"))
})

testthat::test_that("df_equal handles non-data.frames and name mismatch", {
  ns <- asNamespace("EPATADA")
  f <- get(".tada_df_equal", envir = ns)

  testthat::expect_true(f(1:3, 1:3))
  testthat::expect_false(f(1:3, 1:4))

  a <- data.frame(x = 1, y = 2)
  b <- data.frame(y = 2, x = 1)
  testthat::expect_true(f(a, b))

  c <- data.frame(x = 1, z = 2)
  testthat::expect_false(f(a, c))
})

testthat::test_that("tada_require_cols errors with context", {
  ns <- asNamespace("EPATADA")
  f <- get(".tada_require_cols", envir = ns)
  df <- data.frame(a = 1)
  testthat::expect_error(
    f(df, c("x", "y"), "MyTable"),
    "MyTable: missing required columns: x, y"
  )
})

testthat::test_that("DetLimitRef required cols enforced in download_only", {
  ns <- asNamespace("EPATADA")
  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) data.frame(NotName = 1),
    .env = ns
  )
  testthat::expect_error(
    EPATADA::TADA_GetDetLimitRef(download_only = TRUE, refresh = TRUE),
    "missing required columns"
  )
})

testthat::test_that("ActivityTypeRef required cols enforced in download_only", {
  ns <- asNamespace("EPATADA")
  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) data.frame(NotCode = 1),
    .env = ns
  )
  testthat::expect_error(
    EPATADA::TADA_GetActivityTypeRef(download_only = TRUE, refresh = TRUE),
    "missing required columns"
  )
})

testthat::test_that("download_or_extdata_rda emits message and fails when fallback invalid", {
  ns <- asNamespace("EPATADA")
  f <- get(".tada_download_or_extdata_rda", envir = ns)

  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) data.frame(bad = 1),
    .tada_load_extdata_rda = function(...) NULL,
    .env = ns
  )

  testthat::expect_error(
    f(
      "http://x",
      "fallback.rda",
      "OBJ",
      pkg = "EPATADA",
      required_cols = "Need",
      on_fail_message = "msg"
    ),
    "Fallback extdata 'fallback.rda'"
  )
})

testthat::test_that("safe bind rows promotes logical NA placeholders to other atomic types", {
  ns <- asNamespace("EPATADA")
  f <- get(".tada_bind_rows", envir = ns)
  d1 <- data.frame(a = 1L, b = 1.5, c = "x", stringsAsFactors = FALSE)
  d2 <- data.frame(a = NA, b = NA, c = NA, stringsAsFactors = FALSE)
  out <- f(d1, d2)
  testthat::expect_type(out$a, "integer")
  testthat::expect_type(out$b, "double")
  testthat::expect_type(out$c, "character")
  testthat::expect_true(is.na(out$a[2]) && is.na(out$b[2]) && is.na(out$c[2]))
})

testthat::test_that("flag_by_groups trims and first match wins", {
  ns <- asNamespace("EPATADA")
  f <- get(".tada_flag_by_groups", envir = ns)
  df <- data.frame(v = c(" A ", "B", NA), stringsAsFactors = TRUE)
  groups <- list("X" = c("A", "B"), "Y" = c("A"))
  out <- f(df, "v", "flag", groups, default = "D", na_default = "N")
  testthat::expect_identical(out$flag, c("X", "X", "N"))
})

testthat::test_that("CharacteristicRef getter loads internal RDA and caches", {
  ns <- asNamespace("EPATADA")
  EPATADA::TADA_ClearCache()

  fb <- data.frame(
    CharacteristicName = "X",
    Comparable.Name = "Y",
    CAS.Number = "Z",
    Char_Flag = "A",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .tada_load_extdata_rda = function(...) fb,
    .env = ns
  )

  out1 <- EPATADA::TADA_GetCharacteristicRef()
  testthat::expect_identical(out1, fb)

  # Second call should return from cache (no new loader calls)
  out2 <- EPATADA::TADA_GetCharacteristicRef()
  testthat::expect_identical(out2, fb)
})

testthat::test_that("CharacteristicRef updater normalizes and saves", {
  ns <- asNamespace("EPATADA")
  live <- data.frame(
    Name = "N",
    `Comparable.Name` = "C",
    `CAS.Number` = "1",
    `Domain.Value.Status` = "A",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  saved <- FALSE
  captured <- NULL

  testthat::local_mocked_bindings(
    .tada_read_csv_url = function(...) live,
    .tada_save_ext_rda = function(obj, obj_name, ...) {
      saved <<- TRUE
      captured <<- list(obj = obj, obj_name = obj_name)
      tempfile()
    },
    .env = ns
  )

  invisible(EPATADA:::.TADA_UpdateCharacteristicRef())
  testthat::expect_true(saved)
  testthat::expect_identical(captured$obj_name, "WQXCharacteristicRef")
  testthat::expect_identical(
    names(captured$obj),
    c("CharacteristicName", "Comparable.Name", "CAS.Number", "Char_Flag")
  )
})
