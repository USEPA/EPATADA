# tests/testthat/test-cst.R

# Shorthand for internal symbols in EPATADA namespace
ns <- asNamespace("EPATADA")

# Helpers
make_cst_xlsx <- function(
  legend_df,
  sources_df,
  criteria_df,
  sheet_names = c("Legend", "Sources", "Criteria")
) {
  testthat::skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  x <- list(legend_df, sources_df, criteria_df)
  if (!is.null(sheet_names)) {
    names(x) <- sheet_names
  }
  openxlsx::write.xlsx(x, file = tmp, asTable = FALSE, overwrite = TRUE)
  tmp
}

# Reset the package-scoped cache env (clear contents of the locked binding)
reset_cst_cache <- function() {
  if (exists(".TADA_cache", envir = ns, inherits = FALSE)) {
    cache_env <- get(".TADA_cache", envir = ns, inherits = FALSE)
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
  }
}

get_cache_value <- function(key) {
  get(".tada_cache_get", envir = ns)(key)
}

# ============================================================
# Happy path: download succeeds, sheet names recognized,
# normalization + caching behavior
# ============================================================
testthat::test_that("Getters read, normalize (trim + unique), and cache results", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

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

  wb_path <- make_cst_xlsx(
    legend_in,
    sources_in,
    criteria_in,
    sheet_names = c("Legend", "Sources", "Criteria")
  )

  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) wb_path,
    .env = ns
  )

  out_legend <- EPATADA::TADA_CST_GetLegend(
    download_only = FALSE,
    refresh = TRUE
  )
  out_sources <- EPATADA::TADA_CST_GetSources(
    download_only = FALSE,
    refresh = TRUE
  )
  out_criteria <- EPATADA::TADA_CST_GetCriteria(
    download_only = FALSE,
    refresh = TRUE
  )

  testthat::expect_equal(out_legend, expected_legend)
  testthat::expect_equal(out_sources, expected_sources)
  testthat::expect_equal(out_criteria, expected_criteria)

  # Confirm cached entries exist
  testthat::expect_true(is.data.frame(get_cache_value("CST_legend_df")))
  testthat::expect_true(is.data.frame(get_cache_value("CST_sources_df")))
  testthat::expect_true(is.data.frame(get_cache_value("CST_criteria_df")))

  # Confirm workbook path was cached (non-empty character path)
  wp <- get_cache_value("CST_workbook_path")
  testthat::expect_true(is.character(wp) && length(wp) == 1 && nzchar(wp))

  # A second call without refresh returns from cache and does not re-download
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) {
      stop("Should not be called when cache is used")
    },
    .env = ns
  )
  out_legend2 <- EPATADA::TADA_CST_GetLegend(refresh = FALSE)
  testthat::expect_identical(out_legend2, expected_legend)
})

# ============================================================
# download_only semantics: no cache updated
# ============================================================
testthat::test_that("download_only = TRUE returns data but does not populate cache", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  legend_in <- data.frame(A = c("  foo "), stringsAsFactors = FALSE)
  sources_in <- data.frame(Src = c(" bar "), stringsAsFactors = FALSE)
  criteria_in <- data.frame(C1 = c(" baz "), stringsAsFactors = FALSE)

  wb_path <- make_cst_xlsx(
    legend_in,
    sources_in,
    criteria_in,
    sheet_names = c("Legend", "Sources", "Criteria")
  )
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) wb_path,
    .env = ns
  )

  out <- EPATADA::TADA_CST_GetLegend(download_only = TRUE, refresh = FALSE)
  testthat::expect_equal(out, data.frame(A = "foo", stringsAsFactors = FALSE))

  # Neither table cache nor workbook-path cache should be populated
  testthat::expect_null(get_cache_value("CST_legend_df"))
  testthat::expect_null(get_cache_value("CST_workbook_path"))
})

# ============================================================
# refresh = TRUE should bypass cached values and update cache
# ============================================================
testthat::test_that("refresh = TRUE bypasses cache and updates it", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  # First workbook (will be cached)
  wb1 <- make_cst_xlsx(
    legend_df = data.frame(A = "old", stringsAsFactors = FALSE),
    sources_df = data.frame(Src = "old", stringsAsFactors = FALSE),
    criteria_df = data.frame(C1 = "old", stringsAsFactors = FALSE),
    sheet_names = c("Legend", "Sources", "Criteria")
  )

  # Second workbook (new content)
  wb2 <- make_cst_xlsx(
    legend_df = data.frame(A = "new", stringsAsFactors = FALSE),
    sources_df = data.frame(Src = "new", stringsAsFactors = FALSE),
    criteria_df = data.frame(C1 = "new", stringsAsFactors = FALSE),
    sheet_names = c("Legend", "Sources", "Criteria")
  )

  # First, return wb1 and cache it
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) wb1,
    .env = ns
  )
  out1 <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  testthat::expect_equal(out1, data.frame(A = "old", stringsAsFactors = FALSE))

  # Now, mock to return wb2 and force refresh
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) wb2,
    .env = ns
  )
  out2 <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  testthat::expect_equal(out2, data.frame(A = "new", stringsAsFactors = FALSE))

  # Cache should reflect new
  testthat::expect_equal(
    get_cache_value("CST_legend_df"),
    data.frame(A = "new", stringsAsFactors = FALSE)
  )
})

# ============================================================
# Fallback to installed workbook when download fails
# ============================================================
testthat::test_that("Falls back to internal workbook when download fails", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  fallback_wb <- make_cst_xlsx(
    legend_df = data.frame(A = "from_fallback", stringsAsFactors = FALSE),
    sources_df = data.frame(Src = "from_fallback", stringsAsFactors = FALSE),
    criteria_df = data.frame(C1 = "from_fallback", stringsAsFactors = FALSE),
    sheet_names = c("Legend", "Sources", "Criteria")
  )

  # Simulate failed download + system.file returns our fallback path
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) NULL,
    system.file = function(..., package = NULL) fallback_wb,
    .env = ns
  )

  out <- EPATADA::TADA_CST_GetCriteria(refresh = TRUE)
  testthat::expect_equal(
    out,
    data.frame(C1 = "from_fallback", stringsAsFactors = FALSE)
  )
})

# ============================================================
# Error when both download and fallback fail
# ============================================================
testthat::test_that("Errors when download fails and no fallback workbook is found", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) NULL,
    system.file = function(...) "", # nothing found
    .env = ns
  )

  testthat::expect_error(
    EPATADA::TADA_CST_GetLegend(refresh = TRUE),
    "Failed to retrieve CST workbook"
  )
})

# ============================================================
# Sheet selection: by name (regex) and by index fallback
# ============================================================
testthat::test_that("Reads by sheet name using case-insensitive regex", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  # Names that should match anchored patterns: ^legend, ^sources, ^criteria
  wb <- make_cst_xlsx(
    legend_df = data.frame(Lcol = "L_by_name", stringsAsFactors = FALSE),
    sources_df = data.frame(Scol = "S_by_name", stringsAsFactors = FALSE),
    criteria_df = data.frame(Ccol = "C_by_name", stringsAsFactors = FALSE),
    sheet_names = c("LEGEND notes", "SourCes", "criteria_table")
  )

  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) wb,
    .env = ns
  )

  l <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  s <- EPATADA::TADA_CST_GetSources(refresh = TRUE)
  c <- EPATADA::TADA_CST_GetCriteria(refresh = TRUE)

  testthat::expect_equal(
    l,
    data.frame(Lcol = "L_by_name", stringsAsFactors = FALSE)
  )
  testthat::expect_equal(
    s,
    data.frame(Scol = "S_by_name", stringsAsFactors = FALSE)
  )
  testthat::expect_equal(
    c,
    data.frame(Ccol = "C_by_name", stringsAsFactors = FALSE)
  )
})

testthat::test_that("Falls back to fixed sheet index when names not present", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  # Sheet names do not match regex; must pick by index 1, 2, 3 respectively.
  legend_idx_df <- data.frame(Val = "Legend_by_index", stringsAsFactors = FALSE)
  sources_idx_df <- data.frame(
    Val = "Sources_by_index",
    stringsAsFactors = FALSE
  )
  criteria_idx_df <- data.frame(
    Val = "Criteria_by_index",
    stringsAsFactors = FALSE
  )

  wb <- make_cst_xlsx(
    legend_df = legend_idx_df,
    sources_df = sources_idx_df,
    criteria_df = criteria_idx_df,
    sheet_names = c("AAA", "BBB", "CCC")
  )

  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) wb,
    .env = ns
  )

  l <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  s <- EPATADA::TADA_CST_GetSources(refresh = TRUE)
  c <- EPATADA::TADA_CST_GetCriteria(refresh = TRUE)

  testthat::expect_equal(l, legend_idx_df)
  testthat::expect_equal(s, sources_idx_df)
  testthat::expect_equal(c, criteria_idx_df)
})

# ============================================================
# Workbook path resolution caching and refresh behavior
# ============================================================
testthat::test_that("Workbook path is cached and respects refresh flag", {
  # This test does not strictly need openxlsx; it tests the path resolver logic.
  reset_cst_cache()

  path1 <- tempfile(fileext = ".xlsx")
  file.create(path1)
  path2 <- tempfile(fileext = ".xlsx")
  file.create(path2)
  path3 <- tempfile(fileext = ".xlsx")
  file.create(path3)

  # First call: download returns path1 and gets cached
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) path1,
    .env = ns
  )
  p1 <- EPATADA:::.tada_cst_get_workbook_path(
    download_only = FALSE,
    refresh = TRUE
  )
  testthat::expect_identical(p1, path1)

  # Second call: even if download returns path2, with refresh=FALSE it should return cached path1
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) path2,
    .env = ns
  )
  p2 <- EPATADA:::.tada_cst_get_workbook_path(
    download_only = FALSE,
    refresh = FALSE
  )
  testthat::expect_identical(p2, path1)

  # Third call with refresh=TRUE should update to path3
  testthat::local_mocked_bindings(
    .tada_cst_download_workbook = function(url = NULL) path3,
    .env = ns
  )
  p3 <- EPATADA:::.tada_cst_get_workbook_path(
    download_only = FALSE,
    refresh = TRUE
  )
  testthat::expect_identical(p3, path3)
})

# ============================================================
# Dev-time updater: delegates to write helper and returns path invisibly
# ============================================================
testthat::test_that(".TADA_CST_UpdateWorkbook delegates to write helper", {
  reset_cst_cache()

  fake_src <- tempfile(fileext = ".xlsx")
  file.create(fake_src)

  called <- FALSE
  captured_src <- NULL

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
      filename = "cst-workbook.xlsx"
    ) {
      called <<- TRUE
      captured_src <<- src_path
      tempfile() # pretend to return a written path
    },
    .env = ns
  )

  res <- EPATADA:::.TADA_CST_UpdateWorkbook()
  testthat::expect_true(called)
  testthat::expect_identical(captured_src, fake_src)
  testthat::expect_identical(res, fake_src)
})
