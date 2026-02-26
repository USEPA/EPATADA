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

# Small helper to mock the resolver to return a specific workbook path
mock_resolver_to_path <- function(wb_path) {
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
}

# ============================================================
# Getters: read, normalize (trim + unique), cache; classic names
# ============================================================
testthat::test_that("Getters read, normalize, and cache results (classic names)", {
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
    sheet_names = c("LEGEND notes", "SourCes", "criteria_table")
  )

  mock_resolver_to_path(wb_path)

  out_legend <- EPATADA::TADA_CST_GetLegend(refresh = TRUE)
  out_sources <- EPATADA::TADA_CST_GetSources(refresh = TRUE)
  out_criteria <- EPATADA::TADA_CST_GetCriteria(refresh = TRUE)

  testthat::expect_equal(out_legend, expected_legend)
  testthat::expect_equal(out_sources, expected_sources)
  testthat::expect_equal(out_criteria, expected_criteria)

  # Cached entries exist
  testthat::expect_true(is.data.frame(get_cache_value("CST_legend_df")))
  testthat::expect_true(is.data.frame(get_cache_value("CST_sources_df")))
  testthat::expect_true(is.data.frame(get_cache_value("CST_criteria_df")))

  # Workbook path was cached
  wp <- get_cache_value("CST_workbook_path")
  testthat::expect_true(is.character(wp) && length(wp) == 1 && nzchar(wp))

  # Second call without refresh returns from cache
  out_legend2 <- EPATADA::TADA_CST_GetLegend(refresh = FALSE)
  testthat::expect_identical(out_legend2, expected_legend)
})

# ============================================================
# download_only semantics: no cache updated
# ============================================================
testthat::test_that("download_only = TRUE returns data but does not populate cache", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  wb_path <- make_cst_xlsx(
    legend_df = data.frame(A = "  foo "),
    sources_df = data.frame(Src = " bar "),
    criteria_df = data.frame(C1 = " baz "),
    sheet_names = c("Legend", "Sources", "Criteria")
  )

  mock_resolver_to_path(wb_path)

  out <- EPATADA::TADA_CST_GetLegend(download_only = TRUE, refresh = FALSE)
  testthat::expect_equal(out, data.frame(A = "foo", stringsAsFactors = FALSE))

  # Cache should be empty
  testthat::expect_null(get_cache_value("CST_legend_df"))
  testthat::expect_null(get_cache_value("CST_workbook_path"))
})

# ============================================================
# refresh = TRUE should bypass cached values and update it
# ============================================================
testthat::test_that("refresh = TRUE bypasses cached data and updates it", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  wb1 <- make_cst_xlsx(
    legend_df = data.frame(A = "old", stringsAsFactors = FALSE),
    sources_df = data.frame(Src = "old", stringsAsFactors = FALSE),
    criteria_df = data.frame(C1 = "old", stringsAsFactors = FALSE)
  )
  wb2 <- make_cst_xlsx(
    legend_df = data.frame(A = "new", stringsAsFactors = FALSE),
    sources_df = data.frame(Src = "new", stringsAsFactors = FALSE),
    criteria_df = data.frame(C1 = "new", stringsAsFactors = FALSE)
  )

  # Use a variable to switch resolver output
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
    get_cache_value("CST_legend_df"),
    data.frame(A = "new", stringsAsFactors = FALSE)
  )
})

# ============================================================
# Sheet selection: new CST naming
# ============================================================
testthat::test_that("Resolves new CST naming: base=(legend), (2)=(sources), (3)=(criteria)", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  wb <- make_cst_xlsx(
    legend_df = data.frame(Lcol = "L_new", stringsAsFactors = FALSE),
    sources_df = data.frame(Scol = "S_new", stringsAsFactors = FALSE),
    criteria_df = data.frame(Ccol = "C_new", stringsAsFactors = FALSE),
    sheet_names = c(
      "Search Tool Criteria Data", # legend
      "Search Tool Criteria Data (2)", # sources
      "Search Tool Criteria Data (3)" # criteria
    )
  )

  mock_resolver_to_path(wb)

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

# ============================================================
# Errors when sheet names are unrecognized
# ============================================================
testthat::test_that("Errors when sheet names are unrecognized (no index fallback)", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  wb <- make_cst_xlsx(
    legend_df = data.frame(L = "Legend_by_index", stringsAsFactors = FALSE),
    sources_df = data.frame(S = "Sources_by_index", stringsAsFactors = FALSE),
    criteria_df = data.frame(C = "Criteria_by_index", stringsAsFactors = FALSE),
    sheet_names = c("AAA", "BBB", "CCC")
  )

  mock_resolver_to_path(wb)

  testthat::expect_error(
    EPATADA::TADA_CST_GetLegend(refresh = TRUE),
    "Failed to read Legend sheet"
  )
  testthat::expect_error(
    EPATADA::TADA_CST_GetSources(refresh = TRUE),
    "Failed to read Sources sheet"
  )
  testthat::expect_error(
    EPATADA::TADA_CST_GetCriteria(refresh = TRUE),
    "Failed to read Criteria sheet"
  )
})

# ============================================================
# Fallback to installed workbook when download fails
# ============================================================
testthat::test_that("Falls back to internal workbook when download fails", {
  testthat::skip_if_not_installed("openxlsx")
  reset_cst_cache()

  # Build a fallback workbook with classic sheet names
  fallback_wb <- make_cst_xlsx(
    legend_df = data.frame(A = "from_fallback", stringsAsFactors = FALSE),
    sources_df = data.frame(Src = "from_fallback", stringsAsFactors = FALSE),
    criteria_df = data.frame(C1 = "from_fallback", stringsAsFactors = FALSE),
    sheet_names = c("Legend", "Sources", "Criteria")
  )

  # Mock the resolver to return the fallback workbook path
  mock_resolver_to_path(fallback_wb)

  # Now getters should read from the fallback workbook successfully
  out <- EPATADA::TADA_CST_GetCriteria(refresh = TRUE)
  testthat::expect_equal(
    out,
    data.frame(C1 = "from_fallback", stringsAsFactors = FALSE)
  )
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
