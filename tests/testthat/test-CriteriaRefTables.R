ns <- asNamespace("EPATADA")

get_fun <- function(fname) get(fname, envir = ns)

load_installed_rda <- function(filename, objname) {
  fp <- system.file("extdata", filename, package = "EPATADA")
  expect_true(nzchar(fp) && file.exists(fp),
              info = paste("Missing extdata:", filename))
  e <- new.env(parent = emptyenv())
  objs <- load(fp, envir = e)
  expect_true(objname %in% objs,
              info = paste("Object", objname, "not found in", filename))
  get(objname, envir = e)
}

test_that("Installed CST RDAs are present and valid", {
  for (res in list(
    list(file = "CriteriaSearchToolRef.rda", obj = "CriteriaSearchToolRef"),
    list(file = "LegendCSTRef.rda", obj = "LegendCSTRef"),
    list(file = "SourcesCSTRef.rda", obj = "SourcesCSTRef")
  )) {
    df <- load_installed_rda(res$file, res$obj)
    expect_s3_class(df, "data.frame")
    expect_true(ncol(df) >= 1)
  }
})

test_that("TADA_GetSourcesCSTRef returns a data.frame and caches results", {
  df1 <- TADA_GetSourcesCSTRef(download_only = FALSE, refresh = TRUE)
  expect_s3_class(df1, "data.frame")
  expect_true(ncol(df1) >= 1)
  df2 <- TADA_GetSourcesCSTRef(download_only = FALSE, refresh = FALSE)
  expect_true(identical(df1, df2))
})

test_that("Online SourcesCSTRef contains required columns (allow synonyms)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  
  current_df <- TADA_GetSourcesCSTRef(download_only = TRUE, refresh = TRUE)
  expect_s3_class(current_df, "data.frame")
  
  # Canonicalize names: trim, uppercase, replace non-alnum with underscore
  canon_names <- function(x) toupper(gsub("[^A-Za-z0-9]+", "_", trimws(x)))
  cur_cols <- canon_names(names(current_df))
  
  # Accept either legacy or new header names for each required key
  required_map <- list(
    CRIT_SOURCE_ID = c("CRIT_SOURCE_ID", "SOURCE_ID"),
    SOURCE         = c("SOURCE", "DOCUMENT_TITLE")
  )
  
  missing_required <- names(required_map)[vapply(required_map, function(syms) {
    all(!(syms %in% cur_cols))
  }, logical(1))]
  
  if (length(missing_required) > 0) {
    testthat::fail(paste(
      "Online SourcesCSTRef missing required columns (allowing synonyms):",
      paste(missing_required, collapse = ", "),
      "\nCurrent cols:", paste(sort(unique(cur_cols)), collapse = ", ")
    ))
  } else {
    testthat::expect_true(TRUE)
  }
})

test_that("Getters trim character columns and de-duplicate rows", {
  fake <- data.frame(
    A = c("alpha ", "alpha", "beta"),
    B = c(" x", "x", "y "),
    stringsAsFactors = FALSE
  )
  testthat::with_mocked_bindings(
    .env = ns,
    .tada_read_xlsx_url = function(url, sheet) fake, {
      df <- TADA_GetLegendCSTRef(download_only = TRUE, refresh = TRUE)
      expect_equal(nrow(df), 2)
      expect_false(any(grepl("\\s$", df$A)))
      expect_false(any(grepl("^\\s", df$B)))
    }
  )
})

test_that(".TADA_UpdateSourcesCSTRef rewrites when data changed", {
  pkg_root <- withr::local_tempdir()
  dir.create(file.path(pkg_root, "inst", "extdata"), recursive = TRUE)
  writeLines(c("Package: EPATADA", "Version: 0.0.0"), file.path(pkg_root, "DESCRIPTION"))
  
  out_path <- file.path(pkg_root, "inst", "extdata", "SourcesCSTRef.rda")
  baseline <- data.frame(
    SOURCE_ID = c("SRC001", "SRC002"),
    DOCUMENT_TITLE = c("Doc A", "Doc B"),
    stringsAsFactors = FALSE
  )
  save(baseline, file = out_path, compress = "xz", version = 2)
  old_hash <- tools::md5sum(out_path)
  
  changed <- data.frame(
    SOURCE_ID = c("SRC001", "SRC002", "SRC003"),
    DOCUMENT_TITLE = c("Doc A", "Doc B", "Doc C"),
    stringsAsFactors = FALSE
  )
  
  testthat::with_mocked_bindings(
    .env = ns,
    .tada_find_pkg_root = function(start = getwd(), pkg = "EPATADA") pkg_root,
    TADA_GetSourcesCSTRef = function(download_only = TRUE, refresh = TRUE) changed, {
      upd <- get_fun(".TADA_UpdateSourcesCSTRef")
      expect_message(upd(), regexp = "SourcesCSTRef saved to:")
    }
  )
  
  new_hash <- tools::md5sum(out_path)
  expect_false(identical(as.character(old_hash), as.character(new_hash)))
  
  e <- new.env(parent = emptyenv()); load(out_path, envir = e)
  expect_identical(get("SourcesCSTRef", envir = e), changed)
})

test_that(".TADA_UpdateSourcesCSTRef treats reordered/trim-only changes as unchanged", {
  pkg_root <- withr::local_tempdir()
  dir.create(file.path(pkg_root, "inst", "extdata"), recursive = TRUE)
  writeLines(c("Package: EPATADA", "Version: 0.0.0"), file.path(pkg_root, "DESCRIPTION"))
  
  out_path <- file.path(pkg_root, "inst", "extdata", "SourcesCSTRef.rda")
  baseline <- data.frame(
    SOURCE_ID = c("SRC001", "SRC002"),
    DOCUMENT_TITLE = c("Doc A", "Doc B"),
    stringsAsFactors = FALSE
  )
  save(baseline, file = out_path, compress = "xz", version = 2)
  old_hash <- tools::md5sum(out_path)
  
  changed_presentation <- data.frame(
    DOCUMENT_TITLE = c("Doc B ", " Doc A"),
    SOURCE_ID = c("SRC002", "SRC001"),
    stringsAsFactors = FALSE
  )
  
  testthat::with_mocked_bindings(
    .env = ns,
    .tada_find_pkg_root = function(start = getwd(), pkg = "EPATADA") pkg_root,
    TADA_GetSourcesCSTRef = function(download_only = TRUE, refresh = TRUE) changed_presentation, {
      upd <- get_fun(".TADA_UpdateSourcesCSTRef")
      expect_message(upd(), regexp = "No changes to\\s+SourcesCSTRef.*not writing")
    }
  )
  
  new_hash <- tools::md5sum(out_path)
  expect_identical(old_hash, new_hash)
})

