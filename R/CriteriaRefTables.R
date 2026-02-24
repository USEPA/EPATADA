# ============================================================
# EPATADA CST (Criteria Search Tool) utilities
# ============================================================
# - Consistent function names:
#   * TADA_CST_GetCriteria
#   * TADA_CST_GetLegend
#   * TADA_CST_GetSources
#   * TADA_CST_UpdateWorkbook (dev-time)
# - Download workbook once per session and reuse
# - Fallback to package-installed raw XLSX (inst/extdata/cst-workbook.xlsx)
# - No RDA files are read or written
# - Normalize: trim character cols + unique rows
# - Read sheets by name when present; otherwise fallback to fixed indices
#
# Requires: openxlsx

# =========================
# Shared cache + constants
# =========================

# Ensure session cache env exists if this file is sourced independently
if (!exists(".TADA_cache", inherits = FALSE)) {
  .TADA_cache <- new.env(parent = emptyenv())
}

# Lightweight cache helpers
if (!exists(".tada_cache_get", inherits = FALSE)) {
  .tada_cache_get <- function(key) .TADA_cache[[key]]
}
if (!exists(".tada_cache_set", inherits = FALSE)) {
  .tada_cache_set <- function(key, value) {
    .TADA_cache[[key]] <- value
    invisible(value)
  }
}
if (!exists(".tada_trim_char_cols", inherits = FALSE)) {
  .tada_trim_char_cols <- function(df) {
    df[] <- lapply(df, function(x) if (is.character(x)) trimws(x) else x)
    df
  }
}
if (!exists(".tada_find_pkg_root", inherits = FALSE)) {
  .tada_find_pkg_root <- function(start = getwd(), pkg = "EPATADA") {
    cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
    while (nchar(cur) > 0 && cur != dirname(cur)) {
      desc <- file.path(cur, "DESCRIPTION")
      if (file.exists(desc)) {
        dcf <- tryCatch(read.dcf(desc, all = TRUE), error = function(e) NULL)
        if (
          !is.null(dcf) && isTRUE(tolower(dcf[1, "Package"]) == tolower(pkg))
        ) {
          return(cur)
        }
      }
      cur <- dirname(cur)
    }
    NULL
  }
}

# CST authoritative source (XLSX)
.CST_WORKBOOK_URL <- "https://www.epa.gov/system/files/documents/2025-07/criteria-search-tool-data.xlsx"
# Package-installed fallback workbook filename (raw XLSX)
.CST_WORKBOOK_LOCAL_FILENAME <- "cst-workbook.xlsx"

# Session cache keys
.CST_CRITERIA_CACHE_KEY <- "CST_criteria_df"
.CST_LEGEND_CACHE_KEY <- "CST_legend_df"
.CST_SOURCES_CACHE_KEY <- "CST_sources_df"
.CST_WORKBOOK_PATH_CACHE_KEY <- "CST_workbook_path"

# =========================
# Workbook helpers
# =========================

# Download CST workbook to a tempfile and return the path
.tada_cst_download_workbook <- function(url = .CST_WORKBOOK_URL) {
  tf <- tempfile(fileext = ".xlsx")
  ok <- tryCatch(
    {
      utils::download.file(url, tf, mode = "wb", quiet = TRUE)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!ok) {
    return(NULL)
  }
  tf
}

# Resolve a local path to the CST workbook (download once per session).
# If download fails and download_only = FALSE, fallback to package-installed XLSX if present.
.tada_cst_get_workbook_path <- function(
  download_only = FALSE,
  refresh = FALSE,
  pkg = "EPATADA",
  on_fail_message = NULL
) {
  if (!download_only) {
    cached <- .tada_cache_get(.CST_WORKBOOK_PATH_CACHE_KEY)
    if (!is.null(cached) && file.exists(cached) && !isTRUE(refresh)) {
      return(cached)
    }
  }

  # Try to download the latest XLSX
  path <- .tada_cst_download_workbook(.CST_WORKBOOK_URL)
  if (!is.null(path) && file.exists(path)) {
    if (!download_only) {
      .tada_cache_set(.CST_WORKBOOK_PATH_CACHE_KEY, path)
    }
    return(path)
  }

  # If download_only, fail fast
  if (download_only) {
    stop("CST workbook download failed (download_only=TRUE).")
  }

  # Fallback to installed workbook if it exists
  if (!is.null(on_fail_message)) {
    message(on_fail_message)
  }
  fallback_path <- system.file(
    "extdata",
    .CST_WORKBOOK_LOCAL_FILENAME,
    package = pkg
  )
  if (nzchar(fallback_path) && file.exists(fallback_path)) {
    if (!download_only) {
      .tada_cache_set(.CST_WORKBOOK_PATH_CACHE_KEY, fallback_path)
    }
    return(fallback_path)
  }

  NULL
}

# Read a CST sheet by name when available; otherwise fallback to index
# target ∈ {"legend","sources","criteria"}
.tada_cst_read_sheet <- function(
  workbook_path,
  target = c("legend", "sources", "criteria")
) {
  target <- match.arg(target)
  sheet_index <- switch(target, legend = 1, sources = 2, criteria = 3)

  # Try to pick by sheet name
  sheet_name <- NULL
  snames <- tryCatch(
    openxlsx::getSheetNames(workbook_path),
    error = function(e) NULL
  )
  if (!is.null(snames)) {
    pattern <- switch(
      target,
      legend = "(?i)^legend",
      sources = "(?i)^sources",
      criteria = "(?i)^criteria"
    )
    m <- grep(pattern, snames)
    if (length(m) >= 1) sheet_name <- snames[m[1]]
  }

  # Read using chosen sheet name or fallback index
  tryCatch(
    openxlsx::read.xlsx(
      workbook_path,
      sheet = if (is.null(sheet_name)) sheet_index else sheet_name
    ),
    error = function(e) NULL
  )
}

# =========================
# Normalization
# =========================

.tada_cst_prepare_table <- function(df) {
  unique(.tada_trim_char_cols(df))
}

# =========================
# Dev-time: save raw XLSX to inst/extdata only when changed
# =========================

.tada_cst_write_ext_workbook_if_changed <- function(
  src_path,
  pkg = "EPATADA",
  filename = .CST_WORKBOOK_LOCAL_FILENAME
) {
  pkg_root <- .tada_find_pkg_root(pkg = pkg)
  if (is.null(pkg_root)) {
    stop(
      "Could not locate package source root for ",
      pkg,
      ". Run from the package source directory."
    )
  }
  out_path <- file.path(pkg_root, "inst", "extdata", filename)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  # Compare MD5 digests if dest exists
  same <- FALSE
  if (file.exists(out_path)) {
    old_md5 <- tryCatch(
      as.character(tools::md5sum(out_path)),
      error = function(e) NA_character_
    )
    new_md5 <- tryCatch(
      as.character(tools::md5sum(src_path)),
      error = function(e) NA_character_
    )
    same <- isTRUE(old_md5 == new_md5) && !is.na(old_md5) && !is.na(new_md5)
  }

  if (same) {
    message("No changes to CST workbook; not writing ", out_path)
    return(invisible(out_path))
  }

  ok <- file.copy(src_path, out_path, overwrite = TRUE)
  if (!ok) {
    stop("Failed to write CST workbook to ", out_path)
  }
  message("CST workbook saved to: ", out_path)
  invisible(out_path)
}

# Dev-only: refresh the package-installed raw CST workbook
#' @keywords internal
.TADA_CST_UpdateWorkbook <- function() {
  path <- .tada_cst_get_workbook_path(download_only = TRUE, refresh = TRUE)
  .tada_cst_write_ext_workbook_if_changed(
    src_path = path,
    pkg = "EPATADA",
    filename = .CST_WORKBOOK_LOCAL_FILENAME
  )
  invisible(path)
}

# =========================
# Public getters
# =========================

#' Get CST Criteria table
#'
#' Reads the Criteria table (sheet "Criteria" or index 3) from the CST workbook,
#' normalizes the table, and caches the result for the session.
#' Falls back to the installed workbook under inst/extdata/cst-workbook.xlsx on download failure.
#'
#' @return data.frame
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback
#'   and attempts to download the latest workbook directly from EPA, returning the
#'   requested sheet without updating the cache. Errors if the download fails.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh workbook (download, falling back to the
#'   package’s raw workbook on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_CST_GetCriteria <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.CST_CRITERIA_CACHE_KEY)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  path <- .tada_cst_get_workbook_path(
    download_only = download_only,
    refresh = refresh,
    pkg = "EPATADA",
    on_fail_message = "Downloading latest CST workbook failed! Falling back to (possibly outdated) internal workbook."
  )
  if (is.null(path)) {
    stop(
      "Failed to retrieve CST workbook. Ensure internet access or ship inst/extdata/cst-workbook.xlsx."
    )
  }

  df <- .tada_cst_read_sheet(path, target = "criteria")
  if (is.null(df)) {
    stop("Failed to read Criteria sheet from CST workbook.")
  }

  df <- .tada_cst_prepare_table(df)
  if (!download_only) {
    .tada_cache_set(.CST_CRITERIA_CACHE_KEY, df)
  }
  df
}

#' Get CST Legend table
#'
#' Reads the Legend table (sheet "Legend" or index 1) from the CST workbook,
#' normalizes the table, and caches the result for the session.
#' Falls back to the installed workbook under inst/extdata/cst-workbook.xlsx on download failure.
#'
#' @return data.frame
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback
#'   and attempts to download the latest workbook directly from EPA, returning the
#'   requested sheet without updating the cache. Errors if the download fails.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh workbook (download, falling back to the
#'   package’s raw workbook on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_CST_GetLegend <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.CST_LEGEND_CACHE_KEY)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  path <- .tada_cst_get_workbook_path(
    download_only = download_only,
    refresh = refresh,
    pkg = "EPATADA",
    on_fail_message = "Downloading latest CST workbook failed! Falling back to (possibly outdated) internal workbook."
  )
  if (is.null(path)) {
    stop(
      "Failed to retrieve CST workbook. Ensure internet access or ship inst/extdata/cst-workbook.xlsx."
    )
  }

  df <- .tada_cst_read_sheet(path, target = "legend")
  if (is.null(df)) {
    stop("Failed to read Legend sheet from CST workbook.")
  }

  df <- .tada_cst_prepare_table(df)
  if (!download_only) {
    .tada_cache_set(.CST_LEGEND_CACHE_KEY, df)
  }
  df
}

#' Get CST Sources table
#'
#' Reads the Sources table (sheet "Sources" or index 2) from the CST workbook,
#' normalizes the table, and caches the result for the session.
#' Falls back to the installed workbook under inst/extdata/cst-workbook.xlsx on download failure.
#'
#' @return data.frame
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback
#'   and attempts to download the latest workbook directly from EPA, returning the
#'   requested sheet without updating the cache. Errors if the download fails.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh workbook (download, falling back to the
#'   package’s raw workbook on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_CST_GetSources <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.CST_SOURCES_CACHE_KEY)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  path <- .tada_cst_get_workbook_path(
    download_only = download_only,
    refresh = refresh,
    pkg = "EPATADA",
    on_fail_message = "Downloading latest CST workbook failed! Falling back to (possibly outdated) internal workbook."
  )
  if (is.null(path)) {
    stop(
      "Failed to retrieve CST workbook. Ensure internet access or ship inst/extdata/cst-workbook.xlsx."
    )
  }

  df <- .tada_cst_read_sheet(path, target = "sources")
  if (is.null(df)) {
    stop("Failed to read Sources sheet from CST workbook.")
  }

  df <- .tada_cst_prepare_table(df)
  if (!download_only) {
    .tada_cache_set(.CST_SOURCES_CACHE_KEY, df)
  }
  df
}
