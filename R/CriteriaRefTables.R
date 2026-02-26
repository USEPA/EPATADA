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
# - Read sheets by anchored name patterns; also supports new CST naming
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

# Validate that a path is a real .xlsx we can read
if (!exists(".is_valid_xlsx", inherits = FALSE)) {
  .is_valid_xlsx <- function(path) {
    if (
      !is.character(path) ||
        length(path) != 1L ||
        !nzchar(path) ||
        !file.exists(path)
    ) {
      return(FALSE)
    }
    ok <- FALSE
    con <- NULL
    try(
      {
        con <- file(path, "rb")
        on.exit(close(con), add = TRUE)
        sig <- readBin(con, what = "raw", n = 4L)
        if (length(sig) >= 2L && rawToChar(sig[1:2]) == "PK") ok <- TRUE
      },
      silent = TRUE
    )
    if (!ok) {
      return(FALSE)
    }
    tryCatch(
      {
        sn <- openxlsx::getSheetNames(path)
        is.character(sn) && length(sn) > 0
      },
      error = function(e) FALSE
    )
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

# Extract ReportDateTime for comparison:
# - First column label must be exactly "ReportDateTime" (case-sensitive, after trim)
# - Return the second-column value as character (no formatting changes)
if (!exists(".tada_cst_get_report_datetime", inherits = FALSE)) {
  .tada_cst_get_report_datetime <- function(workbook_path) {
    df <- tryCatch(
      .tada_cst_read_sheet(workbook_path, target = "legend"),
      error = function(e) NULL
    )
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || ncol(df) < 2) {
      return(NA_character_)
    }
    labels <- trimws(as.character(df[[1]]))
    idx <- which(!is.na(labels) & labels == "ReportDateTime")
    if (length(idx) == 0) {
      return(NA_character_)
    }
    val <- df[idx[1], 2, drop = TRUE]
    as.character(val) # return as-is (could be display text or numeric serial as string)
  }
}

# Dev-time: save raw XLSX to inst/extdata only when changed.
# Uses Legend.ReportDateTime as the primary gate; if unchanged, skips writing.
# Simplified message: does not include the specific ReportDateTime value.
.tada_cst_write_ext_workbook_if_changed <- function(
  src_path,
  pkg = "EPATADA",
  filename = .CST_WORKBOOK_LOCAL_FILENAME,
  normalize_tabs = TRUE
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

  # Optionally normalize sheet names (Legend/Sources/Criteria) before saving
  to_write <- src_path
  if (isTRUE(normalize_tabs)) {
    norm <- tryCatch(
      .tada_cst_make_normalized_copy(src_path),
      error = function(e) NULL
    )
    if (!is.null(norm) && file.exists(norm)) to_write <- norm
  }

  # If an existing fallback is present, compare ReportDateTime first
  if (file.exists(out_path)) {
    old_dt <- tryCatch(
      .tada_cst_get_report_datetime(out_path),
      error = function(e) NA_character_
    )
    new_dt <- tryCatch(
      .tada_cst_get_report_datetime(to_write),
      error = function(e) NA_character_
    )
    if (!is.na(old_dt) && !is.na(new_dt) && nzchar(old_dt) && nzchar(new_dt)) {
      if (identical(old_dt, new_dt)) {
        message(
          "CST workbook up to date (ReportDateTime unchanged); not writing ",
          out_path
        )
        return(invisible(out_path))
      }
    }
  }

  # Secondary guard: compare MD5 digests if dest exists
  same <- FALSE
  if (file.exists(out_path)) {
    old_md5 <- tryCatch(
      as.character(tools::md5sum(out_path)),
      error = function(e) NA_character_
    )
    new_md5 <- tryCatch(
      as.character(tools::md5sum(to_write)),
      error = function(e) NA_character_
    )
    same <- isTRUE(old_md5 == new_md5) && !is.na(old_md5) && !is.na(new_md5)
  }
  if (same) {
    message("CST workbook up to date; not writing ", out_path)
    return(invisible(out_path))
  }

  # Write the file
  ok <- file.copy(to_write, out_path, overwrite = TRUE)
  if (!ok) {
    stop("Failed to write CST workbook to ", out_path)
  }
  message("CST workbook saved to: ", out_path)
  invisible(out_path)
}

# Download CST workbook to a tempfile and return the path
.tada_cst_download_workbook <- function(url = .CST_WORKBOOK_URL) {
  tf <- tempfile(fileext = ".xlsx")
  ok <- tryCatch(
    {
      if (requireNamespace("curl", quietly = TRUE)) {
        curl::curl_download(url, tf, mode = "wb")
      } else {
        utils::download.file(url, tf, mode = "wb", quiet = TRUE)
      }
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!ok || !.is_valid_xlsx(tf)) {
    if (file.exists(tf)) {
      unlink(tf)
    }
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
    if (
      !is.null(cached) &&
        file.exists(cached) &&
        .is_valid_xlsx(cached) &&
        !isTRUE(refresh)
    ) {
      return(cached)
    }
  }

  # Try to download the latest XLSX
  path <- .tada_cst_download_workbook(.CST_WORKBOOK_URL)
  if (!is.null(path) && file.exists(path) && .is_valid_xlsx(path)) {
    if (!download_only) {
      .tada_cache_set(.CST_WORKBOOK_PATH_CACHE_KEY, path)
    }
    return(path)
  }

  # If download_only, fail fast
  if (download_only) {
    stop(
      "CST workbook download failed (download_only=TRUE), or the file was not a valid .xlsx."
    )
  }

  # Fallback to installed workbook if it exists and is valid
  if (!is.null(on_fail_message)) {
    message(on_fail_message)
  }
  fallback_path <- system.file(
    "extdata",
    .CST_WORKBOOK_LOCAL_FILENAME,
    package = pkg
  )
  if (
    nzchar(fallback_path) &&
      file.exists(fallback_path) &&
      .is_valid_xlsx(fallback_path)
  ) {
    if (!download_only) {
      .tada_cache_set(.CST_WORKBOOK_PATH_CACHE_KEY, fallback_path)
    }
    return(fallback_path)
  }

  NULL
}

# Read a CST sheet by anchored name (classic) or new naming scheme
# target ∈ {"legend","sources","criteria"}
.tada_cst_read_sheet <- function(
  workbook_path,
  target = c("legend", "sources", "criteria")
) {
  target <- match.arg(target)

  snames <- tryCatch(
    openxlsx::getSheetNames(workbook_path),
    error = function(e) NULL
  )
  if (is.null(snames) || length(snames) == 0) {
    return(NULL)
  }

  # 1) Classic names: anchored, case-insensitive
  classic_pat <- switch(
    target,
    legend = "(?i)^\\s*legend",
    sources = "(?i)^\\s*sources?",
    criteria = "(?i)^\\s*criteria"
  )
  m_classic <- grep(classic_pat, snames, perl = TRUE)
  if (length(m_classic) >= 1) {
    sheet_to_read <- snames[m_classic[1]]
  } else {
    # 2) New CST naming: "Search Tool Criteria Data", "(2)", "(3)"
    base_pat <- "(?i)^\\s*search\\s*tool\\s*criteria\\s*data\\s*"
    base_idx <- grep(base_pat, snames, perl = TRUE)

    if (length(base_idx) == 0) {
      warning(sprintf(
        "No %s sheet found. Available sheets: %s",
        target,
        paste(snames, collapse = ", ")
      ))
      return(NULL)
    }

    candidates <- snames[base_idx]
    # Helpers to detect trailing "(n)"
    ends_with_n <- function(x, n) grepl(sprintf("\\(\\s*%d\\s*\\)\\s*$", n), x)
    ends_with_any_num <- function(x) grepl("\\(\\s*\\d+\\s*\\)\\s*$", x)

    sheet_to_read <- switch(
      target,
      criteria = {
        pick <- candidates[ends_with_n(candidates, 3)]
        if (length(pick) >= 1) pick[1] else NULL
      },
      sources = {
        pick <- candidates[ends_with_n(candidates, 2)]
        if (length(pick) >= 1) pick[1] else NULL
      },
      legend = {
        pick <- candidates[!ends_with_any_num(candidates)]
        if (length(pick) >= 1) pick[1] else NULL
      }
    )

    if (is.null(sheet_to_read)) {
      # If exactly three “Search Tool Criteria Data” tabs exist, map heuristically
      if (length(candidates) == 3) {
        crit <- candidates[ends_with_n(candidates, 3)]
        src <- candidates[ends_with_n(candidates, 2)]
        leg <- candidates[!ends_with_any_num(candidates)]
        if (target == "criteria" && length(crit) >= 1) {
          sheet_to_read <- crit[1]
        }
        if (target == "sources" && length(src) >= 1) {
          sheet_to_read <- src[1]
        }
        if (target == "legend" && length(leg) >= 1) sheet_to_read <- leg[1]
      }
    }

    if (is.null(sheet_to_read)) {
      warning(sprintf(
        "Could not resolve %s sheet from new CST naming. Available sheets: %s",
        target,
        paste(snames, collapse = ", ")
      ))
      return(NULL)
    }
  }

  out <- tryCatch(
    openxlsx::read.xlsx(workbook_path, sheet = sheet_to_read),
    error = function(e) NULL
  )
  out
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

# Create a normalized copy of the workbook with explicit sheet names.
# Returns the path to a temporary file if renaming occurred; otherwise NULL.
if (!exists(".tada_cst_make_normalized_copy", inherits = FALSE)) {
  .tada_cst_make_normalized_copy <- function(src_path) {
    snames <- tryCatch(openxlsx::getSheetNames(src_path), error = function(e) {
      NULL
    })
    if (is.null(snames) || length(snames) == 0) {
      return(NULL)
    }

    # If explicit names already exist, nothing to do
    has_classic <- any(grepl("(?i)^\\s*legend", snames, perl = TRUE)) ||
      any(grepl("(?i)^\\s*sources?", snames, perl = TRUE)) ||
      any(grepl("(?i)^\\s*criteria", snames, perl = TRUE))
    if (has_classic) {
      return(NULL)
    }

    # Detect new naming: "Search Tool Criteria Data", "(2)", "(3)"
    base_pat <- "(?i)^\\s*search\\s*tool\\s*criteria\\s*data\\s*"
    idx <- grep(base_pat, snames, perl = TRUE)
    if (length(idx) == 0) {
      return(NULL)
    }

    candidates <- snames[idx]
    ends_with_n <- function(x, n) grepl(sprintf("\\(\\s*%d\\s*\\)\\s*$", n), x)
    ends_with_any_num <- function(x) grepl("\\(\\s*\\d+\\s*\\)\\s*$", x)

    base <- candidates[!ends_with_any_num(candidates)]
    s2 <- candidates[ends_with_n(candidates, 2)]
    s3 <- candidates[ends_with_n(candidates, 3)]

    # Require base + (2) + (3) to avoid guessing
    if (length(base) < 1 || length(s2) < 1 || length(s3) < 1) {
      return(NULL)
    }

    wb <- tryCatch(openxlsx::loadWorkbook(src_path), error = function(e) NULL)
    if (is.null(wb)) {
      return(NULL)
    }

    # Rename; ignore if any sheet is missing (defensive)
    try(
      openxlsx::renameWorksheet(wb, sheet = base[1], newName = "Legend"),
      silent = TRUE
    )
    try(
      openxlsx::renameWorksheet(wb, sheet = s2[1], newName = "Sources"),
      silent = TRUE
    )
    try(
      openxlsx::renameWorksheet(wb, sheet = s3[1], newName = "Criteria"),
      silent = TRUE
    )

    tmp <- tempfile(fileext = ".xlsx")
    ok <- tryCatch(
      {
        openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
        TRUE
      },
      error = function(e) FALSE
    )
    if (!ok || !file.exists(tmp)) {
      return(NULL)
    }
    tmp
  }
}

# Dev-only: refresh the package-installed raw CST workbook
#' @keywords internal
.TADA_CST_UpdateWorkbook <- function() {
  path <- .tada_cst_get_workbook_path(download_only = TRUE, refresh = TRUE)
  .tada_cst_write_ext_workbook_if_changed(
    src_path = path,
    pkg = "EPATADA",
    filename = .CST_WORKBOOK_LOCAL_FILENAME,
    normalize_tabs = TRUE
  )
  invisible(path)
}

# =========================
# Public getters
# =========================

#' Get CST Criteria table
#'
#' Reads the Criteria table from the CST workbook (supports classic "Criteria"
#' or "Search Tool Criteria Data (3)"), normalizes the table, and caches the result.
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
    sn <- tryCatch(openxlsx::getSheetNames(path), error = function(e) {
      character()
    })
    stop(sprintf(
      "Failed to read Criteria sheet. Available sheets: %s",
      if (length(sn)) paste(sn, collapse = ", ") else "<unknown>"
    ))
  }

  df <- .tada_cst_prepare_table(df)
  if (!download_only) {
    .tada_cache_set(.CST_CRITERIA_CACHE_KEY, df)
  }
  df
}

#' Get CST Legend table
#'
#' Reads the Legend table from the CST workbook (supports classic "Legend"
#' or "Search Tool Criteria Data" without numeric suffix), normalizes the table,
#' and caches the result for the session.
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
    sn <- tryCatch(openxlsx::getSheetNames(path), error = function(e) {
      character()
    })
    stop(sprintf(
      "Failed to read Legend sheet. Available sheets: %s",
      if (length(sn)) paste(sn, collapse = ", ") else "<unknown>"
    ))
  }

  df <- .tada_cst_prepare_table(df)
  if (!download_only) {
    .tada_cache_set(.CST_LEGEND_CACHE_KEY, df)
  }
  df
}

#' Get CST Sources table
#'
#' Reads the Sources table from the CST workbook (supports classic "Sources"
#' or "Search Tool Criteria Data (2)"), normalizes the table, and caches the result.
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
    sn <- tryCatch(openxlsx::getSheetNames(path), error = function(e) {
      character()
    })
    stop(sprintf(
      "Failed to read Sources sheet. Available sheets: %s",
      if (length(sn)) paste(sn, collapse = ", ") else "<unknown>"
    ))
  }

  df <- .tada_cst_prepare_table(df)
  if (!download_only) {
    .tada_cache_set(.CST_SOURCES_CACHE_KEY, df)
  }
  df
}
