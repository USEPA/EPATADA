# ============================================================
# EPATADA Criteria Search Tool (CST) reference getters
# ============================================================
# - Robust download of XLSX with installed extdata RDA fallback
# - Session-level caching via .TADA_cache
# - Simple, dependency-light normalization (trim + unique)
# - Dev-only update writers that save extdata as binary RDA files
# - Update writers only write when data have changed
#
# Public getters will:
# - Try to download the latest XLSX from EPA
# - Normalize as needed
# - Fall back to installed RDA if download fails
# - Cache the resulting data.frame for the session
#
# Internal ".TADA_Update*" functions re-create the installed RDA files
# under inst/extdata, using consistent object names for each table.

# =========================
# Shared cache + constants
# =========================

# Ensure session cache env exists if this file is sourced independently
if (!exists(".TADA_cache", inherits = FALSE)) {
  .TADA_cache <- new.env(parent = emptyenv())
}

# If the common helpers are not in scope for some reason, define light wrappers
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
if (!exists(".tada_load_extdata_rda", inherits = FALSE)) {
  .tada_load_extdata_rda <- function(pkg, filename, object_name = NULL,
                                     required_cols = NULL, trim = TRUE) {
    path <- system.file("extdata", filename, package = pkg)
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    e <- new.env(parent = emptyenv())
    objs <- try(load(path, envir = e), silent = TRUE)
    if (inherits(objs, "try-error")) return(NULL)
    if (!is.null(object_name) && object_name %in% objs && is.data.frame(e[[object_name]])) {
      df <- e[[object_name]]
      if (!is.null(required_cols) && !all(required_cols %in% names(df))) return(NULL)
      if (trim) df <- .tada_trim_char_cols(df)
      return(df)
    }
    for (nm in objs) {
      obj <- e[[nm]]
      if (is.data.frame(obj)) {
        if (!is.null(required_cols) && !all(required_cols %in% names(obj))) next
        df <- if (trim) .tada_trim_char_cols(obj) else obj
        return(df)
      }
    }
    NULL
  }
}
if (!exists(".tada_find_pkg_root", inherits = FALSE)) {
  .tada_find_pkg_root <- function(start = getwd(), pkg = "EPATADA") {
    cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
    while (nchar(cur) > 0 && cur != dirname(cur)) {
      desc <- file.path(cur, "DESCRIPTION")
      if (file.exists(desc)) {
        dcf <- tryCatch(read.dcf(desc, all = TRUE), error = function(e) NULL)
        if (!is.null(dcf) && isTRUE(tolower(dcf[1, "Package"]) == tolower(pkg))) {
          return(cur)
        }
      }
      cur <- dirname(cur)
    }
    NULL
  }
}
if (!exists(".tada_save_ext_rda", inherits = FALSE)) {
  .tada_save_ext_rda <- function(obj, obj_name, pkg = "EPATADA", filename,
                                 compress = "xz", version = 2) {
    pkg_root <- .tada_find_pkg_root(pkg = pkg)
    if (is.null(pkg_root)) {
      stop("Could not locate package source root for ", pkg, ". Run from the package source directory.")
    }
    out_path <- file.path(pkg_root, "inst", "extdata", filename)
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    e <- new.env(parent = emptyenv())
    e[[obj_name]] <- obj
    save(list = obj_name, file = out_path, envir = e, version = version, compress = compress)
    message(obj_name, " saved to: ", out_path)
    invisible(out_path)
  }
}

# CST authoritative source (XLSX)
.CST_XLSX_URL <- "https://www.epa.gov/system/files/documents/2025-07/criteria-search-tool-data.xlsx"

# Cache keys
.CSTCriteriaRef_cache_key <- "CriteriaSearchToolRef"
.CSTLegendRef_cache_key   <- "LegendCSTRef"
.CSTSourcesRef_cache_key  <- "SourcesCSTRef"

# =========================
# XLSX helpers
# =========================

# Robust read of XLSX from URL: download to tempfile, then read by sheet index
.tada_read_xlsx_url <- function(url, sheet) {
  tf <- tempfile(fileext = ".xlsx")
  ok <- tryCatch({
    utils::download.file(url, tf, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) return(NULL)
  tryCatch(
    openxlsx::read.xlsx(tf, sheet = sheet),
    error = function(e) NULL
  )
}

# Download authoritative XLSX sheet; if it fails, fallback to installed RDA
.tada_download_or_extdata_rda_xlsx <- function(
    url,
    sheet,
    fallback_filename,
    object_name,
    pkg = "EPATADA",
    required_cols = NULL,
    trim = TRUE,
    on_fail_message = NULL
) {
  df <- .tada_read_xlsx_url(url, sheet = sheet)
  if (!is.null(df)) {
    if (trim) df <- .tada_trim_char_cols(df)
    return(df)
  }
  if (!is.null(on_fail_message)) {
    message(on_fail_message)
  }
  df <- .tada_load_extdata_rda(
    pkg = pkg,
    filename = fallback_filename,
    object_name = object_name,
    required_cols = required_cols,
    trim = trim
  )
  if (is.null(df)) {
    stop(
      "Fallback extdata '", fallback_filename,
      "' not found or invalid in installed package '", pkg, "'."
    )
  }
  df
}

# =========================
# Canonicalization for change detection
# =========================

if (!exists(".tada_canonicalize_df", inherits = FALSE)) {
  .tada_canonicalize_df <- function(df) {
    if (is.null(df)) return(df)
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    df[] <- lapply(df, function(x) {
      if (is.factor(x)) as.character(x) else x
    })
    df <- .tada_trim_char_cols(df)
    # Order columns by name for stable comparison
    df <- df[, sort(names(df)), drop = FALSE]
    # Order rows lexicographically across all columns (best-effort)
    if (nrow(df) > 1) {
      ord_cols <- lapply(df, function(x) {
        if (inherits(x, c("POSIXct", "POSIXt", "Date"))) as.character(x) else x
      })
      o <- try(do.call(order, c(ord_cols, na.last = TRUE)), silent = TRUE)
      if (!inherits(o, "try-error")) {
        df <- df[o, , drop = FALSE]
      }
    }
    rownames(df) <- NULL
    df
  }
}

if (!exists(".tada_write_ext_rda_if_changed", inherits = FALSE)) {
  .tada_write_ext_rda_if_changed <- function(obj, obj_name, pkg = "EPATADA",
                                             filename, compress = "xz", version = 2) {
    pkg_root <- .tada_find_pkg_root(pkg = pkg)
    if (is.null(pkg_root)) {
      stop("Could not locate package source root for ", pkg, ". Run from the package source directory.")
    }
    out_path <- file.path(pkg_root, "inst", "extdata", filename)
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    
    # Load existing object if present
    old_df <- NULL
    if (file.exists(out_path)) {
      e <- new.env(parent = emptyenv())
      objs <- try(load(out_path, envir = e), silent = TRUE)
      if (!inherits(objs, "try-error")) {
        if (obj_name %in% objs && is.data.frame(e[[obj_name]])) {
          old_df <- e[[obj_name]]
        } else {
          for (nm in objs) {
            if (is.data.frame(e[[nm]])) { old_df <- e[[nm]]; break }
          }
        }
      }
    }
    
    new_can <- .tada_canonicalize_df(obj)
    old_can <- if (!is.null(old_df)) .tada_canonicalize_df(old_df) else NULL
    
    if (!is.null(old_can) && identical(new_can, old_can)) {
      message("No changes to ", obj_name, "; not writing ", filename)
      return(invisible(out_path))
    }
    
    .tada_save_ext_rda(
      obj = obj,
      obj_name = obj_name,
      pkg = pkg,
      filename = filename,
      compress = compress,
      version = version
    )
  }
}

# =========================
# Normalization helpers
# =========================

.TADA_prepare_CST_table <- function(df) {
  unique(.tada_trim_char_cols(df))
}

# =========================
# Public getters + updates
# =========================

#' Criteria Search Tool (CST) Reference Table
#'
#' Downloads the State-Specific Water Quality Standards Effective under the CWA
#' from EPA's Criteria Search Tool (sheet 3 of the CST workbook), normalizes
#' the table, and caches the result for the session. Falls back to the
#' installed RDA under inst/extdata if the download fails.
#'
#' @return data.frame of the CST criteria table
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback
#'   and attempts to download the latest table directly from EPA, returning it
#'   without updating the cache. Errors if the download fails. If FALSE (default),
#'   uses a cached copy when available and updates the cache; on download failure,
#'   falls back to the package’s internal file.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetCriteriaSearchToolRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.CSTCriteriaRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  
  if (download_only) {
    df <- .tada_read_xlsx_url(.CST_XLSX_URL, sheet = 3)
    if (is.null(df)) {
      stop("TADA_GetCriteriaSearchToolRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda_xlsx(
      url = .CST_XLSX_URL,
      sheet = 3,
      fallback_filename = "CriteriaSearchToolRef.rda",
      object_name = "CriteriaSearchToolRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Criteria Search Tool Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  
  df <- .TADA_prepare_CST_table(df)
  if (!download_only) {
    .tada_cache_set(.CSTCriteriaRef_cache_key, df)
  }
  df
}

#' Update CST Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateCriteriaSearchToolRef <- function() {
  df <- TADA_GetCriteriaSearchToolRef(download_only = TRUE, refresh = TRUE)
  .tada_write_ext_rda_if_changed(
    obj = df,
    obj_name = "CriteriaSearchToolRef",
    pkg = "EPATADA",
    filename = "CriteriaSearchToolRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Legend for the Criteria Search Tool (CST)
#'
#' Downloads the CST Legend (sheet 1 of the CST workbook), normalizes
#' the table, and caches the result for the session. Falls back to the
#' installed RDA under inst/extdata if the download fails.
#'
#' @return data.frame of the CST legend
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback
#'   and attempts to download the latest table directly from EPA, returning it
#'   without updating the cache. Errors if the download fails. If FALSE (default),
#'   uses a cached copy when available and updates the cache; on download failure,
#'   falls back to the package’s internal file.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetLegendCSTRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.CSTLegendRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  
  if (download_only) {
    df <- .tada_read_xlsx_url(.CST_XLSX_URL, sheet = 1)
    if (is.null(df)) {
      stop("TADA_GetLegendCSTRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda_xlsx(
      url = .CST_XLSX_URL,
      sheet = 1,
      fallback_filename = "LegendCSTRef.rda",
      object_name = "LegendCSTRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest CST Legend Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  
  df <- .TADA_prepare_CST_table(df)
  if (!download_only) {
    .tada_cache_set(.CSTLegendRef_cache_key, df)
  }
  df
}

#' Update CST Legend Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateLegendCSTRef <- function() {
  df <- TADA_GetLegendCSTRef(download_only = TRUE, refresh = TRUE)
  .tada_write_ext_rda_if_changed(
    obj = df,
    obj_name = "LegendCSTRef",
    pkg = "EPATADA",
    filename = "LegendCSTRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Sources for the Criteria Search Tool (CST)
#'
#' Downloads the CST Sources (sheet 2 of the CST workbook), normalizes
#' the table, and caches the result for the session. Falls back to the
#' installed RDA under inst/extdata if the download fails.
#'
#' @return data.frame of the CST sources
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback
#'   and attempts to download the latest table directly from EPA, returning it
#'   without updating the cache. Errors if the download fails. If FALSE (default),
#'   uses a cached copy when available and updates the cache; on download failure,
#'   falls back to the package’s internal file.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetSourcesCSTRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.CSTSourcesRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  
  if (download_only) {
    df <- .tada_read_xlsx_url(.CST_XLSX_URL, sheet = 2)
    if (is.null(df)) {
      stop("TADA_GetSourcesCSTRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda_xlsx(
      url = .CST_XLSX_URL,
      sheet = 2,
      fallback_filename = "SourcesCSTRef.rda",
      object_name = "SourcesCSTRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest CST Sources Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  
  df <- .TADA_prepare_CST_table(df)
  if (!download_only) {
    .tada_cache_set(.CSTSourcesRef_cache_key, df)
  }
  df
}

#' Update CST Sources Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateSourcesCSTRef <- function() {
  df <- TADA_GetSourcesCSTRef(download_only = TRUE, refresh = TRUE)
  .tada_write_ext_rda_if_changed(
    obj = df,
    obj_name = "SourcesCSTRef",
    pkg = "EPATADA",
    filename = "SourcesCSTRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}
