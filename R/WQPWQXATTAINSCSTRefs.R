# ============================================================
# EPATADA reference data utilities: shared helpers and getters
# ============================================================
# Centralized:
# - Robust download of authoritative CSVs with installed extdata RDA fallback
# - Session-level caching with a single environment
# - Simple, dependency-light data normalization and flagging
# - Dev-only update writers that save extdata as binary RDA files
#
# Important: All installed extdata are RDA (not CSV). Public getters will:
# - Try to download the latest CSV from EPA/WQP
# - Normalize/flag as needed
# - Fall back to installed RDA if download fails
# - Cache the resulting data.frame for the session
#
# Internal ".TADA_Update*" functions re-create the installed RDA files
# under inst/extdata, using consistent object names for each table.

# =========================
# Shared cache + constants
# =========================

# Session cache environment (single instance for entire package)
if (!exists(".TADA_cache", envir = topenv(), inherits = FALSE)) {
  .TADA_cache <- new.env(parent = emptyenv())
}

#' Clear EPATADA reference table cache
#' @export
TADA_ClearCache <- function() {
  # Ensure env exists (belt and suspenders)
  if (!exists(".TADA_cache", inherits = TRUE)) {
    .TADA_cache <<- new.env(parent = emptyenv())
  }
  rm(list = ls(envir = .TADA_cache, all.names = TRUE), envir = .TADA_cache)
  invisible(TRUE)
}

#' List keys in the EPATADA cache
#' @export
TADA_ListCacheKeys <- function() {
  if (!exists(".TADA_cache", inherits = TRUE)) {
    character(0)
  } else {
    ls(envir = .TADA_cache, all.names = TRUE)
  }
}

# Authoritative CSV sources
.WQX_URLS <- list(
  Characteristic = "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV",
  QAQCCharacteristicValidation = "https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV",
  MeasureUnit = "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV",
  ResultDetectionCondition = "https://cdx.epa.gov/wqx/download/DomainValues/ResultDetectionCondition.CSV",
  DetectionQuantitationLimitType = "https://cdx.epa.gov/wqx/download/DomainValues/DetectionQuantitationLimitType.CSV",
  ActivityType = "https://cdx.epa.gov/wqx/download/DomainValues/ActivityType.CSV",
  MonitoringLocationType = "https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV",
  ResultMeasureQualifier = "https://cdx.epa.gov/wqx/download/DomainValues/ResultMeasureQualifier.CSV"
  # WQXCharAliasRef = "https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicAlias_CSV.zip" # zip file handled within function
)

.WQP_URLS <- list(
  Organization = "https://www.waterqualitydata.us/data/Organization/search?mimeType=csv&zip=no"
)

# Cache keys
.WQXCharacteristicRef_cache_key <- "WQXCharacteristicRef"
.WQXCharValRef_cache_key <- "WQXcharValRef"
.WQXUnitRef_cache_key <- "WQXunitRef"
.WQXDetCondRef_cache_key <- "WQXResultDetectionConditionRef"
.WQXDetLimitRef_cache_key <- "WQXDetectionQuantitationLimitTypeRef"
.WQXActivityTypeRef_cache_key <- "WQXActivityTypeRef"
.WQXMonLocTypeRef_cache_key <- "WQXMonitoringLocationTypeNameRef"
.WQPProviderRef_cache_key <- "WQPProviderRef"
.WQXMeasureQualifierCodeRef_cache_key <- "WQXMeasureQualifierCodeRef"
.WQXCharAliasRef_cache_key <- "WQXCharAliasRef"
.ATTAINSOrgIDsRef_cache_key <- "ATTAINSOrgIDsRef"
.ATTAINSParamUseOrgRef_cache_key <- "ATTAINSParamUseOrgRef"

# ATTAINS Org IDs RDA constants
.ATTAINS_ORG_IDS_RDA_FILENAME <- "ATTAINSOrgIDsRef.rda"
.ATTAINS_ORG_IDS_OBJ_NAME <- "ATTAINSOrgIDsRef"

# =========================
# Generic helper functions
# =========================

# Convert factors to character (makes further processing consistent
# with downloaded CSVs which are character)
.tada_trim_char_cols <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    if (is.character(x)) trimws(x) else x
  })
  df
}

.tada_require_cols <- function(df, required_cols, context = "table") {
  if (is.null(required_cols)) {
    return(invisible(TRUE))
  }
  missing <- setdiff(required_cols, names(df))
  if (length(missing)) {
    stop(sprintf(
      "%s: missing required columns: %s",
      context,
      paste(missing, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

# Cache helpers
.tada_cache_get <- function(key) {
  if (!exists(".TADA_cache", inherits = TRUE)) {
    .TADA_cache <<- new.env(parent = emptyenv())
  }
  .TADA_cache[[key]]
}
.tada_cache_set <- function(key, value) {
  if (!exists(".TADA_cache", inherits = TRUE)) {
    .TADA_cache <<- new.env(parent = emptyenv())
  }
  .TADA_cache[[key]] <- value
  invisible(value)
}

# Read CSV from URL; returns NULL on error; avoids lingering connections
.tada_read_csv_url <- function(
  url,
  stringsAsFactors = FALSE,
  encodings = c("UTF-8", "latin1"),
  retries = 2,
  user_agent = "EPATADA (R)"
) {
  tf <- tempfile(fileext = ".csv")
  on.exit(
    {
      if (file.exists(tf)) unlink(tf, force = TRUE)
    },
    add = TRUE
  )

  ua <- getOption("EPATADA.user_agent", user_agent)
  verbose <- isTRUE(getOption("EPATADA.verbose"))

  download_ok <- FALSE
  attempt <- 0L

  # 1) Preferred: curl with headers and proxy support
  if (requireNamespace("curl", quietly = TRUE)) {
    while (!download_ok && attempt <= retries) {
      attempt <- attempt + 1L
      download_ok <- tryCatch(
        {
          h <- curl::new_handle(followlocation = TRUE)
          curl::handle_setheaders(
            h,
            "User-Agent" = ua,
            "Accept" = "text/csv, */*"
          )

          if (.Platform$OS.type == "windows") {
            p <- try(curl::ie_proxy_info(), silent = TRUE)
            if (
              !inherits(p, "try-error") &&
                is.list(p) &&
                is.character(p$url) &&
                length(p$url) &&
                nzchar(p$url[1])
            ) {
              curl::handle_setopt(h, proxy = p$url[1])
              if (
                is.character(p$userpwd) &&
                  length(p$userpwd) &&
                  nzchar(p$userpwd[1])
              ) {
                curl::handle_setopt(h, proxyuserpwd = p$userpwd[1])
              }
            }
          }
          px <- Sys.getenv(c(
            "https_proxy",
            "HTTPS_PROXY",
            "http_proxy",
            "HTTP_PROXY"
          ))
          px <- px[nzchar(px)]
          if (length(px)) {
            curl::handle_setopt(h, proxy = px[[1]])
          }

          curl::curl_download(url, tf, mode = "wb", handle = h)
          file.exists(tf) &&
            is.finite(file.info(tf)$size) &&
            file.info(tf)$size > 0
        },
        error = function(e) {
          if (verbose) {
            message("curl_download failed [", conditionMessage(e), "]")
          }
          FALSE
        },
        warning = function(w) {
          if (verbose) {
            message("curl_download warning [", conditionMessage(w), "]")
          }
          FALSE
        }
      )
      if (!download_ok) Sys.sleep(0.4 * attempt)
    }
  }

  # 2) Fallback: utils::download.file (libcurl), then wininet on Windows
  if (!download_ok) {
    ok <- try(
      {
        utils::download.file(
          url,
          tf,
          mode = "wb",
          method = "libcurl",
          quiet = TRUE
        )
        TRUE
      },
      silent = TRUE
    )
    download_ok <- !inherits(ok, "try-error") &&
      file.exists(tf) &&
      file.info(tf)$size > 0
    if (verbose && !download_ok) {
      message("utils::download.file(libcurl) failed.")
    }

    if (!download_ok && .Platform$OS.type == "windows") {
      ok <- try(
        {
          utils::download.file(
            url,
            tf,
            mode = "wb",
            method = "wininet",
            quiet = TRUE
          )
          TRUE
        },
        silent = TRUE
      )
      download_ok <- !inherits(ok, "try-error") &&
        file.exists(tf) &&
        file.info(tf)$size > 0
      if (verbose && !download_ok) {
        message("utils::download.file(wininet) failed.")
      }
    }
  }

  # 3) If we got a file, guard against HTML but still try to parse once
  if (download_ok) {
    head_bytes <- tryCatch(
      readChar(tf, nchars = 256L, useBytes = TRUE),
      error = function(e) ""
    )
    looks_html <- grepl("(?i)<html|<!DOCTYPE", head_bytes)
    if (verbose) {
      message(
        "Downloaded file size: ",
        file.info(tf)$size,
        " bytes; looks_html=",
        looks_html
      )
    }

    # Attempt to parse even if looks_html (some gateways add harmless tags or false positives)
    for (enc in encodings) {
      df <- tryCatch(
        utils::read.csv(
          tf,
          stringsAsFactors = stringsAsFactors,
          fileEncoding = enc,
          comment.char = "",
          check.names = TRUE
        ),
        error = function(e) {
          if (verbose) {
            message(
              "read.csv(temp, enc=",
              enc,
              ") failed: ",
              conditionMessage(e)
            )
          }
          NULL
        }
      )
      if (!is.null(df)) {
        nm <- names(df)
        nm <- sub("^\ufeff", "", nm, perl = TRUE)
        nm <- sub(
          "^\u00EF(?:\u00BB|\\.)?(?:\u00BF|\\.)?\\s*",
          "",
          nm,
          perl = TRUE
        )
        nm <- trimws(nm)
        names(df) <- nm
        return(df)
      }
    }
  }

  # 4) Stream directly from URL using base R (often succeeds when temp-file parse fails)
  df <- tryCatch(
    utils::read.csv(
      url,
      stringsAsFactors = stringsAsFactors,
      comment.char = "",
      check.names = TRUE
    ),
    error = function(e) {
      if (verbose) {
        message("utils::read.csv(URL) failed: ", conditionMessage(e))
      }
      NULL
    }
  )
  if (!is.null(df)) {
    nm <- names(df)
    nm <- sub("^\ufeff", "", nm, perl = TRUE)
    nm <- trimws(nm)
    names(df) <- nm
    return(df)
  }

  # 5) Stream via curl connection (if available)
  if (requireNamespace("curl", quietly = TRUE)) {
    df <- tryCatch(
      {
        con <- curl::curl(url)
        on.exit(try(close(con), silent = TRUE), add = TRUE)
        utils::read.csv(
          con,
          stringsAsFactors = stringsAsFactors,
          comment.char = "",
          check.names = TRUE
        )
      },
      error = function(e) {
        if (verbose) {
          message("read.csv(curl(url)) failed: ", conditionMessage(e))
        }
        NULL
      }
    )
    if (!is.null(df)) {
      nm <- names(df)
      nm <- sub("^\ufeff", "", nm, perl = TRUE)
      nm <- trimws(nm)
      names(df) <- nm
      return(df)
    }
  }

  # 6) readr fallback from URL (if installed)
  if (requireNamespace("readr", quietly = TRUE)) {
    df <- tryCatch(
      {
        tb <- readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
        as.data.frame(tb, stringsAsFactors = stringsAsFactors)
      },
      error = function(e) {
        if (verbose) {
          message("readr::read_csv(URL) failed: ", conditionMessage(e))
        }
        NULL
      }
    )
    if (!is.null(df)) {
      nm <- names(df)
      nm <- sub("^\ufeff", "", nm, perl = TRUE)
      nm <- trimws(nm)
      names(df) <- nm
      return(df)
    }
  }

  NULL
}

# Reusable colname normalizer (BOM strip, trim, syntactic)
.tada_norm_colnames <- function(df) {
  nm <- names(df)
  nm <- sub("^\ufeff", "", nm, perl = TRUE) # strip BOM
  nm <- trimws(nm)
  nm <- make.names(nm, unique = TRUE) # match read.csv(check.names=TRUE)
  names(df) <- nm
  df
}

# Load a data.frame from an installed extdata .rda (returns NULL if not found/invalid)
.tada_load_extdata_rda <- function(
  pkg = "EPATADA",
  filename,
  object_name = NULL,
  required_cols = NULL,
  trim = TRUE
) {
  # Try installed path first
  path <- system.file("extdata", filename, package = pkg)

  # Dev-time fallback: look in package source inst/extdata
  if (!nzchar(path) || !file.exists(path)) {
    pkg_root <- .tada_find_pkg_root(pkg = pkg)
    if (!is.null(pkg_root)) {
      alt <- file.path(pkg_root, "inst", "extdata", filename)
      if (file.exists(alt)) path <- alt
    }
  }

  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }

  e <- new.env(parent = emptyenv())
  objs <- try(load(path, envir = e), silent = TRUE)
  if (inherits(objs, "try-error")) {
    return(NULL)
  }

  pick_df <- function(obj) {
    if (!is.data.frame(obj)) {
      return(NULL)
    }
    if (trim) {
      obj <- .tada_trim_char_cols(obj)
    }
    obj <- .tada_norm_colnames(obj)
    if (!is.null(required_cols) && length(setdiff(required_cols, names(obj)))) {
      return(NULL)
    }
    obj
  }

  if (!is.null(object_name) && object_name %in% objs) {
    df <- pick_df(e[[object_name]])
    if (!is.null(df)) return(df)
  }
  for (nm in objs) {
    df <- pick_df(e[[nm]])
    if (!is.null(df)) return(df)
  }
}

# Download authoritative CSV; if it fails, fallback to installed RDA
.tada_download_or_extdata_rda <- function(
  url,
  fallback_filename,
  object_name,
  pkg = "EPATADA",
  required_cols = NULL,
  stringsAsFactors = FALSE,
  trim = TRUE,
  on_fail_message = NULL
) {
  df <- .tada_read_csv_url(url, stringsAsFactors = stringsAsFactors)
  if (!is.null(df)) {
    if (!is.null(required_cols) && !all(required_cols %in% names(df))) {
      # Structure not as expected; force fallback
      df <- NULL
    } else {
      if (trim) {
        df <- .tada_trim_char_cols(df)
      }
      return(df)
    }
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
      "Fallback extdata '",
      fallback_filename,
      "' not found or invalid in installed package '",
      pkg,
      "'."
    )
  }
  df
}

# Find package source root by locating DESCRIPTION (used by dev-only writers)
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

# Safe bind rows (no external deps): aligns columns by name, fills missing with NA
.tada_bind_rows <- function(df1, df2) {
  all_cols <- union(names(df1), names(df2))
  df1[setdiff(all_cols, names(df1))] <- NA
  df2[setdiff(all_cols, names(df2))] <- NA
  df1 <- df1[, all_cols, drop = FALSE]
  df2 <- df2[, all_cols, drop = FALSE]

  for (nm in all_cols) {
    x <- df1[[nm]]
    y <- df2[[nm]]

    is_date_x <- inherits(x, "Date")
    is_date_y <- inherits(y, "Date")
    is_posix_x <- inherits(x, "POSIXct")
    is_posix_y <- inherits(y, "POSIXct")

    # If either side is Date, ensure logical placeholders become Date NA
    if (is_date_x || is_date_y) {
      if (is.logical(x)) {
        df1[[nm]] <- as.Date(rep(NA_real_, nrow(df1)))
      }
      if (is.logical(y)) {
        df2[[nm]] <- as.Date(rep(NA_real_, nrow(df2)))
      }
      next
    }

    # If either side is POSIXct, ensure logical placeholders become POSIXct NA (respect tzone if present)
    if (is_posix_x || is_posix_y) {
      tzx <- attr(x, "tzone")
      tzy <- attr(y, "tzone")
      tz <- if (!is.null(tzx) && length(tzx) && nzchar(tzx[1])) {
        tzx[1]
      } else if (!is.null(tzy) && length(tzy) && nzchar(tzy[1])) {
        tzy[1]
      } else {
        "" # session default
      }
      if (is.logical(x)) {
        df1[[nm]] <- as.POSIXct(
          rep(NA_real_, nrow(df1)),
          tz = tz,
          origin = "1970-01-01"
        )
      }
      if (is.logical(y)) {
        df2[[nm]] <- as.POSIXct(
          rep(NA_real_, nrow(df2)),
          tz = tz,
          origin = "1970-01-01"
        )
      }
      next
    }

    # Promote factors to character to avoid level mismatches and unintended coercion
    if (is.factor(x)) {
      df1[[nm]] <- as.character(x)
    }
    if (is.factor(y)) {
      df2[[nm]] <- as.character(y)
    }

    # Promote logical NA placeholders to the other's simple atomic type
    if (is.logical(x) && !is.logical(y)) {
      if (is.character(y)) {
        df1[[nm]] <- rep(NA_character_, nrow(df1))
      } else if (is.integer(y)) {
        df1[[nm]] <- rep(NA_integer_, nrow(df1))
      } else if (is.numeric(y)) {
        df1[[nm]] <- rep(NA_real_, nrow(df1))
      }
      # logical vs other complex classes will fall back to rbind's coercion
    } else if (is.logical(y) && !is.logical(x)) {
      if (is.character(x)) {
        df2[[nm]] <- rep(NA_character_, nrow(df2))
      } else if (is.integer(x)) {
        df2[[nm]] <- rep(NA_integer_, nrow(df2))
      } else if (is.numeric(x)) {
        df2[[nm]] <- rep(NA_real_, nrow(df2))
      }
    }
  }

  rbind(df1, df2)
}

# Assign labels based on vector membership (first match wins, NA optional)
.tada_flag_by_groups <- function(
  df,
  source_col,
  out_col,
  groups,
  default = "Not Reviewed",
  na_default = NULL
) {
  if (!(source_col %in% names(df))) {
    stop(sprintf(
      "Missing required column '%s' in data.frame; available: %s",
      source_col,
      paste(names(df), collapse = ", ")
    ))
  }
  # Trim the source vector to make matching resilient to whitespace
  v <- df[[source_col]]
  if (is.factor(v)) {
    v <- as.character(v)
  }
  if (is.character(v)) {
    v <- trimws(v)
  }

  flag <- rep(default, length(v))
  for (lab in names(groups)) {
    idx <- (v %in% groups[[lab]]) & (flag == default)
    flag[idx] <- lab
  }
  if (!is.null(na_default)) {
    flag[is.na(v)] <- na_default
  }
  df[[out_col]] <- as.character(flag)
  df
}

# Compare two data.frames for equality ignoring row order and minor attributes
.tada_df_equal <- function(a, b) {
  # Fall back to identical() if not data.frames
  if (!is.data.frame(a) || !is.data.frame(b)) {
    return(identical(a, b))
  }

  # Same set of column names?
  na <- names(a)
  nb <- names(b)
  if (!identical(sort(na), sort(nb))) {
    return(FALSE)
  }

  # Align by sorted column names
  cols <- sort(na)
  a <- a[, cols, drop = FALSE]
  b <- b[, cols, drop = FALSE]

  # Coerce factors to character to avoid attribute-only diffs
  a[] <- lapply(a, function(x) if (is.factor(x)) as.character(x) else x)
  b[] <- lapply(b, function(x) if (is.factor(x)) as.character(x) else x)

  # Canonical row ordering by all columns
  if (nrow(a) > 0) {
    oa <- try(do.call(order, a), silent = TRUE)
    if (!inherits(oa, "try-error") && length(oa)) a <- a[oa, , drop = FALSE]
  }
  if (nrow(b) > 0) {
    ob <- try(do.call(order, b), silent = TRUE)
    if (!inherits(ob, "try-error") && length(ob)) b <- b[ob, , drop = FALSE]
  }

  # Remove row names before comparison
  rownames(a) <- NULL
  rownames(b) <- NULL

  isTRUE(all.equal(a, b, check.attributes = FALSE))
}

# Dev-only writer: save a data.frame as RDA into inst/extdata under obj_name
# Now skips writing if content hasn't changed compared to existing .rda
.tada_save_ext_rda <- function(
  obj,
  obj_name,
  pkg = "EPATADA",
  filename,
  compress = "xz",
  version = 2
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

  # If an existing file is present, load and compare to avoid unnecessary writes
  if (file.exists(out_path)) {
    e_old <- new.env(parent = emptyenv())
    old_objs <- try(load(out_path, envir = e_old), silent = TRUE)
    if (
      !inherits(old_objs, "try-error") &&
        obj_name %in% old_objs &&
        is.data.frame(e_old[[obj_name]]) &&
        .tada_df_equal(e_old[[obj_name]], obj)
    ) {
      message(
        "No changes detected for ",
        obj_name,
        "; skipping save: ",
        out_path
      )
      return(invisible(out_path))
    }
  }

  # Save updated object
  e <- new.env(parent = emptyenv())
  e[[obj_name]] <- obj
  save(
    list = obj_name,
    file = out_path,
    envir = e,
    version = version,
    compress = compress
  )
  message(obj_name, " saved to: ", out_path)
  invisible(out_path)
}

# =========================
# Characteristic reference
# =========================

# Keeps exactly: Name, Comparable.Name, CAS.Number, Domain.Value.Status
.TADA_normalize_characteristic_ref <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(NULL)
  }
  required <- c("Name", "Comparable.Name", "CAS.Number", "Domain.Value.Status")
  if (!all(required %in% names(df))) {
    return(NULL)
  }
  out <- data.frame(
    CharacteristicName = df[["Name"]],
    Comparable.Name = df[["Comparable.Name"]],
    CAS.Number = df[["CAS.Number"]],
    Char_Flag = df[["Domain.Value.Status"]],
    stringsAsFactors = FALSE
  )
  out <- .tada_trim_char_cols(out)
  unique(out)
}

# ================
# Table helpers
# ================

# QAQC Characteristic Validation
.TADA_flag_WQXCharValRef <- function(df) {
  groups <- list(
    "Pass" = c("Accepted", "Y"),
    "Suspect" = c("Rejected", "Rejected ", "N"),
    "NonStandardized" = c(
      "NonStandardized",
      "Nonstandardized",
      "Non Standardized",
      "InvalidMediaUnit",
      "InvalidChar",
      "MethodNeeded"
    )
  )
  df <- .tada_flag_by_groups(
    df,
    source_col = "Status",
    out_col = "TADA.WQXVal.Flag",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  unique(.tada_trim_char_cols(df))
}

# Measure Unit (no flags)
.TADA_prepare_MeasureUnitRef <- function(df) {
  unique(.tada_trim_char_cols(df))
}

# Result Detection Condition (+ USGS legacy)
.TADA_flag_DetCondRef <- function(df) {
  groups <- list(
    "Non-Detect" = c(
      "Below Daily Detection Limit",
      "Below Detection Limit",
      "Below Long-term Blank-basd Dt Limit",
      "Below Method Detection Limit",
      "Below Reporting Limit",
      "Below Sample-specific Detect Limit",
      "Below System Detection Limit",
      "Between Inst Detect and Quant Limit",
      "Detected Not Quantified",
      "Not Detected",
      "Not Detected at Detection Limit",
      "Not Detected at Reporting Limit",
      "Not Present",
      "Not Reported",
      "Present Below Quantification Limit",
      "Trace"
    ),
    "Over-Detect" = c(
      "Above Operating Range",
      "Present Above Quantification Limit"
    ),
    "Other" = c(
      "Value Decensored",
      "Reported in Raw Data (attached)",
      "High Moisture",
      "Unable to Measure",
      "Value affected by contamination"
    )
  )
  df <- .tada_flag_by_groups(
    df,
    source_col = "Name",
    out_col = "TADA.Detection_Type",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  df <- unique(.tada_trim_char_cols(df))
  others <- data.frame(
    Name = c("*Non-detect", "*Present <QL", "*Present"),
    Description = rep("Hard-coded legacy detection condition", 3),
    TADA.Detection_Type = c("Non-Detect", "Non-Detect", "Non-Detect"),
    Last.Change.Date = c("8/7/2023", "8/7/2023", "12/14/2023"),
    stringsAsFactors = FALSE
  )
  df <- .tada_bind_rows(df, others)
  df <- unique(.tada_trim_char_cols(df))
  df
}

# Detection/Quantitation Limit Type (+ USGS)
.TADA_flag_DetLimitRef <- function(df) {
  groups <- list(
    "Non-Detect" = c(
      "Blank-adjusted method detect limit",
      "Blank-adjusted reporting limit",
      "Contract Detection Limit",
      "Contract Quantitation Limit",
      "Daily detection limit",
      "Detection limit by DQCALC",
      "Estimated Detection Level",
      "Estimated Quantitation Limit",
      "Instrument Detection Level",
      "Interim Reporting Level",
      "Laboratory Reporting Level",
      "Long Term Method Detection Level",
      "Lower Quantitation Limit",
      "Lower Reporting Limit",
      "Lower limit of detection",
      "Method Detection Level",
      "Minimum Reporting Level",
      "Practical Quantitation Limit",
      "Reporting limit",
      "Reporting limit by DQCALC",
      "Required detection limit",
      "Sample Detection Limit",
      "Sample-Specific Quantitation Limit",
      "Sample-specific critical level",
      "Sample-specific min detect conc"
    ),
    "Over-Detect" = c(
      "Upper Quantitation Limit",
      "Upper Reporting Limit",
      "Upper Calibration Limit"
    ),
    "Other" = c(
      "Measurement Uncertainty",
      "Laboratory Holding Time Limit",
      "Drinking Water Maximum",
      "Field Holding Time Limit",
      "Specified in workplan",
      "Statistical Uncertainty",
      "Systematic Uncertainty",
      "Taxonomic Loss Threshold",
      "Water Quality Standard or Criteria",
      "Upper 95% Confidence Limit",
      "Lower 95% Confidence Limit",
      "Censoring level"
    )
  )
  df <- .tada_flag_by_groups(
    df,
    source_col = "Name",
    out_col = "TADA.Limit_Type",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  df <- unique(.tada_trim_char_cols(df))
  usgs <- data.frame(
    Name = c(
      "Elevated Detection Limit",
      "Historical Lower Reporting Limit",
      "Method Detection Limit (MDL)",
      "Lab Reporting Limit, NA"
    ),
    Description = rep("USGS hard-coded limit", 4),
    TADA.Limit_Type = rep("Non-Detect", 4),
    Last.Change.Date = c("4/6/2023", "4/6/2023", "4/6/2023", "12/14/2023"),
    stringsAsFactors = FALSE
  )
  df <- .tada_bind_rows(df, usgs)
  df <- unique(.tada_trim_char_cols(df))
  df
}

# Activity Type (+ hard-coded additions)
.TADA_flag_ActivityTypeRef <- function(df) {
  rep <- c(
    "Quality Control Field Replicate Habitat Assessment",
    "Quality Control Field Replicate Msr/Obs",
    "Quality Control Field Replicate Portable Data Logger",
    "Quality Control Field Replicate Sample-Composite",
    "Quality Control Sample-Field Replicate",
    "Quality Control Field Replicate Sample-Field Subsample"
  )
  dup <- c(
    "Quality Control Alternative Measurement Sensitivity",
    "Quality Control Alternative Measurement Sensitivity Plus",
    "Quality Control Sample-Blind Duplicate",
    "Quality Control Sample-Inter-lab Split",
    "Quality Control Sample-Lab Duplicate",
    "Quality Control Sample-Lab Duplicate 2",
    "Quality Control Sample-Lab Re-Analysis",
    "Quality Control Sample-Lab Split",
    "Quality Control-Meter Lab Duplicate",
    "Quality Control-Meter Lab Duplicate 2",
    "Sample-Routine Resample"
  )
  blank <- c(
    "Quality Control Field Sample Equipment Rinsate Blank",
    "Quality Control Lab Sample Equipment Rinsate Blank",
    "Quality Control Sample-Equipment Blank",
    "Quality Control Sample-Field Ambient Conditions Blank",
    "Quality Control Sample-Field Blank",
    "Quality Control Sample-Lab Blank",
    "Quality Control Sample-Post-preservative Blank",
    "Quality Control Sample-Pre-preservative Blank",
    "Quality Control Sample-Reagent Blank",
    "Quality Control Sample-Trip Blank",
    "Quality Control-Meter Lab Blank",
    "Quality Control-Negative Control",
    "Sample-Depletion Replicate",
    "Sample-Negative Control"
  )
  cal <- c(
    "Quality Control Field Calibration Check",
    "Quality Control Field Msr/Obs Post-Calibration",
    "Quality Control Field Msr/Obs Pre-Calibration",
    "Quality Control Sample-Field Spike",
    "Quality Control Sample-Field Surrogate Spike",
    "Quality Control Sample-Lab Continuing Calibration Verification",
    "Quality Control Sample-Lab Control Sample/Blank Spike",
    "Quality Control Sample-Lab Control Sample/Blank Spike Duplicate",
    "Quality Control Sample-Lab Control Standard",
    "Quality Control Sample-Lab Control Standard Duplicate",
    "Quality Control Sample-Lab Initial Calib Certified Reference Material",
    "Quality Control Sample-Lab Initial Calibration Verification",
    "Quality Control Sample-Lab Matrix Spike",
    "Quality Control Sample-Lab Matrix Spike Duplicate",
    "Quality Control Sample-Lab Spike",
    "Quality Control Sample-Lab Spike Duplicate",
    "Quality Control Sample-Lab Spike Target",
    "Quality Control Sample-Lab Spike of a Lab Blank",
    "Quality Control Sample-Lab Surrogate Control Standard",
    "Quality Control Sample-Lab Surrogate Control Standard Duplicate",
    "Quality Control Sample-Lab Surrogate Method Blank",
    "Quality Control Sample-Measurement Precision Sample",
    "Quality Control Sample-Reference Sample",
    "Quality Control-Calibration Check",
    "Quality Control-Calibration Check Buffer",
    "Sample-Positive Control"
  )
  other <- c("Quality Control Sample-Other")
  nonQC <- c(
    "Field Msr/Obs",
    "Field Msr/Obs-Continuous Time Series",
    "Field Msr/Obs-Habitat Assessment",
    "Field Msr/Obs-Incidental",
    "Field Msr/Obs-Portable Data Logger",
    "Sample-Composite With Parents",
    "Sample-Composite Without Parents",
    "Sample-Field Split",
    "Sample-Field Subsample",
    "Sample-Integrated Cross-Sectional Profile",
    "Sample-Integrated Flow Proportioned",
    "Sample-Integrated Horizontal Profile",
    "Sample-Integrated Horizontal and Vertical Composite Profile",
    "Sample-Integrated Time Series",
    "Sample-Integrated Vertical Profile",
    "Sample-Other",
    "Sample-Routine"
  )
  groups <- list(
    "QC_replicate" = rep,
    "QC_duplicate" = dup,
    "QC_blank" = blank,
    "QC_calibration" = cal,
    "QC_other" = other,
    "Non_QC" = nonQC
  )
  # Force classification by Code
  df <- .tada_flag_by_groups(
    df,
    source_col = "Code",
    out_col = "TADA.ActivityType.Flag",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  df <- unique(.tada_trim_char_cols(df))
  new.atcs <- data.frame(
    Code = c(
      "Quality Control Sample-Blind",
      "Unknown",
      "Not determined",
      "Sample"
    ),
    Description = rep("Hard-coded activity type not in WQX domain", 4),
    TADA.ActivityType.Flag = c(
      "QC_duplicate",
      "Not Reviewed",
      "Not Reviewed",
      "Non_QC"
    ),
    Last.Change.Date = c("8/11/2023", "8/11/2023", "1/5/2024", "1/5/2024"),
    stringsAsFactors = FALSE
  )
  df <- .tada_bind_rows(df, new.atcs)
  df <- unique(.tada_trim_char_cols(df))
  df
}

# Monitoring Location Type Name
.TADA_flag_MonLocTypeRef <- function(df) {
  surface <- c(
    "BEACH Program Site-Channelized stream",
    "BEACH Program Site-Estuary",
    "BEACH Program Site-Great Lake",
    "BEACH Program Site-Lake",
    "BEACH Program Site-River/Stream",
    "Canal Drainage",
    "Canal Irrigation",
    "Canal Transport",
    "Constructed Wetland",
    "Estuary",
    "Great Lake",
    "Intertidal",
    "Lake",
    "Ocean",
    "Other-Surface Water",
    "Pipe, Unspecified Source",
    "Mine/Mine Discharge",
    "Pond",
    "Pond-Anchialine",
    "Pond-Stock",
    "Pond-Wastewater",
    "Reservoir",
    "River/Stream",
    "River/Stream Ephemeral",
    "River/Stream Intermittent",
    "River/Stream Perennial",
    "Riverine Impoundment",
    "Subtidal",
    "Wetland Estuarine-Ditch",
    "Wetland Estuarine-Emergent",
    "BEACH Program Site-Ocean",
    "Wetland Estuarine-Forested",
    "Wetland Estuarine-Marsh",
    "Wetland Estuarine-Pool",
    "River/stream Effluent-Dominated",
    "Wetland Estuarine-Scrub-Shrub",
    "Wetland Estuarine-Tidal Creek",
    "Wetland Lacustrine-Emergent",
    "Wetland Palustrine-Emergent",
    "Wetland Palustrine-Forested",
    "Wetland Palustrine-Moss-Lichen",
    "Wetland Palustrine-Shrub-Scrub",
    "Wetland Riverine-Emergent",
    "Wetland Undifferentiated",
    "Wetland Palustrine Pond",
    "Channelized Stream",
    "Estuary-Freshwater",
    "Pond-Sediment",
    "Pond-Stormwater",
    "Spring",
    "Wetland Lacustrine-Aquatic Bed",
    "Wetland Lacustrine-Unconsolidated Bottom",
    "Wetland Riverine-Unconsolidated Bottom",
    "Wetland Riverine-Aquatic Bed",
    "Storm Sewer",
    "Combined Sewer",
    "Mine/Mine Discharge Adit (Mine Entrance)",
    "Mine/Mine Discharge Tailings Pile",
    "Mine/Mine Discharge Waste Rock Pile",
    "Waste Sewer",
    "Seep",
    "Playa",
    "BEACH Program Site-Land runoff",
    "BEACH Program Site-Storm sewer",
    "BEACH Program Site-Waste sewer",
    "Floodwater Urban",
    "Floodwater non-Urban"
  )
  groundwater <- c("Cave", "Well", "Other-Ground Water")
  df <- .tada_flag_by_groups(
    df,
    source_col = "Name",
    out_col = "TADA.Media.Flag",
    groups = list("Surface Water" = surface, "Groundwater" = groundwater),
    default = "",
    na_default = ""
  )
  unique(.tada_trim_char_cols(df))
}

# WQP Organization/Provider (column selection)
.TADA_prepare_WQPOrgProviderRef <- function(df) {
  keep <- c("OrganizationIdentifier", "OrganizationFormalName", "ProviderName")
  keep <- keep[keep %in% names(df)]
  unique(.tada_trim_char_cols(df[, keep, drop = FALSE]))
}

# =========================
# Paired getters + updates
# =========================

#' Get WQX Characteristic Domain Table (internal-only)
#'
#' Loads the package-installed internal reference table from inst/extdata and caches
#' it for the session. No network is used. Arguments download_only and refresh are
#' kept for backward compatibility but are ignored.
#'
#' @return data.frame with columns: CharacteristicName, Comparable.Name, CAS.Number, Char_Flag
#' @param download_only Ignored. Present for backward compatibility.
#' @param refresh Ignored. Present for backward compatibility.
#' @export
TADA_GetCharacteristicRef <- function(download_only = FALSE, refresh = FALSE) {
  # Cache hit
  cached <- .tada_cache_get(.WQXCharacteristicRef_cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  # Load internal RDA (works in dev and installed builds)
  ref <- .tada_load_extdata_rda(
    pkg = "EPATADA",
    filename = "WQXCharacteristicRef.rda",
    object_name = "WQXCharacteristicRef",
    # Old RDAs might have a different order; we enforce below
    required_cols = NULL,
    trim = TRUE
  )
  if (is.null(ref)) {
    stop(
      "Internal extdata 'WQXCharacteristicRef.rda' not found or invalid. ",
      "Rebuild the package with an internal copy."
    )
  }

  # Enforce final schema and order (drop extras; fill missing with NA)
  keep_order <- c(
    "CharacteristicName",
    "Comparable.Name",
    "CAS.Number",
    "Char_Flag"
  )
  for (nm in keep_order) {
    if (!nm %in% names(ref)) ref[[nm]] <- NA_character_
  }
  ref <- ref[, keep_order, drop = FALSE]
  ref <- unique(.tada_trim_char_cols(ref))

  .tada_cache_set(.WQXCharacteristicRef_cache_key, ref)
  ref
}

#' Update EPATADA Internal Copy of WQX Characteristic Domain Table (DEV-TIME ONLY)
#' Downloads the live CSV, normalizes to the 4-column schema, and writes
#' inst/extdata/WQXCharacteristicRef.rda if changed.
#' @keywords internal
.TADA_UpdateCharacteristicRef <- function() {
  raw.data <- .tada_read_csv_url(
    .WQX_URLS$Characteristic,
    stringsAsFactors = FALSE
  )
  if (is.null(raw.data)) {
    stop(
      ".TADA_UpdateCharacteristicRef: download failed; cannot update internal file."
    )
  }

  # Exact-headers normalizer: requires Name, Comparable.Name, CAS.Number, Domain.Value.Status
  ref <- .TADA_normalize_characteristic_ref(raw.data)
  if (is.null(ref)) {
    stop(
      ".TADA_UpdateCharacteristicRef: Unexpected columns in downloaded table: ",
      paste(names(raw.data), collapse = ", ")
    )
  }

  # Enforce final schema/order defensively
  keep_order <- c(
    "CharacteristicName",
    "Comparable.Name",
    "CAS.Number",
    "Char_Flag"
  )
  for (nm in keep_order) {
    if (!nm %in% names(ref)) ref[[nm]] <- NA_character_
  }
  ref <- ref[, keep_order, drop = FALSE]
  ref <- unique(.tada_trim_char_cols(ref))

  .tada_save_ext_rda(
    obj = ref,
    obj_name = "WQXCharacteristicRef",
    pkg = "EPATADA",
    filename = "WQXCharacteristicRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(ref)
}

#' WQX QAQC Characteristic Validation Reference Table
#' @return data.frame with TADA.WQXVal.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Characteristic Validation reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetWQXCharValRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXCharValRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$QAQCCharacteristicValidation,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetWQXCharValRef(download_only=TRUE): download failed.")
    }
    .tada_require_cols(df, c("Status"), "QAQC Characteristic Validation")
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$QAQCCharacteristicValidation,
      fallback_filename = "WQXcharValRef.rda",
      object_name = "WQXcharValRef",
      pkg = "EPATADA",
      required_cols = c("Status"), # add this
      on_fail_message = "Downloading latest Validation Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_WQXCharValRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXCharValRef_cache_key, df)
  }
  df
}

#' Update Characteristic Validation Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateWQXCharValRef <- function() {
  df <- TADA_GetWQXCharValRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXcharValRef",
    pkg = "EPATADA",
    filename = "WQXcharValRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Measure Unit Reference Table
#' @return data.frame of measure units
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Measure Unit reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetMeasureUnitRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXUnitRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  if (download_only) {
    df <- .tada_read_csv_url(.WQX_URLS$MeasureUnit, stringsAsFactors = FALSE)
    if (is.null(df)) {
      stop("TADA_GetMeasureUnitRef(download_only=TRUE): download failed.")
    }
    .tada_require_cols(df, c("Code"), "Measure Unit") # only "Code" here
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$MeasureUnit,
      fallback_filename = "WQXunitRef.rda",
      object_name = "WQXunitRef",
      pkg = "EPATADA",
      required_cols = c("Code"),
      on_fail_message = "Downloading latest Measure Unit Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }

  df <- .TADA_prepare_MeasureUnitRef(df)

  if (!download_only) {
    .tada_cache_set(.WQXUnitRef_cache_key, df)
  }
  df
}

#' Update Measure Unit Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateMeasureUnitRef <- function() {
  df <- TADA_GetMeasureUnitRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXunitRef",
    pkg = "EPATADA",
    filename = "WQXunitRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

.tada_normalize_detcond_cols <- function(df) {
  # Back-compat: map known legacy label columns to "Name"
  if (!("Name" %in% names(df))) {
    candidates <- c(
      "ResultDetectionConditionText",
      "Result.Detection.Condition",
      "DetectionConditionName"
    )
    alt <- intersect(candidates, names(df))
    if (length(alt) >= 1L) {
      names(df)[names(df) == alt[1L]] <- "Name"
    }
  }
  df
}

#' Get WQX Result Detection Condition Reference Table
#'
#' Retrieve the WQX Result Detection Condition domain table, normalize it, and
#' add a classification column, TADA.Detection_Type. The classifier assigns each
#' detection condition to one of: "Non-Detect", "Over-Detect", "Other", or
#' "Not Reviewed".
#'
#' When download_only = FALSE (default), the function first attempts to download
#' the latest table from WQX; if the download fails or the structure is
#' unexpected (e.g., missing the required "Name" column), it falls back to the
#' package’s installed extdata (.rda). The resulting table is cached for the
#' current R session.
#'
#' @return A data.frame that includes at least:
#'   - Name: the detection condition name (from WQX)
#'   - TADA.Detection_Type: the assigned classification
#'   - Additional columns such as Description and Last.Change.Date are returned
#'     when provided by WQX
#'
#' @param download_only Logical. If TRUE, bypasses the cache and package
#'   fallback and attempts to download the latest Detection Condition table
#'   directly from WQX, returning it without updating the cache. Errors if the
#'   download fails. If FALSE (default), uses a cached copy when available and
#'   updates the cache; on download failure (or unexpected structure), falls
#'   back to the package’s internal file.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore
#'   any cached copy and attempt to retrieve a fresh table (download, falling
#'   back to the package’s internal file on failure), then update the cache. If
#'   FALSE (default), return the cached table when available. Ignored when
#'   download_only = TRUE.
#' @param quiet Logical. If TRUE, suppresses fallback messages when a live
#'   download fails and the function reverts to the installed extdata. Default
#'   is FALSE.
#'
#' @examples
#' # Cached retrieval (download with fallback as needed)
#' ref <- TADA_GetDetCondRef()
#'
#' # Force a fresh retrieval (ignores cache)
#' ref_fresh <- TADA_GetDetCondRef(refresh = TRUE)
#'
#' # Download only: no cache update and no fallback (errors if offline)
#' \dontrun{
#' ref_live <- TADA_GetDetCondRef(download_only = TRUE)
#' }
#'
#' @export
TADA_GetDetCondRef <- function(
  download_only = FALSE,
  refresh = FALSE,
  quiet = FALSE
) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXDetCondRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  msg <- if (quiet) {
    NULL
  } else {
    "Live Detection Condition table unavailable or unexpected structure; using package fallback."
  }

  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$ResultDetectionCondition,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetDetCondRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$ResultDetectionCondition,
      fallback_filename = "WQXResultDetectionConditionRef.rda",
      object_name = "WQXResultDetectionConditionRef",
      pkg = "EPATADA",
      # temporarily allow either new or legacy names via post-normalization
      required_cols = NULL,
      on_fail_message = msg
    )
  }

  # Name harmonization and structural check (covers both live and fallback)
  df <- .tada_normalize_detcond_cols(df)
  .tada_require_cols(df, c("Name"), "Result Detection Condition")

  # Flag, cache, return
  df <- .TADA_flag_DetCondRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXDetCondRef_cache_key, df)
  }
  df
}

#' Update Result Detection Condition Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateDetCondRef <- function() {
  df <- TADA_GetDetCondRef(download_only = TRUE)
  names(df) <- make.names(
    trimws(sub("^\ufeff", "", names(df), perl = TRUE)),
    unique = TRUE
  )
  .tada_save_ext_rda(
    df,
    obj_name = "WQXResultDetectionConditionRef",
    pkg = "EPATADA",
    filename = "WQXResultDetectionConditionRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Detection/Quantitation Limit Type Reference Table
#' @return data.frame with TADA.Limit_Type added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Detection Limit reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetDetLimitRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXDetLimitRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$DetectionQuantitationLimitType,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetDetLimitRef(download_only=TRUE): download failed.")
    }
    .tada_require_cols(df, c("Name"), "Detection/Quantitation Limit Type")
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$DetectionQuantitationLimitType,
      fallback_filename = "WQXDetectionQuantitationLimitTypeRef.rda",
      object_name = "WQXDetectionQuantitationLimitTypeRef",
      pkg = "EPATADA",
      required_cols = c("Name"),
      on_fail_message = "Downloading latest Detection Limit Type Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_DetLimitRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXDetLimitRef_cache_key, df)
  }
  df
}

#' Update Detection Quantitation Limit Type Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateDetLimitRef <- function() {
  df <- TADA_GetDetLimitRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXDetectionQuantitationLimitTypeRef",
    pkg = "EPATADA",
    filename = "WQXDetectionQuantitationLimitTypeRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Activity Type Reference Table
#' @return data.frame with TADA.ActivityType.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetActivityTypeRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXActivityTypeRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  if (download_only) {
    df <- .tada_read_csv_url(.WQX_URLS$ActivityType, stringsAsFactors = FALSE)
    if (is.null(df)) {
      stop("TADA_GetActivityTypeRef(download_only=TRUE): download failed.")
    }
    # Normalize headers defensively (BOM/trim/check.names)
    df <- .tada_norm_colnames(df)
    .tada_require_cols(df, c("Code"), "Activity Type")
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$ActivityType,
      fallback_filename = "WQXActivityTypeRef.rda",
      object_name = "WQXActivityTypeRef",
      pkg = "EPATADA",
      required_cols = c("Code"),
      on_fail_message = "Downloading latest Activity Type Reference Table failed! Falling back to (possibly outdated) internal file."
    )
    df <- .tada_norm_colnames(df)
  }

  df <- .TADA_flag_ActivityTypeRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXActivityTypeRef_cache_key, df)
  }
  df
}

#' Update Activity Type Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateActivityTypeRef <- function() {
  df <- TADA_GetActivityTypeRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXActivityTypeRef",
    pkg = "EPATADA",
    filename = "WQXActivityTypeRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Monitoring Location Type Name Reference Table
#' @return data.frame with TADA.Media.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Monitoring Location Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetMonLocTypeRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXMonLocTypeRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$MonitoringLocationType,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetMonLocTypeRef(download_only=TRUE): download failed.")
    }
    .tada_require_cols(df, c("Name"), "Monitoring Location Type Name")
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$MonitoringLocationType,
      fallback_filename = "WQXMonitoringLocationTypeNameRef.rda",
      object_name = "WQXMonitoringLocationTypeNameRef",
      pkg = "EPATADA",
      required_cols = c("Name"),
      on_fail_message = "Downloading latest Monitoring Location Type Name Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_MonLocTypeRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXMonLocTypeRef_cache_key, df)
  }
  df
}

#' Update Monitoring Location Type Name Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateMonLocTypeRef <- function() {
  df <- TADA_GetMonLocTypeRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXMonitoringLocationTypeNameRef",
    pkg = "EPATADA",
    filename = "WQXMonitoringLocationTypeNameRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQP Organization and Provider Reference Table
#' @return data.frame with OrganizationIdentifier, OrganizationFormalName, ProviderName
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest WQP Organization reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetWQPOrganizationRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQPProviderRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(.WQP_URLS$Organization, stringsAsFactors = FALSE)
    if (is.null(df)) {
      stop("TADA_GetWQPOrganizationRef(download_only=TRUE): download failed.")
    }
    df <- .TADA_prepare_WQPOrgProviderRef(df)
  } else {
    df <- .tada_read_csv_url(.WQP_URLS$Organization, stringsAsFactors = FALSE)
    if (is.null(df)) {
      message(
        "Downloading latest WQP Organization and Provider Reference Table failed! Falling back to (possibly outdated) internal file."
      )
      df <- .tada_load_extdata_rda(
        pkg = "EPATADA",
        filename = "WQPOrganizationRef.rda",
        object_name = "WQPOrganizationRef", # renamed here
        required_cols = c(
          "OrganizationIdentifier",
          "OrganizationFormalName",
          "ProviderName"
        ),
        trim = TRUE
      )
      if (is.null(df)) {
        stop("Fallback extdata 'WQPOrganizationRef.rda' not found or invalid.")
      }
    } else {
      df <- .TADA_prepare_WQPOrgProviderRef(df)
    }
  }
  if (!download_only) {
    .tada_cache_set(.WQPProviderRef_cache_key, df)
  }
  df
}

#' Update WQP Organization Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateWQPOrganizationRef <- function() {
  df <- TADA_GetWQPOrganizationRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQPOrganizationRef", # renamed here
    pkg = "EPATADA",
    filename = "WQPOrganizationRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

# TADA Result Measure Qualifier Code Flags
.TADA_flag_MeasureQualifierCodeRef <- function(df) {
  suspect <- c(
    "(",
    "+",
    "AR",
    "BS",
    "BSR",
    "BT",
    "BVER",
    "C",
    "CAN",
    "CBC",
    "TT",
    "UDL",
    "UDQ",
    "CSR",
    "DE",
    "EER",
    "EFAI",
    "FDB",
    "FDC",
    "FDL",
    "FFB",
    "FFD",
    "TMLF",
    "UNC",
    "TOC",
    "FFS",
    "FFT",
    "FH",
    "FIS",
    "FL",
    "FLD",
    "FLS",
    "FMD",
    "ITNA",
    "JCN",
    "RLRS",
    "FMS",
    "FPC",
    "FPR",
    "FQC",
    "FRS",
    "FSD",
    "FSL",
    "FSP",
    "FUB",
    "NPNF",
    "RPDX",
    "H",
    "H2",
    "H3",
    "HMSD",
    "C25",
    "HE",
    "HIM",
    "ICA",
    "IS",
    "ISAC",
    "ITNM",
    "OS3",
    "QCI",
    "INT",
    "IQCOL",
    "ISP",
    "A",
    "D",
    "DT",
    "EMPC",
    "HH",
    "HIB",
    "ISR**",
    "MDL",
    "OUT",
    "JCW",
    "KCF",
    "KCX",
    "KK",
    "LAC",
    "LBF",
    "CNT",
    "GR4",
    "HICC",
    "J-R",
    "NW",
    "PB",
    "LO",
    "$",
    ")",
    "*",
    "ESD",
    "EST",
    "EVA",
    "EVAD",
    "EVID",
    "FPP",
    "G",
    "LLS",
    "OA3",
    "PK",
    "MI",
    "MSR",
    "NAI",
    "NLBL",
    "NLRO",
    "NN",
    "NRO",
    "F",
    "FLA",
    "I",
    "MSD",
    "NHS",
    "NRP",
    "NRR",
    "NSQ",
    "PNQ",
    "Q",
    "QC",
    "R",
    "RA",
    "FEQ",
    "FLC",
    "GXB",
    "NA",
    "OTHER",
    "RPO",
    "S2",
    "SCA",
    "SCF",
    "SCP",
    "SCX",
    "SD%EL",
    "SDROL",
    "SSR",
    "PP",
    "PPD",
    "PRE",
    "SUS",
    "V",
    "^",
    "RNON",
    "B",
    "CBG",
    "SSRV",
    "K5",
    "M4",
    "H5",
    "K12",
    "B3",
    "M3",
    "F5",
    "F1",
    "H4",
    "#",
    "C40",
    "DIL",
    "EC",
    "EX",
    "F6",
    "F7",
    "FS",
    "HF",
    "HTI",
    "HV",
    "ICM",
    "K10",
    "K11",
    "K6",
    "K7",
    "K8",
    "K9",
    "MHA",
    "MSI",
    "OT",
    "PAM",
    "REC",
    "STE",
    "T5",
    "V1"
  )
  pass <- c(
    "P",
    "NRS",
    "NRB",
    "&",
    "=",
    "M6F",
    "LVER",
    "LSSR",
    "LQ",
    "LOPR",
    "LMSD",
    "LICC",
    "HTH",
    "HNRO",
    "HMSR",
    "AC",
    "AL",
    "ALK",
    "ALT",
    "LOB",
    "AP",
    "BAC",
    "CAJ",
    "CBL",
    "CC",
    "CDI",
    "CG",
    "CKB",
    "CKBJ",
    "CKG",
    "CKJ",
    "CLC",
    "CON",
    "CUG",
    "DEC",
    "DI",
    "DOM",
    "ECI",
    "HLBL",
    "HQ",
    "HVER",
    "J",
    "J+",
    "J-",
    "L",
    "LCS",
    "LF",
    "LIS",
    "LL",
    "LLBL",
    "LMSR",
    "LNRO",
    "LR",
    "LT",
    "N",
    "NFNS",
    "O",
    "PQL",
    "RC",
    "REX",
    "RIN",
    "RMAX",
    "RNAF",
    "RP",
    "RR",
    "RV",
    "RVB",
    "SBB",
    "SLB",
    "SM",
    "SS",
    "T",
    "VS",
    "VVRR",
    "VVRR2",
    "ZZ",
    "J-1",
    "NA",
    "TR",
    "LE",
    "EMCL",
    "SUB",
    "F3",
    "F2",
    "F4"
  )
  nondetect <- c(
    "BQL",
    "2-5B",
    "U",
    "LTGTE",
    "K",
    "IDL",
    "<2B",
    "BRL",
    "D>T",
    "DL"
  )
  overdetect <- c("E", "EE", "GT")
  df <- .tada_flag_by_groups(
    df,
    source_col = "Code",
    out_col = "TADA.MeasureQualifierCode.Flag",
    groups = list(
      "Non-Detect" = nondetect,
      "Over-Detect" = overdetect,
      "Suspect" = suspect,
      "Pass" = pass
    ),
    default = "Not Reviewed",
    na_default = "Pass"
  )
  unique(.tada_trim_char_cols(df))
}

#' Get WQX Result Measure Qualifier Code Reference Table
#' @return data.frame with TADA.MeasureQualifierCode.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Result Measure Qualifier reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetMeasureQualifierCodeRef <- function(
  download_only = FALSE,
  refresh = FALSE
) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXMeasureQualifierCodeRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    # Verbose TRUE to print the underlying cause if it fails
    df <- .tada_read_csv_url(
      .WQX_URLS$ResultMeasureQualifier,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop(
        "TADA_GetMeasureQualifierCodeRef(download_only=TRUE): download failed for ",
        .WQX_URLS$ResultMeasureQualifier,
        "."
      )
    }
    .tada_require_cols(df, c("Code"), "Result Measure Qualifier")
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$ResultMeasureQualifier,
      fallback_filename = "WQXMeasureQualifierCodeRef.rda",
      object_name = "WQXMeasureQualifierCodeRef",
      pkg = "EPATADA",
      required_cols = c("Code"),
      on_fail_message = "Downloading latest Measure Qualifier Code Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_MeasureQualifierCodeRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXMeasureQualifierCodeRef_cache_key, df)
  }
  df
}

#' Update WQX Result Measure Qualifier Code Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateMeasureQualifierCodeRef <- function() {
  df <- TADA_GetMeasureQualifierCodeRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXMeasureQualifierCodeRef",
    pkg = "EPATADA",
    filename = "WQXMeasureQualifierCodeRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' WQX Characteristic Alias Reference Table
#' @return data.frame with "Domain", "Unique.Identifier", "Alias.Name", "Description",
#'          "Characteristic.Name", "Alias.Type.Name", "Last.Change.Date"
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Characteristic Alias reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
#' @examples
#' WQX_alias_ref <- TADA_GetWQXCharAliasRef(download_only = FALSE, refresh = FALSE)
#'
TADA_GetWQXCharAliasRef <- function(download_only = FALSE, refresh = FALSE) {
  # Return cached table unless refresh is requested
  if (!download_only) {
    cached <- .tada_cache_get(.WQXCharAliasRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  # Helper: download ZIP, unzip, locate CSV, read it, preserving row order
  .download_unzip_read_alias <- function() {
    zip_url <- "https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicAlias_CSV.zip"
    temp_zip <- tempfile(fileext = ".zip")
    temp_dir <- tempfile("wqx_alias_unzip_")
    on.exit(
      {
        if (dir.exists(temp_dir)) {
          unlink(temp_dir, recursive = TRUE, force = TRUE)
        }
        if (file.exists(temp_zip)) unlink(temp_zip, force = TRUE)
      },
      add = TRUE
    )

    # 5-minute default timeout (configurable via options(EPATADA.timeout))
    timeout_sec <- suppressWarnings(as.numeric(getOption(
      "EPATADA.timeout",
      300
    )))
    if (!is.finite(timeout_sec) || timeout_sec <= 0) {
      timeout_sec <- 300
    }

    # Download ZIP with longer timeout; prefer curl if available
    ok <- FALSE
    if (requireNamespace("curl", quietly = TRUE)) {
      h <- curl::new_handle(followlocation = TRUE)
      curl::handle_setheaders(
        h,
        "User-Agent" = getOption("EPATADA.user_agent", "EPATADA (R)")
      )
      curl::handle_setopt(h, connecttimeout = 60, timeout = timeout_sec)
      ok <- tryCatch(
        {
          curl::curl_download(
            zip_url,
            destfile = temp_zip,
            handle = h,
            mode = "wb"
          )
          file.exists(temp_zip) &&
            is.finite(file.info(temp_zip)$size) &&
            file.info(temp_zip)$size > 0
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
      )
    }
    if (!ok) {
      old_to <- getOption("timeout")
      old_to_num <- suppressWarnings(as.numeric(old_to))
      if (!is.finite(old_to_num)) {
        old_to_num <- 60
      }
      on.exit(options(timeout = old_to), add = TRUE)
      options(timeout = max(old_to_num, timeout_sec))

      ok <- tryCatch(
        {
          utils::download.file(
            zip_url,
            destfile = temp_zip,
            mode = "wb",
            method = "libcurl",
            quiet = TRUE
          )
          file.exists(temp_zip) &&
            is.finite(file.info(temp_zip)$size) &&
            file.info(temp_zip)$size > 0
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
      )

      if (!ok && .Platform$OS.type == "windows") {
        ok <- tryCatch(
          {
            utils::download.file(
              zip_url,
              destfile = temp_zip,
              mode = "wb",
              method = "wininet",
              quiet = TRUE
            )
            file.exists(temp_zip) &&
              is.finite(file.info(temp_zip)$size) &&
              file.info(temp_zip)$size > 0
          },
          error = function(e) FALSE,
          warning = function(w) FALSE
        )
      }
    }
    if (!ok) {
      return(NULL)
    }

    # List archive and choose the CSV (robust to small filename changes)
    listing <- tryCatch(
      utils::unzip(temp_zip, list = TRUE),
      error = function(e) NULL
    )
    if (is.null(listing) || !is.data.frame(listing) || !nrow(listing)) {
      return(NULL)
    }

    nms <- as.character(listing$Name)
    is_csv <- grepl("(?i)\\.csv$", nms)
    if (!any(is_csv)) {
      return(NULL)
    }

    cand <- which(is_csv)
    base <- basename(nms[cand])
    exact_ci <- cand[tolower(base) == "characteristic alias.csv"]
    pat_ci <- cand[grep(
      "(?i)characteristic.*alias.*\\.csv$",
      base,
      perl = TRUE
    )]
    pick_idx <- if (length(exact_ci) >= 1) {
      exact_ci[1]
    } else if (length(pat_ci) >= 1) {
      pat_ci[1]
    } else {
      cand[which.max(as.numeric(listing$Length[cand]))]
    }
    target_in_zip <- nms[pick_idx]

    # Extract only the chosen CSV
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    extracted <- tryCatch(
      utils::unzip(temp_zip, files = target_in_zip, exdir = temp_dir),
      error = function(e) character(0)
    )
    if (!length(extracted)) {
      return(NULL)
    }

    target_csv <- file.path(temp_dir, target_in_zip)
    if (!file.exists(target_csv)) {
      if (length(extracted) && file.exists(extracted[1])) {
        target_csv <- extracted[1]
      } else {
        return(NULL)
      }
    }

    # Read and normalize; preserve row order
    try_read <- function(enc) {
      tryCatch(
        utils::read.csv(
          target_csv,
          stringsAsFactors = FALSE,
          fileEncoding = enc,
          comment.char = "",
          check.names = TRUE
        ),
        error = function(e) NULL
      )
    }
    df <- try_read("UTF-8")
    if (is.null(df)) {
      df <- try_read("latin1")
    }
    if (is.null(df)) {
      return(NULL)
    }

    # Normalize headers and trim character cols
    nm <- names(df)
    nm <- sub("^\ufeff", "", nm, perl = TRUE)
    nm <- trimws(nm)
    nm <- make.names(nm, unique = TRUE)
    names(df) <- nm
    df <- .tada_trim_char_cols(df)
    rownames(df) <- NULL
    df
  }

  # Use the helper to fetch, with fallback to installed RDA when allowed
  if (download_only) {
    df <- .download_unzip_read_alias()
    if (is.null(df)) {
      stop("TADA_GetWQXCharAliasRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .download_unzip_read_alias()
    if (is.null(df)) {
      message(
        "Downloading latest WQX Characteristic Alias Reference Table failed! ",
        "Falling back to (possibly outdated) internal file."
      )
      df <- .tada_load_extdata_rda(
        pkg = "EPATADA",
        filename = "WQXCharAliasRef.rda",
        object_name = "WQXCharAliasRef",
        trim = TRUE
      )
      if (is.null(df)) {
        stop("Fallback extdata 'WQXCharAliasRef.rda' not found or invalid.")
      }
      rownames(df) <- NULL
    }
  }

  if (!download_only) {
    .tada_cache_set(.WQXCharAliasRef_cache_key, df)
  }
  df
}

# Update Characteristic Validation Reference Table internal file (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateWQXCharAliasRef <- function() {
  df <- TADA_GetWQXCharAliasRef(download_only = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXCharAliasRef",
    pkg = "EPATADA",
    filename = "WQXCharAliasRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' ATTAINS Organization Identifier Reference Table
#'
#' Retrieve the latest ATTAINS domain values for org identifiers. Attempts a live
#' query via rExpertQuery::EQ_DomainValues("org_id"); on failure (or if
#' rExpertQuery isn't installed), falls back to the package’s installed
#' extdata (.rda). Results are cached in the current session.
#'
#' The returned columns mirror the source endpoint. Content is de-duplicated.
#'
#' @param download_only Logical. If TRUE, bypasses cache and fallback and queries
#'   ATTAINS live. Errors if rExpertQuery is unavailable or the query fails.
#'   Default FALSE.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore
#'   any cached copy and attempt a fresh retrieval (download with extdata fallback),
#'   then update the cache.
#' @param quiet Logical. If TRUE, suppress non-critical fallback messages.
#' @return data.frame of organization domain values (de-duplicated)
#' @export
TADA_GetATTAINSOrgIDsRef <- function(
  download_only = FALSE,
  refresh = FALSE,
  quiet = FALSE
) {
  if (!download_only) {
    cached <- .tada_cache_get(.ATTAINSOrgIDsRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  # Attempt live download via rExpertQuery if present
  live_ok <- requireNamespace("rExpertQuery", quietly = TRUE)
  df <- NULL
  if (live_ok) {
    df <- tryCatch(
      suppressWarnings(suppressMessages(rExpertQuery::EQ_DomainValues(
        "org_id"
      ))),
      error = function(e) NULL
    )
  }

  if (download_only) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      stop(
        "TADA_GetATTAINSOrgIDsRef(download_only=TRUE): live query failed or rExpertQuery not available."
      )
    }
    df <- unique(.tada_trim_char_cols(df))
    return(df)
  }

  # Fallback to installed RDA if live failed
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    if (!quiet) {
      message(
        "Downloading latest ATTAINS Org ID domain values failed; falling back to internal RDA."
      )
    }
    df <- .tada_load_extdata_rda(
      pkg = "EPATADA",
      filename = .ATTAINS_ORG_IDS_RDA_FILENAME,
      object_name = .ATTAINS_ORG_IDS_OBJ_NAME,
      trim = TRUE
    )
    if (is.null(df)) {
      stop(
        "Fallback extdata '",
        .ATTAINS_ORG_IDS_RDA_FILENAME,
        "' not found or invalid in the installed package."
      )
    }
  } else {
    df <- unique(.tada_trim_char_cols(df))
  }

  .tada_cache_set(.ATTAINSOrgIDsRef_cache_key, df)
  df
}

#' Update EPATADA internal ATTAINS Org ID table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateATTAINSOrgIDsRef <- function() {
  df <- TADA_GetATTAINSOrgIDsRef(download_only = TRUE)
  .tada_save_ext_rda(
    obj = df,
    obj_name = .ATTAINS_ORG_IDS_OBJ_NAME,
    pkg = "EPATADA",
    filename = .ATTAINS_ORG_IDS_RDA_FILENAME,
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' ATTAINS Parameter and Use Name by Organization Reference
#'
#' Builds a compact crosswalk of parameter and use names by ATTAINS organization,
#' de-duplicated and cached for the session. Attempts live retrieval via
#' rExpertQuery::EQ_NationalExtract("assessments"). If the live query is
#' unavailable or empty, falls back to the package’s installed extdata (.rda).
#'
#' Output columns:
#'   - ATTAINS.OrganizationIdentifier
#'   - ATTAINS.OrganizationName
#'   - ATTAINS.OrganizationType
#'   - ATTAINS.ParameterName
#'   - ATTAINS.UseName
#'   - ATTAINS.WaterType
#'
#' By default, only the latest reporting cycle per organization is used when
#' reportingCycle is available. Set latest_cycle_only = FALSE to include all cycles.
#'
#' @param latest_cycle_only Logical; TRUE (default) keeps only each organization’s
#'   latest reporting cycle when possible; FALSE returns all cycles.
#' @param download_only Logical. If TRUE, bypasses cache and fallback and queries
#'   ATTAINS live. Errors if rExpertQuery is unavailable or the query fails.
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore
#'   cache and attempt a fresh retrieval (download with extdata fallback), then cache.
#' @param quiet Logical. If TRUE, suppress non-critical fallback messages.
#' @return data.frame with the columns listed above, de-duplicated
#' @export
TADA_GetATTAINSParamUseOrgRef <- function(
  latest_cycle_only = TRUE,
  download_only = FALSE,
  refresh = FALSE,
  quiet = FALSE
) {
  cache_key <- paste0(
    .ATTAINSParamUseOrgRef_cache_key,
    if (latest_cycle_only) "_latest" else "_all"
  )
  if (!download_only) {
    cached <- .tada_cache_get(cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }

  # Live retrieval via rExpertQuery if available
  live_ok <- requireNamespace("rExpertQuery", quietly = TRUE)
  nat <- NULL
  if (live_ok) {
    nat <- tryCatch(
      {
        suppressWarnings(suppressMessages(rExpertQuery::EQ_NationalExtract(
          "assessments"
        )))
      },
      error = function(e) NULL
    )
  }

  # Helper: normalize the live table into the stable schema using base R
  .normalize_assessment <- function(nat_df) {
    if (is.null(nat_df) || !is.data.frame(nat_df) || nrow(nat_df) == 0) {
      return(NULL)
    }

    # Keep only relevant columns that might exist
    keep <- c(
      "organizationId",
      "organizationName",
      "organizationType",
      "parameterName",
      "useName",
      "waterType",
      "reportingCycle"
    )
    has <- intersect(colnames(nat_df), keep)
    if (!("organizationId" %in% has)) {
      return(NULL)
    } # critical key missing

    x <- nat_df[, has, drop = FALSE]

    # Optionally reduce to latest cycle per organization when reportingCycle exists
    if (isTRUE(latest_cycle_only) && "reportingCycle" %in% names(x)) {
      rc_chr <- as.character(x$reportingCycle)
      suppressWarnings(rc_num <- as.numeric(rc_chr))

      org_ids <- as.character(x$organizationId)
      keep_idx <- rep(FALSE, nrow(x))

      # Per-organization max cycle logic (numeric if any non-NA, else character)
      uorg <- unique(org_ids)
      for (org in uorg) {
        g <- which(org_ids == org)
        rcg_num <- rc_num[g]
        if (all(is.na(rcg_num))) {
          rcg_chr <- rc_chr[g]
          if (all(is.na(rcg_chr))) {
            # If everything missing, keep all rows for this org
            keep_idx[g] <- TRUE
          } else {
            m <- max(rcg_chr, na.rm = TRUE)
            keep_idx[g] <- !is.na(rcg_chr) & rcg_chr == m
          }
        } else {
          m <- max(rcg_num, na.rm = TRUE)
          keep_idx[g] <- !is.na(rcg_num) & rcg_num == m
        }
      }
      x <- x[keep_idx, , drop = FALSE]
    }

    # Select/rename to stable schema
    out <- data.frame(
      ATTAINS.OrganizationIdentifier = if ("organizationId" %in% names(x)) {
        x[["organizationId"]]
      } else {
        NA_character_
      },
      ATTAINS.OrganizationName = if ("organizationName" %in% names(x)) {
        x[["organizationName"]]
      } else {
        NA_character_
      },
      ATTAINS.OrganizationType = if ("organizationType" %in% names(x)) {
        x[["organizationType"]]
      } else {
        NA_character_
      },
      ATTAINS.ParameterName = if ("parameterName" %in% names(x)) {
        x[["parameterName"]]
      } else {
        NA_character_
      },
      ATTAINS.UseName = if ("useName" %in% names(x)) {
        x[["useName"]]
      } else {
        NA_character_
      },
      ATTAINS.WaterType = if ("waterType" %in% names(x)) {
        x[["waterType"]]
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )

    # Trim and de-duplicate
    out <- unique(.tada_trim_char_cols(out))
    out
  }

  if (download_only) {
    out <- .normalize_assessment(nat)
    if (is.null(out) || nrow(out) == 0) {
      stop(
        "TADA_GetATTAINSParamUseOrgRef(download_only=TRUE): live query failed or returned no usable data."
      )
    }
    return(out)
  }

  # Fallback to installed RDA if live fails
  out <- .normalize_assessment(nat)
  if (is.null(out) || nrow(out) == 0) {
    if (!quiet) {
      message(
        "Downloading latest ATTAINS assessments failed; falling back to internal RDA."
      )
    }
    required_cols <- c(
      "ATTAINS.OrganizationIdentifier",
      "ATTAINS.OrganizationName",
      "ATTAINS.OrganizationType",
      "ATTAINS.ParameterName",
      "ATTAINS.UseName",
      "ATTAINS.WaterType"
    )
    out <- .tada_load_extdata_rda(
      pkg = "EPATADA",
      filename = "ATTAINSParamUseOrgRef.rda",
      object_name = "ATTAINSParamUseOrgRef",
      required_cols = required_cols,
      trim = TRUE
    )
    if (is.null(out)) {
      stop(
        "Fallback extdata 'ATTAINSParamUseOrgRef.rda' not found or invalid in the installed package."
      )
    }
    out <- unique(.tada_trim_char_cols(out))
  }

  .tada_cache_set(cache_key, out)
  out
}

#' Update EPATADA internal ATTAINS Param/Use-by-Org table (DEV-TIME ONLY)
#' Saves the latest-cycle view by default.
#' @param latest_cycle_only logical; default TRUE
#' @keywords internal
.TADA_UpdateATTAINSParamUseOrgRef <- function(latest_cycle_only = TRUE) {
  df <- TADA_GetATTAINSParamUseOrgRef(
    latest_cycle_only = latest_cycle_only,
    download_only = TRUE
  )
  .tada_save_ext_rda(
    obj = df,
    obj_name = "ATTAINSParamUseOrgRef",
    pkg = "EPATADA",
    filename = "ATTAINSParamUseOrgRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

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
