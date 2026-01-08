#' Update cached tribal layers (internal) — One GeoPackage per layer
#'
#' Downloads and refreshes cached tribal feature layers in `inst/extdata`,
#' writing each layer to its own GeoPackage file (`.gpkg`) and replacing it only
#' when content has actually changed.
#'
#' Epoch-millisecond numeric columns are auto-detected by magnitude and converted
#' to Date to avoid schema issues and produce cleaner fields.
#'
#' @section Dependencies:
#' - Imports: sf
#' - Suggests (optional preflight): jsonlite, arcgislayers
#'
#' @return Messages indicate whether each layer was
#' skipped (preflight or unchanged), created, or updated.
#'
#' @keywords internal
#' @noRd
TADA_UpdateTribalLayers <- function() {
  # ---- Resolve internal EPATADA objects without requiring export ----
  ns_get <- function(name) {
    ns <- asNamespace("EPATADA")
    if (exists(name, envir = ns, inherits = FALSE)) {
      get(name, envir = ns, inherits = FALSE)
    } else {
      stop("Object '", name, "' not found in EPATADA namespace.")
    }
  }

  # ---- Sidecar metadata (canonical signature + lastEditDate) ----
  meta_path <- function(dest_gpkg) {
    meta_dir <- file.path(dirname(dest_gpkg), ".meta")
    dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(
      meta_dir,
      paste0(basename(tools::file_path_sans_ext(dest_gpkg)), ".rds")
    )
  }
  read_meta <- function(dest_gpkg) {
    p <- meta_path(dest_gpkg)
    if (file.exists(p)) {
      tryCatch(readRDS(p), error = function(e) NULL)
    } else {
      NULL
    }
  }
  write_meta <- function(dest_gpkg, meta) {
    saveRDS(meta, meta_path(dest_gpkg))
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ---- Preflight: ArcGIS lastEditDate (prefer arcgislayers; fallback jsonlite) ----
  get_arcgis_last_edit <- function(url) {
    is_arcgis <- is.character(url) &&
      grepl("FeatureServer|MapServer", url, ignore.case = TRUE)
    if (!is_arcgis) {
      return(NULL)
    }

    # Preferred: arcgislayers if available
    if (requireNamespace("arcgislayers", quietly = TRUE)) {
      info <- tryCatch(
        arcgislayers::arc_rest_query(
          sub("[?].*$", "", url),
          params = list(f = "json")
        ),
        error = function(e) NULL
      )
      if (!is.null(info)) {
        le <- info$editingInfo$lastEditDate %||%
          (info$timeInfo$timeExtent %||% list(NULL, NULL))[[2]]
        return(if (is.null(le)) NULL else as.numeric(le))
      }
    }

    # Fallback: jsonlite
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      return(NULL)
    }
    u <- paste0(
      sub("[?].*$", "", url),
      if (grepl("[?]", url)) "&" else "?",
      "f=json"
    )
    out <- tryCatch(
      jsonlite::fromJSON(u, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(out)) {
      return(NULL)
    }

    le <- out$editingInfo$lastEditDate %||% out$timeInfo$timeExtent[2]
    if (is.null(le)) NULL else as.numeric(le)
  }

  # ---- Canonical signature: attributes + geometry (WKT), sorted deterministically ----
  canonical_signature <- function(s, digits = 8, num_round = 6) {
    s <- sf::st_zm(s, drop = TRUE, what = "ZM")
    wkt <- sf::st_as_text(sf::st_geometry(s), digits = digits)
    x <- sf::st_set_geometry(s, NULL)
    x[[".__WKT__"]] <- wkt

    is_factor <- vapply(x, is.factor, logical(1))
    if (any(is_factor)) {
      x[is_factor] <- lapply(x[is_factor], as.character)
    }

    is_num <- vapply(
      x,
      function(col) is.numeric(col) || inherits(col, "integer64"),
      logical(1)
    )
    if (any(is_num)) {
      x[is_num] <- lapply(x[is_num], function(col) {
        if (inherits(col, "integer64")) {
          col <- as.numeric(col)
        }
        round(col, num_round)
      })
    }

    x <- x[, order(names(x)), drop = FALSE]
    for (nm in names(x)) {
      if (!is.atomic(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
    }
    ord <- do.call(order, c(x, list(na.last = TRUE)))
    x[ord, , drop = FALSE]
  }

  # ---- Normalize epoch-ms -> Date by auto-detection ----
  is_epoch_ms <- function(x) {
    if (inherits(x, "integer64")) {
      x <- as.numeric(x)
    }
    is.numeric(x) &&
      any(!is.na(x)) &&
      suppressWarnings({
        rng <- range(x, na.rm = TRUE)
        is.finite(rng[1]) && is.finite(rng[2]) && rng[1] > 1e11 && rng[2] < 1e14
      })
  }
  to_date_from_ms <- function(x) {
    if (inherits(x, "integer64")) {
      x <- as.numeric(x)
    }
    as.Date(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC"))
  }
  fix_date_cols <- function(s) {
    nm <- names(s)
    numeric_cols <- nm[vapply(
      s,
      function(col) is.numeric(col) || inherits(col, "integer64"),
      logical(1)
    )]
    to_fix <- Filter(function(nm) is_epoch_ms(s[[nm]]), numeric_cols)
    if (length(to_fix)) {
      for (n in to_fix) {
        s[[n]] <- to_date_from_ms(s[[n]])
      }
    }
    s
  }

  # ---- Reader: prefer GDAL/arcgislayers; fallback GeoJSON; last-resort temp shapefile ----
  read_layer_as_sf <- function(url) {
    # First attempt: let GDAL handle the service directly
    s <- tryCatch(sf::read_sf(url, quiet = TRUE), error = function(e) NULL)
    if (!is.null(s)) {
      return(s)
    }

    is_arcgis <- is.character(url) &&
      grepl("FeatureServer|MapServer", url, ignore.case = TRUE)

    # Preferred: arcgislayers (handles paging, out_sr)
    if (is_arcgis && requireNamespace("arcgislayers", quietly = TRUE)) {
      s <- tryCatch(
        {
          lyr <- arcgislayers::arcgislayer(sub("[?].*$", "", url))
          q <- arcgislayers::arc_select(
            lyr,
            where = "1=1",
            out_fields = "*",
            out_sr = 4326
          )
          arcgislayers::arc_collect(q, as_sf = TRUE)
        },
        error = function(e) NULL
      )
      if (!is.null(s)) {
        return(s)
      }
    }

    # Fallback: ArcGIS GeoJSON endpoint with simple pagination
    if (is_arcgis) {
      base <- sub("[?].*$", "", url)
      chunk <- 2000L
      offset <- 0L
      parts <- list()
      repeat {
        gj <- paste0(
          base,
          "/query?where=1%3D1&outFields=*&outSR=4326&f=geojson",
          "&resultOffset=",
          offset,
          "&resultRecordCount=",
          chunk
        )
        p <- tryCatch(sf::read_sf(gj, quiet = TRUE), error = function(e) NULL)
        if (is.null(p)) {
          break
        }
        n <- nrow(p)
        if (n == 0L) {
          break
        }
        parts[[length(parts) + 1L]] <- p
        offset <- offset + n
        if (n < chunk) {
          break
        }
        if (length(parts) > 10000L) break
      }
      if (length(parts)) {
        s2 <- tryCatch(
          suppressWarnings(do.call(rbind, parts)),
          error = function(e) NULL
        )
        if (!is.null(s2)) {
          return(s2)
        }
      }
    }

    # Last resort: use TADA_WriteLayer -> read from temp shapefile; suppress spurious GDAL warnings
    tmp_dir <- tempfile("layer_tmp_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    tmp_shp <- file.path(tmp_dir, "layer.shp")

    suppressWarnings(ns_get("TADA_WriteLayer")(url, tmp_shp))
    tryCatch(sf::st_read(tmp_shp, quiet = TRUE), error = function(e) {
      stop("Failed to read temp shapefile: ", e$message)
    })
  }

  # ---- Core update logic for a single layer -> one .gpkg file ----
  has_sf <- requireNamespace("sf", quietly = TRUE)

  update_one <- function(url, dest_gpkg) {
    if (!has_sf) {
      message(
        "sf not available; cannot write GeoPackage: ",
        basename(dest_gpkg)
      )
      return(invisible(FALSE))
    }

    # Preflight (ArcGIS): skip fast if lastEditDate unchanged and file exists
    last_edit_remote <- get_arcgis_last_edit(url)
    meta <- read_meta(dest_gpkg)
    if (
      !is.null(last_edit_remote) &&
        !is.null(meta) &&
        !is.null(meta$last_edit) &&
        isTRUE(file.exists(dest_gpkg)) &&
        identical(meta$last_edit, last_edit_remote)
    ) {
      message(
        basename(dest_gpkg),
        " unchanged (preflight) - skipping download."
      )
      return(invisible(FALSE))
    }

    # Read as sf and normalize epoch-ms date fields
    s_new <- read_layer_as_sf(url)
    s_new <- fix_date_cols(s_new)

    # Drop Z/M; GeoPackage supports broad geometry types (no shapefile coercion needed)
    s_out <- sf::st_zm(s_new, drop = TRUE, what = "ZM")

    # Build canonical signature from the object we will actually write
    sig_new <- canonical_signature(s_out)

    # Compare with cached signature
    if (
      !is.null(meta) &&
        !is.null(meta$sig) &&
        isTRUE(file.exists(dest_gpkg)) &&
        identical(meta$sig, sig_new)
    ) {
      write_meta(dest_gpkg, list(sig = sig_new, last_edit = last_edit_remote))
      message(basename(dest_gpkg), " unchanged - skipping write.")
      return(invisible(FALSE))
    }

    # Write: ensure path, delete existing gpkg, write clean dataset
    dir.create(dirname(dest_gpkg), recursive = TRUE, showWarnings = FALSE)
    sf::st_write(
      s_out,
      dsn = dest_gpkg,
      delete_dsn = TRUE, # remove existing file so we have exactly one layer per gpkg
      quiet = TRUE
    )

    # Update sidecar metadata (signature + optional lastEditDate)
    write_meta(dest_gpkg, list(sig = sig_new, last_edit = last_edit_remote))

    message(basename(dest_gpkg), " updated (GeoPackage).")
    invisible(TRUE)
  }

  # Helper to run each update and continue on error
  run_update <- function(url, dest) {
    tryCatch(update_one(url, dest), error = function(e) {
      message(
        "Creating or updating layer ",
        basename(dest),
        " failed.\nError: ",
        conditionMessage(e)
      )
      invisible(FALSE)
    })
  }

  # ---- Run updates: one .gpkg per layer in inst/extdata ----
  run_update(ns_get("AKAllotmentsUrl"), "inst/extdata/AKAllotments.gpkg")
  run_update(ns_get("AKVillagesUrl"), "inst/extdata/AKVillages.gpkg")
  run_update(ns_get("AmericanIndianUrl"), "inst/extdata/AmericanIndian.gpkg")
  run_update(ns_get("OffReservationUrl"), "inst/extdata/OffReservation.gpkg")
  run_update(ns_get("OKTribeUrl"), "inst/extdata/OKTribe.gpkg")
  run_update(ns_get("VATribeUrl"), "inst/extdata/VATribe.gpkg")

  invisible(TRUE)
}
