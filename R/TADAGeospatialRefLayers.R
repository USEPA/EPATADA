#' Update cached tribal shapefiles (internal)
#'
#' Downloads and refreshes cached tribal feature layers in `inst/extdata`,
#' replacing a shapefile set only when the layer content has actually changed.
#'
#' Epoch-millisecond numeric columns are auto-detected by magnitude and converted
#' to DBF-compatible `Date` to avoid GDAL warnings about values too large for field width.
#'
#' @section Dependencies:
#' - Imports: `sf`
#' - Suggests (optional preflight): `jsonlite`
#'
#' @return Invisibly returns `TRUE`. Messages indicate whether each layer was
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
  meta_path <- function(dest_shp) {
    meta_dir <- file.path(dirname(dest_shp), ".meta")
    dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(
      meta_dir,
      paste0(basename(tools::file_path_sans_ext(dest_shp)), ".rds")
    )
  }
  read_meta <- function(dest_shp) {
    p <- meta_path(dest_shp)
    if (file.exists(p)) {
      tryCatch(readRDS(p), error = function(e) NULL)
    } else {
      NULL
    }
  }
  write_meta <- function(dest_shp, meta) {
    saveRDS(meta, meta_path(dest_shp))
  }

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
        le <- NULL
        if (!is.null(info$editingInfo$lastEditDate)) {
          le <- info$editingInfo$lastEditDate
        }
        if (is.null(le) && !is.null(info$timeInfo$timeExtent)) {
          # timeExtent can be a vector/list; take the end time
          le <- info$timeInfo$timeExtent[[2]]
        }
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

    le <- NULL
    if (!is.null(out$editingInfo$lastEditDate)) {
      le <- out$editingInfo$lastEditDate
    }
    if (is.null(le) && !is.null(out$timeInfo$timeExtent)) {
      le <- out$timeInfo$timeExtent[2]
    }
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

  # ---- Shapefile helpers ----
  remove_shapefile_set <- function(dest_shp) {
    base <- tools::file_path_sans_ext(dest_shp)
    exts <- c("shp", "shx", "dbf", "prj", "cpg", "qix", "sbn", "sbx", "shp.xml")
    files <- file.path(dirname(base), paste0(basename(base), ".", exts))
    files <- files[file.exists(files)]
    if (length(files)) unlink(files, force = TRUE)
  }

  # ---- Coerce geometries to a shapefile-safe single type ----
  coerce_for_shapefile <- function(s) {
    # drop Z/M
    s <- sf::st_zm(s, drop = TRUE, what = "ZM")

    # If already polygonal, cast to MULTIPOLYGON
    if (all(sf::st_is(s, c("POLYGON", "MULTIPOLYGON")) | sf::st_is_empty(s))) {
      return(suppressWarnings(sf::st_cast(s, "MULTIPOLYGON")))
    }

    # Prefer polygonal content if present
    s_poly <- suppressWarnings(sf::st_collection_extract(s, "POLYGON"))
    s_poly <- s_poly[!sf::st_is_empty(s_poly), , drop = FALSE]
    if (nrow(s_poly)) {
      return(suppressWarnings(sf::st_cast(s_poly, "MULTIPOLYGON")))
    }

    # Next try linear
    s_line <- suppressWarnings(sf::st_collection_extract(s, "LINESTRING"))
    s_line <- s_line[!sf::st_is_empty(s_line), , drop = FALSE]
    if (nrow(s_line)) {
      return(suppressWarnings(sf::st_cast(s_line, "MULTILINESTRING")))
    }

    # Finally try point
    s_pt <- suppressWarnings(sf::st_collection_extract(s, "POINT"))
    s_pt <- s_pt[!sf::st_is_empty(s_pt), , drop = FALSE]
    if (nrow(s_pt)) {
      return(suppressWarnings(sf::st_cast(s_pt, "MULTIPOINT")))
    }

    stop(
      "Unable to coerce GeometryCollection/mixed geometries to a shapefile-supported type."
    )
  }

  # ---- Prefer arcgislayers; fallback to GeoJSON; last-resort temp shapefile ----
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

    # Last resort: use writeLayer -> read from temp shapefile; suppress spurious GDAL warnings
    tmp_dir <- tempfile("layer_tmp_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    tmp_shp <- file.path(tmp_dir, "layer.shp")

    suppressWarnings(ns_get("writeLayer")(url, tmp_shp))
    tryCatch(sf::st_read(tmp_shp, quiet = TRUE), error = function(e) {
      stop("Failed to read temp shapefile: ", e$message)
    })
  }

  # ---- Core update logic for a single layer ----
  has_sf <- requireNamespace("sf", quietly = TRUE)

  update_one <- function(url, dest_shp) {
    if (!has_sf) {
      message(
        "sf not available; writing ",
        basename(dest_shp),
        " unconditionally."
      )
      ns_get("writeLayer")(url, dest_shp)
      return(invisible(TRUE))
    }

    # Preflight (ArcGIS): skip fast if lastEditDate unchanged and files exist
    last_edit_remote <- get_arcgis_last_edit(url)
    meta <- read_meta(dest_shp)
    if (
      !is.null(last_edit_remote) &&
        !is.null(meta) &&
        !is.null(meta$last_edit) &&
        isTRUE(file.exists(dest_shp)) &&
        identical(meta$last_edit, last_edit_remote)
    ) {
      message(basename(dest_shp), " unchanged (preflight) - skipping download.")
      return(invisible(FALSE))
    }

    # Read as sf and normalize epoch-ms date fields
    s_new <- read_layer_as_sf(url)
    s_new <- fix_date_cols(s_new)

    # Coerce geometry to shapefile-supported type before signature and write
    s_out <- coerce_for_shapefile(s_new)

    # Build canonical signature from the object we will actually write
    sig_new <- canonical_signature(s_out)

    # Compare with cached signature
    if (
      !is.null(meta) &&
        !is.null(meta$sig) &&
        isTRUE(file.exists(dest_shp)) &&
        identical(meta$sig, sig_new)
    ) {
      write_meta(dest_shp, list(sig = sig_new, last_edit = last_edit_remote))
      message(basename(dest_shp), " unchanged - skipping write.")
      return(invisible(FALSE))
    }

    # Write: ensure path, remove existing set, write once with normalized schema
    dir.create(dirname(dest_shp), recursive = TRUE, showWarnings = FALSE)
    remove_shapefile_set(dest_shp)
    # After removing the set, do not request dataset deletion again
    sf::st_write(
      s_out,
      dest_shp,
      delete_dsn = FALSE,
      quiet = TRUE,
      layer_options = c("ENCODING=UTF-8")
    )

    # Update sidecar metadata (signature + optional lastEditDate)
    write_meta(dest_shp, list(sig = sig_new, last_edit = last_edit_remote))

    message(basename(dest_shp), " updated.")
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

  # ---- Run updates (sequential; parallelize outside if desired) ----
  run_update(ns_get("AKAllotmentsUrl"), "inst/extdata/AKAllotments.shp")
  run_update(ns_get("AKVillagesUrl"), "inst/extdata/AKVillages.shp")
  run_update(ns_get("AmericanIndianUrl"), "inst/extdata/AmericanIndian.shp")
  run_update(ns_get("OffReservationUrl"), "inst/extdata/OffReservation.shp")
  run_update(ns_get("OKTribeUrl"), "inst/extdata/OKTribe.shp")
  run_update(ns_get("VATribeUrl"), "inst/extdata/VATribe.shp")

  invisible(TRUE)
}
