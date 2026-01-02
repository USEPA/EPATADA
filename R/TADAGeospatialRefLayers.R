#' Update cached tribal shapefiles (internal)
#'
#' Downloads and refreshes cached tribal feature layers in `inst/extdata`,
#' replacing a shapefile set only when the layer content has actually changed.
#' Messages indicate whether each layer was skipped (preflight or unchanged), 
#' created, or updated.
#'
#' @details
#' For each configured tribal layer URL, the function:
#' - Optionally preflights ArcGIS layers via `?f=json` to read `lastEditDate`; if
#'   unchanged and the destination exists, it skips downloading and canonicalization.
#' - Loads the layer as `sf`, converts configured epoch-millisecond columns to `Date`,
#'   computes a canonical signature (stable ordering and geometry as WKT), and compares
#'   it to a cached signature in a sidecar RDS file. If identical, it skips writing.
#' - Otherwise it removes the existing shapefile set and writes the new one via
#'   `sf::st_write()`, then updates the sidecar cache (signature and lastEditDate).
#'
#' @section Dependencies:
#' - Imports/Suggests: `sf`, `jsonlite` (optional, for ArcGIS preflight).
#' - Requires a writer function: `EPATADA::writeLayer(url, out_path)` that writes
#'   an ESRI shapefile set using `out_path` as the base name (e.g., `"x.shp"`).
#'
#' @seealso
#' sf::st_read(), sf::st_write(), jsonlite::fromJSON()
#'
#' @examples
#' \dontrun{
#' # Update cached tribal shapefiles (writes only when content changed)
#' TADA_UpdateTribalLayers()
#' }
#'
#' @keywords internal
#' @noRd
TADA_UpdateTribalLayers <- function() {
  
  # ---- Sidecar metadata (canonical signature + lastEditDate) ----
  meta_path <- function(dest_shp) {
    meta_dir <- getOption("TADA.tribal.meta_dir",
                          file.path(dirname(dest_shp), ".meta"))
    dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(meta_dir, paste0(basename(tools::file_path_sans_ext(dest_shp)), ".rds"))
  }
  read_meta <- function(dest_shp) {
    p <- meta_path(dest_shp)
    if (file.exists(p)) {
      tryCatch(readRDS(p), error = function(e) NULL)
    } else NULL
  }
  write_meta <- function(dest_shp, meta) {
    saveRDS(meta, meta_path(dest_shp))
  }
  
  # ---- Preflight: ArcGIS lastEditDate (optional) ----
  get_arcgis_last_edit <- function(url) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
    is_arcgis <- grepl("FeatureServer|MapServer", url, ignore.case = TRUE)
    if (!is_arcgis) return(NULL)
    u <- paste0(sub("[?].*$", "", url), if (grepl("[?]", url)) "&" else "?", "f=json")
    out <- tryCatch(jsonlite::fromJSON(u, simplifyVector = TRUE),
                    error = function(e) NULL)
    if (is.null(out)) return(NULL)
    le <- NULL
    if (!is.null(out$editingInfo$lastEditDate)) le <- out$editingInfo$lastEditDate
    if (is.null(le) && !is.null(out$timeInfo$timeExtent)) {
      le <- out$timeInfo$timeExtent[2]
    }
    if (is.null(le)) NULL else as.numeric(le)
  }
  
  # ---- Canonical signature: attributes + geometry (WKT), sorted deterministically ----
  canonical_signature <- function(s, digits = 8, num_round = 6) {
    # Drop Z/M to avoid noise
    s <- sf::st_zm(s, drop = TRUE, what = "ZM")
    # Geometry -> text with fixed precision
    wkt <- sf::st_as_text(sf::st_geometry(s), digits = digits)
    x   <- sf::st_set_geometry(s, NULL)
    x[[".__WKT__"]] <- wkt
    
    # Stabilize types/values
    is_factor <- vapply(x, is.factor, logical(1))
    if (any(is_factor)) x[is_factor] <- lapply(x[is_factor], as.character)
    
    is_num <- vapply(x, is.numeric, logical(1))
    if (any(is_num)) x[is_num] <- lapply(x[is_num], function(col) round(col, num_round))
    
    # Order columns for stability
    x <- x[, order(names(x)), drop = FALSE]
    
    # Ensure atomic for ordering
    for (nm in names(x)) if (!is.atomic(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
    ord <- do.call(order, c(x, list(na.last = TRUE)))
    x[ord, , drop = FALSE]
  }
  
  # ---- Normalize epoch-ms -> Date for selected columns ----
  is_epoch_ms <- function(x) {
    is.numeric(x) &&
      any(!is.na(x)) &&
      suppressWarnings({
        rng <- range(x, na.rm = TRUE)
        is.finite(rng[1]) && is.finite(rng[2]) && rng[1] > 1e11 && rng[2] < 1e14
      })
  }
  to_date_from_ms <- function(x) as.Date(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC"))
  fix_date_cols <- function(s) {
    date_candidates <- getOption("TADA.tribal.date_fields", c("DATE_MO", "CURRENT"))
    to_fix <- intersect(date_candidates, names(s))
    for (nm in to_fix) if (is_epoch_ms(s[[nm]])) s[[nm]] <- to_date_from_ms(s[[nm]])
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
  
  # Try to read the layer as sf in one step; fallback to EPATADA::writeLayer -> read
  read_layer_as_sf <- function(url) {
    s <- tryCatch(sf::read_sf(url, quiet = TRUE), error = function(e) NULL)
    if (!is.null(s)) return(s)
    tmp_dir <- tempfile("layer_tmp_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    tmp_shp <- file.path(tmp_dir, "layer.shp")
    EPATADA::writeLayer(url, tmp_shp)
    tryCatch(sf::st_read(tmp_shp, quiet = TRUE),
             error = function(e) stop("Failed to read temp shapefile: ", e$message))
  }
  
  # ---- Core update logic for a single layer ----
  has_sf <- requireNamespace("sf", quietly = TRUE)
  
  update_one <- function(url, dest_shp) {
    if (!has_sf) {
      message("sf not available; writing ", basename(dest_shp), " unconditionally.")
      EPATADA::writeLayer(url, dest_shp)
      return(invisible(TRUE))
    }
    
    # Preflight (ArcGIS): skip fast if lastEditDate unchanged and files exist
    last_edit_remote <- get_arcgis_last_edit(url)
    meta <- read_meta(dest_shp)
    if (!is.null(last_edit_remote) && !is.null(meta) && !is.null(meta$last_edit) &&
        isTRUE(file.exists(dest_shp)) && identical(meta$last_edit, last_edit_remote)) {
      message(basename(dest_shp), " unchanged (preflight) — skipping download.")
      return(invisible(FALSE))
    }
    
    # Read as sf and normalize epoch-ms date fields
    s_new <- read_layer_as_sf(url)
    s_new <- fix_date_cols(s_new)
    
    # Build canonical signature and compare with cached signature
    sig_new <- canonical_signature(s_new)
    if (!is.null(meta) && !is.null(meta$sig) &&
        isTRUE(file.exists(dest_shp)) && identical(meta$sig, sig_new)) {
      # Update meta with new last_edit (if available) and return
      write_meta(dest_shp, list(sig = sig_new, last_edit = last_edit_remote))
      message(basename(dest_shp), " unchanged — skipping write.")
      return(invisible(FALSE))
    }
    
    # Write: ensure path, remove existing set, write once with normalized schema
    dir.create(dirname(dest_shp), recursive = TRUE, showWarnings = FALSE)
    remove_shapefile_set(dest_shp)
    sf::st_write(s_new, dest_shp, delete_dsn = TRUE, quiet = TRUE)
    
    # Update sidecar metadata (signature + optional lastEditDate)
    write_meta(dest_shp, list(sig = sig_new, last_edit = last_edit_remote))
    
    message(basename(dest_shp), " updated.")
    invisible(TRUE)
  }
  
  # ---- Run updates (sequential; parallelize outside if desired) ----
  update_one(EPATADA::AKAllotmentsUrl,   "inst/extdata/AKAllotments.shp")
  update_one(EPATADA::AKVillagesUrl,     "inst/extdata/AKVillages.shp")
  update_one(EPATADA::AmericanIndianUrl, "inst/extdata/AmericanIndian.shp")
  update_one(EPATADA::OffReservationUrl, "inst/extdata/OffReservation.shp")
  update_one(EPATADA::OKTribeUrl,        "inst/extdata/OKTribe.shp")
  update_one(EPATADA::VATribeUrl,        "inst/extdata/VATribe.shp")
  
  invisible(TRUE)
}
