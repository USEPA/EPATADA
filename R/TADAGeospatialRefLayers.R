#' Update cached tribal shapefiles (internal)
#'
#' Downloads and refreshes cached tribal feature layers in `inst/extdata`,
#' replacing a shapefile set only when the layer content has changed.
#'
#' @details
#' For each configured tribal layer URL, the function:
#' - Writes a temporary shapefile set via `writeLayer(url, out_path)`.
#' - Reads both the temporary and existing shapefiles as `sf` objects.
#' - Canonicalizes the data (drops Z/M, converts geometry to WKT, rounds
#'   numerics, orders columns/rows) and compares digests via `digest`.
#' - If unchanged, it leaves the existing files intact; otherwise, it replaces
#'   the entire shapefile set (`.shp`, `.shx`, `.dbf`, `.prj`, `.cpg`, `.qix`,
#'   `.sbn`, `.sbx`, `.shp.xml` as present).
#'
#' If the `sf` or `digest` packages are not available, the function falls back
#' to the original behavior and writes the shapefile sets unconditionally.
#'
#' Shapefiles are stored under `inst/extdata/` and the directory is created as
#' needed. Existing sidecar files for a given layer are removed prior to
#' replacement to avoid stale artifacts. Informational messages indicate whether
#' a layer was created, updated, or left unchanged.
#'
#' This function relies on the presence of:
#' - A `writeLayer(url, out_path)` function capable of writing an ESRI shapefile
#'   set using `out_path` as the base name (e.g., `".../Layer.shp"`).
#' - Layer URL objects such as `AKAllotmentsUrl`, `AKVillagesUrl`,
#'   `AmericanIndianUrl`, `OffReservationUrl`, `OKTribeUrl`, and `VATribeUrl`.
#'
#' @section Side effects:
#' Writes, replaces, or removes files under `inst/extdata/`. Ensure you have
#' appropriate file permissions when running this function.
#'
#' @return
#' Invisibly returns `TRUE` after attempting all updates. Messages indicate
#' per-layer status (created/updated/unchanged).
#'
#' @seealso
#' [sf::st_read()], [digest::digest()]
#'
#' @keywords internal
#' @noRd
# Used to store cached tribal feature layers
# Function to update tribal layer shapefiles.
# Shapefiles are stored in inst/extdata.
# Existing shapefiles with the same name will be replaced only if content changed.

TADA_UpdateTribalLayers <- function() {
  
  # --- Local helpers ----------------------------------------------------------
  
  # Full set of files that make up a shapefile
  shp_sidecars <- function(base_no_ext) {
    exts <- c("shp", "shx", "dbf", "prj", "cpg", "qix", "sbn", "sbx", "shp.xml")
    file.path(dirname(base_no_ext), paste0(basename(base_no_ext), ".", exts))
  }
  
  # Remove an existing shapefile set
  remove_shapefile_set <- function(dest_shp) {
    base_no_ext <- tools::file_path_sans_ext(dest_shp)
    files <- shp_sidecars(base_no_ext)
    files <- files[file.exists(files)]
    if (length(files)) unlink(files, force = TRUE)
  }
  
  # Replace dest shapefile set with files from a temp set
  replace_shapefile_set <- function(tmp_shp, dest_shp) {
    base_tmp  <- tools::file_path_sans_ext(tmp_shp)
    base_dest <- tools::file_path_sans_ext(dest_shp)
    tmp_files <- shp_sidecars(base_tmp)
    tmp_files <- tmp_files[file.exists(tmp_files)]
    if (!length(tmp_files)) stop("Temporary shapefile set not found at: ", base_tmp)
    
    dir.create(dirname(dest_shp), recursive = TRUE, showWarnings = FALSE)
    remove_shapefile_set(dest_shp)
    
    for (src in tmp_files) {
      ext  <- tools::file_ext(src)
      dest <- paste0(base_dest, ".", ext)
      if (!file.copy(src, dest, overwrite = TRUE)) {
        stop("Failed to copy ", src, " -> ", dest)
      }
    }
  }
  
  # Canonicalize an sf object for stable comparison
  canonicalize_sf <- function(x, digits = 8, num_round = 6) {
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    # Geometry to WKT for robust comparison
    wkt <- sf::st_as_text(sf::st_geometry(x), digits = digits)
    x   <- sf::st_set_geometry(x, NULL)
    x[[".__WKT__"]] <- wkt
    
    # Stabilize types/values
    is_factor <- vapply(x, is.factor, logical(1))
    if (any(is_factor)) x[is_factor] <- lapply(x[is_factor], as.character)
    
    is_num <- vapply(x, is.numeric, logical(1))
    if (any(is_num)) x[is_num] <- lapply(x[is_num], function(col) round(col, num_round))
    
    # Order columns for stability
    x <- x[, order(names(x)), drop = FALSE]
    
    # Deterministic row order by all columns
    # Ensure columns are atomic vectors for order()
    for (nm in names(x)) if (!is.atomic(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
    ord <- do.call(order, c(x, list(na.last = TRUE)))
    x[ord, , drop = FALSE]
  }
  
  # Write a layer only if changed by content comparison; otherwise skip
  write_layer_if_changed <- function(url, dest_shp, writer_fun) {
    tmp_dir <- tempfile("layer_tmp_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    
    tmp_shp <- file.path(tmp_dir, basename(dest_shp))
    
    # Use provided writer (your existing writeLayer) to create a temp shapefile set
    writer_fun(url, tmp_shp)
    
    # If dest doesn't exist, just place new files
    if (!file.exists(dest_shp)) {
      replace_shapefile_set(tmp_shp, dest_shp)
      message(basename(dest_shp), " created.")
      return(invisible(TRUE))
    }
    
    # Read both and compare canonical digests
    new_sf <- tryCatch(sf::st_read(tmp_shp, quiet = TRUE),
                       error = function(e) stop("Failed to read newly written shapefile: ", e$message))
    old_sf <- tryCatch(sf::st_read(dest_shp, quiet = TRUE),
                       error = function(e) NULL)
    
    if (is.null(old_sf)) {
      replace_shapefile_set(tmp_shp, dest_shp)
      message(basename(dest_shp), " replaced (existing file unreadable).")
      return(invisible(TRUE))
    }
    
    canon_new <- canonicalize_sf(new_sf)
    canon_old <- canonicalize_sf(old_sf)
    
    new_hash <- digest::digest(canon_new, algo = "xxhash64", serialize = TRUE)
    old_hash <- digest::digest(canon_old, algo = "xxhash64", serialize = TRUE)
    
    if (identical(new_hash, old_hash)) {
      message(basename(dest_shp), " unchanged — skipping write.")
      return(invisible(FALSE))
    }
    
    replace_shapefile_set(tmp_shp, dest_shp)
    message(basename(dest_shp), " updated.")
    invisible(TRUE)
  }
  
  # --- Update workflow --------------------------------------------------------
  
  has_sf     <- requireNamespace("sf", quietly = TRUE)
  has_digest <- requireNamespace("digest", quietly = TRUE)
  
  # Fallback to unconditional write if dependencies missing
  writer_fun <- function(url, out_path) writeLayer(url, out_path)
  
  update_one <- function(url, path) {
    if (has_sf && has_digest) {
      write_layer_if_changed(url, path, writer_fun)
    } else {
      message("sf and/or digest not available; writing ", basename(path), " unconditionally.")
      writer_fun(url, path)
      invisible(TRUE)
    }
  }
  
  update_one(AKAllotmentsUrl,   "inst/extdata/AKAllotments.shp")
  update_one(AKVillagesUrl,     "inst/extdata/AKVillages.shp")
  update_one(AmericanIndianUrl, "inst/extdata/AmericanIndian.shp")
  update_one(OffReservationUrl, "inst/extdata/OffReservation.shp")
  update_one(OKTribeUrl,        "inst/extdata/OKTribe.shp")
  update_one(VATribeUrl,        "inst/extdata/VATribe.shp")
  
  invisible(TRUE)
}
