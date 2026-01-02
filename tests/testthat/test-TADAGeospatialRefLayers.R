test_that("TADA_UpdateTribalLayers writes shapefiles, caches signature, and skips when unchanged", {
  skip_if_not_installed("sf")
  ns <- try(asNamespace("EPATADA"), silent = TRUE)
  if (inherits(ns, "try-error")) {
    skip("EPATADA namespace not available.")
  }
  url_syms <- c(
    "AKAllotmentsUrl",
    "AKVillagesUrl",
    "AmericanIndianUrl",
    "OffReservationUrl",
    "OKTribeUrl",
    "VATribeUrl"
  )
  for (sym in url_syms) {
    if (!exists(sym, envir = ns, inherits = FALSE)) {
      skip(paste0("Internal symbol ", sym, " not found in EPATADA namespace."))
    }
  }

  # Create a temporary project root with inst/extdata
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

  # Use proper geometry: points and extra epoch-ms fields (<=10 chars to avoid shapefile name warning)
  make_points_sf <- function(n = 2, offset = 0L) {
    pts <- sf::st_sfc(
      lapply(seq_len(n), function(i) {
        sf::st_point(c(-120 + i * 0.01, 38 + i * 0.01))
      }),
      crs = 4326
    )
    base1 <- as.numeric(as.POSIXct("2020-01-01 00:00:00", tz = "UTC")) * 1000
    base2 <- as.numeric(as.POSIXct("2020-06-01 00:00:00", tz = "UTC")) * 1000
    df <- data.frame(
      id = seq_len(n),
      DATE_MO = base1 + offset + (seq_len(n) - 1) * 86400000,
      CURRENT = base2 + offset,
      cur_to = base2 + offset + (seq_len(n) - 1) * 86400000, # <=10 chars
      cur_from = base1 + offset + (seq_len(n) - 1) * 86400000, # <=10 chars
      name = sprintf("feat_%d", seq_len(n))
    )
    sf::st_sf(df, geom = pts)
  }

  # Prepare a local source dataset (GeoJSON) for all URLs
  s1 <- make_points_sf(n = 2, offset = 0L)
  src1 <- file.path(tmp, "src1.geojson")
  sf::write_sf(s1, src1, quiet = TRUE)

  # Assign all internal URL symbols to this local source
  for (sym in url_syms) {
    if (bindingIsLocked(sym, ns)) {
      unlockBinding(sym, ns)
    }
    assignInNamespace(sym, src1, ns = "EPATADA")
  }

  # 1) First run: write all shapefiles and create meta sidecars
  expect_message(
    suppressWarnings(TADA_UpdateTribalLayers()),
    "updated\\.",
    fixed = FALSE
  )

  dests <- file.path(
    "inst/extdata",
    c(
      "AKAllotments.shp",
      "AKVillages.shp",
      "AmericanIndian.shp",
      "OffReservation.shp",
      "OKTribe.shp",
      "VATribe.shp"
    )
  )
  for (d in dests) {
    expect_true(file.exists(d), info = paste("dest missing:", d))
    meta_file <- file.path(
      "inst/extdata/.meta",
      paste0(tools::file_path_sans_ext(basename(d)), ".rds")
    )
    expect_true(
      file.exists(meta_file),
      info = paste("meta sidecar missing:", meta_file)
    )

    # meta signature should exist and date-like epoch-ms fields be Date class after conversion
    meta <- readRDS(meta_file)
    expect_true(is.list(meta))
    expect_true(!is.null(meta$sig))
    sig <- meta$sig
    expect_s3_class(sig, "data.frame")
    for (col in c("DATE_MO", "CURRENT", "cur_to", "cur_from")) {
      if (col %in% names(sig)) expect_s3_class(sig[[col]], "Date")
    }

    # Shapefile read-back should also reflect Date columns
    layer_back <- suppressWarnings(sf::st_read(d, quiet = TRUE))
    x <- sf::st_set_geometry(layer_back, NULL)
    for (col in c("DATE_MO", "CURRENT", "cur_to", "cur_from")) {
      if (col %in% names(x)) expect_s3_class(x[[col]], "Date")
    }
  }

  # 2) Second run unchanged: should skip write based on canonical signature
  expect_message(
    suppressWarnings(TADA_UpdateTribalLayers()),
    "unchanged — skipping write\\.",
    fixed = FALSE
  )

  # 3) Change content for one URL and verify update occurs and replaces shapefile
  s2 <- make_points_sf(n = 3, offset = 0L) # add a row -> content change
  src2 <- file.path(tmp, "src2.geojson")
  sf::write_sf(s2, src2, quiet = TRUE)

  # Change only AKAllotmentsUrl to src2
  if (bindingIsLocked("AKAllotmentsUrl", ns)) {
    unlockBinding("AKAllotmentsUrl", ns)
  }
  assignInNamespace("AKAllotmentsUrl", src2, ns = "EPATADA")

  # Capture mtime before update to confirm change afterwards
  prev_mtime <- file.info("inst/extdata/AKAllotments.shp")$mtime
  expect_message(
    suppressWarnings(TADA_UpdateTribalLayers()),
    "AKAllotments.shp updated\\.",
    fixed = FALSE
  )
  new_mtime <- file.info("inst/extdata/AKAllotments.shp")$mtime
  expect_true(new_mtime > prev_mtime)

  # Confirm that the dest now has 3 rows
  ak <- suppressWarnings(sf::st_read(
    "inst/extdata/AKAllotments.shp",
    quiet = TRUE
  ))
  expect_equal(nrow(ak), 3)

  # Confirm auto-detected columns remain Date after the update
  x <- sf::st_set_geometry(ak, NULL)
  for (col in c("DATE_MO", "CURRENT", "cur_to", "cur_from")) {
    if (col %in% names(x)) expect_s3_class(x[[col]], "Date")
  }
})

test_that("TADA_UpdateTribalLayers preflight lastEditDate skips download when unchanged", {
  skip_if_not_installed("sf")
  skip_if_not_installed("jsonlite")

  ns <- try(asNamespace("EPATADA"), silent = TRUE)
  if (inherits(ns, "try-error")) {
    skip("EPATADA namespace not available.")
  }
  url_syms <- c(
    "AKAllotmentsUrl",
    "AKVillagesUrl",
    "AmericanIndianUrl",
    "OffReservationUrl",
    "OKTribeUrl",
    "VATribeUrl"
  )
  for (sym in url_syms) {
    if (!exists(sym, envir = ns, inherits = FALSE)) {
      skip(paste0("Internal symbol ", sym, " not found in EPATADA namespace."))
    }
  }

  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

  # Create trivial shapefile at each destination so file.exists(dest_shp) is TRUE
  trivial <- sf::st_sf(
    data.frame(x = 1L),
    geom = sf::st_sfc(sf::st_point(c(-120, 38)), crs = 4326)
  )
  dests <- file.path(
    "inst/extdata",
    c(
      "AKAllotments.shp",
      "AKVillages.shp",
      "AmericanIndian.shp",
      "OffReservation.shp",
      "OKTribe.shp",
      "VATribe.shp"
    )
  )
  for (d in dests) {
    # Avoid delete_dsn on initial creation to prevent GDAL warnings
    sf::st_write(trivial, d, quiet = TRUE)
  }

  # Provide sidecar meta with matching last_edit dates for all
  matching_last_edit <- 1234567890000 # epoch-ms
  dir.create(
    file.path("inst/extdata", ".meta"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  for (d in dests) {
    meta_file <- file.path(
      "inst/extdata/.meta",
      paste0(tools::file_path_sans_ext(basename(d)), ".rds")
    )
    saveRDS(
      list(sig = data.frame(dummy = 1), last_edit = matching_last_edit),
      meta_file
    )
  }

  # Set all URLs to look like ArcGIS FeatureServer (to trigger preflight)
  fake_arcgis_url <- "https://example.com/FeatureServer/0"
  for (sym in url_syms) {
    if (bindingIsLocked(sym, ns)) {
      unlockBinding(sym, ns)
    }
    assignInNamespace(sym, fake_arcgis_url, ns = "EPATADA")
  }

  # Mock jsonlite::fromJSON from within jsonlite's namespace
  testthat::with_mocked_bindings(
    fromJSON = function(...) {
      list(editingInfo = list(lastEditDate = matching_last_edit))
    },
    .package = "jsonlite",
    {
      # Capture mtimes to verify they don't change after preflight skip
      old_mtimes <- file.info(dests)$mtime
      expect_message(
        suppressWarnings(TADA_UpdateTribalLayers()),
        "unchanged \\(preflight\\) — skipping download\\.",
        fixed = FALSE
      )
      new_mtimes <- file.info(dests)$mtime
      expect_true(all(new_mtimes == old_mtimes))
    }
  )
})
