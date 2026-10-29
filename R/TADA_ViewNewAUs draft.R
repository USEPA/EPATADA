
testdat <- Data_TribalNations_Harmonized |>
  dplyr::filter(OrganizationIdentifier == "BLCKFEET") |>
  dplyr::slice_sample(n = 25)

testdat_newAUs <- TADA_CreatePointAUs(testdat,
                                      create_geo = TRUE,
                                      tada_df = TRUE,
                                      auid_prefix = "TEST-")


# requires the output of TADA_CreatePointAUs where create_geo = TRUE and tada_df = TRUE
TADA_ViewNewAUs <- function(.data, ref_icons = TRUE) {

  # Expecting a list with two data frames:
  # 1) a simple mapping df with monitoring location identifier, AU id, and geometry
  # 2) a lookup df with monitoring location info for popups
  if (!all(c("geometry", "tada_df", "crosswalk") %in% names(.data))) {
    stop(
      "Your input must be a list with elements 'crosswalk', 'geometry', and 'tada_df'."
    )
  }

  # Basic geometry / column checks
  if (!inherits(.data[["geometry"]], "sf")) {
    stop(".data must be a list containing `geometry` which is an sf object with point geometry.")
  }

  if (!inherits(tada_df, "data.frame")) {
    stop(".data must be a list containing `tada_df` which is df with expected TADA prefixed columns.")
  }


  AU_map_table <- .data[["geometry"]] |>
    dplyr::left_join(.data[["crosswalk"]],
                     dplyr::join_by(ATTAINS.AssessmentUnitIdentifier)) |>
    dplyr::select(ATTAINS.AssessmentUnitIdentifier,
                  ATTAINS.MonitoringLocationIdentifier,
                  geometry) |>
    dplyr::distinct()

  ML_popup_table <- .data[["tada_df"]]


  # Check required columns in AU_map_table
  required_map_cols <- c(
    "ATTAINS.AssessmentUnitIdentifier",
    "ATTAINS.MonitoringLocationIdentifier",
    "geometry"
  )

  if (!all(required_map_cols %in% names(AU_map_table))) {
    stop(
      paste0(
        "`AU_map_table` must contain: ",
        paste(required_map_cols, collapse = ", ")
      )
    )
  }

  # Check for required cols in popup table
  # Update these names to match your actual lookup table fields
  required_popup_cols <- c("monitoring_location_identifier")
  if (!all(required_popup_cols %in% names(ML_popup_table))) {
    stop(
      paste0(
        "`ML_popup_table` must contain: ",
        paste(required_popup_cols, collapse = ", ")
      )
    )
  }

  # Use internal function to get paths to images and labels
  list.images <- getMapIconLabels()
  images <- unlist(list.images[1])
  img.labels <- unlist(list.images[2])
  rm(list.images)

  # Check all image paths exist
  for (path in images) {
    if (!file.exists(path)) {
      stop(sprintf("Image file not found: %s", path))
    }
  }

  suppressMessages(suppressWarnings({

    # Drop geometry for popup/join operations if needed
    AU_map_table_no_geo <- try(
      sf::st_drop_geometry(AU_map_table),
      silent = TRUE
    )
    if (inherits(AU_map_table_no_geo, "try-error")) {
      AU_map_table_no_geo <- AU_map_table
    }

    # Join popup information onto the map table
    map_data <- dplyr::left_join(
      AU_map_table_no_geo,
      ML_popup_table,
      by = "monitoring_location_identifier"
    )

    # Create summary data for basemap / map extent
    # If you have a helper for this, use it; otherwise use the map table
    sumdat <- map_data

    # Basemap
    map <- createTADABasemap(sumdat)

    overlay_groups <- character(0)

    # ----------------------------------------------------------------------------
    # 1) Add assessment unit markers
    # ----------------------------------------------------------------------------
    # Here we assume icon label #14 is the larger AU circle marker
    # If addWQPSites() cannot be reused for AUs, add them directly with leaflet
    au_icon <- NULL
    if (length(images) >= 14) {
      au_icon <- leaflet::makeIcon(
        iconUrl = images[[14]],
        iconWidth = 24,
        iconHeight = 24
      )
    } else {
      stop("Icon #14 is not available in `getMapIconLabels()` output.")
    }

    map <- leaflet::addCircleMarkers(
      map = map,
      data = AU_map_table,
      lng = ~sf::st_coordinates(geometry)[, 1],
      lat = ~sf::st_coordinates(geometry)[, 2],
      radius = 8,
      color = "#14",   # if you literally mean the label; otherwise replace with hex color
      fillColor = "#14",
      fillOpacity = 1,
      stroke = TRUE,
      weight = 1,
      popup = ~paste0(
        "<strong>Assessment Unit:</strong> ", assessment_unit_id
      ),
      group = "Assessment Units"
    )

    overlay_groups <- c(overlay_groups, "Assessment Units")

    # ----------------------------------------------------------------------------
    # 2) Add monitoring locations as solid WQP site circles
    # ----------------------------------------------------------------------------
    # If you already have a helper like addWQPSites(), use that.
    # But since your monitoring locations are simple point features, we can map directly.

    map <- leaflet::addCircleMarkers(
      map = map,
      data = AU_map_table,
      lng = ~sf::st_coordinates(geometry)[, 1],
      lat = ~sf::st_coordinates(geometry)[, 2],
      radius = 5,
      color = "blue",
      fillColor = "blue",
      fillOpacity = 1,
      stroke = TRUE,
      weight = 1,
      popup = ~{
        ml_id <- monitoring_location_identifier
        row <- ML_popup_table[ML_popup_table$monitoring_location_identifier == ml_id, , drop = FALSE]
        if (nrow(row) == 0) {
          paste0("<strong>Monitoring Location:</strong> ", ml_id)
        } else {
          # Customize this to match your popup fields
          paste0(
            "<strong>Monitoring Location:</strong> ", ml_id,
            "<br><strong>Name:</strong> ", ifelse(!is.na(row$name[1]), row$name[1], ""),
            "<br><strong>Agency:</strong> ", ifelse(!is.na(row$agency[1]), row$agency[1], "")
          )
        }
      },
      group = "Monitoring Locations"
    )

    overlay_groups <- c(overlay_groups, "Monitoring Locations")

    # ----------------------------------------------------------------------------
    # 3) Add custom legend
    # ----------------------------------------------------------------------------
    map <- addTADAMapLegend(
      sumdat,
      map = map,
      icons = images,
      icon_labels = img.labels,
      wqp = TRUE,
      ref_icons = ref_icons,
      attains_au = TRUE,
      attains_missing = FALSE,
      nhd_attains = FALSE
    )

    map <- addLegendToggle(map = map)
    map <- addLayerControl(map = map, overlay_groups = overlay_groups)

    rm(sumdat, overlay_groups)

    return(map)
  }))
}
