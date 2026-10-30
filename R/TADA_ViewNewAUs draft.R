
testdat <- Data_TribalNations_Harmonized |>
  dplyr::filter(OrganizationIdentifier == "BLCKFEET") |>
  dplyr::slice_sample(n = 25)

testdat_newAUs <- TADA_CreatePointAUs(testdat,
                                      create_geo = TRUE,
                                      tada_df = TRUE,
                                      auid_prefix = "TEST-")


# requires the tada_df output of TADA_CreatePointAUs where tada_df = TRUE
TADA_ViewNewAUs <- function(.data, ref_icons = TRUE) {

  # Check required columns in AU_map_table
  required_map_cols <- c(
    "ATTAINS.AssessmentUnitIdentifier",
    "TADA.MonitoringLocationIdentifier",
    "TADA.LatitudeMeasure",
    "TADA.LongitudeMeasure"
  )
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

    if (!"TADA.AURefSource" %in% names(.data)) {
      .data$TADA.AURefSource <- "TADA_CreatePointAUs"
    }

    # Create basemap
    map <- createTADABasemap(.data)

    overlay_groups <- character(0)

    # popup for AU markers
    au_popup <- paste0(
      "Assessment Unit ID: ",
      .data$ATTAINS.AssessmentUnitIdentifier,
      "<br> Monitoring Location ID: ",
     .data$TADA.MonitoringLocationIdentifier
    )

    # add AU markers using icon #14
    au_icon <- leaflet::icons(
      iconUrl = dplyr::case_when(
        is.na(.data$TADA.AURefSource) |
          .data$TADA.AURefSource == "Not assigned to AU" | is.na(.data$TADA.AURefSource) ~
          system.file(
            "extdata/icons",
            "circle-dashed.png",
            package = "EPATADA"
          ),
        .data$TADA.AURefSource == "TADA_CreatePointAUs" ~ images[14],
        TRUE ~ images[14]
      ),
      iconWidth = 48,
      iconHeight = 48
    )

    map <- map |>
      leaflet::addMarkers(
        data = .data,
        group = "Assessment Units",
        lng = ~TADA.LongitudeMeasure,
        lat = ~TADA.LatitudeMeasure,
        icon = au_icon,
        popup = au_popup
      )

    overlay_groups <- c(overlay_groups, "Assessment Units")

    # Add monitoring locations using addWQPSites()
    wqp_sites <- addWQPSites(
      .data = .data,
      map = map,
      icons = images,
      icon_labels = img.labels,
      ref_icons = ref_icons,
      overlay_groups = overlay_groups
    )

    map <- wqp_sites$map
    overlay_groups <- wqp_sites$overlay_groups

    # Add legend and controls
    map <- addTADAMapLegend(
     .data,
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

    return(map)
  }))
}
