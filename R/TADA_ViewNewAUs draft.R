testdat <- Data_MT_MissoulaCounty |>
  dplyr::slice_sample(n = 30)

testdat_newpoints <- TADA_CreatePointAUs(testdat, create_geo = TRUE)


TADA_ViewATTAINS(Data_MT_AUMLRef)



  # use internal function to get paths to images and labels
  list.images <- getMapIconLabels()

  # define the paths to the images
  images <- unlist(list.images[1])

  # define the labels
  img.labels <- unlist(list.images[2])

  # remove intermediate objects
  rm(list.images)

  # Check if all image paths exist
  for (path in images) {
    if (!file.exists(path)) {
      stop(sprintf("Image file not found: %s", path))
    }
  }

  # check to make sure WQP observations exist
  checkForWQPData(ATTAINS_table)

  # ATTAINS API seems to be missing some AU data that is still preserved in the catchment layer.
  # Use catchments for those instances for mapping purposes:
  try(
    missing_raw_features <- findATTAINSMissingRawFeatures(
      ATTAINS_catchments,
      points_layer = ATTAINS_points,
      polygons_layer = ATTAINS_polygons,
      lines_layer = ATTAINS_lines
    ),
    silent = TRUE
  )

  suppressMessages(suppressWarnings({
    # if data was spatial, remove for downstream leaflet dev:
    try(ATTAINS_table <- ATTAINS_table |> sf::st_drop_geometry(), silent = TRUE)

    # create df to assign color based on ATTAINS overall status
    colors <- getATTAINSColorsRef()

    # prep ATTAINS assessment unit features for mapping if they exist
    if (
      !is.null(ATTAINS_lines) ||
      !is.null(ATTAINS_points) ||
      !is.null(ATTAINS_polygons)
    ) {
      au_mapper <- prepAllATTAINSMapper(
        color_ref = colors,
        lines_layer = ATTAINS_lines,
        points_layer = ATTAINS_points,
        polygons_layer = ATTAINS_polygons
      )
    }

    # CATCHMENT FEATURES - try to pull missing feature AU data if it exists. Otherwise, move on...
    try(
      missing_raw_mapper <- missing_raw_features |>
        dplyr::left_join(colors, by = "overallstatus") |>
        dplyr::mutate(type = "Raw Feature Unavailable"),
      silent = TRUE
    )

    # Develop WQP site stats (e.g. count of observations, parameters, per site)
    sumdat <- getWQPSiteStats(ATTAINS_table, attains = TRUE)

    # Basemap for AOI:
    map <- createTADABasemap(sumdat)

    # Initialize vectors to hold the names of groups we actually add
    overlay_groups <- character(0)

    # add all ATTAINS geometry to map
    all_attains <- addAllATTAINS(
      map = map,
      points_layer = au_mapper$points_mapper,
      polygons_layer = au_mapper$polygons_mapper,
      lines_layer = au_mapper$lines_mapper,
      catchment_layer = ATTAINS_catchments,
      missing_raw_layer = missing_raw_mapper,
      overlay_groups = overlay_groups,
      icons = images
    )

    map <- all_attains$map

    overlay_groups <- all_attains$overlay_groups

    rm(all_attains)

    # add symbology for any assessment units missing geometry from ATTAINS
    try({
      missing_aus <- showMissingATTAINSAUs(
        ATTAINS_table = ATTAINS_table,
        ATTAINS_polygons = ATTAINS_polygons,
        ATTAINS_points = ATTAINS_points,
        ATTAINS_lines = ATTAINS_lines,
        map = map,
        overlay_groups = overlay_groups
      )

      if (!is.null(missing_aus)) {
        map <- missing_aus$map

        overlay_groups <- missing_aus$overlay_groups
      }

      # remove intermediate objects
      rm(missing_aus)
    })

    # Add WQP observation features (should always exist):
    try(
      {
        wqp_sites <- addWQPSites(
          sumdat,
          map = map,
          icons = images,
          icon_labels = img.labels,
          ref_icons = ref_icons,
          overlay_groups = overlay_groups
        )

        map <- wqp_sites$map

        overlay_groups <- wqp_sites$overlay_groups
      },
      silent = TRUE
    )

    # set up params for adding custom legend
    # ATTAINS assessment units
    attains_au <- ifelse(
      any(
        c(
          "ATTAINS line features",
          "ATTAINS point features",
          "ATTAINS polygon features"
        ) %in%
          overlay_groups
      ),
      TRUE,
      FALSE
    )

    # attains missing
    attains_missing <- ifelse("not in ATTAINS" %in% overlay_groups, TRUE, FALSE)

    # NHD catchments containing ATTAINS features

    nhd_attains <- ifelse("ATTAINS catchments" %in% overlay_groups, TRUE, FALSE)

    # add TADA custom legend to map
    map <- addTADAMapLegend(
      sumdat,
      map = map,
      icons = images,
      icon_labels = img.labels,
      wqp = TRUE,
      ref_icons = ref_icons,
      attains_au = attains_au,
      attains_missing = attains_missing,
      nhd_attains = nhd_attains
    )

    # add button to toggle map legend on/off
    map <- addLegendToggle(map = map)

    # add layer control to map
    map <- addLayerControl(map = map, overlay_groups = overlay_groups)

    # remove intermediate objects
    rm(sumdat, overlay_groups)
    # return leaflet map of TADA WQ and its associated ATTAINS data
    return(map)
  }))
}


