#' getMapIconLabels
#'
#' Internal function to get list of icons (images) and/or a list of their labels
#' for use in TADA mapping functions.
#'
#' @param icons Boolean argument. When icons = TRUE, the list of images used for
#' TADA maps is returned. When icons = FALSE, no list of images is returned. Default
#' is icons = TRUE.
#'
#' @param labels Boolean argument. When labels = TRUE, the list of labels used for
#' TADA maps is returned. When labels = FALSE, no list of labels is returned. Default
#' is labels = TRUE.
#'
#' @return Depending on user input either one list (either labels or icons) or a
#' list containing both the labels list and the icons list.
#'
# create icon and label lists
getMapIconLabels <- function(icons = TRUE, labels = TRUE) {
  # the commented out code creates the legend images using the TADA color palette
  # if the color palette is ever edited, this section needs to be uncommented and run again
  # set palette
  # tada.pal <- TADA_ColorPalette()

  # square <- magick::image_read("vignettes/images/icons/square-solid-full.png")

  # notsupport <- magick::image_fill(square, tada.pal[3], "+500+500")

  # magick::image_write(notsupport, path = "vignettes/images/icons/square-ns.png")

  # fullsupport <- magick::image_fill(square, tada.pal[4], "+500+500")

  # magick::image_write(fullsupport, path = "vignettes/images/icons/square-fs.png")

  # notassessed <- magick::image_fill(square, tada.pal[7], "+500+500")

  # magick::image_write(notassessed, path = "vignettes/images/icons/square-na.png")

  # outline.square <- magick::image_read("vignettes/images/icons/square-regular-full.png")

  # catchment <- magick::image_fill(outline.square, "black", "+500+500")

  # magick::image_write(catchment, path = "vignettes/images/icons/square-catchment.png")

  # # create images for mapping point assessment units
  # setupPointMarkers <- function(path, color, name) {
  #
  #     marker <- magick::image_fill(magick::image_read(path), color, "+500+500")
  #
  #     marker <- magick::image_background(marker, color = "none")
  #
  #     magick::image_write(marker, path = paste0(
  #       "inst/extdata/icons/", name, ".png"))
  #   }
  #
  #   ns.point <- setupPointMarkers(path = "inst/extdata/icons/circle-solid-full.png",
  #                                 color = tada.pal[3],
  #                                 name = "ns.point.circle")
  #
  #   s.point <- setupPointMarkers(path = "inst/extdata/icons/circle-solid-full.png",
  #                                color = tada.pal[4],
  #                                name = "s.point.circle")
  #
  #   na.point <- setupPointMarkers(path = "inst/extdata/icons/circle-solid-full.png",
  #                                 color = tada.pal[7],
  #                                 name = "na.point.circle")

  # if only icons = TRUE, return list of paths to icon images
  if (icons == TRUE) {
    # Define the paths to the images
    images <- c(
      system.file("extdata/icons", "square-ns.png", package = "EPATADA"), # 1
      system.file("extdata/icons", "square-fs.png", package = "EPATADA"), # 2
      system.file("extdata/icons", "square-na.png", package = "EPATADA"), # 3
      system.file("extdata/icons", "circle-dashed.png", package = "EPATADA"), # 4
      system.file(
        "extdata/icons",
        "circle-user-solid-full.png",
        package = "EPATADA"
      ), # 5
      system.file(
        "extdata/icons",
        "circle-check-solid-full.png",
        package = "EPATADA"
      ), # 6
      system.file(
        "extdata/icons",
        "circle-solid-full.png",
        package = "EPATADA"
      ), # 7
      system.file(
        "extdata/icons",
        "circle-solid-full.png",
        package = "EPATADA"
      ), # 8
      system.file(
        "extdata/icons",
        "square-catchment-gray.png",
        package = "EPATADA"
      ), # 9
      system.file("extdata/icons", "square-catchment.png", package = "EPATADA"), # 10
      system.file("extdata/icons", "ns.point.circle.png", package = "EPATADA"), # 11
      system.file("extdata/icons", "s.point.circle.png", package = "EPATADA"), # 12
      system.file("extdata/icons", "na.point.circle.png", package = "EPATADA") # 13
    )

    # return only images if labels are not required
    if (labels == FALSE) {
      return(images)
    }
  }

  # if only labels = TRUE, return list of icon labels
  if (labels == TRUE) {
    img.labels <- c(
      "ATTAINS: Not Supporting", # 1
      "ATTAINS: Supporting", # 2
      "ATTAINS: Not Assessed", # 3
      "ATTAINS: No Geometry Available", # 4
      "WQP: User-supplied Ref", # 5
      "WQP: ATTAINS Crosswalk", # 6
      "WQP: TADA_CreateATTAINSAUMLCrosswalk", # 7
      "WQP: Monitoring Location", # 8
      "NHDPlus HR catchments containing water quality observations + ATTAINS feature are represented as gray polygons with black outlines.", # 9
      "NHDPlus HR catchments containing water quality observations without ATTAINS features are represented as clear polygons with black outlines.", # 10
      "ATTAINS: Not Supporting Point", # 11
      "ATTAINS: Supporting Point", # 12
      "ATTAINS: Not Assessed Point" # 13
    )

    # return only labels if images are not required
    if (icons == FALSE) {
      return(img.labels)
    }
  }

  # return a list containing the image paths and labels if both are required (default setting)
  if (icons == TRUE & labels == TRUE) {
    map.list <- list(images, img.labels)

    return(map.list)
  }
}

#' addATTAINS
#' Internal function to add ATTAINS assessment units (lines, points, or polygons) or
#' ATTAINS catchments (polygons) to TADA maps.
#'
#' @param .data A data frame created using prepATTAINSMapper (must contain a geometry
#' column).
#'
#' @param overlay_groups Initialized vector to add names of groups to map. This is
#' to allow users to toggle specific layers on/off. If it is NULL, the function will
#' fail with an error message. Default is overlay_list = NULL.
#'
#' @param map The TADA leaflet map to add the ATTAINS geometry to. Required, function
#' will not run with map is missing or NULL. Default is map = NULL.
#'
#' @param icons Character argument. The list of icon paths generated by the internal
#' function getMapIconLabels. If already called in a larger mapping function, it can
#' be referenced here (for efficiency). If icons = NULL, getMapIconLabels will run
#' and fetch the list. Default is icons = NULL. This argument is only applied to
#' for point assessment units.
#'
#' @param catchment Boolean argument. Specifies whether the geometries being
#' mapped are catchments or assessment units to determine color and other attributes.
#' Default is catchment = FALSE, meaning that assessment units are being mapped.
#'
#' @param catchment_type Character argument. Evaluated when catchment = TRUE to
#' determine if the catchments being mapped are (1) those corresponding to
#' assessment units that are missing point/line/polygon data but still preserved
#' in the catchment layer (catchment_type = "missing_raw"), (2) catchments containing
#' assessment unit features (catchment_type = "attains_au") or (3) NHD catchments
#' missing from ATTAINS (catchment_type = "wo_attains). Default is catchment_type =
#' "attains_au".
#'
#' @return ATTAINS geometry correctly formatted for display in a TADA leaflet map.
#'
# add ATTAINS geometry to existing leaflet map
addATTAINS <- function(
  .data,
  map = NULL,
  overlay_groups = NULL,
  icons = NULL,
  catchment = FALSE,
  catchment_type = "attains_au"
) {
  # stop function if map is not provided
  if (is.null(map)) {
    stop("addATTAINS: a leaflet map must be supplied to run this function.")
  }

  # stop function if overlay list is not provided
  if (is.null(overlay_groups)) {
    stop("addATTAINS: overlay_groups must be supplied to run this function.")
  }

  if (catchment == FALSE) {
    # get geometry type
    geo.type <- .data$type[1]

    # set group name
    group.name <- switch(
      geo.type,
      "Point Feature" = "ATTAINS point features",
      "Line Feature" = "ATTAINS line features",
      "Polygon Feature" = "ATTAINS polygon features"
    )
  } else {
    group.name <- switch(
      catchment_type,
      "attains_au" = "ATTAINS catchments",
      "missing_raw" = "ATTAINS outlines",
      "wo_attains" = "missing ATTAINS catchment outlines"
    )
  }

  # Add ATTAINS to map
  # ATTAINS assessment unit polygons
  if (group.name == "ATTAINS polygon features") {
    # add ATTAINS assessment unit polygons
    map <- map |>
      leaflet::addPolygons(
        data = .data,
        group = "ATTAINS polygon features",
        color = ~ .data$col,
        fill = ~ .data$col,
        weight = 3,
        fillOpacity = 0.5,
        popup = paste0(
          "Assessment Unit Name: ",
          .data$assessmentunitname,
          "<br> Assessment Unit ID: ",
          .data$assessmentunitidentifier,
          "<br> Status: ",
          .data$overallstatus,
          "<br> Assessment Unit Type: ",
          .data$type,
          "<br> <a href=",
          .data$waterbodyreportlink,
          " target='_blank'>ATTAINS Link</a>"
        )
      )
    overlay_groups <- c(overlay_groups, "ATTAINS polygon features")
  }

  # ATTAINS catchments
  if (group.name == "ATTAINS catchments") {
    # add catchments with ATTAINS features
    map <- map |>
      leaflet::addPolygons(
        data = .data,
        group = "ATTAINS catchments",
        color = "black",
        fillColor = "grey",
        weight = 1,
        fillOpacity = 0.3,
        popup = paste0("NHDPlus HR Catchment ID: ", .data$nhdplusid)
      )
    overlay_groups <- c(overlay_groups, "ATTAINS catchments")
  }

  # polygon assessment units
  if (group.name == "ATTAINS line features") {
    map <- map |>
      leaflet::addPolylines(
        data = .data,
        group = "ATTAINS line features",
        color = ~ .data$col,
        weight = 4,
        fillOpacity = 1,
        popup = paste0(
          "Assessment Unit Name: ",
          .data$assessmentunitname,
          "<br> Assessment Unit ID: ",
          .data$assessmentunitidentifier,
          "<br> Status: ",
          .data$overallstatus,
          "<br> Assessment Unit Type: ",
          .data$type,
          "<br> <a href=",
          .data$waterbodyreportlink,
          " target='_blank'>ATTAINS Link</a>"
        )
      )
    overlay_groups <- c(overlay_groups, "ATTAINS line features")
  }

  # without ATTAINS catchments
  if (group.name == "missing ATTAINS catchment outlines") {
    map <- map |>
      leaflet::addPolygons(
        data = .data,
        group = "missing ATTAINS catchment outlines",
        color = "black",
        weight = 1,
        fillOpacity = 0,
        popup = paste0(.data$NHD.resolution, " catchment ID: ", .data$nhd)
      )
    overlay_groups <- c(overlay_groups, "missing ATTAINS catchment outlines")
  }

  # point assessment units
  if (group.name == "ATTAINS point features") {
    if (is.null(icons)) {
      get.icons <- getMapIconLabels()

      images <- unlist(get.icons[1])

      # remove intermediate objects
      rm(get.icons)
    } else {
      images <- icons

      # remove intermediate objects
      rm(icons)
    }

    pointIcons <- leaflet::icons(
      iconUrl = dplyr::case_when(
        .data$overallstatus == "Fully Supporting" ~ images[12],
        .data$overallstatus == "Not Supporting" ~ images[11],
        .data$overallstatus == "Not Assessed" ~ images[13]
      ),
      iconWidth = 48,
      iconHeight = 48
    )

    map <- map |>
      leaflet::addMarkers(
        data = .data,
        group = "ATTAINS point features",
        lng = ~X,
        lat = ~Y,
        icon = pointIcons,
        popup = paste0(
          "Assessment Unit Name: ",
          .data$assessmentunitname,
          "<br> Assessment Unit ID: ",
          .data$assessmentunitidentifier,
          "<br> Status: ",
          .data$overallstatus,
          "<br> Assessment Unit Type: ",
          .data$type,
          "<br> <a href=",
          .data$waterbodyreportlink,
          " target='_blank'>ATTAINS Link</a>"
        )
      )
    overlay_groups <- c(overlay_groups, "ATTAINS point features")

    # remove intermediate objects
    rm(images, pointIcons)
  }

  # ATTAINS missing raw features
  if (group.name == "ATTAINS outlines") {
    map <- map |>
      leaflet::addPolygons(
        data = .data,
        group = "ATTAINS outlines",
        color = ~ .data$col,
        fill = ~ .data$col,
        weight = 3,
        fillOpacity = 0.25,
        popup = paste0(
          "Assessment Unit Name: ",
          .data$assessmentunitname,
          "<br> Assessment Unit ID: ",
          .data$assessmentunitidentifier,
          "<br> Status: ",
          .data$overallstatus,
          "<br> Assessment Unit Type: ",
          .data$type,
          "<br> <a href=",
          .data$waterbodyreportlink,
          " target='_blank'>ATTAINS Link</a>",
          "<br> NHDPlus HR Catchment ID: ",
          .data$nhdplusid
        )
      )
    overlay_groups <- c(overlay_groups, "ATTAINS outlines")
  }

  # create list containing map and overlay_groups
  au.list <- list(map, overlay_groups)

  names(au.list) <- c("map", "overlay_groups")

  # remove intermediate object
  if (exists("geo.type")) {
    rm(geo.type)
  }

  # return map and list of overlay groups
  return(au.list)
}

#' addAllATTAINS
#' Internal function to add all ATTAINS assessment units (lines, points, or polygons) or
#' ATTAINS catchments (polygons) to TADA maps.
#'
#' @param .data A data frame created using prepATTAINSMapper (must contain a geometry
#' column)
#'
#' @param map The TADA leaflet map to add the ATTAINS geometry to. Required, function
#' will not run with map is missing or NULL. Default is map = NULL.
#'
#' @param overlay_groups Initialized vector to add names of groups to map. This is
#' to allow users to toggle specific layers on/off. If it is NULL, the function will
#' fail with an error message. Default is overlay_list = NULL.
#'
#' @param icons Character argument. The list of icon paths generated by the internal
#' function getMapIconLabels. If already called in a larger mapping function, it can
#' be referenced here (for efficiency). If icons = NULL, getMapIconLabels will run
#' and fetch the list. Default is icons = NULL. This argument is only applied to
#' for point assessment units.
#'
#' @param catchment Boolean argument. Specifies whether the geometries being
#' mapped are catchments or assessment units to determine color and other attributes.
#' Default is catchment = FALSE, meaning that assessment units are being mapped.
#'
#' @param missing_raw_layer Boolean argument. Evaluated when catchment = TRUE to determine
#' if the catchments being mapped are those corresponding to assessment units that
#' are missing point/line/polygon data but still preserved in the catchment layer
#' (missing_raw = TRUE) or catchments containing assessment unit features
#' (missing_raw = FALSE). Default is missing_raw = FALSE.
#'
#' @param lines_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit line geometry. When lines_layer = NULL, the line
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @param polygons_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit polygon geometry. When polygons_layer = NULL, the polygon
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @param points_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit point geometry. Default = NULL.
#'
#' @param catchment_layer Optional data frame argument. Contains the data required to
#' map ATTAINS catchment geometry. Default = NULL.
#'
#' @param outline_layer Optional data frame argument. Contains the data required to
#' map catchments for monitoring locations not associated with an assessment unit.
#' Default = NULL.
#'
#' @param missing_raw_layer Optional data frame argument. Contains the data required to
#' map ATTAINS catchments for ATTAINS assessment units that are missing geometry.
#' Default = NULL.
#'
#' @return ATTAINS geometry correctly formatted for display in a TADA leaflet map.
#'
# add all ATTAINS geometries to map
addAllATTAINS <- function(
  .data = NULL,
  map = NULL,
  points_layer = NULL,
  polygons_layer = NULL,
  lines_layer = NULL,
  catchment_layer = NULL,
  outline_layer = NULL,
  missing_raw_layer = NULL,
  overlay_groups = NULL,
  icons = NULL,
  catchment = NULL
) {
  # ensure map is provided
  if (missing(map) | is.null(map)) {
    stop("addAllATTAINS: Argument 'map' is required.")
  }

  # helper to silence errors while preserving readability
  safe_add <- function(expr) {
    try(expr, silent = TRUE)
    invisible(NULL)
  }

  # Add ATTAINS catchment outlines (if they exist)
  safe_add({
    if (!is.null(catchment_layer) && nrow(catchment_layer) > 0) {
      polygon_catch <- addATTAINS(
        catchment_layer,
        map = map,
        overlay_groups = overlay_groups,
        catchment = TRUE
      )

      map <- polygon_catch$map
      overlay_groups <- polygon_catch$overlay_groups

      rm(polygon_catch)
    }
  })

  # Add ATTAINS catchment outlines as AUs for missing_raw (if they exist)
  safe_add({
    if (!is.null(missing_raw_layer) && nrow(missing_raw_layer) > 0) {
      missing_outlines <- addATTAINS(
        missing_raw_layer,
        map = map,
        overlay_groups = overlay_groups,
        catchment = TRUE,
        catchment_type = "missing_raw"
      )

      map <- missing_outlines$map
      overlay_groups <- missing_outlines$overlay_groups

      rm(missing_outlines)
    }
  })

  # Add outlines that have no ATTAINS (if they exist)
  safe_add({
    if (!is.null(outline_layer) && nrow(outline_layer) > 0) {
      wo_attains <- addATTAINS(
        outline_layer,
        map = map,
        overlay_groups = overlay_groups,
        catchment = TRUE,
        catchment_type = "wo_attains"
      )

      map <- wo_attains$map
      overlay_groups <- wo_attains$overlay_groups

      rm(wo_attains)
    }
  })

  # Add ATTAINS polygon features (if they exist)
  safe_add({
    if (!is.null(polygons_layer) && nrow(polygons_layer) > 0) {
      polygons_aus <- addATTAINS(
        polygons_layer,
        map = map,
        overlay_groups = overlay_groups
      )

      map <- polygons_aus$map
      overlay_groups <- polygons_aus$overlay_groups

      rm(polygons_aus)
    }
  })

  # Add ATTAINS line features (if they exist)
  safe_add({
    if (!is.null(lines_layer) && nrow(lines_layer) > 0) {
      lines_aus <- addATTAINS(
        lines_layer,
        map = map,
        overlay_groups = overlay_groups
      )

      map <- lines_aus$map
      overlay_groups <- lines_aus$overlay_groups

      rm(lines_aus)
    }
  })

  # Add ATTAINS point features (if they exist)
  safe_add({
    if (!is.null(points_layer) && nrow(points_layer) > 0) {
      points_aus <- addATTAINS(
        points_layer,
        map = map,
        overlay_groups = overlay_groups,
        icons = icons
      )

      map <- points_aus$map
      overlay_groups <- points_aus$overlay_groups

      rm(points_aus)
    }
  })

  list(map = map, overlay_groups = overlay_groups)
}

#' getATTAINSColorsRef
#'
#' Internal function to return a data frames specifying the color the feature should be
#' displayed in for a leaflet map based on the value in the "overallstatus" column
#' in the ATTAINS_points, ATTAINS_polygons, or ATTAINS_lines data frames created
#' with TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk.
#'
#' @return A data frame with the columns overallstatus, col, dark_col, and priority.
#'
# create ref for ATTAINS overall support colors
getATTAINSColorsRef <- function() {
  # get TADA palette
  tada.pal <- TADA_ColorPalette()

  # create df of colors for use in mapping functions
  colors <- data.frame(
    overallstatus = c("Not Supporting", "Fully Supporting", "Not Assessed"),
    col = c(tada.pal[3], tada.pal[4], tada.pal[7]),
    dark_col = c(tada.pal[12], tada.pal[6], tada.pal[11]),
    priority = c(1, 2, 3)
  )

  # remove intermediate object
  rm(tada.pal)

  return(colors)
}

#' prepATTAINSMapper
#'
#' Internal function to prepare ATTAINS geometry from TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk for display in a leaflet map.
#'
#' @param .data Data frame. One of the data frames containing geometry from the
#' output of TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk
#' (ATTAINS_catchments, ATTAINS_polygons, ATTAINS_points, or ATTAINS_lines).
#'
#' @param color_ref Data frame. A data frame containing the colors that should be
#' applied to a feature based on its overallstatus in ATTAINS. Must contain the
#' columns overallstatus, col, dark_col, and priority. Can be created with the
#' internal function getATTAINSColorsRef. If color_ref = NULL, the function will
#' run getATTAINSColorsRef to create a color ref data frame.
#'
#' @param geo_type Character string. Type of geometry to be prepared for mapping.
#' Allowable values are "points", "lines", and "polygons". If no geo_type (geo_type
#' = NULL) is supplied the function will attempt to determine the type from the
#' "geometry" column. If geo_type is not supplied and cannot be determined, the
#' function will stop with an error.
#'
#' @param auid_list Character string. List of assessment unit identifiers to filter
#' the data frame before returning. When a list is provided, only assessment unit
#' identifiers included in the list will be shown on the map. When auid_list = NULL
#' all assessment units in the source data set are show on the map. Default = NULL.
#'
#' @return A data frame with the columns overallstatus, col, dark_col, and priority.
#'
# prep data for mapping with ATTAINS
prepATTAINSMapper <- function(
  .data,
  geo_type = NULL,
  color_ref = NULL,
  auid_list = NULL
) {
  # check to see if any data contained in .data
  if (dim(.data)[1] == 0) {
    mapper <- NULL

    # return NULL mapper if no data present in .data
    return(mapper)
  }

  # if geo_type is not provided, determine it from .data
  if (is.null(geo_type)) {
    # get geometry type
    check.type <- unique(as.character(sf::st_geometry_type(
      .data,
      by_geometry = TRUE
    )))

    # check length of check.type
    if (length(check.type) > 1) {
      # find base type if multi types are present
      base <- sub("^MULTI", "", check.type)

      # get list of unique base types
      check.type <- unique(base)

      # stop function with error message if multiple geometry base types are found
      if (length(check.type) > 2) {
        stop(
          "prepATTAINSMapper: geometry column in .data must contain only one base geometry type."
        )
      }
    }

    # normalize geometry type
    geo_type <- dplyr::case_when(
      check.type %in% c("POINT", "MULTIPOINT") ~ "points",
      check.type %in% c("LINESTRING", "MULTILINESTRING") ~ "lines",
      check.type %in% c("POLYGON", "MULTIPOLYGON") ~ "polygons"
    )

    # remove intermediate object
    rm(check.type)
  }

  # check for color ref and create if it does not exist
  if (is.null(color_ref)) {
    color_ref <- getATTAINSColorsRef()
  }

  # check user supplied color ref df to see if it contains required cols
  if (!is.null(color_ref)) {
    req.cols <- c("overallstatus", "col", "dark_col", "priority")

    # stop function if any required columns are missing
    if (!all(req.cols %in% names(color_ref))) {
      stop(
        "prepATTAINSMapper: color_ref must contain the following columns: overallstatus, col, dark_col, priority."
      )
    }
  }

  # prep point data
  if (geo_type == "points") {
    # extract coordinates and convert to a tibble (to handle point or multipoint)
    coords <- sf::st_coordinates(.data)

    if (dim(coords)[1] > 1) {
      coords <- coords |>
        tibble::as_tibble() |>
        tibble::rowid_to_column(var = "index")

      # points mapper setup
      mapper <- .data |>
        dplyr::left_join(color_ref, by = "overallstatus") |>
        dplyr::mutate(type = "Point Feature") |>
        tibble::rowid_to_column(var = "index") |>
        dplyr::right_join(coords, by = "index")
    } else {
      mapper <- NULL
    }
  }

  # prep line data
  if (geo_type == "lines") {
    # add colors to line features
    mapper <- .data |>
      dplyr::left_join(color_ref, by = "overallstatus") |>
      dplyr::mutate(type = "Line Feature")
  }

  # prep polygon data
  if (geo_type == "polygons") {
    # add colors to polygon features
    mapper <- .data |>
      dplyr::left_join(color_ref, by = "overallstatus") |>
      dplyr::mutate(type = "Polygon Feature") |>
      # sort df so smaller assessment units will map on top of larger ones if they overlap
      dplyr::arrange(dplyr::desc(Shape_Area))
  }

  if (!is.null(auid_list) & dim(mapper)[1] > 0) {
    mapper <- mapper |> dplyr::filter(assessmentunitidentifier %in% auid_list)

    if (dim(mapper)[1] == 0) {
      mapper <- NULL
    }
  }
  return(mapper)
}

#' prepAllATTAINSMapper
#'
#' Internal function to prepare all ATTAINS geometry from
#' TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk for display in a
#' leaflet map by running prepATTAINSMapper for all available assessment unit
#' layers.
#'
#' @param lines_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit line geometry. When lines_layer = NULL, no line
#' assessment units are prepared for mapping. Default = NULL.
#'
#' @param polygons_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit polygon geometry. When polygons_layer = NULL, no polygon
#' assessment units are prepared for mapping. Default = NULL.
#'
#' @param points_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit point geometry. When points_layer = NULL, no point
#' assessment units are prepared for mapping. Default = NULL.
#'
#' @param catchment_layer Optional data frame argument. Contains the data required to
#' map ATTAINS catchment polygon geometry. When catchment_layer = NULL, no ATTAINS
#' catchments are prepared for mapping. Default = NULL.
#'
#' @param color_ref Optional data frame argument. Contains the columns "overall
#' status" (ATTAINS overall status), "col" (light fill color), "dark_col" (darker
#' color for points or outlines), and "priority" (for displaying on map). When
#' color_ref = NULL, the function getATTAINSColorRef is used to set the standardized
#' colors used for ATTAINS assessment units throughout EPATADA. The default is
#' color_ref = NULL.
#'
#' @param auid_list A list of any ATTAINS assessment unit identifiers that should
#' be excluded from the output (not included in map).
#'
#' @return A list of data frames ready for use in a TADA leaflet map.
# prep all ATTAINS layers for use in leaflet map
prepAllATTAINSMapper <- function(
  lines_layer = NULL,
  polygons_layer = NULL,
  points_layer = NULL,
  catchment_layer = NULL,
  color_ref = NULL,
  auid_list = NULL
) {
  # get color ref for ATTAINS overall status if not provided
  if (is.null(color_ref)) {
    color_ref <- getATTAINSColorsRef()
  }

  # point assessment units
  points_mapper <- NULL

  points_mapper <- prepATTAINSMapper(
    points_layer,
    geo_type = "points",
    color_ref = color_ref,
    auid_list = auid_list
  )

  # line assessment units
  lines_mapper <- NULL

  lines_mapper <- prepATTAINSMapper(
    lines_layer,
    geo_type = "lines",
    color_ref = color_ref,
    auid_list = auid_list
  )

  # polygon assessment units
  polygons_mapper <- NULL

  polygons_mapper <- prepATTAINSMapper(
    polygons_layer,
    geo_type = "polygons",
    color_ref = color_ref,
    auid_list = auid_list
  )
  # create list of mapper dfs
  au_mapper <- list(points_mapper, lines_mapper, polygons_mapper)

  names(au_mapper) <- c("points_mapper", "lines_mapper", "polygons_mapper")

  # return data frames ready for mapping
  return(au_mapper)
}

#' getWQPSiteStats
#'
#' Internal function to prepare site data for use in leaflet map popup.
#'
#' @param .data TADA data frame. This function can incorporate ATTAINS data in the
#' TADA data frame (for example, using the TADA_with_ATTAINS df created with
#' TADA_CreateATTAINSAUMLCrosswalk, TADA_CreateAUMLCrosswalk).
#'
#' @param attains Boolean argument. If attains = TRUE and ATTAINS prefixed columns
#' are included in .data, ATTAINS Assessment Units will be listed in the popup. If
#' attains = FALSE (or there are no ATTAINS prefixed columns in .data), no assessment
#' unit data is included in popup. Default is attains = TRUE.
#'
#' @return A data frame formatted correctly for use in a leaflet pop up containing
#' WQP site data.
#'
# Develop WQP site stats (e.g. count of observations, parameters, per site)
getWQPSiteStats <- function(.data, attains = TRUE) {
  # set base list of columns to group .data for summarizing
  group.cols <- c(
    "TADA.MonitoringLocationIdentifier",
    "TADA.MonitoringLocationName",
    "OrganizationFormalName",
    "TADA.LatitudeMeasure",
    "TADA.LongitudeMeasure"
  )

  if (attains == TRUE) {
    # if assessment unit data are not available, set attains param to FALSE
    if (!"ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {
      attains <- FALSE

      # print missing to user explaning that assessment unit data are not present
      message(
        "getWQPSiteStats: ATTAINS.AssessmentUnitIdentifier is not present in .data. ",
        "Returning WQP site stats without assessment unit identifiers."
      )
    }

    # if assessment unit data are available
    if ("ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {
      # check for ref source, add if missing
      if (!"TADA.AURefSource" %in% names(.data)) {
        .data <- .data |> dplyr::mutate(TADA.AURefSource = "not provided")
      }

      # add TADA.AURefSource to grouping
      group.cols <- append(group.cols, "TADA.AURefSource")
    }

    # create data summary including ATTAINS assessment unit identifiers
    sumdat <- .data |>
      dplyr::group_by(!!!rlang::syms(group.cols)) |>
      dplyr::summarize(
        Sample_Count = length(unique(ResultIdentifier)),
        Visit_Count = length(unique(ActivityStartDate)),
        Parameter_Count = length(unique(TADA.CharacteristicName)),
        Organization_Count = length(unique(OrganizationIdentifier)),
        ATTAINS_AUs = as.character(list(unique(
          ATTAINS.AssessmentUnitIdentifier
        )))
      ) |>
      dplyr::mutate(
        ATTAINS_AUs = ifelse(is.na(ATTAINS_AUs), "None", ATTAINS_AUs),
        LatitudeMeasure = as.numeric(TADA.LatitudeMeasure),
        LongitudeMeasure = as.numeric(TADA.LongitudeMeasure)
      )

    return(sumdat)
  }

  if (attains == FALSE) {
    # create summary without ATTAIINS data
    sumdat <- .data |>
      dplyr::group_by(!!!rlang::syms(group.cols)) |>
      dplyr::summarize(
        Sample_Count = length(unique(ResultIdentifier)),
        Visit_Count = length(unique(ActivityStartDate)),
        Parameter_Count = length(unique(TADA.CharacteristicName)),
        Organization_Count = length(unique(OrganizationIdentifier))
      ) |>
      dplyr::mutate(
        LatitudeMeasure = as.numeric(TADA.LatitudeMeasure),
        LongitudeMeasure = as.numeric(TADA.LongitudeMeasure)
      )

    return(sumdat)
  }
}

#' createTADABasemap
#'
#' Internal function to create base leaflet map for TADA mapping functions.
#'
#' @param .data A TADA data frame. Must contain the columns TADA.LatitudeMeasure
#' and TADA.Longitude measure to set the extent of the map.
#'
#' @return The basemap for TADA mapping functions.
#'
createTADABasemap <- function(.data) {
  stopifnot(all(
    c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure") %in% names(.data)
  ))
  bbox <- createBBox(.data, as_vector = TRUE)

  btn <- leaflet::easyButton(
    icon = "fa-arrows-alt",
    title = "Reset view",
    position = "topleft",
    onClick = htmlwidgets::JS(sprintf(
      "function(btn, map){ map.fitBounds([[%f,%f],[%f,%f]]); }",
      bbox[2],
      bbox[1],
      bbox[4],
      bbox[3]
    ))
  )

  leaflet::leaflet() |>
    leaflet::addProviderTiles(
      "Esri.WorldTopoMap",
      group = "World topo",
      options = leaflet::providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      )
    ) |>
    leaflet::clearShapes() |>
    leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
    addMapReset(bbox = bbox)
}

#' addMapReset
#'
#' Internal function to add a reset button to the map to return it to its
#' original extent.
#'
#' @param map A leaflet map to add the reset button to.
#'
#' @param bbox The bounding box the map view should return to.
#'
#' @return The original map with a reset button added.
#'
addMapReset <- function(map, bbox = NULL) {
  btn <- leaflet::easyButton(
    icon = "fa-arrows-alt",
    title = "Reset view",
    position = "topleft",
    onClick = htmlwidgets::JS(sprintf(
      "function(btn, map){ map.fitBounds([[%f,%f],[%f,%f]]); }",
      bbox[2],
      bbox[1],
      bbox[4],
      bbox[3]
    ))
  )

  map <- map |> leaflet::addEasyButton(btn)

  rm(btn)

  return(map)
}

#' createBBox
#'
#' Internal function to create bounding box for maps based on TADA.LatitudeMeasure
#' and TADA.LongitudeMeasure columns from a TADA df or ATTAINS geometry.
#'
#' @param .data Data frame. Must contain the columns TADA.LatitudeMeasure and
#' TADA.Longitude measure to set the extent of the map.
#'
#' @param attains_geo Boolean argument. When attains_geo = TRUE, the function must reference
#' the geometry column of .data. When attains_geo = FALSE, the function references
#' the TADA.LatitudeMeasure and TADA.LongitudeMeasure to find the bounding box.
#' Default is attains_geo = FALSE.
#'
#' @param as_vector Boolean argument. When as_vector = TRUE, the bounding box values
#' are returned as vector. When as_vector = FALSE, values are returned as a bounding
#' box. Default is as_vector = TRUE.
#'
#' @return A bounding box for use in leaflet mapping functions.
# Create bounding box
createBBox <- function(.data, as_vector = TRUE, attains_geo = FALSE) {
  if (isFALSE(attains_geo)) {
    # create bounding box
    bbox <- sf::st_bbox(
      c(
        xmin = min(.data$TADA.LongitudeMeasure),
        ymin = min(.data$TADA.LatitudeMeasure),
        xmax = max(.data$TADA.LongitudeMeasure),
        ymax = max(.data$TADA.LatitudeMeasure)
      ),
      crs = sf::st_crs(.data)
    )
  }

  if (isTRUE(attains_geo)) {
    # should a buffer distance be added around lines/points?
    # build a new col from per-row bbox polygons
    bbox_sfc <- sf::st_sfc(
      purrr::map(sf::st_geometry(.data), ~ sf::st_as_sfc(sf::st_bbox(.x))[[1]]),
      crs = sf::st_crs(.data)
    )

    # either add it as an additional column
    .data$bbox_geom <- bbox_sfc
  }

  # return as bounding box
  if (as_vector == FALSE) {
    return(bbox)
  }

  # return as vector (required for some TADA functions)
  if (as_vector == TRUE) {
    bbox <- bbox |> as.vector()

    return(bbox)
  }
}

#' showMissingATTAINSAUs
#' Internal function to apply a dashed circle marking around WQP monitoring locations
#' with a user supplied assessment unit assignment which does not have corresponding
#' geometry in ATTAINS.
#'
#' @param ATTAINS_table A TADA data frame created with TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk (called "TADA_with_ATTAINS" in the list of output dfs).
#'
#' @param ATTAINS_points A data frame containing ATTAINS point assessment units data
#' including geometry that was created with TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk.
#'
#' @param ATTAINS_lines A data frame containing ATTAINS line assessment units data
#' including geometry that was created with TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk.
#'
#' @param ATTAINS_polygons A data frame containing ATTAINS polygon assessment units data
#' including geometry that was created with TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk.
#'
#' @param map A leaflet map of TADA data to apply the symbology for missing ATTAINS
#' assessment units to.
#'
#' @param overlay_groups Initialized vector to add names of groups to map. This is
#' to allow users to toggle specific layers on/off. If it is NULL, the function will
#' fail with an error message. Default is overlay_list = NULL.
#'
#' @return A TADA leaflet map marking WQP monitoring locations with user
#' supplied assessment unit identifiers that do not have matching geometry in
#' ATTAINS by circling them with a black dashed line.
#'
# add ATTAINS geometry to existing leaflet map
showMissingATTAINSAUs <- function(
  map = NULL,
  overlay_groups = NULL,
  ATTAINS_table = NULL,
  ATTAINS_points = NULL,
  ATTAINS_lines = NULL,
  ATTAINS_polygons = NULL
) {
  # stop function if map is not provided
  if (is.null(map)) {
    stop("addATTAINS: a leaflet map must be supplied to run this function.")
  }

  # check for Monitoring Locations with assigned assessment units that do not have geometry from ATTAINS
  if ("TADA.AURefSource" %in% names(ATTAINS_table)) {
    user.refs <- ATTAINS_table |>
      dplyr::filter(TADA.AURefSource == "User-supplied Ref") |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier,
        ATTAINS.AssessmentUnitIdentifier,
        TADA.LatitudeMeasure,
        TADA.LongitudeMeasure,
        ATTAINS.WaterType
      ) |>
      dplyr::distinct()

    # if any assessment unit ids were assigned by user check to see if they have matching geometry from ATTAINS
    if (dim(user.refs)[1] > 0) {
      # internal function to create list of assessment unit ids
      listAUIDs <- function(.data) {
        if (dim(.data)[1] == 0) {
          list <- list()
        } else {
          list <- .data |>
            sf::st_drop_geometry() |>
            dplyr::select(assessmentunitidentifier) |>
            dplyr::distinct() |>
            dplyr::pull()
        }

        return(list)
      }

      # create list of assessment units with geometry
      point.aus <- listAUIDs(ATTAINS_points)

      line.aus <- listAUIDs(ATTAINS_lines)

      polygon.aus <- listAUIDs(ATTAINS_polygons)

      # combine lists and retain unique values
      all.attains.aus <- unique(Reduce(
        c,
        list(point.aus, line.aus, polygon.aus)
      ))

      # find if any assigned aus are missing geometry
      missing.geo <- user.refs |>
        dplyr::filter(!ATTAINS.AssessmentUnitIdentifier %in% all.attains.aus)

      # remove intermediate objects
      rm(point.aus, line.aus, polygon.aus, all.attains.aus, user.refs)

      # if there are any user-assigned assessment unit identifiers without geometry in ATTAINS add to map
      if (dim(missing.geo)[1] > 0) {
        # set up icons for missing geometry
        missingIcon <- leaflet::icons(
          iconUrl = system.file(
            "extdata/icons",
            "circle-dashed.png",
            package = "EPATADA"
          ),
          iconWidth = 48,
          iconHeight = 48
        )

        # add missing assessment unit symbology to map
        map <- map |>
          leaflet::addMarkers(
            data = missing.geo,
            group = "not in ATTAINS",
            lng = ~TADA.LongitudeMeasure,
            lat = ~TADA.LatitudeMeasure,
            icon = missingIcon,
            popup = paste0(
              "Assessment Unit Name: ",
              "not available in ATTAINS",
              "<br> Assessment Unit ID: ",
              missing.geo$ATTAINS.AssessmentUnitIdentifier,
              "<br> Status: ",
              "not available in ATTAINS",
              "<br> Assessment Unit Type: ",
              "not available in ATTAINS"
            )
          )

        overlay_groups <- c(overlay_groups, "not in ATTAINS")

        missing.list <- list(map, overlay_groups)

        names(missing.list) <- c("map", "overlay_groups")

        # remove intermediate objects
        rm(map, overlay_groups)

        # return updated map and list of overlay_groups
        return(missing.list)
      }
    }
  }
}

#' addWQPSites
#' Internal function to add WQP sites to a leaflet map. If TADA.AURefSource is
#' included in the TADA data frame, the default is to display varying icons to
#' indicate the source of the assessment unit/monitoring location crosswalk. If
#' TADA.AURefSource is not included or the user does not want to display
#' assessment unit identifier information via the icons (by
#' setting ref_icons = FALSE), solid black circle markers are used to display
#' all WQP sites.
#'
#' @param .data A TADA data frame created with TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk (called "TADA_with_ATTAINS" in the list of output dfs)
#' or a subsetted TADA data frame containing all columns required for building map
#' and pop up (Note: Add list of required columns (HRM 1/5/26)). Needs to be in sumdat
#' format or getWQPSiteStats will be run.
#'
#' @param map A leaflet map of TADA data to apply the symbology for missing ATTAINS
#' AUs to.
#'
#' @param icons Character argument. The list of icon paths generated by the internal
#' function getMapIconLabels. If already called in a larger mapping function, it can
#' be referenced here (for efficiency). If icons = NULL, getMapIconLabels will run
#' and fetch the list. Default is icons = NULL. This argument is only applied to
#' for point AUs.
#'
#' @param icon_labels Character argument. The list of icon labels generated by the
#' internal function getMapIconLabels. If already called in a larger mapping function,
#' it can be referenced here (for efficiency). If icons = NULL, getMapIconLabels
#' will run and fetch the list. Default is icons = NULL. This argument is only
#' applied for point AUs.
#'
#' @param overlay_groups Initialized vector to add names of groups to map. This is
#' to allow users to toggle specific layers on/off. If it is NULL, the function will
#' fail with an error message. Default is overlay_list = NULL.
#'
#' @param ref_icons Boolean argument. Determines whether custom icons are displayed to differentiate between
#' different crosswalk sources for the assignment of WQP Monitoring Locations to Assessment Units if this
#' information is included in the TADA_with_ATTAINS dataframe supplied to the function. When
#' ref_icons = TRUE three different icons will be used for the map.
#' 1) The circle with the user icon is for matches from the user supplied
#' ref if that was supplied as an input to TADA_CreateAUMLCrosswalk().
#' 2) The circle with a check mark is for matches from [TADA_GetATTAINSAUMLCrosswalk()] which
#' runs within TADA_CreateAUMLCrosswalk(). If an organization has recorded this
#' information in ATTAINS, this gets the organizations crosswalk of known
#' monitoring location identifiers and assessment unit associations.
#' 3) The plain circle represents matches
#' made with [TADA_CreateATTAINSAUMLCrosswalk()] which also runs within
#' TADA_CreateAUMLCrosswalk() to link catchment-based ATTAINS assessment unit
#' data to Water Quality Portal observations.
#' When ref_icons = FALSE or the source is not provided in .data, all
#' Monitoring Locations are show with a plain circle.
#'
#' @return ATTAINS geometry correctly formatted for display in a TADA leaflet map.
#'
# add ATTAINS geometry to existing leaflet map
addWQPSites <- function(
  .data,
  map = NULL,
  icons = NULL,
  icon_labels = NULL,
  ref_icons = TRUE,
  overlay_groups = NULL
) {
  # data summary columns
  sum.cols <- c("Sample_Count", "Visit_Count", "Parameter_Count", "ATTAINS_AUs")

  # check to see if data summary columns are present in .data
  if (!all(sum.cols %in% names(.data))) {
    # calculate summary columns if required
    .data <- getWQPSiteStats(.data)
  }

  # set base pop up for monitoring locations
  set.popup <- paste0(
    "Site ID: ",
    .data$TADA.MonitoringLocationIdentifier,
    "<br> Site Name: ",
    .data$TADA.MonitoringLocationName,
    "<br> Organization Name: ",
    .data$OrganizationFormalName,
    "<br> Measurement Count: ",
    .data$Sample_Count,
    "<br> Visit Count: ",
    .data$Visit_Count,
    "<br> Characteristic Count: ",
    .data$Parameter_Count,
    "<br> ATTAINS Assessment Unit(s): ",
    .data$ATTAINS_AUs
  )

  # add assessment unit ref source to pop up  if available
  if ("TADA.AURefSource" %in% names(.data)) {
    set.popup <- paste0(
      set.popup,
      "<br>",
      "Crosswalk Source: ",
      .data$TADA.AURefSource
    )
  }

  # check if icons are provided
  if (is.null(icons)) {
    # get icons if not provided
    get.icons <- getMapIconLabels()

    # list of icon image paths
    images <- unlist(get.icons[1])

    # list of icon image labels
    img.labels <- unlist(get.icons[2])

    # remove intermediate objects
    rm(get.icons)
  } else {
    images <- icons

    img.labels <- icon_labels

    # remove intermediate objects
    rm(icons, icon_labels)
  }

  # set image ref, image label, and icon url lists for WQP monitoring locations
  if (!"TADA.AURefSource" %in% names(.data) | ref_icons == FALSE) {
    wqp.imgs <- images[8]
    wqp.labels <- img.labels[8]

    wqp.urls <- images[8]
  } else {
    wqp.imgs <- images[5:7]
    wqp.labels <- img.labels[5:7]

    wqp.urls <- dplyr::case_when(
      .data$TADA.AURefSource == "ATTAINS Crosswalk" ~ images[6],
      .data$TADA.AURefSource == "TADA_CreateATTAINSAUMLCrosswalk" ~ images[7],
      .data$TADA.AURefSource == "User-supplied Ref" ~ images[5]
    )
  }

  # Add WQP observation features (should always exist):
  map <- map |>
    leaflet::addMarkers(
      data = .data,
      group = "WQP Obersvations",
      lng = ~TADA.LongitudeMeasure,
      lat = ~TADA.LatitudeMeasure,
      icon = leaflet::icons(
        iconUrl = wqp.urls,
        iconWidth = 24,
        iconHeight = 24
      ),
      popup = set.popup
    )

  overlay_groups <- c(overlay_groups, "WQP Obersvations")

  wqp.list <- list(map, overlay_groups)

  names(wqp.list) <- c("map", "overlay_groups")

  # remove intermediate objects
  rm(map, overlay_groups, wqp.urls, set.popup)

  # return updated map and list of overlay_groups
  return(wqp.list)
}

#' addTADAMapLegend
#'
#'  Internal function to add a legend to TADA maps. Based on user inputs, the
#'  legend will display: circle markers based on ATTAINS assessment unit user ref,
#'  assessment unit overall status, ATTAINS assessment unit catchments, and
#'  assessment units identified by the user which are missing geometry in ATTAINS.
#'
#'  Suggested usage is to assign values to the wqp, ref_icons, attains_au,
#'  attains_missing, nhd_attains, and nhd_no_attains params as part of the
#'  workflow in larger TADA mapping functions (rather than relying on defaults)
#'  to ensure the resulting legend is appropriate for each map.
#'
#' @param .data A TADA data frame created with TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk (called "TADA_with_ATTAINS" in the list of output dfs)
#' or a subsetted TADA data frame containing all columns required for building map
#' and pop up (Note: Add list of required columns (HRM 1/5/26)).
#'
#' @param map A leaflet map of TADA data to add the legend to.
#'
#' @param icons Character argument. The list of icon paths generated by the internal
#' function getMapIconLabels. If already called in a larger mapping function, it can
#' be referenced here (for efficiency). If icons = NULL, getMapIconLabels will run
#' and fetch the list. Default is icons = NULL. This argument is only applied to
#' for point AUs.
#'
#' @param icon_labels Character argument. The list of icon labels generated by the
#' internal function getMapIconLabels. If already called in a larger mapping function,
#' it can be referenced here (for efficiency). If icons = NULL, getMapIconLabels
#' will run and fetch the list. Default is icons = NULL. This argument is only
#' applied for point AUs.
#'
#' @param wqp Boolean argument. Determines whether WQP circle marker icons are
#' displayed in the legend. When wqp = TRUE, the WQP markers are shown in the
#' the legend. Default wqp = TRUE. The ref_icons param determines whether
#' custom or solid fill circle markers are shown in the legend.
#'
#' @param ref_icons Boolean argument. Determines whether custom ref icons are
#' displayed in the legend. When ref_icons = TRUE, the custom icons are shown in
#' the legend. Default is ref_icons = TRUE.
#'
#' @param attains_au Boolean argument. Determines whether overall status for ATTAINS
#' aus colors are shown in the legend. When attains_au = TRUE, the colors indicating
#' ATTAINS assessment unit overall status are shown in the legend. Default is
#' attains_au = TRUE.
#'
#' @param attains_missing Boolean argument. Determines whether the dashed circle
#' indicating whether WQP sites with assessment units assigned by the user are
#' missing corresponding geometry in ATTAINS is shown in the legend. The dashed
#' circle icon is show when attains_missing = TRUE. The default is attains_missing
#' = TRUE.
#'
#' @param nhd_attains Boolean argument. Determines whether the icon for NHD
#' catchments containing ATTAINS features, a gray square with a black outline,
#' is shown in the legend. When nhd_attains = TRUE, the icon is shown in the legend.
#' Default is nhd_attains = TRUE.
#'
#' @return Custom leaflet legend for TADA maps.
#'
# add ATTAINS geometry to existing leaflet map
addTADAMapLegend <- function(
  .data,
  map = NULL,
  icons = NULL,
  icon_labels = NULL,
  wqp = TRUE,
  ref_icons = TRUE,
  attains_au = TRUE,
  attains_missing = TRUE,
  nhd_attains = TRUE
) {
  # stop function if no map is provided
  if (is.null(map)) {
    stop(
      "addTADAMapLegend: Param map is missing. A leaflet map is required to add the legend."
    )
  }

  # if icons or icon labels are not provided, fetch them with internal function getMapIconLabels
  if (is.null(icons) | is.null(icon_labels)) {
    get.icons <- getMapIconLabels()

    images <- unlist(get.icons[1])

    img.labels <- unlist(get.icons[2])

    # remove intermediate objects
    rm(get.icons)
  } else {
    images <- icons

    img.labels <- icon_labels
  }

  # create image and label ref lists for legend
  images.ref <- character(0)

  leg.labels <- character(0)

  # set base image and label ref lists for each section of legend
  # ATTAINS assessment unit overall status
  if (attains_au == TRUE) {
    images.ref <- append(images.ref, images[1:3])

    leg.labels <- append(leg.labels, img.labels[1:3])
  }

  # ATTAINS missing geometry
  if (attains_missing == TRUE) {
    images.ref <- append(images.ref, images[4])

    leg.labels <- append(leg.labels, img.labels[4])
  }

  # add WQP icons to legend
  if (wqp == TRUE) {
    # add ref icons for assessment unit crosswalk sources
    if (ref_icons == TRUE) {
      images.ref <- append(images.ref, images[5:7])

      leg.labels <- append(leg.labels, img.labels[5:7])
    }

    # add solid black circle markers for all WQP sites
    if (ref_icons == FALSE) {
      images.ref <- append(images.ref, images[8])

      leg.labels <- append(leg.labels, img.labels[8])
    }
  }

  # NHD catchments with ATTAINS features
  if (nhd_attains == TRUE) {
    images.ref <- append(images.ref, images[9])

    leg.labels <- append(leg.labels, img.labels[9])
  }

  # add legend to map
  map <- map |>
    leaflegend::addLegendImage(
      images = images.ref,
      labels = leg.labels,
      labelStyle = "font-size: 14px;",
      width = 14,
      height = 14,
      orientation = "vertical",
      title = htmltools::tags$div(
        "Legend",
        style = "font-size: 14px;
        text-align: left; font-weight: bold;"
      ),
      position = "bottomright"
    )

  # remove intermediate objects
  rm(images.ref, leg.labels)

  # return map with added legend
  return(map)
}

#' addLegendToggle
#' Internal function to apply a button to a TADA leaflet map to toggle the legend
#' on/off.
#'
#' @param map A TADA leaflet map to add the Toggle Legend button.
#'
#' @return A TADA map with the Toggle Legend button added
#'
addLegendToggle <- function(map = NULL) {
  if (is.null(map)) {
    stop(
      "addLegendToggle: a TADA leaflet map must be specified in order to add the legend."
    )
  }

  # add legend toggle to map
  map <- htmlwidgets::onRender(
    map,
    "
  function(el, x) {
    var button = document.createElement('button');
    button.innerHTML = 'Toggle Legend';
    button.style.position = 'absolute';
    button.style.top = '10px';
    button.style.right = '10px'; // Positioning in the top-right corner
    button.style.zIndex = 1000;
    button.style.padding = '5px 10px';
    button.style.backgroundColor = '#fff';
    button.style.border = '1px solid #ccc';
    button.style.borderRadius = '4px';
    button.onclick = function() {
      var legend = el.querySelector('.leaflet-control.legend'); // Adjust this selector to target the legend only
      if (legend) {
        if (legend.style.display === 'none') {
          legend.style.display = 'block';
        } else {
          legend.style.display = 'none';
        }
      }
    };
    el.appendChild(button);
  }
"
  )
  return(map)
}

#' addLayerControl
#' Internal function to add layer control to a TADA leaflet map to toggle the
#' legend on/off.
#'
#' @param map A TADA leaflet map to add layer control to.
#'
#' @param overlay_groups Initialized vector to add names of groups to map. This is
#' to allow users to toggle specific layers on/off. If it is NULL, the function will
#' fail with an error message. Default is overlay_list = NULL.
#'
#' @return A TADA map with the layer control added.
#'
# add layer control
addLayerControl <- function(map = NULL, overlay_groups = NULL) {
  if (is.null(map)) {
    stop(
      "addLayerControl: a TADA leaflet map must be specified in order to add the legend."
    )
  }

  # add layer control to map
  if (length(overlay_groups) > 0) {
    overlay_groups <- unique(overlay_groups)

    map <- map |>
      leaflet::addLayersControl(
        baseGroups = c("World topo"), # Always include a base group
        overlayGroups = overlay_groups,
        position = "bottomleft",
        options = leaflet::layersControlOptions(collapsed = TRUE)
      )

    # remove intermediate objects
    rm(overlay_groups)

    # return map with added layer control
    return(map)
  }
}

#' addFlaggedSitesMarkers
#' Internal function to add flagged sites to map for review.
#'
#' @param .data A TADA data frame containing TADA.LatitudeMeasure and
#' TADA.LongitudeMeasure for mapping.
#'
#' @param flag_type Character argument. Flag types are "lowres" and "outsideusa".
#'
#' @param map A leaflet map of TADA data to apply the symbology for flagged sites to.
#'
#' @return A TADA leaflet map with flagged site markers added.
#'
# add ATTAINS geometry to existing leaflet map
addFlaggedSitesMarkers <- function(.data, map = NULL, flag_type = NULL) {
  # add line for null map

  # set markers based on flag type
  # low resolution markers
  if (flag_type == "lowres") {
    flagIcon <- leaflet::makeAwesomeIcon(
      icon = "circle",
      library = "fa",
      iconColor = "#ffffff",
      markerColor = "green"
    )
  }

  # outside usa markers
  if (flag_type == "outsideusa") {
    flagIcon <- leaflet::makeAwesomeIcon(
      icon = "circle",
      library = "fa",
      iconColor = "#ffffff",
      markerColor = "darkblue"
    )
  }

  # create map with selected markers
  map <- map |>
    leaflet::addAwesomeMarkers(
      ~TADA.LongitudeMeasure,
      ~TADA.LatitudeMeasure,
      icon = flagIcon,
      popup = paste0(
        "Site ID: ",
        .data$TADA.MonitoringLocationIdentifier,
        "<br> Site Name: ",
        .data$TADA.MonitoringLocationName,
        "<br> Organization Name: ",
        .data$TADA.OrganizationFormalName,
        "<br> Latitude: ",
        .data$TADA.LatitudeMeasure,
        "<br> Longitude: ",
        .data$TADA.LongitudeMeasure
      ),
      data = .data
    )

  # remove intermediate objects
  rm(flagIcon)

  # return map
  return(map)
}

#' findATTAINSMissingRawFeatures
#'
#' Check ATTAINS_catchment data to identify assessment unit data missing from
#' ATTAINS assessment units points, polygons, and lines layers that is still
#' preserved in the catchment layer.
#'
#' @param .data The ATTAINS_catchments data frame created as a result of
#' TADA_CreateAUMLCrosswalk or TADA_CreateATTAINSAUMLCrosswalk.
#'
#' @param lines_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit line geometry. When lines_layer = NULL, the line
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @param polygons_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit polygon geometry. When polygons_layer = NULL, the polygon
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @param points_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit point geometry. When points_layer = NULL, the point
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @param auid_list A list of any ATTAINS assessment unit identifiers that should
#' be excluded from the output.
#'
#' @return A data frame of assessment data that is missing from assessment units
#' points, lines, and polygons layers but still preserved in the catchment layer.
#'
findATTAINSMissingRawFeatures <- function(
  .data,
  points_layer = NULL,
  lines_layer = NULL,
  polygons_layer = NULL,
  auid_list = NULL
) {
  # set missing raw features to null
  missing_raw_features <- NULL

  # find missing raw features
  missing_raw_features <- ATTAINS_catchments |>
    dplyr::filter(
      !assessmentunitidentifier %in%
        c(
          points_layer$assessmentunitidentifier,
          lines_layer$assessmentunitidentifier,
          polygons_layer$assessmentunitidentifier
        )
    )

  # filter for listed assessment units if required
  if (!is.null(auid_list)) {
    missing_raw_features <- missing_raw_features |>
      dplyr::filter(assessmentunitidentifier %in% auid_list)
  }

  # remove intermediate objects
  rm(points_layer, lines_layer, polygons_layer)

  # return missing raw features
  return(missing_raw_features)
}

#' checkForWQPData
#'
#' Check the results of TADA_CreateATTAINSAUMLCrosswalk and
#' TADA_CreateAUMLCrosswalk to verify the WQP data frame contains observations.
#' For use in TADA leaflet mapping functions that utilize ATTAINS data.
#'
#' @param .data The "TADA_with_ATTAINS" data frame that is part of the
#' output of TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk.
#'
#' @return The function will stop and provide an error message if no WQP
#' observations are present.
# check for WQP data
checkForWQPData <- function(.data = NULL) {
  if (is.null(.data) || dim(.data)[1] == 0) {
    stop("Your WQP dataframe has no observations.")
  }
}

#' checkTADAColsForMap
#'
#' Check to see if data frame selected for mapping contains the related TADA and
#' WQP columns. If param attains = TRUE, additional columns required to include
#' ATTAINS assessment unit identifier and source ref for assessment unit will also
#' be included in the check.
#'
#' @param .data A TADA data frame or the "TADA_with_ATTAINS" data frame that is
#' part of the output of TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk.
#'
#' @param attains Boolean argument. When attains = TRUE, the columns
#' "ATTAINS.AssessmentUnitIdentifier" and "TADA.AURefSource" will be added to the
#' check. Default is ATTAINS = FALSE.
#'
#' @return The function will stop and provide an error message if any required cols
#' are missing.
#'
# check for required columns
checkTADAColsForMap <- function(.data, attains = FALSE) {
  req.cols <- c(
    "TADA.LongitudeMeasure",
    "TADA.LatitudeMeasure",
    "HorizontalCoordinateReferenceSystemDatumName",
    "TADA.CharacteristicName",
    "TADA.MonitoringLocationIdentifier",
    "TADA.MonitoringLocationName",
    "ResultIdentifier",
    "ActivityStartDate",
    "TADA.OrganizationIdentifier"
  )

  if (attains == TRUE) {
    required_columns <- append(
      req.cols,
      c("ATTAINS.AssessmentUnitIdentifier", "TADA.AURefSource")
    )
  }

  if (!any(required_columns %in% colnames(.data))) {
    stop(
      "Your dataframe does not contain the necessary WQP-style column names."
    )
  }
}

#' checkForATTAINSGeo
#'
#' Check to see if any ATTAINS assessment unit geometry was return as a result
#' of TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk. Will stop and
#' return an error message if no ATTAINS assessment unit geometry is present. For
#' use in TADA leaflet mapping functions that rely on ATTAINS geometry.
#'
#'
#' @param lines_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit line geometry. When lines_layer = NULL, the line
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @param polygons_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit polygon geometry. When polygons_layer = NULL, the polygon
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @param points_layer Optional data frame argument. Contains the data required to
#' map ATTAINS assessment unit point geometry. When points_layer = NULL, the point
#' assessment units data are not used when searching for missing raw features.
#' Default = NULL.
#'
#' @return The function print a message if there is no ATTAINS assessment unit
#' geometry.
# check for ATTAINS geometry
checkForATTAINSGeo <- function(
  points_layer = NULL,
  lines_layer = NULL,
  polygons_layer = NULL
) {
  if (is.null(lines_layer) & is.null(points_layer) & is.null(polygons_layer)) {
    message("No ATTAINS data associated with this Water Quality Portal data.")
  }
}

#' fetchWaterType
#'
#' Use Expert Query web services to create a crosswalk of assessment unit identifier
#' to water type.
#'
#' @param au_list A list of assessment units to fetch water types for.
#'
#' @param api_key Optional character string. An api key for Expert Query web
#' services. If not supplied, the default TADA api key will be used. For best
#' performance, it is recommended that users obtain and use their own api key.
#' Request an api key here: https://owapps.epa.gov/expertquery/api-documentation
#'
#' @return The function returns a data frame with an assessment unit/water type
#' crosswalk. If no water type matches are found, a message explaining this is
#' printed.
#'
# get water types
# get water type info using ATTAINS Expert Query
fetchWaterType <- function(au_list, api_key = NULL) {
  au_list <- unique(au_list)

  # split the au_list into chunks
  chunks <- split(au_list, ceiling(seq_along(unique(au_list)) / 20))

  # get default api_key if user does not supply one
  if (is.null(api_key)) {
    api_key <- .setEQKey()
  }

  # get water type
  wat_type <- function(chunk) {
    results <- spsUtil::quiet(rExpertQuery::EQ_AssessmentUnits(
      api_key = api_key,
      auid = chunk
    ))
  }

  results <- purrr::map_dfr(.x = chunks, .f = wat_type)

  results <- results |>
    dplyr::select(assessmentUnitId, waterType) |>
    dplyr::distinct() |>
    dplyr::rename(
      ATTAINS.AssessmentUnitIdentifier = assessmentUnitId,
      ATTAINS.WaterType = waterType
    )

  return(results)
}
