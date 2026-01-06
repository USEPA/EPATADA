#' getMapIconsLabels
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
getMapIconLabels <- function(icons = TRUE,
                             labels = TRUE) {
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

  # # create images for mapping point AUs
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
      system.file("extdata/icons", "circle-solid-full.png", package = "EPATADA"), # 7
      system.file("extdata/icons", "circle-solid-full.png", package = "EPATADA"), # 8
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

    if (icons == FALSE) {
      return(img.labels)
    }
  }

  if (icons == TRUE & labels == TRUE) {
    map.list <- list(images, img.labels)

    return(map.list)
  }
}

#'  addATTAINSAUs
#' Internal function to add ATTAINS assessment unit lines, points, or polygons to
#' TADA maps.
#'
#' @param .data A data frames created from prepATTAINSMapper contains a geometry
#' column.
#'
#' @param overlay_groups Initialized vector to add names of groups added to map. If
#' it is NULL, the function will fail with an error message. Default is overlay_list
#' = NULL.
#'
#' @param icons Character argument. The list of icon paths generated by the internal
#' function getMapIconLabels. If already called in a larger mapping function, it can
#' be referenced here (for efficiency). If icons = NULL, getMapIconLabels will run
#' and fetch the list. Default is icons = NULL. This argument is only applied to
#' for point AUs.
#'
#' @return ATTAINS geometry correctly formatted for display in a TADA leaflet map.
#'
# add ATTAINS geometry to existing leaflet map
  addATTAINSAUs <- function(.data,
                         map = NULL,
                         overlay_groups = NULL,
                         icons = NULL
                         ) {
    # stop function if map is not provided
    if(is.null(map)) {
      stop("addATTAINS: a leaflet map must be supplied to run this function.")
    }

    # stop function if overlay list is not provided
    if(is.null(overlay_groups)) {
    stop("addATTAINS: overlay_groups must be supplied to run this function.")
    }

    # get geometry type
    geo.type <- .data$type[1]

    # set group name
    group.name <- switch(geo.type,
                    "Point Feature" = "ATTAINS point features",
                    "Line Feature" = "ATTAINS line features",
                    "Polygon Feature" = "ATTAINS polygon features")

    # remove intermediate object
    rm(geo.type)

  # Add ATTAINS assessment units
    # polygon assessment units
    if(group.name == "ATTAINS polygon features") {
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

    # polygon assessment units
    if(group.name == "ATTAINS line features") {
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

    # point assessment units
    if(group.name == "ATTAINS point features") {

      if(is.null(icons)) {

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

    au.list <- list(map, overlay_groups)

    names(au.list) <- c('map', 'overlay_groups')

    # return map and list of overlay groups
    return(au.list)
  }


#' getATTAINSColorsRef
#'
#' Internal function to return a data framespecifying the color the feature should be
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
#' applied to a feature based on its overallstatus in ATTAAINS. Must contain the
#' columns overallstatus, col, dark_col, and priority. Can be created with the
#' internal function getATTAINSColorRef. If color_ref = NULL, the function will
#' run getATTAINSColorRef to create a color ref data frame.
#'
#' @param geo_type Character string. Type of geometry to be prepared for mapping.
#' Allowable values are "points", "lines", and "polygons". If no geo_type (geo_type
#' = NULL) is supplied the function will attempt to determine the type from the
#' "geometry" column. If geo_type is not supplied and cannot be determined, the
#' function will stop with an error.
#'
#' @return A data frame with the columns overallstatus, col, dark_col, and priority.
#'
# prep data for mapping with ATTAINS
prepATTAINSMapper <- function(.data,
                              geo_type = NULL,
                              color_ref = NULL) {
  # if geo_type is not provided, determine it from .data
  if (is.null(geo_type)) {
    # get geometry type
    check.type <- unique(as.character(sf::st_geometry_type(.data, by_geometry = TRUE)))

    # check length of check.type
    if (length(check.type) > 1) {
      # find base type if multi types are present
      base <- sub("^MULTI", "", check.type)

      check.type <- unique(base)

      if (length(check.type) > 2) {
        stop("prepATTAINSMapper: geometry column in .data must contain only one base geometry type.")
      }
    }

    # normalize geometry type
    geo_type <- dplyr::case_when(
      check.type %in% c("POINT", "MULTIPOINT") ~ "points",
      check.type %in% c("LINESTRING", "MULTILINESTRING") ~ "lines",
      check.type %in% c("POLYGON", "MULTIPOLYGON") ~ "polygons",
      # check.type %in% c() have to add for catchments raw feature unavail
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
      stop("prepATTAINSMapper: color_ref must contain the following columns: overallstatus, col, dark_col, priority.")
    }
  }

  # prep point data
  if (geo_type == "points") {
    # extract coordinates and convert to a tibble (to handle point or multipoint)
    coords <- sf::st_coordinates(.data) |>
      tibble::as_tibble() |>
      tibble::rowid_to_column(var = "index")

    # points mapper setup
    mapper <- .data |>
      dplyr::left_join(color_ref, by = "overallstatus") |>
      dplyr::mutate(type = "Point Feature") |>
      tibble::rowid_to_column(var = "index") |>
      dplyr::right_join(coords, by = "index")

    # remove intermediate objects
    rm(coords, color_ref)

    # return mapper df
    return(mapper)
  }

  # prep line data
  if (geo_type == "lines") {
    # add colors to line features
    mapper <- .data |>
      dplyr::left_join(color_ref, by = "overallstatus") |>
      dplyr::mutate(type = "Line Feature")

    return(mapper)
  }

  # prep polygon data
  if (geo_type == "polygons") {
    # add colors to polygon features
    mapper <- .data |>
      dplyr::left_join(color_ref, by = "overallstatus") |>
      dplyr::mutate(type = "Polygon Feature") |>
      # sort df so smaller AUs will map on top of larger AUs if they overlap
      dplyr::arrange(dplyr::desc(Shape_Area))

    return(mapper)
  }
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
getWQPSiteStats <- function(.data,
                            attains = TRUE) {
  if (attains == TRUE) {
    if (!"ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {
      attains <- FALSE

      print(paste0(
        "getWQPSiteStats: ATTAINS.AssessmentUnitIdentifier is not present in .data. ",
        "Returning WQP site stats without assessment unit identifiers."
      ))
    }

    if ("ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {

      if(!"TADA.AURefSource" %in% names(.data)) {

        .data <- .data |>
          dplyr::mutate(TADA.AURefSource = "not provided")
      }

      sumdat <- .data |>
        dplyr::group_by(
          TADA.MonitoringLocationIdentifier,
          TADA.MonitoringLocationName,
          OrganizationFormalName,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          TADA.AURefSource
        ) |>
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
  }

  if (attains == FALSE) {
    sumdat <- .data |>
      dplyr::group_by(
        TADA.MonitoringLocationIdentifier,
        TADA.MonitoringLocationName,
        OrganizationFormalName,
        TADA.LatitudeMeasure,
        TADA.LongitudeMeasure
      ) |>
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
#' @param .data Data frame. Must contain the columns TADA.LatitudeMeasure and
#' TADA.Longitude measure to set the extent of the map.
#'
#' @return A list containing elments to produce leaflet base map.
#'
# Create base map
createTADABasemap <- function(.data) {

  if(!all(c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure") %in% names(.data))) {

    stop("createTADABasemap: .data must contain TADA.LongitudeMeasure and TADA.LatitudeMeasure columns.")
  }

  bbox <- createBBox(.data, as_vector = TRUE)

  map <- leaflet::leaflet() |>
  leaflet::addProviderTiles(
    "Esri.WorldTopoMap",
    group = "World topo",
    options = leaflet::providerTileOptions(
      updateWhenZooming = FALSE,
      updateWhenIdle = TRUE
    )
  ) |>
  leaflet::clearShapes() |>
  leaflet::fitBounds(
    lng1 = bbox[1],
    lat1 = bbox[2],
    lng2 = bbox[3],
    lat2 = bbox[4]
  ) |>
  leaflet.extras::addResetMapButton()

  # remove intermediate objects
  rm(bbox)

  return(map)
}

#' createBBox
#'
#' Internal function to create bounding box for maps based on TADA.LatitudeMeasure
#' and TADA.LongitudeMeasure columns.
#'
#' @param .data Data frame. Must contain the columns TADA.LatitudeMeasure and
#' TADA.Longitude measure to set the extent of the map.
#'
#' @param as_vector Boolean argument. When as_vector = TRUE, the bounding box values
#' are returned as vector. When as_vector = FALSE, values are returned as a bounding
#' box. Default is as_vector = TRUE.
#'
#' @return A bounding box for use in leaflet mapping functions.
# Create bounding box
createBBox <- function(.data, as_vector = TRUE) {

  bbox <- sf::st_bbox(
    c(
      xmin = min(.data$TADA.LongitudeMeasure),
      ymin = min(.data$TADA.LatitudeMeasure),
      xmax = max(.data$TADA.LongitudeMeasure),
      ymax = max(.data$TADA.LatitudeMeasure)
    ),
    crs = sf::st_crs(.data)
  )

  if(as_vector == FALSE) {

    return(bbox)
  }

  if(as_vector == TRUE) {

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
#' AUs to.
#'
#' @param overlay_groups Initialized vector to add names of groups added to map. If
#' it is NULL, the function will fail with an error message. Default is overlay_list
#' = NULL.
#'
#' @return ATTAINS geometry correctly formatted for display in a TADA leaflet map.
#'
# add ATTAINS geometry to existing leaflet map
showMissingATTAINSAUs <- function(map = NULL,
                                  overlay_groups = NULL,
                                  ATTAINS_table = NULL,
                                  ATTAINS_points = NULL,
                                  ATTAINS_lines = NULL,
                                  ATTAINS_polygons = NULL) {

  # stop function if map is not provided
  if(is.null(map)) {
    stop("addATTAINS: a leaflet map must be supplied to run this function.")
  }

  # check for Monitoring Locations with assigned AUIDs that do not have geometry from ATTAINS
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

    # if any AUIDs were assigned by user check to see if they have matching geometry from ATTAINS

    if (dim(user.refs)[1] > 0) {
      # internal function to create list of auids
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
      all.attains.aus <- unique(Reduce(c, list(point.aus, line.aus, polygon.aus)))

      # find if any assigned aus are missing geometry
      missing.geo <- user.refs |>
        dplyr::filter(!ATTAINS.AssessmentUnitIdentifier %in% all.attains.aus)

      # remove intermediate objects
      rm(point.aus, line.aus, polygon.aus, all.attains.aus, user.refs)

      # if there are any user-assigned assesment unit identifiers without geometry in ATTAINS add to map
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


        # add missing AU symbology to map
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

            names(missing.list) <- c('map', 'overlay_groups')

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
#' @param icon_labels Character argument.
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
addWQPSites <- function(.data,
                        map = NULL,
                        icons = NULL,
                        icon_labels = NULL,
                        ref_icons = TRUE,
                        overlay_groups = NULL) {

  # data summary columns
  sum.cols <- c("Sample_Count",
                "Visit_Count",
                "Parameter_Count",
                "ATTAINS_AUs")

  # check to see if data summary columns are present in .data
  if(!all(sum.cols %in% names(.data))) {

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

# add au ref source to pop up  if available
if ("TADA.AURefSource" %in% names(.data)) {
  set.popup <- paste0(
    set.popup,
    "<br>",
    "Crosswalk Source: ",
    .data$TADA.AURefSource
  )
}

# check if icons are provided
if(is.null(icons)) {

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
  rm(icons, icon.labels)
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
    .data$TADA.AURefSource == "TADA_CreateATTAINSAUMLCrosswalk" ~ images[
      7
    ],
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

    names(wqp.list) <- c('map', 'overlay_groups')

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
#' @param .data A TADA data frame created with TADA_CreateATTAINSAUMLCrosswalk
#' or TADA_CreateAUMLCrosswalk (called "TADA_with_ATTAINS" in the list of output dfs)
#' or a subsetted TADA data frame containing all columns required for building map
#' and pop up (Note: Add list of required columns (HRM 1/5/26)). Needs to be in sumdat
#' format or getWQPSiteStats will be run.
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
#' ATTAINS assessment unit overall status are shown in the legend. Default
#'
#' @param attains_missing Boolean argument.
#'
#' @param nhd_attains Boolean argument.
#'
#' @param nhd_no_attains Boolean argument.

#' @return Custom leaflet legend for TADA maps.
#'
# add ATTAINS geometry to existing leaflet map
addTADAMapLegend <- function(map = NULL,
                             icons = NULL,
                             icon_labels = NULL,
                             wqp = TRUE,
                             ref_icons = TRUE,
                             attains_au = TRUE,
                             attains_missing = TRUE,
                             nhd_attains = TRUE,
                             nhd_no_attains = FALSE) {

  if(is.null(map)) {

    stop("addTADAMapLegend: Param map is missing. A leaflet map is required to add the legend.")
  }

  if(is.null(icons) | is.null(icon_labels)) {

    get.icons <- getMapIconLabels()

    images <- unlist(get.icons[1])

    img.labels <- unlist(get.icons[2])

    # remove intermediate objects
    rm(get.icons)
  }

  # create image and label ref lists for legend
  images.ref <- character(0)

  leg.labels <- character(0)

# set base image and label ref lists for each section of legend
  # ATTAINS assessment unit overall status
  if(attains_au == TRUE) {

    #attains.imgs <- images[1:3]

    #attains.labels <- img.labels[1:3]

    images.ref <- append(images.ref, images[1:3])

    leg.labels <- append(leg.labels, img.labels[1:3])

    rm(attains.imgs, attains.labels)
  }

  # ATTAINS missing geometry
  if(attains_missing == TRUE) {

    images.ref <- append(images.ref, images[4])

    leg.labels <- append(leg.labels, img.labels[4])
  }

  # add WQP icons to legend
  if(wqp == TRUE) {

    if(ref_icons == TRUE){
      images.ref <- append(images.ref, images[5:7])

      leg.labels <- append(leg.labels, img.labels[5:7])
    }

    if(ref_icons == FALSE){
      images.ref <- append(images.ref, images[8])

      leg.labels <- append(leg.labels, img.labels[8])
    }
  }

  # NHD catchments with ATTAINS features
  if(nhd_attains == TRUE) {

    images.ref <- append(images.ref, images[9])

    leg.labels <- append(leg.labels, img.labels[9])
  }

  # NHD catchments without ATTAINS features
  if(nhd_no_attains == TRUE) {

    images.ref <- append(images.ref, images[10])

    leg.labels <- append(leg.labels, img.labels[10])
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
addLegendToggle <- function(map = NULL) {
  if(is.null(map)) {
    stop("addLegendToggle: a TADA leaflet map must be specified in order to add the legend.")
  }

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
#' @param overlay_groups
#'
#' @return A TADA map with the layer control added.
#'
# add layer control
addLayerControl <- function(map = NULL,
                            overlay_groups = NULL) {

  if(is.null(map)) {
    stop("addLayerControl: a TADA leaflet map must be specified in order to add the legend.")
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

#' #' checkIconAndLabels
#' #'
#' #'  Internal function to check for the icon and icon_labels lists used in TADA
#' #'  mapping functions. If both are missing a message is printed indicating that
#' #'  getMapIconLabels will be run to create them. If only one is missing, this
#' #'  function can either (1) print a message indicating which param is missing
#' #'  and run getMapIconLabels to create both icon and icon parm lists or (2) stop
#' #'  the function with an error message identifying which param is missing. This
#' #'  is determined by the "fail" param. When fail = FALSE, a message is printed
#' #'  and getMapIconLabels is run. When fail = TRUE, the function stops if either
#' #'  icons or icon_labels is missing.
#' #'
#' #' @param fail Boolean argument. When fail = FALSE, a message is printed
#' #' and getMapIconLabels is run. When fail = TRUE, the function stops if either
#' #' icons or icon_labels is missing. Default is fail = FALSE.
#' #'
#' #' @param icons
#' #'
#' #' @param icon_labels
#' #'
#' #' @return Printed message to display status of icons and icon_labels params.
#' #'
#' # check icons and icon_labels
#' checkIconAndLabels <- function(fail = FALSE,
#'                                icons = NULL,
#'                                icon_labels = NULL) {
#'
#'   # check is both icons and icon_labels are not null
#'   if(!is.null(icons) & !is.null(icon_labels)) {
#'
#'     # print message confirming both params are not null
#'     print("checkIconAndLabels: Both icons and icon_labels were provided")
#'   }
#'
#'   if((is.null(icons) & is.null(icon_labels)) |
#'      (!is.null(icons) & is.null(icon_labels)) |
#'      (is.null(icons) & !is.null(icon_labels))) {
#'
#'     param.miss <- dplyr::case_when(
#'       is.null(icons) & is.null(icon_labels) ~ "The icons and icon_labels params were both NULL."
#'       !is.null(icons) & is.null(icon_labels) ~ "The icon_labels param was NULL."
#'       is.null(icons) & !is.null(icon_labels) ~ "The icons param was NULL."
#'     )
#'
#'     if(fail == TRUE) {
#'
#'       stop(paste0("checkIconAndLabels: ",
#'                   param.miss,
#'                   " Provide icon and icon_labels or set fail = FALSE to use getMapIconLabels."))
#'
#'     }
#'
#'     if(fail == FALSE) {
#'
#'       print(paste0("checkIconAndLabels: ",
#'                         param.miss,
#'                         " TADA icons and icon_llabels will be fetched with getMapIconLabels.")))
#'     }
#'   }
#'
#'     # get icons if not provided
#'     get.icons <- getMapIconLabels()
#'
#'     # list of icon image paths
#'     images <- unlist(get.icons[1])
#'
#'     # list of icon image labels
#'     img.labels <- unlist(get.icons[2])
#'
#'     # remove intermediate objects
#'     rm(get.icons)
#'   } else {
#'
#'     images <- icons
#'
#'     img.labels <- icon_labels
#'
#'     # remove intermediate objects
#'     rm(icons, icon.labels)
#'   }
#'
#'
#'
#' }
#'
#'
#'
