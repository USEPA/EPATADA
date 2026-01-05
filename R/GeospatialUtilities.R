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

#' #' addATTAINS (UNDER ACTIVE DEVELOPMENT)
#' #'
#' #' Internal function to add ATTAINS lines, points, or polygons to TADA maps.
#' #'
#' #' @param .data The list of data frames created from TADA_CreateAUMLCrosswalk or
#' #' TADA_CreateATTAINSAUMLCrosswalk.
#' #'
#' #' @param geo_type Character string. Valid options are "line", "point", or
#' #' "polygon". This is required to run the function. If it is NULL, the function
#' #' will fail with an error message. Default is geo_type = NULL.
#' #'
#' #' @param overlay_groups Initialized vector to add names of groups added to map. If
#' #' it is NULL, the function will fail with an error message. Default is
#' #' overlay_list = NULL.
#' #'
#' #' @param group_name Character string. This is the layer name that will appear
#' #' to turn map layers on and off.
#' #'
#' #' @param color Color of line
#' #'
#' #' @param fillColor Fill color for polygons
#' #'
#' #' @param opacity
#' #'
#' #' @param weight
#' #'
#' #' @return ATTAINS geometry correctly formatted for display in a TADA leaflet map.
#' #'
#' # add ATTAINS geometry to existing leaflet map
#' addATTAINS <- function(.data,
#'                         map = NULL,
#'                         geo_type = NULL,
#'                         overlay_groups = NULL,
#'                         group_name = "ATTAINS catchments",
#'                         color = "black",
#'                         fillColor = "gray",
#'                         opacity = 0.3,
#'                         weight = 1
#'                         ) {
#'    # stop function if map is not provided
#'    if(is.null(map)) {
#'      stop("addATTAINS: a leaflet map must be supplied to run this function.")
#'    }
#'
#'    # stop function if geometry type is not provided
#'    if(is.null(geo_type)) {
#'    stop("addATTAINS: geo_type must be supplied to run this function.")
#'  }
#'
#'    # stop function if overlay list is not provided
#'    if(is.null(overlay_groups)) {
#'    stop("addATTAINS: overlay_groups must be supplied to run this function.")
#'    }
#'
#' # select required dfs for mapping
#'
#'    df.list <- .data
#'
#'    .data <- switch(group_name,
#'                    "ATTAINS catchments" = "black",
#'                    "ATTAINS outlines" = .data$col,
#'                    "missing ATTAINS catchment outlines" = "#d62728",
#'                    "ATTAINS polygon features" = "#7f7f7f"
#'    ))
#'
#'    set.color <- switch(
#'      group_name,
#'      "ATTAINS catchments" = "black",
#'      "ATTAINS outlines" = .data$col,
#'      "missing ATTAINS catchment outlines" = "#d62728",
#'      "ATTAINS polygon features" = "#7f7f7f"
#'    )
#'
#' # add additional check for structure of overlay list (NOTE: HRM 12/30/25)?
#'
#' # Add ATTAINS polygons
#'    # options for adding ATTAINS polygons are: ATTAINS catchments, ATTAINS outlines,
#'    # missing ATTAINS catchment outlines, ATTAINS polygon features
#'
#'    if(geo_type == "polygon") {
#'
#'    if(!group_name %in% c("ATTAINS catchments",
#'                         "ATTAINS outlines",
#'                         "missing ATTAINS catchment outlines",
#'                         "ATTAINS polygon features")) {
#'      stop("addATTAINS: Supplied group name does not match allowable ATTAINS polygon
#'           group. group_name must be 'ATTAINS catchments', 'ATTAINS outlines',
#'           'missing ATTAINS catchment outlines' or 'ATTAINS polygon features'")
#'    }
#'
#'      if(!group_name %in% c("missing ATTAINS catchment outlines",
#'                            "ATTAINS catchments")) {
#'        .data <-
#'      }
#'
#'          add without ATTAINS catchments if available
#'        without_ATTAINS_catchments <- NULL
#'      try(
#'        without_ATTAINS_catchments <- .data[["without_ATTAINS_catchments"]] |>
#'          dplyr::rename(nhd = 1),
#'         silent = TRUE
#'       )
#'
#'      set.color <- switch(
#'        group_name,
#'        "ATTAINS catchments" = "black",
#'        "ATTAINS outlines" = .data$col,
#'        "missing ATTAINS catchment outlines" = "#d62728",
#'        "ATTAINS polygon features" = "#7f7f7f"
#'      )
#'
#'      set.fillColor <- switch(
#'        group_name,
#'        "ATTAINS catchments" = "gray",
#'        "ATTAINS outlines" = .data$col,
#'        "missing ATTAINS catchment outlines" = "#d62728",
#'        "ATTAINS polygon features" = "#7f7f7f"
#'      )
#'
#'      set.weight <- switch(
#'        group_name,
#'        "ATTAINS catchments" = 1,
#'        "ATTAINS outlines" = 3,
#'        "missing ATTAINS catchment outlines" = "#d62728",
#'        "ATTAINS polygon features" = "#7f7f7f"
#'      )
#'
#'      set.fillOpacity <- switch(
#'        group_name,
#'        "ATTAINS catchments" = 0.3,
#'        "ATTAINS outlines" = 0.25,
#'        "missing ATTAINS catchment outlines" = "#d62728",
#'        "ATTAINS polygon features" = "#7f7f7f"
#'      )
#'
#'      set.popup <- switch(
#'        group_name,
#'        "ATTAINS catchments" = paste0(
#'          "NHDPlus HR Catchment ID: ",
#'          .data$nhdplusid
#'        ),
#'        "ATTAINS outlines" = paste0(
#'          "Assessment Unit Name: ",
#'          missing_raw_mapper$assessmentunitname,
#'          "<br> Assessment Unit ID: ",
#'          missing_raw_mapper$assessmentunitidentifier,
#'          "<br> Status: ",
#'          missing_raw_mapper$overallstatus,
#'          "<br> Assessment Unit Type: ",
#'          missing_raw_mapper$type,
#'          "<br> <a href=",
#'          missing_raw_mapper$waterbodyreportlink,
#'          " target='_blank'>ATTAINS Link</a>",
#'          "<br> NHDPlus HR Catchment ID: ",
#'          missing_raw_mapper$nhdplusid
#'        ),
#'        "missing ATTAINS catchment outlines" = "#d62728",
#'        "ATTAINS polygon features" = "#7f7f7f"
#'      )
#'
#'  try({
#'    map <- map |>
#'      leaflet::addPolygons(
#'        data = .data,
#'        group = group_name,
#'        color = set.color,
#'        fillColor = set.fillColor,
#'        weight = set.weight,
#'        fillOpacity = set.fillOpacity,
#'        popup = set.popup
#'      )
#'    overlay_groups <- c(overlay_groups, "ATTAINS catchments")
#'  },
#'  silent = TRUE
#'  )
#'  }
#'
#'
#'  addATTAINS(ATTAINS_catchments,
#'             map = orig.map,
#'             geo_type = "polygon",
#'             overlay_groups = overlay_groups)
#'
#'  # Add ATTAINS catchment outlines as AUs:
#'  try({
#'    map <- map |>
#'      leaflet::addPolygons(
#'        data = missing_raw_mapper,
#'        group = "ATTAINS outlines",
#'        color = ~ missing_raw_mapper$col,
#'        fill = ~ missing_raw_mapper$col,
#'        weight = 3,
#'        fillOpacity = 0.25,
#'        popup = paste0(
#'          "Assessment Unit Name: ",
#'          missing_raw_mapper$assessmentunitname,
#'          "<br> Assessment Unit ID: ",
#'          missing_raw_mapper$assessmentunitidentifier,
#'          "<br> Status: ",
#'          missing_raw_mapper$overallstatus,
#'          "<br> Assessment Unit Type: ",
#'          missing_raw_mapper$type,
#'          "<br> <a href=",
#'          missing_raw_mapper$waterbodyreportlink,
#'          " target='_blank'>ATTAINS Link</a>",
#'          "<br> NHDPlus HR Catchment ID: ",
#'          missing_raw_mapper$nhdplusid
#'        )
#'      )
#'    overlay_groups <- c(overlay_groups, "ATTAINS outlines")
#'  },
#'  silent = TRUE
#'  )
#'
#'  # add without ATTAINS catchments if available
#'  without_ATTAINS_catchments <- NULL
#'  try(
#'    without_ATTAINS_catchments <- .data[["without_ATTAINS_catchments"]] |>
#'      dplyr::rename(nhd = 1),
#'    silent = TRUE
#'  )
#'
#'  # Add missing catchment outlines (if they exist):
#'  try({
#'    map <- map |>
#'      leaflet::addPolygons(
#'        data = without_ATTAINS_catchments,
#'        group = "missing ATTAINS catchment outlines",
#'        color = "black",
#'        weight = 1,
#'        fillOpacity = 0,
#'        popup = paste0(
#'          without_ATTAINS_catchments$NHD.resolution,
#'          " catchment ID: ",
#'          without_ATTAINS_catchments$nhd
#'        )
#'      )
#'    overlay_groups <- c(
#'      overlay_groups,
#'      "missing ATTAINS catchment outlines"
#'    )
#'  },
#'  silent = TRUE
#'  )
#'
#'  # Add ATTAINS polygon features (if they exist):
#'  try({
#'    map <- map |>
#'      leaflet::addPolygons(
#'        data = polygons_mapper,
#'        group = "ATTAINS polygon features",
#'        color = ~ polygons_mapper$col,
#'        fill = ~ polygons_mapper$col,
#'        weight = 3,
#'        fillOpacity = 0.5,
#'        popup = paste0(
#'          "Assessment Unit Name: ",
#'          polygons_mapper$assessmentunitname,
#'          "<br> Assessment Unit ID: ",
#'          polygons_mapper$assessmentunitidentifier,
#'          "<br> Status: ",
#'          polygons_mapper$overallstatus,
#'          "<br> Assessment Unit Type: ",
#'          polygons_mapper$type,
#'          "<br> <a href=",
#'          polygons_mapper$waterbodyreportlink,
#'          " target='_blank'>ATTAINS Link</a>"
#'        )
#'      )
#'    overlay_groups <- c(overlay_groups, "ATTAINS polygon features")
#'  },
#'  silent = TRUE
#'  )
#'
#' # Add ATTAINS lines features (if they exist):
#' try({
#'    map <- map |>
#'      leaflet::addPolylines(
#'        data = lines_mapper,
#'        group = "ATTAINS line features",
#'        color = ~ lines_mapper$col,
#'        weight = 4,
#'        fillOpacity = 1,
#'        popup = paste0(
#'          "Assessment Unit Name: ",
#'          lines_mapper$assessmentunitname,
#'          "<br> Assessment Unit ID: ",
#'          lines_mapper$assessmentunitidentifier,
#'          "<br> Status: ",
#'          lines_mapper$overallstatus,
#'          "<br> Assessment Unit Type: ",
#'          lines_mapper$type,
#'          "<br> <a href=",
#'          lines_mapper$waterbodyreportlink,
#'          " target='_blank'>ATTAINS Link</a>"
#'        )
#'      )
#'    overlay_groups <- c(overlay_groups, "ATTAINS line features")
#'  },
#'  silent = TRUE
#'  )
#'
#'  try(
#'    pointIcons <- leaflet::icons(
#'      iconUrl = dplyr::case_when(
#'        points_mapper$overallstatus == "Fully Supporting" ~ images[12],
#'        points_mapper$overallstatus == "Not Supporting" ~ images[11],
#'        points_mapper$overallstatus == "Not Assessed" ~ images[13]
#'      ),
#'      iconWidth = 48,
#'      iconHeight = 48
#'    ),
#'    silent = TRUE
#'  )
#'
#'  # Add ATTAINS point features (if they exist):
#'  try({
#'    map <- map |>
#'      leaflet::addMarkers(
#'        data = points_mapper,
#'        group = "ATTAINS point features",
#'        lng = ~X,
#'        lat = ~Y,
#'        icon = pointIcons,
#'        popup = paste0(
#'          "Assessment Unit Name: ",
#'          points_mapper$assessmentunitname,
#'          "<br> Assessment Unit ID: ",
#'          points_mapper$assessmentunitidentifier,
#'          "<br> Status: ",
#'          points_mapper$overallstatus,
#'          "<br> Assessment Unit Type: ",
#'          points_mapper$type,
#'          "<br> <a href=",
#'          points_mapper$waterbodyreportlink,
#'          " target='_blank'>ATTAINS Link</a>"
#'        )
#'      )
#'    overlay_groups <- c(overlay_groups, "ATTAINS point features")
#'  },
#'  silent = TRUE
#'  )
#'  }
#'

#' getATTAINSColorsRef
#'
#' Internal function to return a data framespecifying the color the feature should be
#' displayed in for a leaflet map based on the value in the "overallstatus" column
#' in the ATTAINS_points, ATTAINS_polygons, or ATTAINS_lines data frames created
#' with TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk.
#'
#' @return A data frame with the columns overallstatus, col, dark_col, and priority.
#'
# create icon and label lists
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
          Organization_Count = length(unique(OrganizationIdentifier)),
          ATTAINS_AUs = as.character(list(unique(
            ATTAINS.AssessmentUnitIdentifier
          ))),
          TADA.AURefSource = ifelse(
            "TADA.AURefSource" %in% names(.data),
            as.character(TADA.AURefSource),
            "not provided"
          )
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
