<<<<<<< Updated upstream
<<<<<<< Updated upstream
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
#
# square <- magick::image_read("vignettes/images/icons/square-solid-full.png")
#
# notsupport <- magick::image_fill(square, tada.pal[3], "+500+500")
#
# magick::image_write(notsupport, path = "vignettes/images/icons/square-ns.png")
#
# fullsupport <- magick::image_fill(square, tada.pal[4], "+500+500")
#
# magick::image_write(fullsupport, path = "vignettes/images/icons/square-fs.png")
#
# notassessed <- magick::image_fill(square, tada.pal[7], "+500+500")
#
# magick::image_write(notassessed, path = "vignettes/images/icons/square-na.png")
#
# outline.square <- magick::image_read("vignettes/images/icons/square-regular-full.png")
#
# catchment <- magick::image_fill(outline.square, "black", "+500+500")
#
# magick::image_write(catchment, path = "vignettes/images/icons/square-catchment.png")
#
# create images for mapping point AUs
# #
#   setupPointMarkers <- function(path, color, name) {
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

if(icons == TRUE) {
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

if(labels == FALSE) {

}
}

if(labels == TRUE) {
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

if(icons == FALSE) {

  return(img.labels)
}
}

  if(icons == TRUE & labels == TRUE) {

  map.list <- list(images, img.labels)

  return(map.list)
  }
}

=======
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
#' addATTAINS (UNDER ACTIVE DEVELOPMENT)
#'
#' Internal function to add ATTAINS lines, points, or polygons to TADA maps.
#'
#' @param .data The list of data frames created from TADA_CreateAUMLCrosswalk or
#' TADA_CreateATTAINSAUMLCrosswalk.
#'
#' @param geo_type Character string. Valid options are "line", "point", or
#' "polygon". This is required to run the function. If it is NULL, the function
#' will fail with an error message. Default is geo_type = NULL.
#'
#' @param overlay_groups Initialized vector to add names of groups added to map. If
#' it is NULL, the function will fail with an error message. Default is
#' overlay_list = NULL.
#'
#' @param group_name Character string. This is the layer name that will appear
#' to turn map layers on and off.
#'
#' @return ATTAINS geometry correctly formatted for display in a TADA leaflet map.
#'
# add ATTAINS geometry to existing leaflet map

addATTAINS <- function(.data,
                       map = NULL,
                       geo_type = NULL,
                       overlay_groups = NULL,
                       group_name = "ATTAINS catchments"
                       ) {

  # stop function if map is not provided
  if(is.null(map)) {
    stop("addATTAINS: a leaflet map must be supplied to run this function.")
  }

  # stop function if geometry type is not provided
  if(is.null(geo_type)) {
  stop("addATTAINS: geo_type must be supplied to run this function.")
}

# stop function if overlay list is not provided
  if(is.null(overlay_groups)) {
  stop("addATTAINS: overlay_groups must be supplied to run this function.")
  }

<<<<<<< Updated upstream
<<<<<<< Updated upstream
# select
=======
=======
>>>>>>> Stashed changes
# select required dfs for mapping

  df.list <- .data

  .data <- switch(group_name,
                  "ATTAINS catchments" = "black",
                  "ATTAINS outlines" = .data$col,
                  "missing ATTAINS catchment outlines" = "#d62728",
                  "ATTAINS polygon features" = "#7f7f7f"
  ))

  set.color <- switch(
    group_name,
    "ATTAINS catchments" = "black",
    "ATTAINS outlines" = .data$col,
    "missing ATTAINS catchment outlines" = "#d62728",
    "ATTAINS polygon features" = "#7f7f7f"
  )

<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes

# add additional check for structure of overlay list (NOTE: HRM 12/30/25)?

# Add ATTAINS polygons
  # options for adding ATTAINS polygons are: ATTAINS catchments, ATTAINS outlines,
  # missing ATTAINS catchment outlines, ATTAINS polygon features

  if(geo_type == "polygon") {

  if(!group_name %in% c("ATTAINS catchments",
                       "ATTAINS outlines",
                       "missing ATTAINS catchment outlines",
                       "ATTAINS polygon features")) {
    stop("addATTAINS: Supplied group name does not match allowable ATTAINS polygon
         group. group_name must be 'ATTAINS catchments', 'ATTAINS outlines',
         'missing ATTAINS catchment outlines' or 'ATTAINS polygon features'")
  }

    if(!group_name %in% c("missing ATTAINS catchment outlines",
                          "ATTAINS catchments")) {
      .data <-
    }

    #   # add without ATTAINS catchments if available
    #   without_ATTAINS_catchments <- NULL
    # try(
    #   without_ATTAINS_catchments <- .data[["without_ATTAINS_catchments"]] |>
    #     dplyr::rename(nhd = 1),
    #   silent = TRUE
    # )

    set.color <- switch(
      group_name,
      "ATTAINS catchments" = "black",
      "ATTAINS outlines" = .data$col,
      "missing ATTAINS catchment outlines" = "#d62728",
      "ATTAINS polygon features" = "#7f7f7f"
    )

    set.fillColor <- switch(
      group_name,
      "ATTAINS catchments" = "gray",
      "ATTAINS outlines" = .data$col,
      "missing ATTAINS catchment outlines" = "#d62728",
      "ATTAINS polygon features" = "#7f7f7f"
    )

    set.weight <- switch(
      group_name,
      "ATTAINS catchments" = 1,
      "ATTAINS outlines" = 3,
      "missing ATTAINS catchment outlines" = "#d62728",
      "ATTAINS polygon features" = "#7f7f7f"
    )

    set.fillOpacity <- switch(
      group_name,
      "ATTAINS catchments" = 0.3,
      "ATTAINS outlines" = 0.25,
      "missing ATTAINS catchment outlines" = "#d62728",
      "ATTAINS polygon features" = "#7f7f7f"
    )

    set.popup <- switch(
      group_name,
      "ATTAINS catchments" = paste0(
        "NHDPlus HR Catchment ID: ",
        .data$nhdplusid
      ),
      "ATTAINS outlines" = paste0(
        "Assessment Unit Name: ",
        missing_raw_mapper$assessmentunitname,
        "<br> Assessment Unit ID: ",
        missing_raw_mapper$assessmentunitidentifier,
        "<br> Status: ",
        missing_raw_mapper$overallstatus,
        "<br> Assessment Unit Type: ",
        missing_raw_mapper$type,
        "<br> <a href=",
        missing_raw_mapper$waterbodyreportlink,
        " target='_blank'>ATTAINS Link</a>",
        "<br> NHDPlus HR Catchment ID: ",
        missing_raw_mapper$nhdplusid
      ),
      "missing ATTAINS catchment outlines" = "#d62728",
      "ATTAINS polygon features" = "#7f7f7f"
    )

try({
  map <- map |>
    leaflet::addPolygons(
      data = .data,
      group = group_name,
      color = set.color,
      fillColor = set.fillColor,
      weight = set.weight,
      fillOpacity = set.fillOpacity,
      popup = set.popup
    )
  overlay_groups <- c(overlay_groups, "ATTAINS catchments")
},
silent = TRUE
)
}


addATTAINS(ATTAINS_catchments,
           map = orig.map,
           geo_type = "polygon",
           overlay_groups = overlay_groups)

# Add ATTAINS catchment outlines as AUs:
try({
  map <- map |>
    leaflet::addPolygons(
      data = missing_raw_mapper,
      group = "ATTAINS outlines",
      color = ~ missing_raw_mapper$col,
      fill = ~ missing_raw_mapper$col,
      weight = 3,
      fillOpacity = 0.25,
      popup = paste0(
        "Assessment Unit Name: ",
        missing_raw_mapper$assessmentunitname,
        "<br> Assessment Unit ID: ",
        missing_raw_mapper$assessmentunitidentifier,
        "<br> Status: ",
        missing_raw_mapper$overallstatus,
        "<br> Assessment Unit Type: ",
        missing_raw_mapper$type,
        "<br> <a href=",
        missing_raw_mapper$waterbodyreportlink,
        " target='_blank'>ATTAINS Link</a>",
        "<br> NHDPlus HR Catchment ID: ",
        missing_raw_mapper$nhdplusid
      )
    )
  overlay_groups <- c(overlay_groups, "ATTAINS outlines")
},
silent = TRUE
)

# add without ATTAINS catchments if available
without_ATTAINS_catchments <- NULL
try(
  without_ATTAINS_catchments <- .data[["without_ATTAINS_catchments"]] |>
    dplyr::rename(nhd = 1),
  silent = TRUE
)

# Add missing catchment outlines (if they exist):
try({
  map <- map |>
    leaflet::addPolygons(
      data = without_ATTAINS_catchments,
      group = "missing ATTAINS catchment outlines",
      color = "black",
      weight = 1,
      fillOpacity = 0,
      popup = paste0(
        without_ATTAINS_catchments$NHD.resolution,
        " catchment ID: ",
        without_ATTAINS_catchments$nhd
      )
    )
  overlay_groups <- c(
    overlay_groups,
    "missing ATTAINS catchment outlines"
  )
},
silent = TRUE
)

# Add ATTAINS polygon features (if they exist):
try({
  map <- map |>
    leaflet::addPolygons(
      data = polygons_mapper,
      group = "ATTAINS polygon features",
      color = ~ polygons_mapper$col,
      fill = ~ polygons_mapper$col,
      weight = 3,
      fillOpacity = 0.5,
      popup = paste0(
        "Assessment Unit Name: ",
        polygons_mapper$assessmentunitname,
        "<br> Assessment Unit ID: ",
        polygons_mapper$assessmentunitidentifier,
        "<br> Status: ",
        polygons_mapper$overallstatus,
        "<br> Assessment Unit Type: ",
        polygons_mapper$type,
        "<br> <a href=",
        polygons_mapper$waterbodyreportlink,
        " target='_blank'>ATTAINS Link</a>"
      )
    )
  overlay_groups <- c(overlay_groups, "ATTAINS polygon features")
},
silent = TRUE
)

# Add ATTAINS lines features (if they exist):
try({
  map <- map |>
    leaflet::addPolylines(
      data = lines_mapper,
      group = "ATTAINS line features",
      color = ~ lines_mapper$col,
      weight = 4,
      fillOpacity = 1,
      popup = paste0(
        "Assessment Unit Name: ",
        lines_mapper$assessmentunitname,
        "<br> Assessment Unit ID: ",
        lines_mapper$assessmentunitidentifier,
        "<br> Status: ",
        lines_mapper$overallstatus,
        "<br> Assessment Unit Type: ",
        lines_mapper$type,
        "<br> <a href=",
        lines_mapper$waterbodyreportlink,
        " target='_blank'>ATTAINS Link</a>"
      )
    )
  overlay_groups <- c(overlay_groups, "ATTAINS line features")
},
silent = TRUE
)

try(
  pointIcons <- leaflet::icons(
    iconUrl = dplyr::case_when(
      points_mapper$overallstatus == "Fully Supporting" ~ images[12],
      points_mapper$overallstatus == "Not Supporting" ~ images[11],
      points_mapper$overallstatus == "Not Assessed" ~ images[13]
    ),
    iconWidth = 48,
    iconHeight = 48
  ),
  silent = TRUE
)

# Add ATTAINS point features (if they exist):
try({
  map <- map |>
    leaflet::addMarkers(
      data = points_mapper,
      group = "ATTAINS point features",
      lng = ~X,
      lat = ~Y,
      icon = pointIcons,
      popup = paste0(
        "Assessment Unit Name: ",
        points_mapper$assessmentunitname,
        "<br> Assessment Unit ID: ",
        points_mapper$assessmentunitidentifier,
        "<br> Status: ",
        points_mapper$overallstatus,
        "<br> Assessment Unit Type: ",
        points_mapper$type,
        "<br> <a href=",
        points_mapper$waterbodyreportlink,
        " target='_blank'>ATTAINS Link</a>"
      )
    )
  overlay_groups <- c(overlay_groups, "ATTAINS point features")
},
silent = TRUE
)
}
