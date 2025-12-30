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

# select

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
