#' Create Overview Map
#'
#' @param .data TADA dataframe containing the data downloaded from the WQP, where
#' each row represents a unique data record. Dataframe must include the columns
#' 'MonitoringLocationIdentifier','MonitoringLocationName','TADA.LatitudeMeasure',
#' 'TADA.LongitudeMeasure', 'ResultIdentifier', 'ActivityStartDate', 'TADA.CharacteristicName',
#' and 'OrganizationIdentifier' to run this function.
#'
#' @return A leaflet map that shows all sites in the dataframe, where larger point sizes
#' indicate more results collected at a site, and darker point colors indicate more
#' characteristics measured at that site. Users can click on points on the map to see
#' a pop-up window with exact counts for measurements (i.e. number of rows),
#' visits (number of unique Activity ID's), and characteristics associated with each site.
#'
#' @export
#'
#' @examples
#' utils::data(Data_Nutrients_UT)
#' utils::data(Data_6Tribes_5y_Harmonized)
#'
#' # Create maps:
#' TADA_OverviewMap(Data_Nutrients_UT)
#' TADA_OverviewMap(Data_6Tribes_5y_Harmonized)
#'
TADA_OverviewMap <- function(.data) {
  suppressMessages(suppressWarnings({
    quiet({
      addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5) {
        colorAdditions <- paste0(
          colors,
          "; border-radius: 50%; width:",
          sizes,
          "px; height:",
          sizes,
          "px"
        )
        labelAdditions <- paste0(
          "<div style='display: inline-block;height: ",
          sizes,
          "px;margin-top: 4px;line-height: ",
          sizes,
          "px;'>",
          labels,
          "</div>"
        )
        return(leaflet::addLegend(
          map,
          colors = colorAdditions,
          labels = labelAdditions,
          opacity = opacity,
          title = "Measurements"
        ))
      }
      sumdat <- .data |>
        dplyr::group_by(
          MonitoringLocationIdentifier,
          MonitoringLocationName,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          OrganizationFormalName,
        ) |>
        dplyr::summarise(
          "Sample_Count" = length(unique(ResultIdentifier)),
          "Visit_Count" = length(unique(ActivityStartDate)),
          "Parameter_Count" = length(unique(TADA.CharacteristicName))
        )
      param_counts <- sort(unique(sumdat$Parameter_Count))
      param_length <- length(param_counts)
      param_diff <- diff(param_counts)
      pt_sizes <- round(
        stats::quantile(sumdat$Sample_Count, probs = c(0.1, 0.25, 0.5, 0.75)),
        0
      )
      pt_labels <- c(
        paste0("<=", pt_sizes[1]),
        paste0(">", pt_sizes[1]),
        paste0(">", pt_sizes[2]),
        paste0(">", pt_sizes[3]),
        paste0(">", pt_sizes[4])
      )
      sumdat$radius <- 5
      sumdat$radius <- ifelse(
        sumdat$Sample_Count > pt_sizes[1],
        10,
        sumdat$radius
      )
      sumdat$radius <- ifelse(
        sumdat$Sample_Count > pt_sizes[2],
        15,
        sumdat$radius
      )
      sumdat$radius <- ifelse(
        sumdat$Sample_Count > pt_sizes[3],
        20,
        sumdat$radius
      )
      sumdat$radius <- ifelse(
        sumdat$Sample_Count > pt_sizes[4],
        30,
        sumdat$radius
      )
      site_size <- data.frame(
        Sample_n = pt_labels,
        Point_size = c(5, 10, 15, 20, 30)
      )
      site_legend <- subset(
        site_size,
        site_size$Point_size %in% unique(sumdat$radius)
      )
      # set breaks to occur only at integers for data sets requiring bins
      pretty.breaks <- unique(round(pretty(sumdat$Parameter_Count)))
      bins_n <- length(pretty.breaks)
      # create TADA color palette
      tada.pal <- TADA_ColorPalette()
      start.rgb.val <- col2rgb(tada.pal[5]) / 255
      new.rgb.start <- start.rgb.val * (1 - 0.7) + 1 * 0.7
      start.color <- rgb(new.rgb.start[1], new.rgb.start[2], new.rgb.start[3])
      end.rgb.val <- col2rgb(tada.pal[10]) / 255
      new.rgb.end <- end.rgb.val * (1 - 0.4)
      end.color <- rgb(new.rgb.end[1], new.rgb.end[2], new.rgb.end[3])
      tada.blues <- grDevices::colorRampPalette(c(start.color, end.color))(
        bins_n
      )
      # set color palette
      # set color palette for small number of characteristics (even intervals, no bins)
      if (length(unique(param_diff)) == 1 & param_length < 10) {
        pal <- leaflet::colorFactor(palette = tada.blues, levels = param_counts)
      } else if (length(unique(param_counts)) == 1) {
        pal <- "orange"
      } else {
        pal <- leaflet::colorBin(palette = tada.blues, bins = pretty.breaks)
      }
      # create custom fill color function so that data sets with one value for parameter count are displayed correctly
      customFillColor <- function(category, pal) {
        if (length(param_diff > 0)) {
          return(pal(category))
        } else {
          return(tada.pal[5])
        }
      }
      # Tribal layers will load by default in the overview map, restricted by the bounding box of the current dataset
      # They can be toggled on and off using a button (all layers work together and can't be turned on/off individually).
      # Colors and icons are as discussed previously (orange/tan colors and open triangle icons for points) but can be changed to match HMW if desired.
      bbox <- sf::st_bbox(
        c(
          xmin = min(sumdat$TADA.LongitudeMeasure),
          ymin = min(sumdat$TADA.LatitudeMeasure),
          xmax = max(sumdat$TADA.LongitudeMeasure),
          ymax = max(sumdat$TADA.LatitudeMeasure)
        ),
        crs = sf::st_crs(sumdat)
      )
      vbbox <- bbox |> as.vector()
      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          "Esri.WorldTopoMap",
          group = "World topo",
          options = leaflet::providerTileOptions(
            updateWhenZooming = FALSE,
            updateWhenIdle = TRUE
          )
        ) |>
        leaflet::clearShapes() |> # get rid of whatever was there before if loading a second dataset
        leaflet::fitBounds(
          lng1 = vbbox[1],
          lat1 = vbbox[2],
          lng2 = vbbox[3],
          lat2 = vbbox[4]
        ) |> # fit to bounds of data in tadat$raw
        leaflet.extras::addResetMapButton() |> # button to reset to initial zoom and lat/long
        leaflet::addMapPane("featurelayers", zIndex = 300) |>
        leaflet::addCircleMarkers(
          data = sumdat,
          lng = ~TADA.LongitudeMeasure,
          lat = ~TADA.LatitudeMeasure,
          # sets color of monitoring site circles
          color = as.character(tada.pal[10]),
          fillColor = customFillColor(sumdat$Parameter_Count, pal),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1.5,
          radius = sumdat$radius,
          popup = paste0(
            "Site ID: ",
            sumdat$MonitoringLocationIdentifier,
            "<br> Site Name: ",
            sumdat$MonitoringLocationName,
            "<br> Organization Name: ",
            sumdat$OrganizationFormalName,
            "<br> Measurement Count: ",
            sumdat$Sample_Count,
            "<br> Visit Count: ",
            sumdat$Visit_Count,
            "<br> Characteristic Count: ",
            sumdat$Parameter_Count
          )
        ) |>
        addLegendCustom(
          colors = "black",
          labels = site_legend$Sample_n,
          sizes = site_legend$Point_size * 2
        )
      # create conditional map legend
      # create legend for single parameter count value data sets
      if (length(param_diff) == 0) {
        map <- map |>
          leaflet::addLegend(
            "bottomright",
            color = tada.pal[5],
            labels = param_counts,
            title = "Characteristics",
            opacity = 0.5
          )
      }
      # create legend for data sets with multiple factors/bins for parameter count
      if (length(param_diff) > 0) {
        map <- map |>
          leaflet::addLegend(
            "bottomright",
            pal = pal,
            values = sumdat$Parameter_Count,
            title = "Characteristics",
            opacity = 0.5
          )
      }
      # TADA_addPolys and TADA_addPoints are in Utilities.R
      map <- TADA_addPolys(
        map,
        "extdata/AKAllotments.shp",
        "Tribes",
        "Alaska Allotments",
        bbox
      )
      map <- TADA_addPolys(
        map,
        "extdata/AmericanIndian.shp",
        "Tribes",
        "American Indian",
        bbox
      )
      map <- TADA_addPolys(
        map,
        "extdata/OffReservation.shp",
        "Tribes",
        "Off Reservation",
        bbox
      )
      map <- TADA_addPolys(
        map,
        "extdata/OKTribe.shp",
        "Tribes",
        "Oklahoma Tribe",
        bbox
      )
      map <- TADA_addPoints(
        map,
        "extdata/AKVillages.shp",
        "Tribes",
        "Alaska Native Villages",
        bbox
      )
      map <- TADA_addPoints(
        map,
        "extdata/VATribe.shp",
        "Tribes",
        "Virginia Tribe",
        bbox
      )
      map <- leaflet::addLayersControl(
        map,
        overlayGroups = c("Tribes"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
      return(map)
    })
  }))
}

#' Create Flagged Sites Map
#'
#' @param .data TADA dataframe containing the data downloaded from the WQP, where
#' each row represents a unique data record. Dataframe must include the columns
#' 'MonitoringLocationIdentifier','MonitoringLocationName','TADA.LatitudeMeasure',
#' and 'TADA.LongitudeMeasure' to run this function.
#'
#' @return A leaflet map that shows all sites in the dataframe that contain
#' flagged data in the form of:
#' 1) imprecise coordinates - latitudes and/or longitudes that contain fewer
#'    then 3 decimal places.
#' 2) outside USA - coordinates that fall outside the bounds of the USA.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example dataframe:
#' utils::data(Data_Nutrients_UT)
#' utils::data(Data_6Tribes_5y_Harmonized)
#'
#' # Create maps:
#' TADA_FlaggedSitesMap(Data_Nutrients_UT)
#' TADA_FlaggedSitesMap(Data_6Tribes_5y_Harmonized)
#' }
#'
TADA_FlaggedSitesMap <- function(.data) {
  invalid <- TADA_FlagCoordinates(.data, flaggedonly = TRUE)
  lowres <- invalid[
    invalid$TADA.SuspectCoordinates.Flag == "Imprecise_lessthan3decimaldigits",
  ]
  outsideusa <- invalid[
    invalid$TADA.SuspectCoordinates.Flag %in%
      c("LAT_OutsideUSA", "LONG_OutsideUSA"),
  ]
  lowresIcon <- leaflet::makeAwesomeIcon(
    icon = "circle",
    library = "fa",
    iconColor = "#ffffff",
    markerColor = "green"
  )
  outsideIcon <- leaflet::makeAwesomeIcon(
    icon = "circle",
    library = "fa",
    iconColor = "#ffffff",
    markerColor = "darkblue"
  )
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(
      "Esri.WorldTopoMap",
      group = "World topo",
      options = leaflet::providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      )
    ) |>
    leaflet.extras::addResetMapButton() # button to reset to initial zoom and lat/long
  if (nrow(outsideusa) > 0) {
    map <- map |>
      leaflet::addAwesomeMarkers(
        ~TADA.LongitudeMeasure,
        ~TADA.LatitudeMeasure,
        icon = outsideIcon,
        # label = ~as.character(MonitoringLocationIdentifier),
        popup = paste0(
          "Site ID: ",
          outsideusa$MonitoringLocationIdentifier,
          "<br> Site Name: ",
          outsideusa$MonitoringLocationName,
          "<br> Organization Name: ",
          outsideusa$OrganizationFormalName,
          "<br> Latitude: ",
          outsideusa$TADA.LatitudeMeasure,
          "<br> Longitude: ",
          outsideusa$TADA.LongitudeMeasure
        ),
        data = outsideusa
      )
  }
  if (nrow(lowres) > 0) {
    map <- map |>
      leaflet::addAwesomeMarkers(
        ~TADA.LongitudeMeasure,
        ~TADA.LatitudeMeasure,
        icon = lowresIcon,
        # label = ~as.character(MonitoringLocationIdentifier),
        popup = paste0(
          "Site ID: ",
          lowres$MonitoringLocationIdentifier,
          "<br> Site Name: ",
          lowres$MonitoringLocationName,
          "<br> Organization Name: ",
          lowres$OrganizationFormalName,
          "<br> Latitude: ",
          lowres$TADA.LatitudeMeasure,
          "<br> Longitude: ",
          lowres$TADA.LongitudeMeasure
        ),
        data = lowres
      )
  }
  return(map)
}

#' Create Nearby Sites Map
#'
#' @param .data TADA dataframe after running TADA.FindNearbySites.
#' @param dist_buffer Distance in m to show a radius around each site marker.
#'
#'
#' @return A leaflet map that shows all sites in the dataframe that contain
#' flagged data in the form of near other sites - groups of sites that are spatially located within
#'    a threshold distance (defaulting to 100 m) from each other and within the same catchment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example dataframe:
#' utils::data(Data_Nutrients_UT)
#'
#'
#' # Create maps:
#' TADA_FlaggedSitesMap(Data_Nutrients_UT)
#' TADA_FlaggedSitesMap(Data_6Tribes_5y_Harmonized)
#' }
#'
TADA_NearbySitesMap <- function(.data, dist_buffer = 100) {
  if (c("TADA.NearbySiteGroup") %in% colnames(.data) == FALSE) {
    .data <- TADA_FindNearbySites(.data)
  }
  .data <- .data |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::mutate(
      LatitudeMeasure = as.numeric(LatitudeMeasure),
      LongitudeMeasure = as.numeric(LongitudeMeasure)
    ) |>
    dplyr::select(
      LongitudeMeasure,
      LatitudeMeasure,
      TADA.MonitoringLocationIdentifier,
      MonitoringLocationIdentifier,
      MonitoringLocationName,
      TADA.LatitudeMeasure,
      TADA.LongitudeMeasure,
      OrganizationIdentifier,
      OrganizationFormalName,
      TADA.NearbySiteGroup
    ) |>
    dplyr::distinct()
  icon.colors <- grDevices::rainbow(as.numeric(length(unique(
    .data$TADA.NearbySiteGroup
  ))))
  pal <- leaflet::colorFactor(
    palette = icon.colors,
    domain = .data$TADA.NearbySiteGroup
  )
  map <- leaflet::leaflet(.data) |>
    leaflet::addProviderTiles(
      "Esri.WorldTopoMap",
      group = "World topo",
      options = leaflet::providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      )
    ) |>
    leaflet.extras::addResetMapButton() # button to reset to initial zoom and lat/long
  if (nrow(.data) > 0) {
    map <- map |>
      leaflet::addCircleMarkers(
        ~LongitudeMeasure,
        ~LatitudeMeasure,
        color = ~ pal(TADA.NearbySiteGroup),
        opacity = 1,
        fillColor = ~ pal(TADA.NearbySiteGroup),
        fillOpacity = 1,
        radius = ifelse(dist_buffer > 200, dist_buffer / 10, 20),
        weight = 1,
        # label = ~as.character(TADA.MonitoringLocationIdentifier),
        popup = paste0(
          "Nearby Group Name: ",
          .data$TADA.MonitoringLocationIdentifier,
          "<br> Nearby Site Group: ",
          .data$TADA.NearbySiteGroup,
          "<br> Site ID: ",
          .data$MonitoringLocationIdentifier,
          "<br> Site Name: ",
          .data$MonitoringLocationName,
          "<br> Organization Name: ",
          .data$OrganizationFormalName,
          "<br> Latitude: ",
          .data$LatitudeMeasure,
          "<br> Longitude: ",
          .data$LongitudeMeasure
        ),
        data = .data,
        clusterOptions = leaflet::markerClusterOptions(),
      ) |>
      leaflet::addCircles(
        ~LongitudeMeasure,
        ~LatitudeMeasure,
        color = ~ pal(TADA.NearbySiteGroup),
        opacity = 0.1,
        fillColor = ~ pal(TADA.NearbySiteGroup),
        fillOpacity = 0.1,
        radius = dist_buffer,
        weight = 1
      )
  }
  return(map)
}


#' TADA_ViewATTAINS
#'
#' This function is designed to visualize the data included in the list returned
#' from TADA_CreateAUMLCrosswalk. The map can be used to review different
#' crosswalk sources used for the assignment of WQP Monitoring Locations to
#' ATTAINS Assessment Units. Please check out the TADAModule2.Rmd for an example workflow.
#'
#' @param .data [TADA_DataRetrieval()] and [TADA_CreateAUMLCrosswalk()] can be run
#' to get a list containing WQP monitoring locations and ATTAINS shapefile objects.
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
#' When rec_icons = FALSE or the source is not provided in .data, all
#' Monitoring Locations are show with a plain circle.
#'
#' @return A leaflet map visualizing Monitoring Locations and linked ATTAINS assessment units. All maps are in WGS84.
#'
#' @seealso [TADA_DataRetrieval()] must be run first to get WQP monitoring locations and results.
#' @seealso [TADA_CreateAUMLCrosswalk()] which runs [TADA_CreateATTAINSAUMLCrosswalk()] with
#' return_sf argument set to TRUE and [TADA_GetATTAINSAUMLCrosswalk()] by default.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get WQP Monitoring Locations
#' tada_data <- TADA_DataRetrieval(
#'   startDate = "1990-01-01",
#'   endDate = "1995-12-31",
#'   characteristicName = "pH",
#'   statecode = "NV",
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' # Match AUs using all available methods
#' all_sources <- TADA_CreateAUMLCrosswalk(tada_data, org_id = "21NEV1")
#'
#' TADA_ViewATTAINS(all_sources)
#'
#' # Only use ATTAINS catchments to match AUs
#' attains_catchments <- TADA_CreateATTAINSAUMLCrosswalk(tada_data,
#'   return_nearest = TRUE, resolution = "hi", return_sf = TRUE
#' )
#'
#' TADA_ViewATTAINS(attains_catchments)
#' }
#'
TADA_ViewATTAINS <- function(.data, ref_icons = TRUE) {
  if (
    !any(
      c(
        "ATTAINS_catchments",
        "ATTAINS_points",
        "ATTAINS_lines",
        "ATTAINS_polygons"
      ) %in%
        names(.data)
    )
  ) {
    stop(
      "Your input dataframe was not produced from `TADA_CreateATTAINSAUMLCrosswalk(return_sf = TRUE)`, or it was modified. Please create your list of ATTAINS features using `TADA_CreateATTAINSAUMLCrosswalk()` and confirm that return_sf had been set to TRUE."
    )
  }

  ATTAINS_table <- .data[["TADA_with_ATTAINS"]]
  ATTAINS_catchments <- .data[["ATTAINS_catchments"]]
  ATTAINS_points <- .data[["ATTAINS_points"]]
  ATTAINS_lines <- .data[["ATTAINS_lines"]]
  ATTAINS_polygons <- .data[["ATTAINS_polygons"]]

  if (
    is.null(ATTAINS_lines) & is.null(ATTAINS_points) & is.null(ATTAINS_polygons)
  ) {
    message("No ATTAINS data associated with this Water Quality Portal data.")
  }

  # load images that are required for all legends

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
    system.file("extdata/icons", "ns.point.circle.png", package = "EPATADA"), # 10
    system.file("extdata/icons", "s.point.circle.png", package = "EPATADA"), # 11
    system.file("extdata/icons", "na.point.circle.png", package = "EPATADA") # 12
  )

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
    "ATTAINS: Not Supporting Point", # 10
    "ATTAINS: Supporting Point", # 11
    "ATTAINS: Not Assessed Point" # 12
  )

  # Check if all image paths exist
  for (path in images) {
    if (!file.exists(path)) {
      stop(sprintf("Image file not found: %s", path))
    }
  }

  # ATTAINS API seems to be missing some AU data that is still preserved in the catchment layer.
  # Use catchments for those instances for mapping purposes:
  missing_raw_features <- NULL

  try(
    missing_raw_features <- ATTAINS_catchments |>
      dplyr::filter(
        !assessmentunitidentifier %in%
          c(
            ATTAINS_points$assessmentunitidentifier,
            ATTAINS_lines$assessmentunitidentifier,
            ATTAINS_polygons$assessmentunitidentifier
          )
      ),
    silent = TRUE
  )

  if (!"without_ATTAINS_catchments" %in% names(.data)) {
    if (nrow(ATTAINS_table) == 0) {
      stop("Your WQP dataframe has no observations.")
    }
  }

  if ("without_ATTAINS_catchments" %in% names(.data)) {
    without_ATTAINS_table <- .data[["TADA_without_ATTAINS"]]

    if (nrow(ATTAINS_table) == 0 & nrow(without_ATTAINS_table) == 0) {
      stop("Your WQP dataframe has no observations.")
    }
  }

  required_columns <- c(
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

  if (!any(required_columns %in% colnames(ATTAINS_table))) {
    stop(
      "Your dataframe does not contain the necessary WQP-style column names."
    )
  }

  suppressMessages(suppressWarnings({
    # if data was spatial, remove for downstream leaflet dev:
    try(ATTAINS_table <- ATTAINS_table |> sf::st_drop_geometry(), silent = TRUE)

    tada.pal <- TADA_ColorPalette()

    colors <- data.frame(
      overallstatus = c("Not Supporting", "Fully Supporting", "Not Assessed"),
      col = c(tada.pal[3], tada.pal[4], tada.pal[7]),
      dark_col = c(tada.pal[12], tada.pal[6], tada.pal[11]),
      priority = c(1, 2, 3)
    )

    # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
    try(
      {
        # extract coordinates and convert to a tibble (to handle point or multipoint)
        coords <- sf::st_coordinates(ATTAINS_points) |>
          tibble::as_tibble() |>
          tibble::rowid_to_column(var = "index")

        # points mapper setup
        points_mapper <- ATTAINS_points |>
          dplyr::left_join(colors, by = "overallstatus") |>
          dplyr::mutate(type = "Point Feature") |>
          tibble::rowid_to_column(var = "index") |>
          dplyr::right_join(coords, by = "index")

        # remove intermediate object
        rm(coords)
      },
      silent = TRUE
    )

    # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
    try(
      lines_mapper <- ATTAINS_lines |>
        dplyr::left_join(colors, by = "overallstatus") |>
        dplyr::mutate(type = "Line Feature"),
      silent = TRUE
    )

    # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
    try(
      polygons_mapper <- ATTAINS_polygons |>
        dplyr::left_join(colors, by = "overallstatus") |>
        dplyr::mutate(type = "Polygon Feature") |>
        # sort df so smaller AUs will map on top of larger AUs if they overlap
        dplyr::arrange(dplyr::desc(Shape_Area)),
      silent = TRUE
    )

    # CATCHMENT FEATURES - try to pull missing feature AU data if it exists. Otherwise, move on...
    try(
      missing_raw_mapper <- missing_raw_features |>
        dplyr::left_join(colors, by = "overallstatus") |>
        dplyr::mutate(type = "Raw Feature Unavailable"),
      silent = TRUE
    )

    # Develop WQP site stats (e.g. count of observations, parameters, per site)
    sumdat <- ATTAINS_table |>
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
          "TADA.AURefSource" %in% names(ATTAINS_table),
          as.character(TADA.AURefSource),
          "not provided"
        )
      ) |>
      dplyr::mutate(
        ATTAINS_AUs = ifelse(is.na(ATTAINS_AUs), "None", ATTAINS_AUs),
        LatitudeMeasure = as.numeric(TADA.LatitudeMeasure),
        LongitudeMeasure = as.numeric(TADA.LongitudeMeasure)
      )

    # Basemap for AOI:
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
        lng1 = min(sumdat$TADA.LongitudeMeasure, na.rm = TRUE),
        lat1 = min(sumdat$TADA.LatitudeMeasure, na.rm = TRUE),
        lng2 = max(sumdat$TADA.LongitudeMeasure, na.rm = TRUE),
        lat2 = max(sumdat$TADA.LatitudeMeasure, na.rm = TRUE)
      ) |>
      leaflet.extras::addResetMapButton()

    # Initialize vectors to hold the names of groups we actually add
    overlay_groups <- character(0)
    # Add ATTAINS catchment outlines (if they exist):
    try(
      {
        map <- map |>
          leaflet::addPolygons(
            data = ATTAINS_catchments,
            group = "ATTAINS catchments",
            color = "black",
            fillColor = "grey",
            weight = 1,
            fillOpacity = 0.3,
            popup = paste0(
              "NHDPlus HR Catchment ID: ",
              ATTAINS_catchments$nhdplusid
            )
          )
        overlay_groups <- c(overlay_groups, "ATTAINS catchments")
      },
      silent = TRUE
    )

    # Add ATTAINS catchment outlines as AUs:
    try(
      {
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

    # Add ATTAINS polygon features (if they exist):
    try(
      {
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
    try(
      {
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
          points_mapper$overallstatus == "Fully Supporting" ~ images[11],
          points_mapper$overallstatus == "Not Supporting" ~ images[10],
          points_mapper$overallstatus == "Not Assessed" ~ images[12]
        ),
        iconWidth = 48,
        iconHeight = 48
      ),
      silent = TRUE
    )

    # Add ATTAINS point features (if they exist):
    try(
      {
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

    # check for Monitoring Locations with assigned AUIDs that do not have geometry from ATTAINS
    if ("TADA.AURefSource" %in% names(ATTAINS_table)) {
      user.refs <- ATTAINS_table |>
        dplyr::filter(
          TADA.AURefSource %in% c("User-supplied Ref", "ATTAINS Crosswalk")
        ) |>
        dplyr::select(
          TADA.MonitoringLocationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          ATTAINS.WaterType
        ) |>
        dplyr::distinct()

      # if any AUIDs were assigned by user or from ATTAINS cw check to see if they have matching geometry from ATTAINS

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

        # combine lists
        all.attains.aus <- append(point.aus, line.aus)

        all.attains.aus <- append(all.attains.aus, polygon.aus)

        # retain unique assessment unit identifiers
        all.attains.aus <- unique(all.attains.aus)

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

          # markers and popup for missing geometry to map
          try(
            {
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
            },
            silent = TRUE
          )
        }
      }
    }

    # set base pop up for monitoring locations
    set.popup <- paste0(
      "Site ID: ",
      sumdat$TADA.MonitoringLocationIdentifier,
      "<br> Site Name: ",
      sumdat$TADA.MonitoringLocationName,
      "<br> Organization Name: ",
      sumdat$OrganizationFormalName,
      "<br> Measurement Count: ",
      sumdat$Sample_Count,
      "<br> Visit Count: ",
      sumdat$Visit_Count,
      "<br> Characteristic Count: ",
      sumdat$Parameter_Count,
      "<br> ATTAINS Assessment Unit(s): ",
      sumdat$ATTAINS_AUs
    )

    # add au ref source to pop up  if available
    if ("TADA.AURefSource" %in% names(ATTAINS_table)) {
      set.popup <- paste0(
        set.popup,
        "<br>",
        "Crosswalk Source: ",
        sumdat$TADA.AURefSource
      )
    }

    # set base image and label ref lists for legend
    attains.imgs <- images[1:3]
    attains.labels <- img.labels[1:3]

    # add missing geometry image and label if needed
    if (exists("missing.geo")) {
      if (dim(missing.geo)[1] > 0) {
        attains.imgs <- append(attains.imgs, images[4])
        attains.labels <- append(attains.labels, img.labels[4])

        # remove intermediate object
        rm(missing.geo)
      }
    }

    # set image ref, image label, and icon url lists for WQP monitoring locations
    if (!"TADA.AURefSource" %in% names(ATTAINS_table) | ref_icons == FALSE) {
      wqp.imgs <- images[8]
      wqp.labels <- img.labels[8]

      wqp.urls <- images[8]
    } else {
      wqp.imgs <- images[5:7]
      wqp.labels <- img.labels[5:7]

      wqp.urls <- dplyr::case_when(
        sumdat$TADA.AURefSource == "ATTAINS Crosswalk" ~ images[6],
        sumdat$TADA.AURefSource == "TADA_CreateATTAINSAUMLCrosswalk" ~ images[
          7
        ],
        sumdat$TADA.AURefSource == "User-supplied Ref" ~ images[5]
      )
    }

    # set image ref for catchments
    catch.imgs <- images[9]
    catch.labels <- img.labels[9]

    # create overall legend labels and images
    images.ref <- c(attains.imgs, wqp.imgs, catch.imgs)

    leg.labels <- c(attains.labels, wqp.labels, catch.labels)

    # remove intermediate objects
    rm(
      attains.imgs,
      attains.labels,
      wqp.imgs,
      wqp.labels,
      catch.imgs,
      catch.labels
    )

    # Add WQP observation features (should always exist):
    try(
      {
        map <- map |>
          leaflet::addMarkers(
            data = sumdat,
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
      },
      silent = TRUE
    )

    # remove intermediate objects
    rm(wqp.urls, set.popup)
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
    # add button to toggle map legend on/off
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
    }
    # Return leaflet map of TADA WQ and its associated ATTAINS data
    return(map)
  }))
}
