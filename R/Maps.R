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
