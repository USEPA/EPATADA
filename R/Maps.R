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
      map <- createTADABasemap(.data)

      map <- map |>
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
      # create bbox for adding tribal layers
      bbox <- createBBox(sumdat, as_vector = FALSE)

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

  # create TADA basemap
  map <- createTADABasemap(.data)

  if (nrow(outsideusa) > 0) {
    map <- addFlaggedSitesMarkers(
      outsideusa,
      map = map,
      flag_type = "outsideusa"
    )
  }
  if (nrow(lowres) > 0) {
    map <- addFlaggedSitesMarkers(lowres, map = map, flag_type = "lowres")
  }

  # remove intermediate objects
  rm(invalid, lowres, outsideusa)

  # return flagged sites map
  return(map)
}

#' Create Nearby Sites Map
#'
#' @param .data Either(1) a TADA dataframe or (2) the list of data frames created
#' by TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk. If
#' TADA_FindNearbySites has not been previously run, it will be as part of this
#' function. In order for ATTAINS assessment units to be displayed on the nearby
#' sites map, .data must be the list of data frames created by
#' TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk.
#'
#' @param dist_buffer Distance in m to show a radius around each site marker.
#'
#' @param attains Boolean. If attains = TRUE and assessment unit geometry is available
#' in the list of data frames created by TADA_CreateATTAINSAUMLCrosswalk or
#' TADA_CreateAUMLCrosswalk, assessment units will be added to the review map.
#' If attains = FALSE, no assessment units will be shown. Default is attains = TRUE.
#'
#' @param catchment Boolean. If catchment = TRUE, any catchment data available in
#' .data are added to the review map. If catchment = FALSE, catchments are not
#' added to the review map. Default is catchment = FALSE.
#'
#' @return A leaflet map that shows all sites in the dataframe that contain
#' flagged data in the form of near other sites - groups of sites that are spatially located within
#' a threshold distance (defaulting to 100 m) from each other and within the same catchment.
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
TADA_NearbySitesMap <- function(
  .data,
  dist_buffer = 100,
  attains = TRUE,
  catchment = FALSE
) {
  # columns to select for nearby site
  nearby.cols <- c(
    "LongitudeMeasure",
    "LatitudeMeasure",
    "TADA.MonitoringLocationIdentifier",
    "MonitoringLocationIdentifier",
    "MonitoringLocationName",
    "TADA.LatitudeMeasure",
    "LatitudeMeasure",
    "TADA.LongitudeMeasure",
    "LongitudeMeasure",
    "OrganizationIdentifier",
    "OrganizationFormalName",
    "TADA.NearbySiteGroup"
  )

  # check to see if input is a single df
  if (inherits(.data, "data.frame")) {
    TADA_table <- .data
  }

  # check to see if input is a list
  if (inherits(.data, "list")) {
    # name dfs for use in function
    TADA_table <- .data[["TADA_with_ATTAINS"]]
    ATTAINS_catchments <- .data[["ATTAINS_catchments"]]
    ATTAINS_points <- .data[["ATTAINS_points"]]
    ATTAINS_lines <- .data[["ATTAINS_lines"]]
    ATTAINS_polygons <- .data[["ATTAINS_polygons"]]

    # add assessment unit columns to nearby.cols
    nearby.cols <- append(
      nearby.cols,
      c("ATTAINS.AssessmentUnitIdentifier", "TADA.AURefSource")
    )

    # check to make sure WQP observations exist
    checkForWQPData(TADA_table)
  }

  # if not previously run, run TADA_FindNearbySites
  if ("TADA.NearbySiteGroup" %in% colnames(TADA_table) == FALSE) {
    # find nearby sites in the TADA df
    TADA_table <- TADA_FindNearbySites(TADA_table, dist_buffer = dist_buffer)
  }

  # create df of nearby sites
  TADA_nearby <- TADA_table |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::mutate(
      LatitudeMeasure = as.numeric(LatitudeMeasure),
      LongitudeMeasure = as.numeric(LongitudeMeasure)
    ) |>
    dplyr::select(dplyr::all_of(nearby.cols)) |>
    dplyr::distinct()

  # check to see if any nearby site groups were found
  if (nrow(TADA_nearby) == 0) {
    stop(
      "TADA_NearbySitesMap: There are no grouped nearby sites in the data frame."
    )
  }

  # find number of colors needed for nearby site groups
  n.colors <- length(unique(TADA_nearby$TADA.NearbySiteGroup))

  # get TADA color palette
  tada.pal <- TADA_ColorPalette()

  # create nearby site groups color palette
  # if needed can incorporate functions from package "farver" to force pal colors
  # away from tada.pal colors (HRM 12/23/25)
  nearby.pal <- Polychrome::createPalette(
    n.colors,
    seedcolors = c(
      tada.pal[1],
      tada.pal[3],
      tada.pal[4],
      tada.pal[7],
      tada.pal[15]
    ),
    M = 5000
  )

  # assign colors to nearby groups
  pal <- leaflet::colorFactor(
    palette = nearby.pal,
    domain = TADA_table$TADA.NearbySiteGroup
  )

  # if data was spatial, remove for downstream leaflet dev:
  try(TADA_table <- TADA_table |> sf::st_drop_geometry(), silent = TRUE)

  # required cols for map popup
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

  # create nearby sites map
  map <- createTADABasemap(TADA_nearby)

  # if attains = TRUE and assessment unit geometry is included in TADA df add AUs to map
  if (
    attains == TRUE & "ATTAINS.AssessmentUnitIdentifier" %in% names(TADA_table)
  ) {
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

    # ATTAINS API seems to be missing some AU data that is still preserved in the catchment layer.
    # Use catchments for those instances for mapping purposes:
    # ATTAINS API seems to be missing some AU data that is still preserved in the catchment layer.
    # Use catchments for those instances for mapping purposes:
    try(
      missing_raw_features <- findATTAINSMissingRawFeatures(
        ATTAINS_catchments,
        points_layer = ATTAINS_points,
        polygons_layer = ATTAINS_polygons,
        lines_layer = ATTAINS_lines,
        auid_list = unique(TADA_table$ATTAINS.AssessmentUnitIdentifier)
      ),
      silent = TRUE
    )

    if (!any(required_columns %in% colnames(TADA_table))) {
      stop(
        "Your dataframe does not contain the necessary WQP-style column names."
      )
    }

    suppressMessages(suppressWarnings({
      # create df to assign color based on ATTAINS overall status
      colors <- getATTAINSColorsRef()

      # prep ATTAINS assessment unit features
      au_mapper <- prepAllATTAINSMapper(
        color_ref = colors,
        lines_layer = ATTAINS_lines,
        points_layer = ATTAINS_points,
        polygons_layer = ATTAINS_polygons,
        auid_list = unique(TADA_nearby$ATTAINS.AssessmentUnitIdentifier)
      )

      # CATCHMENT FEATURES - try to pull missing feature AU data if it exists. Otherwise, move on...
      try(
        missing_raw_mapper <- missing_raw_features |>
          dplyr::left_join(colors, by = "overallstatus") |>
          dplyr::mutate(type = "Raw Feature Unavailable"),
        silent = TRUE
      )

      # remove intermediate object
      rm(missing_raw_features)

      # Initialize vectors to hold the names of groups we actually add
      overlay_groups <- character(0)

      # add these steps to prepATTAINS and prepAllATTAINS
      if (!is.null(ATTAINS_catchments)) {
        ATTAINS_catchments <- ATTAINS_catchments |>
          dplyr::filter(
            assessmentunitidentifier %in%
              unique(TADA_nearby$ATTAINS.AssessmentUnitIdentifier)
          )
      }

      if (!is.null(without_ATTAINS_catchments)) {
        without_ATTAINS_catchments <- without_ATTAINS_catchments |>
          dplyr::filter(
            assessmentunitidentifier %in%
              unique(TADA_nearby$ATTAINS.AssessmentUnitIdentifier)
          )
      }

      # add all ATTAINS geometry to map
      all_attains <- addAllATTAINS(
        map = map,
        points_layer = au_mapper$points_mapper,
        polygons_layer = au_mapper$polygons_mapper,
        lines_layer = au_mapper$lines_layer,
        catchment_layer = ATTAINS_catchments,
        outline_layer = without_ATTAINS_catchments,
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
          ATTAINS_table = TADA_table,
          ATTAINS_polygons = ATTAINS_polygons,
          ATTAINS_points = ATTAINS_points,
          ATTAINS_lines = ATTAINS_lines,
          map = map,
          overlay_groups = overlay_groups
        )

        map <- missing_aus$map

        overlay_groups <- missing_aus$overlay_groups

        # remove intermediate objects
        rm(missing_aus)
      })
    }))
  }

  # add nearby sites to map
  if (nrow(TADA_nearby) > 0) {
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
        popup = ~ paste0(
          "Nearby Group Name: ",
          TADA.MonitoringLocationIdentifier,
          "<br> Nearby Site Group: ",
          TADA.NearbySiteGroup,
          "<br> Site ID: ",
          MonitoringLocationIdentifier,
          "<br> Site Name: ",
          MonitoringLocationName,
          "<br> Organization Name: ",
          OrganizationFormalName,
          "<br> Latitude: ",
          LatitudeMeasure,
          "<br> Longitude: ",
          LongitudeMeasure
        ),
        data = TADA_nearby,
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
        weight = 1,
        data = TADA_nearby
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
#' When ref_icons = FALSE or the source is not provided in .data, all
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

  # check for ATTAINS data
  checkForATTAINSGeo(
    points_layer = ATTAINS_points,
    lines_layer = ATTAINS_lines,
    polygons_layer = ATTAINS_polygons
  )

  # check for required columns in ATTAINS_table for mapping
  checkTADAColsForMap(ATTAINS_table, attains = TRUE)

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

    # prep ATTAINS assessment unit features
    au_mapper <- prepAllATTAINSMapper(
      color_ref = colors,
      lines_layer = ATTAINS_lines,
      points_layer = ATTAINS_points,
      polygons_layer = ATTAINS_polygons
    )

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
          ref_icons = TRUE,
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
