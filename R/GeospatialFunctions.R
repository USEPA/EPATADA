#' TADA_MakeSpatial
#'
#' Transform a Water Quality Portal dataframe into a geospatial sf object.
#'
#' Adds one new column to input dataset, 'geometry', which allows for mapping and additional geospatial capabilities. Check out the TADAModule2.Rmd for an example workflow.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()`.
#' @param crs The coordinate reference system (CRS) you would like the returned point features to be in. The default is CRS 4326 (WGS84).
#'
#' @return The original TADA Water Quality Portal dataframe but as geospatial sf point objects.
#'
#' @seealso [TADA_DataRetrieval()]
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' tada_not_spatial <- TADA_DataRetrieval(
#'   characteristicName = "pH",
#'   statecode = "SC",
#'   countycode = "Abbeville",
#'   applyautoclean = TRUE
#' )
#'
#' # make `tada_not_spatial` an sf object, projected in crs = 4269 (NAD83)
#' tada_spatial <- TADA_MakeSpatial(tada_not_spatial, crs = 4269)
#' }
#' 
TADA_MakeSpatial <- function(.data, crs = 4326) {
  if (!"LongitudeMeasure" %in% colnames(.data) |
    !"LatitudeMeasure" %in% colnames(.data) |
    !"HorizontalCoordinateReferenceSystemDatumName" %in% colnames(.data)) {
    stop("The dataframe does not contain WQP-style latitude and longitude data (column names `HorizontalCoordinateReferenceSystemDatumName`, `LatitudeMeasure`, and `LongitudeMeasure`.")
  } else if (!is.null(.data) & inherits(.data, "sf")) {
    stop("Your data is already a spatial object.")
  }

  suppressMessages(suppressWarnings({
    # Make a reference table for CRS and EPSG codes
    # List should include all codes in WQX domain (see HorizontalCoordinateReferenceSystemDatum CSV at https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads)
    epsg_codes <- tidyr::tribble(
      ~HorizontalCoordinateReferenceSystemDatumName, ~epsg,
      "NAD83", 4269,
      "WGS84", 4326,
      "NAD27", 4267,
      "UNKWN", crs, # Unknowns and NAs should go to user supplied default
      "OTHER", 4326,
      "OLDHI", 4135,
      "AMSMA", 4169,
      "ASTRO", 4727,
      "GUAM", 4675,
      "JHNSN", 4725,
      "PR", 6139,
      "SGEOR", 4138,
      "SLAWR", 4136, 
      "SPAUL", 4137,
      "WAKE", 6732,
      "WGS72", 6322, 
      "HARN", 4152
    )

    # join our CRS reference table to our original WQP dataframe:
    sf <- .data %>%
      tibble::rowid_to_column(var = "index") %>%
      dplyr::mutate(
        lat = as.numeric(LatitudeMeasure),
        lon = as.numeric(LongitudeMeasure)
      ) %>%
      # Add EPSG codes
      dplyr::left_join(
        x = .,
        y = epsg_codes,
        by = "HorizontalCoordinateReferenceSystemDatumName"
      ) %>%
      # Group by CRS:
      split(f = .$HorizontalCoordinateReferenceSystemDatumName) %>%
      # Transform and re-stack:
      purrr::map_df(
        .x = .,
        .f = ~ .x %>%
          sf::st_as_sf(
            coords = c("lon", "lat"),
            crs = unique(.x$epsg)
          ) %>%
          # transform to the selected CRS:
          sf::st_transform(sf::st_crs(as.numeric(crs)))
      ) %>%
      dplyr::arrange(index) %>%
      dplyr::select(-c(index, epsg))
  }))

  return(sf)
}


#' fetchATTAINS
#'
#' Fetch ATTAINS features (entity submitted points, lines, polygons representing their assessment units; and EPA snapshot of the associated NHDPlus HR catchments that the entity submitted features fall within) within a bounding box produced from a set of TADA spatial features.
#'
#' @param .data A dataframe developed using `TADA_DataRetrieval()` or `TADA_MakeSpatial()`.
#' @return spatial features (ATTAINS_catchments, ATTAINS_points, ATTAINS_lines, and ATTAINS_polygons) that are within the spatial bounding box of water quality observations.
#'
#' @seealso [TADA_MakeSpatial()]
#' @seealso [TADA_DataRetrieval()]
#'
#' @examples
#' \dontrun{
#' tada_data <- TADA_DataRetrieval(
#'   startDate = "1990-01-01",
#'   endDate = "1995-12-31",
#'   characteristicName = "pH",
#'   statecode = "NV",
#'   applyautoclean = TRUE
#' )
#'
#' nv_attains_features <- fetchATTAINS(tada_data)
#' }
fetchATTAINS <- function(.data) {
  if (is.null(.data) | nrow(.data) == 0) {
    stop("There is no data in your `data` object to use as a bounding box for selecting ATTAINS features.")
  }

  # EPSG we want our ATTAINS data to be in (always 4326 for this function)
  our_epsg <- 4326

  # If data is already spatial, just make sure it is in the right CRS
  # and add an index as the WQP observations' unique identifier...
  if (!is.null(.data) & inherits(.data, "sf")) {
    if (sf::st_crs(.data)$epsg != our_epsg) {
      .data <- .data %>%
        sf::st_transform(our_epsg)
    } else {
      .data <- .data
    }
  } else {
    # ... Otherwise transform into a spatial object then do the same thing:
    .data <- .data %>%
      # convert dataframe to a spatial object
      TADA_MakeSpatial(.data = ., crs = our_epsg)
  }

  baseurls <- c( # ATTAINS catchments:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?",
    # ATTAINS points:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?",
    # ATTAINS lines:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?",
    # ATTAINS polygons:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?"
  )

  feature_downloader <- function(baseurls) {
    # starting at feature 1 (i.e., no offset):
    offset <- 0
    # empty list to store all features in
    all_features <- list()

    # bounding box (with some minor wiggle) of user's WQP data
    suppressMessages(suppressWarnings({
      bbox <- .data %>%
        sf::st_bbox(.) %>%
        # convert bounding box to characters
        toString(.) %>%
        # encode for use within the API URL
        urltools::url_encode(.)
    }))

    # The ATTAINS API has a limit of 2000 features that can be pulled in at once.
    # Therefore, we must split the call into manageable "chunks" using a moving
    # window of what features to pull in, then munging all the separate API calls
    # together.

    repeat {
      query <- urltools::param_set(baseurls, key = "geometry", value = bbox) %>%
        urltools::param_set(key = "inSR", value = our_epsg) %>%
        # Total of 2000 features at a time...
        urltools::param_set(key = "resultRecordCount", value = 2000) %>%
        # ... starting at the "offset":
        urltools::param_set(key = "resultOffset", value = offset) %>%
        urltools::param_set(key = "spatialRel", value = "esriSpatialRelIntersects") %>%
        urltools::param_set(key = "f", value = "geojson") %>%
        urltools::param_set(key = "outFields", value = "*") %>%
        urltools::param_set(key = "geometryType", value = "esriGeometryEnvelope") %>%
        urltools::param_set(key = "returnGeometry", value = "true") %>%
        urltools::param_set(key = "returnTrueCurves", value = "false") %>%
        urltools::param_set(key = "returnIdsOnly", value = "false") %>%
        urltools::param_set(key = "returnCountOnly", value = "false") %>%
        urltools::param_set(key = "returnZ", value = "false") %>%
        urltools::param_set(key = "returnM", value = "false") %>%
        urltools::param_set(key = "returnDistinctValues", value = "false") %>%
        urltools::param_set(key = "returnExtentOnly", value = "false") %>%
        urltools::param_set(key = "featureEncoding", value = "esriDefault")

      # Fetch features within the offset window and append to list:
      features <- suppressMessages(suppressWarnings({
        tryCatch(
          {
            geojsonsf::geojson_sf(query)
          },
          error = function(e) {
            NULL
          }
        )
      }))


      # Exit loop if no more features or error occurred
      if (is.null(features) || nrow(features) == 0) {
        break
      }

      all_features <- c(all_features, list(features))
      # once done, change offset by 2000 features:
      offset <- offset + 2000

      if (offset == 4000) {
        print("Your TADA data covers a large spatial range. The ATTAINS pull may take a while.")
      }
    }

    all_features <- dplyr::bind_rows(all_features)
  }

  final_features <- baseurls %>%
    purrr::map(~ feature_downloader(.))

  names(final_features) <- c("ATTAINS_catchments", "ATTAINS_points", "ATTAINS_lines", "ATTAINS_polygons")

  return(final_features)
}


#' TADA_GetATTAINS
#'
#' Link catchment-based ATTAINS assessment unit data (EPA snapshot of NHDPlus HR catchments associated with entity submitted assessment unit features - points, lines, and polygons) to Water Quality Portal observations, often imported via `TADA_DataRetrieval()`. This function returns the same raw objects that are mapped in `TADA_ViewATTAINS()`.
#'
#' Adds the following ATTAINS columns to the input dataframe or list: "ATTAINS.organizationid", "ATTAINS.submissionid", "ATTAINS.hasprotectionplan", "ATTAINS.assessmentunitname", "ATTAINS.nhdplusid", "ATTAINS.tas303d", "ATTAINS.isthreatened", "ATTAINS.state", "ATTAINS.on303dlist", "ATTAINS.organizationname", "ATTAINS.region", "ATTAINS.Shape_Length", "ATTAINS.reportingcycle", "ATTAINS.assmnt_joinkey", "ATTAINS.hastmdl", "ATTAINS.orgtype", "ATTAINS.permid_joinkey", "ATTAINS.catchmentistribal", "ATTAINS.ircategory", "ATTAINS.waterbodyreportlink", "ATTAINS.assessmentunitidentifier", "ATTAINS.overallstatus", "ATTAINS.isassessed", "ATTAINS.isimpaired", "ATTAINS.has4bplan", "ATTAINS.huc12", "ATTAINS.hasalternativeplan", "ATTAINS.visionpriority303d", "ATTAINS.areasqkm", "ATTAINS.catchmentareasqkm", "ATTAINS.catchmentstatecode", "ATTAINS.catchmentresolution", "ATTAINS.Shape_Area". Check out the TADAModule2.Rmd for an example workflow.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()` or the sf equivalent made by `TADA_MakeSpatial()`.
#' @param return_sf Whether to return the associated ATTAINS_catchments, ATTAINS_lines, ATTAINS_points, and ATTAINS_polygons shapefile objects. TRUE (yes, return) or FALSE (no, do not return). All ATTAINS features are in WGS84 (crs = 4326).
#'
#' @return A modified `TADA_DataRetrieval()` dataframe with additional columns associated with the ATTAINS assessment unit data. Or, if return_sf = TRUE, a list containing that same data frame plus the raw ATTAINS features associated with those observations.
#'
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_MakeSpatial()]
#' @seealso [TADA_ViewATTAINS()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tada_data <- TADA_DataRetrieval(
#'   startDate = "2018-05-01",
#'   endDate = "2018-09-30",
#'   characteristicName = "pH",
#'   statecode = "IL",
#'   applyautoclean = TRUE
#' )
#'
#' tada_attains <- TADA_GetATTAINS(tada_data, return_sf = FALSE)
#'
#' tada_attains_list <- TADA_GetATTAINS(tada_data, return_sf = TRUE)
#' }
TADA_GetATTAINS <- function(.data, return_sf = TRUE) {
  attains_names <- c(
    "ATTAINS.organizationid", "ATTAINS.submissionid", "ATTAINS.hasprotectionplan",
    "ATTAINS.assessmentunitname", "ATTAINS.nhdplusid", "ATTAINS.tas303d",
    "ATTAINS.isthreatened", "ATTAINS.state", "ATTAINS.on303dlist",
    "ATTAINS.organizationname", "ATTAINS.region", "ATTAINS.Shape_Length",
    "ATTAINS.reportingcycle", "ATTAINS.assmnt_joinkey", "ATTAINS.hastmdl",
    "ATTAINS.orgtype", "ATTAINS.permid_joinkey", "ATTAINS.catchmentistribal",
    "ATTAINS.ircategory", "ATTAINS.waterbodyreportlink", "ATTAINS.assessmentunitidentifier",
    "ATTAINS.overallstatus", "ATTAINS.isassessed", "ATTAINS.isimpaired",
    "ATTAINS.has4bplan", "ATTAINS.huc12", "ATTAINS.hasalternativeplan",
    "ATTAINS.visionpriority303d", "ATTAINS.areasqkm", "ATTAINS.catchmentareasqkm",
    "ATTAINS.catchmentstatecode", "ATTAINS.catchmentresolution", "ATTAINS.Shape_Area"
  )

  if (any(attains_names %in% colnames(.data))) {
    stop("Your data has already been joined with ATTAINS data.")
  }

  if (nrow(.data) == 0) {
    print("Your Water Quality Portal dataframe has no observations. Returning an empty dataframe with empty ATTAINS features.")

    # if no WQP observations, return a modified `data` with empty ATTAINS-related columns:

    col_val_list <- stats::setNames(
      object = rep(
        x = list(NA),
        times = length(attains_names)
      ),
      nm = attains_names
    )

    # Add ATTAINS columns with NA values
    no_WQP_data <- .data %>%
      dplyr::mutate(index = NA) %>%
      dplyr::bind_cols(col_val_list)

    # In this case we'll need to return empty ATTAINS objects
    if (return_sf == TRUE) {
      ATTAINS_catchments <- NULL
      ATTAINS_lines <- NULL
      ATTAINS_points <- NULL
      ATTAINS_polygons <- NULL

      return(list(
        "TADA_with_ATTAINS" = no_WQP_data,
        "ATTAINS_catchments" = ATTAINS_catchments,
        "ATTAINS_points" = ATTAINS_points,
        "ATTAINS_lines" = ATTAINS_lines,
        "ATTAINS_polygons" = ATTAINS_polygons
      ))
      # If ATTAINS objects not requested, then just return the dataframe:
    } else {
      return(no_WQP_data)
    }
  }

  # If data doesn't already contain ATTAINS data and isn't an empty dataframe:
  suppressMessages(suppressWarnings({
    sf::sf_use_s2(FALSE)

    # If data is already spatial, just make sure it is in the right CRS
    # and add unique WQP ID for identifying obs with more than one ATTAINS assessment unit

    if (!is.null(.data) & inherits(.data, "sf")) {
      if (sf::st_crs(.data)$epsg != 4326) {
        TADA_DataRetrieval_data <- .data %>%
          sf::st_transform(4326) %>%
          tibble::rowid_to_column(var = "index")
      } else {
        TADA_DataRetrieval_data <- .data %>%
          tibble::rowid_to_column(var = "index")
      }
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      TADA_DataRetrieval_data <- .data %>%
        # convert dataframe to a spatial object
        TADA_MakeSpatial(.data = ., crs = 4326) %>%
        # add unique WQP ID for identifying obs with more than one ATTAINS assessment unit
        tibble::rowid_to_column(var = "index")
    }
  }))

  attains_features <- try(fetchATTAINS(.data = TADA_DataRetrieval_data), silent = TRUE)

  suppressMessages(suppressWarnings({
    # grab the ATTAINS catchments within our WQP bbox:
    nearby_catchments <- NULL
    # (Wrapped with "try" because it is possible that no ATTAINS data exists in the bbox.)
    try(
      nearby_catchments <- attains_features[["ATTAINS_catchments"]] %>%
        # remove unnecessary columns:
        dplyr::select(-c(OBJECTID, GLOBALID)) %>%
        # select only catchments that have WQP observations in them:
        .[TADA_DataRetrieval_data, ] %>%
        # add prefix "ATTAINS" to ATTAINS data
        dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything()) %>%
        # get rid of dupes (as a precaution)
        dplyr::distinct(.keep_all = TRUE),
      silent = TRUE
    )
  }))

  # if no ATTAINS data, return original dataframe with empty ATTAINS columns:
  if (is.null(nearby_catchments)) {
    print("There are no ATTAINS features associated with these WQP observations. Returning original dataframe with empty ATTAINS columns and empty ATTAINS geospatial features.")

    col_val_list <- stats::setNames(
      object = rep(
        x = list(NA),
        times = length(attains_names)
      ),
      nm = attains_names
    )

    # return a modified `.data` with empty ATTAINS-related columns:
    no_ATTAINS_data <- .data %>%
      dplyr::bind_cols(col_val_list) %>%
      tibble::rowid_to_column(var = "index")

    if (return_sf == TRUE) {
      ATTAINS_catchments <- NULL
      ATTAINS_lines <- NULL
      ATTAINS_points <- NULL
      ATTAINS_polygons <- NULL

      return(list(
        "TADA_with_ATTAINS" = no_ATTAINS_data,
        "ATTAINS_catchments" = ATTAINS_catchments,
        "ATTAINS_points" = ATTAINS_points,
        "ATTAINS_lines" = ATTAINS_lines,
        "ATTAINS_polygons" = ATTAINS_polygons
      ))
    } else {
      return(no_ATTAINS_data)
    }

    # If there IS ATTAINS data...
  } else {
    suppressMessages(suppressWarnings({
      # ... link WQP features to the ATTAINS catchment feature(s) they land in:
      TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
        # left join = TRUE to preserve all observations (with or without ATTAINS features):
        sf::st_join(., nearby_catchments, left = TRUE)

      if (return_sf == FALSE) {
        return(TADA_with_ATTAINS)
      }

      # CATCHMENT FEATURES
      # use original catchment pull, but return column names to original
      ATTAINS_catchments <- nearby_catchments
      colnames(ATTAINS_catchments) <- gsub("ATTAINS.", "", colnames(ATTAINS_catchments))
      # due to the rename, must re-set geometry column:
      sf::st_geometry(ATTAINS_catchments) <- "geometry"

      # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
      ATTAINS_points <- NULL
      try(
        ATTAINS_points <- attains_features[["ATTAINS_points"]] %>%
          # subset to only ATTAINS point features in the same NHD HR catchments as WQP observations
          .[nearby_catchments, ] %>%
          # make sure no duplicate features exist
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )

      # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
      ATTAINS_lines <- NULL
      try(
        ATTAINS_lines <- attains_features[["ATTAINS_lines"]] %>%
          # subset to only ATTAINS line features in the same NHD HR catchments as WQP observations
          .[nearby_catchments, ] %>%
          # make sure no duplicate line features exist
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )

      # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
      ATTAINS_polygons <- NULL
      try(
        ATTAINS_polygons <- attains_features[["ATTAINS_polygons"]] %>%
          # subset to only ATTAINS polygon features in the same NHD HR catchments as WQP observations
          .[nearby_catchments, ] %>%
          # make sure no duplicate polygon features exist
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )
    }))

    return(list(
      "TADA_with_ATTAINS" = TADA_with_ATTAINS,
      "ATTAINS_catchments" = ATTAINS_catchments,
      "ATTAINS_points" = ATTAINS_points,
      "ATTAINS_lines" = ATTAINS_lines,
      "ATTAINS_polygons" = ATTAINS_polygons
    ))
  }
}


#' TADA_ViewATTAINS
#'
#' Visualizes the data returned from TADA_GetATTAINS if return_sf was set to TRUE.
#'
#' This function visualizes the raw ATTAINS features that are linked to the
#' TADA Water Quality Portal observations. For the function to work properly,
#' the input dataframe must be the list produced from `TADA_GetATTAINS()`
#' with `return_sf = TRUE`. The map also displays the Water Quality Portal
#' monitoring locations used to find the ATTAINS features. Check out the
#' TADAModule2.Rmd for an example workflow.
#'
#' @param .data A list containing a data frame and ATTAINS shapefile objects created by `TADA_GetATTAINS()` with the return_sf argument set to TRUE.
#'
#' @return A leaflet map visualizing the TADA water quality observations and the linked ATTAINS assessment units. All maps are in WGS84.
#'
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_GetATTAINS()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tada_data <- TADA_DataRetrieval(
#'   startDate = "1990-01-01",
#'   endDate = "1995-12-31",
#'   characteristicName = "pH",
#'   statecode = "NV",
#'   applyautoclean = TRUE
#' )
#'
#' attains_data <- TADA_GetATTAINS(tada_data, return_sf = TRUE)
#'
#' TADA_ViewATTAINS(attains_data)
#' }
TADA_ViewATTAINS <- function(.data) {
  if (!any(c(
    "TADA_with_ATTAINS", "ATTAINS_catchments", "ATTAINS_points",
    "ATTAINS_lines", "ATTAINS_polygons"
  ) %in% names(.data))) {
    stop("Your input dataframe was not produced from `TADA_GetATTAINS()` or it was modified. Please create your list of ATTAINS features using `TADA_GetATTAINS()` and confirm that return_sf has been set to TRUE.")
  }

  ATTAINS_table <- .data[["TADA_with_ATTAINS"]]
  ATTAINS_catchments <- .data[["ATTAINS_catchments"]]
  ATTAINS_points <- .data[["ATTAINS_points"]]
  ATTAINS_lines <- .data[["ATTAINS_lines"]]
  ATTAINS_polygons <- .data[["ATTAINS_polygons"]]

  if (nrow(ATTAINS_table) == 0) {
    stop("Your WQP dataframe has no observations.")
  }

  required_columns <- c(
    "LongitudeMeasure", "LatitudeMeasure",
    "HorizontalCoordinateReferenceSystemDatumName",
    "CharacteristicName", "MonitoringLocationIdentifier",
    "MonitoringLocationName", "ResultIdentifier",
    "ActivityStartDate", "OrganizationIdentifier"
  )

  if (!any(required_columns %in% colnames(ATTAINS_table))) {
    stop("Your dataframe does not contain the necessary WQP-style column names.")
  }

  suppressMessages(suppressWarnings({
    sf::sf_use_s2(FALSE)

    # if data was spatial, remove for downstream leaflet dev:
    try(ATTAINS_table <- ATTAINS_table %>%
      sf::st_drop_geometry(), silent = TRUE)
    
    tada.pal <- TADA_ColorPalette()

    colors <- data.frame(
      overallstatus = c("Not Supporting", "Fully Supporting", "Not Assessed"),
      col = c("#DC851E", "#059FA4", "#A1A522"),
      dark_col = c("#813B00", "#005258", "#4F5900"),
      priority = c(1, 2, 3)
    )

    # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
    try(
      points_mapper <- ATTAINS_points %>%
        dplyr::left_join(., colors, by = "overallstatus") %>%
        dplyr::mutate(type = "Point Feature") %>%
        tibble::rowid_to_column(var = "index") %>%
        # some point features are actually multipoint features. Must extract all coordinates for mapping
        # later:
        dplyr::right_join(., tibble::as_tibble(sf::st_coordinates(ATTAINS_points)), by = c("index" = "L1")),
      silent = TRUE
    )

    # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
    try(
      lines_mapper <- ATTAINS_lines %>%
        dplyr::left_join(., colors, by = "overallstatus") %>%
        dplyr::mutate(type = "Line Feature"),
      silent = TRUE
    )

    # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
    try(
      polygons_mapper <- ATTAINS_polygons %>%
        dplyr::left_join(., colors, by = "overallstatus") %>%
        dplyr::mutate(type = "Polygon Feature"),
      silent = TRUE
    )

    # Develop WQP site stats (e.g. count of observations, parameters, per site)
    sumdat <- ATTAINS_table %>%
      dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, LatitudeMeasure, LongitudeMeasure) %>%
      dplyr::summarize(
        Sample_Count = length(unique(ResultIdentifier)),
        Visit_Count = length(unique(ActivityStartDate)),
        Parameter_Count = length(unique(CharacteristicName)),
        Organization_Count = length(unique(OrganizationIdentifier)),
        ATTAINS_AUs = as.character(list(unique(ATTAINS.assessmentunitidentifier)))
      ) %>%
      dplyr::mutate(
        ATTAINS_AUs = ifelse(is.na(ATTAINS_AUs), "None", ATTAINS_AUs),
        LatitudeMeasure = as.numeric(LatitudeMeasure),
        LongitudeMeasure = as.numeric(LongitudeMeasure)
      )

    # Basemap for AOI:
    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldTopoMap",
        group = "World topo",
        options = leaflet::providerTileOptions(
          updateWhenZooming = FALSE,
          updateWhenIdle = TRUE
        )
      ) %>%
      leaflet::clearShapes() %>%
      leaflet::fitBounds(
        lng1 = min(sumdat$LongitudeMeasure),
        lat1 = min(sumdat$LatitudeMeasure),
        lng2 = max(sumdat$LongitudeMeasure),
        lat2 = max(sumdat$LatitudeMeasure)
      ) %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet::addLegend(
        position = "bottomright",
        colors = c("#DC851E", "#059FA4", "#A1A522", "black", NA),
        labels = c(
          "ATTAINS: Not Supporting", "ATTAINS: Supporting", "ATTAINS: Not Assessed", "Water Quality Observation(s)",
          "NHDPlus HR catchments containing water quality observations + ATTAINS feature are represented as clear polygons with black outlines."
        ),
        opacity = 1,
        title = "Legend"
      )

    # Add ATTAINS catchment outlines (if they exist):
    try(
      map <- map %>%
        leaflet::addPolygons(
          data = ATTAINS_catchments,
          color = "black",
          weight = 1, fillOpacity = 0,
          popup = paste0("NHDPlus HR Catchment ID: ", ATTAINS_catchments$nhdplusid)
        ),
      silent = TRUE
    )

    # Add ATTAINS polygon features (if they exist):
    try(
      map <- map %>%
        leaflet::addPolygons(
          data = polygons_mapper,
          color = ~ polygons_mapper$col,
          fill = ~ polygons_mapper$col,
          weight = 3, fillOpacity = 1,
          popup = paste0(
            "Assessment Unit Name: ", polygons_mapper$assessmentunitname,
            "<br> Assessment Unit ID: ", polygons_mapper$assessmentunitidentifier,
            "<br> Status: ", polygons_mapper$overallstatus,
            "<br> Assessment Unit Type: ", polygons_mapper$type,
            "<br> <a href=", polygons_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>"
          )
        ),
      silent = TRUE
    )

    # Add ATTAINS lines features (if they exist):
    try(
      map <- map %>%
        leaflet::addPolylines(
          data = lines_mapper,
          color = ~ lines_mapper$col,
          weight = 4, fillOpacity = 1,
          popup = paste0(
            "Assessment Unit Name: ", lines_mapper$assessmentunitname,
            "<br> Assessment Unit ID: ", lines_mapper$assessmentunitidentifier,
            "<br> Status: ", lines_mapper$overallstatus,
            "<br> Assessment Unit Type: ", lines_mapper$type,
            "<br> <a href=", lines_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>"
          )
        ),
      silent = TRUE
    )

    # Add ATTAINS point features (if they exist):
    try(
      map <- map %>%
        leaflet::addCircleMarkers(
          data = points_mapper,
          lng = ~X, lat = ~Y,
          color = ~ points_mapper$col, fillColor = ~ points_mapper$col,
          fillOpacity = 1, stroke = TRUE, weight = 1.5, radius = 5,
          popup = paste0(
            "Assessment Unit Name: ", points_mapper$assessmentunitname,
            "<br> Assessment Unit ID: ", points_mapper$assessmentunitidentifier,
            "<br> Status: ", points_mapper$overallstatus,
            "<br> Assessment Unit Type: ", points_mapper$type,
            "<br> <a href=", points_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>"
          )
        ),
      silent = TRUE
    )

    # Add WQP observation features (should always exist):
    try(
      map <- map %>%
        leaflet::addCircleMarkers(
          data = sumdat,
          lng = ~LongitudeMeasure, lat = ~LatitudeMeasure,
          color = "grey", fillColor = "black",
          fillOpacity = 0.8, stroke = TRUE, weight = 1.5, radius = 6,
          popup = paste0(
            "Site ID: ", sumdat$MonitoringLocationIdentifier,
            "<br> Site Name: ", sumdat$MonitoringLocationName,
            "<br> Measurement Count: ", sumdat$Sample_Count,
            "<br> Visit Count: ", sumdat$Visit_Count,
            "<br> Characteristic Count: ", sumdat$Parameter_Count,
            "<br> ATTAINS Assessment Unit(s): ", sumdat$ATTAINS_AUs
          )
        ),
      silent = TRUE
    )

    if (is.null(ATTAINS_lines) & is.null(ATTAINS_points) & is.null(ATTAINS_polygons)) {
      print("No ATTAINS data associated with this Water Quality Portal data.")
    }

    # Return leaflet map of TADA WQ and its associated ATTAINS data
    return(map)
  }))
}
