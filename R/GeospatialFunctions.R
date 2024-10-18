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
  
  print("Transforming your data into a spatial object.")
  
  suppressMessages(suppressWarnings({
    # Make a reference table for CRS and EPSG codes
    # List should include all codes in WQX domain (see HorizontalCoordinateReferenceSystemDatum CSV at https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads)
    epsg_codes <- tidyr::tribble(
      ~HorizontalCoordinateReferenceSystemDatumName, ~epsg,
      "NAD83", 4269,
      "WGS84", 4326,
      "NAD27", 4267,
      "UNKWN", crs,  # Unknowns and NAs should go to user supplied crs or default
      "Unknown", crs,
      "OTHER", crs,
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
    
    # Check the CRS column for NA or "UNKWN" and warn user if any are found
    if (any(is.na(.data$HorizontalCoordinateReferenceSystemDatumName)) ||
        any(.data$HorizontalCoordinateReferenceSystemDatumName %in% c("UNKWN", "Unknown", "OTHER"))) {
      print(paste0("Your WQP data frame contains observations without a listed coordinate reference system (CRS). For these, we have assigned CRS ", crs, "."))
    }
    # join our CRS reference table to our original WQP dataframe:
    sf <- .data %>%
      tibble::rowid_to_column(var = "index") %>%
      dplyr::mutate(
        lat = as.numeric(LatitudeMeasure),
        lon = as.numeric(LongitudeMeasure),
        # If `HorizontalCoordinateReferenceSystemDatumName` is NA...
        HorizontalCoordinateReferenceSystemDatumName = ifelse(is.na(HorizontalCoordinateReferenceSystemDatumName),
                                                              #... assign it the same crs as the user-supplied crs:
                                                              paste0(epsg_codes %>% dplyr::filter(epsg == as.numeric(crs)) %>% .[1,1]),
                                                              # otherwise, preserve the original crs
                                                              HorizontalCoordinateReferenceSystemDatumName)) %>%
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
#' Fetches ATTAINS features (state- or tribe- or other entity- submitted points, lines, and polygons representing their assessment units; and the EPA snapshot of the associated NHDPlus HR catchments that the state- or tribe- or other entity- submitted features fall within) within a bounding box produced from a set of TADA spatial features.
#'
#' @param .data A dataframe developed using `TADA_DataRetrieval()` or `TADA_MakeSpatial()`.
#' @param catchments_only Whether to return just the summarized ATTAINS catchment features, or both the catchments and raw ATTAINS features. TRUE or FALSE. 
#' @return Spatial features (ATTAINS_catchments, ATTAINS_points, ATTAINS_lines, and ATTAINS_polygons) that are within the spatial bounding box of water quality observations.
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
#' nv_attains_features <- fetchATTAINS(tada_data, catchments_only = FALSE)
#' }
fetchATTAINS <- function(.data, catchments_only = FALSE) {
  
  sf::sf_use_s2(FALSE)
  
  print("Depending on your data's observation count and its spatial range, the ATTAINS pull may take a while.")
  
  # EPSG we want our ATTAINS data to be in (always 4326 for this function)
  our_epsg <- 4326
  
  # If data is already spatial, just make sure it is in the right CRS
  # and add an index as the WQP observations' unique identifier...
  if (!is.null(.data) & inherits(.data, "sf")) {
    if (sf::st_crs(.data)$epsg != our_epsg) {
      .data <- .data %>%
        sf::st_transform(our_epsg) %>%
        dplyr::distinct(geometry, .keep_all = TRUE)
    } else {
      .data <- .data %>%
        dplyr::distinct(geometry, .keep_all = TRUE)
    }
  } else if (!"LongitudeMeasure" %in% colnames(.data) |
             !"LatitudeMeasure" %in% colnames(.data) |
             !"HorizontalCoordinateReferenceSystemDatumName" %in% colnames(.data)) {
    stop("The dataframe does not contain WQP-style latitude and longitude data (column names `HorizontalCoordinateReferenceSystemDatumName`, `LatitudeMeasure`, and `LongitudeMeasure`.")
  } else {
    # ... Otherwise transform into a spatial object then do the same thing:
    .data <- .data %>%
      data.table::data.table(.) %>%
      dplyr::distinct(LongitudeMeasure, LatitudeMeasure, .keep_all = TRUE) %>%
      # convert dataframe to a spatial object
      TADA_MakeSpatial(.data = ., crs = our_epsg)
    
  }
  
  if (is.null(.data) | nrow(.data) == 0) {
    stop("There is no data in your `data` object to use as a bounding box for selecting ATTAINS features.")
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
  
  # bounding box of user's WQP data
  suppressMessages(suppressWarnings({
    bbox_raw <- .data %>%
      sf::st_bbox(.)
    bbox <- bbox_raw %>%
      # convert bounding box to characters
      toString(.) %>%
      # encode for use within the API URL
      urltools::url_encode(.)
  }))
  
  
  feature_downloader <- function(baseurls, sf_bbox) {
    # starting at feature 1 (i.e., no offset):
    offset <- 0
    # empty list to store all features in
    all_features <- list()
    
    # The ATTAINS API has a limit of 2000 features that can be pulled in at once.
    # Therefore, we must split the call into manageable "chunks" using a moving
    # window of what features to pull in, then munging all the separate API calls
    # together.
    
    repeat {
      query <- urltools::param_set(baseurls, key = "geometry", value = sf_bbox) %>%
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
        
      }
    }
    
    all_features <- dplyr::bind_rows(all_features) %>%
      # remove duplicate features (precautionary)
      dplyr::distinct(.keep_all = TRUE)
  }
  
  # If the area of the bbox is massive (about the area of California or larger), AND there
  # aren't that many actual monitoring locations (100)... OR the bbox is about the size of New Hampshire, and the observations are under 25...
  #... speed up processing by going site-by-site:
  if(nrow(.data) <= 100 & as.numeric(sf::st_area(sf::st_as_sfc(bbox_raw))) >= 4e+11 || nrow(.data) <= 25 & as.numeric(sf::st_area(sf::st_as_sfc(bbox_raw))) >= 1e+11){
    
    catchment_features <- vector("list", length = nrow(.data))
    
    for(i in 1:nrow(.data)){
      
      # bounding box of user's WQP data
      suppressMessages(suppressWarnings({
        bbox <- .data[i,] %>%
          sf::st_buffer(0.0000001) %>%
          sf::st_bbox(.) %>%
          # convert bounding box to characters
          toString(.) %>%
          # encode for use within the API URL
          urltools::url_encode(.)
      }))
      
      catchment_features[[i]] <- feature_downloader(baseurls = baseurls[1], sf_bbox = bbox)
      
    }
    
    catchment_features <- catchment_features %>%
      purrr::keep(~ nrow(.) > 0) %>%
      dplyr::bind_rows()
    
    if(length(catchment_features) == 0 || is.null(catchment_features)){print("There are no ATTAINS features associated with your area of interest.")} else {

        ## GRABBING WATER TYPE:
    
    # Use ATTAINS API to grab, for each assessment unit, its WaterType.
    # Query the API in "chunks" so it doesn't break. Sweet spot is ~200:
    split_vector <- function(vector, chunk_size = 200) {
      # Number of chunks needed
      num_chunks <- ceiling(length(vector) / chunk_size)
      
      # Split the vector into chunks
      chunks <- split(vector, ceiling(seq_along(vector) / chunk_size))
      
      return(chunks)
    }
    
    
    all_units <- unique(catchment_features$assessmentunitidentifier)
    chunks <- split_vector(all_units)
    water_types <- vector("list", length = length(chunks))
    
    for(i in 1:length(chunks)){
      
      dat <- httr::GET(paste0("https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=", paste(chunks[[i]], collapse = ","))) %>%
        httr::content(., as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(.)
      
      water_types[[i]] <- dat[["items"]] %>%
        tidyr::unnest("assessmentUnits") %>%
        tidyr::unnest("waterTypes") %>%
        dplyr::select(assessmentUnitIdentifier,
                      waterTypeCode)
      
    }
    
    water_types <- dplyr::bind_rows(water_types)
    
    try(catchment_features <- dplyr::left_join(catchment_features, water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")))
    
    }
    
    # If only interested in grabbing catchment data, return just the catchments
    if(catchments_only == TRUE){
      return(list("ATTAINS_catchments" = catchment_features))
    }
    
    # Otherwise, start grabbing the raw ATTAINS features that intersect those
    # catchments
    points <- vector("list", length = nrow(catchment_features))
    lines <- vector("list", length = nrow(catchment_features))
    polygons <- vector("list", length = nrow(catchment_features))
    
    for(i in 1:nrow(catchment_features)){
      
      # bounding box of catchments
      suppressMessages(suppressWarnings({
        bbox <- catchment_features[i,] %>%
          sf::st_bbox(.) %>%
          # convert bounding box to characters
          toString(.) %>%
          # encode for use within the API URL
          urltools::url_encode(.)
      }))
      
      points[[i]] <- feature_downloader(baseurls = baseurls[2], sf_bbox = bbox)
      lines[[i]] <- feature_downloader(baseurls = baseurls[3], sf_bbox = bbox)
      polygons[[i]] <- feature_downloader(baseurls = baseurls[4], sf_bbox = bbox)
      
    }
    
    points <- points %>%
      purrr::keep(~ nrow(.) > 0) %>%
      dplyr::bind_rows()
    try(
      points <- points %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier"))
      , silent = TRUE)
    
    lines <- lines %>%
      purrr::keep(~ nrow(.) > 0) %>%
      dplyr::bind_rows()
    try(
      lines <- lines %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier"))
      , silent = TRUE)
    
    polygons <- polygons %>%
      purrr::keep(~ nrow(.) > 0) %>%
      dplyr::bind_rows()
    try(
      polygons <- polygons %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier"))
      , silent = TRUE)
    
    final_features <- list("ATTAINS_catchments" = catchment_features,
                           "ATTAINS_points" = points,
                           "ATTAINS_lines" = lines,
                           "ATTAINS_polygons" = polygons)
    
    return(final_features)
    
    # Otherwise, just use the bbox in one pull:
  } else {
    
    catchment_features <- feature_downloader(baseurls = baseurls[1], sf_bbox = bbox)
    
    if(length(catchment_features) == 0 || is.null(catchment_features)){print("There are no ATTAINS features associated with your area of interest.")} else{
    
    ## GRABBING WATER TYPE:
    
    # Use ATTAINS API to grab, for each assessment unit, its WaterType.
    # Query the API in "chunks" so it doesn't break:
    split_vector <- function(vector, chunk_size = 2000) {
      # Number of chunks needed
      num_chunks <- ceiling(length(vector) / chunk_size)
      
      # Split the vector into chunks
      chunks <- split(vector, ceiling(seq_along(vector) / chunk_size))
      
      return(chunks)
    }
    
    # Sweet spot for splitting up the assessment unit vector is ~200:
    
    all_units <- unique(catchment_features$assessmentunitidentifier)
    chunks <- split_vector(all_units, chunk_size = 200)
    water_types <- vector("list", length = length(chunks))
    
    for(i in 1:length(chunks)){
      
      dat <- httr::GET(paste0("https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=", paste(chunks[[i]], collapse = ","))) %>%
        httr::content(., as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(.)
      
      water_types[[i]] <- dat[["items"]] %>%
        tidyr::unnest("assessmentUnits") %>%
        tidyr::unnest("waterTypes") %>%
        dplyr::select(assessmentUnitIdentifier,
                      waterTypeCode)
      
    }
    
    water_types <- dplyr::bind_rows(water_types)
    
    try(catchment_features <- dplyr::left_join(catchment_features, water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")), silent = TRUE)
    }
    
    # If only interested in grabbing catchment data, return just the catchments
    if(catchments_only == TRUE){
      return(list("ATTAINS_catchments" = catchment_features))
    }
    
    # Otherwise, start grabbing the raw ATTAINS features that intersect those
    # catchments
    
    # bounding box of catchments
    try(suppressMessages(suppressWarnings({
      bbox <- catchment_features %>%
        sf::st_bbox(.) %>%
        # convert bounding box to characters
        toString(.) %>%
        # encode for use within the API URL
        urltools::url_encode(.)
    })), silent = TRUE)
    
    # Download associated point, line, and polygon features using catchment bbox
    other_features <- baseurls[2:4] %>%
      purrr::map(function(baseurl) {
        
        features <- feature_downloader(baseurls = baseurl, sf_bbox = bbox)
        
        if (!is.null(features) && nrow(features) > 0) {
          features <- try(dplyr::left_join(features, water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")),  silent = TRUE)
        }
        
        return(features)
        
      })
    
    final_features <- list("ATTAINS_catchments" = catchment_features,
                           "ATTAINS_points" = other_features[[1]],
                           "ATTAINS_lines" = other_features[[2]],
                           "ATTAINS_polygons" = other_features[[3]])
    
    return(final_features)
  }
}


#' fetchNHD
#'
#' Fetches NHD features from either the high resolution or medium resolution version of the National Hydrography Dataset (NHD) that intersect catchments containing TADA Water Quality Portal observations.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()` or the geospatial equivalent made by `TADA_MakeSpatial()`.
#' @param resolution Whether to download the NHDPlus HiRes resolution ("Hi") or medium NHDPlus V2 resolution ("Med") version of the National Hydrography Dataset (NHD). Default is "Hi".
#' @param features Which NHD features to return: "catchments", "flowlines", "waterbodies", or any combination.
#'
#' @return A list containing all selected NHD features associated with the WQP observations of interest. Or, if a single feature type is selected, a single geospatial object instead of a list. Default is "catchments" only.
#'
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_MakeSpatial()]
#'
#' @examples
#' \dontrun{
#' tada_data <- TADA_DataRetrieval(
#'   startDate = "1990-01-01",
#'   endDate = "1990-01-15",
#'   characteristicName = "pH",
#'   statecode = "CO",
#'   applyautoclean = TRUE
#' )
#'
#' nhd_data <- fetchNHD(.data = tada_data, resolution = "Hi", features = c("catchments", "waterbodies", "flowlines"))
#'
#' }
fetchNHD <- function(.data, resolution = "Hi", features = "catchments"){
  
  suppressMessages(suppressWarnings({
    
    # sf::sf_use_s2(TRUE)
    # If data is already spatial, just make sure it is in the right CRS
    if (!is.null(.data) & inherits(.data, "sf")) {
      if (sf::st_crs(.data)$epsg != 4326) {
        geospatial_data <- .data %>%
          sf::st_transform(4326)
      } else {
        geospatial_data <- .data
      }
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      geospatial_data <- .data %>%
        # convert dataframe to a spatial object
        TADA_MakeSpatial(.data = ., crs = 4326) %>%
        dplyr::mutate(geometry_join = geometry)
    }
    
  }))
  
  # Reduce WQP data to unique coordinates
  unique_sites <- dplyr::distinct(geospatial_data, geometry)
  
  # If user wants HighRes NHD...
  if(resolution %in% c("Hi", "hi")){
    suppressMessages(suppressWarnings({
      
      # Map server for NHDPlus_HR that is used to download features:
      nhd_plus_hr_url <- "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer"
      
      # bounding box of user's WQP data
      
      wqp_bboxes <- unique_sites %>%
        sf::st_buffer(1e-07) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(bbox = purrr::map(geometry, sf::st_bbox)) %>%
        sf::st_as_sfc()
      
      # open the nhd_hr - which contains a bunch of layers
      nhd_hr <- arcgislayers::arc_open(nhd_plus_hr_url)
      
      # list the layers of the nhdhr object
      # arcgislayers::list_items(nhd_hr)
      
      # select the layer by id from the items list called above (10 is HR catchments)
      nhd_hr_catchments <- arcgislayers::get_layer(nhd_hr, 10)
      
      # use bboxes of the sites to return their associated catchments
      nhd_catchments_stored <- vector("list", length = length(wqp_bboxes))
      
      for(i in 1:length(wqp_bboxes)){
        try(
          nhd_catchments_stored[[i]] <- arcgislayers::arc_select(nhd_hr_catchments,
                                                                 filter_geom = wqp_bboxes[i],
                                                                 crs = sf::st_crs(wqp_bboxes[i])) %>%
            sf::st_make_valid()
          , silent = TRUE)
      }
      
      nhd_catchments_stored <- nhd_catchments_stored %>%
        purrr::keep(~!is.null(.)) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct() 
      
      try(nhd_catchments_stored <- nhd_catchments_stored %>%
            dplyr::select(nhdplusid,
                          catchmentareasqkm = areasqkm) %>%
            dplyr::mutate(NHD.nhdplusid = as.character(nhdplusid),
                          NHD.resolution = "HR",
                          NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)) %>%
            dplyr::select(NHD.nhdplusid, NHD.resolution, NHD.catchmentareasqkm, geometry), silent = TRUE)
      
      
    }))
    
    # Empty version of the df will be returned if no associated catchments
    # to avoid breaking downstream fxns reliant on catchment info.
    if(nrow(nhd_catchments_stored) == 0 && "catchments" %in% features){
      print("No NHD HR features associated with your area of interest.")
      nhd_catchments_stored <- tibble::tibble(NHD.nhdplusid = character(),
                                              NHD.resolution = character(),
                                              NHD.catchmentareasqkm = numeric())
    }
    
    if(nrow(nhd_catchments_stored) == 0 && !"catchments" %in% features){
      stop("No NHD HR features associated with your area of interest.")
    }
    
    if(length(features) == 1 && features == "catchments") {
      return(nhd_catchments_stored)
    }
    
    # Grab flowlines -
    if("flowlines" %in% features && nrow(nhd_catchments_stored) > 0){
      
      suppressMessages(suppressWarnings({
        
        # use catchments to grab other NHD features
        geospatial_aoi <- nhd_catchments_stored %>%
          sf::st_as_sfc()
        
        # select the layer by id from the items list (3 is HR flowlines)
        nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)
        
        # use catchments to return associated flowlines
        nhd_flowlines_stored <- vector("list", length = length(geospatial_aoi))
        
        for(i in 1:length(geospatial_aoi)){
          try(
            nhd_flowlines_stored[[i]] <- arcgislayers::arc_select(nhd_hr_flowlines,
                                                                  # where = query,
                                                                  filter_geom = geospatial_aoi[i],
                                                                  crs = sf::st_crs(geospatial_aoi[i])) %>%
              sf::st_make_valid()
            , silent = TRUE)
          
          # so all returned meta data binds properly, must transform all columns into characters,
          # EXCEPT for the geometry column:
          try(
            geometry_col <- sf::st_geometry(nhd_flowlines_stored[[i]])
            , silent = TRUE)
          
          try(
            nhd_flowlines_stored[[i]] <- nhd_flowlines_stored[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
            , silent = TRUE)
        }
        
        nhd_flowlines_stored <- nhd_flowlines_stored %>%
          purrr::keep(~!is.null(.)) %>%
          purrr::keep(~!is.character(.)) %>%
          dplyr::bind_rows() %>%
          dplyr::distinct()
        
      }))
      
      if(length(features) == 1 && features == "flowlines") {
        
        if(length(nhd_flowlines_stored) == 0 || is.null(nhd_flowlines_stored)){print("There are no NHD flowlines associated with your area of interest.")}
        
        return(nhd_flowlines_stored)
      }
      
      if(length(nhd_flowlines_stored) == 0 || is.null(nhd_flowlines_stored)){print("There are no NHD flowlines associated with your area of interest.")}
      
    }
    
    # Grab waterbodies -
    if("waterbodies" %in% features & nrow(nhd_catchments_stored) > 0){
      suppressMessages(suppressWarnings({
        
        geospatial_aoi <- nhd_catchments_stored %>%
          sf::st_as_sfc()
        
        # select the layer by id from the items list called above (9 is HR waterbodies)
        nhd_hr_waterbodies <- arcgislayers::get_layer(nhd_hr, 9)
        
        # use catchments to return associated waterbodies
        nhd_waterbodies_stored <- vector("list", length = length(geospatial_aoi))
        
        for(i in 1:length(geospatial_aoi)){
          try(
            nhd_waterbodies_stored[[i]] <- arcgislayers::arc_select(nhd_hr_waterbodies,
                                                                    # where = query,
                                                                    filter_geom = geospatial_aoi[i],
                                                                    crs = sf::st_crs(geospatial_aoi[i])) %>%
              sf::st_make_valid()
            , silent = TRUE)
          
          # so all returned meta data binds properly, must transform all columns into characters,
          # EXCEPT for the geometry column:
          try(
            geometry_col <- sf::st_geometry(nhd_waterbodies_stored[[i]])
            , silent = TRUE)
          
          try(
            nhd_waterbodies_stored[[i]] <- nhd_waterbodies_stored[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
            , silent = TRUE)
          
        }
        
        nhd_waterbodies_stored <- nhd_waterbodies_stored %>%
          purrr::keep(~!is.null(.)) %>%
          purrr::keep(~!is.character(.)) %>%
          dplyr::bind_rows() %>%
          dplyr::distinct()
        
      }))
      
      if(length(features) == 1 && features == "waterbodies") {
        
        if(length(nhd_waterbodies_stored) == 0 || is.null(nhd_waterbodies_stored)){print("There are no NHD waterbodies associated with your area of interest.")}
        
        return(nhd_waterbodies_stored)
      }
      
      if(length(nhd_waterbodies_stored) == 0 || is.null(nhd_waterbodies_stored)){print("There are no NHD waterbodies associated with your area of interest.")}
      
    }
    
    # Combinations of features selected, and what they return:
    
    if(length(features) == 2 && "catchments" %in% features && "flowlines" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments_stored,
                       "NHD_flowlines" = nhd_flowlines_stored)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "catchments" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments_stored,
                       "NHD_waterbodies" = nhd_waterbodies_stored)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_flowlines" = nhd_flowlines_stored,
                       "NHD_waterbodies" = nhd_waterbodies_stored)
      
      return(nhd_list)
      
    } else if(length(features) == 3  && "catchments" %in% features && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments_stored,
                       "NHD_flowlines" = nhd_flowlines_stored,
                       "NHD_waterbodies" = nhd_waterbodies_stored)
      
    } else {stop("Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument.")}
    
    # If user wants NHDPlus V2...
  } else if(resolution %in% c("Med", "med")){
    
    suppressMessages(suppressWarnings({
      
      nhd_catchments <- vector("list", length = nrow(unique_sites))
      
      for(i in 1:nrow(unique_sites)){
        
        # Use {nhdplusTools} to grab associated catchments...
        try(nhd_catchments[[i]] <- nhdplusTools::get_nhdplus(AOI = unique_sites[i,], realization = "catchment") %>%
              sf::st_make_valid() %>%
              dplyr::select(comid = featureid,
                            catchmentareasqkm = areasqkm) %>%
              dplyr::mutate(NHD.comid = as.character(comid),
                            NHD.resolution = "nhdplusV2",
                            NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)) %>%
              dplyr::select(NHD.comid, NHD.resolution, NHD.catchmentareasqkm, geometry)
            , silent = TRUE)
        
      }
      
      nhd_catchments <- nhd_catchments %>%
        purrr::keep(~!is.null(.))
      
      try(nhd_catchments <- dplyr::bind_rows(nhd_catchments) %>%
            dplyr::distinct(), silent = TRUE)
      
      # if NHD catchments are not in the correct CRS, transform them
      try(if (sf::st_crs(nhd_catchments) != sf::st_crs(geospatial_data)) {
        nhd_catchments <- nhd_catchments %>%
          sf::st_transform(sf::st_crs(geospatial_data)$epsg)
      }, silent = TRUE)
      
    }))
    
    if(nrow(nhd_catchments) == 0 && "catchments" %in% features){
      print("No NHDPlus V2 features associated with your WQP observations.")
      nhd_catchments <- tibble::tibble(NHD.comid = character(),
                                       NHD.resolution = character(),
                                       NHD.catchmentareasqkm = numeric())
    }
    
    if(nrow(nhd_catchments) == 0 && !"catchments" %in% features){
      stop("No NHDPlus V2 features associated with your WQP observations.")
    }
    
    if(length(features) == 1 && features == "catchments") {
      return(nhd_catchments)
    }
    
    
    # Grab flowlines -
    if("flowlines" %in% features && nrow(nhd_catchments) > 0){
      suppressMessages(suppressWarnings({
        
        nhd_flowlines <- vector("list", length = nrow(nhd_catchments))
        
        # use catchments to grab other NHD features:
        unique_sites <- nhd_catchments
        
        for(i in 1:nrow(unique_sites)){
          
          # Use {nhdplusTools} to grab associated flowlines...
          try(nhd_flowlines[[i]] <- nhdplusTools::get_nhdplus(AOI = unique_sites[i,], realization = "flowline") %>%
                sf::st_make_valid()
              , silent = TRUE)
          
          try(geometry_col <- sf::st_geometry(nhd_flowlines[[i]])
              , silent = TRUE)
          
          try(nhd_flowlines[[i]] <- nhd_flowlines[[i]] %>%
                dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
              , silent = TRUE)
          
        }
        
        nhd_flowlines <- nhd_flowlines %>%
          purrr::keep(~!is.null(.))
        
        try(nhd_flowlines <- dplyr::bind_rows(nhd_flowlines)) %>%
          dplyr::distinct()
        
        # if NHD flowlines are not in the correct CRS, transform them
        try(if (sf::st_crs(nhd_flowlines) != sf::st_crs(geospatial_data)) {
          nhd_flowlines <- nhd_flowlines %>%
            sf::st_transform(sf::st_crs(geospatial_data)$epsg)
        }, silent = TRUE)
        
      }))
      
      if(nrow(nhd_flowlines) == 0 && "flowlines" %in% features){
        print("No NHDPlus V2 flowlines associated with your WQP observations.")
      }
      
      if(length(features) == 1 && features == "flowlines") {
        return(nhd_flowlines)
      }
    }
    
    # Grab waterbodies -
    if("waterbodies" %in% features && nrow(nhd_catchments) > 0){
      suppressMessages(suppressWarnings({
        
        nhd_waterbodies <- vector("list", length = nrow(nhd_catchments))
        
        # use catchments to grab other NHD features:
        unique_sites <- nhd_catchments
        
        for(i in 1:nrow(unique_sites)){
          
          # Use {nhdplusTools} to grab associated flowlines...
          try(nhd_waterbodies[[i]] <- nhdplusTools::get_waterbodies(AOI = unique_sites[i,]) %>%
                sf::st_make_valid()
              , silent = TRUE)
          
          try(geometry_col <- sf::st_geometry(nhd_waterbodies[[i]])
              , silent = TRUE)
          
          try(nhd_waterbodies[[i]] <- nhd_waterbodies[[i]] %>%
                dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
              , silent = TRUE)
          
        }
        
        nhd_waterbodies <- nhd_waterbodies %>%
          purrr::keep(~!is.null(.))
        
        try(nhd_waterbodies <- dplyr::bind_rows(nhd_waterbodies) %>%
              dplyr::distinct(),
            silent = TRUE)
        
        # if NHD waterbodies are not in the correct CRS, transform them
        try(if (sf::st_crs(nhd_waterbodies) != sf::st_crs(geospatial_data)) {
          nhd_waterbodies <- nhd_waterbodies %>%
            sf::st_transform(sf::st_crs(geospatial_data)$epsg)
        }, silent = TRUE)
        
      }))
      
      if(nrow(nhd_waterbodies) == 0 && "waterbodies" %in% features){
        print("No NHDPlus V2 waterbodies associated with your WQP observations.")
      }
      
      if(length(features) == 1 && features == "waterbodies") {
        return(nhd_waterbodies)
      }
    }
    
    # Combinations of features selected, and what they return:
    
    if(length(features) == 2 && "catchments" %in% features && "flowlines" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_flowlines" = nhd_flowlines)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "catchments" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_waterbodies" = nhd_waterbodies)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_flowlines" = nhd_flowlines,
                       "NHD_waterbodies" = nhd_waterbodies)
      
      return(nhd_list)
      
    } else if(length(features) == 3  && "catchments" %in% features && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_flowlines" = nhd_flowlines,
                       "NHD_waterbodies" = nhd_waterbodies)
      
    } else {stop("Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument.")}
    
  } else {
    stop('User-supplied resolution unavailable. Please select between "Med" or "Hi".')
  }
}



#' TADA_GetATTAINS
#'
#' Link catchment-based ATTAINS assessment unit data (EPA snapshot of NHDPlus HR catchments associated with entity submitted assessment unit features - points, lines, and polygons) to Water Quality Portal observations, often imported via `TADA_DataRetrieval()`. This function returns the objects that can be mapped in `TADA_ViewATTAINS()`. Check out the
#' TADAModule2.Rmd for an example workflow.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()` or the sf equivalent made by `TADA_MakeSpatial()`.
#' @param fill_catchments Whether the user would like to return NHD catchments for WQP observations not associated with an ATTAINS assessment unit (TRUE or FALSE). Defaults to FALSE.
#' @param resolution If fill_catchments = TRUE, whether to use NHDPlus V2 "Med" catchments or NHDPlus HiRes "Hi" catchments. Default is NHDPlus HiRes ("Hi").
#' @param return_sf Whether to return the associated catchments, lines, points, and polygon shapefile objects along with the data frame(s). TRUE (yes, return) or FALSE (no, do not return). All shapefile features are in WGS84 (crs = 4326). Defaults to TRUE.
#'
#' @return A modified `TADA_DataRetrieval()` dataframe with additional columns associated with the ATTAINS assessment unit data, and, if fill_catchments = TRUE, an additional dataframe of the observations without intersecting ATTAINS features.
#' Moreover, if return_sf = TRUE, this function will additionally return the raw ATTAINS and catchment shapefile features associated with those observations.
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
#'   endDate = "2018-07-31",
#'   characteristicName = "pH",
#'   statecode = "IL",
#'   applyautoclean = TRUE
#' )
#'
#' tada_attains <- TADA_GetATTAINS(tada_data, fill_catchments = FALSE, return_sf = FALSE)
#'
#' tada_attains_sf <- TADA_GetATTAINS(tada_data, fill_catchments = FALSE, return_sf = TRUE)
#'
#' tada_attains_filled <- TADA_GetATTAINS(tada_data, fill_catchments = TRUE, resolution = "Hi", return_sf = FALSE)
#'
#' tada_attains_filled_sf <- TADA_GetATTAINS(tada_data, fill_catchments = TRUE, resolution = "Hi", return_sf = TRUE)
#'
#' }
TADA_GetATTAINS <- function(.data, fill_catchments = FALSE, resolution = "Hi", return_sf = TRUE) {
  
  sf::sf_use_s2(FALSE)
  
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
    "ATTAINS.catchmentstatecode", "ATTAINS.catchmentresolution", "ATTAINS.waterTypeCode",
    "ATTAINS.Shape_Area"
  )
  
  if (any(attains_names %in% colnames(.data))) {
    stop("Your data has already been joined with ATTAINS data.")
  }
  
  if (nrow(.data) == 0) {
    
    print("Your Water Quality Portal dataframe has no observations. Returning an empty dataframe with empty ATTAINS features.")
    
    # if no WQP observations, return a modified `data` with empty ATTAINS-related columns:
    
    # Add ATTAINS columns with NA values
    col_val_list <- stats::setNames(
      object = rep(
        x = list(NA),
        times = length(attains_names)
      ),
      nm = attains_names
    )
    
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
  
  if(return_sf == TRUE){
  # grab all ATTAINS features that intersect our WQP objects:
  attains_features <- try(fetchATTAINS(.data = TADA_DataRetrieval_data), silent = TRUE)
  }
  
  if(return_sf == FALSE){
    # grab all ATTAINS features that intersect our WQP objects:
    attains_features <- try(fetchATTAINS(.data = TADA_DataRetrieval_data, catchments_only = FALSE), silent = TRUE)
  }
  
  # Tidy up the intersecting catchment objects:
  suppressMessages(suppressWarnings({
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
    if(is.null(nearby_catchments) || nrow(nearby_catchments) == 0){
      nearby_catchments <- NULL
    }
    
  }))
  
  # If no ATTAINS data associated with WQP obs...
  if (is.null(nearby_catchments)) {
    
    col_val_list <- stats::setNames(
      object = rep(
        x = list(NA),
        times = length(attains_names)
      ),
      nm = attains_names
    )
    
    # ...return a modified `.data` with empty ATTAINS-related columns:
    no_ATTAINS_data <- .data %>%
      dplyr::bind_cols(col_val_list) %>%
      tibble::rowid_to_column(var = "index")
    
    print("There are no ATTAINS catchments associated with these WQP observations. Returning an empty data frame for `TADA_with_ATTAINS`.")
    
    if(fill_catchments == FALSE){
      
      # If there are no intersecting ATTAINS catchments, fill_catchments = FALSE, and return_sf = TRUE, return empty sf features with the
      # empty TADA_with_ATTAINS df.
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
        # If there are no intersecting ATTAINS catchments, fill_catchments = FALSE, and return_sf = FALSE, just return the
        # empty TADA_with_ATTAINS df.
        return(no_ATTAINS_data)
      }
      
      
    } else if(fill_catchments == TRUE){
      
      # "Downloading NHD data to fill in missing ATTAINS features. Depending on the number of observations and their spatial extent, this might take a while...
      nhd_catchments <- fetchNHD(.data = TADA_DataRetrieval_data, resolution = resolution)
      
      TADA_without_ATTAINS <- TADA_DataRetrieval_data %>%
        sf::st_join(nhd_catchments, left = TRUE)
      
      # If there are no intersecting ATTAINS catchments, fill_catchments = TRUE, and return_sf = TRUE, return empty sf features with the
      # empty TADA_with_ATTAINS df PLUS the intersecting NHD catchment features of choice and TADA_without_ATTAINS dataframe.
      if (return_sf == TRUE) {
        ATTAINS_catchments <- NULL
        ATTAINS_lines <- NULL
        ATTAINS_points <- NULL
        ATTAINS_polygons <- NULL
        
        return(list(
          # must remove all obs from TADA_with_ATTAINS, since all exist in TADA_without_ATTAINS
          "TADA_with_ATTAINS" = no_ATTAINS_data[0,],
          "TADA_without_ATTAINS" = TADA_without_ATTAINS,
          "ATTAINS_catchments" = ATTAINS_catchments,
          "ATTAINS_points" = ATTAINS_points,
          "ATTAINS_lines" = ATTAINS_lines,
          "ATTAINS_polygons" = ATTAINS_polygons,
          "without_ATTAINS_catchments" = nhd_catchments
        ))
        
        # If there are no intersecting ATTAINS catchments, fill_catchments = TRUE, and return_sf = FALSE return empty sf features with the
        # empty TADA_with_ATTAINS df PLUS just the TADA_without_ATTAINS df (i.e., no shapefiles returned).
      } else {
        return(list("TADA_with_ATTAINS" = no_ATTAINS_data[0,],
                    "TADA_without_ATTAINS" = TADA_without_ATTAINS))
      }
    }
  }
  
  # If there IS at least some ATTAINS data, and fill_catchments = FALSE...
  if (!is.null(nearby_catchments) & fill_catchments == FALSE) {
    suppressMessages(suppressWarnings({
      # ... link WQP features to the ATTAINS catchment feature(s) they land in:
      TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
        # (left join = TRUE to preserve all observations (with or without ATTAINS features):)
        sf::st_join(., nearby_catchments, left = TRUE)
      
      # If there are intersecting ATTAINS catchments, fill_catchments = FALSE, and return_sf = FALSE, return just the
      #  TADA_with_ATTAINS df
      if (return_sf == FALSE) {
        return(TADA_with_ATTAINS)
      }
      # ... otherwise return_sf = TRUE, and therefore must grab ATTAINS features, too:
      
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
      if(is.null(ATTAINS_points) || nrow(ATTAINS_points) == 0){
        ATTAINS_points <- NULL
      }
      
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
      if(is.null(ATTAINS_lines) || nrow(ATTAINS_lines) == 0){
        ATTAINS_lines <- NULL
      }
      
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
      if(is.null(ATTAINS_polygons) || nrow(ATTAINS_polygons) == 0){
        ATTAINS_polygons <- NULL
      }
      
    }))
    # If there are ATTAINS catchments, fill_catchments = FALSE, and return_sf = TRUE:
    return(list(
      "TADA_with_ATTAINS" = TADA_with_ATTAINS,
      "ATTAINS_catchments" = ATTAINS_catchments,
      "ATTAINS_points" = ATTAINS_points,
      "ATTAINS_lines" = ATTAINS_lines,
      "ATTAINS_polygons" = ATTAINS_polygons
    ))
    
    # If there IS at least some ATTAINS data, and fill_catchments = TRUE...
  } else if (!is.null(nearby_catchments) & fill_catchments == TRUE) {
    # ... link WQP features to the ATTAINS catchment feature(s) they land in:
    TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
      sf::st_join(., nearby_catchments, left = TRUE)
    
    missing_attains <- dplyr::filter(TADA_with_ATTAINS, is.na(ATTAINS.submissionid))
    
    # Splitting up sites with and without ATTAINS, so remove those without ATTAINS:
    TADA_with_ATTAINS <- TADA_with_ATTAINS %>%
      dplyr::filter(!is.na(ATTAINS.submissionid))
    
    # If there are no WQP observations without missing ATTAINS features, return empty df for
    # TADA_without_ATTAINS
    if(nrow(missing_attains) == 0){
      
      print('All WQP features intersect an ATTAINS catchment. Returning empty dataframe for "TADA_without_ATTAINS".')
      
      if(resolution %in% c("Med", "med")){
        TADA_without_ATTAINS <- tibble::tibble(NHD.comid = character(),
                                               NHD.resolution = character(),
                                               NHD.catchmentareasqkm = numeric())
        nhd_catchments <- NULL
      } else if(resolution %in% c("Hi", "hi")){
        TADA_without_ATTAINS <- tibble::tibble(NHD.nhdplusid = character(),
                                               NHD.resolution = character(),
                                               NHD.catchmentareasqkm = numeric())
        
        nhd_catchments <- NULL
      } else {stop('Please select between "Med" or "Hi" for your NHD resolution.')}
    }
    
    # If there are some observations with no attains features, grab those sites' intersecting NHD catchments:
    if(nrow(missing_attains) > 0){
      
      # Downloading NHD data to fill in missing ATTAINS features. Depending on the number of observations and
      # their spatial extent, this can take a while.
      nhd_catchments <- fetchNHD(.data = missing_attains,
                                 resolution = resolution)
      
      TADA_without_ATTAINS <- missing_attains %>%
        # left join = TRUE to preserve all observations:
        sf::st_join(., nhd_catchments, left = TRUE)
      
      # if there are intersecting ATTAINS, fill_catchments = TRUE, and if return_sf = FALSE, return just the dfs:
      if (return_sf == FALSE) {
        return(list("TADA_with_ATTAINS" = TADA_with_ATTAINS,
                    "TADA_without_ATTAINS" = TADA_without_ATTAINS))
      }
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
    if(is.null(ATTAINS_points) || nrow(ATTAINS_points) == 0){
      ATTAINS_points <- NULL
    }
    
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
    if(is.null(ATTAINS_lines) || nrow(ATTAINS_lines) == 0){
      ATTAINS_lines <- NULL
    }
    
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
    if(is.null(ATTAINS_polygons) || nrow(ATTAINS_polygons) == 0){
      ATTAINS_polygons <- NULL
    }
    
    # if there is ATTAINS catchment data, fill_catchments = TRUE, return_sf = TRUE, return everything!
    return(list(
      "TADA_with_ATTAINS" = TADA_with_ATTAINS,
      "TADA_without_ATTAINS" = TADA_without_ATTAINS,
      "ATTAINS_catchments" = ATTAINS_catchments,
      "ATTAINS_points" = ATTAINS_points,
      "ATTAINS_lines" = ATTAINS_lines,
      "ATTAINS_polygons" = ATTAINS_polygons,
      "without_ATTAINS_catchments" = nhd_catchments))
    
  } #else {stop("Some combination of arguments is impossible.")}
  
}


#' TADA_ViewATTAINS
#'
#' Visualizes the data returned from TADA_GetATTAINS if return_sf was set to TRUE.
#'
#' This function visualizes the shapefile features generated with TADA_GetATTAINS and the associated
#' TADA Water Quality Portal monitoring locations used to find the ATTAINS features. For the function to work properly,
#' .data must be the list produced from `TADA_GetATTAINS()`
#' with `return_sf = TRUE`. Check out the
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
    "ATTAINS_catchments", "ATTAINS_points",
    "ATTAINS_lines", "ATTAINS_polygons"
  ) %in% names(.data))) {
    stop("Your input dataframe was not produced from `TADA_GetATTAINS(return_sf = TRUE)`, or it was modified. Please create your list of ATTAINS features using `TADA_GetATTAINS()` and confirm that return_sf had been set to TRUE.")
  }
  
  ATTAINS_table <- .data[["TADA_with_ATTAINS"]]
  ATTAINS_catchments <- .data[["ATTAINS_catchments"]]
  ATTAINS_points <- .data[["ATTAINS_points"]]
  ATTAINS_lines <- .data[["ATTAINS_lines"]]
  ATTAINS_polygons <- .data[["ATTAINS_polygons"]]
  
  # ATTAINS API seems to be missing some AU data that is still preserved in the catchment layer.
  # Use catchments for those instances for mapping purposes:
  missing_raw_features <- NULL
  
  try(missing_raw_features <- ATTAINS_catchments %>%
        dplyr::filter(!assessmentunitidentifier %in% c(ATTAINS_points$assessmentunitidentifier,
                                                       ATTAINS_lines$assessmentunitidentifier,
                                                       ATTAINS_polygons$assessmentunitidentifier)), silent = TRUE)
  
  if(!"without_ATTAINS_catchments" %in% names(.data)){
    
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
        col = c(tada.pal[3], tada.pal[4], tada.pal[7]),
        dark_col = c(tada.pal[12], tada.pal[6], tada.pal[11]),
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
      
      # CATCHMENT FEATURES - try to pull missing feature AU data if it exists. Otherwise, move on...
      try(
        missing_raw_mapper <- missing_raw_features %>%
          dplyr::left_join(., colors, by = "overallstatus") %>%
          dplyr::mutate(type = "Raw Feature Unavailable"),
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
          colors = c(tada.pal[3], tada.pal[4], tada.pal[7], "black", NA),
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
      
      # Add ATTAINS catchment outlines as AUs:
      try(
        map <- map %>%
          leaflet::addPolygons(
            data = missing_raw_mapper,
            color = ~ missing_raw_mapper$col,
            fill = ~ missing_raw_mapper$col,
            weight = 3, fillOpacity = 0.25,
            popup = paste0(
              "Assessment Unit Name: ", missing_raw_mapper$assessmentunitname,
              "<br> Assessment Unit ID: ", missing_raw_mapper$assessmentunitidentifier,
              "<br> Status: ", missing_raw_mapper$overallstatus,
              "<br> Assessment Unit Type: ", missing_raw_mapper$type,
              "<br> <a href=", missing_raw_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>",
              "<br> NHDPlus HR Catchment ID: ", missing_raw_mapper$nhdplusid
            )
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
  
  if("without_ATTAINS_catchments" %in% names(.data)){
    
    without_ATTAINS_table <- .data[["TADA_without_ATTAINS"]]
    
    if (nrow(ATTAINS_table) == 0 & nrow(without_ATTAINS_table) == 0) {
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
    
    without_ATTAINS_catchments <- NULL
    try(without_ATTAINS_catchments <- .data[["without_ATTAINS_catchments"]] %>%
          dplyr::rename(nhd = 1), silent = TRUE)
    
    suppressMessages(suppressWarnings({
      sf::sf_use_s2(FALSE)
      
      # if data was spatial, remove for downstream leaflet dev.
      # But first if no data in the ATTAINS table, add in required column names to
      # without ATTAINS data:
      if(nrow(ATTAINS_table) == 0) {
        
        new_columns <- names(ATTAINS_table)[grep("^ATTAINS\\.", names(ATTAINS_table))]
        ATTAINS_table <- without_ATTAINS_table %>%
          sf::st_drop_geometry()
        
        ATTAINS_table[new_columns] <- NA
        
      } else {
        
        ATTAINS_table <- ATTAINS_table %>%
          sf::st_drop_geometry() %>%
          dplyr::bind_rows(without_ATTAINS_table)
        
      }
      
      tada.pal <- TADA_ColorPalette()
      
      colors <- data.frame(
        overallstatus = c("Not Supporting", "Fully Supporting", "Not Assessed"),
        col = c(tada.pal[3], tada.pal[4], tada.pal[7]),
        dark_col = c(tada.pal[12], tada.pal[6], tada.pal[11]),
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
      
      # CATCHMENT FEATURES - try to pull missing feature AU data if it exists. Otherwise, move on...
      try(
        missing_raw_mapper <- missing_raw_features %>%
          dplyr::left_join(., colors, by = "overallstatus") %>%
          dplyr::mutate(type = "Raw Feature Unavailable"),
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
          colors = c(tada.pal[3], tada.pal[4], tada.pal[7], "black", NA, NA),
          labels = c(
            "ATTAINS: Not Supporting", "ATTAINS: Supporting", "ATTAINS: Not Assessed", "Water Quality Observation(s)",
            "NHDPlus HiRes catchments containing a WQP site + ATTAINS feature(s) are represented as clear polygons.",
            "Intersecting grey catchments are those without an ATTAINS feature if fill_catchments = TRUE."
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
      
      # Add missing catchment outlines (if they exist):
      try(
        map <- map %>%
          leaflet::addPolygons(
            data = without_ATTAINS_catchments,
            color = "black", fillColor = "grey",
            weight = 1, fillOpacity = 0.3,
            popup = paste0(without_ATTAINS_catchments$NHD.resolution, " catchment ID: ", without_ATTAINS_catchments$nhd)
          ),
        silent = TRUE
      )
      
      # Add ATTAINS catchment outlines as AUs:
      try(
        map <- map %>%
          leaflet::addPolygons(
            data = missing_raw_mapper,
            color = ~ missing_raw_mapper$col,
            fill = ~ missing_raw_mapper$col,
            weight = 3, fillOpacity = 0.25,
            popup = paste0(
              "Assessment Unit Name: ", missing_raw_mapper$assessmentunitname,
              "<br> Assessment Unit ID: ", missing_raw_mapper$assessmentunitidentifier,
              "<br> Status: ", missing_raw_mapper$overallstatus,
              "<br> Assessment Unit Type: ", missing_raw_mapper$type,
              "<br> <a href=", missing_raw_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>",
              "<br> NHDPlus HR Catchment ID: ", missing_raw_mapper$nhdplusid
            )
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
}
