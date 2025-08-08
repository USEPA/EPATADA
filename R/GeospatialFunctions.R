#' TADA_MakeSpatial
#'
#' Transform a Water Quality Portal dataframe into a geospatial sf object.
#'
#' Adds one new column to input dataframe, 'geometry', which allows for mapping and additional
#' geospatial capabilities. Check out the TADAModule2.Rmd for an example workflow.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()`.
#' @param crs The coordinate reference system (CRS) you would like the returned point features to
#' be in. The default is CRS 4326 (WGS84).
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
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' # make `tada_not_spatial` an sf object, projected in crs = 4269 (NAD83)
#' tada_spatial <- TADA_MakeSpatial(tada_not_spatial, crs = 4269)
#' }
#'
TADA_MakeSpatial <- function(.data, crs = 4326) {
  if (!"TADA.LongitudeMeasure" %in% names(.data) |
    !"TADA.LatitudeMeasure" %in% names(.data) |
    !"HorizontalCoordinateReferenceSystemDatumName" %in% names(.data)) {
    stop("The dataframe does not contain TADA-style latitude and longitude data (column names `HorizontalCoordinateReferenceSystemDatumName`, `TADA.LatitudeMeasure`, and `TADA.LongitudeMeasure`.")
  } else if (!is.null(.data) & inherits(.data, "sf")) {
    stop("Your data is already a spatial object.")
  }
  message("Transforming your data into a spatial object.")

  suppressMessages(suppressWarnings({
    # Make a reference table for CRS and EPSG codes
    # List should include all codes in WQX domain (see HorizontalCoordinateReferenceSystemDatum CSV at https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads)
    epsg_codes <- tidyr::tribble(
      ~HorizontalCoordinateReferenceSystemDatumName, ~epsg,
      "NAD83", 4269,
      "WGS84", 4326,
      "NAD27", 4267,
      "UNKWN", crs, # Unknowns and NAs should go to user supplied crs or default
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
      message(paste0("Your WQP dataframe contains observations without a listed coordinate reference system (CRS). For these, we have assigned CRS ", crs, "."))
    }
    # join our CRS reference table to our original WQP dataframe:
    sf <- .data %>%
      tibble::rowid_to_column(var = "index") %>%
      dplyr::mutate(
        lat = as.numeric(TADA.LatitudeMeasure),
        lon = as.numeric(TADA.LongitudeMeasure),
        # If `HorizontalCoordinateReferenceSystemDatumName` is NA...
        HorizontalCoordinateReferenceSystemDatumName = ifelse(is.na(HorizontalCoordinateReferenceSystemDatumName),
          # ... assign it the same crs as the user-supplied crs:
          paste0(epsg_codes %>% dplyr::filter(epsg == as.numeric(crs)) %>% .[1, 1]),
          # otherwise, preserve the original crs
          HorizontalCoordinateReferenceSystemDatumName
        )
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
#' Fetches ATTAINS features (state- or tribe- or other entity- submitted points, lines, and polygons
#' representing their assessment units; and the EPA snapshot of the associated NHDPlus HR catchments
#' that the state- or tribe- or other entity- submitted features fall within) within a bounding box
#' produced from a set of TADA spatial features.
#'
#' @param .data A dataframe developed using `TADA_DataRetrieval()` or `TADA_MakeSpatial()`.
#' @param catchments_only Whether to return just the summarized ATTAINS catchment features, or both
#' the catchments and raw ATTAINS features. TRUE or FALSE.
#' @return Spatial features (ATTAINS_catchments, ATTAINS_points, ATTAINS_lines, and
#' ATTAINS_polygons) that are within the spatial bounding box of water quality observations.
#'
#' @seealso [TADA_MakeSpatial()]
#' @seealso [TADA_DataRetrieval()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tada_data <- TADA_DataRetrieval(
#'   startDate = "1990-01-01",
#'   endDate = "1990-12-30",
#'   characteristicName = "pH",
#'   statecode = "NV",
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' nv_attains_features <- EPATADA:::fetchATTAINS(tada_data, catchments_only = FALSE)
#' }
fetchATTAINS <- function(.data, catchments_only = FALSE) {
  # function settings that we ensure go back to their original settings
  # after the function stops running:
  original_s2 <- sf::sf_use_s2() # Store the original s2 setting first
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))), add = TRUE)

  message("Depending on your data's observation count and its spatial range, the ATTAINS pull may take a while.")

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

  # REST for ATTAINS geospatial data:
  baseurls <- c( # ATTAINS catchments:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?",
    # ATTAINS points:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?",
    # ATTAINS lines:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?",
    # ATTAINS polygons:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?"
  )

  # function to download ATTAINS features based on a
  # specified bbox:
  fetch_bbox <- function(baseurls, sf_bbox) {
    # starting at feature 1 (i.e., no offset):
    offset <- 0
    # empty list to store all features in
    all_features <- list()

    # The ATTAINS API has a limit of 1000 features that can be pulled in at once.
    # Therefore, we must split the call into manageable "chunks" using a moving
    # window of what features to pull in, then munging all the separate API calls
    # together.

    repeat {
      query <- urltools::param_set(baseurls, key = "geometry", value = sf_bbox) %>%
        urltools::param_set(key = "inSR", value = our_epsg) %>%
        # Total of 1000 features at a time...
        urltools::param_set(key = "resultRecordCount", value = 1000) %>%
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
            geojsonsf::geojson_sf(url(query))
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
      # once done, change offset by 1000 features:
      offset <- offset + 1000
    }

    all_features <- dplyr::bind_rows(all_features) %>%
      # remove duplicate features (precautionary)
      dplyr::distinct(.keep_all = TRUE)
  }

  # function to download ATTAINS features API based on their name
  fetch_au <- function(baseurls, assessment_unit_ids) {
    # Split the assessment_unit_ids into chunks of 1000
    # API cannot handle more than 1000 features
    id_chunks <- split(assessment_unit_ids, ceiling(seq_along(assessment_unit_ids) / 1000))

    # Query API for a chunk of assessment unit IDs
    fetch_chunk <- function(id_chunk) {
      where_clause <- paste0("assessmentunitidentifier IN ('", paste(id_chunk, collapse = "','"), "')")
      query_params <- list(
        where = where_clause,
        outFields = "*",
        f = "geojson"
      )

      response <- httr::GET(baseurls, query = query_params)

      if (httr::status_code(response) != 200) {
        stop("Failed to retrieve data from EPA ATTAINS API.")
      }

      geojson_data <- httr::content(response, as = "text", encoding = "UTF-8")
      sf_object <- sf::st_read(geojson_data, quiet = TRUE)

      return(sf_object)
    }

    # fetch all chunks and combine results
    purrr::map_dfr(id_chunks, fetch_chunk)
  }

  # function used to grab assessment unit "WaterType".
  # (sweet spot chunk size wise is 50):
  grab_waterbody_type <- function(au_list, chunk_size = 50) {
    # Number of chunks needed
    num_chunks <- ceiling(length(au_list) / chunk_size)

    # split the au_list into chunks
    chunks <- split(au_list, ceiling(seq_along(au_list) / chunk_size))

    water_types <- vector("list", length = length(chunks))

    for (i in 1:length(chunks)) {
      dat <- httr::GET(utils::URLencode(paste0("https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=", paste(chunks[[i]], collapse = ",")))) %>%
        httr::content(., as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(.)

      water_types[[i]] <- dat[["items"]] %>%
        tidyr::unnest("assessmentUnits") %>%
        tidyr::unnest("waterTypes") %>%
        dplyr::select(
          assessmentUnitIdentifier,
          waterTypeCode
        )
    }
    return(dplyr::bind_rows(water_types))
  }

  # FOR AOIs THAT ARE GREATER THAN 6,000 sqkm, split into "clusters":
  if (as.numeric(sf::st_area(sf::st_as_sfc(.data %>% sf::st_bbox(.)))) >= 6e+9) {
    # For user-specified AOIs with a large spatial range, create "clusters" of sites
    # whose bounding boxes are smaller.
    perform_iterative_clustering <- function(points_sf, min_area = 6e+9, max_iterations = 100) {
      # fxn to calculate each cluster's bounding box area
      bbox_area <- function(df, clust) {
        df %>%
          dplyr::filter(cluster == clust) %>%
          sf::st_bbox() %>%
          sf::st_as_sfc() %>%
          sf::st_area() %>%
          tidyr::as_tibble() %>%
          dplyr::mutate(cluster = clust)
      }

      # cluster maker fxn:
      cluster_iteration <- function(points, eps, min_pts, iteration) {
        coords <- sf::st_coordinates(points)
        fr <- dbscan::frNN(coords, eps = eps)
        clusters <- dbscan::dbscan(fr, minPts = min_pts)$cluster

        # create unique cluster IDs that include iteration number
        cluster_ids <- ifelse(clusters == -1,
          paste0("noise_", iteration),
          paste0("cluster_", iteration, "_", clusters)
        )

        points %>%
          dplyr::mutate(
            cluster = cluster_ids,
            iteration = iteration
          )
      }

      # function to check if any clusters still exceed our bbox area threshold
      has_large_clusters <- function(points) {
        if (nrow(points) == 0) {
          return(FALSE)
        }

        areas <- unique(points$cluster) %>%
          purrr::map_dfr(~ bbox_area(df = points, clust = .))
        # Returns TRUE/FALSE
        any(as.numeric(areas$value) > min_area)
      }

      # function to split clusters by their area
      split_clusters_by_area <- function(points, min_area) {
        # calculate areas for all clusters
        cluster_areas <- unique(points$cluster) %>%
          purrr::map_dfr(~ bbox_area(df = points, clust = .))

        # split into "large" and "small" clusters
        large_clusters <- cluster_areas %>%
          dplyr::filter(as.numeric(value) > min_area)

        small_clusters <- cluster_areas %>%
          dplyr::filter(as.numeric(value) <= min_area)

        # create data frames for both sets
        large_points <- points %>%
          dplyr::filter(cluster %in% large_clusters$cluster)

        small_points <- points %>%
          dplyr::filter(cluster %in% small_clusters$cluster)

        list(
          large = large_points,
          small = small_points,
          large_areas = large_clusters,
          small_areas = small_clusters
        )
      }

      # store the results of the analysis run
      all_small_clusters <- list()
      current_points <- points_sf %>% dplyr::distinct(geometry)
      iteration <- 1

      # sequence of eps values to try
      eps_sequence <- c(0.25, 0.05, 1, .1)
      eps_index <- 1

      while (nrow(current_points) > 0 && iteration <= max_iterations) {
        # grab current eps value (cycle through sequence)
        current_eps <- eps_sequence[eps_index]
        eps_index <- (eps_index %% length(eps_sequence)) + 1

        # ... perform cluster
        clustered_points <- cluster_iteration(
          current_points,
          eps = current_eps,
          # Minimum number of points within a single cluster
          # (meaning, a point can be isolated if super far from others)
          min_pts = 1,
          iteration = iteration
        )

        # Split results
        split_results <- split_clusters_by_area(clustered_points, min_area)

        # Store small clusters
        if (nrow(split_results$small) > 0) {
          all_small_clusters[[paste0("iteration_", iteration)]] <- split_results$small
        }

        # check if we have any large bbox clusters to keep processing
        if (nrow(split_results$large) == 0) {
          break
        }

        # update points for next iteration
        current_points <- split_results$large
        iteration <- iteration + 1
      }

      # combine all results
      final_clusters <- dplyr::bind_rows(all_small_clusters) %>%
        dplyr::arrange(iteration)

      # warning if max iterations reached. This should never happen...
      if (iteration == max_iterations) {
        warning("Maximum iterations reached. Some clusters may still exceed the area threshold.")
      }

      return(
        list(
          clusters = final_clusters,
          clusters_by_iteration = all_small_clusters,
          total_iterations = iteration,
          final_eps = current_eps
        )
      )
    }

    # grab all unique points
    points_sf <- dplyr::distinct(.data, geometry)

    # grab initial clusters
    init <- perform_iterative_clustering(points_sf = points_sf) %>%
      .[["clusters_by_iteration"]] %>%
      dplyr::bind_rows()

    # if any didn't get "clustered"....
    final_cluster_list <- points_sf %>%
      dplyr::filter(!geometry %in% init$geometry) %>%
      # tack them on to our df and give them a unique "cluster name"
      tibble::rowid_to_column(var = "cluster") %>%
      dplyr::mutate(cluster = as.character(cluster)) %>%
      dplyr::bind_rows(init)

    catchment_features <- vector("list", length = length(unique(final_cluster_list$cluster)))

    for (i in 1:length(unique(final_cluster_list$cluster))) {
      # bounding box of user's WQP data
      suppressMessages(suppressWarnings({
        bbox <- final_cluster_list %>%
          dplyr::filter(cluster == unique(final_cluster_list$cluster)[i]) %>%
          sf::st_bbox(.) %>%
          toString(.) %>%
          urltools::url_encode(.)
      }))

      catchment_features[[i]] <- fetch_bbox(baseurls = baseurls[1], sf_bbox = bbox)
    }

    catchment_features <- catchment_features %>%
      purrr::keep(~ !is.null(.)) %>%
      purrr::keep(~ nrow(.) > 0) %>%
      dplyr::bind_rows()

    try(
      catchment_features <- catchment_features %>%
        .[points_sf, ],
      silent = TRUE
    )

    ## GRABBING WATER TYPE:
    if (length(catchment_features) == 0 || is.null(catchment_features)) {
      message("There are no ATTAINS features associated with your WQP observations.")
    } else {
      # Use ATTAINS API to grab, for each assessment unit, its WaterType.
      # Query the API in "chunks" so it doesn't break. sweet spot is ~50:
      all_units <- unique(catchment_features$assessmentunitidentifier)
      water_types <- grab_waterbody_type(all_units, chunk_size = 50)
      try(catchment_features <- dplyr::left_join(catchment_features, water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")))
    }

    # If only interested in grabbing catchment data, return just the catchments
    if (catchments_only == TRUE) {
      return(list("ATTAINS_catchments" = catchment_features))
    }

    points <- fetch_au(
      baseurls = baseurls[2],
      assessment_unit_ids = unique(catchment_features$assessmentunitidentifier)
    )
    lines <- fetch_au(
      baseurls = baseurls[3],
      assessment_unit_ids = unique(catchment_features$assessmentunitidentifier)
    )
    polygons <- fetch_au(
      baseurls = baseurls[4],
      assessment_unit_ids = unique(catchment_features$assessmentunitidentifier)
    )

    try(
      points <- points %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")),
      silent = TRUE
    )

    try(
      lines <- lines %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")),
      silent = TRUE
    )

    try(
      polygons <- polygons %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")),
      silent = TRUE
    )

    final_features <- list(
      "ATTAINS_catchments" = catchment_features,
      "ATTAINS_points" = points,
      "ATTAINS_lines" = lines,
      "ATTAINS_polygons" = polygons
    )



    return(final_features)

    # If area is small (< 6e+9 square meters), just use the bbox in one pull:
  } else {
    # FOR AOIs THAT ARE LESS THAN 6,000 sqkm, grab data in one go:
    points_sf <- .data

    bbox <- points_sf %>%
      sf::st_bbox(.) %>%
      # convert bounding box to characters
      toString(.) %>%
      # encode for use within the API URL
      urltools::url_encode(.)

    catchment_features <- fetch_bbox(baseurls = baseurls[1], sf_bbox = bbox)

    try(
      catchment_features <- catchment_features %>%
        .[points_sf, ],
      silent = TRUE
    )

    if (length(catchment_features) == 0 || is.null(catchment_features)) {
      message("There are no ATTAINS features associated with your WQP observations.")
    } else {
      ## GRABBING WATER TYPE:

      # Use ATTAINS API to grab, for each assessment unit, its WaterType.
      # Query the API in "chunks" so it doesn't break:
      all_units <- unique(catchment_features$assessmentunitidentifier)
      water_types <- grab_waterbody_type(all_units, chunk_size = 50)
      try(catchment_features <- dplyr::left_join(catchment_features, water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")), silent = TRUE)
    }

    # If only interested in grabbing catchment data, return just the catchments
    if (catchments_only == TRUE) {
      return(list("ATTAINS_catchments" = catchment_features))
    }

    suppressMessages({
      suppressWarnings({
        # Otherwise, start grabbing the raw ATTAINS features that intersect those
        # catchments

        points <- NULL
        lines <- NULL
        polygons <- NULL

        # Download associated point, line, and polygon features using catchment bbox
        try(
          points <- fetch_au(
            baseurls = baseurls[2],
            assessment_unit_ids = unique(catchment_features$assessmentunitidentifier)
          ),
          silent = TRUE
        )

        try(
          lines <- fetch_au(
            baseurls = baseurls[3],
            assessment_unit_ids = unique(catchment_features$assessmentunitidentifier)
          ),
          silent = TRUE
        )

        try(
          polygons <- fetch_au(
            baseurls = baseurls[4],
            assessment_unit_ids = unique(catchment_features$assessmentunitidentifier)
          ),
          silent = TRUE
        )

        try(points <- points %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")),
          silent = TRUE
        )

        try(lines <- lines %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")),
          silent = TRUE
        )

        try(polygons <- polygons %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")),
          silent = TRUE
        )

        final_features <- list(
          "ATTAINS_catchments" = catchment_features,
          "ATTAINS_points" = points,
          "ATTAINS_lines" = lines,
          "ATTAINS_polygons" = polygons
        )
      })
    })

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
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' nhd_data <- fetchNHD(
#'   .data = tada_data, resolution = "Hi",
#'   features = c("catchments", "waterbodies", "flowlines")
#' )
#' }
fetchNHD <- function(.data, resolution = "Hi", features = "catchments") {
  # function settings that we ensure go back to their original settings
  # after the function stops running:
  original_s2 <- sf::sf_use_s2() # Store the original s2 setting first
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))), add = TRUE)

  suppressMessages(suppressWarnings({
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
  if (resolution %in% c("Hi", "hi")) {
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

      # select the layer by id from the items list called above (10 is HR catchments)
      nhd_hr_catchments <- arcgislayers::get_layer(nhd_hr, 10)

      # use bboxes of the sites to return their associated catchments
      nhd_catchments_stored <- vector("list", length = length(wqp_bboxes))

      for (i in 1:length(wqp_bboxes)) {
        try(
          nhd_catchments_stored[[i]] <- arcgislayers::arc_select(nhd_hr_catchments,
            filter_geom = wqp_bboxes[i],
            crs = sf::st_crs(wqp_bboxes[i])
          ) %>%
            sf::st_make_valid(),
          silent = TRUE
        )
      }

      nhd_catchments_stored <- nhd_catchments_stored %>%
        purrr::keep(~ !is.null(.)) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct()

      try(nhd_catchments_stored <- nhd_catchments_stored %>%
        dplyr::select(nhdplusid,
          catchmentareasqkm = areasqkm
        ) %>%
        dplyr::mutate(
          NHD.nhdplusid = as.character(nhdplusid),
          NHD.resolution = "HR",
          NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)
        ) %>%
        dplyr::select(NHD.nhdplusid, NHD.resolution, NHD.catchmentareasqkm, geometry), silent = TRUE)
    }))

    # Empty version of the df will be returned if no associated catchments
    # to avoid breaking downstream fxns reliant on catchment info.
    if (nrow(nhd_catchments_stored) == 0 && "catchments" %in% features) {
      message("No NHD HR features associated with your WQP observations.")
      nhd_catchments_stored <- tibble::tibble(
        NHD.nhdplusid = character(),
        NHD.resolution = character(),
        NHD.catchmentareasqkm = numeric()
      )
    }

    if (nrow(nhd_catchments_stored) == 0 && !"catchments" %in% features) {
      stop("No NHD HR features associated with your WQP observations.")
    }

    if (length(features) == 1 && features == "catchments") {
      return(nhd_catchments_stored)
    }

    # Grab flowlines -
    if ("flowlines" %in% features && nrow(nhd_catchments_stored) > 0) {
      suppressMessages(suppressWarnings({
        # use catchments to grab other NHD features
        geospatial_aoi <- nhd_catchments_stored %>%
          sf::st_as_sfc()

        # select the layer by id from the items list (3 is HR flowlines)
        nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)

        # use catchments to return associated flowlines
        nhd_flowlines_stored <- vector("list", length = length(geospatial_aoi))

        for (i in 1:length(geospatial_aoi)) {
          try(
            nhd_flowlines_stored[[i]] <- arcgislayers::arc_select(nhd_hr_flowlines,
              filter_geom = geospatial_aoi[i],
              crs = sf::st_crs(geospatial_aoi[i])
            ) %>%
              sf::st_make_valid(),
            silent = TRUE
          )

          # so all returned meta data binds properly, must transform all columns into characters,
          # EXCEPT for the geometry column:
          try(
            geometry_col <- sf::st_geometry(nhd_flowlines_stored[[i]]),
            silent = TRUE
          )

          try(
            nhd_flowlines_stored[[i]] <- nhd_flowlines_stored[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.))),
            silent = TRUE
          )
        }

        nhd_flowlines_stored <- nhd_flowlines_stored %>%
          purrr::keep(~ !is.null(.)) %>%
          purrr::keep(~ !is.character(.)) %>%
          dplyr::bind_rows() %>%
          dplyr::distinct()
      }))

      if (length(features) == 1 && features == "flowlines") {
        if (length(nhd_flowlines_stored) == 0 || is.null(nhd_flowlines_stored)) {
          message("There are no NHD flowlines associated with your WQP observations.")
        }

        return(nhd_flowlines_stored)
      }

      if (length(nhd_flowlines_stored) == 0 || is.null(nhd_flowlines_stored)) {
        message("There are no NHD flowlines associated with your WQP observations.")
      }
    }

    # Grab waterbodies -
    if ("waterbodies" %in% features & nrow(nhd_catchments_stored) > 0) {
      suppressMessages(suppressWarnings({
        geospatial_aoi <- nhd_catchments_stored %>%
          sf::st_as_sfc()

        # select the layer by id from the items list called above (9 is HR waterbodies)
        nhd_hr_waterbodies <- arcgislayers::get_layer(nhd_hr, 9)

        # use catchments to return associated waterbodies
        nhd_waterbodies_stored <- vector("list", length = length(geospatial_aoi))

        for (i in 1:length(geospatial_aoi)) {
          try(
            nhd_waterbodies_stored[[i]] <- arcgislayers::arc_select(nhd_hr_waterbodies,
              # where = query,
              filter_geom = geospatial_aoi[i],
              crs = sf::st_crs(geospatial_aoi[i])
            ) %>%
              sf::st_make_valid(),
            silent = TRUE
          )

          # so all returned meta data binds properly, must transform all columns into characters,
          # EXCEPT for the geometry column:
          try(
            geometry_col <- sf::st_geometry(nhd_waterbodies_stored[[i]]),
            silent = TRUE
          )

          try(
            nhd_waterbodies_stored[[i]] <- nhd_waterbodies_stored[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.))),
            silent = TRUE
          )
        }

        nhd_waterbodies_stored <- nhd_waterbodies_stored %>%
          purrr::keep(~ !is.null(.)) %>%
          purrr::keep(~ !is.character(.)) %>%
          dplyr::bind_rows() %>%
          dplyr::distinct()
      }))

      if (length(features) == 1 && features == "waterbodies") {
        if (length(nhd_waterbodies_stored) == 0 || is.null(nhd_waterbodies_stored)) {
          message("There are no NHD waterbodies associated with your WQP observations.")
        }

        return(nhd_waterbodies_stored)
      }

      if (length(nhd_waterbodies_stored) == 0 || is.null(nhd_waterbodies_stored)) {
        message("There are no NHD waterbodies associated with your WQP observations.")
      }
    }

    # Combinations of features selected, and what they return:

    if (length(features) == 2 && "catchments" %in% features && "flowlines" %in% features) {
      nhd_list <- list(
        "NHD_catchments" = nhd_catchments_stored,
        "NHD_flowlines" = nhd_flowlines_stored
      )

      return(nhd_list)
    } else if (length(features) == 2 && "catchments" %in% features && "waterbodies" %in% features) {
      nhd_list <- list(
        "NHD_catchments" = nhd_catchments_stored,
        "NHD_waterbodies" = nhd_waterbodies_stored
      )

      return(nhd_list)
    } else if (length(features) == 2 && "flowlines" %in% features && "waterbodies" %in% features) {
      nhd_list <- list(
        "NHD_flowlines" = nhd_flowlines_stored,
        "NHD_waterbodies" = nhd_waterbodies_stored
      )

      return(nhd_list)
    } else if (length(features) == 3 && "catchments" %in% features && "flowlines" %in% features && "waterbodies" %in% features) {
      nhd_list <- list(
        "NHD_catchments" = nhd_catchments_stored,
        "NHD_flowlines" = nhd_flowlines_stored,
        "NHD_waterbodies" = nhd_waterbodies_stored
      )
    } else {
      stop("Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument.")
    }

    # If user wants NHDPlus V2...
  } else if (resolution %in% c("Med", "med")) {
    suppressMessages(suppressWarnings({
      nhd_catchments <- vector("list", length = nrow(unique_sites))

      for (i in 1:nrow(unique_sites)) {
        # Use {nhdplusTools} to grab associated catchments...
        try(
          nhd_catchments[[i]] <- nhdplusTools::get_nhdplus(AOI = unique_sites[i, ], realization = "catchment") %>%
            sf::st_make_valid() %>%
            dplyr::select(
              comid = featureid,
              catchmentareasqkm = areasqkm
            ) %>%
            dplyr::mutate(
              NHD.comid = as.character(comid),
              NHD.resolution = "nhdplusV2",
              NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)
            ) %>%
            dplyr::select(NHD.comid, NHD.resolution, NHD.catchmentareasqkm, geometry),
          silent = TRUE
        )
      }

      nhd_catchments <- nhd_catchments %>%
        purrr::keep(~ !is.null(.))

      try(nhd_catchments <- dplyr::bind_rows(nhd_catchments) %>%
        dplyr::distinct(), silent = TRUE)

      # if NHD catchments are not in the correct CRS, transform them
      try(if (sf::st_crs(nhd_catchments) != sf::st_crs(geospatial_data)) {
        nhd_catchments <- nhd_catchments %>%
          sf::st_transform(sf::st_crs(geospatial_data)$epsg)
      }, silent = TRUE)
    }))

    if (nrow(nhd_catchments) == 0 && "catchments" %in% features) {
      message("No NHDPlus V2 features associated with your WQP observations.")
      nhd_catchments <- tibble::tibble(
        NHD.comid = character(),
        NHD.resolution = character(),
        NHD.catchmentareasqkm = numeric()
      )
    }

    if (nrow(nhd_catchments) == 0 && !"catchments" %in% features) {
      stop("No NHDPlus V2 features associated with your WQP observations.")
    }

    if (length(features) == 1 && features == "catchments") {
      return(nhd_catchments)
    }


    # Grab flowlines -
    if ("flowlines" %in% features && nrow(nhd_catchments) > 0) {
      suppressMessages(suppressWarnings({
        nhd_flowlines <- vector("list", length = nrow(nhd_catchments))

        # use catchments to grab other NHD features:
        unique_sites <- nhd_catchments

        for (i in 1:nrow(unique_sites)) {
          # Use {nhdplusTools} to grab associated flowlines...
          try(
            nhd_flowlines[[i]] <- nhdplusTools::get_nhdplus(AOI = unique_sites[i, ], realization = "flowline") %>%
              sf::st_make_valid(),
            silent = TRUE
          )

          try(geometry_col <- sf::st_geometry(nhd_flowlines[[i]]),
            silent = TRUE
          )

          try(
            nhd_flowlines[[i]] <- nhd_flowlines[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.))),
            silent = TRUE
          )
        }

        nhd_flowlines <- nhd_flowlines %>%
          purrr::keep(~ !is.null(.))

        try(nhd_flowlines <- dplyr::bind_rows(nhd_flowlines)) %>%
          dplyr::distinct()

        # if NHD flowlines are not in the correct CRS, transform them
        try(if (sf::st_crs(nhd_flowlines) != sf::st_crs(geospatial_data)) {
          nhd_flowlines <- nhd_flowlines %>%
            sf::st_transform(sf::st_crs(geospatial_data)$epsg)
        }, silent = TRUE)
      }))

      if (nrow(nhd_flowlines) == 0 && "flowlines" %in% features) {
        message("No NHDPlus V2 flowlines associated with your WQP observations.")
      }

      if (length(features) == 1 && features == "flowlines") {
        return(nhd_flowlines)
      }
    }

    # Grab waterbodies -
    if ("waterbodies" %in% features && nrow(nhd_catchments) > 0) {
      suppressMessages(suppressWarnings({
        nhd_waterbodies <- vector("list", length = nrow(nhd_catchments))

        # use catchments to grab other NHD features:
        unique_sites <- nhd_catchments

        for (i in 1:nrow(unique_sites)) {
          # Use {nhdplusTools} to grab associated flowlines...
          try(
            nhd_waterbodies[[i]] <- nhdplusTools::get_waterbodies(AOI = unique_sites[i, ]) %>%
              sf::st_make_valid(),
            silent = TRUE
          )

          try(geometry_col <- sf::st_geometry(nhd_waterbodies[[i]]),
            silent = TRUE
          )

          try(
            nhd_waterbodies[[i]] <- nhd_waterbodies[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.))),
            silent = TRUE
          )
        }

        nhd_waterbodies <- nhd_waterbodies %>%
          purrr::keep(~ !is.null(.))

        try(
          nhd_waterbodies <- dplyr::bind_rows(nhd_waterbodies) %>%
            dplyr::distinct(),
          silent = TRUE
        )

        # if NHD waterbodies are not in the correct CRS, transform them
        try(if (sf::st_crs(nhd_waterbodies) != sf::st_crs(geospatial_data)) {
          nhd_waterbodies <- nhd_waterbodies %>%
            sf::st_transform(sf::st_crs(geospatial_data)$epsg)
        }, silent = TRUE)
      }))

      if (nrow(nhd_waterbodies) == 0 && "waterbodies" %in% features) {
        message("No NHDPlus V2 waterbodies associated with your WQP observations.")
      }

      if (length(features) == 1 && features == "waterbodies") {
        return(nhd_waterbodies)
      }
    }

    # Combinations of features selected, and what they return:

    if (length(features) == 2 && "catchments" %in% features && "flowlines" %in% features) {
      nhd_list <- list(
        "NHD_catchments" = nhd_catchments,
        "NHD_flowlines" = nhd_flowlines
      )

      return(nhd_list)
    } else if (length(features) == 2 && "catchments" %in% features && "waterbodies" %in% features) {
      nhd_list <- list(
        "NHD_catchments" = nhd_catchments,
        "NHD_waterbodies" = nhd_waterbodies
      )

      return(nhd_list)
    } else if (length(features) == 2 && "flowlines" %in% features && "waterbodies" %in% features) {
      nhd_list <- list(
        "NHD_flowlines" = nhd_flowlines,
        "NHD_waterbodies" = nhd_waterbodies
      )

      return(nhd_list)
    } else if (length(features) == 3 && "catchments" %in% features && "flowlines" %in% features && "waterbodies" %in% features) {
      nhd_list <- list(
        "NHD_catchments" = nhd_catchments,
        "NHD_flowlines" = nhd_flowlines,
        "NHD_waterbodies" = nhd_waterbodies
      )
    } else {
      stop("Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument.")
    }
  } else {
    stop('User-supplied resolution unavailable. Please select between "Med" or "Hi".')
  }
}



#' TADA_GetATTAINS
#'
#' Link catchment-based ATTAINS assessment unit data
#' (EPA snapshot of NHDPlus HR catchments associated with entity submitted
#' assessment unit features - points, lines, and polygons) to Water Quality
#' Portal observations, often imported via `TADA_DataRetrieval()`. This
#' function returns the objects that can be mapped in `TADA_ViewATTAINS()`.
#' Check out the TADAModule2.Rmd for an example workflow. Note that
#' approximately 80% of state submitted assessment units in ATTAINS were
#' developed based on high res NHDPlus, so we are using that as the default.
#'
#' The ATTAINS snapshot of NHDPlus HR catchments is not available for areas
#' that do not have existing Assessment Units in ATTAINS. For these areas where
#' there are WQP sites, but no existing ATTAINS assessment units, a user can
#' choose to associate the WQP sites with NHDPlus catchments available from
#' the USGS nhdplusTools package (please be aware that USGS and EPA ATTAINS
#' snapshots of the NHDPlus catchments may vary) using the optional function
#' param 'fill_catchments'.  If desired by the user, the HR
#' catchments could be created as new assessment unit polygons in ATTAINS
#' (that process is outside of TADA).
#'
#' `ResultIdentifier' identifies rows that are the same observation but are
#' linked to multiple ATTAINS assessment units. It is possible for a single
#' TADA WQP observation to have multiple ATTAINS assessment units linked to
#' it and subsequently more than one row of data.
#'
#' If TADA_MakeSpatial has not yet been run, this function runs it which also
#' adds another new column to the input dataframe, 'geometry', which allows
#' for mapping and additional geospatial capabilities.
#'
#' Please review the output of this function carefully, especially waterbody
#' intersections (tributaries), lake/ocean coasts, and other areas with
#' complex hydrology where imprecise WQP monitoring location coordinates can
#' be problematic. Note that many WQP locations will not fall within the bounds
#' of NHDPlus (estuaries, oceans). Manual adjustments and quality control checks
#' are strongly encouraged. WQP monitoring location metadata may also be helpful
#' for matching waterbody  names with ATTAINS waterbody names instead of relying
#' solely on the geospatial location (lat/long).
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()` or the sf
#' equivalent made by `TADA_MakeSpatial()`.
#' @param return_nearest If a WQP observation falls within more than one AU,
#' return ONLY the nearest AU (return_nearest = TRUE), or all AUs
#' (return_nearest = FALSE).
#' @param fill_catchments Whether the user would like to return NHD catchments
#' (USGS snapshot of NHDPlus V2) for WQP observations not associated with an
#' ATTAINS assessment unit (TRUE or FALSE). When fill_catchments = TRUE,
#' the returned list splits observations into two dataframes: WQP observations
#' with ATTAINS catchment data (EPA snapshot of NHDPlus V2), and WQP
#' observations without ATTAINS catchment data. Defaults to FALSE.
#' @param resolution If fill_catchments = TRUE, whether to use NHDPlus V2 "Med"
#' catchments or NHDPlus V2 HiRes "Hi" catchments. Default is NHDPlus V2 HiRes
#' ("Hi") because at approximately 80% of state submitted assessment units in
#' ATTAINS were developed based on NHDPlus V2 HiRes.
#' @param return_sf Whether to return the ATTAINS associated catchments, lines,
#' points, and polygon shapefile objects along with the data frame(s).
#' TRUE (yes, return list) or FALSE (no, do not return). All shapefile features
#' are in WGS84 (crs = 4326). If fill_catchments = TRUE and return_sf = TRUE,
#' the function will additionally return the raw catchment features associated
#' with the observations in TADA_without_ATTAINS in a new shapefile called
#' without_ATTAINS_catchments. Defaults to TRUE.
#' @param au_ref Optional. A df containing the existing crosswalk of known
#' AU and monitoring location identifier combinations. Can be created using
#' TADA_GetATTAINSAUMLCrosswalk or provided by the user from an external file.
#' Must contain the columns ATTAINS.MonitoringLocationIdentifier,
#' ML.OrganizationIdentifier (OrganizationIdentifier for sampling org), and
#' ATTAINS.AssessmentUnitIdentifier. The monitoring location identifiers must
#' match those in the WQP, which may contain the organization and provider in
#' the MonitoringLocationIdentifier.
#'
#' @return A modified `TADA_DataRetrieval()` dataframe or list with additional
#' columns associated with the ATTAINS assessment unit data, and, if
#' fill_catchments = TRUE, an additional dataframe of the observations without
#' intersecting ATTAINS features.
#' Moreover, if return_sf = TRUE, this function will additionally return the
#' raw ATTAINS and catchment shapefile features associated with those
#' observations.
#'
#' This function calculates and reports the distance, 'TADA.DistanceAway.Meters',
#' between each WQP observation and intersecting ATTAINS features within its
#' catchment. A TADA.DistanceAway.Meters value of 0 indicates that the WQP
#' observation is directly on the associated  ATTAINS point or line feature,
#' or located inside the associated ATTAINS polygon.
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
#'   endDate = "2018-05-05",
#'   characteristicName = "pH",
#'   statecode = "VA",
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' # note: these example ATTAINS data retrieval queries below may take a long
#' # time (10+ minutes) to run
#' tada_attains <- TADA_GetATTAINS(tada_data,
#'   fill_catchments = FALSE,
#'   return_sf = FALSE,
#'   return_nearest = FALSE,
#' )
#'
#' tada_attains_sf <- TADA_GetATTAINS(tada_data,
#'   fill_catchments = FALSE,
#'   return_sf = TRUE,
#'   return_nearest = TRUE
#' )
#'
#' tada_attains_filled <- TADA_GetATTAINS(tada_data,
#'   fill_catchments = TRUE,
#'   resolution = "Hi",
#'   return_sf = FALSE,
#'   return_nearest = TRUE
#' )
#'
#' tada_attains_filled_sf <- TADA_GetATTAINS(tada_data,
#'   fill_catchments = TRUE,
#'   resolution = "Hi",
#'   return_sf = TRUE,
#'   return_nearest = TRUE
#' )
#' }
TADA_GetATTAINS <- function(.data, return_nearest = FALSE,
                            fill_catchments = FALSE, resolution = "Hi",
                            return_sf = TRUE) {
  # function settings that we ensure go back to their original settings
  # after the function stops running:
  original_s2 <- sf::sf_use_s2() # Store the original s2 setting first
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))), add = TRUE)

  attains_names <- c(
    "ATTAINS.OrganizationId", "ATTAINS.SubmissionId", "ATTAINS.HasProtectionPlan",
    "ATTAINS.AssessmentUnitName", "ATTAINS.NhdPlusId", "ATTAINS.Tas303d",
    "ATTAINS.IsThreatened", "ATTAINS.State", "ATTAINS.On303dList",
    "ATTAINS.OrganizationName", "ATTAINS.Region", "ATTAINS.ShapeLength",
    "ATTAINS.ReportingCycle", "ATTAINS.AssmntJoinKey", "ATTAINS.HasTmdl",
    "ATTAINS.OrgType", "ATTAINS.PermIdJoinKey", "ATTAINS.CatchmentIsTribal",
    "ATTAINS.IrCategory", "ATTAINS.WaterbodyReportLink", "ATTAINS.AssessmentUnitIdentifier",
    "ATTAINS.OverallStatus", "ATTAINS.IsAssessed", "ATTAINS.IsImpaired",
    "ATTAINS.Has4bPlan", "ATTAINS.Huc12", "ATTAINS.HasAlternativePlan",
    "ATTAINS.VisionPriority303d", "ATTAINS.AreaSqkm", "ATTAINS.CatchmentAreaSqkm",
    "ATTAINS.CatchmentStateCode", "ATTAINS.CatchmentResolution", "ATTAINS.WaterType",
    "ATTAINS.ShapeArea"
  )

  if (any(attains_names %in% colnames(.data))) {
    stop("Your data has already been joined with ATTAINS data.")
  }

  if (nrow(.data) == 0) {
    # if no WQP observations, return a modified `data` with empty ATTAINS-related columns:
    message("Your Water Quality Portal dataframe has no observations. Returning an empty dataframe with empty ATTAINS features.")

    # Add ATTAINS columns with NA values
    col_val_list <- stats::setNames(
      object = rep(
        x = list(NA),
        times = length(attains_names)
      ),
      nm = attains_names
    )

    no_WQP_data <- .data %>%
      dplyr::mutate(ResultIdentifier = NA) %>%
      dplyr::bind_cols(col_val_list) %>%
      dplyr::select(ResultIdentifier, dplyr::everything())

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

      # If return_sf == FALSE, then just return the dataframe:
    } else {
      return(no_WQP_data)
    }
  }

  # Use the "ResultIdentifier" column as our index for tracking WQP obs w/
  # multiple AUs
  .data <- .data %>%
    dplyr::select(ResultIdentifier, dplyr::everything())

  # If data doesn't already contain ATTAINS data and isn't an empty dataframe:
  suppressMessages(suppressWarnings({
    # If data is already spatial, just make sure it is in the right CRS
    if (!is.null(.data) & inherits(.data, "sf")) {
      if (sf::st_crs(.data)$epsg != 4326) {
        TADA_DataRetrieval_data <- .data %>%
          sf::st_transform(4326)
      } else {
        TADA_DataRetrieval_data <- .data
      }
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      TADA_DataRetrieval_data <- .data %>%
        # convert dataframe to a spatial object
        TADA_MakeSpatial(.data = ., crs = 4326)
    }
  }))

  # grab all ATTAINS features in catchments that intersect our WQP objects:
  attains_features <- try(fetchATTAINS(.data = TADA_DataRetrieval_data), silent = TRUE)

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
        dplyr::rename(ATTAINS.SubmissionId = submissionid,
                      ATTAINS.NhdPlusId = nhdplusid,
                      ATTAINS.State = state,
                      ATTAINS.Region = region,
                      ATTAINS.OrganizationId = organizationid,
                      ATTAINS.OrgType = orgtype,
                      ATTAINS.Tas303d = tas303d,
                      ATTAINS.OrganizationName = organizationname,
                      ATTAINS.ReportingCycle = reportingcycle,
                      ATTAINS.AssessmentUnitIdentifier = assessmentunitidentifier,
                      ATTAINS.AssessmentUnitName = assessmentunitname,
                      ATTAINS.WaterbodyReportLink = waterbodyreportlink,
                      ATTAINS.AssmntJoinKey = assmnt_joinkey,
                      ATTAINS.PermIdJoinKey = permid_joinkey,
                      ATTAINS.IrCategory = ircategory,
                      ATTAINS.OverallStatus = overallstatus,
                      ATTAINS.IsAssessed = isassessed,
                      ATTAINS.IsImpaired = isimpaired,
                      ATTAINS.IsThreatened = isthreatened,
                      ATTAINS.On303dList = on303dlist,
                      ATTAINS.HasTmdl = hastmdl,
                      ATTAINS.Has4bPlan = has4bplan,
                      ATTAINS.HasAlternativePlan = hasalternativeplan,
                      ATTAINS.HasProtectionPlan = hasprotectionplan,
                      ATTAINS.VisionPriority303d =  visionpriority303d,
                      ATTAINS.AreaSqkm = areasqkm,
                      ATTAINS.Huc12 = huc12,
                      ATTAINS.XwalkMethod = xwalk_method,
                      ATTAINS.WwalkHuc12Version= xwalk_huc12_version,
                      ATTAINS.CatchmentAreaSqkm = catchmentareasqkm,
                      ATTAINS.CatchmentStateCode = catchmentstatecode,
                      ATTAINS.CatchmentIsTribal = catchmentistribal,
                      ATTAINS.CatchmentResolution = catchmentresolution,
                      ATTAINS.ShapeLength = Shape_Length,
                      ATTAINS.ShapeArea = Shape_Area,
                      ATTAINS.WaterType = waterTypeCode
        ) %>%
        # get rid of dupes (as a precaution)
        dplyr::distinct(.keep_all = TRUE),
      silent = TRUE
    )
    if (is.null(nearby_catchments) || nrow(nearby_catchments) == 0) {
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
      dplyr::bind_cols(col_val_list)

    message("There are no ATTAINS catchments associated with these WQP observations.")

    if (fill_catchments == FALSE) {
      if (return_sf == TRUE) {
        return(list(
          "TADA_with_ATTAINS" = no_ATTAINS_data,
          "ATTAINS_catchments" = NULL,
          "ATTAINS_points" = NULL,
          "ATTAINS_lines" = NULL,
          "ATTAINS_polygons" = NULL
        ))
      } else {
        return(no_ATTAINS_data)
      }
    } else {
      nhd_catchments <- fetchNHD(.data = TADA_DataRetrieval_data, resolution = resolution)
      TADA_without_ATTAINS <- TADA_DataRetrieval_data %>% sf::st_join(nhd_catchments, left = TRUE)

      if (return_sf == TRUE) {
        return(list(
          "TADA_with_ATTAINS" = no_ATTAINS_data[0, ],
          "TADA_without_ATTAINS" = TADA_without_ATTAINS,
          "ATTAINS_catchments" = NULL,
          "ATTAINS_points" = NULL,
          "ATTAINS_lines" = NULL,
          "ATTAINS_polygons" = NULL,
          "without_ATTAINS_catchments" = nhd_catchments
        ))
      } else {
        return(list(
          "TADA_with_ATTAINS" = no_ATTAINS_data[0, ],
          "TADA_without_ATTAINS" = TADA_without_ATTAINS
        ))
      }
    }
  }

  # If there IS at least some ATTAINS data...

  if (!is.null(nearby_catchments)) {
    suppressMessages({
      suppressWarnings({
        # ... link WQP features to the ATTAINS catchment feature(s) they land in:
        TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
          # left = TRUE to preserve all observations (with or without ATTAINS features):
          sf::st_join(., nearby_catchments, left = TRUE)
      })
    })

    if (suppressMessages({
      suppressWarnings({
        TADA_with_ATTAINS %>%
          data.table::data.table() %>%
          dplyr::group_by(ResultIdentifier) %>%
          dplyr::summarize(count = dplyr::n()) %>%
          dplyr::filter(count > 1) %>%
          nrow() > 0
      })
    }) & return_nearest == FALSE) {
      message("WARNING! Some of your WQP observations fall within a catchment that has more than one ATTAINS feature in it.")
      message("For these, duplicate rows have been created, one for each ATTAINS feature. Use `ResultIdentifier` to track these instances.")
      message("If you would like to instead only return the nearest ATTAINS feature, use `return_nearest = TRUE.")
    }

    # Grab each ATTAINS features' distance away from their associated WQP observations.
    # A value of 0 indicates that the WQP observation is either exactly atop an ATTAINS
    # point of line feature, or within an ATTAINS polygon feature.

    find_distances <- function(location) {
      sub_tada <- TADA_with_ATTAINS %>%
        dplyr::filter(as.character(geometry) == location)

      distance <- sub_tada[1, ]

      subset <- attains_features[-1] %>%
        purrr::map(~ tryCatch(
          dplyr::filter(., assessmentunitidentifier %in% sub_tada$ATTAINS.AssessmentUnitIdentifier),
          error = function(e) data.frame(),
          warning = function(w) data.frame()
        )) %>%
        purrr::keep(~ !is.null(.)) %>%
        purrr::keep(~ nrow(.) > 0)

      result <- NULL

      # Calculate distances
      try(distances <- subset %>%
        # for each WQP, grab the distance between the WQP point and all the ATTAINS features within its same catchment. A value of 0 means
        # the WQP observation is exactly atop a point or line ATTAINS feature, or within an ATTAINS polygon.
        purrr::map(~ dplyr::mutate(., TADA.DistanceAway.Meters = as.character(sf::st_distance(., distance)))) %>%
        dplyr::bind_rows() %>%
        sf::st_drop_geometry() %>%
        dplyr::select(
          ATTAINS.AssessmentUnitIdentifier = assessmentunitidentifier,
          TADA.DistanceAway.Meters
        ) %>%
        dplyr::distinct(), silent = TRUE)

      try(result <- sub_tada %>%
        data.table::data.table() %>%
        dplyr::select(ResultIdentifier, ATTAINS.AssessmentUnitIdentifier) %>%
        dplyr::left_join(., distances, by = "ATTAINS.AssessmentUnitIdentifier") %>%
        sf::st_drop_geometry() %>%
        # for AUs with multiple features, only assess the one closest:
        dplyr::group_by(ResultIdentifier, ATTAINS.AssessmentUnitIdentifier) %>%
        dplyr::filter(TADA.DistanceAway.Meters == min(TADA.DistanceAway.Meters)) %>%
        dplyr::ungroup(), silent = TRUE)

      return(result)
    }

    distances_table <- as.character(unique(TADA_with_ATTAINS$geometry)) %>%
      purrr::map_dfr(~ find_distances(location = .))

    TADA_with_ATTAINS <- TADA_with_ATTAINS %>%
      data.table::data.table() %>%
      dplyr::left_join(., distances_table, by = c("ResultIdentifier", "ATTAINS.AssessmentUnitIdentifier")) %>%
      dplyr::distinct() %>%
      sf::st_as_sf()

    # If return_nearest is TRUE, only keep the nearest ATTAINS feature to the WQP observation.
    # Otherwise, return all ATTAINS features associated with the same catchment as the WQP observation.
    if (return_nearest == TRUE) {
      message("Selecting nearest ATTAINS feature for each WQP observation.")
      message("Use `return_nearest = FALSE` to return all features within WQP catchments.")
      TADA_with_ATTAINS <- TADA_with_ATTAINS %>%
        dplyr::group_by(ResultIdentifier) %>%
        dplyr::slice_min(TADA.DistanceAway.Meters) %>%
        dplyr::ungroup()
    }

    if (return_sf == TRUE) {
      # CATCHMENT FEATURES
      # use original catchment pull, but return column names to original
      ATTAINS_catchments <- nearby_catchments %>%
        dplyr::filter(ATTAINS.AssessmentUnitIdentifier %in% TADA_with_ATTAINS$ATTAINS.AssessmentUnitIdentifier) %>%
        dplyr::distinct(.keep_all = TRUE)

      colnames(ATTAINS_catchments) <- gsub("ATTAINS.", "", colnames(ATTAINS_catchments))

      # due to the rename, must re-set geometry column:
      sf::st_geometry(ATTAINS_catchments) <- "geometry"

      # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
      ATTAINS_points <- NULL
      try(
        ATTAINS_points <- attains_features[["ATTAINS_points"]] %>%
          # subset to only ATTAINS point features associated with WQP features
          dplyr::filter(assessmentunitidentifier %in% TADA_with_ATTAINS$ATTAINS.AssessmentUnitIdentifier) %>%
          # make sure no duplicate features exist
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )
      if (is.null(ATTAINS_points) || nrow(ATTAINS_points) == 0) {
        ATTAINS_points <- NULL
      }

      # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
      ATTAINS_lines <- NULL
      try(
        ATTAINS_lines <- attains_features[["ATTAINS_lines"]] %>%
          # subset to only ATTAINS line features associated with WQP features
          dplyr::filter(assessmentunitidentifier %in% TADA_with_ATTAINS$ATTAINS.AssessmentUnitIdentifier) %>%
          # make sure no duplicate line features exist
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )
      if (is.null(ATTAINS_lines) || nrow(ATTAINS_lines) == 0) {
        ATTAINS_lines <- NULL
      }

      # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
      ATTAINS_polygons <- NULL
      try(
        ATTAINS_polygons <- attains_features[["ATTAINS_polygons"]] %>%
          # subset to only ATTAINS polygon features associated with WQP features
          dplyr::filter(assessmentunitidentifier %in% TADA_with_ATTAINS$ATTAINS.AssessmentUnitIdentifier) %>%
          # make sure no duplicate polygon features exist
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )
      if (is.null(ATTAINS_polygons) || nrow(ATTAINS_polygons) == 0) {
        ATTAINS_polygons <- NULL
      }

      if (fill_catchments == FALSE) {
        # If there are ATTAINS catchments, return_sf = TRUE, fill_catchments = FALSE:
        final_list <- list(
          "TADA_with_ATTAINS" = TADA_with_ATTAINS,
          "ATTAINS_catchments" = ATTAINS_catchments,
          "ATTAINS_points" = ATTAINS_points,
          "ATTAINS_lines" = ATTAINS_lines,
          "ATTAINS_polygons" = ATTAINS_polygons
        )

        return(final_list)
      }

      if (fill_catchments == TRUE) {
        TADA_without_ATTAINS <- TADA_DataRetrieval_data %>%
          dplyr::filter(ResultIdentifier %in% c(dplyr::filter(TADA_with_ATTAINS, is.na(ATTAINS.AssessmentUnitIdentifier)) %>% dplyr::pull(ResultIdentifier)))

        nhd_catchments <- fetchNHD(.data = TADA_without_ATTAINS, features = "catchments", resolution = resolution)

        TADA_without_ATTAINS <- TADA_without_ATTAINS %>%
          sf::st_join(nhd_catchments, left = TRUE) %>%
          st_drop_geometry()

        # has ATTAINS catchments, return_sf = FALSE, fill_catchments = TRUE
        final_list <- list(
          "TADA_with_ATTAINS" = TADA_with_ATTAINS %>% dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier)),
          "TADA_without_ATTAINS" = TADA_without_ATTAINS,
          "ATTAINS_catchments" = ATTAINS_catchments,
          "ATTAINS_points" = ATTAINS_points,
          "ATTAINS_lines" = ATTAINS_lines,
          "ATTAINS_polygons" = ATTAINS_polygons,
          "without_ATTAINS_catchments" = nhd_catchments
        )

        return(final_list)
      }
    } else { # return_sf is FALSE

      if (fill_catchments == TRUE) {
        TADA_without_ATTAINS <- TADA_DataRetrieval_data %>%
          dplyr::filter(ResultIdentifier %in% c(dplyr::filter(TADA_with_ATTAINS, is.na(ATTAINS.AssessmentUnitIdentifier)) %>% dplyr::pull(ResultIdentifier)))

        nhd_catchments <- fetchNHD(.data = TADA_without_ATTAINS, features = "catchments", resolution = resolution)

        TADA_without_ATTAINS <- TADA_without_ATTAINS %>%
          sf::st_join(nhd_catchments, left = TRUE) %>%
          st_drop_geometry()

        # has ATTAINS catchments, return_sf = FALSE, fill_catchments = TRUE
        final_list <- list(
          "TADA_with_ATTAINS" = TADA_with_ATTAINS %>% dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier)),
          "TADA_without_ATTAINS" = TADA_without_ATTAINS
        )

        return(final_list)
      } else {
        # has ATTAINS catchments, return_sf = FALSE, fill_catchments = FALSE
        return(TADA_with_ATTAINS)
      }
    }
  }
}

#' TADA_GetATTAINSByAUID
#'
#' Returns ATTAINS data for assessment unit identifiers provided by the user.
#'
#' This function can be used to provide information for known assessment unit
#' identifier/monitoring location combinations in order to provide data compatiable
#' with the results of TADA_GetATTAINS for previously unidentified assessment unit
#' identifier/monitoring location combinations.
#'
#' @param .data A TADA data frame including including some results from monitoring
#' locations already paired with assessment unit identifiers.
#'
#' @param au_ref Required. A df containing the existing crosswalk of known
#' AU and monitoring location identifier combinations. Can be created using
#' TADA_GetATTAINSAUMLCrosswalk or provided by the user from an external file.
#' Must contain the columns ATTAINS.MonitoringLocationIdentifier,
#' and ATTAINS.AssessmentUnitIdentifier. The monitoring location identifiers must
#' match those in the WQP, which may contain the organization and provider in
#' the MonitoringLocationIdentifier.
#'
#' @return A modified `TADA_DataRetrieval()` dataframe or list with additional
#' columns associated with the ATTAINS assessment unit data, and, if
#' fill_catchments = TRUE, an additional dataframe of the observations without
#' intersecting ATTAINS features.
#' Moreover, if return_sf = TRUE, this function will additionally return the
#' raw ATTAINS and catchment shapefile features associated with those
#' observations.
#'
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_GetATTAINS()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
TADA_GetATTAINSByAUID <- function(.data, au_ref = NULL, add_catch = FALSE) {
  # function settings that we ensure go back to their original settings
  # after the function stops running:
  original_s2 <- sf::sf_use_s2() # Store the original s2 setting first
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))), add = TRUE)

  attains_names <- c(
    "ATTAINS.OrganizationId", "ATTAINS.SubmissionId", "ATTAINS.HasProtectionPlan",
    "ATTAINS.AssessmentUnitName", "ATTAINS.NhdPlusId", "ATTAINS.Tas303d",
    "ATTAINS.IsThreatened", "ATTAINS.State", "ATTAINS.On303dList",
    "ATTAINS.OrganizationName", "ATTAINS.Region", "ATTAINS.ShapeLength",
    "ATTAINS.ReportingCycle", "ATTAINS.AssmntJoinKey", "ATTAINS.HasTmdl",
    "ATTAINS.OrgType", "ATTAINS.PermIdJoinKey", "ATTAINS.CatchmentIsTribal",
    "ATTAINS.IrCategory", "ATTAINS.WaterbodyReportLink", "ATTAINS.AssessmentUnitIdentifier",
    "ATTAINS.OverallStatus", "ATTAINS.IsAssessed", "ATTAINS.IsImpaired",
    "ATTAINS.Has4bPlan", "ATTAINS.Huc12", "ATTAINS.HasAlternativePlan",
    "ATTAINS.VisionPriority303d", "ATTAINS.AreaSqkm", "ATTAINS.CatchmentAreaSqkm",
    "ATTAINS.CatchmentStateCode", "ATTAINS.CatchmentResolution", "ATTAINS.WaterType",
    "ATTAINS.ShapeArea"
  )

  if (any(attains_names %in% colnames(.data))) {
    stop("Your data has already been joined with ATTAINS data.")
  }

  if (nrow(.data) == 0) {
    # if no WQP observations, return a modified `data` with empty ATTAINS-related columns:
    message("Your dataframe has no observations. Returning an empty dataframe with empty ATTAINS features.")

    # Add ATTAINS columns with NA values
    col_val_list <- stats::setNames(
      object = rep(
        x = list(NA),
        times = length(attains_names)
      ),
      nm = attains_names
    )

    no_WQP_data <- .data %>%
      dplyr::mutate(ResultIdentifier = NA) %>%
      dplyr::bind_cols(col_val_list) %>%
      dplyr::select(ResultIdentifier, dplyr::everything())

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

      # If return_sf == FALSE, then just return the dataframe:
    } else {
      return(no_WQP_data)
    }
  }

  # drop provider from au_ref
  au_ref <- au_ref %>%
    dplyr::select(ATTAINS.AssessmentUnitIdentifier,
                  ATTAINS.MonitoringLocationIdentifier)

  # filter detain to retain only results with known AUIDs
  .data <- .data %>%
    dplyr::filter(TADA.MonitoringLocationIdentifier %in% au_ref$ATTAINS.MonitoringLocationIdentifier)

  filt.data <- .data %>%
    dplyr::select(TADA.MonitoringLocationIdentifier, TADA.LatitudeMeasure,
                  TADA.LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName) %>%
    dplyr::distinct() %>%
    TADA_MakeSpatial()

  # REST for ATTAINS geospatial data:
  baseurls <- c( # ATTAINS catchments:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?",
    # ATTAINS points:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?",
    # ATTAINS lines:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?",
    # ATTAINS polygons:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?"
  )

  # get water type info using EQ
  get_wb_type <- function(au_list) {

    au_list <- unique(au_list)

    # split the au_list into chunks
    chunks <- split(au_list, ceiling(seq_along(unique(au_list)) / 20))

    # get water type
    # need to edit funciton to silent print outs from EQ_AUs

    wat_type <- function(chunk) {
      results <- spsUtil::quiet(rExpertQuery::EQ_AssessmentUnits(
        api_key = "lfzVzpwIlKS1O4l1QmbOLUeTzxyql4QdbHVR5Yf5",
        auid = chunk))
    }

    results <- purrr::map_dfr(.x = chunks, .f = wat_type)

    results <- results %>%
      dplyr::select(assessmentUnitId, waterType) %>%
      dplyr::distinct()


    return(results)
  }

  # get water types
  water_types <- try(
    get_wb_type(au_ref$ATTAINS.AssessmentUnitIdentifier),
    silent = TRUE)


  # function to download ATTAINS features API based on their name
  fetch_au <- function(baseurls, assessment_unit_ids, chunk_n = 1000) {
    # Split the assessment_unit_ids into chunks of 1000
    # API cannot handle more than 1000 features
    id_chunks <- split(assessment_unit_ids, ceiling(seq_along(assessment_unit_ids) / chunk_n))

    # Query API for a chunk of assessment unit IDs
    fetch_chunk <- function(id_chunk) {
      where_clause <- paste0("assessmentunitidentifier IN ('", paste(id_chunk, collapse = "','"), "')")
      query_params <- list(
        where = where_clause,
        outFields = "*",
        f = "geojson"
      )

      response <- httr::GET(baseurls, query = query_params)

      if (httr::status_code(response) != 200) {
        stop("Failed to retrieve data from EPA ATTAINS API.")
      }

      geojson_data <- httr::content(response, as = "text", encoding = "UTF-8")
      sf_object <- sf::st_read(geojson_data, quiet = TRUE)

      return(sf_object)
    }

    # fetch all chunks and combine results
    purrr::map_dfr(id_chunks, fetch_chunk)
  }

  # start grabbing the raw ATTAINS features
  points <- NULL
  lines <- NULL
  polygons <- NULL
  catchments <- NULL

  # Download associated point, line, polygon, and catchment features using list of auids
  try(
    points <- fetch_au(
      baseurls = baseurls[2],
      assessment_unit_ids = paste0(unique(au_ref$ATTAINS.AssessmentUnitIdentifier)),
      chunk_n = 100
    ),
    silent = TRUE
  )

  try(
    lines <- fetch_au(
      baseurls = baseurls[3],
      assessment_unit_ids = paste0(unique(au_ref$ATTAINS.AssessmentUnitIdentifier)),
      chunk_n = 100
    ),
    silent = TRUE
  )

  try(
    polygons <- fetch_au(
      baseurls = baseurls[4],
      assessment_unit_ids = paste0(unique(au_ref$ATTAINS.AssessmentUnitIdentifier)),
      chunk_n = 100
    ),
    silent = TRUE
  )

  try(points <- points %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitId")),
      silent = TRUE
  )

  try(lines <- lines %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitId")),
      silent = TRUE
  )

  try(polygons <- polygons %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitId")),
      silent = TRUE
  )

  if(add_catch == TRUE) {

    try(
      catchments <- fetch_au(
        baseurls = baseurls[1],
        assessment_unit_ids = paste0(unique(au_ref$ATTAINS.AssessmentUnitIdentifier)),
        chunk_n = 10
      ),
      silent = TRUE
    )

    # get one catchment per WQP location
    catchments.cw <- filt.data %>%
      dplyr::distinct() %>%
      sf::st_join(catchments, join = sf::st_nearest_feature) %>%
      dplyr::group_by(TADA.MonitoringLocationIdentifier) %>%
      dplyr::mutate(catchCount = dplyr::n()) %>%
      dplyr::select(TADA.MonitoringLocationIdentifier, nhdplusid) %>%
      dplyr::distinct() %>%
      sf::st_drop_geometry()

    catchments.filt <- catchments %>%
      dplyr::filter(nhdplusid %in% catchments.cw$nhdplusid)

    catchments.no.geo <- catchments %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct()

    try(catchments <- catchments.filt %>% dplyr::left_join(., water_types, by = c("assessmentunitidentifier" = "assessmentUnitId")),
        silent = TRUE
    )

    # create internal function to rename cols coming from ATTAINS geospatial
    TADA_with_ATTAINS <- .data %>%
      dplyr::left_join(au_ref, by = c("TADA.MonitoringLocationIdentifier" =
                                        "ATTAINS.MonitoringLocationIdentifier")) %>%
      dplyr::left_join(catchments.cw,
                       dplyr::join_by(TADA.MonitoringLocationIdentifier)) %>%
      dplyr::left_join(catchments.no.geo,
                       by = c("nhdplusid" = "nhdplusid",
                              "ATTAINS.AssessmentUnitIdentifier" =
                                "assessmentunitidentifier")) %>%
      dplyr::select(-OBJECTID) %>%
      dplyr::rename(ATTAINS.SubmissionId = submissionid,
                    ATTAINS.NhdPlusId = nhdplusid,
                    ATTAINS.State = state,
                    ATTAINS.Region = region,
                    ATTAINS.OrganizationId = organizationid,
                    ATTAINS.OrgType = orgtype,
                    ATTAINS.Tas303d = tas303d,
                    ATTAINS.OrganizationName = organizationname,
                    ATTAINS.ReportingCycle = reportingcycle,
                    ATTAINS.AssessmentUnitName = assessmentunitname,
                    ATTAINS.WaterbodyReportLink = waterbodyreportlink,
                    ATTAINS.AssmntJoinKey = assmnt_joinkey,
                    ATTAINS.PermIdJoinKey = permid_joinkey,
                    ATTAINS.IrCategory = ircategory,
                    ATTAINS.OverallStatus = overallstatus,
                    ATTAINS.IsAssessed = isassessed,
                    ATTAINS.IsImpaired = isimpaired,
                    ATTAINS.IsThreatened = isthreatened,
                    ATTAINS.On303dList = on303dlist,
                    ATTAINS.HasTmdl = hastmdl,
                    ATTAINS.Has4bPlan = has4bplan,
                    ATTAINS.HasAlternativePlan = hasalternativeplan,
                    ATTAINS.HasProtectionPlan = hasprotectionplan,
                    ATTAINS.VisionPriority303d =  visionpriority303d,
                    ATTAINS.AreaSqkm = areasqkm,
                    ATTAINS.Huc12 = huc12,
                    ATTAINS.XwalkMethod = xwalk_method,
                    ATTAINS.WwalkHuc12Version= xwalk_huc12_version,
                    ATTAINS.CatchmentAreaSqkm = catchmentareasqkm,
                    ATTAINS.CatchmentStateCode = catchmentstatecode,
                    ATTAINS.CatchmentIsTribal = catchmentistribal,
                    ATTAINS.CatchmentResolution = catchmentresolution,
                    ATTAINS.ShapeLength = Shape_Length,
                    ATTAINS.ShapeArea = Shape_Area,
                    ATTAINS.WaterType = waterType
      )
  }

  if(add_catch == FALSE) {

    catchments <- NULL

  TADA_with_ATTAINS <- .data %>%
    dplyr::left_join(au_ref, by = c("TADA.MonitoringLocationIdentifier" =
                                      "ATTAINS.MonitoringLocationIdentifier")) %>%
    # when catchment/monitoring location crosswalk is available, that info can be added here
    # can add the assessment related info sooner via rExpertQuery functions (HRM 8/7/25)
    dplyr::mutate(ATTAINS.SubmissionId = NA,
                  ATTAINS.NhdPlusId = NA,
                  ATTAINS.State = NA,
                  ATTAINS.Region = NA,
                  ATTAINS.OrganizationId = NA,
                  ATTAINS.OrgType = NA,
                  ATTAINS.Tas303d = NA,
                  ATTAINS.OrganizationName = NA,
                  ATTAINS.ReportingCycle = NA,
                  ATTAINS.AssessmentUnitName = NA,
                  ATTAINS.WaterbodyReportLink = NA,
                  ATTAINS.AssmntJoinKey = NA,
                  ATTAINS.PermIdJoinKey = NA,
                  ATTAINS.IrCategory = NA,
                  ATTAINS.OverallStatus = NA,
                  ATTAINS.IsAssessed = NA,
                  ATTAINS.IsImpaired = NA,
                  ATTAINS.IsThreatened = NA,
                  ATTAINS.On303dList = NA,
                  ATTAINS.HasTmdl = NA,
                  ATTAINS.Has4bPlan = NA,
                  ATTAINS.HasAlternativePlan = NA,
                  ATTAINS.HasProtectionPlan = NA,
                  ATTAINS.VisionPriority303d =  NA,
                  ATTAINS.AreaSqkm = NA,
                  ATTAINS.Huc12 = NA,
                  ATTAINS.XwalkMethod = NA,
                  ATTAINS.WwalkHuc12Version= NA,
                  ATTAINS.CatchmentAreaSqkm = NA,
                  ATTAINS.CatchmentStateCode = NA,
                  ATTAINS.CatchmentIsTribal = NA,
                  ATTAINS.CatchmentResolution = NA,
                  ATTAINS.ShapeLength = NA,
                  ATTAINS.ShapeArea = NA,
                  ATTAINS.WaterType = NA
    )
  }

  final_features <- list(
    "TADA_with_ATTAINS" = TADA_with_ATTAINS,
    "ATTAINS_catchments" = catchments,
    "ATTAINS_points" = points,
    "ATTAINS_lines" = lines,
    "ATTAINS_polygons" = polygons
  )

    return(final_features)

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
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' attains_data <- TADA_GetATTAINS(tada_data,
#'   fill_catchments = TRUE,
#'   return_nearest = TRUE, resolution = "hi", return_sf = TRUE
#' )
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
    dplyr::filter(!assessmentunitidentifier %in% c(
      ATTAINS_points$assessmentunitidentifier,
      ATTAINS_lines$assessmentunitidentifier,
      ATTAINS_polygons$assessmentunitidentifier
    )), silent = TRUE)

  if (!"without_ATTAINS_catchments" %in% names(.data)) {
    if (nrow(ATTAINS_table) == 0) {
      stop("Your WQP dataframe has no observations.")
    }

    required_columns <- c(
      "TADA.LongitudeMeasure", "TADA.LatitudeMeasure",
      "HorizontalCoordinateReferenceSystemDatumName",
      "TADA.CharacteristicName", "TADA.MonitoringLocationIdentifier",
      "TADA.MonitoringLocationName", "ResultIdentifier",
      "ActivityStartDate", "TADA.OrganizationIdentifier"
    )

    if (!any(required_columns %in% colnames(ATTAINS_table))) {
      stop("Your dataframe does not contain the necessary WQP-style column names.")
    }

    suppressMessages(suppressWarnings({
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
          ATTAINS_AUs = as.character(list(unique(ATTAINS.AssessmentUnitIdentifier))),
          TADA.AURefSource = ifelse("TADA.AURefSource" %in% names(ATTAINS_table),
                                    as.character(TADA.AURefSource),
                                    "not provided")
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

      if("TADA.AURefSource" %in% names(ATTAINS_table)) {

      # if TADA_AUSourceRef col exists in data
      set.fill <- leaflet::colorFactor(
        palette = c(tada.pal[1], tada.pal[12], tada.pal[13]),
        domain =  c("User-supplied Ref",
          "ATTAINS crosswalk",
          "TADA_GetATTAINS"))

      set.popup <- paste0(
        "Site ID: ", sumdat$MonitoringLocationIdentifier,
        "<br> Site Name: ", sumdat$MonitoringLocationName,
        "<br> Measurement Count: ", sumdat$Sample_Count,
        "<br> Visit Count: ", sumdat$Visit_Count,
        "<br> Characteristic Count: ", sumdat$Parameter_Count,
        "<br> ATTAINS Assessment Unit(s): ", sumdat$ATTAINS_AUs,
        "<br> Crosswalk Source: ", sumdat$TADA.AURefSource
      )


      }

      if(!"TADA.AURefSource" %in% names(ATTAINS_table)) {

        # if TADA_AUSourceRef col exists in data
        set.fill <- leaflet::colorFactor(
          palette = c(tada.pal[1]),
          domain = sumdat$TADA.AURefSource)

        set.popup <- paste0(
          "Site ID: ", sumdat$MonitoringLocationIdentifier,
          "<br> Site Name: ", sumdat$MonitoringLocationName,
          "<br> Measurement Count: ", sumdat$Sample_Count,
          "<br> Visit Count: ", sumdat$Visit_Count,
          "<br> Characteristic Count: ", sumdat$Parameter_Count,
          "<br> ATTAINS Assessment Unit(s): ", sumdat$ATTAINS_AUs
        )
      }


             # Add WQP observation features (should always exist):
      try(
        map <- map %>%
          leaflet::addCircleMarkers(
            data = sumdat,
            lng = ~LongitudeMeasure, lat = ~LatitudeMeasure,
            color = "grey", fillColor = ~set.fill(TADA.AURefSource),
            fillOpacity = 0.8, stroke = TRUE, weight = 1.5, radius = 6,
            popup = set.popup
            ),
        silent = TRUE
      )

      if (is.null(ATTAINS_lines) & is.null(ATTAINS_points) & is.null(ATTAINS_polygons)) {
        message("No ATTAINS data associated with this Water Quality Portal data.")
      }

      # Return leaflet map of TADA WQ and its associated ATTAINS data
      return(map)
    }))
  }

  if ("without_ATTAINS_catchments" %in% names(.data)) {
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
      # if data was spatial, remove for downstream leaflet dev.
      # But first if no data in the ATTAINS table, add in required column names to
      # without ATTAINS data:
      if (nrow(ATTAINS_table) == 0) {
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
          ATTAINS_AUs = as.character(list(unique(ATTAINS.AssessmentUnitIdentifier)))
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
        message("No ATTAINS data associated with this Water Quality Portal data.")
      }

      # Return leaflet map of TADA WQ and its associated ATTAINS data
      return(map)
    }))
  }
}


#' TADA_CreateAUMLRef
#'
#' Create the assessment unit and monitoring location ref by utilizing an optional
#' user-supplied crosswalk, AU/ML crosswalk from ATTAINS (if org has entered that data),
#' and TADA_GetATTAINS to match unassigned monitoring locations to assessment units.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()`.
#' @param au_ref Optional. A user-supplied df with the columns AssessmentUnitIdentifier
#' and MonitoringLocationIdentifier.
#' @param org_id Organization id to match AUs.
#' @param add_catch Optional. When add_catch = TRUE, catchments are matched to monitoring
#' locations from  the user-supplied and ATTAINS crosswalk monitoring locations. Fetching
#' and matching these additional geospatial data will increase the run time of this function
#' significantly. Default is add_catch = FALSE.
#'
#' @return Need to add the full list
#'
#' @seealso [TADA_GetATTAINS()] # add additional functions here
#'
#' @export
#'
#' @examples
#' \dontrun{ }
#'
TADA_CreateAUMLRef <- function(.data, au_ref = NULL,
                               org_id = NULL, add_catch = FALSE) {
  # need to write checks for each component

  # check for user supplied ref
  if(is.null(au_ref)) {

    user.matches <- list(
      "TADA_with_ATTAINS" = NULL,
      "ATTAINS_catchments" = NULL,
      "ATTAINS_points" = NULL,
      "ATTAINS_lines" = NULL,
      "ATTAINS_polygons" = NULL
    )
  }

  if(!is.null(au_ref)) {

    if(!is.data.frame(au_ref)) {
      stop(paste0("TADA_CreateAUMLRef: The user supplied au_ref must be a data frame ",
                  "containg the columns AssessmentUnitIdentifier and MonitoringLocationIdentifier.",
                  "MonitoringLocationIdentifiers must be WQP compatible."))
    }

    if(is.data.frame(au_ref)) {

      print("TADA_CreateAUMLRef: fetching geospatial data for user-supplied crosswalk.")

      req.cols <- c("AssessmentUnitIdentifier",
                    "MonitoringLocationIdentifier")

      # should this be using a more generic function?
      TADA_CheckColumns(au_ref, req.cols)

      # rename au_ref cols for nex function
      au_ref <- au_ref %>%
        dplyr::rename(ATTAINS.MonitoringLocationIdentifier = MonitoringLocationIdentifier,
                      ATTAINS.AssessmentUnitIdentifier = AssessmentUnitIdentifier)

      # subset data for au_ref
      au.ref.mls <- .data %>%
        dplyr::filter(TADA.MonitoringLocationIdentifier %in% au_ref$ATTAINS.MonitoringLocationIdentifier) %>%
        dplyr::mutate(TADA.AURefSource = "User-supplied Ref")

      # get geospatial data for au_ref monitoring locations
      user.matches <- spsUtil::quiet(
        TADA_GetATTAINSByAUID(au.ref.mls, au_ref = au_ref, add_catch = add_catch))
    }
  }

  # ATTAINS supplied ref section
  # get attains crosswalk

  print("TADA_CreateAUMLRef: checking for crosswalk in ATTAINS.")

  attains.cw <- spsUtil::quiet(
    TADA_GetATTAINSAUMLCrosswalk(org_id = org_id))

  if(is.null(attains.cw)) {

    print(paste0("TADA_CreateAUMLRef: There are no MonitoringLocation records ",
                 "in ATTAINS for ", org_id, "."))

    attains.matches <- list(
      "TADA_with_ATTAINS" = NULL,
      "ATTAINS_catchments" = NULL,
      "ATTAINS_points" = NULL,
      "ATTAINS_lines" = NULL,
      "ATTAINS_polygons" = NULL
    )
  }

  if(!is.null(attains.cw)) {
    # we could remove or make this step optional, but it is very helpful for making sure
    # monitoring location identifiers are WQP compatible

    print("TADA_CreateAUMLRef: crosswalk from ATTAINS has been imported.")

    attains.cw <- spsUtil::quiet(
      TADA_UpdateMonitoringLocationsInATTAINS(crosswalk = attains.cw,
                                                          org_id = org_id,
                                                          attains_replace = TRUE))

    attains.cw.mls <- .data %>%
      dplyr::filter(!TADA.MonitoringLocationIdentifier %in% au.ref.mls$TADA.MonitoringLocationIdentifier,
                    TADA.MonitoringLocationIdentifier %in% attains.cw$ATTAINS.MonitoringLocationIdentifier) %>%
      dplyr::mutate(TADA.AURefSource = "ATTAINS crosswalk")

    print("TADA_CreateAUMLRef: fetching geospatial data for crosswalk from ATTAINS.")
    # get geospatial data for attains cw monitoring locations
    attains.matches <- spsUtil::quiet(
      TADA_GetATTAINSByAUID(attains.cw.mls, au_ref = attains.cw, add_catch = add_catch))
  }

  # TADA_GetATTAINS section

  print("TADA_CreateAUMLRef: checking to see if any unmatched MonitoringLocations reamin.")

  get.attains.mls <- .data %>%
    dplyr::filter(!TADA.MonitoringLocationIdentifier %in% au.ref.mls$TADA.MonitoringLocationIdentifier,
                  !TADA.MonitoringLocationIdentifier %in% attains.cw.mls$TADA.MonitoringLocationIdentifier) %>%
    dplyr::mutate(TADA.AURefSource = "TADA_GetATTAINS")

  # add code here for if there are no remaning mls to match
  if(dim(get.attains.mls)[1] == 0) {

    print("TADA_CreateAUMLRef: all MonitoringLocations have already been matched by user or ATTAINS.")

    get.attains.matches <- list(
      "TADA_with_ATTAINS" = NULL,
      "ATTAINS_catchments" = NULL,
      "ATTAINS_points" = NULL,
      "ATTAINS_lines" = NULL,
      "ATTAINS_polygons" = NULL
    )

  }

  if(dim(get.attains.mls)[1] > 0) {

    print("TADA_CreateAUMLRef: using TADA_GetATTAINS to match remaining MonitoringLocations.")

    # use get attains for matching remaining monitoring locations
    get.attains.matches <-  spsUtil::quiet(
      TADA_GetATTAINS(get.attains.mls, return_nearest = TRUE))
  }

  # join all the resulting tables within each list to return as one large list
  #TADA_with_ATTAINS

  print("TADA_CreateAUMLRef: joining results to return list of dataframes compatible with TADA_ViewATTAINS.")

  TADA_with_ATTAINS <- user.matches$TADA_with_ATTAINS %>%
    plyr::rbind.fill(attains.matches$TADA_with_ATTAINS) %>%
    plyr::rbind.fill(get.attains.matches$TADA_with_ATTAINS) %>%
    plyr::rbind.fill() %>%
    sf::st_as_sf()

  ATTAINS_catchments <- user.matches$ATTAINS_catchments %>%
    plyr::rbind.fill(attains.matches$ATTAINS_catchments) %>%
    plyr::rbind.fill(get.attains.matches$ATTAINS_catchments) %>%
    dplyr::distinct() %>%
    sf::st_as_sf()

  ATTAINS_lines <- user.matches$ATTAINS_lines %>%
    plyr::rbind.fill(attains.matches$ATTAINS_lines) %>%
    plyr::rbind.fill(get.attains.matches$ATTAINS_lines) %>%
    plyr::rbind.fill() %>%
    sf::st_as_sf()

  ATTAINS_points <- user.matches$ATTAINS_points %>%
    plyr::rbind.fill(attains.matches$ATTAINS_points) %>%
    plyr::rbind.fill(get.attains.matches$ATTAINS_points) %>%
    dplyr::distinct() %>%
    sf::st_as_sf()

  ATTAINS_polygons <- user.matches$ATTAINS_polygons %>%
    plyr::rbind.fill(attains.matches$ATTAINS_polygons) %>%
    plyr::rbind.fill(get.attains.matches$ATTAINS_polygons) %>%
    dplyr::distinct() %>%
    sf::st_as_sf()


  final_list <- list(
    "TADA_with_ATTAINS" = TADA_with_ATTAINS,
    "ATTAINS_catchments" = ATTAINS_catchments,
    "ATTAINS_points" = ATTAINS_points,
    "ATTAINS_lines" = ATTAINS_lines,
    "ATTAINS_polygons" = ATTAINS_polygons
  )

  return(final_list)
}
