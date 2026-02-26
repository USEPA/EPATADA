#' TADA_MakeSpatial
#'
#' Transforms a Water Quality Portal dataframe into a geospatial `sf` object.
#'
#' This function adds a new column, 'geometry', to the input dataframe, enabling mapping and additional
#' geospatial capabilities. For an example workflow, refer to the TADAModule2.Rmd file.
#'
#' @param .data A dataframe that has been processed using `TADA_DataRetrieval()` and `TADA_AutoClean()`.
#' @param crs The coordinate reference system (CRS) for the returned point features. The default is CRS 4326 (WGS84).
#'
#' @return An `sf` object, which is the original TADA Water Quality Portal dataframe transformed into geospatial point objects.
#'
#' @seealso [TADA_DataRetrieval()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve water quality data
#' tada_not_spatial <- TADA_DataRetrieval(
#'   characteristicName = "pH",
#'   statecode = "SC",
#'   countycode = "Abbeville",
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#'
#' # Convert `tada_not_spatial` into an `sf` object, projected in CRS 4269 (NAD83)
#' tada_spatial <- TADA_MakeSpatial(tada_not_spatial, crs = 4269)
#' }
#'
TADA_MakeSpatial <- function(.data, crs = 4326) {
  # Check if necessary columns are present in the dataframe
  if (
    !"TADA.LongitudeMeasure" %in% names(.data) ||
      !"TADA.LatitudeMeasure" %in% names(.data) ||
      !"HorizontalCoordinateReferenceSystemDatumName" %in% names(.data)
  ) {
    stop(
      "The dataframe does not contain TADA-style latitude and longitude data (column names `HorizontalCoordinateReferenceSystemDatumName`, `TADA.LatitudeMeasure`, and `TADA.LongitudeMeasure`)."
    )
  } else if (!is.null(.data) && inherits(.data, "sf")) {
    # Check if the data is already an `sf` object
    stop("Your data is already a spatial object.")
  }

  message("Transforming your data into a spatial object.")

  suppressMessages(suppressWarnings({
    # Create a reference table for CRS and EPSG codes using `tribble`
    epsg_codes <- tidyr::tribble(
      ~HorizontalCoordinateReferenceSystemDatumName , ~epsg ,
      "NAD83"                                       ,  4269 ,
      "WGS84"                                       ,  4326 ,
      "NAD27"                                       ,  4267 ,
      "UNKWN"                                       , crs   ,
      "Unknown"                                     , crs   ,
      "OTHER"                                       , crs   ,
      "OLDHI"                                       ,  4135 ,
      "AMSMA"                                       ,  4169 ,
      "ASTRO"                                       ,  4727 ,
      "GUAM"                                        ,  4675 ,
      "JHNSN"                                       ,  4725 ,
      "PR"                                          ,  6139 ,
      "SGEOR"                                       ,  4138 ,
      "SLAWR"                                       ,  4136 ,
      "SPAUL"                                       ,  4137 ,
      "WAKE"                                        ,  6732 ,
      "WGS72"                                       ,  6322 ,
      "HARN"                                        ,  4152
    )

    # Handle missing or unknown CRS values
    if (
      any(is.na(.data$HorizontalCoordinateReferenceSystemDatumName)) ||
        any(
          .data$HorizontalCoordinateReferenceSystemDatumName %in%
            c("UNKWN", "Unknown", "OTHER")
        )
    ) {
      message(paste0(
        "Your WQP dataframe contains observations without a listed coordinate reference system (CRS). For these, we have assigned CRS ",
        crs,
        "."
      ))
      .data$HorizontalCoordinateReferenceSystemDatumName[is.na(
        .data$HorizontalCoordinateReferenceSystemDatumName
      )] <- "Unknown"
    }

    # Prepare the data for spatial transformation
    sf <- .data |>
      dplyr::select(-dplyr::any_of("epsg")) |>
      dplyr::left_join(
        epsg_codes,
        by = "HorizontalCoordinateReferenceSystemDatumName"
      ) |>
      dplyr::mutate(
        lat = as.numeric(TADA.LatitudeMeasure),
        lon = as.numeric(TADA.LongitudeMeasure)
      )

    print("Data after CRS assignment:")
    print(sf)

    # Transform each subset of data into an `sf` object
    sf <- purrr::map_df(
      split(sf, sf$HorizontalCoordinateReferenceSystemDatumName),
      function(subset_data) {
        print(paste(
          "Processing CRS:",
          unique(subset_data$HorizontalCoordinateReferenceSystemDatumName)
        ))
        if (nrow(subset_data) == 0) {
          message(
            "Empty subset detected for CRS:",
            unique(subset_data$HorizontalCoordinateReferenceSystemDatumName)
          )
          return(NULL)
        }
        # Convert to `sf` object and transform to the specified CRS
        sf_object <- sf::st_as_sf(
          subset_data,
          coords = c("lon", "lat"), # Specify coordinate columns
          crs = unique(subset_data$epsg) # Use EPSG code for CRS
        )
        sf::st_transform(sf_object, sf::st_crs(as.numeric(crs))) # Transform to target CRS
      }
    )
  }))

  return(sf) # Return the transformed `sf` object
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
#' @param org_id ATTAINS organization identifier(s) as a character string.
#' If populated, Assessment Units  will only be fetched from the specified
#' organization(s). A list of organization identifiers can be found
#' by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param. When
#' org_id = "all", Assessment Units from all organizations will be considered.
#' The default is "all".
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
fetchATTAINS <- function(.data, catchments_only = FALSE, org_id = "all") {
  original_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(
    suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))),
    add = TRUE
  )

  message(
    "Depending on your data's observation count and its spatial range, the ATTAINS pull may take a while."
  )

  our_epsg <- 4326

  if (!is.null(.data) && inherits(.data, "sf")) {
    .data <- .data |>
      sf::st_transform(crs = our_epsg) |>
      dplyr::distinct(geometry, .keep_all = TRUE)
  }

  if (
    !"TADA.LongitudeMeasure" %in% colnames(.data) ||
      !"TADA.LatitudeMeasure" %in% colnames(.data) ||
      !"HorizontalCoordinateReferenceSystemDatumName" %in% colnames(.data)
  ) {
    stop(
      "The dataframe does not contain TADA-style latitude and longitude data (column names `HorizontalCoordinateReferenceSystemDatumName`, `TADA.LatitudeMeasure`, and `TADA.LongitudeMeasure`)."
    )
  }

  if (!is.null(.data) && !inherits(.data, "sf")) {
    # Convert the data to a data.table and ensure distinct latitude and longitude
    distinct_data <- .data |>
      data.table::data.table() |>
      dplyr::distinct(
        TADA.LongitudeMeasure,
        TADA.LatitudeMeasure,
        .keep_all = TRUE
      )

    # Transform the distinct data into an `sf` object
    .data <- TADA_MakeSpatial(.data = distinct_data, crs = our_epsg)
  }

  if (is.null(.data) || nrow(.data) == 0) {
    stop(
      "There is no data in your `data` object to use as a bounding box for selecting ATTAINS features."
    )
  }

  baseurls <- c(
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?",
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?",
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?",
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?"
  )

  fetch_bbox <- function(baseurls, sf_bbox) {
    offset <- 0
    all_features <- list()

    repeat {
      query <- baseurls |>
        urltools::param_set(key = "geometry", value = sf_bbox) |>
        urltools::param_set(key = "inSR", value = our_epsg) |>
        urltools::param_set(key = "resultRecordCount", value = 100) |>
        urltools::param_set(key = "resultOffset", value = offset) |>
        urltools::param_set(
          key = "spatialRel",
          value = "esriSpatialRelIntersects"
        ) |>
        urltools::param_set(key = "f", value = "geojson") |>
        urltools::param_set(key = "outFields", value = "*") |>
        urltools::param_set(
          key = "geometryType",
          value = "esriGeometryEnvelope"
        ) |>
        urltools::param_set(key = "returnGeometry", value = "true") |>
        urltools::param_set(key = "returnTrueCurves", value = "false") |>
        urltools::param_set(key = "returnIdsOnly", value = "false") |>
        urltools::param_set(key = "returnCountOnly", value = "false") |>
        urltools::param_set(key = "returnZ", value = "false") |>
        urltools::param_set(key = "returnM", value = "false") |>
        urltools::param_set(key = "returnDistinctValues", value = "false") |>
        urltools::param_set(key = "returnExtentOnly", value = "false") |>
        urltools::param_set(key = "featureEncoding", value = "esriDefault")

      features <- suppressMessages(suppressWarnings({
        tryCatch(geojsonsf::geojson_sf(url(query)), error = function(e) NULL)
      }))

      if (is.null(features) || nrow(features) == 0) {
        break
      }

      all_features <- c(all_features, list(features))
      offset <- offset + 100
    }

    dplyr::bind_rows(all_features) |> dplyr::distinct(.keep_all = TRUE)
  }

  if (org_id == "all") {
    org_filter <- "1=1"
  } else {
    org_filter <- paste0(
      "organizationid IN ('",
      paste(org_id, collapse = "','"),
      "')"
    )
  }

  fetch_au <- function(baseurls, assessment_unit_ids) {
    id_chunks <- split(
      assessment_unit_ids,
      ceiling(seq_along(assessment_unit_ids) / 100)
    )

    fetch_chunk <- function(id_chunk) {
      where_clause <- paste0(
        "assessmentunitidentifier IN ('",
        paste(id_chunk, collapse = "','"),
        "') AND ",
        org_filter
      )

      query_params <- list(where = where_clause, outFields = "*", f = "geojson")

      response <- httr::GET(baseurls, query = query_params)

      if (httr::status_code(response) != 200) {
        stop("Failed to retrieve data from EPA ATTAINS API.")
      }

      geojson_data <- httr::content(response, as = "text", encoding = "UTF-8")
      sf::st_read(geojson_data, quiet = TRUE)
    }

    purrr::map_dfr(id_chunks, fetch_chunk)
  }

  grab_waterbody_type <- function(au_list, chunk_size = 50) {
    num_chunks <- ceiling(length(au_list) / chunk_size)
    chunks <- split(au_list, ceiling(seq_along(au_list) / chunk_size))
    water_types <- vector("list", length = length(chunks))

    for (i in seq_along(chunks)) {
      dat <- httr::GET(utils::URLencode(paste0(
        "https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=",
        paste(chunks[[i]], collapse = ",")
      ))) |>
        httr::content(as = "text", encoding = "UTF-8") |>
        jsonlite::fromJSON()

      water_types[[i]] <- dat[["items"]] |>
        tidyr::unnest("assessmentUnits") |>
        tidyr::unnest("waterTypes") |>
        dplyr::select(assessmentUnitIdentifier, waterTypeCode)
    }
    dplyr::bind_rows(water_types)
  }

  if (as.numeric(sf::st_area(sf::st_as_sfc(.data |> sf::st_bbox()))) >= 6e+9) {
    perform_iterative_clustering <- function(
      points_sf,
      min_area = 6e+9,
      max_iterations = 100
    ) {
      bbox_area <- function(df, clust) {
        df |>
          dplyr::filter(cluster == clust) |>
          sf::st_bbox() |>
          sf::st_as_sfc() |>
          sf::st_area() |>
          tidyr::as_tibble() |>
          dplyr::mutate(cluster = clust)
      }

      cluster_iteration <- function(points, eps, min_pts, iteration) {
        coords <- sf::st_coordinates(points)
        fr <- dbscan::frNN(coords, eps = eps)
        clusters <- dbscan::dbscan(fr, minPts = min_pts)$cluster

        cluster_ids <- ifelse(
          clusters == -1,
          paste0("noise_", iteration),
          paste0("cluster_", iteration, "_", clusters)
        )

        points |> dplyr::mutate(cluster = cluster_ids, iteration = iteration)
      }

      has_large_clusters <- function(points) {
        if (nrow(points) == 0) {
          return(FALSE)
        }

        areas <- unique(points$cluster) |>
          purrr::map_dfr(~ bbox_area(df = points, clust = .))
        any(as.numeric(areas$value) > min_area)
      }

      split_clusters_by_area <- function(points, min_area) {
        cluster_areas <- unique(points$cluster) |>
          purrr::map_dfr(~ bbox_area(df = points, clust = .))

        large_clusters <- cluster_areas |>
          dplyr::filter(as.numeric(value) > min_area)

        small_clusters <- cluster_areas |>
          dplyr::filter(as.numeric(value) <= min_area)

        large_points <- points |>
          dplyr::filter(cluster %in% large_clusters$cluster)

        small_points <- points |>
          dplyr::filter(cluster %in% small_clusters$cluster)

        list(
          large = large_points,
          small = small_points,
          large_areas = large_clusters,
          small_areas = small_clusters
        )
      }

      all_small_clusters <- list()
      current_points <- points_sf |> dplyr::distinct(geometry)
      iteration <- 1

      eps_sequence <- c(0.25, 0.05, 1, .1)
      eps_index <- 1

      while (nrow(current_points) > 0 && iteration <= max_iterations) {
        current_eps <- eps_sequence[eps_index]
        eps_index <- (eps_index %% length(eps_sequence)) + 1

        clustered_points <- cluster_iteration(
          current_points,
          eps = current_eps,
          min_pts = 1,
          iteration = iteration
        )

        split_results <- split_clusters_by_area(clustered_points, min_area)

        if (nrow(split_results$small) > 0) {
          all_small_clusters[[paste0(
            "iteration_",
            iteration
          )]] <- split_results$small
        }

        if (nrow(split_results$large) == 0) {
          break
        }

        current_points <- split_results$large
        iteration <- iteration + 1
      }

      final_clusters <- dplyr::bind_rows(all_small_clusters) |>
        dplyr::arrange(iteration)

      if (iteration == max_iterations) {
        warning(
          "Maximum iterations reached. Some clusters may still exceed the area threshold."
        )
      }

      list(
        clusters = final_clusters,
        clusters_by_iteration = all_small_clusters,
        total_iterations = iteration,
        final_eps = current_eps
      )
    }

    points_sf <- dplyr::distinct(.data, geometry)

    init <- perform_iterative_clustering(points_sf = points_sf)
    init_clusters <- init[["clusters_by_iteration"]] |> dplyr::bind_rows()

    final_cluster_list <- points_sf |>
      dplyr::filter(!geometry %in% init$geometry) |>
      tibble::rowid_to_column(var = "cluster") |>
      dplyr::mutate(cluster = as.character(cluster)) |>
      dplyr::bind_rows(init)

    catchment_features <- vector(
      "list",
      length = length(unique(final_cluster_list$cluster))
    )

    for (i in seq_along(unique(final_cluster_list$cluster))) {
      suppressMessages(suppressWarnings({
        bbox <- final_cluster_list |>
          dplyr::filter(cluster == unique(final_cluster_list$cluster)[i]) |>
          sf::st_bbox() |>
          toString() |>
          urltools::url_encode()
      }))

      catchment_features[[i]] <- fetch_bbox(
        baseurls = baseurls[1],
        sf_bbox = bbox
      )
    }

    catchment_features <- catchment_features |>
      purrr::keep(~ !is.null(.)) |>
      purrr::keep(~ nrow(.) > 0) |>
      dplyr::bind_rows()

    try(
      {
        catchment_features <- catchment_features |> (\(x) x[points_sf, ])()
      },
      silent = TRUE
    )

    if (length(catchment_features) == 0 || is.null(catchment_features)) {
      message(
        "There are no ATTAINS features associated with your WQP observations."
      )
    } else {
      all_units <- unique(catchment_features$assessmentunitidentifier)
      water_types <- grab_waterbody_type(all_units, chunk_size = 50)
      try(
        catchment_features <- dplyr::left_join(
          catchment_features,
          water_types,
          by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
        )
      )
    }

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
      points <- points |>
        dplyr::left_join(
          water_types,
          by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
        ),
      silent = TRUE
    )

    try(
      lines <- lines |>
        dplyr::left_join(
          water_types,
          by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
        ),
      silent = TRUE
    )

    try(
      polygons <- polygons |>
        dplyr::left_join(
          water_types,
          by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
        ),
      silent = TRUE
    )

    final_features <- list(
      "ATTAINS_catchments" = dplyr::distinct(catchment_features),
      "ATTAINS_points" = dplyr::distinct(points),
      "ATTAINS_lines" = dplyr::distinct(lines),
      "ATTAINS_polygons" = dplyr::distinct(polygons)
    )

    return(final_features)
  } else {
    points_sf <- .data

    bbox <- points_sf |> sf::st_bbox() |> toString() |> urltools::url_encode()

    catchment_features <- fetch_bbox(baseurls = baseurls[1], sf_bbox = bbox)

    try(
      {
        catchment_features <- catchment_features |> (\(x) x[points_sf, ])()
      },
      silent = TRUE
    )

    if (length(catchment_features) == 0 || is.null(catchment_features)) {
      message(
        "There are no ATTAINS features associated with your WQP observations."
      )
    } else {
      all_units <- unique(catchment_features$assessmentunitidentifier)
      water_types <- grab_waterbody_type(all_units, chunk_size = 50)
      try(
        catchment_features <- dplyr::left_join(
          catchment_features,
          water_types,
          by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
        ),
        silent = TRUE
      )
    }

    if (catchments_only == TRUE) {
      return(list("ATTAINS_catchments" = catchment_features))
    }

    suppressMessages({
      suppressWarnings({
        points <- NULL
        lines <- NULL
        polygons <- NULL

        try(
          points <- fetch_au(
            baseurls = baseurls[2],
            assessment_unit_ids = unique(
              catchment_features$assessmentunitidentifier
            )
          ),
          silent = TRUE
        )

        try(
          lines <- fetch_au(
            baseurls = baseurls[3],
            assessment_unit_ids = unique(
              catchment_features$assessmentunitidentifier
            )
          ),
          silent = TRUE
        )

        try(
          polygons <- fetch_au(
            baseurls = baseurls[4],
            assessment_unit_ids = unique(
              catchment_features$assessmentunitidentifier
            )
          ),
          silent = TRUE
        )

        try(
          points <- points |>
            dplyr::left_join(
              water_types,
              by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
            ),
          silent = TRUE
        )

        try(
          lines <- lines |>
            dplyr::left_join(
              water_types,
              by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
            ),
          silent = TRUE
        )

        try(
          polygons <- polygons |>
            dplyr::left_join(
              water_types,
              by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
            ),
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
  on.exit(
    suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))),
    add = TRUE
  )

  suppressMessages(suppressWarnings({
    # If data is already spatial, just make sure it is in the right CRS
    if (!is.null(.data) & inherits(.data, "sf")) {
      if (sf::st_crs(.data)$epsg != 4326) {
        geospatial_data <- .data |> sf::st_transform(4326)
      } else {
        geospatial_data <- .data
      }
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      geospatial_data <- .data |>
        # convert dataframe to a spatial object
        TADA_MakeSpatial(crs = 4326) |>
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

      wqp_bboxes <- unique_sites |>
        sf::st_buffer(1e-07) |>
        dplyr::rowwise() |>
        dplyr::mutate(bbox = purrr::map(geometry, sf::st_bbox)) |>
        sf::st_as_sfc()

      # open the nhd_hr - which contains a bunch of layers
      nhd_hr <- arcgislayers::arc_open(nhd_plus_hr_url)

      # list the layers of the nhdhr object

      # select the layer by id from the items list called above (10 is HR catchments)
      nhd_hr_catchments <- arcgislayers::get_layer(nhd_hr, 10)

      # use bboxes of the sites to return their associated catchments
      fill_USGS_catchments_stored <- vector("list", length = length(wqp_bboxes))

      for (i in 1:length(wqp_bboxes)) {
        try(
          fill_USGS_catchments_stored[[i]] <- arcgislayers::arc_select(
            nhd_hr_catchments,
            filter_geom = wqp_bboxes[i],
            crs = sf::st_crs(wqp_bboxes[i])
          ) |>
            sf::st_make_valid(),
          silent = TRUE
        )
      }

      fill_USGS_catchments_stored <- fill_USGS_catchments_stored |>
        purrr::keep(~ !is.null(.)) |>
        dplyr::bind_rows() |>
        dplyr::distinct()

      try(
        fill_USGS_catchments_stored <- fill_USGS_catchments_stored |>
          dplyr::select(nhdplusid, catchmentareasqkm = areasqkm) |>
          dplyr::mutate(
            NHD.nhdplusid = as.character(nhdplusid),
            NHD.resolution = "HR",
            NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)
          ) |>
          dplyr::select(
            NHD.nhdplusid,
            NHD.resolution,
            NHD.catchmentareasqkm,
            geometry
          ),
        silent = TRUE
      )
    }))

    # Empty version of the df will be returned if no associated catchments
    # to avoid breaking downstream fxns reliant on catchment info.
    if (nrow(fill_USGS_catchments_stored) == 0 && "catchments" %in% features) {
      message("No NHD HR features associated with your WQP observations.")
      fill_USGS_catchments_stored <- tibble::tibble(
        NHD.nhdplusid = character(),
        NHD.resolution = character(),
        NHD.catchmentareasqkm = numeric()
      )
    }

    if (nrow(fill_USGS_catchments_stored) == 0 && !"catchments" %in% features) {
      stop("No NHD HR features associated with your WQP observations.")
    }

    if (length(features) == 1 && features == "catchments") {
      return(fill_USGS_catchments_stored)
    }

    # Grab flowlines -
    if ("flowlines" %in% features && nrow(fill_USGS_catchments_stored) > 0) {
      suppressMessages(suppressWarnings({
        # use catchments to grab other NHD features
        geospatial_aoi <- fill_USGS_catchments_stored |> sf::st_as_sfc()

        # select the layer by id from the items list (3 is HR flowlines)
        nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)

        # use catchments to return associated flowlines
        nhd_flowlines_stored <- vector("list", length = length(geospatial_aoi))

        for (i in 1:length(geospatial_aoi)) {
          try(
            nhd_flowlines_stored[[i]] <- arcgislayers::arc_select(
              nhd_hr_flowlines,
              filter_geom = geospatial_aoi[i],
              crs = sf::st_crs(geospatial_aoi[i])
            ) |>
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
            nhd_flowlines_stored[[i]] <- nhd_flowlines_stored[[i]] |>
              dplyr::mutate(dplyr::across(
                dplyr::where(~ !identical(., geometry_col)),
                ~ as.character(.)
              )),
            silent = TRUE
          )
        }

        nhd_flowlines_stored <- nhd_flowlines_stored |>
          purrr::keep(~ !is.null(.)) |>
          purrr::keep(~ !is.character(.)) |>
          dplyr::bind_rows() |>
          dplyr::distinct()
      }))

      if (length(features) == 1 && features == "flowlines") {
        if (
          length(nhd_flowlines_stored) == 0 || is.null(nhd_flowlines_stored)
        ) {
          message(
            "There are no NHD flowlines associated with your WQP observations."
          )
        }

        return(nhd_flowlines_stored)
      }

      if (length(nhd_flowlines_stored) == 0 || is.null(nhd_flowlines_stored)) {
        message(
          "There are no NHD flowlines associated with your WQP observations."
        )
      }
    }

    # Grab waterbodies -
    if ("waterbodies" %in% features & nrow(fill_USGS_catchments_stored) > 0) {
      suppressMessages(suppressWarnings({
        geospatial_aoi <- fill_USGS_catchments_stored |> sf::st_as_sfc()

        # select the layer by id from the items list called above (9 is HR waterbodies)
        nhd_hr_waterbodies <- arcgislayers::get_layer(nhd_hr, 9)

        # use catchments to return associated waterbodies
        nhd_waterbodies_stored <- vector(
          "list",
          length = length(geospatial_aoi)
        )

        for (i in 1:length(geospatial_aoi)) {
          try(
            nhd_waterbodies_stored[[i]] <- arcgislayers::arc_select(
              nhd_hr_waterbodies,
              # where = query,
              filter_geom = geospatial_aoi[i],
              crs = sf::st_crs(geospatial_aoi[i])
            ) |>
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
            nhd_waterbodies_stored[[i]] <- nhd_waterbodies_stored[[i]] |>
              dplyr::mutate(dplyr::across(
                dplyr::where(~ !identical(., geometry_col)),
                ~ as.character(.)
              )),
            silent = TRUE
          )
        }

        nhd_waterbodies_stored <- nhd_waterbodies_stored |>
          purrr::keep(~ !is.null(.)) |>
          purrr::keep(~ !is.character(.)) |>
          dplyr::bind_rows() |>
          dplyr::distinct()
      }))

      if (length(features) == 1 && features == "waterbodies") {
        if (
          length(nhd_waterbodies_stored) == 0 || is.null(nhd_waterbodies_stored)
        ) {
          message(
            "There are no NHD waterbodies associated with your WQP observations."
          )
        }

        return(nhd_waterbodies_stored)
      }

      if (
        length(nhd_waterbodies_stored) == 0 || is.null(nhd_waterbodies_stored)
      ) {
        message(
          "There are no NHD waterbodies associated with your WQP observations."
        )
      }
    }

    # Combinations of features selected, and what they return:

    if (
      length(features) == 2 &&
        "catchments" %in% features &&
        "flowlines" %in% features
    ) {
      nhd_list <- list(
        "fill_USGS_catchments" = fill_USGS_catchments_stored,
        "NHD_flowlines" = nhd_flowlines_stored
      )

      return(nhd_list)
    } else if (
      length(features) == 2 &&
        "catchments" %in% features &&
        "waterbodies" %in% features
    ) {
      nhd_list <- list(
        "fill_USGS_catchments" = fill_USGS_catchments_stored,
        "NHD_waterbodies" = nhd_waterbodies_stored
      )

      return(nhd_list)
    } else if (
      length(features) == 2 &&
        "flowlines" %in% features &&
        "waterbodies" %in% features
    ) {
      nhd_list <- list(
        "NHD_flowlines" = nhd_flowlines_stored,
        "NHD_waterbodies" = nhd_waterbodies_stored
      )

      return(nhd_list)
    } else if (
      length(features) == 3 &&
        "catchments" %in% features &&
        "flowlines" %in% features &&
        "waterbodies" %in% features
    ) {
      nhd_list <- list(
        "fill_USGS_catchments" = fill_USGS_catchments_stored,
        "NHD_flowlines" = nhd_flowlines_stored,
        "NHD_waterbodies" = nhd_waterbodies_stored
      )
    } else {
      stop(
        "Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument."
      )
    }

    # If user wants NHDPlus V2...
  } else if (resolution %in% c("Med", "med")) {
    suppressMessages(suppressWarnings({
      fill_USGS_catchments <- vector("list", length = nrow(unique_sites))

      for (i in 1:nrow(unique_sites)) {
        # Use {nhdplusTools} to grab associated catchments...
        try(
          fill_USGS_catchments[[i]] <- nhdplusTools::get_nhdplus(
            AOI = unique_sites[i, ],
            realization = "catchment"
          ) |>
            sf::st_make_valid() |>
            dplyr::select(comid = featureid, catchmentareasqkm = areasqkm) |>
            dplyr::mutate(
              NHD.comid = as.character(comid),
              NHD.resolution = "nhdplusV2",
              NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)
            ) |>
            dplyr::select(
              NHD.comid,
              NHD.resolution,
              NHD.catchmentareasqkm,
              geometry
            ),
          silent = TRUE
        )
      }

      fill_USGS_catchments <- fill_USGS_catchments |> purrr::keep(~ !is.null(.))

      try(
        fill_USGS_catchments <- dplyr::bind_rows(fill_USGS_catchments) |>
          dplyr::distinct(),
        silent = TRUE
      )

      # if NHD catchments are not in the correct CRS, transform them
      try(
        if (sf::st_crs(fill_USGS_catchments) != sf::st_crs(geospatial_data)) {
          fill_USGS_catchments <- fill_USGS_catchments |>
            sf::st_transform(sf::st_crs(geospatial_data)$epsg)
        },
        silent = TRUE
      )
    }))

    if (nrow(fill_USGS_catchments) == 0 && "catchments" %in% features) {
      message("No NHDPlus V2 features associated with your WQP observations.")
      fill_USGS_catchments <- tibble::tibble(
        NHD.comid = character(),
        NHD.resolution = character(),
        NHD.catchmentareasqkm = numeric()
      )
    }

    if (nrow(fill_USGS_catchments) == 0 && !"catchments" %in% features) {
      stop("No NHDPlus V2 features associated with your WQP observations.")
    }

    if (length(features) == 1 && features == "catchments") {
      return(fill_USGS_catchments)
    }

    # Grab flowlines -
    if ("flowlines" %in% features && nrow(fill_USGS_catchments) > 0) {
      suppressMessages(suppressWarnings({
        nhd_flowlines <- vector("list", length = nrow(fill_USGS_catchments))

        # use catchments to grab other NHD features:
        unique_sites <- fill_USGS_catchments

        for (i in 1:nrow(unique_sites)) {
          # Use {nhdplusTools} to grab associated flowlines...
          try(
            nhd_flowlines[[i]] <- nhdplusTools::get_nhdplus(
              AOI = unique_sites[i, ],
              realization = "flowline"
            ) |>
              sf::st_make_valid(),
            silent = TRUE
          )

          try(
            geometry_col <- sf::st_geometry(nhd_flowlines[[i]]),
            silent = TRUE
          )

          try(
            nhd_flowlines[[i]] <- nhd_flowlines[[i]] |>
              dplyr::mutate(dplyr::across(
                dplyr::where(~ !identical(., geometry_col)),
                ~ as.character(.)
              )),
            silent = TRUE
          )
        }

        nhd_flowlines <- nhd_flowlines |> purrr::keep(~ !is.null(.))

        try(nhd_flowlines <- dplyr::bind_rows(nhd_flowlines)) |>
          dplyr::distinct()

        # if NHD flowlines are not in the correct CRS, transform them
        try(
          if (sf::st_crs(nhd_flowlines) != sf::st_crs(geospatial_data)) {
            nhd_flowlines <- nhd_flowlines |>
              sf::st_transform(sf::st_crs(geospatial_data)$epsg)
          },
          silent = TRUE
        )
      }))

      if (nrow(nhd_flowlines) == 0 && "flowlines" %in% features) {
        message(
          "No NHDPlus V2 flowlines associated with your WQP observations."
        )
      }

      if (length(features) == 1 && features == "flowlines") {
        return(nhd_flowlines)
      }
    }

    # Grab waterbodies -
    if ("waterbodies" %in% features && nrow(fill_USGS_catchments) > 0) {
      suppressMessages(suppressWarnings({
        nhd_waterbodies <- vector("list", length = nrow(fill_USGS_catchments))

        # use catchments to grab other NHD features:
        unique_sites <- fill_USGS_catchments

        for (i in 1:nrow(unique_sites)) {
          # Use {nhdplusTools} to grab associated flowlines...
          try(
            nhd_waterbodies[[i]] <- nhdplusTools::get_waterbodies(
              AOI = unique_sites[i, ]
            ) |>
              sf::st_make_valid(),
            silent = TRUE
          )

          try(
            geometry_col <- sf::st_geometry(nhd_waterbodies[[i]]),
            silent = TRUE
          )

          try(
            nhd_waterbodies[[i]] <- nhd_waterbodies[[i]] |>
              dplyr::mutate(dplyr::across(
                dplyr::where(~ !identical(., geometry_col)),
                ~ as.character(.)
              )),
            silent = TRUE
          )
        }

        nhd_waterbodies <- nhd_waterbodies |> purrr::keep(~ !is.null(.))

        try(
          nhd_waterbodies <- dplyr::bind_rows(nhd_waterbodies) |>
            dplyr::distinct(),
          silent = TRUE
        )

        # if NHD waterbodies are not in the correct CRS, transform them
        try(
          if (sf::st_crs(nhd_waterbodies) != sf::st_crs(geospatial_data)) {
            nhd_waterbodies <- nhd_waterbodies |>
              sf::st_transform(sf::st_crs(geospatial_data)$epsg)
          },
          silent = TRUE
        )
      }))

      if (nrow(nhd_waterbodies) == 0 && "waterbodies" %in% features) {
        message(
          "No NHDPlus V2 waterbodies associated with your WQP observations."
        )
      }

      if (length(features) == 1 && features == "waterbodies") {
        return(nhd_waterbodies)
      }
    }

    # Combinations of features selected, and what they return:

    if (
      length(features) == 2 &&
        "catchments" %in% features &&
        "flowlines" %in% features
    ) {
      nhd_list <- list(
        "fill_USGS_catchments" = fill_USGS_catchments,
        "NHD_flowlines" = nhd_flowlines
      )

      return(nhd_list)
    } else if (
      length(features) == 2 &&
        "catchments" %in% features &&
        "waterbodies" %in% features
    ) {
      nhd_list <- list(
        "fill_USGS_catchments" = fill_USGS_catchments,
        "NHD_waterbodies" = nhd_waterbodies
      )

      return(nhd_list)
    } else if (
      length(features) == 2 &&
        "flowlines" %in% features &&
        "waterbodies" %in% features
    ) {
      nhd_list <- list(
        "NHD_flowlines" = nhd_flowlines,
        "NHD_waterbodies" = nhd_waterbodies
      )

      return(nhd_list)
    } else if (
      length(features) == 3 &&
        "catchments" %in% features &&
        "flowlines" %in% features &&
        "waterbodies" %in% features
    ) {
      nhd_list <- list(
        "fill_USGS_catchments" = fill_USGS_catchments,
        "NHD_flowlines" = nhd_flowlines,
        "NHD_waterbodies" = nhd_waterbodies
      )
    } else {
      stop(
        "Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument."
      )
    }
  } else {
    stop(
      'User-supplied resolution unavailable. Please select between "Med" or "Hi".'
    )
  }
}

#' TADA_CreateATTAINSAUMLCrosswalk
#'
#' Link catchment-based ATTAINS assessment unit data
#' (EPA snapshot of NHDPlus HR catchments associated with entity submitted
#' assessment unit features - points, lines, and polygons) to Water Quality
#' Portal observations, often imported via `TADA_DataRetrieval()`. This
#' function returns the objects that can be mapped in `TADA_ViewATTAINS()`.
#' Check out the TADAModule2.Rmd for an example workflow. Note that
#' approximately 80% of state submitted assessment units in ATTAINS were
#' developed based on high res NHDPlus, so we are using that as the default.
#' The ATTAINS snapshot of NHDPlus HR catchments is not available for areas
#' that do not have existing Assessment Units in ATTAINS.
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
#' for matching waterbody names with ATTAINS waterbody names instead of relying
#' solely on the geospatial location (lat/long).
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()` or the sf
#' equivalent made by `TADA_MakeSpatial()`.
#' @param org_id ATTAINS organization identifier(s) as a character string.
#' If populated, Monitoring Locations will only be matched to Assessment Units from the
#' specified organization(s). A list of organization
#' identifiers can be found
#' by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param. When
#' org_id = "all", the MonitoringLocationIdentifier/AssessmentUnitIdentifier matches
#' from all organizations will be considered. When org_id = "none" or NULL, no
#' crosswalk data from ATTAINS will be considered. The default is "all".
#' @param return_nearest If a WQP observation falls within more than one AU,
#' return ONLY the nearest AU (return_nearest = TRUE), or all AUs
#' (return_nearest = FALSE).
#' @param return_sf Whether to return the ATTAINS associated catchments, lines,
#' points, and polygon shapefile objects along with the data frame(s).
#' TRUE (yes, return list) or FALSE (no, do not return). All shapefile features
#' are in WGS84 (crs = 4326). Defaults to TRUE.
#'
#' @return A modified `TADA_DataRetrieval()` dataframe or list with additional
#' columns associated with the ATTAINS assessment unit data.
#' Moreover, if return_sf = TRUE, this function will additionally return the
#' raw ATTAINS and catchment shapefile features associated with those
#' observations.
#'
#' This function calculates and reports the distance, 'TADA.DistanceAway.Meters',
#' between each WQP observation and intersecting ATTAINS features within its
#' catchment. A TADA.DistanceAway.Meters value of 0 indicates that the WQP
#' observation is directly on the associated ATTAINS point or line feature,
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
#' tada_attains <- TADA_CreateATTAINSAUMLCrosswalk(tada_data,
#'   return_sf = FALSE,
#'   return_nearest = FALSE
#' )
#'
#' tada_attains_sf <- TADA_CreateATTAINSAUMLCrosswalk(tada_data,
#'   return_sf = TRUE,
#'   return_nearest = TRUE
#' )
#' }
TADA_CreateATTAINSAUMLCrosswalk <- function(
  .data,
  org_id = "all",
  return_nearest = TRUE,
  return_sf = TRUE
) {
  # Store original settings for s2 geometry and timeout for restoration after execution
  original_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(
    suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))),
    add = TRUE
  )

  # Retrieve ATTAINS column names for validation
  attains_names <- renameATTAINSCols(return_list = TRUE)

  # Check if ATTAINS data is already present in `.data`
  if (any(attains_names %in% colnames(.data))) {
    stop("Your data has already been joined with ATTAINS data.")
  }

  # Handle empty input data scenario
  if (nrow(.data) == 0) {
    message(
      "Your Water Quality Portal dataframe has no observations. Returning an empty dataframe with empty ATTAINS features."
    )
    col_val_list <- stats::setNames(
      object = rep(x = list(NA), times = length(attains_names)),
      nm = attains_names
    )
    no_WQP_data <- .data |>
      dplyr::mutate(ResultIdentifier = NA) |>
      dplyr::bind_cols(col_val_list) |>
      dplyr::select(ResultIdentifier, dplyr::everything())

    if (return_sf == TRUE) {
      return(list(
        "TADA_with_ATTAINS" = no_WQP_data,
        "ATTAINS_catchments" = NULL,
        "ATTAINS_points" = NULL,
        "ATTAINS_lines" = NULL,
        "ATTAINS_polygons" = NULL
      ))
    } else {
      return(no_WQP_data)
    }
  }

  # Ensure ResultIdentifier is the first column for tracking
  .data <- .data |> dplyr::select(ResultIdentifier, dplyr::everything())

  # Convert data to spatial format if not already
  suppressMessages(suppressWarnings({
    if (!is.null(.data) && inherits(.data, "sf")) {
      # Check CRS and transform if necessary
      if (all(!is.na(.data$epsg) & .data$epsg != 4326)) {
        TADA_DataRetrieval_data <- .data |> sf::st_transform(4326)
      } else {
        TADA_DataRetrieval_data <- .data
      }
    } else {
      # Convert to spatial object using TADA_MakeSpatial
      TADA_DataRetrieval_data <- TADA_MakeSpatial(.data = .data, crs = 4326) |>
        sf::st_make_valid()
    }
  }))

  # Fetch ATTAINS features intersecting with WQP data
  attains_features <- try(
    fetchATTAINS(.data = TADA_DataRetrieval_data, org_id = org_id),
    silent = TRUE
  )

  # Process intersecting catchment objects
  suppressMessages(suppressWarnings({
    nearby_catchments <- NULL
    try(
      {
        nearby_catchments <- attains_features[["ATTAINS_catchments"]] |>
          dplyr::select(-c(OBJECTID, GLOBALID)) |>
          (\(x) x[TADA_DataRetrieval_data, ])() |>
          dplyr::distinct(.keep_all = TRUE)
      },
      silent = TRUE
    )

    if (is.null(nearby_catchments) || nrow(nearby_catchments) == 0) {
      nearby_catchments <- NULL
    }
  }))

  # Handle scenario where no ATTAINS data is associated with WQP observations
  if (is.null(nearby_catchments)) {
    col_val_list <- stats::setNames(
      object = rep(x = list(NA), times = length(attains_names)),
      nm = attains_names
    )
    no_ATTAINS_data <- .data |> dplyr::bind_cols(col_val_list)

    message(
      "There are no ATTAINS catchments associated with these WQP observations."
    )

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
  }

  # If ATTAINS data are present, link WQP features to ATTAINS catchments
  if (!is.null(nearby_catchments)) {
    suppressMessages({
      suppressWarnings({
        TADA_with_ATTAINS <- sf::st_join(
          TADA_DataRetrieval_data,
          nearby_catchments,
          left = TRUE
        )
      })
    })

    # Check for multiple ATTAINS features within the same catchment
    if (
      suppressMessages({
        suppressWarnings({
          TADA_with_ATTAINS |>
            data.table::data.table() |>
            dplyr::group_by(ResultIdentifier) |>
            dplyr::summarize(count = dplyr::n()) |>
            dplyr::filter(count > 1) |>
            nrow() >
            0
        })
      }) &
        return_nearest == FALSE
    ) {
      message(
        "WARNING! Some of your WQP observations fall within a catchment that has more than one ATTAINS feature in it."
      )
      message(
        "For these, duplicate rows have been created, one for each ATTAINS feature. Use `ResultIdentifier` to track these instances."
      )
      message(
        "If you would like to instead only return the nearest ATTAINS feature, use `return_nearest = TRUE."
      )
    }
  }

  # Function to calculate distances between WQP observations and ATTAINS features
  find_distances <- function(location) {
    sub_tada <- TADA_with_ATTAINS |>
      dplyr::filter(as.character(geometry) == location)

    distance <- sub_tada[1, ]

    # Function to calculate distances between WQP observations and ATTAINS features
    find_distances <- function(location) {
      sub_tada <- TADA_with_ATTAINS |>
        dplyr::filter(as.character(geometry) == location)

      distance <- sub_tada[1, ]

      subset <- attains_features[-1] |>
        purrr::map(
          ~ tryCatch(
            dplyr::filter(
              .,
              assessmentunitidentifier %in% sub_tada$assessmentunitidentifier
            ),
            error = function(e) data.frame(),
            warning = function(w) data.frame()
          )
        ) |>
        purrr::keep(~ !is.null(.)) |>
        purrr::keep(~ nrow(.) > 0)

      result <- NULL

      try(
        distances <- subset |>
          purrr::map(
            ~ dplyr::mutate(
              .,
              TADA.DistanceAway.Meters = as.character(sf::st_distance(
                .,
                distance
              ))
            )
          ) |>
          dplyr::bind_rows() |>
          sf::st_drop_geometry() |>
          dplyr::select(assessmentunitidentifier, TADA.DistanceAway.Meters) |>
          dplyr::distinct(),
        silent = TRUE
      )

      try(
        result <- sub_tada |>
          data.table::data.table() |>
          dplyr::select(ResultIdentifier, assessmentunitidentifier) |>
          dplyr::left_join(
            distances,
            by = "assessmentunitidentifier",
            relationship = "many-to-many"
          ) |>
          sf::st_drop_geometry() |>
          dplyr::group_by(ResultIdentifier, assessmentunitidentifier) |>
          dplyr::filter(
            TADA.DistanceAway.Meters == min(TADA.DistanceAway.Meters)
          ) |>
          dplyr::ungroup(),
        silent = TRUE
      )

      return(result)
    }

    # Create a dataframe of all distances
    distances_table <- purrr::map_dfr(
      as.character(unique(TADA_with_ATTAINS$geometry)),
      find_distances
    )

    # Add distance data to TADA dataframe
    TADA_with_ATTAINS <- TADA_with_ATTAINS |>
      data.table::data.table() |>
      dplyr::left_join(
        distances_table,
        by = c("ResultIdentifier", "assessmentunitidentifier")
      ) |>
      dplyr::distinct() |>
      sf::st_as_sf()

    # If return_nearest is TRUE, keep only the nearest ATTAINS feature
    if (return_nearest == TRUE) {
      message("Selecting nearest ATTAINS feature for each WQP observation.")
      message(
        "Use `return_nearest = FALSE` to return all features within WQP catchments."
      )
      TADA_with_ATTAINS <- TADA_with_ATTAINS |>
        dplyr::group_by(ResultIdentifier) |>
        dplyr::slice_min(TADA.DistanceAway.Meters) |>
        dplyr::ungroup()
    }

    if (return_sf == TRUE) {
      # Process catchment features
      ATTAINS_catchments <- nearby_catchments |>
        dplyr::filter(
          assessmentunitidentifier %in%
            TADA_with_ATTAINS$assessmentunitidentifier
        ) |>
        dplyr::distinct(.keep_all = TRUE)

      # Process point features
      ATTAINS_points <- NULL
      try(
        ATTAINS_points <- attains_features[["ATTAINS_points"]] |>
          dplyr::filter(
            assessmentunitidentifier %in%
              TADA_with_ATTAINS$assessmentunitidentifier
          ) |>
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )
      if (is.null(ATTAINS_points) || nrow(ATTAINS_points) == 0) {
        ATTAINS_points <- NULL
      }

      # Process line features
      ATTAINS_lines <- NULL
      try(
        ATTAINS_lines <- attains_features[["ATTAINS_lines"]] |>
          dplyr::filter(
            assessmentunitidentifier %in%
              TADA_with_ATTAINS$assessmentunitidentifier
          ) |>
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )
      if (is.null(ATTAINS_lines) || nrow(ATTAINS_lines) == 0) {
        ATTAINS_lines <- NULL
      }

      # Process polygon features
      ATTAINS_polygons <- NULL
      try(
        ATTAINS_polygons <- attains_features[["ATTAINS_polygons"]] |>
          dplyr::filter(
            assessmentunitidentifier %in%
              TADA_with_ATTAINS$assessmentunitidentifier
          ) |>
          dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE
      )
      if (is.null(ATTAINS_polygons) || nrow(ATTAINS_polygons) == 0) {
        ATTAINS_polygons <- NULL
      }
    }
  }

  # ensure these exist in all code paths
  if (!exists("ATTAINS_catchments", inherits = FALSE)) {
    ATTAINS_catchments <- NULL
  }
  if (!exists("ATTAINS_points", inherits = FALSE)) {
    ATTAINS_points <- NULL
  }
  if (!exists("ATTAINS_lines", inherits = FALSE)) {
    ATTAINS_lines <- NULL
  }
  if (!exists("ATTAINS_polygons", inherits = FALSE)) {
    ATTAINS_polygons <- NULL
  }

  # create final list for output
  final_list <- list(
    "TADA_with_ATTAINS" = TADA_with_ATTAINS |> renameATTAINSCols(),
    "ATTAINS_catchments" = ATTAINS_catchments,
    "ATTAINS_points" = ATTAINS_points,
    "ATTAINS_lines" = ATTAINS_lines,
    "ATTAINS_polygons" = ATTAINS_polygons
  )

  return(final_list)
}

#' TADA_GetATTAINSByAUID
#'
#' Returns ATTAINS data for assessment unit identifiers provided by the user.
#'
#' This function can be used to fetch information for known assessment unit
#' identifier/monitoring location combinations in order to provide data compatible
#' with the results of TADA_CreateATTAINSAUMLCrosswalk for previously unidentified assessment unit
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
#' @param fill_ATTAINS_catch Boolean argument. Specifies whether catchment-based
#' ATTAINS assessment unit data (EPA snapshot of NHDPlus HR catchments associated
#' with entity submitted assessment unit features - points, lines, and polygons)
#' should be queried and downloaded for the assessment units included in the
#' USER-SUPPLIED `au_ref`. When fill_ATTAINS_catch = TRUE, the catchment data
#' are included in the output. When fill_ATTAINS_catch = FALSE, catchment data
#' are not included. Setting fill_ATTAINS_catch = TRUE, may increase the
#' run time of the function significantly. Default is fill_ATTAINS_catch = FALSE.
#'
#' @param api_key Optional character string. An api key for Expert Query web
#' services. If not supplied, the default TADA api key will be used. For best
#' performance, it is recommended that users obtain and use their own api key.
#' Request an api key here: https://owapps.epa.gov/expertquery/api-documentation
#'
#' @return A modified `TADA_DataRetrieval()` dataframe or list with additional
#' columns associated with the ATTAINS assessment unit data and the raw
#' ATTAINS and catchment shapefile features associated with those observations.
#'
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_CreateATTAINSAUMLCrosswalk()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic use with default settings
#' # Assume my_data is a TADA data frame with some monitoring
#' # location results
#' # Assume my_au_ref is a data frame containing known AU and monitoring
#' # location combinations
#' result <- TADA_GetATTAINSByAUID(my_data, au_ref = my_au_ref)
#'
#' # Example 2: Fetching ATTAINS data with catchment information
#' # Set fill_ATTAINS_catch to TRUE to include catchment data in the output
#' result_with_catch <- TADA_GetATTAINSByAUID(my_data,
#'   au_ref = my_au_ref,
#'   fill_ATTAINS_catch = TRUE
#' )
#'
#' # Example 3: Handling empty data frames
#' # If the input data frame has no observations, the function returns an
#' # empty data frame with ATTAINS columns
#' empty_data <- data.frame()
#' empty_result <- TADA_GetATTAINSByAUID(empty_data, au_ref = my_au_ref)
#'
#' # Example 4: Custom AU reference data from an external file
#' # Load AU reference data from a CSV file and use it in the function
#' au_ref_from_file <- read.csv("path/to/au_ref.csv")
#' result_with_file_au_ref <- TADA_GetATTAINSByAUID(my_data,
#'   au_ref = au_ref_from_file
#' )
#' }
#'
TADA_GetATTAINSByAUID <- function(
  .data,
  au_ref,
  fill_ATTAINS_catch = FALSE,
  api_key = NULL
) {
  # get default api_key if user does not supply one
  if (is.null(api_key)) {
    api_key <- .setDefaultEQKey()
  }

  # function settings that we ensure go back to their original settings
  # after the function stops running:
  original_s2 <- sf::sf_use_s2() # Store the original s2 setting first
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(
    suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))),
    add = TRUE
  )

  attains_names <- renameATTAINSCols(return_list = TRUE)

  # should ATTAINS prefixed cols already present stop this function?
  if (any(attains_names %in% colnames(.data))) {
    # remove intermediate object
    rm(attains_names)

    # print message and stop function
    stop("Your data has already been joined with ATTAINS data.")
  }

  if (nrow(.data) == 0) {
    # if no WQP observations, return a modified `data` with empty ATTAINS-related columns:
    message(
      "Your dataframe has no observations. Returning an empty dataframe with empty ATTAINS features."
    )

    # Add ATTAINS columns with NA values
    col_val_list <- stats::setNames(
      object = rep(x = list(NA), times = length(attains_names)),
      nm = attains_names
    )

    no_WQP_data <- .data |>
      dplyr::mutate(ResultIdentifier = NA) |>
      dplyr::bind_cols(col_val_list) |>
      TADA_CorrectColType() |>
      dplyr::select(ResultIdentifier, dplyr::everything())

    # Return empty ATTAINS objects
    return(list(
      "TADA_with_ATTAINS" = no_WQP_data,
      "ATTAINS_catchments" = NULL,
      "ATTAINS_points" = NULL,
      "ATTAINS_lines" = NULL,
      "ATTAINS_polygons" = NULL
    ))
  }

  req.cols <- c(
    "AssessmentUnitIdentifier",
    "MonitoringLocationIdentifier",
    "WaterType"
  )

  # get column names by using internal function checkColName (in Utilities.R)
  col.ids <- purrr::map_dfr(req.cols, ~ checkColName(au_ref, .x))

  # assign values to col.id variables - might be possible to rewrite with purrr function
  assign(col.ids$col.id[1], col.ids$select.col[1])

  assign(col.ids$col.id[2], col.ids$select.col[2])

  assign(col.ids$col.id[3], col.ids$select.col[3])

  # rename au_ref cols for next function
  au_ref <- au_ref |>
    dplyr::rename(
      TADA.MonitoringLocationIdentifier = paste0(ml.col),
      ATTAINS.AssessmentUnitIdentifier = paste0(auid.col),
      Ref.WaterType = paste0(type.col)
    ) |>
    dplyr::select(
      TADA.MonitoringLocationIdentifier,
      ATTAINS.AssessmentUnitIdentifier,
      Ref.WaterType
    )

  # filter detain to retain only results with known AUIDs
  filt.data <- .data |>
    dplyr::filter(
      TADA.MonitoringLocationIdentifier %in%
        au_ref$TADA.MonitoringLocationIdentifier
    ) |>
    dplyr::left_join(
      au_ref,
      by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
    )

  # check to see if any of the rows in the TADA df match MonitorignLocationIdentifiers in the user ref
  if (dim(.data)[1] < 1) {
    stop(paste0(
      "TADA_GetATTAINSByAUID: No records in the TADA data frame are associated with ",
      "MonitoringLocationIdentifiers in the user-supplied ref."
    ))
  }

  # REST for ATTAINS geospatial data:
  baseurls <- c(
    # ATTAINS catchments:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?",
    # ATTAINS points:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?",
    # ATTAINS lines:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?",
    # ATTAINS polygons:
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?"
  )

  # get water type info using ATTAINS Expert Query
  get_wb_type <- function(au_list) {
    au_list <- unique(au_list)

    # split the au_list into chunks
    chunks <- split(au_list, ceiling(seq_along(unique(au_list)) / 20))

    # get water type
    # need to edit funciton to silent print outs from EQ_AUs

    wat_type <- function(chunk) {
      results <- spsUtil::quiet(rExpertQuery::EQ_AssessmentUnits(
        api_key = api_key,
        auid = chunk
      ))
    }

    results <- purrr::map_dfr(.x = chunks, .f = wat_type)

    results <- results |>
      dplyr::select(assessmentUnitId, waterType) |>
      dplyr::distinct()

    return(results)
  }

  # get water types
  water_types <- try(
    get_wb_type(au_ref$ATTAINS.AssessmentUnitIdentifier),
    silent = TRUE
  )

  # function to download ATTAINS features API based on their assessment unit id

  fetch_au <- function(baseurls, assessment_unit_ids, chunk_n = 1000) {
    # Split the assessment_unit_ids into chunks of 1000
    # API cannot handle more than 1000 features
    id_chunks <- split(
      assessment_unit_ids,
      ceiling(seq_along(assessment_unit_ids) / chunk_n)
    )

    # Query API for a chunk of assessment unit IDs
    fetch_chunk <- function(id_chunk) {
      where_clause <- paste0(
        "assessmentunitidentifier IN ('",
        paste(id_chunk, collapse = "','"),
        "')"
      )
      query_params <- list(where = where_clause, outFields = "*", f = "geojson")

      response <- httr::GET(baseurls, query = query_params)

      if (httr::status_code(response) != 200) {
        stop("Failed to retrieve data from EPA ATTAINS API.")
      }

      geojson_data <- httr::content(response, as = "text", encoding = "UTF-8")
      sf_object <- sf::st_read(geojson_data, quiet = TRUE)

      return(sf_object)
    }

    # fetch all chunks and combine results
    au_results <- purrr::map_dfr(id_chunks, fetch_chunk)

    return(au_results)
  }

  # start grabbing the raw ATTAINS features
  points <- NULL
  lines <- NULL
  polygons <- NULL
  catchments <- NULL
  TADA_with_ATTAINS <- .data

  # Download associated point, line, polygon, and catchment features using list of auids
  try(
    points <- fetch_au(
      baseurls = baseurls[2],
      assessment_unit_ids = paste0(unique(
        filt.data$ATTAINS.AssessmentUnitIdentifier
      )),
      chunk_n = 100
    ),
    silent = TRUE
  )

  try(
    lines <- fetch_au(
      baseurls = baseurls[3],
      assessment_unit_ids = paste0(unique(
        filt.data$ATTAINS.AssessmentUnitIdentifier
      )),
      chunk_n = 100
    ),
    silent = TRUE
  )

  try(
    polygons <- fetch_au(
      baseurls = baseurls[4],
      assessment_unit_ids = paste0(unique(
        filt.data$ATTAINS.AssessmentUnitIdentifier
      )),
      chunk_n = 100
    ),
    silent = TRUE
  )

  if (nrow(lines) == 0 & nrow(points) == 0 & nrow(polygons) == 0) {
    final_features <- list(
      "TADA_with_ATTAINS" = TADA_with_ATTAINS,
      "ATTAINS_catchments" = catchments,
      "ATTAINS_points" = points,
      "ATTAINS_lines" = lines,
      "ATTAINS_polygons" = polygons
    )

    return(final_features)
  }

  try(
    points <- points |>
      dplyr::left_join(
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitId")
      ),
    silent = TRUE
  )

  try(
    lines <- lines |>
      dplyr::left_join(
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitId")
      ),
    silent = TRUE
  )

  try(
    polygons <- polygons |>
      dplyr::left_join(
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitId")
      ),
    silent = TRUE
  )

  if (fill_ATTAINS_catch == FALSE) {
    catchments <- NULL
  }

  # create TADA_with_ATTAINS df for list output
  TADA_with_ATTAINS <- filt.data

  # NEED to figure out what should happen here when no geometry is found (HRM note: 1/21/26)

  # create list of tada prefix columns
  tada.cols <- colnames(TADA_with_ATTAINS)

  # create list of attains prefix cols
  attains.cols <- renameATTAINSCols(return_list = TRUE, format = "attains")

  # create a combined list of tada and attains prefix cols
  comb.cols <- append(tada.cols, attains.cols) |> unique()

  attains.geo <- data.frame(matrix(nrow = 1, ncol = length(comb.cols)))

  # change col names of attains.geo to match tada and attains prefix cols
  colnames(attains.geo) <- comb.cols

  # remove unnecessary column from attains.geo
  attains.geo <- attains.geo |>
    dplyr::select(-assessmentunitidentifier) |>
    TADA_CorrectColType()

  # remove intermediate objects
  rm(tada.cols, attains.cols, comb.cols)

  if (fill_ATTAINS_catch == TRUE) {
    try(
      catchments <- fetch_au(
        baseurls = baseurls[1],
        assessment_unit_ids = paste0(unique(
          filt.data$ATTAINS.AssessmentUnitIdentifier
        )),
        chunk_n = 10
      ),
      silent = TRUE
    )

    if (nrow(catchments) > 0) {
      # get one catchment per WQP location
      catchments.cw <- filt.data |>
        dplyr::select(
          TADA.MonitoringLocationIdentifier,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          HorizontalCoordinateReferenceSystemDatumName
        ) |>
        dplyr::distinct() |>
        TADA_MakeSpatial() |>
        sf::st_join(catchments, join = sf::st_nearest_feature) |>
        dplyr::group_by(TADA.MonitoringLocationIdentifier) |>
        dplyr::mutate(catchCount = dplyr::n()) |>
        dplyr::select(TADA.MonitoringLocationIdentifier, nhdplusid) |>
        dplyr::distinct() |>
        sf::st_drop_geometry()

      catchments.filt <- catchments |>
        dplyr::filter(nhdplusid %in% catchments.cw$nhdplusid)

      catchments.no.geo <- catchments |>
        sf::st_drop_geometry() |>
        dplyr::distinct()

      if (!inherits(water_types, "try-error")) {
        catchments <- catchments.filt |>
          dplyr::left_join(
            water_types,
            by = c("assessmentunitidentifier" = "assessmentUnitId")
          )
      } else {
        catchments <- catchments.filt
        warning(
          "Problem with ExpertQuery, returning catchments without all fields"
        )
      }
    }
  }

  # internal function to combine attains.geo data
  combineATTAINSGeo <- function(.data, geo.data, attains.geo) {
    # rename AU column in geo.data
    geo.data <- geo.data |>
      dplyr::rename(
        ATTAINS.AssessmentUnitIdentifier = assessmentunitidentifier
      ) |>
      TADA_CorrectColType()

    # join data from ATTAINS with TADA df
    df <- .data |>
      dplyr::left_join(geo.data, by = c("ATTAINS.AssessmentUnitIdentifier")) |>
      TADA_CorrectColType()

    # Bind with existing attains.geo data
    attains.geo <- plyr::rbind.fill(attains.geo, df)
    # Check if GLOBALID exists in the combined data frame
    if ("GLOBALID" %in% names(attains.geo)) {
      # Filter out rows with NA in GLOBALID
      attains.geo <- attains.geo |> dplyr::filter(!is.na(GLOBALID))
    }

    # remove intermediate object
    rm(df)

    # return tada df with added attains data
    return(attains.geo)
  }

  # add attains data returned from lines query if any exists
  if (dim(lines)[1] > 0) {
    attains.geo <- combineATTAINSGeo(
      .data = TADA_with_ATTAINS,
      geo.data = lines,
      attains.geo = attains.geo
    )
  }

  # add attains data returned from points query if any exists
  if (dim(points)[1] > 0) {
    attains.geo <- combineATTAINSGeo(
      .data = TADA_with_ATTAINS,
      geo.data = points,
      attains.geo = attains.geo
    )
  }

  # add attains data returned from polygons query if any exists
  if (dim(polygons)[1] > 0) {
    attains.geo <- combineATTAINSGeo(
      .data = TADA_with_ATTAINS,
      geo.data = polygons,
      attains.geo = attains.geo
    )
  }

  # remame cols and set up TADA_with_ATTAINS df
  TADA_with_ATTAINS <- attains.geo |>
    TADA_CorrectColType() |>
    dplyr::filter(!is.na(ResultIdentifier)) |>
    dplyr::full_join(.data, by = names(.data)) |>
    renameATTAINSCols() |>
    dplyr::mutate(
      ATTAINS.WaterType = ifelse(
        is.na(ATTAINS.WaterType),
        Ref.WaterType,
        ATTAINS.WaterType
      )
    ) |>
    dplyr::group_by(ResultIdentifier)

  # Check to see if any mismatches
  mismatch_check <- TADA_with_ATTAINS |>
    dplyr::select(
      ATTAINS.AssessmentUnitIdentifier,
      ATTAINS.WaterType,
      Ref.WaterType
    ) |>
    dplyr::mutate(
      ATTAINS.WaterType = ifelse(
        is.na(ATTAINS.WaterType),
        "NA",
        ATTAINS.WaterType
      ),
      Ref.WaterType = ifelse(is.na(Ref.WaterType), "NA", Ref.WaterType),
      Mismatch = ATTAINS.WaterType != Ref.WaterType
    ) |>
    dplyr::filter(Mismatch == TRUE)

  # Remove mismatch test if none exist
  if (nrow(mismatch_check) == 0) {
    rm(mismatch_check)
  }

  # Print message if mismatches exist
  if (exists("mismatch_check") && nrow(mismatch_check) > 0) {
    mismatch.text <- mismatch_check |>
      dplyr::mutate(
        MatchMessage = paste0(
          ATTAINS.AssessmentUnitIdentifier,
          " (ATTAINS: ",
          ATTAINS.WaterType,
          ", User-ref: ",
          Ref.WaterType,
          ")"
        )
      ) |>
      dplyr::select(MatchMessage) |>
      dplyr::pull()

    mismatch.text <- stringi::stri_replace_last(
      paste(mismatch.text, collapse = "; "),
      fixed = "; ",
      " and "
    )

    print(paste0(
      "TADA_GetATTAINSByAUID: There are mismatches between the ATTAINS and user-supplied ",
      "ref water type for one or more assessment units. ",
      "The ATTAINS water type has been retained for all mismatched records. ",
      "If you would like to update the water type, that must be done through the ATTAINS ",
      "user interface or batch upload. ",
      "Mismatch details: ",
      mismatch.text,
      "."
    ))

    rm(mismatch.text, mismatch_check)
  }

  # remove Ref.WaterType col as it is no longer needed
  TADA_with_ATTAINS <- TADA_with_ATTAINS |>
    dplyr::select(-Ref.WaterType) |>
    dplyr::distinct()

  # remove intermediate object
  rm(attains.geo, filt.data)

  final_features <- list(
    "TADA_with_ATTAINS" = TADA_with_ATTAINS,
    "ATTAINS_catchments" = catchments,
    "ATTAINS_points" = points,
    "ATTAINS_lines" = lines,
    "ATTAINS_polygons" = polygons
  )

  return(final_features)
}

#' Identify and group nearby monitoring locations (UNDER ACTIVE DEVELOPMENT)
#'
#' This function takes a TADA dataset and identifies the NHD catchments that
#' each MonitoringLocation is in. Within each group of MonitoringLocations in
#' the same catchment, a distance matrix is created and an adjacency matrix
#' is used to identify groups of nearby sites within the same catchment.
#' Groups of nearby sites are given a new TADA.MonitoringLocationIdentifier
#' which is created by concatenating the original
#' TADA.MonitoringLocationIdentifiers of all sites within the group. If the
#' ATTAINS.AssessmentUnitIdentifier column in present, the default is
#' only monitoring locations within the same assessment unit will be grouped together.
#' It is recommended to assign monitoring locations to assessment units before running
#' this function. If ATTAINS.AssessmentUnitIdentifier is present and the user does not
#' want it to be factored into to nearby site groupings, the by_AU param can be set to
#' FALSE. Two additional columns, TADA.NearbySiteGroup and TADA.NearbySites.Flag are added.
#' TADA.NearbySiteGroup contains a unique numeric value for each group of sites
#' within the same catchment. TADA.NearbySites.Flag identifies whether or not
#' a result is from a grouped site or not and for grouped sites identifies how
#' the TADA prefixed metadata columns (TADA.MonitoringLocationName,
#' TADA.MonitoringLocationTypeName, TADA.LongitudeMeasure, and
#' TADA.LatitudeMeasure) were determined.
#'
#' @param .data TADA dataframe OR TADA sites dataframe.
#'
#' @param dist_buffer Numeric. The maximum distance (in meters) two sites can be
#' from one another to be considered "nearby" and grouped together.
#'
#' @param catchment Boolean. When catchment = TRUE, two sites will only be matched
#' if they are within the same NHD catchment. When catchment = FALSE catchment
#' is not considered when matching sites. Default is catchment = TRUE.
#'
#' @param by_AU Boolean. When by_AU = TRUE, two sites will only be matched
#' if they are within the same ATTAINS assessment unit. When by_AU = FALSE the
#' assessment unit is not considered when matching nearby sites. In order to
#' consider assessment unit when matching, the TADA data frame must contain the
#' column ATTAINS.AssessmentUnitIdentifier. Default is by_AU = TRUE.
#'
#' @param nhd_res Character argument to determine whether the NHD catchments
#' used should be high ("Hi") or medium ("Med") res. Default = "Hi" for
#' consistency with other TADA geospatial functions.
#'
#' @param org_hierarchy Vector of organization identifiers that acts as the
#' order in which the function should select representative metadata for
#' grouped sites based on the organization that collected the data. If left
#' blank, the function does not factor organization in to the metadata
#' selection process. When a vector is provided, the metadata will first be
#' selected by organization and the "meta_select" argument will only be
#' applied in cases where more than one set of metadata per site grouping are
#' available from the highest ranking organization available.
#'
#' @param meta_select Character argument to determine how metadata should be
#' selected if no org_hierarchy is specified or if multiple options for metadata
#' from the same organization exist. Options are "oldest", which selects the
#' metadata associated with the oldest result from the grouped nearby sites,
#' "newest", which selects the metadata associated with the newest result from
#' the grouped nearby sites, "count" which selects the metadata associated with
#' the greatest number of results, and "random" which selects random metadata
#' from the site group. The default is meta_select = "random".
#'
#' @return Input dataframe with a TADA.SiteGroup column that indicates the
#' nearby site group each monitoring location belongs to. Grouped sites are
#' concatenated in the TADA.MonitoringLocationIdentifier column
#' (e.g. "USGS-10010025","USGS-10010026" enclosed in square brackets []).
#' This JSON array is the new TADA monitoring location ID for the grouped sites.
#' TADA.MonitoringLocationIdentifier can be leveraged to analyze data from
#' nearby sites together (as the same general location). Related metadata,
#' including TADA.MonitoringLocationName, TADA.LatitudeMeasure,
#' TADA.LongitudeMeasure, and TADA.MonitoringLocationTypeName are added to the
#' input df. Meta data selection is determined by user inputs as users may
#' provide an organization hierarchy to determine which organization's
#' metadata should be preferentially selected and further specify whether
#' metadata should be selected: randomly, by the oldest or newest sampling date,
#' or by the site with the greatest number of overall results in the TADA df.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # use MT example data set
#' testdat <- Data_MT_AUMLRef$TADA_with_ATTAINS
#'
#' # example grouping nearby sites by distance only
#' test.dist <- TADA_FindNearbySites(testdat,
#'   catchment = FALSE,
#'   by_AU = FALSE,
#'   dist_buffer = 250
#' )
#'
#' # example grouping nearby sites by distance and catchment
#' test.catch <- TADA_FindNearbySites(testdat,
#'   catchment = TRUE,
#'   by_AU = FALSE,
#'   dist_buffer = 250
#' )
#'
#' # example grouping nearby sites by distance and assessment unit
#' test.au.only <- TADA_FindNearbySites(testdat,
#'   catchment = FALSE,
#'   by_AU = TRUE,
#'   dist_buffer = 250
#' )
#'
#' # example grouping nearby sites by distance, catchment, and assessment unit
#' test.all <- TADA_FindNearbySites(testdat,
#'   catchment = TRUE,
#'   by_AU = TRUE,
#'   dist_buffer = 250
#' )
#' }
TADA_FindNearbySites <- function(
  .data,
  dist_buffer = 100,
  nhd_res = "Hi",
  org_hierarchy = "none",
  meta_select = "random",
  catchment = TRUE,
  by_AU = TRUE
) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.MonitoringLocationIdentifier",
    "TADA.LongitudeMeasure",
    "TADA.LatitudeMeasure"
  )

  TADA_CheckColumns(.data, expected_cols)

  # remove intermediate object
  rm(expected_cols)

  # retain only necessary columns unique Monitoring Locations
  unique.mls <- .data |>
    dplyr::select(
      "TADA.MonitoringLocationIdentifier",
      "TADA.MonitoringLocationName",
      "TADA.LongitudeMeasure",
      "TADA.LatitudeMeasure",
      "HorizontalCoordinateReferenceSystemDatumName"
    ) |>
    dplyr::distinct()

  # convert to sf object if not already spatial
  if (!inherits(.data, "sf")) {
    unique.mls <- try(TADA_MakeSpatial(unique.mls), silent = TRUE)
  }

  # create a distance matrix in meters
  dist.matrix <- as.matrix(sf::st_distance(unique.mls)) # Great Circle distance since in lat/lon

  # remove units from distance matrix
  dist.matrix <- dist.matrix |> units::drop_units()

  rownames(dist.matrix) <- unique.mls$TADA.MonitoringLocationIdentifier
  colnames(dist.matrix) <- unique.mls$TADA.MonitoringLocationIdentifier

  # convert distances to those within buffer (1) and beyond buffer (0)
  dist.matrix <- apply(dist.matrix, c(1, 2), function(x) {
    if (x <= dist_buffer) {
      x <- 1
    } else {
      x <- 0
    }
  })

  # create adjacency graph
  adj.graph <- igraph::graph_from_adjacency_matrix(
    dist.matrix,
    mode = "undirected",
    diag = FALSE
  )

  # find connected sites
  comp.results <- igraph::components(adj.graph)

  # create site group dfs
  group.sites <- data.frame(
    TADA.MonitoringLocationIdentifier = names(comp.results$membership),
    Group = comp.results$membership,
    row.names = NULL
  ) |>
    dplyr::group_by(Group) |>
    dplyr::mutate(n = length(TADA.MonitoringLocationIdentifier)) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n) |>
    dplyr::ungroup()

  # remove intermediate objects
  rm(dist.matrix, adj.graph, comp.results)

  # add flag column, stop function, and print message if no nearby sites found
  if (nrow(group.sites) == 0) {
    # #if no groups, give a TADA.NearbySiteGroup column filled with
    # "No nearby sites"
    print(
      "TADA_FindNearbySites: No nearby sites detected. Columns for TADA.NearbySitesFlag and TADA.NearbySiteGroup added for tracking purposes."
    )

    .data <- .data |>
      dplyr::mutate(
        TADA.NearbySites.Flag = "No nearby sites detected.",
        TADA.NearbySiteGroup = NA
      )

    return(.data)
  }

  # if catchment should be factored into site groupings
  if (catchment == TRUE) {
    # subset nearby sites
    near.sites <- unique.mls |>
      dplyr::filter(
        TADA.MonitoringLocationIdentifier %in%
          group.sites$TADA.MonitoringLocationIdentifier
      ) |>
      dplyr::left_join(
        group.sites,
        by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
      )

    # break into multiple dfs
    near.dfs <- near.sites |> dplyr::group_split(Group, .keep = FALSE)

    # fetch nhdplus catchment information
    nhd.catch <- near.dfs |> purrr::map(~ .x |> fetchNHD(resolution = nhd_res))

    # remove intermediate object
    rm(near.dfs)

    # remove any fetchNHD dfs that do not contain any data (to prevent bind rows error)
    nhd.catch.filt <- purrr::keep(nhd.catch, ~ nrow(.) > 0)

    # create one df from all fetchNHD data
    nhd.catch.all <- dplyr::bind_rows(nhd.catch.filt)

    # join nhd catchments with monitoring locations, filter to include group/catchment
    group.sites <- near.sites |>
      sf::st_join(nhd.catch.all, left = TRUE) |>
      dplyr::distinct() |>
      dplyr::group_by(Group, NHD.nhdplusid) |>
      dplyr::mutate(n = length(TADA.MonitoringLocationIdentifier)) |>
      dplyr::filter(n > 1) |>
      dplyr::select(TADA.MonitoringLocationIdentifier, Group) |>
      sf::st_drop_geometry()

    # remove intermediate objects
    rm(near.sites, nhd.catch, nhd.catch.filt, nhd.catch.all)

    if (nrow(group.sites) == 0) {
      # #if no groups, give a TADA.NearbySiteGroup column filled with
      # "No nearby sites"
      print(
        "TADA_FindNearbySites: No nearby sites detected. Columns for TADA.NearbySitesFlag and TADA.NearbySiteGroup added for tracking purposes."
      )

      .data <- .data |>
        dplyr::mutate(
          TADA.NearbySites.Flag = "No nearby sites detected.",
          TADA.NearbySiteGroup = NA
        )

      return(.data)
    }
  }
  # check if .data contains the column "ATTAINS.AssessmentUnitIdentifier"
  # and status of by_AU param
  if ("ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {
    if (by_AU == TRUE) {
      print(
        "TADA_FindNearbySites: ATTAINS.AssessmentUnitIdentifier is present. Monitoring Locations will only be grouped if they fall within the same assessment unit."
      )

      # create crosswalk for monitoring locations and assessment units
      au.ml.cw <- .data |>
        dplyr::select(
          TADA.MonitoringLocationIdentifier,
          ATTAINS.AssessmentUnitIdentifier
        ) |>
        dplyr::distinct()

      # group by ATTAINS.AssessmentUnitIdentifier (and catchment)
      group.sites <- group.sites |>
        sf::st_drop_geometry() |>
        dplyr::left_join(
          au.ml.cw,
          by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
        ) |>
        dplyr::group_by(ATTAINS.AssessmentUnitIdentifier) |>
        dplyr::filter(
          !is.na(ATTAINS.AssessmentUnitIdentifier),
          ATTAINS.AssessmentUnitIdentifier != ""
        ) |>
        dplyr::mutate(Group.n = dplyr::n()) |>
        dplyr::filter(Group.n > 1) |>
        dplyr::select(-Group.n)
    } else {
      print(
        "TADA_FindNearbySites: ATTAINS.AssessmentUnitIdentifier is present. User has specified that assessment unit should not be considered when grouping nearby sites."
      )
    }
  }

  # create df of all groups and create unique id for each group
  new.ids <- group.sites |>
    # remove any previous grouping
    dplyr::ungroup() |>
    # add new grouping
    dplyr::group_by(Group) |>
    # create new TADA.MonitoringLocationIdentifier
    dplyr::mutate(
      TADA.MonitoringLocationIdentifier.New = paste(
        TADA.MonitoringLocationIdentifier,
        collapse = ", "
      ),
      TADA.MonitoringLocationIdentifier.New = paste0(
        "[",
        TADA.MonitoringLocationIdentifier.New,
        "]"
      ),
      TADA.NearbySiteGroup = dplyr::cur_group_id()
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      TADA.MonitoringLocationIdentifier.New,
      TADA.MonitoringLocationIdentifier,
      TADA.NearbySiteGroup
    ) |>
    dplyr::distinct()

  # remove intermediate object
  rm(unique.mls)

  # create a df of unique grouped sites
  group.sites <- new.ids |>
    dplyr::left_join(
      .data,
      by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
    ) |>
    dplyr::filter(!is.na(TADA.MonitoringLocationIdentifier.New)) |>
    dplyr::select(
      TADA.MonitoringLocationName,
      TADA.MonitoringLocationIdentifier.New,
      TADA.NearbySiteGroup,
      TADA.MonitoringLocationName,
      TADA.LatitudeMeasure,
      TADA.LongitudeMeasure,
      TADA.MonitoringLocationTypeName,
      OrganizationIdentifier
    ) |>
    dplyr::distinct() |>
    sf::st_drop_geometry()

  # create list of orgs from TADA df
  all.orgs <- unique(.data$OrganizationIdentifier)

  # compare list of orgs from TADA df to user supplied org_hierachy to find missing orgs
  missing.orgs <- setdiff(all.orgs, org_hierarchy)

  # create string for flagging based on meta_select
  if (meta_select == "random") {
    meta.string <- "random selection"
  }

  if (meta_select == "oldest") {
    meta.string <- "oldest sampling date"
  }

  if (meta_select == "newest") {
    meta.string <- "most reccent sampling date"
  }

  if (meta_select == "count") {
    meta.string <- "greatest number of results in TADA data frame"
  }

  # use org hierarchy for first round of metadata selection
  if (isTRUE(org_hierarchy == "none")) {
    # create string for flagging
    org.string <- "Metadata were selected by "

    # print message
    print(
      "TADA_FindNearbySites: No org_hierarchy supplied by user. Organization will not be taken into account during metadata selection."
    )

    # create consistent org rank to facilitate meta data selection (all orgs ranked equally)
    org.ranks <- as.data.frame(all.orgs) |>
      dplyr::mutate(OrgRank = 99) |>
      dplyr::rename(OrganizationIdentifier = all.orgs)
  }

  # if org hierarchy is supplied by user
  if (org_hierarchy[1] != "none") {
    # create string for flagging
    org.string <- "Metadata were selected by filtering based on the user supplied hierarchy, then by "

    if (!is.vector(org_hierarchy)) {
      stop(
        "TADA_FindNearbySites: Organization hierarchy must be supplied as a vector."
      )
    }

    if (length(org_hierarchy) == 0) {
      stop("TADA_FindNearbySites: No organization identifiers were supplied.")
    }

    if (length(missing.orgs) > 0) {
      print(paste0(
        "TADA_FindNearbySites: ",
        length(missing.orgs),
        " organization identifiers are missing from org_hierarchy (",
        stringi::stri_replace_last(
          paste(missing.orgs, collapse = ", "),
          fixed = ", ",
          " and "
        ),
        ").",
        " Function will continue to run using partial org_hierarchy."
      ))

      # create df for organization ranks from user-supplied hierarchy
      org.ranks <- as.data.frame(org_hierarchy) |>
        dplyr::mutate(OrgRank = dplyr::row_number()) |>
        dplyr::rename(OrganizationIdentifier = org_hierarchy)

      # create df for all organizations missing from user-supplied hierarchy
      # all missing orgs will share the same rank and be ranked below any orgs supplied by user
      missing.ranks <- as.data.frame(missing.orgs) |>
        dplyr::mutate(OrgRank = (length(org_hierarchy) + 1)) |>
        dplyr::rename(OrganizationIdentifier = missing.orgs)

      # add missing orgs to org rank df
      org.ranks <- org.ranks |> dplyr::bind_rows(missing.ranks)
    }

    if (length(missing.orgs) == 0) {
      # create df for organization ranks from user-supplied hierarchy
      org.ranks <- as.data.frame(org_hierarchy) |>
        dplyr::mutate(OrgRank = dplyr::row_number()) |>
        dplyr::rename(OrganizationIdentifier = org_hierarchy)
    }

    rm(all.orgs, missing.orgs)
  }

  # add org ranks to df of all TADA.MonitoringLocationIdentifier.New
  org.ranks.added <- group.sites |>
    dplyr::left_join(org.ranks, by = dplyr::join_by(OrganizationIdentifier))

  rm(org.ranks)

  # filter to retain metadata for TADA.MonitoringLocation.New where there is only one set of
  # metadata from the highest ranked org
  org.meta.filter <- org.ranks.added |>
    dplyr::group_by(TADA.MonitoringLocationIdentifier.New) |>
    dplyr::mutate(CountSites = length(TADA.MonitoringLocationName)) |>
    dplyr::filter(CountSites > 1) |>
    dplyr::slice_min(OrgRank) |>
    dplyr::ungroup() |>
    dplyr::select(-OrgRank, -CountSites) |>
    dplyr::mutate(
      TADA.NearbySites.Flag = paste0(
        "This monitoring location was grouped with other nearby site(s). ",
        org.string,
        meta.string,
        "."
      )
    )

  # select and assign metadata randomly for grouped sites when meta_select equals "random"

  if (meta_select == "random") {
    # select random metadata where necessary (no org rank or more than one set of metdata for one
    # TADA.MonitoringLocationIdentifier.New)
    random.meta <- org.meta.filter |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(TADA.MonitoringLocationIdentifier.New)) |>
      dplyr::group_by(TADA.NearbySiteGroup) |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier.New,
        TADA.MonitoringLocationName,
        TADA.LatitudeMeasure,
        TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName,
        TADA.NearbySiteGroup
      ) |>
      dplyr::distinct() |>
      dplyr::slice_sample(n = 1) |>
      dplyr::ungroup() |>
      dplyr::rename(
        TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
        TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName,
        TADA.NearbySiteGroup.New = TADA.NearbySiteGroup,
        TADA.MonitoringLocationName.New = TADA.MonitoringLocationName
      )

    # join the metadata filtering results to create a df with all metadata to apply to TADA df by
    # TADA.MonitoringLocationIdentifier.New
    select.meta <- random.meta |>
      dplyr::full_join(
        org.meta.filter,
        dplyr::join_by(TADA.MonitoringLocationIdentifier.New)
      ) |>
      dplyr::mutate(
        TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected randomly."
      ) |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier.New,
        TADA.LatitudeMeasure.New,
        TADA.LongitudeMeasure.New,
        TADA.MonitoringLocationTypeName.New,
        TADA.MonitoringLocationName.New,
        TADA.NearbySiteGroup.New,
        TADA.NearbySites.Flag
      ) |>
      dplyr::distinct()

    # remove intermediate objects
    rm(random.meta, org.ranks.added)
  }

  if (meta_select == "oldest" | meta_select == "newest") {
    # prep site groups for metadata selection by date
    date.meta <- group.sites |>
      dplyr::left_join(
        org.ranks.added,
        by = dplyr::join_by(
          TADA.MonitoringLocationIdentifier.New,
          TADA.NearbySiteGroup,
          TADA.MonitoringLocationName,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          TADA.MonitoringLocationTypeName,
          OrganizationIdentifier
        )
      ) |>
      dplyr::mutate(OrgRank = ifelse(is.na(OrgRank), rank.default, OrgRank)) |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier.New) |>
      dplyr::slice_min(OrgRank) |>
      dplyr::left_join(
        .data,
        dplyr::join_by(
          TADA.MonitoringLocationName,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          TADA.MonitoringLocationTypeName,
          OrganizationIdentifier
        )
      ) |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier.New,
        TADA.NearbySiteGroup,
        TADA.MonitoringLocationName,
        TADA.LatitudeMeasure,
        TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName,
        OrganizationIdentifier,
        ActivityStartDate
      ) |>
      dplyr::distinct()

    if (meta_select == "oldest") {
      # select oldest metadata for group
      date.meta <- date.meta |> dplyr::slice_min(ActivityStartDate)

      # specify oldest for flagging string
      date.choice <- "oldest"
    }

    if (meta_select == "newest") {
      # select newest metadata for group
      date.meta <- date.meta |> dplyr::slice_max(ActivityStartDate)

      # specify newest for flagging string
      date.choice <- "newest"
    }

    # select metadata by date
    select.meta <- date.meta |>
      dplyr::full_join(
        org.meta.filter,
        by = dplyr::join_by(
          TADA.MonitoringLocationIdentifier.New,
          TADA.MonitoringLocationName,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          TADA.MonitoringLocationTypeName,
          OrganizationIdentifier,
          TADA.NearbySiteGroup
        )
      ) |>
      dplyr::select(-OrganizationIdentifier, -ActivityStartDate) |>
      dplyr::rename(
        TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
        TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
        TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName,
        TADA.NearbySiteGroup.New = TADA.NearbySiteGroup
      ) |>
      dplyr::group_by(TADA.NearbySiteGroup.New) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::mutate(
        TADA.NearbySites.Flag = paste0(
          "This monitoring location was grouped with other",
          " nearby site(s). Metadata were selected from ",
          "the ",
          date.choice,
          " result available."
        )
      )

    rm(date.meta)
  }

  if (meta_select == "count") {
    # select metadata by finding site with greatest number of results in TADA df
    select.meta <- org.ranks.added |>
      dplyr::left_join(
        .data,
        by = dplyr::join_by(
          TADA.MonitoringLocationName,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          TADA.MonitoringLocationTypeName
        )
      ) |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier) |>
      dplyr::mutate(NCount = length(TADA.ResultMeasureValue)) |>
      dplyr::ungroup() |>
      dplyr::select(-TADA.MonitoringLocationIdentifier) |>
      dplyr::distinct() |>
      dplyr::group_by(TADA.NearbySiteGroup) |>
      dplyr::slice_max(NCount) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier.New,
        TADA.NearbySiteGroup,
        TADA.MonitoringLocationName,
        TADA.LatitudeMeasure,
        TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName
      ) |>
      dplyr::rename(
        TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
        TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
        TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName,
        TADA.NearbySiteGroup.New = TADA.NearbySiteGroup
      ) |>
      dplyr::mutate(
        TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected from MonitoringLocation with the most results available across all characteristics."
      )
  }

  # remove intermediate objects
  rm(org.meta.filter, org.string, meta.string)

  # remove site group from crosswalk
  ml.crosswalk <- new.ids |>
    sf::st_drop_geometry() |>
    dplyr::select(-TADA.NearbySiteGroup) |>
    dplyr::distinct()

  # join selected metadata to TADA df
  .data <- .data |>
    dplyr::left_join(
      ml.crosswalk,
      by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
    ) |>
    dplyr::left_join(
      select.meta,
      by = dplyr::join_by(TADA.MonitoringLocationIdentifier.New)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      TADA.MonitoringLocationName = ifelse(
        !is.na(TADA.MonitoringLocationName.New),
        TADA.MonitoringLocationName.New,
        TADA.MonitoringLocationName
      ),
      TADA.LatitudeMeasure = ifelse(
        !is.na(TADA.LatitudeMeasure.New),
        TADA.LatitudeMeasure.New,
        TADA.LatitudeMeasure
      ),
      TADA.LongitudeMeasure = ifelse(
        !is.na(TADA.LongitudeMeasure.New),
        TADA.LongitudeMeasure.New,
        TADA.LongitudeMeasure
      ),
      TADA.MonitoringLocationTypeName = ifelse(
        !is.na(TADA.MonitoringLocationTypeName.New),
        TADA.MonitoringLocationTypeName.New,
        TADA.MonitoringLocationTypeName
      ),
      TADA.MonitoringLocationIdentifier = ifelse(
        !is.na(TADA.MonitoringLocationIdentifier.New),
        TADA.MonitoringLocationIdentifier.New,
        TADA.MonitoringLocationIdentifier
      ),
      TADA.NearbySiteGroup = TADA.NearbySiteGroup.New
    ) |>
    dplyr::select(
      -TADA.MonitoringLocationIdentifier.New,
      -TADA.MonitoringLocationName.New,
      -TADA.LatitudeMeasure.New,
      -TADA.LongitudeMeasure.New,
      -TADA.MonitoringLocationTypeName.New,
      -TADA.NearbySiteGroup.New
    ) |>
    TADA_OrderCols()

  # remove intermediate objects
  rm(select.meta, ml.crosswalk, group.sites, new.ids)

  # add flag for any ungrouped sites and order columns correctly
  .data <- TADA_OrderCols(.data) |>
    dplyr::mutate(
      TADA.NearbySites.Flag = ifelse(
        is.na(TADA.NearbySiteGroup),
        "No nearby sites detected using input buffer distance.",
        TADA.NearbySites.Flag
      )
    ) |>
    TADA_CorrectColType()

  # return TADA df with added columns for tracking
  return(.data)
}


#' Get grouped monitoring stations that are near each other
#'
#' This function takes a TADA dataset that contains grouped nearby monitoring stations
#' and returns a unique dataset of the original MonitoringLocationIdentifier, the grouped
#' TADA.MonitoringLocationIdentifier, as well as the original and TADA-prefixed LongitudeMeasure,
#' LatitudeMeasure, MonitoringLocationName, and MonitoringLocationTypeName, filtered for only those
#' stations that have a nearby station.
#'
#' @param .data TADA dataframe
#'
#' @return New dataframe with unique combinations of original and TADA MonitoringLocationIdentifier,
#' LongitudeMeasure, LatitudeMeasure, MonitoringLocationName, and MonitoringLocationTypeName.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # use MT example data set
#' testdat <- Data_MT_MissoulaCounty
#'
#' # find unique nearby sites
#' testdat.unique <- testdat |>
#'   TADA_FindNearbySites() |>
#'   TADA_GetUniqueNearbySites()
#' }
#'
TADA_GetUniqueNearbySites <- function(.data) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "MonitoringLocationIdentifier",
    "TADA.MonitoringLocationIdentifier",
    "MonitoringLocationName",
    "TADA.MonitoringLocationName",
    "LongitudeMeasure",
    "TADA.LongitudeMeasure",
    "LatitudeMeasure",
    "TADA.LatitudeMeasure",
    "MonitoringLocationTypeName",
    "TADA.MonitoringLocationTypeName",
    "MonitoringLocationDescriptionText",
    "TADA.NearbySites.Flag",
    "TADA.NearbySiteGroup"
  )
  TADA_CheckColumns(.data, expected_cols)

  # filter only for locations with nearby sites
  .data <- .data |>
    dplyr::filter(
      !is.na(TADA.NearbySites.Flag),
      TADA.NearbySites.Flag !=
        "No nearby sites detected using input buffer distance."
    ) |>
    # retain only required columns
    dplyr::select(dplyr::all_of(expected_cols)) |>
    # retain only unique records
    dplyr::distinct()

  return(.data)
}

#' TADA_CreateAUMLCrosswalk
#'
#' Create the assessment unit and monitoring location ref by utilizing an optional
#' user-supplied crosswalk, AU/ML crosswalk from ATTAINS (if org has entered that data),
#' and TADA_CreateATTAINSAUMLCrosswalk to match unassigned monitoring locations to assessment units.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()`.
#' @param au_ref Optional. A user-supplied df with the columns AssessmentUnitIdentifier,
#' MonitoringLocationIdentifier and WaterType.
#' @param org_id ATTAINS organization identifier(s) as a character string.
#' If populated, Monitoring Locations will only be matched to Assessment Units from the
#' specified organization(s). A list of organization
#' identifiers can be found
#' by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param. When
#' org_id = "all", the MonitoringLocationIdentifier/AssessmentUnitIdentifier matches
#' from all organizations will be considered. When org_id = "none" or NULL, no
#' crosswalk data from ATTAINS will be considered. The default is "all".
#' @param fill_ATTAINS_catch Boolean argument. Specifies whether catchment-based
#' ATTAINS assessment unit data (EPA snapshot of NHDPlus HR catchments associated
#' with entity submitted assessment unit features - points, lines, and polygons)
#' should be queried and downloaded for the assessment units included in the
#' USER-SUPPLIED `au_ref`. When fill_ATTAINS_catch = TRUE, the catchment data
#' are included in the output. When fill_ATTAINS_catch = FALSE, catchment data
#' are not included. Setting fill_ATTAINS_catch = TRUE, may increase the
#' run time of the function significantly. Default is fill_ATTAINS_catch = FALSE.
#' @param return_nearest  If a WQP observation falls within more than one AU,
#' return ONLY the nearest AU (return_nearest = TRUE), or all AUs
#' (return_nearest = FALSE). This param applies only to WQP observations that do
#' not have matches in the user-supplied ref or ATTAINS.
#' @param batch_upload Boolean argument. When batch_upload = TRUE, an additional data frame
#' which matches the format required for batch upload to ATTAINS is included in the
#' output. When batch_upload = FALSE, this df is not included in the output.
#' Default is batch_upload = FALSE. If you would like to add new monitoring location
#' data links or retain existing ones in ATTAINS, you will need to run
#' TADA_UpdateATTAINSAUMLCrosswalk on the ATTAINS_batchupload data frame from this
#' function's output.
#' @param api_key Optional character string. An api key for Expert Query web
#' services. If not supplied, the default TADA api key will be used. For best
#' performance, it is recommended that users obtain and use their own api key.
#' Request an api key here: https://owapps.epa.gov/expertquery/api-documentation
#'
#' @return A list containing a modified TADA data frame with added ATTAINS columns and
#' data frames for ATTAINS data and features for points, lines, polygons and catchments.
#' When batch_upload = TRUE, the list will contain an additional data frame formatted
#' for compatibility with ATTAINS batch upload for Monitoring_Stations.
#'
#' @seealso
#' [TADA_CreateATTAINSAUMLCrosswalk()]
#' [TADA_GetATTAINSAUMLCrosswalk()]
#' [TADA_UpdateATTAINSAUMLCrosswalk()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the example data
#' utils::data(Data_MT_MissoulaCounty)
#'
#' # Example 1: Basic use with default settings
#' result <- TADA_CreateAUMLCrosswalk(Data_MT_MissoulaCounty)
#'
#' # Example 2: Using a user-supplied crosswalk
#' user_crosswalk <- data.frame(
#'   AssessmentUnitIdentifier = c("AU1", "AU2"),
#'   MonitoringLocationIdentifier = c("ML1", "ML2"),
#'   WaterType = c("River", "Lake")
#' )
#' result <- TADA_CreateAUMLCrosswalk(
#'   Data_MT_MissoulaCounty,
#'   au_ref = user_crosswalk
#' )
#'
#' # Example 3: Including ATTAINS catchment data
#' result <- TADA_CreateAUMLCrosswalk(
#'   Data_MT_MissoulaCounty,
#'   fill_ATTAINS_catch = TRUE
#' )
#'
#' # Example 4: Preparing for batch upload
#' result <- TADA_CreateAUMLCrosswalk(
#'   Data_MT_MissoulaCounty,
#'   batch_upload = TRUE
#' )
#'
#' # Example 5: Using multiple options together
#' org_id <- "EPA"
#' result <- TADA_CreateAUMLCrosswalk(
#'   Data_MT_MissoulaCounty,
#'   au_ref = user_crosswalk,
#'   org_id = org_id,
#'   fill_ATTAINS_catch = TRUE,
#'   return_nearest = FALSE,
#'   batch_upload = TRUE
#' )
#'
#' # View the results
#' print(result$TADA_with_ATTAINS)
#' print(result$ATTAINS_catchments)
#' print(result$ATTAINS_batchupload)
#' }
#'
TADA_CreateAUMLCrosswalk <- function(
  .data,
  au_ref = NULL,
  org_id = "all",
  fill_ATTAINS_catch = FALSE,
  return_nearest = TRUE,
  batch_upload = FALSE,
  api_key = NULL
) {
  # get default api_key if user does not supply one
  if (is.null(api_key)) {
    api_key <- .setDefaultEQKey()
  }

  # create list where all user matches dfs are set to NULL
  user.matches <- list(
    "TADA_with_ATTAINS" = NULL,
    "ATTAINS_catchments" = NULL,
    "ATTAINS_points" = NULL,
    "ATTAINS_lines" = NULL,
    "ATTAINS_polygons" = NULL
  )

  # check to see if user supplied ref is NULL
  if (is.null(au_ref)) {
    print(paste0(
      "TADA_CreateAUMLCrosswalk: no au_ref (user-supplied crosswalk ",
      "was provided."
    ))
  }

  # check to see if user supplied ref is not NULL
  if (!is.null(au_ref)) {
    # check to see if user supplied ref is not a data frame
    if (!is.data.frame(au_ref)) {
      # stop function with printed message if the user supplied ref is not a data frame
      stop(paste0(
        "TADA_CreateAUMLCrosswalk: The user supplied au_ref must be a data frame ",
        "containing the columns AssessmentUnitIdentifier, MonitoringLocationIdentifier, and ATTAINS.WaterType.",
        "MonitoringLocationIdentifiers must be WQP compatible."
      ))
    }

    # check to see if user supplied ref is a data frame
    if (is.data.frame(au_ref)) {
      print(paste0(
        "TADA_CreateAUMLCrosswalk: fetching ATTAINS geospatial data ",
        "for assessment units in the user-supplied crosswalk."
      ))

      # list of partial string matches for columns in au_ref
      req.cols <- c(
        "AssessmentUnitIdentifier",
        "MonitoringLocationIdentifier",
        "WaterType"
      )

      # get column names by using internal function checkColName (in Utilities.R)
      col.ids <- purrr::map_dfr(req.cols, ~ checkColName(au_ref, .x))

      # assign values to col.id variables - might be possible to rewrite with purrr function (HRM 9/8/25)
      # assign assessment unit id
      assign(col.ids$col.id[1], col.ids$select.col[1])

      # assign monitoring location identifier
      assign(col.ids$col.id[2], col.ids$select.col[2])

      # assign water type
      assign(col.ids$col.id[3], col.ids$select.col[3])

      # rename au_ref cols for next function
      au_ref <- au_ref |>
        dplyr::rename(
          ATTAINS.MonitoringLocationIdentifier = paste0(ml.col),
          ATTAINS.AssessmentUnitIdentifier = paste0(auid.col),
          User.WaterType = paste0(type.col)
        )

      rm(col.ids, req.cols, auid.col, ml.col, type.col)

      # subset data for au_ref
      au.ref.mls <- .data |>
        dplyr::filter(
          TADA.MonitoringLocationIdentifier %in%
            au_ref$ATTAINS.MonitoringLocationIdentifier
        ) |>
        dplyr::mutate(TADA.AURefSource = "User-supplied Ref")

      if (dim(au.ref.mls)[1] > 0) {
        # get geospatial data for au_ref monitoring locations
        user.matches <- spsUtil::quiet(TADA_GetATTAINSByAUID(
          au.ref.mls,
          au_ref = au_ref,
          fill_ATTAINS_catch = fill_ATTAINS_catch,
          api_key = api_key
        ))

        # add AUIDs if user ref contained AUs not found in ATTAINS
        # set up user ref for join
        user.aus <- au_ref |>
          dplyr::select(
            ATTAINS.MonitoringLocationIdentifier,
            ATTAINS.AssessmentUnitIdentifier
          ) |>
          dplyr::rename(
            TADA.MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier,
            UserRef.AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier
          )

        # replace NA AUIDs with AUID from user ref where possible
        user.matches$TADA_with_ATTAINS <- user.matches$TADA_with_ATTAINS |>
          dplyr::left_join(user.aus) |>
          dplyr::mutate(
            ATTAINS.AssessmentUnitIdentifier = ifelse(
              is.na(ATTAINS.AssessmentUnitIdentifier) &
                !is.na(UserRef.AssessmentUnitIdentifier),
              UserRef.AssessmentUnitIdentifier,
              ATTAINS.AssessmentUnitIdentifier
            )
          ) |>
          dplyr::select(-UserRef.AssessmentUnitIdentifier) |>
          dplyr::distinct()

        # remove intermediate object
        rm(user.aus)
      }
    }
  }

  # ATTAINS supplied ref section
  # get attains crosswalk

  attains.matches <- list(
    "TADA_with_ATTAINS" = NULL,
    "ATTAINS_catchments" = NULL,
    "ATTAINS_points" = NULL,
    "ATTAINS_lines" = NULL,
    "ATTAINS_polygons" = NULL
  )

  # if no org id is provided, no crosswalk is imported from ATTAINS
  if (!is.null(org_id)) {
    if (org_id == "none" | is.null(org_id)) {
      print(paste0(
        "TADA_CreateAUMLCrosswalk: User has specified that ATTAINS ",
        "should not be checked for monitoring location and assessment unit matches."
      ))
    }
  }

  # if an org id is provided, ATTAINS is checked for a crosswalk
  if (format(org_id) != "none" & !is.null(org_id)) {
    print("TADA_CreateAUMLCrosswalk: checking for crosswalk in ATTAINS.")

    # get crosswalk from ATTAINS
    attains.cw <- spsUtil::quiet(TADA_GetATTAINSAUMLCrosswalk(org_id = org_id))

    # create string to describe ATTAINS orgs for print message
    org.text <- ifelse(
      is.null(org_id),
      "all organizations",
      stringi::stri_replace_last(
        paste(org_id, collapse = ", "),
        fixed = ", ",
        replacement = " and "
      )
    )

    # count number of records from ATTAINS crosswalk
    record.count <- dim(attains.cw)[1]

    # create text to describe number of records
    count.text <- ifelse(record.count == 0, "no", record.count)

    # print message summarizing the results of fetching crosswalk data from ATTAINS
    print(paste0(
      "TADA_CreateAUMLCrosswalk: There are ",
      count.text,
      " MonitoringLocation records ",
      "in ATTAINS for ",
      org.text,
      "."
    ))

    rm(org.text, record.count, count.text)

    if (dim(attains.cw)[1] > 0) {
      print(
        "TADA_CreateAUMLCrosswalk: crosswalk from ATTAINS has been imported."
      )

      # we could remove or make this step optional, but it is helpful for making sure
      # monitoring location identifiers are WQP compatible
      attains.cw <- TADA_UpdateATTAINSAUMLCrosswalk(
        org_id = org_id,
        crosswalk = attains.cw,
        attains_replace = TRUE
      )

      # create list of monitoring location identifiers from TADA df
      tada.mls <- .data |>
        dplyr::select(TADA.MonitoringLocationIdentifier) |>
        dplyr::distinct() |>
        dplyr::pull()

      # filter attains.cw to remove any assessment units that don't have a monitoring location
      # match in the TADA df
      attains.cw <- attains.cw |>
        dplyr::filter(ATTAINS.MonitoringLocationIdentifier %in% tada.mls)

      # remove intermediate object
      rm(tada.mls)

      # if au_ref was provided  by user, remove any records with monitoring locations matching user ref
      if (!is.null(au_ref)) {
        attains.cw.mls <- .data |>
          dplyr::filter(
            !TADA.MonitoringLocationIdentifier %in%
              au.ref.mls$TADA.MonitoringLocationIdentifier,
            TADA.MonitoringLocationIdentifier %in%
              attains.cw$ATTAINS.MonitoringLocationIdentifier
          )
      }

      # if au_ref was not provided  by user, retain all records that match ATTAINS ref
      if (is.null(au_ref)) {
        attains.cw.mls <- .data |>
          dplyr::filter(
            TADA.MonitoringLocationIdentifier %in%
              attains.cw$ATTAINS.MonitoringLocationIdentifier
          )
      }

      # set TADA_with_ATTAINS to null if no matches between monitoring location identifiers and ATTAINS crosswalk
      if (dim(attains.cw.mls)[1] == 0) {
        attains.matches <- list(
          "TADA_with_ATTAINS" = NULL,
          "ATTAINS_catchments" = NULL,
          "ATTAINS_points" = NULL,
          "ATTAINS_lines" = NULL,
          "ATTAINS_polygons" = NULL
        )
      } else {
        # add source column for ATTAINS Crosswalk matched records
        attains.cw.mls <- attains.cw.mls |>
          dplyr::mutate(TADA.AURefSource = "ATTAINS Crosswalk")

        print(paste0(
          "TADA_CreateAUMLCrosswalk: fetching ATTAINS geospatial data ",
          "for assessment units from the ATTAINS crosswalk."
        ))
        # get geospatial data for attains cw monitoring locations
        attains.matches <- spsUtil::quiet(TADA_GetATTAINSByAUID(
          attains.cw.mls,
          au_ref = attains.cw,
          fill_ATTAINS_catch = fill_ATTAINS_catch
        ))

        if (nrow(attains.cw) > 0) {
          # add AUIDs if ATTAINS crosswalk contained AUs not found in ATTAINS geospatial services
          # set up user ref for join
          attains.cw.aus <- attains.cw |>
            dplyr::select(
              ATTAINS.MonitoringLocationIdentifier,
              ATTAINS.AssessmentUnitIdentifier
            ) |>
            dplyr::filter(
              ATTAINS.MonitoringLocationIdentifier %in%
                attains.matches$TADA_with_ATTAINS$TADA.MonitoringLocationIdentifier
            ) |>
            dplyr::rename(
              TADA.MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier,
              Ref.AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier
            ) |>
            dplyr::distinct()

          # replace NA AUIDs with AUID from ATTAINS ref where possible

          attains.matches$TADA_with_ATTAINS <- attains.matches$TADA_with_ATTAINS |>
            dplyr::left_join(
              attains.cw.aus,
              by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
            ) |>
            dplyr::mutate(
              ATTAINS.AssessmentUnitIdentifier = ifelse(
                !is.na(Ref.AssessmentUnitIdentifier),
                Ref.AssessmentUnitIdentifier,
                NA
              ),
              TADA.AURefSource = ifelse(
                !is.na(Ref.AssessmentUnitIdentifier),
                "ATTAINS Crosswalk",
                NA
              )
            ) |>
            dplyr::select(-Ref.AssessmentUnitIdentifier) |>
            dplyr::distinct() |>
            TADA_CorrectColType()
        }

        # remove intermediate objects
        if (exists("attains.cw", inherits = FALSE)) {
          rm("attains.cw")
        }
      }
    }
  }

  # TADA_CreateATTAINSAUMLCrosswalk section

  print(paste0(
    "TADA_CreateAUMLCrosswalk: checking to see if any unmatched ",
    "monitoring locations remain in the original TADA data frame."
  ))

  get.attains.mls <- .data

  if (!is.null(attains.matches$TADA_with_ATTAINS)) {
    get.attains.mls <- get.attains.mls |>
      dplyr::filter(
        !TADA.MonitoringLocationIdentifier %in%
          attains.cw.mls$TADA.MonitoringLocationIdentifier
      )
  }

  if (!is.null(user.matches$TADA_with_ATTAINS)) {
    get.attains.mls <- get.attains.mls |>
      dplyr::filter(
        !TADA.MonitoringLocationIdentifier %in%
          au.ref.mls$TADA.MonitoringLocationIdentifier
      )

    # remove intermediate object
    rm(au.ref.mls)
  }

  # add code here for if there are no remaining mls to match
  if (dim(get.attains.mls)[1] == 0) {
    print(paste0(
      "TADA_CreateAUMLCrosswalk: all monitorintg locations have ",
      "already been matched to an assessment unit by the user or ATTAINS."
    ))

    get.attains.matches <- list(
      "TADA_with_ATTAINS" = NULL,
      "ATTAINS_catchments" = NULL,
      "ATTAINS_points" = NULL,
      "ATTAINS_lines" = NULL,
      "ATTAINS_polygons" = NULL
    )
  }

  if (dim(get.attains.mls)[1] > 0) {
    print(
      "TADA_CreateAUMLCrosswalk: using TADA_CreateATTAINSAUMLCrosswalk to match remaining monitoring locations to ATTAINS assessment units using a spatial join (EPA snapshot of NHDPlus HR catchments associated with entity submitted assessment unit features). Also returning USGS snapshot of NHDPlus V2 HR for monitoring locations not near any ATTAINS assessment unit."
    )

    # add source ref column for TADA_CreateATTAINSAUMLCrosswalk matches
    get.attains.mls <- get.attains.mls |>
      dplyr::mutate(TADA.AURefSource = "TADA_CreateATTAINSAUMLCrosswalk")

    # set org id for pulling cw from ATTAINS
    org_id <- if (org_id == "none") {
      # set org id to all so that geospatial data from all orgs are considered
      org_id <- "all"
    } else {
      org_id <- org_id
    }

    # use get attains for matching remaining monitoring locations
    get.attains.matches <- TADA_CreateATTAINSAUMLCrosswalk(
      # spsUtil::quiet(
      get.attains.mls,
      return_nearest = return_nearest,
      return_sf = TRUE,
      org_id = org_id
    ) # )
  }

  # remove intermediate objects
  if (exists("attains.cw.mls")) {
    rm(attains.cw.mls)
  }

  if (exists("get.attains.mls")) {
    rm(get.attains.mls)
  }

  # join all the resulting tables within each list to return as one large list
  # TADA_with_ATTAINS

  print(
    "TADA_CreateAUMLCrosswalk: joining results to return list of dataframes compatible with TADA_ViewATTAINS."
  )

  # internal function to prep output by binding rows from different crosswalk sources
  outputPrep <- function(df.name, user, attains, get.attains) {
    # correct column types and filter out invalid geometries for each dataframe
    user <- user[[df.name]]

    if (!is.null(user) & df.name != "TADA_with_ATTAINS") {
      user <- sf::st_make_valid(user)
    }

    if (!is.null(user) & df.name == "TADA_with_ATTAINS") {
      user_geometry <- sf::st_as_sf(
        user,
        coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
        crs = 4326
      )

      geometry <- sf::st_geometry(user_geometry)

      user$geometry <- geometry

      user <- sf::st_as_sf(user)

      rm(geometry)
    }

    user <- TADA_CorrectColType(user)

    attains <- attains[[df.name]]

    if (!is.null(attains) & df.name != "TADA_with_ATTAINS") {
      attains <- sf::st_make_valid(attains)
    }

    if (!is.null(attains) & df.name == "TADA_with_ATTAINS") {
      attains_geometry <- sf::st_as_sf(
        attains,
        coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
        crs = 4326
      )
      geometry <- sf::st_geometry(attains_geometry)

      attains$geometry <- geometry

      attains <- sf::st_as_sf(attains)

      rm(geometry)
    }

    attains <- TADA_CorrectColType(attains)

    get.attains <- get.attains[[df.name]]

    if (!is.null(get.attains) & df.name != "TADA_with_ATTAINS") {
      get.attains <- sf::st_make_valid(get.attains)
    }

    if (!is.null(get.attains) & df.name == "TADA_with_ATTAINS") {
      get.attains_geometry <- sf::st_as_sf(
        get.attains,
        coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
        crs = 4326
      )
      geometry <- sf::st_geometry(get.attains_geometry)

      get.attains$geometry <- geometry

      get.attains <- sf::st_as_sf(get.attains)

      rm(geometry)
    }

    get.attains <- TADA_CorrectColType(get.attains)

    # Check if any of the inputs are not NULL
    if (!is.null(user) || !is.null(attains) || !is.null(get.attains)) {
      # Bind rows and remove duplicates
      df <- dplyr::bind_rows(user, attains, get.attains) |> dplyr::distinct()
    } else {
      df <- NULL
    }

    return(df)
  }

  # create TADA_with_ATTAINS for output list
  TADA_with_ATTAINS <- outputPrep(
    df.name = "TADA_with_ATTAINS",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  ) |>
    TADA_CorrectColType() |>
    suppressMessages(dplyr::full_join(.data))

  # create ATTAINS_catchments for output list
  ATTAINS_catchments <- outputPrep(
    df.name = "ATTAINS_catchments",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  # create ATTAINS_lines for output list
  ATTAINS_lines <- outputPrep(
    df.name = "ATTAINS_lines",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  # create ATTAINS_points for output list
  ATTAINS_points <- outputPrep(
    df.name = "ATTAINS_points",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  # create ATTAINS_polygons for output list
  ATTAINS_polygons <- outputPrep(
    df.name = "ATTAINS_polygons",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  # create ATTAINS_crosswalk for output list
  ATTAINS_crosswalk <- TADA_with_ATTAINS |>
    sf::st_drop_geometry() |>
    dplyr::select(
      OrganizationIdentifier,
      TADA.MonitoringLocationIdentifier,
      ATTAINS.OrganizationIdentifier,
      ATTAINS.AssessmentUnitIdentifier,
      ATTAINS.WaterType,
      TADA.AURefSource
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier))

  # create final output list of all dfs
  final_list <- list(
    "TADA_with_ATTAINS" = TADA_with_ATTAINS,
    "ATTAINS_catchments" = ATTAINS_catchments,
    "ATTAINS_points" = ATTAINS_points,
    "ATTAINS_lines" = ATTAINS_lines,
    "ATTAINS_polygons" = ATTAINS_polygons,
    "ATTAINS_crosswalk" = ATTAINS_crosswalk
  )

  # add batch upload df to list for output if user has selected this option
  if (batch_upload == TRUE) {
    # create batch upload for ATTAINS df
    ATTAINS_batchupload <- TADA_with_ATTAINS |>
      sf::st_drop_geometry() |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier,
        ATTAINS.AssessmentUnitIdentifier,
        OrganizationIdentifier
      ) |>
      dplyr::distinct() |>
      dplyr::rename(
        MS_LOCATION_ID = TADA.MonitoringLocationIdentifier,
        ASSESSMENT_UNIT_ID = ATTAINS.AssessmentUnitIdentifier,
        MS_ORG_ID = OrganizationIdentifier
      ) |>
      dplyr::mutate(MS_DATA_LINK = NA) |>
      dplyr::select(
        ASSESSMENT_UNIT_ID,
        MS_ORG_ID,
        MS_LOCATION_ID,
        MS_DATA_LINK
      ) |>
      dplyr::filter(!is.na(ASSESSMENT_UNIT_ID))

    # add batch upload df to list for output
    final_list <- c(
      final_list,
      list("ATTAINS_batchupload" = ATTAINS_batchupload)
    )

    # remove intermediate objects
    rm(ATTAINS_batchupload)
  }

  # remove intermediate objects
  rm(
    TADA_with_ATTAINS,
    ATTAINS_catchments,
    ATTAINS_points,
    ATTAINS_lines,
    ATTAINS_polygons,
    ATTAINS_crosswalk
  )

  # remove intermediate objects
  rm(attains.matches, user.matches, get.attains.matches)

  # return final list of dfs based on user inputs
  return(final_list)
}
