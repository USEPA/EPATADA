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
  # Early validations
  if (is.null(.data)) {
    stop("Input `.data` is NULL.")
  }
  if (inherits(.data, "sf")) {
    stop("Your data is already a spatial object.")
  }

  required_cols <- c(
    "TADA.LongitudeMeasure",
    "TADA.LatitudeMeasure",
    "HorizontalCoordinateReferenceSystemDatumName"
  )
  if (!all(required_cols %in% names(.data))) {
    stop(
      "The dataframe does not contain TADA-style latitude and longitude data ",
      "(column names `HorizontalCoordinateReferenceSystemDatumName`, ",
      "`TADA.LatitudeMeasure`, and `TADA.LongitudeMeasure`)."
    )
  }

  message("Transforming your data into a spatial object.")

  # Resolve target CRS (as EPSG and crs object)
  target_epsg <- suppressWarnings(as.numeric(crs))
  if (is.na(target_epsg)) {
    # allow st_crs inputs like "EPSG:4326" or list crs objects
    target_crs_obj <- sf::st_crs(crs)
    target_epsg <- target_crs_obj$epsg
    if (is.na(target_epsg)) {
      stop(
        "Could not interpret `crs`. Provide an EPSG code (e.g., 4326) or a valid `st_crs` value."
      )
    }
  }
  target_crs_obj <- sf::st_crs(target_epsg)

  # Reference table for CRS/EPSG codes
  epsg_codes <- tibble::tribble(
    ~HorizontalCoordinateReferenceSystemDatumName , ~epsg       ,
    "NAD83"                                       ,        4269 ,
    "WGS84"                                       ,        4326 ,
    "NAD27"                                       ,        4267 ,
    "UNKWN"                                       , target_epsg ,
    "Unknown"                                     , target_epsg ,
    "OTHER"                                       , target_epsg ,
    "OLDHI"                                       ,        4135 ,
    "AMSMA"                                       ,        4169 ,
    "ASTRO"                                       ,        4727 ,
    "GUAM"                                        ,        4675 ,
    "JHNSN"                                       ,        4725 ,
    "PR"                                          ,        6139 ,
    "SGEOR"                                       ,        4138 ,
    "SLAWR"                                       ,        4136 ,
    "SPAUL"                                       ,        4137 ,
    "WAKE"                                        ,        6732 ,
    "WGS72"                                       ,        6322 ,
    "HARN"                                        ,        4152
  )

  # Handle missing/unknown CRS labels
  if (
    any(is.na(.data$HorizontalCoordinateReferenceSystemDatumName)) ||
      any(
        .data$HorizontalCoordinateReferenceSystemDatumName %in%
          c("UNKWN", "Unknown", "OTHER")
      )
  ) {
    message(sprintf(
      "Your WQP dataframe contains observations without a listed CRS. Assigning CRS %s to those rows.",
      target_epsg
    ))
    .data$HorizontalCoordinateReferenceSystemDatumName[is.na(
      .data$HorizontalCoordinateReferenceSystemDatumName
    )] <- "Unknown"
  }

  # Prepare data: attach EPSG and numeric lon/lat
  df <- dplyr::left_join(
    .data,
    epsg_codes,
    by = "HorizontalCoordinateReferenceSystemDatumName"
  ) |>
    dplyr::mutate(
      lat = suppressWarnings(as.numeric(TADA.LatitudeMeasure)),
      lon = suppressWarnings(as.numeric(TADA.LongitudeMeasure))
    )

  # Drop rows with missing coordinates
  n_before <- nrow(df)
  df <- df[!is.na(df$lon) & !is.na(df$lat), ]
  n_dropped <- n_before - nrow(df)
  if (n_dropped > 0) {
    message(sprintf(
      "Dropped %d rows with missing longitude/latitude.",
      n_dropped
    ))
  }
  if (nrow(df) == 0) {
    stop("No valid rows with latitude/longitude found.")
  }

  # Convert each CRS subset to sf, then transform to target CRS
  sflist <- lapply(
    split(df, df$HorizontalCoordinateReferenceSystemDatumName),
    function(subset_data) {
      if (nrow(subset_data) == 0) {
        return(NULL)
      }
      epsg_val <- unique(subset_data$epsg)
      if (length(epsg_val) != 1 || is.na(epsg_val)) {
        epsg_val <- target_epsg
      }
      sf_obj <- sf::st_as_sf(
        subset_data,
        coords = c("lon", "lat"),
        crs = epsg_val,
        remove = TRUE
      )
      sf::st_transform(sf_obj, target_crs_obj)
    }
  )

  # Remove empty elements
  sflist <- Filter(Negate(is.null), sflist)
  if (length(sflist) == 0) {
    stop(
      "No valid point geometries could be created (check latitude/longitude and CRS values)."
    )
  }

  # Row-bind while preserving sf class (rbind has S3 methods for sf)
  sf_out <- do.call(rbind, sflist)

  return(sf_out)
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
  # Dependencies
  if (!requireNamespace("arcgislayers", quietly = TRUE)) {
    stop(
      "The 'arcgislayers' package is required. Install with install.packages('arcgislayers')."
    )
  }

  original_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(
    suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))),
    add = TRUE
  )

  message(
    "Depending on your data's observation count and its spatial range, the ATTAINS pull may take a while."
  )

  our_epsg <- 4326

  # Normalize input sf/data.frame
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
    distinct_data <- .data |>
      data.table::data.table() |>
      dplyr::distinct(
        TADA.LongitudeMeasure,
        TADA.LatitudeMeasure,
        .keep_all = TRUE
      )
    .data <- TADA_MakeSpatial(.data = distinct_data, crs = our_epsg)
  }

  if (is.null(.data) || nrow(.data) == 0) {
    stop(
      "There is no data in your `data` object to use as a bounding box for selecting ATTAINS features."
    )
  }

  # ATTAINS MapServer layer URLs
  layer_urls <- list(
    catchments = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3",
    points = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0",
    lines = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1",
    polygons = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2"
  )

  # Helper: arcgislayers query by bbox (tries arcgis_features, then arcgis_read)
  agl_query_bbox <- function(
    layer_url,
    sf_bbox_obj,
    where = "1=1",
    out_fields = "*"
  ) {
    bb <- sf::st_bbox(sf_bbox_obj)
    bbox_vec <- c(
      as.numeric(bb["xmin"]),
      as.numeric(bb["ymin"]),
      as.numeric(bb["xmax"]),
      as.numeric(bb["ymax"])
    )

    # Try arcgis_features first
    res <- try(
      arcgislayers::arcgis_features(
        url = layer_url,
        where = where,
        outFields = out_fields,
        bbox = bbox_vec,
        spatialRel = "esriSpatialRelIntersects",
        outSR = our_epsg,
        returnGeometry = TRUE
      ),
      silent = TRUE
    )

    # Fallback to arcgis_read if needed
    if (inherits(res, "try-error") || is.null(res)) {
      res <- try(
        arcgislayers::arcgis_read(
          url = layer_url,
          where = where,
          outFields = out_fields,
          bbox = bbox_vec
        ),
        silent = TRUE
      )
    }

    if (inherits(res, "try-error") || is.null(res)) {
      return(NULL)
    }
    if (inherits(res, "sf")) {
      suppressWarnings(res <- sf::st_transform(res, our_epsg))
    }
    res
  }

  # org filter
  if (org_id == "all") {
    org_filter <- "1=1"
  } else {
    org_filter <- paste0(
      "organizationid IN ('",
      paste(org_id, collapse = "','"),
      "')"
    )
  }

  # Fetch AU features by assessmentunitidentifier using arcgislayers (chunked)
  fetch_au <- function(layer_url, assessment_unit_ids, org_filter) {
    if (length(assessment_unit_ids) == 0) {
      return(NULL)
    }
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

      # Try arcgis_features first
      res <- try(
        arcgislayers::arcgis_features(
          url = layer_url,
          where = where_clause,
          outFields = "*",
          returnGeometry = TRUE,
          outSR = our_epsg
        ),
        silent = TRUE
      )

      # Fallback to arcgis_read if needed
      if (inherits(res, "try-error") || is.null(res)) {
        res <- try(
          arcgislayers::arcgis_read(
            url = layer_url,
            where = where_clause,
            outFields = "*"
          ),
          silent = TRUE
        )
      }

      if (inherits(res, "try-error") || is.null(res)) {
        return(NULL)
      }
      if (inherits(res, "sf")) {
        suppressWarnings(res <- sf::st_transform(res, our_epsg))
      }
      res
    }

    purrr::map_dfr(id_chunks, fetch_chunk)
  }

  # Grab waterbody type codes from ATTAINS API (unchanged)
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

  # Iterative clustering for large areas (unchanged logic)
  if (as.numeric(sf::st_area(sf::st_as_sfc(sf::st_bbox(.data)))) >= 6e+9) {
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
      eps_sequence <- c(0.25, 0.05, 1, 0.1)
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

    # Build cluster list and query catchments per cluster bbox
    clustered_points <- dplyr::bind_rows(init[["clusters_by_iteration"]])
    # Points not clustered as "small" become individual clusters
    remaining_points <- dplyr::anti_join(
      points_sf,
      clustered_points,
      by = "geometry"
    )
    remaining_points <- remaining_points |>
      tibble::rowid_to_column(var = "cluster") |>
      dplyr::mutate(cluster = as.character(cluster))

    final_cluster_list <- dplyr::bind_rows(
      if (!is.null(clustered_points) && nrow(clustered_points) > 0) {
        clustered_points
      } else {
        NULL
      },
      remaining_points
    )

    catchment_features <- vector(
      "list",
      length = length(unique(final_cluster_list$cluster))
    )
    uniq_clusters <- unique(final_cluster_list$cluster)

    for (i in seq_along(uniq_clusters)) {
      this_cluster <- uniq_clusters[i]
      cluster_sf <- final_cluster_list |> dplyr::filter(cluster == this_cluster)
      # Query with arcgislayers using cluster bbox
      catchment_features[[i]] <- try(
        agl_query_bbox(
          layer_urls$catchments,
          cluster_sf,
          where = "1=1",
          out_fields = "*"
        ),
        silent = TRUE
      )
      if (inherits(catchment_features[[i]], "try-error")) {
        catchment_features[[i]] <- NULL
      }
    }

    catchment_features <- catchment_features |>
      purrr::keep(~ !is.null(.)) |>
      purrr::keep(~ inherits(., "sf") && nrow(.) > 0) |>
      dplyr::bind_rows() |>
      dplyr::distinct(.keep_all = TRUE)

    # Clip to input points if possible
    try(
      catchment_features <- try(
        (\(x) x[points_sf, ])(catchment_features),
        silent = TRUE
      ),
      silent = TRUE
    )

    if (
      length(catchment_features) == 0 ||
        is.null(catchment_features) ||
        nrow(catchment_features) == 0
    ) {
      message(
        "There are no ATTAINS features associated with your WQP observations."
      )
      water_types <- NULL
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

    if (isTRUE(catchments_only)) {
      return(list("ATTAINS_catchments" = catchment_features))
    }

    # Fetch raw AU features (points/lines/polygons) by AU IDs using arcgislayers
    points <- fetch_au(
      layer_urls$points,
      unique(catchment_features$assessmentunitidentifier),
      org_filter
    )
    lines <- fetch_au(
      layer_urls$lines,
      unique(catchment_features$assessmentunitidentifier),
      org_filter
    )
    polygons <- fetch_au(
      layer_urls$polygons,
      unique(catchment_features$assessmentunitidentifier),
      org_filter
    )

    try(
      points <- dplyr::left_join(
        points,
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
      ),
      silent = TRUE
    )
    try(
      lines <- dplyr::left_join(
        lines,
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
      ),
      silent = TRUE
    )
    try(
      polygons <- dplyr::left_join(
        polygons,
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
    return(final_features)
  } else {
    # Moderate-small area: single bbox query
    points_sf <- .data
    catchment_features <- agl_query_bbox(
      layer_urls$catchments,
      points_sf,
      where = "1=1",
      out_fields = "*"
    )

    try(
      catchment_features <- try(
        (\(x) x[points_sf, ])(catchment_features),
        silent = TRUE
      ),
      silent = TRUE
    )

    if (
      length(catchment_features) == 0 ||
        is.null(catchment_features) ||
        nrow(catchment_features) == 0
    ) {
      message(
        "There are no ATTAINS features associated with your WQP observations."
      )
      water_types <- NULL
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

    if (isTRUE(catchments_only)) {
      return(list("ATTAINS_catchments" = catchment_features))
    }

    # Fetch raw AU features (points/lines/polygons) by AU IDs using arcgislayers
    points <- lines <- polygons <- NULL

    try(
      points <- fetch_au(
        layer_urls$points,
        unique(catchment_features$assessmentunitidentifier),
        org_filter
      ),
      silent = TRUE
    )
    try(
      lines <- fetch_au(
        layer_urls$lines,
        unique(catchment_features$assessmentunitidentifier),
        org_filter
      ),
      silent = TRUE
    )
    try(
      polygons <- fetch_au(
        layer_urls$polygons,
        unique(catchment_features$assessmentunitidentifier),
        org_filter
      ),
      silent = TRUE
    )

    try(
      points <- dplyr::left_join(
        points,
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
      ),
      silent = TRUE
    )
    try(
      lines <- dplyr::left_join(
        lines,
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitIdentifier")
      ),
      silent = TRUE
    )
    try(
      polygons <- dplyr::left_join(
        polygons,
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
  # Ensure arcgislayers for HiRes path
  if (resolution %in% c("Hi", "hi")) {
    if (!requireNamespace("arcgislayers", quietly = TRUE)) {
      stop(
        "Package 'arcgislayers' is required for resolution = 'Hi'. Install with install.packages('arcgislayers')."
      )
    }
  }

  # function settings that we ensure go back to their original settings after the function stops:
  original_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  original_timeout <- getOption("timeout")
  options(timeout = 30000)
  on.exit(options(timeout = original_timeout), add = TRUE)
  on.exit(
    suppressMessages(suppressWarnings(sf::sf_use_s2(original_s2))),
    add = TRUE
  )

  # Helper: arc_select compatibility (tries different arg names and bbox fallback)
  .arc_select_compat <- function(
    lyr,
    where = "1=1",
    fields = "*",
    filter_geom = NULL,
    bbox = NULL,
    spatialRel = "esriSpatialRelIntersects",
    crs = 4326,
    geometry = TRUE
  ) {
    # Attempt 1: filter_geom + fields/crs/geometry
    res <- try(
      arcgislayers::arc_select(
        lyr,
        where = where,
        fields = fields,
        filter_geom = filter_geom,
        spatialRel = spatialRel,
        crs = crs,
        geometry = geometry
      ),
      silent = TRUE
    )
    if (!inherits(res, "try-error") && !is.null(res)) {
      if (inherits(res, "sf")) {
        res <- suppressWarnings(sf::st_transform(res, 4326))
      }
      return(res)
    }

    # Attempt 2: filter_geom + outFields/outSR/returnGeometry
    res <- try(
      arcgislayers::arc_select(
        lyr,
        where = where,
        outFields = fields,
        filter_geom = filter_geom,
        spatialRel = spatialRel,
        outSR = crs,
        returnGeometry = geometry
      ),
      silent = TRUE
    )
    if (!inherits(res, "try-error") && !is.null(res)) {
      if (inherits(res, "sf")) {
        res <- suppressWarnings(sf::st_transform(res, 4326))
      }
      return(res)
    }

    # Attempt 3: bbox + fields/crs/geometry
    if (!is.null(bbox)) {
      res <- try(
        arcgislayers::arc_select(
          lyr,
          where = where,
          fields = fields,
          bbox = bbox,
          spatialRel = spatialRel,
          crs = crs,
          geometry = geometry
        ),
        silent = TRUE
      )
      if (!inherits(res, "try-error") && !is.null(res)) {
        if (inherits(res, "sf")) {
          res <- suppressWarnings(sf::st_transform(res, 4326))
        }
        return(res)
      }

      # Attempt 4: bbox + outFields/outSR/returnGeometry
      res <- try(
        arcgislayers::arc_select(
          lyr,
          where = where,
          outFields = fields,
          bbox = bbox,
          spatialRel = spatialRel,
          outSR = crs,
          returnGeometry = geometry
        ),
        silent = TRUE
      )
      if (!inherits(res, "try-error") && !is.null(res)) {
        if (inherits(res, "sf")) {
          res <- suppressWarnings(sf::st_transform(res, 4326))
        }
        return(res)
      }
    }

    # Attempt 5: minimal where + fields
    res <- try(
      arcgislayers::arc_select(lyr, where = where, fields = fields),
      silent = TRUE
    )
    if (!inherits(res, "try-error") && !is.null(res)) {
      if (inherits(res, "sf")) {
        res <- suppressWarnings(sf::st_transform(res, 4326))
      }
      return(res)
    }

    # Attempt 6: minimal where + outFields
    res <- try(
      arcgislayers::arc_select(lyr, where = where, outFields = fields),
      silent = TRUE
    )
    if (!inherits(res, "try-error") && !is.null(res)) {
      if (inherits(res, "sf")) {
        res <- suppressWarnings(sf::st_transform(res, 4326))
      }
      return(res)
    }

    NULL
  }

  # Helper: force sf to a target CRS (EPSG:4326 by default)
  harmonize_sf <- function(x, target = sf::st_crs(4326)) {
    if (!inherits(x, "sf")) {
      return(x)
    }
    out <- x
    out <- suppressWarnings({
      if (is.na(sf::st_crs(out))) {
        sf::st_set_crs(out, target)
      } else if (sf::st_crs(out) != target) {
        sf::st_transform(out, target)
      } else {
        out
      }
    })
    out
  }

  # Helpers to return empty sf with CRS for safe downstream joins
  .empty_catchments_hr <- function() {
    sf::st_sf(
      NHD.nhdplusid = character(),
      NHD.resolution = character(),
      NHD.catchmentareasqkm = numeric(),
      geometry = sf::st_sfc(crs = sf::st_crs(4326))
    )
  }
  .empty_catchments_med <- function() {
    sf::st_sf(
      NHD.comid = character(),
      NHD.resolution = character(),
      NHD.catchmentareasqkm = numeric(),
      geometry = sf::st_sfc(crs = sf::st_crs(4326))
    )
  }
  .empty_geom <- function() {
    sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(4326)))
  }

  suppressMessages(suppressWarnings({
    # If data is already spatial, just make sure it is in the right CRS
    if (!is.null(.data) & inherits(.data, "sf")) {
      if (sf::st_crs(.data)$epsg != 4326) {
        geospatial_data <- .data |> sf::st_transform(4326)
      } else {
        geospatial_data <- .data
      }
    } else {
      # Otherwise transform into a spatial object then do the same thing:
      geospatial_data <- .data |>
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

      # Build per-site envelopes as sfc objects (WGS84)
      geoms <- sf::st_geometry(unique_sites)
      wqp_bboxes_sfc <- lapply(geoms, function(g) {
        sf::st_as_sfc(sf::st_bbox(g), crs = sf::st_crs(unique_sites))
      })

      # Also build numeric bbox vectors for fallback
      wqp_bboxes_num <- lapply(wqp_bboxes_sfc, function(sfc) {
        bb <- sf::st_bbox(sfc)
        c(
          as.numeric(bb["xmin"]),
          as.numeric(bb["ymin"]),
          as.numeric(bb["xmax"]),
          as.numeric(bb["ymax"])
        )
      })

      # Open the service and layers once
      nhd_hr <- arcgislayers::arc_open(nhd_plus_hr_url)
      nhd_hr_catchments <- arcgislayers::get_layer(nhd_hr, 10) # HR catchments
      nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3) # HR flowlines
      nhd_hr_waterbodies <- arcgislayers::get_layer(nhd_hr, 9) # HR waterbodies

      # Catchments by per-site bbox
      fill_USGS_catchments_stored <- vector(
        "list",
        length = length(wqp_bboxes_sfc)
      )
      for (i in seq_along(wqp_bboxes_sfc)) {
        res <- try(
          .arc_select_compat(
            lyr = nhd_hr_catchments,
            where = "1=1",
            fields = "*",
            filter_geom = wqp_bboxes_sfc[[i]][[1]],
            bbox = wqp_bboxes_num[[i]],
            crs = 4326,
            geometry = TRUE
          ),
          silent = TRUE
        )
        if (
          !inherits(res, "try-error") &&
            !is.null(res) &&
            inherits(res, "sf") &&
            nrow(res) > 0
        ) {
          res <- sf::st_make_valid(res)
          fill_USGS_catchments_stored[[i]] <- harmonize_sf(
            res,
            sf::st_crs(4326)
          )
        }
      }

      # Keep only non-null, sf elements and bind
      fill_USGS_catchments_stored <- fill_USGS_catchments_stored |>
        purrr::keep(~ inherits(., "sf") && nrow(.) > 0)

      if (length(fill_USGS_catchments_stored) > 0) {
        fill_USGS_catchments_stored <- suppressWarnings(do.call(
          rbind,
          fill_USGS_catchments_stored
        )) |>
          dplyr::distinct()

        # Standardize attribute names used downstream
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
      } else {
        fill_USGS_catchments_stored <- .empty_catchments_hr()
      }
    }))

    # Empty version of the df will be returned if no associated catchments
    if (nrow(fill_USGS_catchments_stored) == 0 && "catchments" %in% features) {
      message("No NHD HR features associated with your WQP observations.")
      fill_USGS_catchments_stored <- .empty_catchments_hr()
    }

    if (nrow(fill_USGS_catchments_stored) == 0 && !"catchments" %in% features) {
      stop("No NHD HR features associated with your WQP observations.")
    }

    if (length(features) == 1 && features == "catchments") {
      return(fill_USGS_catchments_stored)
    }

    # Grab flowlines
    if ("flowlines" %in% features && nrow(fill_USGS_catchments_stored) > 0) {
      suppressMessages(suppressWarnings({
        geospatial_aoi <- fill_USGS_catchments_stored |> sf::st_as_sfc()

        nhd_flowlines_stored <- vector("list", length = length(geospatial_aoi))
        for (i in seq_along(geospatial_aoi)) {
          res <- try(
            .arc_select_compat(
              lyr = nhd_hr_flowlines,
              where = "1=1",
              fields = "*",
              filter_geom = geospatial_aoi[i],
              # Fallback bbox
              bbox = {
                bb <- sf::st_bbox(geospatial_aoi[i])
                c(
                  as.numeric(bb["xmin"]),
                  as.numeric(bb["ymin"]),
                  as.numeric(bb["xmax"]),
                  as.numeric(bb["ymax"])
                )
              },
              crs = 4326,
              geometry = TRUE
            ),
            silent = TRUE
          )

          if (
            !inherits(res, "try-error") &&
              !is.null(res) &&
              inherits(res, "sf") &&
              nrow(res) > 0
          ) {
            res <- sf::st_make_valid(res)
            res <- harmonize_sf(res, sf::st_crs(4326))

            # Convert non-geometry columns to character for robust binding
            geometry_name <- attr(res, "sf_column")
            cols_to_cast <- setdiff(names(res), geometry_name)
            if (length(cols_to_cast)) {
              res <- dplyr::mutate(
                res,
                dplyr::across(dplyr::all_of(cols_to_cast), ~ as.character(.))
              )
            }
            nhd_flowlines_stored[[i]] <- res
          }
        }

        nhd_flowlines_stored <- nhd_flowlines_stored |>
          purrr::keep(~ inherits(., "sf") && nrow(.) > 0)

        if (length(nhd_flowlines_stored) > 0) {
          nhd_flowlines_stored <- suppressWarnings(do.call(
            rbind,
            nhd_flowlines_stored
          )) |>
            dplyr::distinct()
        } else {
          nhd_flowlines_stored <- .empty_geom()
        }
      }))

      if (length(features) == 1 && features == "flowlines") {
        if (
          is.null(nrow(nhd_flowlines_stored)) || nrow(nhd_flowlines_stored) == 0
        ) {
          message(
            "There are no NHD flowlines associated with your WQP observations."
          )
        }
        return(nhd_flowlines_stored)
      }

      if (nrow(nhd_flowlines_stored) == 0) {
        message(
          "There are no NHD flowlines associated with your WQP observations."
        )
      }
    } else if ("flowlines" %in% features) {
      nhd_flowlines_stored <- .empty_geom()
      message(
        "There are no NHD flowlines associated with your WQP observations."
      )
    }

    # Grab waterbodies
    if ("waterbodies" %in% features & nrow(fill_USGS_catchments_stored) > 0) {
      suppressMessages(suppressWarnings({
        geospatial_aoi <- fill_USGS_catchments_stored |> sf::st_as_sfc()

        nhd_waterbodies_stored <- vector(
          "list",
          length = length(geospatial_aoi)
        )
        for (i in seq_along(geospatial_aoi)) {
          res <- try(
            .arc_select_compat(
              lyr = nhd_hr_waterbodies,
              where = "1=1",
              fields = "*",
              filter_geom = geospatial_aoi[i],
              # Fallback bbox
              bbox = {
                bb <- sf::st_bbox(geospatial_aoi[i])
                c(
                  as.numeric(bb["xmin"]),
                  as.numeric(bb["ymin"]),
                  as.numeric(bb["xmax"]),
                  as.numeric(bb["ymax"])
                )
              },
              crs = 4326,
              geometry = TRUE
            ),
            silent = TRUE
          )

          if (
            !inherits(res, "try-error") &&
              !is.null(res) &&
              inherits(res, "sf") &&
              nrow(res) > 0
          ) {
            res <- sf::st_make_valid(res)
            res <- harmonize_sf(res, sf::st_crs(4326))

            # Convert non-geometry cols to character for robust binding
            geometry_name <- attr(res, "sf_column")
            cols_to_cast <- setdiff(names(res), geometry_name)
            if (length(cols_to_cast)) {
              res <- dplyr::mutate(
                res,
                dplyr::across(dplyr::all_of(cols_to_cast), ~ as.character(.))
              )
            }
            nhd_waterbodies_stored[[i]] <- res
          }
        }

        nhd_waterbodies_stored <- nhd_waterbodies_stored |>
          purrr::keep(~ inherits(., "sf") && nrow(.) > 0)

        if (length(nhd_waterbodies_stored) > 0) {
          nhd_waterbodies_stored <- suppressWarnings(do.call(
            rbind,
            nhd_waterbodies_stored
          )) |>
            dplyr::distinct()
        } else {
          nhd_waterbodies_stored <- .empty_geom()
        }
      }))

      if (length(features) == 1 && features == "waterbodies") {
        if (
          is.null(nrow(nhd_waterbodies_stored)) ||
            nrow(nhd_waterbodies_stored) == 0
        ) {
          message(
            "There are no NHD waterbodies associated with your WQP observations."
          )
        }
        return(nhd_waterbodies_stored)
      }

      if (nrow(nhd_waterbodies_stored) == 0) {
        message(
          "There are no NHD waterbodies associated with your WQP observations."
        )
      }
    } else if ("waterbodies" %in% features) {
      nhd_waterbodies_stored <- .empty_geom()
      message(
        "There are no NHD waterbodies associated with your WQP observations."
      )
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
      return(nhd_list)
    } else if (
      !"catchments" %in% features &&
        !"flowlines" %in% features &&
        !"waterbodies" %in% features
    ) {
      stop(
        "Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `features`."
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
        if (
          inherits(fill_USGS_catchments, "sf") &&
            sf::st_crs(fill_USGS_catchments) != sf::st_crs(geospatial_data)
        ) {
          fill_USGS_catchments <- fill_USGS_catchments |>
            sf::st_transform(sf::st_crs(geospatial_data)$epsg)
        },
        silent = TRUE
      )

      if (
        is.null(fill_USGS_catchments) || !inherits(fill_USGS_catchments, "sf")
      ) {
        fill_USGS_catchments <- .empty_catchments_med()
      }
    }))

    if (nrow(fill_USGS_catchments) == 0 && "catchments" %in% features) {
      message("No NHDPlus V2 features associated with your WQP observations.")
      fill_USGS_catchments <- .empty_catchments_med()
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
          try(
            nhd_flowlines[[i]] <- nhdplusTools::get_nhdplus(
              AOI = unique_sites[i, ],
              realization = "flowline"
            ) |>
              sf::st_make_valid(),
            silent = TRUE
          )

          try(
            geometry_name <- attr(nhd_flowlines[[i]], "sf_column"),
            silent = TRUE
          )

          try(
            {
              cols_to_cast <- setdiff(names(nhd_flowlines[[i]]), geometry_name)
              if (length(cols_to_cast)) {
                nhd_flowlines[[i]] <- nhd_flowlines[[i]] |>
                  dplyr::mutate(dplyr::across(
                    dplyr::all_of(cols_to_cast),
                    ~ as.character(.)
                  ))
              }
            },
            silent = TRUE
          )
        }

        nhd_flowlines <- purrr::keep(
          nhd_flowlines,
          ~ inherits(., "sf") && nrow(.) > 0
        )

        nhd_flowlines <- tryCatch(
          dplyr::bind_rows(nhd_flowlines) |> dplyr::distinct(),
          error = function(e) .empty_geom()
        )

        try(
          if (
            inherits(nhd_flowlines, "sf") &&
              sf::st_crs(nhd_flowlines) != sf::st_crs(geospatial_data)
          ) {
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
    } else if ("flowlines" %in% features) {
      nhd_flowlines <- .empty_geom()
      message("No NHDPlus V2 flowlines associated with your WQP observations.")
    }

    # Grab waterbodies -
    if ("waterbodies" %in% features && nrow(fill_USGS_catchments) > 0) {
      suppressMessages(suppressWarnings({
        nhd_waterbodies <- vector("list", length = nrow(fill_USGS_catchments))

        unique_sites <- fill_USGS_catchments

        for (i in 1:nrow(unique_sites)) {
          try(
            nhd_waterbodies[[i]] <- nhdplusTools::get_waterbodies(
              AOI = unique_sites[i, ]
            ) |>
              sf::st_make_valid(),
            silent = TRUE
          )

          try(
            geometry_name <- attr(nhd_waterbodies[[i]], "sf_column"),
            silent = TRUE
          )

          try(
            {
              cols_to_cast <- setdiff(
                names(nhd_waterbodies[[i]]),
                geometry_name
              )
              if (length(cols_to_cast)) {
                nhd_waterbodies[[i]] <- nhd_waterbodies[[i]] |>
                  dplyr::mutate(dplyr::across(
                    dplyr::all_of(cols_to_cast),
                    ~ as.character(.)
                  ))
              }
            },
            silent = TRUE
          )
        }

        nhd_waterbodies <- purrr::keep(
          nhd_waterbodies,
          ~ inherits(., "sf") && nrow(.) > 0
        )

        nhd_waterbodies <- tryCatch(
          dplyr::bind_rows(nhd_waterbodies) |> dplyr::distinct(),
          error = function(e) .empty_geom()
        )

        try(
          if (
            inherits(nhd_waterbodies, "sf") &&
              sf::st_crs(nhd_waterbodies) != sf::st_crs(geospatial_data)
          ) {
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
    } else if ("waterbodies" %in% features) {
      nhd_waterbodies <- .empty_geom()
      message(
        "No NHDPlus V2 waterbodies associated with your WQP observations."
      )
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
      return(nhd_list)
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
#'
#' The ATTAINS snapshot of NHDPlus HR catchments is not available for areas
#' that do not have existing Assessment Units in ATTAINS. For these areas where
#' there are WQP sites, but no existing ATTAINS assessment units, a user can
#' choose to associate the WQP sites with NHDPlus catchments available from
#' the USGS nhdplusTools package (please be aware that USGS and EPA ATTAINS
#' snapshots of the NHDPlus catchments may vary) using the optional function
#' param 'fill_USGS_catch'.  If desired by the user, the HR
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
#' @param fill_USGS_catch Whether the user would like to return NHD catchments
#' (USGS snapshot of NHDPlus V2) for WQP observations not associated with an
#' ATTAINS assessment unit (TRUE or FALSE). When fill_USGS_catch = TRUE,
#' the returned list splits observations into two dataframes: WQP observations
#' with ATTAINS catchment data (EPA snapshot of NHDPlus V2), and WQP
#' observations without ATTAINS catchment data. Defaults to FALSE.
#' @param resolution If fill_USGS_catch = TRUE, whether to use NHDPlus V2 "Med"
#' catchments or NHDPlus V2 HiRes "Hi" catchments. Default is NHDPlus V2 HiRes
#' ("Hi") because at approximately 80% of state submitted assessment units in
#' ATTAINS were developed based on NHDPlus V2 HiRes.
#' @param return_sf Whether to return the ATTAINS associated catchments, lines,
#' points, and polygon shapefile objects along with the data frame(s).
#' TRUE (yes, return list) or FALSE (no, do not return). All shapefile features
#' are in WGS84 (crs = 4326). If fill_USGS_catch = TRUE and return_sf = TRUE,
#' the function will additionally return the raw catchment features associated
#' with the observations in TADA_without_ATTAINS in a new shapefile called
#' without_ATTAINS_catchments. Defaults to TRUE.
#'
#' @return A modified `TADA_DataRetrieval()` dataframe or list with additional
#' columns associated with the ATTAINS assessment unit data, and, if
#' fill_USGS_catch = TRUE, an additional dataframe of the observations without
#' intersecting ATTAINS features.
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
TADA_CreateATTAINSAUMLCrosswalk <- function(
  .data,
  org_id = "all",
  return_nearest = TRUE,
  fill_USGS_catch = FALSE,
  resolution = "Hi",
  return_sf = TRUE
) {
  # basic dependency checks
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required.")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }

  # Valid resolutions
  valid_resolutions <- c("Hi", "Med")
  if (!resolution %in% valid_resolutions) {
    stop("User-supplied resolution unavailable")
  }

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

  # Helper to canonicalize CRS to EPSG:4326 with identical WKT
  canonicalize_4326 <- function(s) {
    if (is.null(s)) {
      return(NULL)
    }
    if (!inherits(s, "sf")) {
      return(s)
    }
    s <- suppressWarnings(sf::st_transform(s, 4326))
    sf::st_set_crs(s, sf::st_crs(4326))
  }

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

  # Convert data to spatial format if not already and canonicalize CRS
  suppressMessages(suppressWarnings({
    if (!is.null(.data) && inherits(.data, "sf")) {
      if (is.na(sf::st_crs(.data)) || sf::st_crs(.data)$epsg != 4326) {
        TADA_DataRetrieval_data <- sf::st_transform(.data, 4326)
      } else {
        TADA_DataRetrieval_data <- .data
      }
      TADA_DataRetrieval_data <- sf::st_set_crs(
        TADA_DataRetrieval_data,
        sf::st_crs(4326)
      )
    } else {
      TADA_DataRetrieval_data <- TADA_MakeSpatial(.data = .data, crs = 4326) |>
        sf::st_make_valid()
      TADA_DataRetrieval_data <- sf::st_set_crs(
        TADA_DataRetrieval_data,
        sf::st_crs(4326)
      )
    }
  }))

  # Fetch ATTAINS features intersecting with WQP data
  attains_features <- try(
    fetchATTAINS(.data = TADA_DataRetrieval_data, org_id = org_id),
    silent = TRUE
  )

  # Canonicalize CRS for all sf elements returned by fetchATTAINS
  if (!inherits(attains_features, "try-error") && is.list(attains_features)) {
    for (nm in names(attains_features)) {
      attains_features[[nm]] <- canonicalize_4326(attains_features[[nm]])
    }
  }

  # Process intersecting catchment objects
  suppressMessages(suppressWarnings({
    nearby_catchments <- NULL
    try(
      {
        ac <- attains_features[["ATTAINS_catchments"]]
        if (!is.null(ac) && inherits(ac, "sf")) {
          ac <- sf::st_set_crs(ac, sf::st_crs(4326))
          nearby_catchments <- ac |>
            dplyr::select(-c(OBJECTID, GLOBALID)) |>
            (\(x) x[TADA_DataRetrieval_data, ])() |>
            dplyr::distinct(.keep_all = TRUE)
        }
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

    if (fill_USGS_catch == FALSE) {
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
      fill_USGS_catchments <- fetchNHD(
        .data = TADA_DataRetrieval_data,
        resolution = resolution
      )
      TADA_without_ATTAINS <- TADA_DataRetrieval_data |>
        sf::st_join(fill_USGS_catchments, left = TRUE)

      if (return_sf == TRUE) {
        return(list(
          "TADA_with_ATTAINS" = no_ATTAINS_data[0, ],
          "TADA_with_NHD" = TADA_without_ATTAINS,
          "ATTAINS_catchments" = NULL,
          "ATTAINS_points" = NULL,
          "ATTAINS_lines" = NULL,
          "ATTAINS_polygons" = NULL,
          "with_NHD_catchments" = fill_USGS_catchments
        ))
      } else {
        return(list(
          "TADA_with_ATTAINS" = no_ATTAINS_data[0, ],
          "TADA_with_NHD" = TADA_without_ATTAINS
        ))
      }
    }
  }

  # If ATTAINS data is present, link WQP features to ATTAINS catchments
  if (!is.null(nearby_catchments)) {
    suppressMessages({
      suppressWarnings({
        nearby_catchments <- sf::st_set_crs(nearby_catchments, sf::st_crs(4326))
        TADA_DataRetrieval_data <- sf::st_set_crs(
          TADA_DataRetrieval_data,
          sf::st_crs(4326)
        )
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

    # Function to calculate distances between WQP observations and ATTAINS features (use EPSG:3857 for meters)
    find_distances <- function(location) {
      sub_tada <- TADA_with_ATTAINS |>
        dplyr::filter(as.character(geometry) == location)

      distance_pt <- sub_tada[1, ]

      subset <- attains_features[-1] |>
        purrr::map(
          ~ tryCatch(
            dplyr::filter(
              .,
              assessmentunitidentifier %in% sub_tada$assessmentunitidentifier
            ),
            error = function(e) NULL,
            warning = function(w) NULL
          )
        ) |>
        purrr::keep(~ !is.null(.)) |>
        purrr::keep(~ inherits(., "sf") && nrow(.) > 0)

      if (length(subset) == 0) {
        return(NULL)
      }

      distance_pt_3857 <- suppressWarnings(sf::st_transform(distance_pt, 3857))

      distances <- subset |>
        purrr::map(
          ~ {
            feat_3857 <- suppressWarnings(sf::st_transform(., 3857))
            d <- suppressWarnings(sf::st_distance(feat_3857, distance_pt_3857))
            d_num <- as.numeric(d)
            dplyr::mutate(., TADA.DistanceAway.Meters = d_num)
          }
        ) |>
        dplyr::bind_rows() |>
        sf::st_drop_geometry() |>
        dplyr::select(assessmentunitidentifier, TADA.DistanceAway.Meters) |>
        dplyr::distinct()

      result <- NULL

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
            TADA.DistanceAway.Meters ==
              min(TADA.DistanceAway.Meters, na.rm = TRUE)
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
        dplyr::slice_min(TADA.DistanceAway.Meters, with_ties = FALSE) |>
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
      ATTAINS_catchments <- canonicalize_4326(ATTAINS_catchments)

      # Process point features
      ATTAINS_points <- NULL
      try(
        {
          ATTAINS_points <- attains_features[["ATTAINS_points"]] |>
            dplyr::filter(
              assessmentunitidentifier %in%
                TADA_with_ATTAINS$assessmentunitidentifier
            ) |>
            dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE)
          ATTAINS_points <- canonicalize_4326(ATTAINS_points)
        },
        silent = TRUE
      )
      if (
        is.null(ATTAINS_points) ||
          (inherits(ATTAINS_points, "sf") && nrow(ATTAINS_points) == 0)
      ) {
        ATTAINS_points <- NULL
      }

      # Process line features
      ATTAINS_lines <- NULL
      try(
        {
          ATTAINS_lines <- attains_features[["ATTAINS_lines"]] |>
            dplyr::filter(
              assessmentunitidentifier %in%
                TADA_with_ATTAINS$assessmentunitidentifier
            ) |>
            dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE)
          ATTAINS_lines <- canonicalize_4326(ATTAINS_lines)
        },
        silent = TRUE
      )
      if (
        is.null(ATTAINS_lines) ||
          (inherits(ATTAINS_lines, "sf") && nrow(ATTAINS_lines) == 0)
      ) {
        ATTAINS_lines <- NULL
      }

      # Process polygon features
      ATTAINS_polygons <- NULL
      try(
        {
          ATTAINS_polygons <- attains_features[["ATTAINS_polygons"]] |>
            dplyr::filter(
              assessmentunitidentifier %in%
                TADA_with_ATTAINS$assessmentunitidentifier
            ) |>
            dplyr::distinct(assessmentunitidentifier, .keep_all = TRUE)
          ATTAINS_polygons <- canonicalize_4326(ATTAINS_polygons)
        },
        silent = TRUE
      )
      if (
        is.null(ATTAINS_polygons) ||
          (inherits(ATTAINS_polygons, "sf") && nrow(ATTAINS_polygons) == 0)
      ) {
        ATTAINS_polygons <- NULL
      }

      if (fill_USGS_catch == FALSE) {
        final_list <- list(
          "TADA_with_ATTAINS" = TADA_with_ATTAINS |> renameATTAINSCols(),
          "ATTAINS_catchments" = ATTAINS_catchments,
          "ATTAINS_points" = ATTAINS_points,
          "ATTAINS_lines" = ATTAINS_lines,
          "ATTAINS_polygons" = ATTAINS_polygons
        )

        return(final_list)
      }

      if (fill_USGS_catch == TRUE) {
        TADA_without_ATTAINS <- TADA_DataRetrieval_data |>
          dplyr::filter(
            ResultIdentifier %in%
              c(
                dplyr::filter(
                  TADA_with_ATTAINS,
                  is.na(assessmentunitidentifier)
                ) |>
                  dplyr::pull(ResultIdentifier)
              )
          )

        fill_USGS_catchments <- fetchNHD(
          .data = TADA_without_ATTAINS,
          features = "catchments",
          resolution = resolution
        )

        TADA_without_ATTAINS <- TADA_without_ATTAINS |>
          sf::st_join(fill_USGS_catchments, left = TRUE) |>
          sf::st_drop_geometry()

        final_list <- list(
          "TADA_with_ATTAINS" = TADA_with_ATTAINS |>
            dplyr::filter(!is.na(assessmentunitidentifier)) |>
            renameATTAINSCols(),
          "TADA_with_NHD" = TADA_without_ATTAINS,
          "ATTAINS_catchments" = ATTAINS_catchments,
          "ATTAINS_points" = ATTAINS_points,
          "ATTAINS_lines" = ATTAINS_lines,
          "ATTAINS_polygons" = ATTAINS_polygons,
          "with_NHD_catchments" = fill_USGS_catchments
        )

        return(final_list)
      }
    } else {
      if (fill_USGS_catch == TRUE) {
        TADA_without_ATTAINS <- TADA_DataRetrieval_data |>
          dplyr::filter(
            ResultIdentifier %in%
              c(
                dplyr::filter(
                  TADA_with_ATTAINS,
                  is.na(assessmentunitidentifier)
                ) |>
                  dplyr::pull(ResultIdentifier)
              )
          )

        fill_USGS_catchments <- fetchNHD(
          .data = TADA_without_ATTAINS,
          features = "catchments",
          resolution = resolution
        )

        TADA_without_ATTAINS <- TADA_without_ATTAINS |>
          sf::st_join(fill_USGS_catchments, left = TRUE) |>
          sf::st_drop_geometry()

        final_list <- list(
          "TADA_with_ATTAINS" = TADA_with_ATTAINS |>
            dplyr::filter(!is.na(assessmentunitidentifier)) |>
            renameATTAINSCols(),
          "TADA_with_NHD" = TADA_without_ATTAINS
        )

        return(final_list)
      } else {
        return(TADA_with_ATTAINS)
      }
    }
  }
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
#' @param return_sf Whether to return the ATTAINS associated catchments, lines,
#' points, and polygon shapefile objects along with the data frame(s).
#' TRUE (yes, return list) or FALSE (no, do not return). All shapefile features
#' are in WGS84 (crs = 4326).
#'
#' @return A modified `TADA_DataRetrieval()` dataframe or list with additional
#' columns associated with the ATTAINS assessment unit data, and, if
#' fill_USGS_catch = TRUE, an additional dataframe of the observations without
#' intersecting ATTAINS features.
#' Moreover, if return_sf = TRUE, this function will additionally return the
#' raw ATTAINS and catchment shapefile features associated with those
#' observations.
#'
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_CreateATTAINSAUMLCrosswalk()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- TADA_GetATTAINSByAUID(my_data, au_ref = my_au_ref)
#' result_with_catch <- TADA_GetATTAINSByAUID(my_data, au_ref = my_au_ref, fill_ATTAINS_catch = TRUE)
#' }
#'
TADA_GetATTAINSByAUID <- function(
  .data,
  au_ref = NULL,
  fill_ATTAINS_catch = FALSE,
  return_sf = TRUE
) {
  # dependency checks
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required.")
  }
  if (!requireNamespace("geojsonsf", quietly = TRUE)) {
    stop("Package 'geojsonsf' is required.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required.")
  }
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required.")
  }
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("Package 'plyr' is required.")
  }
  if (!requireNamespace("spsUtil", quietly = TRUE)) {
    stop("Package 'spsUtil' is required.")
  }
  if (!requireNamespace("rExpertQuery", quietly = TRUE)) {
    stop("Package 'rExpertQuery' is required.")
  }
  if (!requireNamespace("EPATADA", quietly = TRUE)) {
    stop("Package 'EPATADA' is required.")
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
    rm(attains_names)
    stop("Your data has already been joined with ATTAINS data.")
  }

  if (nrow(.data) == 0) {
    message(
      "Your dataframe has no observations. Returning an empty dataframe with empty ATTAINS features."
    )

    col_val_list <- stats::setNames(
      object = rep(x = list(NA), times = length(attains_names)),
      nm = attains_names
    )

    no_WQP_data <- .data |>
      dplyr::mutate(ResultIdentifier = NA) |>
      dplyr::bind_cols(col_val_list) |>
      EPATADA::TADA_CorrectColType() |>
      dplyr::select(ResultIdentifier, dplyr::everything())

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
    } else {
      return(no_WQP_data)
    }
  }

  req.cols <- c(
    "AssessmentUnitIdentifier",
    "MonitoringLocationIdentifier",
    "WaterType"
  )

  # get column names by using internal function checkColName (in Utilities.R)
  col.ids <- purrr::map_dfr(req.cols, ~ checkColName(au_ref, .x))

  # assign values to col.id variables
  assign(col.ids$col.id[1], col.ids$select.col[1]) # auid.col
  assign(col.ids$col.id[2], col.ids$select.col[2]) # ml.col
  assign(col.ids$col.id[3], col.ids$select.col[3]) # type.col

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

  # filter data to retain only results with known AUIDs
  filt.data <- .data |>
    dplyr::filter(
      TADA.MonitoringLocationIdentifier %in%
        au_ref$TADA.MonitoringLocationIdentifier
    ) |>
    dplyr::left_join(
      au_ref,
      by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
    )

  # check if any rows in the filtered TADA df match MonitoringLocationIdentifiers in the user ref
  if (nrow(filt.data) < 1) {
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
    chunks <- split(au_list, ceiling(seq_along(unique(au_list)) / 20))

    wat_type <- function(chunk) {
      spsUtil::quiet(rExpertQuery::EQ_AssessmentUnits(
        api_key = "lfzVzpwIlKS1O4l1QmbOLUeTzxyql4QdbHVR5Yf5",
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

  # function to download ATTAINS features API based on their assessment unit id (GeoJSON via REST)
  fetch_au <- function(baseurls, assessment_unit_ids, chunk_n = 1000) {
    if (length(assessment_unit_ids) == 0) {
      return(NULL)
    }

    id_chunks <- split(
      assessment_unit_ids,
      ceiling(seq_along(assessment_unit_ids) / chunk_n)
    )

    fetch_chunk <- function(id_chunk) {
      where_clause <- paste0(
        "assessmentunitidentifier IN ('",
        paste(id_chunk, collapse = "','"),
        "')"
      )
      query_params <- list(where = where_clause, outFields = "*", f = "geojson")

      resp <- httr::GET(baseurls, query = query_params)
      if (httr::status_code(resp) != 200) {
        stop("Failed to retrieve data from EPA ATTAINS API (", baseurls, ").")
      }

      geojson_text <- httr::content(resp, as = "text", encoding = "UTF-8")

      # Read GeoJSON text into sf; enforce canonical EPSG:4326
      sf_obj <- tryCatch(
        geojsonsf::geojson_sf(geojson_text),
        error = function(e) {
          stop("Failed to parse GeoJSON from ATTAINS: ", conditionMessage(e))
        }
      )

      if (!inherits(sf_obj, "sf")) {
        return(NULL)
      }
      sf_obj <- suppressWarnings(sf::st_transform(sf_obj, 4326))
      sf_obj <- sf::st_set_crs(sf_obj, sf::st_crs(4326))
      return(sf_obj)
    }

    # bind all chunks robustly (sf rbind preserves geometry)
    chunks <- purrr::map(id_chunks, fetch_chunk)
    chunks <- purrr::keep(chunks, ~ inherits(., "sf") && nrow(.) > 0)
    if (length(chunks) == 0) {
      return(NULL)
    }

    out <- suppressWarnings(do.call(rbind, chunks))
    if (nrow(out) == 0) {
      return(NULL)
    }

    out <- dplyr::distinct(out)
    return(out)
  }

  # start grabbing the raw ATTAINS features
  points <- NULL
  lines <- NULL
  polygons <- NULL
  catchments <- NULL

  # Download associated point, line, polygon features using list of auids
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

  # Join water types and enforce canonical CRS on outputs
  if (!is.null(points) && inherits(points, "sf") && nrow(points) > 0) {
    points <- dplyr::left_join(
      points,
      water_types,
      by = c("assessmentunitidentifier" = "assessmentUnitId")
    )
    points <- sf::st_set_crs(
      suppressWarnings(sf::st_transform(points, 4326)),
      sf::st_crs(4326)
    )
  }
  if (!is.null(lines) && inherits(lines, "sf") && nrow(lines) > 0) {
    lines <- dplyr::left_join(
      lines,
      water_types,
      by = c("assessmentunitidentifier" = "assessmentUnitId")
    )
    lines <- sf::st_set_crs(
      suppressWarnings(sf::st_transform(lines, 4326)),
      sf::st_crs(4326)
    )
  }
  if (!is.null(polygons) && inherits(polygons, "sf") && nrow(polygons) > 0) {
    polygons <- dplyr::left_join(
      polygons,
      water_types,
      by = c("assessmentunitidentifier" = "assessmentUnitId")
    )
    polygons <- sf::st_set_crs(
      suppressWarnings(sf::st_transform(polygons, 4326)),
      sf::st_crs(4326)
    )
  }

  if (fill_ATTAINS_catch == FALSE) {
    catchments <- NULL
  }

  # create TADA_with_ATTAINS df for list output
  TADA_with_ATTAINS <- filt.data

  # create list of tada prefix columns
  tada.cols <- colnames(TADA_with_ATTAINS)

  # create list of attains prefix cols
  attains.cols <- renameATTAINSCols(return_list = TRUE, format = "attains")

  # create a combined list of tada and attains prefix cols
  comb.cols <- append(tada.cols, attains.cols) |> unique()

  attains.geo <- data.frame(matrix(nrow = 1, ncol = length(comb.cols)))
  colnames(attains.geo) <- comb.cols

  attains.geo <- attains.geo |>
    dplyr::select(-assessmentunitidentifier) |>
    EPATADA::TADA_CorrectColType()

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

    # enforce canonical CRS on catchments too
    if (
      !is.null(catchments) && inherits(catchments, "sf") && nrow(catchments) > 0
    ) {
      catchments <- sf::st_set_crs(
        suppressWarnings(sf::st_transform(catchments, 4326)),
        sf::st_crs(4326)
      )
    }

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

    if (
      !is.null(catchments.filt) &&
        inherits(catchments.filt, "sf") &&
        nrow(catchments.filt) > 0
    ) {
      catchments <- dplyr::left_join(
        catchments.filt,
        water_types,
        by = c("assessmentunitidentifier" = "assessmentUnitId")
      )
      catchments <- sf::st_set_crs(
        suppressWarnings(sf::st_transform(catchments, 4326)),
        sf::st_crs(4326)
      )
    }
  }

  # internal function to combine attains.geo data
  combineATTAINSGeo <- function(.data, geo.data, attains.geo) {
    if (is.null(geo.data) || !is.data.frame(geo.data) || nrow(geo.data) == 0) {
      return(attains.geo)
    }

    geo.data <- geo.data |>
      dplyr::rename(ATTAINS.AssessmentUnitIdentifier = assessmentunitidentifier)

    df <- .data |>
      dplyr::left_join(geo.data, by = c("ATTAINS.AssessmentUnitIdentifier"))

    attains.geo <- plyr::rbind.fill(attains.geo, df)
    if ("GLOBALID" %in% names(attains.geo)) {
      attains.geo <- attains.geo |> dplyr::filter(!is.na(GLOBALID))
    }

    return(attains.geo)
  }

  # add attains data returned from lines/points/polygons if any exists
  if (!is.null(lines) && inherits(lines, "sf") && nrow(lines) > 0) {
    attains.geo <- combineATTAINSGeo(
      .data = TADA_with_ATTAINS,
      geo.data = lines,
      attains.geo = attains.geo
    )
  }

  if (!is.null(points) && inherits(points, "sf") && nrow(points) > 0) {
    attains.geo <- combineATTAINSGeo(
      .data = TADA_with_ATTAINS,
      geo.data = points,
      attains.geo = attains.geo
    )
  }

  if (!is.null(polygons) && inherits(polygons, "sf") && nrow(polygons) > 0) {
    attains.geo <- combineATTAINSGeo(
      .data = TADA_with_ATTAINS,
      geo.data = polygons,
      attains.geo = attains.geo
    )
  }

  # remame cols and set up TADA_with_ATTAINS df
  TADA_with_ATTAINS <- attains.geo |>
    EPATADA::TADA_CorrectColType() |>
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

  if (nrow(mismatch_check) == 0) {
    rm(mismatch_check)
  }

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

  TADA_with_ATTAINS <- TADA_with_ATTAINS |>
    dplyr::select(-Ref.WaterType) |>
    dplyr::distinct()

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

#' Identify and group nearby monitoring locations
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
#' @note Implementation safeguards:
#' - Ensures sf objects are used for spatial operations (no st_join with non-sf)
#' - Avoids one-to-many spatial joins by assigning at most one catchment per site (st_within + first match)
#' - Ungroups before select/distinct that would drop grouping variables to keep the console clean
#' - Fixes minor issues: grouped.sites reference, default OrgRank used when missing
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
  rm(expected_cols)

  # retain only necessary columns unique Monitoring Locations
  unique.mls <- .data |>
    dplyr::select(
      "TADA.MonitoringLocationIdentifier",
      "TADA.LongitudeMeasure",
      "TADA.LatitudeMeasure",
      "HorizontalCoordinateReferenceSystemDatumName"
    ) |>
    dplyr::distinct()

  # convert to sf object if not already spatial (work on unique.mls)
  if (!inherits(unique.mls, "sf")) {
    unique.mls <- try(TADA_MakeSpatial(unique.mls), silent = TRUE)
    if (inherits(unique.mls, "try-error") || !inherits(unique.mls, "sf")) {
      stop(
        "TADA_FindNearbySites: Unable to convert monitoring locations to sf. Check coordinate columns/CRS."
      )
    }
  }

  # create a distance matrix in meters
  dist.matrix <- as.matrix(sf::st_distance(unique.mls)) # Great Circle distance since in lat/lon
  dist.matrix <- units::drop_units(dist.matrix)
  rownames(dist.matrix) <- unique.mls$TADA.MonitoringLocationIdentifier
  colnames(dist.matrix) <- unique.mls$TADA.MonitoringLocationIdentifier

  # convert distances to those within buffer (1) and beyond buffer (0)
  dist.matrix <- apply(dist.matrix, c(1, 2), function(x) {
    if (x <= dist_buffer) 1 else 0
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
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n) |>
    dplyr::ungroup()

  # remove intermediate objects
  rm(dist.matrix, adj.graph, comp.results)

  # add flag column, stop function, and print message if no nearby sites found
  if (nrow(group.sites) == 0) {
    message(
      "TADA_FindNearbySites: No nearby sites detected. Columns for TADA.NearbySites.Flag and TADA.NearbySiteGroup added for tracking purposes."
    )

    .data <- .data |>
      dplyr::mutate(
        TADA.NearbySites.Flag = "No nearby sites detected.",
        TADA.NearbySiteGroup = NA
      )

    return(.data)
  }

  # if catchment should be factored into site groupings
  if (isTRUE(catchment)) {
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
    nhd.catch <- near.dfs |> purrr::map(~ fetchNHD(.x, resolution = nhd_res))
    rm(near.dfs)

    # remove any fetchNHD dfs that do not contain any data (to prevent bind rows error)
    nhd.catch.filt <- purrr::keep(nhd.catch, ~ inherits(., "sf") && nrow(.) > 0)

    if (length(nhd.catch.filt) > 0) {
      # Harmonize CRS across list elements
      target_crs <- sf::st_crs(nhd.catch.filt[[1]])

      nhd.catch.filt <- lapply(nhd.catch.filt, function(x) {
        if (!inherits(x, "sf")) {
          return(x)
        }
        if (is.na(sf::st_crs(x))) {
          x <- sf::st_set_crs(x, target_crs)
        }
        if (sf::st_crs(x) != target_crs) {
          x <- suppressWarnings(sf::st_transform(x, target_crs))
        }
        x
      })

      # bind sf rows robustly (preserve geometry)
      nhd.catch.all <- suppressWarnings(do.call(rbind, nhd.catch.filt))
      if (!inherits(nhd.catch.all, "sf")) {
        stop(
          "TADA_FindNearbySites: NHD catchment data is not sf. Cannot perform spatial matching."
        )
      }

      # Avoid row multiplication: assign at most one catchment per site.
      # Use st_within; if multiple, take first; if none, NA.
      hits <- sf::st_within(near.sites, nhd.catch.all)
      first_hit <- vapply(
        hits,
        function(ix) if (length(ix)) ix[[1]] else NA_integer_,
        integer(1)
      )
      catch_attrs <- sf::st_drop_geometry(nhd.catch.all)[
        first_hit,
        ,
        drop = FALSE
      ]

      # bind attributes (drop geometry for grouping step)
      near.sites.with.catch <- dplyr::bind_cols(
        sf::st_drop_geometry(near.sites),
        catch_attrs
      )

      # group on catchment and filter to groups with >= 2 sites
      group.sites <- near.sites.with.catch |>
        dplyr::group_by(Group, NHD.nhdplusid) |>
        dplyr::filter(!is.na(NHD.nhdplusid)) |>
        dplyr::mutate(n = dplyr::n()) |>
        dplyr::filter(n > 1) |>
        dplyr::ungroup() |> # ungroup before select to avoid dplyr console messages
        dplyr::select(TADA.MonitoringLocationIdentifier, Group) |>
        dplyr::distinct()
    } else {
      message(
        "TADA_FindNearbySites: No NHD catchment data returned for nearby sites. Proceeding without catchment constraint."
      )
    }

    rm(near.sites, nhd.catch, nhd.catch.filt)
    if (exists("nhd.catch.all")) {
      rm(nhd.catch.all)
    }

    if (nrow(group.sites) == 0) {
      message(
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
    if (isTRUE(by_AU)) {
      message(
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
        dplyr::select(-Group.n) |>
        dplyr::ungroup()
    } else {
      message(
        "TADA_FindNearbySites: ATTAINS.AssessmentUnitIdentifier is present. User has specified that assessment unit should not be considered when grouping nearby sites."
      )
    }
  }

  # create df of all groups and create unique id for each group
  new.ids <- group.sites |>
    dplyr::ungroup() |>
    dplyr::group_by(Group) |>
    dplyr::mutate(
      TADA.MonitoringLocationIdentifier.New = paste0(
        "[",
        paste(TADA.MonitoringLocationIdentifier, collapse = ", "),
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

  rm(unique.mls)

  # create a df of unique grouped sites
  group.sites <- new.ids |>
    dplyr::full_join(
      .data,
      by = dplyr::join_by(TADA.MonitoringLocationIdentifier)
    ) |>
    dplyr::select(
      TADA.MonitoringLocationName,
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
    org.string <- "Metadata were selected by "
    message(
      "TADA_FindNearbySites: No org_hierarchy supplied by user. Organization will not be taken into account during metadata selection."
    )
    org.ranks <- as.data.frame(all.orgs) |>
      dplyr::mutate(OrgRank = 99L) |>
      dplyr::rename(OrganizationIdentifier = all.orgs)
  }

  # if org hierarchy is supplied by user
  if (org_hierarchy[1] != "none") {
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
      message(paste0(
        "TADA_FindNearbySites: ",
        length(missing.orgs),
        " organization identifiers are missing from org_hierarchy (",
        stringi::stri_replace_last(
          paste(missing.orgs, collapse = ", "),
          fixed = ", ",
          " and "
        ),
        "). Function will continue to run using partial org_hierarchy."
      ))

      org.ranks <- as.data.frame(org_hierarchy) |>
        dplyr::mutate(OrgRank = dplyr::row_number()) |>
        dplyr::rename(OrganizationIdentifier = org_hierarchy)

      missing.ranks <- data.frame(
        OrganizationIdentifier = missing.orgs,
        OrgRank = length(org_hierarchy) + 1L,
        stringsAsFactors = FALSE
      )

      org.ranks <- org.ranks |> dplyr::bind_rows(missing.ranks)
    } else {
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
    dplyr::group_by(TADA.NearbySiteGroup, OrgRank) |>
    dplyr::mutate(CountSites = dplyr::n()) |>
    dplyr::filter(CountSites == 1) |>
    dplyr::ungroup() |> # ungroup before dropping grouping vars
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
    random.meta <- org.ranks.added |>
      dplyr::ungroup() |>
      dplyr::filter(
        !TADA.NearbySiteGroup %in% org.meta.filter$TADA.NearbySiteGroup
      ) |>
      dplyr::group_by(TADA.NearbySiteGroup) |>
      dplyr::slice_min(OrgRank, with_ties = TRUE) |>
      dplyr::select(
        TADA.MonitoringLocationIdentifier.New,
        TADA.MonitoringLocationName,
        TADA.LatitudeMeasure,
        TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName,
        TADA.NearbySiteGroup,
        OrganizationIdentifier,
        ActivityStartDate
      ) |>
      dplyr::distinct() |>
      dplyr::slice_sample(n = 1) |>
      dplyr::ungroup()

    select.meta <- random.meta |>
      dplyr::full_join(org.meta.filter, by = names(random.meta)) |>
      dplyr::select(-OrganizationIdentifier, -ActivityStartDate) |>
      dplyr::rename(
        TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
        TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
        TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName
      ) |>
      dplyr::mutate(
        TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected randomly."
      )

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
          OrganizationIdentifier,
          ActivityStartDate
        )
      ) |>
      dplyr::filter(
        !TADA.NearbySiteGroup %in% org.meta.filter$TADA.NearbySiteGroup
      ) |>
      dplyr::mutate(OrgRank = ifelse(is.na(OrgRank), 99L, OrgRank)) |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier.New)

    if (meta_select == "oldest") {
      date.meta <- date.meta |>
        dplyr::slice_min(ActivityStartDate, with_ties = TRUE)
      date.choice <- "oldest"
    }

    if (meta_select == "newest") {
      date.meta <- date.meta |>
        dplyr::slice_max(ActivityStartDate, with_ties = TRUE)
      date.choice <- "newest"
    }

    select.meta <- date.meta |>
      dplyr::full_join(
        org.meta.filter,
        by = dplyr::join_by(
          TADA.MonitoringLocationIdentifier.New,
          TADA.NearbySiteGroup,
          TADA.MonitoringLocationName,
          TADA.LatitudeMeasure,
          TADA.LongitudeMeasure,
          TADA.MonitoringLocationTypeName
        )
      ) |>
      dplyr::select(-OrganizationIdentifier, -OrgRank, -ActivityStartDate) |>
      dplyr::group_by(TADA.NearbySiteGroup) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::ungroup() |>
      dplyr::rename(
        TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
        TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
        TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
        TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName
      ) |>
      dplyr::mutate(
        TADA.NearbySites.Flag = paste0(
          "This monitoring location was grouped with other nearby site(s). Metadata were selected from the ",
          date.choice,
          " result available."
        )
      )

    rm(date.meta)
  }

  if (meta_select == "count") {
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
      dplyr::mutate(NCount = dplyr::n()) |>
      dplyr::ungroup() |> # ungroup before dropping grouping var
      dplyr::select(-TADA.MonitoringLocationIdentifier) |>
      dplyr::distinct() |>
      dplyr::group_by(TADA.NearbySiteGroup) |>
      dplyr::slice_max(NCount, with_ties = TRUE) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::ungroup() |>
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
        TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName
      ) |>
      dplyr::mutate(
        TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected from MonitoringLocation with the most results available across all characteristics."
      )
  }

  rm(org.meta.filter, org.string, meta.string)

  # remove site group from crosswalk
  ml.crosswalk <- new.ids |>
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
      )
    ) |>
    dplyr::select(
      -TADA.MonitoringLocationIdentifier.New,
      -TADA.MonitoringLocationName.New,
      -TADA.LatitudeMeasure.New,
      -TADA.LongitudeMeasure.New,
      -TADA.MonitoringLocationTypeName.New
    ) |>
    TADA_OrderCols()

  rm(select.meta, ml.crosswalk, group.sites, new.ids)

  # add flag for any ungrouped sites and order columns correctly
  .data <- TADA_OrderCols(.data) |>
    dplyr::mutate(
      TADA.NearbySites.Flag = ifelse(
        is.na(TADA.NearbySiteGroup),
        "No nearby sites detected using input buffer distance.",
        TADA.NearbySites.Flag
      )
    )

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
#' @param fill_USGS_catch Boolean argument. Whether the user would like to return
#' NHD catchments (USGS snapshot of NHDPlus V2) for WQP observations not associated
#' with an ATTAINS assessment unit (TRUE or FALSE). When fill_USGS_catch = TRUE,
#' the returned list splits observations into two dataframes: WQP observations
#' with ATTAINS catchment data (EPA snapshot of NHDPlus V2), and WQP observations without ATTAINS catchment data. Defaults to FALSE. This param
#' applies only to WQP observations that do not have matches in the user-supplied ref
#' or ATTAINS.
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
#'
#' @return A list containing a modified TADA data frame with added ATTAINS columns and
#' data frames for ATTAINS data and features for points, lines, polygons and catchments.
#' When batch_upload = TRUE, the list will contain an additional data frame formatted
#' for compatibility with ATTAINS batch upload for Monitoring_Stations.
#'
#' @seealso [TADA_CreateATTAINSAUMLCrosswalk()]
#'          [TADA_GetATTAINSAUMLCrosswalk()]
#'          [TADA_UpdateATTAINSAUMLCrosswalk()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' utils::data(Data_MT_MissoulaCounty)
#' result <- TADA_CreateAUMLCrosswalk(Data_MT_MissoulaCounty)
#' }
#'
TADA_CreateAUMLCrosswalk <- function(
  .data,
  au_ref = NULL,
  org_id = "all",
  fill_ATTAINS_catch = FALSE,
  fill_USGS_catch = FALSE,
  return_nearest = TRUE,
  batch_upload = FALSE
) {
  # Required packages
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("purrr", quietly = TRUE))
  stopifnot(requireNamespace("stringi", quietly = TRUE))
  stopifnot(requireNamespace("spsUtil", quietly = TRUE))
  stopifnot(requireNamespace("httr", quietly = TRUE))

  # Local log helpers to prefix all messages consistently
  .log_info <- function(msg) message(paste0("TADA_CreateAUMLCrosswalk: ", msg))
  .log_warn <- function(msg) {
    warning(paste0("TADA_CreateAUMLCrosswalk: ", msg), call. = FALSE)
  }

  # Ensure .data is a data.frame/tibble (sf is also a data.frame)
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data.frame/tibble (sf is OK).")
  }

  # Increase timeout for remote calls unless user set higher already
  if (getOption("timeout") < 120) {
    options(timeout = 120)
  }

  # Initialize objects referenced later
  attains.cw <- NULL
  attains.cw.mls <- NULL
  au.ref.mls <- NULL

  # Default NULL org_id to "all" (unless explicitly "none")
  if (is.null(org_id)) {
    .log_info(
      "org_id is NULL; defaulting to 'all'. Set org_id = 'none' to skip ATTAINS."
    )
    org_id <- "all"
  }

  # Initialize containers for matches
  user.matches <- list(
    "TADA_with_ATTAINS" = NULL,
    "ATTAINS_catchments" = NULL,
    "ATTAINS_points" = NULL,
    "ATTAINS_lines" = NULL,
    "ATTAINS_polygons" = NULL
  )

  if (is.null(au_ref)) {
    .log_info("no au_ref (no user-supplied crosswalk was provided).")
  }

  if (!is.null(au_ref)) {
    if (!is.data.frame(au_ref)) {
      stop(paste0(
        "TADA_CreateAUMLCrosswalk: The user supplied au_ref must be a data frame ",
        "containing the columns AssessmentUnitIdentifier, MonitoringLocationIdentifier, and ATTAINS.WaterType.",
        "MonitoringLocationIdentifiers must be WQP compatible."
      ))
    }

    .log_info(
      "fetching ATTAINS geospatial data for assessment units in the user-supplied crosswalk."
    )

    req.cols <- c(
      "AssessmentUnitIdentifier",
      "MonitoringLocationIdentifier",
      "WaterType"
    )
    col.ids <- purrr::map_dfr(req.cols, ~ checkColName(au_ref, .x))

    assign(col.ids$col.id[1], col.ids$select.col[1]) # auid.col
    assign(col.ids$col.id[2], col.ids$select.col[2]) # ml.col
    assign(col.ids$col.id[3], col.ids$select.col[3]) # type.col

    au_ref <- au_ref |>
      dplyr::rename(
        ATTAINS.MonitoringLocationIdentifier = paste0(ml.col),
        ATTAINS.AssessmentUnitIdentifier = paste0(auid.col),
        User.WaterType = paste0(type.col)
      )

    rm(col.ids, req.cols, auid.col, ml.col, type.col)

    au.ref.mls <- .data |>
      dplyr::filter(
        TADA.MonitoringLocationIdentifier %in%
          au_ref$ATTAINS.MonitoringLocationIdentifier
      ) |>
      dplyr::mutate(TADA.AURefSource = "User-supplied Ref")

    if (nrow(au.ref.mls) > 0) {
      t0 <- Sys.time()
      .log_info(paste0(
        "Starting ATTAINS geospatial fetch for user-supplied AUs at ",
        t0,
        if (isTRUE(fill_ATTAINS_catch)) {
          " (fill_ATTAINS_catch=TRUE may increase runtime)"
        } else {
          ""
        }
      ))

      user.matches <- suppressWarnings(tryCatch(
        spsUtil::quiet(TADA_GetATTAINSByAUID(
          au.ref.mls,
          au_ref = au_ref,
          fill_ATTAINS_catch = fill_ATTAINS_catch
        )),
        error = function(e) {
          .log_warn(paste0(
            "TADA_GetATTAINSByAUID (user ref) failed: ",
            conditionMessage(e)
          ))
          list(
            "TADA_with_ATTAINS" = NULL,
            "ATTAINS_catchments" = NULL,
            "ATTAINS_points" = NULL,
            "ATTAINS_lines" = NULL,
            "ATTAINS_polygons" = NULL
          )
        }
      ))

      t1 <- Sys.time()
      .log_info(paste0(
        "Finished ATTAINS geospatial fetch for user-supplied AUs at ",
        t1,
        " (elapsed ",
        round(as.numeric(difftime(t1, t0, units = "secs")), 1),
        " s)"
      ))

      user.aus <- au_ref |>
        dplyr::select(
          ATTAINS.MonitoringLocationIdentifier,
          ATTAINS.AssessmentUnitIdentifier
        ) |>
        dplyr::rename(
          TADA.MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier,
          UserRef.AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier
        )

      if (!is.null(user.matches$TADA_with_ATTAINS)) {
        user.matches$TADA_with_ATTAINS <- user.matches$TADA_with_ATTAINS |>
          dplyr::left_join(
            user.aus,
            by = "TADA.MonitoringLocationIdentifier"
          ) |>
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
      }

      rm(user.aus)
    }
  }

  # ATTAINS crosswalk fetch (skips if org_id == "none")
  attains.matches <- list(
    "TADA_with_ATTAINS" = NULL,
    "ATTAINS_catchments" = NULL,
    "ATTAINS_points" = NULL,
    "ATTAINS_lines" = NULL,
    "ATTAINS_polygons" = NULL
  )

  if (!is.null(org_id)) {
    if (any(org_id == "none") | is.null(org_id)) {
      .log_info(
        "User has specified that ATTAINS should not be checked for monitoring location and assessment unit matches."
      )
    }
  }

  if (!is.null(org_id) && !any(org_id == "none")) {
    .log_info("checking for crosswalk in ATTAINS.")
    t0 <- Sys.time()
    .log_info(paste0("Starting ATTAINS crosswalk query at ", t0))

    attains.cw <- tryCatch(
      spsUtil::quiet(TADA_GetATTAINSAUMLCrosswalk(org_id = org_id)),
      error = function(e) {
        .log_warn(paste0(
          "TADA_GetATTAINSAUMLCrosswalk failed: ",
          conditionMessage(e)
        ))
        NULL
      }
    )

    t1 <- Sys.time()
    .log_info(paste0(
      "Finished ATTAINS crosswalk query at ",
      t1,
      " (elapsed ",
      round(as.numeric(difftime(t1, t0, units = "secs")), 1),
      " s)"
    ))

    org.text <- ifelse(
      is.null(org_id),
      "all organizations",
      stringi::stri_replace_last(
        paste(org_id, collapse = ", "),
        fixed = ", ",
        replacement = " and "
      )
    )

    record.count <- if (!is.null(attains.cw)) nrow(attains.cw) else 0
    count.text <- ifelse(record.count == 0, "no", record.count)

    .log_info(paste0(
      "There are ",
      count.text,
      " MonitoringLocation records in ATTAINS for ",
      org.text,
      "."
    ))

    rm(org.text, record.count, count.text)
  }

  if (!is.null(attains.cw) && nrow(attains.cw) > 0) {
    .log_info("crosswalk from ATTAINS has been imported.")

    TADA_UpdateATTAINSAUMLCrosswalk(
      org_id = org_id,
      crosswalk = attains.cw,
      attains_replace = TRUE
    )

    tada.mls <- .data |>
      dplyr::select(TADA.MonitoringLocationIdentifier) |>
      dplyr::distinct() |>
      dplyr::pull()

    attains.cw <- attains.cw |>
      dplyr::filter(ATTAINS.MonitoringLocationIdentifier %in% tada.mls)

    rm(tada.mls)

    if (!is.null(au_ref) && !is.null(au.ref.mls)) {
      attains.cw.mls <- .data |>
        dplyr::filter(
          !TADA.MonitoringLocationIdentifier %in%
            au.ref.mls$TADA.MonitoringLocationIdentifier,
          TADA.MonitoringLocationIdentifier %in%
            attains.cw$ATTAINS.MonitoringLocationIdentifier
        )
    }

    if (is.null(au_ref)) {
      attains.cw.mls <- .data |>
        dplyr::filter(
          TADA.MonitoringLocationIdentifier %in%
            attains.cw$ATTAINS.MonitoringLocationIdentifier
        )
    }

    if (is.null(attains.cw.mls) || nrow(attains.cw.mls) == 0) {
      attains.matches <- list(
        "TADA_with_ATTAINS" = NULL,
        "ATTAINS_catchments" = NULL,
        "ATTAINS_points" = NULL,
        "ATTAINS_lines" = NULL,
        "ATTAINS_polygons" = NULL
      )
    } else {
      attains.cw.mls <- attains.cw.mls |>
        dplyr::mutate(TADA.AURefSource = "ATTAINS Crosswalk")

      .log_info(
        "fetching ATTAINS geospatial data for assessment units from the ATTAINS crosswalk."
      )
      t0 <- Sys.time()
      .log_info(paste0(
        "Starting ATTAINS geospatial fetch for ATTAINS crosswalk AUs at ",
        t0,
        if (isTRUE(fill_ATTAINS_catch)) {
          " (fill_ATTAINS_catch=TRUE may increase runtime)"
        } else {
          ""
        }
      ))

      attains.matches <- suppressWarnings(tryCatch(
        spsUtil::quiet(TADA_GetATTAINSByAUID(
          attains.cw.mls,
          au_ref = attains.cw,
          fill_ATTAINS_catch = fill_ATTAINS_catch
        )),
        error = function(e) {
          .log_warn(paste0(
            "TADA_GetATTAINSByAUID (ATTAINS cw) failed: ",
            conditionMessage(e)
          ))
          list(
            "TADA_with_ATTAINS" = NULL,
            "ATTAINS_catchments" = NULL,
            "ATTAINS_points" = NULL,
            "ATTAINS_lines" = NULL,
            "ATTAINS_polygons" = NULL
          )
        }
      ))

      t1 <- Sys.time()
      .log_info(paste0(
        "Finished ATTAINS geospatial fetch for ATTAINS crosswalk AUs at ",
        t1,
        " (elapsed ",
        round(as.numeric(difftime(t1, t0, units = "secs")), 1),
        " s)"
      ))
    }

    rm(attains.cw)
  }

  .log_info(
    "checking to see if any unmatched monitoring locations remain in the original TADA data frame."
  )

  get.attains.mls <- .data

  if (!is.null(attains.matches$TADA_with_ATTAINS) && !is.null(attains.cw.mls)) {
    get.attains.mls <- get.attains.mls |>
      dplyr::filter(
        !TADA.MonitoringLocationIdentifier %in%
          attains.cw.mls$TADA.MonitoringLocationIdentifier
      )
  }

  if (!is.null(user.matches$TADA_with_ATTAINS) && !is.null(au.ref.mls)) {
    get.attains.mls <- get.attains.mls |>
      dplyr::filter(
        !TADA.MonitoringLocationIdentifier %in%
          au.ref.mls$TADA.MonitoringLocationIdentifier
      )

    rm(au.ref.mls)
  }

  if (nrow(get.attains.mls) == 0) {
    .log_info(
      "all monitoring locations have already been matched to an assessment unit by the user or ATTAINS."
    )

    get.attains.matches <- list(
      "TADA_with_ATTAINS" = NULL,
      "ATTAINS_catchments" = NULL,
      "ATTAINS_points" = NULL,
      "ATTAINS_lines" = NULL,
      "ATTAINS_polygons" = NULL
    )
  }

  if (nrow(get.attains.mls) > 0) {
    .log_info(
      "using TADA_CreateATTAINSAUMLCrosswalk to match remaining monitoring locations to ATTAINS assessment units via spatial join. Also returning USGS snapshot of NHDPlus V2 HR for monitoring locations not near any ATTAINS assessment unit."
    )

    get.attains.mls <- get.attains.mls |>
      dplyr::mutate(TADA.AURefSource = "TADA_CreateATTAINSAUMLCrosswalk")

    t0 <- Sys.time()
    .log_info(paste0(
      "Starting spatial matching via TADA_CreateATTAINSAUMLCrosswalk at ",
      t0
    ))

    get.attains.matches <- suppressWarnings(tryCatch(
      spsUtil::quiet(TADA_CreateATTAINSAUMLCrosswalk(
        get.attains.mls,
        return_nearest = return_nearest,
        fill_USGS_catch = fill_USGS_catch,
        return_sf = TRUE,
        org_id = org_id
      )),
      error = function(e) {
        .log_warn(paste0(
          "TADA_CreateATTAINSAUMLCrosswalk failed: ",
          conditionMessage(e)
        ))
        list(
          "TADA_with_ATTAINS" = NULL,
          "ATTAINS_catchments" = NULL,
          "ATTAINS_points" = NULL,
          "ATTAINS_lines" = NULL,
          "ATTAINS_polygons" = NULL
        )
      }
    ))

    t1 <- Sys.time()
    .log_info(paste0(
      "Finished spatial matching via TADA_CreateATTAINSAUMLCrosswalk at ",
      t1,
      " (elapsed ",
      round(as.numeric(difftime(t1, t0, units = "secs")), 1),
      " s)"
    ))
  }

  if (exists("attains.cw.mls")) {
    rm(attains.cw.mls)
  }
  if (exists("get.attains.mls")) {
    rm(get.attains.mls)
  }

  .log_info(
    "joining results to return list of dataframes compatible with TADA_ViewATTAINS."
  )

  # Robust output preparation: correct types, preserve geometry, harmonize CRS and column types
  outputPrep <- function(df.name, user, attains, get.attains) {
    # Safe type correction only for data.frame-like
    safe_correct <- function(x) {
      if (is.null(x)) {
        return(NULL)
      }
      if (!is.data.frame(x)) {
        return(NULL)
      }
      EPATADA::TADA_CorrectColType(x)
    }
    u <- safe_correct(user[[df.name]])
    a <- safe_correct(attains[[df.name]])
    g <- safe_correct(get.attains[[df.name]])

    # Helpers for sf handling
    make_valid_safe <- function(s) {
      if (is.null(s)) {
        return(NULL)
      }
      if (!inherits(s, "sf")) {
        return(s)
      }
      if (nrow(s) == 0) {
        return(s)
      }
      suppressWarnings(tryCatch(sf::st_make_valid(s), error = function(e) s))
    }
    make_points_sf <- function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      if (!is.data.frame(df)) {
        return(df)
      }
      if (
        !all(c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure") %in% names(df))
      ) {
        return(df)
      }
      df <- dplyr::filter(
        df,
        !is.na(.data$TADA.LongitudeMeasure),
        !is.na(.data$TADA.LatitudeMeasure)
      )
      if (nrow(df) == 0) {
        return(df)
      }
      obj <- suppressWarnings(tryCatch(
        sf::st_as_sf(
          df,
          coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
          crs = 4326
        ),
        error = function(e) df
      ))
      if (inherits(obj, "sf")) {
        obj <- sf::st_set_crs(obj, sf::st_crs(4326))
      }
      obj
    }

    # Build sf/non-sf objects
    if (df.name == "TADA_with_ATTAINS") {
      u <- make_points_sf(u)
      a <- make_points_sf(a)
      g <- make_points_sf(g)
    } else {
      u <- make_valid_safe(u)
      a <- make_valid_safe(a)
      g <- make_valid_safe(g)
    }

    # If everything is NULL, return NULL
    if (is.null(u) && is.null(a) && is.null(g)) {
      return(NULL)
    }

    # Ensure ATTAINS.*Use and logical ATTAINS.* are character (keep geometry intact)
    ensure_attains_use_char <- function(df) {
      if (is.null(df) || !is.data.frame(df)) {
        return(df)
      }
      use_cols <- grep("^ATTAINS\\..*Use$", names(df), value = TRUE)
      for (nm in use_cols) {
        if (nm %in% names(df) && !inherits(df[[nm]], "sfc")) {
          df[[nm]] <- as.character(df[[nm]])
        }
      }
      df
    }
    harmonize_attains_logicals <- function(df) {
      if (is.null(df) || !is.data.frame(df)) {
        return(df)
      }
      dplyr::mutate(
        df,
        dplyr::across(dplyr::everything(), function(x) {
          nm <- dplyr::cur_column()
          if (
            !inherits(x, "sfc") && grepl("^ATTAINS\\.", nm) && is.logical(x)
          ) {
            as.character(x)
          } else {
            x
          }
        })
      )
    }
    u <- ensure_attains_use_char(u)
    a <- ensure_attains_use_char(a)
    g <- ensure_attains_use_char(g)
    u <- harmonize_attains_logicals(u)
    a <- harmonize_attains_logicals(a)
    g <- harmonize_attains_logicals(g)

    # Canonical CRS alignment for all sf objects (force same WKT for 4326)
    target <- sf::st_crs(4326)
    align_crs <- function(s) {
      if (is.null(s) || !inherits(s, "sf")) {
        return(s)
      }
      cr <- sf::st_crs(s)
      if (is.na(cr)) {
        s <- sf::st_set_crs(s, target)
      } else if (isTRUE(cr$epsg == 4326)) {
        s <- sf::st_set_crs(s, target)
      } else {
        s <- suppressWarnings(sf::st_transform(s, 4326))
        s <- sf::st_set_crs(s, target)
      }
      s
    }
    u <- align_crs(u)
    a <- align_crs(a)
    g <- align_crs(g)

    # Drop empty sf to reduce binding complexity
    drop_empty_sf <- function(x) {
      if (inherits(x, "sf") && nrow(x) == 0) NULL else x
    }
    u <- drop_empty_sf(u)
    a <- drop_empty_sf(a)
    g <- drop_empty_sf(g)

    # Prepare list of non-NULL objects
    L <- Filter(Negate(is.null), list(u, a, g))
    if (length(L) == 0) {
      return(NULL)
    }

    # Bind robustly:
    # - If all are sf: use sf rbind (CRS already aligned)
    # - If mixed sf and data.frame: coerce df with 'geometry' into sf, else drop non-sf
    all_sf <- all(vapply(L, function(x) inherits(x, "sf"), logical(1)))
    any_sf <- any(vapply(L, function(x) inherits(x, "sf"), logical(1)))

    if (all_sf) {
      out <- suppressWarnings(do.call(rbind, L))
      out <- dplyr::distinct(out)
      return(out)
    }
    if (any_sf) {
      L <- lapply(L, function(x) {
        if (!inherits(x, "sf") && "geometry" %in% names(x)) {
          # Try to restore sf class from geometry list-column
          tryCatch(sf::st_as_sf(x), error = function(e) NULL)
        } else if (!inherits(x, "sf")) {
          NULL
        } else {
          x
        }
      })
      L <- Filter(Negate(is.null), L)
      if (length(L) == 0) {
        return(NULL)
      }
      out <- suppressWarnings(do.call(rbind, L))
      out <- dplyr::distinct(out)
      return(out)
    }

    # No sf present: plain bind_rows
    out <- dplyr::bind_rows(L)
    out <- dplyr::distinct(out)
    out
  }

  # Build outputs
  TADA_with_ATTAINS <- outputPrep(
    df.name = "TADA_with_ATTAINS",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  ATTAINS_catchments <- outputPrep(
    df.name = "ATTAINS_catchments",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  ATTAINS_lines <- outputPrep(
    df.name = "ATTAINS_lines",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  ATTAINS_points <- outputPrep(
    df.name = "ATTAINS_points",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  ATTAINS_polygons <- outputPrep(
    df.name = "ATTAINS_polygons",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )

  # Guard crosswalk creation if TADA_with_ATTAINS is NULL
  ATTAINS_crosswalk <- if (!is.null(TADA_with_ATTAINS)) {
    TADA_with_ATTAINS |>
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
  } else {
    dplyr::tibble(
      OrganizationIdentifier = character(),
      TADA.MonitoringLocationIdentifier = character(),
      ATTAINS.OrganizationIdentifier = character(),
      ATTAINS.AssessmentUnitIdentifier = character(),
      ATTAINS.WaterType = character(),
      TADA.AURefSource = character()
    )
  }

  final_list <- list(
    "TADA_with_ATTAINS" = TADA_with_ATTAINS,
    "ATTAINS_catchments" = ATTAINS_catchments,
    "ATTAINS_points" = ATTAINS_points,
    "ATTAINS_lines" = ATTAINS_lines,
    "ATTAINS_polygons" = ATTAINS_polygons,
    "ATTAINS_crosswalk" = ATTAINS_crosswalk
  )

  if (batch_upload == TRUE) {
    ATTAINS_batchupload <- if (!is.null(TADA_with_ATTAINS)) {
      TADA_with_ATTAINS |>
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
    } else {
      dplyr::tibble(
        ASSESSMENT_UNIT_ID = character(),
        MS_ORG_ID = character(),
        MS_LOCATION_ID = character(),
        MS_DATA_LINK = character()
      )
    }

    final_list <- c(
      final_list,
      list("ATTAINS_batchupload" = ATTAINS_batchupload)
    )
    rm(ATTAINS_batchupload)
  }

  rm(
    TADA_with_ATTAINS,
    ATTAINS_catchments,
    ATTAINS_points,
    ATTAINS_lines,
    ATTAINS_polygons,
    ATTAINS_crosswalk
  )

  if (fill_USGS_catch == TRUE) {
    if (
      exists("get.attains.matches") &&
        !is.null(get.attains.matches$with_NHD_catchments) &&
        !is.null(get.attains.matches$TADA_with_NHD)
    ) {
      final_list <- c(
        final_list,
        list("with_NHD_catchments" = get.attains.matches$with_NHD_catchments),
        list("TADA_with_NHD" = get.attains.matches$TADA_with_NHD)
      )
    } else {
      .log_info(
        "fill_USGS_catch = TRUE, but there are no USGS catchment outputs to append (no unmatched MLs or matching failed)."
      )
    }
  }

  rm(attains.matches, user.matches, get.attains.matches)
  return(final_list)
}
