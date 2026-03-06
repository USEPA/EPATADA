#' TADA_CreateNHDAUMLCrosswalk
#'
#' Link NHDPlus HR catchments (NHDPlus HR or MR catchments)  to Water Quality
#' Portal observations, often imported via `TADA_DataRetrieval()`. This
#' function returns the objects that can be mapped in `TADA_ViewNHD()` (currently
#' under development).
#'
#' Need to create an example vignette/workflow for this.
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
#' are strongly encouraged.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()` or the sf
#' equivalent made by `TADA_MakeSpatial()`.
#' @param return_sf Whether to return the associated NHD catchment geometry along
#' with the data frame(s). TRUE (yes, return list) or FALSE (no, do not return).
#' All shapefile features
#' are in WGS84 (crs = 4326). Defaults to TRUE.'
#' @param res Character argument to determine whether the NHD catchments
#' returned should be high ("Hi") or medium ("Med") res. Default = "Hi" for
#' consistency with other TADA geospatial functions.
#' @param features Which NHD features to return: "catchments", "flowlines",
#' "waterbodies", or any combination. Default is "catchments".
#'
#' @return A modified `TADA_DataRetrieval()` dataframe or list with additional
#' columns associated with NHD catchment data. Moreover, if return_sf = TRUE,
#' this function will additionally return the NHD catchment shapefile features
#'  associated with those observations.
#'
#' This function uses geospatial joins to determine which catchment each Water
#' Quality Portal observation falls within. If no catchment is found for an
#' observation, the NHD associated columns will all be filled with NAs.
#'
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_MakeSpatial()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' }
TADA_CreateNHDAUMLCrosswalk <- function(
    .data,
    return_sf = TRUE,
    res = "Hi",
    features = c("catchments", "waterbodies", "flowlines")
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

    # NEED TO UPDATE THIS FOR NHD OUTPUTS
    if (return_sf == TRUE) {

      nhd.list <- "add items here"
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

  # Fetch NHD catchments containing WQP data
  nhd.catch <- TADA_DataRetrieval_data |> fetchNHD(resolution = res,
                                                   features = features)



  # Handle scenario where no NHD data is associated with WQP observations
  if (is.data.frame(nhd.catch) & is.null(nhd.catch) |
      is.list(nhd.catch) & length(purrr::compact(nhd.catch)) == 0) {

    nhd.list <- c("fill_USGS_catchments",
                  "NHD_flowlines",
                  "NHD_waterbodies")

    nhd.cols <-

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


  nhd.catch2 <- TADA_DataRetrieval_data |> sf::st_join(nhd.catch, left = TRUE)



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
