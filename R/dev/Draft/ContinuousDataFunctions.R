#' TADA_listNWIS (DRAFT - UNDER ACTIVE DEVELOPMENT)
#'
#' List USGS continuous monitoring sites with available daily statistics
#'
#' @description
#' Retrieves available metadata from USGS National Water Information System (NWIS) based on
#' different spatial queries: area of interest (AOI), specific sites, or state boundaries.
#' Returns a spatial sf object containing continuous monitoring site information and
#' available parameters and statistics.
#' If no data is found, returns an empty sf object with appropriate column structure.
#'
#' @param aoi_sf An sf object defining the area of interest. All individual sf
#' features (or "rows" in the sf data frame) must be under 118,078 square miles
#' (roughly the area of Nevada).
#' @param statecode Character vector of two-letter state codes (e.g., c("CA", "OR")).
#' @param siteid Character vector of USGS site numbers.
#'
#' @return An sf object containing NWIS continuous monitoring site summary
#' information including:
#'   \itemize{
#'     \item site_no: USGS site identification number
#'     \item site_name: Station name
#'     \item site_type: Description of the site type
#'     \item site_type_cd: Site type code
#'     \item data_type: Type of data available (e.g., "Daily", "Water Quality")
#'     \item data_type_cd: Code identifying type of data/service (e.g. "dv" = Daily Values)
#'     \item stat_type: Statistic type
#'     \item stat_cd: Statistic code
#'     \item parameter: Parameter name and description
#'     \item parameter_code: Parameter code
#'     \item n_obs: Number of observations
#'     \item begin_date: Start date of data collection
#'     \item end_date: End date of data collection
#'     \item geometry

#'   }
#'   Returns an empty sf object with the same structure if no data is found.
#'
#' @details Only one of the query arguments (`aoi_sf`, `statecode`, or `siteid`)
#' should be provided. The function will stop if none or more than one are provided.
#' Moreover, all sf features must be under 118,078 square miles (roughly the area of Nevada).
#'
#' @examples
#' \dontrun{
#' # Example 1: Query by area of interest
#' navajo_sf <- sf::read_sf("inst/extdata/AmericanIndian.shp") |>
#'   dplyr::filter(NAME == "Navajo Nation")
#' sites_aoi_sf <- TADA_listNWIS(aoi_sf = navajo_sf)
#'
#' # Example 2: Query by specific site numbers
#' site_nums <- c("11530500", "11532500")
#' sites_specific <- TADA_listNWIS(siteid = site_nums)
#'
#' # Example 3: Query by state
#' sites_state <- TADA_listNWIS(statecode = c("CT", "RI"))
#' }
#'
TADA_listNWIS <- function(
  aoi_sf = "null",
  statecode = "null",
  siteid = "null"
) {
  # Confirm only a single argument has been provided
  if (
    !sum(purrr::map_lgl(
      list(aoi_sf, statecode[1], siteid[1]),
      ~ is.null(.x) || (is.character(.x) && .x == "null")
    )) %in%
      c(2, 3)
  ) {
    stop(paste0(
      "Multiple data-querying arguments (`aoi_sf`, `statecode`, `siteid`) have been provided. ",
      "Please use only one of these query options."
    ))
  } else if (
    sum(purrr::map_lgl(
      list(aoi_sf, statecode[1], siteid[1]),
      ~ is.null(.x) || (is.character(.x) && .x == "null")
    )) ==
      3
  ) {
    stop(paste0(
      "No data-querying argument (`aoi_sf`, `statecode`, `siteid`) has been provided. ",
      "Please select from one of these query options."
    ))
  }

  # Create empty sf object template with correct structure for "no return" data
  empty_sf <- function() {
    sf::st_sf(data.frame(
      site_no = character(),
      site_name = character(),
      site_type = character(),
      site_type_cd = character(),
      data_type = character(),
      data_type_cd = character(),
      stat_type = character(),
      stat_cd = character(),
      parameter = character(),
      parameter_code = character(),
      n_obs = character(),
      begin_date = character(),
      end_date = character(),
      geometry = sf::st_sfc(crs = 4269)
    ))
  }

  # Parameter code info grabber:

  pcodes <- function() {
    tables <- rvest::read_html(
      "https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
    ) |>
      rvest::html_nodes("table") |>
      rvest::html_table()

    pcodes <- tables[[1]] |>
      janitor::clean_names() |>
      dplyr::mutate(
        parm_cd = stringr::str_pad(as.character(parameter_code), 5, pad = "0")
      )

    return(pcodes)
  }

  # Site info grabber:

  nwis_table <- function() {
    site_url <- "https://maps.waterdata.usgs.gov/mapper/help/sitetype.html"

    table <- rvest::read_html(site_url) |>
      rvest::html_nodes("table") |>
      rvest::html_table()

    nwis_table <- rbind(
      table[[1]],
      table[[2]],
      table[[3]],
      table[[4]],
      table[[5]]
    ) |>
      dplyr::select(site_type_cd = 1, site_type = 2)

    return(nwis_table)
  }

  # Daily stats info grabber:

  stats_table <- function() {
    site_url <- "https://help.waterdata.usgs.gov/stat_code"

    table <- rvest::read_html(site_url) |>
      rvest::html_nodes("table") |>
      rvest::html_table() |>
      .[[1]] |>
      dplyr::mutate(stat_cd = sprintf("%05d", `Statistic Type Code`)) |>
      dplyr::select(stat_cd, stat_type = `Statistic Type Description`)

    return(table)
  }

  # Grab NWIS by an area of interest:
  if ((unlist(aoi_sf)[1] != "null")) {
    og_epsg <- sf::st_crs(aoi_sf)$epsg

    if (sf::st_crs(aoi_sf)$epsg != 4269) {
      aoi_sf <- aoi_sf |> sf::st_transform(4269)
    }

    # Validate AOI features - stop if any bounding box exceeds 118,078 square miles
    validate_aoi_size <- function(aoi_sf) {
      # Get square mile conversion factor for the projection
      # 2.58999e+6 converts square meters to square miles
      sq_m_to_sq_miles <- 1 / 2.58999e+6
      max_area_sq_miles <- 118078

      # Process each feature in the sf object
      aoi_with_area <- aoi_sf |>
        dplyr::mutate(
          bbox_area_sq_miles = purrr::map_dbl(1:dplyr::n(), function(i) {
            # Get bounding box
            bbox <- sf::st_bbox(aoi_sf[i, ])

            # Convert bbox to polygon
            bbox_polygon <- sf::st_as_sfc(bbox, crs = sf::st_crs(aoi_sf))

            # Calculate area in square miles
            area_sq_miles <- sf::st_area(bbox_polygon) * sq_m_to_sq_miles

            return(as.numeric(area_sq_miles))
          })
        )

      # Check if any area exceeds maximum and stop if too large
      if (any(aoi_with_area$bbox_area_sq_miles > max_area_sq_miles)) {
        stop(
          "At least one of your user-supplied features in 'aoi_sf' is too large - all features must be less than 118,078 square miles (roughly the area of Nevada). For state queries, please use the argument `state` instead of `aoi_sf`."
        )
      }

      return(aoi_with_area)
    }

    aoi_sf <- validate_aoi_size(aoi_sf)

    gage_sites <- vector("list", length = nrow(aoi_sf))

    suppressMessages({
      suppressWarnings({
        for (i in 1:nrow(aoi_sf)) {
          bbox <- sf::st_bbox(aoi_sf[i, ]) |>
            as.vector() |>
            round(., digits = 7)

          gage_sites[[i]] <- tryCatch(
            {
              dataRetrieval::whatNWISdata(bBox = c(bbox), service = "dv") |>
                dplyr::mutate(dplyr::across(
                  -c(dec_long_va, dec_lat_va),
                  as.character
                ))
            },
            error = function(e) {
              # Get error message as character string
              err_msg <- as.character(e$message)

              # Check for HTTP 404 in the error message
              if (grepl("404", err_msg)) {} else {
                # For any other error, stop with server error message
                stop(paste0(
                  "Something went wrong:",
                  err_msg,
                  " See https://waterservices.usgs.gov/docs/site-service/site-service-details/#error-codes."
                ))
              }
            }
          )
        }
      })
    })

    gage_sites <- dplyr::bind_rows(gage_sites)

    if (nrow(gage_sites) == 0) {
      message("No daily USGS-NWIS data in specified query.")
      return(empty_sf())
    }

    gage_sites <- gage_sites |>
      sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269)

    aoi_inventory <- gage_sites |>
      .[aoi_sf, ] |>
      dplyr::left_join(pcodes(), by = "parm_cd") |>
      dplyr::left_join(
        .,
        nwis_table(),
        by = c("site_tp_cd" = "site_type_cd")
      ) |>
      dplyr::left_join(., stats_table(), by = "stat_cd") |>
      dplyr::mutate(data_type = "Daily") |>
      dplyr::select(
        site_no,
        site_name = station_nm,
        site_type,
        site_type_cd = site_tp_cd,
        data_type,
        data_type_cd,
        stat_type,
        stat_cd,
        parameter = parameter_name_description,
        parameter_code = parm_cd,
        n_obs = count_nu,
        begin_date,
        end_date
      ) |>
      # remove any dupes if they exist (precautionary - they shouldn't!)
      dplyr::distinct(., .keep_all = TRUE)

    # Make sure returned USGS object is in the same CRS as what the user-supplied AOI is in:
    if (as.numeric(og_epsg) != 4269) {
      message(paste0(
        "The `aoi_sf` is in CRS = ",
        og_epsg,
        ". Returning NWIS sites in the same CRS."
      ))
      aoi_inventory <- sf::st_transform(aoi_inventory, sf::st_crs(og_epsg))
    }

    return(aoi_inventory)

    # Grab NWIS by vector of sites:
  } else if (any(unlist(siteid) != "null")) {
    # Check and split 'siteid' into chunks if necessary
    # {dataRetrieval} is limited by site list length:
    site_chunks <- if (length(siteid) > 1000) {
      message(
        "Your query will return many results and will take some time to process."
      )
      split(siteid, ceiling(seq_along(siteid) / 1000))
    } else {
      list(siteid)
    }

    # Grab NWIS sites by state code:
  } else if (any(unlist(statecode) != "null")) {
    # ensure proper capitalization
    statecode <- toupper(statecode)

    valid_statecode <- c(
      "AL",
      "AK",
      "AZ",
      "AR",
      "CA",
      "CO",
      "CT",
      "DE",
      "FL",
      "GA",
      "HI",
      "ID",
      "IL",
      "IN",
      "IA",
      "KS",
      "KY",
      "LA",
      "ME",
      "MD",
      "MA",
      "MI",
      "MN",
      "MS",
      "MO",
      "MT",
      "NE",
      "NV",
      "NH",
      "NJ",
      "NM",
      "NY",
      "NC",
      "ND",
      "OH",
      "OK",
      "OR",
      "PA",
      "RI",
      "SC",
      "SD",
      "TN",
      "TX",
      "UT",
      "VT",
      "VA",
      "WA",
      "WV",
      "WI",
      "WY",
      "PR",
      "VI",
      "MP",
      "GU",
      "AS"
    )

    if (!any(statecode %in% valid_statecode)) {
      stop(
        "Valid state abbreviation not provided. Please use state abbreviations."
      )
    }

    siteid <- vector("list", length = length(statecode))

    suppressWarnings({
      suppressMessages({
        for (i in 1:length(statecode)) {
          tryCatch(
            {
              siteid[[i]] <- dataRetrieval::whatNWISsites(
                stateCd = statecode[i]
              ) |>
                dplyr::mutate(dplyr::across(
                  -c(dec_long_va, dec_lat_va),
                  as.character
                ))
            },
            error = function(e) {
              # Get error message as character string
              err_msg <- as.character(e$message)

              # Check for HTTP 404 in the error message
              if (grepl("404", err_msg)) {} else {
                # For any other error, stop with server error message
                stop(paste0(
                  "Something went wrong:",
                  err_msg,
                  " See https://waterservices.usgs.gov/docs/site-service/site-service-details/#error-codes."
                ))
              }
            }
          )
        }
      })
    })

    siteid <- siteid |> dplyr::bind_rows() |> dplyr::distinct() |> .$site_no

    # Check and split 'siteid' into chunks if necessary
    site_chunks <- if (length(siteid) > 1000) {
      message(
        "Your query will return many results and will take some time to process."
      )
      split(siteid, ceiling(seq_along(siteid) / 1000))
    } else {
      list(siteid)
    }
  }

  # Map over the chunks to retrieve and process NWIS data

  inventory <- vector("list", length = length(site_chunks))

  for (i in 1:length(site_chunks)) {
    suppressWarnings({
      suppressMessages({
        inventory[[i]] <- tryCatch(
          {
            data <- dataRetrieval::whatNWISdata(
              siteNumber = site_chunks[[i]],
              service = "dv"
            ) |>
              dplyr::mutate(dplyr::across(
                -c(dec_long_va, dec_lat_va),
                as.character
              ))
          },
          error = function(e) {
            # Get error message as character string
            err_msg <- as.character(e$message)

            # Check for HTTP 404 in the error message
            if (grepl("404", err_msg)) {
              empty_sf()
            } else {
              # For any other error, stop with server error message
              stop(paste0(
                "Something went wrong:",
                err_msg,
                " See https://waterservices.usgs.gov/docs/site-service/site-service-details/#error-codes."
              ))
            }
          }
        )
      })
    })
  }

  inventory <- dplyr::bind_rows(inventory)

  # If no data, return empty data frame
  if (nrow(inventory) == 0) {
    return(empty_sf())
  }

  inventory <- inventory |>
    sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269) |>
    dplyr::left_join(pcodes(), by = "parm_cd") |>
    dplyr::left_join(., nwis_table(), by = c("site_tp_cd" = "site_type_cd")) |>
    dplyr::left_join(., stats_table(), by = "stat_cd") |>
    dplyr::mutate(data_type = "Daily") |>
    dplyr::select(
      site_no,
      site_name = station_nm,
      site_type,
      site_type_cd = site_tp_cd,
      data_type,
      data_type_cd,
      stat_type,
      stat_cd,
      parameter = parameter_name_description,
      parameter_code = parm_cd,
      n_obs = count_nu,
      begin_date,
      end_date
    ) |>
    # Remove any duplicates if they exist (precautionary - they shouldn't!)
    dplyr::distinct(., .keep_all = TRUE)

  # If no data, return empty data frame
  if (nrow(inventory) == 0) {
    message("No daily USGS-NWIS data in specified query.")
    return(empty_sf())
  }
  return(inventory)
}


#' Retrieve and tidy daily values from NWIS (DRAFT - UNDER ACTIVE DEVELOPMENT)
#'
#' This function interfaces with the USGS National Water Information System (NWIS) to
#' retrieve daily values (DV) water quality data using the TADA (Tools for Automated
#' Data Analysis) framework. Users can query data based on a spatial area of interest
#' (AOI), a vector of state abbreviations, or a vector of specific site ids, along
#' with relevant USGS parameter codes, statistics to return, and a date range.
#'
#' @param aoi_sf An sf object defining the area of interest. All individual sf
#' features (or "rows" in the sf data frame) must be under 118,078 square miles
#' (roughly the area of Nevada).
#' @param statecode A character vector of two-letter state abbreviations (e.g., `"CA"`, `"NY"`).
#' @param siteid A character vector of USGS site numbers.
#' @param parameter_codes A character vector of NWIS parameter codes to filter for (e.g., `"00060"` for discharge). Parameter codes and
#' names can be found at https://help.waterdata.usgs.gov/parameter_cd?group_cd=%
#' @param stat_codes A character vector of statistical types (e.g, the daily mean, the daily maximum, etc.) to return. Statistical code
#' names can be found at https://help.waterdata.usgs.gov/stat_code. Default is mean ("00003").
#' @param start_date A character string representing the start date for data retrieval in `"YYYY-MM-DD"` format.
#' @param end_date A character string representing the end date for data retrieval in `"YYYY-MM-DD"` format.
#'
#' @return A tidy `data.frame` containing daily values for each site, date, and parameter,
#' including a corresponding status code for each measurement.
#'
#' @details Only one of the query arguments (`aoi_sf`, `statecode`, or `siteid`)
#' should be provided. The function will stop if none or more than one are provided.
#' Moreover, all sf features must be under 118,078 square miles (roughly the area of Nevada).
#'
#' @examples
#' \dontrun{
#' # Example 1: Query by area of interest
#' locs_sf <- sf::read_sf("inst/extdata/AmericanIndian.shp") |>
#'   dplyr::filter(NAME %in% c("Spokane", "Navajo Nation"))
#' sites_aoi_sf <- TADA_getNWIS(
#'   aoi_sf = locs_sf,
#'   parameter_codes =
#'     c("00060", "00010"),
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31"
#' )
#'
#' # Example 2: Query by specific site numbers
#' sites_specific <- TADA_getNWIS(
#'   siteid = c("11530500", "11532500"),
#'   parameter_codes = c("00060", "00010"),
#'   start_date = "2020-01-01",
#'   end_date = "2020-12-31"
#' )
#'
#' # Example 3: Query by statecode
#' nwis_data <- TADA_getNWIS(
#'   statecode = c("RI", "CO"),
#'   stat_codes = c("00001"),
#'   parameter_codes = c("00010"),
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-02"
#' )
#' }
#'
TADA_getNWIS <- function(
  aoi_sf = "null",
  statecode = "null",
  siteid = "null",
  parameter_codes,
  stat_codes = "00003",
  start_date,
  end_date
) {
  # Confirm only a single argument has been provided
  if (
    !sum(purrr::map_lgl(
      list(aoi_sf, statecode[1], siteid[1]),
      ~ is.null(.x) || (is.character(.x) && .x == "null")
    )) %in%
      c(2, 3)
  ) {
    stop(paste0(
      "Multiple data-querying arguments (`aoi_sf`, `statecode`, `siteid`) have been provided. ",
      "Please use only one of these query options."
    ))
  } else if (
    sum(purrr::map_lgl(
      list(aoi_sf, statecode[1], siteid[1]),
      ~ is.null(.x) || (is.character(.x) && .x == "null")
    )) ==
      3
  ) {
    stop(paste0(
      "No data-querying argument (`aoi_sf`, `statecode`, `siteid`) has been provided. ",
      "Please select from one of these query options."
    ))
  }

  # rename so filter works later
  end <- end_date

  # Grab NWIS by an area of interest:
  # For large areas, this is quite slow.

  if ((unlist(aoi_sf)[1] != "null")) {
    # Validate AOI features - stop if any bounding box exceeds 118,078 square miles
    validate_aoi_size <- function(aoi_sf) {
      # Get square mile conversion factor for the projection
      # 2.58999e+6 converts square meters to square miles
      sq_m_to_sq_miles <- 1 / 2.58999e+6
      max_area_sq_miles <- 118078

      # Process each feature in the sf object
      aoi_with_area <- aoi_sf |>
        dplyr::mutate(
          bbox_area_sq_miles = purrr::map_dbl(1:dplyr::n(), function(i) {
            # Get bounding box
            bbox <- sf::st_bbox(aoi_sf[i, ])

            # Convert bbox to polygon
            bbox_polygon <- sf::st_as_sfc(bbox, crs = sf::st_crs(aoi_sf))

            # Calculate area in square miles
            area_sq_miles <- sf::st_area(bbox_polygon) * sq_m_to_sq_miles

            return(as.numeric(area_sq_miles))
          })
        )

      # Check if any area exceeds maximum and stop if too large
      if (any(aoi_with_area$bbox_area_sq_miles > max_area_sq_miles)) {
        stop(
          "At least one of your user-supplied features in 'aoi_sf' is too large - all features must be less than 118,078 square miles (roughly the area of Nevada). For state queries, please use the argument `state` instead of `aoi_sf`."
        )
      }

      return(aoi_with_area)
    }

    aoi_sf <- validate_aoi_size(aoi_sf)

    siteid <- vector("list", length = nrow(aoi_sf))

    suppressMessages({
      suppressWarnings({
        for (i in 1:nrow(aoi_sf)) {
          bbox <- sf::st_bbox(aoi_sf[i, ]) |>
            as.vector() |>
            round(., digits = 7)

          siteid[[i]] <- tryCatch(
            {
              dataRetrieval::whatNWISdata(
                bBox = c(bbox),
                service = "dv",
                startDate = start_date,
                endDate = end_date,
                parameterCd = parameter_codes,
                statCd = stat_codes
              ) |>
                dplyr::mutate(dplyr::across(
                  -c(dec_long_va, dec_lat_va),
                  as.character
                ))
            },
            error = function(e) {
              # Get error message as character string
              err_msg <- as.character(e$message)

              # Check for HTTP 404 in the error message
              if (grepl("404", err_msg)) {} else {
                # For any other error, stop with server error message
                stop(paste0(
                  "Something went wrong: ",
                  err_msg,
                  " See https://waterservices.usgs.gov/docs/site-service/site-service-details/#error-codes."
                ))
              }
            }
          )
        }
      })
    })

    list <- dplyr::bind_rows(siteid)

    # Grab NWIS by states:
  } else if ((unlist(statecode)[1] != "null")) {
    # ensure proper capitalization
    statecode <- toupper(statecode)

    valid_statecode <- c(
      "AL",
      "AK",
      "AZ",
      "AR",
      "CA",
      "CO",
      "CT",
      "DE",
      "FL",
      "GA",
      "HI",
      "ID",
      "IL",
      "IN",
      "IA",
      "KS",
      "KY",
      "LA",
      "ME",
      "MD",
      "MA",
      "MI",
      "MN",
      "MS",
      "MO",
      "MT",
      "NE",
      "NV",
      "NH",
      "NJ",
      "NM",
      "NY",
      "NC",
      "ND",
      "OH",
      "OK",
      "OR",
      "PA",
      "RI",
      "SC",
      "SD",
      "TN",
      "TX",
      "UT",
      "VT",
      "VA",
      "WA",
      "WV",
      "WI",
      "WY",
      "PR",
      "VI",
      "MP",
      "GU",
      "AS"
    )

    if (!any(statecode %in% valid_statecode)) {
      stop(
        "Valid state abbreviation not provided. Please use state abbreviations."
      )
    }

    siteid <- vector("list", length = length(statecode))

    suppressWarnings({
      suppressMessages({
        for (i in 1:length(statecode)) {
          tryCatch(
            {
              siteid[[i]] <- dataRetrieval::whatNWISdata(
                stateCd = statecode[i],
                service = "dv",
                startDate = start_date,
                endDate = end_date,
                parameterCd = parameter_codes,
                statCd = stat_codes
              ) |>
                dplyr::mutate(dplyr::across(
                  -c(dec_long_va, dec_lat_va),
                  as.character
                ))
            },
            error = function(e) {
              # Get error message as character string
              err_msg <- as.character(e$message)

              # Check for HTTP 404 in the error message
              if (grepl("404", err_msg)) {} else {
                # For any other error, stop with server error message
                stop(paste0(
                  "Something went wrong:",
                  err_msg,
                  " See https://waterservices.usgs.gov/docs/site-service/site-service-details/#error-codes."
                ))
              }
            }
          )
        }
      })
    })

    list <- siteid |> dplyr::bind_rows() |> dplyr::distinct()
  } else if ((unlist(siteid)[1] != "null")) {
    list <- tibble::tibble(site_no = siteid)
  }

  # Check if list is empty after filtering
  if (nrow(list) == 0) {
    stop(paste0(
      "No data available for the specified parameter(s) ",
      paste(parameter_codes, collapse = ", "),
      " and/or statistic(s) ",
      paste(stat_codes, collapse = ", "),
      " at these sites during the time frame ",
      start_date,
      " to ",
      end_date,
      "."
    ))
  }

  # Check if we need to split the sites into chunks
  site_chunks <- if (length(list$site_no) > 1000) {
    message(
      "Your query contains many sites and will take some time to process."
    )
    split(list$site_no, ceiling(seq_along(list$site_no) / 1000))
  } else {
    list(list$site_no)
  }

  # Map over the chunks to retrieve and process NWIS data
  full_data <- {
    purrr::map_dfr(site_chunks, function(chunk) {
      # Process chunk with error handling
      result <- tryCatch(
        {
          suppressMessages({
            suppressWarnings({
              data <- dataRetrieval::readNWISdv(
                siteNumbers = chunk,
                parameterCd = parameter_codes,
                startDate = start_date,
                endDate = end_date,
                statCd = stat_codes
              )

              if (nrow(data) > 0) {
                data <- data |>
                  dataRetrieval::renameNWISColumns() |>
                  data.table::data.table() |>
                  dplyr::mutate(dplyr::across(
                    dplyr::everything(),
                    as.character
                  ))
              }

              return(data)
            })
          })
        },
        error = function(e) {
          # Get error message as character string
          err_msg <- as.character(e$message)

          # Check for HTTP 404 in the error message
          if (grepl("404", err_msg)) {
            # Return empty data frame with appropriate structure
            return(data.frame())
          } else {
            # For any other error, stop with server error message
            stop(paste0(
              "Something went wrong: ",
              err_msg,
              " See https://waterservices.usgs.gov/docs/site-service/site-service-details/#error-codes."
            ))
          }
        }
      )

      return(result)
    })
  }

  # If no data found across all chunks, inform user
  if (nrow(full_data) == 0) {
    message(
      "No daily USGS-NWIS data found for the specified parameters and date range."
    )
  }

  # Check if full_data is empty
  if (nrow(full_data) == 0) {
    stop(paste0(
      "Query returned no data for specified parameter(s) ",
      paste(parameter_codes, collapse = ", "),
      " and/or statistic(s) ",
      paste(stat_codes, collapse = ", "),
      " at these sites during the time frame ",
      start_date,
      " to ",
      end_date,
      "."
    ))
  }

  data <- full_data |>
    tidyr::pivot_longer(
      cols = -c(site_no, agency_cd, Date, dplyr::ends_with("_cd")), # Keep these columns fixed
      names_to = "NWIS.parameter",
      values_to = "NWIS.value"
    ) |>
    dplyr::mutate(
      NWIS.parameter = ifelse(
        !grepl("_", NWIS.parameter),
        paste0(NWIS.parameter, "_mean"),
        NWIS.parameter
      )
    ) |>
    dplyr::select(
      NWIS.site_no = site_no,
      NWIS.date = Date,
      NWIS.parameter,
      NWIS.value
    )

  status <- full_data |>
    tidyr::pivot_longer(
      cols = c(dplyr::ends_with("_cd"), -agency_cd), # Keep these columns fixed
      names_to = "NWIS.parameter",
      values_to = "NWIS.status"
    ) |>
    dplyr::select(NWIS.status)

  tidied <- dplyr::bind_cols(data, status) |> dplyr::filter(!is.na(NWIS.value))

  # Check if final data is empty after removing NA values
  if (nrow(tidied) == 0) {
    stop(
      "All retrieved data contained NA values. No valid data available for the specified parameters and/or stats and time frame."
    )
  }

  return(tidied)
}

# # Tests for TADA_listNWIS
# testthat::test_that("TADA_listNWIS returns correct structure when querying by siteid", {
#   # Test with known site numbers
#   site_nums <- c("11530500", "11532500")
#   siteid_result <- TADA_listNWIS(siteid = site_nums)
#
#   # Check basic structure and content
#   testthat::expect_s3_class(siteid_result, "sf")
#   testthat::expect_true("site_no" %in% colnames(siteid_result))
#   testthat::expect_true("parameter" %in% colnames(siteid_result))
#   testthat::expect_true(all(siteid_result$site_no %in% site_nums))
# })
#
# testthat::test_that("TADA_listNWIS returns empty sf with correct structure when no data found", {
#   # Test with non-existent site
#   nonexistent_site <- "99999999"
#   result <- TADA_listNWIS(siteid = nonexistent_site)
#
#   # Check structure of empty return
#   testthat::expect_s3_class(result, "sf")
#   testthat::expect_equal(nrow(result), 0)
#   expected_cols <- c(
#     "site_no",
#     "site_name",
#     "site_type",
#     "site_type_cd",
#     "data_type",
#     "data_type_cd",
#     "parameter",
#     "parameter_code",
#     "n_obs",
#     "begin_date",
#     "end_date",
#     "geometry"
#   )
#   testthat::expect_true(all(expected_cols %in% colnames(result)))
# })
#
# testthat::test_that("TADA_listNWIS validates input parameters correctly", {
#   # Test with multiple query types
#   testthat::expect_error(
#     TADA_listNWIS(siteid = c("11530500"), statecode = "CA"),
#     "Multiple data-querying arguments"
#   )
#
#   # Test with no query types
#   testthat::expect_error(
#     TADA_listNWIS(),
#     "No data-querying argument"
#   )
#
#   # Test invalid state code
#   testthat::expect_error(
#     TADA_listNWIS(statecode = "ZZ"),
#     "Valid state abbreviation not provided"
#   )
# })
#
# testthat::test_that("TADA_listNWIS errors when aoi_sf is too large", {
#   # Test big shapefiles (should error if larger than 118,078 square miles)
#
#   # Create an artificially large polygon (covering most of the US)
#   large_bbox <- c(
#     xmin = -125, # West coast
#     ymin = 24, # Southern border
#     xmax = -66, # East coast
#     ymax = 49 # Northern border
#   )
#
#   # Convert to bbox object, then to sfc
#   large_poly <- sf::st_as_sfc(sf::st_bbox(large_bbox, crs = 4269))
#   large_sf <- sf::st_sf(geometry = large_poly)
#
#   # Test with artificial large AOI
#   testthat::expect_error(
#     TADA_listNWIS(aoi_sf = large_sf),
#     "At least one of your user-supplied features in 'aoi_sf' is too large"
#   )
#
#   # Create a multi-feature sf object with one small and one large polygon
#   small_bbox <- c(
#     xmin = -77.1,
#     ymin = 38.8,
#     xmax = -76.9,
#     ymax = 38.9
#   )
#
#   small_poly <- sf::st_as_sfc(sf::st_bbox(small_bbox, crs = 4269))
#
#   combined_sf <- sf::st_sf(
#     name = c("small", "large"),
#     geometry = c(small_poly, large_poly)
#   )
#
#   # Test with combined small+large features
#   testthat::expect_error(
#     TADA_listNWIS(aoi_sf = combined_sf),
#     "At least one of your user-supplied features in 'aoi_sf' is too large"
#   )
# })
#
# # Tests for TADA_getNWIS
# testthat::test_that("TADA_getNWIS returns correct structure with site query", {
#   # Test with known site that has discharge data
#   site_num <- "11530500"
#   start_date <- "2020-01-01"
#   end_date <- "2020-01-05"
#
#   result <- TADA_getNWIS(
#     siteid = site_num,
#     parameter_codes = "00060",
#     start_date = start_date,
#     end_date = end_date
#   )
#
#   # Check structure and content
#   testthat::expect_s3_class(result, "data.frame")
#   testthat::expect_true(all(
#     c(
#       "NWIS.site_no",
#       "NWIS.date",
#       "NWIS.parameter",
#       "NWIS.value",
#       "NWIS.status"
#     ) %in%
#       colnames(result)
#   ))
#   testthat::expect_true(all(result$NWIS.site_no == site_num))
#   testthat::expect_true(all(as.Date(result$NWIS.date) >= as.Date(start_date)))
#   testthat::expect_true(all(as.Date(result$NWIS.date) <= as.Date(end_date)))
# })
#
# testthat::test_that("TADA_getNWIS validates input parameters correctly", {
#   # Test with multiple query types
#   testthat::expect_error(
#     TADA_getNWIS(
#       siteid = "11530500",
#       statecode = "CA",
#       parameter_codes = "00060",
#       start_date = "2020-01-01",
#       end_date = "2020-01-05"
#     ),
#     "Multiple data-querying arguments"
#   )
#
#   # Test with no query types
#   testthat::expect_error(
#     TADA_getNWIS(
#       parameter_codes = "00060",
#       start_date = "2020-01-01",
#       end_date = "2020-01-05"
#     ),
#     "No data-querying argument"
#   )
#
#   # Test with missing required parameters
#   testthat::expect_error(
#     TADA_getNWIS(
#       siteid = "11530500",
#       start_date = "2020-01-01",
#       end_date = "2020-01-05"
#     )
#   )
# })
#
