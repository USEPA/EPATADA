#' TADA_listNWIS
#'
#' List available USGS daily NWIS data
#' 
#' @description
#' Retrieves available metadata from USGS National Water Information System (NWIS) based on 
#' different spatial queries: area of interest (AOI), specific sites, or state boundaries.
#' Returns a spatial {sf} object containing site information and available parameters.
#' If no data is found, returns an empty sf object with appropriate column structure.
#'
#' @param aoi_sf An sf object defining the area of interest.
#' @param states Character vector of two-letter state codes (e.g., c("CA", "OR")). 
#' @param sites Character vector of USGS site numbers. 
#'
#' @return An sf object containing NWIS site information including:
#'   \itemize{
#'     \item site_no: USGS site identification number
#'     \item site_name: Station name
#'     \item data_type: Type of data available (e.g., "Daily", "Water Quality")
#'     \item site_type: Description of the site type
#'     \item n_obs: Number of observations
#'     \item begin_date: Start date of data collection
#'     \item end_date: End date of data collection
#'     \item parameter: Parameter name and description
#'     \item code: Parameter code
#'   }
#'   Returns an empty sf object with the same structure if no data is found.
#'
#' @details Only one of the query arguments (`aoi_sf`, `states`, or `sites`) 
#' should be provided. The function will stop if none or more than one are provided.
#' 
#' @examples
#' \dontrun{
#' # Example 1: Query by area of interest
#' navajo_sf <- sf::read_sf("inst/extdata/AmericanIndian.shp") %>% dplyr::filter(NAME == "Navajo Nation")
#' sites_aoi_sf <- TADA_listNWIS(aoi_sf = navajo_sf)
#'
#' # Example 2: Query by specific site numbers
#' site_nums <- c("11530500", "11532500")
#' sites_specific <- TADA_listNWIS(sites = site_nums)
#'
#' # Example 3: Query by state
#' sites_state <- TADA_listNWIS(states = "CA")
#' }
#'
TADA_listNWIS <- function(aoi_sf = "null", states = "null", sites = "null"){
  if (!sum(purrr::map_lgl(list(aoi_sf, states[1], sites[1]), ~ is.null(.x) || (is.character(.x) && .x == "null"))) %in% c(2, 3)) {
    stop(
      paste0(
        "Multiple data-querying arguments (`aoi_sf`, `states`, `sites`) have been provided. ",
        "Please use only one of these query options."
      )
    )
  } else if (sum(purrr::map_lgl(list(aoi_sf, states[1], sites[1]), ~ is.null(.x) || (is.character(.x) && .x == "null"))) == 3) {
    stop(
      paste0(
        "No data-querying argument (`aoi_sf`, `states`, `sites`) has been provided. ",
        "Please select from one of these query options."
      )
    )
  }
  
  
  # Create empty sf object template with correct structure for "no return" data
  empty_sf <- function() {
    sf::st_sf(
      data.frame(
        site_no = character(),
        site_name = character(),
        site_type = character(),
        site_type_cd = character(),
        data_type= character(),
        data_type_cd = character(),
        parameter = character(),
        parameter_code = character(),
        n_obs = character(),
        begin_date = character(),
        end_date = character(),
        geometry = sf::st_sfc(crs = 4269)
      )
    )
  }
  
  # Parameter code info grabber:
  
  pcodes <- function() {
    
    tables <- rvest::read_html('https://help.waterdata.usgs.gov/parameter_cd?group_cd=%') %>%
      rvest::html_nodes('table') %>%
      rvest::html_table()
    
    pcodes <- tables[[1]] %>%
      janitor::clean_names() %>%
      dplyr::mutate(parm_cd = stringr::str_pad(as.character(parameter_code), 5, pad = "0"))
    
    return(pcodes)
    
  }
  
  # Site info grabber:
  
  nwis_table <- function() {
    
    site_url <- 'https://maps.waterdata.usgs.gov/mapper/help/sitetype.html'
    
    table <- rvest::read_html(site_url) %>%
      rvest::html_nodes('table') %>%
      rvest::html_table()
    
    nwis_table <- rbind(table[[1]], table[[2]], table[[3]], table[[4]], table[[5]]) %>%
      dplyr::select(site_type_cd = 1,
                    site_type = 2)
    
    return(nwis_table)
    
  }
  
  # Grab NWIS by an area of interest:
  if ((unlist(aoi_sf)[1] != "null")){
    
    gage_sites <- vector("list", length = nrow(aoi_sf))
    
    for (i in 1:nrow(aoi_sf)){
      
      bbox <- sf::st_bbox(aoi_sf[i,]) %>%
        as.vector() %>%
        round(., digits = 7) %>% 
        paste(collapse = ",")
      
      gage_sites[[i]] <- tryCatch(
        {
          suppressMessages(dataRetrieval::whatNWISdata(bBox = bbox, service = "dv"))
        },
        error = function(e) {
          stop("At least one of your user-supplied features in 'aoi_sf' is too large - all features must be less than 118,078 square miles (roughly the area of Nevada). For state queries, please use the argument `state` instead of `aoi_sf`.")
        }
      )
    }
    
    gage_sites <- dplyr::bind_rows(gage_sites) 
    
    if (nrow(gage_sites) == 0) {
      message("No daily USGS-NWIS data in selected query.")
      return(empty_sf())
    }
    
    gage_sites <- gage_sites %>%
      sf::st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4269) # USGS supplies crs 4326 always
    
    # Make sure returned USGS object is in the same CRS as what the user-supplied AOI is in:
    if(sf::st_crs(aoi_sf) != sf::st_crs(gage_sites)){
      
      print(paste0("The `aoi_sf` is in CRS = ", sf::st_crs(aoi_sf)$epsg, ". Returning NWIS sites in the same CRS."))
      gage_sites <- sf::st_transform(gage_sites, sf::st_crs(aoi_sf))
      
    }
    
    aoi_inventory <- gage_sites %>%
      .[aoi_sf,] %>%
      dplyr::left_join(pcodes(), by = "parm_cd") %>%
      dplyr::left_join(., nwis_table(), by = c("site_tp_cd" = "site_type_cd")) %>%
      dplyr::mutate(data_type = "Daily") %>%
      dplyr::select(site_no,
                    site_name = station_nm,
                    site_type,
                    site_type_cd = site_tp_cd,
                    data_type,
                    data_type_cd,
                    parameter = parameter_name_description,
                    parameter_code = parm_cd,
                    n_obs = count_nu,
                    begin_date,
                    end_date) %>%
      # remove any dupes if they exist (precautionary - they shouldn't!)
      dplyr::distinct(., .keep_all = TRUE)
    
    return(aoi_inventory)
    
    # Grab NWIS by vector of sites:
  } else if (any(unlist(sites) != "null")){
    
    # Check and split 'sites' into chunks if necessary
    # {dataRetrieval} crashes if site list is too big:
    site_chunks <- if (length(sites) > 35000) {
      split(sites, ceiling(seq_along(sites) / 35000))
    } else {
      list(sites)
    }
    
    # Map over the chunks to retrieve and process NWIS data
    site_list_inventory <- purrr::map_dfr(site_chunks, function(chunk) {
      result <- tryCatch({
        data <- dataRetrieval::whatNWISdata(siteNumber = chunk, service = "dv")
        
        # If no data, return empty data frame
        if (nrow(data) == 0) {
          return(empty_sf()) 
        }
        
        data %>%
          sf::st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4269) %>%
          dplyr::left_join(pcodes(), by = "parm_cd") %>%
          dplyr::left_join(., nwis_table(), by = c("site_tp_cd" = "site_type_cd")) %>%
          dplyr::mutate(data_type = "Daily") %>%
          dplyr::select(site_no,
                        site_name = station_nm,
                        site_type,
                        site_type_cd = site_tp_cd,
                        data_type,
                        data_type_cd,
                        parameter = parameter_name_description,
                        parameter_code = parm_cd,
                        n_obs = count_nu,
                        begin_date,
                        end_date)
      }, error = function(e) {
        message("No daily USGS-NWIS data in selected query.") 
        return(empty_sf())  # Return an empty df to keep processing
      })
      
      return(result)
    }) %>%
      # Remove any duplicates if they exist (precautionary - they shouldn't!)
      dplyr::distinct(., .keep_all = TRUE)
    
    return(site_list_inventory)
    
    # Grab NWIS sites by state code: 
  } else  if (any(unlist(states) != "null")){
    
    valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                      "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                      "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                      "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                      "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                      "PR")
    
    if (!any(states %in% valid_states)) {
      stop("Valid state abbreviation not provided. Please use state abbreviations.")
    }
    
    sites <- vector("list", length = length(states))
    
    for(i in 1:length(states)){
      sites[[i]] <- dataRetrieval::whatNWISsites(stateCd = states[i])
    }
    
    sites <- sites %>% dplyr::bind_rows() %>% dplyr::distinct() %>% .$site_no
    
    # Check and split 'sites' into chunks if necessary
    site_chunks <- if (length(sites) > 35000) {
      split(sites, ceiling(seq_along(sites) / 35000))
    } else {
      list(sites)
    }
    
    # Map over the chunks to retrieve and process NWIS data
    state_list_inventory <- purrr::map_dfr(site_chunks, function(chunk) {
      result <- tryCatch({
        data <- dataRetrieval::whatNWISdata(siteNumber = chunk, service = "dv")
        
        # If no data, return empty data frame
        if (nrow(data) == 0) {
          return(empty_sf()) 
        }
        
        data %>%
          sf::st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4269) %>%
          dplyr::left_join(pcodes(), by = "parm_cd") %>%
          dplyr::left_join(., nwis_table(), by = c("site_tp_cd" = "site_type_cd")) %>%
          dplyr::mutate(data_type = "Daily") %>%
          dplyr::select(site_no,
                        site_name = station_nm,
                        site_type,
                        site_type_cd = site_tp_cd,
                        data_type,
                        data_type_cd,
                        parameter = parameter_name_description,
                        parameter_code = parm_cd,
                        n_obs = count_nu,
                        begin_date,
                        end_date)
      }, error = function(e) {
        message("No daily USGS-NWIS data in selected query.") 
        return(empty_sf())  # Return an empty df to keep processing
      })
      
      return(result)
    }) %>%
      # Remove any duplicates if they exist (precautionary - they shouldn't!)
      dplyr::distinct(., .keep_all = TRUE)
    
    
    return(state_list_inventory)
    
  } 
  
}

#' Retrieve and tidy daily values from NWIS
#'
#' This function interfaces with the USGS National Water Information System (NWIS) to 
#' retrieve daily values (DV) water quality data using the TADA (Tools for Analysis of 
#' Data from Agencies) framework. Users can query data based on a spatial area of interest 
#' (AOI), a vector of state abbreviations, or a vector of specific site numbers, along 
#' with relevant parameter codes and a date range.
#'
#' @param aoi_sf An `sf` object specifying the area of interest.
#' @param states A character vector of two-letter state abbreviations (e.g., `"CA"`, `"NY"`).
#' @param sites A character vector of USGS site numbers.
#' @param parameter_codes A character vector of NWIS parameter codes to filter for (e.g., `"00060"` for discharge). Parameter codes and
#' names can be found at https://help.waterdata.usgs.gov/parameter_cd?group_cd=%
#' @param start_date A character string representing the start date for data retrieval in `"YYYY-MM-DD"` format.
#' @param end_date A character string representing the end date for data retrieval in `"YYYY-MM-DD"` format.
#'
#' @return A tidy `data.frame` containing daily values for each site, date, and parameter, 
#' including a corresponding status code for each measurement.
#'
#' @details Only one of the query arguments (`aoi_sf`, `states`, or `sites`) 
#' should be provided. The function will stop if none or more than one are provided.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' #' # Example 1: Query by area of interest
#' navajo_sf <- sf::read_sf("inst/extdata/AmericanIndian.shp") %>% dplyr::filter(NAME == "Navajo Nation")
#' sites_aoi_sf <- TADA_getNWIS(aoi_sf = navajo_sf, parameter_codes = c("00060", "00010"), start_date = "2020-01-01", end_date = "2020-01-31")
#'
#' # Example 2: Query by specific site numbers
#' sites_specific <- TADA_getNWIS(sites = c("11530500", "11532500"), parameter_codes = c("00060", "00010"), start_date = "2020-01-01", end_date = "2020-12-31")
#'
#' # Example 3: Query by states
#' nwis_data <- TADA_getNWIS(states = c("RI", "CO"), parameter_codes = c("00060", "00010"), start_date = "2020-01-01", end_date = "2020-01-02")
#' }
#' 
TADA_getNWIS <- function(aoi_sf = "null", states = "null", sites = "null", parameter_codes, start_date, end_date){
  
  if (!sum(purrr::map_lgl(list(aoi_sf, states[1], sites[1]), ~ is.null(.x) || (is.character(.x) && .x == "null"))) %in% c(2, 3)) {
    stop(
      paste0(
        "Multiple data-querying arguments (`aoi_sf`, `states`, `sites`) have been provided. ",
        "Please use only one of these query options."
      )
    )
  } else if (sum(purrr::map_lgl(list(aoi_sf, states[1], sites[1]), ~ is.null(.x) || (is.character(.x) && .x == "null"))) == 3) {
    stop(
      paste0(
        "No data-querying argument (`aoi_sf`, `states`, `sites`) has been provided. ",
        "Please select from one of these query options."
      )
    )
  }
  
  # rename so filter works later
  end <- end_date
  
  # Grab NWIS by an area of interest:
  if ((unlist(aoi_sf)[1] != "null")){
    
    list <- TADA_listNWIS(aoi_sf = aoi_sf) %>%
      dplyr::filter(parameter_code %in% parameter_codes,
                    lubridate::ymd(begin_date) <= lubridate::ymd(start_date),
                    lubridate::ymd(end_date) >= lubridate::ymd(end))
    
  } else if ((unlist(states)[1] != "null")){
    
    list <- TADA_listNWIS(states = states) %>%
      dplyr::filter(parameter_code %in% parameter_codes,
                    lubridate::ymd(begin_date) <= lubridate::ymd(start_date),
                    lubridate::ymd(end_date) >= lubridate::ymd(end))
  } else if ((unlist(sites)[1] != "null")){
    
    list <- TADA_listNWIS(sites = sites) %>%
      dplyr::filter(parameter_code %in% parameter_codes,
                    lubridate::ymd(begin_date) <= lubridate::ymd(start_date),
                    lubridate::ymd(end_date) >= lubridate::ymd(end))
  }
  
  # Check if list is empty after filtering
  if (nrow(list) == 0) {
    stop(paste0("No data available for the specified parameter(s) ", 
                paste(parameter_codes, collapse = ", "), 
                " at these sites during the time frame ", 
                start_date, " to ", end_date, "."))
  }
  
  full_data <- dataRetrieval::readNWISdv(siteNumbers = list$site_no, 
                                         parameterCd = parameter_codes, 
                                         startDate = start_date,
                                         endDate = end) %>%
    dataRetrieval::renameNWISColumns() %>%
    data.table::data.table() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # Check if full_data is empty
  if (nrow(full_data) == 0) {
    stop(paste0("Query returned no data for the specified parameter(s) ", 
                paste(parameter_codes, collapse = ", "), 
                " at these sites during the time frame ", 
                start_date, " to ", end_date, "."))
  }
  
  data <- full_data %>%
    tidyr::pivot_longer(
      cols = -c(site_no, agency_cd, Date, dplyr::ends_with("_cd")),  # Keep these columns fixed
      names_to = "NWIS.parameter",
      values_to = "NWIS.value"
    ) %>%
    dplyr::select(NWIS.site_no = site_no, NWIS.date = Date, NWIS.parameter, NWIS.value)
  
  status <- full_data %>%
    tidyr::pivot_longer(
      cols = c(dplyr::ends_with("_cd"), -agency_cd),  # Keep these columns fixed
      names_to = "NWIS.parameter",
      values_to = "NWIS.status"
    ) %>%
    dplyr::select(NWIS.status)
  
  tidied <- dplyr::bind_cols(data, status) %>%
    dplyr::filter(!is.na(NWIS.value))
  
  # Check if final data is empty after removing NA values
  if (nrow(tidied) == 0) {
    stop("All retrieved data contained NA values. No valid data available for the specified parameters and time frame.")
  }
  
  return(tidied)
  
}
