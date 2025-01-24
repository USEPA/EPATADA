#' Generate TADA-compatible dataframe from WQP Data
#'
#' Retrieve data from Water Quality Portal (WQP) and generate a TADA-compatible
#' dataframe. Note that the inputs (e.g. project, organization, siteType) with the
#' exceptions of endDate and startDate match the web service call format from the
#' online WQP GUI. endDate and startDate match the format suggested in USGS's
#' dataRetrieval package (endDate = "YYYY-MM-DD"), which is a more familiar date
#' format for R users than the WQP GUI's endDateHi = "MM-DD-YYYY". aoi_sf,
#' tribal_area_type, and tribe_name_parcel do not have corresponding inputs in
#' the web service.
#'
#' Multiple fields are queried together using AND logic, but multiple values within
#' one field are queried together using OR logic. For example, within
#' characteristicName, if you enter, c("pH", "Dissolved oxygen (DO)), the
#' function will return all results that are "pH" OR "Dissolved oxygen (DO)". Similarly,
#' if you enter c("VA", "IL"), the function will return results from Virginia OR Illinois.
#' But the combo of these fields are ANDs: The function will return any pH and DO data
#' from only Virginia or Illinois; the data must fit into one of the values from BOTH
#' of the query fields.
#' characteristicName and Characteristic Group also work as an AND, therefore the
#' characteristicName must fall within the Characteristic Group when both are entered.
#'
#' aoi_sf cannot be used with tribal_area_type. If countrycode, countycode, huc,
#' siteid, or statecode are used with aoi_sf or tribal_area_type they will be ignored
#' under the assumption that the sf object or tribal location are the intended
#' area of interest.
#'
#' Users can reference the \href{https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads}{WQX domain tables}
#' to find allowable values for queries, e.g., reference the WQX domain table to find countycode and statecode: https://cdx.epa.gov/wqx/download/DomainValues/County_CSV.zip
#' Alternatively, you can use the WQP services to find areas where data is available in the US: https://www.waterqualitydata.us/Codes/countycode
#'
#' TADA_DataRetrieval automatically runs TADA_AutoClean on the incoming data frame. TADA_AutoClean
#' is important for categorizing result value and detection limit data, as well as
#' harmonizing key columns used in TADA. See ?TADA_AutoClean for more information.
#'
#' Note: TADA_DataRetrieval (by leveraging dataRetrieval),  automatically converts
#' the date times to UTC. It also automatically converts the data to dates,
#' datetimes, numerics based on a standard algorithm. See: ?dataRetrieval::readWQPdata
#'
#' @param startDate Start Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param endDate End Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param aoi_sf An sf object to use for a query area of interest
#' @param countrycode Code that identifies a country or ocean (e.g. countrycode = "CA" for Canada, countrycode = "OA" for Atlantic Ocean). See https://www.waterqualitydata.us/Codes/countrycode for options.
#' @param statecode FIPS state alpha code that identifies a state (e.g. statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param countycode FIPS county name. Note that a state code must also be supplied (e.g. statecode = "AL", countycode = "Chilton"). See https://www.waterqualitydata.us/Codes/countycode for options.
#' @param huc A numeric code denoting a hydrologic unit. Example: "04030202". Different size hucs can be entered. See https://epa.maps.arcgis.com/home/item.html?id=796992f4588c401fabec7446ecc7a5a3 for a map with HUCS. Click on a HUC to find the associated code.
#' @param siteid Unique monitoring location identifier.
#' @param siteType Type of waterbody. See https://www.waterqualitydata.us/Codes/sitetype for options.
#' @param tribal_area_type One of four tribal spatial layers: "Alaska Native Allotments", "American Indian Reservations", "Off-reservation Trust Lands", or "Oklahoma Tribal Statistical Areas". More info in TADA_TribalOptions(). Note that "Alaska Native Villages" and "Virginia Federally Recognized Tribes" layers will not return a successful query.
#' @param tribe_name_parcel The name of one or more tribes corresponding to entries in the TRIBE_NAME field of the specified tribal_area_type. OR if the type is "Alaska Native Allotments" then the corresponding PARCEL_NO. More info in TADA_TribalOptions().
#' @param characteristicName Name of parameter. See https://www.waterqualitydata.us/Codes/characteristicName for options.
#' @param characteristicType Groups of environmental measurements/parameters. See https://www.waterqualitydata.us/Codes/characteristicType for options.
#' @param sampleMedia Sampling substrate such as water, air, or sediment. See https://www.waterqualitydata.us/Codes/sampleMedia for options.
#' @param organization A string of letters and/or numbers (some additional characters also possible) used to signify an organization with data in the Water Quality Portal. See https://www.waterqualitydata.us/Codes/organization for options.
#' @param project A string of letters and/or numbers (some additional characters also possible) used to signify a project with data in the Water Quality Portal. See https://www.waterqualitydata.us/Codes/project for options.
#' @param providers Leave blank to include all, or specify "STEWARDS", "STORET" (i.e., WQX), and/or "NWIS". See https://www.waterqualitydata.us/Codes/providers for options.
#' @param maxrecs Maximum number of records to query at once (i.e., without breaking into smaller queries).
#' @param ask A logical value indicating whether the user should be asked for approval before downloads begin.
#' @param applyautoclean Logical, defaults to TRUE. Applies TADA_AutoClean function on the returned data profile. Suggest switching to FALSE for queries that are expected to be large.
#'
#' @return TADA-compatible dataframe
#'
#' @note
#' Alaska Native Villages and Virginia Federally Recognized Tribes are point
#' geometries in the Map Service, not polygons. At the time of this writing they
#' do not return any data when used for WQP bbox queries and so are set to return
#' errors when used with this function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # example for WI
#' tada1 <- TADA_DataRetrieval(
#'   statecode = "WI", countycode = "Dane",
#'   characteristicName = "Phosphorus",
#'   ask = FALSE
#' )
#'
#' # example for UT
#' tada2 <- TADA_DataRetrieval(
#'   statecode = "UT",
#'   characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
#'   ask = FALSE
#' )
#'
#' # example for SC
#' tada3 <- TADA_DataRetrieval(statecode = "SC", countycode = "Abbeville", ask = FALSE)
#'
#' # example for CT
#' tada4 <- TADA_DataRetrieval(statecode = "CT", startDate = "2020-10-01", ask = FALSE)
#'
#'
#' # note that countycode queries require a statecode (see example below)
#' tada5 <- TADA_DataRetrieval(countycode = "US:02:020", ask = FALSE)
#'
#' # example for NM
#' tada6 <- TADA_DataRetrieval(
#'   statecode = "NM",
#'   characteristicName = c(
#'     "Ammonia",
#'     "Nitrate",
#'     "Nitrogen"
#'   ),
#'   startDate = "2020-05-01",
#'   ask = FALSE
#' )
#'
#' # example for AK project
#' tada7 <- TADA_DataRetrieval(project = "Anchorage Bacteria 20-21", ask = FALSE)
#'
#' # another example for AK
#' tada8 <- TADA_DataRetrieval(
#'   statecode = "AK",
#'   characteristicName = c(
#'     "Fecal Coliform",
#'     "Escherichia coli",
#'     "Enterococcus",
#'     "Ammonia",
#'     "Nitrate",
#'     "Nitrogen"
#'   ),
#'   startDate = "2018-05-01",
#'   ask = FALSE
#' )
#'
#' # example for tribes
#' # Download data for many of the ATTAINS participating tribes
#' # Note this query may take about a half hour to run
#' # https://www.itecmembers.org/attains/
#' # ATTAINS participating tribes also have tribal pages in EPA's
#' # How's My Waterway Application
#' # Example: https://mywaterway.epa.gov/tribe/SFNOES
#' #
#' # Sac & Fox Nation, Oklahoma "SFNOES_WQX"
#' # Citizen Potawatomi Nation, Oklahoma "CPNWATER"
#' # Delaware Nation, Oklahoma "DELAWARENATION"
#' # Hoopa Valley Tribe, California "HVTEPA_WQX"
#' # Otoe Missouria Tribe of Oklahoma "O_MTRIBE_WQX"
#' # Minnesota Chippewa Tribe, Minnesota (Fond du Lac Band) "FONDULAC_WQX"
#' # Pueblo of San Ildefonso, New Mexico "SANILDEFONSODECP"
#' # Pueblo of Santa Ana, New Mexico "PUEBLO_SANTAANA"
#' # Pueblo of Tesuque, New Mexico "PUEBLOOFTESUQUE"
#' # Red Lake Band of Chippewa Indians, Minnesota "REDLAKE_WQX"
#' # Seneca-Cayuga Nation "SCEQ"
#' # The Chickasaw Nation "CNENVSER"
#' # The Choctaw Nation of Oklahoma "CHOCNATWQX"
#' # Wyandotte Nation "WNENVDPT_WQX"
#' # Pueblo of Pojoaque "PUEBLO_POJOAQUE"
#'
#' tada9 <- TADA_DataRetrieval(organization = c(
#'   "SFNOES_WQX",
#'   "CPNWATER",
#'   "DELAWARENATION",
#'   "HVTEPA_WQX",
#'   "O_MTRIBE_WQX",
#'   "FONDULAC_WQX",
#'   "SANILDEFONSODECP",
#'   "PUEBLO_SANTAANA",
#'   "PUEBLOOFTESUQUE",
#'   "REDLAKE_WQX",
#'   "SCEQ",
#'   "CNENVSER",
#'   "CHOCNATWQX",
#'   "WNENVDPT_WQX",
#'   "PUEBLO_POJOAQUE"
#' ),
#' ask = FALSE)
#'
#' # query only NWIS data for a 10 year period in CT
#' tada10 <- TADA_DataRetrieval(
#'   startDate = "2013-01-01",
#'   endDate = "2022-12-31",
#'   sampleMedia = c("Water", "water"),
#'   statecode = "CT", # consider downloading only 1 state at a time
#'   providers = "NWIS",
#'   applyautoclean = FALSE,
#'   ask = FALSE
#' )
#'
#' # query by country code (e.g. Canada, countrycode = "CA")
#' tada11 <- TADA_DataRetrieval(
#'   startDate = "2015-01-01",
#'   countrycode = "CA",
#'   ask = FALSE
#' )
#' }
#'
TADA_DataRetrieval <- function(startDate = "null",
                               endDate = "null",
                               aoi_sf = NULL,
                               countrycode = "null",
                               countycode = "null",
                               huc = "null",
                               siteid = "null",
                               siteType = "null",
                               tribal_area_type = "null",
                               tribe_name_parcel = "null",
                               characteristicName = "null",
                               characteristicType = "null",
                               sampleMedia = "null",
                               statecode = "null",
                               organization = "null",
                               project = "null",
                               providers = "null",
                               maxrecs = 250000,
                               ask = TRUE,
                               applyautoclean = TRUE) {
  
  # Require one tribal area type:
  if (length(tribal_area_type == "null") >1) {
    stop("The tribal_area_type argument only accepts a single value, but multiple have been provided.")
  }
  
  # Check for incomplete or inconsistent inputs:

  # If both an sf object and tribe information are provided it's unclear what
  # the priority should be for the query
  if (!is.null(aoi_sf) &
    any((tribal_area_type != "null") | (tribe_name_parcel != "null"))) {
    stop(
      paste0(
        "Both sf data and tribal information have been provided. ",
        "Please use only one of these query options."
      )
    )
  }

  # Check for other arguments that indicate location. Function will ignore
  # these inputs but warn the user
  if (
    # sf object provided
    (!is.null(aoi_sf) & inherits(aoi_sf, "sf")) &
      # with additional location info
      any(
        (countrycode != "null"), (countycode != "null"), (huc != "null"),
        (siteid != "null"), (statecode != "null")
      )
  ) {
    warning(
      paste0(
        "Location information has been provided in addition to an sf object. ",
        "Only the sf object will be used in the query."
      )
    )
  } else if (
    # Tribe info provided
    (tribal_area_type != "null") &
      # with additional location info
      any(
        (countrycode != "null"), (countycode != "null"), (huc != "null"),
        (siteid != "null"), (statecode != "null")
      )
  ) {
    warning(
      paste0(
        "Location information has been provided in addition to tribal information. ",
        "Only the tribal information will be used in the query."
      )
    )
  }

  # Insufficient tribal info provided:
  # Type but no parcel
  if ((tribal_area_type != "null") & all(tribe_name_parcel == "null")) {
    stop("A tribe_name_parcel is required if tribal_area_type is provided.")
  }
  # Parcel but no type
  if ((tribal_area_type == "null") & all(tribe_name_parcel != "null")) {
    stop("A tribal_area_type is required if tribe_name_parcel is provided.")
  }
  
  # If an sf object OR tribal info are provided they will be the basis of the query
  # (The tribal data handling uses sf objects as well)
  if ((!is.null(aoi_sf) & inherits(aoi_sf, "sf")) | (tribal_area_type != "null")) {
    # Build the non-sf part of the query:

    # Set query parameters
    WQPquery <- list()

    # StartDate
    if (length(startDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = list(startDate))
    } else if (startDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = startDate)
    }
    # SiteType
    if (length(siteType) > 1) {
      WQPquery <- c(WQPquery, siteType = list(siteType))
    } else if (siteType != "null") {
      WQPquery <- c(WQPquery, siteType = siteType)
    }
    # CharacteristicName
    if (length(characteristicName) > 1) {
      WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
    } else if (characteristicName != "null") {
      WQPquery <- c(WQPquery, characteristicName = characteristicName)
    }
    # CharacteristicType
    if (length(characteristicType) > 1) {
      WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
    } else if (characteristicType != "null") {
      WQPquery <- c(WQPquery, characteristicType = characteristicType)
    }
    # SampleMedia
    if (length(sampleMedia) > 1) {
      WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
    } else if (sampleMedia != "null") {
      WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
    }
    # Project
    if (length(project) > 1) {
      WQPquery <- c(WQPquery, project = list(project))
    } else if (project != "null") {
      WQPquery <- c(WQPquery, project = project)
    }
    # Provider
    if (length(providers) > 1) {
      WQPquery <- c(WQPquery, providers = list(providers))
    } else if (providers != "null") {
      WQPquery <- c(WQPquery, providers = providers)
    }
    # Organization
    if (length(organization) > 1) {
      WQPquery <- c(WQPquery, organization = list(organization))
    } else if (organization != "null") {
      WQPquery <- c(WQPquery, organization = organization)
    }
    # EndDate
    if (length(endDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = list(endDate))
    } else if (endDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = endDate)
    }

    # sf AOI prep for query

    # If tribe info is provided then grab the corresponding sf object:
    if (tribal_area_type != "null") {
      # Make a reference table for tribal area type + url matching
      # (options that don't return results are commented out)
      map_service_urls <- tibble::tribble(
        ~tribal_area,                            ~url,
        "Alaska Native Allotments",              "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0",
        # "Alaska Native Villages",                "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1",
        "American Indian Reservations",          "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2",
        "Off-reservation Trust Lands",           "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3",
        "Oklahoma Tribal Statistical Areas",     "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4" # ,
        # "Virginia Federally Recognized Tribes",  "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5"
      )

      # Keep to a single type:
      if (length(tribal_area_type) > 1) {
        stop("tribal_area_type must be of length 1.")
      }

      # These two layers will not return any data when used for bboxes
      if (tribal_area_type == "Alaska Native Villages") {
        stop("Alaska Native Villages data are centroid points, not spatial boundaries.")
      } else if (tribal_area_type == "Virginia Federally Recognized Tribes") {
        stop("Federally recognized tribal entities in Virginia do not have any available spatial boundaries.")
      }

      # These area types allow filtering by TRIBE_NAME (unique within each type)
      if (tribal_area_type %in% c(
        "American Indian Reservations",
        "Off-reservation Trust Lands",
        "Oklahoma Tribal Statistical Areas" 
      )
      ) {
        # Get the relevant url
        aoi_sf <- dplyr::filter(
          map_service_urls,
          tribal_area == tribal_area_type
        )$url %>%
          # Pull data
          arcgislayers::arc_open() %>%
          # Return sf
          arcgislayers::arc_select() %>%
          # If a value provided, then filter
          {
            if (all(tribe_name_parcel != "null")) {
              dplyr::filter(., TRIBE_NAME %in% tribe_name_parcel)
            } else {
              .
            }
          }

        # Otherwise filter by PARCEL_NO (Note that values in this col are not unique)
      } else if (tribal_area_type == "Alaska Native Allotments") {
        aoi_sf <- dplyr::filter(
          map_service_urls,
          tribal_area == tribal_area_type
        )$url %>%
          arcgislayers::arc_open() %>%
          arcgislayers::arc_select() %>%
          {
            if (all(tribe_name_parcel != "null")) {
              dplyr::filter(., PARCEL_NO %in% tribe_name_parcel)
            } else {
              .
            }
          }
      } else {
        stop("Tribal area type or tribal name parcel not recognized. Refer to TADA_TribalOptions() for query options.")
      }
    }

    # Check and/or fix geometry
    aoi_sf <- sf::st_make_valid(aoi_sf)

    # Match CRS
    if (sf::st_crs(aoi_sf) != 4326) {
      aoi_sf <- sf::st_transform(aoi_sf, crs = 4326)
    }

    # Get bbox of the sf object
    input_bbox <- sf::st_bbox(aoi_sf)

    # Query info on available data within the bbox
    # Don't want to print every message that's returned by WQP
    quiet_whatWQPdata <- purrr::quietly(dataRetrieval::whatWQPdata)

    # Try getting WQP info
    message("Checking what data is available. This may take a moment.")
    
    quiet_bbox_avail <- quiet_whatWQPdata(
      WQPquery,
      bBox = c(input_bbox$xmin, input_bbox$ymin, input_bbox$xmax, input_bbox$ymax)
    )

    # Alert & stop if an http error was received
    if (is.null(quiet_bbox_avail$result)) {
      stop_message <- quiet_bbox_avail$messages %>%
        grep(pattern = "failed|HTTP", x = ., ignore.case = FALSE, value = TRUE) %>%
        paste("\n", ., collapse = "") %>%
        paste("The WQP request returned a NULL with the following message(s): \n",
          .,
          collapse = "\n"
        )

      stop(stop_message)
    }

    # Use result only
    bbox_avail <- quiet_bbox_avail$result

    # Check if any sites are within the aoi
    if ((nrow(bbox_avail) > 0) == FALSE) {
      stop("No monitoring sites were returned within your area of interest (no data available).")
    }


    quiet_whatWQPsites <- purrr::quietly(dataRetrieval::whatWQPsites)

    quiet_bbox_sites <- quiet_whatWQPsites(
      siteid = bbox_avail$MonitoringLocationIdentifier
    )

    if (is.null(quiet_bbox_sites$result)) {
      stop_message <- quiet_bbox_sites$messages %>%
        grep(pattern = "failed|HTTP", x = ., ignore.case = FALSE, value = TRUE) %>%
        paste("\n", ., collapse = "") %>%
        paste("The WQP request returned a NULL with the following message(s): \n",
          .,
          collapse = "\n"
        )
      stop(stop_message)
    }

    # Reformat returned info as sf
    bbox_sites_sf <- TADA_MakeSpatial(quiet_bbox_sites$result, crs = 4326)

    # Subset sites to only within shapefile and get IDs
    clipped_sites_sf <- bbox_sites_sf[aoi_sf, ]

    clipped_site_ids <- clipped_sites_sf$MonitoringLocationIdentifier

    record_count <- bbox_avail %>%
      dplyr::filter(MonitoringLocationIdentifier %in% clipped_site_ids) %>%
      dplyr::pull(resultCount) %>%
      sum()

    # Should we proceed with downloads? If ask == TRUE then ask the user.
    if (ask == TRUE) {
      user_decision <- ask_user(n_records = record_count)

      # Act on input
      if (user_decision == "yes") {
        print("Proceeding with download.")
      } else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }

    # Continue now with site count
    site_count <- length(clipped_site_ids)

    # Check for either more than 300 sites or more records than max_recs.
    # If either is true then we'll approach the pull as a "big data" pull
    if (site_count > 300 | record_count > maxrecs) {
      message(
        paste0(
          "The number of sites and/or records matched by the AOI and query terms is large, so the download may take some time. ",
          "If your AOI is a county, state, country, or HUC boundary it would be more efficient to provide a code instead of an sf object."
        )
      )

      # Use helper function to download large data volume
      results.DR <- withCallingHandlers(
        TADA_BigDataHelper(
          record_summary = bbox_avail %>%
            dplyr::select(MonitoringLocationIdentifier, resultCount) %>%
            dplyr::filter(MonitoringLocationIdentifier %in% clipped_site_ids),
          WQPquery = WQPquery,
          maxrecs = maxrecs,
          maxsites = 300
        ),
        message = function(m) message(m$message)
      )


      rm(bbox_avail, bbox_sites_sf)
      gc()

      # Check if any results were returned
      if ((nrow(results.DR) > 0) == FALSE) {
        print(
          paste0(
            "Returning empty results dataframe: ",
            "Your WQP query returned no results (no data available). ",
            "Try a different query. ",
            "Removing some of your query filters OR broadening your search area may help."
          )
        )
        # Empty
        TADAprofile.clean <- results.DR
      } else {
        # Get site metadata
        sites.DR <- clipped_sites_sf %>%
          dplyr::as_tibble() %>%
          dplyr::select(-geometry)

        # Get project metadata
        projects.DR <- suppressMessages(
          dataRetrieval::readWQPdata(
            siteid = clipped_site_ids,
            WQPquery,
            ignore_attributes = TRUE,
            service = "Project"
          )
        )

        # Join results, sites, projects
        TADAprofile <- TADA_JoinWQPProfiles(
          FullPhysChem = results.DR,
          Sites = sites.DR,
          Projects = projects.DR
        ) %>% dplyr::mutate(
          dplyr::across(tidyselect::everything(), as.character)
        )

        # run TADA_AutoClean function
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")

          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        } else {
          TADAprofile.clean <- TADAprofile
        }
      }

      return(TADAprofile.clean)

      # Doesn't meet "big data" threshold:
    } else {
      # Retrieve all 3 profiles
      print("Downloading WQP query results. This may take some time depending upon the query size.")
      print(WQPquery)

      # Get results
      results.DR <- suppressMessages(
        dataRetrieval::readWQPdata(
          siteid = clipped_site_ids,
          WQPquery,
          dataProfile = "resultPhysChem",
          ignore_attributes = TRUE
        )
      )

      # Check if any results were returned
      if ((nrow(results.DR) > 0) == FALSE) {
        paste0(
          "Returning empty results dataframe: ",
          "Your WQP query returned no results (no data available). ",
          "Try a different query. ",
          "Removing some of your query filters OR broadening your search area may help."
        )
        TADAprofile.clean <- results.DR
      } else {
        # Get site metadata
        sites.DR <- clipped_sites_sf %>%
          dplyr::as_tibble() %>%
          dplyr::select(-geometry)

        # Get project metadata
        projects.DR <- suppressMessages(
          dataRetrieval::readWQPdata(
            siteid = clipped_site_ids,
            WQPquery,
            ignore_attributes = TRUE,
            service = "Project"
          )
        )

        # Join results, sites, projects
        TADAprofile <- TADA_JoinWQPProfiles(
          FullPhysChem = results.DR,
          Sites = sites.DR,
          Projects = projects.DR
        ) %>% dplyr::mutate(
          dplyr::across(tidyselect::everything(), as.character)
        )

        # Run TADA_AutoClean function
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")

          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        } else {
          TADAprofile.clean <- TADAprofile
        }
      }

      return(TADAprofile.clean)
    }

    # If no sf object provided:
  } else {
    # Set query parameters
    WQPquery <- list()

    if (!"null" %in% statecode) {
      load(system.file("extdata", "statecodes_df.Rdata", package = "EPATADA"))
      statecode <- as.character(statecode)
      statecodes_sub <- statecodes_df %>% dplyr::filter(STUSAB %in% statecode)
      statecd <- paste0("US:", statecodes_sub$STATE)
      if (nrow(statecodes_sub) == 0) {
        stop("State code is not valid. Check FIPS state/territory abbreviations.")
      }
      if (length(statecode) >= 1) {
        WQPquery <- c(WQPquery, statecode = list(statecd))
      }
    }

    if (length(huc) > 1) {
      WQPquery <- c(WQPquery, huc = list(huc))
    } else if (huc != "null") {
      WQPquery <- c(WQPquery, huc = huc)
    }

    if (length(startDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = list(startDate))
    } else if (startDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = startDate)
    }

    if (length(countrycode) > 1) {
      WQPquery <- c(WQPquery, countrycode = list(countrycode))
    } else if (countrycode != "null") {
      WQPquery <- c(WQPquery, countrycode = countrycode)
    }

    if (length(countycode) > 1) {
      WQPquery <- c(WQPquery, countycode = list(countycode))
    } else if (countycode != "null") {
      WQPquery <- c(WQPquery, countycode = countycode)
    }

    if (length(siteid) > 1) {
      WQPquery <- c(WQPquery, siteid = list(siteid))
    } else if (siteid != "null") {
      WQPquery <- c(WQPquery, siteid = siteid)
    }

    if (length(siteType) > 1) {
      WQPquery <- c(WQPquery, siteType = list(siteType))
    } else if (siteType != "null") {
      WQPquery <- c(WQPquery, siteType = siteType)
    }

    if (length(characteristicName) > 1) {
      WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
    } else if (characteristicName != "null") {
      WQPquery <- c(WQPquery, characteristicName = characteristicName)
    }

    if (length(characteristicType) > 1) {
      WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
    } else if (characteristicType != "null") {
      WQPquery <- c(WQPquery, characteristicType = characteristicType)
    }

    if (length(sampleMedia) > 1) {
      WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
    } else if (sampleMedia != "null") {
      WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
    }

    if (length(project) > 1) {
      WQPquery <- c(WQPquery, project = list(project))
    } else if (project != "null") {
      WQPquery <- c(WQPquery, project = project)
    }

    if (length(providers) > 1) {
      WQPquery <- c(WQPquery, providers = list(providers))
    } else if (providers != "null") {
      WQPquery <- c(WQPquery, providers = providers)
    }

    if (length(organization) > 1) {
      WQPquery <- c(WQPquery, organization = list(organization))
    } else if (organization != "null") {
      WQPquery <- c(WQPquery, organization = organization)
    }

    if (length(endDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = list(endDate))
    } else if (endDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = endDate)
    }

    # Query info on available data
    message("Checking what data is available. This may take a moment.")
    
    # Don't want to print every message that's returned by WQP
    quiet_whatWQPdata <- purrr::quietly(dataRetrieval::whatWQPdata)

    quiet_query_avail <- quiet_whatWQPdata(WQPquery)

    if (is.null(quiet_query_avail$result)) {
      stop_message <- quiet_query_avail$messages %>%
        grep(pattern = "failed|HTTP", x = ., ignore.case = FALSE, value = TRUE) %>%
        paste("\n", ., collapse = "") %>%
        paste("The WQP request returned a NULL with the following message(s): \n",
          .,
          collapse = "\n"
        )

      stop(stop_message)
    }

    query_avail <- quiet_query_avail$result

    site_count <- length(query_avail$MonitoringLocationIdentifier)

    record_count <- query_avail %>%
      dplyr::pull(resultCount) %>%
      sum()

    # Should we proceed with downloads? If ask == TRUE then ask the user.
    if (ask == TRUE) {
      user_decision <- ask_user(n_records = record_count)

      # Act on input
      if (user_decision == "yes") {
        print("Proceeding with download.")
      } else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }

    # Check for either more than 300 sites or more records than max_recs.
    # If either is true then we'll approach the pull as a "big data" pull
    if (site_count > 300 | record_count > maxrecs) {
      message(
        "The number of sites and/or records matched by the query terms is large, so the download may take some time."
      )

      # Use helper function to download large data volume
      results.DR <- suppressMessages(
        TADA_BigDataHelper(
          record_summary = query_avail %>%
            dplyr::select(MonitoringLocationIdentifier, resultCount),
          WQPquery = WQPquery,
          maxrecs = maxrecs,
          maxsites = 300
        )
      )

      rm(query_avail)
      gc()

      # Get site metadata
      sites.DR <- suppressMessages(
        dataRetrieval::whatWQPsites(
          siteid = unique(results.DR$MonitoringLocationIdentifier)
        )
      )

      # Get project metadata
      projects.DR <- suppressMessages(
        dataRetrieval::readWQPdata(
          siteid = unique(results.DR$MonitoringLocationIdentifier),
          WQPquery,
          ignore_attributes = TRUE,
          service = "Project"
        )
      )

      # Join results, sites, projects
      TADAprofile <- TADA_JoinWQPProfiles(
        FullPhysChem = results.DR,
        Sites = sites.DR,
        Projects = projects.DR
      ) %>% dplyr::mutate(
        dplyr::across(tidyselect::everything(), as.character)
      )

      # run TADA_AutoClean function
      if (applyautoclean == TRUE) {
        print("Data successfully downloaded. Running TADA_AutoClean function.")

        TADAprofile.clean <- TADA_AutoClean(TADAprofile)
      } else {
        TADAprofile.clean <- TADAprofile
      }

      return(TADAprofile.clean)

      # If not a "big data" pull:
    } else {
      # Retrieve all 3 profiles
      print("Downloading WQP query results. This may take some time depending upon the query size.")
      print(WQPquery)
      results.DR <- suppressMessages(
        dataRetrieval::readWQPdata(WQPquery,
          dataProfile = "resultPhysChem",
          ignore_attributes = TRUE
        )
      )

      # check if any results are available
      if ((nrow(results.DR) > 0) == FALSE) {
        print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
        TADAprofile.clean <- results.DR
      } else {
        sites.DR <- suppressMessages(dataRetrieval::whatWQPsites(WQPquery))

        projects.DR <- suppressMessages(
          dataRetrieval::readWQPdata(WQPquery,
            ignore_attributes = TRUE,
            service = "Project"
          )
        )

        TADAprofile <- TADA_JoinWQPProfiles(
          FullPhysChem = results.DR,
          Sites = sites.DR,
          Projects = projects.DR
        ) %>% dplyr::mutate(
          dplyr::across(tidyselect::everything(), as.character)
        )

        # run TADA_AutoClean function
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")

          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        } else {
          TADAprofile.clean <- TADAprofile
        }
      }

      return(TADAprofile.clean)
    }
  }
}

#' Access options available for querying tribal spatial data with `TADA_DataRetrieval()`.
#'
#' @description
#' This function provides access to [six layer datasets](https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer)
#' containing spatial data related to tribal lands: "Alaska Native Allotments",
#' "Alaska Native Villages", "American Indian Reservations", "Off-reservation Trust Lands",
#' "Oklahoma Tribal Statistical Areas", and "Virginia Federally Recognized Tribes".
#' These datasets are used by `TADA_DataRetrieval()` when retrieving spatial data
#' for tribal lands specified by the user.
#'
#' The purpose of `TADA_TribalOptions()` is to allow the user to review the available
#' data in those datasets and identify the records they would like to query with
#' `TADA_DataRetrieval()`.
#'
#' An interactive map of the six layer datasets is available on ArcGIS Online Map
#' Viewer here: https://www.arcgis.com/apps/mapviewer/index.html?url=https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer&source=sd
#'
#' @param tribal_area_type A character string. Must be one of the six tribal
#' spatial layers: "Alaska Native Allotments", "Alaska Native Villages",
#' "American Indian Reservations", "Off-reservation Trust Lands",
#' "Oklahoma Tribal Statistical Areas", or "Virginia Federally Recognized Tribes".
#'
#' @param return_sf Logical. Should the function return the dataset as an `sf`
#' object (TRUE) or a data frame (FALSE)? Defaults to FALSE.
#'
#' @returns A data frame or `sf` object containing the specified layer from the EPA
#' Map Service.
#'
#' @note
#' Alaska Native Villages and Virginia Federally Recognized Tribes are point
#' geometries in the Map Service, not polygons. At the time of this writing they
#' do not return any data when used for WQP bbox queries.
#'
#' @export
#'
#' @seealso [TADA_DataRetrieval()]
#'

TADA_TribalOptions <- function(tribal_area_type, return_sf = FALSE) {
  # Make a reference table for tribal area type + url matching
  map_service_urls <- tibble::tribble(
    ~tribal_area,                            ~url,
    "Alaska Native Allotments",              "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0",
    "Alaska Native Villages",                "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1",
    "American Indian Reservations",          "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2",
    "Off-reservation Trust Lands",           "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3",
    "Oklahoma Tribal Statistical Areas",     "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4",
    "Virginia Federally Recognized Tribes",  "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5"
  )

  # Confirm usable string provided
  if (!(tribal_area_type %in% map_service_urls$tribal_area)) {
    stop("tribal_area_type must match one of the six tribal spatial layer names.")
  }

  # Query Map Service
  tribal_area_sf <- dplyr::filter(
    map_service_urls,
    tribal_area == tribal_area_type
  )$url %>%
    arcgislayers::arc_open() %>%
    # Return sf
    arcgislayers::arc_select() %>%
    sf::st_make_valid()

  # Convert to df if needed, export
  if (return_sf == FALSE) {
    return(
      tribal_area_sf %>%
        sf::st_drop_geometry()
    )
  } else {
    return(tribal_area_sf)
  }
}

#' Read in WQP data using the Water Quality Portal (WQP) web services
#'
#' Go to the WQP website (https://www.waterqualitydata.us/) and fill out the
#' advanced query form. Choose the file format Comma-Separated.
#' Then, choose a data profile. When finished, do not hit the download button.
#' Instead, copy the web service URL located at the bottom of the page under
#' the header "Station" or "Result". This is the url in the second box from the
#' top. Use that web service URL as the input for this function to download
#' data directly into R.
#'
#' We recommend retrieving data for all the following profiles
#' (you can run this function three separate times to bring in all three profiles):
#' 1. Sample Results (physical/chemical metadata)
#' 2. Project Data
#' 3. Site Data Only
#'
#' After you retrieve all three profiles, you can use TADA_JoinWQPProfiles to
#' join the three dataframes into a single dataframe.
#'
#' Note: It may be useful to save the Query URL from the WQP as well as a
#' comment within your code. This URL let's you return to the WQP query page
#' with all your selected data filters. For example, this is the query used
#' in the examples for this function:
#' https://www.waterqualitydata.us/#statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&dataProfile=biological&providers=NWIS&providers=STEWARDS&providers=STORET
#'
#' **Extra tip:** Note that the web service call built using the Water
#' Quality Portal uses the inputs startDateLo and startDateHi rather than
#' startDate and endDate, and dates are in the format MM-DD-YYYY rather
#' than the TADA_DataRetrieval and dataRetrieval format of YYYY-MM-DD. The
#' functions use the latter format rather than the web service call date
#' format because YYYY-MM-DD is a more easily utilized format in the R
#' coding environment. However, users of USGS's dataRetrieval may use the
#' date format MM-DD-YYYY *only if* they specify with "startDateLo" and
#' "startDateHi" inputs. For coding consistency, it is recommended users
#' stick with YYYY-MM-DD.
#'
#' @param webservice WQP Web Service URL, entered within quotes, i.e., "webserviceurl"
#'
#' @return WQP Data Profile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' physchemresults1 <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&dataProfile=biological&providers=NWIS&providers=STEWARDS&providers=STORET")
#' sites1 <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' projects1 <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Project/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' }
#'
TADA_ReadWQPWebServices <- function(webservice) {
  # consider function dataRetrieval::getWebServiceData
  # read in csv from WQP web service
  if (grepl("zip=yes", webservice)) {
    webservice <- stringr::str_replace(webservice, "zip=yes", "zip=no")

    # download data
    webservice <- data.table::fread(toString(webservice))

    # if input df was not downloaded using USGS's dataRetrieval, then the
    # column names will include / separators instead of . and TADA uses .
    # (e.g. ResultMeasure/MeasureUnitCode vs. ResultMeasure.MeasureUnitCode)
    colnames(webservice) <- gsub("/", ".", colnames(webservice))

    return(webservice)
  } else {
    # download data
    webservice <- data.table::fread(toString(webservice))

    # if input df was not downloaded using USGS's dataRetrieval, then the
    # column names will include / separators instead of . and TADA uses .
    # (e.g. ResultMeasure/MeasureUnitCode vs. ResultMeasure.MeasureUnitCode)
    colnames(webservice) <- gsub("/", ".", colnames(webservice))

    return(webservice)
  }
}

#' Assist with large WQP data pulls using dataRetrieval
#'
#' This is a helper function that takes large WQP (waterqualitydata.us) queries
#' and splits them up into multiple, smaller queries. By default it pulls data
#' for up to 300 sites or 250,000 data records at a time and then joins the separate
#' pulls back together to produce a single TADA compatible dataframe as the output.
#' Computer memory may limit the size of data frames that your R console will
#' be able to hold in one session.
#'
#' @param record_summary MonitoringLocationIdentifier and resultCount columns from the output of `dataRetrieval::whatWQPdata` for the WQP query being used.
#' @param WQPquery A named list of query terms to supply dataRetrieval functions.
#' @param maxrecs Maximum number of records to query at once.
#' @param maxsites Maximum number of sites to query at once.
#'
#' @return TADA-compatible dataframe
TADA_BigDataHelper <- function(record_summary, WQPquery, maxrecs = 250000, maxsites = 300) {
  # Get total number of results per site and separate out sites with >maxrecs results
  tot_sites <- record_summary %>%
    dplyr::group_by(MonitoringLocationIdentifier) %>%
    dplyr::summarise(tot_n = sum(resultCount)) %>%
    dplyr::filter(tot_n > 0) %>%
    dplyr::arrange(tot_n)

  # Sites with less than/equal to maxrecs
  smallsites <- tot_sites %>% dplyr::filter(tot_n <= maxrecs)
  # Sites with more than maxrecs
  bigsites <- tot_sites %>% dplyr::filter(tot_n > maxrecs)

  df_small <- data.frame()
  df_big <- data.frame()

  # Work with small sites first:
  # Build download groups. Total record count limited to value of maxrecs.
  # Number of sites per download group limited to 300.
  if (dim(smallsites)[1] > 0) {
    smallsitesgrp <- smallsites %>%
      dplyr::mutate(group = MESS::cumsumbinning(
        x = tot_n,
        threshold = maxrecs,
        maxgroupsize = 300
      ))

    # Status update to user
    print(
      paste0(
        "Downloading data from sites with fewer than ",
        maxrecs,
        " results by grouping them together."
      )
    )

    small_prog_bar <- txtProgressBar(min = 0, max = sum(smallsites$tot_n), style = 3)

    # Download the data for each group
    for (i in 1:max(smallsitesgrp$group)) {
      small_site_chunk <- subset(
        smallsitesgrp$MonitoringLocationIdentifier,
        smallsitesgrp$group == i
      )
      # Query result data
      results_small <- suppressMessages(
        dataRetrieval::readWQPdata(
          siteid = small_site_chunk,
          WQPquery,
          dataProfile = "resultPhysChem",
          ignore_attributes = TRUE
        )
      ) %>%
        dplyr::mutate(dplyr::across(everything(), as.character))

      # If data is returned, stack with what's already been retrieved
      if (dim(results_small)[1] > 0) {
        df_small <- dplyr::bind_rows(df_small, results_small)
      }

      # Update progress
      setTxtProgressBar(pb = small_prog_bar, value = nrow(df_small))
    }
    # Close progress bar when complete
    close(small_prog_bar)

    rm(smallsites, smallsitesgrp)
    gc()
  }

  # Large sites (>= maxrecs) next:
  if (dim(bigsites)[1] > 0) {
    print(
      paste0(
        "Downloading data from sites with greater than ",
        maxrecs,
        " results, chunking queries by site."
      )
    )

    big_prog_bar <- txtProgressBar(min = 0, max = sum(bigsites$tot_n), style = 3)

    # Unique site IDs
    bsitesvec <- unique(bigsites$MonitoringLocationIdentifier)

    # For each site
    for (i in 1:length(bsitesvec)) {
      # Download each site's data individually
      results_big <- suppressMessages(
        dataRetrieval::readWQPdata(
          siteid = bsitesvec[i],
          WQPquery,
          dataProfile = "resultPhysChem",
          ignore_attributes = TRUE
        )
      ) %>%
        dplyr::mutate(dplyr::across(everything(), as.character))

      if (dim(results_big)[1] > 0) {
        df_big <- dplyr::bind_rows(df_big, results_big)
      }
      # Update progress
      setTxtProgressBar(pb = big_prog_bar, value = nrow(df_big))
    }
    # Close progress bar when complete
    close(big_prog_bar)

    rm(bigsites)
    gc()
  }


  df_out <- dplyr::bind_rows(df_small, df_big)

  return(df_out)
}


#' Join WQP Profiles
#'
#' After retrieving multiple result and metadata profiles from the WQP, this
#' function joins those profiles together into one dataframe.
#' The FullPhysChem data input is required to run this function.
#'
#' @param FullPhysChem Full physical chemical data profile
#' @param Sites Sites data profile
#' @param Projects Projects data profile
#'
#' @return TADA-compatible dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load WQP data
#' # WQP URL: https://www.waterqualitydata.us/#statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET
#' # Use TADA_ReadWQPWebServices to load each profile
#' stationProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' physchemProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&dataProfile=resultPhysChem&providers=NWIS&providers=STEWARDS&providers=STORET")
#' projectProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Project/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#'
#' # Join all three profiles using TADA_JoinWQPProfiles
#' TADAProfile <- TADA_JoinWQPProfiles(FullPhysChem = physchemProfile, Sites = stationProfile, Projects = projectProfile)
#' }
#'
TADA_JoinWQPProfiles <- function(FullPhysChem = "null",
                                 Sites = "null",
                                 Projects = "null") {
  FullPhysChem.df <- FullPhysChem

  Sites.df <- Sites

  Projects.df <- Projects

  # Join station data to full phys/chem (FullPhysChem.df)
  if (length(Sites.df > 1)) {
    if (nrow(Sites.df) > 0) {
      join1 <- FullPhysChem.df %>%
        # join stations to results
        dplyr::left_join(Sites.df,
          by = "MonitoringLocationIdentifier",
          multiple = "all",
          relationship = "many-to-many"
        ) %>%
        # remove ".x" suffix from column names
        dplyr::rename_at(dplyr::vars(dplyr::ends_with(".x")), ~ stringr::str_replace(., "\\..$", "")) %>%
        # remove columns with ".y" suffix
        dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    } else {
      join1 <- FullPhysChem.df
    }
  } else {
    join1 <- FullPhysChem.df
  }


  # Add QAPP columns from project
  if (length(Projects.df) > 1) {
    if (nrow(Projects.df) > 0) {
      join2 <- join1 %>%
        dplyr::left_join(
          dplyr::select(
            Projects.df, OrganizationIdentifier, OrganizationFormalName,
            ProjectIdentifier, ProjectName, ProjectDescriptionText,
            SamplingDesignTypeCode, QAPPApprovedIndicator, QAPPApprovalAgencyName,
            ProjectFileUrl, ProjectMonitoringLocationWeightingUrl
          ),
          by = c(
            "OrganizationIdentifier", "OrganizationFormalName",
            "ProjectIdentifier", "ProjectName"
          ),
          multiple = "all",
          # need to specify that this is expected to be a 1-to-many relationship
          relationship = "many-to-many"
        )
    } else {
      join2 <- join1
    }
  } else {
    join2 <- join1
  }

  return(join2)
}

#' Ask user to approve WQP downloads
#'
#' Once record counts have been retrieved from the Water Quality Portal (WQP) for
#' a query, this function is used to prompt the user to decide (i.e., "yes"/"no")
#' whether the download should proceed. The user is also reminded of the limits of
#' Microsoft Excel for row counts as a comparison.
#'
#' @param n_records A numeric value indicating the number of records that will be downloaded from the WQP if the user decides to proceed.
ask_user <- function(n_records) {
  # Text to show user
  user_prompt <- cat(
    "Your WQP query will return ",
    n_records,
    " records.\nFor reference, Microsoft Excel will only display ~one million.\n",
    "Would you like to continue with the download? [yes/no] ",
    sep = ""
  )

  # Ask user if they want to continue & check for valid response
  while (TRUE) {
    user_input <- readline(prompt = user_prompt)
    # Convert response to lower and no whitespace
    user_input <- tolower(trimws(user_input))
    if (user_input == "yes" || user_input == "no") {
      return(user_input)
    } else {
      cat("Invalid input. Please enter 'yes' or 'no'.\n")
    }
  }
}


# function for chunking by records
make_groups <- function(x, maxrecs) {
  if (sum(x$tot_n) <= maxrecs | dim(x)[1] == 1) { # WARNING: if there's only one row and it's more than maxrecs, it will try to run the query anyway
    groupings <- x
    groupings$group <- 1
  } else {
    groupings <- data.frame()
    group <- data.frame()
    i <- 1
    while (nrow(x) > nrow(group)) {
      x$csum <- cumsum(x$tot_n)
      brk <- which(x$csum > maxrecs)[1]
      group <- x[1:(brk - 1), ]
      group$group <- i
      if (brk > 1) {
        x <- x[brk:length(x$tot_n), ]
      } else {
        x <- x[2:length(x$tot_n), ]
      }
      i <- i + 1
      groupings <- plyr::rbind.fill(groupings, group)
    }

    x$group <- i

    groupings <- plyr::rbind.fill(groupings, x)
  }
  return(groupings)
}
