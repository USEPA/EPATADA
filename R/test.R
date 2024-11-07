TADA_FindNearbySites <- function(.data, dist_buffer = 100) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  
  # .data required columns
  required_cols <- c("MonitoringLocationIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure")
  # check .data has required columns
  TADA_CheckColumns(.data, required_cols)
  
  # create spatial dataset based on sites
  data_sf <- .data %>%
    dplyr::select("MonitoringLocationIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure") %>%
    unique()
  
  # convert to sf object
  data_sf <- sf::st_as_sf(data_sf,
                          coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
                          # Change to your CRS
                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )
  # create a distance matrix in meters
  dist.mat <- data.frame(sf::st_distance(data_sf)) # Great Circle distance since in lat/lon
  
  row.names(dist.mat) <- data_sf$MonitoringLocationIdentifier
  colnames(dist.mat) <- data_sf$MonitoringLocationIdentifier
  
  # convert distances to those within buffer (1) and beyond buffer (0)
  dist.mat1 <- apply(dist.mat, c(1, 2), function(x) {
    if (x <= dist_buffer) {
      x <- 1
    } else {
      x <- 0
    }
  })
  
  # create empty dataframe for groups
  groups <- data.frame()
  
  # loop through distance matrix and extract site groups that are within the buffer distance from one another
  for (i in 1:dim(dist.mat1)[1]) {
    fsite <- rownames(dist.mat1)[i] # focal site
    dat <- data.frame(Count = dist.mat1[i, ]) # get focal site count row as a column
    dat$MonitoringLocationIdentifier <- colnames(dist.mat1) # give df site names along with counts
    sites <- dat$MonitoringLocationIdentifier[dat$Count == 1] # filter to sites within buffer
    sites1 <- sites[!sites %in% fsite] # get site list within buffer that does not include focal site
    if (length(sites1) > 0) { # if this list is greater than 0, combine sites within buffer into data frame
      df <- data.frame(MonitoringLocationIdentifier = sites, TADA.MonitoringLocationIdentifier = paste0(sites, collapse = ","))
      df[c("TADA.MonitoringLocationIdentifier")] <- lapply(df[c("TADA.MonitoringLocationIdentifier")], TADA_FormatDelimitedString)
      groups <- plyr::rbind.fill(groups, df)
    }
  }
  
  # get unique groups (since represented multiple times for each site looped through, above)
  groups <- unique(groups)
  
  if (dim(groups)[1] > 0) {
  # create group id numbers
  group_ids <- groups %>% 
    dplyr::group_by(TADA.MonitoringLocationIdentifier) %>%
    dplyr::mutate(TADA.SiteGroup = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(MonitoringLocationIdentifier) %>%
    dplyr::mutate(TADA.MonitoringLocationIdentifier = paste(TADA.MonitoringLocationIdentifier, collapse = ","),
                  TADA.SiteGroup = paste(TADA.SiteGroup, collapse = ",")) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  }
  
    # find any sites within multiple groups
    summ_sites <- group_ids %>%
      dplyr::group_by(MonitoringLocationIdentifier) %>%
      dplyr::mutate(GroupCount = 1:length(MonitoringLocationIdentifier))
    
    # pivot wider if a site belongs to multiple groups
    groups_prep <- merge(group_ids, summ_sites, all.x = TRUE)
    groups_wide <- tidyr::pivot_wider(groups_prep, id_cols = "MonitoringLocationIdentifier", names_from = "GroupCount", names_prefix = "TADA.MonitoringLocationIdentifier", values_from = "TADA.MonitoringLocationIdentifier")
    ids_wide <- tidyr::pivot_wider(groups_prep, id_cols = "MonitoringLocationIdentifier", names_from = "GroupCount", names_prefix = "TADA.SiteGroup", values_from = "TADA.SiteGroup")
    # merge data to site groupings
    .data <- merge(.data, groups_wide, all.x = TRUE)
    .data <- merge(.data, ids_wide, all.x = TRUE)
  
    # concatenate and move site id cols to right place
    grpcols <- names(.data)[grepl("TADA.MonitoringLocationIdentifier", names(.data))]
    idcols <- names (.data)[grepl("TADA.SIteGroup", names(.data))]
    
    .data <- .data %>% tidyr::unite(col = TADA.MonitoringLocationIdentifier.New, dplyr::all_of(grpcols), sep = ", ", na.rm = TRUE)
    .data <- .data %>% tidyr::unite(col = TADA.SiteGroup, dplyr::all_of(idcols), sep = ", ", na.rm = TRUE)
  
  if (dim(groups)[1] == 0) { # #if no groups, give a TADA.MonitoringLocationIdentifier column filled with NA
    print("No nearby sites detected using input buffer distance.")
  }
  
  # order columns
  if ("ResultIdentifier" %in% names(.data)) {
    .data <- TADA_OrderCols(.data)
  }
  
  return(.data)
}

