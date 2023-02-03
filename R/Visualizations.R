#' @title Generate Animated Map
#' 
#' @description
#' Animated map code adapted from USGS blog: https://waterdata.usgs.gov/blog/large_sample_pull/
#'
#' @param .data TADA dataframe
#'
#' @return Animated map
#'
#' @export
#' 

CreateAnimatedMap <- function(.data) { 
  
  # code adapted from USGS blog: https://waterdata.usgs.gov/blog/large_sample_pull/
  # reference: https://cran.r-project.org/web/packages/usmap/vignettes/advanced-mapping.html
  # this function relies on the remote github version of usmap https://github.com/pdil/usmap
  
  # pull the year from the date
  .data$year <- base::format(as.Date(.data$ActivityStartDate, format="%Y-%m-%d"),"%Y")
  
  # create new data frame of summarized result data by year
  n_bysite <- 
    .data %>% 
    dplyr::group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, year) %>% 
    dplyr::summarize(mean = mean(.data$ResultMeasureValue, na.rm = TRUE), 
              median = median(.data$ResultMeasureValue, na.rm = TRUE))
  
  # create a new character column with total nitrogen acceptable range designations
  n_bysite <- 
    n_bysite %>% dplyr::mutate(TN_mean=
                               dplyr::case_when(mean<2 ~ "<2 mg/l", 
                               mean>=2 & mean<=6 ~ "2-6 mg/l", 
                               mean>6 ~ ">6 mg/l")) 

  # convert latitude, longitude, and year data to numeric form
  n_bysite$LatitudeMeasure <- as.numeric(n_bysite$LatitudeMeasure)
  n_bysite$LongitudeMeasure <- as.numeric(n_bysite$LongitudeMeasure)
  n_bysite$year <- as.numeric(n_bysite$year)
  
  # plot the base map and add data to it
  base_map <- 
    map_with_data <- usmap::plot_usmap("counties", include = "AK", labels = FALSE) +
    ggplot2::geom_point(data = usmap::usmap_transform(n_bysite, 
                                                      input_names = c("LongitudeMeasure", "LatitudeMeasure"), 
                                                      output_names = c("x", "y")), 
                        aes(x = x, 
                            y = y),
                        color = "black", fill = "white")
  
  # second, plot the base map and add data to it
  map_with_data <- base_map +
    ggplot2::geom_point(data = usmap::usmap_transform(n_bysite, 
                                                      input_names = c("LongitudeMeasure", "LatitudeMeasure"), 
                                                      output_names = c("x", "y")), 
                        aes(x = x, 
                            y = y, 
                            color = TN_mean, 
                            group = year, 
                            frame = year)) +
    gganimate::transition_time(year) +
    ggplot2::ggtitle('Year: {frame_time}', # add year to the title
                     subtitle = 'Frame {frame} of {nframes}') +
    ggplot2::scale_colour_manual(values = c("blue", "red", "green"))
  
    num_years <- max(n_bysite$year)-min(n_bysite$year) + 1 

  # lastly, run the animation
  gganimate::animate(map_with_data, nframes = num_years, fps = 1)
  
  }


#' @title Generate Map
#'
#' @description 
#' Function will plot WQP stations on a map. Stations with an invalid or imprecise
#' latitude or longitude will be colored red; all other stations will be colored 
#' blue. 
#'
#' @param .data TADA dataframe
#' @param flag Boolean argument; set equal to "TRUE" or "FALSE".
#'
#' @return When flag = TRUE, this function colors invalid
#' or imprecise latitude or longitude values red; and colors all other stations blue.
#' When flag = FALSE, all stations are colored blue. The default is flag = TRUE.
#'
#' @export
#' 
#'

GenerateMap <- function(.data, flag = TRUE) { 
  
#create a map of the world
maps::map()

# uncomment below to create USA base map instead
# maps::map("usa")

# uncomment below to add state outlines to usa map
# maps::map("state", add=TRUE)

# uncomment below to add county outlines to usa map
# maps::map("county", add=TRUE)

# uncomment below to draw map of specific state instead, with county lines
# maps::map('county', 'utah')

# draw the site locations onto the map
graphics::points(.data$LongitudeMeasure, .data$LatitudeMeasure, col="red", pch=20) 

}


#' @title Summarize data downloaded for each Characteristic
#' 
#' @description 
#' Function to summarize the number of sites and records downloaded from the 
#' WQP for each CharacteristicName.
#' 
#' @param .data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record.
#' 
#' @return 
#' Saves a .csv file containing the total number of sites and records downloaded
#' from the Water Quality Portal for each requested characteristic name.
#'  
#' @export
#'  

SummarizeCharacteristics <- function(.data){
  
  # Summarize WQP data pull
  wqp_summary <- .data %>%
    dplyr::group_by(CharacteristicName) %>%
    dplyr::summarize(n_sites = length(unique(MonitoringLocationIdentifier)),
                     n_records = length(ResultMeasureValue),
                     .groups = 'drop') %>%
    dplyr::select(CharacteristicName, n_sites, n_records)
  
}
