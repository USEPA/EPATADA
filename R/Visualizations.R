#' Generate Animated Map
#'
#' @param .data TADA dataframe
#'
#' @return Animated map
#'

CreateAnimatedMap <- function(.data) { 
  
  #code adapted from USGS blog: https://waterdata.usgs.gov/blog/large_sample_pull/
  
  # pull the year from the date
  .data$year <- format(as.Date(.data$ActivityStartDate, format="%Y-%m-%d"),"%Y")
  
  # create new data frame of summarized result data by year
  n_bysite <- 
    .data %>% 
    dplyr::group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, year) %>% 
    dplyr::summarize(mean = mean(.data$ResultMeasureValue, na.rm = TRUE), 
              median = stats::median(.data$ResultMeasureValue, na.rm = TRUE))
  
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
  
  # first, create the base map data frame
  all_state <- "usa"
  
  usa <- map_data("state", interior=TRUE)
  base_map <- ggplot2::ggplot(data = usa, mapping = aes(x = long, 
                                               y = lat, 
                                               group = group)) +
    ggplot2::geom_polygon(color = "black", fill = "white") +
    ggplot2::coord_quickmap() +
    ggplot2::theme_void() 
  
  # second, plot the base map and add data to it
  map_with_data <- base_map +
    ggplot2::geom_point(data = n_bysite, aes(x = LongitudeMeasure, 
                                    y = LatitudeMeasure, 
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


#' Generate Map
#'
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

GenerateMap <- function(.data, flag = TRUE) { 
  
#create a map of the world
maps::map()

# uncomment below to create USA base map
# maps::map("usa")

# uncomment below to add state outlines
# maps::map("state", add=TRUE)

# uncomment below to add county outlines
# maps::map("county", add=TRUE)

# uncomment below to draw map of specific state
# maps::map('county', 'utah')

# draw the site locations onto the map
graphics::points(.data$LongitudeMeasure, .data$LatitudeMeasure, col="red", pch=20) 

}