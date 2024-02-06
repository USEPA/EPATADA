#' TADA_DepthCategory.Flag
#'
#'This function creates a new column, TADA.DepthCategory.Flag with values: "No
#' depth info", "Epilimnion-surface", "Hypolimnion-bottom", and
#' "Metalimnion/Thermocline-middle" For results for with MonitoringLocationTypeName 
#' "Lake, Reservoir, Impoundment", "Great Lakes", "Lake", or "Great Lake". 
#' Categories are: less than 2m depth = "Epilimnion", from bottom up to 2m from
#' bottom= "Hypolimnion-bottom", and the full depth profile for
#' "Metalimnion/Thermocline-middle". For results with other MonitoringLocationTypeNames
#' the TADA.DepthCategory.Flag is "Not Calculated for MonitoringLocationType".
#'
#' When more than one result is available for a MonitoringLocationIdentifier,
#' ActivityStartDate, OrganizationIdentifier, and TADA.CharacteristicName, the
#' user can choose a single result value (average, max, or min value) to use for that
#' day and location. If results vary with depth, the user may also define whether
#' the daily aggregation occurs over each depth category (surface, middle, or bottom)
#' or for the entire depth profile.
#'
#' @param .data TADA dataframe which must include the columns 
#' TADA.ActivityDepthHeightMeasure.MeasureValue, TADA.ResultDepthHeightMeasure.MeasureValue,
#' TADA.ActivityBottomDepthHeightMeasure.MeasureValue, ActivityRelativeDepthName,
#' and MonitoringLocationTypeName.
#'
#' @param dailyagg Character argument; with options "none", "avg", "min", or
#' "max". The default is daily_agg = "none". When daily_agg = "none", all results
#' will be retained. When daily_agg == "avg", the mean value in each group of
#' results (as determined by the depth category) will be identified or calculated for each
#' MonitoringLocation, ActivityDate, Organization ID, and TADA.CharacteristicName combination.
#' When daily_agg == "min" or when daily_agg == "max", the min or max
#' value in each group of results (as determined by the depth category) will
#' be identified or calculated for each MonitoringLocation, ActivityDate, and TADA.CharacteristicName
#' combination. An additional column, TADA.DepthProfileAggregation.Flag will be added
#' to describe aggregation.
#'
#' @param bycategory character argument with options "no", "all", "surface", "middle",
#' "bottom". The default is bycategory = "no" which means that any aggregate values
#' are based on the entire water column at a Monitoring Location. When bycategory
#' = "all", any aggregate values are determined for each depth category for each 
#' Monitoring Location. When bycategory = "surface", "middle", or "bottom", aggregate
#' values are determined ONLY for results with TADA.DepthCategory.Flags
#' "Epilimnion-surface", "Hypolimnion-bottom", or "Metalimnion/Thermocline-middle" 
#' results respectively.
#' 
#' @param bottomvalue numeric argument. The user enters how many meters from the 
#' bottom should be included in the "Hypolimnion-bottom" category. Default is 
#' bottom = 2.
#' 
#' @param surfacevalue numeric argument. The user enters how many meters from the 
#' top should be included in the "Hypolimnion-bottom" category. Default is 
#' top = 2.
#'
#' @param aggregatedonly Boolean argument with options "TRUE" or "FALSE". The 
#' default is aeggrgatedonly = "FALSE" which means that all results are returned.
#' When aggregatedonly = "TRUE", only aggregate values are returned.
#' 
#' @param clean Boolean argument with options "TRUE" or "FALSE". The 
#' default is clean = "FALSE" which means that all results are returned.
#' When clean = "TRUE", only aggregate results which can be assigned to a depth
#' category are included in the returned dataframe.
#'
#' @param .data TADA dataframe
#'
#' @return The same input TADA dataframe with additional columns TADA.DepthCategory.Flag
#' and TADA.DepthProfileAggregation.Flag. If a daily_agg = "avg",
#' "min", or "max", aggregated values will be identified in the TADA.ResultAggregation.Flag
#' column. In the case of daily_agg = "avg", additional rows to display averages will be
#' added to the data frame. They can be identified by the prefix ("TADA-") of
#' their result identifiers.
#'
#' @export
#'
#' @examples
#' # Load dataset
#' data(Data_6Tribes_5y)
#'
#' # assign TADA.DepthCategory.Flag with no aggregation
#' # Data_6Tribs_5y_DepthCat <- TADA_DepthCategory.Flag(Data_6Tribes_5y)
#'
#' # assign TADA.DepthCategory.Flag and determine average values by depth category and returning only aggregate values
#' Data_6Tribs_5y_Mean <- TADA_DepthCategory.Flag(Data_6Tribes_5y, bycategory = "all", dailyagg = "avg", aggregatedonly = FALSE)
#'
TADA_DepthCategory.Flag <- function(.data, bycategory = "no", bottomvalue = 2, surfacevalue = 2, dailyagg = "none", aggregatedonly = FALSE, clean = FALSE) {
  depthcat.list <- c("Epilimnion-surface", "Hypolimnion-bottom", "Metalimnion/Thermocline-middle")
  
  ard.ref <- utils::read.csv(system.file("extdata", "WQXActivityRelativeDepthRef.csv", package = "TADA")) %>%
    dplyr::rename(
      ARD_Category = TADA.DepthCategory.Flag,
      ActivityRelativeDepthName = Name
    ) %>%
    dplyr::select(ARD_Category, ActivityRelativeDepthName)
  
  depth.count <- .data %>%
    dplyr::filter(!is.na(TADA.ActivityDepthHeightMeasure.MeasureValue) |
                    !is.na(TADA.ResultDepthHeightMeasure.MeasureValue)) %>%
    nrow()
  
  length.units <- c("M", "FT", "IN")
  
  depth.params <- 
    
    if (depth.count > 0) {
      print(paste("TADA_DepthCategory.Flag: checking data set for depth values. ", depth.count, " results have depth values available.", sep = ""))
      
      print("TADA_DepthCategory.Flag: assigning depth categories.")
      
      .data <- .data %>%
        # set equal to TADA.ResultDepthHeighMeasure.MeasureValue if available, otherwise use TADA.ActivityDepthHeightMeasure.MeasureValue
        dplyr::mutate(
          TADA.ConsolidatedDepth = ifelse(!is.na(TADA.ResultDepthHeightMeasure.MeasureValue), TADA.ResultDepthHeightMeasure.MeasureValue,
                                          TADA.ActivityDepthHeightMeasure.MeasureValue),
          TADA.ConsolidatedDepth.Unit = ifelse(!is.na(TADA.ResultDepthHeightMeasure.MeasureUnitCode),
                                               TADA.ResultDepthHeightMeasure.MeasureUnitCode, TADA.ActivityDepthHeightMeasure.MeasureUnitCode),
          TADA.ConsolidatedDepth.Unit = toupper(TADA.ConsolidatedDepth.Unit)
        ) %>%
        # use group_by to identify profile data
        dplyr::group_by(ActivityStartDate, MonitoringLocationIdentifier, OrganizationIdentifier) %>%
        # determine the number of Depths per group
        dplyr::mutate(
          DepthsPerGroup = length(unique(TADA.ConsolidatedDepth)),
          # determine bottom value using TADA.ActivityBottomDepthHeightMeasure.MeasureValue or the max depth record for profile data
          TADA.ConsolidatedDepth.Bottom = ifelse(DepthsPerGroup > 1 & is.na(TADA.ActivityBottomDepthHeightMeasure.MeasureValue), max(TADA.ConsolidatedDepth, na.rm = TRUE), TADA.ActivityBottomDepthHeightMeasure.MeasureValue)
        ) %>%
        dplyr::ungroup() %>%
        # assign depth categories by using depth information
        dplyr::mutate(TADA.DepthCategory.Flag = dplyr::case_when(
          TADA.ConsolidatedDepth <= surfacevalue ~ "Epilimnion-surface",
          TADA.ConsolidatedDepth <= TADA.ConsolidatedDepth.Bottom & TADA.ConsolidatedDepth >= TADA.ConsolidatedDepth.Bottom - bottomvalue ~ "Hypolimnion-bottom",
          TADA.ConsolidatedDepth < surfacevalue & TADA.ConsolidatedDepth < TADA.ConsolidatedDepth.Bottom - bottomvalue ~ "Metalimnion/Thermocline-middle"
        )) %>%
        # assign depth categories that could not be assigned using depth
        dplyr::left_join(ard.ref, by = "ActivityRelativeDepthName") %>%
        dplyr::mutate(
          TADA.DepthCategory.Flag = ifelse(is.na(TADA.DepthCategory.Flag), ARD_Category, TADA.DepthCategory.Flag),
          TADA.DepthCategory.Flag = ifelse(is.na(TADA.ActivityDepthHeightMeasure.MeasureValue) & is.na(TADA.ConsolidatedDepth.Bottom) & is.na(TADA.ResultDepthHeightMeasure.MeasureValue) & is.na(TADA.DepthCategory.Flag), "No depth info", TADA.DepthCategory.Flag),
          TADA.DepthCategory.Flag = ifelse(is.na(TADA.DepthCategory.Flag), "Not enough depth info to determine category", TADA.DepthCategory.Flag)
        ) %>%
        dplyr::select(-ARD_Category, -DepthsPerGroup)
      
      if (depth.count == 0) {
        print(paste("TADA_DepthCategory.Flag: checking data set for depth values. No results have depth values available, TADA_DepthCategory.Flag cannot be used on this data set.", sep = ""))
        
        return(.data)
      }
      
    }
  
  if(clean == TRUE) {
    
    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag %in% depthcat.list)
  }
  
  if(clean == FALSE) {
    
    .data <- .data 
  }
  
  if (bycategory == "all") {
    print("TADA_DepthCategory.Flag: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, ActivityStartDate, and TADA.DepthCategory.Flag for aggregation by TADA.DepthCategory.Flag.")
    
    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate",
      "TADA.DepthCategory.Flag"
    )
    
    .data <- .data
  }
  
  if (bycategory == "no") {
    print("TADA_DepthCategory.Flag: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for entire water column.")
    
    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )
    
    .data <- .data
  }
  
  if (bycategory == "surface") {
    print("TADA_DepthCategory.Flag: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for surface samples only.")
    
    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )
    
    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag == "Epilimnion-surface")
  }
  
  if (bycategory == "middle") {
    print("TADA_DepthCategory.Flag: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for middle samples only.")
    
    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )
    
    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag == "Metalimnion/Thermocline-middle")
  }
  
  if (bycategory == "bottom") {
    print("TADA_DepthCategory.Flag: Grouping results by MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for bottom samples only.")
    
    group.list <- c(
      "MonitoringLocationIdentifier", "OrganizationIdentifier",
      "TADA.CharacteristicName", "ActivityStartDate"
    )
    
    .data <- .data %>%
      dplyr::filter(TADA.DepthCategory.Flag == "Hypolimnion-bottom")
  }
  
  if (dailyagg == "none") {
    print("TADA_DepthCategory.Flag: No aggregation performed.")
    
    # add TADA.ResultValue.Aggregation.Flag, remove unecessary columns, and order columns
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, "No aggregation perfomed", "No aggregation needed")) %>%
      dplyr::select(-DepthsByGroup) %>%
      dplyr::ungroup() %>%
      TADA_OrderCols()
    
    if (aggregatedonly == TRUE) {
      stop("Function not executed because clean cannot be TRUE while daily_agg is 'no'")
    }
    
    if (aggregatedonly == FALSE) {
      return(orig.data)
    }
  }
  if ((dailyagg == "avg")) {
    print("TADA_DepthCategory.Flag: Calculating mean aggregate value with randomly selected metadata.")
    
    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, "Used in average aggregation function but not selected ", "No aggregation needed"),
        TADA.DepthProfileAggregation.Flag = ifelse(!TADA.DepthCategory.Flag %in% depthcat.list, "No aggregation needed", TADA.DepthProfileAggregation.Flag)
      ) 
    
    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, calculate mean result value per group, and assign random metadata from group.
    agg.data <- orig.data %>%
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) %>%
      dplyr::mutate(TADA.ResultMeasureValue1 = mean(TADA.ResultMeasureValue, na.rm = TRUE)) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = paste0("Calculated mean aggregate value, with randomly selected metadata from a row in the aggregate group")) %>%
      dplyr::select(-TADA.ResultMeasureValue, -DepthsByGroup) %>%
      dplyr::rename(TADA.ResultMeasureValue = TADA.ResultMeasureValue1) %>%
      dplyr::mutate(ResultIdentifier = paste0("TADA-", ResultIdentifier)) %>%
      dplyr::ungroup()
    
    if (aggregatedonly == TRUE) {
      rm(orig.data)
      
      return(agg.data)
    }
    
    if (aggregatedonly == FALSE) {
      # combine original and aggregate data
      comb.data <- plyr::rbind.fill(orig.data, agg.data) %>%
        dplyr::ungroup() %>%
        dplyr::select(-DepthsByGroup) %>%
        TADA_OrderCols()
      
      rm(agg.data, orig.data)
      
      return(comb.data)
    }
  }
  if ((dailyagg == "min")) {
    print("TADA_DepthCategory.Flag: Selecting minimum aggregate value.")
    
    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(
        TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, "Used in minimum aggregation function but not selected ", "No aggregation needed"),
        TADA.DepthProfileAggregation.Flag = ifelse(!TADA.DepthCategory.Flag %in% depthcat.list, "No aggregation needed", TADA.DepthProfileAggregation.Flag)
      ) 
    
    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, and select minimum result value per group.
    agg.data <- orig.data %>%
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) %>%
      dplyr::slice_min(order_by = TADA.ResultMeasureValue, n = 1, with_ties = FALSE) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = paste0("Selected as min aggregate value")) %>%
      dplyr::select(-DepthsByGroup) %>%
      dplyr::ungroup()
    
    if (aggregatedonly == TRUE) {
      rm(orig.data)
      
      return(agg.data)
    }
    
    if (aggregatedonly == FALSE) {
      # create list of result identifiers for selected aggregate data
      agg.list <- agg.data %>%
        dplyr::ungroup() %>%
        dplyr::select(ResultIdentifier) %>%
        unique() %>%
        dplyr::pull()
      
      # combine original and aggregate data
      comb.data <- orig.data %>%
        dplyr::filter(!ResultIdentifier %in% agg.list) %>%
        plyr::rbind.fill(agg.data) %>%
        dplyr::ungroup() %>%
        dplyr::select(-DepthsByGroup) %>%
        TADA_OrderCols()
      
      rm(agg.data, orig.data, agg.list)
      
      return(comb.data)
    }
  }
  
  if ((dailyagg == "max")) {
    print("TADA_DepthCategory.Flag: Selecting maximum aggregate value.")
    
    # add TADA.ResultValue.Aggregation.Flag and remove unnecessary columns in original data set
    orig.data <- .data %>%
      dplyr::group_by_at(group.list) %>%
      dplyr::mutate(DepthsByGroup = length(unique(TADA.ConsolidatedDepth))) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = ifelse(DepthsByGroup > 1, "Used in maximum aggregation function but not selected ", "No aggregation needed"))
    
    # add TADA.ResultValue.Aggregation.Flag, remove necessary columns, and select maximum result value per group.
    agg.data <- orig.data %>%
      dplyr::filter(
        DepthsByGroup > 1,
        TADA.DepthCategory.Flag %in% depthcat.list
      ) %>%
      dplyr::slice_max(order_by = TADA.ResultMeasureValue, n = 1, with_ties = FALSE) %>%
      dplyr::mutate(TADA.DepthProfileAggregation.Flag = paste0("Selected as max aggregate value")) %>%
      dplyr::mutate(ResultIdentifier = paste0("TADA-", ResultIdentifier)) %>%
      dplyr::select(-DepthsByGroup) %>%
      dplyr::ungroup()
    
    if (aggregatedonly == TRUE) {
      rm(orig.data)
      
      return(agg.data)
    }
    
    if (aggregatedonly == FALSE) {
      # create list of result identifiers for selected aggregate data
      agg.list <- agg.data %>%
        dplyr::ungroup() %>%
        dplyr::select(ResultIdentifier) %>%
        unique() %>%
        dplyr::pull()
      
      # combine original and aggregate data
      comb.data <- orig.data %>%
        dplyr::filter(!ResultIdentifier %in% agg.list) %>%
        plyr::rbind.fill(agg.data) %>%
        dplyr::ungroup() %>%
        dplyr::select(-DepthsByGroup) %>%
        TADA_OrderCols()
      
      rm(agg.data, orig.data, agg.list)
      
      return(comb.data)
    }
  }
}


#' TADA_IDDepthProfiles
#'
#'This function identifies depth profiles within a data frame to assist the user in
#'selecting params for TADA_DepthProfilePlot. A TADA compatible data set is required.
#'If TADA_DepthCategory.Flag has not yet been run, it will be run as part of this 
#'function. The output data frame is grouped by MonitoringLocationIdentifier,
#'OrganizationIdentifier, and ActivityStartDate. 
#'
#'A new column, TADA.CharacteristicsForDepthProfile, is created which lists the
#'characteristics available for depth profile analysis. Using the, nresults param,
#'users can specify whether characteristic names should be followed by the number
#'of results available for the characteristic in parentheses.
#'
#' @param .data TADA dataframe which must include the columns ActivityStartDate,
#' TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Unit, TADA.ConsolidatedDepth.Bottom,
#' TADA.ResultMeasureValue, TADA.ResultMeasureValue.UnitCode,
#' OrganizationIdentifier, MonitoringLocationName, MonitoringLocationIdentifier,
#' and TADA.ComparableDataIdentifier.
#' 
#' @param nresults Boolean argument with options "TRUE" or "FALSE". The 
#' default is nresults = TRUE, which means that the number of results for each
#' characterstic are added within the TADA.CharacteristicsForDepthProfile column.
#' When nresults = FALSE.
#' 
#' @param aggregates Boolean argument with options "TRUE" or "FALSE". The default is
#' aggregates = FALSE, which means that any aggregate values created in
#' TADA_DepthCategory.Flag are excluded from identifying depth profile data. When
#' aggregates = TRUE, aggregate values are included when identifying depth profile data.
#'
#' @return A dataframe with the columns MonitoringLocationIdentifier,
#' MonitoringLocationName, OrganizationIdentifier, ActivityStartDate,
#' TADA.CharacteristicsForDepthProfile. Based on the user input for the nresults
#' param, TADA.CharacteristicsForDepthProfile may or may not contain the number
#' of results for each characteristic.
#'
#' @export
#'
#' @examples
#' # Load dataset
#' data(Data_6Tribes_5y)
#' 
 TADA_IDDepthProfiles <- function(.data, nresults = TRUE, aggregates = FALSE) {
   
   if (aggregates = FALSE) {
     
     .data <- .data %>%
       dplyr::filter(!TADA.DepthProfileAggregation.Flag %in%
                       c("Selected as max aggregate value",
                         "Selected as mean aggregate value",
                         "Calculated mean aggregate value, with randomly selected metadata from a row in the aggregate group"))
   }
   
   if (aggregates = TRUE) {
     
     .data <- .data
   }
   
   if(nresults = TRUE) {

   
   testfunc <- .data %>%
     dplyr::group_by(MonitoringLocationIdentifier, OrganizationIdentifier, ActivityStartDate,
                     TADA.ComparableDataIdentifier) %>%
     dplyr::mutate(TADA.NResults = length(TADA.ResultMeasureValue),
                   TADA.CharacteristicsForDepthProfile = paste(
                     TADA.ComparableDataIdentifier, " (", TADA.NResults, ")", sep = "")) %>%
     dplyr::ungroup() %>%
     dplyr::group_by(MonitoringLocationIdentifier, OrganizationIdentifier, ActivityStartDate) %>%
     dplyr::mutate(TADA.CharacteristicsForDepthProfile = paste(
                    TADA.CharacteristicsForDepthProfile, "; ", sep = ""))
    }
