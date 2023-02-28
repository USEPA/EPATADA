#' Identify Potential Duplicate Data Uploads
#' 
#' Identifies data records uploaded by different organizations with the same date,
#' time, characteristic name, and result value within X meters of each other and
#' flags as potential duplicates. However, it is at the discretion of the data user
#' to determine if the data records are unique or represent overlap that could cause
#' issues in the data analysis.
#' 
#' @param .data TADA dataframe
#' @param dist_buffer Numeric. The distance in meters below which two sites with 
#' measurements at the same time on the same day of the same parameter will
#' be flagged as potential duplicates.
#' 
#' @return The same TADA dataframe with two additional columns, a duplicate flag
#' column, and a distance between sites (in meters) column.
#' 
#' @export
#' 

identifyPotentialDuplicates <- function(.data, dist_buffer = 100){
  dat = .data
  dups = dat%>%dplyr::filter(!is.na(ResultMeasureValue))%>%dplyr::mutate(roundRV = round(ResultMeasureValue,digits=2))%>%dplyr::group_by(ActivityStartDate, ActivityStartTime.Time, CharacteristicName,ResultMeasureValue)%>%dplyr::summarise(numorgs = length(unique(OrganizationIdentifier)))%>%dplyr::filter(numorgs>1)
  dups$dup_id = seq(1:dim(dups)[1])
  
  tdups = dplyr::left_join(dups, dat)
  tdups$LatitudeMeasure = as.numeric(tdups$LatitudeMeasure)
  tdups$LongitudeMeasure = as.numeric(tdups$LongitudeMeasure)
  
  distances = tdups%>%dplyr::ungroup()%>%dplyr::select(dup_id,LatitudeMeasure,LongitudeMeasure)
  dcoords = sf::st_as_sf(x = distances, coords = c("LongitudeMeasure","LatitudeMeasure"), crs="EPSG:4326")
  
  dists = data.frame()
  for(i in 1:max(dcoords$dup_id)){
    ds = subset(dcoords, dcoords$dup_id==i)
    dist = as.numeric(sf::st_distance(ds$geometry[1],ds$geometry[2]))
    dsdist = data.frame(dup_id = i, distance_m = as.numeric(dist))
    dsdist$TADA.idPotentialDuplicates.Flag = ifelse(dist<=dist_buffer,"POTENTIAL DUPLICATE DATAPOINT",NA)
    dists = rbind(dists, dsdist)
  }
  
  tdups1 = merge(tdups, dists, all.x = TRUE)
  tdups1 = tdups1[,!names(tdups1)%in%c("dup_id","numorgs")]
  dat1 = merge(dat, tdups1, all.x = TRUE)
  
  return(dat1)
}

