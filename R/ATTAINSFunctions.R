#' Import Expert Query National Extract
#'
#' Returns data frame of user-specified Expert Query National Extracts for use in TADA functions.
#' National extracts can and more information about Expert Query can be found here:
#' https://owapps.epa.gov/expertquery/national-downloads
#' 
#' @param profile Character argument. Specifies which Expert Query National Extract should be
#' imported. Options are "actions", "assessments", "au", "auml", "catchment", "sources", and
#' "tmdl". The default is NULL, which means no extract will be returned.
#'
#' @return A data frame containing the user-specified national extract.
#'
#' @export
#'
#' @examples
#' assessments <- TADA_EQExtract(profile = "assessments")
#' 
#' aus_monloc <- TADA_EQExtract(profile = "auml")
#' 
TADA_EQExtract <- function(profile = NULL) {
  
  if(is.null(profile)) {
    stop("TADA_EQExtract: Function requires user to select Expert Query Profile to return.")
  }
  
  if(is.null(profile) & 
     !profile %in% c("actions", "assessments", "au", "auml", "catchment", "sources", "tmdl")) {
    stop("TADA_EQExtract: Function requires user to select Expert Query Profile to return.")
  }
  
  base.url <- "https://cg-7343d0e5-571f-451f-971f-8aaaf971df7e.s3-us-gov-west-1.amazonaws.com/national-downloads/"
  
  latest.json <- jsonlite::fromJSON(paste0(base.url, "latest.json"))
  
  folder.num <- latest.json$julian
  
  date.print <- format(lubridate::as_datetime(folder.num), "%B %d, %Y")
  
  # select profile based on user selection
  # when json is updated, date.print will be determined for each profile below label
  if(profile == "actions") {
    
    file <- "actions.csv.zip"
    
    label <- "Actions Profile"
    
  }
  
  if(profile == "assessments") {
    
    file <- "assessments.csv"
    
    label <- "Assessments Profile"
    
  }
  
  if(profile == "au") {
    
    file <- "assessment_units.csv"
    
    label <- "Assessment Units Profile"
    
  }
  
  if(profile == "auml") {
    
    file <- "assessment_units_monitoring_locations.csv"
    
    label <- "Assessment Units with Monitoring Locations Profile"
    
  }
  
  if(profile == "catchment") {
    
    file <- "catchment_correspondence.csv"
    
    label <- "Catchment Correspondance Profile"
    
  }
  
  if(profile == "sources") {
    
    file <- "sources.csv"
    
    label <- "Sources Profile"
    
  }
  
  if(profile == "tmdl") {
    
    file <- "tmdl.csv"
    
    label <- "Total Maximum Daily Load Profile"
    
  }
  
  print(paste0("TADA_EQExtract: ", label, " (Expert Query National Extract) " , 
               "was last updated on ", date.print, "." ))
  
  url <- paste0(base.url, folder.num, "/", file, ".zip")
  
  # set up tempfile
  temp <- tempfile(fileext = ".zip")
  
  # increase timeout (for large files)
  options(timeout = 1200)
  
  # download zipped file
  httr::GET(url, httr::write_disk(temp, overwrite = TRUE), httr::progress())
  
  #utils::download.file(url, temp, method = "curl", mode = "wb")
  
  #download.file(url, temp)
  
  # unzip file
  unzipped.file <- utils::unzip(temp, exdir = tempdir())
  
  csv.file <- unzip[grep("\\.csv$", unzipped.file)]
  
  df <- read.csv(csv.file)
  
  unlink(temp)
  unlink(unzip.file)
  
  # remove intermediate objects
  rm(url, latest.json, base.url, folder.num, date.print, url)
  
  return(df)
}


# Used to store cached EQ Assessments Reference Table
EQAssessments_Cached <- NULL

#' Open Expert Query National Extract for Assessments
#'
#' Downloads and returns data frame of Expert Query National Extracts for Assessments.
#' National extracts can and more information about Expert Query can be found here:
#' https://owapps.epa.gov/expertquery/national-downloads
#' 
#' @return A data frame containing the assessments national extract.
#'
#' @export
#'
#' @examples
#' assessments <- TADA_EQExtract(profile = "assessments")
#' 
#' 
TADA_GetEQAssessments <- function() {
  # If there is a cached table available return it
  if (!is.null(EQAssessments_Cached)) {
    return(EQAssessments_Cached)
  }
  
  # Try to download up-to-date raw data
  
  raw.data <- tryCatch(
    {
      TADA_EQExtract("assessments")
    },
    error = function(err) {
      NULL
    }
  )
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Expert Query Assessments National Extract failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "EPATADA")))
  }
  
  # Save updated table in cache
  EQAssessments_Cached <- EQAssessments
  
  EQAssessments
  
}

# Update Characteristic Validation Reference Table internal file
# (for internal use only)

TADA_UpdateEQAssessments <- function() {
  raw.data <- TADA_EQExtract("assessments")
  
  save(raw.data, file = "inst/extdata/EQAssessments.RData")
}
