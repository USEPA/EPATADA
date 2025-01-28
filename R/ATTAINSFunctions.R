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
#' assessments <- TADA_EQExtractRetrieval(profile = "assessments")
#'
#' aus_monloc <- TADA_EQExtractRetrieval(profile = "auml")
#'
TADA_EQExtractRetrieval <- function(profile = NULL) {
  if (is.null(profile)) {
    stop("TADA_EQExtract: Function requires user to select Expert Query Profile to return.")
  }

  if (is.null(profile) &
    !profile %in% c("actions", "assessments", "au", "auml", "catchment", "sources", "tmdl")) {
    stop("TADA_EQExtract: Function requires user to select Expert Query Profile to return.")
  }

  base.url <- "https://cg-7343d0e5-571f-451f-971f-8aaaf971df7e.s3-us-gov-west-1.amazonaws.com/national-downloads/"

  latest.json <- jsonlite::fromJSON(paste0(base.url, "latest.json"))

  folder.num <- latest.json$julian

  date.print <- format(lubridate::as_datetime(folder.num), "%B %d, %Y")

  # select profile based on user selection
  # when json is updated, date.print will be determined for each profile below label
  if (profile == "actions") {
    file <- "actions.csv.zip"

    label <- "Actions Profile"
  }

  if (profile == "assessments") {
    file <- "assessments.csv"

    label <- "Assessments Profile"
  }

  if (profile == "au") {
    file <- "assessment_units.csv"

    label <- "Assessment Units Profile"
  }

  if (profile == "auml") {
    file <- "assessment_units_monitoring_locations.csv"

    label <- "Assessment Units with Monitoring Locations Profile"
  }

  if (profile == "catchment") {
    file <- "catchment_correspondence.csv"

    label <- "Catchment Correspondance Profile"
  }

  if (profile == "sources") {
    file <- "sources.csv"

    label <- "Sources Profile"
  }

  if (profile == "tmdl") {
    file <- "tmdl.csv"

    label <- "Total Maximum Daily Load Profile"
  }

  print(paste0(
    "TADA_EQExtract: downloading ", label, " (Expert Query National Extract).",
    " It was last updated on ", date.print, "."
  ))

  url <- paste0(base.url, folder.num, "/", file, ".zip")

  # set up tempfile
  temp <- tempfile(fileext = ".zip")

  # increase timeout (for large files)
  options(timeout = 1200)

  # download zipped file
 httr::GET(url, httr::write_disk(temp, overwrite = TRUE))
 
 print(paste0(
   "TADA_EQExtract: unzipping ", label, " (Expert Query National Extract)."
 ))

  # unzip file
  unzipped.file <- utils::unzip(temp, exdir = tempdir())

  # identify csv file to read in
  csv.file <- unzipped.file[grep("\\.csv$", unzipped.file, ignore.case = TRUE)]

  print(paste0(
    "TADA_EQExtract: ", "opening ", label, " (Expert Query National Extract).",
    "."
  ))

  # open large csv file
  # can add verbose = FALSE, if we want to remove the progress bar here
  df <- data.table::fread(csv.file)

  unlink(temp)

  unlink(unzipped.file)

  # remove intermediate objects
  rm(
    url, latest.json, base.url, folder.num, date.print, label, file, temp,
    unzipped.file, csv.file
  )

  return(df)
}
