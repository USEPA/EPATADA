#' Used to store cached WQX QAQC Characteristic Validation Reference Table
WQXCharValRef_Cached <- NULL



#' WQX QAQC Characteristic Validation Reference Table
#'
#' Function downloads and returns the newest available (cleaned)
#' raw Water Quality Exchange (WQX) QAQC Characteristic
#' Validation reference table. The WQXcharValRef data frame
#' contains information for four functions: InvalidFraction, InvalidResultUnit,
#' InvalidSpeciation, and UncommonAnalyticalMethodID.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated WQXcharValRef object
#'

GetWQXCharValRef <- function() {

  # If there is a cached table available return it
  if (!is.null(WQXCharValRef_Cached)) {
    return(WQXCharValRef_Cached)
  }

  # Try to download up-to-date raw data
  raw.data <- tryCatch({
    # read raw csv from url
    utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV"))
  }, error = function(err) {
    NULL
  })

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message('Downloading latest Validation Reference Table failed!')
    message('Falling back to (possibly outdated) internal file.')
    return(utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "TADA")))
  }
  
  # filter data to include only accepted (valid) values and remove extraneous columns
  WQXcharValRef <- raw.data %>%
    dplyr::select(-c(
      "Domain", "Unique.Identifier", "Note.Recommendation",
      "Last.Change.Date"
    ))
  # replace "Status" values with Valid, Invalid, Unknown
  WQXcharValRef["Status"][WQXcharValRef["Status"] == "Accepted"] <- "Valid"
  WQXcharValRef["Status"][WQXcharValRef["Status"] == "Rejected"] <- "Invalid"
  WQXcharValRef["Status"][WQXcharValRef["Status"] == "Nonstandardized" |
    WQXcharValRef["Status"] == "InvalidMediaUnit" |
    WQXcharValRef["Status"] == "InvalidChar" |
    WQXcharValRef["Status"] == "MethodNeeded"] <- "Nonstandardized"

  # Save updated table in cache
  WQXCharValRef_Cached <- WQXcharValRef

  WQXcharValRef
}

#' Update Characteristic Validation Reference Table internal file 
#' (for internal use only)

UpdateWQXCharValRef <- function() {
  utils::write.csv(GetWQXCharValRef(), file = "inst/extdata/WQXcharValRef.csv", row.names = FALSE)
}


#' Used to store cached Measure Unit Reference Table

WQXunitRef_Cached <- NULL

#' Update Measure Unit Reference Table
#'
#' Function downloads and returns in the latest WQX MeasureUnit Domain table, 
#' adds additional target unit information, and writes the data to sysdata.rda.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return sysdata.rda with updated WQXunitRef object (unit conversion reference
#' table)
#'

GetMeasureUnitRef <- function() {

  # If there is a cached table available return it
  if (!is.null(WQXunitRef_Cached)) {
    return(WQXunitRef_Cached)
  }

  # Try to download up-to-date raw data
  raw.data <- tryCatch({
    # read raw csv from url
    utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"))
  }, error = function(err) {
    NULL
  })

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message('Downloading latest Measure Unit Reference Table failed!')
    message('Falling back to (possibly outdated) internal file.')
    return(utils::read.csv(system.file("extdata", "WQXunitRef.csv", package = "TADA")))
  }

  WQXunitRef <- raw.data
  # add m and ft as target units for "Length Distance" (Description field) rows
  # target.unit = m
  target.m <- data.frame(
    Domain = rep("Measurement Unit(MeasureUnitCode)", 13),
    Unique.Identifier = rep(NA, 13),
    Code = c(
      "Angst", "cm", "dm", "feet", "ft", "in",
      "km", "m", "mi", "mm", "nm", "nmi", "yd"
    ),
    Description = c(
      "Length Distance, Angstroms",
      "Length Distance, Centimeters",
      "Length Distance, Decimeters",
      "Length Distance, Feet",
      "Length Distance, Feet",
      "Length Distance, Inches",
      "Length Distance, Kilometers",
      "Length Distance, Meters",
      "Length Distance, Miles",
      "Length Distance, Millimeters",
      "Length Distance, Nanometers",
      "Length Distance, Nautical miles",
      "Length Distance, Yards"
    ),
    Last.Change.Date = rep("3/24/2022 12:00:00 PM", 13),
    Target.Unit = rep("m", 13),
    Conversion.Factor = c(
      1e-10, 0.01, 0.1, 0.3048, 0.3048,
      0.0254, 1000, 1, 1609.34, 0.001,
      1e-9, 1825, 0.9144
    ),
    Conversion.Coefficient = rep(0, 13)
  )
  # target.unit = ft
  target.ft <- data.frame(
    Domain = rep("Measurement Unit(MeasureUnitCode)", 13),
    Unique.Identifier = rep(NA, 13),
    Code = c(
      "Angst", "cm", "dm", "feet", "ft", "in",
      "km", "m", "mi", "mm", "nm", "nmi", "yd"
    ),
    Description = c(
      "Length Distance, Angstroms",
      "Length Distance, Centimeters",
      "Length Distance, Decimeters",
      "Length Distance, Feet",
      "Length Distance, Feet",
      "Length Distance, Inches",
      "Length Distance, Kilometers",
      "Length Distance, Meters",
      "Length Distance, Miles",
      "Length Distance, Millimeters",
      "Length Distance, Nanometers",
      "Length Distance, Nautical miles",
      "Length Distance, Yards"
    ),
    Last.Change.Date = rep("3/24/2022 12:00:00 PM", 13),
    Target.Unit = rep("ft", 13),
    Conversion.Factor = c(
      3.28084e-10, 0.0328084, 0.328084,
      1, 1, 0.08333, 3280.84, 3.28084,
      5280, 0.00328084, 3.2808e-9,
      6076.12, 3
    ),
    Conversion.Coefficient = rep(0, 13)
  )
  # add data to WQXunitRef
  WQXunitRef <- plyr::rbind.fill(WQXunitRef, target.m, target.ft)

  # Save updated table in cache
  WQXunitRef_Cached <- WQXunitRef

  WQXunitRef
}

#' Update Measure Unit Reference Table internal file (for internal use only)

UpdateMeasureUnitRef <- function() {
  utils::write.csv(GetMeasureUnitRef(), file = "inst/extdata/WQXunitRef.csv", row.names = FALSE)
}
