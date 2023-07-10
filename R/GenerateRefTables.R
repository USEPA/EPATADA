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
#' @export

TADA_GetWQXCharValRef <- function() {

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
  WQXcharValRef$Status2 = ifelse(WQXcharValRef$Status%in%c("Accepted"),"Valid","Invalid")
  WQXcharValRef$Status2 = ifelse(WQXcharValRef$Status%in%c("NonStandardized",
                                                           "Nonstandardized",
                                                           "InvalidMediaUnit",
                                                           "InvalidChar",
                                                           "MethodNeeded"),"Nonstandardized",WQXcharValRef$Status2)
  
  WQXcharValRef = WQXcharValRef%>%dplyr::select(-Status)%>%dplyr::rename(Status = Status2)%>%dplyr::distinct()

  # Save updated table in cache
  WQXCharValRef_Cached <- WQXcharValRef

  WQXcharValRef
}

#' Update Characteristic Validation Reference Table internal file 
#' (for internal use only)

TADA_UpdateWQXCharValRef <- function() {
  utils::write.csv(TADA_GetWQXCharValRef(), file = "inst/extdata/WQXcharValRef.csv", row.names = FALSE)
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
#' @export
#'

TADA_GetMeasureUnitRef <- function() {

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
  WQXunitRef <- plyr::rbind.fill(WQXunitRef, target.m, target.ft)%>%dplyr::distinct()

  # Save updated table in cache
  WQXunitRef_Cached <- WQXunitRef

  WQXunitRef
}

#' Update Measure Unit Reference Table internal file (for internal use only)

TADA_UpdateMeasureUnitRef <- function() {
  utils::write.csv(TADA_GetMeasureUnitRef(), file = "inst/extdata/WQXunitRef.csv", row.names = FALSE)
}

#' Used to store cached Result Detection Condition Reference Table

WQXDetCondRef_Cached <- NULL

#' Update Result Detection Condition Reference Table
#'
#' Function downloads and returns in the latest WQX ResultDetectionCondition Domain table, 
#' adds additional target unit information, and writes the data to sysdata.rda.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return sysdata.rda with updated WQXResultDetectionConditionRef object (detection condition reference
#' table for censored data)
#' @export
#'

TADA_GetDetCondRef <- function() {
  
  # If there is a cached table available return it
  if (!is.null(WQXDetCondRef_Cached)) {
    return(WQXDetCondRef_Cached)
  }
  
  # Try to download up-to-date raw data
  raw.data <- tryCatch({
    # read raw csv from url
    utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ResultDetectionCondition.CSV"))
  }, error = function(err) {
    NULL
  })
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message('Downloading latest Measure Unit Reference Table failed!')
    message('Falling back to (possibly outdated) internal file.')
    return(utils::read.csv(system.file("extdata", "WQXResultDetectionConditionRef.csv", package = "TADA")))
  }
  
  WQXDetCondRef <- raw.data%>%
    dplyr::mutate(TADA.Detection_Type = dplyr::case_when(
      Name%in%c("Above Operating Range","Present Above Quantification Limit") ~ as.character("Over-Detect"),
      Name%in%c("Value Decensored","Reported in Raw Data (attached)","High Moisture") ~ as.character("Other"),
      TRUE ~ as.character("Non-Detect")
    ))%>%dplyr::distinct()
  
  # Save updated table in cache
  WQXDetCondRef_Cached <- WQXDetCondRef
  
  WQXDetCondRef
}

#' Update Measure Unit Reference Table internal file (for internal use only)

TADA_UpdateDetCondRef <- function() {
  utils::write.csv(TADA_GetDetCondRef(), file = "inst/extdata/WQXResultDetectionConditionRef.csv", row.names = FALSE)
}

#' Used to store cached Result Detection Condition Reference Table

WQXDetLimitRef_Cached <- NULL

#' Update Detection Quantitation Limit Type Reference Table
#'
#' Function downloads and returns in the latest WQX DetectionQuantitationLimitType Domain table, 
#' adds additional target unit information, and writes the data to sysdata.rda.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return sysdata.rda with updated WQXDetectionQuantitationLimitTypeRef object (detection limit type reference
#' table for censored data)
#'
#' @export

TADA_GetDetLimitRef <- function() {
  
  # If there is a cached table available return it
  if (!is.null(WQXDetLimitRef_Cached)) {
    return(WQXDetLimitRef_Cached)
  }
  
  # Try to download up-to-date raw data
  raw.data <- tryCatch({
    # read raw csv from url
    utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/DetectionQuantitationLimitType.CSV"))
  }, error = function(err) {
    NULL
  })
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message('Downloading latest Measure Unit Reference Table failed!')
    message('Falling back to (possibly outdated) internal file.')
    return(utils::read.csv(system.file("extdata", "WQXDetectionQuantitationLimitTypeRef.csv", package = "TADA")))
  }
  
  WQXDetLimitRef <- raw.data%>%
    dplyr::mutate(TADA.Limit_Type = dplyr::case_when(
      Name%in%c("Upper Quantitation Limit","Upper Reporting Limit","Upper Calibration Limit") ~ as.character("Over-Detect"),
      Name%in%c("Drinking Water Maximum","Field Holding Time Limit","Specified in workplan","Statistical Uncertainty","Systematic Uncertainty","Taxonomic Loss Threshold","Water Quality Standard or Criteria","Upper 95% Confidence Limit","Lower 95% Confidence Limit") ~ as.character("Other"),
      TRUE ~ as.character("Non-Detect")
    ))%>%dplyr::distinct()
  
  ## Add USGS limits not in WQX domain table
  usgs = data.frame(Name = c("Elevated Detection Limit","Historical Lower Reporting Limit","Method Detection Limit (MDL)"),
                    Description = c("USGS hard-coded limit","USGS hard-coded limit","USGS hard-coded limit"),
                    TADA.Limit_Type = c("Non-Detect","Non-Detect","Non-Detect"),
                    Last.Change.Date = rep("4/6/2023 12:00:00 PM",3))
  
  WQXDetLimitRef = plyr::rbind.fill(WQXDetLimitRef, usgs)
  
  # Save updated table in cache
  WQXDetLimitRef_Cached <- WQXDetLimitRef
  
  WQXDetLimitRef
}

#' Update Measure Unit Reference Table internal file (for internal use only)

TADA_UpdateDetLimitRef <- function() {
  utils::write.csv(TADA_GetDetLimitRef(), file = "inst/extdata/WQXDetectionQuantitationLimitTypeRef.csv", row.names = FALSE)
}

#' Used to store cached Activity Type Reference Table

WQXActivityTypeRef_Cached <- NULL

#' Update Activity Type Reference Table
#'
#' Function downloads and returns in the latest WQX ActivityType Domain table, 
#' adds QC category information, and writes the data to sysdata.rda.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return sysdata.rda with updated WQXActivityTypeRef object
#'
#' @export

TADA_GetActivityTypeRef <- function() {
  
  # If there is a cached table available return it
  if (!is.null(WQXActivityTypeRef_Cached)) {
    return(WQXActivityType_Cached)
  }
  
  # Try to download up-to-date raw data
  raw.data <- tryCatch({
    # read raw csv from url
    utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ActivityType.CSV"))
  }, error = function(err) {
    NULL
  })
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message('Downloading latest Activity Type Reference Table failed!')
    message('Falling back to (possibly outdated) internal file.')
    return(utils::read.csv(system.file("extdata", "WQXActivityTypeRef.csv", package = "TADA")))
  }
  
  # Categorize Activity Types
  dup <- c("Quality Control Alternative Measurement Sensitivity",
           "Quality Control Alternative Measurement Sensitivity Plus",
           "Quality Control Field Replicate Habitat Assessment",
           "Quality Control Field Replicate Msr/Obs",
           "Quality Control Field Replicate Portable Data Logger",
           "Quality Control Field Replicate Sample-Composite",
           "Quality Control Sample-Blind Duplicate",
           "Quality Control Sample-Field Replicate",
           "Quality Control Sample-Inter-lab Split",
           "Quality Control Sample-Lab Duplicate",
           "Quality Control Sample-Lab Duplicate 2",
           "Quality Control Sample-Lab Re-Analysis",
           "Quality Control Sample-Lab Split",
           "Quality Control-Meter Lab Duplicate",
           "Quality Control-Meter Lab Duplicate 2",
           "Sample-Routine Resample")
  blank <- c("Quality Control Field Sample Equipment Rinsate Blank",
             "Quality Control Lab Sample Equipment Rinsate Blank",
             "Quality Control Sample-Equipment Blank",
             "Quality Control Sample-Field Ambient Conditions Blank",
             "Quality Control Sample-Field Blank",
             "Quality Control Sample-Lab Blank",
             "Quality Control Sample-Post-preservative Blank",
             "Quality Control Sample-Pre-preservative Blank",
             "Quality Control Sample-Reagent Blank",
             "Quality Control Sample-Trip Blank",
             "Quality Control-Meter Lab Blank",
             "Quality Control-Negative Control",
             "Sample-Depletion Replicate",
             "Sample-Negative Control")
  cal <- c("Quality Control Field Calibration Check",
           "Quality Control Field Msr/Obs Post-Calibration",
           "Quality Control Field Msr/Obs Pre-Calibration",
           "Quality Control Sample-Field Spike",
           "Quality Control Sample-Field Surrogate Spike",
           "Quality Control Sample-Lab Continuing Calibration Verification",
           "Quality Control Sample-Lab Control Sample/Blank Spike",
           "Quality Control Sample-Lab Control Sample/Blank Spike Duplicate",
           "Quality Control Sample-Lab Control Standard",
           "Quality Control Sample-Lab Control Standard Duplicate",
           "Quality Control Sample-Lab Initial Calib Certified Reference Material",
           "Quality Control Sample-Lab Initial Calibration Verification",
           "Quality Control Sample-Lab Matrix Spike",
           "Quality Control Sample-Lab Matrix Spike Duplicate",
           "Quality Control Sample-Lab Spike",
           "Quality Control Sample-Lab Spike Duplicate",
           "Quality Control Sample-Lab Spike Target",
           "Quality Control Sample-Lab Spike of a Lab Blank",
           "Quality Control Sample-Lab Surrogate Control Standard",
           "Quality Control Sample-Lab Surrogate Control Standard Duplicate",
           "Quality Control Sample-Lab Surrogate Method Blank",
           "Quality Control Sample-Measurement Precision Sample",
           "Quality Control Sample-Reference Sample",
           "Quality Control-Calibration Check",
           "Quality Control-Calibration Check Buffer",
           "Sample-Positive Control")
  other <- c("Quality Control Sample-Other")
  
  WQXActivityTypeRef <- raw.data%>%
    dplyr::mutate(TADA.ActivityType.Flag = dplyr::case_when(
      Code %in% dup ~ "QC_duplicate",
      Code %in% blank ~ "QC_blank",
      Code %in% cal ~ "QC_calibration",
      Code %in% other ~ "QC_other",
      TRUE ~ as.character("Non_QC")
    ))%>%dplyr::distinct()
  
  # Save updated table in cache
  WQXActivityTypeRef_Cached <- WQXActivityTypeRef
  
  return(WQXActivityTypeRef)
}

#' Update Activity Type Reference Table internal file (for internal use only)

TADA_UpdateActivityTypeRef <- function() {
  utils::write.csv(TADA_GetActivityTypeRef(), file = "inst/extdata/WQXActivityTypeRef.csv", row.names = FALSE)
}

#' Used to store cached Characteristic Reference Table

WQXCharacteristicRef_Cached <- NULL

#' Update Characteristic Reference Table
#'
#' Function downloads and returns in the latest WQX Characteristic Domain table and writes the data to sysdata.rda.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return sysdata.rda with updated WQXCharacteristicRef object (characteristic reference
#' table)
#' @export
#'

TADA_GetCharacteristicRef <- function() {
  
  # If there is a cached table available return it
  if (!is.null(WQXCharacteristicRef_Cached)) {
    return(WQXCharacteristicRef_Cached)
  }
  
  # Try to download up-to-date raw data
  raw.data <- tryCatch({
    # read raw csv from url
    utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))
  }, error = function(err) {
    NULL
  })
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message('Downloading latest Measure Unit Reference Table failed!')
    message('Falling back to (possibly outdated) internal file.')
    return(utils::read.csv(system.file("extdata", "WQXCharacteristicRef.csv", package = "TADA")))
  }
  
  # rename some columns
  WQXCharacteristicRef = raw.data%>%dplyr::rename(CharacteristicName = Name, Char_Flag = Domain.Value.Status)%>%dplyr::select(CharacteristicName, Char_Flag, Comparable.Name)
  
  # Save updated table in cache
  WQXCharacteristicRef_Cached <- WQXCharacteristicRef
  
  WQXCharacteristicRef
}

#' Update Characteristic Reference Table internal file (for internal use only)

TADA_UpdateCharacteristicRef <- function() {
  utils::write.csv(TADA_GetCharacteristicRef(), file = "inst/extdata/WQXCharacteristicRef.csv", row.names = FALSE)
}
