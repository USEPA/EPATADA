# Used to store cached WQX QAQC Characteristic Validation Reference Table
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

  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Validation Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(load(file = "inst/extdata/WQXcharValRef.rda"))
    # return(utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "TADA")))
  }

  # Categorize status values
  notreviewed <- "Not Reviewed"
  valid <- c("Accepted", "Y")
  invalid <- c("Rejected", "Rejected ", "N")
  nonstandard <- c(
    "NonStandardized",
    "Nonstandardized",
    "InvalidMediaUnit",
    "InvalidChar",
    "MethodNeeded"
  )

  WQXcharValRef <- raw.data %>%
    dplyr::mutate(TADA.WQXVal.Flag = dplyr::case_when(
      Status %in% notreviewed ~ "Not Reviewed",
      Status %in% valid ~ "Valid",
      Status %in% invalid ~ "Invalid",
      Status %in% nonstandard ~ "NonStandardized",
      Status %in% NA ~ "Not Reviewed",
      TRUE ~ as.character("Not Reviewed")
    )) %>%
    dplyr::distinct()

  # Save updated table in cache
  WQXCharValRef_Cached <- WQXcharValRef

  return(WQXcharValRef)
}

# Update Characteristic Validation Reference Table internal file
# (for internal use only)

TADA_UpdateWQXCharValRef <- function() {
  
  WQXcharValRef = TADA_GetWQXCharValRef()
  # utils::write.csv(TADA_GetWQXCharValRef_updated, file = "inst/extdata/WQXcharValRef.csv", row.names = FALSE)
  save(WQXcharValRef, 
       file = "inst/extdata/WQXcharValRef.rda", 
       ascii = FALSE, 
       compress = "xz", 
       version = 3)
  # load(file = "inst/extdata/WQXcharValRef.rda")
}
  

# Used to store cached Measure Unit Reference Table
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
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Measure Unit Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXunitRef.csv", package = "TADA")))
  }


  # Save updated table in cache
  WQXunitRef_Cached <- raw.data
  WQXunitRef <- raw.data
}

# Update Measure Unit Reference Table internal file (for internal use only)

TADA_UpdateMeasureUnitRef <- function() {
  utils::write.csv(TADA_GetMeasureUnitRef(), file = "inst/extdata/WQXunitRef.csv", row.names = FALSE)
}

# Used to store cached Result Detection Condition Reference Table
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
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ResultDetectionCondition.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Result Detection Condition Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXResultDetectionConditionRef.csv", package = "TADA")))
  }

  # Add detection type for all domain values. Review new values when updating.
  WQXDetCondRef <- raw.data %>%
    dplyr::mutate(TADA.Detection_Type = dplyr::case_when(
      Name %in% c(
        "Below Daily Detection Limit",
        "Below Detection Limit",
        "Below Long-term Blank-basd Dt Limit",
        "Below Method Detection Limit",
        "Below Reporting Limit",
        "Below Sample-specific Detect Limit",
        "Below System Detection Limit",
        "Between Inst Detect and Quant Limit",
        "Detected Not Quantified",
        "Not Detected",
        "Not Detected at Detection Limit",
        "Not Detected at Reporting Limit",
        "Not Present",
        "Not Reported",
        "Present Below Quantification Limit",
        "Trace"
      ) ~ as.character("Non-Detect"),
      Name %in% c(
        "Above Operating Range",
        "Present Above Quantification Limit"
      ) ~ as.character("Over-Detect"),
      Name %in% c(
        "Value Decensored",
        "Reported in Raw Data (attached)",
        "High Moisture",
        "Unable to Measure",
        "Value affected by contamination"
      ) ~ as.character("Other"),
      Name %in% NA ~ "Not Reviewed",
      TRUE ~ as.character("Not Reviewed")
    )) %>%
    dplyr::distinct()

  ## Add USGS detection conditions not in WQX domain table
  others <- data.frame(
    Name = c(
      "*Non-detect",
      "*Present <QL",
      "*Present"
    ),
    Description = c(
      "Hard-coded legacy detection condition",
      "Hard-coded legacy detection condition",
      "Hard-coded legacy detection condition"
    ),
    TADA.Detection_Type = c(
      "Non-Detect",
      "Non-Detect",
      "Non-Detect"
    ),
    Last.Change.Date = c(
      "8/7/2023 12:00:00 PM",
      "8/7/2023 12:00:00 PM",
      "12/14/2023 05:00:00 PM"
    )
  )

  WQXDetCondRef <- plyr::rbind.fill(WQXDetCondRef, others)

  # Save updated table in cache
  WQXDetCondRef_Cached <- WQXDetCondRef

  WQXDetCondRef
}

# Update Measure Unit Reference Table internal file (for internal use only)

TADA_UpdateDetCondRef <- function() {
  utils::write.csv(TADA_GetDetCondRef(), file = "inst/extdata/WQXResultDetectionConditionRef.csv", row.names = FALSE)
}

# Used to store cached Result Detection Condition Reference Table
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
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/DetectionQuantitationLimitType.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Measure Unit Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXDetectionQuantitationLimitTypeRef.csv", package = "TADA")))
  }

  WQXDetLimitRef <- raw.data %>%
    dplyr::mutate(TADA.Limit_Type = dplyr::case_when(
      Name %in% c(
        "Blank-adjusted method detect limit",
        "Blank-adjusted reporting limit",
        "Contract Detection Limit",
        "Contract Quantitation Limit",
        "Daily detection limit",
        "Detection limit by DQCALC",
        "Estimated Detection Level",
        "Estimated Quantitation Limit",
        "Instrument Detection Level",
        "Interim Reporting Level",
        "Laboratory Reporting Level",
        "Long Term Method Detection Level",
        "Lower Quantitation Limit",
        "Lower Reporting Limit",
        "Lower limit of detection",
        "Method Detection Level",
        "Minimum Reporting Level",
        "Practical Quantitation Limit",
        "Reporting limit",
        "Reporting limit by DQCALC",
        "Required detection limit",
        "Sample Detection Limit",
        "Sample-Specific Quantitation Limit",
        "Sample-specific critical level",
        "Sample-specific min detect conc"
      ) ~ as.character("Non-Detect"),
      Name %in% c(
        "Upper Quantitation Limit",
        "Upper Reporting Limit",
        "Upper Calibration Limit"
      ) ~ as.character("Over-Detect"),
      Name %in% c(
        "Measurement Uncertainty",
        "Laboratory Holding Time Limit",
        "Drinking Water Maximum",
        "Field Holding Time Limit",
        "Specified in workplan",
        "Statistical Uncertainty",
        "Systematic Uncertainty",
        "Taxonomic Loss Threshold",
        "Water Quality Standard or Criteria",
        "Upper 95% Confidence Limit",
        "Lower 95% Confidence Limit",
        "Censoring level"
      ) ~ as.character("Other"),
      Name %in% NA ~ "Not Reviewed",
      TRUE ~ as.character("Not Reviewed")
    )) %>%
    dplyr::distinct()

  ## Add USGS limits not in WQX domain table
  usgs <- data.frame(
    Name = c(
      "Elevated Detection Limit", "Historical Lower Reporting Limit",
      "Method Detection Limit (MDL)", "Lab Reporting Limit, NA"
    ),
    Description = c(
      "USGS hard-coded limit", "USGS hard-coded limit",
      "USGS hard-coded limit", "USGS hard-coded limit"
    ),
    TADA.Limit_Type = c("Non-Detect", "Non-Detect", "Non-Detect", "Non-Detect"),
    Last.Change.Date = c(
      "4/6/2023 12:00:00 PM", "4/6/2023 12:00:00 PM",
      "4/6/2023 12:00:00 PM", "12/14/2023 05:00:00 PM"
    )
  )

  WQXDetLimitRef <- plyr::rbind.fill(WQXDetLimitRef, usgs)

  # Save updated table in cache
  WQXDetLimitRef_Cached <- WQXDetLimitRef

  WQXDetLimitRef
}

# Update Measure Unit Reference Table internal file (for internal use only)

TADA_UpdateDetLimitRef <- function() {
  utils::write.csv(TADA_GetDetLimitRef(), file = "inst/extdata/WQXDetectionQuantitationLimitTypeRef.csv", row.names = FALSE)
}

# Used to store cached Activity Type Reference Table
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
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ActivityType.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Activity Type Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXActivityTypeRef.csv",
      package = "TADA"
    )))
  }

  # Categorize Activity Types
  rep <- c(
    "Quality Control Field Replicate Habitat Assessment",
    "Quality Control Field Replicate Msr/Obs",
    "Quality Control Field Replicate Portable Data Logger",
    "Quality Control Field Replicate Sample-Composite",
    "Quality Control Sample-Field Replicate",
    "Quality Control Field Replicate Sample-Field Subsample"
  )
  dup <- c(
    "Quality Control Alternative Measurement Sensitivity",
    "Quality Control Alternative Measurement Sensitivity Plus",
    "Quality Control Sample-Blind Duplicate",
    "Quality Control Sample-Inter-lab Split",
    "Quality Control Sample-Lab Duplicate",
    "Quality Control Sample-Lab Duplicate 2",
    "Quality Control Sample-Lab Re-Analysis",
    "Quality Control Sample-Lab Split",
    "Quality Control-Meter Lab Duplicate",
    "Quality Control-Meter Lab Duplicate 2",
    "Sample-Routine Resample"
  )
  blank <- c(
    "Quality Control Field Sample Equipment Rinsate Blank",
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
    "Sample-Negative Control"
  )
  cal <- c(
    "Quality Control Field Calibration Check",
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
    "Sample-Positive Control"
  )
  other <- c("Quality Control Sample-Other")

  nonQC <- c(
    "Field Msr/Obs",
    "Field Msr/Obs-Continuous Time Series",
    "Field Msr/Obs-Habitat Assessment",
    "Field Msr/Obs-Incidental",
    "Field Msr/Obs-Portable Data Logger",
    "Sample-Composite With Parents",
    "Sample-Composite Without Parents",
    "Sample-Field Split",
    "Sample-Field Subsample",
    "Sample-Integrated Cross-Sectional Profile",
    "Sample-Integrated Flow Proportioned",
    "Sample-Integrated Horizontal Profile",
    "Sample-Integrated Horizontal and Vertical Composite Profile",
    "Sample-Integrated Time Series",
    "Sample-Integrated Vertical Profile",
    "Sample-Other",
    "Sample-Routine"
  )

  WQXActivityTypeRef <- raw.data %>%
    dplyr::mutate(TADA.ActivityType.Flag = dplyr::case_when(
      Code %in% rep ~ "QC_replicate",
      Code %in% dup ~ "QC_duplicate",
      Code %in% blank ~ "QC_blank",
      Code %in% cal ~ "QC_calibration",
      Code %in% other ~ "QC_other",
      Code %in% nonQC ~ "Non_QC",
      TRUE ~ as.character("Not Reviewed"),
      Code %in% NA ~ "Not Reviewed"
    )) %>%
    dplyr::distinct()

  # Hard-code add activity types from NWIS
  ## Add USGS limits not in WQX domain table
  new.atcs <- data.frame(
    Code = c(
      "Quality Control Sample-Blind",
      "Unknown",
      "Not determined",
      "Sample"
    ),
    Description = c(
      "Hard-coded activity type not in WQX domain",
      "Hard-coded activity type not in WQX domain",
      "Hard-coded activity type not in WQX domain",
      "Hard-coded activity type not in WQX domain"
    ),
    TADA.ActivityType.Flag = c(
      "QC_duplicate",
      "Not Reviewed",
      "Not Reviewed",
      "Non_QC"
    ),
    Last.Change.Date = c(
      "8/11/2023  12:00:00 PM",
      "8/11/2023  12:00:00 PM",
      "1/5/2024  12:00:00 PM",
      "1/5/2024  12:00:00 PM"
    )
  )

  WQXActivityTypeRef <- plyr::rbind.fill(WQXActivityTypeRef, new.atcs)

  # Save updated table in cache
  WQXActivityTypeRef_Cached <- WQXActivityTypeRef

  return(WQXActivityTypeRef)
}

# Update Activity Type Reference Table internal file (for internal use only)

TADA_UpdateActivityTypeRef <- function() {
  utils::write.csv(TADA_GetActivityTypeRef(), file = "inst/extdata/WQXActivityTypeRef.csv", row.names = FALSE)
}

# Used to store cached Characteristic Reference Table

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
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Measure Unit Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXCharacteristicRef.csv", package = "TADA")))
  }

  # rename some columns
  WQXCharacteristicRef <- raw.data %>%
    dplyr::rename(CharacteristicName = Name, Char_Flag = Domain.Value.Status) %>%
    dplyr::select(CharacteristicName, Char_Flag, Comparable.Name)

  # Save updated table in cache
  WQXCharacteristicRef_Cached <- WQXCharacteristicRef

  WQXCharacteristicRef
}

# Update Characteristic Reference Table internal file (for internal use only)

TADA_UpdateCharacteristicRef <- function() {
  utils::write.csv(TADA_GetCharacteristicRef(), file = "inst/extdata/WQXCharacteristicRef.csv", row.names = FALSE)
}



# Used to store cached WQXMeasureQualifierCodeRef Ref Table
WQXMeasureQualifierCodeRef_Cached <- NULL


#' Update result Measure Qualifier Code Reference Table
#'
#' Function downloads and returns in the latest WQX ResultMeasureQualifier Domain table,
#' adds category information, and writes the data to sysdata.rda.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return sysdata.rda with updated WQXMeasureQualifierCodeRef object
#'
#' @export

TADA_GetMeasureQualifierCodeRef <- function() {
  # If there is a cached table available return it
  if (!is.null(WQXMeasureQualifierCodeRef_Cached)) {
    return(WQXMeasureQualifierCodeRef_Cached)
  }

  # Try to download up-to-date raw data
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ResultMeasureQualifier.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Measure Qualifier Code Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXMeasureQualifierCodeRef.csv", package = "TADA")))
  }

  # Categorize Result Measure Qualifiers
  # Categorization should be conservative
  suspect <- c(
    "(", "+", "AR", "BS", "BSR", "BT", "BVER", "C", "CAN", "CBC","TT","UDL", "UDQ",
    "CSR", "DE", "EER", "EFAI", "FDB", "FDC", "FDL", "FFB", "FFD","TMLF","UNC", "TOC",
    "FFS", "FFT", "FH", "FIS", "FL", "FLD", "FLS", "FMD","ITNA",  "JCN","RLRS",
    "FMS", "FPC", "FPR", "FQC", "FRS", "FSD", "FSL", "FSP", "FUB","NPNF", "RPDX",
    "H", "H2", "H3", "HMSD", "C25", "HE","HIM","ICA","IS","ISAC","ITNM","OS3","QCI",
    "INT", "IQCOL", "ISP", "A", "D", "DT", "EMPC","HH", "HIB", "ISR**","MDL","OUT",
    "JCW", "KCF", "KCX", "KK", "LAC", "LBF", "CNT","GR4","HICC",  "J-R","NW", "PB",
    "LO", "$", ")", "*", "ESD", "EST","EVA","EVAD","EVID","FPP","G", "LLS","OA3","PK",
    "MI", "MSR", "NAI", "NLBL", "NLRO", "NN", "NRO", "F", "FLA",  "I", "MSD", "NHS",
    "NRP", "NRR", "NSQ", "PNQ", "Q", "QC", "R", "RA","FEQ", "FLC", "GXB","NA","OTHER",
    "RPO", "S2", "SCA", "SCF", "SCP", "SCX", "SD%EL", "SDROL", "SSR","PP",  "PPD","PRE",
    "SUS", "V", "^", "RNON", "B", "CBG", "SSRV" # this is used by USGS for surrogates
  )
  pass <- c(
    "P", "NRS", "NRB", "&", "=","M6F", "LVER", "LSSR", "LQ", "LOPR", "LMSD", "LICC",
    "HTH", "HNRO", "HMSR",  "AC", "AL", "ALK", "ALT", "LOB","AP", "BAC", "CAJ", 
    "CBL", "CC","CDI", "CG", "CKB", "CKBJ", "CKG", "CKJ", "CLC",  "CON", "CUG",
    "DEC", "DI", "DOM",  "ECI", "HLBL", "HQ", "HVER", "J", "J+", "J-", "L", "LCS",
    "LF", "LIS", "LL", "LLBL",  "LMSR", "LNRO","LR", "LT", "N",  "NFNS", "O", 
    "PQL",  "RC", "REX", "RIN",  "RMAX", "RNAF", "RP","RR", "RV", "RVB",
    "SBB", "SLB", "SM", "SS", "T",  "VS", "VVRR", "VVRR2",  "ZZ","J-1", "NA", "TR"
  )
  nondetect <- c("BQL", "2-5B", "U", "LTGTE", "K", "IDL", "<2B", "BRL", "D>T", "DL")

  overdetect <- c("E", "EE", "GT")

  WQXMeasureQualifierCodeRef <- raw.data %>%
    dplyr::mutate(TADA.MeasureQualifierCode.Flag = dplyr::case_when(
      Code %in% nondetect ~ "Non-Detect",
      Code %in% overdetect ~ "Over-Detect",
      Code %in% suspect ~ "Suspect",
      Code %in% pass ~ "Pass",
      Code %in% NA ~ "Pass",
      TRUE ~ as.character("Not Reviewed")
    )) %>%
    dplyr::distinct()

  # ## Add detection conditions not in WQX domain table
  # ## No longer needed because these are handled in measure qualifier flag function
  # others <- data.frame(
  #   Code = c(
  #     "H;J", "LT;MDL", "HMSR;J", "J;QC", "D;H", "J;U", "H;LAC",
  #     "FQC;J", "B;J", "FMS;J", "D;U", "FSL;J"
  #   ),
  #   Description = c(
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination",
  #     "Hard-coded combination"
  #   ),
  #   TADA.MeasureQualifierCode.Flag = c(
  #     "Pass",
  #     "Non-Detect",
  #     "Suspect",
  #     "Suspect",
  #     "Suspect",
  #     "Non-Detect",
  #     "Suspect",
  #     "Suspect",
  #     "Suspect",
  #     "Suspect",
  #     "Non-Detect",
  #     "Suspect"
  #   ),
  #   Last.Change.Date = c(
  #     "8/7/2023 02:36:00 PM",
  #     "8/7/2023 05:00:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 07:42:00 PM",
  #     "8/7/2023 08:14:00 PM"
  #   )
  # )
  #
  # WQXMeasureQualifierCodeRef <- plyr::rbind.fill(WQXMeasureQualifierCodeRef, others)

  # Save updated table in cache
  WQXMeasureQualifierCodeRef_Cached <- WQXMeasureQualifierCodeRef

  return(WQXMeasureQualifierCodeRef)
}

# Update WQX ResultMeasureQualifier Reference Table internal file (for internal use only)

TADA_UpdateMeasureQualifierCodeRef <- function() {
  utils::write.csv(TADA_GetMeasureQualifierCodeRef(),
    file = "inst/extdata/WQXMeasureQualifierCodeRef.csv",
    row.names = FALSE
  )
}

# Used to store cached WQXMonLocTypeRef
WQXMonLocTypeRef_Cached <- NULL

#' Update Monitoring Location Type Name Reference Table
#'
#' Function downloads and returns in the latest WQX MonitoringLocationTypeName
#' Domain table, adds additional information to assist in identifying groundwater
#' and surface water samples, and writes the data to sysdata.rda.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return sysdata.rda with updated WQXMonitoringLocationTypeName object
#' (reference table for identifying surface water samples by
#' MonitoringLocationTypeName)
#' @export
#'

TADA_GetMonLocTypeRef <- function() {
  # If there is a cached table available return it
  if (!is.null(WQXMonLocTypeRef_Cached)) {
    return(WQXMonLocTypeRef_Cached)
  }

  # Try to download up-to-date raw data
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Monitoring Location Type Name Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXMonitoringLocationTypeNameRef.csv", package = "TADA")))
  }

  # Add TADA.Media.Flag for all domain values. Review new values when updating.
  MonLocTypeRef <- raw.data %>%
    dplyr::mutate(
      TADA.Media.Flag = dplyr::case_when(
        Name %in% c(
          "BEACH Program Site-Channelized stream",
          "BEACH Program Site-Estuary",
          "BEACH Program Site-Great Lake",
          "BEACH Program Site-Lake",
          "BEACH Program Site-River/Stream",
          "Canal Drainage",
          "Canal Irrigation",
          "Canal Transport",
          "Constructed Wetland",
          "Estuary",
          "Great Lake",
          "Intertidal",
          "Lake",
          "Ocean",
          "Other-Surface Water",
          "Pipe, Unspecified Source",
          "Mine/Mine Discharge",
          "Pond",
          "Pond-Anchialine",
          "Pond-Stock",
          "Pond-Wastewater",
          "Reservoir",
          "River/Stream",
          "River/Stream Ephemeral",
          "River/Stream Intermittent",
          "River/Stream Perennial",
          "Riverine Impoundment",
          "Subtidal",
          "Wetland Estuarine-Ditch",
          "Wetland Estuarine-Emergent",
          "BEACH Program Site-Ocean",
          "Wetland Estuarine-Forested",
          "Wetland Estuarine-Marsh",
          "Wetland Estuarine-Pool",
          "River/stream Effluent-Dominated",
          "Wetland Estuarine-Scrub-Shrub",
          "Wetland Estuarine-Tidal Creek",
          "Wetland Lacustrine-Emergent",
          "Wetland Palustrine-Emergent",
          "Wetland Palustrine-Forested",
          "Wetland Palustrine-Moss-Lichen",
          "Wetland Palustrine-Shrub-Scrub",
          "Wetland Riverine-Emergent",
          "Wetland Undifferentiated",
          "Wetland Palustrine Pond",
          "Channelized Stream",
          "Estuary-Freshwater",
          "Pond-Sediment",
          "Pond-Stormwater",
          "Spring",
          "Wetland Lacustrine-Aquatic Bed",
          "Wetland Lacustrine-Unconsolidated Bottom",
          "Wetland Riverine-Unconsolidated Bottom",
          "Wetland Riverine-Aquatic Bed",
          "Storm Sewer",
          "Combined Sewer",
          "Mine/Mine Discharge Adit (Mine Entrance)",
          "Mine/Mine Discharge Tailings Pile",
          "Mine/Mine Discharge Waste Rock Pile",
          "Waste Sewer",
          "Seep",
          "Playa",
          "BEACH Program Site-Land runoff",
          "BEACH Program Site-Storm sewer",
          "BEACH Program Site-Waste sewer",
          "Floodwater Urban",
          "Floodwater non-Urban"
        ) ~ as.character("Surface Water"),
        Name %in% c(
          "Cave",
          "Well",
          "Other-Ground Water"
        ) ~ as.character("Groundwater")
      ),
      TADA.Media.Flag = ifelse(is.na(TADA.Media.Flag), "", TADA.Media.Flag)
    ) %>%
    dplyr::distinct()


  # Save updated table in cache,
  WQXMonLocTypeRef_Cached <- MonLocTypeRef

  return(WQXMonLocTypeRef_Cached)
}

# Update WQX MonitoringLocationTypeName Reference Table internal file (for internal use only)

TADA_UpdateMonLocTypeRef <- function() {
  utils::write.csv(TADA_GetMonLocTypeRef(),
    file = "inst/extdata/WQXMonitoringLocationTypeNameRef.csv",
    row.names = FALSE
  )
}
