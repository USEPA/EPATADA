# ============================================================
# EPATADA reference data utilities: shared helpers and getters
# ============================================================
# Centralized:
# - Robust download of authoritative CSVs with installed extdata RDA fallback
# - Session-level caching with a single environment
# - Simple, dependency-light data normalization and flagging
# - Dev-only update writers that save extdata as binary RDA files
#
# Important: All installed extdata are RDA (not CSV). Public getters will:
# - Try to download the latest CSV from EPA/WQP
# - Normalize/flag as needed
# - Fall back to installed RDA if download fails
# - Cache the resulting data.frame for the session
#
# Internal ".TADA_Update*" functions re-create the installed RDA files
# under inst/extdata, using consistent object names for each table.

# =========================
# Shared cache + constants
# =========================

# Session cache env (avoids <<- scoping issues)
if (!exists(".TADA_cache", inherits = FALSE)) {
  .TADA_cache <- new.env(parent = emptyenv())
}

# Authoritative CSV sources
.WQX_URLS <- list(
  Characteristic = "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV",
  QAQCCharacteristicValidation = "https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV",
  MeasureUnit = "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV",
  ResultDetectionCondition = "https://cdx.epa.gov/wqx/download/DomainValues/ResultDetectionCondition.CSV",
  DetectionQuantitationLimitType = "https://cdx.epa.gov/wqx/download/DomainValues/DetectionQuantitationLimitType.CSV",
  ActivityType = "https://cdx.epa.gov/wqx/download/DomainValues/ActivityType.CSV",
  MonitoringLocationType = "https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV",
  ResultMeasureQualifier = "https://cdx.epa.gov/wqx/download/DomainValues/ResultMeasureQualifier.CSV"
)

.WQP_URLS <- list(
  Organization = "https://www.waterqualitydata.us/data/Organization/search?mimeType=csv&zip=no"
)

# Cache keys
.WQXCharacteristicRef_cache_key <- "WQXCharacteristicRef"
.WQXCharValRef_cache_key <- "WQXcharValRef"
.WQXUnitRef_cache_key <- "WQXunitRef"
.WQXDetCondRef_cache_key <- "WQXResultDetectionConditionRef"
.WQXDetLimitRef_cache_key <- "WQXDetectionQuantitationLimitTypeRef"
.WQXActivityTypeRef_cache_key <- "WQXActivityTypeRef"
.WQXMonLocTypeRef_cache_key <- "WQXMonitoringLocationTypeNameRef"
.WQPProviderRef_cache_key <- "WQPProviderRef"
.WQXMeasureQualifierCodeRef_cache_key <- "WQXMeasureQualifierCodeRef"

# =========================
# Generic helper functions
# =========================

# Trim character columns in any data.frame (defensive hygiene)
.tada_trim_char_cols <- function(df) {
  df[] <- lapply(df, function(x) if (is.character(x)) trimws(x) else x)
  df
}

# Cache helpers
.tada_cache_get <- function(key) .TADA_cache[[key]]
.tada_cache_set <- function(key, value) {
  .TADA_cache[[key]] <- value
  invisible(value)
}

# Read CSV from URL; returns NULL on error (network/format)
.tada_read_csv_url <- function(url, stringsAsFactors = FALSE) {
  tryCatch(
    utils::read.csv(url, stringsAsFactors = stringsAsFactors),
    error = function(e) NULL
  )
}

# Load a data.frame from an installed extdata .rda (returns NULL if not found/invalid)
# - object_name: preferred object name inside the .rda
# - required_cols: optional structural check
# - trim: trim character columns if TRUE
.tada_load_extdata_rda <- function(
  pkg = "EPATADA",
  filename,
  object_name = NULL,
  required_cols = NULL,
  trim = TRUE
) {
  path <- system.file("extdata", filename, package = pkg)
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  e <- new.env(parent = emptyenv())
  objs <- try(load(path, envir = e), silent = TRUE)
  if (inherits(objs, "try-error")) {
    return(NULL)
  }
  # Prefer explicit object_name when provided
  if (
    !is.null(object_name) &&
      object_name %in% objs &&
      is.data.frame(e[[object_name]])
  ) {
    df <- e[[object_name]]
    if (!is.null(required_cols) && !all(required_cols %in% names(df))) {
      return(NULL)
    }
    if (trim) {
      df <- .tada_trim_char_cols(df)
    }
    return(df)
  }
  # Otherwise, pick the first data.frame that meets required_cols (if specified)
  for (nm in objs) {
    obj <- e[[nm]]
    if (is.data.frame(obj)) {
      if (!is.null(required_cols) && !all(required_cols %in% names(obj))) {
        next
      }
      df <- if (trim) .tada_trim_char_cols(obj) else obj
      return(df)
    }
  }
  NULL
}

# Download authoritative CSV; if it fails, fallback to installed RDA
.tada_download_or_extdata_rda <- function(
  url,
  fallback_filename,
  object_name,
  pkg = "EPATADA",
  required_cols = NULL,
  stringsAsFactors = FALSE,
  trim = TRUE,
  on_fail_message = NULL
) {
  # Attempt online download (CSV)
  df <- .tada_read_csv_url(url, stringsAsFactors = stringsAsFactors)
  if (!is.null(df)) {
    if (trim) {
      df <- .tada_trim_char_cols(df)
    }
    return(df)
  }
  # Inform the user about fallback
  if (!is.null(on_fail_message)) {
    message(on_fail_message)
  }
  # Attempt installed extdata fallback (RDA)
  df <- .tada_load_extdata_rda(
    pkg = pkg,
    filename = fallback_filename,
    object_name = object_name,
    required_cols = required_cols,
    trim = trim
  )
  if (is.null(df)) {
    stop(
      "Fallback extdata '",
      fallback_filename,
      "' not found or invalid in installed package '",
      pkg,
      "'."
    )
  }
  df
}

# Find package source root by locating DESCRIPTION (used by dev-only writers)
.tada_find_pkg_root <- function(start = getwd(), pkg = "EPATADA") {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  while (nchar(cur) > 0 && cur != dirname(cur)) {
    desc <- file.path(cur, "DESCRIPTION")
    if (file.exists(desc)) {
      dcf <- tryCatch(read.dcf(desc, all = TRUE), error = function(e) NULL)
      if (!is.null(dcf) && isTRUE(tolower(dcf[1, "Package"]) == tolower(pkg))) {
        return(cur)
      }
    }
    cur <- dirname(cur)
  }
  NULL
}

# Safe bind rows (no external deps): aligns columns by name, fills missing with NA
.tada_bind_rows <- function(df1, df2) {
  all_cols <- union(names(df1), names(df2))
  df1[setdiff(all_cols, names(df1))] <- NA
  df2[setdiff(all_cols, names(df2))] <- NA
  df1 <- df1[, all_cols]
  df2 <- df2[, all_cols]
  rbind(df1, df2)
}

# Assign labels based on vector membership (first match wins, NA optional)
.tada_flag_by_groups <- function(
  df,
  source_col,
  out_col,
  groups,
  default = "Not Reviewed",
  na_default = NULL
) {
  stopifnot(source_col %in% names(df))
  v <- df[[source_col]]
  flag <- rep(default, length(v))
  for (lab in names(groups)) {
    idx <- (v %in% groups[[lab]]) & (flag == default)
    flag[idx] <- lab
  }
  if (!is.null(na_default)) {
    flag[is.na(v)] <- na_default
  }
  df[[out_col]] <- as.character(flag)
  df
}

# Compare two data.frames for equality ignoring row order and minor attributes
.tada_df_equal <- function(a, b) {
  # Fall back to identical() if not data.frames
  if (!is.data.frame(a) || !is.data.frame(b)) {
    return(identical(a, b))
  }

  # Same set of column names?
  na <- names(a)
  nb <- names(b)
  if (!identical(sort(na), sort(nb))) {
    return(FALSE)
  }

  # Align by sorted column names
  cols <- sort(na)
  a <- a[, cols, drop = FALSE]
  b <- b[, cols, drop = FALSE]

  # Coerce factors to character to avoid attribute-only diffs
  a[] <- lapply(a, function(x) if (is.factor(x)) as.character(x) else x)
  b[] <- lapply(b, function(x) if (is.factor(x)) as.character(x) else x)

  # Canonical row ordering by all columns
  if (nrow(a) > 0) {
    oa <- try(do.call(order, a), silent = TRUE)
    if (!inherits(oa, "try-error") && length(oa)) a <- a[oa, , drop = FALSE]
  }
  if (nrow(b) > 0) {
    ob <- try(do.call(order, b), silent = TRUE)
    if (!inherits(ob, "try-error") && length(ob)) b <- b[ob, , drop = FALSE]
  }

  # Remove row names before comparison
  rownames(a) <- NULL
  rownames(b) <- NULL

  isTRUE(all.equal(a, b, check.attributes = FALSE))
}

# Dev-only writer: save a data.frame as RDA into inst/extdata under obj_name
# Now skips writing if content hasn't changed compared to existing .rda
.tada_save_ext_rda <- function(
  obj,
  obj_name,
  pkg = "EPATADA",
  filename,
  compress = "xz",
  version = 2
) {
  pkg_root <- .tada_find_pkg_root(pkg = pkg)
  if (is.null(pkg_root)) {
    stop(
      "Could not locate package source root for ",
      pkg,
      ". Run from the package source directory."
    )
  }
  out_path <- file.path(pkg_root, "inst", "extdata", filename)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  # If an existing file is present, load and compare to avoid unnecessary writes
  if (file.exists(out_path)) {
    e_old <- new.env(parent = emptyenv())
    old_objs <- try(load(out_path, envir = e_old), silent = TRUE)
    if (
      !inherits(old_objs, "try-error") &&
        obj_name %in% old_objs &&
        is.data.frame(e_old[[obj_name]]) &&
        .tada_df_equal(e_old[[obj_name]], obj)
    ) {
      message(
        "No changes detected for ",
        obj_name,
        "; skipping save: ",
        out_path
      )
      return(invisible(out_path))
    }
  }

  # Save updated object
  e <- new.env(parent = emptyenv())
  e[[obj_name]] <- obj
  save(
    list = obj_name,
    file = out_path,
    envir = e,
    version = version,
    compress = compress
  )
  message(obj_name, " saved to: ", out_path)
  invisible(out_path)
}

# =========================
# Characteristic reference
# =========================

# Normalize authoritative CSV to stable schema
.TADA_normalize_characteristic_ref <- function(df) {
  if (!all(c("Name", "Domain.Value.Status") %in% names(df))) {
    return(NULL)
  }
  ref <- data.frame(
    CharacteristicName = df[["Name"]],
    Char_Flag = df[["Domain.Value.Status"]],
    stringsAsFactors = FALSE
  )
  if ("Comparable.Name" %in% names(df)) {
    ref[["Comparable.Name"]] <- df[["Comparable.Name"]]
  }
  if ("CAS.Number" %in% names(df)) {
    ref[["CAS.Number"]] <- df[["CAS.Number"]]
  }
  ref <- .tada_trim_char_cols(ref)
  unique(ref)
}

# ================
# Table helpers
# ================

# QAQC Characteristic Validation
.TADA_flag_WQXCharValRef <- function(df) {
  groups <- list(
    "Pass" = c("Accepted", "Y"),
    "Suspect" = c("Rejected", "Rejected ", "N"),
    "NonStandardized" = c(
      "NonStandardized",
      "Nonstandardized",
      "Non Standardized",
      "InvalidMediaUnit",
      "InvalidChar",
      "MethodNeeded"
    )
  )
  df <- .tada_flag_by_groups(
    df,
    source_col = "Status",
    out_col = "TADA.WQXVal.Flag",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  unique(.tada_trim_char_cols(df))
}

# Measure Unit (no flags)
.TADA_prepare_MeasureUnitRef <- function(df) {
  unique(.tada_trim_char_cols(df))
}

# Result Detection Condition (+ USGS legacy)
.TADA_flag_DetCondRef <- function(df) {
  groups <- list(
    "Non-Detect" = c(
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
    ),
    "Over-Detect" = c(
      "Above Operating Range",
      "Present Above Quantification Limit"
    ),
    "Other" = c(
      "Value Decensored",
      "Reported in Raw Data (attached)",
      "High Moisture",
      "Unable to Measure",
      "Value affected by contamination"
    )
  )
  df <- .tada_flag_by_groups(
    df,
    source_col = "Name",
    out_col = "TADA.Detection_Type",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  df <- unique(.tada_trim_char_cols(df))
  others <- data.frame(
    Name = c("*Non-detect", "*Present <QL", "*Present"),
    Description = rep("Hard-coded legacy detection condition", 3),
    TADA.Detection_Type = c("Non-Detect", "Non-Detect", "Non-Detect"),
    Last.Change.Date = c(
      "8/7/2023 12:00:00 PM",
      "8/7/2023 12:00:00 PM",
      "12/14/2023 05:00:00 PM"
    ),
    stringsAsFactors = FALSE
  )
  df <- .tada_bind_rows(df, others)
  df <- unique(.tada_trim_char_cols(df))
  df
}

# Detection/Quantitation Limit Type (+ USGS)
.TADA_flag_DetLimitRef <- function(df) {
  groups <- list(
    "Non-Detect" = c(
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
    ),
    "Over-Detect" = c(
      "Upper Quantitation Limit",
      "Upper Reporting Limit",
      "Upper Calibration Limit"
    ),
    "Other" = c(
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
    )
  )
  df <- .tada_flag_by_groups(
    df,
    source_col = "Name",
    out_col = "TADA.Limit_Type",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  df <- unique(.tada_trim_char_cols(df))
  usgs <- data.frame(
    Name = c(
      "Elevated Detection Limit",
      "Historical Lower Reporting Limit",
      "Method Detection Limit (MDL)",
      "Lab Reporting Limit, NA"
    ),
    Description = rep("USGS hard-coded limit", 4),
    TADA.Limit_Type = rep("Non-Detect", 4),
    Last.Change.Date = c(
      "4/6/2023 12:00:00 PM",
      "4/6/2023 12:00:00 PM",
      "4/6/2023 12:00:00 PM",
      "12/14/2023 05:00:00 PM"
    ),
    stringsAsFactors = FALSE
  )
  df <- .tada_bind_rows(df, usgs)
  df <- unique(.tada_trim_char_cols(df))
  df
}

# Activity Type (+ hard-coded additions)
.TADA_flag_ActivityTypeRef <- function(df) {
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
  groups <- list(
    "QC_replicate" = rep,
    "QC_duplicate" = dup,
    "QC_blank" = blank,
    "QC_calibration" = cal,
    "QC_other" = other,
    "Non_QC" = nonQC
  )
  # Force classification by Code
  df <- .tada_flag_by_groups(
    df,
    source_col = "Code",
    out_col = "TADA.ActivityType.Flag",
    groups = groups,
    default = "Not Reviewed",
    na_default = "Not Reviewed"
  )
  df <- unique(.tada_trim_char_cols(df))
  new.atcs <- data.frame(
    Code = c(
      "Quality Control Sample-Blind",
      "Unknown",
      "Not determined",
      "Sample"
    ),
    Description = rep("Hard-coded activity type not in WQX domain", 4),
    TADA.ActivityType.Flag = c(
      "QC_duplicate",
      "Not Reviewed",
      "Not Reviewed",
      "Non_QC"
    ),
    Last.Change.Date = c(
      "8/11/2023 12:00:00 PM",
      "8/11/2023 12:00:00 PM",
      "1/5/2024 12:00:00 PM",
      "1/5/2024 12:00:00 PM"
    ),
    stringsAsFactors = FALSE
  )
  df <- .tada_bind_rows(df, new.atcs)
  df <- unique(.tada_trim_char_cols(df))
  df
}

# Monitoring Location Type Name
.TADA_flag_MonLocTypeRef <- function(df) {
  surface <- c(
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
  )
  groundwater <- c("Cave", "Well", "Other-Ground Water")
  df <- .tada_flag_by_groups(
    df,
    source_col = "Name",
    out_col = "TADA.Media.Flag",
    groups = list("Surface Water" = surface, "Groundwater" = groundwater),
    default = "",
    na_default = ""
  )
  unique(.tada_trim_char_cols(df))
}

# WQP Organization/Provider (column selection)
.TADA_prepare_WQPOrgProviderRef <- function(df) {
  keep <- c("OrganizationIdentifier", "OrganizationFormalName", "ProviderName")
  keep <- keep[keep %in% names(df)]
  unique(.tada_trim_char_cols(df[, keep, drop = FALSE]))
}

# Measure Qualifier Code
.TADA_flag_MeasureQualifierCodeRef <- function(df) {
  suspect <- c(
    "(",
    "+",
    "AR",
    "BS",
    "BSR",
    "BT",
    "BVER",
    "C",
    "CAN",
    "CBC",
    "TT",
    "UDL",
    "UDQ",
    "CSR",
    "DE",
    "EER",
    "EFAI",
    "FDB",
    "FDC",
    "FDL",
    "FFB",
    "FFD",
    "TMLF",
    "UNC",
    "TOC",
    "FFS",
    "FFT",
    "FH",
    "FIS",
    "FL",
    "FLD",
    "FLS",
    "FMD",
    "ITNA",
    "JCN",
    "RLRS",
    "FMS",
    "FPC",
    "FPR",
    "FQC",
    "FRS",
    "FSD",
    "FSL",
    "FSP",
    "FUB",
    "NPNF",
    "RPDX",
    "H",
    "H2",
    "H3",
    "HMSD",
    "C25",
    "HE",
    "HIM",
    "ICA",
    "IS",
    "ISAC",
    "ITNM",
    "OS3",
    "QCI",
    "INT",
    "IQCOL",
    "ISP",
    "A",
    "D",
    "DT",
    "EMPC",
    "HH",
    "HIB",
    "ISR**",
    "MDL",
    "OUT",
    "JCW",
    "KCF",
    "KCX",
    "KK",
    "LAC",
    "LBF",
    "CNT",
    "GR4",
    "HICC",
    "J-R",
    "NW",
    "PB",
    "LO",
    "$",
    ")",
    "*",
    "ESD",
    "EST",
    "EVA",
    "EVAD",
    "EVID",
    "FPP",
    "G",
    "LLS",
    "OA3",
    "PK",
    "MI",
    "MSR",
    "NAI",
    "NLBL",
    "NLRO",
    "NN",
    "NRO",
    "F",
    "FLA",
    "I",
    "MSD",
    "NHS",
    "NRP",
    "NRR",
    "NSQ",
    "PNQ",
    "Q",
    "QC",
    "R",
    "RA",
    "FEQ",
    "FLC",
    "GXB",
    "NA",
    "OTHER",
    "RPO",
    "S2",
    "SCA",
    "SCF",
    "SCP",
    "SCX",
    "SD%EL",
    "SDROL",
    "SSR",
    "PP",
    "PPD",
    "PRE",
    "SUS",
    "V",
    "^",
    "RNON",
    "B",
    "CBG",
    "SSRV"
  )
  pass <- c(
    "P",
    "NRS",
    "NRB",
    "&",
    "=",
    "M6F",
    "LVER",
    "LSSR",
    "LQ",
    "LOPR",
    "LMSD",
    "LICC",
    "HTH",
    "HNRO",
    "HMSR",
    "AC",
    "AL",
    "ALK",
    "ALT",
    "LOB",
    "AP",
    "BAC",
    "CAJ",
    "CBL",
    "CC",
    "CDI",
    "CG",
    "CKB",
    "CKBJ",
    "CKG",
    "CKJ",
    "CLC",
    "CON",
    "CUG",
    "DEC",
    "DI",
    "DOM",
    "ECI",
    "HLBL",
    "HQ",
    "HVER",
    "J",
    "J+",
    "J-",
    "L",
    "LCS",
    "LF",
    "LIS",
    "LL",
    "LLBL",
    "LMSR",
    "LNRO",
    "LR",
    "LT",
    "N",
    "NFNS",
    "O",
    "PQL",
    "RC",
    "REX",
    "RIN",
    "RMAX",
    "RNAF",
    "RP",
    "RR",
    "RV",
    "RVB",
    "SBB",
    "SLB",
    "SM",
    "SS",
    "T",
    "VS",
    "VVRR",
    "VVRR2",
    "ZZ",
    "J-1",
    "NA",
    "TR"
  )
  nondetect <- c(
    "BQL",
    "2-5B",
    "U",
    "LTGTE",
    "K",
    "IDL",
    "<2B",
    "BRL",
    "D>T",
    "DL"
  )
  overdetect <- c("E", "EE", "GT")
  df <- .tada_flag_by_groups(
    df,
    source_col = "Code",
    out_col = "TADA.MeasureQualifierCode.Flag",
    groups = list(
      "Non-Detect" = nondetect,
      "Over-Detect" = overdetect,
      "Suspect" = suspect,
      "Pass" = pass
    ),
    default = "Not Reviewed",
    na_default = "Pass"
  )
  unique(.tada_trim_char_cols(df))
}

# =========================
# Paired getters + updates
# =========================

#' Get WQX Characteristic Domain Table
#' @return data.frame with columns CharacteristicName, Char_Flag, Comparable.Name, and CAS.Number
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetCharacteristicRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    ref_cached <- .tada_cache_get(.WQXCharacteristicRef_cache_key)
    if (!is.null(ref_cached) && !isTRUE(refresh)) return(ref_cached)
  }
  if (download_only) {
    raw.data <- .tada_read_csv_url(
      .WQX_URLS$Characteristic,
      stringsAsFactors = FALSE
    )
    if (is.null(raw.data)) {
      stop("TADA_GetCharacteristicRef(download_only=TRUE): download failed.")
    }
    ref <- .TADA_normalize_characteristic_ref(raw.data)
    if (is.null(ref)) {
      stop("TADA_GetCharacteristicRef: Unexpected columns in downloaded table.")
    }
  } else {
    ref <- .tada_load_extdata_rda(
      pkg = "EPATADA",
      filename = "WQXCharacteristicRef.rda",
      object_name = "WQXCharacteristicRef",
      required_cols = c("CharacteristicName", "Char_Flag"),
      trim = TRUE
    )
    if (is.null(ref)) {
      raw.data <- .tada_read_csv_url(
        .WQX_URLS$Characteristic,
        stringsAsFactors = FALSE
      )
      if (is.null(raw.data)) {
        stop(
          "TADA_GetCharacteristicRef: extdata RDA not found and download failed."
        )
      }
      ref <- .TADA_normalize_characteristic_ref(raw.data)
      if (is.null(ref)) {
        stop(
          "TADA_GetCharacteristicRef: Unexpected columns in downloaded table."
        )
      }
    }
  }
  if (!download_only) {
    .tada_cache_set(.WQXCharacteristicRef_cache_key, ref)
  }
  ref
}

#' Update EPATADA Internal Copy of WQX Characteristic Domain Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateCharacteristicRef <- function() {
  ref <- TADA_GetCharacteristicRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    ref,
    obj_name = "WQXCharacteristicRef",
    pkg = "EPATADA",
    filename = "WQXCharacteristicRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(ref)
}

#' WQX QAQC Characteristic Validation Reference Table
#' @return data.frame with TADA.WQXVal.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetWQXCharValRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXCharValRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$QAQCCharacteristicValidation,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetWQXCharValRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$QAQCCharacteristicValidation,
      fallback_filename = "WQXcharValRef.rda",
      object_name = "WQXcharValRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Validation Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_WQXCharValRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXCharValRef_cache_key, df)
  }
  df
}

#' Update Characteristic Validation Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateWQXCharValRef <- function() {
  df <- TADA_GetWQXCharValRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXcharValRef",
    pkg = "EPATADA",
    filename = "WQXcharValRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Measure Unit Reference Table
#' @return data.frame of measure units
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetMeasureUnitRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXUnitRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(.WQX_URLS$MeasureUnit, stringsAsFactors = FALSE)
    if (is.null(df)) {
      stop("TADA_GetMeasureUnitRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$MeasureUnit,
      fallback_filename = "WQXunitRef.rda",
      object_name = "WQXunitRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Measure Unit Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_prepare_MeasureUnitRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXUnitRef_cache_key, df)
  }
  df
}

#' Update Measure Unit Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateMeasureUnitRef <- function() {
  df <- TADA_GetMeasureUnitRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXunitRef",
    pkg = "EPATADA",
    filename = "WQXunitRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Result Detection Condition Reference Table
#' @return data.frame with TADA.Detection_Type added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetDetCondRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXDetCondRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$ResultDetectionCondition,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetDetCondRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$ResultDetectionCondition,
      fallback_filename = "WQXResultDetectionConditionRef.rda",
      object_name = "WQXResultDetectionConditionRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Result Detection Condition Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_DetCondRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXDetCondRef_cache_key, df)
  }
  df
}

#' Update Result Detection Condition Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateDetCondRef <- function() {
  df <- TADA_GetDetCondRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXResultDetectionConditionRef",
    pkg = "EPATADA",
    filename = "WQXResultDetectionConditionRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Detection/Quantitation Limit Type Reference Table
#' @return data.frame with TADA.Limit_Type added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetDetLimitRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXDetLimitRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$DetectionQuantitationLimitType,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetDetLimitRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$DetectionQuantitationLimitType,
      fallback_filename = "WQXDetectionQuantitationLimitTypeRef.rda",
      object_name = "WQXDetectionQuantitationLimitTypeRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Detection Limit Type Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_DetLimitRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXDetLimitRef_cache_key, df)
  }
  df
}

#' Update Detection Quantitation Limit Type Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateDetLimitRef <- function() {
  df <- TADA_GetDetLimitRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXDetectionQuantitationLimitTypeRef",
    pkg = "EPATADA",
    filename = "WQXDetectionQuantitationLimitTypeRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Activity Type Reference Table
#' @return data.frame with TADA.ActivityType.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetActivityTypeRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXActivityTypeRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(.WQX_URLS$ActivityType, stringsAsFactors = FALSE)
    if (is.null(df)) {
      stop("TADA_GetActivityTypeRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$ActivityType,
      fallback_filename = "WQXActivityTypeRef.rda",
      object_name = "WQXActivityTypeRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Activity Type Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_ActivityTypeRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXActivityTypeRef_cache_key, df)
  }
  df
}

#' Update Activity Type Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateActivityTypeRef <- function() {
  df <- TADA_GetActivityTypeRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXActivityTypeRef",
    pkg = "EPATADA",
    filename = "WQXActivityTypeRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Monitoring Location Type Name Reference Table
#' @return data.frame with TADA.Media.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetMonLocTypeRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXMonLocTypeRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$MonitoringLocationType,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop("TADA_GetMonLocTypeRef(download_only=TRUE): download failed.")
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$MonitoringLocationType,
      fallback_filename = "WQXMonitoringLocationTypeNameRef.rda",
      object_name = "WQXMonitoringLocationTypeNameRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Monitoring Location Type Name Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_MonLocTypeRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXMonLocTypeRef_cache_key, df)
  }
  df
}

#' Update Monitoring Location Type Name Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateMonLocTypeRef <- function() {
  df <- TADA_GetMonLocTypeRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXMonitoringLocationTypeNameRef",
    pkg = "EPATADA",
    filename = "WQXMonitoringLocationTypeNameRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQP Organization and Provider Reference Table
#' @return data.frame with OrganizationIdentifier, OrganizationFormalName, ProviderName
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetWQPOrganizationRef <- function(download_only = FALSE, refresh = FALSE) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQPProviderRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(.WQP_URLS$Organization, stringsAsFactors = FALSE)
    if (is.null(df)) {
      stop("TADA_GetWQPOrganizationRef(download_only=TRUE): download failed.")
    }
    df <- .TADA_prepare_WQPOrgProviderRef(df)
  } else {
    df <- .tada_read_csv_url(.WQP_URLS$Organization, stringsAsFactors = FALSE)
    if (is.null(df)) {
      message(
        "Downloading latest WQP Organization and Provider Reference Table failed! Falling back to (possibly outdated) internal file."
      )
      df <- .tada_load_extdata_rda(
        pkg = "EPATADA",
        filename = "WQPOrganizationRef.rda",
        object_name = "WQPOrganizationRef", # renamed here
        required_cols = c(
          "OrganizationIdentifier",
          "OrganizationFormalName",
          "ProviderName"
        ),
        trim = TRUE
      )
      if (is.null(df)) {
        stop("Fallback extdata 'WQPOrganizationRef.rda' not found or invalid.")
      }
    } else {
      df <- .TADA_prepare_WQPOrgProviderRef(df)
    }
  }
  if (!download_only) {
    .tada_cache_set(.WQPProviderRef_cache_key, df)
  }
  df
}

#' Update WQP Organization Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateWQPOrganizationRef <- function() {
  df <- TADA_GetWQPOrganizationRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQPOrganizationRef", # renamed here
    pkg = "EPATADA",
    filename = "WQPOrganizationRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}

#' Get WQX Result Measure Qualifier Code Reference Table
#' @return data.frame with TADA.MeasureQualifierCode.Flag added
#' @param download_only Logical. If TRUE, bypasses the cache and package fallback and
#'   attempts to download the latest Activity Type reference table directly from WQX,
#'   returning it without updating the cache. Errors if the download fails. If FALSE
#'   (default), uses a cached copy when available and updates the cache; on download
#'   failure, falls back to the package’s internal file.
#'
#' @param refresh Logical. Only used when download_only = FALSE. If TRUE, ignore any
#'   cached copy and attempt to retrieve a fresh table (download, falling back to the
#'   package’s internal file on failure), then update the cache. If FALSE (default),
#'   return the cached table when available. Ignored when download_only = TRUE.
#' @export
TADA_GetMeasureQualifierCodeRef <- function(
  download_only = FALSE,
  refresh = FALSE
) {
  if (!download_only) {
    cached <- .tada_cache_get(.WQXMeasureQualifierCodeRef_cache_key)
    if (!is.null(cached) && !isTRUE(refresh)) return(cached)
  }
  if (download_only) {
    df <- .tada_read_csv_url(
      .WQX_URLS$ResultMeasureQualifier,
      stringsAsFactors = FALSE
    )
    if (is.null(df)) {
      stop(
        "TADA_GetMeasureQualifierCodeRef(download_only=TRUE): download failed."
      )
    }
  } else {
    df <- .tada_download_or_extdata_rda(
      url = .WQX_URLS$ResultMeasureQualifier,
      fallback_filename = "WQXMeasureQualifierCodeRef.rda",
      object_name = "WQXMeasureQualifierCodeRef",
      pkg = "EPATADA",
      on_fail_message = "Downloading latest Measure Qualifier Code Reference Table failed! Falling back to (possibly outdated) internal file."
    )
  }
  df <- .TADA_flag_MeasureQualifierCodeRef(df)
  if (!download_only) {
    .tada_cache_set(.WQXMeasureQualifierCodeRef_cache_key, df)
  }
  df
}

#' Update WQX Result Measure Qualifier Code Reference Table (DEV-TIME ONLY)
#' @keywords internal
.TADA_UpdateMeasureQualifierCodeRef <- function() {
  df <- TADA_GetMeasureQualifierCodeRef(download_only = TRUE, refresh = TRUE)
  .tada_save_ext_rda(
    df,
    obj_name = "WQXMeasureQualifierCodeRef",
    pkg = "EPATADA",
    filename = "WQXMeasureQualifierCodeRef.rda",
    compress = "xz",
    version = 2
  )
  invisible(df)
}
