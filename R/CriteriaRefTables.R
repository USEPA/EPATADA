# Used to store cached CriteriaSearchToolRef Reference Table
CriteriaSearchToolRef_Cached <- NULL

#' Criteria Search Tool (CST) Reference Table
#'
#' This function downloads State-Specific Water Quality Standards Effective
#' under the Clean Water Act (CWA) from EPA's Criteria Search Tool. This file is
#' reformatted as a data frame for use in R. This function caches the table after
#' it has been called once so subsequent calls will be faster. To get the data
#' dictionary for the CST see TADA_GetLegendCSTRef. For the WQS document sources
#' see TADA_GetSourcesCSTRef.
#'
#' @return Updated sysdata.rda with updated CriteriaSearchToolRef object
#'
#' @export
#'
#' @examples
#' CWACriteria <- TADA_GetCriteriaSearchToolRef()
#'
TADA_GetCriteriaSearchToolRef <- function() {
  # If there is a cached table available return it
  if (!is.null(CriteriaSearchToolRef_Cached)) {
    return(CriteriaSearchToolRef_Cached)
  }
  # Try to download up-to-date raw data
  raw.data <- tryCatch(
    {
      # read raw xlsx from url
      openxlsx::read.xlsx(
        "https://www.epa.gov/system/files/documents/2025-07/criteria-search-tool-data.xlsx",
        sheet = 3
      )
    },
    error = function(err) {
      NULL
    }
  )
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Criteria Search Tool Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    # Load the dataset from the package into this function's environment
    utils::data("CriteriaSearchToolRef", package = "EPATADA", envir = environment())
    CriteriaSearchToolRef <- get("CriteriaSearchToolRef", envir = environment())
    CriteriaSearchToolRef <- dplyr::distinct(CriteriaSearchToolRef)
    # Cache and return
    CriteriaSearchToolRef_Cached <<- CriteriaSearchToolRef
    return(CriteriaSearchToolRef)
  }

  CriteriaSearchToolRef <- dplyr::distinct(raw.data)
  CriteriaSearchToolRef_Cached <- CriteriaSearchToolRef
  CriteriaSearchToolRef
}


# Update CriteriaSearchToolRef Reference Table internal file
# (for internal use only)
TADA_UpdateCriteriaSearchToolRef <- function() {
  CriteriaSearchToolRef <- TADA_GetCriteriaSearchToolRef()
  save(
    CriteriaSearchToolRef,
    file = "inst/extdata/CriteriaSearchToolRef.rda",
    ascii = FALSE,
    compress = "xz",
    version = 3
  )
}


# Used to store cached LegendCSTRef Reference Table
LegendCSTRef_Cached <- NULL

#' Legend for the Criteria Search Tool Reference Table
#'
#' This function downloads the legend from the EPA's Criteria Search Tool which
#' contains State-Specific Water Quality Standards Effective under the Clean
#' Water Act (CWA). This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated CriteriaSearchToolRef object
#'
#' @export
#'
#' @examples
#' CWACriteria <- TADA_GetLegendCSTRef()
#'
TADA_GetLegendCSTRef <- function() {
  # If there is a cached table available return it
  if (!is.null(LegendCSTRef_Cached)) {
    return(LegendCSTRef_Cached)
  }
  # Try to download up-to-date raw data
  raw.data <- tryCatch(
    {
      # read raw xlsx from url
      openxlsx::read.xlsx(
        "https://www.epa.gov/system/files/documents/2025-07/criteria-search-tool-data.xlsx",
        sheet = 1
      )
    },
    error = function(err) {
      NULL
    }
  )
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message(
      "Downloading latest Legend for the Criteria Search Tool Reference Table failed!"
    )
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file(
      "extdata",
      "CriteriaSearchToolRef.csv",
      package = "EPATADA"
    )))
  }
  LegendCSTRef <- raw.data |> dplyr::distinct()
  # Save updated table in cache
  LegendCSTRef_Cached <- LegendCSTRef
  LegendCSTRef
}


# Update LegendCSTRef Reference Table internal file
# (for internal use only)
TADA_UpdateLegendCSTRef <- function() {
  utils::write.csv(
    TADA_GetLegendCSTRef(),
    file = "inst/extdata/LegendCSTRef.csv",
    row.names = FALSE
  )
}


# Used to store cached SourcesCSTRef Reference Table
SourcesCSTRef_Cached <- NULL

#' Sources for the Criteria Search Tool Reference Table
#'
#' This function downloads the sources from the EPA's Criteria Search Tool which
#' contains State-Specific Water Quality Standards Effective under the Clean
#' Water Act (CWA). This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated SourcesCSTRef object
#'
#' @export
#'
#' @examples
#' CWACriteria <- TADA_GetSourcesCSTRef()
#'
TADA_GetSourcesCSTRef <- function() {
  # If there is a cached table available return it
  if (!is.null(SourcesCSTRef_Cached)) {
    return(SourcesCSTRef_Cached)
  }
  # Try to download up-to-date raw data
  raw.data <- tryCatch(
    {
      # read raw xlsx from url
      openxlsx::read.xlsx(
        "https://www.epa.gov/system/files/documents/2025-07/criteria-search-tool-data.xlsx",
        sheet = 2
      )
    },
    error = function(err) {
      NULL
    }
  )
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message(
      "Downloading latest Sources for the Criteria Search Tool Reference Table failed!"
    )
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file(
      "extdata",
      "CriteriaSearchToolRef.csv",
      package = "EPATADA"
    )))
  }
  SourcesCSTRef <- raw.data |> dplyr::distinct()
  # Save updated table in cache
  SourcesCSTRef_Cached <- SourcesCSTRef
  SourcesCSTRef
}


# Update SourcesCSTRef Reference Table internal file
# (for internal use only)
TADA_UpdateSourcesCSTRef <- function() {
  utils::write.csv(
    TADA_GetSourcesCSTRef(),
    file = "inst/extdata/SourcesCSTRef.csv",
    row.names = FALSE
  )
}
