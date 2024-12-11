#' ATTAINS Organization Identifier Reference Table
#'
#' Function downloads and returns the newest available crosswalk of state/tribe/territory codes
#' and their respective organization names and organization identifiers.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated ATTAINSOrgIDsRef object
#'
#' @export

TADA_GetATTAINSOrgIDsRef <- function() {
  # If there is a cached table available return it
  if (!is.null(ATTAINSOrgIDsRef_Cached)) {
    return(ATTAINSOrgIDsRef_Cached)
  }
  
  # Try to download up-to-date raw data
  
  raw.data <- tryCatch(
    {
      # get data from ATTAINS
    rATTAINS::domain_values(domain_name = "OrgName")
    },
    error = function(err) {
      NULL
    }
  )
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest ATTAINS Organization Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "ATTAINSOrgIDsRef.csv", package = "EPATADA")))
  }
  
  ATTAINSOrgIDsRef <- raw.data %>%
    dplyr::distinct()
  
  # Save updated table in cache
  ATTAINSOrgIDsRef_Cached <- ATTAINSOrgIDsRef
  
  ATTAINSOrgIDsRef
}

# Update  ATTAINS Organization Identifier Reference Table
# (for internal use only)

TADA_UpdateATTAINSOrgIDsRef <- function() {
  utils::write.csv(TADA_GetATTAINSOrgIDsRef(), file = "inst/extdata/ATTAINSOrgIDsRef.csv", row.names = FALSE)
}


# Used to store cached Measure Unit Reference Table
ATTAINSOrgIDsRef_Cached <- NULL