# Used to store cached CST EPA304a Reference Table
EPACSTRef_Cached <- NULL

#' Get EPA Criteria Search Tool (CST) Data
#'
#' Function downloads and returns the newest available Criteria Search Tool and
#' associated EPA 304a Criteria pollutant names as a reference dataframe.
#' This dataframe is used in TADA_CreateParamRef() and
#' TADA_CreateUseParamRef() as the basis for the pulling in EPA304a recommended
#' pollutant names and use names.
#'
#' Currently only characteristics identified by the TADA Working Group as
#' priorities are included in the TADA crosswalk of WQP/TADA characteristics
#' and CST pollutant names. Run the following code in the console to
#' review the crosswalk:
#' 'utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))'
#'
#' @return Dataframe of EPA304a recommended criteria from EPA's Criteria Search
#' Tool (CST) for a pollutant and use name.
#'
#' @export
#'

TADA_GetEPACSTRef <- function() {
  # If there is a cached table available return it
  if (!is.null(EPACSTRef_Cached)) {
    return(EPACSTRef_Cached)
  }

  # Try to download up-to-date raw data

  raw.data <- tryCatch(
    {
      # read raw xlsx from url
      openxlsx::read.xlsx("https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx")
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Criteria Search Tool Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")))
  }

  # Creates and formats the CST ref table below:

  # Find the CST row that contains the column name of dataframe - removes extraneous details
  CST.begin <- as.integer(which(rowSums(is.na(raw.data)) == 0)[1])
  colnames(raw.data) <- as.character(raw.data[CST.begin, ])

  # import TADA unit reference for priority characteristics (characteristic specific)
  tada.char.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))

  # Pulls in column names that will be used as a reference table
  EPACSTRef <- raw.data %>%
    utils::tail(-CST.begin) %>%
    dplyr::filter(ENTITY_ABBR == "304A") %>%
    dplyr::left_join(tada.char.ref, by = c("POLLUTANT_NAME" = "CST.PollutantName"), relationship = "many-to-many") %>%
    dplyr::select(TADA.CharacteristicName, POLLUTANT_NAME,
      organization_identifier = ENTITY_ABBR,
      use_name = USE_CLASS_NAME_LOCATION_ETC, CRITERION_VALUE,
      CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
      CRITERIATYPE_ACUTECHRONIC, CRITERIATYPE_WATERORG, UNIT_NAME
    )

  # Remove intermediate variables
  rm(CST.begin, tada.char.ref, raw.data)

  # Save updated table in cache
  EPACSTRef_Cached <- EPACSTRef

  EPACSTRef
}

# Update Criteria Search Tool Reference Table internal file
# (for internal use only)

TADA_UpdateEPACSTRef <- function() {
  utils::write.csv(TADA_GetEPACSTRef(), file = "inst/extdata/CST.csv", row.names = FALSE)
}
