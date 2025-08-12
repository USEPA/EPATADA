# Used to store cached ATTAINSOrgIDsRef Reference Table
ATTAINSParameterWQPCharRef_Cached <- NULL

#' ATTAINS Parameter and WQP Characteristic Exact Match Reference Table
#'
#' Function downloads and returns the newest available crosswalk of exact
#' matches between ATTAINS.ParameterName and TADA.CharacteristicName.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated ATTAINSParameterWQPCharRef object
#'
#' @export
TADA_GetATTAINSParameterWQPCharRef <- function() {
  # If there is a cached table available return it
  if (!is.null(ATTAINSParameterWQPCharRef_Cached)) {
    return(ATTAINSParameterWQPCharRef_Cached)
  }

  # Try to download up-to-date raw data
  raw.data <- tryCatch(
    {
      # get data from ATTAINS
      attainsParamRef <- data.frame(name = rExpertQuery::EQ_DomainValues("param_name")[, "name"])

      WQXCharRef <- utils::read.csv(system.file("extdata", "WQXCharacteristicRef.csv", package = "EPATADA"))

      WQXCharRef$CharacteristicName <- toupper(WQXCharRef$CharacteristicName)

      matches <- intersect(WQXCharRef$CharacteristicName, attainsParamRef$rExpertQuery..EQ_DomainValues..param_name......name..)

      ## Add manual additional TADA.ComparableDataIdentifier and ATTAINS Parameter alias
      others <- data.frame(
        CharacteristicName = c(
          "ESCHERICHIA COLI",
          "DISSOLVED OXYGEN (DO)",
          "SPECIFIC CONDUCTANCE",
          "CHLOROPHYLL A",
          "ORGANIC CARBON",
          "ALKALINITY",
          "TOTAL DISSOLVED SOLIDS"
        ),
        Char_Flag = c(
          "Accepted",
          "Accepted",
          "Accepted",
          "Accepted",
          "Accepted",
          "Accepted",
          "Accepted"
        ),
        ATTAINS.ParameterName = c(
          "ESCHERICHIA COLI (E. COLI)",
          "DISSOLVED OXYGEN",
          "SPECIFIC CONDUCTIVITY",
          "CHLOROPHYLL-A",
          "TOTAL ORGANIC CARBON (TOC)",
          "ALKALINITY, TOTAL",
          "TOTAL DISSOLVED SOLIDS (TDS)"
        )
      )

      attainsWQXRef <- WQXCharRef %>%
        dplyr::inner_join(attainsParamRef, by = c("CharacteristicName" = "rExpertQuery..EQ_DomainValues..param_name......name..")) %>%
        dplyr::mutate(ATTAINS.ParameterName = CharacteristicName) %>%
        dplyr::full_join(others, by = c("CharacteristicName", "Char_Flag", "ATTAINS.ParameterName")) %>%
        dplyr::distinct()
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest ATTAINS and WQP Char Ref Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "ATTAINSParameterWQPCharRef.csv", package = "EPATADA")))
  }

  ATTAINSParameterWQPCharRef <- raw.data %>%
    dplyr::distinct()

  # Save updated table in cache
  ATTAINSParameterWQPCharRef_Cached <- ATTAINSParameterWQPCharRef

  ATTAINSParameterWQPCharRef
}

# Update  ATTAINS Organization Identifier Reference Table
# (for internal use only)
TADA_UpdateATTAINSParameterWQPCharRef <- function() {
  utils::write.csv(TADA_GetATTAINSParameterWQPCharRef(), file = "inst/extdata/ATTAINSParameterWQPCharRef.csv", row.names = FALSE)
}

# Used to store cached ATTAINSOrgIDsRef Reference Table
ATTAINSOrgIDsRef_Cached <- NULL

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
      rExpertQuery::EQ_DomainValues("org_id")
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

# Used to store cached ATTAINSParamUseOrg Reference Table
ATTAINSParamUseOrgRef_Cached <- NULL

#' ATTAINS Parameter and Use Name by Organization Reference Key
#'
#' Function downloads and returns the newest available ATTAINS domain values
#' reference dataframe which includes all parameters and uses
#' listed as a cause by ATTAINS organizations in previous assessments.
#' This dataframe is used in TADA_CreateParamRef() and
#' TADA_CreateUseParamRef() as the basis for pulling in prior ATTAINS
#' parameter names and use names by organization name. This helps to filter
#' selections in the Excel drop down menu.
#'
#' @return Dataframe including ATTAINS parameters and uses for each organization.
#'
#' @export
#'
TADA_GetATTAINSParamUseOrgRef <- function() {
  # If there is a cached table available return it
  if (!is.null(ATTAINSParamUseOrgRef_Cached)) {
    return(ATTAINSParamUseOrgRef_Cached)
  }

  # from national download
  nat.assessments <- rExpertQuery::EQ_NationalExtract("assessments")

  if (!exists("nat.assessments")) {
    message("Downloading latest ATTAINSParamUseOrg Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA")))
  }

  # to match you origianal query which by default was considering only the
  # latest cycle form each org, you could skip this step and use params from
  # all assessment cycles

  latest.assessments <- nat.assessments %>%
    dplyr::group_by(organizationId) %>%
    dplyr::slice_max(reportingCycle) %>%
    dplyr::select(-objectId) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

    latest.params <- latest.assessments %>%
    dplyr::select(organizationId, organizationName,
                  organizationType, parameterName,
                  useName) %>%
    dplyr::rename( ATTAINS.OrganizationIdentifier = organizationId,
                   ATTAINS.OrganizationName = organizationName,
                   ATTAINS.OrganizationType = organizationType,
                   ATTAINS.ParameterName = parameterName,
                   ATTAINS.UseName = useName) %>%
      dplyr::distinct()

  # remove intermediate variables
  rm(nat.assessments, latest.assessments)
  # If the download failed fall back to internal data (and report it)

  ATTAINSParamUseOrgRef <- latest.params

  rm(latest.params)

  # Save updated table in cache
  ATTAINSParamUseOrgRef_Cached <- ATTAINSParamUseOrgRef

  ATTAINSParamUseOrgRef
}

# Update ATTAINSParamUseOrg Reference Table internal file
# (for internal use only)
TADA_UpdateATTAINSParamUseOrgRef <- function() {
  utils::write.csv(TADA_GetATTAINSParamUseOrgRef(), file = "inst/extdata/ATTAINSParamUseEntityRef.csv", row.names = FALSE)
}
