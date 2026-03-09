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
      spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message(
      "TADA_UpdateATTAINSOrgIDsRef: Downloading latest ATTAINS Organization ID domain options failed! Falling back to (possibly outdated) internal file."
    )
    return(utils::read.csv(system.file(
      "extdata",
      "ATTAINSOrgIDsRef.csv",
      package = "EPATADA"
    )))
  }

  ATTAINSOrgIDsRef <- raw.data |> dplyr::distinct()

  # Save updated table in cache
  ATTAINSOrgIDsRef_Cached <- ATTAINSOrgIDsRef

  ATTAINSOrgIDsRef
}

# Update ATTAINS Organization Identifier Reference Table
# (for internal use only)
TADA_UpdateATTAINSOrgIDsRef <- function() {
  utils::write.csv(
    TADA_GetATTAINSOrgIDsRef(),
    file = "inst/extdata/ATTAINSOrgIDsRef.csv",
    row.names = FALSE
  )
}

# Used to store cached ATTAINSParamUseOrg Reference Table
ATTAINSParamUseOrgRef_Cached <- NULL

#' ATTAINS Parameter and Use Name by Organization Reference Key
#'
#' Function downloads and returns the newest available ATTAINS domain values
#' reference dataframe which includes all parameters and uses
#' listed as a cause by ATTAINS organizations in previous assessments.
#' This dataframe is used as the basis for pulling in prior ATTAINS
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
  nat.assessments <- spsUtil::quiet(rExpertQuery::EQ_NationalExtract(
    "assessments"
  ))

  if (!exists("nat.assessments")) {
    message("Downloading latest ATTAINSParamUseOrg Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file(
      "extdata",
      "ATTAINSParamUseEntityRef.csv",
      package = "EPATADA"
    )))
  }

  # considers only the latest cycle from each org, you could skip this step
  # and use params from all assessment cycles - What is preferred?

  latest.assessments <- nat.assessments |>
    dplyr::group_by(organizationId) |>
    # dplyr::slice_max(reportingCycle) |>
    dplyr::select(-objectId) |>
    dplyr::distinct() |>
    dplyr::ungroup()

  latest.params <- latest.assessments |>
    dplyr::select(
      organizationId,
      organizationName,
      organizationType,
      parameterName,
      useName,
      waterType
    ) |>
    dplyr::rename(
      ATTAINS.OrganizationIdentifier = organizationId,
      ATTAINS.OrganizationName = organizationName,
      ATTAINS.OrganizationType = organizationType,
      ATTAINS.ParameterName = parameterName,
      ATTAINS.UseName = useName,
      ATTAINS.WaterType = waterType
    ) |>
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
  utils::write.csv(
    TADA_GetATTAINSParamUseOrgRef(),
    file = "inst/extdata/ATTAINSParamUseEntityRef.csv",
    row.names = FALSE
  )
}
