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



# Used to store cached ATTAINSParamUseOrg Reference Table
ATTAINSParamUseOrgRef_Cached <- NULL

#' ATTAINS Parameter and Use Name by Organization Reference Key
#'
#' Function downloads and returns the newest available ATTAINS domain values
#' reference dataframe which includes all parameters and uses
#' listed as a cause by ATTAINS organizations in previous assessments.
#' This dataframe is used in TADA_CreateParamRef() and
#' TADA_CreateParamUseRef() as the basis for the pulling in prior ATTAINS
#' parameter names and use names by organization name. This helps to filter
#' selections in the Excel drop down menu.
#'
#' @return Dataframe including ATTAINS parameters and uses for each organization.
#'
#' @export
#'

TADA_GetATTAINSParamUseOrgRef <- function() {
  # # If there is a cached table available return it
  # if (!is.null(ATTAINSParamUseOrgRef_Cached)) {
  #   return(ATTAINSParamUseOrgRef_Cached)
  # }
  #
  # # Try to download up-to-date raw data
  #
  # raw.data <- tryCatch(
  #   {
  #     # reads rATTAINS domain value
  #     org_id <- rATTAINS::domain_values("OrgName")[[3]]
  #     all.data <- list()
  #
  #     for(i in 1:length(org_id)){
  #       all.data[[i]] <- rATTAINS::assessments(organization_id = org_id[i])[[2]]
  #     }
  #
  #     all.data2 <- dplyr::bind_rows(all.data, .id = "column_label")
  #   },
  #   error = function(err) {
  #     NULL
  #   }
  # )
  #
  # # remove intermediate variables
  # rm(org_id, all.data, all.data2)
  #
  # # If the download failed fall back to internal data (and report it)
  # if (is.null(raw.data)) {
  #   message("Downloading latest ATTAINSParamUseOrg Reference Table failed!")
  #   message("Falling back to (possibly outdated) internal file.")
  #   return(utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA")))
  # }
  #
  # # Creates and formats the ATTAINSParamUseOrg ref table containing parameters by use name for each org
  # use_attainments <- raw.data %>% tidyr::unnest(c(use_attainments), names_sep = ".")
  # use_parameters <- use_attainments %>% tidyr::unnest(c(parameters), names_sep = ".")
  #
  # ATTAINSParamUseOrgRef <- use_parameters %>%
  #   dplyr::select(organization_identifier, organization_name, organization_type_text,
  #                 use_attainments.use_name, parameters.parameter_name) %>%
  #   dplyr::distinct()
  #
  # # remove intermediate variables
  # rm(use_attainments, use_parameters)
  #

  ATTAINSParamUseOrgRef <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))

  # Save updated table in cache
  ATTAINSParamUseOrgRef_Cached <- ATTAINSParamUseOrgRef

  ATTAINSParamUseOrgRef
}


# Update ATTAINSParamUseOrg Reference Table internal file
# (for internal use only)

TADA_UpdateATTAINSParamUseOrgRef <- function() {
  utils::write.csv(TADA_GetATTAINSParamUseOrgRef(), file = "inst/extdata/ATTAINSParamUseEntityRef.csv", row.names = FALSE)
}
