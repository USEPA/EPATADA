#' Get Monitoring Location Identifier and Assessment Unit Identifier
#' Crosswalk from ATTAINS
#'
#' Tribes and States who participate in electronic reporting of water quality
#' conditions through EPA ATTAINS may optionally submit a crosswalk of WQP
#' monitoring location identifiers associated with their assessment units to
#' ATTAINS. If an organization has recorded this information in ATTAINS,
#' this function can be used to get the ATTAINS user submitted crosswalk of
#' known monitoring location identifiers and assessment units. As of 2025, all
#' tribal nations record this information in ATTAINS but only a few states.
#'
#' @param org_id Character string. The ATTAINS organization identifier must be
#' supplied by the user. More than one org_id may be provided.
#' Enter `rExpertQuery::EQ_DomainValues("org_id")` into the console to
#' get a list of valid organization identifiers. A list of organization identifiers
#' can also be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "code" column of the "OrgName" tab.
#' When org_id = "all", all assessment unit/monitoring locations matches recorded in
#' ATTAINS from all organizations will be returned. The default is org_id = "all".

#' @param api_key Optional character string. An api key for Expert Query web
#' services. If not supplied, the default TADA api key will be used. For best
#' performance, it is recommended that users obtain and use their own api key.
#' Request an api key here: https://owapps.epa.gov/expertquery/api-documentation
#'
#' @param batch_upload Boolean argument. When batch_upload = TRUE, the final column
#' names in the output will match those required for batch upload to ATTAINS. When
#' batch_upload = FALSE, the output column names will be consistent with the TADA
#' workflow. Default is batch_upload = FALSE.
#'
#' @return When batch_upload = FALSE, A dataframe with six columns:
#' OrganizationIdentifier, ATTAINS.OrganizationIdentifier,
#' ATTAINS.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
#' ATTAINS.MonitoringDataLinkText, ATTAINS.WaterType is returned.
#' When batch_upload = TRUE, A dataframe with four columns:
#' MS_ORG_ID, MS_LOCATION_ID, ASSESSMENT_UNIT_ID, MS_DATA_LINK
#' is returned. This is the crosswalk between monitoring location identifiers
#' and assessment units that the state or tribal organization submitted
#' to ATTAINS (optional). If an ATTAINS organization has not submitted this
#' information in ATTAINS, the function will not return a dataframe.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Alaska example in
#' AK_crosswalk <- TADA_GetATTAINSAUMLCrosswalk(
#'   org_id = "AKDECWQ", batch_upload = TRUE
#' )
#'
#' # Alaska example with ATTAINS prefix compatible with TADA Analysis workflow
#' AK_crosswalk2 <- TADA_GetATTAINSAUMLCrosswalk(
#'   org_id = "AKDECWQ", batch_upload = FALSE
#' )
#'
#' # Pueblo of Tesuque example
#' PUEBLOOFTESUQUE_crosswalk <- TADA_GetATTAINSAUMLCrosswalk(
#'   org_id = "PUEBLOOFTESUQUE"
#' )
#'
#' # Arizona example, returns an empty df as of 10/17/25
#' AZ_crosswalk <- TADA_GetATTAINSAUMLCrosswalk(org_id = "21ARIZ")
#' }
#'
TADA_GetATTAINSAUMLCrosswalk <- function(
  org_id = "all",
  batch_upload = FALSE,
  api_key = NULL
) {
  # get default api_key if user does not supply one
  if (is.null(api_key)) {
    api_key <- .setEQKey()
  }

  # get reference df of all organization ids
  org.ref <- TADA_GetATTAINSOrgIDsRef()

  # check to see if org_id is not "all"
  if (org_id != "all") {
    # check to make sure organization ids supplied by user match those in ATTAINS
    if (all(!org_id %in% org.ref$code)) {
      # remove intermediate objects
      rm(org.ref)

      # stop function if organization ids are not found in ATTAINS
      stop(paste0(
        "TADA_GetATTAINSAUMLCrosswalk: ",
        "organization identifier(s) entered by user not found in ATTAINS."
      ))
    }
  }

  # if org_id is NULL return the AU/ML national extract, otherwise query by org_id
  if (org_id == "all") {
    au.info <- spsUtil::quiet(rExpertQuery::EQ_NationalExtract("au_mls"))
  } else {
    au.info <- spsUtil::quiet(rExpertQuery::EQ_AUsMLs(
      org_id = org_id,
      api_key = api_key
    ))
  }

  # select, filter and rename crosswalk columns
  au.crosswalk <- au.info |>
    dplyr::select(
      monitoringLocationId,
      monitoringLocationOrgId,
      assessmentUnitId,
      monitoringLocationDataLink,
      waterType,
      organizationId
    ) |>
    dplyr::filter(!is.na(monitoringLocationId), monitoringLocationId != "") |>
    dplyr::distinct() |>
    dplyr::rename(
      ATTAINS.AssessmentUnitIdentifier = assessmentUnitId,
      MonitoringLocationIdentifier = monitoringLocationId,
      OrganizationIdentifier = monitoringLocationOrgId,
      MonitoringDataLinkText = monitoringLocationDataLink,
      ATTAINS.WaterType = waterType,
      ATTAINS.OrganizationIdentifier = organizationId
    ) |>
    dplyr::rename(
      ATTAINS.MonitoringLocationIdentifier = MonitoringLocationIdentifier,
      ATTAINS.MonitoringDataLinkText = MonitoringDataLinkText
    ) |>
    dplyr::select(
      OrganizationIdentifier,
      ATTAINS.OrganizationIdentifier,
      ATTAINS.MonitoringLocationIdentifier,
      ATTAINS.AssessmentUnitIdentifier,
      ATTAINS.MonitoringDataLinkText,
      ATTAINS.WaterType
    )

  # remove intermediate object
  rm(au.info)

  # if org_id is NULL, set to "all organizations" for printed message
  if (org_id == "all") {
    org_id <- "all organizations"
  }

  # check for how many org_ids were provided by user if org_id is not NULL
  if (!is.null(org_id)) {
    # determine how many org_ids were provided
    if (length(org_id) > 1) {
      # create string for print message
      org_id <- stringi::stri_replace_last(
        paste(org_id, collapse = ", "),
        fixed = ", ",
        replacement = " and "
      )
    }
  }

  # check to see if the crosswalk contains any results
  if (length(au.crosswalk$ATTAINS.MonitoringLocationIdentifier) > 0) {
    # print a message describing the number of results
    print(paste0(
      "TADA_GetATTAINSAUMLCrosswalk: ",
      "There are ",
      nrow(au.crosswalk),
      " monitoring location identifiers associated with assessment units for ",
      org_id,
      " in ATTAINS."
    ))
  }

  # if batch_upload is TRUE, create an ATTAINS formatted batch upload df
  if (
    batch_upload == TRUE &
      length(au.crosswalk$ATTAINS.MonitoringLocationIdentifier) > 0
  ) {
    au.crosswalk <- au.crosswalk |>
      dplyr::select(-ATTAINS.WaterType) |>
      dplyr::select(-ATTAINS.OrganizationIdentifier) |>
      dplyr::rename(
        ASSESSMENT_UNIT_ID = ATTAINS.AssessmentUnitIdentifier,
        MS_ORG_ID = ATTAINS.MonitoringLocationIdentifier,
        MS_LOCATION_ID = OrganizationIdentifier,
        MS_DATA_LINK = ATTAINS.MonitoringDataLinkText
      )
  }

  # print a message if no crosswalk is found
  if (length(au.crosswalk$ATTAINS.MonitoringLocationIdentifier) == 0) {
    print(paste0(
      "TADA_GetATTAINSAUMLCrosswalk: ",
      "No monitoring location identifiers were recorded in ATTAINS for ",
      org_id,
      " assessment units."
    ))

    rm(org.ref)
  }

  return(au.crosswalk)
}


#' Update Monitoring Location Identifier and Assessment Unit Identifier
#' Crosswalk in ATTAINS
#'
#' This function creates the batch upload files needed to create or update
#' Monitoring Location Identifiers in ATTAINS Assessment Unit profiles. Users
#' can specify whether all records should be overwritten (replaced) or if new
#' Monitoring Location Identifiers should be appended (added) to existing
#' records.
#'
#' ATTAINS batch upload files are available here:
#' https://www.epa.gov/waterdata/upload-data-resources-registered-attains-users#batch-upload-templates
#' See Assessment Unit Batch Upload Template.
#'
#' @param org_id Character string. The ATTAINS organization identifier may be
#' supplied by the user. More than one org_id may be provided when batch_upload
#' = FALSE. If batch_upload = TRUE, only one org_id may be provided as batch
#' uploads to ATTAINS can only be done for one organization at a time. A list of
#' organization identifiers can be found by downloading the ATTAINS Domains
#' Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param. When
#' org_id = "all", all assessment unit/monitoring locations matches recorded in
#' ATTAINS from all organizations will be returned. The default is org_id = "all".
#'
#' @param api_key Optional character string. An api key for Expert Query web
#' services. If not supplied, the default TADA api key will be used. For best
#' performance, it is recommended that users obtain and use their own api key.
#' Request an api key here: https://owapps.epa.gov/expertquery/api-documentation
#'
#' @param wqp_data_links Character argument. When wqp_data_links is equal to
#' "add" or "replace", the function will build the URL for the Water Quality
#' Portal Data Site page for each Monitoring Location Identifier in the data
#' frame. When wqp_data_links = "add", the URL will be added to any existing text in the
#' MS_DATA_LINK_TEXT column. When wqp_data_links = "replace", the URL will
#' replace any existing text in the MS_DATA_LINK_TEXT column. When
#' wqp_data_links = "none", no URLs will be created or added to the returned
#' data frame. Default is wqp_data_links = "add".
#'
#' @param check_links Boolean argument. When check_links = TRUE the function
#' will examine the response code of each MS_DATA_LINK URL and only retain those
#' with a 200 response, which indicates the URL is valid.
#'
#' @param attains_replace Character argument. When attains_replace = FALSE, all
#' Monitoring Location Identifiers in the user supplied crosswalk will be
#' appended to the existing ATTAINS crosswalk. When attains_replace = TRUE,
#' Monitoring Location Identifiers will only be retained if they are in the
#' user supplied crosswalk. Default equals FALSE.
#'
#' @param update_mlid Boolean argument. Updates MonitoringLocationIdentifier to
#' be compatible with WQP MonitoringLocationIdentifiers by adding prefix for provider
#' and organization identifier if needed when update_mlid = TRUE. Default is
#' update_mlid = TRUE.
#'
#' @param crosswalk A user-supplied dataframe. The column names and contents can match either
#' those in the ATTAINS_batchupload (ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID,
#' and MS_DATA_LINK) or the ATTAINS_crosswalk (OrganizationIdentifier,
#' ATTAINS.OrganizationIdentifier, ATTAINS.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, ATTAINS.MonitoringDataLinkText, and
#' ATTAINS.WaterType) output from TADA_CreateAUMLCrosswalk or
#' TADA_CreateATTAINSAUMLCrosswalk. When crosswalk = NULL, the crosswalk will be
#' downloaded from ATTAINS. This allows users to add URLs for the Water Quality
#' Portal data site pages to the ATTAINS assessment unit profile where possible
#' without updating other information in ATTAINS.
#'
#' @param batch_upload Boolean argument. When batch_upload = TRUE, the column
#' names in the returned df will match the column names required for ATTAINS
#' batch upload. When batch_upload = FALSE, the column names will match those in
#' the TADA workflow. Default is batch_upload = FALSE.
#'
#' @return When batch_upload = FALSE, A dataframe with six columns:
#' OrganizationIdentifier, ATTAINS.OrganizationIdentifier,
#' ATTAINS.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
#' ATTAINS.MonitoringDataLinkText, ATTAINS.WaterType is returned.
#' When batch_upload = TRUE, A dataframe with four columns:
#' MS_ORG_ID, MS_LOCATION_ID, ASSESSMENT_UNIT_ID, MS_DATA_LINK
#' is returned. This is the crosswalk between monitoring location identifiers
#' and assessment units that the state or tribal organization submitted
#' to ATTAINS (optional).
#'
#' @seealso [TADA_GetATTAINSAUMLCrosswalk()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Alaska example to updated data links with no user supplied crosswalk
#' AK_adddatalinks <- TADA_UpdateATTAINSAUMLCrosswalk(
#'   org_id = "AKDECWQ",
#'   crosswalk = NULL,
#'   attains_replace = FALSE,
#'   wqp_data_links = "replace"
#' )
#'
#' # Alaska example using a user supplied crosswalk to update entries in
#' # ATTAINS by appending user supplied information to ATTAINS crosswalk
#'
#' # example assessment unit identifiers
#' ATTAINS.AssessmentUnitIdentifier <- c(
#'   "AK_M_1021211_000", "AK_M_1021008_000",
#'   "AK_M_1021109_013", "AK_M_1021109_013",
#'   "AK_M_1021109_013"
#' )
#'
#' # example organization identifiers
#' OrganizationIdentifier <-
#'   c("AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ")
#' ATTAINS.OrganizationIdentifier <-
#'   c("AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ")
#'
#' # example monitoring location identifiers
#' ATTAINS.MonitoringLocationIdentifier <- c(
#'   "ExampleSite1", "ExampleSite2", "ExampleSite3",
#'   "ExampleSite4", "ExampleSite5"
#' )
#'
#' # example water types
#' ATTAINS.WaterType <- c(
#'   "BEACH", "BAY", "CREEK",
#'   "ESTUARY", "CREEK"
#' )
#'
#' # example urls
#' ATTAINS.MonitoringDataLinkText <- c(
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/"
#' )
#'
#' # create example crosswalk data frame
#' ex.user.cw <- data.frame(
#'   ATTAINS.MonitoringLocationIdentifier, OrganizationIdentifier,
#'   ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier,
#'   ATTAINS.WaterType, ATTAINS.MonitoringDataLinkText
#' )
#'
#' AK_appenduserdata <- TADA_UpdateATTAINSAUMLCrosswalk(
#'   org_id = "AKDECWQ",
#'   crosswalk = ex.user.cw,
#'   attains_replace = FALSE,
#'   wqp_data_links = "none"
#' )
#' }
#'
TADA_UpdateATTAINSAUMLCrosswalk <- function(
  org_id = "all",
  crosswalk = NULL,
  attains_replace = FALSE,
  wqp_data_links = "add",
  update_mlid = TRUE,
  batch_upload = FALSE,
  check_links = FALSE,
  api_key = NULL
) {
  # get default api_key if user does not supply one
  if (is.null(api_key)) {
    api_key <- .setEQKey()
  }

  if (is.null(crosswalk) & attains_replace == TRUE) {
    stop(paste0(
      "TADA_UpdateATTAINSAUMLCrosswalk: ",
      "in order to replace MonitoringLocations stored in ATTAINS ",
      "user must provide a ",
      "MonitoringLocation/AssessmentUnitcrosswalk."
    ))
  }

  if (
    (org_id == "all" & batch_upload == TRUE) |
      (length(org_id) > 1 & batch_upload == TRUE)
  ) {
    stop(paste0(
      "TADA_UpdateATTAINSAUMLCrosswalk: ",
      "in order to create a batch upload file, the user must specify a single org_id."
    ))
  }

  # if the crosswalk is not a dataframe, stop the function
  if (!is.data.frame(crosswalk) & !is.null(crosswalk)) {
    stop(paste0(
      "TADA_UpdateATTAINSAUMLCrosswalk: ",
      "A crosswalk dataframe with columns 'ATTAINS.assessmentunit.identifier' and ",
      "'MonitoringLocationIdentifier' or setting crosswalk = NULL is required to run ",
      "this function."
    ))
  }

  # check that crosswalk is a dataframe before proceeding
  if (is.data.frame(crosswalk)) {
    # check crosswalk has all of the required columns
    crosswalk_cols <- c(
      "ATTAINS.AssessmentUnitIdentifier",
      "ATTAINS.MonitoringLocationIdentifier",
      "OrganizationIdentifier",
      "ATTAINS.OrganizationIdentifier"
    )

    batchupload_cols <- c(
      "ASSESSMENT_UNIT_ID",
      "MS_ORG_ID",
      "MS_LOCATION_ID",
      "MS_DATA_LINK"
    )

    if (
      !all(crosswalk_cols %in% names(crosswalk)) &
        !all(batchupload_cols %in% names(crosswalk))
    ) {
      stop(paste0(
        "Column names must reflect the TADA workflow requirements. ",
        "Review function documentation for more information."
      ))
    }

    if (all(batchupload_cols %in% names(crosswalk))) {
      crosswalk <- crosswalk |>
        dplyr::rename(
          ATTAINS.AssessmentUnitIdentifier = ASSESSMENT_UNIT_ID,
          OrganizationIdentifier = MS_ORG_ID,
          ATTAINS.MonitoringLocationIdentifier = MS_LOCATION_ID,
          ATTAINS.MonitoringDataLinkText = MS_DATA_LINK
        ) |>
        dplyr::mutate(ATTAINS.OrganizationIdentifier = org_id)

      wat.types <- au.info <- spsUtil::quiet(rExpertQuery::EQ_AUsMLs(
        org_id = org_id,
        api_key = api_key
      )) |>
        dplyr::select(assessmentUnitId, waterType) |>
        dplyr::rename(
          ATTAINS.AssessmentUnitIdentifier = assessmentUnitId,
          ATTAINS.WaterType = waterType
        ) |>
        dplyr::distinct()

      crosswalk <- crosswalk |>
        dplyr::left_join(
          wat.types,
          by = dplyr::join_by(ATTAINS.AssessmentUnitIdentifier)
        )
    }
  }

  if (attains_replace == FALSE) {
    # create assessment unit crosswalk from ATTAINS

    attains.crosswalk <- suppressMessages(TADA_GetATTAINSAUMLCrosswalk(
      org_id = org_id
    ))

    if (is.null(crosswalk)) {
      update.crosswalk <- attains.crosswalk

      rm(attains.crosswalk)
    }

    if (!is.null(crosswalk)) {
      # combine user supplied and attains crosswalks to create one crosswalk
      # no rows are omitted
      update.crosswalk <- attains.crosswalk |>
        dplyr::full_join(
          crosswalk,
          by = dplyr::join_by(
            ATTAINS.MonitoringLocationIdentifier,
            OrganizationIdentifier,
            ATTAINS.OrganizationIdentifier,
            ATTAINS.AssessmentUnitIdentifier,
            ATTAINS.MonitoringDataLinkText,
            ATTAINS.WaterType
          )
        ) |>
        dplyr::distinct()

      rm(attains.crosswalk, crosswalk)
    }
  }

  # when replace is true, only rows in user-supplied crosswalk are used

  if (attains_replace == TRUE) {
    update.crosswalk <- crosswalk

    rm(crosswalk)
  }

  # add provider ref if required

  if (
    wqp_data_links == "add" | wqp_data_links == "replace" | update_mlid == TRUE
  ) {
    provider.ref <- TADA_GetWQPOrganizationRef() |>
      dplyr::select(OrganizationIdentifier, ProviderName) |>
      dplyr::distinct() |>
      dplyr::mutate(OrgIDForURL = OrganizationIdentifier)
  }

  # internal function to update monitoring location identifiers
  updateMonLocIds <- function(.data) {
    # add additional rows to account for the addition of "_WQX" to many org
    # names for WQP data
    add.orgs <- provider.ref |>
      dplyr::filter(
        ProviderName == "STORET",
        grepl("_WQX", OrganizationIdentifier)
      ) |>
      dplyr::mutate(
        OrganizationIdentifier = stringr::str_remove_all(OrgIDForURL, "_WQX")
      )

    # combine provider refs
    provider.ref <- provider.ref |> dplyr::bind_rows(add.orgs)

    # remove intermediate object
    rm(add.orgs)

    # join provider ref df to crosswalk
    update.crosswalk <- .data |>
      dplyr::left_join(
        provider.ref,
        by = dplyr::join_by(OrganizationIdentifier)
      )

    # build the updated mls for storet results
    update.crosswalk.storet <- update.crosswalk |>
      dplyr::filter(ProviderName == "STORET") |>
      dplyr::mutate(
        ATTAINS.MonitoringLocationIdentifier = stringr::str_remove(
          ATTAINS.MonitoringLocationIdentifier,
          paste0(OrganizationIdentifier, "-")
        ),
        ATTAINS.MonitoringLocationIdentifier = stringr::str_remove(
          ATTAINS.MonitoringLocationIdentifier,
          OrganizationIdentifier
        ),
        ATTAINS.MonitoringLocationIdentifier = stringr::str_remove(
          ATTAINS.MonitoringLocationIdentifier,
          "_WQX"
        ),
        ATTAINS.MonitoringLocationIdentifier = paste0(
          OrganizationIdentifier,
          "-",
          ATTAINS.MonitoringLocationIdentifier
        )
      )

    # join nwis and storet crosswalks
    update.crosswalk <- update.crosswalk |>
      dplyr::filter(!ProviderName %in% c("STORET")) |>
      dplyr::bind_rows(update.crosswalk.storet)

    rm(update.crosswalk.storet, provider.ref)

    return(update.crosswalk)
  }

  # internal function to create new urls for monitoring locations
  createNewMLUrls <- function(.data, ref) {
    if (!"ProviderName" %in% names(.data)) {
      .data <- .data |>
        dplyr::left_join(ref, by = dplyr::join_by(OrganizationIdentifier))
    }

    new.urls <- .data |>
      # dplyr::filter(ProviderName == "STORET") |>
      dplyr::mutate(
        ATTAINS.MonitoringDataLinkText.New = as.character(ifelse(
          is.na(OrgIDForURL),
          NA,
          URLencode(paste0(
            "https://www.waterqualitydata.us/provider/",
            ProviderName,
            "/",
            OrgIDForURL,
            "/",
            ATTAINS.MonitoringLocationIdentifier,
            "/"
          ))
        ))
      ) |>
      dplyr::select(-OrgIDForURL)

    return(new.urls)
  }

  if (check_links == TRUE) {
    # internal function to check urls
    checkUrlResp <- function(.data, url.col) {
      # create df of urls to check
      urls.to.check <- .data |> dplyr::filter(!is.na(!!rlang::sym(url.col)))

      # check to see if any urls to check
      if (dim(urls.to.check)[1] == 0) {
        .data <- .data |> dplyr::mutate(response.code = "none")

        rm(urls.to.check)

        return(.data)
      }

      if (dim(urls.to.check)[1] > 0) {
        # retrieve http response headers from url list
        headers <- urls.to.check |>
          dplyr::select(!!rlang::sym(url.col)) |>
          dplyr::pull() |>
          purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))

        # extract response code from first line of header response
        response.code <- sapply(headers, "[[", 1)

        # create dataframe of urls and response codes
        response.df <- data.frame(urls.to.check, response.code) |>
          dplyr::distinct()

        # join response codes to add.urls df
        .data <- .data |>
          dplyr::left_join(response.df, by = names(update.crosswalk))

        rm(urls.to.check, headers, response.code, response.df)

        return(.data)
      }
    }
  }

  if (update_mlid == TRUE & wqp_data_links == "none") {
    update.crosswalk <- updateMonLocIds(update.crosswalk)

    if (check_links == TRUE) {
      update.crosswalk <- checkUrlResp(
        update.crosswalk,
        url.col = "ATTAINS.MonitoringDataLinkText"
      )

      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText = ifelse(
            stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )
        )
    }
  }

  if (update_mlid == TRUE & wqp_data_links == "replace") {
    update.crosswalk <- updateMonLocIds(update.crosswalk)

    update.crosswalk <- createNewMLUrls(update.crosswalk, ref = provider.ref) |>
      dplyr::select(-ATTAINS.MonitoringDataLinkText) |>
      dplyr::rename(
        ATTAINS.MonitoringDataLinkText = ATTAINS.MonitoringDataLinkText.New
      )

    if (check_links == TRUE) {
      update.crosswalk <- checkUrlResp(
        update.crosswalk,
        url.col = "ATTAINS.MonitoringDataLinkText"
      )

      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText = ifelse(
            stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )
        )
    }
  }

  if (update_mlid == TRUE & wqp_data_links == "add") {
    update.crosswalk <- updateMonLocIds(update.crosswalk)

    update.crosswalk <- createNewMLUrls(update.crosswalk, ref = provider.ref)

    if (check_links == TRUE) {
      update.crosswalk <- checkUrlResp(
        update.crosswalk,
        url.col = "ATTAINS.MonitoringDataLinkText"
      )

      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText = ifelse(
            stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )
        ) |>
        dplyr::select(-response.code)

      update.crosswalk <- checkUrlResp(
        update.crosswalk,
        url.col = "ATTAINS.MonitoringDataLinkText.New"
      )

      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText.New = ifelse(
            stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )
        ) |>
        dplyr::select(-response.code)
    }

    update.crosswalk <- update.crosswalk |>
      dplyr::mutate(
        ATTAINS.MonitoringDataLinkText = dplyr::case_when(
          !is.na(ATTAINS.MonitoringDataLinkText) &
            !is.na(ATTAINS.MonitoringDataLinkText.New) ~ paste0(
            ATTAINS.MonitoringDataLinkText,
            "; ",
            ATTAINS.MonitoringDataLinkText.New
          ),
          is.na(ATTAINS.MonitoringDataLinkText) &
            !is.na(
              ATTAINS.MonitoringDataLinkText.New
            ) ~ ATTAINS.MonitoringDataLinkText.New,
          !is.na(ATTAINS.MonitoringDataLinkText) &
            is.na(
              ATTAINS.MonitoringDataLinkText.New
            ) ~ ATTAINS.MonitoringDataLinkText,
          is.na(ATTAINS.MonitoringDataLinkText) &
            is.na(ATTAINS.MonitoringDataLinkText.New) ~ NA
        )
      ) |>
      dplyr::select(-ATTAINS.MonitoringDataLinkText.New)
  }

  if (update_mlid == FALSE & wqp_data_links == "none") {
    update.crosswalk <- update.crosswalk
  }

  if (update_mlid == FALSE & wqp_data_links == "replace") {
    update.crosswalk <- update.crosswalk |>
      dplyr::mutate(
        OLD_ATTAINS.MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier
      ) |>
      updateMonLocIds()

    update.crosswalk <- createNewMLUrls(update.crosswalk, ref = provider.ref) |>
      dplyr::select(-ATTAINS.MonitoringDataLinkText) |>
      dplyr::rename(
        ATTAINS.MonitoringDataLinkText = ATTAINS.MonitoringDataLinkText.New
      ) |>
      dplyr::select(-ATTAINS.MonitoringLocationIdentifier, -OrgIDForURL) |>
      dplyr::rename(
        ATTAINS.MonitoringLocationIdentifier = OLD_ATTAINS.MonitoringLocationIdentifier
      )
  }

  if (update_mlid == FALSE & wqp_data_links == "add") {
    update.crosswalk <- update.crosswalk |>
      dplyr::mutate(
        OLD_ATTAINS.MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier
      ) |>
      updateMonLocIds()

    update.crosswalk <- createNewMLUrls(update.crosswalk, ref = provider.ref)

    if (check_links == TRUE) {
      update.crosswalk <- checkUrlResp(
        update.crosswalk,
        url.col = "ATTAINS.MonitoringDataLinkText"
      )

      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText = ifelse(
            stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )
        ) |>
        dplyr::select(-response.code)

      update.crosswalk <- checkUrlResp(
        update.crosswalk,
        url.col = "ATTAINS.MonitoringDataLinkText.New"
      )

      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText.New = ifelse(
            stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )
        ) |>
        dplyr::select(-response.code)

      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText = dplyr::case_when(
            !is.na(ATTAINS.MonitoringDataLinkText) &
              !is.na(ATTAINS.MonitoringDataLinkText.New) ~ paste0(
              ATTAINS.MonitoringDataLinkText,
              "; ",
              ATTAINS.MonitoringDataLinkText.New
            ),
            is.na(ATTAINS.MonitoringDataLinkText) &
              !is.na(
                ATTAINS.MonitoringDataLinkText.New
              ) ~ ATTAINS.MonitoringDataLinkText.New,
            !is.na(ATTAINS.MonitoringDataLinkText) &
              is.na(
                ATTAINS.MonitoringDataLinkText.New
              ) ~ ATTAINS.MonitoringDataLinkText,
            is.na(ATTAINS.MonitoringDataLinkText) &
              is.na(ATTAINS.MonitoringDataLinkText.New) ~ NA
          )
        ) |>
        dplyr::select(
          -ATTAINS.MonitoringDataLinkText.New,
          -ATTAINS.MonitoringLocationIdentifier
        ) |>
        dplyr::rename(
          ATTAINS.MonitoringLocationIdentifier = OLD_ATTAINS.MonitoringLocationIdentifier
        )
    }

    if (check_links == FALSE) {
      update.crosswalk <- update.crosswalk |>
        dplyr::mutate(
          ATTAINS.MonitoringDataLinkText = dplyr::case_when(
            !is.na(ATTAINS.MonitoringDataLinkText) &
              !is.na(ATTAINS.MonitoringDataLinkText.New) ~ paste0(
              ATTAINS.MonitoringDataLinkText,
              "; ",
              ATTAINS.MonitoringDataLinkText.New
            ),
            is.na(ATTAINS.MonitoringDataLinkText) &
              !is.na(
                ATTAINS.MonitoringDataLinkText.New
              ) ~ ATTAINS.MonitoringDataLinkText.New,
            !is.na(ATTAINS.MonitoringDataLinkText) &
              is.na(
                ATTAINS.MonitoringDataLinkText.New
              ) ~ ATTAINS.MonitoringDataLinkText,
            is.na(ATTAINS.MonitoringDataLinkText) &
              is.na(ATTAINS.MonitoringDataLinkText.New) ~ NA
          )
        ) |>
        dplyr::select(
          -ATTAINS.MonitoringDataLinkText.New,
          -ATTAINS.MonitoringLocationIdentifier
        ) |>
        dplyr::rename(
          ATTAINS.MonitoringLocationIdentifier = OLD_ATTAINS.MonitoringLocationIdentifier
        )
    }
  }

  # select relevant column names and ordering for output in TADA workflow format.
  update.crosswalk <- update.crosswalk |>
    dplyr::select(
      OrganizationIdentifier,
      ATTAINS.OrganizationIdentifier,
      ATTAINS.MonitoringLocationIdentifier,
      ATTAINS.AssessmentUnitIdentifier,
      ATTAINS.MonitoringDataLinkText,
      ATTAINS.WaterType
    )

  # If batch upload is desired, format the output in the required format.
  if (batch_upload == TRUE) {
    update.crosswalk <- update.crosswalk |>
      dplyr::select(-c(ATTAINS.WaterType, ATTAINS.OrganizationIdentifier)) |>
      dplyr::rename(
        ASSESSMENT_UNIT_ID = ATTAINS.AssessmentUnitIdentifier,
        MS_ORG_ID = ATTAINS.MonitoringLocationIdentifier,
        MS_LOCATION_ID = OrganizationIdentifier,
        MS_DATA_LINK = ATTAINS.MonitoringDataLinkText
      )
  }

  return(update.crosswalk)
}


#' Create or Update ATTAINS, TADA/WQP/WQX, and EPA Criteria Search Tool (CST)
#' Parameter Name Crosswalk
#'
#' Use this function to help generate a crosswalk between each
#' ATTAINS.ParameterName used by a specific state or tribal nation and each
#' TADA.ComparableDataIdentifier present in the input TADA dataframe. The
#' crosswalk can be filled out by users within R or Excel. By default this
#' function will generate a user friendly Excel spreadsheet that includes a
#' drop down list of all ATTAINS parameters that have been listed as a cause in
#' prior ATTAINS cycle for the organization selected in the function input 'org_id'.
#' It also highlights the cells in which users should input information. The excel
#' spreadsheet will be automatically downloaded to a user's downloads folder path.
#' Users may need to insert additional rows into the crosswalk if:
#' 1) an ATTAINS.ParameterName corresponds with multiple TADA.ComparableDataIdentifiers
#'    Example: An organization uses "ALUMINUM" for all aluminum related parameter causes
#'    but this ATTAINS.ParameterName may crosswalk to "ALUMINUM_TOTAL_NA_UG/L"
#'    for one use and "ALUMINUM_DISSOLVED_NA_UG/L" for another use; or
#' 2) an TADA.ComparableDataIdentifiers corresponds with multiple ATTAINS.ParameterNames.
#'    Example: An organization uses both "pH, HIGH" and "pH, LOW" as ATTAINS.ParameterNames,
#'    but both crosswalk to the same TADA.ComparableDataIdentifier, "PH_NA_NA_STD UNITS".
#'
#' Users who have already created an ATTAINS parameter and TADA/WQP characteristic
#' crosswalk can provide it as an input to this function. The user-supplied
#' crosswalk (dataframe entered into paramRef function input) must contain the
#' two required columns: TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
#' In addition, users who are interested in performing analyses for more than
#' one organization (multiple states and/or tribes) also need to include an additional column name:
#' 'ATTAINS.OrganizationIdentifier'. This ensures that the crosswalk between
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName is specific and
#' accurate for each organization. If a crosswalk has already been created in the
#' past and is entered into this function as a starting point, then any
#' TADA.ComparableDataIdentifiers that were previously matched
#' with ATTAINS parameters will be retained in the crosswalk, and any new
#' TADA.ComparableDataIdentifiers from the new input data frame will be added
#' to the crosswalk. Users can then focus on matching only the new
#' TADA.ComparableDataIdentifiers with applicable ATTAINS parameter names.
#'
#' Future development efforts may allow users to pull in magnitude values
#' for an ATTAINS parameter through the Criteria Search Tool depending on a
#' users quality control and review of these metrics.
#' The EPA TADA team created a draft crosswalk between characteristic
#' names (TADA.ComparableDataIdentifier) and EPA 304A pollutant names
#' (sourced from the Criteria Search Tool:
#' https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa)
#' This crosswalk only includes priority characteristics identified by the TADA
#' Working Group. You are welcome to reach out to the TADA team to ask for
#' additional matches to be included. You may run the following line of code
#' in the console to review this crosswalk:
#' 'TADAPriorityChar <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))'.
#'
#' If no existing ATTAINS parameter name corresponds with a specific
#' TADA.ComparableDataIdentifier, users may contact the ATTAINS helpdesk
#' \email{attains@epa.gov} to inquire about adding the parameter. Users are
#' free to use any ATTAINS parameter name found in the ATTAINS parameter domain
#' value list, even if the parameter name  has not previously
#' been listed as a cause by the specific organization in the
#' past. The full list of ATTAINS parameter names can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' In the meantime, users can proceed by overriding the data validation in Excel
#' by value pasting. In that case, users will be warned in the
#' ATTAINS.FlagParameterName column that they choose to include an
#' ATTAINS.ParameterName that was not used by the selected organization in prior
#' ATTAINS assessment cycles.
#'
#' @param .data A TADA dataframe after all desired data cleaning,
#' processing, harmonization, filtering, and censored data handling functions
#' have been applied.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. "USEPA" may be included as an org_id which will populate the EPA 304(a)
#' recommended criteria for any TADA.CharacteristicName if one is found. "All" or
#' "NULL" are also allowable values and may be helpful for new ATTAINS users or
#' those performing assessments for multiple states and tribes. If "All" is
#' selected, this will return all prior ATTAINS information from all ATTAINS
#' organizations in prior ATTAINS assessment cycles as individual rows for each
#' organization. If "NULL" is selected all unique prior ATTAINS information from
#' any ATTAINS organizations are returned but are not labeled and can be manually
#' edited. Enter `rExpertQuery::EQ_DomainValues("org_id")` into the console to
#' get a list of valid organization identifiers. A list of organization identifiers
#' can also be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "code" column of the "OrgName" tab.
#'
#' @param paramRef A data frame which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier(s) and ATTAINS.ParameterName(s).
#' This data frame must contain at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
#' Users who are interested in performing analyses for more than
#' one organization (multiple states and/or tribes) also need to include an
#' additional column name: 'ATTAINS.OrganizationIdentifier'.
#'
#' @param auto_assign Character string with value of "None", "All", or "Org".
#' Default is "All". If a user selects "All" this provides a match between
#' ATTAINS.ParameterName(s) and TADA.CharacteristicName(s)/TADA.ComparableDataIdentifier(s)
#' using a TADA reviewed characteristic alias table. If "Org" is selected then
#' this only returns the ATTAINS.ParameterName(s) and TADA.CharacteristicName(s)/
#' TADA.ComparableDataIdentifier(s) match if the specified ATTAINS organization
#' has included that ATTAINS parameter name in past assessment cycles. If "None"
#' is selected, users will be required to fill the crosswalk on their own completely
#' or provide their own paramRef crosswalk which contains the crosswalk of
#' ATTAINS.ParameterName(s) to TADA.CharacteristicName(s)/TADA.ComparableDataIdentifier(s).
#'
#' @param AUMLRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. This data frame must contain the following
#' column names which can be generated from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value. If overwrite = TRUE, the excel file will be
#' replaced (overwritten) by the new file you create if you re-run this function.
#' Users should only specify overwrite = TRUE once they are ready to re-run this
#' function if they have already ran it once.
#'
#' @return An excel file or data frame which contains the columns:
#' TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
#' ATTAINS.ParameterName, and ATTAINS.FlagParameterName. Users will need to
#' complete the crosswalk between ATTAINS.ParameterName and
#' TADA.ComparableDataIdentifier.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This creates a blank paramRef template of UT Nutrients data.
#' # Users will need to fill this template out.
#' # Uncomment example below to generate Excel file
#' # (we recommended working on this in Excel):
#' # TADA_ParametersForAnalysis(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = TRUE)
#' # Example below generates the same output as a dataframe
#' paramRef_UT <- TADA_ParametersForAnalysis(
#'   Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", auto_assign = "None", excel = FALSE
#' )
#' # Users can choose to edit the paramRef_UT through the R environment or in
#' # the excel spreadsheet. Users should be aware that any updates done only
#' # in the R environment will not reflect the 'ATTAINS.FlagParameterName' values
#' # correctly. If completed in R, we recommend users rerun this function
#' # to update the 'ATTAINS.FlagParameterName'.
#' # See below for a simple example of this workflow:
#'
#' # Manually add ATTAINS parameters to crosswalk using R
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT,
#'   ATTAINS.ParameterName = dplyr::case_when(
#'     grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'     grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'     grepl("NITROGEN", TADA.ComparableDataIdentifier) ~
#'       "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#'   )
#' )
#' # Update the 'ATTAINS.FlagParameterName' values
#' paramRef_UT3 <- TADA_ParametersForAnalysis(Data_Nutrients_UT,
#'   paramRef = paramRef_UT2,
#'   org_id = "UTAHDWQ", auto_assign = "None", excel = FALSE
#' )
#'
#' # How does auto_assign = "All" compare to paramRef_UT3?
#' paramRef_UT4 <- TADA_ParametersForAnalysis(Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", auto_assign = "All", excel = FALSE
#' )
#'
#' # Example where multiple org_id's are selected
#' # Retrieve data
#' shepherdstown <- TADA_DataRetrieval(
#'   startDate = "2022-01-01",
#'   endDate = "2025-12-31",
#'   huc = "02070004",
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#' # First, run key flag functions and harmonize synonyms across
#' # characteristic, fraction, and speciation columns
#' shepherdstown2 <- TADA_RunKeyFlagFunctions(shepherdstown, clean = TRUE)
#' shepherdstown3 <- TADA_HarmonizeSynonyms(shepherdstown2)
#' # Create ATTAINS parameter crosswalk for MD, VA, and PA
#' paramRef_shepherdstown <- TADA_ParametersForAnalysis(shepherdstown3,
#'   org_id = c("MDE_EASP", "21VASWCB", "21PA"),
#'   auto_assign = "All",
#'   excel = FALSE
#' )
#' }
#'
TADA_ParametersForAnalysis <- function(
  .data,
  org_id = NULL,
  paramRef = NULL, # If provided, crosswalk is based on user supplied crosswalk.
  auto_assign = c("None", "All", "Org"), # Only auto_assigns if a TADA.ComparableDataIdentifier is left blank.
  AUMLRef = NULL, # If org_id = "ALL", filters by this arg input.
  excel = FALSE,
  overwrite = FALSE
) {
  # argument input selection for auto_assign
  auto_assign <- match.arg(auto_assign)

  # Return an empty dataframe with column names only if a user does not define any arg inputs.
  if (
    missing(.data) && missing(org_id) && missing(paramRef) && missing(AUMLRef)
  ) {
    message(
      "All arguments are blank, returning an empty dataframe with column names only."
    )

    .data <- data.frame(
      TADA.CharacteristicName = NA_character_,
      TADA.ComparableDataIdentifier = NA_character_
    )

    CreateParamRef <- data.frame(
      TADA.ComparableDataIdentifier = character(0),
      ATTAINS.OrganizationIdentifier = character(0),
      ATTAINS.ParameterName = character(0),
      ATTAINS.FlagParameterName = character(0),
      Flag.ParameterInput = character(0)
    )
  } else {
    # overwrite argument should only be used when creating an excel file.
    if (excel == FALSE && overwrite == TRUE) {
      stop(paste0(
        "TADA_ParametersForAnalysis: ",
        "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
        "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
      ))
    }

    # Ensures you have used a valid auto_assign name
    if (!auto_assign %in% c("None", "All", "Org")) {
      stop(paste0(
        "TADA_ParametersForAnalysis: ",
        "argument input ",
        auto_assign,
        " is not a valid entry. Please type one of 'None', 'All', 'Org' as a value."
      ))
    }

    # if null, creates a list of all unique TADA.ComparableDataIdentifier, but no org populated.
    if (!is.character(org_id) & is.null(org_id)) {
      org_id <- ""
    }

    # if org_id = all, create a crosswalk for all ATTAINS org in the data frame.
    if ("all" %in% tolower(org_id)) {
      # If a user selects org_id = all but doesn't provide an AUMLRef with ATTAINS organization identifier.
      if (is.null(AUMLRef)) {
        print(paste0(
          "TADA_ParametersForAnalysis: org_id == 'All' was selected. ",
          "No AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier domain value."
        ))
        org_id <- c(
          utils::read.csv(system.file(
            "extdata",
            "ATTAINSOrgIDsRef.csv",
            package = "EPATADA"
          ))[, "code"],
          ""
        )
      }
      # If a user selects org_id = all and does provide an AUMLRef with ATTAINS organization identifier.
      if (!is.null(AUMLRef)) {
        print(paste0(
          "TADA_ParametersForAnalysis: org_id == 'All' was selected. ",
          "An AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier in your AUMLRef."
        ))
        org_id <- unique(stats::na.omit(AUMLRef$ATTAINS.OrganizationIdentifier))
      }
    }

    # If more than 1 org, it will create n duplicate rows for each TADA.ComparableDataIdentifier.
    if (length(org_id) > 1) {
      print(paste0(
        "TADA_ParametersForAnalysis: More than one org_name was defined in your dataframe. ",
        "Generating duplicate rows of TADA.ComparableDataIdentifier for each org."
      ))
    }

    # check to see if user-supplied parameter ref is a df with appropriate columns
    if (!is.null(paramRef) & !is.character(paramRef)) {
      if (!is.data.frame(paramRef)) {
        stop(paste0(
          "TADA_ParametersForAnalysis: 'paramRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
        ))
      }

      if (is.data.frame(paramRef)) {
        col.names <- c("TADA.ComparableDataIdentifier", "ATTAINS.ParameterName")

        ref.names <- names(paramRef)

        # Users are required to provide a parameter ref that contains
        # TADA.ComparableDataIdentifier and ATTAINS.ParameterName
        if (
          length(setdiff(col.names, ref.names)) > 0 &&
            !("TADA.ComparableDataIdentifier" %in% names(paramRef))
        ) {
          stop(paste0(
            "TADA_ParametersForAnalysis: 'paramRef' must be a data frame with these 2 columns:",
            "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
          ))
        }
      }
    }

    # If users don't provide TADA.ComparableDataIdentifier in their paramRef input,
    # crosswalk using TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText
    if (
      !is.null(paramRef) &
        !("TADA.ComparableDataIdentifier" %in% names(paramRef))
    ) {
      paramRef <- paramRef |>
        dplyr::left_join(
          .data,
          c(
            "TADA.CharacteristicName",
            "TADA.MethodSpeciationName",
            "TADA.ResultSampleFractionText"
          )
        ) |>
        dplyr::select(
          "TADA.CharacteristicName",
          "TADA.ComparableDataIdentifier",
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName",
          "ATTAINS.FlagParameterName"
        )
    }

    # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
    TADA_param <- dplyr::distinct(.data[,
      c("TADA.ComparableDataIdentifier"),
      drop = FALSE
    ]) |>
      dplyr::distinct() |>
      dplyr::mutate(ATTAINS.OrganizationIdentifier = NA_character_) |>
      tidyr::complete(
        TADA.ComparableDataIdentifier,
        ATTAINS.OrganizationIdentifier = org_id
      ) |>
      dplyr::filter(!is.na(ATTAINS.OrganizationIdentifier)) |>
      dplyr::left_join(
        .data[, c("TADA.ComparableDataIdentifier", "TADA.CharacteristicName")],
        by = dplyr::join_by(TADA.ComparableDataIdentifier),
        relationship = "many-to-many"
      ) |>
      dplyr::distinct()

    # Pulls in all domain values of parameter and use names in ATTAINS.
    ATTAINS_param_all <- utils::read.csv(system.file(
      "extdata",
      "ATTAINSParamUseEntityRef.csv",
      package = "EPATADA"
    ))

    ATTAINS_param <- ATTAINS_param_all |>
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) |>
      dplyr::arrange(ATTAINS.ParameterName)

    if ("" %in% org_id) {
      ATTAINS_param <- ATTAINS_param_all |>
        dplyr::mutate(ATTAINS.OrganizationIdentifier = "")
    }

    # Checks if org_id(s) are found in ATTAINS
    if (
      sum(
        !org_id[org_id != "EPA304a"] %in%
          c(
            utils::read.csv(system.file(
              "extdata",
              "ATTAINSOrgIDsRef.csv",
              package = "EPATADA"
            ))[, "code"],
            ""
          )
      ) >
        0
    ) {
      warning(paste0(
        "TADA_ParametersForAnalysis: ",
        "One or more organization identifiers entered by user is not found in ATTAINS."
      ))
    }

    # If no paramRef is provided, the ATTAINS.ParameterName returns a blank column of NA that will need user input.
    if (tolower(auto_assign) == tolower("None")) {
      CreateParamRef <- TADA_param |>
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) |>
        dplyr::arrange(ATTAINS.OrganizationIdentifier) |>
        dplyr::mutate(
          ATTAINS.ParameterName = as.character(NA),
          ATTAINS.FlagParameterName = "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment"
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = "Default. No crosswalk was provided."
        ) |>
        dplyr::distinct()
    }

    if (tolower(auto_assign) == tolower("All")) {
      print(paste0(
        "TADA_ParametersForAnalysis: auto_assign == 'All' was selected, \n",
        "finding an ATTAINS.ParameterName alias match for each TADA.ComparableDataIdentifier - by WQP CharacteristicName, if one is found."
      ))
      TADACharAliasRef <- utils::read.csv(system.file(
        "extdata",
        "TADACharAliasRef.csv",
        package = "EPATADA"
      ))

      TADACharAliasRef <- TADACharAliasRef |>
        dplyr::filter(
          ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName
        )

      CreateParamRef <- TADA_param |>
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) |>
        dplyr::select(
          TADA.CharacteristicName,
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) |>
        dplyr::left_join(
          TADACharAliasRef,
          by = c("TADA.CharacteristicName" = "CharacteristicName"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) |>
        dplyr::arrange(ATTAINS.OrganizationIdentifier) |>
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "Not Applicable for Analysis." |
              is.na(
                ATTAINS.ParameterName
              ) ~ "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in%
              ATTAINS_param_all$ATTAINS.ParameterName ~ "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
            ATTAINS.ParameterName %in%
              ATTAINS_param_all$ATTAINS.ParameterName &
              !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
                paste(
                  ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                  ATTAINS_param_all$ATTAINS.ParameterName
                ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(
                ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                ATTAINS_param_all$ATTAINS.ParameterName
              ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization."
          )
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = dplyr::if_else(
            !is.na(ATTAINS.ParameterName),
            "This crosswalk was provided through an exact match auto_assign = 'All', between ATTAINS.ParameterName and TADA.CharacteristicName.",
            "No crosswalk was provided and no exact matches were found."
          )
        ) |>
        dplyr::distinct()
    }

    if (tolower(auto_assign) == tolower("Org")) {
      print(paste0(
        "TADA_ParametersForAnalysis: auto_assign == 'Org' was selected, ",
        "finding an exact ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
      ))

      TADACharAliasRef <- utils::read.csv(system.file(
        "extdata",
        "TADACharAliasRef.csv",
        package = "EPATADA"
      ))

      TADACharAliasRef <- TADACharAliasRef |>
        dplyr::filter(
          ATTAINS.ParameterName %in% ATTAINS_param$ATTAINS.ParameterName
        )

      CreateParamRef <- TADA_param |>
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) |>
        dplyr::select(
          TADA.CharacteristicName,
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) |>
        dplyr::left_join(
          TADACharAliasRef,
          by = c("TADA.CharacteristicName" = "CharacteristicName"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) |>
        dplyr::arrange(ATTAINS.OrganizationIdentifier) |>
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "Not Applicable for Analysis." |
              is.na(
                ATTAINS.ParameterName
              ) ~ "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in%
              ATTAINS_param_all$ATTAINS.ParameterName ~ "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
            ATTAINS.ParameterName %in%
              ATTAINS_param_all$ATTAINS.ParameterName &
              !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
                paste(
                  ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                  ATTAINS_param_all$ATTAINS.ParameterName
                ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(
                ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                ATTAINS_param_all$ATTAINS.ParameterName
              ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization."
          )
        ) |>
        # since auto_assign = Org matches only, then we must flag the parameter name, then only keep if it is a match
        dplyr::mutate(
          ATTAINS.ParameterName = dplyr::if_else(
            ATTAINS.FlagParameterName ==
              "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization." |
              ATTAINS.OrganizationIdentifier == "",
            ATTAINS.ParameterName,
            NA
          )
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = dplyr::if_else(
            !is.na(ATTAINS.ParameterName),
            "This crosswalk was provided through an exact match auto_assign = 'Org', between ATTAINS.ParameterName and TADA.CharacteristicName.",
            "No crosswalk was provided and no exact matches were found for this organization."
          )
        ) |>
        #dplyr::filter(!is.na(ATTAINS.ParameterName)) |>
        dplyr::distinct()
    }

    # User provides their own user supplied parameter crosswalk
    if (!is.null(paramRef)) {
      paramRef <- paramRef |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = "This crosswalk was provided through a user supplied table"
        ) |>
        dplyr::filter(!is.na(ATTAINS.ParameterName))

      CreateParamRef <- CreateParamRef |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName,
          Flag.ParameterInput
        ) |>
        dplyr::filter(
          !TADA.ComparableDataIdentifier %in%
            paramRef$TADA.ComparableDataIdentifier
        ) |>
        dplyr::bind_rows(paramRef[, c(
          "ATTAINS.OrganizationIdentifier",
          "TADA.ComparableDataIdentifier",
          "ATTAINS.ParameterName",
          "Flag.ParameterInput"
        )]) |>
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "Not Applicable for Analysis." |
              is.na(
                ATTAINS.ParameterName
              ) ~ "No ATTAINS.ParameterName crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in%
              ATTAINS_param_all$ATTAINS.ParameterName ~ "Parameter name is not included in ATTAINS, contact ATTAINS to add ATTAINS.ParameterName name to Domain List.",
            ATTAINS.ParameterName %in%
              ATTAINS_param_all$ATTAINS.ParameterName &
              !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
                paste(
                  ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                  ATTAINS_param_all$ATTAINS.ParameterName
                ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(
                ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                ATTAINS_param_all$ATTAINS.ParameterName
              ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization"
          )
        ) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.FlagParameterName,
          Flag.ParameterInput
        )
    }
    rm(TADA_param)
  }
  if (excel == TRUE) {
    # Excel ref files to be stored in the Downloads folder location.
    # Define the OneDrive Downloads path
    onedrive_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "OneDrive",
      "Downloads",
      "myfileRef.xlsx"
    )

    # Define the default Downloads path
    default_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "Downloads",
      "myfileRef.xlsx"
    )

    # Check if the OneDrive Downloads path exists, and prioritize it
    if (file.exists(onedrive_downloads_path)) {
      downloads_path <- onedrive_downloads_path
    } else {
      downloads_path <- default_downloads_path
    }

    # Print message if there are many combinations of TADA Characteristic as it may slow run time.
    n <- nrow(CreateParamRef)
    if (n > 100 & excel == TRUE) {
      message(paste(
        "There are",
        n,
        "unique TADA.ComparableDataIdentifier names in your TADA data frame.",
        "This may result in slow runtime for TADA_ParametersForAnalysis() when generating the excel spreadsheet.",
        "Excel formulas will only be generated for the first 100 rows. Please fill cells down on Cells D1 and Cells E1",
        "in the excel spreadsheet to make all flagging columns update dynamically (automatically updates the flag if a change was made to a crosswalk)."
      ))
    }

    # Create column names for an empty dataframe
    columns <- c(
      "TADA.ComparableDataIdentifier",
      "ATTAINS.ParameterName",
      "ATTAINS.OrganizationIdentifier",
      "ATTAINS.FlagParameterName"
    )

    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
    colnames(par) <- columns

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "ATTAINSOrgNamesParamRef", visible = FALSE)
    openxlsx::addWorksheet(wb, "CreateParamRef", visible = TRUE)
    openxlsx::addWorksheet(wb, "Index", visible = FALSE)

    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }

    # Format header and bodystyle
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    bodyStyle <- openxlsx::createStyle(wrapText = TRUE)

    # Format Column widths
    openxlsx::setColWidths(
      wb,
      "CreateParamRef",
      cols = 1:ncol(CreateParamRef),
      widths = "auto"
    )

    # New row to rbind if a user selects "Not Applicable for Analysis."
    no_match_df <- data.frame(
      ATTAINS.OrganizationIdentifier = "NA",
      ATTAINS.ParameterName = "Not Applicable for Analysis.",
      ATTAINS.UseName = "Not Applicable for Analysis."
    )

    # Pulls in all domain values of parameter and use names in ATTAINS.
    ATTAINS_param_all <- utils::read.csv(system.file(
      "extdata",
      "ATTAINSParamUseEntityRef.csv",
      package = "EPATADA"
    ))

    # Filters the full domain value by the specified org_id(s)
    ATTAINS_param <- ATTAINS_param_all |>
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) |>
      dplyr::arrange(ATTAINS.ParameterName)

    # Index of allowable values for drop-down lists
    openxlsx::writeData(
      wb,
      "Index",
      startCol = 4,
      x = rbind(
        no_match_df,
        ATTAINS_param_all[, c(
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName",
          "ATTAINS.UseName"
        )] |>
          dplyr::arrange(ATTAINS.ParameterName)
      )
    )

    openxlsx::writeData(
      wb,
      "Index",
      startCol = 2,
      x = CreateParamRef[, c("ATTAINS.ParameterName", "Flag.ParameterInput")]
    )

    openxlsx::writeData(
      wb,
      "Index",
      startCol = 1,
      x = data.frame(
        ATTAINS.ParameterName = c(unique(ATTAINS_param$ATTAINS.ParameterName))
      )
    )

    openxlsx::writeData(
      wb,
      "CreateParamRef",
      startCol = 1,
      x = CreateParamRef,
      headerStyle = header_st
    )

    # Creates a tab that contains the ATTAINS parameter-use filtered by the org_id input.
    openxlsx::writeData(
      wb,
      "ATTAINSOrgNamesParamRef",
      startCol = 1,
      x = ATTAINS_param,
      headerStyle = header_st
    )

    # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab

    # Note: If we make edits to the data validation, please ensure the entire
    # data frame column is being referenced.
    # Ex. data validation will capture values in tab [Index] column h, for rows 2:50000 for input, value = sprintf("'Index'!$H$2:$H$50000")

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "CreateParamRef",
      cols = 3,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index'!$E$2:$E$30000"), # please ensure this covers all values in the column E in the Index tab for future development.
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    # remove intermediate objects
    rm(ATTAINS_param, ATTAINS_param_all)

    max_loops <- 0

    for (i in 1:nrow(CreateParamRef)) {
      max_loops <- max_loops + 1
      if (max_loops > 100) {
        break
      }

      openxlsx::writeFormula(
        wb,
        "CreateParamRef",
        startCol = 4,
        startRow = i + 1,
        array = TRUE,
        x = paste0(
          "=IF(OR(C",
          i + 1,
          '="",C',
          i + 1,
          '="Not Applicable for Analysis."),"No ATTAINS.ParameterName crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment",
          IF(ISNA(MATCH(C',
          i + 1,
          ',Index!E:E,0)),
            "Parameter name is not included in ATTAINS, contact ATTAINS to add ATTAINS.ParameterName name to Domain List.",
          IF(ISNA(MATCH(1,(C',
          i + 1,
          "=ATTAINSOrgNamesParamRef!D:D)*(B",
          i + 1,
          '=ATTAINSOrgNamesParamRef!A:A),0)),
            "This ATTAINS parameter name was included in past ATTAINS assessment cycles, but not for this organization.",
            "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization.")))'
        )
      )

      openxlsx::writeFormula(
        wb,
        "CreateParamRef",
        startCol = 5,
        startRow = i + 1,
        array = TRUE,
        x = paste0(
          "IF(C",
          i + 1,
          "=Index!B$",
          i + 1,
          ",Index!C$",
          i + 1,
          ',"This ATTAINS.ParameterName crosswalk was MODIFIED by your input(s) for this TADA.ComparableDataIdentifier.")'
        )
      )
    }

    openxlsx::conditionalFormatting(
      wb,
      "CreateParamRef",
      cols = 3,
      rows = 1:nrow(CreateParamRef) + 1,
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )

    openxlsx::conditionalFormatting(
      wb,
      "CreateParamRef",
      cols = 3,
      rows = 1:nrow(CreateParamRef) + 1,
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    )

    # If a user has chose to Exclude a use name for a parameter, flag as a red cell.
    openxlsx::conditionalFormatting(
      wb,
      "CreateParamRef",
      cols = 3,
      rows = 1:nrow(CreateParamRef) + 1,
      type = "contains",
      rule = c("Not Applicable for Analysis."),
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )

    # remove intermediate objects
    rm(max_loops)

    # Format column widths in CreateParamRef - for future considerations of formatting
    openxlsx::setColWidths(
      wb,
      "CreateParamRef",
      cols = 1:ncol(CreateParamRef) + 2,
      widths = "auto"
    )

    if (overwrite == TRUE) {
      message(paste0("Overwriting sheet [CreateParamRef] in ", downloads_path))
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }

    if (overwrite == FALSE) {
      message(
        "If you would like to replace sheet [CreateParamRef], use overwrite = TRUE argument in TADA_ParametersForAnalysis."
      )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }

    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  return(CreateParamRef)
}


#' Create or Update ATTAINS Parameter and Use crosswalk
#'
#' This function generates a crosswalk of all parameters and uses applicable to
#' your WQP/TADA data frame and selected organization(s) in ATTAINS.
#' Users should review and validate each ATTAINS.ParameterName and associated
#' ATTAINS.UseName combination.As part of this review process, users should
#' check to make sure each ATTAINS.UseName generated by this
#' function accurately corresponds to the correct TADA.ComparableDataIdentifier
#' and ATTAINS.ParameterName found in the TADA dataframe. This function should be
#' run after creating your parameter (ATTAINS.ParameterName and
#' TADA.ComparableDataIdentifier) crosswalk.
#'
#' Before running this function, users must run TADA_ParametersForAnalysis() to
#' create the crosswalk that defines the ATTAINS.ParameterName(s) needing validation.
#' All unique ATTAINS.UseNames from prior ATTAINS assessment cycles are pulled
#' in using ATTAINS Expert Query in this function. If a user has
#' defined multiple TADA.ComparableDataIdentifier matches to an
#' ATTAINS.ParameterName, they will need to define whether every
#' TADA.ComparableDataIdentifier matches to an associated ATTAINS.UseName.
#' If certain parameter and use combinations only apply to certain
#' TADA.ComparableDataIdentifier(s), users will need to select 'Exclude' or
#' select a blank value for the ATTAINS.UseName to properly capture this logic.
#'
#' If an ATTAINS use name is not listed as a prior domain value for your organization
#' from prior ATTAINS assessment cycles, users can contact the ATTAINS helpdesk
#' \email{attains@epa.gov} to inquire about adding the use to the ATTAINS
#' domain list. However, even when these new uses are submitted to ATTAINS, they
#' cannot be retrieved from ATTAINS assessment profiles until the current/new
#' assessment cycle is approved.
#'
#' Thus, if a user has a list of new use names that cannot be pulled from ATTAINS,
#' they should consider using the AU_UsesRef argument input or the usesRef
#' argument input which would specify that the use names should come from a
#' user supplied list rather than from prior ATTAINS assessment cycles.
#' If a list of use names come from the AU_UsesRef, this function will apply any
#' new use names to an ATTAINS parameter name, found in your paramRef
#' argument input, by joining the ATTAINS.WaterType of the AUs defined in
#' your AU_UsesRef to the ATTAINS.WaterType found from ATTAINS Expert Query.
#'
#' Otherwise, users can still
#' proceed by overriding the data validation by value pasting in Excel.
#' Users will be warned in the ATTAINS.FlagUseName column if they choose to
#' include an ATTAINS use name that was not listed in prior ATTAINS assessment cycles as:
#' 'Use name has not been assessed in prior cycles by this organization' or
#' 'Use name has been assessed in prior cycles by this organization, but not for this parameter name'.
#'
#' @param .data A TADA dataframe after all desired data cleaning,
#' processing, harmonization, filtering, and censored data handling functions
#' have been applied.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. "USEPA" may be included as an org_id which will populate the EPA 304(a)
#' recommended criteria for any TADA.CharacteristicName if one is found. "All" or
#' "NULL" are also allowable values and may be helpful for new ATTAINS users or
#' those performing assessments for multiple states and tribes. If "All" is
#' selected, this will return all prior ATTAINS information from all ATTAINS
#' organizations in prior ATTAINS assessment cycles as individual rows for each
#' organization. If "NULL" is selected all unique prior ATTAINS information from
#' any ATTAINS organizations are returned but are not labeled and can be manually
#' edited. Enter `rExpertQuery::EQ_DomainValues("org_id")` into the console to
#' get a list of valid organization identifiers. A list of organization identifiers
#' can also be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "code" column of the "OrgName" tab.
#'
#' @param paramRef A data frame which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier(s) and ATTAINS.ParameterName(s).
#' This data frame must contain at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
#' Users who are interested in performing analyses for more than
#' one organization (multiple states and/or tribes) also need to include an
#' additional column name: 'ATTAINS.OrganizationIdentifier'.
#'
#' @param usesRef A data frame which contains a completed crosswalk of
#' ATTAINS.ParameterName(s) that will be analyzed for each ATTAINS.UseName.
#' Users will need to ensure this crosswalk contains the appropriate column
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and review this output for accuracy.
#'
#' @param AU_UsesRef An optional data frame input. If provided, the ATTAINS.UseName
#' will be populated from the ATTAINS.UseName found in this data frame rather
#' than the ATTAINS assessment profile. This data frame must contain the following
#' column names which can be generated from the output of TADA_AssignUsesToAU:
#' ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName,
#' and ATTAINS.WaterType.
#'
#' @param AUMLRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. This data frame must contain the following
#' column names which can be generated from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
#'
#' @param auto_assign A boolean value. If TRUE, this will assign all unique
#' ATTAINS.UseName to an ATTAINS.ParameterName if that parameter has not been
#' included in prior ATTAINS assessment cycles for that ATTAINS.OrganizationIdentifier.
#' If FALSE, the value for ATTAINS.UseName will be left blank for that ATTAINS.ParameterName
#' and you will need to manually assign the use names as needed.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value. If overwrite = TRUE, the excel file will be
#' replaced (overwritten) by the new file you create if you re-run this function.
#' Users should only specify overwrite = TRUE once they are ready to re-run this
#' function if they have already ran it once.
#'
#' @return A dataframe which contains the columns: TADA.ComparableDataIdentifier,
#' ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName,
#' and ATTAINS.FlagUseName. Users will need to review the crosswalk between
#' ATTAINS.ParameterName, ATTAINS.UseName and TADA.ComparableDataIdentifier.
#'
#' @seealso [TADA_ParametersForAnalysis()]
#'
#' @export
#'
#' @examples
#' # First, generate and fill out a parameter crosswalk (see TADA_ParametersForAnalysis()):
#' paramRef_UT <- TADA_ParametersForAnalysis(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_ParametersForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' paramRef_UT4 <- TADA_ParametersForAnalysis(
#'   Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", auto_assign = "All", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_UsesForAnalysis():
#' usesRef_UT <- TADA_UsesForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Now, let's compare the crosswalk for paramRef_UT4 when we use auto_assign = "All".
#' # Notice, there are NA values for ATTAINS.UseName as these UT ATTAINS Parameter Name were
#' # not listed as a cause in prior ATTAINS assessment cycles.
#' usesRef_UT2 <- TADA_UsesForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT4, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Let's test the "auto_assign" input
#' usesRef_UT3 <- TADA_UsesForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT4, auto_assign = TRUE, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
TADA_UsesForAnalysis <- function(
  .data,
  org_id = NULL,
  paramRef = NULL, # Required, filter the use(s) by only those found for unique param(s) found in this ref.
  usesRef = NULL, # If provided, any param(s) to use(s) assignments will be based on this user supplied list.
  AU_UsesRef = NULL, # If provided, any use assignments will be based on this domain list rather than from ATTAINS.
  AUMLRef = NULL, # If provided and if org_id = "ALL" then this will filter org_id(s) from this df.
  auto_assign = FALSE, # DEV NOTE: Should only auto assign any ATTAINS.ParameterName that isn't found in either user supplied usesRef or in ATTAINS.
  excel = FALSE,
  overwrite = FALSE
) {
  # Return an empty dataframe with column names only if a user does not define any arg inputs.
  if (
    missing(.data) &&
      missing(org_id) &&
      missing(paramRef) &&
      missing(usesRef) &&
      missing(AU_UsesRef) &&
      missing(AUMLRef)
  ) {
    message(
      "All arguments are blank, returning an empty dataframe with column names only."
    )

    CreateUsesRef <- data.frame(
      TADA.ComparableDataIdentifier = character(0),
      ATTAINS.OrganizationIdentifier = character(0),
      ATTAINS.ParameterName = character(0),
      ATTAINS.UseName = character(0),
      IncludeOrExclude = character(0),
      ATTAINS.FlagUseName = character(0),
      Flag.UseInput = character(0)
    )
  } else {
    # overwrite argument should only be used when creating an excel file.
    if (excel == FALSE && overwrite == TRUE) {
      stop(paste0(
        "Argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
        "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
      ))
    }

    # Checks if paramRef argument contains a dataframe and necessary columns to proceed.
    if (is.null(paramRef)) {
      stop(paste0("TADA.CreateUsesRef: No paramRef argument provided."))
    }

    # If a user does not fill in ANY values for the crosswalk of ATTAINS.ParameterName.
    # Users may want to proceed with only the EPA 304(a) criteria crosswalk,
    # therefore we will allow users to proceed in this case.
    if (sum(!is.na(paramRef$ATTAINS.ParameterName)) == 0) {
      warning(paste0(
        "No values were found in ATTAINS.ParameterName. ",
        "Please ensure that you have inputted all field values of interest in the ",
        "ATTAINS.ParameterName column generated from TADA_ParametersForAnalysis() function."
      ))
    }

    # If a user leaves at least one values for the crosswalk of ATTAINS.ParameterName blank.
    # Users are recommended to select 'No parameter match for this TADA.ComparableDataIdentifier' if
    # there is no crosswalk, but leaving it blank will be treated similarly.
    if (sum(is.na(paramRef$ATTAINS.ParameterName)) > 1) {
      print(paste0(
        "NAs were found in ATTAINS.ParameterName. ",
        "Please ensure that you have inputted all field values of interest in ",
        "the ATTAINS.ParameterName column generated from TADA_ParametersForAnalysis() function."
      ))
    }

    # check to see if user-supplied parameter ref is a df with appropriate columns and is filled out.
    if (!is.null(paramRef) & !is.character(paramRef)) {
      if (!is.data.frame(paramRef)) {
        stop(paste0(
          "TADA_UsesForAnalysis: 'paramRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName."
        ))
      }

      if (is.data.frame(paramRef)) {
        col.names <- c("TADA.ComparableDataIdentifier", "ATTAINS.ParameterName")

        ref.names <- names(paramRef)

        if (
          length(setdiff(col.names, ref.names)) > 0 &&
            !("TADA.ComparableDataIdentifier" %in% names(paramRef))
        ) {
          stop(paste0(
            "TADA_UsesForAnalysis: 'paramRef' must be a data frame with these 2 columns:",
            "TADA.ComparableDataIdentifier and ATTAINS.ParameterName."
          ))
        }
      }
    }

    # check to see if user-supplied parameter-use ref is a df with appropriate columns and is filled out.
    if (!is.null(usesRef) & !is.character(usesRef)) {
      if (!is.data.frame(usesRef)) {
        stop(paste0(
          "TADA_UsesForAnalysis: 'usesRef' must be a data frame with these 3 columns:",
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName, ATTAINS.UseName"
        ))
      }

      if (is.data.frame(usesRef)) {
        col.names <- c(
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName",
          "ATTAINS.UseName"
        )

        ref.names <- names(usesRef)

        if (
          length(setdiff(col.names, ref.names)) > 0 &&
            !("TADA.ComparableDataIdentifier" %in% names(usesRef))
        ) {
          stop(paste0(
            "TADA_UsesForAnalysis: 'usesRef' must be a data frame with these 3 columns:",
            "ATTAINS.OrganizationIdentifier, TADA.ComparableDataIdentifier, ",
            "ATTAINS.ParameterName, ATTAINS.UseName"
          ))
        }
      }
    }

    .data <- as.data.frame(.data)

    # Pulls in all domain values of parameter and use names by orgs in ATTAINS.
    ATTAINS_param_all <- utils::read.csv(system.file(
      "extdata",
      "ATTAINSParamUseEntityRef.csv",
      package = "EPATADA"
    ))

    # If a user provides a AU_UsesRef, We will use the uses in this table
    if (!is.null(AU_UsesRef)) {
      ATTAINS_param_all <- ATTAINS_param_all |>
        dplyr::select(-ATTAINS.UseName) |>
        dplyr::distinct() |>
        dplyr::left_join(
          AU_UsesRef,
          by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.WaterType")
        )
    }

    # Considers if we want to separate speciation, fraction, units as separate columns in the future for crosswalk.
    if (
      !is.null(paramRef) &
        !("TADA.ComparableDataIdentifier" %in% names(paramRef))
    ) {
      paramRef <- paramRef |>
        dplyr::left_join(
          .data,
          by = c(
            "TADA.CharacteristicName",
            "TADA.MethodSpeciationName",
            "TADA.ResultSampleFractionText"
          )
        ) |>
        dplyr::select(
          "TADA.CharacteristicName",
          "TADA.ComparableDataIdentifier",
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName",
          "ATTAINS.FlagParameterName"
        )
    }

    # if null, creates a list of all unique TADA.ComparableDataIdentifier, but no org populated.
    if (!is.character(org_id) & is.null(org_id)) {
      org_id <- ""
    }

    # if org_id = all, create a crosswalk for all ATTAINS org in the data frame.
    if (tolower("all") %in% tolower(org_id)) {
      # If a user selects org_id = all but doesn't provide an AUMLRef with ATTAINS organization identifier.
      if (is.null(AUMLRef)) {
        print(paste0(
          "org_id == 'All' was selected, ",
          "No AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier domain value."
        ))
        org_id <- rExpertQuery::EQ_DomainValues("org_id")[, "code"]
      }
      # If a user selects org_id = all and does provide an AUMLRef with ATTAINS organization identifier.
      if (!is.null(AUMLRef)) {
        print(paste0(
          "org_id == 'All' was selected, ",
          "An AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier in your AUMLRef."
        ))
        org_id <- unique(stats::na.omit(AUMLRef$ATTAINS.OrganizationIdentifier))
      }
    }

    # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA 304(a)" as that is not an ATTAINS org_id.
    # 5/14/25 KW: We should use separate columns for CST organization/pollutant/use names in the future.
    if (
      sum(
        !org_id[org_id != "EPA304a"] %in%
          c(
            utils::read.csv(system.file(
              "extdata",
              "ATTAINSOrgIDsRef.csv",
              package = "EPATADA"
            ))[, "code"],
            ""
          )
      ) >
        0
    ) {
      warning(paste0(
        "TADA_UsesForAnalysis: ",
        "One or more organization identifiers entered by user is not found in ATTAINS. "
      ))
    }

    # Filters the ATTAINS parameter and use names by the org_id in user supplied df.
    ATTAINS_param <- ATTAINS_param_all |>
      dplyr::select(
        ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName,
        ATTAINS.UseName
      ) |>
      dplyr::filter(
        ATTAINS.ParameterName %in% paramRef$ATTAINS.ParameterName
      ) |>
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id)

    # Create the parameter-use reference table for validation
    CreateUsesRef <- paramRef |>
      dplyr::left_join(
        ATTAINS_param,
        by = c("ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier"),
        relationship = "many-to-many"
      ) |>
      dplyr::select(
        TADA.ComparableDataIdentifier,
        ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName,
        ATTAINS.UseName
      ) |>
      # tidyr::drop_na(ATTAINS.ParameterName) |>
      dplyr::filter(ATTAINS.ParameterName != "Not Applicable for Analysis.") |>
      dplyr::distinct() |>
      dplyr::mutate(
        IncludeOrExclude = dplyr::if_else(
          is.na(ATTAINS.UseName),
          "Exclude",
          "Include"
        )
      ) |>
      dplyr::mutate(
        ATTAINS.FlagUseName = dplyr::if_else(
          is.na(ATTAINS.UseName),
          "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
          "Use name has been assessed in prior cycles by this organization."
        )
      ) |>
      dplyr::mutate(
        Flag.UseInput = "Default: no modification was made to this row."
      )

    if (auto_assign == TRUE) {
      print(paste0(
        "TADA_UsesForAnalysis: auto_assign == TRUE was selected, ",
        "assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an ",
        "organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed."
      ))

      use.names <- CreateUsesRef |>
        dplyr::select(ATTAINS.OrganizationIdentifier, ATTAINS.UseName) |>
        tidyr::drop_na() |>
        dplyr::distinct()

      CreateUsesRef_temp <- CreateUsesRef |>
        dplyr::filter(is.na(ATTAINS.UseName)) |>
        dplyr::left_join(
          use.names,
          by = c("ATTAINS.OrganizationIdentifier"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(
          ATTAINS.UseName = dplyr::coalesce(
            ATTAINS.UseName.x,
            ATTAINS.UseName.y
          )
        ) |>
        dplyr::select(-c(ATTAINS.UseName.x, ATTAINS.UseName.y)) |>
        # dplyr::mutate(TADA.ComparableDataIdentifier = dplyr::coalesce(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) |>
        # dplyr::select(-c(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) |>
        dplyr::mutate(IncludeOrExclude = "Include") |>
        dplyr::mutate(Flag.UseInput = "This row was MODIFIED by your input(s).")

      CreateUsesRef <- CreateUsesRef |>
        # dplyr::select(TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.OrganizationIdentifier) |>
        dplyr::filter(!is.na(ATTAINS.UseName)) |>
        dplyr::full_join(
          CreateUsesRef_temp,
          by = c(
            "ATTAINS.ParameterName",
            "ATTAINS.UseName",
            "ATTAINS.OrganizationIdentifier",
            "IncludeOrExclude",
            "ATTAINS.FlagUseName",
            "Flag.UseInput"
          )
        ) |>
        dplyr::mutate(
          ATTAINS.FlagUseName = dplyr::case_when(
            paste(
              ATTAINS.OrganizationIdentifier,
              ATTAINS.ParameterName,
              ATTAINS.UseName
            ) %in%
              paste(
                ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                ATTAINS_param_all$ATTAINS.ParameterName,
                ATTAINS_param_all$ATTAINS.UseName
              ) ~ "Use name has been assessed in prior cycles by this organization.",
            !paste(
              ATTAINS.OrganizationIdentifier,
              ATTAINS.ParameterName,
              ATTAINS.UseName
            ) %in%
              paste(
                ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                ATTAINS_param_all$ATTAINS.ParameterName,
                ATTAINS_param_all$ATTAINS.UseName
              ) &
              ATTAINS.UseName %in%
                ATTAINS_param_all$ATTAINS.UseName ~ "Use name has been assessed in prior cycles by this organization, but not for this parameter name.",
            is.na(
              ATTAINS.UseName
            ) ~ "No use name is provided. Consider choosing an appropriate ATTAINS.UseName."
          )
        ) |>
        dplyr::mutate(
          TADA.ComparableDataIdentifier = dplyr::coalesce(
            TADA.ComparableDataIdentifier.x,
            TADA.ComparableDataIdentifier.y
          )
        ) |>
        dplyr::select(
          -c(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)
        ) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          IncludeOrExclude,
          ATTAINS.FlagUseName,
          Flag.UseInput
        ) |>
        dplyr::arrange(
          match(IncludeOrExclude, c("Include")),
          ATTAINS.OrganizationIdentifier,
          ATTAINS.UseName
        ) |>
        dplyr::distinct()
    }

    if (!is.null(usesRef)) {
      # user may have only supplied a usesRef table with TADA.CharacteristicName rather than TADA.ComparableDataIdentifier
      # This also validates the TADA.ComparableDataIdentifier crosswalk to ensure it is up to date (drops and re-join)
      if ("TADA.CharacteristicName" %in% names(usesRef)) {
        usesRef <- usesRef |>
          dplyr::select(-TADA.ComparableDataIdentifier) |>
          dplyr::left_join(
            .data |>
              dplyr::select(
                TADA.ComparableDataIdentifier,
                TADA.CharacteristicName
              ),
            by = ("TADA.CharacteristicName")
          )
      }

      # check if users have specified an include or exclude column. If not, assume it is all 'include'
      if ("IncludeOrExclude" %in% names(usesRef)) {
        usesRef <- usesRef |>
          dplyr::select(
            ATTAINS.OrganizationIdentifier,
            ATTAINS.ParameterName,
            ATTAINS.UseName,
            IncludeOrExclude
          ) |>
          dplyr::left_join(
            paramRef,
            by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName")
          )
      } else {
        print(
          "IncludeOrExclude was not found as a column name in your user supplied, assuming all parameter and uses are applicable for your analysis."
        )
        usesRef <- usesRef |>
          dplyr::select(
            ATTAINS.OrganizationIdentifier,
            ATTAINS.ParameterName,
            ATTAINS.UseName
          ) |>
          dplyr::left_join(
            paramRef,
            by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName"),
            relationship = "many-to-many"
          ) |>
          dplyr::mutate(IncludeOrExclude = "Include")
      }

      usesRef$ATTAINS.ParameterName <- toupper(usesRef$ATTAINS.ParameterName)

      # identifies if a user has excluded any useParam rows. This row is showing up as a new entry but has not been defined.
      # This should flag users that they need to review this entry and if they
      # truly want to exclude it or not. What should the default be?
      Flag1 <- CreateUsesRef |>
        dplyr::anti_join(
          usesRef,
          by = c(
            "TADA.ComparableDataIdentifier",
            "ATTAINS.OrganizationIdentifier",
            "ATTAINS.ParameterName"
            # "ATTAINS.UseName", "IncludeOrExclude", "ATTAINS.FlagUseName"
          )
        ) |>
        dplyr::mutate(
          Flag.UseInput = "Suspect: Your usesRef argument did not include this TADA.ComparableDataIdentifier. Please ensure you have provided all ATTAINS.UseName and ATTAINS.ParameterName combinations in your input."
        )
      # dplyr::mutate(IncludeOrExclude = "Exclude")

      # identifies if a user has MODIFIED any useParam rows.
      Flag2 <- usesRef |>
        dplyr::anti_join(
          CreateUsesRef,
          by = c(
            "TADA.ComparableDataIdentifier",
            "ATTAINS.OrganizationIdentifier",
            "ATTAINS.ParameterName",
            "ATTAINS.UseName",
            "IncludeOrExclude" # , "ATTAINS.FlagUseName"
          )
        ) |>
        dplyr::mutate(
          Flag.UseInput = "This row was MODIFIED by your input(s)."
        ) |>
        dplyr::select(
          "TADA.ComparableDataIdentifier",
          "ATTAINS.OrganizationIdentifier",
          "IncludeOrExclude",
          "ATTAINS.ParameterName",
          "ATTAINS.UseName"
        )

      CreateUsesRef <- usesRef |>
        dplyr::select(
          "TADA.ComparableDataIdentifier",
          "ATTAINS.OrganizationIdentifier",
          "IncludeOrExclude",
          "ATTAINS.ParameterName",
          "ATTAINS.UseName"
        ) |>
        dplyr::full_join(
          Flag1 |>
            dplyr::full_join(
              Flag2,
              by = c(
                "TADA.ComparableDataIdentifier",
                "ATTAINS.OrganizationIdentifier",
                "IncludeOrExclude",
                "ATTAINS.ParameterName",
                "ATTAINS.UseName"
              ),
              relationship = "many-to-many"
            ),
          by = c(
            "TADA.ComparableDataIdentifier",
            "ATTAINS.OrganizationIdentifier",
            "IncludeOrExclude",
            "ATTAINS.ParameterName",
            "ATTAINS.UseName"
          ),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(
          ATTAINS.FlagUseName = dplyr::case_when(
            is.na(
              ATTAINS.UseName
            ) ~ "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
            paste(
              ATTAINS.OrganizationIdentifier,
              ATTAINS.ParameterName,
              ATTAINS.UseName
            ) %in%
              paste(
                ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                ATTAINS_param_all$ATTAINS.ParameterName,
                ATTAINS_param_all$ATTAINS.UseName
              ) ~ "Use name has been assessed in prior cycles by this organization.",
            !paste(
              ATTAINS.OrganizationIdentifier,
              ATTAINS.ParameterName,
              ATTAINS.UseName
            ) %in%
              paste(
                ATTAINS_param_all$ATTAINS.OrganizationIdentifier,
                ATTAINS_param_all$ATTAINS.ParameterName,
                ATTAINS_param_all$ATTAINS.UseName
              ) &
              ATTAINS.UseName %in%
                ATTAINS_param_all$ATTAINS.UseName ~ "Use name has been assessed in prior cycles by this organization, but not for this parameter name.",
            TRUE ~ "Use name has not been assessed in prior cycles."
          )
        ) |>
        dplyr::mutate(
          Flag.UseInput = dplyr::case_when(
            is.na(
              Flag.UseInput
            ) ~ "Default: no modification was made to this row.",
            !is.na(Flag.UseInput) ~ Flag.UseInput
          )
        ) |>
        dplyr::filter(
          ATTAINS.OrganizationIdentifier %in% org_id,
          ATTAINS.ParameterName %in% paramRef$ATTAINS.ParameterName,
          !is.na(ATTAINS.ParameterName)
        ) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          IncludeOrExclude,
          ATTAINS.FlagUseName,
          Flag.UseInput
        ) |>
        dplyr::arrange(
          match(IncludeOrExclude, c("Include")),
          ATTAINS.OrganizationIdentifier,
          ATTAINS.UseName
        ) |>
        dplyr::distinct()

      # remove intermediate objects
      rm(Flag1, Flag2)
    }

    # remove intermediate objects
    rm(ATTAINS_param)
  }
  if (excel == TRUE) {
    # Define the OneDrive Downloads path
    onedrive_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "OneDrive",
      "Downloads",
      "myfileRef.xlsx"
    )

    # Define the default Downloads path
    default_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "Downloads",
      "myfileRef.xlsx"
    )

    # Check if the OneDrive Downloads path exists, and prioritize it
    if (file.exists(onedrive_downloads_path)) {
      downloads_path <- onedrive_downloads_path
    } else {
      downloads_path <- default_downloads_path
    }

    # Print message if there are many combinations of TADA Characteristic as it may slow run time.
    n <- nrow(CreateUsesRef)
    if (n > 100) {
      message(paste0(
        "There are ",
        n,
        " rows in your CreateUsesRef.
    This may result in slow runtime for TADA_UsesForAnalysis() when generating the excel spreadsheet.
    Excel formulas will only be generated for the first 100 rows. Please fill down on Cells F1 and Cells G1 in excel
    to make all rows function dynamically (automatically updates the flag if a change was made to a crosswalk)."
      ))
    }

    # Create column names for an empty dataframe
    columns <- c(
      "ATTAINS.OrganizationIdentifier",
      "ATTAINS.ParameterName",
      "ATTAINS.UseName",
      "ATTAINS.FlagParameterName",
      "ATTAINS.FlagUseName"
    )

    # empty dataframe with just column names
    par <- data.frame(matrix(nrow = 0, ncol = length(columns)))
    colnames(par) <- columns

    wb <- openxlsx::loadWorkbook(wb, downloads_path)

    # If a user chooses to rerun the TADA_UsesForAnalysis() function,
    # the sheet will already exist and error.
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateUsesRef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateUsesRef")
        openxlsx::addWorksheet(wb, "CreateUsesRef")
      }
    )

    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }

    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")

    # Format Column widths
    openxlsx::setColWidths(
      wb,
      "CreateUsesRef",
      cols = 1:ncol(CreateUsesRef),
      widths = "auto"
    )

    # Export CreateUsesRef dataframe into the excel spreadsheet tab
    openxlsx::writeData(
      wb,
      "CreateUsesRef",
      startCol = 1,
      x = CreateUsesRef,
      headerStyle = header_st
    )

    # Index of allowable values for drop-down lists
    openxlsx::writeData(
      wb,
      "Index",
      startCol = 9,
      x = data.frame("IncludeOrExclude" = c("Include", "Exclude"))
    )

    openxlsx::writeData(
      wb,
      "Index",
      startCol = 7,
      x = CreateUsesRef[, c("ATTAINS.FlagUseName", "Flag.UseInput")]
    )

    # Data validation drop down list created below.
    # Note: ATTAINSOrgNamesParamRef contains the list of prior param and use cause by org names specific.
    # Since Use Names are individual to each Organization.
    if (!missing(.data)) {
      suppressWarnings(
        # Data validation for ATTAINS.UseName.
        openxlsx::dataValidation(
          wb,
          sheet = "CreateUsesRef",
          cols = 4,
          rows = 2:10000,
          type = "list",
          value = sprintf("'ATTAINSOrgNamesParamRef'!$E$2:$E$50000"),
          allowBlank = TRUE,
          showErrorMsg = TRUE,
          showInputMsg = TRUE
        )
      )
    }

    # For case in which a blank template is generated, allowable use_name = entire ATTAINS domain value.
    if (missing(.data)) {
      suppressWarnings(
        # Data validation for ATTAINS.UseName.
        openxlsx::dataValidation(
          wb,
          sheet = "CreateUsesRef",
          cols = 4,
          rows = 2:10000,
          type = "list",
          value = sprintf("'Index'!$F$2:$F$50000"),
          allowBlank = TRUE,
          showErrorMsg = TRUE,
          showInputMsg = TRUE
        )
      )
    }

    suppressWarnings(
      # Data validation for "Include" or "Exclude" values.
      openxlsx::dataValidation(
        wb,
        sheet = "CreateUsesRef",
        cols = 5,
        rows = 2:1000,
        type = "list",
        value = sprintf("'Index'!$I$2:$I$5"),
        allowBlank = TRUE,
        showErrorMsg = TRUE,
        showInputMsg = TRUE
      )
    )

    max_loops <- 0

    for (i in 1:nrow(CreateUsesRef)) {
      # Formula based cell values in excel.
      openxlsx::writeFormula(
        wb,
        "CreateUsesRef",
        startCol = 6,
        startRow = i + 1,
        array = TRUE,
        x = paste0(
          "=IF(E",
          i + 1,
          '="Exclude",
            "Use name does not apply for this ATTAINS.ParameterName. Excluding this use name from analysis.",
          IF(ISBLANK(D',
          i + 1,
          '),
            "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
          IF(ISNA(MATCH(1,(D',
          i + 1,
          "=ATTAINSOrgNamesParamRef!E:E)*(B",
          i + 1,
          '=ATTAINSOrgNamesParamRef!A:A),0)),
            "Use name has not been assessed in prior cycles.",
          IF(ISNA(MATCH(1,(C',
          i + 1,
          "=ATTAINSOrgNamesParamRef!D:D)*(D",
          i + 1,
          "=ATTAINSOrgNamesParamRef!E:E)*(B",
          i + 1,
          '=ATTAINSOrgNamesParamRef!A:A),0)),
            "Use name has been assessed in prior cycles by this organization, but not for this parameter name.",
            "Use name has been assessed in prior cycles by this organization."))))'
        )
      )

      openxlsx::writeFormula(
        wb,
        "CreateUsesRef",
        startCol = 7,
        startRow = i + 1,
        array = TRUE,
        x = paste0(
          "IF(F",
          i + 1,
          "=Index!G$",
          i + 1,
          ",Index!H$",
          i + 1,
          ',"This row was MODIFIED by your input(s).")'
        )
      )
      max_loops <- max_loops + 1
      if (max_loops > 100) break
    }

    # Conditional formatting created below.

    # If a user has left an ATTAINS.UseName blank, flag as a red cell.
    openxlsx::conditionalFormatting(
      wb,
      "CreateUsesRef",
      cols = 4,
      rows = 1:nrow(CreateUsesRef) + 1,
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )

    # If a user has an ATTAINS.UseName filled out, flag as a yellow cell.
    openxlsx::conditionalFormatting(
      wb,
      "CreateUsesRef",
      cols = 4,
      rows = 1:nrow(CreateUsesRef) + 1,
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    )

    # If a user has chose to Exclude a use name for a parameter, flag as a red cell.
    openxlsx::conditionalFormatting(
      wb,
      "CreateUsesRef",
      cols = 5,
      rows = 1:nrow(CreateUsesRef) + 1,
      type = "contains",
      rule = c("Exclude"),
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )

    # If a user has chose to Include a use name for a parameter, flag as a yellow cell.
    openxlsx::conditionalFormatting(
      wb,
      "CreateUsesRef",
      cols = 5,
      rows = 1:nrow(CreateUsesRef) + 1,
      type = "contains",
      rule = c("Include"),
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    )

    # Format Formula Based Column widths
    openxlsx::setColWidths(wb, "CreateUsesRef", cols = 6:7, widths = 16)

    # Handles overwriting the excel file.
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }

    if (overwrite == FALSE) {
      warning(
        "If you would like to replace [CreateUsesRef], use overwrite = TRUE argument in TADA_UsesForAnalysis"
      )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }

    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  return(CreateUsesRef)
}


#' ATTAINS Assessment Unit and Use Name Crosswalk
#'
#' This function pulls in all prior ATTAINS Use names associated with each
#' Assessment Unit (AU) from the prior ATTAINS cycle. This function requires an
#' ATTAINS.OrganizationIdentifier and a crosswalk of an organization's WQP
#' Monitoring Locations, ATTAINS Assessment Units, and ATTAINS Water Type as a
#' function input (AUMLRef). The output from the $ATTAINS_crosswalk list from
#' `TADA_CreateATTAINSAUMLCrosswalk(.data, return_sf = FALSE)` can
#' be used directly as the AUMLRef argument input in this function. Alternatively,
#' a user supplied crosswalk can be entered or `TADA_GetATTAINSAUMLCrosswalk()`
#' and/or `TADA_UpdateATTAINSAUMLCrosswalk()` functions can be leveraged
#' to generate the crosswalk.
#'
#' This function is mainly designed to assist with pulling
#' existing Uses that have been entered
#' into ATTAINS in the prior ATTAINS cycle (most recent assessment).
#'
#' For any NEW AUs and/or NEW uses, users must modify
#' the output of this function to manually add those uses and assessment units to
#' the crosswalk. Alternatively, we have developed a helper function,
#' [TADA_AssignUsesToWaterType()], to assist with assigning uses to NEW AUs. This
#' can be leveraged to assign uses for any new assessment units based on water type.
#' Users can either supply their own Water Type to Use crosswalk or utilize ATTAINS
#' webservices to pull in the Water Type to Use reference file. This Water to Use
#' reference file can be used to assign all unique Uses to a new/modified AU based
#' on which uses have been assigned to that water type in the past by the specified
#' ATTAINS organization. Any new or modified AU and use information that gets
#' submitted to ATTAINS in the current assessment cycle will not be available via
#' ATTAINS webservices until the assessment is approved and completed.
#'
#' @param .data A TADA dataframe after all desired data cleaning,
#' processing, harmonization, filtering, and censored data handling functions
#' have been applied.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. "USEPA" may be included as an org_id which will populate the EPA 304(a)
#' recommended criteria for any TADA.CharacteristicName if one is found. "All" or
#' "NULL" are also allowable values and may be helpful for new ATTAINS users or
#' those performing assessments for multiple states and tribes. If "All" is
#' selected, this will return all prior ATTAINS information from all ATTAINS
#' organizations in prior ATTAINS assessment cycles as individual rows for each
#' organization. If "NULL" is selected all unique prior ATTAINS information from
#' any ATTAINS organizations are returned but are not labeled and can be manually
#' edited. Enter `rExpertQuery::EQ_DomainValues("org_id")` into the console to
#' get a list of valid organization identifiers. A list of organization identifiers
#' can also be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "code" column of the "OrgName" tab.
#'
#' @param api_key Optional character string. An api key for Expert Query web
#' services. If not supplied, the default TADA api key will be used. For best
#' performance, it is recommended that users obtain and use their own api key.
#' Request an api key here: https://owapps.epa.gov/expertquery/api-documentation
#'
#' @param AU_UsesRef An optional data frame input. If provided, the ATTAINS.UseName
#' will be populated from the ATTAINS.UseName found in this data frame rather
#' than the ATTAINS assessment profile. This data frame must contain the following
#' column names which can be generated from the output of TADA_AssignUsesToAU:
#' ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName,
#' and ATTAINS.WaterType.
#'
#' @param AUMLRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. This data frame must contain the following
#' column names which can be generated from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
#'
#' @param waterUseRef An optional data frame input containing a user supplied list
#' of ATTAINS uses to ATTAINS water type. Users will need to ensure this crosswalk
#' contains the appropriate column names in order to run the function.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value. If overwrite = TRUE, the excel file will be
#' replaced (overwritten) by the new file you create if you re-run this function.
#' Users should only specify overwrite = TRUE once they are ready to re-run this
#' function if they have already ran it once.
#'
#' @seealso [TADA_DataRetrieval()] for the required format of .data
#' @seealso [TADA_CreateATTAINSAUMLCrosswalk()] to help generate the required AUMLRef
#' @seealso [TADA_GetATTAINSAUMLCrosswalk()] to help generate the required AUMLRef
#' @seealso [TADA_UpdateATTAINSAUMLCrosswalk()] to help generate the required AUMLRef
#' @seealso [TADA_AssignUsesToWaterType()] to help assign ATTAINS Uses to NEW ATTAINS Assessment Units based on ATTAINS Water Type
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for each defined AU.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pull a sample TADA data frame
#' TADA_AK_Example <- TADA_DataRetrieval(
#'   startDate = "2022-01-01", endDate = "2022-12-31",
#'   organization = "AKDECWQ", statecode = "AK",
#'   characteristicName = c("Enterococcus", "Escherichia", "Escherichia coli"),
#'   ask = FALSE
#' )
#'
#' # Alaska example to update data links with no user supplied crosswalk
#' AK_adddatalinks <- TADA_UpdateATTAINSAUMLCrosswalk(
#'   org_id = "AKDECWQ",
#'   crosswalk = NULL,
#'   attains_replace = FALSE,
#'   wqp_data_links = "replace"
#' )
#'
#' # Alaska example using a user supplied crosswalk to update entries in
#' # ATTAINS by appending user supplied information to ATTAINS crosswalk
#'
#' # example new AU identifiers
#' ATTAINS.AssessmentUnitIdentifier <- c(
#'   "NEW:AK_M_1021211_000", "NEW:AK_M_1021008_000",
#'   "NEW:AK_M_1021109_013", "NEW:AK_L_2040108_063",
#'   "NEW:AK_M_1021109_013"
#' )
#'
#' # example organization identifiers
#' ATTAINS.OrganizationIdentifier <-
#'   c("AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ")
#'
#' # example ML matched to new AUs, these are only examples
#' ATTAINS.MonitoringLocationIdentifier <- c(
#'   "AKDECWQ-Snag Point", "AKDECWQ-Kanakanak", "AKDECWQ-ANC02_HSLP",
#'   "AKDECWQ-Scandinavian", "AKDECWQ-ANC03_HSLP"
#' )
#'
#' ATTAINS.WaterType <- c(
#'   "BEACH", "BAY", "CREEK",
#'   "ESTUARY", "CREEK"
#' )
#'
#' # create example crosswalk data frame
#' ex.user.cw <- data.frame(
#'   ATTAINS.MonitoringLocationIdentifier,
#'   ATTAINS.OrganizationIdentifier,
#'   OrganizationIdentifier = ATTAINS.OrganizationIdentifier,
#'   ATTAINS.AssessmentUnitIdentifier,
#'   ATTAINS.MonitoringDataLinkText = NA, ATTAINS.WaterType
#' )
#'
#' AK_appenduserdata <- TADA_UpdateATTAINSAUMLCrosswalk(
#'   org_id = "AKDECWQ",
#'   crosswalk = ex.user.cw,
#'   attains_replace = FALSE,
#'   wqp_data_links = "replace"
#' )
#'
#' # New AUs that are not found in ATTAINS show blank ATTAINS.UseName
#' AK_CreateAU_UsesRef <- TADA_AssignUsesToAU(
#'   TADA_AK_Example,
#'   org_id = "AKDECWQ",
#'   AUMLRef = AK_appenduserdata,
#'   excel = FALSE
#' )
#'
#' # Let's use a wateruseRef now to fill in these values.
#' AK_CreateAU_UsesRef_auto_assign <- TADA_AssignUsesToAU(
#'   TADA_AK_Example,
#'   org_id = "AKDECWQ",
#'   AUMLRef = AK_appenduserdata,
#'   waterUseRef = TADA_AssignUsesToWaterType(TADA_AK_EXAMPLE, org_id = "AKDECWQ"),
#'   excel = FALSE
#' )
#'
#' # We can save and reuse a AU_UsesRef as desired.
#' AK_CreateAU_UsesRef2 <- TADA_AssignUsesToAU(
#'   TADA_AK_Example,
#'   org_id = "AKDECWQ",
#'   AU_UsesRef = AK_CreateAU_UsesRef_auto_assign,
#'   AUMLRef = AK_appenduserdata,
#'   excel = FALSE
#' )
#' }
#'
TADA_AssignUsesToAU <- function(
  .data,
  org_id = NULL,
  AUMLRef = NULL,
  AU_UsesRef = NULL,
  waterUseRef = NULL,
  excel = FALSE,
  overwrite = FALSE,
  api_key = NULL
) {
  # get default api_key if user does not supply one
  if (is.null(api_key)) {
    api_key <- .setEQKey()
  }

  # Return an empty dataframe with column names only if a user does not define any arg inputs.
  if (
    missing(.data) && missing(org_id) && missing(excel) && missing(overwrite)
  ) {
    message(
      "All arguments are blank, returning an empty dataframe with column names only."
    )

    empty_df <- data.frame(
      ATTAINS.OrganizationIdentifier = character(0),
      ATTAINS.AssessmentUnitIdentifier = character(0), # ATTAINS.assessmentunitname,
      ATTAINS.UseName = character(0),
      ATTAINS.WaterType = character(0),
      TADA.AssessmentUnitStatus = character(0),
      IncludeOrExclude = character(0)
    )

    return(empty_df)
  } else {
    # overwrite argument should only be used when creating an excel file.
    if (excel == FALSE && overwrite == TRUE) {
      stop(paste0(
        "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
        "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE."
      ))
    }

    if (is.null(AUMLRef)) {
      stop(paste0(
        "TADA_AssignUsesToAU: ",
        "You must provide an AUMLRef to run this function."
      ))
    }

    # Pulls in all domain values of parameter and use names by orgs in ATTAINS. Filtering by state is done in the next steps.
    ATTAINS_param_all <- utils::read.csv(system.file(
      "extdata",
      "ATTAINSParamUseEntityRef.csv",
      package = "EPATADA"
    ))

    # check to see if user-supplied AUMLRef is a df with appropriate columns and is filled out.
    if (!is.null(AUMLRef) & !is.character(AUMLRef)) {
      if (!is.data.frame(AUMLRef)) {
        stop(paste0(
          "TADA_AssignUsesToAU: 'AUMLRef' must be a data frame with these 3 columns:",
          "ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier, and ATTAINS.OrganizationIdentifier."
        ))
      }

      if (is.data.frame(AUMLRef)) {
        col.names <- c(
          "ATTAINS.WaterType",
          "ATTAINS.AssessmentUnitIdentifier",
          "ATTAINS.OrganizationIdentifier"
        )

        if (!any(col.names %in% names(AUMLRef))) {
          stop(paste0(
            "TADA_AssignUsesToAU: 'AUMLRef' must be a data frame with these 3 columns:",
            "ATTAINS.WaterType, ATTAINS.OrganizationIdentifier and ATTAINS.AssessmentUnitIdentifier"
          ))
        }

        AULMLRef <- AUMLRef |>
          dplyr::select(
            ATTAINS.AssessmentUnitIdentifier,
            ATTAINS.WaterType,
            ATTAINS.OrganizationIdentifier
          )
      }
    }

    # if null, creates a list of all unique TADA.ComparableDataIdentifier, but no org populated.
    if (!is.character(org_id) & is.null(org_id)) {
      org_id <- ""
    }
    # if org_id = all, create a crosswalk for all ATTAINS org in the data frame.
    if (tolower("all") %in% tolower(org_id)) {
      # If a user selects org_id = all but doesn't provide an AUMLRef with ATTAINS organization identifier.
      if (is.null(AUMLRef)) {
        print(paste0(
          "org_id == 'All' was selected, ",
          "No AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier domain value."
        ))
        org_id <- rExpertQuery::EQ_DomainValues("org_id")[, "code"]
      }
      # If a user selects org_id = all and does provide an AUMLRef with ATTAINS organization identifier.
      if (!is.null(AUMLRef)) {
        print(paste0(
          "org_id == 'All' was selected, ",
          "An AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier in your AUMLRef."
        ))
        org_id <- unique(stats::na.omit(AUMLRef$ATTAINS.OrganizationIdentifier))
      }
    }

    # Handle later, if multiple org_id are used, create a loop when calling rATTAINS (or if we use ATTAINS Expert Query National extract, no loop needed)
    # org_id <- as.list(org_id)

    # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA 304(a)" as that is not an ATTAINS org_id.
    if (
      sum(
        !org_id[org_id != "EPA304a"] %in%
          c(
            utils::read.csv(system.file(
              "extdata",
              "ATTAINSOrgIDsRef.csv",
              package = "EPATADA"
            ))[, "code"],
            ""
          )
      ) >
        0
    ) {
      warning(paste0(
        "TADA_AssignUsesToAU: ",
        "One or more organization identifiers entered by user is not found in ATTAINS. "
      ))
    }

    # Pulls in Existing Uses by Existing AU from ATTAINS Expert Query
    print(
      "TADA_AssignUsesToAU: Importing existing uses by AU from ATTAINS Expert Query."
    )

    OrgID_assessments <- spsUtil::quiet(rExpertQuery::EQ_Assessments(
      org_id = org_id,
      api_key = api_key
    ))

    OrgID_assessments <- dplyr::filter(
      OrgID_assessments,
      assessmentUnitId %in% unique(AUMLRef$ATTAINS.AssessmentUnitIdentifier)
    )

    # Joins Existing Uses to Existing AUs in your AUMLRef dataframe. Non-matches are flagged as New AU.
    CreateAU_UsesRef <- AUMLRef |>
      dplyr::left_join(
        OrgID_assessments,
        by = c(
          "ATTAINS.AssessmentUnitIdentifier" = "assessmentUnitId",
          "ATTAINS.OrganizationIdentifier" = "organizationId"
        ),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        TADA.AssessmentUnitStatus = dplyr::if_else(
          ATTAINS.AssessmentUnitIdentifier %in%
            unique(OrgID_assessments$assessmentUnitId),
          "Existing",
          "New"
        )
      ) |>
      dplyr::bind_cols(data.frame(
        IncludeOrExclude = as.character("Include")
      )) |>
      dplyr::mutate(
        ATTAINS.WaterType = dplyr::coalesce(waterType, ATTAINS.WaterType)
      ) |>
      dplyr::select(
        ATTAINS.OrganizationIdentifier,
        ATTAINS.AssessmentUnitIdentifier, # ATTAINS.assessmentunitname,
        ATTAINS.UseName = useName,
        ATTAINS.WaterType,
        TADA.AssessmentUnitStatus,
        IncludeOrExclude
      ) |>
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) |>
      sf::st_drop_geometry() |>
      dplyr::distinct() |>
      dplyr::arrange(ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName)

    # User provides a WaterUseRef - specifying the assignment of Uses to AUs not found in ATTAINS by its Water Type.
    if (!is.null(waterUseRef)) {
      AUMLRef <- dplyr::select(
        AUMLRef,
        ATTAINS.AssessmentUnitIdentifier,
        ATTAINS.OrganizationIdentifier,
        ATTAINS.WaterType
      )

      waterUseRef <- waterUseRef |> dplyr::filter(IncludeOrExclude == "Include")

      CreateAU_UsesRef_MissingUse <- dplyr::filter(
        CreateAU_UsesRef,
        is.na(ATTAINS.UseName)
      )

      CreateAU_UsesRef_MissingUse <- CreateAU_UsesRef_MissingUse |>
        dplyr::select(
          ATTAINS.AssessmentUnitIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.WaterType,
          TADA.AssessmentUnitStatus
        ) |>
        dplyr::left_join(
          AUMLRef,
          by = c(
            "ATTAINS.OrganizationIdentifier",
            "ATTAINS.AssessmentUnitIdentifier",
            "ATTAINS.WaterType"
          )
        ) |>
        dplyr::left_join(
          waterUseRef,
          by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.WaterType")
        )

      CreateAU_UsesRef <- CreateAU_UsesRef |>
        dplyr::filter(!is.na(ATTAINS.UseName)) |>
        dplyr::bind_rows(CreateAU_UsesRef_MissingUse) |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier, # ATTAINS.assessmentunitname,
          ATTAINS.UseName,
          ATTAINS.WaterType,
          TADA.AssessmentUnitStatus,
          IncludeOrExclude
        ) |>
        dplyr::distinct()
    }

    # User provides their own AU_UsesRef that has been filled out.
    if (!is.null(AU_UsesRef)) {
      message(paste(
        "TADA_AssignUsesToAU: When AU_UsesRef (a user-provided mapping of Assessment Units to Uses) is supplied,",
        "the function assigns only the Uses listed in that mapping to the AUs it contains and does not append any additional Uses."
      ))

      AU_UsesRef_matches <- AU_UsesRef |>
        dplyr::filter(
          ATTAINS.AssessmentUnitIdentifier %in%
            OrgID_assessments$assessmentUnitId
        ) |>
        dplyr::mutate(
          TADA.AssessmentUnitStatus = "Existing",
          IncludeOrExclude = dplyr::if_else(
            is.na(IncludeOrExclude),
            "Include",
            IncludeOrExclude
          )
        )

      CreateAU_UsesRef <- CreateAU_UsesRef |>
        dplyr::filter(
          !ATTAINS.AssessmentUnitIdentifier %in%
            AU_UsesRef$ATTAINS.AssessmentUnitIdentifier
        ) |>
        dplyr::mutate(
          TADA.AssessmentUnitStatus = "New",
          IncludeOrExclude = "Include"
        ) |>
        plyr::rbind.fill(AU_UsesRef_matches)
    }

    if (excel == TRUE) {
      # default Downloads file location.
      downloads_path <- file.path(
        Sys.getenv("USERPROFILE"),
        "Downloads",
        "CreateAU_UsesRef.xlsx"
      )

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "CreateAU_UsesRef", visible = TRUE)
      openxlsx::addWorksheet(wb, "Index", visible = FALSE)

      # Format column header
      header_st <- openxlsx::createStyle(textDecoration = "Bold")
      # Format Column widths
      openxlsx::setColWidths(
        wb,
        "CreateAU_UsesRef",
        cols = 1:ncol(CreateAU_UsesRef),
        widths = "auto"
      )

      # set zoom size
      set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
      n_sheets <- length(wb$worksheets)
      for (i in 1:n_sheets) {
        sV <- wb$worksheets[[i]]$sheetViews
        wb$worksheets[[i]]$sheetViews <- set_zoom(90)
      }

      # writes CreateAU_UsesRef dataframe
      openxlsx::writeData(
        wb,
        "CreateAU_UsesRef",
        startCol = 1,
        x = CreateAU_UsesRef,
        headerStyle = header_st
      )

      # Index of allowable values for drop-down lists
      openxlsx::writeData(
        wb,
        "Index",
        startCol = 1,
        x = data.frame("IncludeOrExclude" = c("Include", "Exclude"))
      )

      # data validation drop down list created below.
      suppressWarnings(openxlsx::dataValidation(
        wb,
        sheet = "CreateAU_UsesRef",
        cols = 6,
        rows = 2:10000,
        type = "list",
        value = sprintf("'Index'!$A$2:$A$5"),
        allowBlank = TRUE,
        showErrorMsg = TRUE,
        showInputMsg = TRUE
      ))

      # Conditional Formatting
      openxlsx::conditionalFormatting(
        wb,
        "CreateAU_UsesRef",
        cols = 6,
        rows = 2:(nrow(CreateAU_UsesRef) + 1),
        type = "contains",
        rule = "Include",
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
      ) # default values or indicates good to go cells.

      openxlsx::conditionalFormatting(
        wb,
        "CreateAU_UsesRef",
        cols = 6,
        rows = 2:(nrow(CreateAU_UsesRef) + 1),
        type = "contains",
        rule = "Exclude",
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      ) # using yellow to indicate modified cell

      if (overwrite == TRUE) {
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
      }

      if (overwrite == FALSE) {
        warning(
          "If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateUseAUef"
        )
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
      }

      cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")

      CreateAU_UsesRef <- openxlsx::read.xlsx(
        downloads_path,
        sheet = "CreateAU_UsesRef"
      )
    }

    return(CreateAU_UsesRef)
  }
}


#' Helper Function to Apply Uses to Unassigned Assessment Units by Water Type
#'
#' This is a helper function to TADA_AssignUsesToAU and is meant to help users
#' with reviewing all water type and use name combination from their org.
#' This function will help to assign ATTAINS use names to any new or modified
#' assessment unit provided from a user's AUMLRef if there are any.
#'
#' This function will assume all use names applies to a water type from the
#' prior assessment cycles are being done for an organization's assessment.
#' Users are expected to modify this ref file as needed.
#'
#' @param .data A TADA dataframe after all desired data cleaning,
#' processing, harmonization, filtering, and censored data handling functions
#' have been applied.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. "USEPA" may be included as an org_id which will populate the EPA 304(a)
#' recommended criteria for any TADA.CharacteristicName if one is found. "All" or
#' "NULL" are also allowable values and may be helpful for new ATTAINS users or
#' those performing assessments for multiple states and tribes. If "All" is
#' selected, this will return all prior ATTAINS information from all ATTAINS
#' organizations in prior ATTAINS assessment cycles as individual rows for each
#' organization. If "NULL" is selected all unique prior ATTAINS information from
#' any ATTAINS organizations are returned but are not labeled and can be manually
#' edited. Enter `rExpertQuery::EQ_DomainValues("org_id")` into the console to
#' get a list of valid organization identifiers. A list of organization identifiers
#' can also be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "code" column of the "OrgName" tab.
#'
#' @param api_key Optional character string. An api key for Expert Query web
#' services. If not supplied, the default TADA api key will be used. For best
#' performance, it is recommended that users obtain and use their own api key.
#' Request an api key here: https://owapps.epa.gov/expertquery/api-documentation
#'
#' @param waterUseRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with a water type.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function.
#'
#' @param AUMLRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. This data frame must contain the following
#' column names which can be generated from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#'
#' @seealso [TADA_AssignUsesToAU()]
#'
#' @export
#'
#' @examples
#' TADA_AssignUsesToWaterType(TADA_AK_EXAMPLE, org_id = "AKDECWQ")
#'
TADA_AssignUsesToWaterType <- function(
  .data,
  org_id = NULL,
  waterUseRef = NULL,
  AUMLRef = NULL,
  api_key = NULL
) {
  # get default api_key if user does not supply one
  if (is.null(api_key)) {
    api_key <- .setEQKey()
  }

  # if null, creates a list of all unique TADA.ComparableDataIdentifier, but no org populated.
  if (!is.character(org_id) & is.null(org_id)) {
    org_id <- ""
  }
  # if org_id = all, create a crosswalk for all ATTAINS org in the data frame.
  if (tolower("all") %in% tolower(org_id)) {
    # If a user selects org_id = all but doesn't provide an AUMLRef with ATTAINS organization identifier.
    if (is.null(AUMLRef)) {
      print(paste0(
        "org_id == 'All' was selected, ",
        "No AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier domain value."
      ))
      org_id <- rExpertQuery::EQ_DomainValues("org_id")[, "code"]
    }
    # If a user selects org_id = all and does provide an AUMLRef with ATTAINS organization identifier.
    if (!is.null(AUMLRef)) {
      print(paste0(
        "org_id == 'All' was selected, ",
        "An AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier in your AUMLRef."
      ))
      org_id <- unique(stats::na.omit(AUMLRef$ATTAINS.OrganizationIdentifier))
    }
  }

  # rExpertQuery API key for TADA
  tadakey <- "EKtgCrmatyP4G8iFgADMIfwlddbpDlSqRxetlN09"

  # Pulls in all domain values of parameter and use names by orgs in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file(
    "extdata",
    "ATTAINSParamUseEntityRef.csv",
    package = "EPATADA"
  ))

  # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA 304(a)" as that is not an ATTAINS org_id.
  if (
    sum(
      !org_id[org_id != "EPA304a"] %in%
        c(
          utils::read.csv(system.file(
            "extdata",
            "ATTAINSOrgIDsRef.csv",
            package = "EPATADA"
          ))[, "code"],
          ""
        )
    ) >
      0
  ) {
    warning(paste0(
      "TADA_CreateWaterusesRef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS. "
    ))
  }

  # Calls on EQ_Assessments from latest assessment cycle. Pulls in unique water types and uses by org
  print(paste0(
    "TADA_CreateWaterusesRef: Importing unique water types and uses ",
    "by organization from Expert Query."
  ))

  OrgID_assessments <- spsUtil::quiet(rExpertQuery::EQ_Assessments(
    org_id = org_id,
    api_key = api_key
  ))

  CreateWaterUseRef <- OrgID_assessments[, c(
    "organizationName",
    "organizationId",
    "waterType",
    "useName"
  )] |>
    dplyr::distinct() |>
    dplyr::bind_cols(data.frame(IncludeOrExclude = as.character("Include"))) |>
    dplyr::select(
      ATTAINS.OrganizationName = organizationName,
      ATTAINS.OrganizationIdentifier = organizationId,
      ATTAINS.UseName = useName,
      ATTAINS.WaterType = waterType,
      IncludeOrExclude
    )

  # User supplies their own use to water ref table.
  if (!is.null(waterUseRef)) {
    CreateWaterUseRef <- waterUseRef |>
      dplyr::distinct() |>
      dplyr::bind_cols(data.frame(
        IncludeOrExclude = as.character("Include")
      )) |>
      dplyr::select(
        ATTAINS.OrganizationName,
        ATTAINS.OrganizationIdentifier,
        ATTAINS.UseName,
        ATTAINS.WaterType,
        IncludeOrExclude
      )
  }

  return(CreateWaterUseRef)
}


#' Review and Apply Any Site-specific Criteria to Monitoring Location Sites or Assessment Units
#'
#' This function will pull in all unique MonitoringLocationName, MonitoringLocationType,
#' and MonitoringLocationIdentifier from the TADA dataframe and join it to
#' TADA_UsesForAnalysis. Users are not required to provide a crosswalk between
#' WQP Monitoring locations and Assessment units if they are only interested in
#' summarizing assessments on a monitoring location level.
#'
#' If users are interested in summarizing water quality data results by Assessment
#' Units, users will need to provide an AUMLRef and AU_UsesRef file which
#' (see TADA Module 2 tools) to assist in their monitoring location to assessment
#' unit crosswalk (see TADA_GetATTAINSAUMLCrosswalk, TADA_CreateAUMLCrosswalk,
#' and TADA_GetATTAINSByAUID) and uses to assessment unit crosswalk
#' (see TADA_CreateWaterusesRef and TADA_AssignUsesToAU) prior to this step.
#'
#' Users can apply any unique site-specific criteria (for example, warm waters,
#' cold waters, water classifications, species-based waters, ecoregions etc.) to
#' any monitoring location sites or assessment units as needed. Users are recommended
#' to utilize the excel file for easy filtering across columns to apply any
#' site specific criteria as needed.
#'
#' @param .data A TADA dataframe after all desired data cleaning,
#' processing, harmonization, filtering, and censored data handling functions
#' have been applied.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. "USEPA" may be included as an org_id which will populate the EPA 304(a)
#' recommended criteria for any TADA.CharacteristicName if one is found. "All" or
#' "NULL" are also allowable values and may be helpful for new ATTAINS users or
#' those performing assessments for multiple states and tribes. If "All" is
#' selected, this will return all prior ATTAINS information from all ATTAINS
#' organizations in prior ATTAINS assessment cycles as individual rows for each
#' organization. If "NULL" is selected all unique prior ATTAINS information from
#' any ATTAINS organizations are returned but are not labeled and can be manually
#' edited. Enter `rExpertQuery::EQ_DomainValues("org_id")` into the console to
#' get a list of valid organization identifiers. A list of organization identifiers
#' can also be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "code" column of the "OrgName" tab.
#'
#' @param usesRef A data frame which contains a completed crosswalk of
#' ATTAINS.ParameterName(s) that will be analyzed for each ATTAINS.UseName.
#' Users will need to ensure this crosswalk contains the appropriate column
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and review this output for accuracy.
#'
#' @param AU_UsesRef An optional data frame input. If provided, the ATTAINS.UseName
#' will be populated from the ATTAINS.UseName found in this data frame rather
#' than the ATTAINS assessment profile. This data frame must contain the following
#' column names which can be generated from the output of TADA_AssignUsesToAU:
#' ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName,
#' and ATTAINS.WaterType.
#'
#' @param AUMLRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. This data frame must contain the following
#' column names which can be generated from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
#'
#' @param MLSummaryRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit. If provided the data frame must contain
#' these columns:
#' "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
#' "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
#' "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
#' "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "LongitudeMeasure",
#' "LatitudeMeasure", "IncludeOrExclude" and "UniqueSpatialCriteria".
#'
#' @param displayNA A boolean value. If TRUE, this allows user to view MLSummaryRef
#' for all uses and parameter assigned to a ML or AU regardless if that site contains
#' WQP data for that parameter. This is useful if a user is interested in an explicit
#' list of everything that will be analyzed. Default is FALSE.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value. If overwrite = TRUE, the excel file will be
#' replaced (overwritten) by the new file you create if you re-run this function.
#' Users should only specify overwrite = TRUE once they are ready to re-run this
#' function if they have already ran it once.
#'
#' @return A data frame with any unique spatial descriptions defined with columns:
#' "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
#' "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
#' "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
#' "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "LongitudeMeasure",
#' "LatitudeMeasure", "IncludeOrExclude" and "UniqueSpatialCriteria".
#'
#' @seealso [TADA_UsesForAnalysis()]
#' @seealso [TADA_AssignUsesToAU()]
#' @seealso [TADA_AssignUsesToWaterType()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First, generate and fill out a parameter crosswalk (see TADA_ParametersForAnalysis()):
#' paramRef_UT <- TADA_ParametersForAnalysis(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_ParametersForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_UsesForAnalysis():
#' usesRef_UT <- TADA_UsesForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Now, run TADA_MLSummary()
#' MLSummaryRef_UT <- TADA_MLSummary(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   AU_UsesRef = NULL, AUMLRef = NULL,
#'   usesRef = usesRef_UT,
#'   excel = FALSE
#' )
#' }
#'
TADA_MLSummary <- function(
  .data,
  org_id = NULL,
  usesRef = NULL,
  AUMLRef = NULL,
  AU_UsesRef = NULL,
  MLSummaryRef = NULL, # If provided, keep all rows in this user supplied list if the param and use is found in the usesRef.
  displayNA = FALSE, # If FALSE, only show rows for param(s) and uses(s) if that param is found for a WQP site.
  excel = FALSE,
  overwrite = FALSE
) {
  # Return an empty dataframe with column names only if a user does not define any arg inputs.
  if (
    missing(.data) &&
      missing(org_id) &&
      missing(usesRef) &&
      missing(AUMLRef) &&
      missing(AU_UsesRef) &&
      missing(MLSummaryRef)
  ) {
    message(
      "All arguments are blank, returning an empty dataframe with column names only."
    )

    CreateMLSummaryRef <- data.frame(
      ATTAINS.OrganizationIdentifier = character(0),
      ATTAINS.AssessmentUnitIdentifier = character(0),
      MonitoringLocationIdentifier = character(0),
      MonitoringLocationTypeName = character(0),
      TADA.ComparableDataIdentifier = character(0),
      ATTAINS.ParameterName = character(0),
      ATTAINS.UseName = character(0),
      ATTAINS.WaterType = character(0),
      SaltFresh = character(0),
      DepthCategory = character(0),
      LongitudeMeasure = character(0),
      LatitudeMeasure = character(0),
      IncludeOrExclude = character(0),
      UniqueSpatialCriteria = character(0)
    )
  } else {
    # overwrite argument should only be used when creating an excel file.
    if (excel == FALSE && overwrite == TRUE) {
      stop(paste0(
        "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
        "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
      ))
    }

    # Creates the data frame.
    CreateMLSummaryRef <- data.frame()
    # This allows a user to provide the mod 2 function TADA_CreateATTAINSAUMLCrosswalk() as the .data data frame.
    # In this case, the ML to AU crosswalk is generated from TADA_CreateATTAINSAUMLCrosswalk().
    if (!is.data.frame(.data)) {
      if (
        !any(
          c(
            "TADA_with_ATTAINS",
            "ATTAINS_catchments",
            "ATTAINS_points",
            "ATTAINS_lines",
            "ATTAINS_polygons"
          ) %in%
            names(.data)
        )
      ) {
        stop(
          "Your input dataframe was not produced from `TADA_CreateATTAINSAUMLCrosswalk()` or it was modified. Please create your list of ATTAINS features using `TADA_GetATTAINS(return_sf = TRUE)`"
        )
      }
      # .data <- .data[["TADA_with_ATTAINS"]]
    }

    # check to see if user-supplied AU_UsesRef is a df with appropriate columns and is filled out.
    if (!is.null(AU_UsesRef) & !is.character(AU_UsesRef)) {
      if (!is.data.frame(AU_UsesRef)) {
        stop(paste0(
          "TADA_MLSummary: 'AU_UsesRef' must be a data frame with these 3 columns:",
          "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.AssessmentUnitIdentifier"
        ))
      }

      if (is.data.frame(AU_UsesRef)) {
        col.names <- c(
          "ATTAINS.UseName",
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.AssessmentUnitIdentifier"
        )

        ref.names <- names(AU_UsesRef)

        if (length(setdiff(col.names, ref.names)) > 0) {
          stop(paste0(
            "TADA_MLSummary: 'AU_UsesRef' must be a data frame with these 3 columns:",
            "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.AssessmentUnitIdentifier"
          ))
        }
      }
    }

    # check to see if user-supplied usesRef ref is a df with appropriate columns and filled out.
    if (!is.null(usesRef) & !is.character(usesRef)) {
      if (!is.data.frame(usesRef)) {
        stop(paste0(
          "TADA_MLSummary: 'usesRef' must be a data frame with these 5 columns:",
          "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
          "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
        ))
      }

      if (is.data.frame(usesRef)) {
        col.names <- c(
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName",
          "ATTAINS.UseName"
        )

        ref.names <- names(usesRef)

        if (length(setdiff(col.names, ref.names)) > 0) {
          stop(paste0(
            "TADA_MLSummary: 'usesRef' must be a data frame with these 5 columns:",
            "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
            "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
          ))
        }
      }
    }

    # Runs TADA_FlagDepthCategory if not already ran
    # if (!"DepthCategory" %in% names(.data)) {
    #   .data <- TADA_FlagDepthCategory(.data)
    # }

    usesRef <- dplyr::filter(usesRef, IncludeOrExclude == "Include")

    # Identify all unique monitoring location id in the .data data frame to filter by.
    unique_ML <- unique(.data$MonitoringLocationIdentifier)

    # set a limit of 1k if we want to display all sites-param-use combinations.
    if (
      displayNA == TRUE &&
        nrow(usesRef) * length(unique_ML) > 1000 |
        length(org_id) > 20
    ) {
      warning(paste0(
        "TADA_MLSummary: displayNA = TRUE was selected: ",
        "Too many sites or uses and parameters. Cannot assign all uses and parameters to each monitoring sites in the output. ",
        "Defaulting to displayNA = FALSE"
      ))

      displayNA <- FALSE
    }

    if (displayNA == TRUE && nrow(usesRef) * length(unique_ML) < 1000) {
      print(paste0(
        "TADA_MLSummary: displayNA = TRUE: ",
        "This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
      ))

      # Applies all unique combos of param and uses to each monitoring location.
      CreateMLSummaryRef <- usesRef |>
        tidyr::uncount(weights = length(unique_ML))

      CreateMLSummaryRef <- CreateMLSummaryRef |>
        dplyr::mutate(
          MonitoringLocationIdentifier = as.character(rep(
            unique_ML,
            nrow(CreateMLSummaryRef) / length(unique_ML)
          ))
        ) |>
        dplyr::full_join(
          .data,
          by = c("MonitoringLocationIdentifier"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = NA) |>
        dplyr::mutate(ATTAINS.WaterType = NA) |>
        dplyr::mutate(SaltFresh = NA) |>
        dplyr::mutate(UniqueSpatialCriteria = NA) |>
        dplyr::mutate(IncludeOrExclude = "Include") |>
        dplyr::mutate(DepthCategory = NA) |>
        # dplyr::mutate(Flag.AssessmentNote = "Default: No spatial criteria applied.") |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier,
          MonitoringLocationTypeName,
          TADA.ComparableDataIdentifier = TADA.ComparableDataIdentifier.x,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          ATTAINS.WaterType,
          SaltFresh,
          DepthCategory,
          LongitudeMeasure,
          LatitudeMeasure,
          IncludeOrExclude,
          UniqueSpatialCriteria
        ) |>
        dplyr::distinct()

      # data frame to only display sites that contains the parameter
      CreateMLSummaryRef2 <- usesRef |>
        tidyr::uncount(weights = length(unique_ML)) |>
        dplyr::full_join(
          .data,
          by = c("TADA.ComparableDataIdentifier"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = NA) |>
        dplyr::mutate(ATTAINS.WaterType = NA) |>
        dplyr::mutate(SaltFresh = NA) |>
        dplyr::mutate(UniqueSpatialCriteria = NA) |>
        dplyr::mutate(IncludeOrExclude = "Include") |>
        dplyr::mutate(DepthCategory = NA) |>
        dplyr::mutate(
          TADA.ParameterInSite.Flag = "Pass: This ML contains the parameter in your TADA data frame."
        ) |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier,
          MonitoringLocationTypeName,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          ATTAINS.WaterType,
          SaltFresh,
          DepthCategory,
          LongitudeMeasure,
          LatitudeMeasure,
          TADA.ParameterInSite.Flag,
          IncludeOrExclude,
          UniqueSpatialCriteria
        ) |>
        dplyr::distinct()

      # joins the table back together and flag appropriately
      CreateMLSummaryRef <- CreateMLSummaryRef |>
        # dplyr::bind_rows(CreateMLSummaryRef2)
        dplyr::left_join(CreateMLSummaryRef2) |>
        dplyr::mutate(
          TADA.ParameterInSite.Flag = dplyr::if_else(
            is.na(TADA.ParameterInSite.Flag),
            "Suspect: This ML site does not contain information for this parameter in your TADA data frame.",
            "Pass: This ML contains the parameter in your TADA data frame."
          )
        ) |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier,
          MonitoringLocationTypeName,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          ATTAINS.WaterType,
          SaltFresh,
          DepthCategory,
          LongitudeMeasure,
          LatitudeMeasure,
          TADA.ParameterInSite.Flag,
          IncludeOrExclude,
          UniqueSpatialCriteria
        ) |>
        dplyr::arrange(MonitoringLocationIdentifier)
    }

    # If we want to exclude rows of sites with no specified parameters
    if (displayNA == FALSE) {
      print(paste0(
        "displayNA = FALSE: ",
        "This MLSummaryRef table will only display parameters and uses for a ML if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
      ))

      CreateMLSummaryRef2 <- usesRef |>
        dplyr::full_join(
          .data,
          by = c("TADA.ComparableDataIdentifier"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = NA) |>
        dplyr::mutate(ATTAINS.WaterType = NA) |>
        dplyr::mutate(SaltFresh = NA) |>
        dplyr::mutate(UniqueSpatialCriteria = NA) |>
        dplyr::mutate(IncludeOrExclude = "Include") |>
        dplyr::mutate(DepthCategory = NA) |>
        dplyr::mutate(
          TADA.ParameterInSite.Flag = "Pass: This ML contains the parameter in your TADA data frame."
        ) |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier,
          MonitoringLocationTypeName,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          ATTAINS.WaterType,
          SaltFresh,
          DepthCategory,
          LongitudeMeasure,
          LatitudeMeasure,
          TADA.ParameterInSite.Flag,
          IncludeOrExclude,
          UniqueSpatialCriteria
        ) |>
        dplyr::distinct()

      CreateMLSummaryRef <- CreateMLSummaryRef2 |>
        dplyr::arrange(MonitoringLocationIdentifier)
    }

    # If a user DOES provide an AUMLRef, this will create the Spatial Table on an AU level
    if (!is.null(AUMLRef)) {
      # NOTE: Check for required columns in AUMLRef
      # If a user provides output from TADA_CreateATTAINSAUMLCrosswalk, select only relevant columns
      if ("TADA.MonitoringLocationIdentifier" %in% names(AUMLRef)) {
        AUMLRef <- dplyr::select(
          AUMLRef,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier = TADA.MonitoringLocationIdentifier,
          ATTAINS.WaterType
        )
      }
      if (!"TADA.MonitoringLocationIdentifier" %in% names(AUMLRef)) {
        AUMLRef <- dplyr::select(
          AUMLRef,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier,
          ATTAINS.WaterType
        )
      }

      # If user does not provide an AU_UsesRef, run it to pull in prior uses for AU,
      # Otherwise, if a user has already customized this and provided this AU_UsesRef, then use that table.
      if (is.null(AU_UsesRef)) {
        # Pulls in AU_UsesRef
        print(
          "An AUMLRef was provided, but no AU_UsesRef was provided. Please provide this as an argument input."
        )
        AU_UsesRef <- TADA_AssignUsesToAU(
          .data = .data,
          org_id = org_id,
          AUMLRef = AUMLRef,
          waterUseRef = TADA_AssignUsesToWaterType(
            .data,
            org_id = org_id,
            AUMLRef = AUMLRef
          )
        )
      }

      # Only keep rows that have "Include"
      AU_UsesRef <- AU_UsesRef |>
        dplyr::filter(IncludeOrExclude == "Include") |>
        dplyr::select(-IncludeOrExclude)

      # Identify all unique monitoring location id in the .data data frame to filter by.
      unique_ML <- unique(.data$MonitoringLocationIdentifier)

      # Define the user's defined uses, param, sites and AU crosswalks.
      useParamAUMLRef <- AU_UsesRef |>
        dplyr::left_join(
          AUMLRef,
          by = c(
            "ATTAINS.OrganizationIdentifier",
            "ATTAINS.AssessmentUnitIdentifier",
            "ATTAINS.WaterType"
          )
        ) |>
        dplyr::left_join(
          usesRef,
          by = c("ATTAINS.UseName", "ATTAINS.OrganizationIdentifier")
        ) |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          ATTAINS.WaterType
        )

      # Only join the AU to the CreateMLSummaryRef
      if (displayNA == TRUE) {
        print(paste0(
          "TADA_MLSummary: displayNA = TRUE was selected:",
          "This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
        ))

        CreateMLSummaryRef <- CreateMLSummaryRef |>
          dplyr::left_join(
            useParamAUMLRef,
            by = dplyr::join_by(
              ATTAINS.OrganizationIdentifier,
              MonitoringLocationIdentifier,
              ATTAINS.ParameterName,
              ATTAINS.UseName,
              TADA.ComparableDataIdentifier
            )
          ) |>
          dplyr::select(
            ATTAINS.OrganizationIdentifier,
            ATTAINS.AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier.y,
            MonitoringLocationIdentifier,
            MonitoringLocationTypeName,
            TADA.ComparableDataIdentifier,
            ATTAINS.ParameterName,
            ATTAINS.UseName,
            ATTAINS.WaterType = ATTAINS.WaterType.y,
            SaltFresh,
            DepthCategory,
            DepthCategory,
            LongitudeMeasure,
            LatitudeMeasure,
            TADA.ParameterInSite.Flag,
            IncludeOrExclude,
            UniqueSpatialCriteria
          ) |>
          # dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier)) |>
          dplyr::arrange(
            MonitoringLocationIdentifier,
            ATTAINS.AssessmentUnitIdentifier
          ) |>
          dplyr::distinct()
      }

      # Filters your MLSummaryRef based on your defined uses, param, sites and AU crosswalks.
      if (displayNA == FALSE) {
        print(paste0(
          "TADA_MLSummary: displayNA = FALSE was selected:",
          "This MLSummaryRef table will only display parameters and uses for a ML/AU if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
        ))

        CreateMLSummaryRef <- CreateMLSummaryRef |>
          dplyr::right_join(
            useParamAUMLRef,
            by = dplyr::join_by(
              ATTAINS.OrganizationIdentifier,
              MonitoringLocationIdentifier,
              ATTAINS.ParameterName,
              ATTAINS.UseName,
              TADA.ComparableDataIdentifier
            )
          ) |>
          dplyr::select(
            ATTAINS.OrganizationIdentifier,
            ATTAINS.AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier.y,
            MonitoringLocationIdentifier,
            MonitoringLocationTypeName,
            TADA.ComparableDataIdentifier,
            ATTAINS.ParameterName,
            ATTAINS.UseName,
            ATTAINS.WaterType = ATTAINS.WaterType.y,
            SaltFresh,
            DepthCategory,
            DepthCategory,
            LongitudeMeasure,
            LatitudeMeasure,
            TADA.ParameterInSite.Flag,
            IncludeOrExclude,
            UniqueSpatialCriteria
          ) |>
          dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier)) |>
          dplyr::filter(!is.na(MonitoringLocationIdentifier)) |>
          dplyr::arrange(
            ATTAINS.ParameterName,
            MonitoringLocationIdentifier,
            ATTAINS.AssessmentUnitIdentifier
          ) |>
          dplyr::distinct()
      }
    }

    if (!"ATTAINS.AssessmentUnitIdentifier" %in% colnames(CreateMLSummaryRef)) {
      print(paste0(
        "TADA_MLSummary: No Monitoring Location to Assessment Unit crosswalk provided. ",
        "Consider providing this crosswalk if you would like to summarize WQP data on an Assessment Unit level."
      ))
    }
  }
  # Only run if user wants to create an excel guided spreadsheet.
  if (excel == TRUE) {
    # default Downloads file location.
    # Define the OneDrive Downloads path
    onedrive_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "OneDrive",
      "Downloads",
      "myfileRef.xlsx"
    )

    # Define the default Downloads path
    default_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "Downloads",
      "myfileRef.xlsx"
    )

    # Check if the OneDrive Downloads path exists, and prioritize it
    if (file.exists(onedrive_downloads_path)) {
      downloads_path <- onedrive_downloads_path
    } else {
      downloads_path <- default_downloads_path
    }

    # if a user generates a blank template, the prior blank template must also be generated in excel
    if (missing(.data)) {
      suppressMessages(TADA_ParametersForAnalysis(
        excel = excel,
        overwrite = overwrite
      ))

      suppressMessages(TADA_UsesForAnalysis(
        excel = excel,
        overwrite = overwrite
      ))
    }

    wb <- openxlsx::loadWorkbook(wb, downloads_path)

    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateMLSummaryRef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateMLSummaryRef")
        openxlsx::addWorksheet(wb, "CreateMLSummaryRef")
      }
    )

    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")

    # Format Column widths
    openxlsx::setColWidths(
      wb,
      "CreateMLSummaryRef",
      cols = 8:ncol(CreateMLSummaryRef),
      widths = "auto"
    )

    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }

    # writes CreateMLSummaryRef dataframe
    openxlsx::writeData(
      wb,
      "CreateMLSummaryRef",
      startCol = 1,
      x = CreateMLSummaryRef,
      headerStyle = header_st
    )

    # data validation drop down list created below.
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "CreateMLSummaryRef",
      cols = 9,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index'!$B$2:$B$5"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    # Conditional Formatting
    openxlsx::conditionalFormatting(
      wb,
      "CreateMLSummaryRef",
      cols = 16,
      rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "contains",
      rule = "Include",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # default values or indicates good to go cells.

    openxlsx::conditionalFormatting(
      wb,
      "CreateMLSummaryRef",
      cols = 16,
      rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "contains",
      rule = "Exclude",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    # conditionalFormatting(wb, "CreateMLSummaryRef",
    #                       cols = 8, rows = 2:(nrow(CreateMLSummaryRef) + 1),
    #                       type = "notContains", rule = c("Exclude","Include"), style = createStyle(bgFill = "red")) # Likely error. Invalid value is possible here.
    openxlsx::conditionalFormatting(
      wb,
      "CreateMLSummaryRef",
      cols = 17,
      rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # green is default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb,
      "CreateMLSummaryRef",
      cols = 17,
      rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell

    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }

    if (overwrite == FALSE) {
      warning(
        "If you would like to replace the file, use overwrite = TRUE argument in TADA_ParametersForAnalysis"
      )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }

    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")

    CreateMLSummaryRef <- openxlsx::read.xlsx(
      downloads_path,
      sheet = "CreateMLSummaryRef"
    )
  }
  return(CreateMLSummaryRef)
}
