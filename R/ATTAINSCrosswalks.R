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
#' supplied by the user. More than one org_id may be provided. A list of
#' organization identifiers can be found by downloading the ATTAINS Domains
#' Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param. When
#' org_id = NULL, all assessment unit/monitoring locations matches recorded in
#' ATTAINS from all organizations will be returned.
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
#' # Arizona example, returns blank dataframe as of 1/21/25
#' AZ_crosswalk <- TADA_GetATTAINSAUMLCrosswalk(org_id = "21ARIZ")
#' }
#'
TADA_GetATTAINSAUMLCrosswalk <- function(org_id = NULL,
                                         batch_upload = FALSE) {
  # get reference df of all organization ids
  org.ref <- TADA_GetATTAINSOrgIDsRef()

  # check to see if org_id is not NULL
  if (!is.null(org_id)) {
    # check to make sure organization ids supplied by user match those in ATTAINS
    if (all(!org_id %in% org.ref$code)) {
      # remove intermediate objects
      rm(org.ref)

      # stop function if organization ids are not in ATTAINS
      stop(paste0(
        "TADA_GetATTAINSAUMLCrosswalk: ",
        "organization identifier(s) entered by user not found in ATTAINS."
      ))
    }
  }

  # if org_id is NULL return the AU/ML national extract, otherwise query by org_id
  if (is.null(org_id)) {
    au.info <- spsUtil::quiet(rExpertQuery::EQ_NationalExtract("au_mls"))
  } else {
    au.info <- spsUtil::quiet(rExpertQuery::EQ_AUsMLs(org_id = org_id, api_key = "lfzVzpwIlKS1O4l1QmbOLUeTzxyql4QdbHVR5Yf5"))
  }

  # select, filter and rename crosswalk columns
  au.crosswalk <- au.info %>%
    dplyr::select(
      monitoringLocationId, monitoringLocationOrgId,
      assessmentUnitId, monitoringLocationDataLink,
      waterType, organizationId
    ) %>%
    dplyr::filter(
      !is.na(monitoringLocationId),
      monitoringLocationId != ""
    ) %>%
    dplyr::distinct() %>%
    dplyr::rename(
      ATTAINS.AssessmentUnitIdentifier = assessmentUnitId,
      MonitoringLocationIdentifier = monitoringLocationId,
      OrganizationIdentifier = monitoringLocationOrgId,
      MonitoringDataLinkText = monitoringLocationDataLink,
      ATTAINS.WaterType = waterType,
      ATTAINS.OrganizationIdentifier = organizationId
    ) %>%
    dplyr::rename(
      ATTAINS.MonitoringLocationIdentifier = MonitoringLocationIdentifier,
      ATTAINS.MonitoringDataLinkText = MonitoringDataLinkText
    ) %>%
    dplyr::select(
      OrganizationIdentifier, ATTAINS.OrganizationIdentifier,
      ATTAINS.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
      ATTAINS.MonitoringDataLinkText, ATTAINS.WaterType
    )

  # remove intermediate object
  rm(au.info)

  # if org_id is NULL, set to "all organizations" for printed message
  if (is.null(org_id)) {
    org_id <- "all organizations"
  }

  # check to see if the crosswalk contains any results
  if (length(au.crosswalk$ATTAINS.MonitoringLocationIdentifier > 0)) {
    # print a message describing the number of results
    print(paste0(
      "TADA_GetATTAINSAUMLCrosswalk: ",
      "There are ", nrow(au.crosswalk),
      " monitoring location identifiers associated with assessment units for ",
      org_id, " in ATTAINS."
    ))

    # if batch_upload is TRUE, create an ATTAINS formatted batch upload df
    if (batch_upload == TRUE) {
      au.crosswalk <- au.crosswalk %>%
        dplyr::select(-ATTAINS.WaterType) %>%
        dplyr::select(-ATTAINS.OrganizationIdentifier) %>%
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
        org_id, " assessment units."
      ))

      rm(org.ref)
    }

    return(au.crosswalk)
  }
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
#' @param org_id Character argument. The ATTAINS organization identifier must
#' be supplied by the user. A list of organization identifiers can be found by
#' downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param.
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
#' @param crosswalk A user-supplied dataframe with the columns
#' ATTAINS.MonitoringLocationIdentifier, OrganizationIdentifier, ATTAINS.WaterType,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.MonitoringDataLinkText
#' is required. The ATTAINS.AssessmentUnitIdentifier and
#' ATTAINS.MonitoringLocationIdentifier must be filled out in order to use this
#' function. The additional columns, ATTAINS.MonitoringDataLinkText,
#' containing a single URL or "; " separated URLs linking to information about
#' the monitoring location, and OrganizationIdentifier, containing the WQP
#' organization identifier for the monitoring location can
#' be left blank and the function will still run. Data link URLS to WQP site
#' pages cannot be automatically generated by this function unless the
#' OrganizationIdentifier column is populated with the WQP OrganizationIdentifier.
#' When crosswalk = NULL, the crosswalk will be downloaded from ATTAINS. This allows
#' users to add URLs for the Water Quality Portal data site pages to the ATTAINS
#' assessment unit profile where possible without updating other information
#' in ATTAINS.
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
TADA_UpdateATTAINSAUMLCrosswalk <- function(org_id = NULL,
                                            crosswalk = NULL,
                                            attains_replace = FALSE,
                                            wqp_data_links = "add",
                                            update_mlid = TRUE,
                                            batch_upload = FALSE,
                                            check_links = FALSE) {
  # get list of organization identifiers from ATTAINS
  org.ref <- utils::read.csv(system.file("extdata", "ATTAINSOrgIDsRef.csv",
    package = "EPATADA"
  ))

  # stop function if organization identifiers is not found in ATTAINS
  if (!org_id %in% org.ref$code) {
    stop(paste0(
      "TADA_UpdateATTAINSAUMLCrosswalk: ",
      "The organization identifier entered by user is not found in ATTAINS."
    ))
  }

  if (is.null(crosswalk) & attains_replace == TRUE) {
    stop(paste0(
      "TADA_UpdateATTAINSAUMLCrosswalk: ",
      "in order to replace MonitoringLocations stored in ATTAINS ",
      "(with attains_replace = TRUE), user must provide a ",
      "MonitoringLocation/AssessmentUnitcrosswalk."
    ))
  }

  if (org_id %in% org.ref$code) {
    # remove intermediate object
    rm(org.ref)

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
      user_cols <- c(
        "ATTAINS.AssessmentUnitIdentifier",
        "ATTAINS.MonitoringLocationIdentifier",
        "OrganizationIdentifier",
        "ATTAINS.OrganizationIdentifier",
        "ATTAINS.MonitoringDataLinkText",
        "ATTAINS.WaterType"
      )

      batch_cols <- c(
        "ASSESSMENT_UNIT_ID",
        "MS_LOCATION_ID",
        "MS_ORG_ID",
        "MS_DATA_LINK"
      )

      if (!all(user_cols %in% names(crosswalk)) &
        !all(batch_cols %in% names(crosswalk))) {
        stop(paste0(
          "Column names must reflect either the TADA workflow or the ATTAINS ",
          "batch upload requirements. Review function documentation for more information"
        ))
      }

      if (all(batch_cols %in% names(crosswalk))) {
        crosswalk <- crosswalk %>%
          dplyr::rename(
            ATTAINS.AssessmentUnitIdentifier = ASSESSMENT_UNIT_ID,
            ATTAINS.MonitoringLocationIdentifier = MS_LOCATION_ID,
            OrganizationIdentifier = MS_ORG_ID,
            ATTAINS.MonitoringDataLinkText = MS_DATA_LINK
          ) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            ATTAINS.OrganizationIdentifier = org_id,
            ATTAINS.WaterType =
              ifelse(
                "ATTAINS.WaterType" %in% names(.), ATTAINS.WaterType,
                NA_character_
              )
          )
      }
    }

    if (attains_replace == FALSE) {
      # create assessment unit crosswalk from ATTAINS
      attains.crosswalk <- suppressMessages(TADA_GetATTAINSAUMLCrosswalk(org_id = org_id))

      if (is.null(crosswalk)) {
        update.crosswalk <- attains.crosswalk


        rm(attains.crosswalk)
      }

      if (!is.null(crosswalk)) {
        # combine user supplied and attains crosswalks to create one crosswalk
        # no rows are omitted
        update.crosswalk <- attains.crosswalk %>%
          dplyr::full_join(crosswalk, by = dplyr::join_by(
            ATTAINS.MonitoringLocationIdentifier,
            OrganizationIdentifier,
            ATTAINS.OrganizationIdentifier,
            ATTAINS.AssessmentUnitIdentifier,
            ATTAINS.MonitoringDataLinkText,
            ATTAINS.WaterType
          )) %>%
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

    if (wqp_data_links == "add" | wqp_data_links == "replace" |
      update_mlid == TRUE) {
      provider.ref <- TADA_GetWQPOrgProviderRef() %>%
        dplyr::select(OrganizationIdentifier, ProviderName) %>%
        dplyr::distinct() %>%
        dplyr::mutate(OrgIDForURL = OrganizationIdentifier)
    }

    # internal function to update monitoring location identifiers
    updateMonLocIds <- function(.data) {
      # add additional rows to account for the addition of "_WQX" to many org
      # names for WQP data
      add.orgs <- provider.ref %>%
        dplyr::filter(
          ProviderName == "STORET",
          grepl("_WQX", OrganizationIdentifier)
        ) %>%
        dplyr::mutate(OrganizationIdentifier = stringr::str_remove_all(
          OrgIDForURL,
          "_WQX"
        ))

      # combine provider refs
      provider.ref <- provider.ref %>%
        dplyr::bind_rows(add.orgs)

      # remove intermediate object
      rm(add.orgs)

      # join provider ref df to crosswalk
      update.crosswalk <- .data %>%
        dplyr::left_join(provider.ref, by = dplyr::join_by(OrganizationIdentifier))

      # build the updated mls for storet results
      update.crosswalk.storet <- update.crosswalk %>%
        dplyr::filter(ProviderName == "STORET") %>%
        dplyr::mutate(
          ATTAINS.MonitoringLocationIdentifier =
            stringr::str_remove(
              ATTAINS.MonitoringLocationIdentifier,
              paste0(OrganizationIdentifier, "-")
            ),
          ATTAINS.MonitoringLocationIdentifier =
            stringr::str_remove(
              ATTAINS.MonitoringLocationIdentifier,
              OrganizationIdentifier
            ),
          ATTAINS.MonitoringLocationIdentifier = stringr::str_remove(ATTAINS.MonitoringLocationIdentifier, "_WQX"),
          ATTAINS.MonitoringLocationIdentifier = paste0(
            OrganizationIdentifier, "-",
            ATTAINS.MonitoringLocationIdentifier
          )
        )

      # join nwis and storet crosswalks
      update.crosswalk <- update.crosswalk %>%
        dplyr::filter(!ProviderName %in% c("STORET")) %>%
        dplyr::bind_rows(update.crosswalk.storet)

      rm(update.crosswalk.storet, provider.ref)

      return(update.crosswalk)
    }

    # internal function to create new urls for monitoring locations
    createNewMLUrls <- function(.data) {
      if (!"ProviderName" %in% names(.data)) {
        .data <- .data %>%
          dplyr::left_join(provider.ref,
            by = dplyr::join_by(OrganizationIdentifier)
          )
      }

      new.urls <- .data %>%
        # dplyr::filter(ProviderName == "STORET") %>%
        dplyr::mutate(ATTAINS.MonitoringDataLinkText.New = as.character(ifelse(
          is.na(OrgIDForURL), NA,
          URLencode(paste0(
            "https://www.waterqualitydata.us/provider/", ProviderName,
            "/", OrgIDForURL, "/", ATTAINS.MonitoringLocationIdentifier, "/"
          ))
        ))) %>%
        dplyr::select(-OrgIDForURL)

      return(new.urls)
    }

    if (check_links == TRUE) {
      # internal function to check urls
      checkUrlResp <- function(.data, url.col) {
        # create df of urls to check
        urls.to.check <- .data %>%
          dplyr::filter(!is.na(!!rlang::sym(url.col)))

        # check to see if any urls to check
        if (dim(urls.to.check)[1] == 0) {
          .data <- .data %>%
            dplyr::mutate(response.code = "none")

          rm(urls.to.check)

          return(.data)
        }

        if (dim(urls.to.check)[1] > 0) {
          # retrieve http response headers from url list
          headers <- urls.to.check %>%
            dplyr::select(!!rlang::sym(url.col)) %>%
            dplyr::pull() %>%
            purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))

          # extract response code from first line of header response
          response.code <- sapply(headers, "[[", 1)

          # create dataframe of urls and response codes
          response.df <- data.frame(urls.to.check, response.code) %>%
            dplyr::distinct()

          # join response codes to add.urls df
          .data <- .data %>%
            dplyr::left_join(response.df, by = names(update.crosswalk))

          rm(urls.to.check, headers, response.code, response.df)

          return(.data)
        }
      }
    }

    if (update_mlid == TRUE & wqp_data_links == "none") {
      update.crosswalk <- updateMonLocIds(update.crosswalk)

      if (check_links == TRUE) {
        update.crosswalk <- checkUrlResp(update.crosswalk,
          url.col = "ATTAINS.MonitoringDataLinkText"
        )

        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText = ifelse(stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New, NA
          ))
      }
    }

    if (update_mlid == TRUE & wqp_data_links == "replace") {
      update.crosswalk <- updateMonLocIds(update.crosswalk)

      update.crosswalk <- createNewMLUrls(update.crosswalk) %>%
        dplyr::select(-ATTAINS.MonitoringDataLinkText) %>%
        dplyr::rename(ATTAINS.MonitoringDataLinkText = ATTAINS.MonitoringDataLinkText.New)

      if (check_links == TRUE) {
        update.crosswalk <- checkUrlResp(update.crosswalk,
          url.col = "ATTAINS.MonitoringDataLinkText"
        )

        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText = ifelse(stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          ))
      }
    }

    if (update_mlid == TRUE & wqp_data_links == "add") {
      update.crosswalk <- updateMonLocIds(update.crosswalk)

      update.crosswalk <- createNewMLUrls(update.crosswalk)

      if (check_links == TRUE) {
        update.crosswalk <- checkUrlResp(update.crosswalk,
          url.col = "ATTAINS.MonitoringDataLinkText"
        )

        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText = ifelse(stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )) %>%
          dplyr::select(-response.code)

        update.crosswalk <- checkUrlResp(update.crosswalk,
          url.col = "ATTAINS.MonitoringDataLinkText.New"
        )

        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText.New = ifelse(stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )) %>%
          dplyr::select(-response.code)
      }

      update.crosswalk <- update.crosswalk %>%
        dplyr::mutate(ATTAINS.MonitoringDataLinkText = dplyr::case_when(
          !is.na(ATTAINS.MonitoringDataLinkText) & !is.na(ATTAINS.MonitoringDataLinkText.New) ~
            paste0(
              ATTAINS.MonitoringDataLinkText, "; ",
              ATTAINS.MonitoringDataLinkText.New
            ),
          is.na(ATTAINS.MonitoringDataLinkText) & !is.na(ATTAINS.MonitoringDataLinkText.New) ~ ATTAINS.MonitoringDataLinkText.New,
          !is.na(ATTAINS.MonitoringDataLinkText) & is.na(ATTAINS.MonitoringDataLinkText.New) ~ ATTAINS.MonitoringDataLinkText,
          is.na(ATTAINS.MonitoringDataLinkText) & is.na(ATTAINS.MonitoringDataLinkText.New) ~ NA
        )) %>%
        dplyr::select(-ATTAINS.MonitoringDataLinkText.New)
    }

    if (update_mlid == FALSE & wqp_data_links == "none") {
      update.crosswalk <- update.crosswalk
    }

    if (update_mlid == FALSE & wqp_data_links == "replace") {
      update.crosswalk <- update.crosswalk %>%
        dplyr::mutate(OLD_ATTAINS.MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier) %>%
        updateMonLocIds()

      update.crosswalk <- createNewMLUrls(update.crosswalk) %>%
        dplyr::select(-ATTAINS.MonitoringDataLinkText) %>%
        dplyr::rename(ATTAINS.MonitoringDataLinkText = ATTAINS.MonitoringDataLinkText.New) %>%
        dplyr::select(-ATTAINS.MonitoringLocationIdentifier, -OrgIDForURL) %>%
        dplyr::rename(ATTAINS.MonitoringLocationIdentifier = OLD_ATTAINS.MonitoringLocationIdentifier)
    }

    if (update_mlid == FALSE & wqp_data_links == "add") {
      update.crosswalk <- update.crosswalk %>%
        dplyr::mutate(OLD_ATTAINS.MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier) %>%
        updateMonLocIds()

      update.crosswalk <- createNewMLUrls(update.crosswalk)

      if (check_links == TRUE) {
        update.crosswalk <- checkUrlResp(update.crosswalk,
          url.col = "ATTAINS.MonitoringDataLinkText"
        )

        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText = ifelse(stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )) %>%
          dplyr::select(-response.code)

        update.crosswalk <- checkUrlResp(update.crosswalk,
          url.col = "ATTAINS.MonitoringDataLinkText.New"
        )

        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText.New = ifelse(stringr::str_detect(response.code, "200"),
            ATTAINS.MonitoringDataLinkText.New,
            NA
          )) %>%
          dplyr::select(-response.code)

        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText = dplyr::case_when(
            !is.na(ATTAINS.MonitoringDataLinkText) & !is.na(ATTAINS.MonitoringDataLinkText.New) ~
              paste0(
                ATTAINS.MonitoringDataLinkText, "; ",
                ATTAINS.MonitoringDataLinkText.New
              ),
            is.na(ATTAINS.MonitoringDataLinkText) & !is.na(ATTAINS.MonitoringDataLinkText.New) ~ ATTAINS.MonitoringDataLinkText.New,
            !is.na(ATTAINS.MonitoringDataLinkText) & is.na(ATTAINS.MonitoringDataLinkText.New) ~ ATTAINS.MonitoringDataLinkText,
            is.na(ATTAINS.MonitoringDataLinkText) & is.na(ATTAINS.MonitoringDataLinkText.New) ~ NA
          )) %>%
          dplyr::select(-ATTAINS.MonitoringDataLinkText.New, -ATTAINS.MonitoringLocationIdentifier) %>%
          dplyr::rename(ATTAINS.MonitoringLocationIdentifier = OLD_ATTAINS.MonitoringLocationIdentifier)
      }

      if (check_links == FALSE) {
        update.crosswalk <- update.crosswalk %>%
          dplyr::mutate(ATTAINS.MonitoringDataLinkText = dplyr::case_when(
            !is.na(ATTAINS.MonitoringDataLinkText) & !is.na(ATTAINS.MonitoringDataLinkText.New) ~
              paste0(
                ATTAINS.MonitoringDataLinkText, "; ",
                ATTAINS.MonitoringDataLinkText.New
              ),
            is.na(ATTAINS.MonitoringDataLinkText) & !is.na(ATTAINS.MonitoringDataLinkText.New) ~ ATTAINS.MonitoringDataLinkText.New,
            !is.na(ATTAINS.MonitoringDataLinkText) & is.na(ATTAINS.MonitoringDataLinkText.New) ~ ATTAINS.MonitoringDataLinkText,
            is.na(ATTAINS.MonitoringDataLinkText) & is.na(ATTAINS.MonitoringDataLinkText.New) ~ NA
          )) %>%
          dplyr::select(-ATTAINS.MonitoringDataLinkText.New, -ATTAINS.MonitoringLocationIdentifier) %>%
          dplyr::rename(ATTAINS.MonitoringLocationIdentifier = OLD_ATTAINS.MonitoringLocationIdentifier)
      }
    }

    # select relevant column names and ordering for output in TADA workflow format.
    update.crosswalk <- update.crosswalk %>%
      dplyr::select(
        OrganizationIdentifier, ATTAINS.OrganizationIdentifier,
        ATTAINS.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
        ATTAINS.MonitoringDataLinkText, ATTAINS.WaterType
      )

    # If batch upload is desired, format the output in the required format.
    if (batch_upload == TRUE) {
      update.crosswalk <- update.crosswalk %>%
        dplyr::select(-c(ATTAINS.WaterType, ATTAINS.OrganizationIdentifier)) %>%
        dplyr::rename(
          ASSESSMENT_UNIT_ID = ATTAINS.AssessmentUnitIdentifier,
          MS_ORG_ID = ATTAINS.MonitoringLocationIdentifier,
          MS_LOCATION_ID = OrganizationIdentifier,
          MS_DATA_LINK = ATTAINS.MonitoringDataLinkText
        )
    }

    return(update.crosswalk)
  }
}

#' Create or Update ATTAINS, TADA/WQP/WQX, and EPA Criteria Search Tool (CST)
#' Parameter Name Crosswalk
#'
#' Use this function to help generate a crosswalk between each
#' ATTAINS.ParameterName used by a specific state or tribal nation and each
#' TADA.ComparableDataIdentifier present in the input TADA dataframe. The
#' crosswalk can be filled out by users within R or Excel. By default this
#' function will generate a user friendly Excel spreadsheet that includes a
#' drop down list list of all ATTAINS parameters that are applicable to the
#' organization selected by the function input 'org_id'. It also
#' highlights the cells in which users should input information. The excel
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
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName are specific and
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
#' 'CSTtoATTAINSParamCrosswalk <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))'.
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
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this parameter. If a user supplied crosswalk is entered
#' into paramRef AND a user does not provide an org_id argument,
#' the function can identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If needed, please type the following into your R console:
#' file.path(Sys.getenv("USERPROFILE"), "Downloads") to ensure the file is downloaded
#' to the correct location. The file will be named "myfileRef.xlsx".
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param paramRef A dataframe which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier and ATTAINS.ParameterName. Users will need to
#' ensure this crosswalk contains the appropriate column names in order to
#' run the function. paramRef must contain at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName. Users who are
#' interested in performing analyses for more than
#' one organization (multiple states or tribes, or a single state/tribe and
#' EPA 304a criteria) also need to include an additional column name:
#' 'organization_identifier'.
#'
#' @param auto_assign A string value of "None", "All", or "Org". Default is "All".
#' If a user selects "All" this provides a match using TADA logic (IN DEVELOPMENT:
#' currently based on and exact match of WQP CharacteristicName with
#' ATTAINS ParameterName along with a few manual review). If "Org" then this
#' only provide the TADA logic match if your ATTAINS organization has included that
#' ATTAINS ParameterName in the past. If not, this will be left blank for your
#' organization to specify. "None" will result in an empty ATTAINS.ParameterName
#' column. Users will be required to fill this out on their own completely or
#' through a prior paramRef crosswalk. See paramRef argument input above for more
#' information.
#'
#' @return A excel file or data frame which contains the columns:
#' TADA.ComparableDataIdentifier, organization_identifier,
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
#' # TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = TRUE)
#' # Example below generates the same output as a dataframe
#' paramRef_UT <- TADA_CreateParamRef(
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
#' paramRef_UT3 <- TADA_CreateParamRef(Data_Nutrients_UT,
#'   paramRef = paramRef_UT2,
#'   org_id = "UTAHDWQ", auto_assign = "None", excel = FALSE
#' )
#'
#' # How does auto_assign = "All" compare to paramRef_UT3?
#' paramRef_UT4 <- TADA_CreateParamRef(Data_Nutrients_UT,
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
#' paramRef_shepherdstown <- TADA_CreateParamRef(shepherdstown3,
#'   org_id = c("MDE_EASP", "21VASWCB", "21PA"),
#'   auto_assign = "All",
#'   excel = FALSE
#' )
#' }
#'
TADA_CreateParamRef <- function(.data, org_id = NULL, paramRef = NULL, auto_assign = c("None", "All", "Org"),
                                excel = FALSE, overwrite = FALSE) {
  auto_assign <- match.arg(auto_assign)

  # Return an empty dataframe with column names only if a user does not define any arg inputs.
  if (missing(.data) && missing(org_id) && missing(auto_assign) && missing(excel) && missing(overwrite)) {
    message("All arguments are blank, returning an empty dataframe with column names only.")

    empty_df <- data.frame(
      TADA.ComparableDataIdentifier = character(0),
      ATTAINS.OrganizationIdentifier = character(0),
      ATTAINS.ParameterName = character(0),
      ATTAINS.FlagParameterName = character(0),
      Flag.ParameterInput = character(0)
    )

    return(empty_df)
  } else {
    # overwrite argument should only be used when creating an excel file.
    if (excel == FALSE && overwrite == TRUE) {
      stop(paste0(
        "TADA_CreateParamRef: ",
        "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
        "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
      ))
    }

    # Ensures you have used a valid auto_assign name
    if (!auto_assign %in% c("None", "All", "Org")) {
      stop(paste0(
        "TADA_CreateParamRef: ",
        "argument input ", auto_assign, " is not a valid entry. Please type one of 'None', 'All', 'Org' as a value."
      ))
    }

    # if a user provides an org_id argument, it must be a character vector.
    if (!is.character(org_id) & is.null(org_id)) {
      stop("TADA.CreateParamRef: org_id must be a character vector")
    }

    # Allows for users to crosswalk parameters by multiple orgs.
    org_id <- as.list(org_id)

    # If  more than 1 org, it will create n duplicate rows for each TADA.ComparableDataIdentifier.
    if (length(org_id) > 1) {
      print(paste0(
        "TADA.CreateParamRef: More than one org_name was defined in your dataframe. ",
        "Generating duplicate rows of TADA.ComparableDataIdentifier for each org."
      ))
    }

    # Checks if your org is found in ATTAINS domain.
    org.ref <- utils::read.csv(system.file("extdata", "ATTAINSOrgIDsRef.csv", package = "EPATADA"))

    if (!sum(org_id %in% org.ref$code) == length(org_id)) {
      warning(paste0(
        "TADA_CreateParamRef: ",
        "One or more organization identifier(s) entered by user is not found in ATTAINS."
      ))
    }

    # check to see if user-supplied parameter ref is a df with appropriate columns
    if (!is.null(paramRef) & !is.character(paramRef)) {
      if (!is.data.frame(paramRef)) {
        stop(paste0(
          "TADA_CreateParamRef: 'paramRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
        ))
      }

      if (is.data.frame(paramRef)) {
        col.names <- c(
          "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName"
        )

        ref.names <- names(paramRef)

        # Users are required to provide a parameter ref that contains
        # TADA.ComparableDataIdentifier and ATTAINS.ParameterName
        if (length(setdiff(col.names, ref.names)) > 0 &&
          !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
          stop(paste0(
            "TADA_CreateParamRef: 'paramRef' must be a data frame with these 2 columns:",
            "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
          ))
        }
      }
    }

    # If users don't provide TADA.ComparableDataIdentifier in their paramRef input,
    # crosswalk using TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText
    if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
      paramRef <- paramRef %>%
        dplyr::left_join(
          .data, c(
            "TADA.CharacteristicName", "TADA.MethodSpeciationName",
            "TADA.ResultSampleFractionText"
          )
        ) %>%
        dplyr::select(
          "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName", "ATTAINS.FlagParameterName"
        )
    }

    # 304a parameter name and standards are pulled in from the Criteria Search Tool (CST)
    # CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA"))

    # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
    TADA_param <- dplyr::distinct(
      .data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]
    ) %>%
      tidyr::uncount(weights = length(org_id)) %>%
      dplyr::mutate(ATTAINS.OrganizationIdentifier = as.character(rep(org_id, nrow(.) / length(org_id))))

    # Pulls in all domain values of parameter and use names in ATTAINS.
    ATTAINS_param_all <- utils::read.csv(
      system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA")
    )

    # Filters the full domain value by the specified org_id(s)
    ATTAINS_param <- ATTAINS_param_all %>%
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) %>%
      dplyr::arrange(ATTAINS.ParameterName)

    # Should we stop or warn users in this step?
    if (sum(!org_id %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
      warning(paste0(
        "TADA_CreateParamRef: ",
        "One or more organization identifiers entered by user is not found in ATTAINS."
      ))
    }

    # If no paramRef is provided, the ATTAINS.ParameterName returns a blank column of NA that will need user input.
    if (tolower(auto_assign) == tolower("None")) {
      CreateParamRef <- TADA_param %>%
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) %>%
        dplyr::select(
          TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) %>%
        dplyr::arrange(ATTAINS.OrganizationIdentifier) %>%
        dplyr::mutate(
          ATTAINS.ParameterName = as.character(NA),
          ATTAINS.FlagParameterName = "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment"
        ) %>%
        dplyr::mutate(
          Flag.ParameterInput =
            "Default. No Crosswalk was provided."
        ) %>%
        dplyr::distinct()
    }

    if (tolower(auto_assign) == tolower("All")) {
      print(paste0(
        "auto_assign == 'All' was selected, ",
        "finding an exact ATTAINS.ParameterName match for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
      ))
      ATTAINSParameterWQPCharRef <- utils::read.csv(system.file("extdata", "ATTAINSParamToWQPCharRef.csv", package = "EPATADA"))

      ATTAINSParameterWQPCharRef <- ATTAINSParameterWQPCharRef %>%
        dplyr::filter(ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName)

      CreateParamRef <- TADA_param %>%
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) %>%
        dplyr::select(
          TADA.CharacteristicName, TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) %>%
        dplyr::left_join(ATTAINSParameterWQPCharRef, by = c("TADA.CharacteristicName" = "CharacteristicName")) %>%
        dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) %>%
        dplyr::select(
          TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) %>%
        dplyr::arrange(ATTAINS.OrganizationIdentifier) %>%
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "No parameter match for TADA.ComparableDataIdentifier" | is.na(ATTAINS.ParameterName) ~
              "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName ~
              "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
            ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName & !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS for this organization."
          )
        ) %>%
        dplyr::mutate(
          Flag.ParameterInput = dplyr::if_else(
            !is.na(ATTAINS.ParameterName),
            "This crosswalk was provided through an exact match auto_assign = 'All', between ATTAINS.ParameterName and TADA.CharacteristicName.",
            "No Crosswalk was provided and no exact matches were found."
          )
        ) %>%
        dplyr::distinct()
    }

    if (tolower(auto_assign) == tolower("Org")) {
      print(paste0(
        "auto_assign == 'Org' was selected, ",
        "finding an exact ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
      ))

      ATTAINSParameterWQPCharRef <- utils::read.csv(system.file("extdata", "ATTAINSParamToWQPCharRef.csv", package = "EPATADA"))

      ATTAINSParameterWQPCharRef <- ATTAINSParameterWQPCharRef %>%
        dplyr::filter(ATTAINS.ParameterName %in% ATTAINS_param$ATTAINS.ParameterName)

      CreateParamRef <- TADA_param %>%
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) %>%
        dplyr::select(
          TADA.CharacteristicName, TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) %>%
        dplyr::left_join(ATTAINSParameterWQPCharRef, by = c("TADA.CharacteristicName" = "CharacteristicName")) %>%
        dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) %>%
        dplyr::select(
          TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName # , EPA304A.PollutantName
        ) %>%
        dplyr::arrange(ATTAINS.OrganizationIdentifier) %>%
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "No parameter match for TADA.ComparableDataIdentifier" | is.na(ATTAINS.ParameterName) ~
              "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName ~
              "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
            ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName & !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS for this organization."
          )
        ) %>%
        # since auto_assign = Org matches only, then we must flag the parameter name, then only keep if it is a match
        dplyr::mutate(ATTAINS.ParameterName = dplyr::if_else(
          ATTAINS.FlagParameterName == "Parameter name is listed as a prior cause in ATTAINS for this organization.",
          ATTAINS.ParameterName,
          NA
        )) %>%
        dplyr::mutate(
          Flag.ParameterInput = dplyr::if_else(
            !is.na(ATTAINS.ParameterName),
            "This crosswalk was provided through an exact match auto_assign = 'Org', between ATTAINS.ParameterName and TADA.CharacteristicName.",
            "No Crosswalk was provided and no exact matches were found for this organization."
          )
        ) %>%
        dplyr::distinct()
    }

    # User provides their own user supplied parameter crosswalk
    if (!is.null(paramRef)) {
      # Identifies NEW rows in your current CreateParamRef data frame that are missing from your paramRef input -
      # i.e. current WQP Characteristics that you have not defined a crosswalk for
      Flag1 <- CreateParamRef %>%
        # anti_join will identify observations that exist in your 1st data frame, but not in the 2nd data frame.
        dplyr::anti_join(
          paramRef,
          by =
            c(
              "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier"
              # "ATTAINS.ParameterName", ATTAINS.FlagParameterName) # Exclude any dynamic values or possible NAs
            )
        ) %>%
        dplyr::mutate(
          Flag.ParameterInput =
            "Suspect: Your paramRef argument did not include this TADA.ComparableDataIdentifier. Please ensure this is not a new WQP Characteristic Name entry that needs to be crosswalked."
        )

      # identifies if a user has MODIFIED any ATTAINS.ParameterName values by TADA.ComparableDataIdentifier and ATTAINS.OrganizationIdentifier
      Flag2 <- paramRef %>%
        dplyr::anti_join(
          CreateParamRef,
          by = c(
            "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier"
          )
        ) %>%
        dplyr::mutate(
          Flag.ParameterInput =
            "This ATTAINS.ParameterName crosswalk was MODIFIED by your input(s) for this TADA.ComparableDataIdentifier."
        ) %>%
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "No parameter match for TADA.ComparableDataIdentifier" | is.na(ATTAINS.ParameterName) ~
              "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName ~
              "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
            ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName & !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS for this organization."
          )
        )

      CreateParamRef <- paramRef %>%
        dplyr::select("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName") %>%
        dplyr::full_join(
          Flag1 %>%
            dplyr::full_join(
              Flag2,
              by =
                c(
                  "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier",
                  "ATTAINS.ParameterName", "ATTAINS.FlagParameterName", "Flag.ParameterInput"
                )
            ),
          by = c("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName")
        ) %>%
        dplyr::mutate(Flag.ParameterInput = dplyr::if_else(is.na(ATTAINS.ParameterName), NA, Flag.ParameterInput)) %>%
        dplyr::rows_patch(CreateParamRef, by = "TADA.ComparableDataIdentifier") %>%
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "Not Applicable for Analysis." | is.na(ATTAINS.ParameterName) ~
              "No ATTAINS.ParameterName crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName ~
              "Parameter name is not included in ATTAINS, contact ATTAINS to add ATTAINS.ParameterName name to Domain List.",
            ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName & !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
              "Parameter name is listed as a prior cause in ATTAINS for this organization"
          )
        ) %>%
        dplyr::select(
          TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName,
          ATTAINS.FlagParameterName, Flag.ParameterInput
        ) %>%
        dplyr::distinct()

      # remove intermediate object Flag1
      rm(Flag1, Flag2)
    }

    # Excel ref files to be stored in the Downloads folder location.
    # Define the OneDrive Downloads path
    onedrive_downloads_path <- file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Downloads", "myfileRef.xlsx")

    # Define the default Downloads path
    default_downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")

    # Check if the OneDrive Downloads path exists, and prioritize it
    if (file.exists(onedrive_downloads_path)) {
      downloads_path <- onedrive_downloads_path
    } else {
      downloads_path <- default_downloads_path
    }

    if (excel == TRUE) {
      # Print message if there are many combinations of TADA Characteristic as it may slow run time.
      n <- nrow(dplyr::distinct(.data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]))
      if (n > 100 & excel == TRUE) {
        message(paste0("There are ", n, " unique TADA.ComparableDataIdentifier names in your TADA data frame.
      This may result in slow runtime for TADA_CreateParamRef() when generating the excel spreadsheet.
      Excel formulas will only be generated for the first 100 rows. Please fill down on Cells D1 and Cells E1 in excel
      to make all rows function dynamically (automatically updates the flag if a change was made to a crosswalk)."))
      }

      # Create column names for an empty dataframe
      columns <- c(
        "TADA.ComparableDataIdentifier",
        "ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier", "ATTAINS.FlagParameterName"
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
        wb, "CreateParamRef",
        cols = 1:ncol(CreateParamRef),
        widths = "auto"
      )

      # New row to rbind if a user selects "Not Applicable for Analysis."
      no_match_df <- data.frame(
        ATTAINS.OrganizationIdentifier = "NA",
        ATTAINS.ParameterName = "Not Applicable for Analysis.",
        ATTAINS.UseName = "Not Applicable for Analysis."
      )

      # Index of allowable values for drop-down lists
      openxlsx::writeData(
        wb, "Index",
        startCol = 4,
        x = rbind(
          no_match_df,
          ATTAINS_param_all[, c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName")]
          %>% dplyr::arrange(ATTAINS.ParameterName)
        )
      )

      openxlsx::writeData(
        wb, "Index",
        startCol = 2,
        x = CreateParamRef[, c("ATTAINS.ParameterName", "Flag.ParameterInput")]
      )

      openxlsx::writeData(
        wb, "Index",
        startCol = 1,
        x = data.frame(
          ATTAINS.ParameterName = c(unique(ATTAINS_param$ATTAINS.ParameterName))
        )
      )

      openxlsx::writeData(
        wb, "CreateParamRef",
        startCol = 1,
        x = CreateParamRef,
        headerStyle = header_st
      )

      # Creates a tab that contains the ATTAINS parameter-use filtered by the org_id input.
      openxlsx::writeData(
        wb, "ATTAINSOrgNamesParamRef",
        startCol = 1,
        x = ATTAINS_param,
        headerStyle = header_st
      )

      # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab

      # Note: If we make edits to the data validation, please ensure the entire
      # data frame column is being referenced.
      # Ex. data validation will capture values in tab [Index] column h, for rows 2:50000 for input, value = sprintf("'Index'!$H$2:$H$50000")

      suppressWarnings(
        openxlsx::dataValidation(
          wb,
          sheet = "CreateParamRef",
          cols = 3, rows = 2:1000,
          type = "list",
          value = sprintf("'Index'!$E$2:$E$30000"), # please ensure this covers all values in the column E in the Index tab for future development.
          allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        )
      )

      # remove intermediate objects
      rm(ATTAINS_param, ATTAINS_param_all)

      max_loops <- 0

      for (i in 1:nrow(CreateParamRef)) {
        max_loops <- max_loops + 1
        if (max_loops > 100) break

        openxlsx::writeFormula(
          wb, "CreateParamRef",
          startCol = 4,
          startRow = i + 1,
          array = TRUE,
          x = paste0(
            "=IF(OR(C", i + 1, '="",C', i + 1,
            '="Not Applicable for Analysis."),"No ATTAINS.ParameterName crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment",
            IF(ISNA(MATCH(C', i + 1, ',Index!E:E,0)),
              "Parameter name is not included in ATTAINS, contact ATTAINS to add ATTAINS.ParameterName name to Domain List.",
            IF(ISNA(MATCH(1,(C', i + 1, "=ATTAINSOrgNamesParamRef!D:D)*(B", i + 1, '=ATTAINSOrgNamesParamRef!A:A),0)),
              "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
              "Parameter name is listed as a prior cause in ATTAINS for this organization.")))'
          )
        )

        openxlsx::writeFormula(
          wb, "CreateParamRef",
          startCol = 5,
          startRow = i + 1,
          array = TRUE,
          x = paste0(
            "IF(C", i + 1, "=Index!B$", i + 1, ",Index!C$", i + 1,
            ',"This ATTAINS.ParameterName crosswalk was MODIFIED by your input(s) for this TADA.ComparableDataIdentifier.")'
          )
        )
      }

      openxlsx::conditionalFormatting(
        wb, "CreateParamRef",
        cols = 3,
        rows = 1:nrow(CreateParamRef) + 1,
        type = "blanks",
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
      )

      openxlsx::conditionalFormatting(
        wb, "CreateParamRef",
        cols = 3,
        rows = 1:nrow(CreateParamRef) + 1,
        type = "notBlanks",
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      )

      # remove intermediate objects
      rm(TADA_param, max_loops)

      # Format column widths in CreateParamRef - for future considerations of formatting
      openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(CreateParamRef) + 2, widths = "auto")

      if (overwrite == TRUE) {
        message(
          paste0("Overwriting sheet [CreateParamRef] in ", downloads_path)
        )
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
      }

      if (overwrite == FALSE) {
        message(
          "If you would like to replace sheet [CreateParamRef], use overwrite = TRUE argument in TADA_CreateParamRef."
        )
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
      }

      cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
    }

    return(CreateParamRef)
  }
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
#' Before running this function, users must run TADA_CreateParamRef() to
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
#' they should consider using the useAURef argument input or the useParamRef
#' argument input which would specify that the use names should come from a
#' user supplied list rather than from prior ATTAINS assessment cycles.
#' If a list of use names come from the useAURef, this function will apply any
#' new use names to an ATTAINS parameter name, found in your paramRef
#' argument input, by joining the ATTAINS.WaterType of the AUs defined in
#' your useAURef to the ATTAINS.WaterType found from ATTAINS Expert Query.
#'
#'
#' Otherwise, users can still
#' proceed by overriding the data validation by value pasting in Excel.
#' Users will be warned in the ATTAINS.FlagUseName column if they choose to
#' include an ATTAINS use name that was not listed in prior ATTAINS assessment cycles as:
#' 'Use name is not listed as a prior cause in ATTAINS for this organization' or
#' 'Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name'.
#'
#' Note: Future development work will allow for crosswalking other names from the WQP
#' such as using pollutant names from the EPA's Criteria Search Tool (CST):
#' www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa.
#' The TADA Team has crosswalked the CST pollutant names for EPA304a standards with
#' TADA.ComparableDataIdentifier(s) to make the criteria values available for
#' use within TADA functions. The ATTAINS.UseName(s) associated with the EPA304a
#' criteria are included from the CST. All other ATTAINS.UseName(s) are specific to an
#' ATTAINS organization and come from the ATTAINS domain value for use_name.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param paramRef A dataframe which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier and ATTAINS.ParameterName. Users will need to
#' ensure this crosswalk contains the appropriate column names in order to
#' run the function. paramRef must contain at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName. Users who are
#' interested in performing analyses for more than
#' one organization (multiple states and/or tribes) also need to include an
#' additional column name: 'ATTAINS.OrganizationIdentifier'
#'
#' @param useParamRef A dataframe which contains a completed crosswalk of
#' organization specific ATTAINS.UseName(s) for each ATTAINS.ParameterName.
#' Users will need to ensure this crosswalk contains the appropriate column
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and review this output for accuracy.
#'
#' @param useAURef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with each assessment unit.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function.
#'
#' @param auto_assign NOTE: this has not been developed, will this be helpful?
#' A boolean value. If TRUE, this will assign all unique
#' use names to an ATTAINS.ParameterName that has not been defined by your
#' organization from ATTAINS. If FALSE, the values will be left blank and
#' will need you to manually assign use names as needed.
#'
#' @return A dataframe which contains the columns: TADA.ComparableDataIdentifier,
#' ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName,
#' and ATTAINS.FlagUseName. Users will need to review the crosswalk between
#' ATTAINS.ParameterName, ATTAINS.UseName and TADA.ComparableDataIdentifier.
#'
#' @seealso [TADA_CreateParamRef()]
#' @seealso [TADA_GetEPACSTRef()]
#'
#' @export
#'
#' @examples
#' # First, generate and fill out a parameter crosswalk (see TADA_CreateParamRef()):
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' paramRef_UT4 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", auto_assign = "All", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_CreateUseParamRef():
#' UseParamRef_UT <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Now, let's compare the crosswalk for paramRef_UT4 when we use auto_assign = "All".
#' # Notice, there are NA values for ATTAINS.UseName as these UT ATTAINS Parameter Name were
#' # not listed as a cause in prior ATTAINS assessment cycles.
#' UseParamRef_UT2 <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT4, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Let's test the "auto_assign" input
#' UseParamRef_UT3 <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT4, auto_assign = TRUE, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
TADA_CreateUseParamRef <- function(.data, org_id = NULL, paramRef = NULL, useParamRef = NULL,
                                   useAURef = NULL, auto_assign = FALSE,
                                   excel = FALSE, overwrite = FALSE) {
  # Return an empty dataframe with column names only if a user does not define any arg inputs.
  if (missing(.data) && missing(org_id) && missing(auto_assign) && missing(excel) && missing(overwrite)) {
    message("All arguments are blank, returning an empty dataframe with column names only.")

    empty_df <- data.frame(
      TADA.ComparableDataIdentifier = character(0),
      ATTAINS.OrganizationIdentifier = character(0),
      ATTAINS.ParameterName = character(0),
      ATTAINS.UseName = character(0),
      IncludeOrExclude = character(0),
      ATTAINS.FlagUseName = character(0),
      Flag.UseInput = character(0)
    )

    return(empty_df)
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
      stop(paste0(
        "TADA.CreateUseParamRef: No paramRef argument provided."
      ))
    }

    # If a user does not fill in ANY values for the crosswalk of ATTAINS.ParameterName.
    # Users may want to proceed with only the EPA304a criteria crosswalk,
    # therefore we will allow users to proceed in this case.
    if (sum(!is.na(paramRef$ATTAINS.ParameterName)) == 0) {
      warning(paste0(
        "No values were found in ATTAINS.ParameterName. ",
        "Please ensure that you have inputted all field values of interest in the ",
        "ATTAINS.ParameterName column generated from TADA_CreateParamRef() function."
      ))
    }

    # If a user leaves at least one values for the crosswalk of ATTAINS.ParameterName blank.
    # Users are recommended to select 'No parameter match for this TADA.ComparableDataIdentifier' if
    # there is no crosswalk, but leaving it blank will be treated similarly.
    if (sum(is.na(paramRef$ATTAINS.ParameterName)) > 1) {
      print(paste0(
        "NAs were found in ATTAINS.ParameterName. ",
        "Please ensure that you have inputted all field values of interest in ",
        "the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function."
      ))
    }

    # check to see if user-supplied parameter ref is a df with appropriate columns and is filled out.
    if (!is.null(paramRef) & !is.character(paramRef)) {
      if (!is.data.frame(paramRef)) {
        stop(paste0(
          "TADA_CreateUseParamRef: 'paramRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName."
        ))
      }

      if (is.data.frame(paramRef)) {
        col.names <- c(
          "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName"
        )

        ref.names <- names(paramRef)

        if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
          stop(paste0(
            "TADA_CreateUseParamRef: 'paramRef' must be a data frame with these 2 columns:",
            "TADA.ComparableDataIdentifier and ATTAINS.ParameterName."
          ))
        }
      }
    }

    # check to see if user-supplied parameter-use ref is a df with appropriate columns and is filled out.
    if (!is.null(useParamRef) & !is.character(useParamRef)) {
      if (!is.data.frame(useParamRef)) {
        stop(paste0(
          "TADA_CreateUseParamRef: 'useParamRef' must be a data frame with these 3 columns:",
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName, ATTAINS.UseName"
        ))
      }

      if (is.data.frame(useParamRef)) {
        col.names <- c(
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName", "ATTAINS.UseName"
        )

        ref.names <- names(useParamRef)

        if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(useParamRef))) {
          stop(paste0(
            "TADA_CreateUseParamRef: 'useParamRef' must be a data frame with these 3 columns:",
            "ATTAINS.OrganizationIdentifier, TADA.ComparableDataIdentifier, ",
            "ATTAINS.ParameterName, ATTAINS.UseName"
          ))
        }
      }
    }

    .data <- as.data.frame(.data)

    # Pulls in all domain values of parameter and use names by orgs in ATTAINS.
    ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))

    # If a user provides a useAURef, We will use the uses in this table
    if (!is.null(useAURef)) {
      ATTAINS_param_all <- ATTAINS_param_all %>%
        dplyr::select(-ATTAINS.UseName) %>%
        dplyr::distinct() %>%
        dplyr::left_join(
          useAURef,
          by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.WaterType")
        )
    }

    # Considers if we want to separate speciation, fraction, units as separate columns in the future for crosswalk.
    if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
      paramRef <- paramRef %>%
        dplyr::left_join(
          .data,
          by = c(
            "TADA.CharacteristicName", "TADA.MethodSpeciationName",
            "TADA.ResultSampleFractionText"
          )
        ) %>%
        dplyr::select(
          "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName", "ATTAINS.FlagParameterName"
        )
    }

    # if user doesn't provide an org_id argument,
    # Selection of org_id will filter the drop down lists in future steps of creating the reference tables.
    if (is.null(org_id)) {
      stop("TADA.CreateUseParamRef: No organization identifier(s) provided.")
    }

    org_id <- as.list(org_id)

    # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_id.
    # 5/14/25 KW: We should use separate columns for CST organization/pollutant/use names in the future.
    if (sum(!org_id[tolower(org_id) != tolower("EPA304a")] %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
      warning(paste0(
        "TADA_CreateuseParamRef: ",
        "One or more organization identifiers entered by user is not found in ATTAINS. "
      ))
    }

    # Filters the ATTAINS parameter and use names by the org_id in user supplied df.
    ATTAINS_param <- ATTAINS_param_all %>%
      dplyr::select(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %>%
      dplyr::filter(ATTAINS.ParameterName %in% paramRef$ATTAINS.ParameterName) %>%
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id)

    # Create the parameter-use reference table for validation
    CreateUseParamRef <- paramRef %>%
      dplyr::left_join(
        ATTAINS_param,
        by = c("ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier"),
        relationship = "many-to-many"
      ) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName, ATTAINS.UseName
      ) %>%
      # tidyr::drop_na(ATTAINS.ParameterName) %>%
      dplyr::filter(ATTAINS.ParameterName != "No parameter match for TADA.ComparableDataIdentifier") %>%
      dplyr::distinct() %>%
      dplyr::mutate(IncludeOrExclude = dplyr::if_else(
        is.na(ATTAINS.UseName),
        "Exclude",
        "Include"
      )) %>%
      dplyr::mutate(ATTAINS.FlagUseName = dplyr::if_else(
        is.na(ATTAINS.UseName),
        "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
        "Use name is listed as a prior cause in ATTAINS for this organization."
      )) %>%
      dplyr::mutate(
        Flag.UseInput =
          "Default: no modification was made to this row."
      )

    if (auto_assign == TRUE) {
      print(paste0(
        "auto_assign == TRUE was selected, ",
        "assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an ",
        "organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed."
      ))

      use.names <- CreateUseParamRef %>%
        dplyr::select(ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
        tidyr::drop_na() %>%
        dplyr::distinct()

      CreateUseParamRef_temp <- CreateUseParamRef %>%
        dplyr::filter(is.na(ATTAINS.UseName)) %>%
        dplyr::left_join(use.names, by = c("ATTAINS.OrganizationIdentifier"), relationship = "many-to-many") %>%
        dplyr::mutate(ATTAINS.UseName = dplyr::coalesce(ATTAINS.UseName.x, ATTAINS.UseName.y)) %>%
        dplyr::select(-c(ATTAINS.UseName.x, ATTAINS.UseName.y)) %>%
        # dplyr::mutate(TADA.ComparableDataIdentifier = dplyr::coalesce(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
        # dplyr::select(-c(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
        dplyr::mutate(IncludeOrExclude = "Include") %>%
        dplyr::mutate(
          Flag.UseInput =
            "This row was MODIFIED by your input(s)."
        )

      CreateUseParamRef <- CreateUseParamRef %>%
        # dplyr::select(TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.OrganizationIdentifier) %>%
        dplyr::filter(!is.na(ATTAINS.UseName)) %>%
        dplyr::full_join(CreateUseParamRef_temp, by = c("ATTAINS.ParameterName", "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude", "ATTAINS.FlagUseName", "Flag.UseInput")) %>%
        dplyr::mutate(ATTAINS.FlagUseName = dplyr::case_when(
          paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) ~
            "Use name is listed as a prior cause in ATTAINS for this organization.",
          !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) &
            ATTAINS.UseName %in% ATTAINS_param_all$ATTAINS.UseName ~
            "Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name.",
          is.na(ATTAINS.UseName) ~
            "No use name is provided. Consider choosing an appropriate ATTAINS.UseName."
        )) %>%
        dplyr::mutate(TADA.ComparableDataIdentifier = dplyr::coalesce(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
        dplyr::select(-c(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
        dplyr::select(
          TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
          IncludeOrExclude, ATTAINS.FlagUseName, Flag.UseInput
        ) %>%
        dplyr::arrange(match(IncludeOrExclude, c("Include")), ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
        dplyr::distinct()
    }

    if (!is.null(useParamRef)) {
      # user may have only supplied a useParamRef table with TADA.CharacteristicName rather than TADA.ComparableDataIdentifier
      # This also validates the TADA.ComparableDataIdentifier crosswalk to ensure it is up to date (drops and re-join)
      if ("TADA.CharacteristicName" %in% names(useParamRef)) {
        useParamRef <- useParamRef %>%
          dplyr::select(-TADA.ComparableDataIdentifier) %>%
          dplyr::left_join(
            .data %>%
              dplyr::select(TADA.ComparableDataIdentifier, TADA.CharacteristicName),
            by = ("TADA.CharacteristicName")
          )
      }

      # check if users have specified an include or exclude column. If not, assume it is all 'include'
      if ("IncludeOrExclude" %in% names(useParamRef)) {
        useParamRef <- useParamRef %>%
          dplyr::select(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude) %>%
          dplyr::left_join(paramRef, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "IncludeOrExclude"))
      } else {
        print("IncludeOrExclude was not found as a column name in your user supplied, assuming all parameter and uses are applicable for your analysis.")
        useParamRef <- useParamRef %>%
          dplyr::select(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %>%
          dplyr::left_join(paramRef, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName"), relationship = "many-to-many") %>%
          dplyr::mutate(IncludeOrExclude = "Include")
      }

      useParamRef$ATTAINS.ParameterName <- toupper(useParamRef$ATTAINS.ParameterName)

      # identifies if a user has excluded any useParam rows. This row is showing up as a new entry but has not been defined.
      # This should flag users that they need to review this entry and if they
      # truly want to exclude it or not. What should the default be?
      Flag1 <- CreateUseParamRef %>%
        dplyr::anti_join(
          useParamRef,
          by =
            c(
              "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName"
              # "ATTAINS.UseName", "IncludeOrExclude", "ATTAINS.FlagUseName"
            )
        ) %>%
        dplyr::mutate(
          Flag.UseInput =
            "Suspect: Your useParamRef argument did not include this TADA.ComparableDataIdentifier. Please ensure you have provided all ATTAINS.UseName and ATTAINS.ParameterName combinations in your input."
        )
      # dplyr::mutate(IncludeOrExclude = "Exclude")

      # identifies if a user has MODIFIED any useParam rows.
      Flag2 <- useParamRef %>%
        dplyr::anti_join(
          CreateUseParamRef,
          by = c(
            "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
            "IncludeOrExclude" # , "ATTAINS.FlagUseName"
          )
        ) %>%
        dplyr::mutate(
          Flag.UseInput =
            "This row was MODIFIED by your input(s)."
        ) %>%
        dplyr::select("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude", "ATTAINS.ParameterName", "ATTAINS.UseName")


      CreateUseParamRef <- useParamRef %>%
        dplyr::select("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude", "ATTAINS.ParameterName", "ATTAINS.UseName") %>%
        dplyr::full_join(
          Flag1 %>%
            dplyr::full_join(
              Flag2,
              by =
                c(
                  "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude",
                  "ATTAINS.ParameterName", "ATTAINS.UseName"
                ),
              relationship = "many-to-many"
            ),
          by = c("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude", "ATTAINS.ParameterName", "ATTAINS.UseName"),
          relationship = "many-to-many"
        ) %>%
        dplyr::mutate(ATTAINS.FlagUseName = dplyr::case_when(
          is.na(ATTAINS.UseName) ~
            "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
          paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) ~
            "Use name is listed as a prior cause in ATTAINS for this organization.",
          !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) &
            ATTAINS.UseName %in% ATTAINS_param_all$ATTAINS.UseName ~
            "Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name.",
          TRUE ~
            "Use name is not listed as a prior cause in ATTAINS."
        )) %>%
        dplyr::mutate(Flag.UseInput = dplyr::case_when(
          is.na(Flag.UseInput) ~
            "Default: no modification was made to this row.",
          !is.na(Flag.UseInput) ~
            Flag.UseInput
        )) %>%
        dplyr::filter(
          ATTAINS.OrganizationIdentifier %in% org_id,
          ATTAINS.ParameterName %in% paramRef$ATTAINS.ParameterName,
          !is.na(ATTAINS.ParameterName)
        ) %>%
        dplyr::select(
          TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
          IncludeOrExclude, ATTAINS.FlagUseName, Flag.UseInput
        ) %>%
        dplyr::arrange(match(IncludeOrExclude, c("Include")), ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
        dplyr::distinct()

      # remove intermediate objects
      rm(Flag1, Flag2)
    }

    # remove intermediate objects
    rm(ATTAINS_param)

    # Define the OneDrive Downloads path
    onedrive_downloads_path <- file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Downloads", "myfileRef.xlsx")

    # Define the default Downloads path
    default_downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")

    # Check if the OneDrive Downloads path exists, and prioritize it
    if (file.exists(onedrive_downloads_path)) {
      downloads_path <- onedrive_downloads_path
    } else {
      downloads_path <- default_downloads_path
    }

    if (excel == TRUE) {
      # Print message if there are many combinations of TADA Characteristic as it may slow run time.
      n <- nrow(CreateUseParamRef)
      if (n > 100) {
        message(paste0("There are ", n, " rows in your CreateUseParamRef.
      This may result in slow runtime for TADA_CreateUseParamRef() when generating the excel spreadsheet.
      Excel formulas will only be generated for the first 100 rows. Please fill down on Cells F1 and Cells G1 in excel
      to make all rows function dynamically (automatically updates the flag if a change was made to a crosswalk)."))
      }

      # Create column names for an empty dataframe
      columns <- c(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
        "ATTAINS.FlagParameterName", "ATTAINS.FlagUseName"
      )

      # empty dataframe with just column names
      par <- data.frame(matrix(nrow = 0, ncol = length(columns)))
      colnames(par) <- columns

      wb <- openxlsx::loadWorkbook(wb, downloads_path)

      # If a user chooses to rerun the TADA_CreateUseParamRef() function,
      # the sheet will already exist and error.
      tryCatch(
        {
          openxlsx::addWorksheet(wb, "CreateUseParamRef")
        },
        error = function(e) {
          openxlsx::removeWorksheet(wb, "CreateUseParamRef")
          openxlsx::addWorksheet(wb, "CreateUseParamRef")
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
        wb, "CreateUseParamRef",
        cols = 1:ncol(CreateUseParamRef),
        widths = "auto"
      )

      # Export CreateUseParamRef dataframe into the excel spreadsheet tab
      openxlsx::writeData(wb, "CreateUseParamRef", startCol = 1, x = CreateUseParamRef, headerStyle = header_st)

      # Index of allowable values for drop-down lists
      openxlsx::writeData(wb, "Index", startCol = 9, x = data.frame("IncludeOrExclude" = c("Include", "Exclude")))

      openxlsx::writeData(wb, "Index", startCol = 7, x = CreateUseParamRef[, c("ATTAINS.FlagUseName", "Flag.UseInput")])

      # Data validation drop down list created below.
      # Note: ATTAINSOrgNamesParamRef contains the list of prior param and use cause by org names specific.
      # Since Use Names are individual to each Organization.
      suppressWarnings(
        # Data validation for ATTAINS.UseName.
        openxlsx::dataValidation(
          wb,
          sheet = "CreateUseParamRef",
          cols = 4, rows = 2:1000,
          type = "list",
          value = sprintf("'ATTAINSOrgNamesParamRef'!$E$2:$E$50000"),
          allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        )
      )

      suppressWarnings(
        # Data validation for "Include" or "Exclude" values.
        openxlsx::dataValidation(
          wb,
          sheet = "CreateUseParamRef",
          cols = 5, rows = 2:1000,
          type = "list",
          value = sprintf("'Index'!$I$2:$I$5"),
          allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        )
      )

      max_loops <- 0

      for (i in 1:nrow(CreateUseParamRef)) {
        # Formula based cell values in excel.
        openxlsx::writeFormula(
          wb, "CreateUseParamRef",
          startCol = 6, startRow = i + 1,
          array = TRUE,
          x = paste0(
            "=IF(E", i + 1, '="Exclude",
              "Use name does not apply for this ATTAINS.ParameterName. Excluding this use name from analysis.",
            IF(ISBLANK(D', i + 1, '),
              "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
            IF(ISNA(MATCH(1,(D', i + 1, "=ATTAINSOrgNamesParamRef!E:E)*(B", i + 1, '=ATTAINSOrgNamesParamRef!A:A),0)),
              "Use name is not listed as a prior cause in ATTAINS for this organization.",
            IF(ISNA(MATCH(1,(C', i + 1, "=ATTAINSOrgNamesParamRef!D:D)*(D", i + 1, "=ATTAINSOrgNamesParamRef!E:E)*(B", i + 1, '=ATTAINSOrgNamesParamRef!A:A),0)),
              "Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name.",
              "Use name is listed as a prior cause in ATTAINS for this organization."))))'
          )
        )

        openxlsx::writeFormula(
          wb, "CreateUseParamRef",
          startCol = 7,
          startRow = i + 1,
          array = TRUE,
          x = paste0(
            "IF(F", i + 1, "=Index!G$", i + 1, ",Index!H$", i + 1,
            ',"This row was MODIFIED by your input(s).")'
          )
        )
        max_loops <- max_loops + 1
        if (max_loops > 100) break
      }

      # Conditional formatting created below.

      # If a user has left an ATTAINS.UseName blank, flag as a red cell.
      openxlsx::conditionalFormatting(
        wb, "CreateUseParamRef",
        cols = 4, rows = 1:nrow(CreateUseParamRef) + 1,
        type = "blanks",
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
      )

      # If a user has an ATTAINS.UseName filled out, flag as a yellow cell.
      openxlsx::conditionalFormatting(
        wb, "CreateUseParamRef",
        cols = 4, rows = 1:nrow(CreateUseParamRef) + 1,
        type = "notBlanks",
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      )

      # If a user has chose to Exclude a use name for a parameter, flag as a red cell.
      openxlsx::conditionalFormatting(
        wb, "CreateUseParamRef",
        cols = 5, rows = 1:nrow(CreateUseParamRef) + 1,
        type = "contains",
        rule = c("Exclude"),
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
      )

      # If a user has chose to Include a use name for a parameter, flag as a yellow cell.
      openxlsx::conditionalFormatting(
        wb, "CreateUseParamRef",
        cols = 5, rows = 1:nrow(CreateUseParamRef) + 1,
        type = "contains",
        rule = c("Include"),
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      )

      # Format Column widths
      openxlsx::setColWidths(wb, "CreateUseParamRef", cols = 1:ncol(CreateUseParamRef) + 2, widths = "auto")

      # Handles overwriting the excel file.
      if (overwrite == TRUE) {
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
      }

      if (overwrite == FALSE) {
        warning(
          "If you would like to replace [CreateUseParamRef], use overwrite = TRUE argument in TADA_CreateUseParamRef"
        )
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
      }

      cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
    }
    return(CreateUseParamRef)
  }
}



#' ATTAINS Assessment Unit and Use Name Crosswalk
#'
#' This function pulls in all prior ATTAINS Use names associated with each
#' ATTAINS organization's Assessment Unit (AU) from the prior ATTAINS cycle.
#' This function requires an ATTAINS org_id and
#' a crosswalk of an organization's WQP
#' Monitoring Location's, ATTAINS Assessment Unit's, and ATTAINS
#' Water Type codes as a function input (AUMLRef). The output from
#' `TADA_CreateATTAINSAUMLCrosswalk(.data, return_sf = FALSE)` can be used
#' directly as the AUMLRef argument input in this function. Alternatively,
#' a user supplied crosswalk can be entered or `TADA_GetATTAINSAUMLCrosswalk()`
#' and/or `TADA_UpdateATTAINSAUMLCrosswalk()` functions can be leveraged
#' to generate the crosswalk.
#'
#' This function is mainly designed to assist with pulling
#' existing Uses that have been entered
#' into ATTAINS in the prior ATTAINS cycle (most recent assessment).
#'
#' For any NEW AUs and/or NEW uses, users must modify
#' the output of this function to manually add those uses and AU's to the crosswalk.
#' Alternatively, we have developed a helper function, [TADA_CreateWaterUseRef()],
#' to assist with assigning uses to NEW AU's. This can be leveraged to assign
#' uses for any new AUs based on the water type of the AU.
#' Users can either supply their own Water
#' Type to Use crosswalk or utilize ATTAINS webservices to pull in the Water Type to
#' Use reference file. This Water to Use reference file can be used to assign all
#' unique Uses to a new/modified AU based on which uses have been assigned to that
#' water type in the past for the specified ATTAINS organization.
#' Any new or modified AU and use information that gets submitted to ATTAINS
#' in the current assessment cycle will not be available in ATTAINS until the
#' assessment is approved and completed.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param AUMLRef A required data frame input. This data frame
#' should contain a completed crosswalk of WQP Monitoring Locations
#' associated with each ATTAINS Assessment Unit. Users will need to ensure
#' this crosswalk contains the appropriate column names in order to run this function.
#' See module 2 vignette and sample output of [TADA_CreateATTAINSAUMLCrosswalk()].
#'
#' @param useAURef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with each assessment unit.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function.
#'
#' @param waterUseRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with a water type.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @seealso [TADA_DataRetrieval()] for the required format of .data
#' @seealso [TADA_CreateATTAINSAUMLCrosswalk()] to help generate the required AUMLRef
#' @seealso [TADA_GetATTAINSAUMLCrosswalk()] to help generate the required AUMLRef
#' @seealso [TADA_UpdateATTAINSAUMLCrosswalk()] to help generate the required AUMLRef
#' @seealso [TADA_CreateWaterUseRef()] to help assign ATTAINS Uses to NEW ATTAINS Assessment Units based on ATTAINS Water Type
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for each defined AU.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pull a sample WQP data query
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
#' AK_CreateUseAURef <- TADA_CreateUseAURef(
#'   TADA_AK_Example,
#'   org_id = "AKDECWQ",
#'   AUMLRef = AK_appenduserdata,
#'   excel = FALSE
#' )
#'
#' # Let's use a wateruseRef now to fill in these values.
#' AK_CreateUseAURef_auto_assign <- TADA_CreateUseAURef(
#'   TADA_AK_Example,
#'   org_id = "AKDECWQ",
#'   AUMLRef = AK_appenduserdata,
#'   waterUseRef = TADA_CreateWaterUseRef(TADA_AK_EXAMPLE, org_id = "AKDECWQ"),
#'   excel = FALSE
#' )
#'
#' # We can save and reuse a useAURef as desired.
#' AK_CreateUseAURef2 <- TADA_CreateUseAURef(
#'   TADA_AK_Example,
#'   org_id = "AKDECWQ",
#'   useAURef = AK_CreateUseAURef_auto_assign,
#'   AUMLRef = AK_appenduserdata,
#'   excel = FALSE
#' )
#' }
#'
TADA_CreateUseAURef <- function(.data, org_id = NULL, AUMLRef = NULL, # Required inputs in this line
                                useAURef = NULL, waterUseRef = NULL,
                                excel = FALSE, overwrite = FALSE) {
  # Return an empty dataframe with column names only if a user does not define any arg inputs.
  if (missing(.data) && missing(org_id) && missing(excel) && missing(overwrite)) {
    message("All arguments are blank, returning an empty dataframe with column names only.")

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
        "TADA_CreateUseAURef: ",
        "You must provide a AUMLRef to run this function."
      ))
    }

    # rExpertQuery API key for TADA
    tadakey <- "EKtgCrmatyP4G8iFgADMIfwlddbpDlSqRxetlN09"

    # Pulls in all domain values of parameter and use names by orgs in ATTAINS. Filtering by state is done in the next steps.
    ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))

    # check to see if user-supplied AUMLRef is a df with appropriate columns and is filled out.
    if (!is.null(AUMLRef) & !is.character(AUMLRef)) {
      if (!is.data.frame(AUMLRef)) {
        stop(paste0(
          "TADA_CreateUseAURef: 'AUMLRef' must be a data frame with these 3 columns:",
          "ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier, and ATTAINS.OrganizationIdentifier."
        ))
      }

      if (is.data.frame(AUMLRef)) {
        col.names <- c(
          "ATTAINS.WaterType", "ATTAINS.AssessmentUnitIdentifier",
          "ATTAINS.OrganizationIdentifier"
        )

        if (!any(col.names %in% names(AUMLRef))) {
          stop(paste0(
            "TADA_CreateUseAURef: 'AUMLRef' must be a data frame with these 3 columns:",
            "ATTAINS.WaterType, ATTAINS.OrganizationIdentifier and ATTAINS.AssessmentUnitIdentifier"
          ))
        }

        AULMLRef <- AUMLRef %>%
          dplyr::select(
            ATTAINS.AssessmentUnitIdentifier, ATTAINS.WaterType,
            ATTAINS.OrganizationIdentifier
          )
      }
    }

    # if user doesn't provide an org_id argument
    if (is.null(org_id)) {
      stop("TADA_CreateUseAURef: No organization identifier(s) provided.")
    }

    # Handle later, if multiple org_id are used, create a loop when calling rATTAINS (or if we use EQ National extract, no loop needed)
    # org_id <- as.list(org_id)

    # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_id.
    if (sum(!org_id[org_id != "EPA304a"] %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
      warning(paste0(
        "TADA_CreateUseAURef: ",
        "One or more organization identifiers entered by user is not found in ATTAINS. "
      ))
    }

    # Pulls in Existing Uses by Existing AU from ATTAINS EQ
    print("TADA_CreateUseAURef: Importing existing uses by AU from Expert Query.")

    OrgID_assessments <- spsUtil::quiet(
      rExpertQuery::EQ_Assessments(org_id = org_id, api_key = tadakey)
    )

    OrgID_assessments <- dplyr::filter(
      OrgID_assessments,
      assessmentUnitId %in% unique(AUMLRef$ATTAINS.AssessmentUnitIdentifier)
    )

    # Joins Existing Uses to Existing AUs in your AUMLRef dataframe. Non-matches are flagged as New AU.
    CreateUseAURef <- AUMLRef %>%
      dplyr::left_join(
        OrgID_assessments,
        by = c(
          "ATTAINS.AssessmentUnitIdentifier" = "assessmentUnitId",
          "ATTAINS.OrganizationIdentifier" = "organizationId"
        ),
        relationship = "many-to-many"
      ) %>%
      dplyr::mutate(
        TADA.AssessmentUnitStatus =
          dplyr::if_else(
            ATTAINS.AssessmentUnitIdentifier %in% unique(OrgID_assessments$assessmentUnitId), "Existing", "New"
          )
      ) %>%
      dplyr::bind_cols(
        data.frame(
          IncludeOrExclude = as.character("Include")
        )
      ) %>%
      dplyr::mutate(
        ATTAINS.WaterType = dplyr::coalesce(waterType, ATTAINS.WaterType)
      ) %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, # ATTAINS.assessmentunitname,
        ATTAINS.UseName = useName, ATTAINS.WaterType, TADA.AssessmentUnitStatus, IncludeOrExclude
      ) %>%
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct() %>%
      dplyr::arrange(ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName)

    # User provides a WaterUseRef - specifying the assignment of Uses to AUs not found in ATTAINS by its Water Type.
    if (!is.null(waterUseRef)) {
      AUMLRef <- dplyr::select(
        AUMLRef,
        ATTAINS.AssessmentUnitIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.WaterType
      )

      waterUseRef <- waterUseRef %>%
        dplyr::filter(IncludeOrExclude == "Include")

      CreateUseAURef_MissingUse <- dplyr::filter(CreateUseAURef, is.na(ATTAINS.UseName))

      CreateUseAURef_MissingUse <- CreateUseAURef_MissingUse %>%
        dplyr::select(ATTAINS.AssessmentUnitIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.WaterType, TADA.AssessmentUnitStatus) %>%
        dplyr::left_join(AUMLRef, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier", "ATTAINS.WaterType")) %>%
        dplyr::left_join(waterUseRef, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.WaterType"))

      CreateUseAURef <- CreateUseAURef %>%
        dplyr::filter(!is.na(ATTAINS.UseName)) %>%
        dplyr::bind_rows(CreateUseAURef_MissingUse) %>%
        dplyr::select(
          ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, # ATTAINS.assessmentunitname,
          ATTAINS.UseName, ATTAINS.WaterType, TADA.AssessmentUnitStatus, IncludeOrExclude
        ) %>%
        dplyr::distinct()
    }

    # User provides their own useAURef that has been filled out.
    if (!is.null(useAURef)) {
      # What rows did the user have in their useAURef that was not found in the most recent ATTAINS data system?
      Flag1 <- CreateUseAURef %>%
        dplyr::anti_join(
          useAURef,
          by =
            c(
              "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
              "ATTAINS.UseName", "ATTAINS.WaterType", "TADA.AssessmentUnitStatus", "IncludeOrExclude"
            )
        ) %>%
        dplyr::mutate(
          TADA.AssessmentUnitStatus = dplyr::case_when(
            !ATTAINS.AssessmentUnitIdentifier %in% AUMLRef$ATTAINS.AssessmentUnitIdentifier ~ "New",
            ATTAINS.AssessmentUnitIdentifier %in% AUMLRef$ATTAINS.AssessmentUnitIdentifier ~
              "Suspect: Excluding from Assessment. This AU and use is not found in your useAURef"
          )
        ) %>%
        dplyr::mutate(
          IncludeOrExclude = dplyr::case_when(
            ATTAINS.AssessmentUnitIdentifier %in% AUMLRef$ATTAINS.AssessmentUnitIdentifier ~
              "Exclude"
          )
        )

      CreateUseAURef <- Flag1 %>%
        dplyr::full_join(
          useAURef,
          by =
            c(
              "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
              "ATTAINS.UseName", "ATTAINS.WaterType", "TADA.AssessmentUnitStatus", "IncludeOrExclude"
            )
        ) %>%
        dplyr::mutate(
          TADA.AssessmentUnitStatus = dplyr::case_when(
            !ATTAINS.AssessmentUnitIdentifier %in% AUMLRef$ATTAINS.AssessmentUnitIdentifier ~ "New",
            TRUE ~ TADA.AssessmentUnitStatus
          )
        ) %>%
        dplyr::arrange(match(IncludeOrExclude, c("Include")), ATTAINS.WaterType, ATTAINS.UseName) %>%
        dplyr::distinct()
    }

    if (excel == TRUE) {
      # default Downloads file location.
      downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "CreateUseAURef.xlsx")

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "CreateUseAURef", visible = TRUE)
      openxlsx::addWorksheet(wb, "Index", visible = FALSE)

      # Format column header
      header_st <- openxlsx::createStyle(textDecoration = "Bold")
      # Format Column widths
      openxlsx::setColWidths(wb, "CreateUseAURef", cols = 1:ncol(CreateUseAURef), widths = "auto")

      # set zoom size
      set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
      n_sheets <- length(wb$worksheets)
      for (i in 1:n_sheets) {
        sV <- wb$worksheets[[i]]$sheetViews
        wb$worksheets[[i]]$sheetViews <- set_zoom(90)
      }

      # writes CreateUseAuRef dataframe
      openxlsx::writeData(wb, "CreateUseAURef", startCol = 1, x = CreateUseAURef, headerStyle = header_st)

      # Index of allowable values for drop-down lists
      openxlsx::writeData(
        wb, "Index",
        startCol = 1,
        x = data.frame("IncludeOrExclude" = c("Include", "Exclude"))
      )

      # data validation drop down list created below.
      suppressWarnings(openxlsx::dataValidation(
        wb,
        sheet = "CreateUseAURef",
        cols = 6, rows = 2:10000,
        type = "list",
        value = sprintf("'Index'!$A$2:$A$5"),
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
      ))

      # Conditional Formatting
      openxlsx::conditionalFormatting(
        wb, "CreateUseAURef",
        cols = 6, rows = 2:(nrow(CreateUseAURef) + 1),
        type = "contains", rule = "Include", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
      ) # default values or indicates good to go cells.

      openxlsx::conditionalFormatting(
        wb, "CreateUseAURef",
        cols = 6, rows = 2:(nrow(CreateUseAURef) + 1),
        type = "contains", rule = "Exclude", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      ) # using yellow to indicate modified cell

      if (overwrite == TRUE) {
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
      }

      if (overwrite == FALSE) {
        warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateUseAUef")
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
      }

      cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")

      CreateUseAURef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseAURef")
    }

    return(CreateUseAURef)
  }
}



#' Helper Function to Apply Uses to Unassigned Assessment Units by Water Type
#'
#' This is a helper function to TADA_CreateUseAURef and is meant to help users
#' with reviewing all water type and use name combination from their org.
#' This function will help to assign ATTAINS use names to any new or modified
#' assessment unit provided from a user's AUMLRef if there are any.
#'
#' This function will assume all use names applies to a water type from the
#' prior assessment cycles are being done for an organization's assessment.
#' Users are expected to modify this ref file as needed.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param waterUseRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with a water type.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function.
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#'
#' @seealso [TADA_CreateUseAURef()]
#'
#' @export
#'
#' @examples
#' TADA_CreateWaterUseRef(TADA_AK_EXAMPLE, org_id = "AKDECWQ")
#'
TADA_CreateWaterUseRef <- function(.data, org_id = NULL, waterUseRef = NULL) {
  # If org_id argument is not provided, this will attempt to pull in org_id from TADA_GetATTAINS.
  if (is.null(org_id)) {
    print(paste0(
      "TADA_CreateWaterUseParamRef: No organization identifier(s) provided. ",
      "Attempting to pull in organization identifiers found in the TADA data frame. ",
      "Please ensure that TADA_CreateATTAINSAUMLCrosswalk has been run if you did not provide ",
      "an org_id input."
    ))
    print(
      "Users should provide one or more ATTAINS.OrganizationIdentifier",
      "that pertains to their analysis."
    )
    TADA_CheckColumns(.data, "ATTAINS.OrganizationId")
    org_id <- .data %>%
      sf::st_drop_geometry() %>%
      dplyr::select(ATTAINS.OrganizationId) %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  # User needs to supply their ATTAINS org id
  if (is.null(org_id)) {
    stop("TADA_CreateWaterUseParamRef: No organization identifier(s) provided.")
  }

  # rExpertQuery API key for TADA
  tadakey <- "EKtgCrmatyP4G8iFgADMIfwlddbpDlSqRxetlN09"

  # Pulls in all domain values of parameter and use names by orgs in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))

  # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_id.
  if (sum(!org_id[org_id != "EPA304a"] %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
    warning(paste0(
      "TADA_CreateWaterUseParamRef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS. "
    ))
  }

  # Calls on EQ_Assessments from latest assessment cycle. Pulls in unique water types and uses by org
  print(paste0(
    "TADA_CreateWaterUseParamRef: Importing unique water types and uses ",
    "by organization from Expert Query."
  ))

  OrgID_assessments <- spsUtil::quiet(rExpertQuery::EQ_Assessments(org_id = org_id, api_key = tadakey))

  CreateWaterUseRef <- OrgID_assessments[, c("organizationName", "organizationId", "waterType", "useName")] %>%
    dplyr::distinct() %>%
    dplyr::bind_cols(
      data.frame(
        IncludeOrExclude = as.character("Include")
      )
    ) %>%
    dplyr::select(
      ATTAINS.OrganizationName = organizationName, ATTAINS.OrganizationIdentifier = organizationId,
      ATTAINS.UseName = useName, ATTAINS.WaterType = waterType, IncludeOrExclude
    )

  # User supplies their own use to water ref table.
  if (!is.null(waterUseRef)) {
    CreateWaterUseRef <- waterUseRef %>%
      dplyr::distinct() %>%
      dplyr::bind_cols(
        data.frame(
          IncludeOrExclude = as.character("Include")
        )
      ) %>%
      dplyr::select(
        ATTAINS.OrganizationName, ATTAINS.OrganizationIdentifier,
        ATTAINS.UseName, ATTAINS.WaterType, IncludeOrExclude
      )
  }

  return(CreateWaterUseRef)
}



#' Review and Apply Any Site-specific Criteria to Monitoring Location Sites or Assessment Units
#'
#' This function will pull in all unique MonitoringLocationName, MonitoringLocationType,
#' and MonitoringLocationIdentifier from the TADA dataframe and join it to
#' TADA_CreateUseParamRef. Users are not required to provide a crosswalk between
#' WQP Monitoring locations and Assessment units if they are only interested in
#' summarizing assessments on a monitoring location level.
#'
#' If users are interested in summarizing water quality data results by Assessment
#' Units, users will need to provide an AUMLRef and useAURef file which
#' (see TADA Module 2 tools) to assist in their monitoring location to assessment
#' unit crosswalk (see TADA_GetATTAINSAUMLCrosswalk, TADA_CreateAUMLCrosswalk,
#' and TADA_GetATTAINSByAUID) and uses to assessment unit crosswalk
#' (see TADA_CreateWaterUseParamRef and TADA_CreateUseAURef) prior to this step.
#'
#' Users can apply any unique site-specific criteria (for example, warm waters,
#' cold waters, water classifications, species-based waters, ecoregions etc.) to
#' any monitoring location sites or assessment units as needed. Users are recommended
#' to utilize the excel file for easy filtering across columns to apply any
#' site specific criteria as needed.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param useParamRef A required data frame which contains a completed crosswalk of
#' organization specific ATTAINS.UseName(s) for each ATTAINS.ParameterName.
#' Users will need to ensure this crosswalk contains the appropriate column
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and review this output for accuracy.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param useAURef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with an assessment unit.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function. See output of [TADA_CreateUseAURef()] for column names.
#'
#' @param AUMLRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. Users will need to ensure this crosswalk contains the
#' appropriate column names in order to run the function.
#' See module 2 vignette and sample output of [TADA_CreateAUMLCrosswalk()].
#'
#' @param MLSummaryRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit.
#'
#' @param displayNA A boolean value. If TRUE, this allows user to view MLSummaryRef
#' for all uses and parameter assigned to a ML or AU regardless if that site contains
#' WQP data for that parameter. This is useful if a user is interested in an explicit
#' list of everything that will be analyzed. Default is FALSE.
#'
#' An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with a water type.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function.
#'
#' @return A data frame with any unique spatial descriptions defined for
#'
#' @seealso [TADA_CreateUseParamRef()]
#' @seealso [TADA_CreateUseAURef()]
#' @seealso [TADA_CreateWaterUseRef()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First, generate and fill out a parameter crosswalk (see TADA_CreateParamRef()):
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_CreateUseParamRef():
#' UseParamRef_UT <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Now, run TADA_CreateMLSummaryRef()
#' MLSummaryRef_UT <- TADA_CreateMLSummaryRef(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   useAURef = NULL, AUMLRef = NULL,
#'   useParamRef = UseParamRef_UT,
#'   excel = FALSE
#' )
#' }
#'
TADA_CreateMLSummaryRef <- function(.data, org_id = NULL, useParamRef = NULL, displayNA = FALSE,
                                    AUMLRef = NULL, useAURef = NULL, MLSummaryRef = NULL,
                                    excel = FALSE, overwrite = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
      "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
      "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
    ))
  }

  # Creates the data frame.
  CreateMLSummaryRef <- data.frame()

  # default Downloads file location.
  # Define the OneDrive Downloads path
  onedrive_downloads_path <- file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Downloads", "myfileRef.xlsx")

  # Define the default Downloads path
  default_downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")

  # Check if the OneDrive Downloads path exists, and prioritize it
  if (file.exists(onedrive_downloads_path)) {
    downloads_path <- onedrive_downloads_path
  } else {
    downloads_path <- default_downloads_path
  }

  # This allows a user to provide the mod 2 function TADA_GetATTAINS() as the .data data frame.
  # In this case, the ML to AU crosswalk is generated from TADA_GetATTAINS().
  if (!is.data.frame(.data)) {
    if (!any(c(
      "TADA_with_ATTAINS", "ATTAINS_catchments", "ATTAINS_points", "ATTAINS_lines", "ATTAINS_polygons"
    ) %in% names(.data))) {
      stop("Your input dataframe was not produced from `TADA_GetATTAINS()` or it was modified. Please create your list of ATTAINS features using `TADA_GetATTAINS(return_sf = TRUE)`")
    }
    # .data <- .data[["TADA_with_ATTAINS"]]
  }

  # check to see if user-supplied UseAURef is a df with appropriate columns and is filled out.
  if (!is.null(useAURef) & !is.character(useAURef)) {
    if (!is.data.frame(useAURef)) {
      stop(paste0(
        "TADA_CreateMLSummaryRef: 'useAURef' must be a data frame with these 3 columns:",
        "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.AssessmentUnitIdentifier"
      ))
    }

    if (is.data.frame(useAURef)) {
      col.names <- c(
        "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier"
      )

      ref.names <- names(useAURef)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop(paste0(
          "TADA_CreateMLSummaryRef: 'useAURef' must be a data frame with these 3 columns:",
          "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.AssessmentUnitIdentifier"
        ))
      }
    }
  }

  # check to see if user-supplied useParamRef ref is a df with appropriate columns and filled out.
  if (!is.null(useParamRef) & !is.character(useParamRef)) {
    if (!is.data.frame(useParamRef)) {
      stop(paste0(
        "TADA_CreateMLSummaryRef: 'useParamRef' must be a data frame with these 5 columns:",
        "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
        "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
      ))
    }

    if (is.data.frame(useParamRef)) {
      col.names <- c(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName"
      )

      ref.names <- names(useParamRef)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop(paste0(
          "TADA_CreateMLSummaryRef: 'useParamRef' must be a data frame with these 5 columns:",
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

  useParamRef <- dplyr::filter(useParamRef, IncludeOrExclude == "Include")

  # Identify all unique monitoring location id in the .data data frame to filter by.
  unique_ML <- unique(.data$MonitoringLocationIdentifier)

  if (displayNA == TRUE && nrow(useParamRef) < 1000 && length(unique_ML) < 1000) {
    print(paste0(
      "displayNA = TRUE: ",
      "This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your WQP data query."
    ))

    # Applies all unique combos of param and uses to each monitoring location.
    CreateMLSummaryRef <- useParamRef %>%
      tidyr::uncount(weights = length(unique_ML)) %>%
      dplyr::mutate(MonitoringLocationIdentifier = as.character(rep(unique_ML, nrow(.) / length(unique_ML)))) %>%
      dplyr::full_join(.data, by = c("MonitoringLocationIdentifier"), relationship = "many-to-many") %>%
      dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = NA) %>%
      dplyr::mutate(ATTAINS.WaterType = NA) %>%
      dplyr::mutate(SaltFresh = NA) %>%
      dplyr::mutate(UniqueSpatialCriteria = NA) %>%
      dplyr::mutate(IncludeOrExclude = "Include") %>%
      dplyr::mutate(DepthCategory = NA) %>%
      # dplyr::mutate(Flag.AssessmentNote = "Default: No spatial criteria applied.") %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier,
        MonitoringLocationIdentifier, MonitoringLocationTypeName,
        TADA.ComparableDataIdentifier = TADA.ComparableDataIdentifier.x, ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.WaterType, SaltFresh, DepthCategory,
        LongitudeMeasure, LatitudeMeasure, IncludeOrExclude, UniqueSpatialCriteria
      ) %>%
      dplyr::distinct()

    # data frame to only display sites that contains the parameter
    CreateMLSummaryRef2 <- useParamRef %>%
      tidyr::uncount(weights = length(unique_ML)) %>%
      # dplyr::mutate(MonitoringLocationIdentifier = as.character(rep(unique_ML, nrow(.) / length(unique_ML)))) %>%
      dplyr::full_join(.data, by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many") %>%
      dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = NA) %>%
      dplyr::mutate(ATTAINS.WaterType = NA) %>%
      dplyr::mutate(SaltFresh = NA) %>%
      dplyr::mutate(UniqueSpatialCriteria = NA) %>%
      dplyr::mutate(IncludeOrExclude = "Include") %>%
      dplyr::mutate(DepthCategory = NA) %>%
      dplyr::mutate(TADA.ParameterInSite.Flag = "Pass: This ML contains the parameter in your WQP data query.") %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier,
        MonitoringLocationIdentifier, MonitoringLocationTypeName,
        TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.WaterType, SaltFresh, DepthCategory,
        LongitudeMeasure, LatitudeMeasure, TADA.ParameterInSite.Flag, IncludeOrExclude, UniqueSpatialCriteria
      ) %>%
      dplyr::distinct()

    # joins the table back together and flag appropriately
    CreateMLSummaryRef <- CreateMLSummaryRef %>%
      # dplyr::bind_rows(CreateMLSummaryRef2)
      dplyr::left_join(CreateMLSummaryRef2) %>%
      dplyr::mutate(
        TADA.ParameterInSite.Flag =
          dplyr::if_else(
            is.na(TADA.ParameterInSite.Flag), "Suspect: This ML site does not contain information for this parameter in your WQP data query.",
            "Pass: This ML contains the parameter in your WQP data query."
          )
      ) %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier,
        MonitoringLocationIdentifier, MonitoringLocationTypeName,
        TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.WaterType, SaltFresh, DepthCategory,
        LongitudeMeasure, LatitudeMeasure, TADA.ParameterInSite.Flag, IncludeOrExclude, UniqueSpatialCriteria
      ) %>%
      dplyr::arrange(MonitoringLocationIdentifier)
  }

  if (displayNA == TRUE && nrow(useParamRef) > 2000 || length(unique_ML) > 2000) {
    warning(paste0(
      "displayNA = TRUE: ",
      "Too many sites or uses and parameters. Cannot assign all uses and parameters to each monitoring sites in the output. ",
      "Defaulting to displayNA = FALSE"
    ))

    displayNA <- FALSE
  }

  # If we want to exclude rows of sites with no specified parameters
  if (displayNA == FALSE) {
    print(paste0(
      "displayNA = FALSE: ",
      "This MLSummaryRef table will only display parameters and uses for a ML if it contains data collected for that TADA.CharacteristicName in your WQP data query."
    ))

    CreateMLSummaryRef2 <- useParamRef %>%
      # tidyr::uncount(weights = length(unique_ML)) %>%
      # dplyr::mutate(MonitoringLocationIdentifier = as.character(rep(unique_ML, nrow(.) / length(unique_ML)))) %>%
      dplyr::full_join(.data, by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many") %>%
      dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = NA) %>%
      dplyr::mutate(ATTAINS.WaterType = NA) %>%
      dplyr::mutate(SaltFresh = NA) %>%
      dplyr::mutate(UniqueSpatialCriteria = NA) %>%
      dplyr::mutate(IncludeOrExclude = "Include") %>%
      dplyr::mutate(DepthCategory = NA) %>%
      dplyr::mutate(TADA.ParameterInSite.Flag = "Pass: This ML contains the parameter in your WQP data query.") %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier,
        MonitoringLocationIdentifier, MonitoringLocationTypeName,
        TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.WaterType, SaltFresh, DepthCategory,
        LongitudeMeasure, LatitudeMeasure, TADA.ParameterInSite.Flag, IncludeOrExclude, UniqueSpatialCriteria
      ) %>%
      dplyr::distinct()

    CreateMLSummaryRef <- CreateMLSummaryRef2 %>%
      dplyr::arrange(MonitoringLocationIdentifier)
  }

  # If a user DOES provide a AUMLRef, this will create the Spatial Table on an AU level
  if (!is.null(AUMLRef)) {
    # NOTE: Check for required columns in AUMLRef
    # If a user provides output from TADA_GetATTAINS, select only relevant columns
    AUMLRef <- dplyr::select(
      AUMLRef,
      ATTAINS.OrganizationIdentifier, OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier,
      MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier, ATTAINS.WaterType
    )

    # If user does not provide a UseAURef, run it to pull in prior uses for AU,
    # Otherwise, if a user has already customized this and provided this useAURef, then use that table.
    if (is.null(useAURef)) {
      # Pulls in UseAURef
      stop("An AUMLRef was provided, but no UseAURef was provided. Please provide this as an argument input.")
    }

    # Only keep rows that have include
    useAURef <- useAURef %>%
      dplyr::filter(IncludeOrExclude == "Include") %>%
      dplyr::select(-IncludeOrExclude)

    # Identify all unique monitoring location id in the .data data frame to filter by.
    unique_ML <- unique(.data$MonitoringLocationIdentifier)

    # Define the user's defined uses, param, sites and AU crosswalks.
    useParamAUMLRef <- useAURef %>%
      dplyr::left_join(
        AUMLRef,
        by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier", "ATTAINS.WaterType")
      ) %>%
      dplyr::left_join(
        useParamRef,
        by = c("ATTAINS.UseName", "ATTAINS.OrganizationIdentifier")
      ) %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier,
        MonitoringLocationIdentifier,
        TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.WaterType
      )

    # Only join the AU to the CreateMLSummaryRef
    if (displayNA == TRUE) {
      print(paste0(
        "displayNA = TRUE:",
        "This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your WQP data query."
      ))


      CreateMLSummaryRef <- CreateMLSummaryRef %>%
        dplyr::left_join(
          useParamAUMLRef,
          by = dplyr::join_by(
            ATTAINS.OrganizationIdentifier, MonitoringLocationIdentifier,
            ATTAINS.ParameterName, ATTAINS.UseName, TADA.ComparableDataIdentifier
          )
        ) %>%
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier.y,
          MonitoringLocationIdentifier, MonitoringLocationTypeName,
          TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.WaterType = ATTAINS.WaterType.y, SaltFresh, DepthCategory,
          DepthCategory, LongitudeMeasure, LatitudeMeasure, TADA.ParameterInSite.Flag, IncludeOrExclude, UniqueSpatialCriteria
        ) %>%
        # dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier)) %>%
        dplyr::arrange(MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier) %>%
        dplyr::distinct()
    }

    # Filters your MLSummaryRef based on your defined uses, param, sites and AU crosswalks.
    if (displayNA == FALSE) {
      print(paste0(
        "displayNA = FALSE:",
        "This MLSummaryRef table will only display parameters and uses for a ML/AU if it contains data collected for that TADA.CharacteristicName in your WQP data query."
      ))

      CreateMLSummaryRef <- CreateMLSummaryRef %>%
        dplyr::right_join(
          useParamAUMLRef,
          by = dplyr::join_by(
            ATTAINS.OrganizationIdentifier, MonitoringLocationIdentifier,
            ATTAINS.ParameterName, ATTAINS.UseName, TADA.ComparableDataIdentifier
          )
        ) %>%
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier.y,
          MonitoringLocationIdentifier, MonitoringLocationTypeName,
          TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.WaterType = ATTAINS.WaterType.y, SaltFresh, DepthCategory,
          DepthCategory, LongitudeMeasure, LatitudeMeasure, TADA.ParameterInSite.Flag, IncludeOrExclude, UniqueSpatialCriteria
        ) %>%
        dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier)) %>%
        dplyr::filter(!is.na(MonitoringLocationIdentifier)) %>%
        dplyr::arrange(ATTAINS.ParameterName, MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier) %>%
        dplyr::distinct()
    }
  }

  if (!"ATTAINS.AssessmentUnitIdentifier" %in% colnames(CreateMLSummaryRef)) {
    print(paste0(
      "No Monitoring Location to Assessment Unit crosswalk provided. ",
      "Consider providing this crosswalk if you would like to summarize WQP data on an Assessment Unit level."
    ))
  }

  # Only run if user wants to create an excel guided spreadsheet.
  if (excel == TRUE) {
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
      wb, "CreateMLSummaryRef",
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
      wb, "CreateMLSummaryRef",
      startCol = 1,
      x = CreateMLSummaryRef,
      headerStyle = header_st
    )

    # data validation drop down list created below.
    suppressWarnings(
      openxlsx::dataValidation(
        wb,
        sheet = "CreateMLSummaryRef",
        cols = 9, rows = 2:1000,
        type = "list",
        value = sprintf("'Index'!$B$2:$B$5"),
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
      )
    )


    # Conditional Formatting
    openxlsx::conditionalFormatting(
      wb, "CreateMLSummaryRef",
      cols = 16, rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "contains",
      rule = "Include",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # default values or indicates good to go cells.

    openxlsx::conditionalFormatting(
      wb, "CreateMLSummaryRef",
      cols = 16, rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "contains",
      rule = "Exclude",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    # conditionalFormatting(wb, "CreateMLSummaryRef",
    #                       cols = 8, rows = 2:(nrow(CreateMLSummaryRef) + 1),
    #                       type = "notContains", rule = c("Exclude","Include"), style = createStyle(bgFill = "red")) # Likely error. Invalid value is possible here.
    openxlsx::conditionalFormatting(
      wb, "CreateMLSummaryRef",
      cols = 17, rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # green is default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb, "CreateMLSummaryRef",
      cols = 17, rows = 2:(nrow(CreateMLSummaryRef) + 1),
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell

    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }

    if (overwrite == FALSE) {
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }

    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")

    CreateMLSummaryRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateMLSummaryRef")
  }

  return(CreateMLSummaryRef)
}
