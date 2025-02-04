#' Expert Query Extract Params
#'
#' Return the crosswalk for params for the specified Exper Query Extract.
#'
#' @param extract enter extract type. Options are: "actions", "act_docs",
#' "assessments", "aus", "au_mls", "catch_corr", "sources", and "tmdl".
#'
#' @return A df of the params for the selecte extract
#'
#' @export
#'
EQ_ExtractParams <- function(extract = NULL)  {
  
  # select filter column
  extract.filter <- dplyr::case_when(
    extract == "actions" ~ extract,
    extract == "act_docs" ~ "action_documents",
    extract == "assessments" ~ extract,
    extract == "aus" ~ "assessment_units",
    extract == "au_mls" ~ "assessment_units_mls",
    extract == "catch_corr" ~ "catchment_correspondance",
    extract == "sources" ~ extract,
    extract == "tmdl" ~ extract
  )

  
  # import crosswalk ref file
  params.cw <- utils::read.csv(file = "inst/extdata/EQParamsCrosswalk.csv") %>%
    dplyr::filter(.data[[extract.filter]] == "yes") %>%
    dplyr::select(param, eq_name)
  
  # return the crosswalk
  return(params.cw)
}

#' Expert Query Format Params
#'
#' Format user-supplied params in the Expert Query export functions.
#'
#' @param funct The Expert Query exported function to call parameters from
#'
#' @return A df of the params for the selecte extract
#'
#' @export
#'  
  EQ_FormatParams <- function(.data) {
  # change language to character in value column
    params.df <- .data
    
    params.df$value <- sapply(params.df$value, function(x) {
    if (is.language(x)) {
      deparse(x)
    } else if (is.logical(x) || is.numeric(x)) {
      as.character(x)
    } else {
      x
    }
  }
  )
    
    return(params.df)
  }
  
  
  
  EQ_CreateBody <- function(.data) {
   
    # create param filters for POST
    params.body <- .data %>%
      dplyr::mutate(value = ifelse(param == "report_cycle" & value == "any",
                                   -1, value)) %>%
      dplyr::filter(!value %in% c("NULL", "latest"),
                    param != "api_key") %>%
      dplyr::left_join(params.cw, by = dplyr::join_by(param)) %>%
      dplyr::mutate(value = gsub('c\\(|\\)|"', '', value)) %>%
      tidyr::separate_rows(value, sep = ',\\s*') %>%
      dplyr::mutate(value = paste0('"', value, '"')) %>%
      dplyr::group_by(eq_name) %>%
      dplyr::mutate(value = paste0(value, collapse = ",")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(value = paste0('"', eq_name, '":', "[", value, "]")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = paste0(value, collapse = ",")) %>%
      dplyr::select(value) %>%
      dplyr::distinct() %>%
      dplyr::pull()
    
    # setup body for finding row count of query
    count.setup <- paste0(
      '{"filters":{',
      params.body, '}}'
    )
    
    # set up body for POST including filters, options, and columns
    body.setup <- paste0(
      '{"filters":{',
      params.body, '},',
      '"options":{"format":"csv"},',
      '"columns":["objectId","region","state","organizationType",',
      '"organizationId","organizationName","waterType","reportingCycle",',
      '"cycleLastAssessed","assessmentUnitId","assessmentUnitName","assessmentUnitStatus",',
      ' "overallStatus","epaIrCategory","stateIrCategory","useGroup","useName","useClassName",',
      '"useSupport","useIrCategory","useStateIrCategory","monitoringStartDate",',
      '"monitoringEndDate","assessmentDate","assessmentTypes","assessmentMethods",',
      '"assessmentBasis","parameterGroup","parameterName","parameterStatus",',
      '"parameterAttainment","parameterIrCategory","parameterStateIrCategory",',
      '"delisted","delistedReason","pollutantIndicator","cycleFirstListed",',
      '"alternateListingIdentifier","vision303dPriority","cwa303dPriorityRanking",',
      '"cycleScheduledForTmdl","cycleExpectedToAttain","consentDecreeCycle","cycleId",',
      '"seasonStartDate","seasonEndDate","associatedActionId","associatedActionName",',
      '"associatedActionType","associatedActionStatus","associatedActionAgency",',
      '"locationDescription","sizeSource","sourceScale","waterSize","waterSizeUnits"]}'
    )
    
    post.bodies <- list(count.setup, body.setup)
    
    rm(.data, params.body, count.setup, body.setup)
    
   return(post.bodies)
  }
    
  EQ_CreateHeader <- function(key) { # create headers for POST
    
    headers.setup <- c(
      `X-Api-Key` = key,
      `Content-Type` = "application/json",
      Accept = "application/json"
    )
    
    rm(key)
   
    return(headers.setup) 
    
  }

  EQ_PostAndContent <- function(headers, body, extract) {
    
    base.url <- "https://api.epa.gov/expertquery/api/attains/"
    
    extract.url.name <- dplyr::case_when(
      extract == "actions" ~ extract,
      extract == "act_docs" ~ "actionDocuments",
      extract == "assessments" ~ extract,
      extract == "aus" ~ "assessmentUnits",
      extract == "au_mls" ~ "assessmentUnitsMonitoringLocations",
      extract == "catch_corr" ~ "catchmentCorrespondance",
      extract == "sources" ~ extract,
      extract == "tmdl" ~ extract
    )
    
    query.url <- paste0(base.url, extract.url.name)
    
    
    row.res <- httr::POST(url = paste0(query.url, "/count"),
                          httr::add_headers(.headers = headers),
                          body = post.bodies[[1]])
    
    row.n <- httr::content(row.res, as = "parse", encoding = "UTF-8")
    
    # stop function if row count exceeds one million
    if(isTRUE(row.n$count > row.n$maxCount)) {
      stop(paste0("EQ_Assessments: The current query exceeds the maximum query size of ",
                  format(row.n$maxCount, big.mark = ","), " rows.",
                  "Please refine the search or use the Expert Query National Extract."))
    }
    
    # if row count is less than one million, print message with row count and continue
    if(isTRUE(row.n$count < row.n$maxCount)) {
      print(paste0("EQ_Assesments: The current query will return ",
                   format(row.n$count, big.mark = ","), " rows."))
    }
    
    # remove intermediate objects
    rm(count.setup, rowres, rowcont)
    
    # remove intermediate objects
    rm(params.body)
    
    query.res <- httr::POST(url = query.url, 
                            httr::add_headers(.headers = headers), 
                            body = post.bodies[[2]])
    
    query.df <- httr::content(query.res, as = "parsed", encoding = "UTF-8")
    
    # remove intermediate objects
    rm(headers, base.url, extract.url.name, query.url, row.res, row.n, body)
    
    return(query.df)
  }
  

 
  

  
 
  



#' Expert Query Assessments
#'
#' Return assessments data from Expert Query.
#'
#' @param api_key required https://owapps.epa.gov/expertquery/api-key-signup
#' @param region EPA region
#' @param statecode Two character state code
#' @param org_type
#' @param org_id ATTAINS organization identifier. More than one can be supplied.Notes on when to use
#' National Extract needed.
#' @param org_name
#' @param water_type
#' @param report_cycle reporting cycle
#' @param last_cycle_start cycle last assessed
#' @param last_cycle_end
#' @param auid
#' @param au_name
#' @param au_status Assessment unit status.Options are "Active", "Historical", "Retired", or "All".
#' More than one status can be specified (in a list).
#' @param overall_status
#' @param epa_ir_cat
#' @param state_ir_cat
#' @param use_group
#' @param use_name
#' @param use_class Use class name
#' @param use_support
#' @param use_ir_cat
#' @param use_state_ir_cat
#' @param mon_start_date
#' @param mon_end_date
#' @param assess_date
#' @param assess_types
#' @param assess_methods
#' @param assess_basis
#' @param param_group
#' @param param_name
#' @param param_status
#' @param param_attain
#' @param param_ir_cat
#' @param param_state_ir_cat
#' @param delisted
#' @param delist_reason
#' @param pollut_ind pollutant indicator
#' @param cycle_first cycle first listed
#' @param alt_list_id
#' @param vis vision 303 d priority
#' @param cwa cwa 303d priority ranking
#' @param tmdl_cycle cycleScheduledForTmdl
#' @param expect_attain_cycle cycleExpectedToAttain
#' @param cd_cycle consentDecreeCycle
#' @param cycle_id cycleId
#' @param seas_start_date
#' @param seas_end_date
#' @param aa_id associatedActionId
#' @param aa_act_type associatedActionType
#' @param aa_act_status associatedActionStatus
#' @param aa_act_agency associatedActionAgency
#' @param loc_desc locationDescription
#' @param size_source
#' @param source_scale
#' @param water_size
#' @param size_units
#'
#' @return A data frame of ATTAINS assessments served via Expert Query webservices including the
#' columns "objectId", "region", "state", "organizationType", "organizationId", "organizationName",
#' "waterType", "reportingCycle", "cycleLastAssessed", "assessmentUnitId", "assessmentUnitName",
#' "assessmentUnitStatus", "overallStatus", "epaIrCategory", "stateIrCategory", "useGroup",
#' "useName", "useClassName", "useSupport", "useIrCategory", "useStateIrCategory",
#' "monitoringStartDate", "monitoringEndDate", "assessmentDate", "assessmentTypes",
#' "assessmentMethods", "assessmentBasis", "parameterGroup", "parameterName", "parameterStatus",
#' "parameterAttainment", "parameterIrCategory", "parameterStateIrCategory", "delisted",
#' "delistedReason", "pollutantIndicator", "cycleFirstListed", "alternateListingIdentifier",
#' "vision303dPriority", "cwa303dPriorityRanking", "cycleScheduledForTmdl", "cycleExpectedToAttain",
#' "consentDecreeCycle", "cycleId", "seasonStartDate", "seasonEndDate", "associatedActionId",
#' "associatedActionName", "associatedActionType", "associatedActionStatus",
#' "associatedActionAgency", "locationDescription", "sizeSource", "sourceScale", "waterSize", and
#' "waterSizeUnits".
#'
#' @export
#'
EQ_Assessments <- function(region = NULL, statecode = NULL, org_type = NULL, org_id = NULL,
                           org_name = NULL, water_type = NULL, report_cycle = "latest",
                           last_cycle_start = NULL, last_cycle_end = NULL, auid = NULL,
                           au_name = NULL, au_status = "A",  overall_status = NULL,
                           epa_ir_cat = NULL, state_ir_cat = NULL, use_group = NULL,
                           use_name = NULL, use_class = NULL, use_support = NULL,
                           use_ir_cat = NULL, use_state_ir_cat = NULL, mon_start_date = NULL,
                           mon_end_date = NULL, assess_date = NULL, assess_types = NULL,
                           assess_methods = NULL, assess_basis = NULL, param_group = NULL,
                           param_name = NULL, param_status = NULL, param_attain = NULL,
                           param_ir_cat = NULL, param_state_ir_cat = NULL, delisted = NULL,
                           delist_reason = NULL, pollut_ind = NULL, cycle_first = NULL,
                           alt_list_id = NULL, vis = NULL, cwa = NULL, tmdl_cycle = NULL,
                           expect_attain_cycle = NULL, cd_cycle = NULL, cycle_id = NULL,
                           seas_start_date = NULL, seas_end_date = NULL, aa_id = NULL,
                           aa_act_type = NULL, aa_act_status = NULL, aa_act_agency = NULL,
                           loc_desc = NULL, size_source = NULL, source_scale = NULL,
                           water_size = NULL, size_units = NULL, api_key = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Assessments: An api key is required to access EQ web services.")
  }
  
  # import crosswalk ref file
  params.cw <- utils::read.csv(file = "inst/extdata/EQParamsCrosswalk.csv") %>%
    dplyr::filter(assessments == "yes") %>%
    dplyr::select(param, eq_name)


  # create param filters for POST
  params.body <- params.df %>%
    dplyr::mutate(value = ifelse(param == "report_cycle" & value == "any",
                                 -1, value)) %>%
    dplyr::filter(!value %in% c("NULL", "latest"),
                  param != "api_key") %>%
    dplyr::left_join(params.cw, by = dplyr::join_by(param)) %>%
    dplyr::mutate(value = gsub('c\\(|\\)|"', '', value)) %>%
    tidyr::separate_rows(value, sep = ',\\s*') %>%
    dplyr::mutate(value = paste0('"', value, '"')) %>%
    dplyr::group_by(eq_name) %>%
    dplyr::mutate(value = paste0(value, collapse = ",")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(value = paste0('"', eq_name, '":', "[", value, "]")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = paste0(value, collapse = ",")) %>%
    dplyr::select(value) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  # remove intermediate objects
  rm(params.cw, params.df)
  
  # create headers for POST
  headers.setup <- c(
    `X-Api-Key` = api_key,
    `Content-Type` = "application/json",
    Accept = "application/json"
  )
  
  # setup body for finding row count of query
  count.setup <- paste0(
    '{"filters":{',
    params.body, '}}'
  )
  
  # get row count of query
  rowres <- httr::POST(url = "https://api.epa.gov/expertquery/api/attains/assessments/count", httr::add_headers(.headers=headers.setup), body = count.setup)
  
  rowcont <- httr::content(rowres, as = "parsed", encoding = "UTF-8")
  
  # stop function if row count exceeds one million
  if(isTRUE(rowcont$count > rowcont$maxCount)) {
    stop(paste0("EQ_Assessments: The current query exceeds the maximum query size of ",
                format(rowcont$maxCount, big.mark = ","), " rows.",
                "Please refine the search or use the Expert Query National Extract."))
  }
  
  # if row count is less than one million, print message with row count and continue
  if(isTRUE(rowcont$count < rowcont$maxCount)) {
    print(paste0("EQ_Assesments: The current query will return ",
                 format(rowcont$count, big.mark = ","), " rows."))
  }
  
  # remove intermediate objects
  rm(count.setup, rowres, rowcont)

  # set up body for POST including filters, options, and columns
  body.setup <- paste0(
    '{"filters":{',
    params.body, '},',
    '"options":{"format":"csv"},',
    '"columns":["objectId","region","state","organizationType",',
    '"organizationId","organizationName","waterType","reportingCycle",',
    '"cycleLastAssessed","assessmentUnitId","assessmentUnitName","assessmentUnitStatus",',
    ' "overallStatus","epaIrCategory","stateIrCategory","useGroup","useName","useClassName",',
    '"useSupport","useIrCategory","useStateIrCategory","monitoringStartDate",',
    '"monitoringEndDate","assessmentDate","assessmentTypes","assessmentMethods",',
    '"assessmentBasis","parameterGroup","parameterName","parameterStatus",',
    '"parameterAttainment","parameterIrCategory","parameterStateIrCategory",',
    '"delisted","delistedReason","pollutantIndicator","cycleFirstListed",',
    '"alternateListingIdentifier","vision303dPriority","cwa303dPriorityRanking",',
    '"cycleScheduledForTmdl","cycleExpectedToAttain","consentDecreeCycle","cycleId",',
    '"seasonStartDate","seasonEndDate","associatedActionId","associatedActionName",',
    '"associatedActionType","associatedActionStatus","associatedActionAgency",',
    '"locationDescription","sizeSource","sourceScale","waterSize","waterSizeUnits"]}'
  )
  
  # remove intermediate objects
  rm(params.body)
  
  res <- httr::POST(url = "https://api.epa.gov/expertquery/api/attains/assessments", httr::add_headers(.headers=headers.setup), body = body.setup)
  
  df <- httr::content(res, as = "parsed", encoding = "UTF-8")
  
  # remove intermediate objects
  rm(headers.setup, body.setup, res)
  
  return(df)
}




