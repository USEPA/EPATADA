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

#' Expert Query Default Params
#'
#' Format user-supplied params in the Expert Query export functions.
#'
#' @param funct The Expert Query exported function to call parameters from
#'
#' @return A df of the params for the selecte extract
#'
#' @export

EQ_DefaultParams <- function(func) {
  
# create df of function formals
params.df <- formals(func) %>%
  as.list() %>%
  tibble::enframe(name = "param", value = "value") %>%
  as.data.frame() 

return(params.df)
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
  
  params.df <- params.df %>%
    dplyr::mutate(value = as.character(value))
  
  return(params.df)
}

#' Expert Query Compare Params
#'
#' Compare user-supplied and default params in the Expert Query export functions to create df of
#' all params that should be used to build body for post request.
#'
#' @param funct The Expert Query exported function to call parameters from
#'
#' @return A df of the params for the selecte extract
#'
#' @export
#'  
EQ_CompareParams <- function(default, user) {
  
  # filter out any default params that user entered a value for
  default.params <- default %>%
    dplyr::filter(!param %in% user$param)
  
  # combine user supplied and default params
  all.params <- user %>%
    dplyr::full_join(default.params, by = names(user))
  
  # remove intermediate objects
  rm(default.params, default, user)
  
  # return all params for use in body
  return(all.params)
}
  
  
  
  EQ_CreateBody <- function(.data, crosswalk) {
   
    # create param filters for POST
    params.body <- .data %>%
      dplyr::mutate(value = ifelse(param == "report_cycle" & value == "any",
                                   -1, value)) %>%
      dplyr::filter(!value %in% c("NULL", "latest"),
                    param != "api_key") %>%
      dplyr::left_join(crosswalk, by = dplyr::join_by(param)) %>%
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

  EQ_PostAndContent <- function(headers, body.list, extract) {
    
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
                          body = body.list[[1]])
    
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
    rm(row.res, row.n)
    
    query.res <- httr::POST(url = query.url, 
                            httr::add_headers(.headers = headers), 
                            body = body.list[[2]])
    
    query.df <- suppressWarnings(httr::content(query.res, as = "parsed", encoding = "UTF-8"))
    
    # remove intermediate objects
    rm(headers, base.url, extract.url.name, query.url, body.list)
    
    return(query.df)
  }