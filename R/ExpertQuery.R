#' Expert Query Actions
#'
#' Return actions data from Expert Query.
#'
#' @param api_key
#' @param act_agency      
#' @param act_id
#' @param act_name
#' @param act_type
#' @param au_name
#' @param auid
#' @param comp_date_end
#' @param comp_date_start 
#' @param fisc_year_end
#' @param fisc_year_start
#' @param in_meas
#' @param indian_country
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param param_group
#' @param region
#' @param statecode
#' @param water_type
#'
#' @return A data frame of ATTAINS actions served via Expert Query webservices including 
#' the columns "objectId", "region", "state", "organizationType", "organizationId",  
#' "organizationName", "waterType", "assessmentUnitId", "assessmentUnitName", "parameterGroup", 
#' "locationDescription", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_Actions <- function(api_key = NULL, act_agency = NULL, act_id = NULL, act_name = NULL, 
                       act_type = NULL, au_name = NULL, auid = NULL, comp_date_end = NULL,
                       comp_date_start = NULL, fisc_year_end = NULL, fisc_year_start = NULL,
                       in_meas = NULL, indian_country = NULL, obj_id = NULL, org_id = NULL,
                       org_name = NULL, org_type = NULL, param = NULL, param_group = NULL,
                       region = NULL, statecode = NULL, water_type = NULL)  {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Actions: An api key is required to access EQ web services.")
  }
  
  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "actions")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_Actions) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "actions")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "actions")
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}

#' Expert Query Actions Documents
#'
#' Return actions documents data from Expert Query.
#'
#' @param api_key
#' @param act_id
#' @param act_name
#' @param act_type
#' @param comp_date_end
#' @param comp_date_start
#' @param doc_file_name
#' @param doc_key
#' @param doc_name
#' @param doc_query
#' @param doc_type
#' @param doc_url
#' @param file_type
#' @param fisc_year_start
#' @param obj_id 
#' @param org_id
#' @param org_name
#' @param region
#' @param statecode
#' @param tmdl_date_end
#' @param tmdl_date_start
#'
#' @return A data frame of ATTAINS actions documents served via Expert Query webservices including 
#' the columns "objectId", "organizationName", "organizationType", "region", "state", "tmdlDate",'
#' "documentDesc", "documentFileName", "documentFileTypeName", "documentKey", "documentName",
#' and "actionDocumentType".
#'
#' @export
#'
EQ_ActionsDocuments <- function(api_key = NULL, act_id = NULL, act_name = NULL, act_type = NULL,
                                comp_date_end = NULL, comp_date_start = NULL, doc_file_name = NULL,
                                doc_key = NULL, doc_name = NULL, doc_query = NULL, doc_type = NULL, 
                                doc_url = NULL, file_type = NULL, fisc_year_start = NULL, 
                                obj_id = NULL, org_id = NULL, org_name = NULL, region = NULL, 
                                statecode = NULL, tmdl_date_end = NULL, tmdl_date_start = NULL) {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_ActionsDocuments: An api key is required to access EQ web services.")
  }
  
  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "act_docs")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_ActionsDocuments) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "act_docs")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "act_docs")
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}

#' Expert Query Assessments
#'
#' Return assessments data from Expert Query.
#'
#' @param api_key required https://owapps.epa.gov/expertquery/api-key-signup
#' @param act_agency
#' @param act_status          
#' @param act_type
#' @param act_id              
#' @param alt_list_id         
#' @param assess_basis        
#' @param assess_date        
#' @param assess_methods
#' @param assess_types
#' @param au_name
#' @param au_status
#' @param auid
#' @param cd_cycle
#' @param cwa
#' @param cycle_first
#' @param cycle_id
#' @param delist_reason
#' @param delisted
#' @param epa_ir_cat
#' @param expect_attain_cycle 
#' @param last_cycle_end
#' @param last_cycle_start    
#' @param loc_desc
#' @param mon_end_date        
#' @param mon_start_date
#' @param org_id
#' @param org_name
#' @param org_type
#' @param overall_status
#' @param param_attain
#' @param param_group
#' @param param_ir_cat
#' @param param_name
#' @param param_state_ir_cat  
#' @param param_status
#' @param pollut_ind
#' @param region
#' @param report_cycle
#' @param seas_end_date
#' @param seas_start_date
#' @param size_source
#' @param size_units
#' @param source_scale
#' @param state_ir_cat
#' @param statecode
#' @param tmdl_cycle
#' @param use_class
#' @param use_group
#' @param use_ir_cat
#' @param use_name
#' @param use_state_ir_cat
#' @param use_suppor"         
#' @param vis                 
#' @param water_size          
#' @param water_type 
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
EQ_Assessments <- function(api_key = NULL, act_agency = NULL, act_status = NULL, act_type = NULL,
                           act_id = NULL, alt_list_id = NULL, assess_basis = NULL, 
                           assess_date = NULL, assess_methods = NULL, assess_types = NULL,
                           au_name = NULL, au_status = "A", auid = NULL, cd_cycle = NULL,
                           cwa = NULL, cycle_first = NULL, cycle_id = NULL, delist_reason = NULL,
                           delisted = NULL, epa_ir_cat = NULL, expect_attain_cycle = NULL,
                           last_cycle_end = NULL, last_cycle_start = NULL, loc_desc = NULL,
                           mon_end_date = NULL, mon_start_date = NULL, org_id = NULL,
                           org_name = NULL, org_type = NULL, overall_status = NULL,
                           param_attain = NULL, param_group = NULL, param_ir_cat = NULL,
                           param_name = NULL, param_state_ir_cat = NULL, param_status = NULL,
                           pollut_ind = NULL, region = NULL, report_cycle = "latest",
                           seas_end_date = NULL, seas_start_date = NULL, size_source = NULL,
                           size_units = NULL, source_scale = NULL, state_ir_cat = NULL,
                           statecode = NULL, tmdl_cycle = NULL, use_class = NULL, use_group = NULL,
                           use_ir_cat = NULL, use_name = NULL, use_state_ir_cat = NULL,
                           use_support = NULL, vis = NULL, water_size = NULL, water_type = NULL) {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Assessments: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "assessments")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_Assessments) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "assessments")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "assessments")
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}

#' Expert Query Assessment Units
#'
#' Return assessment units data from Expert Query.
#'
#' @param api_key
#' @param au_name
#' @param au_status
#' @param au_id
#' @param cycle_id
#' @param loc_txt
#' @param loc_type
#' @param region
#' @param report_cyle
#' @param statecode
#' @param use_class
#' @param water_type
#'
#' @return A data frame of ATTAINS assessment units served via Expert Query webservices including 
#' the columns "region", "state", "organizationType", "organizationId", "organizationName",
#' "waterType", "locationTypeCode", "locationText", "useClassName", "assessmentUnitId",
#' "assessmentUnitName", "assessmentUnitStatus", "reportingCycle", "cycleId", 
#' "locationDescription", "sizeSource", "sourceScale", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_AssessmentUnits <- function(api_key = NULL, au_name = NULL, au_status = "A", auid = NULL, 
                               cycle_id = NULL, loc_txt = NULL, loc_type = NULL, region = NULL, 
                               report_cycle = NULL, statecode = NULL, use_class = NULL)  {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_AssessmentUnits: An api key is required to access EQ web services.")
  }
  
  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "aus")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_AssessmentUnits) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "aus")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "aus")
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}

#' Expert Query Assessment Units with Monitoring Locations
#'
#' Return assessment units with monitoring locations data from Expert Query.
#'
#' @param api_key
#' @param au_name
#' @param au_status
#' @param auid
#' @param cycle_id
#' @param mon_loc_id
#' @param mon_loc_org
#' @param nhd_id
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param region
#' @param report_cycle
#' @param statecode
#' @param use_class
#' @param water_type 
#'
#' @return A data frame of ATTAINS assessment units with monitoring locations served via Expert 
#' Query webservices including the columns "objectId", "region", "state", "organizationType", 
#' "organizationId", "organizationName", "waterType", "useClassName", "monitoringLocationId", 
#' "monitoringLocationOrgId", "assessmentUnitId", "assessmentUnitName", "assessmentUnitStatus", 
#' "reportingCycle", "locationDescription", "monitoringLocationDataLink", "sizeSource", 
#' "sourceScale", "waterSize", an "waterSizeUnits".
#'
#' @export
#'
EQ_AUsMLs <- function(api_key = NULL, au_name = NULL, au_status = "A", auid = NULL,
                      cycle_id = NULL, mon_loc_id = NULL, mon_loc_org = NULL,  nhd_id = NULL,
                      obj_id = NULL, org_id = NULL, org_name = NULL, org_type = NULL,
                      region = NULL, report_cycle = "latest", statecode = NULL, use_class = NULL,
                      water_type = NULL)  {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_AUsMLs: An api key is required to access EQ web services.")
  }
  
  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "au_mls")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_AUsMLs) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "au_mls")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "au_mls")
  # should rows where ml is NA be filtered out?
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}

#' Expert Query Catchment Correspondence
#'
#' Return catchment correspondence data from Expert Query.
#'
#' @param api_key
#' @param au_name
#' @param auid
#' @param cycle_id
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param region       
#' @param report_cycle
#' @param statecode   
#'
#' @return A data frame of ATTAINS catchment correspondence served via Expert Query webservices 
#' including the columns "region", "state", "organizationType", "organizationId", 
#' "organizationName", "waterType", "locationTypeCode", "locationText", "useClassName", 
#' "assessmentUnitId", "assessmentUnitName", "assessmentUnitStatus", "reportingCycle", "cycleId", 
#' "locationDescription", "sizeSource", "sourceScale", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_CatchCorr <- function(api_key = NULL, au_name = NULL, auid = NULL, cycle_id = NULL, 
                         obj_id = NULL, org_id = NULL, org_name = NULL, org_type = NULL,
                         region = NULL, report_cycle = "latest", statecode = NULL)  {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_CatchCorr: An api key is required to access EQ web services.")
  }
  
  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "catch_corr")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_CatchCorr) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "catch_corr")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "catch_corr")
  # should rows where ml is NA be filtered out?
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}

#' Expert Query Sources
#'
#' Return sources data from Expert Query.
#'
#' @param api_key
#' @param au_name
#' @param auid
#' @param cause
#' @param confirmed
#' @param cycle_id
#' @param epa_ir_cat
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param overall_status
#' @param param_group
#' @param region
#' @param report_cycle
#' @param source
#' @param state_ir_cat
#' @param statecode
#' @param water_type  
#'
#' @return A data frame of ATTAINS sources served via Expert Query webservices including 
#' the columns "objectId", "region", "state", "organizationType", "organizationId", 
#' "organizationName", "waterType", "assessmentUnitId", "assessmentUnitName", "reportingCycle", 
#' "overallStatus", "epaIrCategory", "stateIrCategory", "parameterGroup", "causeName", 
#' "sourceName", "confirmed", "cycleId", "locationDescription", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_Sources <- function(api_key = NULL, au_name = NULL, auid = NULL, cause = NULL,
                       confirmed = NULL, cycle_id = NULL, epa_ir_cat = NULL, obj_id = NULL,
                       org_id = NULL, org_name = NULL, org_type = NULL, 
                       overall_status = NULL, param_group = NULL, region = NULL, 
                       report_cycle = "latest", source = NULL, state_ir_cat = NULL,
                       statecode = NULL, water_type = NULL)  {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Sources: An api key is required to access EQ web services.")
  }
  
  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "sources")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_Sources) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "sources")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "sources")
  # should rows where ml is NA be filtered out?
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}

#' Expert Query TMDLs
#'
#' Return tmdl data from Expert Query.
#'
#' @param api_key
#' @param act_agency
#' @param act_id
#' @param act_name
#' @param au_name
#' @param auid
#' @param comp_date_end
#' @param comp_date_start
#' @param fisc_year_end
#' @param fisc_year_start
#' @param in_meas
#' @param indian_country
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param region
#' @param statecode
#' @param tmdl_date_end
#' @param tmdl_date_start
#' @param water_type
#' @param ad_param
#' @param ad_param_group
#' @param mos_exp
#' @param mos_imp
#' @param npdes_id
#' @param other_id
#' @param pollutant
#' @param poll_group
#' @param source_type   
#'
#' @return A data frame of ATTAINS tmdls served via Expert Query webservices including 
#' the columns "objectId", "region", "state", "organizationType", "organizationId", 
#' "organizationName", "waterType", "pollutantGroup", "pollutant", "addressedParameterGroup",
#' "addressedParameter", "sourceType", "npdesIdentifier", "otherIdentifier", "actionId",
#' "actionName", "actionAgency", "inIndianCountry", "explicitMarginOfSafety",
#' "implicitMarginOfSafety", "includeInMeasure", "completionDate", "tmdlDate", 
#' "fiscalYearEstablished", "assessmentUnitId", "assessmentUnitName", "loadAllocation", 
#' "loadAllocationUnits", "locationDescription", "tmdlEndpoint", "waterSize", "waterSizeUnits",
#' "wasteLoadAllocation", and "planSummaryLink".
#'
#' @export
#'
EQ_TMDLs <- function(api_key = NULL, act_agency = NULL, act_id = NULL, act_name = NULL, 
                     au_name = NULL, auid = NULL, comp_date_end = NULL, comp_date_start = NULL,
                     fisc_year_end = NULL, fisc_year_start = NULL, in_meas = NULL, 
                     indian_country = NULL,  obj_id = NULL, org_id = NULL, org_name = NULL,
                     org_type = NULL, region = NULL, statecode = NULL, tmdl_date_end = NULL,
                     tmdl_date_start = NULL, water_type = NULL, ad_param = NULL, 
                     ad_param_group = NULL, mos_exp = NULL, mos_imp = NULL, npdes_id = NULL,
                     other_id = NULL, pollutant = NULL, poll_group = NULL, source_type = NULL)  {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_TMDLs: An api key is required to access EQ web services.")
  }
  
  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "tmdl")
  
  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_TMDLs) %>%
    # format for building body
    EQ_FormatParams()
  
  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()
  
  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)
  
  # remove intermediate objects
  rm(user.params, default.params)
  
  # create post bodies
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw, extract = "tmdl")
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "tmdl")
  # should rows where ml is NA be filtered out?
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}
