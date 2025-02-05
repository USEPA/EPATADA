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
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw)
  
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
#' the columns "objectId", "region", "state", "organizationType", "organizationId", 
#' "organizationName", "waterType", "reportingCycle", "assessmentUnitId", "assessmentUnitName",
#' "assessmentUnitStatus", "useClassName", "cycleId", "locationDescription", "sizeSource", 
#' "sourceScale", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_AssessmentUnits <- function(au_name = NULL, au_status = "A", auid = NULL, cycle_id = NULL,
                               loc_txt = NULL, loc_type = NULL, region = NULL, report_cycle = NULL,
                               statecode = NULL, use_class = NULL, api_key = NULL)  {
  
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
  post.bodies <- EQ_CreateBody(.data = params.df, crosswalk = params.cw)
  
  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)
  
  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers, 
                                body.list = post.bodies, 
                                extract = "aus")
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}