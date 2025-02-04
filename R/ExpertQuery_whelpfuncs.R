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
EQ_Assessments <- function(..., default.params = list(region = NULL, statecode = NULL, org_type = NULL, org_id = NULL,
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
                           water_size = NULL, size_units = NULL, api_key = NULL))  {
  
  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Assessments: An api key is required to access EQ web services.")
  }

  params.cw <- EQ_ExtractParams(extract = "assessments")
  
  params.default <- EQ_FormatParams(EQ_Assessments) %>%
    as.list()
  
  
 #  # create df of function formals
 # default.params <- formals(EQ_Assessments) %>%
 #    dplyr::slice_tail(n = 1)
 #  
 #  # change language to character in value column
 #  params.df$value <- sapply(params.df$value, function(x) {
 #    if (is.language(x)) {
 #      deparse(x)
 #    } else if (is.logical(x) || is.numeric(x)) {
 #      as.character(x)
 #    } else {
 #      x
 #    }
 #  }
 #  )
 #  
  
  # create df of function match.calls
  user.params <- list(...)
  
  combined.params <- defaults <- default.params
  
  combined.params[names(user.params)] <- user.params
  
  extra.params <- user.params[!names(user.params) %in% names(default.params)]
  
  combined.params <- c(combined.params, extra.params)
  
  params.df <- tibble::enframe(combined.params, name = "param", value = "value")
  
  params.df <- EQ_FormatParams(params.df)
  
  post.bodies <- EQ_CreateBody(params.df)
  
  post.headers <- EQ_CreateHeader(key = api_key)
  
  query.df <- EQ_PostAndContent(headers = post.headers, body = post.bodies, extract = "assessments")
  
  rm(params.cw, params.df, post.bodies, post.headers)
  
  return(query.df)
}