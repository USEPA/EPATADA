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
EQ_Assessments <- function(region = NULL, statecode = c("AK", "CA"), org_type = NULL, org_id = NULL,
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

  # import crosswalk ref file
  params.cw <- utils::read.csv(file = "inst/extdata/EQParamsCrosswalk.csv")

  # create df of function formals
    params.df <- formals(EQ_Assessments) %>%
    as.list() %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame()

  
  # create list of params with non-null values to be included in request body
  params.body <- params.df %>%
    dplyr::mutate(value = ifelse(param == "report_cycle" & value == "any",
                                 -1, value)) %>%
    dplyr::filter(!value %in% c("NULL", "latest")) %>%
    dplyr::left_join(params.cw, by = dplyr::join_by(param))
  
  # set up filters
  filter.setup <- params.body %>%
    dplyr::mutate(value = paste0('"', value, '"'))
    
    
    paste0('"', params.body$eq_name[1], '":', '[', params.body$value, ']')
  
  

# headers <- c(
#   `X-Api-Key` = api_key,
#   `Content-Type` = "application/json",
#   Accept = "application/json"
}


# post_setup <- function()
#
 data <- '{"filters":{"assessmentUnitStatus":["A"],"organizationId":["AKDECWQ"],"state":["AK"]},"options":{"format":"csv"},"columns":["objectId","region","state","organizationType","organizationId","organizationName","waterType","reportingCycle","cycleLastAssessed","assessmentUnitId","assessmentUnitName","assessmentUnitStatus","overallStatus","epaIrCategory","stateIrCategory","useGroup","useName","useClassName","useSupport","useIrCategory","useStateIrCategory","monitoringStartDate","monitoringEndDate","assessmentDate","assessmentTypes","assessmentMethods","assessmentBasis","parameterGroup","parameterName","parameterStatus","parameterAttainment","parameterIrCategory","parameterStateIrCategory","delisted","delistedReason","pollutantIndicator","cycleFirstListed","alternateListingIdentifier","vision303dPriority","cwa303dPriorityRanking","cycleScheduledForTmdl","cycleExpectedToAttain","consentDecreeCycle","cycleId","seasonStartDate","seasonEndDate","associatedActionId","associatedActionName","associatedActionType","associatedActionStatus","associatedActionAgency","locationDescription","sizeSource","sourceScale","waterSize","waterSizeUnits"]}'
#
# res <- httr::POST(url = "https://api.epa.gov/expertquery/api/attains/assessments", httr::add_headers(.headers=headers), body = data)
#
# df <- httr::content(res, as = "text", encoding = "UTF-8")




#statecode = "AK"
