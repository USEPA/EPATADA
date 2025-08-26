#' Define Criteria and Methodology
#'
#' It is recommended to run the three TADA reference functions in order of
#' [TADA_CreateParamRef()], [TADA_CreateUseParamRef], and [TADA_CreateMLSummaryRef] 
#' to generate the Criteria and Methodology table specific for your organization.
#' However, users can choose to proceed with an 'auto_assign'
#' option which will use default assignments during each of these three functions.
#' If you would like to update any of these reference tables from the defaults, 
#' you can choose to do so in the excel spreadsheet file and then specify the starting 
#' reference table that you have updated with the argument input 'updateRef' 
#' This which will update the Criteria and Methodology table to reflect any changes made.
#'
#' This criteria and methodology table will be in a TADA compatible format and 
#' contain a list of allowable values within each column to define the full 
#' criteria or magnitude only values associated with an ATTAINS parameter name 
#' and use name. For each criteria/magnitude value,
#' users will need to ensure they properly define any additional methods that
#' correctly reflects their water quality standards for a parameter and use.
#' For example, if there are separate standards for acute versus chronic,
#' rivers versus estuary, different seasons, etc., then a user will need to create
#' additional rows to reflect this. Additional columns are included in this output
#' to capture data sufficiency considerations.
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
#' @param criteriaMethods An optional data frame which contains the completed criteria
#' and methodology table completed. This will be a user supplied table and any
#' inputs in this table will be prioritized. Additional rows for any parameter(s)
#' and use(s) combinations that are not found in the user supplied table will be 
#' included in the output. These rows will need the criteria and methodology inputs
#' filled out accordingly.
#'
#' @param MLSummaryRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit. For any unique groupings of sites, this 
#' input is recommended.
#'
#' @param MLAURef An optional data frame input. This data frame
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
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function. 
#' This helps prevent users from overwriting their progress.
#'
#' @return A data frame with the criteria and methodology table in TADA format.
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
#'   waterUseParamRef = NULL, useAURef = NULL, MLAURef = NULL,
#'   useParamRef = UseParamRef_UT,
#'   excel = FALSE
#' )
#'
#' DefineCriteriaMethodology_UT <- TADA_DefineCriteriaMethodology(
#'   Data_Nutrients_UT,
#'   MLSummaryRef = MLSummaryRef_UT,
#'   excel = FALSE
#' )
#'
TADA_DefineCriteriaMethodology <- function(.data,  MLSummaryRef = NULL, # required inputs for the recommended workflow
                                           criteriaMethods = NULL, epa304a = FALSE, # ref = c("ATTAINS", "CST", "TADA", "Other") future development to consider additional crosswalk alternatives?
                                           auto_fill = FALSE, org_id = NULL, MLAURef = NULL, useAURef = NULL, # Optional if auto_assign = TRUE
                                           updateRef = c("none", "paramRef", "useParamRef", "MLSummaryRef"), # hierarchical dependency
                                           excel = TRUE, overwrite = FALSE) {
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # updateRef defaults to "none" if one is not entered
  if (is.null(updateRef)) {
    updateRef = "none"
  }
  
  # ensures updateRef is an allowable entry
  updateRef <- match.arg(updateRef)
  
  # Ensures you have used a valid auto_assign name
  if (!updateRef %in% c("none", "paramRef", "useParamRef", "MLSummaryRef")) {
    stop(paste0(
      "TADA_DefineCriteriaMethodology: ",
      "argument input ", updateRef, " is not a valid entry. Please type one of 'None', 'paramRef', 'useParamRef', 'MLSummaryRef' as a value."
    ))
  }
  
  # Invalid function input combos - can only use updateRef =  none with auto_fill = FALSE
  if (auto_fill == FALSE && updateRef != "none") {
    stop("TADA_DefineCriteriaMethodology: auto_fill = FALSE. The updateRef function input must be none. If you have updated a reference table, use auto_fill == TRUE")
  }
  
  # Generates a blank Criteria and Methods file
  if (auto_fill == FALSE && is.null(MLSummaryRef) && epa304a == FALSE) {
    desired_cols <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "AcuteChronic",
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "TADA.DepthCategory.Flag", "ApplyUniqueSpatialCriteria",
      # Criteria Columns
      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
      "DurationValue",	"DurationUnit", "DurationAggregation",
      "FrequencyCriteriaValue",	"FrequencyCriteriaMethod",
      # Data Sufficiency Columns
      "DataSufficiency.AssessPeriod", "DataSufficiency.BegAssessDate", "DataSufficiency.EndAssessDate",
      "DataSufficiency.Season", "DataSufficiency.SeasonBegDate", "DataSufficiency.SeasonEndDate",
      "DataSufficiency.CountSamplingDistribution", "DataSufficiency.SamplingDistribution", "DataSufficiency.MinSamplePerDistribution"
    )
    
    DefineCriteriaMethodology <- data.frame(matrix(ncol = length(desired_cols), nrow = 0))
    
    names(DefineCriteriaMethodology) <- desired_cols
    
    cols_to_convert <- c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
                         "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "AcuteChronic",
                         # Spatial Columns
                         "ATTAINS.WaterType", "SaltFresh", "TADA.DepthCategory.Flag", "ApplyUniqueSpatialCriteria")
    
    DefineCriteriaMethodology[c(cols_to_convert)] <- lapply(DefineCriteriaMethodology[cols_to_convert], as.character)
    
    TADA_ParamRef <- TADA_CreateParamRef(
      .data = .data, 
      org_id = org_id, 
      excel = excel, 
      overwrite = overwrite
    )
    
    TADA_UseParamRef <- TADA_CreateUseParamRef(
      .data,
      paramRef = TADA_ParamRef,
      org_id = org_id, 
      excel = excel, 
      overwrite = overwrite
    )
    
    TADA_MLSummaryRef <- TADA_CreateMLSummaryRef(
      .data,
      useParamRef = TADA_UseParamRef,
      org_id = org_id, 
      excel = excel, 
      overwrite = overwrite)
  }
  
  # If user wants to create a pre-populated CriteriaMethods table, it will run all crosswalk tables and use the default.
  # Users can edit one or more of the ref files which will update all accordingly.
  if (auto_fill == TRUE) {
    # default, runs all reference tables with no user edits
    if(updateRef == "none") {
      message(paste0("auto_fill = TRUE selected. Running TADA_CreateParamRef with default assignment. Please review this paramRef table output."))
      TADA_ParamRef <- TADA_CreateParamRef(  
        .data, 
        org_id = org_id,
        auto_assign = "All", # auto-populate any exact matches found between WQP CharacteristicName and ATTAINS ParameterName
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      message(paste0("auto_fill = TRUE selected. Running TADA_CreateUseParamRef with default assignment. Please review this Use to paramRef table output."))
      TADA_UseParamRef <- TADA_CreateUseParamRef(  
        .data, 
        org_id = org_id,
        paramRef = TADA_ParamRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      message(paste0("auto_fill = TRUE selected. Running TADA_CreateMLSummaryRef with default assignment. Please review  this sites Ref table output."))
      MLSummaryRef <- TADA_CreateMLSummaryRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        MLAURef = MLAURef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
    
    # user only updates paramRef. This will update paramRef, useParamRef, and MLSummaryRef based on these modifications.
    if (updateRef == "paramRef") {
      message(paste0("auto_fill = TRUE and updateRef = paramRef selected. Running TADA_CreateParamRef with use supplied paramRef assignment. Please review this paramRef table output."))
      myfile_ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef") 
      
      TADA_ParamRef <- TADA_CreateParamRef(  
        .data, 
        org_id = org_id,
        paramRef = myfile_ParamRef,
        auto_assign = "None", # User has now edited the table, turn the auto_assign of in TADA_CreateParamRef
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      TADA_UseParamRef <- TADA_CreateUseParamRef(  
        .data, 
        org_id = org_id,
        paramRef = TADA_ParamRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      MLSummaryRef <- TADA_CreateMLSummaryRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        MLAURef = MLAURef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
      
    # user only updates useParamRef. This will update useParamRef, MLSummaryRef based on this modifications.
    if (updateRef == "useParamRef") {
      message(paste0("auto_assign = TRUE and updateRef = useParamRef selected. Running TADA_CreateParamRef with use supplied paramRef assignment. Please review this paramRef table output."))
      myfile_UseParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseParamRef") 
      
      TADA_ParamRef <- TADA_CreateParamRef(  
        .data, 
        org_id = org_id,
        paramRef = myfile_UseParamRef, # will update paramRef based on useParamRef
        auto_assign = "All",
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      TADA_UseParamRef <- TADA_CreateUseParamRef(  
        .data, 
        org_id = org_id,
        paramRef = TADA_ParamRef,
        useParamRef = myfile_UseParamRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      MLSummaryRef <- TADA_CreateMLSummaryRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        MLAURef = MLAURef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
    
    # user only updates MLSummaryRef in excel. This will update MLSummaryRef based on this modifications.
    if (updateRef == "MLSummaryRef") {
      message(paste0("auto_assign = TRUE and updateRef = MLSummaryRef selected. Running TADA_CreateMLSummaryRef with use supplied paramRef assignment. Please review this paramRef table output."))
      myfile_MLSummaryRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateMLSummaryRef") 
      
      TADA_ParamRef <- TADA_CreateParamRef(  
        .data, 
        org_id = org_id,
        paramRef = myfile_MLSummaryRef, # will update paramRef based on useParamRef
        auto_assign = "All",
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      TADA_UseParamRef <- TADA_CreateUseParamRef(  
        .data, 
        org_id = org_id,
        paramRef = TADA_ParamRef,
        useParamRef = myfile_MLSummaryRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      MLSummaryRef <- TADA_CreateMLSummaryRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        MLAURef = MLAURef,
        MLSummaryRef = myfile_MLSummaryRef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(MLSummaryRef) & !is.character(MLSummaryRef)) {
    if (!is.data.frame(MLSummaryRef)) {
      stop("TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, ApplyUniqueSpatialCriteria,
        ATTAINS.WaterType, ATTAINS.assessmentunitidentifier")
    }
    
    if (is.data.frame(MLSummaryRef)) {
      col.names <- c(
        "ATTAINS.ParameterName",
        "ATTAINS.UseName",
        "ATTAINS.OrganizationIdentifier",
        "ApplyUniqueSpatialCriteria",
        "ATTAINS.WaterType",
        "ATTAINS.assessmentunitidentifier"
      )
      
      ref.names <- names(MLSummaryRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, ApplyUniqueSpatialCriteria,
        ATTAINS.WaterType, ATTAINS.assessmentunitidentifier")
      }
    }
  }
  
  # User has went through the recommended workflow. Criteria table is generated
  # from the MLSummaryRef file. This file also contains unique spatial criteria
  # as an option and will include these values if they have been populated.
  if(!is.null(MLSummaryRef)) {
    MLSummaryRef$ATTAINS.WaterType <- as.character(MLSummaryRef$ATTAINS.WaterType)
    MLSummaryRef$SaltFresh <- as.character(MLSummaryRef$SaltFresh)
    # Extracts the characteristic, speciation and fraction columns to join
    MLSummaryRef <- MLSummaryRef %>%
      dplyr::left_join(
        .data[,c(
          "TADA.ComparableDataIdentifier",
          "TADA.CharacteristicName"
          #"TADA.ResultSampleFractionText",
          #"TADA.MethodSpeciationName"
          )] %>%
        dplyr::distinct(),
        by = "TADA.ComparableDataIdentifier"
        )
  
    # Handles Dissolved Metals Criteria and Method Splits by Acute/Chronic and Salt/Fresh
    # Need to consider cases in which some orgs may not have separate criteria splits for dissolved metals.
    # metal_list <- data.frame(
    #   ATTAINS.ParameterName = c("ARSENIC", "ZINC", "CADMIUM", "COPPER", "LEAD", "MERCURY", "NICKEL")
    # ) %>%
    #   cbind(AcuteChronic = rep(c("Acute", "Chronic", "Acute", "Chronic"), each = 7)) %>%
    #   cbind(SaltFresh = rep(c("Salt", "Fresh", "Fresh", "Salt"), each = 7)) %>%
    #   dplyr::arrange(ATTAINS.ParameterName)
    
    # Creates the DefineCriteriaMethodology table from the MLSummaryRef.
    DefineCriteriaMethodology <- MLSummaryRef %>%
      dplyr::select(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", 
        "TADA.ComparableDataIdentifier", "TADA.CharacteristicName",
        "SaltFresh", "TADA.DepthCategory.Flag", "ApplyUniqueSpatialCriteria", "ATTAINS.WaterType"
      ) %>%
      # Spatial Columns - only pre-populates if a unique spatial criteria is applied.
      dplyr::mutate(ATTAINS.WaterType = dplyr::if_else( # Only pre-populates if a unique spatial criteria is applied
        is.na(ApplyUniqueSpatialCriteria),
        as.character(NA),
        as.character(ATTAINS.WaterType)
      )) %>%
      dplyr::mutate(SaltFresh = dplyr::if_else( # Only pre-populates if a unique spatial criteria is applied
        is.na(ApplyUniqueSpatialCriteria),
        as.character(NA),
        as.character(SaltFresh)
      )) %>%
      dplyr::mutate(TADA.DepthCategory.Flag = dplyr::if_else( # Only pre-populates if a unique spatial criteria is applied
        is.na(ApplyUniqueSpatialCriteria),
        as.character(NA),
        as.character(TADA.DepthCategory.Flag)
      )) %>%
      # dplyr::filter(!dplyr::if_all(c(ApplyUniqueSpatialCriteria, ATTAINS.WaterType), is.na)) %>%
      dplyr::bind_cols(
        data.frame(
          TADA.ResultSampleFractionText = as.character(NA), 
          TADA.MethodSpeciationName = as.character(NA), AcuteChronic = as.character(NA),
          # Criteria Columns
          EquationBased = as.character(NA),
          MagnitudeValueLower = as.numeric(NA), MagnitudeValueUpper = as.numeric(NA), MagnitudeUnit = as.character(NA),
          DurationValue = as.numeric(NA),	DurationUnit = as.character(NA), DurationAggregation = as.character(NA),
          FrequencyCriteriaValue = as.numeric(NA), FrequencyCriteriaMethod = as.character(NA),
          # Data Sufficiency Columns
          DataSufficiency.AssessPeriod = as.character(NA), DataSufficiency.BegAssessDate = as.Date(NA), DataSufficiency.EndAssessDate = as.Date(NA), Season = as.character(NA),
          DataSufficiency.Season = as.character(NA), DataSufficiency.SeasonBegDate = as.Date(NA), DataSufficiency.SeasonEndDate = as.Date(NA),
          DataSufficiency.CountSamplingDistribution = as.numeric(NA), DataSufficiency.SamplingDistribution = as.character(NA), DataSufficiency.MinSamplePerDistribution = as.numeric(NA)
        )
      ) %>%
      #dplyr::left_join(metal_list, by = ("ATTAINS.ParameterName"), relationship = "many-to-many") %>%
      #dplyr::mutate(AcuteChronic = dplyr::coalesce(AcuteChronic.x, AcuteChronic.y)) %>%
      #dplyr::select(-c(AcuteChronic.x, AcuteChronic.y)) %>%
      #dplyr::mutate(SaltFresh = dplyr::coalesce(SaltFresh.x, SaltFresh.y)) %>%
      #dplyr::select(-c(SaltFresh.x, SaltFresh.y)) %>%
      tidyr::drop_na(ATTAINS.ParameterName) %>%
      dplyr::select(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", 
        "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "AcuteChronic", 
        # Spatial Columns
        "ATTAINS.WaterType", "SaltFresh", "TADA.DepthCategory.Flag", "ApplyUniqueSpatialCriteria",
        # Criteria Columns
        "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
        "DurationValue",	"DurationUnit", "DurationAggregation",
        "FrequencyCriteriaValue",	"FrequencyCriteriaMethod",
        # Data Sufficiency Columns
        "DataSufficiency.AssessPeriod", "DataSufficiency.BegAssessDate", "DataSufficiency.EndAssessDate",
        "DataSufficiency.Season", "DataSufficiency.SeasonBegDate", "DataSufficiency.SeasonEndDate", 
        "DataSufficiency.CountSamplingDistribution", "DataSufficiency.SamplingDistribution", "DataSufficiency.MinSamplePerDistribution"
      ) %>%
    dplyr::distinct()
  }
  
  # User wants to populate the Criteria table using the EPA304a standards
  # joins the epa304a standards to the current Criteria Table.
  if (epa304a == TRUE) {
    # Handling of auto populating EPA304a Criteria in the future if desired.
    CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
      dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, ATTAINS.UseName = use_name, CRITERIATYPE_ACUTECHRONIC, CRITERIATYPEFRESHSALTWATER, CRITERION_VALUE, UNIT_NAME) %>%
      dplyr::mutate(ATTAINS.OrganizationIdentifier = "EPA304a")
    
    DefineCriteriaMethodology <- DefineCriteriaMethodology %>%
      dplyr::full_join(CST_param, c("EPA304A.PollutantName", "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier"), relationship = "many-to-many") %>%
      dplyr::mutate(AcuteChronic = CRITERIATYPE_ACUTECHRONIC) %>%
      dplyr::mutate(SaltFresh = CRITERIATYPEFRESHSALTWATER) %>%
      dplyr::mutate(MagnitudeValueLower = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE, "-"), stringr::str_extract(CRITERION_VALUE, "[^-]+"),
        ""
      )) %>%
      dplyr::mutate(MagnitudeValueUpper = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE, "-"), stringr::str_split(CRITERION_VALUE, "-", simplify = TRUE)[, 2],
        CRITERION_VALUE
      )) %>%
      dplyr::mutate(dplyr::across(
        c(
          MagnitudeValueLower, MagnitudeValueUpper, DurationValue,	FrequencyCriteriaValue,	
          MinimumSampleSize, MinimumSamplingPeriod,	TADA.DepthCategory.Flag
        ), as.numeric)) %>%
      dplyr::mutate(dplyr::across(
        c(
          ATTAINS.WaterType,
          MonitoringLocationTypeName,
          AcuteChronic, SaltFresh, Season, EquationBased,
          ApplyUniqueSpatialCriteria # Will depend on the user's crosswalk of ML to this criteria for filtering.
        ), as.factor
      )) %>%
      dplyr::mutate(MagnitudeUnit = UNIT_NAME) %>%
      dplyr::select(-c(CRITERIATYPEFRESHSALTWATER, CRITERIATYPE_ACUTECHRONIC, CRITERION_VALUE, UNIT_NAME)) %>%
      dplyr::mutate(MagnitudeUnit = toupper(MagnitudeUnit)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(ATTAINS.OrganizationIdentifier != "EPA304a", ATTAINS.OrganizationIdentifier)
  }
  
  # User wants to populate the Criteria table using a user supplied table.
  # This option will prioritize a user-supplied table, but may include 
  # all rows generated from this function either from 1) auto_fill default values,
  # 2) epa 304a values, 3) any updated ref values from the updateRef functions,
  # 4) from the recommended workflow based on MLSummaryRef, or 5) a blank template 
  # which will only include rows relevant to all unique TADA.CharacteristicName in 
  # the TADA data frame.
  if (!is.null(criteriaMethods)) {
    desired_cols <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "AcuteChronic",
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "TADA.DepthCategory.Flag", "ApplyUniqueSpatialCriteria",
      # Criteria Columns
      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
      "DurationValue",	"DurationUnit", "DurationAggregation",
      "FrequencyCriteriaValue",	"FrequencyCriteriaMethod",
      # Data Sufficiency Columns
      "DataSufficiency.AssessPeriod", "DataSufficiency.BegAssessDate", "DataSufficiency.EndAssessDate",
      "DataSufficiency.Season", "DataSufficiency.SeasonBegDate", "DataSufficiency.SeasonEndDate",
      "DataSufficiency.CountSamplingDistribution", "DataSufficiency.SamplingDistribution", "DataSufficiency.MinSamplePerDistribution"
    )
    
    unique_param <- unique(.data$TADA.CharacteristicName)
    
    DefineCriteriaMethodology <- criteriaMethods %>%
      dplyr::full_join(
        dplyr::select(
          DefineCriteriaMethodology, "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", 
          "ATTAINS.UseName", "TADA.CharacteristicName"), 
        by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", "TADA.CharacteristicName")) %>%
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) %>%
      dplyr::filter(TADA.CharacteristicName %in%  unique_param) %>%
      dplyr::select(
        dplyr::any_of(desired_cols)
        )
      
      # 2. Identify missing columns
      missing_cols <- setdiff(desired_cols, names(DefineCriteriaMethodology))
      
      # 3. Add missing columns with NA values using mutate()
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          DefineCriteriaMethodology <- DefineCriteriaMethodology %>%
            dplyr::mutate(!!col := NA)
        }
      }
      
      # should not be a problem if we control what column names are allowed,
      # but including this for the case if edits are made to the function to ensure
      # excel allowable values are still in the correct order.
      DefineCriteriaMethodology <- dplyr::select(DefineCriteriaMethodology, desired_cols)
  }
  
  # Generates the excel function (HIGHLY Recommended for users to export)
  if (excel == TRUE) {
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
        openxlsx::addWorksheet(wb, "Index-Criteria", visible = FALSE)
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "DefineCriteriaMethodology")
        openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
        
        openxlsx::removeWorksheet(wb, "Index-Criteria")
        openxlsx::addWorksheet(wb, "Index-Criteria", visible = FALSE)
      }
    )
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }
    
    columns <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", 
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName","AcuteChronic",  
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "TADA.DepthCategory.Flag", "ApplyUniqueSpatialCriteria",
      # Criteria Columns
      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
      "DurationValue",	"DurationUnit", "DurationAggregation",
      "FrequencyCriteriaValue",	"FrequencyCriteriaMethod",
      # Data Sufficiency Columns
      "DataSufficiency.AssessPeriod", "DataSufficiency.BegAssessDate", "DataSufficiency.EndAssessDate",
      "DataSufficiency.Season", "DataSufficiency.SeasonBegDate", "DataSufficiency.SeasonEndDate", 
      "DataSufficiency.CountSamplingDistribution", "DataSufficiency.SamplingDistribution", "DataSufficiency.MinSamplePerDistribution"
    )
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(wb, sheet = "DefineCriteriaMethodology", cols = 1:ncol(DefineCriteriaMethodology), widths = "auto")
    openxlsx::setColWidths(wb, sheet = "DefineCriteriaMethodology", cols = 1:5, widths = 20)
    
    # Export DefineCriteriaMethodology dataframe into the excel spreadsheet tab
    openxlsx::writeData(wb, "DefineCriteriaMethodology", startCol = 1, x = DefineCriteriaMethodology, headerStyle = header_st)
    
    # Creates the Index-Criteria List of allowable values under each column
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 7, startRow = 1, 
      # AcuteChronic
      x = unique(.data[,c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")])
    ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 10, startRow = 1, 
      # AcuteChronic
      x = data.frame(
        AcuteChronic = c("Acute", "Chronic", "NA")
        )
      ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 11, startRow = 1, 
      # ATTAINS.WaterType
      x = data.frame( 
        ATTAINS.WaterType = c(unique(MLSummaryRef$ATTAINS.WaterType), "All", "NA")
        )
      ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 12, startRow = 1, 
      # SaltFresh
      x = data.frame(
        SaltFresh = c("Salt", "Fresh", "NA")
        )
      ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 13, startRow = 1, 
      x = data.frame(
        TADA.DepthCategory.Flag = c("No depth info", "Epilimnion-surface", "Surface", "Bottom", "Middle")
      )
    )
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 14, startRow = 1, 
      # ApplyUniqueSpatialCriteria
      x = data.frame(
        ApplyUniqueSpatialCriteria = c(unique(MLSummaryRef$ApplyUniqueSpatialCriteria), "NA")
        )
      ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 15, startRow = 1, 
      # EquationBased
      x = data.frame(
        EquationBased = c("Yes", "No", "NA")
        )
      ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 18, startRow = 1, 
      # MagnitudeUnit
      x = data.frame(
        MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode)
      )
    ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 20, startRow = 1, 
      # DurationUnit
      x = data.frame(
        DurationUnit = c("n-hour", "n-day", "n-week", "n-month", "n-quarter")
      )
    ) 
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 21, startRow = 1, 
      # DurationAggregation
      x = data.frame(
        DurationAggregation = c(
          "arithmetic mean", "arithmetic median", "arithmetic max", "arithmetic min", 
          "geometric mean", "rolling geometric mean", "rolling arithmetric mean"
          )
      )
    ) 

    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 23, startRow = 1, 
      # FrequencyCriteriaMethod
      x = data.frame(
        FrequencyCriteriaMethod = c(
          "Percent of samples not meeting", "percentile",
          "n-samples in 3 years", "n-samples in 4 years", "n-samples in 5 years", 
          "binomial test", "NumberNotMeeting" 
        )
      )
    )     
        
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 24, startRow = 1, 
      x = data.frame(
        DataSufficiency.AssessPeriod = c("Last 30 years", "Last 10 years", "Last 5 years", "Last 3 years", "Last year", "NA")
        )
      )
        
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 27, startRow = 1, 
      x = data.frame(
        DataSufficiency.Season = c("Summer", "Fall", "Spring", "Winter", "NA")
      )
    )
    
    openxlsx::writeData(
      wb, "Index-Criteria", 
      startCol = 31, startRow = 1, 
      x = data.frame(
        DataSufficiency.SamplingDistribution = c("Seasonal", "Annual", "Semi-Annual", "Quarterly", "Monthly", "Bi-weekly", "Weekly", "10 days", "NA")
      )
    )
    
    # The list of allowable values for each column in excel tab [DefineCriteriaMethodology] will be defined by the [Index-Criteria] tab
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$G$2:$G$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$H$2:$H$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 7, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 8, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 10, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$M$2:$M$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 11, rows = 2:1000, type = "list", value = sprintf("'CreateMLSummaryRef'!$Q$2:$Q$10000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 12, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$O$2:$O$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 15, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$R$2:$R$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 17, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$T$2:$T$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 18, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$U$2:$U$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 20, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$W$2:$W$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 21, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$X$2:$X$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 24, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$AA$2:$AA$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 28, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$AE$2:$AE$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
    
    # Conditional Formatting
    openxlsx::freezePane(wb, "DefineCriteriaMethodology", firstActiveRow = 2, firstActiveCol = 4)
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology",
                                    cols = 5:29, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
                                    type = "notBlanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology",
                                    cols = 5:29, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
                                    type = "blanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    ) # modified cells.
    
    # Group DataSufficiency Columns
    openxlsx::groupColumns(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 21:29,
      hidden = FALSE, 
      level = -1
      )
    
    # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  
  return(DefineCriteriaMethodology)
}



#' Criteria Summary
#'
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#'
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#'
#' @export
#'
#' @examples
#' Data_Nutrients_UT_GetATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateUseParamRef(Data_Nutrients_UT)
#'
TADA_CriteriaSummary <- function(.data, criteriaMethods = NULL, MLSummaryRef = NULL, 
                                 summarizeBy = c("All", "Criteria", "Char"), 
                                 spatialSummary = c("groupedML", "individualML", "AU", NULL),
                                 #criteriaOutput = c("")
                                 excel = FALSE, overwrite = FALSE) {
  # check to make sure summarizeBy is populated with allowable value
  summarizeBy <- match.arg(summarizeBy)
  
  # Runs TADA_FlagDepthCategory if not already ran
  # if (!"TADA.DepthCategory.Flag" %in% names(.data)) {
  #   .data <- TADA_FlagDepthCategory(.data)
  # }
  
  if ( sum(is.na(criteriaMethods$MagnitudeValueUpper) & is.na(criteriaMethods$MagnitudeValueLower)) > 0) {
    print(
    paste0(
      "Warning: There are ", 
      sum(is.na(criteriaMethods$MagnitudeValueUpper) & is.na(criteriaMethods$MagnitudeValueLower)), 
      " row(s) with no magnitude values defined or specification of being an equation-based standards. Cannot compare these results to an NA value." ))
  }
  
  # Combine all and summarize by ONLY characteristic and ignore fraction and speciation
  # We can say "it's not recommended" - good question to ask to M3 subgroup - but would people find it useful?
  if( summarizeBy == "Char") {
     data_with_criteria <- .data %>%
      dplyr::left_join(criteriaMethods, by = c("TADA.CharacteristicName")) %>%
      dplyr::mutate(DurationPeriod = gsub("n-", paste0(DurationValue," "), DurationUnit)) %>%
      dplyr::mutate(
        ActivityStartDate = as.POSIXct(ActivityStartDate, format = "%Y-%m-%d"),
        ActivityStartDateTime = as.POSIXct(ActivityStartDateTime, format = "%Y-%m-%d %H:%M:%S")
        )
      #%>%
      # dplyr::mutate(Flag.CharOnly...) # will help to see if the logic makes sense
  }
  
  # User will summarize only by defined WQP to ATTAINS Parameters in criteria table.
  if( summarizeBy == "Criteria") {
    data_with_criteria <- .data %>%
      dplyr::left_join(criteriaMethods, by = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")) %>%
      dplyr::mutate(DurationPeriod = gsub("n-", paste0(DurationValue," "), DurationUnit)) %>%
      dplyr::mutate(
        ActivityStartDate = as.POSIXct(ActivityStartDate, format = "%Y-%m-%d"),
        ActivityStartDateTime = as.POSIXct(ActivityStartDateTime, format = "%Y-%m-%d %H:%M:%S")
      )
  }
  
  # Split the joined .data by duration period.
  Duration_splits <- split(data_with_criteria, data_with_criteria$DurationPeriod)
  
  df_final <- list() # will contain raw data aggregated by Duration and CriteriaSummary
  df_raw_aggregated <- data.frame()
  # df_raw_aggregated_rolling <- data.frame()
  df_summary <- data.frame()
  
  for (i in 1:length(Duration_splits)) {

    DurationUnit <- gsub("n-", "", as.character(unique(Duration_splits[[i]]["DurationUnit"])))
      
    DurationPeriod <- as.character(unique(Duration_splits[[i]]["DurationPeriod"]))
    
    # for each unique duration period perform aggregation. rbind in final df.
    df_raw <- Duration_splits[[i]]
    
    start_date <- as.POSIXct(tryCatch(
      min(df_raw$ActivityStartDateTime, na.rm = TRUE),
      warning = function(w) {
        min(df_raw$ActivityStartDate, na.rm = TRUE)
                           }))
    
    end_date <- as.POSIXct(tryCatch(
      max(df_raw$ActivityStartDateTime, na.rm = TRUE),
      warning = function(w) {
        max(df_raw$ActivityStartDate, na.rm = TRUE)
      }))
    
    regular_timestamps <- seq(start_date, end_date, by = DurationPeriod)
    
    regular_timestamps_df <- data.frame(
      AggregatedActivityStartDateTime = as.POSIXct( regular_timestamps[-length(regular_timestamps)]),
      AggregatedActivityEndDateTime = as.POSIXct( regular_timestamps[2:length(regular_timestamps)])
      )
    
    df_start_end <- dplyr::left_join(
      df_raw, regular_timestamps_df, 
      by = dplyr::join_by(dplyr::between(ActivityStartDateTime, AggregatedActivityStartDateTime, AggregatedActivityEndDateTime))
      )
    
    df_aggregated <- df_start_end %>%
      #tidyr::drop_na(ActivityStartDateTime) %>%
      dplyr::filter(!is.na(TADA.ResultMeasureValue)) %>%
      dplyr::filter(!is.na(ActivityStartDate)) %>%
      dplyr::filter(!is.na(MagnitudeValueLower) | !is.na(MagnitudeValueUpper)) %>%
      dplyr::group_by(
        DurationPeriod, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
        ATTAINS.ParameterName, ATTAINS.UseName,
        ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper
        #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        AggregatedActivityStartDateTime, AggregatedActivityEndDateTime, 
        DurationPeriod, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
        ATTAINS.ParameterName, ATTAINS.UseName,
        ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper
        #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      ) %>%
      dplyr::summarize(
        geomean_TADA.ResultMeasureValue = exp(mean(log(TADA.ResultMeasureValue), na.rm = TRUE)),
        arithmetic_mean_TADA.ResultMeasureValue = mean(TADA.ResultMeasureValue, na.rm = TRUE),
        count = dplyr::n(),
        Min = min(TADA.ResultMeasureValue, na.rm = TRUE),
        Max = max(TADA.ResultMeasureValue, na.rm = TRUE),
        Percentile_5th = stats::quantile(TADA.ResultMeasureValue, .05),
        Percentile_10th = stats::quantile(TADA.ResultMeasureValue, .10),
        Percentile_15th = stats::quantile(TADA.ResultMeasureValue, .15),
        Percentile_25th = stats::quantile(TADA.ResultMeasureValue, .25),
        Percentile_50th_Median = stats::quantile(TADA.ResultMeasureValue, .50),
        Percentile_75th = stats::quantile(TADA.ResultMeasureValue, .75),
        Percentile_85th = stats::quantile(TADA.ResultMeasureValue, .85),
        Percentile_95th = stats::quantile(TADA.ResultMeasureValue, .95),
        Percentile_98th = stats::quantile(TADA.ResultMeasureValue, .98),
        .groups = "drop"
      )
    
    TADA_with_Summary <- df_start_end %>% 
      dplyr::select(-ActivityStartDate) %>% 
      dplyr::left_join(
        df_aggregated, 
        by = c("AggregatedActivityStartDateTime","AggregatedActivityEndDateTime","TADA.ComparableDataIdentifier")
        ) %>% 
      dplyr::rename(ActivityStartDate = AggregatedActivityStartDateTime)
  
    #TADA_Scatterplot(TADA_with_Summary)
    #####################################################
      # # For rolling summary calculations
      # DurationValue <- as.numeric(unique(Duration_splits[[i]]["DurationValue"]))
      # DurationUnit <- gsub("n-", "", as.character(unique(Duration_splits[[i]]["DurationUnit"])))
      # 
      # df_raw <- Duration_splits[[i]]
      # 
      # start_date_roll <- min(df_raw$ActivityStartDateTime, na.rm = TRUE)
      # end_date_roll <- max(df_raw$ActivityStartDateTime, na.rm = TRUE)
      # regular_timestamps_roll <- seq(start_date_roll, end_date_roll, by = DurationUnit)
      # 
      # regular_timestamps_df_roll <- data.frame(
      #   AggregatedActivityStartDateTime = as.POSIXct( regular_timestamps_roll[-length(regular_timestamps_roll)], format = "%Y-%m-%d %H:%M:%S"),
      #   AggregatedActivityEndDateTime = regular_timestamps_roll[2:length(regular_timestamps_roll)]
      # )
      # 
      # df_start_end_roll <- dplyr::right_join(
      #   df_raw, regular_timestamps_df_roll, 
      #   by = dplyr::join_by(dplyr::between(ActivityStartDateTime, AggregatedActivityStartDateTime, AggregatedActivityEndDateTime))
      # )
      # 
      # df_aggregated_roll <- df_start_end_roll %>%
      #   tidyr::drop_na(ActivityStartDateTime) %>%
      #   dplyr::filter(!is.na(TADA.ResultMeasureValue)) %>%
      #   dplyr::filter(!is.na(ActivityStartDateTime)) %>%
      #   dplyr::filter(!is.na(MagnitudeValueLower) | !is.na(MagnitudeValueUpper)) %>%
      #   dplyr::group_by(
      #     DurationValue, DurationUnit, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
      #     ATTAINS.ParameterName, ATTAINS.UseName,
      #     ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper
      #     #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      #   ) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::group_by(
      #     AggregatedActivityStartDateTime, AggregatedActivityEndDateTime, 
      #     DurationValue, DurationUnit, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
      #     ATTAINS.ParameterName, ATTAINS.UseName,
      #     ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper
      #     #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      #   ) %>%
      #   dplyr::summarize(
      #     geomean_TADA.ResultMeasureValue = exp(mean(log(TADA.ResultMeasureValue), na.rm = TRUE)),
      #     arithmetic_mean_TADA.ResultMeasureValue = mean(TADA.ResultMeasureValue, na.rm = TRUE),
      #     count = dplyr::n(),
      #     Min = min(TADA.ResultMeasureValue, na.rm = TRUE),
      #     Max = max(TADA.ResultMeasureValue, na.rm = TRUE),
      #     Percentile_5th = stats::quantile(TADA.ResultMeasureValue, .05),
      #     Percentile_10th = stats::quantile(TADA.ResultMeasureValue, .10),
      #     Percentile_15th = stats::quantile(TADA.ResultMeasureValue, .15),
      #     Percentile_25th = stats::quantile(TADA.ResultMeasureValue, .25),
      #     Percentile_50th_Median = stats::quantile(TADA.ResultMeasureValue, .50),
      #     Percentile_75th = stats::quantile(TADA.ResultMeasureValue, .75),
      #     Percentile_85th = stats::quantile(TADA.ResultMeasureValue, .85),
      #     Percentile_95th = stats::quantile(TADA.ResultMeasureValue, .95),
      #     Percentile_98th = stats::quantile(TADA.ResultMeasureValue, .98)
      #   )
      # 
      # 
      # # rolling_mean_custom <- function(x, k, na_rm = TRUE) {
      # #   if (k == 1) {
      # #     return(x) # For a window of 1, the average is just the value itself
      # #   }
      # #   
      # #   n <- length(x)
      # #   result <- numeric(n)
      # #   
      # #   for (i in 1:n) {
      # #     start_index <- max(1, i - k + 1)
      # #     window_values <- x[start_index:i]
      # #     
      # #     # if (na_rm) {
      # #     #   window_values <- window_values[!is.na(window_values)]
      # #     # }
      # #     
      # #     if (length(window_values) > 0) { # Ensure there are non-NA values to average
      # #       result[i] <- mean(tail(na.omit(window_values, k, na.rm = TRUE)))
      # #     } else {
      # #       result[i] <- NA # If all values in window are NA or empty after na_rm
      # #     }
      # #   }
      # #   return(result)
      # # }
      # # 
      # # df_aggregated_rolling <- regular_timestamps_df_roll %>%
      # #   dplyr::left_join(df_aggregated_roll) %>%
      # #   dplyr::filter(TADA.ComparableDataIdentifier == "DISSOLVED OXYGEN (DO) MG/L") %>%
      # #   tidyr::fill(DurationValue, DurationUnit, TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
      # #               ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper, .direction = "down")  %>%
      # #   dplyr::group_by(
      # #     AggregatedActivityStartDateTime, AggregatedActivityEndDateTime, 
      # #     DurationValue, DurationUnit, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
      # #     ATTAINS.ParameterName, ATTAINS.UseName,
      # #     ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper
      # #     #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      # #   ) %>%
      # #   dplyr::mutate(rolling_avg_geomean = rolling_mean_custom( geomean_TADA.ResultMeasureValue, k = 4, na_rm = TRUE))
      # # 
      # 
      # # calculates rolling average by specified k window size
      # rolling_mean_na_rm <- function(x, y, k) {
      #   if (k == 1) {
      #     return(x) # For a window of 1, the average is just the value itself
      #   }
      # 
      #   if (length(x) < k) {
      #     return(NA_real_) # Return NA if window size is larger than available data
      #   }
      #   # Calculate weighted mean of the last 'k' non-NA values
      #   return(sum(tail(na.omit(x * y), k), na.rm = TRUE) / sum(tail(na.omit(y), k)) )
      # }
      # 
      # unique_parameters_use <- unique(df_aggregated_roll$TADA.ComparableDataIdentifier)
      # df_aggregated_rolling <- data.frame()
      # 
      # for (i in 1:length(unique_parameters)){
      # temp_df <- regular_timestamps_df_roll %>%
      #   dplyr::left_join(df_aggregated_roll[df_aggregated_roll$TADA.ComparableDataIdentifier == unique_parameters[i],]) %>%
      #   tidyr::fill(DurationValue, DurationUnit, TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
      #               ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper, .direction = "down") %>%
      #   dplyr::select(
      #     DurationValue, DurationUnit, TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
      #     ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper) %>%
      #   dplyr::mutate(
      #     rolling_avg = purrr::map_dbl(dplyr::row_number(), ~{
      #       current_index <- .x
      #       start_index <- max(1, current_index - as.numeric(unique(Duration_splits[[i]]["DurationValue"])) + 1) # Adjust for window size
      #       window_data <- geomean_TADA.ResultMeasureValue[start_index:current_index]
      #       window_weights <- count[start_index:current_index]
      #       rolling_mean_na_rm(window_data, window_weights, as.numeric(unique(Duration_splits[[i]]["DurationValue"])))
      #     })
      #   )
      # 
      # df_aggregated_rolling <- rbind(df_aggregated_rolling, temp_df)
      # }
    ####################
      
    # non rolling raw data  
    df_raw_aggregated <- rbind(df_raw_aggregated, TADA_with_Summary)
    # rolling raw data
    #df_raw_aggregated_rolling <- rbind(df_raw_aggregated_rolling, df_aggregated_rolling)
    
    # For Non-Rolling Summary
    df_aggregated_summary <- df_aggregated %>%
      dplyr::group_by(
        #AggregatedActivityStartDateTime, 
        DurationPeriod,
        TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
        ATTAINS.ParameterName, ATTAINS.UseName,
        ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper
        #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      ) %>%
      dplyr::summarize(
        n_Aggregatedsamples = dplyr::n(),
        n_exceedance = sum(geomean_TADA.ResultMeasureValue > MagnitudeValueUpper, na.rm = TRUE) 
        + sum(geomean_TADA.ResultMeasureValue < MagnitudeValueLower, na.rm = TRUE), # Will need to know what is being compared - geomean, arithmetic mean, max, min etc.
        percent_exccedance = round(n_exceedance/n_Aggregatedsamples * 100, 3),
        .groups = "drop"
      )
    
    df_summary <- rbind(df_summary, df_aggregated_summary) %>% 
      dplyr::distinct()
  }
  df_final <- list(TADA_with_Summary_Stats = df_raw_aggregated, CriteriaSummary = df_summary)
  
  return(df_final)
}

#' Criteria Summary Plot
#'
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#'
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#'
#' @export
#'
#' @examples
#' Data_Nutrients_UT_GetATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateUseParamRef(Data_Nutrients_UT)
#'
TADA_SummaryScatterplot <- function(summaryRef = NULL) {
  
  # unique TADA.ComparableDataIdentifier Names extracted from TADA_Scatterplot base function
  # note: think of a better way to match each TADA comparabledataidentifier
  param_names <- sort(unique(names(TADA_Scatterplot(summaryRef$TADA_with_Summary_Stats))))
  param_names2 <- sort(unique(summaryRef$TADA_with_Summary_Stats$TADA.ComparableDataIdentifier))
  
  TADA_Summary_Scatter <- list()
  subplot_TADA_Summary_Scatter <- list()
  #unique_Durations <- unique(TADA_SummaryOutput$CriteriaSummary[TADA_SummaryOutput$CriteriaSummary$DurationPeriod])
  
  for (i in 1:length(param_names)){
    temp_df <- summaryRef$TADA_with_Summary_Stats[summaryRef$TADA_with_Summary_Stats$TADA.ComparableDataIdentifier == param_names2[i],]
    
    unique_Durations <- unique(temp_df$DurationPeriod.x)
    
    for (n in 1:length(unique_Durations)) {
      subplot_TADA_Summary_Scatter[[n]] <- TADA_Scatterplot(dplyr::filter(temp_df, DurationPeriod.x == unique_Durations[n])) %>%
        plotly::add_trace(
        # plots the criteria measure not to be exceeded. ex. geomean, arithimetic mean, median etc.
        data = dplyr::filter(temp_df, DurationPeriod.x == unique_Durations[n]),
        x = ~ ActivityStartDate, 
        y = ~ geomean_TADA.ResultMeasureValue, 
        type = "scatter", mode = "markers", 
        name = paste0(unique_Durations[n] ," geometric mean"), 
        hoverinfo = "none",
        marker = list(color = TADA_ColorPalette()[n])) 
    
      lowerMagnitudes <- unique(dplyr::filter(temp_df, DurationPeriod.x == unique_Durations[n])[, "MagnitudeValueLower.x"])
      upperMagnitudes <- unique(dplyr::filter(temp_df, DurationPeriod.x == unique_Durations[n])[, "MagnitudeValueUpper.x"])

      for (j in 1:nrow(lowerMagnitudes)) {
        subplot_TADA_Summary_Scatter[[n]] <- subplot_TADA_Summary_Scatter[[n]] %>%
          plotly::add_lines(
            y = as.numeric(
              c(
                lowerMagnitudes[j,],
                lowerMagnitudes[j,])
            ),
            x = c(min(summaryRef$TADA_with_Summary_Stats$ActivityStartDate, na.rm = TRUE), max(summaryRef$TADA_with_Summary_Stats$ActivityStartDate, na.rm = TRUE)),
            inherit = FALSE,
            line = list(color = "red"),
            name = paste0("Lower Limit ", j),
            hoverinfo = "none"
          )
      }

      for (k in 1:nrow(upperMagnitudes)) {
        subplot_TADA_Summary_Scatter[[n]] <- subplot_TADA_Summary_Scatter[[n]] %>%
          plotly::add_lines(
            y = as.numeric(
              c(
                upperMagnitudes[k,],
                upperMagnitudes[k,])
            ),
            x = c(min(summaryRef$TADA_with_Summary_Stats$ActivityStartDate, na.rm = TRUE), max(summaryRef$TADA_with_Summary_Stats$ActivityStartDate, na.rm = TRUE)),
            inherit = FALSE,
            line = list(color = "black"),
            name = paste0("Upper Limit ", k),
            hoverinfo = "none"
          )
      }
      
      

    }
    TADA_Summary_Scatter[[i]] <- plotly::subplot(subplot_TADA_Summary_Scatter, nrows = length(unique_Durations), shareX = T )  

  }
  return(TADA_Summary_Scatter)
}
