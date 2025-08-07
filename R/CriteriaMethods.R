#' Define Criteria and Methodology
#'
#' Users will need to provide the completed reference tables from
#' TADA_CreateSiteRef. This will generate a template for users to fill out
#' and define either the full Criteria or magnitude only values associated with
#' an ATTAINS Parameter name and use name. For each Criteria/Magnitude value,
#' users will need to ensure they properly define any additional methods that will
#' correctly reflect their assessment standards for that parameter and use.
#' For example, if there are separate standards for acute versus chronic,
#' rivers versus estuary, different seasons, etc. then a user will need to create
#' additional rows to reflect this.
#'
#' Efforts have been made to pull in the EPA304a recommended standards
#' automatically from the Criteria Search Tool (CST). Users should validate this
#' final output if a user has decided to include the EPA304a standards.
#' User will need to determine if any additional adjustments are needed.
#' For example, does the crosswalk between the EPA304A.PollutantName
#' and TADA.ComparableDataIdentifier seem valid for your organization's method?
#' Is your organization only interested in providing the EPA304a recommended
#' standards for certain seasons (Fall, Summer, Spring, Winter) etc.
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
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#'
#' @param siteRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit.
#' 
#' @param ref "TADA"
#'
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
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
#' # Now, run TADA_CreateSiteRef()
#' siteRef_UT <- TADA_CreateSiteRef(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   waterUseParamRef = NULL, useAURef = NULL, sitesAURef = NULL,
#'   useParamRef = UseParamRef_UT,
#'   excel = FALSE
#' )
#'
#' DefineCriteriaMethodology_UT <- TADA_DefineCriteriaMethodology(
#'   Data_Nutrients_UT,
#'   siteRef = siteRef_UT,
#'   excel = FALSE
#' )
#'
TADA_DefineCriteriaMethodology <- function(.data, siteRef = NULL, criteriaMethods = NULL, epa304a = FALSE, # ref = c("ATTAINS", "CST", "TADA", "Other") future development to consider additional crosswalk alternatives?
                                           auto_assign = FALSE, org_id = NULL, sitesAURef = NULL, # Optional if auto_assign = TRUE
                                           updateRef = "none", # c("none", "paramRef", "useParamRef", "siteRef"), # hierarchical dependency
                                           excel = TRUE, overwrite = FALSE) {
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # Invalid function input combos
  if (auto_assign == FALSE && updateRef != "none") {
    stop("TADA_DefineCriteriaMethodology: auto_assign = FALSE and updateRef = 'none' is an invalid function input.")
  }
  
  # Ensures you have used a valid auto_assign name
  if (!updateRef %in% c("none", "paramRef", "useParamRef", "siteRef")) {
    stop(paste0(
      "TADA_DefineCriteriaMethodology: ",
      "argument input ", updateRef, " is not a valid entry. Please type one of 'None', 'paramRef', 'useParamRef', 'siteRef' as a value."
    ))
  }
  
  # If user wants to create a prepopulated CriteriaMethods table, it will run all crosswalk tables and use the default.
  if (auto_assign == TRUE) {
    # default, runs all reference tables with no user edits
    if(updateRef == "none") {
      message(paste0("auto_assign = TRUE selected. Running TADA_CreateParamRef with default assignment. Please review this paramRef table output."))
      TADA_ParamRef <- TADA_CreateParamRef(  
        .data, 
        org_id = org_id,
        auto_assign = "All", # auto-populate any exact matches found between WQP CharacteristicName and ATTAINS ParameterName
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      message(paste0("auto_assign = TRUE selected. Running TADA_CreateUseParamRef with default assignment. Please review this Use to paramRef table output."))
      TADA_UseParamRef <- TADA_CreateUseParamRef(  
        .data, 
        org_id = org_id,
        paramRef = TADA_ParamRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      message(paste0("auto_assign = TRUE selected. Running TADA_CreateSiteRef with default assignment. Please review  this sites Ref table output."))
      siteRef <- TADA_CreateSiteRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        sitesAURef = sitesAURef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
    
    # user only updates paramRef. This will update paramRef, useParamRef, and siteRef based on these modifications.
    if (updateRef == "paramRef") {
      message(paste0("auto_assign = TRUE and updateRef = paramRef selected. Running TADA_CreateParamRef with use supplied paramRef assignment. Please review this paramRef table output."))
      myfile_ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef") 
      
      TADA_ParamRef <- TADA_CreateParamRef(  
        .data, 
        org_id = org_id,
        paramRef = myfile_ParamRef,
        auto_assign = "All", # auto-populate any exact matches found between WQP CharacteristicName and ATTAINS ParameterName
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      TADA_UseParamRef <- TADA_CreateUseParamRef(  
        .data, 
        org_id = org_id,
        paramRef = TADA_ParamRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      siteRef <- TADA_CreateSiteRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        sitesAURef = sitesAURef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
      
    # user only updates useParamRef. This will update useParamRef, siteRef based on this modifications.
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
      
      siteRef <- TADA_CreateSiteRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        sitesAURef = sitesAURef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
    
    # user only updates siteRef in excel. This will update siteRef based on this modifications.
    if (updateRef == "siteRef") {
      message(paste0("auto_assign = TRUE and updateRef = siteRef selected. Running TADA_CreateSiteRef with use supplied paramRef assignment. Please review this paramRef table output."))
      myfile_SiteRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateSiteRef") 
      
      TADA_ParamRef <- TADA_CreateParamRef(  
        .data, 
        org_id = org_id,
        paramRef = myfile_SiteRef, # will update paramRef based on useParamRef
        auto_assign = "All",
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      TADA_UseParamRef <- TADA_CreateUseParamRef(  
        .data, 
        org_id = org_id,
        paramRef = TADA_ParamRef,
        useParamRef = myfile_SiteRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
      
      siteRef <- TADA_CreateSiteRef(  
        .data, 
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        sitesAURef = sitesAURef,
        siteRef = myfile_SiteRef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    }
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(siteRef) & !is.character(siteRef)) {
    if (!is.data.frame(siteRef)) {
      stop("TADA_DefineCriteriaMethodology: 'siteRef' must be a data frame with six columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, ApplyUniqueSpatialCriteria,
        ATTAINS.WaterType, ATTAINS.assessmentunitidentifier")
    }
    
    if (is.data.frame(siteRef)) {
      col.names <- c(
        "ATTAINS.ParameterName",
        "ATTAINS.UseName",
        "ATTAINS.OrganizationIdentifier",
        "ApplyUniqueSpatialCriteria",
        "ATTAINS.WaterType",
        "ATTAINS.assessmentunitidentifier"
      )
      
      ref.names <- names(siteRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineCriteriaMethodology: 'siteRef' must be a data frame with six columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, ApplyUniqueSpatialCriteria,
        ATTAINS.WaterType, ATTAINS.assessmentunitidentifier")
      }
    }
  }
  
  siteRef$ATTAINS.WaterType <- as.character(siteRef$ATTAINS.WaterType)
  siteRef$SaltFresh <- as.character(siteRef$SaltFresh)
  # Extracts the characteristic, speciation and fraction columns to join
  siteRef <- siteRef %>%
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
  
  # Creates the DefineCriteriaMethodology table from the siteRef.
  DefineCriteriaMethodology <- siteRef %>%
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
  
  # Handling of auto populating EPA304a Criteria in the future if desired.
  CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
    dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, ATTAINS.UseName = use_name, CRITERIATYPE_ACUTECHRONIC, CRITERIATYPEFRESHSALTWATER, CRITERION_VALUE, UNIT_NAME) %>%
    dplyr::mutate(ATTAINS.OrganizationIdentifier = "EPA304a")
  
  if (epa304a == TRUE) {
    DefineCriteriaMethodology <- DefineCriteriaMethodology %>%
      dplyr::left_join(CST_param, c("EPA304A.PollutantName", "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier"), relationship = "many-to-many") %>%
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
        ATTAINS.WaterType = c(unique(siteRef$ATTAINS.WaterType), "All", "NA")
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
        ApplyUniqueSpatialCriteria = c(unique(siteRef$ApplyUniqueSpatialCriteria), "NA")
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
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 11, rows = 2:1000, type = "list", value = sprintf("'CreateSiteRef'!$Q$2:$Q$10000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) 
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
TADA_CriteriaSummary <- function(.data, criteriaMethods = NULL, siteRef = NULL, 
                                 summarizeBy = c("All", "Criteria", "Char"), 
                                 spatialSummary = c("groupedML", "individualML", "AU", NULL),
                                 #criteriaOutput = c("")
                                 excel = FALSE, overwrite = FALSE) {
  
  # Runs TADA_FlagDepthCategory if not already ran
  if (!"TADA.DepthCategory.Flag" %in% names(.data)) {
    .data <- TADA_FlagDepthCategory(.data)
  }
  
  if( length(is.na(criteriaMethods$MagnitudeValueLower)) > 0 ){
    print(paste0("Warning: There are ", length(is.na(criteriaMethods$MagnitudeValueLower)), "rows with no magnitude values
                 defined or specification of being an equation-based standards. Cannot compare these results to an NA value." ))
  }
  
  # Combine all and summarize by ONLY characteristic and ignore fraction and speciation
  # We can say "it's not recommended" - good question to ask to M3 subgroup - but would people find it useful?
  if( summarizeBy == "Char") {
    TADA_Example4.1 <- TADA_Example4 %>%
      dplyr::left_join(myfileRef, by = c("TADA.CharacteristicName")) %>%
      dplyr::mutate(DurationPeriod = gsub("n-", paste0(DurationValue," "), DurationUnit)) %>%
      mutate(
        ActivityStartDate = as.POSIXct(ActivityStartDate, format = "%Y-%m-%d %H:%M:%S"),
        ActivityStartDateTime = as.POSIXct(ActivityStartDateTime, format = "%Y-%m-%d %H:%M:%S")
        )
      #%>%
      # dplyr::mutate(Flag.CharOnly...) # will help to see if the logic makes sense
  }
  
  if( summarizeBy == "Criteria") {
    TADA_Example4.1 <- TADA_Example4 %>%
      dplyr::left_join(TADA_CriteriaMethodology_AU_Final, by = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName"))
  }
  
  Duration_splits <- split(TADA_Example4.1, TADA_Example4.1$DurationPeriod)
  
  Duration_hour <- Duration_splits$`1hour`
  
  for (i in 1:length(Duration_splits)) {
    # For rolling summary calculations
    Duration_splits <- split(TADA_Example4.1, TADA_Example4.1$DurationPeriod)
    
    DurationPeriod <- as.character(unique(Duration_splits[[i]]["DurationPeriod"]))
    
    RollingAgg <- Duration_splits$`1hour`
    
    start_date <- min(Duration_hour$ActivityStartDateTime, na.rm = TRUE)
    end_date <- max(Duration_hour$ActivityStartDateTime, na.rm = TRUE)
    regular_timestamps <- seq(start_date, end_date, by = DurationPeriod)
    
    regular_timestamps_df <- data.frame(AggregatedActivityStartDateTime = regular_timestamps)
    
    Duration_hour_Aggregated <- regular_timestamps_df %>% 
      dplyr::left_join(Duration_hour, by = c("AggregatedActivityStartDateTime" = "ActivityStartDateTime"), keep = TRUE) %>% 
      tidyr::fill(ResultMeasureValue, .direction = "down") %>%
      tidyr::drop_na(ActivityStartDateTime) %>%
      dplyr::filter(!is.na(TADA.ResultMeasureValue)) %>%
      dplyr::filter(!is.na(ActivityStartDateTime)) %>%
      dplyr::filter(!is.na(MagnitudeValueLower), !is.na(MagnitudeValueUpper)) %>%
      dplyr::group_by(
        DurationPeriod, DurationUnit, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
        ATTAINS.ParameterName, ATTAINS.UseName,
        ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper,
        MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      ) %>%
      dplyr::mutate(
        beg = min(ActivityStartDateTime, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        ActivityStartDateTime, beg,
        #AggregatedActivityStartDateTime, 
        DurationPeriod, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
        ATTAINS.ParameterName, ATTAINS.UseName,
        ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper,
        MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      ) %>%
      dplyr::summarize(
        rolling_geomean_TADA.ResultMeasureValue = exp(mean(log(TADA.ResultMeasureValue), na.rm = TRUE)),
        rolling_arithmetic_mean_TADA.ResultMeasureValue = mean(TADA.ResultMeasureValue, na.rm = TRUE),
        count = dplyr::n(),
        Min = min(TADA.ResultMeasureValue),
        Max = max(TADA.ResultMeasureValue),
        Percentile_5th = stats::quantile(TADA.ResultMeasureValue, .05),
        Percentile_10th = stats::quantile(TADA.ResultMeasureValue, .10),
        Percentile_15th = stats::quantile(TADA.ResultMeasureValue, .15),
        Percentile_25th = stats::quantile(TADA.ResultMeasureValue, .25),
        Percentile_50th_Median = stats::quantile(TADA.ResultMeasureValue, .50),
        Percentile_75th = stats::quantile(TADA.ResultMeasureValue, .75),
        Percentile_85th = stats::quantile(TADA.ResultMeasureValue, .85),
        Percentile_95th = stats::quantile(TADA.ResultMeasureValue, .95),
        Percentile_98th = stats::quantile(TADA.ResultMeasureValue, .98)
    ) 
  }
  
  # For Non-Rolling Summary
  
  
  # Aggregates data by duration period, then provides summary stats on the aggregated data.
  TADA_Example4_Aggregated <- TADA_Example4.1 %>%
    dplyr::mutate(DurationPeriod = gsub("n-", DurationValue, DurationUnit)) %>%
    dplyr::filter(!is.na(TADA.ResultMeasureValue)) %>%
    dplyr::filter(!is.na(ActivityStartDateTime)) %>%
    dplyr::filter(!is.na(MagnitudeValueLower), !is.na(MagnitudeValueUpper)) %>%
    dplyr::group_by(
      DurationPeriod, DurationUnit, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
      ATTAINS.ParameterName, ATTAINS.UseName,
      ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper,
      #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
      ) %>%
    dplyr::mutate(
      beg = min(ActivityStartDateTime, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      ActivityStartDateTime, beg,
      #AggregatedActivityStartDateTime, 
      DurationPeriod, TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
      ATTAINS.ParameterName, ATTAINS.UseName,
      ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper,
      #MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
    ) %>%
    dplyr::summarize(
      geomean_TADA.ResultMeasureValue = exp(mean(log(TADA.ResultMeasureValue), na.rm = TRUE)),
      arithmetic_mean_TADA.ResultMeasureValue = mean(TADA.ResultMeasureValue, na.rm = TRUE),
      count = dplyr::n(),
      Min = min(TADA.ResultMeasureValue),
      Max = max(TADA.ResultMeasureValue),
      Percentile_5th = stats::quantile(TADA.ResultMeasureValue, .05),
      Percentile_10th = stats::quantile(TADA.ResultMeasureValue, .10),
      Percentile_15th = stats::quantile(TADA.ResultMeasureValue, .15),
      Percentile_25th = stats::quantile(TADA.ResultMeasureValue, .25),
      Percentile_50th_Median = stats::quantile(TADA.ResultMeasureValue, .50),
      Percentile_75th = stats::quantile(TADA.ResultMeasureValue, .75),
      Percentile_85th = stats::quantile(TADA.ResultMeasureValue, .85),
      Percentile_95th = stats::quantile(TADA.ResultMeasureValue, .95),
      Percentile_98th = stats::quantile(TADA.ResultMeasureValue, .98)
    ) 
  
  TADA_Example4_Aggregated_Rolling <- TADA_Example4_Aggregated %>%
    dplyr::filter(!is.na(DurationPeriod)) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(
      AggregatedActivityStartDateTime = dplyr::if_else(
        is.na(DurationPeriod) | beg < lubridate::floor_date(as.POSIXct(ActivityStartDateTime), DurationPeriod),
        as.POSIXct(ActivityStartDateTime),
        # If not based on a calendar period, then find the minimum start date and use that as our starting window.
        lubridate::floor_date(as.POSIXct(ActivityStartDateTime), DurationPeriod) + 
          difftime(as.POSIXct(beg), lubridate::floor_date(as.POSIXct(ActivityStartDateTime), DurationPeriod))
      )
    )
    %>% 
    dplyr::ungroup()
  
  TADA_Example4_30day_NA <- TADA_Example4_30day %>%
    dplyr::filter(is.na(DurationPeriod)) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(
      AggregatedActivityStartDateTime = ActivityStartDateTime
    )%>% 
    dplyr::ungroup()
    
  TADA_Example4_30day_Final <- rbind(TADA_Example4_30day_Filled,TADA_Example4_30day_NA)
  
  # Compares the specified stat to the Magnitude Criteria. Count number of exceedance and percent exceedances by param and use
  CriteriaSummary <- TADA_Example4_30day_Final %>%
    dplyr::group_by(
      #AggregatedActivityStartDateTime, 
      DurationPeriod,
      TADA.ComparableDataIdentifier, # TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName, 
      ATTAINS.ParameterName, ATTAINS.UseName,
      ActivityTypeCode, MagnitudeValueLower, MagnitudeValueUpper,
      MonitoringLocationName, MonitoringLocationIdentifier, MonitoringLocationTypeName
    ) %>%
    dplyr::summarize(
      n_Aggregatedsamples = dplyr::n(),
      n_exceedance = sum(geomean_TADA.ResultMeasureValue > MagnitudeValueUpper), # Will need to know what is being compared - geomean, arithmetic mean, max, min etc.
      percent_exccedance = round(n_exceedance/n_Aggregatedsamples * 100, 3)  
    )
  
  
  if( rolling) {
    TADA_Example4_30day2 <- TADA_Example4_30day %>%
      group_by(time_window = floor_date(timestamp, "30 minutes")) %>%
      summarize(total_value = sum(value))
  }

  
  criteria <- dplyr::select(
    TADA_CriteriaMethodology,
    MagnitudeValueLower,	MagnitudeValueUpper,	MagnitudeUnit,	
    DurationValue,	DurationUnit,	DurationAggregation,	
    FrequencyCriteriaValue,	FrequencyCriteriaMethod
  )
  
  data_with_criteria <- dplyr::left_join()
  
  StatsTable <- TADA_Stats(.data)
  
  StatsTable %>% dplyr::group_by()
  
  group_cols <- unique(c("TADA.ComparableDataIdentifier", 
                         "ATTAINS.ParameterName",
                         "ATTAINS.UseName",
                         "ATTAINS.AssessmentUnitIdentifier",
                         "TADA.MonitoringLocationIdentifier"
                         
  ))
  
  CriteriaSummaryTable <- criteriaMethods %>%
    dplyr::left_join(StatsTable, by = "TADA.ComparableDataIdentifier")
    
    dplyr::filter(!is.na(TADA.ResultMeasureValue)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      Location_Count = length(unique(TADA.MonitoringLocationIdentifier)),
      Measurement_Count = length(unique(ResultIdentifier)),
      Non_Detect_Count = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Non-Detect")]),
      Non_Detect_Pct = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Non-Detect")]) / length(TADA.CensoredData.Flag) * 100,
      Non_Detect_Lvls = length(unique(DetectionQuantitationLimitTypeName[TADA.CensoredData.Flag %in% c("Non-Detect")])),
      Over_Detect_Count = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Over-Detect")]),
      Over_Detect_Pct = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Over-Detect")]) / length(TADA.CensoredData.Flag) * 100,
      # To build this fence we take 1.5 times the IQR and then subtract this value
      # from Q1 and add this value to Q3. This gives us the minimum and maximum fence
      # posts that we compare each observation to. Any observations that are more than
      # 1.5 IQR below Q1 or more than 1.5 IQR above Q3 are considered outliers
      UpperFence = (stats::quantile(TADA.ResultMeasureValue, c(.75)) + (1.5 * stats::IQR(TADA.ResultMeasureValue))),
      LowerFence = (stats::quantile(TADA.ResultMeasureValue, c(.25)) - (1.5 * stats::IQR(TADA.ResultMeasureValue))),
      Min = min(TADA.ResultMeasureValue),
      Mean = mean(TADA.ResultMeasureValue),
      Max = max(TADA.ResultMeasureValue),
      Percentile_5th = stats::quantile(TADA.ResultMeasureValue, .05),
      Percentile_10th = stats::quantile(TADA.ResultMeasureValue, .10),
      Percentile_15th = stats::quantile(TADA.ResultMeasureValue, .15),
      Percentile_25th = stats::quantile(TADA.ResultMeasureValue, .25),
      Percentile_50th_Median = stats::quantile(TADA.ResultMeasureValue, .50),
      Percentile_75th = stats::quantile(TADA.ResultMeasureValue, .75),
      Percentile_85th = stats::quantile(TADA.ResultMeasureValue, .85),
      Percentile_95th = stats::quantile(TADA.ResultMeasureValue, .95),
      Percentile_98th = stats::quantile(TADA.ResultMeasureValue, .98)
    ) %>%
    dplyr::mutate(ND_Estimation_Method = dplyr::case_when(
      Non_Detect_Pct == 0 ~ as.character("No non-detects to estimate"),
      Non_Detect_Pct > 80 ~ as.character("Percent censored too high for estimation methods"), # greater than 80, cannot estimate
      Non_Detect_Pct < 50 & Non_Detect_Lvls > 1 ~ as.character("Kaplan-Meier"), # less than 50% censored, and multiple censoring levels (no minimum n)
      Non_Detect_Pct < 50 ~ as.character("Robust Regression Order Statistics"), # less than 50% censored and one censoring level (no minimum n?)
      Measurement_Count >= 50 ~ as.character("Maximum Likelihood Estimation"), # 50%-80% censored, 50 or more measurements
      Measurement_Count < 50 ~ as.character("Robust Regression Order Statistics")
    )) # 50%-80% censored, less than 50 measures
  
  # StatsTable = StatsTable[,!names(StatsTable)%in%c("Non_Detect_Pct","Non_Detect_Lvls","Over_Detect_Pct")]
  
  return(StatsTable)
  
}
#' #  # Attempt to pull in the ref files from the default Downloads location.
#' #  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
#' #  if (is.null(StandardsRef)) {
#' #    StandardsRef <- openxlsx::read.xlsx(downloads_path, sheet = "DefineCriteriaMethodology")
#' #  }
#' #  # check to see if user-supplied standards ref is a df with appropriate columns and filled out.
#' #  if (!is.null(StandardsRef) & !is.character(StandardsRef)) {
#' #    if (!is.data.frame(StandardsRef)) {
#' #      stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
#' #      ATTAINS.ParameterName,	ATTAINS.OrganizationIdentifier,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
#' #    }
#' #    if (is.data.frame(StandardsRef)) {
#' #      col.names <- c(
#' #        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName"
#' #      )
#' #      ref.names <- names(StandardsRef)
#' #      if (length(setdiff(col.names, ref.names)) > 0) {
#' #        stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
#' #        ATTAINS.ParameterName,	ATTAINS.OrganizationIdentifier,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
#' #      }
#' #    }
#' #  }
#' #  wb <- openxlsx::loadWorkbook(wb, downloads_path)
#' #  tryCatch(
#' #    {
#' #      openxlsx::addWorksheet(wb, "MagnitudeExcursions")
#' #    },
#' #    error = function(e) {
#' #      openxlsx::removeWorksheet(wb, "MagnitudeExcursions")
#' #      openxlsx::addWorksheet(wb, "MagnitudeExcursions")
#' #    }
#' #  )
#' #  # Format column header
#' #  header_st <- openxlsx::createStyle(textDecoration = "Bold")
#' #  # Reference tables (required)
#' #  ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
#' #  UseParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseParamRef")
#' #  AURef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAURef")
#' #  # Contains all AU ref columns such as Site-specific names and User defined exclusions to be joined in the TADA dataframe.
#' #  temp_AU <- .data %>%
#' #    dplyr::right_join(AURef, by = c(
#' #      "MonitoringLocationIdentifier", "MonitoringLocationName",
#' #      "LongitudeMeasure", "LatitudeMeasure", "MonitoringLocationTypeName"
#' #    ), relationship = "many-to-many")
#' #  # Magnitude Excursion Summary
#' #  TADA_MagnitudeExcursions <- StandardsRef %>%
#' #    dplyr::mutate(dplyr::across(c(MagnitudeValueLower, MagnitudeValueUpper), as.numeric)) %>%
#' #    dplyr::mutate(dplyr::across(
#' #      c(
#' #        ATTAINS.WaterType,
#' #        MonitoringLocationTypeName,
#' #        AcuteChronic, SaltFresh, Season, EquationBased,
#' #        ApplyUniqueSpatialCriteria, # Will depend on the user's crosswalk of ML to this criteria for filtering.
#' #      ), as.factor
#' #    )) %>%
#' #    dplyr::left_join(temp_AU, by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many") %>%
#' #    dplyr::distinct() %>%
#' #    dplyr::mutate(across(MagnitudeValueLower, as.numeric)) %>%
#' #    dplyr::group_by(.[, c(
#' #      "TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName",
#' #      "ATTAINS.OrganizationIdentifier", "ATTAINS.UseName",
#' #      "ATTAINS.assessmentunitidentifier", "MonitoringLocationIdentifier",
#' #      "MonitoringLocationTypeName.y", "ATTAINS.WaterType.y", "AcuteChronic", "SaltFresh",
#' #      "BegAssessDate", "EndAssessDate",
#' #      "Season", "MinimumSample", "ApplyUniqueSpatialCriteria.y",
#' #      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit"
#' #    )]) %>%
#' #    dplyr::summarise(
#' #      n_MonitoringLocationID = length(unique(MonitoringLocationIdentifier)),
#' #      n_discrete = sum(!is.na(TADA.ResultMeasureValue)),
#' #      n_exceedance = sum(TADA.ResultMeasureValue < MagnitudeValueLower, na.rm = TRUE) + sum(TADA.ResultMeasureValue > MagnitudeValueUpper, na.rm = TRUE),
#' #      .groups = "drop"
#' #    )
#' #  if (!is.null(UseAURef)) {
#' #    # If a user provides UseAURef and UseParamRef, this creates a Use name to AU and Parameter crosswalk. This helps filter down the summary list further.
#' #    UseParamAU <- UseAURef %>%
#' #      dplyr::right_join(UseParamRef, by = c("ATTAINS.UseName", "ATTAINS.OrganizationIdentifier"), relationship = "many-to-many") %>%
#' #      dplyr::filter(!(!ATTAINS.OrganizationIdentifier %in% c("EPA304a") & is.na(ATTAINS.assessmentunitidentifier))) %>%
#' #      dplyr::select(
#' #        ATTAINS.OrganizationIdentifier, ATTAINS.assessmentunitidentifier,
#' #        ATTAINS.assessmentunitname, TADA.ComparableDataIdentifier,
#' #        EPA304A.PollutantName, ATTAINS.ParameterName, ATTAINS.UseName
#' #      )
#' #    UseParamAU2 <- UseParamAU %>%
#' #      dplyr::group_by(ATTAINS.ParameterName, ATTAINS.assessmentunitidentifier, ATTAINS.assessmentunitname) %>%
#' #      dplyr::summarize(.groups = "keep") %>%
#' #      dplyr::mutate(ATTAINS.OrganizationIdentifier = "EPA304a") %>%
#' #      stats::na.omit() %>%
#' #      dplyr::full_join(UseParamAU, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName"), relationship = "many-to-many") %>%
#' #      dplyr::mutate(
#' #        ATTAINS.assessmentunitidentifier = dplyr::coalesce(ATTAINS.assessmentunitidentifier.x, ATTAINS.assessmentunitidentifier.y),
#' #        ATTAINS.assessmentunitname = dplyr::coalesce(ATTAINS.assessmentunitname.x, ATTAINS.assessmentunitname.y)
#' #      ) %>%
#' #      dplyr::select(-c(ATTAINS.assessmentunitidentifier.x, ATTAINS.assessmentunitidentifier.y, ATTAINS.assessmentunitname.x, ATTAINS.assessmentunitname.y)) %>%
#' #      dplyr::distinct()
#' #    TADA_MagnitudeExcursions <- TADA_MagnitudeExcursions %>%
#' #      dplyr::right_join(UseParamAU2)
#' #  }
#' #  # set zoom size
#' #  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
#' #  sV <- wb$worksheets[[8]]$sheetViews
#' #  wb$worksheets[[8]]$sheetViews <- set_zoom(90)
#' #  # Format header and bodystyle
#' #  header_st <- openxlsx::createStyle(textDecoration = "Bold")
#' #  bodyStyle <- openxlsx::createStyle(wrapText = TRUE)
#' #  # Write column names in the excel spreadsheet under the tab [DefineStandards]
#' #  # writeData(wb, "DefineStandards", startCol = 1, x = par, headerStyle = header_st)
#' #  # Export DefineStandards dataframe into the excel spreadsheet tab
#' #  openxlsx::writeData(wb, "MagnitudeExcursions", startCol = 1, x = TADA_MagnitudeExcursions, headerStyle = header_st)
#' #  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
#' #  if (!is.null(downloads_path)) {
#' #    # saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
#' #    downloads_path <- downloads_path
#' #  }
#' #  if (overwrite == TRUE) {
#' #    openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
#' #  }
#' #  if (overwrite == FALSE) {
#' #    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
#' #    openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
#' #  }
#' #  cat("File saved to:", gsub("/", "\", downloads_path), "\n")
#' #  MagnitudeExcursions <- openxlsx::read.xlsx(downloads_path, sheet = "MagnitudeExcursions")
#' #  return(MagnitudeExcursions)
# }