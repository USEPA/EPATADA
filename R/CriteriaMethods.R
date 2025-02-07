#' Define Magnitude Standards
#' 
#' Users will need to provide the completed reference tables from
#' TADA_CreateUseParamRef() and TADA_CreateAURef() as the two argument inputs. 
#' This will generate a template for users to fill out and define the Magnitude values 
#' associated with an ATTAINS Parameter name and use name.For each Magnitude value,
#' users will need to ensure they properly define any additional methods that will
#' correctly reflect their assessment standards for that parameter and use. For example,
#' if there are separate standards for acute versus chronic, or for rivers versus estuary,
#' different seasons, then a user will need to create additional rows to reflect this.
#' 
#' Efforts have been to pull in the EPA304a recommended standards automatically from the
#' Criteria Search Tool (CST). Users should validate this final output if a user has 
#' decided to include the EPA304a standards. User will need to determine if any
#' additional adjustments are needed. For example, does the crosswalk between the EPA304A.PollutantName
#' and TADA.ComparableDataIdentifier seem valid for your organization's method? Is your organizaton
#' only interested in providing the EPA304a recommended standards for certain seasons
#' (Fall, Summer, Spring, Winter) etc.
#' 
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#' 
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateUseParamRef(Data_Nutrients_UT)
#' 

TADA_DefineMagnitude <- function(.data, ref = "TADA", paramRef = NULL, UseParamRef = NULL, AURef = NULL, excel = TRUE, overwrite = FALSE){ 
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if(is.null(paramRef)){
    paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  }
  
  if(is.null(UseParamRef)){
    UseParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseParamRef")
  }
  
  # if(is.null(AURef)){
  #   AURef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAURef")
  # }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(UseParamRef) & !is.character(UseParamRef)) {
    if (!is.data.frame(UseParamRef)) {
      stop("TADA_DefineMagnitude: 'UseParamRef' must be a data frame with five columns: organization_identifier,	ATTAINS.ParameterName,	use_name,	nrowsStandardsRep")
    }
    
    if (is.data.frame(UseParamRef)) {
      col.names <- c(
        "organization_identifier",	"ATTAINS.ParameterName",	"use_name"
      )
      
      ref.names <- names(UseParamRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineMagnitude: 'UseParamRef' must be a data frame with five columns: organization_identifier,	ATTAINS.ParameterName,	use_name")
      }
    }
  }
  
  DefineMagnitude <- paramRef %>%
    dplyr::full_join(UseParamRef, by = c("TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_identifier"), relationship = "many-to-many") %>%
    dplyr::select("TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_identifier", "use_name") %>%
    dplyr::bind_cols(
      data.frame(
        MonitoringLocationTypeName = as.character(NA), ATTAINS.waterTypeCode = as.character(NA), 
        AcuteChronic = as.character(NA), SaltFresh = as.character(NA),
        BegAssessDate = as.Date(NA), EndAssessDate = as.Date(NA), Season = as.character(NA), MinimumSample = as.numeric(NA), 
        ApplyUniqueSpatialCriteria = as.character(NA), EquationBased = as.character(NA), 
        MagnitudeValueLower = as.character(NA), MagnitudeValueUpper = as.character(NA), MagnitudeUnit = as.character(NA)
      )
    ) %>%
    dplyr::filter(ATTAINS.ParameterName != "Parameter not used for assessment") %>%
    dplyr::distinct() %>%
    dplyr::arrange(organization_identifier != "EPA304a", organization_identifier)
  
  CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
    dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, use_name, CRITERIATYPE_ACUTECHRONIC, CRITERIATYPEFRESHSALTWATER, CRITERION_VALUE, UNIT_NAME) %>%
    dplyr::mutate(organization_identifier = "EPA304a")
  
  if("EPA304a" %in% DefineMagnitude$organization_identifier){
    DefineMagnitude <- DefineMagnitude %>%
      dplyr::left_join(CST_param, c("EPA304A.PollutantName", "use_name", "organization_identifier"), relationship = "many-to-many") %>%
      dplyr::mutate(AcuteChronic = CRITERIATYPE_ACUTECHRONIC) %>%
      dplyr::mutate(SaltFresh = CRITERIATYPEFRESHSALTWATER) %>%
      dplyr::mutate(MagnitudeValueLower = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE,"-"), stringr::str_extract(CRITERION_VALUE,"[^-]+"),
        "")) %>%
      dplyr::mutate(MagnitudeValueUpper = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE,"-"), stringr::str_split(CRITERION_VALUE, "-", simplify = TRUE)[,2],
        CRITERION_VALUE)) %>%
      dplyr::mutate(dplyr::across(c(MagnitudeValueLower, MagnitudeValueUpper), as.numeric)) %>%
      dplyr::mutate(dplyr::across(
        c(ATTAINS.waterTypeCode, 
          MonitoringLocationTypeName, 
          AcuteChronic, SaltFresh, Season, EquationBased,
          ApplyUniqueSpatialCriteria, # Will depend on the user's crosswalk of ML to this criteria for filtering.
        ), as.factor)
      ) %>%
      dplyr::mutate(MagnitudeUnit = UNIT_NAME) %>%
      dplyr::select(-c(CRITERIATYPEFRESHSALTWATER, CRITERIATYPE_ACUTECHRONIC, CRITERION_VALUE, UNIT_NAME)) %>%
      dplyr::mutate(MagnitudeUnit = toupper(MagnitudeUnit)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(organization_identifier != "EPA304a", organization_identifier)
    
  }
  
  paramRef$ATTAINS.ParameterName <- as.character(paramRef$ATTAINS.ParameterName)
  # Pulls in all the units that are found in TADA.ResultMeasure.MeasureUnitCode as unique allowable unit column
  MagnitudeValue <- paramRef %>%
    dplyr::left_join( 
      (dplyr::select(.data, "TADA.ComparableDataIdentifier", "TADA.ResultMeasure.MeasureUnitCode") %>% 
         dplyr::distinct() %>%
         tidyr::drop_na(TADA.ResultMeasure.MeasureUnitCode)
      ), 
      by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many") %>%
    dplyr::select(ATTAINS.ParameterName, TADA.ResultMeasure.MeasureUnitCode) %>%
    dplyr::distinct() %>%
    dplyr::right_join(DefineMagnitude, by = c("ATTAINS.ParameterName"), relationship = "many-to-many") %>%
    dplyr::select(TADA.ResultMeasure.MeasureUnitCode) %>%
    dplyr::rename(MagnitudeUnit = TADA.ResultMeasure.MeasureUnitCode)
  
  if(excel == TRUE){
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    tryCatch({
      openxlsx::addWorksheet(wb, "DefineMagnitude")
    },
    error = function(e){
      openxlsx::removeWorksheet(wb, "DefineMagnitude")
      openxlsx::addWorksheet(wb, "DefineMagnitude")
    }
    )
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    sV <- wb$worksheets[[7]]$sheetViews
    wb$worksheets[[7]]$sheetViews <- set_zoom(90)
    
    columns <- c(
      "TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", 
      "organization_identifier", "use_name", "MonitoringLocationTypeName",
      "ATTAINS.waterTypeCode", "AcuteChronic", "SaltFresh", "BegAssessDate", "EndAssessDate", 
      "Season", "MinSamplePerDuration", "ApplyUniqueSpatialCriteria", 
      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit"
    )
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(wb, "DefineMagnitude", cols = 1:ncol(DefineMagnitude), widths = "auto")
    openxlsx::setColWidths(wb, sheet = "DefineMagnitude", cols = 1:5, widths = 20)
    
    # Write column names in the excel spreadsheet under the tab [DefineMagnitude]
    #writeData(wb, "DefineMagnitude", startCol = 1, x = par, headerStyle = header_st)
    # Export DefineMagnitude dataframe into the excel spreadsheet tab
    openxlsx::writeData(wb, "DefineMagnitude", startCol = 1, x = DefineMagnitude, headerStyle = header_st)
    #writeData(wb, "DefineMagnitude", startCol = 13, startRow = 1, x = MagnitudeValue)
    
    openxlsx::writeData(wb, "Index", startCol = 9, startRow = 1, x = data.frame(MonitoringLocationTypeName = c(unique(.data$MonitoringLocationTypeName), "All", "NA"))) # WQP MonitoringTypeLocationName
    
    openxlsx::writeData(wb, "Index", startCol = 10, startRow = 1, x = data.frame(ATTAINS.waterTypeCode = c(unique(AURef$ATTAINS.waterTypeCode), "All", "NA"))) # ATTAINS.waterTypeCode
    openxlsx::writeData(wb, "Index", startCol = 11, startRow = 1, x = data.frame(AcuteChronic = c("A", "C", "NA"))) # AcuteChronic
    openxlsx::writeData(wb, "Index", startCol = 12, startRow = 1, x = data.frame(AcuteChronic = c("S", "F", "NA"))) # SaltFresh
    
    openxlsx::writeData(wb, "Index", startCol = 13, startRow = 1, x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA"))) #Season
    
    openxlsx::writeData(wb, "Index", startCol = 14, startRow = 1, x = data.frame(ApplyUniqueSpatialCriteria = c(unique(AURef$ApplyUniqueSpatialCriteria), "NA")))# ApplyUniqueSpatialCriteria
    openxlsx::writeData(wb, "Index", startCol = 15, startRow = 1, x = data.frame(EquationBased = c("Yes", "No", "NA"))) # EquationBased
    
    openxlsx::writeData(wb, "Index", startCol = 16, startRow = 1, x = data.frame(MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode))) # MagnitudeUnit
    
    # The list of allowable values for each column in excel tab [DefineMagnitude] will be defined by the [Index] tab
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # WQP MonitoringTypeLocationName
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 7, rows = 2:1000, type = "list", value = sprintf("'Index'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ATTAINS.waterTypeCode
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 8, rows = 2:1000, type = "list", value = sprintf("'Index'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # AcuteChronic
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # SaltFresh
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 12, rows = 2:1000, type = "list", value = sprintf("'Index'!$M$2:$M$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # Season
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 14, rows = 2:1000, type = "list", value = sprintf("'Index'!$N$2:$N$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ApplyUniqueSpatialCriteria
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 15, rows = 2:1000, type = "list", value = sprintf("'Index'!$O$2:$O$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # EquationBased
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 18, rows = 2:1000, type = "list", value = sprintf("'Index'!$P$2:$P$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # MagnitudeUnit
  
    # Conditional Formatting
    openxlsx::freezePane(wb, "DefineMagnitude", firstActiveRow = 2, firstActiveCol = 6)
    openxlsx::conditionalFormatting(wb, "DefineMagnitude",
                                    cols = 5:18, rows = 2:(nrow(DefineMagnitude) + 1),
                                    type = "notBlanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(wb, "DefineMagnitude",
                                    cols = 5:18, rows = 2:(nrow(DefineMagnitude) + 1),
                                    type = "blanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])) # modified cells.
    
    # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
    if(overwrite == TRUE){
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if(overwrite == FALSE){
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  }
  
  return(DefineMagnitude)
}



#' Magnitude Summary
#' 
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#' 
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateUseParamRef(Data_Nutrients_UT)
#' 

TADA_MagnitudeSummary <- function(.data, StandardsRef = NULL, UseAURef = NULL, overwrite = FALSE){
  
  # Attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if(is.null(StandardsRef)){
    StandardsRef <- openxlsx::read.xlsx(downloads_path, sheet = "DefineMagnitude")
  }
  
  # check to see if user-supplied standards ref is a df with appropriate columns and filled out.
  if (!is.null(StandardsRef) & !is.character(StandardsRef)) {
    if (!is.data.frame(StandardsRef)) {
      stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
      ATTAINS.ParameterName,	organization_identifier,	use_name, StandardValue,	StandardUnit,	StandardLimit")
    }
    
    if (is.data.frame(StandardsRef)) {
      col.names <- c(
        "organization_identifier",	"ATTAINS.ParameterName",	"use_name"
      )
      
      ref.names <- names(StandardsRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
        ATTAINS.ParameterName,	organization_identifier,	use_name, StandardValue,	StandardUnit,	StandardLimit")
      }
    }
  }
  
  wb <- openxlsx::loadWorkbook(wb, downloads_path)
  
  tryCatch({
    openxlsx::addWorksheet(wb, "MagnitudeExcursions")
  },
  error = function(e){
    openxlsx::removeWorksheet(wb, "MagnitudeExcursions")
    openxlsx::addWorksheet(wb, "MagnitudeExcursions")
  })
  
  # Format column header
  header_st <- openxlsx::createStyle(textDecoration = "Bold")
  
  # Reference tables (required)
  ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  UseParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseParamRef")
  AURef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAURef")
  
  # Contains all AU ref columns such as Site-specific names and User defined exclusions to be joined in the TADA dataframe.
  temp_AU <- .data %>%
    dplyr::right_join(AURef, by = c(
      "MonitoringLocationIdentifier", "MonitoringLocationName",	
      "LongitudeMeasure",	"LatitudeMeasure", "MonitoringLocationTypeName"), relationship = "many-to-many")
  
  
  
  # Magnitude Excursion Summary
  TADA_MagnitudeExcursions <- StandardsRef %>%
    dplyr::mutate(dplyr::across(c(MagnitudeValueLower, MagnitudeValueUpper), as.numeric)) %>%
    dplyr::mutate(dplyr::across(
      c(ATTAINS.waterTypeCode, 
        MonitoringLocationTypeName, 
        AcuteChronic, SaltFresh, Season, EquationBased,
        ApplyUniqueSpatialCriteria, # Will depend on the user's crosswalk of ML to this criteria for filtering.
      ), as.factor)
    ) %>%
    dplyr::left_join(temp_AU, by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many") %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(MagnitudeValueLower, as.numeric)) %>%
    dplyr::group_by(.[,c("TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", 
                         "organization_identifier", "use_name", 
                         "ATTAINS.assessmentunitidentifier", "MonitoringLocationIdentifier",
                         "MonitoringLocationTypeName.y", "ATTAINS.waterTypeCode.y", "AcuteChronic", "SaltFresh", 
                         "BegAssessDate", "EndAssessDate", 
                         "Season", "MinimumSample", "ApplyUniqueSpatialCriteria.y", 
                         "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit")]) %>%
    dplyr::summarise(
      n_MonitoringLocationID = length(unique(MonitoringLocationIdentifier)),
      n_discrete = sum(!is.na(TADA.ResultMeasureValue)),
      n_exceedance = sum(TADA.ResultMeasureValue < MagnitudeValueLower, na.rm = TRUE) + sum(TADA.ResultMeasureValue > MagnitudeValueUpper, na.rm = TRUE),
      .groups = "drop"
    )
  
  if(!is.null(UseAURef)){
    # If a user provides UseAURef and UseParamRef, this creates a Use name to AU and Parameter crosswalk. This helps filter down the summary list further.
    UseParamAU <- UseAURef %>%
      dplyr::right_join(UseParamRef, by = c("use_name", "organization_identifier"), relationship = "many-to-many") %>%
      dplyr::filter(!(!organization_identifier %in% c("EPA304a") & is.na(ATTAINS.assessmentunitidentifier))) %>%
      dplyr::select(
        organization_identifier, ATTAINS.assessmentunitidentifier, 
        ATTAINS.assessmentunitname, TADA.ComparableDataIdentifier, 
        EPA304A.PollutantName, ATTAINS.ParameterName, use_name
        )
    
    UseParamAU2 <- UseParamAU %>%
      dplyr::group_by(ATTAINS.ParameterName, ATTAINS.assessmentunitidentifier, ATTAINS.assessmentunitname) %>% 
      dplyr::summarize(.groups = "keep") %>%
      dplyr::mutate(organization_identifier = "EPA304a") %>%
      stats::na.omit() %>%
      dplyr::full_join(UseParamAU, by = c("organization_identifier", "ATTAINS.ParameterName"),relationship = "many-to-many") %>% 
      dplyr::mutate(
        ATTAINS.assessmentunitidentifier = dplyr::coalesce(ATTAINS.assessmentunitidentifier.x, ATTAINS.assessmentunitidentifier.y),
        ATTAINS.assessmentunitname = dplyr::coalesce(ATTAINS.assessmentunitname.x, ATTAINS.assessmentunitname.y)
      ) %>%
      dplyr::select(-c(ATTAINS.assessmentunitidentifier.x, ATTAINS.assessmentunitidentifier.y, ATTAINS.assessmentunitname.x, ATTAINS.assessmentunitname.y)) %>%
      dplyr::distinct()
    
    TADA_MagnitudeExcursions <- TADA_MagnitudeExcursions %>%
      dplyr::right_join(UseParamAU2)
  }
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[8]]$sheetViews
  wb$worksheets[[8]]$sheetViews <- set_zoom(90)
  # Format header and bodystyle
  header_st <- openxlsx::createStyle(textDecoration = "Bold")
  bodyStyle <- openxlsx::createStyle(wrapText = TRUE)
  
  # Write column names in the excel spreadsheet under the tab [DefineStandards]
  # writeData(wb, "DefineStandards", startCol = 1, x = par, headerStyle = header_st)
  # Export DefineStandards dataframe into the excel spreadsheet tab
  openxlsx::writeData(wb, "MagnitudeExcursions", startCol = 1, x = TADA_MagnitudeExcursions, headerStyle = header_st)
  
  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
  if(!is.null(downloads_path)){
    #saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
    downloads_path <- downloads_path
  }
  
  if(overwrite == TRUE){
    openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  MagnitudeExcursions <- openxlsx::read.xlsx(downloads_path, sheet = "MagnitudeExcursions")
  
  return(MagnitudeExcursions)
}


