#' Define Standards
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#' 
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateParamUseRef(Data_Nutrients_UT)
#' 

TADA_DefineMagnitude <- function(.data, ref = "TADA", paramRef = NULL, paramUseRef = NULL, AUIDRef = NULL, excel = TRUE, overwrite = FALSE){ 
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if(is.null(paramRef)){
    paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  }
  
  if(is.null(paramUseRef)){
    paramUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamUseRef")
  }
  
  # if(is.null(AUIDRef)){
  #   AUIDRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAUIDRef")
  # }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(paramUseRef) & !is.character(paramUseRef)) {
    if (!is.data.frame(paramUseRef)) {
      stop("TADA_DefineMagnitude: 'paramUseRef' must be a data frame with five columns: organization_name,	ATTAINS.ParameterName,	use_name,	nrowsStandardsRep")
    }
    
    if (is.data.frame(paramUseRef)) {
      col.names <- c(
        "organization_name",	"ATTAINS.ParameterName",	"use_name"
      )
      
      ref.names <- names(paramUseRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineMagnitude: 'paramUseRef' must be a data frame with five columns: organization_name,	ATTAINS.ParameterName,	use_name")
      }
    }
  }
  
  # if (!is.character(AUIDRef)) {
  #   if (!is.data.frame(AUIDRef)) {
  #     stop("TADA_DefineMagnitude: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
  #   }
  #   
  #   if (is.data.frame(AUIDRef)) {
  #     col.names <- c(
  #       "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
  #       "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "LongitudeMeasure", "LatitudeMeasure"
  #     )
  #     
  #     ref.names <- names(AUIDRef)
  #     
  #     if (length(setdiff(col.names, ref.names)) > 0) {
  #       stop("TADA_DefineMagnitude: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
  #     }
  #   }
  # }
  # 
  # if(sum(is.na(AUIDRef$ApplyUniqueSpatialCriteria)) > 1){
  #   print("NAs were found in column ApplyUniqueSpatialCriteria for your AUIDRef. Please ensure that you have inputted all field values of interest in the ApplyUniqueSpatialCriteria column that defines your organization's unique site-specific mapping criteria")
  # }
  
  DefineMagnitude <- paramRef %>%
    dplyr::full_join(paramUseRef, by = c("TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_name"), relationship = "many-to-many") %>%
    dplyr::select("TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_name", "use_name") %>%
    dplyr::bind_cols(
      data.frame(
        MonitoringLocationTypeName = as.character(NA), ATTAINS.WaterType = as.character(NA), 
        AcuteChronic = as.character(NA), SaltFresh = as.character(NA),
        BegAssessDate = as.Date(NA), EndAssessDate = as.Date(NA), Season = as.character(NA), MinimumSample = as.numeric(NA), 
        ApplyUniqueSpatialCriteria = as.character(NA), EquationBased = as.character(NA), 
        MagnitudeValueLower = as.character(NA), MagnitudeValueUpper = as.character(NA), MagnitudeUnit = as.character(NA)
      )
    ) %>%
    dplyr::filter(ATTAINS.ParameterName != "Parameter not used for assessment") %>%
    dplyr::distinct() %>%
    dplyr::arrange(organization_name != "EPA304a", organization_name)
  
  CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
    dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, use_name, CRITERIATYPE_ACUTECHRONIC, CRITERIATYPEFRESHSALTWATER, CRITERION_VALUE, UNIT_NAME) %>%
    dplyr::mutate(organization_name = "EPA304a")
  
  if("EPA304a" %in% DefineMagnitude$organization_name){
    DefineMagnitude <- DefineMagnitude %>%
      dplyr::left_join(CST_param, c("EPA304A.PollutantName", "use_name", "organization_name"), relationship = "many-to-many") %>%
      dplyr::mutate(AcuteChronic = CRITERIATYPE_ACUTECHRONIC) %>%
      dplyr::mutate(SaltFresh = CRITERIATYPEFRESHSALTWATER) %>%
      dplyr::mutate(MagnitudeValueLower = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE,"-"), stringr::str_extract(CRITERION_VALUE,"[^-]+"),
        CRITERION_VALUE)) %>%
      dplyr::mutate(MagnitudeValueUpper = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE,"-"), stringr::str_split(CRITERION_VALUE, "-", simplify = TRUE)[,2],
        "")) %>%
      dplyr::mutate(MagnitudeUnit = UNIT_NAME) %>%
      dplyr::select(-c(CRITERIATYPEFRESHSALTWATER, CRITERIATYPE_ACUTECHRONIC, CRITERION_VALUE, UNIT_NAME)) %>%
      dplyr::mutate(MagnitudeUnit = toupper(MagnitudeUnit)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(organization_name != "EPA304a", organization_name)
    
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
    sV <- wb$worksheets[[6]]$sheetViews
    wb$worksheets[[6]]$sheetViews <- set_zoom(90)
    
    columns <- c(
      "TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", 
      "organization_name", "use_name", "MonitoringLocationTypeName",
      "ATTAINS.WaterType", "AcuteChronic", "SaltFresh", "BegAssessDate", "EndAssessDate", 
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
    
    openxlsx::writeData(wb, "Index", startCol = 10, startRow = 1, x = data.frame(ATTAINS.WaterType = c(unique(AUIDRef$ATTAINS.WaterType), "All", "NA"))) # ATTAINS.WaterType
    openxlsx::writeData(wb, "Index", startCol = 11, startRow = 1, x = data.frame(AcuteChronic = c("A", "C", "NA"))) # AcuteChronic
    openxlsx::writeData(wb, "Index", startCol = 12, startRow = 1, x = data.frame(AcuteChronic = c("S", "F", "NA"))) # SaltFresh
    
    openxlsx::writeData(wb, "Index", startCol = 13, startRow = 1, x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA"))) #Season
    
    openxlsx::writeData(wb, "Index", startCol = 14, startRow = 1, x = data.frame(ApplyUniqueSpatialCriteria = c(unique(AUIDRef$ApplyUniqueSpatialCriteria), "NA")))# ApplyUniqueSpatialCriteria
    openxlsx::writeData(wb, "Index", startCol = 15, startRow = 1, x = data.frame(EquationBased = c("Yes", "No", "NA"))) # EquationBased
    
    openxlsx::writeData(wb, "Index", startCol = 16, startRow = 1, x = data.frame(MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode))) # MagnitudeUnit
    
    # The list of allowable values for each column in excel tab [DefineMagnitude] will be defined by the [Index] tab
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # WQP MonitoringTypeLocationName
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineMagnitude", cols = 7, rows = 2:1000, type = "list", value = sprintf("'Index'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ATTAINS.WaterType
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
