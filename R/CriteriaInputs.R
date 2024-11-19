#' ATTAINS Parameter Name and TADA/WQP Parameter Name Crosswalk
#' 
#' Users will be required to complete and provide a crosswalk of the appropriate mapping
#' of ATTAIN parameter names found in prior ATTAINS assessment cycles for their
#' organization with the TADA/WQP parameter names found in the user's TADA datapull.
#' This specification will need to be done by TADA.CharacteristicName, TADA.MethodSpeciationName, 
#' and TADA.ResultSampleFractionText. Cells are color coded to help guide users on where to 
#' input information to complete the crosswalk.
#' 
#' If an ATTAINS parameter name is not listed as an
#' allowable value choice in the data validation table, users should consider contacting
#' ATTAINS to add this to the domain list. Otherwise, users will need to value paste the value 
#' outside the created spreadsheet from this function to override the data validation options.
#' 
#' Users who have a complete dataframe of this parameter crosswalk, that contains the 
#' required column names from this function, can provide this as a reference for an argument
#' (to be added in the future), which would allow users to join this data to the output table 
#' from this function. From there, users will need to determine if any new TADA parameters 
#' need to be matched with any ATTAIN parameters based on any missing cell values.
#' 
#'
#' @param .data A TADA dataframe. Users are expected to have already run the appropriate data 
#' cleaning, processing, harmonization and filtering functions prior to this step as well as
#' provide the geospatial components with Module 2 TADA_GetATTAINS() function.
#' 
#' @param overwrite A Boolean value that allows users to not overwrite the excel file
#' that is created. This will help prevent a user from overwriting their progress
#' 
#' @param downloads_path A Character value that allows users to specify the location
#' path on where to save the file. Users must specify the path and a file name with
#' .xlsx suffix. The default is in the user's Downloads folder as "myfileRef.xlsx".
#' 
#' @return A data frame with all allowable ATTAINS Parameter by the organization state code
#' and an excel spreadsheet will be created in the Downloads folder or other user defined
#' folder's path.
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateParamUseRef(Data_Nutrients_UT)
#' 

TADA_CreateParamRef <- function(.data, paramRef = NULL, validate = FALSE, excel = TRUE, overwrite = FALSE){ 
  
  # If user does not define the path, attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  CST_param <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop("TADA_CreateParamRef: 'paramRef' must be a data frame with these four columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName")
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.ParameterName"
      )
      
      ref.names <- names(paramRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateParamRef: 'paramRef' must be a data frame with these four columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName")
      }
    }
  }
  
  # Pulls in StateCode to get entity's stateCode
  stateCode <- utils::read.csv(system.file("extdata", "statecode.csv", package = "EPATADA"))
  stateCode$STATE <- sprintf("%02d", stateCode$STATE)
  entity <- unique(.data$StateCode)
  
  # Returns the name of the entity based on the stateCode
  entity_name <- dplyr::filter(stateCode, STATE %in% entity)[[2]]
  
  org_name <- rATTAINS::domain_values(domain_name = "OrgName") %>% 
    dplyr::filter(name %in% entity_name)
  
  if(nrow(org_name) > 1){
    # Is this possible for more than one state to show up in a df if a single state is doing their WQS assessments?
    message("More than one organization found in your dataframe. Pulling in allowable parameter domain values found in all organization through ATTAINS geospatial services")
  }
  
  # Pulls in all unique combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText in user's dataframe.
  TADA_param <- dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "ATTAINS.organizationname")]) %>%
    dplyr::left_join(CST_param, "TADA.CharacteristicName") %>%
    dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName, organization_name = ATTAINS.organizationname) %>%
    dplyr::arrange(TADA.CharacteristicName) %>%
    group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
    filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T)
  
  if(is.null(paramRef)){
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = NA) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName, ATTAINS.ParameterName, organization_name) %>%
      dplyr::arrange(TADA.CharacteristicName) %>%
      group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
      filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T)
    }
  
  if(!is.null(paramRef)){
    CreateParamRef <- TADA_param %>%
      dplyr::left_join(paramRef, c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "organization_name")) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName = CST.PollutantName.x, ATTAINS.ParameterName, organization_name) %>%
      dplyr::arrange(TADA.CharacteristicName) %>%
      group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
      filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T) %>%
      dplyr::distinct()
  }
  
  # Pulls in all domain values of parameter names in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::filter(organization_name %in% org_name) %>%
    dplyr::arrange(parameter)
  
  # # Uses rATTAINS to pull in all allowable parameter values by the entity in the dataframe
  # ATTAINS_param <- list()
  # for(i in 1:nrow(org_name)){
  #   ATTAINS_param[[i]] <- rATTAINS::assessments(
  #     organization_id = org_name[["code"]][i]
  #   )
  # 
  # # Extracts all unique parameter names by the entity
  # ATTAINS_param[[i]] <- ATTAINS_param[[i]]$use_assessment %>%
  #   dplyr::select(organization_identifier, organization_name, parameters, use_attainments) %>%
  #   tidyr::unnest(c(use_attainments), names_sep = ".") %>%
  #   tidyr::unnest(c(parameters), names_sep = ".") %>%
  #   dplyr::select(parameter, organization_identifier, organization_name, use_attainments.use_name) %>%
  #   dplyr::distinct() %>%
  #   dplyr::arrange(parameter)
  # }
  # 
  # ATTAINS_param <- Reduce(dplyr::full_join, ATTAINS_param)
  
  # Create column names for an empty dataframe
  columns <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "CST.PollutantName", "ATTAINS.ParameterName", "organization_name", "ATTAINS.FlagParameterName"
  )
  
  par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
  colnames(par) = columns
  
  if(excel == TRUE){
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "CreateParamRef", visible = TRUE)
  addWorksheet(wb, "Index", visible = FALSE)
  addWorksheet(wb, "ATTAINSParamAllOrgs", visible = FALSE)
  addWorksheet(wb, "ATTAINSParamRef", visible = FALSE)
  #addStyle(wb, "ATTAINSParamRef", rows = 1:200, cols = 1:2, gridExpand = TRUE, style = createStyle(locked = FALSE))
  
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[1]]$sheetViews
  wb$worksheets[[1]]$sheetViews <- set_zoom(90)
  # Format header and bodystyle
  header_st <- createStyle(textDecoration = "Bold")
  bodyStyle <- createStyle(wrapText = TRUE)
  
  # Index of ATTAINS allowable values of Entity/Parameter
  writeData(wb, "Index", startCol = 4, x = ATTAINS_param)
  writeData(wb, "Index", startCol = 3, x = data.frame(organization_name = c(unique(ATTAINS_param$organization_name),"EPA 304a"))) # Should we allow all orgs?
  writeData(wb, "Index", startCol = 2, x = CST_param$CST.PollutantName) # Should we allow all orgs?
  # Index of Unique combinations of WQP TADA Parameters found in TADA dataframe
  # writeData(wb, "Index", startCol = 1, x = CreateParamRef)
  
  # Creates an ATTAINS Param ref tab for state and all orgs on separate sheets.
  writeData(wb, "ATTAINSParamAllOrgs", startCol = 1, 
            x = 
              ATTAINS_param_all %>% 
                dplyr::select(parameter, organization_name) %>%
                dplyr::distinct()
            , headerStyle = header_st)
  writeData(wb, "ATTAINSParamRef", startCol = 1, 
            x = ATTAINS_param %>% 
              dplyr::select(parameter, organization_name) %>% 
              dplyr::distinct()
            , headerStyle = header_st)
  addStyle(wb, sheet = "ATTAINSParamRef", bodyStyle, cols = 2, rows = 1:200)
  addStyle(wb, sheet = "ATTAINSParamRef", bodyStyle, cols = 3, rows = 1:200)
  
  writeData(wb, "CreateParamRef", startCol = 1, x = par, headerStyle = header_st) # Write column names in the excel spreadsheet under the tab [CreateParamRef]
  
  # Format Column widths
  openxlsx::setColWidths(wb, "ATTAINSParamAllOrgs", cols = 1:ncol(par), widths = "auto")
  openxlsx::setColWidths(wb, "ATTAINSParamRef", cols = 1:ncol(par), widths = 40)
  openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(par), widths = "auto")
  
  writeData(wb, "CreateParamRef", startCol = 1, x = CreateParamRef)
  
  # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab
    
    suppressWarnings(dataValidation(wb, sheet = "ATTAINSParamRef", cols = 1, rows = 2:5000, type = "list", value = sprintf("'ATTAINSParamAllOrgs'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'ATTAINSParamRef'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index'!$C$2:$C$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    conditionalFormatting(wb, "ATTAINSParamRef", 
                          cols = 1:2, rows = 2+nrow(ATTAINS_param %>% 
                                                      dplyr::select(parameter, organization_name) %>% 
                                                      dplyr::distinct()):1000, 
                          type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8]))
    
    n <- nrow(ATTAINS_param %>% 
                dplyr::select(parameter, organization_name) %>% 
                dplyr::distinct()
    )
    for(i in 1:20){
      writeFormula(wb, "ATTAINSParamRef", startCol = 3, 
                   startRow = i+1+n, array = TRUE, 
                   x = paste0('=IF(ISNA(MATCH(1,(A', i+1+n,'=Index!D:D)*(B',i+1+n,'=Index!F:F),0)),"Warning: This parameter is not listed as a prior cause for your orgnaization. User will need to manually add additional rows to input any designated use for this parameter done in TADA_CreateParamUseRef","")')
      )
      
      writeFormula(wb, "ATTAINSParamRef", startCol = 2, 
                   startRow = i+1+n, array = TRUE,
                   x = paste0('=IF(ISBLANK(A', i+1+n,'),"",TEXTJOIN(",",,IF(A',i+1+n,'=ATTAINSParamAllOrgs!A:A,ATTAINSParamAllOrgs!B:B,"")))')
      )  
      setRowHeights(wb, "ATTAINSParamRef", rows = 1+i+n, height = 100)
    }
    
    for(i in 1:nrow(TADA_param)){
      # writeFormula(wb, "CreateParamRef", startCol = 5, 
      #            startRow = i+1, array = TRUE,
      #            x = paste0('=IFERROR(INDEX(Index!D:F,MATCH(CreateParamRef!D', i+1,',Index!D:D,0),3),"")')
      #           ) 
      writeFormula(wb, "CreateParamRef", startCol = 7, 
                   startRow = i+1, array = TRUE,
                   x = paste0('=IF(AND(ISNA(MATCH(1,(E',i+1,'=Index!H:H)*(F',i+1,'=Index!E:E),0)),E',i+1, '<>""),
                              "Suspect: parameter not listed as a prior cause for your organization",IF(E', i+1,'="",
                              "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
                              "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments"))'
                              )
      )
  }
  
  for(i in 1:nrow(TADA_param)){
    conditionalFormatting(wb, "CreateParamRef", 
                          cols = 5:6, rows = 1:i+1, 
                          type = "blanks", style = createStyle(bgFill = "#FFC7CE"))
    
    conditionalFormatting(wb, "CreateParamRef", 
                          cols = 5:6, rows = 1:i+1, 
                          type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8]))
  }

  if(overwrite == TRUE){
    print(paste0('Overwriting sheet [CreateParamRef] in ', downloads_path))
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    print("If you would like to replace sheet [CreateParamRef], use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  # ParamRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  }
  
  # The following below will create an R dataframe in the R environment. Users will have greater flexibility modifying the dataframe in this environment if it is preferred.
  CreateParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  
  if(is.null(paramRef)){
    CreateParamRef <- CreateParamRef %>%
      dplyr::mutate(ATTAINS.ParameterName = NA, ATTAINS.FlagParameterName = "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment")
  }
  
  if(!is.null(paramRef)){
    CreateParamRef <- CreateParamRef %>% 
      #dplyr::left_join(ATTAINS_param, by = c("ATTAINS.ParameterName" = "parameter")) %>%
      dplyr::mutate(ATTAINS.FlagParameterName = case_when(
        organization_name == "EPA 304a" & CST.PollutantName != NA ~ "Pass: Will use the EPA 304a recommended standards for this parameter",
        organization_name == "EPA 304a" & CST.PollutantName != NA ~ "Suspect: No CST.PollutantName found. Please select an appropriate crosswalk if you would like to use the EPA 304a recommended standards for this parameter",
        is.na(organization_name) & is.na(ATTAINS.ParameterName) ~ "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
        is.na(organization_name) & !is.na(ATTAINS.ParameterName) ~ "Suspect: parameter not listed as a prior cause for this organization",
        !is.na(organization_name) & !is.na(ATTAINS.ParameterName) ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments")
      ) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName,	ATTAINS.ParameterName, organization_name,	ATTAINS.FlagParameterName)
  }
  
  # Re-runs the flagging data after a user has inputted values - will need to be done if a user inputs values in the R environment
  if( validate == TRUE){
    paramRef <- paramRef %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName,	ATTAINS.ParameterName, organization_name,	ATTAINS.FlagParameterName)
    
    CreateParamRef <- CreateParamRef %>% 
      dplyr::left_join(paramRef, by = c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "CST.PollutantName", "ATTAINS.ParameterName")) %>%
      dplyr::mutate(ATTAINS.FlagParameterName = case_when(
        organization_name.y == "EPA 304a" & !is.na(CST.PollutantName) ~ "Pass: Will use the EPA 304a recommended standards for this parameter",
        organization_name.y == "EPA 304a" & is.na(CST.PollutantName) ~ "Suspect: No CST.PollutantName found. Please select an appropriate crosswalk if you would like to use the EPA 304a recommended standards for this parameter",
        organization_name.y != "EPA 304a" & is.na(organization_name.y) & is.na(ATTAINS.ParameterName) ~ "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
        organization_name.y != "EPA 304a" & is.na(organization_name.y) & !is.na(ATTAINS.ParameterName) ~ "Suspect: parameter not listed as a prior cause for this organization",
        organization_name.y != "EPA 304a" & !is.na(organization_name.y) & !is.na(ATTAINS.ParameterName) ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments")
      ) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName,	ATTAINS.ParameterName, organization_name = organization_name.y,	ATTAINS.FlagParameterName)
  }
  
  return(CreateParamRef)
}  

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

TADA_CreateParamUseRef <- function(ParamRef = NULL, overwrite = FALSE, downloads = NULL, user_guide = FALSE){ 

  # If user does not define the path, will attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  # testing out different downloads_path as an argument is needed.
  if(!is.null(downloads)){
    downloads_path <- downloads
  }
  
  # Checks if ref contains a dataframe and necessary columns to proceed.
  if(is.null(ParamRef)){
    ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  }
  
  if(sum(is.na(ParamRef$ATTAINS.ParameterName)) > 1){
    warning("NAs were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function")
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(ParamRef) & !is.character(ParamRef)) {
    if (!is.data.frame(ParamRef)) {
      stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName, ATTAINS.OrgName")
    }
    
    if (is.data.frame(ParamRef)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.ParameterName", "ATTAINS.OrgName"
      )
      
      ref.names <- names(ParamRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName, ATTAINS.OrgName")
      }
    }
  }
 
  wb <- loadWorkbook(wb, downloads_path)
  
  # If a user chooses to rerun the TADA_CreateParamUseRef() function, the sheet will already exist and error. 
  tryCatch({
    addWorksheet(wb, "CreateParamUseRef")
    },
    error = function(e){
      removeWorksheet(wb, "CreateParamUseRef")
      addWorksheet(wb, "CreateParamUseRef")
    }
  )
  
  CreateParamUseRef <- ParamRef %>%
    dplyr::left_join(openxlsx::read.xlsx(downloads_path, sheet = "Index", cols = 4:7), by = c("ATTAINS.ParameterName" = "parameter"), relationship = "many-to-many") %>%
    # dplyr::filter(organization_name %in% unique(ParamRef$ATTAINS.OrgName)) %>%
    dplyr::filter(ATTAINS.ParameterName %in% unique(ParamRef$ATTAINS.ParameterName)) %>%
    # dplyr::rename(parameter, ATTAINS.ParameterName) %>%
    dplyr::select(ATTAINS.OrgName,	ATTAINS.ParameterName, use_attainments.use_name) %>%
    dplyr::mutate(nrowsStandardsRep = as.numeric(1)) %>%
    dplyr::mutate(add_nrows_AcuteChronic = 0) %>%
    dplyr::mutate(add_nrows_WaterTypes = 0) %>%
    dplyr::mutate(add_nrows_SiteSpecific = 0) %>%
    dplyr::mutate(add_nrows_Other = 0) %>%
    dplyr::rename(ATTAINS.UseName = use_attainments.use_name) %>%
    dplyr::arrange(ATTAINS.ParameterName, ATTAINS.UseName) %>%
    dplyr::distinct()

  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[3]]$sheetViews
  wb$worksheets[[3]]$sheetViews <- set_zoom(90)
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Format Column widths
  openxlsx::setColWidths(wb, "CreateParamUseRef", cols = 1:ncol(CreateParamUseRef), widths = "auto")
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[3]]$sheetViews
  wb$worksheets[[3]]$sheetViews <- set_zoom(90)
  
  # Export CreateParamUseRef dataframe into the excel spreadsheet tab
  writeData(wb, "CreateParamUseRef", startCol = 1, x = CreateParamUseRef, headerStyle = header_st)
  
  # Writes an excel formula for nrowsStandardsRep column. Will be based on other two column values
  for(i in 1:nrow(CreateParamUseRef)){
    writeFormula(wb, "CreateParamUseRef", startCol = 4, startRow = i + 1, x = paste0('=1+E',i+1,'+','F',i+1,'+G',i+1,'+H',i+1))
  }
  addStyle(wb,  "CreateParamUseRef", cols = 3, rows = 1:(nrow(CreateParamUseRef)+2) , style = createStyle(numFmt = "0"), gridExpand = TRUE)
  
  # If the user chooses to enter a separate ParamRef dataframe argument, this makes sure the excel tab reflects the user's changes
  writeData(wb, "CreateParamRef", startCol = 1, x = ParamRef, headerStyle = header_st)
  
  # Conditional Formatting
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 4, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "!=1" , style = createStyle(fontColour = TADA_ColorPalette()[13])) # using yellow to indicate modified cell
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 4, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==1", style = createStyle(fontColour = TADA_ColorPalette()[9])) # default values or indicates good to go cells.
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 5, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==1" , style = createStyle(bgFill = TADA_ColorPalette()[8])) # using yellow to indicate modified cell
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 5, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==0", style = createStyle(bgFill = TADA_ColorPalette()[9])) # default values or indicates good to go cells.
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 5, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = ">1" , style = createStyle(bgFill = TADA_ColorPalette()[13])) # red to indicate potential error in this cell.
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 6:8, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "!=0" , style = createStyle(bgFill = TADA_ColorPalette()[8])) # using yellow to indicate modified cell
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 6:8, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==0", style = createStyle(bgFill = TADA_ColorPalette()[9])) # default values or indicates good to go cells.
  dataValidation(wb, "CreateParamUseRef", 
                 cols = 5:8, rows = 2:(nrow(CreateParamUseRef) + 1), type = "whole", operator = "between", value = c(0, 9))
  
  # Orders the tab/sheets in the User Excel spreadsheet
  length(sheets(wb))
  #worksheetOrder(wb) <- c(1,3,2,4)
  
  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
  if(!is.null(downloads)){
    #saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
    downloads_path <- downloads
  }
  
  if(overwrite == TRUE){
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  ParamUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamUseRef")
  # Error handling below, this makes the column for 'nrowsStandardsRep' to equal all 1 by default in this dataframe as the return value.
  ParamUseRef$nrowsStandardsRep <- as.numeric(1)

  return(ParamUseRef)
}


#' Assessment Unit and MonitoringLocationName/MonitoringLocationType/MonitoringLocationId Crosswalk
#'
#' This function will pull in all MonitoringLocationName/MonitoringLocationType/MonitoringLocationId 
#' for AUID(s) from a TADA dataframe with ATTAINS data. This function requires users to have already 
#' ran TADA_GetATTAINS(). Users are able to specify which AUID(S) to pull in from this ref file 
#' when creating the final CriteriaRef file to be compatible with an organization's WQS assessments.
#' 
#' Users are expected to modify this AUID ref file with the appropriate AUID and 
#' MonitoringLocationName/MonitoringLocationType/MonitoringLocationId crosswalk
#' for the current Assessment cycle. Users can decide to "Include or Exclude" a MonitoringLocation 
#' within an AU if desired. This can be used if a MoniotringLocation would still like to be 
#' crosswalk to the AU but may only be applicable for certain parameters.
#' 
#' @param .data A TADA dataframe with TADA_GetATTAINS() geospatial function ran.
#' 
#' @param AUID Character argument. Users can specify which AUID they are interested in
#' defining WQS criteria for. If this argument is left as NULL, then all unique AUID
#' records will be displayed in this ref file for users to define.
#' 
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#' 
#' @export
#'
#' @examples
#' # Loads dataframe with the example dataframe for Data_Nutrient_UT with TADA_GetATTAINS() ran.
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' 
#' # Creates a crosswalk for MonitoringLocationName/MonitoringLocationType/MonitoringLocationId for only AUID = "UT16020102-053_00"
#' Data_Nutrients_AUID_ref <- TADA_CreateAUIDRef(Data_Nutrients_UT_ATTAINS$TADA_with_ATTAINS, AUID = "UT16020102-053_00")
#' # Creates a crosswalk for MonitoringLocationName/MonitoringLocationType/MonitoringLocationId for all AU found in .data.
#' Data_Nutrients_AUID_ref2 <- TADA_CreateAUIDRef(Data_Nutrients_UT_ATTAINS$TADA_with_ATTAINS)
#' 

TADA_CreateAUIDRef <- function(.data, AUID = NULL, downloads_path = NULL, overwrite = FALSE){
  
  # If user does not define the path, attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  # testing out different downloads_path as an argument is needed.
  if(!is.null(downloads_path)){
    downloads_path <- downloads_path
  }
  
  library(rATTAINS)
  
  if (!any(c(
    "TADA_with_ATTAINS", "ATTAINS_catchments", "ATTAINS_points", "ATTAINS_lines", "ATTAINS_polygons"
  ) %in% names(.data))) {
    stop("Your input dataframe was not produced from `TADA_GetATTAINS()` or it was modified. Please create your list of ATTAINS features using `TADA_GetATTAINS(return_sf = TRUE)`")
  }
  
  .data <- .data[["TADA_with_ATTAINS"]]
  
  if(is.null(AUID)){
    print("Creating AUIDRef dataframe for all unique combinations of AUID found in the TADA dataframe by MonitoringLocationName/MonitoringLocationType/MonitoringLocationId.")
  }
  
  if(!is.null(AUID)){
    print(paste0("Filtering by AUIDs = ", AUID, ". Creating a dataframe for unique combinations of these AUID by MonitoringLocationName/MonitoringLocationType/MonitoringLocationId."))
  }
  
  wb <- loadWorkbook(wb, downloads_path)
  
  tryCatch({
    addWorksheet(wb, "CreateAUIDRef")
  },
  error = function(e){
    removeWorksheet(wb, "CreateAUIDRef")
    addWorksheet(wb, "CreateAUIDRef")
  }
  )
  
  # Filters by AUID if desired, otherwise creates a dataframe of all unique AUID in the TADA dataframe pull
  CreateAUIDRef <- .data %>%
    dplyr::filter(if (is.null(AUID)) TRUE 
      else ATTAINS.assessmentunitidentifier == AUID
      ) %>%
    dplyr::select(
      c(
        "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "LongitudeMeasure", "LatitudeMeasure"
      )
    ) %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    dplyr::distinct(.keep_all = FALSE) %>%
    dplyr::mutate(IncludeorExcludeStation = "Include") %>%
    dplyr::mutate(ExcludeStationReason = NA) %>% # Users can customize this for "warm waters", "only trout based waters" etc. 
    dplyr::mutate(SiteSpecificName = NA) %>%
    dplyr::arrange(ATTAINS.assessmentunitname,	ATTAINS.assessmentunitidentifier,	MonitoringLocationIdentifier)
  
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Format Column widths
  openxlsx::setColWidths(wb, "CreateAUIDRef", cols = 8:ncol(CreateAUIDRef), widths = "auto")
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[4]]$sheetViews
  wb$worksheets[[4]]$sheetViews <- set_zoom(90)
  
  writeData(wb, "CreateAUIDRef", startCol = 1, x = CreateAUIDRef, headerStyle = header_st)
  # Writes an excel formula for IncludeorExcludeStation column. Will be based on other two column values
  for(i in 1:nrow(CreateAUIDRef)){
    writeFormula(wb, "CreateAUIDRef", startCol = 8, startRow = i + 1, x = paste0('=IF(I',i+1,'="","Include","Exclude")'))
  }
  # Conditional Formatting
  conditionalFormatting(wb, "CreateAUIDRef",
                        cols = 8, rows = 2:(nrow(CreateAUIDRef) + 1),
                        type = "contains", rule = "Include", style = createStyle(bgFill = TADA_ColorPalette()[9])) # default values or indicates good to go cells.
  conditionalFormatting(wb, "CreateAUIDRef",
                        cols = 8, rows = 2:(nrow(CreateAUIDRef) + 1),
                        type = "contains", rule = "Exclude", style = createStyle(bgFill = TADA_ColorPalette()[8])) # using yellow to indicate modified cell
  # conditionalFormatting(wb, "CreateAUIDRef",
  #                       cols = 8, rows = 2:(nrow(CreateAUIDRef) + 1),
  #                       type = "notContains", rule = c("Exclude","Include"), style = createStyle(bgFill = "red")) # Likely error. Invalid value is possible here.
  conditionalFormatting(wb, "CreateAUIDRef",
                        cols = 9:10, rows = 2:(nrow(CreateAUIDRef) + 1),
                        type = "blanks", style = createStyle(bgFill = TADA_ColorPalette()[9])) # green is default values or indicates good to go cells.
  conditionalFormatting(wb, "CreateAUIDRef",
                        cols = 9:10, rows = 2:(nrow(CreateAUIDRef) + 1),
                        type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8])) # using yellow to indicate modified cell

  # Ensures the file is updated with the user's CreateParamUseRef
  # writeData(wb, "CreateParamUseRef", startCol = 1, x = CreateParamUseRef, headerStyle = header_st)
  
  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
  if(!is.null(downloads_path)){
    #saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
    downloads_path <- downloads_path
  }
  
  if(overwrite == TRUE){
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  CreateAUIDRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAUIDRef")

  return(CreateAUIDRef)
}
  

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

TADA_DefineStandards <- function(.data, ParamUseRef = NULL, AUIDRef = NULL, overwrite = FALSE, downloads_path = NULL){ 
  
  # If user does not define the path, attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  # testing out different downloads_path as an argument is needed.
  if(!is.null(downloads_path)){
    downloads_path <- downloads_path
  }
  
  if(is.null(ParamUseRef)){
    ParamUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamUseRef")
  }
  
  if(is.null(AUIDRef)){
    AUIDRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAUIDRef")
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(ParamUseRef) & !is.character(ParamUseRef)) {
    if (!is.data.frame(ParamUseRef)) {
      stop("TADA_DefineStandards: 'ParamUseRef' must be a data frame with five columns: ATTAINS.OrgName,	ATTAINS.ParameterName,	ATTAINS.UseName,	nrowsStandardsRep")
    }
    
    if (is.data.frame(ParamUseRef)) {
      col.names <- c(
        "ATTAINS.OrgName",	"ATTAINS.ParameterName",	"ATTAINS.UseName",	"nrowsStandardsRep"
      )
      
      ref.names <- names(ParamUseRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineStandards: 'ParamUseRef' must be a data frame with five columns: ATTAINS.OrgName,	ATTAINS.ParameterName,	ATTAINS.UseName,	nrowsStandardsRep")
      }
    }
  }
  
  if (!is.character(AUIDRef)) {
    if (!is.data.frame(AUIDRef)) {
      stop("TADA_DefineStandards: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
    }
    
    if (is.data.frame(AUIDRef)) {
      col.names <- c(
        "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "LongitudeMeasure", "LatitudeMeasure"
      )
      
      ref.names <- names(AUIDRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineStandards: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
      }
    }
  }
  
  if(sum(is.na(AUIDRef$SiteSpecificName)) > 1){
    warning("NAs were found in column SiteSpecificName for your AUIDRef. Please ensure that you have inputted all field values of interest in the SiteSpecificName column that defines your organization's unique site-specific mapping criteria")
  }
  
  wb <- loadWorkbook(wb, downloads_path)
  #removeWorksheet(wb, "CreateParamUseRef")
  tryCatch({
    addWorksheet(wb, "DefineStandards")
  },
  error = function(e){
    removeWorksheet(wb, "DefineStandards")
    addWorksheet(wb, "DefineStandards")
  }
  )
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[5]]$sheetViews
  wb$worksheets[[5]]$sheetViews <- set_zoom(90)
  
  columns <- c(
    "ATTAINS.ParameterName", "ATTAINS.OrgName", "ATTAINS.UseName", 
    "ATTAINS.WaterType","AcuteChronic", "BegAssessDate", "EndAssessDate", 
    "Season", "MinSamplePerDuration", "SiteSpecificName", "ExcludeStationReason", 
    "EquationBased", "StandardValue", "StandardUnit", "StandardLimit"	
  )
  
  #param <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  #colnames(param) = columns
  ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  
  DefineStandards <- ParamRef %>%
    dplyr::left_join(ParamUseRef, by = c("ATTAINS.ParameterName", "ATTAINS.OrgName"), relationship = "many-to-many") %>%
    dplyr::select("ATTAINS.ParameterName", "ATTAINS.OrgName", "ATTAINS.UseName", "nrowsStandardsRep", "add_nrows_AcuteChronic", "add_nrows_WaterTypes", "add_nrows_SiteSpecific") %>%
    dplyr::bind_cols(
      data.frame(
        ATTAINS.WaterType = as.character(NA), AcuteChronic = as.character(NA), BegAssessDate = as.Date(NA),
        EndAssessDate = as.Date(NA), Season = as.character(NA), MinimumSample = as.numeric(NA), SiteSpecificName = as.character(NA), ExcludeStationReason = as.character(NA),
        EquationBased = as.character(NA), StandardValue = as.numeric(NA), StandardUnit = as.character(NA), StandardLimit = as.numeric(NA)
        )
        ) %>%
    distinct() %>%
    tidyr::uncount(nrowsStandardsRep, .id = "id") %>%
    dplyr::mutate(
      AcuteChronic = case_when(
        id == 1 & add_nrows_AcuteChronic == 1 ~ "Acute",
        id == 2 & add_nrows_AcuteChronic == 1 ~ "Chronic")
      ) %>%
    dplyr::mutate(
      ATTAINS.WaterType = case_when(
        add_nrows_WaterTypes >= 1 & !between(id - add_nrows_AcuteChronic - add_nrows_SiteSpecific, 0, add_nrows_WaterTypes - 1)
          ~ "All Others",
        add_nrows_WaterTypes >= 1 & between(id - add_nrows_AcuteChronic - add_nrows_SiteSpecific, 0, add_nrows_WaterTypes - 1)
        ~ "AddWaterTypeHere")
    ) %>%
    dplyr::mutate(
      SiteSpecificName = case_when(
        add_nrows_SiteSpecific >= 1 & (id - add_nrows_AcuteChronic - add_nrows_WaterTypes > 1)
        ~ "AddSiteNameHere")
    ) %>%
    dplyr::select(-c(add_nrows_SiteSpecific, add_nrows_AcuteChronic, add_nrows_WaterTypes, id)) %>%
    dplyr::arrange(ATTAINS.ParameterName, ATTAINS.UseName)
     
  ParamRef$ATTAINS.ParameterName <- as.character(ParamRef$ATTAINS.ParameterName)
  # Pulls in all the units that are found in TADA.ResultMeasure.MeasureUnitCode as unique allowable unit column
  StandardValue <- ParamRef %>%
    dplyr::left_join( 
      (select(.data, "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode") %>% 
         dplyr::distinct() %>%
         drop_na(TADA.ResultMeasure.MeasureUnitCode)
       ), 
      by = c("TADA.CharacteristicName", "TADA.MethodSpeciationName","TADA.ResultSampleFractionText")) %>%
    dplyr::select(ATTAINS.ParameterName, TADA.ResultMeasure.MeasureUnitCode) %>%
    distinct() %>%
    dplyr::right_join(DefineStandards, by = c("ATTAINS.ParameterName")) %>%
    dplyr::select(TADA.ResultMeasure.MeasureUnitCode) %>%
    dplyr::rename(StandardUnit = TADA.ResultMeasure.MeasureUnitCode)
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Format Column widths
  openxlsx::setColWidths(wb, "DefineStandards", cols = 1:ncol(DefineStandards), widths = "auto")
  
  # Write column names in the excel spreadsheet under the tab [DefineStandards]
  #writeData(wb, "DefineStandards", startCol = 1, x = par, headerStyle = header_st)
  # Export DefineStandards dataframe into the excel spreadsheet tab
  writeData(wb, "DefineStandards", startCol = 1, x = DefineStandards, headerStyle = header_st)
  writeData(wb, "DefineStandards", startCol = 13, startRow = 1, x = StandardValue)
  
  writeData(wb, "Index", startCol = 8, startRow = 1, x = data.frame(ATTAINS.WaterType = c(unique(.data$MonitoringLocationTypeName), "All Others", "AddWaterTypeHere", "NA"))) # ATTAINS.WaterType
  writeData(wb, "Index", startCol = 9, startRow = 1, x = data.frame(AcuteChronic = c("Acute", "Chronic", "NA"))) # AcuteChronic
  writeData(wb, "Index", startCol = 10, startRow = 1, x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA"))) #Season
   
  writeData(wb, "Index", startCol = 11, startRow = 1, x = data.frame(SiteSpecificName = c(unique(AUIDRef$SiteSpecificName), "AddSiteNameHere", "NA")))# SiteSpecificName
  writeData(wb, "Index", startCol = 12, startRow = 1, x = data.frame(ExcludeStationReason = c(unique(AUIDRef$ExcludeStationReason), "NA"))) # ExcludeStationReason
  writeData(wb, "Index", startCol = 13, startRow = 1, x = data.frame(EquationBased = c("Yes", "No", "NA"))) # EquationBased
  
  writeData(wb, "Index", startCol = 14, startRow = 1, x = data.frame(StandardUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode))) # StandardUnit
  writeData(wb, "Index", startCol = 15, startRow = 1, x = data.frame(StandardLimit = c("Upper", "Lower", "Range", "NA"))) # StandardLimit
  
  # The list of allowable values for each column in excel tab [DefineStandards] will be defined by the [Index] tab
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$H$2:$H$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ATTAINS.WaterType
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # AcuteChronic
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 8, rows = 2:1000, type = "list", value = sprintf("'Index'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # Season
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 10, rows = 2:1000, type = "list", value = sprintf("'Index'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # SiteSpecificName
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 12, rows = 2:1000, type = "list", value = sprintf("'Index'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ExcludeSiteReason
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 13, rows = 2:1000, type = "list", value = sprintf("'Index'!$M$2:$M$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # EquationBased
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 14, rows = 2:1000, type = "list", value = sprintf("'Index'!$N$2:$N$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # StandardUnit
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 15, rows = 2:1000, type = "list", value = sprintf("'Index'!$O$2:$O$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # StandardLimit
  
  # If the user chooses to enter a separate ParamUseRef dataframe argument, this makes sure the excel tab reflects the user's changes
  # writeData(wb, "CreateParamUseRef", startCol = 1, x = ParamUseRef, headerStyle = header_st)
  # writeData(wb, "CreateAUIDRef", startCol = 1, x = AUIDRef, headerStyle = header_st)
  # 
  # Conditional Formatting...
  # Conditional Formatting
  conditionalFormatting(wb, "DefineStandards",
                        cols = 4:14, rows = 2:(nrow(DefineStandards) + 1),
                        type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8])) # default values or indicates good to go cells.
  conditionalFormatting(wb, "DefineStandards",
                        cols = 4:14, rows = 2:(nrow(DefineStandards) + 1),
                        type = "blanks", style = createStyle(bgFill = TADA_ColorPalette()[13])) # default values or indicates good to go cells.
  
  
  # If the user chooses to enter a separate ParamRef dataframe argument, this will replace the ParamRef tab of the spreadsheet.
  # writeData(wb, "CreateAUIDRef", startCol = 1, x = AUIDRef, headerStyle = header_st)
  # writeData(wb, "CreateParamUseRef", startCol = 1, x = ParamUseRef, headerStyle = header_st)
  
  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
  if(!is.null(downloads_path)){
    #saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
    downloads_path <- downloads_path
  }
  
  if(overwrite == TRUE){
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  DefineStandards <- openxlsx::read.xlsx(downloads_path, sheet = "DefineStandards")

  return(DefineStandards)
}

#' Standards Exceedance
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
#' Data_Nutrients_Param_Ref <- TADA_CreateParamUseRef(Data_Nutrients_UT)
#' 

TADA_StandardsExceedance <- function(.data, StandardsRef = NULL, overwrite = FALSE, downloads_path = NULL){ 
  
  # If user does not define the path, attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # testing out different downloads_path as an argument is needed.
  if(!is.null(downloads_path)){
    downloads_path <- downloads_path
  }
  
  if(is.null(StandardsRef)){
    StandardsRef <- openxlsx::read.xlsx(downloads_path, sheet = "DefineStandards")
  }
  
  # if("AddSiteNameHere" %in% unique(StandardsRef$SiteSpecificName) ){
  #   stop('"AddSiteNameHere" was found under SiteSpecificName in your StandardsRef dataframe. Please ensure you select an appropriate site-specific name for each field value in this column before proceeding')
  # }
   
  # check to see if user-supplied standards ref is a df with appropriate columns and filled out.
  if (!is.null(StandardsRef) & !is.character(StandardsRef)) {
    if (!is.data.frame(StandardsRef)) {
      stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
      ATTAINS.ParameterName,	ATTAINS.OrgName,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
    }

    if (is.data.frame(StandardsRef)) {
      col.names <- c(
        "ATTAINS.OrgName",	"ATTAINS.ParameterName",	"ATTAINS.UseName"
      )

      ref.names <- names(StandardsRef)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
        ATTAINS.ParameterName,	ATTAINS.OrgName,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
      }
    }
  }
  
  wb <- loadWorkbook(wb, downloads_path)

  tryCatch({
    addWorksheet(wb, "StandardsExceedance")
  },
  error = function(e){
    removeWorksheet(wb, "StandardsExceedance")
    addWorksheet(wb, "StandardsExceedance")
  }
  )
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  
  #param <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  #colnames(param) = columns
  
  # TADA_StandardsExceedance <- temp %>%
  #   #dplyr::left_join(TADA_dataframe_test, by = c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName")) %>%
  #   dplyr::group_by(.[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName","ATTAINS.assessmentunitidentifier", "MonitoringLocationIdentifier")]) %>%
  #   dplyr::summarise(
  #     n_discrete_records = length(TADA.ResultMeasureValue),
  #     n_aggregated_records = length(unique(ActivityIdentifier)), # this will include discrete records
  #     .groups = "drop"
  #   ) %>%
  #   dplyr::mutate(n_discrete_count = sum(.$n_discrete_records)) %>%
  #   dplyr::mutate(n_aggregated_count = sum(.$n_aggregated_records))
  ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  ParamUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamUseRef")
  AUIDRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAUIDRef")
  
  # Contains all AUID ref columns such as Site-specific names and User defined exclusions to be joined in the TADA dataframe.
  temp_AUID <- .data %>%
    dplyr::left_join(AUIDRef, by = c("ATTAINS.assessmentunitname",	"ATTAINS.assessmentunitidentifier",	"MonitoringLocationIdentifier",
                                          "MonitoringLocationName",	"LongitudeMeasure",	"LatitudeMeasure", "MonitoringLocationTypeName"
    ))
  
  # This will summarize by groupings of any columns. We will need another group table to join into this if we want an "all" ungrouped row.
  TADA_StandardsExceedance <- ParamRef %>%
    dplyr::left_join(ParamUseRef, by = "ATTAINS.ParameterName", relationship = "many-to-many") %>%
    dplyr::right_join(temp_AUID, by = c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName"), relationship = "many-to-many") %>%
    select(-c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName")) %>% 
    distinct() %>%
    dplyr::left_join(StandardsRef, by = c("ATTAINS.ParameterName","ATTAINS.UseName","ATTAINS.OrgName"), relationship = "many-to-many") %>%
    dplyr::mutate(SiteSpecificName = 
      case_when(SiteSpecificName.x == SiteSpecificName.y ~ SiteSpecificName.x,
                is.na(SiteSpecificName.y) ~ "All",
                !is.na(SiteSpecificName.y) & SiteSpecificName.x != SiteSpecificName.y ~ "flag.removal"
                )
      ) %>% 
    dplyr::mutate(ATTAINS.WaterType2 =
      case_when(ATTAINS.WaterType == MonitoringLocationTypeName ~ ATTAINS.WaterType,
                is.na(ATTAINS.WaterType) ~ "All",
                !is.na(ATTAINS.WaterType) & MonitoringLocationTypeName != ATTAINS.WaterType ~ "flag.removal" # If user does not fill in a water type with a proper domain name (Ex. AddWaterTypeHere )
                # Needs to handle all others case
                )
    ) %>%
    #dplyr::filter(ATTAINS.WaterType2 != "flag.removal") %>%
    dplyr::filter(SiteSpecificName != "flag.removal") %>%
    distinct() %>%
    dplyr::group_by(.[,c("ATTAINS.ParameterName", "ATTAINS.OrgName", "ATTAINS.UseName", 
                         "ATTAINS.WaterType2","AcuteChronic", "BegAssessDate", "EndAssessDate", 
                         "Season", "ATTAINS.assessmentunitidentifier", "SiteSpecificName", "EquationBased",
                         "StandardValue", "StandardUnit", "StandardLimit")]) %>%
    dplyr::summarise(
      n_MonitoringLocationID = length(unique(MonitoringLocationIdentifier)),
      n_discrete = length(TADA.ResultMeasureValue),
      n_exceedance = sum(TADA.ResultMeasureValue > StandardValue),
      .groups = "drop"
    )
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Format Column widths
  openxlsx::setColWidths(wb, "StandardsExceedance", cols = 1:ncol(TADA_StandardsExceedance), widths = "auto")
  
  # Write column names in the excel spreadsheet under the tab [DefineStandards]
  #writeData(wb, "DefineStandards", startCol = 1, x = par, headerStyle = header_st)
  # Export DefineStandards dataframe into the excel spreadsheet tab
  writeData(wb, "StandardsExceedance", startCol = 1, x = TADA_StandardsExceedance, headerStyle = header_st)
  
  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
  if(!is.null(downloads_path)){
    #saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
    downloads_path <- downloads_path
  }
  
  if(overwrite == TRUE){
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  StandardsExceedance <- openxlsx::read.xlsx(downloads_path, sheet = "StandardsExceedance")
  
  return(StandardsExceedance)
}


