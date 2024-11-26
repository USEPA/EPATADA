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
#' and an excel spreadsheet will be created in the Downloads folder.
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateParamUseRef(Data_Nutrients_UT)
#' 

TADA_CreateParamRef <- function(.data, org_names = NULL, paramRef = NULL, validate = FALSE, excel = TRUE, overwrite = FALSE){ 
  
  if(is.null(org_names)){
    print("No organization name(s) provided. Attempting to pull in organization names found in the TADA data frame. Users should provide a list of ATTAINS organization state or tribal name that pertains to their assessment. ")
  }
  
  # ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # 304a parameter name and standards are pulled in from the Criteria Search Tool (CST)
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
  
  .data <- as.data.frame(.data)
  
  # Print message if there are many combinations of TADA Characteristic
  n <- nrow(dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")]))
  if(n > 100){
    message(paste0("There are ",n," combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText in your TADA data frame.
                 This may result in slow runtime for TADA_CreateParamRef()"))
  }
  
  # If user does not provide org names, then it will attempt to pull in org name from what is found in the data frame
  if(is.null(org_names)){
    org_names <- unique(na.omit(.data[,"ATTAINS.organizationname"]))
  }
  org_names <- as.list(org_names)
  
  if(length(org_names) > 1){
    print("More than one org_name was defined in your dataframe. Generating duplicate rows for TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText for each orgs that will need a crosswalk")
  }
  
  # Pulls in all unique combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText in user's dataframe.
  TADA_param <- dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")]) %>%
    dplyr::left_join(CST_param, "TADA.CharacteristicName") %>%
    dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
    dplyr::arrange(TADA.CharacteristicName) %>%
    uncount(weights = length(org_names)) %>% mutate(organization_name = rep(org_names, nrow(.)/length(org_names)))
    # group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
    #dplyr::mutate(organization_name = toString(org_names)) %>%
    #separate_rows(organization_name, sep =", ")
  
  if(is.null(paramRef)){
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = NA) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, CST.PollutantName, ATTAINS.ParameterName) %>%
      dplyr::arrange(TADA.CharacteristicName)
    #group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName)
    #filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T)
  }
  
  if(!is.null(paramRef)){
    CreateParamRef <- TADA_param %>%
      dplyr::left_join(paramRef, c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText"), relationship = "many-to-many") %>%
      dplyr::rename(any_of(c(CST.PollutantName = "CST.PollutantName.y", organization_name = "organization_name.y", ATTAINS.ParameterName = "ATTAINS.ParameterName.y"))) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, CST.PollutantName, ATTAINS.ParameterName) %>%
      dplyr::arrange(TADA.CharacteristicName) %>%
      #group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
      #filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T) %>%
      dplyr::distinct()
  }
  
  # Pulls in all domain values of parameter names in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::filter(organization_name %in% org_names) %>%
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
  
  if(validate == FALSE){

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
        dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, CST.PollutantName,	ATTAINS.ParameterName,	ATTAINS.FlagParameterName)
    }

  }
  
  # Re-runs the flagging data after a user has inputted values - will need to be done if a user inputs values in the R environment
  if( validate == TRUE){
    
    if(is.null(paramRef)){
      stop("User must provide a paramRef as an argument input in the function TADA_CreateParamRef() if validate = TRUE")
    }
    
    paramRef <- paramRef %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, CST.PollutantName,	ATTAINS.ParameterName)
    # 
    CreateParamRef <- CreateParamRef %>% 
      dplyr::left_join(paramRef, by = c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")) %>%
      dplyr::mutate(ATTAINS.FlagParameterName = case_when(
        organization_name.y == "EPA 304a" & !is.na(CST.PollutantName.y) ~ "Pass: Will use the EPA 304a recommended standards for this parameter",
        organization_name.y == "EPA 304a" & is.na(CST.PollutantName.y) ~ "Suspect: No CST.PollutantName found. Please select an appropriate crosswalk if you would like to use the EPA 304a recommended standards for this parameter",
        is.na(organization_name.y) & is.na(ATTAINS.ParameterName.y) ~ "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
        is.na(organization_name.y) & !is.na(ATTAINS.ParameterName.y) ~ "Suspect: parameter not listed as a prior cause for this organization",
        !is.na(organization_name.y) & organization_name.y != "EPA 304a" & !is.na(ATTAINS.ParameterName.y) ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments"
      )
      ) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name = organization_name.y, CST.PollutantName = CST.PollutantName.y,	ATTAINS.ParameterName = ATTAINS.ParameterName.y,	ATTAINS.FlagParameterName)
  }
  
  if(excel == TRUE){
    # Create column names for an empty dataframe
    columns <- c(
      "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "CST.PollutantName", "ATTAINS.ParameterName", "organization_name", "ATTAINS.FlagParameterName"
    )
    
    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
    colnames(par) = columns
    
    library(openxlsx)
    wb <- createWorkbook()
    addWorksheet(wb, "CreateParamRef", visible = TRUE)
    addWorksheet(wb, "Index", visible = FALSE)
      
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    sV <- wb$worksheets[[1]]$sheetViews
    wb$worksheets[[1]]$sheetViews <- set_zoom(90)
    # Format header and bodystyle
    header_st <- createStyle(textDecoration = "Bold")
    bodyStyle <- createStyle(wrapText = TRUE)
    
    # Index of ATTAINS allowable values of Entity/Parameter
    writeData(wb, "Index", startCol = 4, x = ATTAINS_param)
    writeData(wb, "Index", startCol = 3, x = data.frame(organization_name = c(unique(ATTAINS_param$organization_name)))) # Should we allow all orgs?
    writeData(wb, "Index", startCol = 2, x = unique(CST_param$CST.PollutantName)) # Should we allow all orgs?
    writeData(wb, "Index", startCol = 1, x = data.frame(ATTAINS.ParameterName = c(unique(ATTAINS_param$parameter),"Parameter not used for assessment"))) 
    openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(par), widths = "auto")
    
    writeData(wb, "CreateParamRef", startCol = 1, x = CreateParamRef, headerStyle = header_st)
    
    # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab
    
    #suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 1, rows = 2:5000, type = "list", value = sprintf("'Index'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$C$2:$C$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    for(i in 1:nrow(TADA_param)){

      writeFormula(wb, "CreateParamRef", startCol = 7, startRow = i + 1, array = TRUE,
                   x = paste0('=IF(F',i+1,'="EPA 304a","Pass: Will use the EPA 304a recommended standards for this parameter",IF(ISNA(MATCH(1,(F',i+1,'=Index!H:H)*(D',i+1,'=Index!E:E),0)),
                "Suspect: parameter name is not found as a prior use for this organization",
                "Pass: parameter name is listed as prior cause in ATTAINS for this organization"))')
      )
    }
    
    for(i in 1:nrow(TADA_param)){
      conditionalFormatting(wb, "CreateParamRef", 
                            cols = 6, rows = 1:i+1, 
                            type = "blanks", style = createStyle(bgFill = "#FFC7CE"))
      
      conditionalFormatting(wb, "CreateParamRef", 
                            cols = 6, rows = 1:i+1, 
                            type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8]))
    }
    
    if(overwrite == TRUE){
      message(paste0('Overwriting sheet [CreateParamRef] in ', downloads_path))
      saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if(overwrite == FALSE){
      message("If you would like to replace sheet [CreateParamRef], use overwrite = TRUE argument in TADA_CreateParamRef")
      saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
    # ParamRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  }
  
  return(CreateParamRef)
}  


#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#' 
#' @param orgList A character with values of c("EPA304a","State-Specific","CustomList")
#' 
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateParamUseRef(Data_Nutrients_UT)
#' 

TADA_CreateParamUseRef <- function(.data, org_names = NULL, paramRef = NULL, excel = FALSE, overwrite = FALSE, orgList = "State-Specific"){ 
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # Checks if ref contains a dataframe and necessary columns to proceed.
  if(is.null(paramRef)){
    paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  }
  
  if(is.null(orgList)){
    print("No organization name provided, users should provide a list of ATTAINS domain organization state or tribal name that pertains to their dataframe. Attempting to pull in organization names found in the TADA data frame.")
  }
  
  if(sum(is.na(paramRef$ATTAINS.ParameterName)) > 1){
    print("NAs were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function")
  }
  
  if(sum(is.na(paramRef$CST.PollutantName)) > 1 & orgList == "EPA304a"){
    print("NAs were found in CST.PollutantName. Please ensure that you have inputted all field values of interest in the CST.PollutantName column generated from TADA_CreateParamRef() function if you are interested in using the 304a recommended standards")
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName, organization_name")
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.ParameterName", "organization_name"
      )
      
      ref.names <- names(paramRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName, organization_name")
      }
    }
  }
  
  # If user does not provide org names, then it will attempt to pull in org name from what is found in the data frame
  if(is.null(org_names)){
    org_names <- unique(na.omit(.data[,"ATTAINS.organizationname"]))
  }
  org_names <- as.list(org_names)
  
  # Pulls in all unique combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText in user's dataframe.
  TADA_param <- dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")]) %>%
    dplyr::left_join(CST_param, "TADA.CharacteristicName") %>%
    dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
    dplyr::arrange(TADA.CharacteristicName) %>%
    # group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName) %>%
    dplyr::mutate(organization_name = toString(org_names)) %>%
    separate_rows(organization_name, sep =",")
  
  # Pulls in all domain values of parameter names in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::select(organization_name, parameter, use_name) %>%
    dplyr::filter(organization_name %in% org_names)
  
  # EPAorgs <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  # Create the parameter-use reference table for validation
  CreateParamUseRef <- paramRef %>%
    dplyr::ungroup() %>%
    separate_rows(organization_name, sep = ", ") %>%
    dplyr::left_join(ATTAINS_param, by = c("ATTAINS.ParameterName" = "parameter", "organization_name"), relationship = "many-to-many") %>%
    dplyr::select(organization_name,	ATTAINS.ParameterName, use_name) %>%
    tidyr::drop_na(ATTAINS.ParameterName) %>%
    dplyr::filter(ATTAINS.ParameterName != "Parameter not used for assessment") %>%
    # dplyr::mutate(nrowsStandardsRep = as.numeric(1)) %>%
    # dplyr::mutate(add_nrows_AcuteChronic = 0) %>%
    # dplyr::mutate(add_nrows_WaterTypes = 0) %>%
    # dplyr::mutate(add_nrows_SiteSpecific = 0) %>%
    # dplyr::mutate(add_nrows_Other = 0) %>%
    # dplyr::rename(ATTAINS.UseName = use_attainments.use_name) %>%
    # dplyr::arrange(ATTAINS.ParameterName, ATTAINS.UseName) %>%
    dplyr::distinct()
  
  if(excel == TRUE){
    # Create column names for an empty dataframe
    columns <- c(
      "organization_name", "ATTAINS.ParameterName", "use_name", "ATTAINS.FlagParameterName", "ATTAINS.FlagUseName"
    )
    
    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
    colnames(par) = columns
    
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
    writeData(wb, "CreateParamUseRef", startCol = 1, x = CreateParamUseRef, headerStyle = header_st)
    
    # data validation drop down list created
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 1, rows = 2:1000, type = "list", value = sprintf("'Index'!$C$2:$C$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 2, rows = 2:1000, type = "list", value = sprintf("'Index'!$H$2:$H$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 3, rows = 2:1000, type = "list", value = sprintf("'Index'!$G$2:$G$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    
    # Writes an excel formula for nrowsStandardsRep column. Will be based on other two column values
    for(i in 1:nrow(CreateParamUseRef)){
      conditionalFormatting(wb, "CreateParamUseRef", 
                            cols = 1:3, rows = 1:i+1, 
                            type = "blanks", style = createStyle(bgFill = "#FFC7CE"))

      writeFormula(wb, "CreateParamUseRef", startCol = 4, startRow = i + 1, array = TRUE,
                   x = paste0('=IF(A',i+1,'="EPA 304a","Pass: Will use the EPA 304a recommended standards for this parameter",IF(ISNA(MATCH(1,(C',i+1,'=Index!G:G)*(A',i+1,'=Index!E:E),0)),
                "Suspect: use name is not listed as a prior use for this organization",IF(ISNA(MATCH(1,(B',i+1,'=Index!H:H)*(C',i+1,'=Index!G:G)*(A',i+1,'=Index!E:E),0)),
                "Suspect: use name is listed as a prior use name in this organization but not for this parameter name",
                "Pass: parameter name and use name are listed as prior cause in ATTAINS for this org and will be used for assessments")))')
      )
    }
    
    if(overwrite == TRUE){
      saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if(overwrite == FALSE){
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
      saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
    
    CreateParamUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamUseRef")
  }
  
  # The following below will create an R dataframe in the R environment. Users will have greater flexibility modifying the dataframe in this environment if it is preferred.
  
  CreateParamUseRef <- CreateParamUseRef %>% 
    dplyr::mutate(ATTAINS.FlagUseName = case_when(
      organization_name == "EPA 304a" ~ "Pass: Will use the EPA 304a recommended standards for this parameter",
      is.na(organization_name) & is.na(ATTAINS.ParameterName) ~ "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
      is.na(organization_name) & !is.na(ATTAINS.ParameterName) ~ "Suspect: parameter not listed as a prior cause for this organization",
      !is.na(organization_name) & !is.na(ATTAINS.ParameterName) ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments")
    ) %>%
    dplyr::select(organization_name, ATTAINS.ParameterName, use_name,	ATTAINS.FlagUseName)
  
  return(CreateParamUseRef)
}
