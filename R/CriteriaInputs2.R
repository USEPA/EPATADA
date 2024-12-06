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
  
  if(validate == FALSE){
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
    dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, EPA304A.PollutantName = CST.PollutantName) %>%
    dplyr::arrange(TADA.CharacteristicName) %>%
    uncount(weights = length(org_names)) %>% 
    dplyr::mutate(organization_name = rep(org_names, nrow(.)/length(org_names)))
    # group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, EPA304A.PollutantName) %>%
    #dplyr::mutate(organization_name = toString(org_names)) %>%
    #separate_rows(organization_name, sep =", ")
  
  if(is.null(paramRef)){
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = NA) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName) %>%
      dplyr::arrange(TADA.CharacteristicName)
    #group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, EPA304A.PollutantName)
    #filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T)
  }
  
  TADA_param$organization_name <- as.character(TADA_param$organization_name)
  
  if(!is.null(paramRef)){
    paramRef$organization_name <- as.character(paramRef$organization_name)
    
    CreateParamRef <- TADA_param %>%
      dplyr::left_join(paramRef, c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "organization_name"), relationship = "many-to-many") %>%
      dplyr::rename(any_of(c(EPA304A.PollutantName = "EPA304A.PollutantName.x", organization_name = "organization_name.x", ATTAINS.ParameterName = "ATTAINS.ParameterName.y"))) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName) %>%
      dplyr::arrange(TADA.CharacteristicName) %>%
      dplyr::filter(organization_name %in% org_names) %>%
      #group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, EPA304A.PollutantName) %>%
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

  }
  
  # Re-runs the flagging data after a user has inputted values - will need to be done if a user inputs values in the R environment
  if(is.null(paramRef)){
    CreateParamRef <- CreateParamRef %>%
      dplyr::mutate(ATTAINS.ParameterName = NA, ATTAINS.FlagParameterName = "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment")
  }
  
  if(!is.null(paramRef)){
  Flag1 <- paramRef %>%
    anti_join(ATTAINS_param_all, by = c("ATTAINS.ParameterName" = "parameter", "organization_name")) %>%
    select(ATTAINS.ParameterName, organization_name) %>%
    distinct() %>%
    mutate(ATTAINS.FlagParameterName1 = case_when(
      ATTAINS.ParameterName == "Parameter not used for assessment" | is.na(ATTAINS.ParameterName) ~ "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
      !is.na(ATTAINS.ParameterName) ~ "Suspect: parameter name is not found as a prior use for this organization")
    )
    
  CreateParamRef <- CreateParamRef %>% 
    left_join(Flag1, c("ATTAINS.ParameterName", "organization_name")) %>%
    dplyr::mutate(ATTAINS.FlagParameterName = case_when(
       !is.na(ATTAINS.FlagParameterName1) ~ ATTAINS.FlagParameterName1, 
       is.na(ATTAINS.FlagParameterName1) ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments")
    ) %>%
    dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, EPA304A.PollutantName,	ATTAINS.ParameterName,	ATTAINS.FlagParameterName)
  }
  
  if(excel == TRUE){
    # Create column names for an empty dataframe
    columns <- c(
      "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_name", "ATTAINS.FlagParameterName"
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
    writeData(wb, "Index", startCol = 2, x = unique(CST_param$EPA304A.PollutantName)) # Should we allow all orgs?
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
                   x = paste0('=IF(OR(F',i+1,'="",F',i+1,'="Parameter not used for assessment"),
                   "Suspect: no parameter name provided. parameter will not be used for assessment",IF(ISNA(MATCH(1,(F',i+1,'=Index!H:H)*(D',i+1,'=Index!E:E),0)),
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

TADA_CreateParamUseRef <- function(.data, org_names = NULL, paramRef = NULL, validate = FALSE, excel = FALSE, overwrite = FALSE){ 
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # Checks if ref contains a dataframe and necessary columns to proceed.
  if(is.null(paramRef)){
    paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  }
  
  if(is.null(org_names)){
    print("No organization name provided, users should provide a list of ATTAINS domain organization state or tribal name that pertains to their dataframe. Attempting to pull in organization names found in the TADA data frame.")
  }
  
  if(sum(!is.na(paramRef$ATTAINS.ParameterName)) == 0 ){
    stop("No values were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function")
  }
  
  if(sum(is.na(paramRef$ATTAINS.ParameterName)) > 1){
    print("NAs were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function")
  }
  
  if(sum(is.na(paramRef$EPA304A.PollutantName)) > 1 && org_names == "EPA304a"){
    print("NAs were found in EPA304A.PollutantName. Please ensure that you have inputted all field values of interest in the EPA304A.PollutantName column generated from TADA_CreateParamRef() function if you are interested in using the 304a recommended standards")
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop("TADA_CreateStandardsRef: 'paramRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName, organization_name")
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.ParameterName", "organization_name"
      )
      
      ref.names <- names(paramRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'paramRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName, organization_name")
      }
    }
  }
  
  if(validate == FALSE){
  # If user does not provide org names, then it will attempt to pull in org name from what is found in the data frame
  if(is.null(org_names)){
    org_names <- unique(na.omit(.data[,"ATTAINS.organizationname"]))
  }

  org_names <- as.list(org_names)
  
  # Pulls in all domain values of parameter names in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::select(organization_name, parameter, use_name) %>%
    dplyr::filter(parameter %in% paramRef$ATTAINS.ParameterName) %>%
    dplyr::filter(organization_name %in% org_names)
  
  # Create the parameter-use reference table for validation
  CreateParamUseRef <- paramRef %>%
    dplyr::ungroup() %>%
    # separate_rows(organization_name, sep = ", ") %>%
    dplyr::left_join(ATTAINS_param, by = c("ATTAINS.ParameterName" = "parameter", "organization_name"), relationship = "many-to-many") %>%
    dplyr::select(organization_name, EPA304A.PollutantName,	ATTAINS.ParameterName, use_name) %>%
    tidyr::drop_na(ATTAINS.ParameterName) %>%
    dplyr::filter(ATTAINS.ParameterName != "Parameter not used for assessment") %>%
    dplyr::distinct()
  
  # If users want the EPA304a standards. This pulls in the CST reference file. Extracts the associated EPA304a pollutant names and its use_names.
  if("EPA304a" %in% org_names){
    CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
      select(EPA304A.PollutantName = POLLUTANT_NAME, use_name = USE_CLASS_NAME_LOCATION_ETC) %>%
      mutate(organization_name = "EPA304a")
    
    EPA_param <- CreateParamUseRef %>%
      left_join(CST_param, c("EPA304A.PollutantName"), relationship = "many-to-many") %>%
      select(organization_name = organization_name.y,	ATTAINS.ParameterName, EPA304A.PollutantName, use_name = use_name.y) %>%
      distinct()
    
    CreateParamUseRef <- CreateParamUseRef %>%
      dplyr::ungroup() %>%
      full_join(EPA_param, c("ATTAINS.ParameterName", "organization_name", "EPA304A.PollutantName", "use_name")) %>%
      dplyr::select(organization_name, EPA304A.PollutantName,	ATTAINS.ParameterName, use_name)
    }
  }
  
  # The following below will create an R dataframe in the R environment. Users will have greater flexibility modifying the dataframe in this environment if it is preferred.
  Flag1 <- CreateParamUseRef %>%
    anti_join(ATTAINS_param_all, by = c("use_name", "organization_name")) %>%
    select(use_name, organization_name) %>%
    distinct() %>%
    mutate(ATTAINS.FlagUseName1 = "Suspect: use name is listed as a prior use name in this organization but not for this parameter name")
  
  Flag2 <- CreateParamUseRef %>%
    anti_join(ATTAINS_param_all, by = c("ATTAINS.ParameterName" = "parameter","use_name", "organization_name"))%>%
    select(use_name, ATTAINS.ParameterName,organization_name) %>%
    distinct() %>%
    mutate(ATTAINS.FlagUseName2 = "Suspect: use name is not listed as a prior use for this organization")
  
  CreateParamUseRef <- CreateParamUseRef %>% 
    left_join(Flag2, c("ATTAINS.ParameterName", "use_name", "organization_name")) %>%
    left_join(Flag1, c("use_name", "organization_name")) %>%
    dplyr::mutate(ATTAINS.FlagUseName = case_when(
      !is.na(ATTAINS.FlagUseName2) ~ ATTAINS.FlagUseName2,
      !is.na(ATTAINS.FlagUseName1) ~ ATTAINS.FlagUseName1,
      organization_name == "EPA304a" ~ "Pass: Will use the EPA304a recommended standards for this parameter",
      organization_name != "EPA304a" ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments")
    ) %>%
    dplyr::select(organization_name, EPA304A.PollutantName, ATTAINS.ParameterName, use_name, ATTAINS.FlagUseName)
  
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
    
    # If a user performs TADA_CreateParamRef() with excel = FALSE, but decides to perform TADA_CreateParamUseRef() with excel = TRUE,
    # and if the file already exists, we need to consider how to display the excel tab for [CreateParamRef] to ensure it does not display
    # non-matching rows to avoid confusion.
    excel_paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.ParameterName", "organization_name")]
    
    if(nrow(dplyr::inner_join(
      paramRef[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.ParameterName", "organization_name")],
      excel_paramRef[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.ParameterName", "organization_name")]))
      !=nrow(excel_paramRef)){
        if(nrow(excel_paramRef)!=nrow(paramRef)){
          warning("Your user-supplied paramRef table does not match the parameter reference table in excel sheet [CreateParamRef]. This may have occured if you have previously ran TADA_CreateParamRef() with excel = TRUE on a previous dataframe and proceeded with running TADA_CreateParamRef() with excel = FALSE and TADACreateParamUseRef() with excel = TRUE")
        }
    }
    
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
    
    # data validation drop down list created
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 1, rows = 2:1000, type = "list", value = sprintf("'Index'!$C$2:$C$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 2, rows = 2:1000, type = "list", value = sprintf("'Index'!$H$2:$H$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$G$2:$G$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    
    # Writes an excel formula for nrowsStandardsRep column. Will be based on other two column values
    for(i in 1:nrow(CreateParamUseRef)){
      conditionalFormatting(wb, "CreateParamUseRef", 
                            cols = 1:4, rows = 1:i+1, 
                            type = "blanks", style = createStyle(bgFill = "#FFC7CE"))

      writeFormula(wb, "CreateParamUseRef", startCol = 5, startRow = i + 1, array = TRUE,
                   x = paste0('=IF(A',i+1,'="EPA304a","Pass: Will use the EPA304a recommended standards for this parameter",IF(ISNA(MATCH(1,(D',i+1,'=Index!G:G)*(A',i+1,'=Index!E:E),0)),
                "Suspect: use name is not listed as a prior use for this organization",IF(ISNA(MATCH(1,(C',i+1,'=Index!H:H)*(D',i+1,'=Index!G:G)*(A',i+1,'=Index!E:E),0)),
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
  }
  return(CreateParamUseRef)
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

TADA_CreateAUIDRef <- function(.data, AUID = NULL, overwrite = FALSE){
  
  # data <- rATTAINS::assessments(organization_id = "MDE_EASP")
  # 
  # use_assessments <- data$use_assessment
  # use_attainments <- use_assessments %>% unnest(c(use_attainments), names_sep = ".")
  # use_parameters <- use_attainments %>% unnest(c(parameters), names_sep = ".")
  # 
  # use_data <- use_parameters %>%
  #   dplyr::select(
  #     organization_identifier, organization_name, organization_type_text,
  #     use_attainments.use_name, parameters.parameter_name) %>%
  #   distinct()
  # 
  # rm(use_assessments, use_attainments, use_parameters)
  # 
  # 
  # If user does not define the path, attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  # testing out different downloads_path as an argument is needed.
  if(!is.null(downloads_path)){
    downloads_path <- downloads_path
  }
  
  library(rATTAINS)
  
  if(!is.data.frame(.data)){
    if (!any(c(
      "TADA_with_ATTAINS", "ATTAINS_catchments", "ATTAINS_points", "ATTAINS_lines", "ATTAINS_polygons"
    ) %in% names(.data))) {
      stop("Your input dataframe was not produced from `TADA_GetATTAINS()` or it was modified. Please create your list of ATTAINS features using `TADA_GetATTAINS(return_sf = TRUE)`")
    }
    
    .data <- .data[["TADA_with_ATTAINS"]]
  }
  
  
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
    # dplyr::select(-geometry) %>%
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

TADA_DefineMagnitude <- function(.data, ref = "TADA", paramRef = NULL, paramUseRef = NULL, AUIDRef = NULL, excel = TRUE, overwrite = FALSE){ 
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if(is.null(paramRef)){
    paramUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamUseRef")
  }
  
  if(is.null(paramUseRef)){
    paramUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamUseRef")
  }
  
  if(is.null(AUIDRef)){
    AUIDRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAUIDRef")
  }
  
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
  
  if (!is.character(AUIDRef)) {
    if (!is.data.frame(AUIDRef)) {
      stop("TADA_DefineMagnitude: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
    }
    
    if (is.data.frame(AUIDRef)) {
      col.names <- c(
        "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "LongitudeMeasure", "LatitudeMeasure"
      )
      
      ref.names <- names(AUIDRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineMagnitude: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
      }
    }
  }
  
  if(sum(is.na(AUIDRef$SiteSpecificName)) > 1){
    print("NAs were found in column SiteSpecificName for your AUIDRef. Please ensure that you have inputted all field values of interest in the SiteSpecificName column that defines your organization's unique site-specific mapping criteria")
  }
  
  wb <- loadWorkbook(wb, downloads_path)
  tryCatch({
    addWorksheet(wb, "DefineMagnitude")
  },
  error = function(e){
    removeWorksheet(wb, "DefineMagnitude")
    addWorksheet(wb, "DefineMagnitude")
  }
  )
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[5]]$sheetViews
  wb$worksheets[[5]]$sheetViews <- set_zoom(90)
  
  columns <- c(
    "EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_name", "use_name", 
    "ATTAINS.WaterType", "AcuteChronic", "BegAssessDate", "EndAssessDate", 
    "Season", "MinSamplePerDuration", "SiteSpecificName", "ExcludeStationReason", 
    "EquationBased", "StandardValue", "StandardUnit"
  )
  
  DefineMagnitude <- paramRef %>%
    dplyr::full_join(paramUseRef, by = c("EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_name"), relationship = "many-to-many") %>%
    dplyr::select("EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_name", "use_name") %>%
    dplyr::bind_cols(
      data.frame(
        ATTAINS.WaterType = as.character(NA), AcuteChronic = as.character(NA), BegAssessDate = as.Date(NA),
        EndAssessDate = as.Date(NA), Season = as.character(NA), MinimumSample = as.numeric(NA), SiteSpecificName = as.character(NA), ExcludeStationReason = as.character(NA),
        EquationBased = as.character(NA), MagnitudeValue = as.character(NA), MagnitudeUnit = as.character(NA)
      )
    ) %>%
    dplyr::filter(ATTAINS.ParameterName != "Parameter not used for assessment") %>%
    distinct() %>%
    dplyr::arrange(organization_name != "EPA304a", organization_name)
  
  CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
    select(EPA304A.PollutantName = POLLUTANT_NAME, use_name = USE_CLASS_NAME_LOCATION_ETC, CRITERIATYPE_ACUTECHRONIC, CRITERION_VALUE, UNIT_NAME) %>%
    mutate(organization_name = "EPA304a")
  
  if("EPA304a" %in% DefineMagnitude$organization_name){
    DefineMagnitude <- DefineMagnitude %>%
      left_join(CST_param, c("EPA304A.PollutantName", "use_name", "organization_name"), relationship = "many-to-many") %>%
      mutate(AcuteChronic = CRITERIATYPE_ACUTECHRONIC) %>%
      mutate(MagnitudeValue = CRITERION_VALUE) %>%
      mutate(MagnitudeUnit = UNIT_NAME) %>%
      select(-c(CRITERIATYPE_ACUTECHRONIC, CRITERION_VALUE, UNIT_NAME)) %>%
      distinct() %>%
      dplyr::arrange(organization_name != "EPA304a", organization_name)
      
  }
  
  paramRef$ATTAINS.ParameterName <- as.character(paramRef$ATTAINS.ParameterName)
  # Pulls in all the units that are found in TADA.ResultMeasure.MeasureUnitCode as unique allowable unit column
  StandardValue <- paramRef %>%
    dplyr::left_join( 
      (select(.data, "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode") %>% 
         dplyr::distinct() %>%
         drop_na(TADA.ResultMeasure.MeasureUnitCode)
      ), 
      by = c("TADA.CharacteristicName", "TADA.MethodSpeciationName","TADA.ResultSampleFractionText"), relationship = "many-to-many") %>%
    dplyr::select(ATTAINS.ParameterName, TADA.ResultMeasure.MeasureUnitCode) %>%
    distinct() %>%
    dplyr::right_join(DefineMagnitude, by = c("ATTAINS.ParameterName"), relationship = "many-to-many") %>%
    dplyr::select(TADA.ResultMeasure.MeasureUnitCode) %>%
    dplyr::rename(StandardUnit = TADA.ResultMeasure.MeasureUnitCode)
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Format Column widths
  openxlsx::setColWidths(wb, "DefineMagnitude", cols = 1:ncol(DefineMagnitude), widths = "auto")
  
  # Write column names in the excel spreadsheet under the tab [DefineMagnitude]
  #writeData(wb, "DefineMagnitude", startCol = 1, x = par, headerStyle = header_st)
  # Export DefineMagnitude dataframe into the excel spreadsheet tab
  writeData(wb, "DefineMagnitude", startCol = 1, x = DefineMagnitude, headerStyle = header_st)
  #writeData(wb, "DefineMagnitude", startCol = 13, startRow = 1, x = StandardValue)
  
  writeData(wb, "Index", startCol = 9, startRow = 1, x = data.frame(ATTAINS.WaterType = c(unique(.data$MonitoringLocationTypeName), "All Others", "AddWaterTypeHere", "NA"))) # ATTAINS.WaterType
  writeData(wb, "Index", startCol = 10, startRow = 1, x = data.frame(AcuteChronic = c("Acute", "Chronic", "NA"))) # AcuteChronic
  writeData(wb, "Index", startCol = 11, startRow = 1, x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA"))) #Season
  
  writeData(wb, "Index", startCol = 12, startRow = 1, x = data.frame(SiteSpecificName = c(unique(AUIDRef$SiteSpecificName), "AddSiteNameHere", "NA")))# SiteSpecificName
  writeData(wb, "Index", startCol = 13, startRow = 1, x = data.frame(ExcludeStationReason = c(unique(AUIDRef$ExcludeStationReason), "NA"))) # ExcludeStationReason
  writeData(wb, "Index", startCol = 14, startRow = 1, x = data.frame(EquationBased = c("Yes", "No", "NA"))) # EquationBased
  
  writeData(wb, "Index", startCol = 15, startRow = 1, x = data.frame(StandardUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode))) # StandardUnit

  # The list of allowable values for each column in excel tab [DefineMagnitude] will be defined by the [Index] tab
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ATTAINS.WaterType
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # AcuteChronic
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # Season
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 11, rows = 2:1000, type = "list", value = sprintf("'Index'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # SiteSpecificName
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 13, rows = 2:1000, type = "list", value = sprintf("'Index'!$M$2:$M$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ExcludeSiteReason
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 14, rows = 2:1000, type = "list", value = sprintf("'Index'!$N$2:$N$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # EquationBased
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 15, rows = 2:1000, type = "list", value = sprintf("'Index'!$O$2:$O$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # StandardUnit
  suppressWarnings(dataValidation(wb, sheet = "DefineMagnitude", cols = 16, rows = 2:1000, type = "list", value = sprintf("'Index'!$P$2:$P$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # StandardLimit
  
  # Conditional Formatting
  conditionalFormatting(wb, "DefineMagnitude",
                        cols = 5:16, rows = 2:(nrow(DefineMagnitude) + 1),
                        type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8])) # default values or indicates good to go cells.
  conditionalFormatting(wb, "DefineMagnitude",
                        cols = 5:16, rows = 2:(nrow(DefineMagnitude) + 1),
                        type = "blanks", style = createStyle(bgFill = TADA_ColorPalette()[13])) # default values or indicates good to go cells.
  
  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
  if(overwrite == TRUE){
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  return(DefineMagnitude)
}
