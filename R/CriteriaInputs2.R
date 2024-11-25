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
  
  # If user does not define the path, attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # 304a standards are pulled in from the Criteria Search Tool (CST)
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
    dplyr::mutate(organization_name = toString(org_names))
  
  # TADA_param2 <- separate_rows(TADA_param, organization_name, sep =",")
  
  if(is.null(paramRef)){
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = NA) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName, ATTAINS.ParameterName, organization_name) %>%
      dplyr::arrange(TADA.CharacteristicName)
    #group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName)
    #filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T)
  }
  
  if(!is.null(paramRef)){
    CreateParamRef <- TADA_param %>%
      dplyr::left_join(paramRef, c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")) %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName, ATTAINS.ParameterName, organization_name) %>%
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
  
  # if(validate == FALSE){
  #   
  #   if(is.null(paramRef)){
  #     CreateParamRef <- CreateParamRef %>%
  #       dplyr::mutate(ATTAINS.ParameterName = NA, ATTAINS.FlagParameterName = "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment")
  #   }
  #   
  #   if(!is.null(paramRef)){
  #     CreateParamRef <- CreateParamRef %>% 
  #       #dplyr::left_join(ATTAINS_param, by = c("ATTAINS.ParameterName" = "parameter")) %>%
  #       dplyr::mutate(ATTAINS.FlagParameterName = case_when(
  #         organization_name == "EPA 304a" & CST.PollutantName != NA ~ "Pass: Will use the EPA 304a recommended standards for this parameter",
  #         organization_name == "EPA 304a" & CST.PollutantName != NA ~ "Suspect: No CST.PollutantName found. Please select an appropriate crosswalk if you would like to use the EPA 304a recommended standards for this parameter",
  #         is.na(organization_name) & is.na(ATTAINS.ParameterName) ~ "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
  #         is.na(organization_name) & !is.na(ATTAINS.ParameterName) ~ "Suspect: parameter not listed as a prior cause for this organization",
  #         !is.na(organization_name) & !is.na(ATTAINS.ParameterName) ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments")
  #       ) %>%
  #       dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, organization_name, CST.PollutantName,	ATTAINS.ParameterName,	ATTAINS.FlagParameterName)
  #   }
  #   
  # }
  
  # Re-runs the flagging data after a user has inputted values - will need to be done if a user inputs values in the R environment
  if( validate == TRUE){
    
    if(is.null(paramRef)){
      stop("User must provide a paramRef as an argument input in the function TADA_CreateParamRef() if validate = TRUE")
    }
    paramRef <- paramRef %>%
      dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, CST.PollutantName,	ATTAINS.ParameterName, organization_name)
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
    #addWorksheet(wb, "ATTAINSParamAllOrgs", visible = FALSE)
    #addWorksheet(wb, "ATTAINSParamRef", visible = FALSE)
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
    writeData(wb, "Index", startCol = 3, x = data.frame(organization_name = c(unique(ATTAINS_param$organization_name)))) # Should we allow all orgs?
    writeData(wb, "Index", startCol = 2, x = unique(CST_param$CST.PollutantName)) # Should we allow all orgs?
    writeData(wb, "Index", startCol = 1, x = data.frame(ATTAINS.ParameterName = c(unique(ATTAINS_param$parameter),"Parameter not used for assessment"))) 
    # Index of Unique combinations of WQP TADA Parameters found in TADA dataframe
    # writeData(wb, "Index", startCol = 1, x = CreateParamRef)
    
    # Creates an ATTAINS Param ref tab for state and all orgs on separate sheets.
    # writeData(wb, "ATTAINSParamAllOrgs", startCol = 1, 
    #           x = 
    #             ATTAINS_param_all %>% 
    #               dplyr::select(parameter, organization_name) %>%
    #               dplyr::distinct()
    #           , headerStyle = header_st)
    # writeData(wb, "ATTAINSParamRef", startCol = 1, 
    #           x = ATTAINS_param %>% 
    #             dplyr::select(parameter, organization_name) %>% 
    #             dplyr::distinct()
    #           , headerStyle = header_st)
    # addStyle(wb, sheet = "ATTAINSParamRef", bodyStyle, cols = 2, rows = 1:200)
    # addStyle(wb, sheet = "ATTAINSParamRef", bodyStyle, cols = 3, rows = 1:200)
    # 
    # writeData(wb, "CreateParamRef", startCol = 1, x = par, headerStyle = header_st) # Write column names in the excel spreadsheet under the tab [CreateParamRef]
    # 
    # # Format Column widths
    # openxlsx::setColWidths(wb, "ATTAINSParamAllOrgs", cols = 1:ncol(par), widths = "auto")
    # openxlsx::setColWidths(wb, "ATTAINSParamRef", cols = 1:ncol(par), widths = 40)
    openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(par), widths = "auto")
    
    writeData(wb, "CreateParamRef", startCol = 1, x = CreateParamRef)
    
    # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab
    
    #suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 1, rows = 2:5000, type = "list", value = sprintf("'Index'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'ATTAINSParamRef'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    # conditionalFormatting(wb, "ATTAINSParamRef", 
    #                       cols = 1:2, rows = 2+nrow(ATTAINS_param %>% 
    #                                                   dplyr::select(parameter, organization_name) %>% 
    #                                                   dplyr::distinct()):1000, 
    #                       type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8]))
    # 
    # n <- nrow(ATTAINS_param %>% 
    #             dplyr::select(parameter, organization_name) %>% 
    #             dplyr::distinct()
    # )
    # for(i in 1:20){
    #   writeFormula(wb, "ATTAINSParamRef", startCol = 3, 
    #                startRow = i+1+n, array = TRUE, 
    #                x = paste0('=IF(ISNA(MATCH(1,(A', i+1+n,'=Index!D:D)*(B',i+1+n,'=Index!F:F),0)),"Warning: This parameter is not listed as a prior cause for your orgnaization. User will need to manually add additional rows to input any designated use for this parameter done in TADA_CreateParamUseRef","")')
    #   )
    #   
    #   writeFormula(wb, "ATTAINSParamRef", startCol = 2, 
    #                startRow = i+1+n, array = TRUE,
    #                x = paste0('=IF(ISBLANK(A', i+1+n,'),"",TEXTJOIN(",",,IF(A',i+1+n,'=ATTAINSParamAllOrgs!A:A,ATTAINSParamAllOrgs!B:B,"")))')
    #   )  
    #   setRowHeights(wb, "ATTAINSParamRef", rows = 1+i+n, height = 100)
    # }
    
    # for(i in 1:nrow(TADA_param)){
    # writeFormula(wb, "CreateParamRef", startCol = 5, 
    #            startRow = i+1, array = TRUE,
    #            x = paste0('=IFERROR(INDEX(Index!D:F,MATCH(CreateParamRef!D', i+1,',Index!D:D,0),3),"")')
    #           ) 
    # writeFormula(wb, "CreateParamRef", startCol = 7, 
    #              startRow = i+1, array = TRUE,
    #              x = paste0('=IF(F',i+1,'="EPA 304a",IF(D',i+1,'="",
    #                         "Suspect: No CST.PollutantName found. Please select an appropriate crosswalk if you would like to use the EPA 304a recommended standards for this parameter",
    #                         "Pass: Will use the EPA 304a recommended standards for this parameter"),IF(AND(ISNA(MATCH(1,(D',i+1,'=Index!E:E)*(F',i+1,'=Index!E:E),0)),E',i+1, '<>""),
    #                         "Suspect: parameter not listed as a prior cause for your organization",IF(E', i+1,'="",
    #                         "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
    #                         "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments")))'
    #                         )
    # )
    #}
    
    for(i in 1:nrow(TADA_param)){
      conditionalFormatting(wb, "CreateParamRef", 
                            cols = 5, rows = 1:i+1, 
                            type = "blanks", style = createStyle(bgFill = "#FFC7CE"))
      
      conditionalFormatting(wb, "CreateParamRef", 
                            cols = 5, rows = 1:i+1, 
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
  
  return(CreateParamRef)
}  