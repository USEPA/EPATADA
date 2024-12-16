#' ATTAINS Parameter Name and TADA/WQP Parameter Name Crosswalk
#' 
#' Users will be required to complete and provide a crosswalk of the appropriate mapping
#' between ATTAINS parameter names for their organization with each TADA/WQP characteristic names/ 
#' TADA.ComparableDataIdentifier, found in the user's TADA data frame. This crosswalk allows 
#' users to select which 'ATTAINS.ParameterName' from the drop-down excel spreadsheet aligns
#' with the corresponding unique list of TADA/WQP Characteristic Names/TADA.ComparableDataIdentifier. 
#' 
#' If an ATTAINS parameter name is not listed as a prior domain value for your org from prior 
#' ATTAINS assessment cycles, users should consider contacting the ATTAINS team to add this to the domain list. 
#' Otherwise, users can still proceed by overriding the data validation by value pasting. 
#' Users will be warned in the ATTAINS.FlagParameterName column if they choose to include an
#' ATTAINS.ParameterName that was not named in prior ATTAINS assessment cycles as: 
#' 'Suspect: parameter name is not found as a prior parameter name for this organization'
#' 
#' Users who already have a completed dataframe of this parameter crosswalk, that contains the 
#' required column names from this function, can provide this as a reference for an argument
#' input, which would allow users to join this data to the blank output table template 
#' from this function. From there, users will need to determine if any new TADA parameters 
#' need to be matched with any ATTAIN parameters based on any missing cell values. 
#' 
#' Users will need to ensure that this crosswalk table they provide contains the appropriate column 
#' names that are needed in order to run the function. Users will have two options:
#' 
#' 1) Supply a paramRef data frame which contains at least these four column names: 
#' TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName
#' 2) Supply a paramRef data frame which contains at least these two column names: 
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName 
#' 
#' Users who are interested in doing an assessment for more than one org_names will need to also include in 
#' the paramRef data frame which contains an additional column name: 'organization_name'
#' in order to determine the proper crosswalk between TADA.ComparableDataIdentifier and ATTAINS.ParameterName
#' by organization name.
#' 
#' A crosswalk between TADA.CharacteristicName and EPA 304A pollutant names (sourced from the Criteria Search Tool)
#' has been done on the EPATADA internal team. This crosswalk is still in development and only focuses on the
#' priority TADA.CharacteristicName.
#'
#' @param .data A TADA dataframe. Users are expected to have already run the appropriate data 
#' cleaning, processing, harmonization and filtering functions prior to this step as well as
#' provide the geospatial components with Module 2 TADA_GetATTAINS() function.
#' 
#' @param org_names The ATTAINS organization name must be supplied by the user. A list of
#' organization names can be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. Organization names
#' are listed in the "OrgName" tab. The "code" column contains the organization names that
#' should be used for this param. If a user does not provide an org_names argument, the function 
#' attempts to identify which organization name(s) are found in the dataframe to use as a reference.
#' 
#' @param excel A boolean value that returns an excel spreadsheet if excel = TRUE. This spreadsheet is 
#' created in the user's downloads folder path. 
#' In the R console please type in: file.path(Sys.getenv("USERPROFILE"), "Downloads") in File explorer 
#' if there is trouble locating the file. The file will be named "myfileRef.xlsx". 
#' The excel spreadsheet will highlight the cells in which users should input information. Users may need to
#' insert additional rows to capture certain ATTAINS.ParameterName that correspond with multiple TADA.CharacteristicName.
#' Example: If your organization defines pH as "pH, High" and "pH, Low" that correspond to the TADA.CharacteristicName 'PH'.
#' 
#' @param overwrite A Boolean value that allows users to not overwrite the excel file
#' that is created. This will help prevent a user from overwriting their progress when 
#' completing the excel spreadsheet.
#' 
#' @param paramRef A data frame which contains a completed crosswalk between TADA_ComparableDataIdentifier
#' and ATTAINS.ParameterName. Users will need to ensure this crosswalk contains the appropriate column 
#' names in order to run the function. Users will have two options:
#' 1) Supply a paramRef data frame which contains at least these four column names: 
#' TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName
#' 2) Supply a paramRef data frame which contains at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName 
# 
#' Users who are interested in doing an assessment for more than one org_names will need to also include in 
#' the paramRef data frame which contains an additional column name: 'organization_name'
#' in order to determine the proper crosswalk between TADA.ComparableDataIdentifier and ATTAINS.ParameterName
#' by organization name.
#' 
#' @return A data frame which contains the columns: TADA.ComparableDataIdentifier, organization_name, 
#' EPA304A.PollutantName, ATTAINS.ParameterName, and ATTAINS.FlagParameterName. Users will need to complete
#' the crosswalk between ATTAINS.ParameterName and TADA.ComparableDataIdentifier.
#' 
#' @export
#'
#' @examples
#' # This creates a blank paramRef template of UT Nutrients data. Users will need to fill this template out.
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_names = "Utah", excel = FALSE)
#' 
#' # User can choose to edit the paramRef_UT through the R environment or in the excel spreadsheet. User
#' # should be aware that any updates done only in the R environment will not reflect the 'ATTAINS.FlagParameterName 
#' # values correctly. They are recommended to rerun the function with this completed crosswalk to do so if it 
#' # is done in the R environment only and not in excel.
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'     TADA.CharacteristicName == "AMMONIA" ~ "AMMONIA, TOTAL", 
#'     TADA.CharacteristicName == "NITRATE" ~ "NITRATE", 
#'     TADA.CharacteristicName == "NITROGEN" ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)" )
#'     )
#' paramRef_UT3 <- TADA_CreateParamRef(Data_Nutrients_UT, paramRef = paramRef_UT2, org_names = "Utah", excel = FALSE)
#' 
#' # Multiple org_names selection is allowed. Users should only provide org_names that pertains to their 
#' # assessments for cross walking ATTAINS.ParameterNames to WQP/TADA CharacteristicName.
#' Data_NCTCShepherdstown <- TADA_RunKeyFlagFunctions(Data_NCTCShepherdstown_HUC12)
#' Data_NCTCShepherdstown2 <- TADA_HarmonizeSynonyms(Data_NCTCShepherdstown)
#' paramRef_NCTC <- TADA_CreateParamRef(Data_NCTCShepherdstown2, org_names = c("Maryland", "Virginia", "Pennsylvania"), excel = FALSE)
#' 
TADA_CreateParamRef <- function(.data, org_names = NULL, paramRef = NULL, excel = TRUE, overwrite = FALSE) {
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop("TADA_CreateParamRef: 'paramRef' must be a data frame with either these four columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName
            or with these 2 columns: TADA.ComparableDataIdentifier and ATTAINS.ParameterName")
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "ATTAINS.ParameterName"
      )
      
      ref.names <- names(paramRef)
      
      # Users are required to provide a parameter ref that contains either TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName or TADA.ComparableDataIdentifier and ATTAINS.ParameterName
      if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
        stop("TADA_CreateParamRef: 'paramRef' must be a data frame with either these four columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName
              or these 2 columns: TADA.ComparableDataIdentifier and ATTAINS.ParameterName")
      }
    }
  }
  
  .data <- as.data.frame(.data)
  
  # If users don't provide TADA.ComparableDataIdentifier in their paramRef input, we will crosswalk this using TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText
  if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
    paramRef <- paramRef %>%
      dplyr::left_join(.data, c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")) %>%
      dplyr::select("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "organization_name", "EPA304A.PollutantName", "ATTAINS.ParameterName", "ATTAINS.FlagParameterName")
  }
  
  # if a user provides an org_names argument, it must be a character vector.
  if (!is.character(org_names) & !is.null(org_names)) {
    stop("org_names, if defined as an argument input, must be a character vector or left as NULL")
  }
  
  # if user doesn't provide an org_names argument, the function extracts the unique org_names from TADA_GetATTAINS().
  # Users will need to have ran TADA_GetATTAINS() for this option to be allowed. Selection of org_names will filter the drop down lists in future steps of creating the reference tables.
  if (is.null(org_names)) {
    print("No organization name(s) provided. Attempting to pull in organization names found in the TADA data frame.
          Please ensure that you have ran TADA_GetATTAINS if you did not provide an org_names argument input.")
    print("Users should provide a list of ATTAINS organization state or tribal name that pertains to their assessment.")
    TADA_CheckColumns(.data, "ATTAINS.organizationname")
    org_names <- unique(stats::na.omit(.data[, "ATTAINS.organizationname"]))
  }
  
  org_names <- as.list(org_names)
  
  # If a user provides more than 1 org, it will create n duplicate rows for each TADA.ComparableDataIdentifier.
  if (length(org_names) > 1) {
    print("More than one org_name was defined in your dataframe. Generating duplicate rows for TADA.CharacteristicName, TADA.ComparableDataIdentifier for each orgs that will need a crosswalk")
  }
  
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop("argument input excel = FALSE and overwrite = TRUE is an invalid combination. Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE")
  }
  
  # 304a parameter name and standards are pulled in from the Criteria Search Tool (CST)
  CST_param <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))
  
  # Pulls in all unique combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText in user's dataframe.
  TADA_param <- dplyr::distinct(.data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]) %>%
    dplyr::left_join(CST_param, "TADA.CharacteristicName") %>%
    dplyr::select(TADA.CharacteristicName, TADA.ComparableDataIdentifier, EPA304A.PollutantName = CST.PollutantName) %>%
    dplyr::arrange(TADA.CharacteristicName, TADA.ComparableDataIdentifier) %>%
    tidyr::uncount(weights = length(org_names)) %>%
    dplyr::mutate(organization_name = rep(org_names, nrow(.) / length(org_names)))
  # group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, EPA304A.PollutantName) %>%
  # dplyr::mutate(organization_name = toString(org_names)) %>%
  # separate_rows(organization_name, sep =", ")
  
  TADA_param$organization_name <- as.character(TADA_param$organization_name)
  
  # Pulls in all domain values of parameter names in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::filter(organization_name %in% org_names) %>%
    dplyr::arrange(parameter)
  
  if (sum(!org_names %in% ATTAINS_param_all$organization_name) > 0) {
    warning(paste0(
      "TADA_CreateParamRef: ",
      "One or more organization names entered by user is not found in ATTAINS."
    ))
  }
  
  # Print message if there are many combinations of TADA Characteristic as it may slow run time.
  n <- nrow(dplyr::distinct(.data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]))
  if (n > 100) {
    message(paste0("There are ", n, " unique TADA.ComparableDataIdentifier names in your TADA data frame.
    This may result in slow runtime for TADA_CreateParamRef() if you are generating an excel spreadsheet.
    Consider filtering your .data TADA dataframe to a smaller subset of TADA.ComparableDataIdentifier first."))
  }
  
  # If no paramRef is provided, the ATTAINS.ParameterName returns a blank column of NA that will need user input.
  if (is.null(paramRef)) {
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = NA) %>%
      dplyr::select(TADA.CharacteristicName, TADA.ComparableDataIdentifier, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName) %>%
      dplyr::arrange(organization_name, TADA.CharacteristicName)
    # group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, EPA304A.PollutantName)
    # filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T)
  }
  
  # If a user does provide a paramRef, this joins the dataframe to match the final output. rename is done as we may need to consider if we want to consider allowing users
  # to crosswalk CST parameter names or modify EPA 304A pollutant names on their ends.
  if (!is.null(paramRef)) {
    paramRef$organization_name <- as.character(paramRef$organization_name)
    
    CreateParamRef <- TADA_param %>%
      dplyr::left_join(paramRef, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "organization_name"), relationship = "many-to-many") %>%
      dplyr::rename(dplyr::any_of(c(EPA304A.PollutantName = "EPA304A.PollutantName.x", organization_name = "organization_name.x", ATTAINS.ParameterName = "ATTAINS.ParameterName.y"))) %>%
      dplyr::select(TADA.CharacteristicName, TADA.ComparableDataIdentifier, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName) %>%
      dplyr::filter(organization_name %in% org_names) %>%
      dplyr::arrange(organization_name, TADA.CharacteristicName) %>%
      # group_by(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, EPA304A.PollutantName) %>%
      # filter(if(sum(!is.na(organization_name))) !is.na(organization_name) else T) %>%
      dplyr::distinct()
  }
  
  # Re-runs the flagging data after a user has inputted values - will need to be done if a user only inputs values in the R environment and not in excel.
  if (is.null(paramRef)) {
    CreateParamRef <- CreateParamRef %>%
      dplyr::mutate(ATTAINS.ParameterName = NA, ATTAINS.FlagParameterName = "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment")
  }
  
  if (!is.null(paramRef)) {
    Flag1 <- paramRef %>%
      dplyr::anti_join(ATTAINS_param_all, by = c("ATTAINS.ParameterName" = "parameter", "organization_name")) %>%
      dplyr::select(ATTAINS.ParameterName, organization_name) %>%
      dplyr::distinct() %>%
      dplyr::mutate(ATTAINS.FlagParameterName1 = dplyr::case_when(
        ATTAINS.ParameterName == "Parameter not used for assessment" | is.na(ATTAINS.ParameterName) ~ "Suspect: No parameter crosswalk provided. Parameter will not be used for assessment",
        !is.na(ATTAINS.ParameterName) ~ "Suspect: parameter name is not found as a prior use for this organization"
      ))
    
    CreateParamRef <- CreateParamRef %>%
      dplyr::left_join(Flag1, c("ATTAINS.ParameterName", "organization_name")) %>%
      dplyr::mutate(ATTAINS.FlagParameterName = dplyr::case_when(
        !is.na(ATTAINS.FlagParameterName1) ~ ATTAINS.FlagParameterName1,
        is.na(ATTAINS.FlagParameterName1) ~ "Pass: parameter is listed as prior cause in ATTAINS and will be used for assessments"
      )) %>%
      dplyr::select(TADA.CharacteristicName, TADA.ComparableDataIdentifier, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName, ATTAINS.FlagParameterName)
    
    # remove intermediate object Flag1
    rm(Flag1)
  }
  
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if (excel == TRUE) {
    # Create column names for an empty dataframe
    columns <- c(
      "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName", "organization_name", "ATTAINS.FlagParameterName"
    )
    
    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
    colnames(par) <- columns
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "CreateParamRef", visible = TRUE)
    openxlsx::addWorksheet(wb, "Index", visible = FALSE)
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    sV <- wb$worksheets[[1]]$sheetViews
    wb$worksheets[[1]]$sheetViews <- set_zoom(90)
    # Format header and bodystyle
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    bodyStyle <- openxlsx::createStyle(wrapText = TRUE)
    
    # Index of ATTAINS allowable values of Entity/Parameter
    openxlsx::writeData(wb, "Index", startCol = 4, x = rbind(ATTAINS_param, c(rep("NA", 5))))
    openxlsx::writeData(wb, "Index", startCol = 3, x = data.frame(organization_name = c(unique(ATTAINS_param$organization_name)))) # Should we allow all orgs?
    openxlsx::writeData(wb, "Index", startCol = 2, x = unique(CST_param$CST.PollutantName)) # Should we allow all orgs?
    openxlsx::writeData(wb, "Index", startCol = 1, x = data.frame(ATTAINS.ParameterName = c(unique(ATTAINS_param$parameter), "Parameter does not apply for assessment")))
    openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(par), widths = "auto")
    
    openxlsx::writeData(wb, "CreateParamRef", startCol = 1, x = CreateParamRef, headerStyle = header_st)
    
    # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab
    
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 1, rows = 2:5000, type = "list", value = sprintf("'Index'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "CreateParamRef", cols = 3, rows = 2:1000, type = "list", value = sprintf("'Index'!$C$2:$C$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "CreateParamRef", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "CreateParamRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$A$2:$A$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    # remove intermediate object ATTAINS_param
    rm(ATTAINS_param)
    
    for (i in 1:nrow(TADA_param)) {
      openxlsx::writeFormula(wb, "CreateParamRef",
                             startCol = 6, startRow = i + 1, array = TRUE,
                             x = paste0("=IF(OR(E", i + 1, '="",E', i + 1, '="Parameter not used for assessment"),
                   "Suspect: no parameter name provided. parameter will not be used for assessment",IF(ISNA(MATCH(1,(E', i + 1, "=Index!H:H)*(C", i + 1, '=Index!E:E),0)),
                   "Suspect: parameter name is not found as a prior use for this organization",
                   "Pass: parameter name is listed as prior cause in ATTAINS for this organization"))')
      )
    }
    
    for (i in 1:nrow(TADA_param)) {
      openxlsx::conditionalFormatting(wb, "CreateParamRef",
                                      cols = 5, rows = 1:i + 1,
                                      type = "blanks", style = openxlsx::createStyle(bgFill = "#FFC7CE")
      )
      
      openxlsx::conditionalFormatting(wb, "CreateParamRef",
                                      cols = 5, rows = 1:i + 1,
                                      type = "notBlanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      )
    }
    
    # remove intermediate object TADA_param
    rm(TADA_param)
    
    if (overwrite == TRUE) {
      message(paste0("Overwriting sheet [CreateParamRef] in ", downloads_path))
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      message("If you would like to replace sheet [CreateParamRef], use overwrite = TRUE argument in TADA_CreateParamRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  
  return(CreateParamRef)
}

#' Parameter and Use Name crosswalk
#'
#' Users will be required to validate the use name crosswalk that applies to their org(s)
#' for each ATTAIN parameter name and associated use_name to the TADA/WQP characteristic names/
#' TADA.ComparableDataIdentifier. This crosswalk will need to be done
#' by determining if the matched 'use_name' from the drop-down excel spreadsheet aligns
#' with the corresponding unique list of TADA/WQP Characteristic Names/TADA.ComparableDataIentifier.
#'
#' Users are required to have already ran TADA_CreateParamRef() which will contain the completed
#' crosswalk that defines which ATTAINS.ParameterName and use_name needs validation.
#' All unique use_names from prior ATTAINS assessment cycle is pulled in using
#' TADA_CreateParamUseRef(). If a user has defined multiple TADA.ComparableDataIdentifier matches
#' to an ATTAINS.ParameterName, they will need to define whether every TADA.ComparableDataIdentifier
#' matches to an associated use_name. If certain parameter and use combinations only apply to
#' certain TADA.ComparableDataIdentifier, users will need to select 'NA' or leave it as blank to
#' properly capture this logic before summarizing Criteria and Methodology results.
#'
#' If an ATTAINS use name is not listed as a prior domain value for your org from prior
#' ATTAINS assessment cycles, users should consider contacting the ATTAINS team to add this to the domain list.
#' Otherwise, users can still proceed by overriding the data validation by value pasting.
#' Users will be warned in the ATTAINS.FlagUseName column if they choose to include an
#' ATTAINS use name that was not named in prior ATTAINS assessment cycles as:
#' 'Suspect: use name is not found as a prior use name for this organization' or
#' 'Suspect: use name is not found as a prior use name for this parameter'
#'
#' Users will have the flexibility to include the EPA304a standards by including this string in the org_names
#' function argument. Users who only want the EPA304a standards would input as an argument input:
#'
#' org_names = "EPA304a"
#'
#' Users who want both their orgs and EPA304a standards would input a character vector that contains both:
#'
#' org_names = c("EPA304a", "Utah")
#'
#' The use_name for EPA304a standards are matched from the CriteriaSearchTool:
#' (CST) https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa
#' while the use_name for any other ATTAINS organization names come from ATTAINS domain value for use_name.
#'
#'
#' @param .data A TADA dataframe. Users are expected to have already run the appropriate data
#' cleaning, processing, harmonization and filtering functions prior to this step as well as
#' provide the geospatial components with Module 2 TADA_GetATTAINS() function.
#'
#' @param org_names The ATTAINS organization name must be supplied by the user. A list of
#' organization names can be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. Organization names
#' are listed in the "OrgName" tab. The "code" column contains the organization names that
#' should be used for this param. If a user does not provide an org_names argument, the function
#' attempts to identify which organization name(s) are found in the dataframe to use as a reference.
#'
#' NOTE: USERS CAN ADD ON 'EPA304a' AS AN ORG_NAMES INPUT IF THEY ARE INTERESTED IN THE EPA304A STANDARDS
#'
#' This org_name argument input allows for the drop-down list to reflect prior ATTAINS parameter names
#' and ATTAINS use names that were used in prior ATTAINS CWA assessment cycles by your org_name.
#' Users are expected to be the subject matter experts to perform the parameter crosswalk for the org_names
#' that they have provided in this function.
#'
#' @param excel A boolean value that returns an excel spreadsheet if excel = TRUE. This spreadsheet is
#' created in the user's downloads folder path.
#' In the R console please type in: file.path(Sys.getenv("USERPROFILE"), "Downloads") in File explorer
#' if there is trouble locating the file. The file will be named "myfileRef.xlsx".
#'
#' The excel spreadsheet will highlight the cells in which users should input information. Users may need to
#' insert additional rows to capture certain ATTAINS.ParameterName that correspond with multiple TADA.CharacteristicName.
#' Example: If your organization defines pH as "pH, High" and "pH, Low" that correspond to the TADA.CharacteristicName 'PH'.
#'
#' @param overwrite A Boolean value that allows users to not overwrite the excel file
#' that is created. This will help prevent a user from overwriting their progress when
#' completing the excel spreadsheet.
#'
#' @param paramRef A data frame which contains a completed crosswalk between TADA_ComparableDataIdentifier
#' and ATTAINS.ParameterName. Users will need to ensure this crosswalk contains the appropriate column
#' names in order to run the function. Users will have two options:
#'
#' 1) Supply a paramRef data frame which contains at least these four column names:
#' TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName
#'
#' 2) Supply a paramRef data frame which contains at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName
#'
#' Users who are interested in doing an assessment for more than one org_names will need to also include in
#' the paramRef data frame which contains an additional column name: 'organization_name'
#' in order to determine the proper crosswalk between TADA.ComparableDataIdentifier and ATTAINS.ParameterName
#' by organization name.
#'
#' @return A data frame which contains the columns: TADA.ComparableDataIdentifier, organization_name,
#' EPA304A.PollutantName, ATTAINS.ParameterName, and ATTAINS.FlagUseName. Users will need to review
#' the crosswalk between ATTAINS.ParameterName, use_name and TADA.ComparableDataIdentifier.
#'
#' @export
#'
#' @examples
#' # Users are required to provide a paramRef argument first. See TADA_CreateParamRef() for additional comments.
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_names = "Utah", excel = FALSE)
#'
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   TADA.CharacteristicName == "AMMONIA" ~ "AMMONIA, TOTAL",
#'   TADA.CharacteristicName == "NITRATE" ~ "NITRATE",
#'   TADA.CharacteristicName == "NITROGEN" ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(Data_Nutrients_UT, paramRef = paramRef_UT2, org_names = "Utah", excel = FALSE)
#'
#' # After running TADA_CreateParamRef() users will provide this as a function input. First example is only for the org Utah
#' paramUseRef_UT <- TADA_CreateParamUseRef(Data_Nutrients_UT, paramRef = paramRef_UT3, org_names = c("Utah"), excel = FALSE)
#'
#' # Users can include the EPA304a standards by itself or compared to their org(s)
#' paramUseRef_UT2 <- TADA_CreateParamUseRef(Data_Nutrients_UT, paramRef = paramRef_UT3, org_names = c("EPA304a", "Utah"), excel = FALSE)
#' paramUseRef_UT3 <- TADA_CreateParamUseRef(Data_Nutrients_UT, paramRef = paramRef_UT3, org_names = c("EPA304a"), excel = FALSE)
#' 
TADA_CreateParamUseRef <- function(.data, org_names = NULL, paramRef = NULL, excel = FALSE, overwrite = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop("argument input excel = FALSE and overwrite = TRUE is an invalid combination. Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE")
  }
  
  # Checks if ref contains a dataframe and necessary columns to proceed.
  if (is.null(paramRef)) {
    paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  }
  
  if (is.null(org_names)) {
    print("No organization name provided, users should provide a list of ATTAINS domain organization state or tribal name that pertains to their dataframe. Attempting to pull in organization names found in the TADA data frame.")
  }
  
  if (sum(!is.na(paramRef$ATTAINS.ParameterName)) == 0) {
    stop("No values were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function")
  }
  
  if (sum(is.na(paramRef$ATTAINS.ParameterName)) > 1) {
    print("NAs were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function")
  }
  
  if (sum(is.na(paramRef$EPA304A.PollutantName)) > 1 && org_names == "EPA304a") {
    print("NAs were found in EPA304A.PollutantName. Please ensure that you have inputted all field values of interest in the EPA304A.PollutantName column generated from TADA_CreateParamRef() function if you are interested in using the 304a recommended standards")
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop("TADA_CreateParamUseRef: 'paramRef' must be a data frame with either these four columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName or these 2 columns: TADA.ComparableDataIdentifier and ATTAINS.ParameterName")
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "ATTAINS.ParameterName"
      )
      
      ref.names <- names(paramRef)
      
      if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
        stop("TADA_CreateParamUseRef: 'paramRef' must be a data frame with either these four columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.ParameterName or these 2 columns: TADA.ComparableDataIdentifier and ATTAINS.ParameterName")
      }
    }
  }
  
  .data <- as.data.frame(.data)
  
  # Pulls in all domain values of parameter names in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
    paramRef <- paramRef %>%
      dplyr::left_join(.data, c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")) %>%
      dplyr::select("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "organization_name", "EPA304A.PollutantName", "ATTAINS.ParameterName", "ATTAINS.FlagParameterName")
  }
  
  # If user does not provide org names, then it will attempt to pull in org name from what is found in the data frame
  if (is.null(org_names)) {
    org_names <- unique(stats::na.omit(.data[, "ATTAINS.organizationname"]))
  }
  
  org_names <- as.list(org_names)
  
  # Checks if org_names are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_names
  if (sum(!org_names[org_names != "EPA304a"] %in% ATTAINS_param_all$organization_name) > 0) {
    warning(paste0(
      "TADA_CreateParamRef: ",
      "One or more organization names entered by user is not found in ATTAINS."
    ))
  }
  
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::select(organization_name, parameter, use_name) %>%
    dplyr::filter(parameter %in% paramRef$ATTAINS.ParameterName) %>%
    dplyr::filter(organization_name %in% org_names)
  
  # Create the parameter-use reference table for validation
  CreateParamUseRef <- paramRef %>%
    dplyr::ungroup() %>%
    # separate_rows(organization_name, sep = ", ") %>%
    dplyr::left_join(ATTAINS_param, by = c("ATTAINS.ParameterName" = "parameter", "organization_name"), relationship = "many-to-many") %>%
    dplyr::select(TADA.ComparableDataIdentifier, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName, use_name) %>%
    tidyr::drop_na(ATTAINS.ParameterName) %>%
    dplyr::filter(ATTAINS.ParameterName != "Parameter not used for assessment") %>%
    dplyr::distinct()
  
  # If users want the EPA304a standards. This pulls in the CST reference file. Extracts the associated EPA304a pollutant names and its use_names.
  if ("EPA304a" %in% org_names) {
    CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
      dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, use_name = USE_CLASS_NAME_LOCATION_ETC) %>%
      dplyr::mutate(organization_name = "EPA304a")
    
    EPA_param <- CreateParamUseRef %>%
      dplyr::left_join(CST_param, c("EPA304A.PollutantName"), relationship = "many-to-many") %>%
      dplyr::select(TADA.ComparableDataIdentifier, organization_name = organization_name.y, ATTAINS.ParameterName, EPA304A.PollutantName, use_name = use_name.y) %>%
      dplyr::distinct()
    
    rm(CST_param)
    
    CreateParamUseRef <- CreateParamUseRef %>%
      dplyr::ungroup() %>%
      dplyr::full_join(EPA_param, c("TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "organization_name", "EPA304A.PollutantName", "use_name")) %>%
      dplyr::select(TADA.ComparableDataIdentifier, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName, use_name) %>%
      dplyr::filter(organization_name %in% org_names)
    
    rm(EPA_param)
  }
  
  # This updates the flagging column. Users who only creates an R dataframe in the R environment will need to ensure they re-run the function with their completed paramRef as an input to reflect this column accurately.
  Flag1 <- CreateParamUseRef %>%
    dplyr::anti_join(ATTAINS_param_all, by = c("use_name", "organization_name")) %>%
    dplyr::select(use_name, organization_name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ATTAINS.FlagUseName1 = "Suspect: use name is listed as a prior use name in this organization but not for this parameter name")
  
  Flag2 <- CreateParamUseRef %>%
    dplyr::anti_join(ATTAINS_param_all, by = c("ATTAINS.ParameterName" = "parameter", "use_name", "organization_name")) %>%
    dplyr::select(use_name, ATTAINS.ParameterName, organization_name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ATTAINS.FlagUseName2 = "Suspect: use name is not listed as a prior use for this organization")
  
  CreateParamUseRef <- CreateParamUseRef %>%
    dplyr::left_join(Flag2, c("ATTAINS.ParameterName", "use_name", "organization_name")) %>%
    dplyr::left_join(Flag1, c("use_name", "organization_name")) %>%
    dplyr::mutate(ATTAINS.FlagUseName = dplyr::case_when(
      organization_name == "EPA304a" ~ "Pass: Will use the EPA304a recommended standards for this parameter",
      organization_name != "EPA304a" ~ "Pass: parameter name and use name are listed as prior cause in ATTAINS for this org and will be used for assessments",
      !is.na(ATTAINS.FlagUseName2) ~ ATTAINS.FlagUseName2,
      !is.na(ATTAINS.FlagUseName1) ~ ATTAINS.FlagUseName1,
    )) %>%
    dplyr::select(TADA.ComparableDataIdentifier, organization_name, EPA304A.PollutantName, ATTAINS.ParameterName, use_name, ATTAINS.FlagUseName)
  
  # remove intermediate objects
  rm(ATTAINS_param, Flag1, Flag2)
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if (excel == TRUE) {
    # Create column names for an empty dataframe
    columns <- c(
      "organization_name", "ATTAINS.ParameterName", "use_name", "ATTAINS.FlagParameterName", "ATTAINS.FlagUseName"
    )
    
    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
    colnames(par) <- columns
    
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    
    # If a user chooses to rerun the TADA_CreateParamUseRef() function, the sheet will already exist and error.
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateParamUseRef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateParamUseRef")
        openxlsx::addWorksheet(wb, "CreateParamUseRef")
      }
    )
    
    # If a user performs TADA_CreateParamRef() with excel = FALSE, but decides to perform TADA_CreateParamUseRef() with excel = TRUE,
    # and if the file already exists, we need to consider how to display the excel tab for [CreateParamRef] to ensure it does not display
    # non-matching dataframe crosswalk to avoid confusion.
    # excel_paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")[,c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "organization_name")]
    #
    # if(nrow(dplyr::inner_join(
    #   paramRef[,c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "organization_name")],
    #   excel_paramRef[,c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "organization_name")]))
    #   !=nrow(excel_paramRef)){
    #     if(nrow(excel_paramRef)!=nrow(paramRef)){
    #       warning("Your user-supplied paramRef table does not match the parameter reference table in excel sheet [CreateParamRef]. This may have occured if you have previously ran TADA_CreateParamRef() with excel = TRUE on a previous dataframe and proceeded with running TADA_CreateParamRef() with excel = FALSE and TADACreateParamUseRef() with excel = TRUE")
    #     }
    # }
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    sV <- wb$worksheets[[2]]$sheetViews
    wb$worksheets[[2]]$sheetViews <- set_zoom(90)
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(wb, "CreateParamUseRef", cols = 1:ncol(CreateParamUseRef), widths = "auto")
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    sV <- wb$worksheets[[3]]$sheetViews
    wb$worksheets[[3]]$sheetViews <- set_zoom(90)
    
    # Export CreateParamUseRef dataframe into the excel spreadsheet tab
    openxlsx::writeData(wb, "CreateParamUseRef", startCol = 1, x = CreateParamUseRef, headerStyle = header_st)
    
    # data validation drop down list created
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 1, rows = 2:1000, type = "list", value = sprintf("'Index'!$C$2:$C$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    # suppressWarnings(dataValidation(wb, sheet = "CreateParamUseRef", cols = 2, rows = 2:1000, type = "list", value = sprintf("'Index'!$H$2:$H$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "CreateParamUseRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$G$2:$G$5000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    for (i in 1:nrow(CreateParamUseRef)) {
      openxlsx::conditionalFormatting(wb, "CreateParamUseRef",
                                      cols = 5, rows = 1:i + 1,
                                      type = "blanks", style = openxlsx::createStyle(bgFill = "#FFC7CE")
      )
      
      openxlsx::conditionalFormatting(wb, "CreateParamUseRef",
                                      cols = 5, rows = 1:i + 1,
                                      type = "notBlanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      )
      
      openxlsx::conditionalFormatting(wb, "CreateParamUseRef",
                                      cols = 5, rows = 1:i + 1,
                                      type = "contains", rule = c("NA"), style = openxlsx::createStyle(bgFill = "#FFC7CE")
      )
      
      openxlsx::writeFormula(wb, "CreateParamUseRef",
                             startCol = 6, startRow = i + 1, array = TRUE,
                             x = paste0("=IF(B", i + 1, '="EPA304a","Pass: Will use the EPA304a recommended standards for this parameter",IF(ISNA(MATCH(1,(E', i + 1, "=Index!G:G)*(B", i + 1, '=Index!E:E),0)),
                "Suspect: use name is not listed as a prior use for this organization",IF(ISNA(MATCH(1,(D', i + 1, "=Index!H:H)*(E", i + 1, "=Index!G:G)*(B", i + 1, '=Index!E:E),0)),
                "Suspect: use name is listed as a prior use name in this organization but not for this parameter name",
                "Pass: parameter name and use name are listed as prior cause in ATTAINS for this org and will be used for assessments")))')
      )
    }
    
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      warning("If you would like to replace [CreateParamUseRef], use overwrite = TRUE argument in TADA_CreateParamUseRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  return(CreateParamUseRef)
}

