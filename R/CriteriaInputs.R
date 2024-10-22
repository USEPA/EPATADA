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
#' Users who have a completed a dataframe for this parameter crosswalk that contains the 
#' required column names, that is created from this function, can choose to skip this step 
#' and proceed to TADA.CreateParamUseRef().
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

TADA_CreateParamRef <- function(.data, overwrite = FALSE, downloads_path = NULL){ 
  
  # If user does not define the path, attempt to pull in the ref files from the default Downloads location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  # testing out different downloads_path as an argument is needed.
  if(!is.null(downloads_path)){
    downloads_path <- downloads_path
  }
  
  # attempting to handle cases in which we don't want to restart a user's entire progress if this function is reran by accident.
  # if(overwrite == FALSE){
  #   loadWorkbook(wb, downloads_path)
  #   stop("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
  # }
  
  # Pulls in all unique combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText in user's dataframe.
  TADA_param <- dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")])
  
  # Pulls in all domain values of parameters. Filtering by state is done in the next steps.
  ATTAINS_param <- rATTAINS::domain_values("ParameterName")
  
  # Pulls in StateCode to get entity's StateCode
  stateCode <- utils::read.csv(system.file("extdata", "statecode.csv", package = "EPATADA"))
  stateCode$STATE <- sprintf("%02d", stateCode$STATE)
  entity <- unique(.data$StateCode)
  
  # Returns the name of the entity based on the stateCode
  entity_name <- dplyr::filter(stateCode, STATE == entity)[[2]]
  
  org_name <- rATTAINS::domain_values(domain_name = "OrgName") %>% 
    dplyr::filter(name == entity_name)
  
  if(nrow(org_name) > 1){
    # Is this possible for more than one state to show up in a df if a single state is doing their WQS assessments?
    warning("More than one organization found in your dataframe")
  }
  
  # Uses rATTAINS to pull in all allowable parameter values by the entity in the dataframe
  ATTAINS_param <- rATTAINS::assessments(
    organization_id = org_name[[3]]
  )
  
  # Extracts all unique parameter names by the entity
  ATTAINS_param <- ATTAINS_param$use_assessment %>%
    dplyr::select(organization_identifier, organization_name, parameters, use_attainments) %>%
    unnest(c(use_attainments), names_sep = ".") %>%
    unnest(c(parameters), names_sep = ".") %>%
    dplyr::select(parameters.parameter_name, organization_identifier, organization_name, use_attainments.use_name) %>%
    distinct() 

  # Create column names for an empty dataframe
  columns <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.CharacteristicName", "ATTAINS.OrgName"
  )
  
  par <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(par) = columns
  
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Index")
  #protectWorksheet(wb, "Index", protect = TRUE)
  addWorksheet(wb, "CreateParamRef")
  
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[1]]$sheetViews
  wb$worksheets[[1]]$sheetViews <- set_zoom(90)
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Write column names in the excel spreadsheet under the tab [CreateParamRef]
  writeData(wb, "CreateParamRef", startCol = 1, x = par, headerStyle = header_st)
  # Format Column widths
  openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(par), widths = "auto")
  
  # Index of ATTAINS allowable values of Entity/Parameter
  writeData(wb, "Index", startCol = 4, x = ATTAINS_param)
  # Index of Unique combinations of WQP TADA Parameters found in TADA dataframe
  writeData(wb, "Index", startCol = 1, x = TADA_param)
  
  # Prints a dataframe of all the unique TADA Characteristic/Fraction/Speciation beginning in column 3 of the tab [CreateparamRef]
  
  # Should we consider trying to provide any sort of param crosswalk to users?
  # intersect(TADA_param$TADA.CharacteristicName, ATTAINS_param$parameters.parameter_name)
  
  writeData(wb, "CreateParamRef", startCol = 1, x = TADA_param)
  
  # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab
  suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 1, rows = 2:1000, type = "list", value = sprintf("'Index'!$A$2:$A$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 2, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 3, rows = 2:1000, type = "list", value = sprintf("'Index'!$C$2:$C$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$D$2:$D$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "CreateParamRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$F$2:$F$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

  # Needs to allow users to input each allowable use for a parameter (this should be in the next step, and not this function - should only be a parameter mapping here)
  for(i in 1:nrow(TADA_param)){
    # excel_formula <- paste0("INDEX(Index!$A$2:$C$1000,MATCH(A",i + 1,",Index!$A$2:$A$1000,0),3)")
    # writeFormula(wb, sheet = "UserCriteriaRef", x = excel_formula, startCol = 3, startRow = i + 1)
    writeData(wb, "CreateParamRef", startCol = 5, startRow = i + 1, x = entity_name)
    conditionalFormatting(wb, "CreateParamRef", 
                          cols = 4, rows = 1:i+1, 
                          type = "blanks", style = createStyle(bgFill = "#FFC7CE"))
    conditionalFormatting(wb, "CreateParamRef", 
                          cols = 4, rows = 1:i+1, 
                          type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8]))
  }
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  if(!is.null(downloads_path)){
    #saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
    downloads_path <- downloads_path
  }
  
  if(overwrite == TRUE){
    print(paste0("Overwriting ", downloads_path))
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    print("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  # ParamRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
  return(ParamRef)
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

TADA_CreateParamUseRef <- function(ParamRef = NULL, overwrite = FALSE, downloads = NULL){ 

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
  
  if(sum(is.na(ParamRef$ATTAINS.CharacteristicName)) > 1){
    warning("NAs were found in ATTAINS.CharacteristicName. Please ensure that you have inputted all field values of interest in the ATTAINS.CharacteristicName column generated from TADA_CreateParamRef() function")
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(ParamRef) & !is.character(ParamRef)) {
    if (!is.data.frame(ParamRef)) {
      stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.CharacteristicName, ATTAINS.OrgName")
    }
    
    if (is.data.frame(ParamRef)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "ATTAINS.CharacteristicName", "ATTAINS.OrgName"
      )
      
      ref.names <- names(ParamRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, ATTAINS.CharacteristicName, ATTAINS.OrgName")
      }
    }
  }
 
  wb <- loadWorkbook(wb, downloads_path)
  
  tryCatch({
    addWorksheet(wb, "CreateParamUseRef")
    },
    error = function(e){
      removeWorksheet(wb, "CreateParamUseRef")
      addWorksheet(wb, "CreateParamUseRef")
    }
  )
  
  CreateParamUseRef <- openxlsx::read.xlsx(downloads_path, sheet = "Index", cols = 4:7) %>%
    dplyr::filter(organization_name == unique(ParamRef$ATTAINS.OrgName)) %>%
    dplyr::filter(parameters.parameter_name %in% unique(ParamRef$ATTAINS.CharacteristicName)) %>%
    # dplyr::rename(parameters.parameter_name, ATTAINS.CharacteristicName) %>%
    dplyr::select(organization_identifier,	organization_name,	parameters.parameter_name, use_attainments.use_name) %>%
    dplyr::mutate(nrowsStandardsRep = as.numeric(1)) %>%
    dplyr::mutate(add_nrows_AcuteChronic = 0) %>%
    dplyr::mutate(add_nrows_WaterTypes = 0) %>%
    dplyr::mutate(add_nrows_SiteSpecific = 0) %>%
    dplyr::rename(ATTAINS.CharacteristicName = parameters.parameter_name, ATTAINS.UseName = use_attainments.use_name) %>%
    dplyr::arrange(ATTAINS.CharacteristicName, ATTAINS.UseName) %>%
    dplyr::distinct()

  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Format Column widths
  openxlsx::setColWidths(wb, "CreateParamUseRef", cols = 1:ncol(CreateParamUseRef), widths = "auto")
  # set zoom size
  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
  sV <- wb$worksheets[[1]]$sheetViews
  wb$worksheets[[1]]$sheetViews <- set_zoom(90)
  
  # Export CreateParamUseRef dataframe into the excel spreadsheet tab
  writeData(wb, "CreateParamUseRef", startCol = 1, x = CreateParamUseRef, headerStyle = header_st)
  
  # Writes an excel formula for nrowsStandardsRep column. Will be based on other two column values
  for(i in 1:nrow(CreateParamUseRef)){
    writeFormula(wb, "CreateParamUseRef", startCol = 5, startRow = i + 1, x = paste0('=1+F',i+1,'+','G',i+1,'+H',i+1))
  }
  addStyle(wb,  "CreateParamUseRef", cols = 4, rows = 1:(nrow(CreateParamUseRef)+2) , style = createStyle(numFmt = "0"), gridExpand = TRUE)
  
  # If the user chooses to enter a separate ParamRef dataframe argument, this makes sure the excel tab reflects the user's changes
  writeData(wb, "CreateParamRef", startCol = 1, x = ParamRef, headerStyle = header_st)
  
  # Conditional Formatting
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 5, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "!=1" , style = createStyle(fontColour = "red")) # using yellow to indicate modified cell
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 5, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==1", style = createStyle(fontColour = "red")) # default values or indicates good to go cells.
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 6, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==1" , style = createStyle(bgFill = TADA_ColorPalette()[8])) # using yellow to indicate modified cell
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 6, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==0", style = createStyle(bgFill = TADA_ColorPalette()[9])) # default values or indicates good to go cells.
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 6, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = ">1" , style = createStyle(bgFill = TADA_ColorPalette()[13])) # red to indicate potential error in this cell.
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 7:8, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "!=0" , style = createStyle(bgFill = TADA_ColorPalette()[8])) # using yellow to indicate modified cell
  conditionalFormatting(wb, "CreateParamUseRef", 
                        cols = 7:8, rows = 2:(nrow(CreateParamUseRef) + 1), 
                        type = "expression", rule = "==0", style = createStyle(bgFill = TADA_ColorPalette()[9])) # default values or indicates good to go cells.
  dataValidation(wb, "CreateParamUseRef", 
                 cols = 6:7, rows = 2:(nrow(CreateParamUseRef) + 1), type = "whole", operator = "between", value = c(0, 9))
  
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
    dplyr::mutate(ExcludeReasoning = NA) %>% # Users can customize this for "warm waters", "only trout based waters" etc. 
    dplyr::mutate(SiteSpecificName = NA) %>%
    dplyr::arrange(ATTAINS.assessmentunitname,	ATTAINS.assessmentunitidentifier,	MonitoringLocationIdentifier)
  
  
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Format Column widths
  openxlsx::setColWidths(wb, "CreateAUIDRef", cols = 8:ncol(CreateAUIDRef), widths = "auto")
  
  writeData(wb, "CreateAUIDRef", startCol = 1, x = CreateAUIDRef, headerStyle = header_st)
  # Writes an excel formula for IncludeorExclude column. Will be based on other two column values
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
      stop("TADA_DefineStandards: 'ParamUseRef' must be a data frame with five columns: organization_identifier,	organization_name,	ATTAINS.CharacteristicName,	ATTAINS.UseName,	nrowsStandardsRep")
    }
    
    if (is.data.frame(ParamUseRef)) {
      col.names <- c(
        "organization_identifier",	"organization_name",	"ATTAINS.CharacteristicName",	"ATTAINS.UseName",	"nrowsStandardsRep"
      )
      
      ref.names <- names(ParamUseRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineStandards: 'ParamUseRef' must be a data frame with five columns: organization_identifier,	organization_name,	ATTAINS.CharacteristicName,	ATTAINS.UseName,	nrowsStandardsRep")
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
  
  columns <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", 
    #"ATTAINS.CharacteristicName", "ATTAINS.OrgName", "ATTAINS.assessmentunitname", "ATTAINS.UseName", "ATTAINS.WaterType", "AUIDExcludeReasoning",
    "ATTAINS.CharacteristicName", "ATTAINS.OrgName", "ATTAINS.UseName", 
    "ATTAINS.WaterType","AcuteChronic",
    "BegAssessDate", "EndAssessDate", "Season", "MinSamplePerDuration", "SiteSpecificName",
    "StandardValue", "StandardUnit", "StandardLimit"	
  )
  
  #param <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  #colnames(param) = columns
  
  DefineStandards <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef") %>%
    dplyr::left_join(ParamUseRef, by = "ATTAINS.CharacteristicName", relationship = "many-to-many") %>%
    dplyr::select("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText","ATTAINS.CharacteristicName", "ATTAINS.OrgName","ATTAINS.UseName", "nrowsStandardsRep", "add_nrows_AcuteChronic", "add_nrows_WaterTypes", "add_nrows_SiteSpecific") %>%
    dplyr::bind_cols(
      data.frame(
        ATTAINS.WaterType = as.character(NA), AcuteChronic = as.character(NA), BegAssessDate = as.Date(NA),
        EndAssessDate = as.Date(NA), Season = as.character(NA), MinimumSample = as.numeric(NA), SiteSpecificName = as.character(NA),
        StandardValue = as.numeric(NA), StandardUnit = as.character(NA), StandardLimit = as.numeric(NA)
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
        add_nrows_WaterTypes >= 1 & !between(id - add_nrows_AcuteChronic - add_nrows_SiteSpecific, 1, add_nrows_WaterTypes)
          ~ "All Others",
        add_nrows_WaterTypes >= 1 & between(id - add_nrows_AcuteChronic - add_nrows_SiteSpecific, 1, add_nrows_WaterTypes)
        ~ "AddWaterTypeHere")
    ) %>%
    dplyr::mutate(
      SiteSpecificName = case_when(
        add_nrows_SiteSpecific >= 1 & (id - add_nrows_AcuteChronic - add_nrows_WaterTypes > 1)
        ~ "AddSiteNameHere")
    ) %>%
    dplyr::select(-c(add_nrows_SiteSpecific, add_nrows_AcuteChronic, add_nrows_WaterTypes, id)) %>%
    dplyr::arrange(ATTAINS.CharacteristicName, ATTAINS.UseName)
     
  DefineStandards$TADA.MethodSpeciationName <- as.character(DefineStandards$TADA.MethodSpeciationName) 
  StandardValue <- DefineStandards %>%
    dplyr::left_join( 
      (select(.data, "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode") %>% 
         dplyr::distinct() %>%
         drop_na(TADA.ResultMeasure.MeasureUnitCode)
       ), 
      by = c("TADA.CharacteristicName", "TADA.MethodSpeciationName","TADA.ResultSampleFractionText")) %>%
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
  writeData(wb, "DefineStandards", startCol = 15, startRow = 1, x = StandardValue)
  
  writeData(wb, "Index", startCol = 8, startRow = 1, x = data.frame(ATTAINS.WaterType = c(unique(.data$MonitoringLocationTypeName), "All Others", "AddWaterTypeHere", "NA")))
  writeData(wb, "Index", startCol = 9, startRow = 1, x = data.frame(AcuteChronic = c("Acute", "Chronic", "NA")))
  writeData(wb, "Index", startCol = 10, startRow = 1, x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA")))
  writeData(wb, "Index", startCol = 11, startRow = 1, x = data.frame(StandardUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode)))
  writeData(wb, "Index", startCol = 12, startRow = 1, x = data.frame(SiteSpecificName = c(unique(AUIDRef$SiteSpecificName), "AddSiteNameHere", "NA")))
  
  # The list of allowable values for each column in excel tab [DefineStandards] will be defined by the [Index] tab
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 7, rows = 2:1000, type = "list", value = sprintf("'Index'!$H$2:$H$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 8, rows = 2:1000, type = "list", value = sprintf("'Index'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 11, rows = 2:1000, type = "list", value = sprintf("'Index'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 13, rows = 2:1000, type = "list", value = sprintf("'Index'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "DefineStandards", cols = 15, rows = 2:1000, type = "list", value = sprintf("'Index'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  
  # If the user chooses to enter a separate ParamUseRef dataframe argument, this makes sure the excel tab reflects the user's changes
  # writeData(wb, "CreateParamUseRef", startCol = 1, x = ParamUseRef, headerStyle = header_st)
  # writeData(wb, "CreateAUIDRef", startCol = 1, x = AUIDRef, headerStyle = header_st)
  # 
  # Conditional Formatting...
  # Conditional Formatting
  conditionalFormatting(wb, "DefineStandards",
                        cols = 7:16, rows = 2:(nrow(DefineStandards) + 1),
                        type = "notBlanks", style = createStyle(bgFill = TADA_ColorPalette()[8])) # default values or indicates good to go cells.
  conditionalFormatting(wb, "DefineStandards",
                        cols = 7:16, rows = 2:(nrow(DefineStandards) + 1),
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
  
  # check to see if user-supplied standards ref is a df with appropriate columns and filled out.
  if (!is.null(StandardsRef) & !is.character(StandardsRef)) {
    if (!is.data.frame(StandardsRef)) {
      stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least nine columns: 
      TADA.CharacteristicName,	TADA.MethodSpeciationName,	TADA.ResultSampleFractionText,	ATTAINS.CharacteristicName,	ATTAINS.OrgName,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
    }
    
    if (is.data.frame(StandardsRef)) {
      col.names <- c(
        "organization_identifier",	"organization_name",	"ATTAINS.CharacteristicName",	"ATTAINS.UseName",	"nrowsStandardsRep"
      )
      
      ref.names <- names(StandardsRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least nine columns: 
        TADA.CharacteristicName,	TADA.MethodSpeciationName,	TADA.ResultSampleFractionText,	ATTAINS.CharacteristicName,	ATTAINS.OrgName,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
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
  
  columns <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", 
    #"ATTAINS.CharacteristicName", "ATTAINS.OrgName", "ATTAINS.assessmentunitname", "ATTAINS.UseName", "ATTAINS.WaterType", "AUIDExcludeReasoning",
    "ATTAINS.CharacteristicName", "ATTAINS.OrgName", "ATTAINS.UseName", 
    "ATTAINS.WaterType","AcuteChronic",
    "BegAssessDate", "EndAssessDate", "Season", "MinSamplePerDuration", "SiteSpecificName",
    "StandardValue", "StandardUnit", "StandardLimit"	
  )
  
  #param <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  #colnames(param) = columns
  
  TADA_StandardsExceedance <- .data %>%
    #dplyr::left_join(TADA_dataframe_test, by = c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName")) %>%
    dplyr::group_by(.data[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName", "MonitoringLocationIdentifier")]) %>%
    dplyr::summarise(
      n_discrete_records = length(TADA.ResultMeasureValue),
      n_aggregated_records = length(unique(ActivityIdentifier)), # this will include discrete records
      .groups = "drop"
    ) %>%
    dplyr::mutate(n_discrete_count = sum(.$n_discrete_records)) %>%
    dplyr::mutate(n_aggregated_count = sum(.$n_aggregated_records))
  
  
  
}


























TADA_CreateParamRef <- function(.data, param = NULL, overwrite = FALSE, downloads_path = NULL){ 
  
  TADAparamUse <- dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")])
  
  # check for required columns if a .data is provided. Not a needed argument, but if users want this ref file
  # to be compatible and filtered by parameters found within their dataframe, they can provide their TADA dataframe.
  req_cols <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText"
  )
  
  # This ref table pulls in the allowable designated uses by Entity and Parameter. Will be used to join onto the TADA by TADA.CharacteristicName

  
  # Pulls in StateCode to get entity's name
  stateCode <- utils::read.csv(system.file("extdata", "statecode.csv", package = "EPATADA"))
  stateCode$STATE <- sprintf("%02d", stateCode$STATE)
  entity <- unique(.data$StateCode)
  
  # Returns the name of the entity based on the stateCode
  entity_name <- dplyr::filter(stateCode, STATE == entity)[[2]]
  
  org_name <- rATTAINS::domain_values(domain_name = "OrgName") %>% 
    dplyr::filter(name == entity_name)
  
  org_name[[3]]
  
  # Uses rATTAINS to pull in all AUID and designated uses for each AUID, and uses this as a reference table to pull in all allowable values in the output table
  ATTAINSParamUse <- rATTAINS::assessments(
    organization_id = org_name[[3]]
  )
  
  # Filters ATTAINSPArameterUse to get use_name by entity
  ATTAINSParamUse <- ATTAINSParamUse %>%
    # dplyr::filter(parameter %in% as.list(unique(char$TADA.CharacteristicName))) %>%
    dplyr::select(organization_name, parameter, use_name) %>%
    dplyr::arrange(param)
  
  # 
  param_ref <- param_ref %>% 
    dplyr::filter(organization_name == entity_name) %>% 
    dplyr::filter(if (is.null(param)) TRUE 
                  else parameter == param
    )
  
  # This creates an empty dataframe for user inputs on Criteria and Methodology of assessments
  columns <- c(
    "TADA.StateCode", "ATTAINS.CharacteristicName", "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText"
  )
  
  par <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(par) = columns
  
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Index")
  protectWorksheet(wb, "Index", protect = TRUE)
  addWorksheet(wb, "ParamRef")
  # Format column header
  header_st <- createStyle(textDecoration = "Bold")
  # Write column name
  writeData(wb, 2, startCol = 1, x = par, headerStyle = header_st)
  # Format Column widths
  openxlsx::setColWidths(wb, 2, cols = 1:ncol(par), widths = "auto")
  
  # Index of ATTAINS allowable values for Entity/Parameter/Designated Use
  writeData(wb, 1, startCol = 1, x = param_ref)
  # Index of Unique combinations of WQP TADA Parameters found in TADA dataframe
  writeData(wb, 1, startCol = 4, x = dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]))
  writeData(wb, 2, startCol = 3, x = dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]))
  
  
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 1, rows = 2:1000, type = "list", value = sprintf("'Index'!$A$2:$A$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 2, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 3, rows = 2:1000, type = "list", value = sprintf("'Index'!$D$2:$D$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$E$2:$E$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$F$2:$F$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  
  # Needs to allow users to input each allowable use for a parameter (this should be in the next step, and not this function - should only be a parameter mapping here)
  for(i in 1:nrow(TADAparamUse)){
    # excel_formula <- paste0("INDEX(Index!$A$2:$C$1000,MATCH(A",i + 1,",Index!$A$2:$A$1000,0),3)")
    # writeFormula(wb, sheet = "UserCriteriaRef", x = excel_formula, startCol = 3, startRow = i + 1)
    writeData(wb, 2, startCol = 1, startRow = i + 1, x = unique(ref$organization_name))
  }
  
  if(is.null(downloads_path)){
    #saveWorkbook(wb, "inst/extdata/myfile.xlsx", overwrite = F)
    downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfile.xlsx")
  }
  
  if(overwrite == TRUE){
    saveWorkbook(wb, downloads_path, overwrite = T)
  }
  
  if(overwrite == FALSE){
    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  
  ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "UserCriteriaRef")
  # ParamRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
  return(ParamRef)
}

#' Create Criteria Ref Table for User Inputs
#'
#' This function returns a .xlsx file named "myfile.xlsx". 
#' All unique combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, and
#' TADA.ResultSampleFractionText will be generated and require users to input the 
#' appropriate ATTAINS.CharacteristicName, ATTAINS.UseName (designated use associated with the parameter),
#' the waterbody Type that this parameter is associated with (if applicable) and whether 
#' this parameter and designated use is associated with a chronic or acute condition. 
#' 
#' Users will have the option to remove or add rows they feel as needed. For example, if NITRATE_AS N_TOTAL
#' criteria standards are specific to a waterbody type (Lakes versus River), then users can add an additional
#' row for NITRATE_AS N_TOTAL to account for each of these conditions.
#' 
#' If there is a parameter that is site specific, then users can input "SiteSpecific" as the TADA.UserStandard value
#' and will have the option of running TADA_SiteSpecific function. Users will input an AU name and all
#' monitoringLocationName found within that AU will be generated as additional rows with all other column values 
#' being the same, with monitoringLocationName being the only difference. 
#' 
#' Users are required to input information on the Standards (Magnitude of concentration) that is not
#' to be exceeded. Users will run TADA_DefineCriteriaRef duration and frequency specifications as needed. 
#' 
#' If no duration or frequency values are provided, then the end output when determining impairments 
#' will use each row in the TADA dataframe as a discrete observation and use the total number of rows 
#' that contains a ResultMeasureValue as the denominator when counting the number of exceedance. 
#' 
#' @param .data TADA dataframe
#' 
#' @param entity a character string abbreviation of the entity's name
#' 
#' @param default a boolean value. If users would like to have each row to capture all combinations
#' of ATTAINS useName for each Parameter, they can indicate this with "TRUE" and are able to delete rows
#' if that parameter is not applicable to that designated useName.
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, ATTAINS.CharacteristicName, ATTAINS.UseName, ATTAINS.EntityName, 
#' TADA.UserStandardUnit, TADA.UserStandardValue
#' 
#' 
#' @export
#'
#' @examples
#' # create criteria reference for Utah nutrients example data set
#' Data_Nutrients_UT <- TADA_AutoFilter(Data_Nutrients_UT)
#' Data_Nutrients_UT <- TADA_FindNearbySites(Data_Nutrients_UT)
#' UT_CriteriaRef <- TADA_CreateStandardsRef(Data_Nutrients_UT)
#' UT_CriteriaRef <- TADA_CreateStandardsRef(Data_Nutrients_UT, ParamUseRef = Data_Nutrients_ParamUse_ref, AUIDRef = Data_Nutrients_AUID_ref, default = TRUE)
#' UT_CriteriaRef2 <- TADA_CreateStandardsRef(Data_Nutrients_UT, ParamUseRef = Data_Nutrients_ParamUse_ref, AUIDRef = Data_Nutrients_AUID_ref2, default = TRUE)
#' UT_CriteriaRef_with_use <- TADA_CreateStandardsRef(Data_Nutrients_UT, useNameRep = TRUE, ParamUseRef = Data_Nutrients_ParamUse_ref)
#' 

TADA_CreateStandardsRef <- function(.data, overwrite = FALSE, ParamRef = NULL, AUIDRef = NULL, downloads_path = NULL) {
  
  # myfile.xlsx is the default name of the file created in TADA_CreateParamRef
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfile.xlsx")
  
  if(is.null(ParamRef)){
    ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "UserCriteriaRef")
  }
  
  if(sum(is.na(ParamRef$ATTAINS.CharacteristicName)) > 1){
    warning("NAs were found in ATTAINS.CharacteristicName. Please ensure that you have inputted all field values of interest in the ATTAINS.CharacteristicName column generated from TADA_CreateParamRef() function")
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
   if (!is.null(ParamRef) & !is.character(ParamRef)) {
    if (!is.data.frame(ParamRef)) {
      stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.StateCode, ATTAINS.CharacteristicName, TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText")
    }

    if (is.data.frame(ParamRef)) {
      col.names <- c(
        "TADA.StateCode", "ATTAINS.CharacteristicName", "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText"
      )

      ref.names <- names(ParamRef)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'ParamRef' must be a data frame with five columns: TADA.StateCode, ATTAINS.CharacteristicName, TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText")
      }
    }
  }
  
  if (!is.character(AUIDRef)) {
    if (!is.data.frame(AUIDRef)) {
      stop("TADA_CreateStandardsRef: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
    }
    
    if (is.data.frame(AUIDRef)) {
      col.names <- c(
        "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "LongitudeMeasure", "LatitudeMeasure"
      )
      
      ref.names <- names(AUIDRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
      }
    }
  }
  
  AllowableUse <- utils::read.csv(system.file("extdata", "ATTAINSParameterUseMapRef.csv", package = "EPATADA"))

  AllowableUse <- AllowableUse %>%
    dplyr::filter(parameter %in% as.list(unique(ParamRef$ATTAINS.CharacteristicName))) %>%
    dplyr::filter(organization_name %in% as.list(unique(ParamRef$TADA.StateCode))) %>%
    dplyr::select(use_name) %>%
    dplyr::distinct()
  
  # This creates an empty dataframe for user inputs on Criteria and Methodology of assessments
  columns <- c(
    "TADA.StateCode", "ATTAINS.CharacteristicName", "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText",
    "ATTAINS.UseName", "ATTAINS.assessmentunitname",	"ATTAINS.WaterType", "ATTAINS.CharacteristicGroup",
    "TADA.UserAcuteChronic", "TADA.UserStandardValue", "TADA.UserStandardUnit", "TADA.StandardLimit"	
  )
  
  param <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(param) = columns
  
  library(openxlsx)
  wb <- loadWorkbook(file = system.file("extdata", "myfile.xlsx", package = "EPATADA"))
  header_st <- createStyle(textDecoration = "Bold")
  removeWorksheet(wb, "UserCriteriaRef")
  addWorksheet(wb, "UserCriteriaRef2")
  writeData(wb, 2, startCol = 1, x = param, headerStyle = header_st)
  openxlsx::setColWidths(wb, 2, cols = 1:ncol(param), widths = "auto")
  
  writeData(wb, 1, startCol = 7, x = data.frame(ATTAINS.UseName = AllowableUse$use_name))
  writeData(wb, 1, startCol = 8, x = data.frame(ATTAINS.assessmentunitname = unique(AUIDRef$ATTAINS.assessmentunitname)))
  writeData(wb, 1, startCol = 9, x = data.frame(ATTAINS.WaterType = unique(.data[, "MonitoringLocationTypeName"])))
  writeData(wb, 1, startCol = 10, x = data.frame(ATTAINS.CharacteristicGroup = c("Metals", "Nutrients", "Microbiological", "Dissolved Oxygen", "Other", "NA" )))
  writeData(wb, 1, startCol = 11, x = data.frame(TADA.UserAcuteChronic = c("Acute", "Chronic", "NA")))
  writeData(wb, 1, startCol = 12, x = data.frame(TADA.UserStandardValue = c("equation", "site-specific", as.character(1:90))))
  writeData(wb, 1, startCol = 13, x = data.frame(TADA.UserStandardUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode)))
  writeData(wb, 1, startCol = 14, x = data.frame(TADA.StandardLimit = c("Upper", "Lower", "Range")))
  #writeData(wb, 2, x = ATTAINSParameterUse)
  
  # param_data_frame <- dplyr::distinct(
  #   +     Data_Nutrients_UT[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]) %>% 
  #   +     mutate(entity = "utah") %>%
  #   +     uncount(nrow(ParamUseRef)) %>%
  #   +     mutate(ATTAINS.assessmentunitname = rep(Data_Nutrients_AUID_ref$ATTAINS.assessmentunitname, length.out = n()))
  # > View(param_data_frame)
  
  # Pulls in AUIDRef for all unique assessment unit name. Will repeat each record in UserCriteriaRef for each unique AUID label
  ParamRef <- ParamRef %>% 
    uncount(length(unique(AUIDRef$ATTAINS.assessmentunitname))) %>%
    dplyr::mutate(ATTAINS.assessmentunitname = rep(unique(AUIDRef$ATTAINS.assessmentunitname), length.out = n())) %>%
    dplyr::mutate(across(ATTAINS.CharacteristicName, as.character))
  
  AUID_data_frame <- dplyr::distinct(
    AUIDRef[,"ATTAINS.assessmentunitname"]) %>% 
    dplyr::mutate(entity = entity_name)
  
  # writeData(wb, 2, x = param, headerStyle = header_st)
  # writeData(wb, 2, startCol = 1, x = ParamRef, headerStyle = header_st)
  
  # Pulls in StateCode to get entity's name
  stateCode <- utils::read.csv(system.file("extdata", "statecode.csv", package = "EPATADA"))
  stateCode$STATE <- sprintf("%02d", stateCode$STATE)
  entity <- unique(.data$StateCode)
  
  # Returns the name of the entity based on the stateCode
  entity_name <- dplyr::filter(stateCode, STATE == entity)[[2]]
  
  org_name <- rATTAINS::domain_values(domain_name = "OrgName") %>% 
    dplyr::filter(name == entity_name)
  
  org_name[[3]]
  
  # Uses rATTAINS to pull in all AUID and designated uses for each AUID, and uses this as a reference table to pull in all allowable values in the output table
  data_test <- rATTAINS::assessments(
    organization_id = org_name[[3]]
  )

  data_test_ref <- data_test$use_assessment %>%
    dplyr::select(organization_name, assessment_unit_identifier, use_attainments, parameters) %>%
    unnest(c(use_attainments), names_sep = ".") %>%
    unnest(c(parameters), names_sep = ".") %>%
    dplyr::select(organization_name, assessment_unit_identifier, use_attainments.use_name, parameters.parameter_name) %>%
    distinct() 
  
  # testing <- left_join(AUIDRef,data_test_ref, by = c("ATTAINS.assessmentunitidentifier"= "assessment_unit_identifier", "ATTAINS.CharacteristicName" = "parameters.parameter_name"), relationship = "many-to-many")
  
  # Pulls in UNIQUE COMBINATIONS of Param and Des Use found from rATTAINS as a 'ParamUseRef'.
  ParamRef2 <- left_join(ParamRef, data_test_ref, by = c("ATTAINS.CharacteristicName" = "parameters.parameter_name"), relationship = "many-to-many")
  
  writeData(
    wb, 2, startCol = 1, 
      x = 
        ParamRef2 %>%
        # uncount(n) %>% 
        # mutate(ATTAINS.UseName = rep(AllowableUse$use_name, length.out = n())) %>%
        select("TADA.StateCode", "ATTAINS.CharacteristicName", "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText",
                 "use_attainments.use_name") %>%
        distinct(),
    headerStyle = header_st
  )
    # writeData(wb, 2, startCol = 12,
    #     x = 
    #       ParamRef %>% 
    #       uncount(n) %>% 
    #       mutate(ATTAINS.UseName = rep(AllowableUse$use_name, length.out = n())) %>%
    #       dplyr::left_join(chars, by = c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")) %>%
    #       select("TADA.ResultMeasure.MeasureUnitCode"),
    #   headerStyle = header_st
    # )
  
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 1, rows = 2:1000, type = "list", value = sprintf("'Index'!$A$2:$A$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 2, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 3, rows = 2:1000, type = "list", value = sprintf("'Index'!$D$2:$D$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$E$2:$E$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$F$2:$F$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index'!$G$2:$G$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 7, rows = 2:1000, type = "list", value = sprintf("'Index'!$H$2:$H$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 8, rows = 2:1000, type = "list", value = sprintf("'Index'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 10, rows = 2:1000, type = "list", value = sprintf("'Index'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 11, rows = 2:1000, type = "list", value = sprintf("'Index'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 12, rows = 2:1000, type = "list", value = sprintf("'Index'!$M$2:$M$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef2", cols = 13, rows = 2:1000, type = "list", value = sprintf("'Index'!$N$2:$N$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  
  if(is.null(downloads_path)){
    #saveWorkbook(wb, "inst/extdata/myfile.xlsx", overwrite = F)
    downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfile2.xlsx")
  }
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfile2.xlsx")
  
  if(overwrite == TRUE){
    saveWorkbook(wb, downloads_path, overwrite = T)
    cat("File saved to:", gsub("/","\\\\",downloads_path), "\n")
  }
  
  if(overwrite == FALSE){
    paste0("Please open the excel file generated under ", gsub("/","\\\\",downloads_path), " and make the appropriate edits that corresponds to your WQS assessments.")
    print("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
    saveWorkbook(wb, downloads_path, overwrite = F)
  }
  
  rm(columns, param)
  
  CriteriaRef <- openxlsx::read.xlsx(downloads_path, sheet = "UserCriteriaRef2")
  print(paste0("Please open the excel file generated under ",downloads_path, "and make the appropriate edits that corresponds to your WQS assessments."))
  
  return(CriteriaRef)
  
}

#' Create Additional Criteria Ref Table for User Inputs
#' 
#' User will need to have ran TADA_CreateCriteriaRef function first. Current function does replace 
#' the output, so considerations on how to save a user's progress will need to be considered during
#' each step.
#'
#' @param .data TADA dataframe
#' 
#' @param entity a character string abbreviation of the entity's name
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, ATTAINS.CharacteristicName, ATTAINS.UseName, ATTAINS.EntityName, 
#' TADA.UserStandardUnit, TADA.UserStandardValue, TADA.UserDurationValue, TADA.UserDurationUnit,. 
#' All values for
#' 
#' 
#' @export
#'
#' @examples
#' # create criteria reference for Utah nutrients example data set
#' UT_CriteriaRef <- TADA_CreateStandardRef(Data_Nutrients_UT, entity = "Utah")
#' UT_CriteriaRef2 <- TADA_CreateAdditionalStandardsRef(Data_Nutrients_UT)
#' 

TADA_CreateAdditionalStandardsRef <- function(.data, ref = "null") {
  
  # Run a check to see if TADA_CreateCriteriaRef has been ran or not
  # check to see if user-supplied ref is a df
  if (!is.character(ref)) {
    if (!is.data.frame(ref)) {
      stop("TADA_CreateAdditionalStandardsRef: 'ref' must be a data frame with eleven columns: TADA.CharacteristicName,
         TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
         TADA.PairingGroup.Rank, and TADA.PairingGroup.")
    }
    
    if (is.data.frame(ref)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode",
        "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText",
        "TADA.PairingGroup.Rank", "TADA.PairingGroup"
      )
      
      ref.names <- names(ref)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'ref' must be a data frame with six columns: TADA.CharacteristicName,
         TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
         TADA.PairingGroup.Rank, and TADA.PairingGroup.")
      }
    }
  }
  
  
  # Additional CriteriaRef definitions should be defined. 
  # Allowable values for these columns will be needed to run TADA functionalities down the line.
  # Examples on why each column represents should be shown as well.
  # TADA.MinimumSampleSize = numeric, how many observations are needed. Will this be based on samples per duration period (ex. daily average) or 
  #     for each discrete observation? 
  # TADA.A
  
  
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  addCol <- c(
    "TADA.MinimumSampleSize", "TADA.AssessmentBegDate", "TADA.AssessmentEndDate", 
    "TADA.Season", "TADA.otherParamDependency", "TADA.MultipleStandards"
    )
  
  CriteriaRef[, addCol] <- NA
  
  wb <- loadWorkbook(file = system.file("extdata", "myfile.xlsx", package = "EPATADA"))
  writeData(wb, 2, x = CriteriaRef)
  saveWorkbook(wb, "inst/extdata/myfile.xlsx", overwrite = T)
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")

  return(CriteriaRef)
}

#' Duration and Frequency Criteria Specification
#'
#' This function requires users to run TADA_CreateCriteriaRef and fill out the CriteriaRef file.
#' For each row depending on its parameter group, additional columns for duration and frequency
#' will be generated and filled out. 
#' 
#' If users do not include Duration and Frequency criteria in their ref file, then impairment decisions
#' will be based on each observation in the tada dataframe with no averaging period used and the denominator
#' will be for every observation that matches a row entry in the CriteriaRef as no frequency or duration is defined.
#' 
#' 
#' Users will be required to validate these values for reasonability based on their state's water quality standards criteria. 
#' This function attempts to fill out default duration and frequency values for common parameters. However,
#' each state may deviate and have their own defined and approved criteria standards that they use.
#' 
#' Duration is defined as the averaging time period over which a parameter is measured. 
#' 
#' If an average is used, then an average based on the defined Duration Value - such as "one hour"- will look at 
#' one hour averages 
#' 
#'
#' @param .data TADA dataframe
#' 
#' @param entity a character string abbreviation
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, TADA.UserStandardUnit, and TADA.UserStandardValue. All values for
#' TADA.UserStandardUnit and TADA.UserStandardValue are NA and must be filled in by the user before
#' use in TADA_SimpleCriteriaComparison.
#' 
#' @export
#'
#' @examples
#' UT_CriteriaRef3 <- TADA_DefineCriteriaRef()
#' 

TADA_DefineCriteriaRef <- function(StandardsRef = NULL, downloads_path = NULL, overwrite = FALSE, autofill = TRUE) {
  # Users will be able to specify what duration and/or frequency should be applied for a parameterGroup 
  # (or should this be applied on a TADA.Characteristic level?). 
  
  # myfile2.xlsx is the default name of the Standards Ref file created in TADA_CreateParamRef
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfile2.xlsx")
  
  if(is.null(StandardsRef)){
    CriteriaRef <- openxlsx::read.xlsx(downloads_path, sheet = "UserCriteriaRef2")
  }
  
  # check .data has all of the required columns - Needs edit still
  expected_cols <- c(
    "TADA.StateCode", "ATTAINS.CharacteristicName", "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText",
    "ATTAINS.UseName", "ATTAINS.assessmentunitname",	"ATTAINS.WaterType", "ATTAINS.CharacteristicGroup",
    "TADA.UserAcuteChronic", "TADA.UserStandardValue", "TADA.UserStandardUnit", "TADA.StandardLimit"
  )
  
  # TADA_CheckColumns(CriteriaRef, expected_cols)
  
  col_Criteria <- c("TADA.DurationValue", "TADA.DurationPeriod", "TADA.FrequencyValue", "TADA.FrequencyPeriod")
  
  if(autofill == TRUE){
    CriteriaRef[, col_Criteria] <- NA
    DefineCritera <- CriteriaRef %>%
    # Attempts to capture common duration and frequency values based on Parameter Group definition.
    ## Should we consider this on a parameter level instead? Does Parameter Group encompass too many different parameters
    ## that don't follow the same criteria?
    dplyr::mutate(
      TADA.UserDurationValue = case_when(
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Acute" ~ 1,
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Chronic" ~ 4,
        ATTAINS.CharacteristicGroup == "Pathogens" & TADA.UserAcuteChronic == "Acute" ~ 1, #E. coli, Enterococci
        ATTAINS.CharacteristicGroup == "Nutrients" ~ 30
        ) 
      )%>%
    dplyr::mutate(
      TADA.UserDurationPeriod = case_when(
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Acute" ~ "hour average",
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Chronic" ~ "day average",
        ATTAINS.CharacteristicGroup == "Pathogens" & TADA.UserAcuteChronic == "Acute" ~ "hour geometric mean", #E. coli, Enterococci
        ATTAINS.CharacteristicGroup == "Nutrients" ~ "day arithmetic average"
                                                       
      ))
  }
  
  if(autofill == FALSE){
    CriteriaRef[, col_Criteria] <-NA
  }
  # temp <- TADA_CreatePairRef(.data, hardness = TRUE)
  # TADA_PairForCriteriaCalc(temp)
  
  # Attempts to define common duration periods for certain Parameter Groupings, Acute versus Chronic, and by EPA 10004d federal standards.
  # Users will be warned to validate these duration periods and to take note these are not mandated standards, but recommended ones.
  
  
  return(DefineCritera)
  
}

#' Returns Criteria Standards template dataframe with Rows for each MonitoringLocationIdentifier/MonitoringLocationName
#'
#' This function will ask users to submit a completed TADA.CriteriaRef 
#' dataframe. For each parameter and designated use row entry that is filled with "site-specific"
#' for the standards column, separate rows for unique monitoringLocationName(s) will be generated
#' based on user defined AUID(s) or AU_name(s). 
#' 
#' Users are asked to fill in the TADA.UserStandardValue for each site-specfic defined parameter and designated use
#' standards. Any rows that are not defined as site specific in TADA.UserStandardValue will be kept as is and not changed.
#' 
#'
#' @param .data A TADA dataframe with TADA_GetATTAINS() geospatial function ran.
#' 
#' @param .dataCriteriaRef a dataframe generated from TADA_CreateRef(). 
#' 
#' @param stateCode a character string of the abbreviated state in which the TADA dataframe should be filtered by.
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#' 
#' @export
#'
#' @examples
#' # example dataset with ATTAINS getspatial below
#' UT_SiteSpecific_Ref <- TADA_SiteSpecificStandards(UT_CriteriaRef3, AUIDRef = Data_Nutrients_AUID_ref, AUID = "UT16020102-053_00")
#' 

TADA_SiteSpecificStandards <- function(CriteriaRef, AUIDRef, AUID){ 

  # check for required columns
  req_cols <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode", "ATTAINS.assessmentunitidentifier",
    "MonitoringLocationIdentifier", "MonitoringLocationName"
  )
  
  if(is.na(sum(str_detect(CriteriaRef$TADA.UserStandardValue, "site-specific")))){
    stop("There are no 'site-specific' standards to define in your CriteriaRef list. TADA_SiteSpecificStandards will only run if there are rows that need to be further defined by specific monitoring sites")
  }
  
  # Must perform a check if user has filled out the CriteriaRef file
  # CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
  # Filters the TADA_ATTAINS dataframe by the AUID or monitoringLocationName argument parameter in the function
  # and returns a dataframe of all the unique monitoringLocationName by that AUID.
  # if (!is.null(AUID)){
  #   AUID_ref <- TADA_AUIDMonitoringLocation(.data, AUID = AUID)
  # }
  # 
  # if (is.null(AUID)){
  #   AUID_ref <- TADA_AUIDMonitoringLocation(.data)
  # }
  
  AUID_CriteriaRef <- CriteriaRef %>%
    #dplyr::mutate(., case_when(is.na(AUID), monitoringLocationName = NA)) %>%
    dplyr::left_join(AUIDRef, by = "ATTAINS.assessmentunitname", relationship = "many-to-many") %>%
    dplyr::mutate(ATTAINS.MonitoringLocationIdentifier =
      case_when(
        TADA.UserStandardValue ==  "site-specific" ~ MonitoringLocationIdentifier)
      ) %>%
    dplyr::mutate(TADA.UserStandardValue =
      case_when(
        TADA.UserStandardValue ==  "site-specific" ~ NA)
    ) %>%
    select("ATTAINS.CharacteristicName", "TADA.CharacteristicName", "TADA.MethodSpeciationName",
           "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode", "entity", "ATTAINS.assessmentunitname", "ATTAINS.MonitoringLocationIdentifier", "ATTAINS.UseName", colnames(CriteriaRef)[8:length(CriteriaRef)] ) %>%
    distinct()
  
  return(AUID_CriteriaRef)
}

#' Returns Impairment Decisions for each Row in CriteriaRef
#'
#' This function will ask users to submit a completed TADA.CriteriaRef 
#' dataframe. For each parameter and designated use row entry that is filled with "site-specific"
#' for the standards column, separate rows for unique monitoringLocationName(s) will be generated
#' based on user defined AUID(s) or AU_name(s). 
#' 
#' Users are asked to fill in the TADA.UserStandardValue for each site-specfic defined parameter and designated use
#' standards. Any rows that are not defined as site specific in TADA.UserStandardValue will be kept as is and not changed.
#' 
#'
#' @param .data A TADA dataframe with TADA_GetATTAINS() geospatial function ran.
#' 
#' @param .dataCriteriaRef a dataframe generated from TADA_CreateRef(). 
#' 
#' @param stateCode a character string of the abbreviated state in which the TADA dataframe should be filtered by.
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#' 
#' @export
#'
#' @examples
#' # example dataset with ATTAINS getspatial below
#  # ATTAINS_DATA_UT <- TADA_GetATTAINS(Data_Nutrients_UT)
#' load(ATTAINS_DATA_UT)
#' 

TADA_ATTAINSImpairmentDecision <- function(.data, monitoringLocationName, AUID){ 
  
  library(arsenal)
  library(httr)
  library(tidyverse)
  library(dplyr)
  library(jsonlite)
  
  # check for required columns
  req_cols <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode", "ATTAINS.assessmentunitidentifier"
  )
  
  if(!is.character(stateCode)){
    stop('stateCode must be a character input. Please specify the abbreviation of the state name in which that your data is contained')
  }
  # ATTAINS web services - does this need to be pulled in? 
  temp <- GET(paste0("https://attains.epa.gov/attains-public/api/assessmentUnits?stateCode=",stateCode)) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE)
  
  # Filters the TADA_ATTAINS dataframe by the AUID or monitoringLocationName argument parameter in the function
  # and returns a dataframe of all the unique monitoringLocationName by that AUID.
  MonitoringRef <- dplyr::filter(.data$TADA_with_ATTAINS, ATTAINS.assessmentunitname == AUID) %>%
    select("MonitoringLocationName", "MonitoringLocationTypeName", "MonitoringLocationIdentifier", "ATTAINS.assessmentunitname")
  distinct()
  
  .dataCriteriaRef <- .dataCriteriaRef %>%
    dplyr::mutate(., case_when(TADA.UserStandardValue ==  "site specific", ATTAINS.assessmentunitname = AUID, ATTAINS.assessmentunitname = NA)) %>%
    #dplyr::mutate(., case_when(is.na(AUID), monitoringLocationName = NA)) %>%
    dplyr::left_join(., MonitoringRef, by = "ATTAINS.assessmentunitname")
  
  
  return(.data)
}
