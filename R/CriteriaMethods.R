#' Criteria and Methodology Template
#'
#' Assessment criteria and methodologies used to evaluate water quality vary
#' across the country. TADA users can fill out this template to define the specific
#' criteria and methodologies for each parameter and use combination they are 
#' interested in analyzing. This table can be filled out manually, auto-populated 
#' with uses and parameters from ATTAINS and the input WQP dataframe, or 
#' developed with TADA helper functions (recommended).It is recommended to run 
#' these three TADA helper functions, [TADA_CreateParamRef()], 
#' [TADA_CreateUseParamRef], and [TADA_CreateMLSummaryRef], in that order to 
#' generate the Criteria and Methodology table specific for your organization. 
#'
#' This criteria and methodology table will be in a TADA compatible format and
#' contain a list of allowable values within each column to define the full
#' criteria, or magnitude only, values associated with an ATTAINS parameter name
#' and use name. For each criteria/magnitude value,
#' users will need to ensure they properly define any additional methods that
#' reflects their water quality criteria and methodologies for a parameter and use.
#' For example, if there are separate criteria and methods for acute versus chronic,
#' rivers versus estuary, different seasons, etc., then a user will need to create
#' additional rows to reflect this. Additional columns are included in this output
#' to capture data sufficiency considerations such as minimum sample sizes,
#' assessment period dates, and seasonality components.
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
#' @param criteriaMethods An optional data frame which contains the completed
#' criteria and methodology table. This will be a user supplied table and any
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
#' @param AUMLRef An optional data frame input. This data frame
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
#' @param auto_assign A Boolean value ("TRUE" or "FALSE"). The default value 
#' is FALSE. If "TRUE", a draft criteria and methods table is generated using 
#' default function inputs for [TADA_CreateParamRef()], 
#' [TADA_CreateUseParamRef], and [TADA_CreateMLSummaryRef]. .data and org_id are
#' required inputs for this function if auto_assign = "TRUE". It is also 
#' recommended to set excel = "TRUE" when 'auto_assign' = "TRUE". The criteria 
#' and methodogology template should be reviewed carefully and edits can be 
#' made manually in Excel. When your review is complete, read the file back into
#' R and re-run this function, TADA_DefineCriteriaMethodology, again. This time, 
#' use the criteriaMethods function input to specify the criteria and methodology
#' table that has already been filled out.
#' 
#' @param displayUniqueId A Boolean value. If TRUE, this will print all unique 
#' TADA.ComparableDataIdentifier in the criteria and methods table output. This is 
#' useful in the alternative options to generate the criteria and methods table
#' without the reference tables.
#'
#' @param epa304a A Boolean value to return epa304a recommended standards for any
#' WQP/TADA/ATTAINS parameter if one is found. Default is FALSE.
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
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' \dontrun{
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
#'   useAURef = NULL, AUMLRef = NULL,
#'   useParamRef = UseParamRef_UT,
#'   excel = FALSE
#' )
#'
#' DefineCriteriaMethodology_UT <- TADA_DefineCriteriaMethodology(
#'   Data_Nutrients_UT,
#'   MLSummaryRef = MLSummaryRef_UT,
#'   displayUniqueId = TRUE,
#'   excel = FALSE
#' )
#' }
#'
TADA_DefineCriteriaMethodology <- function(.data, 
                                           MLSummaryRef = NULL, 
                                           org_id = NULL, # required inputs for the recommended workflow
                                           criteriaMethods = NULL, 
                                           auto_assign = FALSE, # ref = c("ATTAINS", "CST", "TADA", "Other") future development to consider additional crosswalk alternatives?
                                           AUMLRef = NULL, 
                                           useAURef = NULL, # Optional if auto_assign = TRUE
                                           epa304a = FALSE, 
                                           displayUniqueId = FALSE, 
                                           excel = TRUE, 
                                           overwrite = FALSE) {
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")

  # Ensures you have used a valid auto_assign name
  if (!updateRef %in% c("none", "paramRef", "useParamRef", "MLSummaryRef")) {
    stop(paste0(
      "TADA_DefineCriteriaMethodology: ",
      "argument input ", updateRef, " is not a valid entry. Please type one of 'None', 'paramRef', 'useParamRef', 'MLSummaryRef' as a value."
    ))
  }

  # Invalid function input combos - can only use updateRef =  none with auto_assign = FALSE
  if (auto_assign == FALSE && updateRef != "none") {
    stop("TADA_DefineCriteriaMethodology: auto_assign = FALSE. The updateRef function input must be none. If you have updated a reference table, use auto_assign == TRUE")
  }

  # If user supplies criteria methods table, then auto_assign = T for any non-matched values
  if ( !is.null(criteriaMethods)) {
    auto_assign = TRUE
  }  
  
  # Invalid function input combos - supply one or the other.
  # if ( !is.null(MLSummaryRef) && !is.null(criteriaMethods) ) {
  #   stop("TADA_DefineCriteriaMethodology: MLSummaryRef and criteriaMethods are both provided. You can only proceed with one (or none) of these options provided.")
  # }

  # Invalid function input combos - MLSummaryRef and autofill = TRUE cannot be used together
  if (!is.null(MLSummaryRef) && auto_assign == TRUE) {
    stop("TADA_DefineCriteriaMethodology: MLSummaryRef is provided and autofill = TRUE are not valid function argument input combinations.")
  }

  # Generates a blank Criteria and Methods file.
  # Users can still append the epa304a recommended standards if desired.
  if (auto_assign == FALSE && is.null(MLSummaryRef)) {
    desired_cols <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", 
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria",
      # Criteria Columns
      "AcuteChronic", "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
      "DurationValue", "DurationUnit", "DurationMethod",
      "FreqValue", "FreqMethod",
      # Data Sufficiency Columns
      "AssessPeriod", "AssessPeriodStartDate", "AssessPeriodEndDate",
      "Season", "SeasonStartDate", "SeasonEndDate",
      "DistrCount", "DistrPeriod", "DistrMinSample", "Notes"
    )

    DefineCriteriaMethodology <- data.frame(matrix(ncol = length(desired_cols), nrow = 0))

    names(DefineCriteriaMethodology) <- desired_cols

    cols_to_convert <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", 
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria",
      # Criteria Columns
      "AcuteChronic", "EquationBased", 
      # Data Sufficiency Columns
      "AssessPeriod", "Season", "DistrPeriod"
    )

    DefineCriteriaMethodology[c(cols_to_convert)] <- lapply(DefineCriteriaMethodology[cols_to_convert], as.character)

    suppressMessages(
      TADA_ParamRef <- TADA_CreateParamRef(
        .data = .data,
        org_id = org_id,
        excel = excel,
        overwrite = overwrite
      )
    )

    suppressWarnings(
      TADA_UseParamRef <- TADA_CreateUseParamRef(
        .data,
        paramRef = TADA_ParamRef,
        org_id = org_id,
        excel = excel,
        overwrite = overwrite
      )
    )

    suppressMessages(
      MLSummaryRef <- TADA_CreateMLSummaryRef(
        .data,
        useParamRef = TADA_UseParamRef,
        org_id = org_id,
        excel = excel,
        overwrite = overwrite
      )
    )
  }

  # If user wants to create a pre-populated CriteriaMethods table, it will run all crosswalk tables and use the default.
  # Users can edit one or more of the ref files which will update all accordingly.
  if (auto_assign == TRUE) {
    # default, runs all reference tables with no user edits
    #if (updateRef == "none") {
    message(paste0("auto_assign = TRUE selected. Running TADA_CreateParamRef with default assignment."))
    suppressMessages(
      TADA_ParamRef <- TADA_CreateParamRef(
        .data,
        org_id = org_id,
        auto_assign = "All", # auto-populate any exact matches found between WQP CharacteristicName and ATTAINS ParameterName
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    )

    message(paste0("auto_assign = TRUE selected. Running TADA_CreateUseParamRef with default assignment."))
    suppressWarnings(
      TADA_UseParamRef <- TADA_CreateUseParamRef(
        .data,
        org_id = org_id,
        paramRef = TADA_ParamRef,
        auto_assign = TRUE,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    )

    message(paste0("auto_assign = TRUE selected. Running TADA_CreateMLSummaryRef with default assignment."))
    suppressMessages(
      MLSummaryRef <- TADA_CreateMLSummaryRef(
        .data,
        org_id = org_id,
        useParamRef = TADA_UseParamRef,
        AUMLRef = AUMLRef,
        excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      )
    )

    unique_param <- unique(.data$TADA.CharacteristicName)
    # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
    TADA_param <- dplyr::distinct(
      .data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]
    ) %>%
      tidyr::uncount(weights = length(org_id)) %>%
      dplyr::select(-TADA.CharacteristicName) %>%
      dplyr::distinct() %>%
      dplyr::mutate(ATTAINS.OrganizationIdentifier = as.character(rep(org_id, nrow(.) / length(org_id))))

    # Will include all unique TADA Char/ComparableDataIdentifier to be shown in the criteria table
    MLSummaryRef <- TADA_param %>%
      dplyr::left_join(MLSummaryRef)
    #}

    # # user only updates paramRef. This will update paramRef, useParamRef, and MLSummaryRef based on these modifications.
    # if (updateRef == "paramRef") {
    #   message(paste0("auto_assign = TRUE and updateRef = paramRef selected. Running TADA_CreateParamRef with use supplied paramRef assignment. Please review this paramRef table output."))
    #   myfile_ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
    # 
    #   TADA_ParamRef <- TADA_CreateParamRef(
    #     .data,
    #     org_id = org_id,
    #     paramRef = myfile_ParamRef,
    #     auto_assign = "None", # User has now edited the table, turn the auto_assign of in TADA_CreateParamRef
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # 
    #   TADA_UseParamRef <- TADA_CreateUseParamRef(
    #     .data,
    #     org_id = org_id,
    #     paramRef = TADA_ParamRef,
    #     auto_assign = TRUE,
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # 
    #   MLSummaryRef <- TADA_CreateMLSummaryRef(
    #     .data,
    #     org_id = org_id,
    #     useParamRef = TADA_UseParamRef,
    #     AUMLRef = AUMLRef,
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # }
    # 
    # # user only updates useParamRef. This will update useParamRef, MLSummaryRef based on this modifications.
    # if (updateRef == "useParamRef") {
    #   message(paste0("auto_assign = TRUE and updateRef = useParamRef selected. Running TADA_CreateParamRef with use supplied paramRef assignment. Please review this paramRef table output."))
    #   myfile_UseParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseParamRef")
    # 
    #   TADA_ParamRef <- TADA_CreateParamRef(
    #     .data,
    #     org_id = org_id,
    #     paramRef = myfile_UseParamRef, # will update paramRef based on useParamRef
    #     auto_assign = "All",
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # 
    #   TADA_UseParamRef <- TADA_CreateUseParamRef(
    #     .data,
    #     org_id = org_id,
    #     paramRef = TADA_ParamRef,
    #     useParamRef = myfile_UseParamRef,
    #     auto_assign = TRUE,
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # 
    #   MLSummaryRef <- TADA_CreateMLSummaryRef(
    #     .data,
    #     org_id = org_id,
    #     useParamRef = TADA_UseParamRef,
    #     AUMLRef = AUMLRef,
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # }
    # 
    # # user only updates MLSummaryRef in excel. This will update MLSummaryRef based on this modifications.
    # if (updateRef == "MLSummaryRef") {
    #   message(paste0("auto_assign = TRUE and updateRef = MLSummaryRef selected. Running TADA_CreateMLSummaryRef with use supplied paramRef assignment. Please review this paramRef table output."))
    #   myfile_MLSummaryRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateMLSummaryRef")
    # 
    #   TADA_ParamRef <- TADA_CreateParamRef(
    #     .data,
    #     org_id = org_id,
    #     paramRef = myfile_MLSummaryRef, # will update paramRef based on useParamRef
    #     auto_assign = "All",
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # 
    #   TADA_UseParamRef <- TADA_CreateUseParamRef(
    #     .data,
    #     org_id = org_id,
    #     paramRef = TADA_ParamRef,
    #     useParamRef = myfile_MLSummaryRef,
    #     auto_assign = TRUE,
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # 
    #   MLSummaryRef <- TADA_CreateMLSummaryRef(
    #     .data,
    #     org_id = org_id,
    #     useParamRef = TADA_UseParamRef,
    #     AUMLRef = AUMLRef,
    #     MLSummaryRef = myfile_MLSummaryRef,
    #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
    #   )
    # }
  }

  # check to see if user-supplied MLSummary ref is a df with appropriate columns and filled out.
  if (!is.null(MLSummaryRef) & !is.character(MLSummaryRef)) {
    if (!is.data.frame(MLSummaryRef)) {
      stop("TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, UniqueSpatialCriteria,
        ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier")
    }

    if (is.data.frame(MLSummaryRef)) {
      col.names <- c(
        "ATTAINS.ParameterName",
        "ATTAINS.UseName",
        "ATTAINS.OrganizationIdentifier",
        "UniqueSpatialCriteria",
        "ATTAINS.WaterType",
        "ATTAINS.AssessmentUnitIdentifier"
      )

      ref.names <- names(MLSummaryRef)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, UniqueSpatialCriteria,
        ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier")
      }
    }
  }

  # User has went through the recommended workflow. Criteria table is generated
  # from the MLSummaryRef file. This file also contains unique spatial criteria
  # as an option and will include these values if they have been populated.
  if (!is.null(MLSummaryRef)) {
    MLSummaryRef$ATTAINS.ParameterName <- toupper(MLSummaryRef$ATTAINS.ParameterName)
    MLSummaryRef$TADA.ComparableDataIdentifier <- toupper(MLSummaryRef$TADA.ComparableDataIdentifier)
    MLSummaryRef$ATTAINS.WaterType <- as.character(MLSummaryRef$ATTAINS.WaterType)
    MLSummaryRef$SaltFresh <- as.character(MLSummaryRef$SaltFresh)
    MLSummaryRef$TADA.ComparableDataIdentifier <- as.character(MLSummaryRef$TADA.ComparableDataIdentifier)
    MLSummaryRef$ATTAINS.OrganizationIdentifier <- as.character(MLSummaryRef$ATTAINS.OrganizationIdentifier)
    # Extracts the characteristic, speciation and fraction columns to join
    MLSummaryRef <- MLSummaryRef %>%
      dplyr::right_join(
        .data[, c(
          "TADA.ComparableDataIdentifier",
          "TADA.CharacteristicName"
          # "TADA.ResultSampleFractionText",
          # "TADA.MethodSpeciationName"
        )] %>%
          dplyr::distinct(),
        by = "TADA.ComparableDataIdentifier"
      )

    # Creates the DefineCriteriaMethodology table from the MLSummaryRef.
    DefineCriteriaMethodology <- MLSummaryRef %>%
      dplyr::select(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
        "TADA.ComparableDataIdentifier", "TADA.CharacteristicName",
        "SaltFresh",
        "DepthCategory", "UniqueSpatialCriteria", "ATTAINS.WaterType"
      ) %>%
      # Spatial Columns - only pre-populates if a unique spatial criteria is applied.
      dplyr::mutate(ATTAINS.WaterType = dplyr::if_else( # Only pre-populates if a unique spatial criteria is applied
        is.na(UniqueSpatialCriteria),
        as.character(NA),
        as.character(ATTAINS.WaterType)
      )) %>%
      dplyr::mutate(SaltFresh = dplyr::if_else( # Only pre-populates if a unique spatial criteria is applied
        is.na(UniqueSpatialCriteria),
        as.character(NA),
        as.character(SaltFresh)
      )) %>%
      dplyr::mutate(DepthCategory = dplyr::if_else( # Only pre-populates if a unique spatial criteria is applied
        is.na(UniqueSpatialCriteria),
        as.character(NA),
        as.character(DepthCategory)
      )) %>%
      # dplyr::filter(!dplyr::if_all(c(UniqueSpatialCriteria, ATTAINS.WaterType), is.na)) %>%
      dplyr::bind_cols(
        data.frame(
          TADA.ResultSampleFractionText = as.character(NA),
          TADA.MethodSpeciationName = as.character(NA), AcuteChronic = as.character(NA),
          # Criteria Columns
          EquationBased = as.character(NA),
          MagnitudeValueLower = as.numeric(NA), MagnitudeValueUpper = as.numeric(NA), MagnitudeUnit = as.character(NA),
          DurationValue = as.numeric(NA), DurationUnit = as.character(NA), DurationMethod = as.character(NA),
          FreqValue = as.numeric(NA), FreqMethod = as.character(NA),
          # Data Sufficiency Columns
          AssessPeriod = as.character(NA), AssessPeriodStartDate = as.Date(NA), AssessPeriodEndDate = as.Date(NA), Season = as.character(NA),
          Season = as.character(NA), SeasonStartDate = as.Date(NA), SeasonEndDate = as.Date(NA),
          DistrCount = as.numeric(NA), DistrPeriod = as.character(NA), DistrMinSample = as.numeric(NA), Notes = as.character(NA)
        )
      ) %>%
      #tidyr::drop_na(ATTAINS.ParameterName) %>%
      dplyr::select(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", "TADA.ComparableDataIdentifier",
        "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", 
        # Spatial Columns
        "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria",
        # Criteria Columns
        "AcuteChronic", "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
        "DurationValue", "DurationUnit", "DurationMethod",
        "FreqValue", "FreqMethod",
        # Data Sufficiency Columns
        "AssessPeriod", "AssessPeriodStartDate", "AssessPeriodEndDate",
        "Season", "SeasonStartDate", "SeasonEndDate",
        "DistrCount", "DistrPeriod", "DistrMinSample", "Notes"
      ) %>%
      dplyr::arrange(ATTAINS.UseName) %>%
      dplyr::distinct()

    col_names_MLSummary <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", 
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria",
      # Criteria Columns
      "AcuteChronic", "EquationBased", 
      # Data Sufficiency Columns
      "AssessPeriod", "Season", "DistrPeriod"
    )

    DefineCriteriaMethodology[c(col_names_MLSummary)] <- lapply(DefineCriteriaMethodology[col_names_MLSummary], as.character)
  }

  # User wants to populate the criteria table using a user supplied table.
  # This option will prioritize a user-supplied table, but will include
  # all rows for any missing WQP Characteristic (or TADA.ComparableDataIdenftifier) 
  # generated from the auto_assign default values. Users may also append epa 304a values.
  if (!is.null(criteriaMethods)) {
    desired_cols <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", 
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria",
      # Criteria Columns
      "AcuteChronic", "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
      "DurationValue", "DurationUnit", "DurationMethod",
      "FreqValue", "FreqMethod",
      # Data Sufficiency Columns
      "AssessPeriod", "AssessPeriodStartDate", "AssessPeriodEndDate",
      "Season", "SeasonStartDate", "SeasonEndDate",
      "DistrCount", "DistrPeriod", "DistrMinSample", "Notes"
    )

    criteriaMethods$ATTAINS.ParameterName <- toupper(criteriaMethods$ATTAINS.ParameterName)

    # # checks to see if a user supplied criteria table contains ATTAINS.ParameterName found in ATTAINS domain value
    # ATTAINS_param <- rExpertQuery::EQ_DomainValues(domain = "param_name")
    # if (any(!criteriaMethods$ATTAINS.ParameterName %in% ATTAINS_param$name)) {
    #   warning(paste0("Your user supplied criteria table contains a parameter under ATTAINS.ParameterName which is not found as an ATTAINS domain value."))
    # }

    # identifies all unique TADA.CharacteristicNames in TADA data frame
    unique_param <- unique(.data$TADA.CharacteristicName)
    # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
    TADA_param <- dplyr::distinct(
      .data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]
    ) %>%
      tidyr::uncount(weights = length(org_id)) %>%
      dplyr::mutate(ATTAINS.OrganizationIdentifier = as.character(rep(org_id, nrow(.) / length(org_id))))

    criteriaMethods <- criteriaMethods %>%
      dplyr::select(-TADA.ComparableDataIdentifier) %>% # we will join by TADA.CharacteristicName from our TADA dataframe to ensure accurate crosswalk
      dplyr::full_join(
        TADA_param,
        by = c("ATTAINS.OrganizationIdentifier", "TADA.CharacteristicName")
      ) %>%
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id)

    # 2. Identify missing columns
    missing_cols <- setdiff(desired_cols, names(criteriaMethods))

    # 3. Add missing columns with NA values using mutate()
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        criteriaMethods <- criteriaMethods %>%
          dplyr::mutate(!!col := NA)
      }
    }

    # What WQP Characteristic names did the user supplied table miss?
    non_definedCriteria <- criteriaMethods %>%
      dplyr::filter(is.na(ATTAINS.ParameterName)) %>%
      dplyr::select(dplyr::all_of(desired_cols)) %>%
      as.data.frame()

    if (nrow(non_definedCriteria) > 0 && displayUniqueId == TRUE) {
      warning(paste0(
        "Your user supplied criteriaMethods file contains ",
        length(unique(non_definedCriteria$TADA.ComparableDataIdentifier)),
        " unique TADA.ComparableDataIdentifier(s) without a valid ",
        "ATTAINS.ParameterName crosswalk ",
        "when compared to the domain value of ATTAINS from the prior ",
        "ATTAINS assessment cycle for your organization(s). ",
        "Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
      ))
    }

    if (nrow(non_definedCriteria) > 0 && displayUniqueId == FALSE) {
      warning(paste0(
        "Your user supplied criteriaMethods file contains ",
        length(unique(non_definedCriteria$TADA.CharacteristicName)),
        " unique TADA.CharacteristicName(s) without a valid ATTAINS.ParameterName crosswalk ",
        "when compared to the domain value of ATTAINS from the prior ATTAINS assessment cycle for your organization(s). ",
        "Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
      ))
    }

    # If the source of the ATTAINS param and uses is the prior ATTAINS assessment cycle.
    if (auto_assign == TRUE & is.null(useAURef)) {
      warning(paste0(
        "You selected auto_assign == TRUE. No useAURef was provided. ",
        "Filling in these blanks with ATTAINS.ParameterName and ATTAINS.UseName pulled in from the prior ATTAINS Assessment Cycle. ",
        "Please review or edit these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
      ))
    }
    # If the source of the ATTAINS param and uses is from the user supplied useAURef.
    if (auto_assign == TRUE & !is.null(useAURef)) {
      warning(paste0(
        "You selected auto_assign == TRUE. A useAURef was provided. ",
        "Filling in these blanks with ATTAINS.ParameterName and ATTAINS.UseName pulled in from your useAURef. ",
        "Please review or edit these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
      ))  
    }
    
    # From the user supplied criteriaMethods, fill in any values from the pre-filled MLSummaryRef template generated.
    definedCriteria <- criteriaMethods %>%
      dplyr::filter(!is.na(ATTAINS.ParameterName)) %>%
      dplyr::filter(TADA.CharacteristicName %in% TADA_param$TADA.CharacteristicName) %>%
      dplyr::select(dplyr::all_of(desired_cols)) %>%
      as.data.frame()

    # Must now match the data types
    desired_types <- sapply(DefineCriteriaMethodology, class)

    suppressWarnings(
      for (i in 1:ncol(non_definedCriteria)) {
        if (desired_types[[i]] == "numeric") {
          non_definedCriteria[, i] <- as.numeric(non_definedCriteria[, i])
          definedCriteria[, i] <- as.numeric(definedCriteria[, i])
        } else if (desired_types[[i]] == "character") {
          non_definedCriteria[, i] <- as.character(non_definedCriteria[, i])
          definedCriteria[, i] <- as.character(definedCriteria[, i])
        } else if (desired_types[[i]] == "Date") {
          non_definedCriteria[, i] <- as.Date(non_definedCriteria[, i])
          definedCriteria[, i] <- as.Date(definedCriteria[, i])
        }
      }
    )
    
    # If MLSummaryRef does not get generated, and only a user supplied criteriaMethods table is provided
    if (nrow(DefineCriteriaMethodology) == 0 && auto_assign == FALSE) {
      DefineCriteriaMethodology <- criteriaMethods %>%
        dplyr::filter(TADA.CharacteristicName %in% TADA_param$TADA.CharacteristicName) %>%
        dplyr::distinct()
    }

    DefineCriteriaMethodology <- DefineCriteriaMethodology %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
        TADA.ComparableDataIdentifier, TADA.CharacteristicName
      ) %>%
      dplyr::full_join(definedCriteria) %>%
      dplyr::arrange(ATTAINS.UseName) %>%
      dplyr::distinct()

    # should not be a problem if we control what column names are allowed,
    # but including this for the case if edits are made to the function to ensure
    # excel allowable values are still in the correct order.
    DefineCriteriaMethodology <- dplyr::select(DefineCriteriaMethodology, desired_cols)
  }

  # User wants to populate the Criteria table using the EPA304a standards
  # joins the epa304a standards to the current Criteria Table.
  if (epa304a == TRUE) {
    print(paste0(
      "epa304a == TRUE was selected: Joining EPA304a recommended standards by each unique TADA.CharacteristicName only if found."
    ))

    # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
    TADA_param <- dplyr::distinct(
      .data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]
    ) %>%
      tidyr::uncount(weights = length(org_id)) %>%
      dplyr::mutate(ATTAINS.OrganizationIdentifier = as.character(rep(org_id, nrow(.) / length(org_id))))

    # Handling of auto populating EPA304a Criteria in the future if desired.
    suppressWarnings(
      CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
        dplyr::full_join(TADA_param, by = c("TADA.CharacteristicName")) %>%
        dplyr::select(TADA.CharacteristicName, TADA.ComparableDataIdentifier, ATTAINS.ParameterName = POLLUTANT_NAME, ATTAINS.UseName = use_name, AcuteChronic = CRITERIATYPE_ACUTECHRONIC, SaltFresh = CRITERIATYPEFRESHSALTWATER, CRITERION_VALUE, MagnitudeUnit = UNIT_NAME) %>%
        dplyr::mutate(ATTAINS.OrganizationIdentifier = "EPA304a") %>%
        dplyr::mutate(MagnitudeValueLower = dplyr::if_else(
          stringr::str_detect(CRITERION_VALUE, "-"), stringr::str_extract(CRITERION_VALUE, "[^-]+"),
          ""
        )) %>%
        dplyr::mutate(MagnitudeValueUpper = dplyr::if_else(
          stringr::str_detect(CRITERION_VALUE, "-"), stringr::str_split(CRITERION_VALUE, "-", simplify = TRUE)[, 2],
          CRITERION_VALUE
        )) %>%
        dplyr::mutate(MagnitudeUnit = toupper(MagnitudeUnit)) %>%
        dplyr::mutate(dplyr::across(MagnitudeUnit, as.character)) %>%
        dplyr::mutate(dplyr::across(c(MagnitudeValueLower, MagnitudeValueUpper), as.numeric)) %>%
        dplyr::select(-CRITERION_VALUE) %>%
        dplyr::filter(TADA.CharacteristicName %in% TADA_param$TADA.CharacteristicName) %>%
        dplyr::filter(TADA.CharacteristicName %in% DefineCriteriaMethodology$TADA.CharacteristicName)
    )

    DefineCriteriaMethodology <- DefineCriteriaMethodology %>%
      dplyr::full_join(CST_param, relationship = "many-to-many") %>%
      dplyr::arrange(ATTAINS.OrganizationIdentifier != "EPA304a", ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
      dplyr::distinct()
  }

  # Display all unique TADA.ComparableDataIdentifier in the Criteria Methods list or not.
  # Helps a user identifies all WQP data if they do not fill out the reference tables when TRUE
  # FALSE is recommended if a user has gone through a step by step review process to
  # determine what they would like summarized in their final output.
  if (displayUniqueId == FALSE) {
    print(paste0(
      "displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. ",
      "Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."
    ))

    DefineCriteriaMethodology <- DefineCriteriaMethodology %>%
      dplyr::mutate(TADA.ComparableDataIdentifier = NA) %>%
      dplyr::arrange(ATTAINS.OrganizationIdentifier != "EPA304a", ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
      # tidyr::drop_na(ATTAINS.ParameterName) %>%
      dplyr::distinct()
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
    
    # IMPORTANT: Set the "DefineCriteriaMethodology" sheet as the active sheet
    openxlsx::activeSheet(wb) <- "DefineCriteriaMethodology" 
    
    # Set visibility
    names(wb)
    openxlsx::sheetVisibility(wb)[1] <- "hidden"
    openxlsx::sheetVisibility(wb)[2] <- "hidden"
    openxlsx::sheetVisibility(wb)[3] <- "hidden"
    openxlsx::sheetVisibility(wb)[4] <- "hidden"
    openxlsx::sheetVisibility(wb)[5] <- "hidden"
    openxlsx::sheetVisibility(wb)[6] <- TRUE

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
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName", "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", 
      # Spatial Columns
      "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria",
      # Criteria Columns
      "AcuteChronic", "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit",
      "DurationValue", "DurationUnit", "DurationMethod",
      "FreqValue", "FreqMethod",
      # Data Sufficiency Columns
      "AssessPeriod", "AssessPeriodStartDate", "AssessPeriodEndDate",
      "Season", "SeasonStartDate", "SeasonEndDate",
      "DistrCount", "DistrPeriod", "DistrMinSample", "Notes"
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
      startCol = 6, startRow = 1,
      x = unique(.data[, c("TADA.ComparableDataIdentifier", "TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")])
    )

    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 14, startRow = 1,
      # AcuteChronic
      x = data.frame(
        AcuteChronic = c("Acute", "Chronic", "NA")
      )
    )

    # get list of ATTAINS Water Types from ATTAINS
    All.WaterTypeList <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))

    Org.WaterTypeList <- dplyr::filter(All.WaterTypeList, ATTAINS.OrganizationIdentifier %in% org_id)


    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 10, startRow = 1,
      # ATTAINS.WaterType
      x = unique(Org.WaterTypeList$ATTAINS.WaterType)
    )

    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 11, startRow = 1,
      # SaltFresh
      x = data.frame(
        SaltFresh = c("Salt", "Fresh", "NA")
      )
    )

    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 12, startRow = 1,
      x = data.frame(
        DepthCategory = c("No depth info", "Epilimnion-surface", "Surface", "Bottom", "Middle")
      )
    )

    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 13, startRow = 1,
      # UniqueSpatialCriteria
      x = data.frame(
        UniqueSpatialCriteria = c(unique(MLSummaryRef$UniqueSpatialCriteria), "NA")
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
      # DurationMethod
      x = data.frame(
        DurationMethod = c(
          "arithmetic mean", "arithmetic median", "arithmetic max", "arithmetic min",
          "geometric mean", "rolling geometric mean", "rolling arithmetric mean"
        )
      )
    )

    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 23, startRow = 1,
      # FreqMethod
      x = data.frame(
        FreqMethod = c(
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
        AssessPeriod = c("Last 30 years", "Last 10 years", "Last 5 years", "Last 3 years", "Last year", "NA")
      )
    )

    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 27, startRow = 1,
      x = data.frame(
        Season = c("Summer", "Fall", "Spring", "Winter", "NA")
      )
    )

    openxlsx::writeData(
      wb, "Index-Criteria",
      startCol = 31, startRow = 1,
      x = data.frame(
        DistrPeriod = c("Seasonal", "Annual", "Semi-Annual", "Quarterly", "Monthly", "Bi-weekly", "Weekly", "10 days", "NA")
      )
    )

    # The list of allowable values for each column in excel tab [DefineCriteriaMethodology] will be defined by the [Index-Criteria] tab
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$F$2:$F$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$G$2:$G$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$H$2:$H$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 7, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 8, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 10, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 11, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$M$2:$M$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 12, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$N$2:$N$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 13, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$O$2:$O$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 16, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$R$2:$R$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 18, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$T$2:$T$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 19, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$U$2:$U$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 21, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$W$2:$W$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 22, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$X$2:$X$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 25, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$AA$2:$AA$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 29, rows = 2:1000, type = "list", value = sprintf("'Index-Criteria'!$AE$2:$AE$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))

    # Conditional Formatting
    openxlsx::freezePane(wb, "DefineCriteriaMethodology", firstActiveRow = 2, firstActiveCol = 4)
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology",
      cols = 1:31, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "notBlanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology",
      cols = 1:31, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "blanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    ) # modified cells.

    # Group DataSufficiency Columns
    openxlsx::groupColumns(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 22:31,
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

    TADA_CriteriaDataDictionary()
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }

  return(DefineCriteriaMethodology)
}



#' Data Dictionary for Criteria and Methodology
#'
#' Defines and summarizes the column names found in the TADA format for the
#' Criteria and Methodology table for users to fill out.
#'
#' @return An excel data frame tab
#'
#' @export
#'
TADA_CriteriaDataDictionary <- function() {
  
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  wb <- openxlsx::loadWorkbook(wb, downloads_path)
  tryCatch(
    {
      openxlsx::addWorksheet(wb, "DataDictionary")
    },
    error = function(e) {
      openxlsx::removeWorksheet(wb, "DataDictionary")
      openxlsx::addWorksheet(wb, "DataDictionary")
    }
  )
  
  # Example data frame
  data_to_write <- data.frame(
    ColumnName = c(
      "ATTAINS.OrganizationIdentifier",	"ATTAINS.ParameterName",	
      "ATTAINS.UseName", "TADA.ComparableDataIdentifier",	"TADA.CharacteristicName",	"TADA.ResultSampleFractionText",	"TADA.MethodSpeciationName",	
      "ATTAINS.WaterType",	"SaltFresh",	"DepthCategory",	"UniqueSpatialCriteria",	
      "AcuteChronic", "EquationBased",	"MagnitudeValueLower",	"MagnitudeValueUpper",	"MagnitudeUnit",	
      "DurationValue",	"DurationUnit",	"DurationMethod",	"FreqValue",	"FreqMethod",	
      "AssessPeriod",	"AssessPeriodStartDate",	"AssessPeriodEndDate",	
      "Season",	"SeasonStartDate",	"SeasonEndDate",	"DistrCount",	"DistrPeriod",	"DistrMinSample",	"Notes"
      ),
    Requirement = c(
      "Required",	"Required",	"Required",	"Recommended",	"Required",	
      "Recommended",	"Recommended",	"Optional",	"Optional",	"Optional",	"Optional",	
      "Optional",	"Optional",	"Required",	"Required",	"Required",	"Optional",	"Optional",	
      "Optional",	"Optional",	"Optional",	"Optional",	"Optional",	"Optional",	"Optional",	
      "Optional",	"Optional",	"Optional",	"Optional",	"Optional",	"Optional"
    ),
    Source= c(
      "ATTAINS*",	"ATTAINS*",	"ATTAINS*",	"TADA",	"TADA",	"TADA",	"TADA",	"User Supplied",	
      "User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	
      "User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	
      "User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	"User Supplied",	
      "User Supplied",	"User Supplied",	"User Supplied"
      ),
    ColumnType= c(
      "Crosswalk",	"Crosswalk",	"Crosswalk",	"Crosswalk",	"Crosswalk",	"Crosswalk",	"Crosswalk",	"Spatial",	
      "Spatial",	"Spatial",	"Spatial",	"Spatial",	"Criteria",	"Criteria",	"Criteria",	"Criteria",	
      "Criteria",	"Criteria",	"Criteria",	"Criteria",	"Criteria",	"Methodology",	
      "Methodology",	"Methodology",	"Methodology",	"Methodology",	"Methodology",	"Methodology",	
      "Methodology",	"Methodology",	"Methodology"
    ),
    Description = c(
      "The id of your organization that gets submitted to ATTAINS.",
      "The name of the parameter that gets submitted to ATTAINS. These do not need to be unique to your organization.",
      "The name of the use of a waterbody that gets submitted to ATTAINS. These use names should be specific to your organization.",
      paste0(
        "To populate this field, specify displayUniqueId = TRUE. Concatenates the WQP Characteristic, Fraction and speciation into one string.",
        "If provided, this will crosswalk an ATTAINS.ParameterName to this TADA.ComparableDataIdentifier. ", 
        "It is recommended to have performed this crosswalk in TADA_CreateParamRef to avoid any duplicated ",
        "definition of your organization's criteria if they are the same for multiple TADA.ComparableDataIdentifiers.", 
        collapse = " "
        ),	
      "Name of TADA characteristic in the WQP that gets matched to an ATTAINS parameter.",	
      "If TADA.ComparableDataIdentifier is blank, this will group all TADA.CharacteristicName to an ATTAINS.ParameterName on the condition of the specified Fraction Type.",
      "If TADA.ComparableDataIdentifier is blank, this will group all TADA.CharacteristicName to an ATTAINS.ParameterName on the condition of the specified speciation.",
      "The name of the waterbody type associated with an Assessment Unit from the ATTAINS domain value. These values will only be avaialble if a sites to ATTAINS Assessment Units crosswalk is provided.",
      "The salt or freshwater classification of the ATTAINS Waterbody Type. Users should specify if a standard only applies to salt or freshwater types.",
      "Users should specify a specific water column that a standard applies to if applicable. Users can run TADA.FlagDepthCategory to populate this entry.",	
      "Users should specify any monitoring location sites that may contain a unique spatial critieria for a parameter or use in CreateMLSummaryRef.",	
      "If a parameter and use depends depends on differing criteria standards for acute or chronic conditions. Acute is defined as short term while chronic is long term.",	
      "If your water quality standards depend on an equation calculated numeric value, the equation column should be specified as yes. Users will need to specify either a custom equation or choose from a list of common equations and define each equation parameter appropriately. NOTE: Equation handling in TADA is still in development.",	
      "An exceedance is recorded if a ResultValue falls below the defined lower magnitude limit for this parameter and use.",	
      "An exceedance is recorded if a ResultValue is above the defined lower magnitude limit for this parameter and use.",	
      "The numeric value component of the length of time in which a waterbody can be exposed to a magnitude of a parameter without negatively impacting its designated use.",	
      "The units component of the length of time in which a waterbody can be exposed to a magnitude of a parameter without negatively impacting its designated use.",	
      "",	"",	"",	"",	"",	"",	"",	"",	"",	"",	"",	"",	""
      )
  )
  
  # Write the data frame to the worksheet, starting at cell B2
  openxlsx::writeData(wb, "DataDictionary", data_to_write, startCol = 2, startRow = 2)
  
  # Create a style for the header row
  header_style <- openxlsx::createStyle(
    fontSize = 12,
    textDecoration = "bold",
    halign = "center",
    fgFill = "#DCE6F1", # Light blue background
    border = "TopBottomLeftRight",
    borderColour = "#000000"
  )
  
  # Apply the header style to the second row (header)
  openxlsx::addStyle(wb, "DataDictionary", header_style, rows = 2, cols = 2:(ncol(data_to_write) + 1), gridExpand = TRUE)
  
  # Create a style for borders on all data cells
  data_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#000000" # Light grey border
  )
  
  # Apply data border style to all data rows and columns besides header
  openxlsx::addStyle(wb, "DataDictionary", data_border_style, rows = 3:(nrow(data_to_write) + 2), cols = 2:(ncol(data_to_write) + 1), gridExpand = TRUE)
  
  # Define description text that gets wrapped
  wrapStyle <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#000000", # Light grey border
    wrapText = TRUE
  )

  # only applies to the last column. We shifted the table to B2, adjust accordingly
  openxlsx::addStyle(wb, "DataDictionary", wrapStyle, rows = 3:(nrow(data_to_write) + 1), cols = ncol(data_to_write) + 1)

  openxlsx::setColWidths(wb, "DataDictionary", cols = ncol(data_to_write) + 1, widths = 80) # Adjust width as needed
  
  # Set column widths to automatically fit content, except last column
  openxlsx::setColWidths(wb, "DataDictionary", cols = 1:(ncol(data_to_write) - 1), widths = "auto")
  
  # Save the workbook to an Excel file
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
  
}

