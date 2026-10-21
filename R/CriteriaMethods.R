#' Criteria and Methodology Template
#'
#' Assessment criteria and methodologies used to evaluate water quality vary
#' across the country. TADA users can fill out this template to define the specific
#' criteria and methodologies for each parameter and use combination they are
#' interested in analyzing. This table can be filled out manually, auto-populated
#' with uses and parameters from ATTAINS and the input WQP dataframe, or
#' developed with TADA helper functions (recommended). It is recommended to run
#' these four TADA helper functions, [TADA_CrosswalkWQPATTAINSParam()],
#' [TADA_CrosswalkCSTATTAINSUses()], [TADA_CreateParamUseRef()] and [TADA_MLSummary()], in that
#' order to generate the Criteria and Methodology table specific for your organization.
#'
#' This criteria and methodology table will be in a TADA compatible format and
#' contain a list of allowable values within each column. For each ATTAINS parameter
#' name and use name, users may choose to define the full criteria and methodologies
#' information or magnitude values only. For example, if there are separate criteria
#' and methods for acute versus chronic, rivers versus estuaries, different seasons,
#' etc., then a user will need to create additional rows to reflect this.
#' Additional columns are included in this output
#' to capture data sufficiency information such as minimum sample sizes,
#' assessment period dates, and seasonality.
#'
#' Allowable values for ATTAINS.UseName, ATTAINS.ParameterName, and
#' ATTAINS.OrganizationIdentifier:
#' ATTAINS.uses = rExpertQuery::EQ_DomainValues("use_name", api_key = .setEQKey())
#' ATTAINS.parameters <- rExpertQuery::EQ_DomainValues("param_name", api_key = .setEQKey())
#' ATTAINS.organizations <- rExpertQuery::EQ_DomainValues("org_id", api_key = .setEQKey())
#'
#' @param .data A TADA data frame. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. "USEPA" may be included as an org_id which will populate the EPA 304(a)
#' recommended criteria for any TADA.CharacteristicName if one is found. "All" or
#' "NULL" are also allowable values and may be helpful for new ATTAINS users or
#' those performing assessments for multiple states and tribes. If "All" is
#' selected, this will return all prior ATTAINS information from all ATTAINS
#' organizations in prior ATTAINS assessment cycles as individual rows for each
#' organization. If "NULL" is selected all unique prior ATTAINS information from
#' any ATTAINS organizations are returned but are not labeled and can be manually
#' edited. Enter `rExpertQuery::EQ_DomainValues("org_id", api_key = .setEQKey())`
#' into the console to get a list of valid organization identifiers. A list of
#' organization identifiers can also be found by downloading the ATTAINS Domains
#' Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "code" column of the "OrgName" tab.
#'
#' @param criteriaMethods An optional data frame which contains the completed
#' criteria and methodology table. This will be a user supplied table and any
#' inputs in this table will be prioritized. Additional rows for any parameter(s)
#' that are not found in the user supplied table will be included in the output.
#' These rows will need to have the ATTAINS.ParameterName, ATTAINS.UseName,
#' and the criteria and methodology inputs filled out manually if you would like
#' analysis to be done for it.
#'
#' @param MLSummaryRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit. If provided the data frame must contain
#' these columns:
#' "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
#' "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
#' "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
#' "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "LongitudeMeasure",
#' "LatitudeMeasure", "IncludeOrExclude" and "UniqueSpatialCriteria".
#' When MLSummaryRef is provided and displayUniqueId is not specified, IDs are
#' retained by default to support MLSummary-based filtering.
#'
#' @param AU_UsesRef An optional data frame input. If provided, the ATTAINS.UseName
#' will be filtered by the ATTAINS.UseName found in this data frame. It will also
#' assign the uses to each assessment unit defined in this table if an AUMLRef
#' is also provided. This data frame must contain the following column names
#' which can be generated from the output of TADA_AssignUsesToAU:
#' ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName,
#' and ATTAINS.WaterType.
#'
#' @param AUMLRef An optional data frame input containing a completed crosswalk
#' of monitoring location sites associated with an assessment unit. If provided,
#' each Monitoring location site will get assigned to an ATTAINS.AssessmentUnitIdentifier
#' to allow users to analyze by either assessment units or by monitoring location.
#' This data frame must contain the following column names which can be generated
#' from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
#'
#' @param auto_assign Boolean argument with two possible values: TRUE and FALSE.
#' The default value is FALSE. If TRUE, a draft criteria and methods table is
#' generated using default function inputs for [TADA_CrosswalkWQPATTAINSParam()],
#' [TADA_CrosswalkCSTATTAINSUses()], [TADA_CreateParamUseRef()] and [TADA_MLSummary()]. 
#' .data and org_id are required inputs for this function if auto_assign = TRUE.
#' It is also recommended to set excel = TRUE when auto_assign = TRUE. The
#' criteria and methodology template should be reviewed carefully and edits can be
#' made manually in Excel. When your review is complete, read the file back into
#' R and re-run this function, TADA_DefineCriteriaMethodology, again. This time,
#' use the criteriaMethods function input to specify the criteria and methodology
#' table that has already been filled out.
#'
#' @param displayUniqueId A Boolean value. If TRUE, this will print all unique
#' TADA.ComparableDataIdentifier in the criteria and methods table output. If your
#' analysis needs are dependent on differing fractions or speciations, displaying
#' the unique TADA.ComparableDataIdentifier will ensure you specify the correct
#' crosswalk between ATTAINS.ParameterName that each individual
#' TADA.ComparableDataIdentifier groups to in your TADA data frame. This is
#' useful in the alternative options to generate the criteria and methods table
#' without the reference tables.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "CriteriaMethodology.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value. If overwrite = TRUE, the excel file will be
#' replaced (overwritten) by the new file you create if you re-run this function.
#' Users should only specify overwrite = TRUE once they are ready to re-run this
#' function if they have already ran it once.
#'
#' @return A list containing three data frames with the (1) criteria and methodology table in TADA format,
#' (2) the Data Dictionary table, and (3) the Allowable Values table.
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # Example 1
#' # First, generate and fill out a parameter crosswalk (see TADA_CrosswalkWQPATTAINSParam()):
#' paramRef_UT <- TADA_CrosswalkWQPATTAINSParam(
#'   Data_Nutrients_UT,
#'   auto_assign = "All",
#'   org_id = "UTAHDWQ",
#'   excel = FALSE)
#' 
#' modified.paramRef_UT <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' 
#' paramRef_UT2 <- TADA_CrosswalkWQPATTAINSParam(
#'   Data_Nutrients_UT, auto_assign = "None",
#'   paramRef = modified.paramRef_UT, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' # Optional step: Modify the ATTAINS.UseName to CST.UseName crosswalk
#' usesRef_UT <- TADA_CrosswalkCSTATTAINSUses(org_id = "UTAHDWQ", paramRef = paramRef_UT2)
#' 
#' modified.useRef_UT <- dplyr::filter(
#'   usesRef_UT,
#'     USE_CLASS_NAME_LOCATION_ETC == "AQUATIC WILDLIFE; CLASSES 3A, 3B, 3C, 3D" &
#'       (ATTAINS.UseName == "AQUATIC WILDLIFE (COLD WATER)" |
#'       ATTAINS.UseName == "AQUATIC WILDLIFE (WARM WATER)") |
#'     (USE_CLASS_NAME_LOCATION_ETC == "DOMESTIC SOURCE, CLASS 1C" &
#'       ATTAINS.UseName == "DOMESTIC SOURCE"))
#' 
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_CrosswalkCSTATTAINSUses():
#' paramUsesRef_UT <- TADA_CreateParamUseRef(
#'   Data_Nutrients_UT, usesRef = modified.useRef_UT,
#'   paramRef = paramRef_UT2, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Now, run TADA_MLSummary()
#' MLSummaryRef_UT <- TADA_MLSummary(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   AU_UsesRef = NULL, AUMLRef = NULL,
#'   usesRef = usesRef_UT,
#'   excel = FALSE
#' )
#'
#' DefineCriteriaMethodology_UT <- TADA_DefineCriteriaMethodology(
#'   Data_Nutrients_UT,
#'   MLSummaryRef = MLSummaryRef_UT,
#'   org_id = "UTAHDWQ",
#'   displayUniqueId = TRUE,
#'   excel = FALSE
#' )
#'
#' # Example 2: fill template with EPA304(a) criteria
#' epa_only <- TADA_DefineCriteriaMethodology(
#'   Data_MT_MissoulaCounty,
#'   org_id = "USEPA",
#'   auto_assign = TRUE
#' )
#'
#' # Example 3: fill template with EPA304(a)
#' # and ATTAINS parameters and uses for MTDEQ:
#' epa_MT <- TADA_DefineCriteriaMethodology(Data_MT_MissoulaCounty,
#'   org_id = c("USEPA", "MTDEQ"), auto_assign = TRUE
#' )
#'
TADA_DefineCriteriaMethodology <- function(
    .data,
    org_id = NULL,
    MLSummaryRef = NULL,
    criteriaMethods = NULL,
    auto_assign = FALSE,
    AUMLRef = NULL,
    AU_UsesRef = NULL,
    displayUniqueId = FALSE,
    excel = FALSE,
    overwrite = FALSE
) {
  desired_cols <- c(
    "ATTAINS.OrganizationIdentifier",
    "ATTAINS.ParameterName",
    "ATTAINS.UseName",
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "ATTAINS.WaterType",
    "SaltFresh",
    "DepthCategory",
    "UniqueSpatialCriteria",
    "AcuteChronic",
    "EquationBased",
    "MagnitudeValueLower",
    "MagnitudeValueUpper",
    "MagnitudeUnit",
    "DurationValue",
    "DurationUnit",
    "DurationMethod",
    "FreqValue",
    "FreqMethod",
    "AssessPeriodStartDate",
    "AssessPeriodEndDate",
    "SeasonStartDate",
    "SeasonEndDate",
    "DistrCount",
    "DistrPeriod",
    "DistrMinSample",
    "Notes",
    "Links",
    "EquationType",
    "EquationFormula",
    "pHThreshold",
    "pHDirection",
    "hardness_param_1",
    "hardness_param_2",
    "hardness_param_3",
    "hardness_param_4",
    "AmmoniaEqType",
    "pH_param_1",
    "pH_param_2",
    "pH_param_3",
    "pH_param_4",
    "pH_param_5",
    "pH_param_6",
    "pH_param_7",
    "pH_param_8",
    "pH_param_9",
    "pH_param_10",
    "pH_param_11",
    "pH_param_12",
    "MinEqMagnitude",
    "MaxEqMagnitude"
  )
  
  blank_criteria <- TADA_CorrectColType(setNames(
    data.frame(matrix(ncol = length(desired_cols), nrow = 0)),
    desired_cols
  ))
  
  if (
    missing(.data) &&
    is.null(MLSummaryRef) &&
    is.null(criteriaMethods) &&
    is.null(AUMLRef) &&
    is.null(AU_UsesRef)
  ) {
    message("All arguments are blank, returning an empty dataframe with column names only.")
    return(blank_criteria)
  }
  
  if (!is.logical(auto_assign)) {
    stop("TADA_DefineCriteriaMethodology: auto_assign must be TRUE/FALSE.")
  }
  
  if (is.null(org_id)) {
    org_id <- character(0)
  }
  
  # ------------------------------------------------------------
  # 1) Build blank criteria from unique comparable identifiers
  # ------------------------------------------------------------
  unique_comparable <- dplyr::select(.data, TADA.ComparableDataIdentifier) |>
    dplyr::distinct()
  
  criteria_TADA <- blank_criteria |>
    dplyr::bind_rows(unique_comparable)
  
  # ------------------------------------------------------------
  # 2) If auto_assign = TRUE, build MLSummaryRef with CST-based summary
  # ------------------------------------------------------------
  if (isTRUE(auto_assign)) {
    # if no MLSummaryRef is provided, create it with defaults
    if (is.null(MLSummaryRef)) {
      message(paste0(
        "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected but no MLSummaryRef. Generating TADA_MLSummary with default assignment."
      ))
      
      # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's data frame.
      TADA_param <- dplyr::distinct(.data[,
                                          c("TADA.ComparableDataIdentifier"),
                                          drop = FALSE
      ]) |>
        dplyr::mutate(ATTAINS.OrganizationIdentifier = NA_character_) |>
        tidyr::complete(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier = org_id
        ) |>
        dplyr::filter(!is.na(ATTAINS.OrganizationIdentifier))
      
      # default, runs all reference tables with no user edits
      message(paste0(
        "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_CrosswalkWQPATTAINSParam with default assignment."
      ))
      suppressMessages(
        TADA_ParamRef <- TADA_CrosswalkCSTWQPATTAINSParam(
          .data,
          org_id = org_id,
          auto_assign = "Org", # auto-populate any exact matches found between WQP CharacteristicName and ATTAINS ParameterName
          excel = excel,
          overwrite = overwrite # Changed to FALSE when auto_assign = T KW 4/17/26
        )
      )
      
      message(paste0(
        "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_CrosswalkCSTATTAINSUses with default assignment."
      ))
      suppressWarnings(
        TADA_usesRef <- TADA_CrosswalkCSTATTAINSUses(
          org_id = org_id,
          paramRef = TADA_ParamRef,
          excel = excel,
          overwrite = FALSE # Changed to FALSE when auto_assign = T KW 4/17/26
        )
      )
      
      suppressWarnings(
        TADA_usesParamRef <- TADA_CreateParamUseRef(
          .data,
          org_id = org_id,
          paramRef = TADA_ParamRef,
          excel = excel,
          overwrite = FALSE # Changed to FALSE when auto_assign = T KW 4/17/26
        )
      )
      
      suppressMessages(
        MLSummaryRef <- TADA_MLSummary(
          .data,
          org_id = org_id,
          paramUseRef = TADA_usesParamRef,
          AUMLRef = AUMLRef,
          AU_UsesRef = AU_UsesRef,
          excel = excel,
          overwrite = FALSE # Changed to FALSE when auto_assign = T KW 4/17/26
        )
      )
      
      # correct column types for any empty columns
      MLSummaryRef <- TADA_CorrectColType(MLSummaryRef)
      # Will include all unique TADA Char/ComparableDataIdentifier to be shown in the criteria table
      MLSummaryRef <- TADA_param |>
        dplyr::full_join(MLSummaryRef, by = names(TADA_param))
    }
  }
  # ------------------------------------------------------------
  # 3) If MLSummaryRef contains CST.PollutantName (from user supplied table or auto_assign)
  # ------------------------------------------------------------    
  if (!is.null(MLSummaryRef) && "CST.PollutantName" %in% names(MLSummaryRef)) {
    ATTAINSOrgToCSTEntityRef <- utils::read.csv(
      system.file("extdata", "ATTAINSOrgToCSTEntityRef.csv", package = "EPATADA"),
      stringsAsFactors = FALSE
    )
    
    internal_path <- system.file("extdata", "cst-workbook.xlsx", package = "EPATADA")
    if (!nzchar(internal_path) || !file.exists(internal_path)) {
      stop(
        "Internal CST workbook is missing: inst/extdata/cst-workbook.xlsx. ",
        "Please add this file to the EPATADA package."
      )
    }
    
    CriteriaSearchToolRef <- .tada_cst_read_sheet(internal_path, target = "criteria")
    CriteriaSearchToolRef_Legend <- .tada_cst_read_sheet(internal_path, target = "legend")
    CriteriaSearchToolRef_Sources <- .tada_cst_read_sheet(internal_path, target = "sources")
    
    if (is.null(CriteriaSearchToolRef)) {
      stop("Failed to read 'Criteria' sheet from internal CST workbook at: ", internal_path)
    }
    
    CriteriaSearchToolRef <- .tada_cst_prepare_table(CriteriaSearchToolRef)
    CriteriaSearchToolRef_Legend <- .tada_cst_prepare_table(CriteriaSearchToolRef_Legend)
    CriteriaSearchToolRef_Sources <- .tada_cst_prepare_table(CriteriaSearchToolRef_Sources)
    
    CriteriaSearchToolRef <- CriteriaSearchToolRef |>
      dplyr::mutate(
        UNIT_NAME = stringr::str_replace_all(UNIT_NAME, "\u00B5", "u"),
        dplyr::across(where(is.character), toupper)
      )
    
    cst_updated <- CriteriaSearchToolRef |>
      dplyr::left_join(ATTAINSOrgToCSTEntityRef) |>
      dplyr::left_join(
        CriteriaSearchToolRef_Sources,
        by = c("CRIT_SOURCE_ID"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(CST.SourceLink = paste0(SOURCE, "#page=", as.character(PDFPGNO)))|>
      tidyr::separate(
        col = CRITERION_VALUE,
        into = c("MagnitudeValueLower", "MagnitudeValueUpper"),
        sep = "\\s*-\\s*",
        fill = "left",
        convert = TRUE,
        extra = "drop"
      ) |>
      dplyr::mutate(
        SaltFresh = CRITERIATYPEFRESHSALTWATER,
        AcuteChronic = CRITERIATYPE_ACUTECHRONIC,
        MagnitudeUnit = UNIT_NAME,
        CST.PollutantName = toupper(POLLUTANT_NAME),
        CST.UseName = toupper(USE_CLASS_NAME_LOCATION_ETC)
      )
    
    # This is the updated summary used downstream
    MLSummaryRef <- MLSummaryRef |>
      dplyr::left_join(
        cst_updated,
        by = c(
          "CST.PollutantName",
          "CST.UseName",
          "ATTAINS.OrganizationIdentifier"
        )
      )
  }
  
  # ------------------------------------------------------------
  # 4) If MLSummaryRef is supplied, use it
  # ------------------------------------------------------------
  if (!is.null(MLSummaryRef)) {
    criteria_TADA <- dplyr::filter(MLSummaryRef, toupper(IncludeOrExclude) == "INCLUDE")
    
    if (!is.data.frame(MLSummaryRef)) {
      stop("TADA_DefineCriteriaMethodology: MLSummaryRef must be a data frame.")
    }
    
    required_cols <- c(
      "TADA.MonitoringLocationIdentifier",
      "TADA.MonitoringLocationTypeName",  
      "TADA.LongitudeMeasure",
      "TADA.LatitudeMeasure",             
      "TADA.ComparableDataIdentifier",
      "ATTAINS.OrganizationIdentifier",   
      "ATTAINS.ParameterName",
      "ATTAINS.UseName",                  
      "IncludeOrExclude",
      "UniqueSpatialCriteria",            
      "Flag.UseInput" 
    )
    missing_cols <- setdiff(required_cols, names(MLSummaryRef))
    if (length(missing_cols) > 0) {
      stop(
        "TADA_DefineCriteriaMethodology: MLSummaryRef is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    
    # ensure optional spatial columns exist
    optional_cols <- c("ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria")
    for (col in optional_cols) {
      if (!col %in% names(MLSummaryRef)) {
        MLSummaryRef[[col]] <- NA_character_
      }
    }
    
    DefineCriteriaMethodology <- MLSummaryRef |>
      TADA_CorrectColType() |>
      dplyr::bind_rows(blank_criteria) |>
      #dplyr::left_join(criteria_TADA) |>
      dplyr::select(dplyr::any_of(c(
        desired_cols, "CST.PollutantName", "CST.UseName", "CST.SourceLink"))
        ) |>
      dplyr::distinct()
    
  } else {
    DefineCriteriaMethodology <- criteria_TADA
  }
  
  # ------------------------------------------------------------
  # 5) If criteriaMethods is supplied, merge it in and prioritize
  # ------------------------------------------------------------
  if (!is.null(criteriaMethods)) {
    if(!is.null(MLSummaryRef)) {
      stop("only one of MLSummaryRef or criteriaMethods is allowed as an input.")
    }
    
    if (!"TADA.ComparableDataIdentifier" %in% names(criteriaMethods)) {
      criteriaMethods$TADA.ComparableDataIdentifier <- NA_character_
    }
    
    missing_cols <- setdiff(desired_cols, names(criteriaMethods))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        criteriaMethods[[col]] <- NA
      }
    }
    
    criteriaMethods <- criteriaMethods |>
      TADA_CorrectColType() |>
      dplyr::select(dplyr::any_of(c(
        desired_cols, "CST.PollutantName", "CST.UseName", "CST.SourceLink"))
      ) |>
      dplyr::distinct()
    
    DefineCriteriaMethodology <- DefineCriteriaMethodology |>
      dplyr::full_join(criteriaMethods, by = desired_cols) |>
      dplyr::distinct()
  }
  
  # ------------------------------------------------------------
  # 6) Final cleanup
  # ------------------------------------------------------------
  DefineCriteriaMethodology <- DefineCriteriaMethodology |>
    dplyr::select(dplyr::any_of(c(
      desired_cols, "CST.PollutantName", "CST.UseName", "CST.SourceLink"))
    ) |>
    TADA_CorrectColType()
  
  # ------------------------------------------------------------
  # 7) Excel export
  # ------------------------------------------------------------
  if (isTRUE(excel)) {
    downloads_path <- invisible(.get_downloads_path("CriteriaMethodology.xlsx"))
    wb <- openxlsx::createWorkbook()
    
    openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
    openxlsx::addWorksheet(wb, "Index-Criteria")
    
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    
    openxlsx::writeData(
      wb,
      "DefineCriteriaMethodology",
      x = DefineCriteriaMethodology,
      startCol = 1,
      headerStyle = header_st
    )
    
    openxlsx::setColWidths(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 1:ncol(DefineCriteriaMethodology),
      widths = "auto"
    )
    
    openxlsx::freezePane(wb, "DefineCriteriaMethodology", firstActiveRow = 2, firstActiveCol = 4)
    
    # Save workbook
    save_path <- downloads_path
    if (!isTRUE(overwrite) && file.exists(downloads_path)) {
      base <- tools::file_path_sans_ext(downloads_path)
      ext <- tools::file_ext(downloads_path)
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      save_path <- sprintf("%s_%s.%s", base, ts, ext)
    }
    
    openxlsx::saveWorkbook(wb, save_path, overwrite = TRUE)
    .TADA_CriteriaDataDictionary(save_path)
    message("Saved as: ", normalizePath(save_path))
  }
  
  # Return supporting tables too
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_xlsx), add = TRUE)
  .TADA_CriteriaDataDictionary(tmp_xlsx)
  
  DataDictionary <- openxlsx::read.xlsx(tmp_xlsx, sheet = "DataDictionary", startRow = 2)
  AllowableValues <- openxlsx::read.xlsx(tmp_xlsx, sheet = "AllowableValues", startRow = 2)
  
  list(
    DefineCriteriaMethodology = DefineCriteriaMethodology,
    DataDictionary = DataDictionary,
    AllowableValues = AllowableValues
  )
}

#' Data Dictionary for Criteria and Methodology Workbook
#'
#' Create or refresh documentation tabs for the Criteria and Methodology workbook
#' used by TADA. This helper builds two worksheets:
#'   - DataDictionary: human-readable definitions for each column in the
#'     Criteria/Methodology template (name, requirement, source, type, description).
#'   - AllowableValues: curated domain references and example values for each column,
#'     including labeled hyperlinks to EPA ATTAINS domain values and WQX Characteristics.
#'
#' The function is primarily called by TADA_DefineCriteriaMethodology() to
#' ensure the workbook includes up-to-date guidance for users who fill out criteria,
#' methodology, and (optionally) equation parameterization.
#'
#' If the target Excel file does not exist, a new workbook is created at that path
#' with base sheets "DefineCriteriaMethodology" and hidden "Index-Criteria", then the
#' two documentation tabs are added (or replaced if already present).
#'
#' @param downloads_path Character string path to the Excel workbook to update
#'   (e.g., "CriteriaMethodology.xlsx"). If NULL (default), the function
#'   attempts to locate the user's Downloads folder.
#'
#' @return No return value; called for its side effects of creating or updating
#'   an Excel workbook in the downloads_path. The function writes or refreshes:
#'   - "DataDictionary" worksheet with columns:
#'     ColumnName, Requirement, Source, ColumnType, Description.
#'   - "AllowableValues" worksheet with columns:
#'     ColumnName, ColumnType, AllowableValues, ExampleValues.
#'
#' @seealso [TADA_DefineCriteriaMethodology()] [TADA_CrosswalkWQPATTAINSParam()]
#' @keywords internal
#'
#' @examples
#' # Example 1: Write to a temporary path (recommended for reproducible scripts/tests)
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   tmp_xlsx <- file.path(tempdir(), "CriteriaMethodology.xlsx")
#'   # Calling the internal function is possible within EPATADA package via :::,
#'   # but generally discouraged for users. Kept here only for demonstration.
#'   EPATADA:::.TADA_CriteriaDataDictionary(tmp_xlsx)
#'   openxlsx::getSheetNames(tmp_xlsx)
#' }
#'
#' # Example 2: Use the default Downloads location (may vary by OS/user)
#' # \dontrun{
#' # .TADA_CriteriaDataDictionary()
#' # }
#'
.TADA_CriteriaDataDictionary <- function(downloads_path = NULL) {
  if (is.null(downloads_path)) {
    # get downloads path
    downloads_path <- .get_downloads_path("CriteriaMethodology.xlsx")
  }

  if (!file.exists(downloads_path)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
    openxlsx::addWorksheet(wb, "Index-Criteria", visible = FALSE)
    openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)
  }
  wb <- openxlsx::loadWorkbook(downloads_path)

  tryCatch(openxlsx::addWorksheet(wb, "DataDictionary"), error = function(e) {
    openxlsx::removeWorksheet(wb, "DataDictionary")
    openxlsx::addWorksheet(wb, "DataDictionary")
  })

  tryCatch(openxlsx::addWorksheet(wb, "AllowableValues"), error = function(e) {
    openxlsx::removeWorksheet(wb, "AllowableValues")
    openxlsx::addWorksheet(wb, "AllowableValues")
  })

  # Example data frame
  data_to_write <- data.frame(
    ColumnName = c(
      "ATTAINS.OrganizationIdentifier",
      "ATTAINS.ParameterName",
      "ATTAINS.UseName",
      "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName",
      "ATTAINS.WaterType",
      "SaltFresh",
      "DepthCategory",
      "UniqueSpatialCriteria",
      "AcuteChronic",
      "EquationBased",
      "MagnitudeValueLower",
      "MagnitudeValueUpper",
      "MagnitudeUnit",
      "DurationValue",
      "DurationUnit",
      "DurationMethod",
      "FreqValue",
      "FreqMethod",
      "AssessPeriodStartDate",
      "AssessPeriodEndDate",
      "SeasonStartDate",
      "SeasonEndDate",
      "DistrCount",
      "DistrPeriod",
      "DistrMinSample",
      "Notes",
      "Links",
      "EquationType",
      "EquationFormula",
      "pHThreshold",
      "pHDirection",
      "hardness_param_1",
      "hardness_param_2",
      "hardness_param_3",
      "hardness_param_4",
      "AmmoniaEqType",
      "pH_param_1",
      "pH_param_2",
      "pH_param_3",
      "pH_param_4",
      "pH_param_5",
      "pH_param_6",
      "pH_param_7",
      "pH_param_8",
      "pH_param_9",
      "pH_param_10",
      "pH_param_11",
      "pH_param_12",
      "MinEqMagnitude",
      "MaxEqMagnitude"
    ),
    Requirement = c(
      "Required",
      "Required",
      "Required",
      "Recommended",
      "Required",
      "Recommended",
      "Recommended",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Required",
      "Required",
      "Required",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional",
      "Optional"
    ),
    Source = c(
      "ATTAINS*",
      "ATTAINS*",
      "ATTAINS*",
      "TADA",
      "TADA",
      "TADA",
      "TADA",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied",
      "User Supplied"
    ),
    ColumnType = c(
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Spatial",
      "Spatial",
      "Spatial",
      "Spatial",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation"
    ),
    Description = c(
      # ATTAINS.OrganizationIdentifier
      "The id of your organization that gets submitted to ATTAINS.",
      # ATTAINS.ParameterName
      "The name of the parameter that gets submitted to ATTAINS. These do not need to be unique to your organization.",
      # ATTAINS.UseName
      "The name of the waterbody use that gets submitted to ATTAINS. These use names should be specific to your organization.",
      # TADA.ComparableDataIdentifier
      paste0(
        c(
          "To populate this field, specify displayUniqueId = TRUE as an input into TADA_DefineCriteriaMethodology function. ",
          "Concatenates the WQP Characteristic, Fraction and speciation into one string. ",
          "If provided, this will crosswalk an ATTAINS.ParameterName to this TADA.ComparableDataIdentifier. ",
          "It is recommended to have performed this crosswalk in TADA_CrosswalkWQPATTAINSParam to avoid any duplicated ",
          "definition of your organization's criteria if they are the same for multiple TADA.ComparableDataIdentifiers."
        ),
        collapse = " "
      ),
      # TADA.CharacteristicName
      "Name of TADA characteristic in the WQP that gets matched to an ATTAINS parameter.",
      # TADA.ResultSampleFractionText
      "If TADA.ComparableDataIdentifier is blank, this will group all TADA.CharacteristicName to an ATTAINS.ParameterName on the condition of the specified fraction type.",
      # TADA.MethodSpeciationName
      "If TADA.ComparableDataIdentifier is blank, this will group all TADA.CharacteristicName to an ATTAINS.ParameterName on the condition of the specified speciation.",
      # ATTAINS.WaterType
      "The name of the waterbody type associated with an Assessment Unit from the ATTAINS domain value. These values will only be available if a sites-to-ATTAINS Assessment Units crosswalk is provided.",
      # SaltFresh
      "The salt or freshwater classification of the ATTAINS Waterbody Type. Users should specify if a standard only applies to salt or freshwater types.",
      # DepthCategory
      "The depth within water column that a standard applies to if applicable.",
      # UniqueSpatialCriteria
      "Users should specify any monitoring location sites that may contain a unique spatial criteria for a parameter or use in CreateMLSummaryRef.",
      # AcuteChronic
      "If a parameter and use depends depends on differing criteria standards for acute or chronic conditions. Acute is defined as short term while chronic is long term.",
      # EquationBased
      "If your water quality standards depend on an equation calculated numeric value, the equation column should be specified as yes. Users will need to specify either a custom equation or choose from a list of common equations and define each equation parameter appropriately. NOTE: Equation handling in TADA is still in development.",
      # MagnitudeValueLower
      "The lower limit of the amount or concentration of a pollutant or parameter that is allowable in water. An excursion or exceedance occurs if a ResultValue falls below this defined lower limit for this parameter and use.",
      # MagnitudeValueUpper
      "The upper limit of the amount or concentration of a pollutant or parameter that is allowable in water. An excursion or exceedance occurs if a ResultValue falls above this defined lower limit for this parameter and use.",
      # MagnitudeUnit
      "Defines the units component of the amount or concentration of a pollutant or parameter that is allowable in water.",
      # DurationValue
      "The numeric value component of the length of time in which a waterbody can be exposed to a magnitude of a parameter without negatively impacting its designated use.",
      # DurationUnit
      "The units component of the length of time in which a waterbody can be exposed to a magnitude of a parameter without negatively impacting its designated use.",
      # DurationMethod
      "The specific aggregation calculation of samples that are collected during a duration period.",
      # FreqValue
      "The numeric value of how often a magnitude value can be exceeded before being considered impaired.",
      # FreqMethod
      "How often a magnitude value can be exceeded percentage or number of times a magnitude value can be exceeded over a specified duration period.",
      # AssessPeriodStartDate
      "The start date in which WQP data will be analyzed for this parameter and use.",
      # AssessPeriodEndDate
      "The end date in which WQP data will be analyzed for this parameter and use.",
      # SeasonStartDate
      "The start date of the season in which assessments are done for during a calendar year (ex. Apr 1).",
      # SeasonEndDate
      "The end date of the season in which assessments are done for during a calendar year (ex. Sep 30).",
      # DistrCount
      "A numeric value specifying the minimum number of sampling events (consecutive) over a distribution period.",
      # DistrPeriod
      "The period of time in which samples must be collected during an assessment data window.",
      # DistrMinSample
      "How many samples must be collected during each specified DistrPeriod",
      # Notes
      "Additonal free form notes column for any notes that must be considered for this parameter and use that may not be able to be captured in the TADA criteria table format.",
      #Links
      "Link(s) to relevant WQS document for others who may wish to review the assessment criteria and methods.",
      # EquationType
      "What parameters are dependent for the equation. NOTE: Equation handling in TADA is still in development.",
      # Equation
      "Magnitude equation typed out. NOTE: Equation handling in TADA is still in development.",
      #pHThreshold
      "For pH and Hardness equations only. PH threshold at which the hardness-dependent equation changes.",
      #pHDirection
      "Whether the equation is applied for pH values above or below the pHThreshold value",
      # hardness_param_1
      paste0(
        c(
          "First coefficient in the conversion factor in a typical hardness-dependent equation format: ",
          "CF*e^(param_4(ln(hardness)) + param_5); CF = param_1 - ln(hardness)*param_2. ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # hardness_param_2
      paste0(
        c(
          "Second coefficient in the conversion factor in a typical hardness-dependent equation format: ",
          "CF*e^(param_4(ln(hardness)) + param_5); CF = param_1 - ln(hardness)*param_2. ",
          "
             NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # hardness_param_3
      paste0(
        c(
          "First coefficient in the main chunk of a typical hardness-dependent equation format: ",
          "CF*e^(param_3(ln(hardness)) + param_4); CF = param_1 - ln(hardness)*param_2. ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # hardness_param_4
      paste0(
        c(
          "Second coefficient in the main chunk of a typical hardness-dependent equation format: ",
          "CF*e^(param_3(ln(hardness)) + param_4); CF = param_1 - ln(hardness)*param_2. ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      #AmmoniaEqType
      "For pH and Temperature equations only (Ammonia equations). Specifies if the equation format should follow the Min Multiplier, Max Exponent, or Overall Min format.",
      # pH_param_1
      paste0(
        c(
          "First coefficient in the typical pH-dependent equation format: ",
          "param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_2
      paste0(
        c(
          "Second coefficient in the typical pH-dependent equation format: ",
          "param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_3
      paste0(
        c(
          "Third coefficient in the typical pH-dependent equation format: ",
          "param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_4
      paste0(
        c(
          "Fourth coefficient in the typical pH-dependent equation format: ",
          "param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_5
      paste0(
        c(
          "Fifth coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_6
      paste0(
        c(
          "Sixth coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_7
      paste0(
        c(
          "Seventh coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_8
      paste0(
        c(
          "Eigth coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_9
      paste0(
        c(
          "Ninth coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_10
      paste0(
        c(
          "Tenth coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_11
      paste0(
        c(
          "Eleventh coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_12
      paste0(
        c(
          "Twelfth coefficient in the typical pH- & temperature-dependent equation format: ",
          "Overall Min: min(param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH)), param_5*[param_6/(1+10^(param_7-pH)) + param_8/(1+10^(param_9-pH))]*(param_10*10^(param_11*(param_12-Temperature))))",
          "OR ",
          "Min Multiplier: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "Max Exponent: param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # MinEqMagnitude
      "Numeric value that represents a minimum value that should replace a calculated value that falls below this.",
      # MaxEqMagnitude
      "Numeric value that represents a maximum value that should replace a calculated value that falls above this."
    )
  )

  # Write the data frame to the worksheet, starting at cell B2
  openxlsx::writeData(
    wb,
    "DataDictionary",
    data_to_write,
    startCol = 2,
    startRow = 2
  )

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
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    header_style,
    rows = 2,
    cols = 2:(ncol(data_to_write) + 1),
    gridExpand = TRUE,
    stack = TRUE
  )
  # add blue shading to ColumnType == Crosswalk rows
  crosswalk_style <- openxlsx::createStyle(
    fgFill = "#DAEEF3", # Light blue background
  )

  crosswalk_loc <- which(data_to_write$ColumnType == "Crosswalk") + 2

  # apply Crosswalk blue shading
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    crosswalk_style,
    rows = crosswalk_loc,
    cols = 2:6,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add orange shading to ColumnType == Spatial rows
  spatial_style <- openxlsx::createStyle(
    fgFill = "#FDE9D9", # Light orange background
  )

  spatial_loc <- which(data_to_write$ColumnType == "Spatial") + 2

  # apply Spatial orange shading
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    spatial_style,
    rows = spatial_loc,
    cols = 2:6,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add green shading to ColumnType == Criteria rows
  criteria_style <- openxlsx::createStyle(
    fgFill = "#EBF1DE", # Light green background
  )

  criteria_loc <- which(data_to_write$ColumnType == "Criteria") + 2

  # apply criteria green shading
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    criteria_style,
    rows = criteria_loc,
    cols = 2:6,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add red shading to ColumnType == Methodology rows
  method_style <- openxlsx::createStyle(
    fgFill = "#F2DCDB", # Light red background
  )

  method_loc <- which(data_to_write$ColumnType == "Methodology") + 2

  # apply Methodology red shading
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    method_style,
    rows = method_loc,
    cols = 2:6,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add purple shading to ColumnType == Equation rows
  eq_style <- openxlsx::createStyle(
    fgFill = "#E9E1F2", # Light purple background
  )

  eq_loc <- which(data_to_write$ColumnType == "Equation") + 2

  # apply Methodology purple shading
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    eq_style,
    rows = eq_loc,
    cols = 2:6,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Create a style for borders on all data cells
  data_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#000000" # Light grey border
  )

  # Apply data border style to all data rows and columns besides header
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    data_border_style,
    rows = 3:(nrow(data_to_write) + 2),
    cols = 2:(ncol(data_to_write) + 1),
    gridExpand = TRUE,
    stack = TRUE
  )

  # Define description text that gets wrapped
  wrapStyle <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#000000", # Light grey border
    wrapText = TRUE
  )

  # only applies to the last column. We shifted the table to B2, adjust accordingly
  openxlsx::addStyle(
    wb,
    "DataDictionary",
    wrapStyle,
    rows = 3:(nrow(data_to_write) + 2),
    cols = ncol(data_to_write) + 1,
    stack = TRUE
  )

  openxlsx::setColWidths(
    wb,
    "DataDictionary",
    cols = ncol(data_to_write) + 1,
    widths = 80
  ) # Adjust width as needed

  # Set column widths to automatically fit content, except last column
  openxlsx::setColWidths(
    wb,
    "DataDictionary",
    cols = 1:ncol(data_to_write),
    widths = "auto"
  )

  # Build the data frame with plain URLs, not =HYPERLINK(...)
  data_to_write_allow <- data.frame(
    ColumnName = c(
      "ATTAINS.OrganizationIdentifier",
      "ATTAINS.ParameterName",
      "ATTAINS.UseName",
      "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName",
      "ATTAINS.WaterType",
      "SaltFresh",
      "DepthCategory",
      "UniqueSpatialCriteria",
      "AcuteChronic",
      "EquationBased",
      "MagnitudeValueLower",
      "MagnitudeValueUpper",
      "MagnitudeUnit",
      "DurationValue",
      "DurationUnit",
      "DurationMethod",
      "FreqValue",
      "FreqMethod",
      "AssessPeriodStartDate",
      "AssessPeriodEndDate",
      "SeasonStartDate",
      "SeasonEndDate",
      "DistrCount",
      "DistrPeriod",
      "DistrMinSample",
      "Notes",
      "Links",
      "EquationType",
      "EquationFormula",
      "pHThreshold",
      "pHDirection",
      "hardness_param_1",
      "hardness_param_2",
      "hardness_param_3",
      "hardness_param_4",
      "AmmoniaEqType",
      "pH_param_1",
      "pH_param_2",
      "pH_param_3",
      "pH_param_4",
      "pH_param_5",
      "pH_param_6",
      "pH_param_7",
      "pH_param_8",
      "pH_param_9",
      "pH_param_10",
      "pH_param_11",
      "pH_param_12",
      "MinEqMagnitude",
      "MaxEqMagnitude"
    ),
    ColumnType = c(
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Crosswalk",
      "Spatial",
      "Spatial",
      "Spatial",
      "Spatial",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Criteria",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation",
      "Equation"
    ),
    AllowableValues = c(
      "https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx",
      "https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx",
      "https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx",
      "",
      "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV",
      "https://cdx.epa.gov/wqx/download/DomainValues/ResultSampleFraction.CSV",
      "https://cdx.epa.gov/wqx/download/DomainValues/MethodSpeciation.CSV",
      "https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx",
      "S; F; NA",
      "No depth info; Epilimnion-surface; Surface; Bottom; Middle",
      "NA",
      "A; C; NA",
      "Yes; No; NA",
      "",
      "",
      "",
      "",
      "n-hour; n-day; n-week; n-month; n-quarter",
      "arithmetic mean; arithmetic median; arithmetic max; arithmetic min; arithmetic extremes; geometric mean; rolling geometric mean; rolling arithmetic mean; mean of daily minima; mean of daily maxima",
      "",
      "Percent of samples not meeting; percentile; n-samples in 3 years; n-samples in 4 years; n-samples in 5 years; binomial test; NumberNotMeeting",
      "",
      "",
      "",
      "",
      "",
      "Seasonal; Annual; Semi-Annual; Quarterly; Monthly; Bi-weekly; Weekly; 10 days; NA",
      "",
      "",
      "",
      "Hardness; pH; pH and Temperature; pH and Hardness",
      "",
      "",
      "Above; Below; NA",
      "",
      "",
      "",
      "",
      "Min Multiplier, Max Exponent, Overall Min",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    ),
    ExampleValues = c(
      "21COL001",
      "DISSOLVED OXYGEN; TURBIDITY; ZINC, TOTAL; ZINC, DISSOLVED",
      "Aquatic Life; Agriculture; Domestic Water Supply; Aquatic Life Coldwater",
      "TEMPERATURE, WATER_NA_NA_DEG C; ENTEROCOCCUS_TOTAL_NA_MPN/100ML; HARDNESS, NON-CARBONATE_DISSOLVED_NA_MG/L CACO3; AMMONIA-NITROGEN_UNFILTERED, FIELD_AS N_MG/L",
      "DISSOLVED OXYGEN (DO); TURBIDITY; ZINC; CHROMIUM(III)",
      "DISSOLVED; TOTAL; TOTAL RECOVERABLE",
      "AS N; AS NH3",
      "Creek; Estuary; River; Stream",
      "",
      "",
      "",
      "",
      "",
      "5.2",
      "98.5",
      "MG/L; UG/L; NTU",
      "4",
      "",
      "",
      "10",
      "",
      "2024-10-01",
      "2025-09-30",
      "Apr 01",
      "Jul 15",
      "5",
      "",
      "10",
      "New addition to ATTAINS in FY2026",
      "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table",
      "",
      paste0(
        c(
          "Hardness: 1.101672 - ln(hardness) (0.041838) * e(0.7977*ln(hardness)-3.909) ",
          "OR ",
          "pH: 0.275/(1+10^(7.204-pH)) + 39/(1+10^(pH-7.204)) ",
          "OR ",
          "pH-Temp Acute-Fish: min(0.275/(1+10^(7.204-pH)) + 39/(1+10^(pH-7.204)), 0.7249*(((0.0114/(1+10^(7.204-pH)))+(1.6181/(1+10^(pH-7.204))))*51.93,23.12*10^(0.036*(20-Temperature))))",
          "OR ",
          "pH-Temp Chronic: 0.8876*(((0.0278/(1+10^(7.688-pH)))+(1.1994/(1+10^(pH-7.688))))*(2.126*10^(0.028*(20-max(Temperature,7)))))",
          "OR ",
          "pH-Temp Acute-NonFish: 0.7249*(((0.0114/(1+10^(7.204-pH)))+(1.6181/(1+10^(pH-7.204))))*min(51.93,23.12*10^(0.036*(20-Temperature))))",
          "OR ",
          "pH-Hardess: pH above 7; e^(1.3695*ln(hardness)-0.1158); pH below 7; min(87, e^(1.3695*ln(hardness)-0.1158))"
        ),
        collapse = "\r\n"
      ),
      "7",
      "",
      "1.101672",
      "0.041838",
      "0.7977",
      "-3.909",
      "",
      "0.275",
      "7.204",
      "39",
      "0.8876",
      "7.204",
      "2.216",
      "0.028",
      "20",
      "7",
      "23.12",
      "0.036",
      "20",
      "87",
      "900"
    ),
    stringsAsFactors = FALSE
  )

  start_col <- 2
  start_row <- 2

  openxlsx::writeData(
    wb,
    "AllowableValues",
    data_to_write_allow,
    startCol = start_col,
    startRow = start_row
  )

  # Excel column number for AllowableValues
  allowable_col_excel <- start_col +
    which(names(data_to_write_allow) == "AllowableValues") -
    1

  # Identify rows with URLs
  link_rows <- which(grepl("^https?://", data_to_write_allow$AllowableValues))

  # Create simple labels
  link_labels <- ifelse(
    grepl("domains_.*\\.xlsx$", data_to_write_allow$AllowableValues),
    "EPA ATTAINS",
    ifelse(
      grepl("Characteristic\\.CSV$", data_to_write_allow$AllowableValues),
      "WQX Characteristics",
      ifelse(
        grepl(
          "ResultSampleFraction\\.CSV$",
          data_to_write_allow$AllowableValues
        ),
        "WQX ResultSampleFraction",
        ifelse(
          grepl("MethodSpeciation\\.CSV$", data_to_write_allow$AllowableValues),
          "WQX MethodSpeciation",
          "Link"
        )
      )
    )
  )

  # Build formulas only for hyperlink rows
  hyperlink_formulas <- paste0(
    'HYPERLINK("',
    data_to_write_allow$AllowableValues[link_rows],
    '", "',
    link_labels[link_rows],
    '")'
  )

  # Write each hyperlink formula into its specific row
  for (i in seq_along(link_rows)) {
    openxlsx::writeFormula(
      wb,
      sheet = "AllowableValues",
      x = hyperlink_formulas[i],
      startCol = allowable_col_excel,
      startRow = start_row + link_rows[i]
    )
  }

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
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    header_style,
    rows = 2,
    cols = 2:(ncol(data_to_write_allow) + 1),
    gridExpand = TRUE,
    stack = TRUE
  )
  # add blue shading to ColumnType == Crosswalk rows
  crosswalk_style <- openxlsx::createStyle(
    fgFill = "#DAEEF3", # Light blue background
  )

  crosswalk_loc <- which(data_to_write_allow$ColumnType == "Crosswalk") + 2

  # apply Crosswalk blue shading
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    crosswalk_style,
    rows = crosswalk_loc,
    cols = 2:5,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add orange shading to ColumnType == Spatial rows
  spatial_style <- openxlsx::createStyle(
    fgFill = "#FDE9D9", # Light orange background
  )

  spatial_loc <- which(data_to_write_allow$ColumnType == "Spatial") + 2

  # apply Spatial orange shading
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    spatial_style,
    rows = spatial_loc,
    cols = 2:5,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add green shading to ColumnType == Criteria rows
  criteria_style <- openxlsx::createStyle(
    fgFill = "#EBF1DE", # Light green background
  )

  criteria_loc <- which(data_to_write_allow$ColumnType == "Criteria") + 2

  # apply criteria green shading
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    criteria_style,
    rows = criteria_loc,
    cols = 2:5,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add red shading to ColumnType == Methodology rows
  method_style <- openxlsx::createStyle(
    fgFill = "#F2DCDB", # Light red background
  )

  method_loc <- which(data_to_write_allow$ColumnType == "Methodology") + 2

  # apply Methodology red shading
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    method_style,
    rows = method_loc,
    cols = 2:5,
    gridExpand = TRUE,
    stack = TRUE
  )

  # add purple shading to ColumnType == Equation rows
  eq_style <- openxlsx::createStyle(
    fgFill = "#E9E1F2", # Light purple background
  )

  eq_loc <- which(data_to_write_allow$ColumnType == "Equation") + 2

  # apply Methodology purple shading
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    eq_style,
    rows = eq_loc,
    cols = 2:5,
    gridExpand = TRUE,
    stack = TRUE
  )
  # Create a style for borders on all data cells
  data_border_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#000000" # Light grey border
  )

  # Apply data border style to all data rows and columns besides header
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    data_border_style,
    rows = 3:(nrow(data_to_write_allow) + 2),
    cols = 2:(ncol(data_to_write_allow) + 1),
    gridExpand = TRUE,
    stack = TRUE
  )

  # Define description text that gets wrapped
  wrapStyle <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#000000", # Light grey border
    wrapText = TRUE
  )

  # only applies to the last column. We shifted the table to B2, adjust accordingly
  openxlsx::addStyle(
    wb,
    "AllowableValues",
    wrapStyle,
    rows = 3:(nrow(data_to_write_allow) + 2),
    cols = ncol(data_to_write_allow) + 1,
    stack = TRUE
  )

  openxlsx::setColWidths(
    wb,
    "AllowableValues",
    cols = ncol(data_to_write_allow):(ncol(data_to_write_allow) + 1),
    widths = 80
  ) # Adjust width as needed

  # Set column widths to automatically fit content, except last column
  openxlsx::setColWidths(
    wb,
    "AllowableValues",
    cols = 1:(ncol(data_to_write_allow) - 1),
    widths = "auto"
  )

  # Save the workbook to an Excel file
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)
}
