#' Criteria and Methodology Template
#'
#' Assessment criteria and methodologies used to evaluate water quality vary
#' across the country. TADA users can fill out this template to define the specific
#' criteria and methodologies for each parameter and use combination they are
#' interested in analyzing. This table can be filled out manually, auto-populated
#' with uses and parameters from ATTAINS and the input WQP dataframe, or
#' developed with TADA helper functions (recommended). It is recommended to run
#' these three TADA helper functions, [TADA_ParametersForAnalysis()],
#' [TADA_UsesForAnalysis], and [TADA_MLSummary], in that order to
#' generate the Criteria and Methodology table specific for your organization.
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
#' ATTAINS.uses = rExpertQuery::EQ_DomainValues("use_name")
#' ATTAINS.parameters <- rExpertQuery::EQ_DomainValues("param_name")
#' ATTAINS.organizations <- rExpertQuery::EQ_DomainValues("org_id")
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
#' edited. Enter `rExpertQuery::EQ_DomainValues("org_id")` into the console to
#' get a list of valid organization identifiers. A list of organization identifiers
#' can also be found by downloading the ATTAINS Domains Excel file:
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
#'
#' @param AU_UsesRef An optional data frame input. If provided, the ATTAINS.UseName
#' will be populated from the ATTAINS.UseName found in this data frame rather
#' than the ATTAINS assessment profile. This data frame must contain the following
#' column names which can be generated from the output of TADA_AssignUsesToAU:
#' ATTAINS.OrganizationIdentifier, ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName,
#' and ATTAINS.WaterType.
#'
#' @param AUMLRef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. This data frame must contain the following
#' column names which can be generated from the output of TADA_CreateAUMLCrosswalk:
#' ATTAINS.OrganizationIdentifier, TADA.MonitoringLocationIdentifier,
#' ATTAINS.AssessmentUnitIdentifier, and ATTAINS.WaterType.
#'
#' @param auto_assign Boolean argument with two possible values: TRUE and FALSE.
#' The default value is FALSE. If TRUE, a draft criteria and methods table is
#' generated using default function inputs for [TADA_ParametersForAnalysis()],
#' [TADA_UsesForAnalysis], and [TADA_MLSummary]. .data and org_id are
#' required inputs for this function if auto_assign = TRUE. It is also
#' recommended to set excel = TRUE when auto_assign = TRUE. The criteria
#' and methodology template should be reviewed carefully and edits can be
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
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value. If overwrite = TRUE, the excel file will be
#' replaced (overwritten) by the new file you create if you re-run this function.
#' Users should only specify overwrite = TRUE once they are ready to re-run this
#' function if they have already ran it once.
#'
#' @return A data frame with the criteria and methodology table in TADA format.
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # Example 1
#' # First, generate and fill out a parameter crosswalk (see TADA_ParametersForAnalysis()):
#' paramRef_UT <- TADA_ParametersForAnalysis(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_ParametersForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_UsesForAnalysis():
#' usesRef_UT <- TADA_UsesForAnalysis(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
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
  criteriaMethods = NULL, # user supplied input here
  auto_assign = FALSE, # ref = c("ATTAINS", "CST", "TADA", "Other") future development to consider additional crosswalk alternatives?
  AUMLRef = NULL,
  AU_UsesRef = NULL, # Optional if auto_assign = TRUE
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
    # Spatial Columns
    "ATTAINS.WaterType",
    "SaltFresh",
    "DepthCategory",
    "UniqueSpatialCriteria",
    # Criteria Columns
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
    # Data Sufficiency Columns
    "AssessPeriod",
    "AssessPeriodStartDate",
    "AssessPeriodEndDate",
    "Season",
    "SeasonStartDate",
    "SeasonEndDate",
    "DistrCount",
    "DistrPeriod",
    "DistrMinSample",
    "Notes"
  )

  # Return an empty data frame with column names only if a user does not define any arg inputs.
  if (
    missing(.data) &&
      missing(MLSummaryRef) &&
      missing(criteriaMethods) &&
      missing(AUMLRef) &&
      missing(AU_UsesRef)
  ) {
    # if (!"USEPA" %in% org_id) {
    #   stop("org_id can only equal NULL or 'USEPA' if all other argument inputs are left blank.")
    # }
    message(
      "All arguments are blank, returning an empty dataframe with column names only."
    )

    DefineCriteriaMethodology <- data.frame(matrix(
      ncol = length(desired_cols),
      nrow = 0
    ))

    names(DefineCriteriaMethodology) <- desired_cols

    DefineCriteriaMethodology <- TADA_CorrectColType(DefineCriteriaMethodology)
  } else {
    # Check if auto_assign is boolean
    if (!is.logical(auto_assign)) {
      stop(
        "TADA_DefineCriteriaMethodology: auto_assign must be a boolean (TRUE/FALSE) value."
      )
    }
    # # Commenting out all code related to updateRef for now. See https://github.com/USEPA/EPATADA/issues/667
    # # Ensures users have entered a valid input to updateRef
    # if (!updateRef %in% c("none", "paramRef", "usesRef", "MLSummaryRef")) {
    #   stop(paste0(
    #     "TADA_DefineCriteriaMethodology: ",
    #     "argument input ", updateRef, " is not a valid entry for updateRef. Please type one of 'None', 'paramRef', 'usesRef', 'MLSummaryRef' as a value."
    #   ))
    # }
    # # Invalid function input combos - can only use updateRef = none with auto_assign = FALSE
    # if (auto_assign == FALSE && updateRef != "none") {
    #   stop("TADA_DefineCriteriaMethodology: auto_assign = FALSE. The updateRef function input must be none. If you have updated a reference table, use auto_assign == TRUE")
    # }

    # If auto_assign = TRUE and no MLSummaryRef OR criteriaMethods arg input is provided, this results in error.
    if (auto_assign == TRUE && !is.null(criteriaMethods)) {
      stop(
        "TADA_DefineCriteriaMethodology: criteriaMethodology is provided and auto_assign = TRUE are not valid function argument input combinations."
      )
    }

    # Invalid function input combos - supply one or the other.
    if (!is.null(MLSummaryRef) && !is.null(criteriaMethods)) {
      stop(
        "TADA_DefineCriteriaMethodology: MLSummaryRef and criteriaMethods are both provided. You can only proceed with one (or none) of these options provided."
      )
    }

    # Invalid function input combos - MLSummaryRef and auto_assign = TRUE cannot be used together
    if (!is.null(MLSummaryRef) && auto_assign == TRUE) {
      stop(
        "TADA_DefineCriteriaMethodology: MLSummaryRef is provided and auto_assign = TRUE are not valid function argument input combinations."
      )
    }

    # if null, creates a list of all unique TADA.ComparableDataIdentifier, but no org populated.
    if (!is.character(org_id) & is.null(org_id)) {
      org_id <- ""
    }
    # if org_id = all, create a crosswalk for all ATTAINS org in the data frame.
    if (tolower("all") %in% tolower(org_id)) {
      # If a user selects org_id = all but doesn't provide an AUMLRef with ATTAINS organization identifier.
      if (is.null(AUMLRef)) {
        print(paste0(
          "org_id == 'All' was selected, ",
          "No AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier domain value."
        ))
        org_id <- rExpertQuery::EQ_DomainValues("org_id")[, "code"]
      }
      # If a user selects org_id = all and does provide an AUMLRef with ATTAINS organization identifier.
      if (!is.null(AUMLRef)) {
        print(paste0(
          "org_id == 'All' was selected, ",
          "An AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier in your AUMLRef."
        ))
        org_id <- unique(stats::na.omit(AUMLRef$ATTAINS.OrganizationIdentifier))
      }
    }

    # Generates a criteria table with only unique TADA.CharacteristicName(s) populated.
    if (
      auto_assign == FALSE && is.null(MLSummaryRef) && is.null(criteriaMethods)
    ) {
      suppressMessages(
        TADA_ParamRef <- TADA_ParametersForAnalysis(
          .data = .data,
          org_id = org_id,
          excel = excel,
          overwrite = overwrite
        )
      )

      suppressWarnings(
        TADA_usesRef <- TADA_UsesForAnalysis(
          .data,
          paramRef = TADA_ParamRef,
          org_id = org_id,
          excel = excel,
          overwrite = overwrite
        )
      )

      suppressMessages(
        MLSummaryRef <- TADA_MLSummary(
          .data,
          usesRef = TADA_usesRef,
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
      # commenting out all code related to updateRef for now. See https://github.com/USEPA/EPATADA/issues/667
      # if (updateRef == "none") {
      print(paste0(
        "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment."
      ))
      suppressMessages(
        TADA_ParamRef <- TADA_ParametersForAnalysis(
          .data,
          org_id = org_id,
          auto_assign = "Org", # auto-populate any exact matches found between WQP CharacteristicName and ATTAINS ParameterName
          excel = excel,
          overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
        )
      )

      print(paste0(
        "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_UsesForAnalysis with default assignment."
      ))
      suppressWarnings(
        TADA_usesRef <- TADA_UsesForAnalysis(
          .data,
          org_id = org_id,
          paramRef = TADA_ParamRef,
          AU_UsesRef = AU_UsesRef,
          AUMLRef = AUMLRef,
          auto_assign = TRUE,
          excel = excel,
          overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
        )
      )

      print(paste0(
        "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_MLSummary with default assignment."
      ))
      suppressMessages(
        MLSummaryRef <- TADA_MLSummary(
          .data,
          displayNA = TRUE,
          org_id = org_id,
          usesRef = TADA_usesRef,
          AUMLRef = AUMLRef,
          AU_UsesRef = AU_UsesRef,
          excel = excel,
          overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
        )
      )

      unique_param <- unique(.data$TADA.CharacteristicName)
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

      MLSummaryRef <- TADA_CorrectColType(MLSummaryRef)
      # Will include all unique TADA Char/ComparableDataIdentifier to be shown in the criteria table
      MLSummaryRef <- TADA_param |>
        dplyr::full_join(MLSummaryRef, by = names(TADA_param))

      # # Commenting out all code related to updateRef for now. See https://github.com/USEPA/EPATADA/issues/667
      # # user only updates paramRef. This will update paramRef, usesRef, and MLSummaryRef based on these modifications.
      # if (updateRef == "paramRef") {
      #   message(paste0("auto_assign = TRUE and updateRef = paramRef selected. Running TADA_ParametersForAnalysis with use supplied paramRef assignment. Please review this paramRef table output."))
      #   myfile_ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
      #
      #   TADA_ParamRef <- TADA_ParametersForAnalysis(
      #     .data,
      #     org_id = org_id,
      #     paramRef = myfile_ParamRef,
      #     auto_assign = "None", # User has now edited the table, turn the auto_assign of in TADA_ParametersForAnalysis
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      #
      #   TADA_usesRef <- TADA_UsesForAnalysis(
      #     .data,
      #     org_id = org_id,
      #     paramRef = TADA_ParamRef,
      #     auto_assign = TRUE,
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      #
      #   MLSummaryRef <- TADA_MLSummary(
      #     .data,
      #     org_id = org_id,
      #     usesRef = TADA_usesRef,
      #     AUMLRef = AUMLRef,
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      # }
      #
      # # user only updates usesRef. This will update usesRef, MLSummaryRef based on this modifications.
      # if (updateRef == "usesRef") {
      #   message(paste0("auto_assign = TRUE and updateRef = usesRef selected. Running TADA_ParametersForAnalysis with use supplied paramRef assignment. Please review this paramRef table output."))
      #   myfile_usesRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateusesRef")
      #
      #   TADA_ParamRef <- TADA_ParametersForAnalysis(
      #     .data,
      #     org_id = org_id,
      #     paramRef = myfile_usesRef, # will update paramRef based on usesRef
      #     auto_assign = "All",
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      #
      #   TADA_usesRef <- TADA_UsesForAnalysis(
      #     .data,
      #     org_id = org_id,
      #     paramRef = TADA_ParamRef,
      #     usesRef = myfile_usesRef,
      #     auto_assign = TRUE,
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      #
      #   MLSummaryRef <- TADA_MLSummary(
      #     .data,
      #     org_id = org_id,
      #     usesRef = TADA_usesRef,
      #     AUMLRef = AUMLRef,
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      # }
      #
      # # user only updates MLSummaryRef in excel. This will update MLSummaryRef based on this modifications.
      # if (updateRef == "MLSummaryRef") {
      #   message(paste0("auto_assign = TRUE and updateRef = MLSummaryRef selected. Running TADA_MLSummary with use supplied paramRef assignment. Please review this paramRef table output."))
      #   myfile_MLSummaryRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateMLSummaryRef")
      #
      #   TADA_ParamRef <- TADA_ParametersForAnalysis(
      #     .data,
      #     org_id = org_id,
      #     paramRef = myfile_MLSummaryRef, # will update paramRef based on usesRef
      #     auto_assign = "All",
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      #
      #   TADA_usesRef <- TADA_UsesForAnalysis(
      #     .data,
      #     org_id = org_id,
      #     paramRef = TADA_ParamRef,
      #     usesRef = myfile_MLSummaryRef,
      #     auto_assign = TRUE,
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      #
      #   MLSummaryRef <- TADA_MLSummary(
      #     .data,
      #     org_id = org_id,
      #     usesRef = TADA_usesRef,
      #     AUMLRef = AUMLRef,
      #     MLSummaryRef = myfile_MLSummaryRef,
      #     excel = excel, overwrite = overwrite # You must include overwrite = TRUE to overwrite the excel file when you first create the excel spreadsheet.
      #   )
      # }
    }

    # check to see if user-supplied MLSummary ref is a df with appropriate columns and filled out.
    if (!is.null(MLSummaryRef) & !is.character(MLSummaryRef)) {
      if (!is.data.frame(MLSummaryRef)) {
        stop(
          "TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:
          ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, UniqueSpatialCriteria,
          ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier"
        )
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
          stop(
            "TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:
          ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, UniqueSpatialCriteria,
          ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier"
          )
        }
      }
    }

    # user has went through the recommended workflow or chose autoassign = T.
    # criteria table will be generated from the MLSummaryRef file. This file also contains unique spatial criteria
    # as an option and will include these values if they have been populated.
    if (!is.null(MLSummaryRef)) {
      # corrects for data types
      MLSummaryRef <- TADA_CorrectColType(MLSummaryRef)
      # Extracts the characteristic, speciation and fraction columns to join
      MLSummaryRef <- MLSummaryRef |>
        dplyr::right_join(
          .data[, c(
            "TADA.ComparableDataIdentifier",
            "TADA.CharacteristicName"
            # "TADA.ResultSampleFractionText",
            # "TADA.MethodSpeciationName"
          )] |>
            dplyr::distinct(),
          by = "TADA.ComparableDataIdentifier"
        )

      # Creates the DefineCriteriaMethodology table from the MLSummaryRef.
      DefineCriteriaMethodology <- MLSummaryRef |>
        dplyr::select(
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName",
          "ATTAINS.UseName",
          "TADA.ComparableDataIdentifier",
          "TADA.CharacteristicName",
          "SaltFresh",
          "DepthCategory",
          "UniqueSpatialCriteria",
          "ATTAINS.WaterType"
        ) |>
        # Spatial Columns - only pre-populates if a unique spatial criteria is applied.
        dplyr::mutate(
          ATTAINS.WaterType = dplyr::if_else(
            # Only pre-populates if a unique spatial criteria is applied
            is.na(UniqueSpatialCriteria),
            as.character(NA),
            as.character(ATTAINS.WaterType)
          )
        ) |>
        dplyr::mutate(
          SaltFresh = dplyr::if_else(
            # Only pre-populates if a unique spatial criteria is applied
            is.na(UniqueSpatialCriteria),
            as.character(NA),
            as.character(SaltFresh)
          )
        ) |>
        dplyr::mutate(
          DepthCategory = dplyr::if_else(
            # Only pre-populates if a unique spatial criteria is applied
            is.na(UniqueSpatialCriteria),
            as.character(NA),
            as.character(DepthCategory)
          )
        ) |>
        # dplyr::filter(!dplyr::if_all(c(UniqueSpatialCriteria, ATTAINS.WaterType), is.na)) |>
        dplyr::bind_cols(data.frame(
          TADA.ResultSampleFractionText = as.character(NA),
          TADA.MethodSpeciationName = as.character(NA),
          AcuteChronic = as.character(NA),
          # Criteria Columns
          EquationBased = as.character(NA),
          MagnitudeValueLower = as.numeric(NA),
          MagnitudeValueUpper = as.numeric(NA),
          MagnitudeUnit = as.character(NA),
          DurationValue = as.numeric(NA),
          DurationUnit = as.character(NA),
          DurationMethod = as.character(NA),
          FreqValue = as.numeric(NA),
          FreqMethod = as.character(NA),
          # Data Sufficiency Columns
          AssessPeriod = as.character(NA),
          AssessPeriodStartDate = as.Date(NA),
          AssessPeriodEndDate = as.Date(NA),
          Season = as.character(NA),
          SeasonStartDate = as.Date(NA),
          SeasonEndDate = as.Date(NA),
          DistrCount = as.numeric(NA),
          DistrPeriod = as.character(NA),
          DistrMinSample = as.numeric(NA),
          Notes = as.character(NA)
        )) |>
        dplyr::select(
          desired_cols # defined in beginning of code
        ) |>
        dplyr::arrange(ATTAINS.UseName) |>
        tidyr::complete(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier = org_id
        ) |>
        dplyr::distinct()

      if (auto_assign == TRUE && !all(org_id == "USEPA")) {
        # currently, we will only apply joining the CST magnitudes when the org_id is known.
        if ("" %in% org_id) {
          DefineCriteriaMethodology <- DefineCriteriaMethodology
        }
        if (!"" %in% org_id) {
          # all lines below will focus on joining CST magnitude values to the auto_assign table
          # pulls in alias crosswalk between CST STD.PollutantName and ATTAINS.ParameterName
          CST_ATTAINS_Param <- TADA_AdditionalCharAliasForReview(
            displayPercent = FALSE,
            ATTAINS.WQX.tolerance = 0.75,
            WQX.ATTAINS.tolerance = 0.75,
            ATTAINS.CST.tolerance = 0.75, # can change as desired for tolerance on matches
            CST.ATTAINS.tolerance = 0.75, # can change as desired for tolerance on matches
            includeCST = TRUE
          ) |>
            dplyr::mutate(dplyr::across(where(is.character), toupper))

          # print message to indicate we are joining CST magnitudes to user criteria table, additional review is likely needed.
          message(cat(paste(
            "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected.",
            "Finding an alias match between ATTAINS parameter name and Criteria Search Tool (CST) standardized pollutant names.",
            "Finding an alias match between ATTAINS use name and Criteria Search Tool (CST) uses.",
            "If an ATTAINS.ParameterName and ATTAINS.UseName alias was found, populating these rows with the CST magnitude values.",
            "A many-to-many match is likely. User review is needed to ensure the proper parameter and uses from ATTAINS and CST alias crosswalk was accomplished (remove or add rows as needed).",
            sep = "\n"
          )))

          # pulls in uses alias table between ATTAINS.UseName and CST uses
          uses <- suppressMessages(TADA_UsesAliasForReview(
            ATTAINS.CST.tolerance = 0.15, # lower tolerance for more matches to ensure user reviews the uses crosswalks.
            CST.ATTAINS.tolerance = 0.15 # uses a lower value as CST uses can be very long.
          ))
          # filters uses crosswalk by the org_id
          uses <- uses |>
            dplyr::mutate(ATTAINS.UseName = toupper(name)) |>
            dplyr::filter(
              !is.na(ATTAINS.OrganizationIdentifier),
              ATTAINS.OrganizationIdentifier %in%
                unique(DefineCriteriaMethodology$ATTAINS.OrganizationIdentifier)
            )
          # pulls in Criteria Search Tool
          CST_Ref <- TADA_CST_GetCriteria()

          # upper case all character columns for consistency
          CST_Ref_upper <- CST_Ref |>
            dplyr::mutate(dplyr::across(where(is.character), toupper))
          # upper case all character columns for consistency
          DefineCriteriaMethodology <- DefineCriteriaMethodology |>
            dplyr::mutate(dplyr::across(where(is.character), toupper))

          # join the parameter and pollutant names from ATTAINS and CST
          DefineCriteriaMethodology2 <- DefineCriteriaMethodology |>
            dplyr::left_join(
              CST_ATTAINS_Param,
              by = c("TADA.CharacteristicName" = "CharacteristicName"),
              relationship = "many-to-many"
            ) |>
            dplyr::mutate(
              ATTAINS.UseName = toupper(ATTAINS.UseName),
              ATTAINS.ParameterName = ATTAINS.ParameterName.x
            ) |>
            # Now join by ATTAINS uses and CST uses
            dplyr::left_join(
              uses,
              c("ATTAINS.UseName", "ATTAINS.OrganizationIdentifier"),
              relationship = "many-to-many"
            ) |>
            dplyr::mutate(dplyr::across(where(is.character), toupper)) |>
            # Now, pull in the magnitude value if the CST pollutant name and uses are matched
            dplyr::left_join(
              CST_Ref_upper,
              by = dplyr::join_by(
                POLLUTANT_NAME,
                STD_POLLUTANT_NAME,
                ENTITY_ABBR,
                ENTITY_NAME,
                CRITERIATYPEAQUAHUMHLTH,
                CRITERIATYPEFRESHSALTWATER,
                CRITERIATYPE_ACUTECHRONIC,
                USE_CLASS_NAME_LOCATION_ETC
              ),
              relationship = "many-to-many"
            ) |>
            dplyr::filter(!is.na(CRITERION_VALUE)) |>
            # format the criterion values to the TADA magnitude format, for cases when there's a range.
            tidyr::separate(
              col = CRITERION_VALUE,
              into = c("MagnitudeValueLower", "MagnitudeValueUpper"),
              sep = "-", # Split by " - "
              fill = "left",
              convert = TRUE, # Automatically convert to the appropriate type (numeric)
              extra = "drop"
            ) |>
            # convert CST columns to TADA criteria column name
            dplyr::mutate(SaltFresh = CRITERIATYPEFRESHSALTWATER) |>
            dplyr::mutate(AcuteChronic = CRITERIATYPE_ACUTECHRONIC) |>
            dplyr::mutate(MagnitudeUnit = UNIT_NAME) |>
            # selct relevant columns found in the TADA criteria table, append CST pollutant name and use at the end
            dplyr::select(
              names(suppressMessages(TADA_DefineCriteriaMethodology())),
              CST.STD_POLLUTANT_NAME = STD_POLLUTANT_NAME,
              CST.USE = USE_CLASS_NAME_LOCATION_ETC
            ) |>
            dplyr::distinct()

          # print message to indicate we are joining CST magnitudes to user criteria table, additional review is likely needed.
          if (nrow(DefineCriteriaMethodology2) == 0) {
            message(paste(
              "TADA_DefineCriteriaMethodology: auto_assign = TRUE.",
              "No parameter(s) and/or use(s) were matched between ATTAINS and CST for your defined org_id(s). No magnitude values could be populated for your org(s)."
            ))
          }

          # final join, make sure that any ATTAINS param/uses that we could not match to CST remains in the criteria table
          DefineCriteriaMethodology2 <- DefineCriteriaMethodology2 |>
            dplyr::right_join(
              dplyr::select(
                DefineCriteriaMethodology,
                ATTAINS.OrganizationIdentifier,
                ATTAINS.ParameterName,
                ATTAINS.UseName,
                TADA.ComparableDataIdentifier,
                TADA.CharacteristicName,
                TADA.ResultSampleFractionText,
                TADA.MethodSpeciationName
              ),
              by = dplyr::join_by(
                ATTAINS.OrganizationIdentifier,
                ATTAINS.ParameterName,
                ATTAINS.UseName,
                TADA.ComparableDataIdentifier,
                TADA.CharacteristicName,
                TADA.ResultSampleFractionText,
                TADA.MethodSpeciationName
              )
            )

          # We will filter out any instances of ph variation, temperature rise above ambient and any other
          # CST pollutant name which TADA analysis function may not be able to handle currently.
          # NOTE FOR DEVELOPERS: We may wish to include these pollutants back eventually if we can
          # think of a way to handle these unique cases for analysis.
          if (
            any(
              DefineCriteriaMethodology2$CST.STD_POLLUTANT_NAME %in%
                c("PH VARIATION", "TEMPERATURE RISE ABOVE AMBIENT")
            )
          ) {
            print(paste(
              "TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'.",
              "TADA functions cannot currently handle analysis for these instances."
            ))
          }
          DefineCriteriaMethodology <- DefineCriteriaMethodology2 |>
            dplyr::filter(
              !CST.STD_POLLUTANT_NAME %in%
                c("PH VARIATION", "TEMPERATURE RISE ABOVE AMBIENT")
            )
        }
      }
      # final formatting to ensure all column types are correct
      DefineCriteriaMethodology <- TADA_CorrectColType(
        DefineCriteriaMethodology
      )
    }

    # User wants to populate the criteria table using a user supplied table.
    # This option will prioritize a user-supplied table, but will include
    # all rows for any missing WQP Characteristic (or TADA.ComparableDataIdenftifier)
    # generated from the auto_assign default values. Users may also append epa 304a values.
    if (!is.null(criteriaMethods)) {
      # If user specifies org_id = NULL (handled upstream in this function).
      # Users who may want to do the ATTAINS crosswalk later on in the process, can choose to
      # specify org_id = NULL and to decide how to populate on their own after analysis.
      if ("" %in% org_id) {
        criteriaMethods$ATTAINS.OrganizationIdentifier <- ""
      }

      criteriaMethods$ATTAINS.ParameterName <- toupper(
        criteriaMethods$ATTAINS.ParameterName
      )

      # identifies all unique TADA.CharacteristicNames in TADA data frame
      unique_param <- unique(.data$TADA.CharacteristicName)
      # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
      TADA_param <- dplyr::distinct(.data[, c(
        "TADA.CharacteristicName",
        "TADA.ComparableDataIdentifier"
      )]) |>
        tidyr::uncount(weights = length(org_id))

      TADA_param <- TADA_param |>
        dplyr::mutate(
          ATTAINS.OrganizationIdentifier = as.character(rep(
            org_id,
            nrow(TADA_param) / length(org_id)
          ))
        )

      criteriaMethods <- criteriaMethods |>
        dplyr::select(-TADA.ComparableDataIdentifier) |> # we will join by TADA.CharacteristicName from our TADA dataframe to ensure accurate crosswalk
        dplyr::full_join(
          TADA_param,
          by = c("ATTAINS.OrganizationIdentifier", "TADA.CharacteristicName")
        ) |>
        dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id)

      # 2. Identify missing columns
      missing_cols <- setdiff(desired_cols, names(criteriaMethods))

      # 3. Add missing columns with NA values using mutate()
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          criteriaMethods <- criteriaMethods |> dplyr::mutate(!!col := NA)
        }
      }

      # What WQP Characteristic names did the user supplied table miss?
      non_definedCriteria <- criteriaMethods |>
        dplyr::filter(is.na(ATTAINS.ParameterName)) |>
        dplyr::filter(TADA.CharacteristicName %in% unique_param) |>
        dplyr::select(dplyr::all_of(desired_cols)) |>
        as.data.frame()

      if (nrow(non_definedCriteria) > 0 && displayUniqueId == TRUE) {
        warning(paste0(
          "Your user supplied criteriaMethods file is missing",
          length(unique(non_definedCriteria$TADA.ComparableDataIdentifier)),
          "unique TADA.ComparableDataIdentifier(s):",
          unique(non_definedCriteria$TADA.ComparableDataIdentifier),
          "without an ATTAINS.ParameterName crosswalk.",
          "Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
        ))
      }

      if (nrow(non_definedCriteria) > 0 && displayUniqueId == FALSE) {
        warning(paste(
          "Your user supplied criteriaMethods file is missing",
          length(unique(non_definedCriteria$TADA.CharacteristicName)),
          "unique TADA.ComparableDataIdentifier(s)",
          ": \n",
          paste0(
            unique(non_definedCriteria$TADA.CharacteristicName),
            collapse = ", "
          ),
          "without an ATTAINS.ParameterName crosswalk.",
          "Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
        ))
      }

      # If the source of the ATTAINS param and uses is the prior ATTAINS assessment cycle.
      # NOTE: If criteriaMethods is provided, we are now setting auto_assign = FALSE as default. These code chunks
      # for auto_assign == TRUE may no longer be needed. Leaving it in though for in case we decide otherwise. KW 12/12/25
      if (auto_assign == TRUE & is.null(AU_UsesRef)) {
        warning(paste0(
          "You selected auto_assign == TRUE. No AU_UsesRef was provided. ",
          "Filling in these blanks with ATTAINS.ParameterName and ATTAINS.UseName pulled in from the prior ATTAINS Assessment Cycle. ",
          "Please review or edit these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
        ))
      }
      # If the source of the ATTAINS param and uses is from the user supplied AU_UsesRef.
      if (auto_assign == TRUE & !is.null(AU_UsesRef)) {
        warning(paste0(
          "You selected auto_assign == TRUE. An AU_UsesRef was provided. ",
          "Filling in these blanks with ATTAINS.ParameterName and ATTAINS.UseName pulled in from your AU_UsesRef. ",
          "Please review or edit these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."
        ))
      }

      # From the user supplied criteriaMethods, fill in any values from the pre-filled MLSummaryRef template generated.
      definedCriteria <- criteriaMethods |>
        # dplyr::filter(!is.na(ATTAINS.ParameterName)) |>
        dplyr::filter(
          TADA.CharacteristicName %in% TADA_param$TADA.CharacteristicName
        ) |>
        dplyr::relocate(dplyr::all_of(desired_cols)) |>
        as.data.frame()

      # Create empty criteria methods data frame with just column names.
      suppressMessages(
        DefineCriteriaMethodology <- TADA_DefineCriteriaMethodology()
      )

      # Must now match the data types. Developer note: can this be modified with TADA TADA_CorrectColType function?
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
            non_definedCriteria[, i] <- as.Date(
              non_definedCriteria[, i],
              format = "%b %d"
            )
            definedCriteria[, i] <- as.Date(
              definedCriteria[, i],
              format = "%b %d"
            )
          }
        }
      )
      # format season dates to only contain MM-DD
      non_definedCriteria$SeasonStartDate <- format(
        non_definedCriteria$SeasonStartDate,
        format = "%b %d"
      )
      non_definedCriteria$SeasonEndDate <- format(
        non_definedCriteria$SeasonEndDate,
        format = "%b %d"
      )
      definedCriteria$SeasonStartDate <- format(
        definedCriteria$SeasonStartDate,
        format = "%b %d"
      )
      definedCriteria$SeasonEndDate <- format(
        definedCriteria$SeasonEndDate,
        format = "%b %d"
      )

      DefineCriteriaMethodology <- DefineCriteriaMethodology |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          TADA.ComparableDataIdentifier,
          TADA.CharacteristicName
        ) |>
        dplyr::full_join(
          definedCriteria,
          by = dplyr::join_by(
            ATTAINS.OrganizationIdentifier,
            ATTAINS.ParameterName,
            ATTAINS.UseName,
            TADA.ComparableDataIdentifier,
            TADA.CharacteristicName
          )
        ) |>
        dplyr::arrange(ATTAINS.UseName) |>
        dplyr::distinct()

      # ensure the first n columns are shown in TADA criteria table format. Additional columns are allowed for notes etc.
      DefineCriteriaMethodology <- dplyr::relocate(
        DefineCriteriaMethodology,
        desired_cols # NOTE: 12/16/25 changed from dplyr::select to relocate. Allow additional columns from user supplied table.
      )
    }

    # Display all unique TADA.ComparableDataIdentifier in the Criteria Methods list or not.
    # Helps a user identifies all WQP data if they do not fill out the reference tables when TRUE
    # FALSE is recommended if a user has gone through a step by step review process to
    # determine what they would like summarized in their final output.
    if (displayUniqueId == FALSE) {
      print(paste0(
        "TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. ",
        "Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."
      ))

      DefineCriteriaMethodology <- DefineCriteriaMethodology |>
        dplyr::mutate(TADA.ComparableDataIdentifier = NA) |>
        dplyr::arrange(
          ATTAINS.OrganizationIdentifier != "USEPA",
          ATTAINS.OrganizationIdentifier,
          ATTAINS.UseName
        ) |>
        # tidyr::drop_na(ATTAINS.ParameterName) |>
        dplyr::distinct()
    }
  }

  # User wants to populate the Criteria table using the EPA304(a) criteria
  # joins the EPA304(a) criteria to the current Criteria Table.
  if ("USEPA" %in% org_id) {
    print(paste0(
      "TADA_DefineCriteriaMethodology: USEPA was included in your 'org_id': Including EPA304a recommended criteria by each unique TADA.CharacteristicName if one is found."
    ))
    epa304a <- utils::read.csv(
      system.file("extdata", "EPA304a_criteria_table.csv", package = "EPATADA"),
      fileEncoding = "UTF-8-BOM"
    )
    if (displayUniqueId == TRUE) {
      uniqueID <- unique(.data[, c(
        "TADA.ComparableDataIdentifier",
        "TADA.CharacteristicName"
      )])
      epa304a <- epa304a |>
        dplyr::select(-TADA.ComparableDataIdentifier) |>
        dplyr::left_join(uniqueID, by = dplyr::join_by(TADA.CharacteristicName))
    }
    # read in ref csv
    coltype.ref <- utils::read.csv(system.file(
      "extdata",
      "TADAColTypeRef.csv",
      package = "EPATADA"
    ))
    if (missing(.data)) {
      epa304a <- suppressWarnings(TADA_CorrectColType(epa304a)) |>
        dplyr::select(names(epa304a)[
          names(epa304a) %in% coltype.ref$column_name
        ]) |>
        dplyr::mutate(ATTAINS.ParameterName = toupper(ATTAINS.ParameterName))
    }
    if (!missing(.data)) {
      epa304a <- suppressWarnings(TADA_CorrectColType(epa304a)) |>
        dplyr::select(names(epa304a)[
          names(epa304a) %in% coltype.ref$column_name
        ]) |>
        dplyr::mutate(ATTAINS.ParameterName = toupper(ATTAINS.ParameterName)) |>
        dplyr::filter(
          TADA.CharacteristicName %in%
            DefineCriteriaMethodology$TADA.CharacteristicName
        )
    }

    DefineCriteriaMethodology <- DefineCriteriaMethodology |>
      # filters out the blank EPA304a criteria table but keep any unique
      # TADA Characteristic not defined from the epa304a criteria table.
      dplyr::filter(
        !(ATTAINS.OrganizationIdentifier == "USEPA" &
          TADA.CharacteristicName %in% epa304a$TADA.CharacteristicName)
      ) |>
      plyr::rbind.fill(epa304a) |>
      dplyr::arrange(ATTAINS.OrganizationIdentifier != "USEPA")
  }

  # Final formatting of criteria table for consistent output
  if (!all(is.na(DefineCriteriaMethodology$ATTAINS.OrganizationIdentifier))) {
    DefineCriteriaMethodology <- DefineCriteriaMethodology |>
      tidyr::complete(
        ATTAINS.OrganizationIdentifier,
        TADA.CharacteristicName
      ) |>
      dplyr::filter(!is.na(ATTAINS.OrganizationIdentifier)) |>
      dplyr::arrange(
        ATTAINS.OrganizationIdentifier != "USEPA",
        ATTAINS.OrganizationIdentifier,
        ATTAINS.UseName
      ) |>
      # tidyr::drop_na(ATTAINS.ParameterName) |>
      dplyr::distinct()
  }

  # Generates the excel function (HIGHLY Recommended for users to export)
  if (excel == TRUE) {
    # Excel ref files to be stored in the Downloads folder location.
    # Define the OneDrive Downloads path
    onedrive_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "OneDrive",
      "Downloads",
      "myfileRef.xlsx"
    )

    # Define the default Downloads path
    default_downloads_path <- file.path(
      Sys.getenv("USERPROFILE"),
      "Downloads",
      "myfileRef.xlsx"
    )

    # Check if the OneDrive Downloads path exists, and prioritize it
    if (file.exists(onedrive_downloads_path)) {
      downloads_path <- onedrive_downloads_path
    } else {
      downloads_path <- default_downloads_path
    }

    # if a user generates a blank template, the prior blank template must also be generated in excel
    if (missing(.data)) {
      suppressMessages(TADA_MLSummary(excel = excel, overwrite = overwrite))
    }

    wb <- openxlsx::loadWorkbook(wb, downloads_path)

    tryCatch(
      {
        openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
        openxlsx::addWorksheet(wb, "Index-Criteria", visible = FALSE)
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "DefineCriteriaMethodology")
        openxlsx::removeWorksheet(wb, "Index-Criteria")
        openxlsx::removeWorksheet(wb, "DataDictionary") # gets added at the end.
        openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
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
    openxlsx::sheetVisibility(wb)[7] <- "hidden"

    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }

    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 1:ncol(DefineCriteriaMethodology),
      widths = "auto"
    )
    openxlsx::setColWidths(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 1:5,
      widths = 20
    )

    # Export DefineCriteriaMethodology dataframe into the excel spreadsheet tab
    openxlsx::writeData(
      wb,
      "DefineCriteriaMethodology",
      startCol = 1,
      x = DefineCriteriaMethodology,
      headerStyle = header_st
    )

    if (missing(.data)) {
      .data <- data.frame(
        TADA.ComparableDataIdentifier = NA_character_,
        TADA.CharacteristicName = NA_character_,
        TADA.ResultSampleFractionText = NA_character_,
        TADA.MethodSpeciationName = NA_character_,
        TADA.ResultMeasure.MeasureUnitCode = NA_character_
      )
    }

    # Creates the Index-Criteria List of allowable values under each column
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 6,
      startRow = 1,
      x = unique(.data[, c(
        "TADA.ComparableDataIdentifier",
        "TADA.CharacteristicName",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName"
      )])
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 14,
      startRow = 1,
      # AcuteChronic
      x = data.frame(AcuteChronic = c("A", "C", "NA"))
    )

    # get list of ATTAINS Water Types from ATTAINS
    All.WaterTypeList <- utils::read.csv(system.file(
      "extdata",
      "ATTAINSParamUseEntityRef.csv",
      package = "EPATADA"
    ))

    Org.WaterTypeList <- dplyr::filter(
      All.WaterTypeList,
      ATTAINS.OrganizationIdentifier %in% org_id
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 10,
      startRow = 1,
      # ATTAINS.WaterType
      x = unique(Org.WaterTypeList$ATTAINS.WaterType)
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 11,
      startRow = 1,
      # SaltFresh
      x = data.frame(SaltFresh = c("S", "F", "NA"))
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 12,
      startRow = 1,
      x = data.frame(
        DepthCategory = c(
          "No depth info",
          "Epilimnion-surface",
          "Surface",
          "Bottom",
          "Middle"
        )
      )
    )

    if (is.null(MLSummaryRef)) {
      MLSummaryRef <- data.frame(UniqueSpatialCriteria = NA_character_)
    }

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 13,
      startRow = 1,
      # UniqueSpatialCriteria
      x = data.frame(
        UniqueSpatialCriteria = c(
          unique(MLSummaryRef$UniqueSpatialCriteria),
          "NA"
        )
      )
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 15,
      startRow = 1,
      # EquationBased
      x = data.frame(EquationBased = c("Yes", "No", "NA"))
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 18,
      startRow = 1,
      # MagnitudeUnit
      x = data.frame(
        MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode)
      )
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 20,
      startRow = 1,
      # DurationUnit
      x = data.frame(
        DurationUnit = c("n-hour", "n-day", "n-week", "n-month", "n-quarter")
      )
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 21,
      startRow = 1,
      # DurationMethod
      x = data.frame(
        DurationMethod = c(
          "arithmetic mean",
          "arithmetic median",
          "arithmetic max",
          "arithmetic min",
          "arithmetic extremes",
          "geometric mean",
          "rolling geometric mean",
          "rolling arithmetic mean",
          "mean of daily minima", # added 1/21/26 common only for DO it seems.
          "mean of daily maxima" # added 1/21/26 common only for DO it seems.
        )
      )
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 23,
      startRow = 1,
      # FreqMethod
      x = data.frame(
        FreqMethod = c(
          "Percent of samples not meeting",
          "percentile",
          "n-samples in 3 years",
          "n-samples in 4 years",
          "n-samples in 5 years",
          "binomial test",
          "NumberNotMeeting"
        )
      )
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 24,
      startRow = 1,
      x = data.frame(
        AssessPeriod = c(
          "Last 30 years",
          "Last 10 years",
          "Last 5 years",
          "Last 3 years",
          "Last year",
          "NA"
        )
      )
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 27,
      startRow = 1,
      x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA"))
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 31,
      startRow = 1,
      x = data.frame(
        DistrPeriod = c(
          "Seasonal",
          "Annual",
          "Semi-Annual",
          "Quarterly",
          "Monthly",
          "Bi-weekly",
          "Weekly",
          "10 days",
          "NA"
        )
      )
    )

    # allowable values for ATTAINS.ParameterName (entire domain, not org specific)
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 3,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index'!$E$2:$E$60000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    # allowable values for ATTAINS.UseName (org specific)
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 4,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$G$2:$G$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    # allowable value for TADA.ComparableDataIdentifier
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 5,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$F$2:$F$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 6,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$H$2:$H$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 7,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$I$2:$I$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 8,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$J$2:$J$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 9,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$K$2:$K$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 10,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$L$2:$L$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 11,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$M$2:$M$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 12,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$N$2:$N$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 13,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$O$2:$O$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 16,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$R$2:$R$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 18,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$T$2:$T$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 19,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$U$2:$U$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 21,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$W$2:$W$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 22,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$X$2:$X$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 25,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$AA$2:$AA$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 29,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$AE$2:$AE$1000"),
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    # Conditional Formatting
    openxlsx::freezePane(
      wb,
      "DefineCriteriaMethodology",
      firstActiveRow = 2,
      firstActiveCol = 4
    )
    openxlsx::conditionalFormatting(
      wb,
      "DefineCriteriaMethodology",
      cols = 1:31,
      rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb,
      "DefineCriteriaMethodology",
      cols = 1:31,
      rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    ) # modified cells.

    # Group DataSufficiency Columns
    openxlsx::groupColumns(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 22:length(names(DefineCriteriaMethodology)),
      hidden = FALSE,
      level = -1
    )

    # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }

    if (overwrite == FALSE) {
      warning(
        "If you would like to replace the file, use overwrite = TRUE argument in TADA_ParametersForAnalysis"
      )
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
  # Define the OneDrive Downloads path
  onedrive_downloads_path <- file.path(
    Sys.getenv("USERPROFILE"),
    "OneDrive",
    "Downloads",
    "myfileRef.xlsx"
  )

  # Define the default Downloads path
  default_downloads_path <- file.path(
    Sys.getenv("USERPROFILE"),
    "Downloads",
    "myfileRef.xlsx"
  )

  # Check if the OneDrive Downloads path exists, and prioritize it
  if (file.exists(onedrive_downloads_path)) {
    downloads_path <- onedrive_downloads_path
  } else {
    downloads_path <- default_downloads_path
  }

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
      "AssessPeriod",
      "AssessPeriodStartDate",
      "AssessPeriodEndDate",
      "Season",
      "SeasonStartDate",
      "SeasonEndDate",
      "DistrCount",
      "DistrPeriod",
      "DistrMinSample",
      "Notes"
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
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology",
      "Methodology"
    ),
    Description = c(
      # ATTAINS.OrganizationIdentifier
      "The id of your organization that gets submitted to ATTAINS.",
      # ATTAINS.ParameterName
      "The name of the parameter that gets submitted to ATTAINS. These do not need to be unique to your organization.",
      # ATTAINS.UseName
      "The name of the use of a waterbody that gets submitted to ATTAINS. These use names should be specific to your organization.",
      # TADA.ComparableDataIdentifier
      paste0(
        "To populate this field, specify displayUniqueId = TRUE. Concatenates the WQP Characteristic, Fraction and speciation into one string.",
        "If provided, this will crosswalk an ATTAINS.ParameterName to this TADA.ComparableDataIdentifier. ",
        "It is recommended to have performed this crosswalk in TADA_ParametersForAnalysis to avoid any duplicated ",
        "definition of your organization's criteria if they are the same for multiple TADA.ComparableDataIdentifiers.",
        collapse = " "
      ),
      # TADA.CharacteristicName
      "Name of TADA characteristic in the WQP that gets matched to an ATTAINS parameter.",
      # TADA.ResultSampleFractionText
      "If TADA.ComparableDataIdentifier is blank, this will group all TADA.CharacteristicName to an ATTAINS.ParameterName on the condition of the specified Fraction Type.",
      # TADA.MethodSpeciationName
      "If TADA.ComparableDataIdentifier is blank, this will group all TADA.CharacteristicName to an ATTAINS.ParameterName on the condition of the specified speciation.",
      # ATTAINS.WaterType
      "The name of the waterbody type associated with an Assessment Unit from the ATTAINS domain value. These values will only be avaialble if a sites to ATTAINS Assessment Units crosswalk is provided.",
      # SaltFresh
      "The salt or freshwater classification of the ATTAINS Waterbody Type. Users should specify if a standard only applies to salt or freshwater types.",
      # DepthCategory
      "The depth within water column that a standard applies to if applicable. Users can run TADA.FlagDepthCategory to populate this entry (or can specify a specific unit measurement?).",
      # UniqueSpatialCriteria
      "Users should specify any monitoring location sites that may contain a unique spatial critieria for a parameter or use in CreateMLSummaryRef.",
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
      # AssessPeriod
      "Labels the assessment period of which the WQP data must be collected from. Users should define the assessment date range in the beginning and end date columns that proceeds this one.",
      # AssessPeriodStartDate
      "The start date in which WQP data will be analyzed for this parameter and use.",
      # AssessPeriodEndDate
      "The end date in which WQP data will be analyzed for this parameter and use.",
      # Season
      "Labels the season in which the standards apply for this parameter and use. Specify the start and end dates of your season in the proceeding two columns.",
      # SeasonStartDate
      "The start date of the season in which assessments are done for during a calendar year.",
      # SeasonEndDate
      "The end date of the season in which assessments are done for during a calendar year.",
      # DistrCount
      "A numeric value specifying the minimum number of sampling events (consecutive) over a distribution period.",
      # DistrPeriod
      "The period of time in which samples must be collected during an assessment data window.",
      # DistrMinSample
      "How many samples must be collected during each specified DistrPeriod",
      # Notes
      "Additonal free form notes column for any notes that must be considered for this parameter and use that may not be able to be captured in the TADA criteria table format."
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
    gridExpand = TRUE
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
    gridExpand = TRUE
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
    cols = ncol(data_to_write) + 1
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
    cols = 1:(ncol(data_to_write) - 1),
    widths = "auto"
  )

  # Save the workbook to an Excel file
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
}
