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
#' When MLSummaryRef is provided and displayUniqueId is not specified, IDs are
#' retained by default to support MLSummary-based filtering.
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
#' The file will be named "CriteriaMethodology.xlsx". The excel spreadsheet will highlight
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
    "Notes",
    # Equation Columns
    "EquationType",
    "EquationFormula",
    "pHThreshold",
    "pHDirection",
    "hardness_param_1",
    "hardness_param_2",
    "hardness_param_3",
    "hardness_param_4",
    "TemperatureExtreme",
    "pH_param_1",
    "pH_param_2",
    "pH_param_3",
    "pH_param_4",
    "pH_param_5",
    "pH_param_6",
    "pH_param_7",
    "pH_param_8",
    "pH_param_9",
    "MinEqMagnitude",
    "MaxEqMagnitude"
  )

  # If MLSummaryRef is provided and user did not explicitly set displayUniqueId,
  # default to TRUE to retain ComparableDataIdentifier for MLSummary-based filtering/assertions.
  if (!is.null(MLSummaryRef) && missing(displayUniqueId)) {
    displayUniqueId <- TRUE
  }

  # Helper: parse month-day strings to a Date using an anchor year
  .parse_season_date <- function(x, anchor_year = 1972L) {
    if (inherits(x, "Date")) {
      return(x)
    }
    x <- as.character(x)
    # Preserve zero-length length explicitly
    if (length(x) == 0L) {
      return(as.Date(character()))
    }
    x <- ifelse(is.na(x) | trimws(x) == "", NA_character_, x)
    out <- as.Date(paste(x, anchor_year), format = "%b %d %Y")
    bad <- is.na(out)
    if (any(bad)) {
      out[bad] <- as.Date(paste(x[bad], anchor_year), format = "%m-%d %Y")
    }
    out
  }

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

    # If auto_assign = TRUE and no MLSummaryRef OR criteriaMethods arg input is provided, this results in error.
    if (auto_assign == TRUE && !is.null(criteriaMethods)) {
      stop(
        "TADA_DefineCriteriaMethodology: criteriaMethods is provided and auto_assign = TRUE are not valid function argument input combinations."
      )
    }

    # Invalid function input combos - supply one or the other.
    if (!is.null(MLSummaryRef) && !is.null(criteriaMethods)) {
      stop(
        "TADA_DefineCriteriaMethodology: MLSummaryRef and criteriaMethods are both provided. You can only proceed with one (or none) of these options provided."
      )
    }

    # If MLSummaryRef and auto_assign = TRUE, assign a final filter dataframe
    if (!is.null(MLSummaryRef) && auto_assign == TRUE) {
      MLSummary_params <- unique(MLSummaryRef$TADA.ComparableDataIdentifier)
    } else {
      MLSummary_params <- NULL
    }

    # if null, creates a list of all unique TADA.ComparableDataIdentifier, but no org populated.
    if (is.null(org_id)) {
      org_id <- ""
    }

    # if org_id = all, create a crosswalk for all ATTAINS org in the data frame.
    if (tolower("all") %in% tolower(org_id)) {
      if (is.null(criteriaMethods)) {
        if (is.null(AUMLRef)) {
          # Emit a simple, early message unconditionally
          message(
            "org_id == 'All' was selected, no AUMLRef provided; attempting to pull domain orgs."
          )

          # Attempt to retrieve domain orgs; warn on failure but keep going
          org_id <- tryCatch(
            {
              dv <- rExpertQuery::EQ_DomainValues("org_id")
              if (!is.null(dv) && "code" %in% names(dv)) {
                dv[["code"]]
              } else {
                warning(
                  "EQ_DomainValues('org_id') returned no 'code' column; proceeding with empty org list."
                )
                character()
              }
            },
            error = function(e) {
              warning(
                "Failed to retrieve ATTAINS org domain values: ",
                conditionMessage(e)
              )
              character()
            }
          )
        } else {
          message(
            "org_id == 'All' was selected, AUMLRef provided; using orgs found in AUMLRef."
          )
          org_id <- unique(stats::na.omit(
            AUMLRef$ATTAINS.OrganizationIdentifier
          ))
        }
      } else {
        org_id <- unique(stats::na.omit(
          criteriaMethods$ATTAINS.OrganizationIdentifier
        ))
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
          "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment."
        ))
        suppressMessages(
          TADA_ParamRef <- TADA_ParametersForAnalysis(
            .data,
            org_id = org_id,
            auto_assign = "Org", # auto-populate any exact matches found between WQP CharacteristicName and ATTAINS ParameterName
            excel = F,
            overwrite = F # Changed to FALSE when auto_assign = T KW 4/17/26
          )
        )

        message(paste0(
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
            excel = F,
            overwrite = F # Changed to FALSE when auto_assign = T KW 4/17/26
          )
        )

        suppressMessages(
          MLSummaryRef <- TADA_MLSummary(
            .data,
            displayNA = TRUE,
            org_id = org_id,
            usesRef = TADA_usesRef,
            AUMLRef = AUMLRef,
            AU_UsesRef = AU_UsesRef,
            excel = F,
            overwrite = F # Changed to FALSE when auto_assign = T KW 4/17/26
          )
        )

        # correct column types for any empty columns
        MLSummaryRef <- TADA_CorrectColType(MLSummaryRef)
        # Will include all unique TADA Char/ComparableDataIdentifier to be shown in the criteria table
        MLSummaryRef <- TADA_param |>
          dplyr::full_join(MLSummaryRef, by = names(TADA_param))
      }

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
    if (!is.null(MLSummaryRef) && !is.character(MLSummaryRef)) {
      if (!is.data.frame(MLSummaryRef)) {
        stop(
          "TADA_DefineCriteriaMethodology: MLSummaryRef must be a data frame."
        )
      }

      if (is.data.frame(MLSummaryRef)) {
        required_cols <- c(
          "ATTAINS.ParameterName",
          "ATTAINS.UseName",
          "ATTAINS.OrganizationIdentifier",
          "UniqueSpatialCriteria",
          "ATTAINS.WaterType",
          "ATTAINS.AssessmentUnitIdentifier",
          "TADA.ComparableDataIdentifier",
          "SaltFresh",
          "DepthCategory"
        )
        missing_cols <- setdiff(required_cols, names(MLSummaryRef))
        if (length(missing_cols) > 0) {
          stop(
            "TADA_DefineCriteriaMethodology: MLSummaryRef is missing required columns: ",
            paste(missing_cols, collapse = ", ")
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

      # Compute safe vectors in the function environment (avoid rlang .data pronoun)
      org_id_vec <- unique(as.character(org_id))
      ids_vec <- if (
        !missing(.data) && "TADA.ComparableDataIdentifier" %in% names(.data)
      ) {
        unique(.data[["TADA.ComparableDataIdentifier"]])
      } else {
        unique(MLSummaryRef[["TADA.ComparableDataIdentifier"]])
      }

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
          Notes = as.character(NA),
          EquationType = as.character(NA),
          EquationFormula = as.character(NA),
          pHThreshold = as.numeric(NA),
          pHDirection = as.character(NA),
          hardness_param_1 = as.numeric(NA),
          hardness_param_2 = as.numeric(NA),
          hardness_param_3 = as.numeric(NA),
          hardness_param_4 = as.numeric(NA),
          TemperatureExtreme = as.character(NA),
          pH_param_1 = as.numeric(NA),
          pH_param_2 = as.numeric(NA),
          pH_param_3 = as.numeric(NA),
          pH_param_4 = as.numeric(NA),
          pH_param_5 = as.numeric(NA),
          pH_param_6 = as.numeric(NA),
          pH_param_7 = as.numeric(NA),
          pH_param_8 = as.numeric(NA),
          pH_param_9 = as.numeric(NA),
          MinEqMagnitude = as.numeric(NA),
          MaxEqMagnitude = as.numeric(NA)
        )) |>
        dplyr::select(
          dplyr::all_of(desired_cols) # defined in beginning of code
        ) |>
        dplyr::arrange(ATTAINS.UseName) |>
        tidyr::complete(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier = org_id_vec
        ) |>
        # EXTRA: enforce the full (ComparableDataIdentifier x org) grid explicitly
        dplyr::right_join(
          tidyr::expand_grid(
            TADA.ComparableDataIdentifier = ids_vec,
            ATTAINS.OrganizationIdentifier = org_id_vec
          ),
          by = c(
            "TADA.ComparableDataIdentifier",
            "ATTAINS.OrganizationIdentifier"
          )
        ) |>
        dplyr::distinct()

      if (auto_assign == TRUE && !all(org_id == "USEPA")) {
        # currently, we will only apply joining the CST magnitudes when the org_id is known.
        if ("" %in% org_id) {
          DefineCriteriaMethodology <- DefineCriteriaMethodology
        }
        if (!"" %in% org_id) {
          # upper case all character columns for consistency
          DefineCriteriaMethodology <- DefineCriteriaMethodology |>
            dplyr::mutate(dplyr::across(where(is.character), toupper))

          # all lines below will focus on joining CST magnitude values to the auto_assign table
          # pulls in alias crosswalk between CST STD.PollutantName and ATTAINS.ParameterName
          DefineCriteriaMethodology <- DefineCriteriaMethodology |>
            dplyr::mutate(dplyr::across(where(is.character), toupper))

          # all lines below will focus on joining CST magnitude values to the auto_assign table
          # pulls in alias crosswalk between CST STD.PollutantName and ATTAINS.ParameterName
          CST_ATTAINS_Param <- utils::read.csv(system.file(
            "extdata",
            "TADACharAliasRef.csv",
            package = "EPATADA"
          )) |>
            dplyr::mutate(dplyr::across(where(is.character), toupper)) |>
            dplyr::filter(
              (CharacteristicName %in%
                stats::na.omit(unique(
                  DefineCriteriaMethodology$TADA.CharacteristicName
                )) &
                ATTAINS.ParameterName %in%
                  stats::na.omit(unique(
                    DefineCriteriaMethodology$ATTAINS.ParameterName
                  ))) |
                (CharacteristicName %in%
                  stats::na.omit(unique(
                    DefineCriteriaMethodology$TADA.CharacteristicName
                  )) &
                  is.na(ATTAINS.ParameterName))
            )

          # print message to indicate we are joining CST magnitudes to user criteria table, additional review is likely needed.
          message(paste(
            "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected.",
            "  Finding an alias match between ATTAINS parameter name and Criteria Search Tool (CST) standardized pollutant names.",
            "  Finding an alias match between ATTAINS use name and Criteria Search Tool (CST) uses.",
            "  If an ATTAINS.ParameterName and ATTAINS.UseName alias was found, populating these rows with the CST magnitude values.",
            "  A many-to-many match is likely. User review is needed to ensure accuracy in crosswalk method.",
            sep = "\r\n"
          ))

          # pulls in uses alias table between ATTAINS.UseName and CST uses
          uses <- utils::read.csv(system.file(
            "extdata",
            "TADAUsesAliasRef.csv",
            package = "EPATADA"
          ))
          # filters uses crosswalk by the org_id
          uses <- uses |>
            dplyr::mutate(dplyr::across(where(is.character), toupper)) |>
            dplyr::filter(
              !is.na(ATTAINS.OrganizationIdentifier),
              ATTAINS.OrganizationIdentifier %in%
                unique(DefineCriteriaMethodology$ATTAINS.OrganizationIdentifier)
            )
          # pulls in CriteriaSearchToolRef.rda
          # Extract CST Criteria from the internal workbook only; error if missing/unreadable
          internal_path <- system.file(
            "extdata",
            "cst-workbook.xlsx",
            package = "EPATADA"
          )
          if (!nzchar(internal_path) || !file.exists(internal_path)) {
            stop(
              "Internal CST workbook is missing: inst/extdata/cst-workbook.xlsx. ",
              "Please add this file to the EPATADA package (dev-time: run .TADA_CST_UpdateWorkbook())."
            )
          }

          CriteriaSearchToolRef <- .tada_cst_read_sheet(
            internal_path,
            target = "criteria"
          )
          CriteriaSearchToolRef_Legend <- .tada_cst_read_sheet(
            internal_path,
            target = "legend"
          )
          CriteriaSearchToolRef_Sources <- .tada_cst_read_sheet(
            internal_path,
            target = "sources"
          )
          if (is.null(CriteriaSearchToolRef)) {
            stop(
              "Failed to read 'Criteria' sheet from internal CST workbook at: ",
              internal_path
            )
          }
          CriteriaSearchToolRef <- .tada_cst_prepare_table(
            CriteriaSearchToolRef
          )
          CriteriaSearchToolRef_Legend <- .tada_cst_prepare_table(
            CriteriaSearchToolRef_Legend
          )
          CriteriaSearchToolRef_Sources <- .tada_cst_prepare_table(
            CriteriaSearchToolRef_Sources
          )

          # remove intermediate variable
          rm(internal_path)

          # upper case all character columns for consistency
          CriteriaSearchToolRef <- CriteriaSearchToolRef |>
            dplyr::mutate(
              UNIT_NAME = stringr::str_replace_all(UNIT_NAME, "\u00B5", "u"),
              dplyr::across(where(is.character), toupper)
            )

          # filter the CST to relevant org, parameters and uses
          CriteriaSearchToolRef_filtered <- CriteriaSearchToolRef |>
            dplyr::right_join(
              CST_ATTAINS_Param,
              by = c("POLLUTANT_NAME", "STD_POLLUTANT_NAME")
            ) |>
            dplyr::right_join(
              dplyr::filter(
                uses,
                ATTAINS.OrganizationIdentifier %in%
                  DefineCriteriaMethodology$ATTAINS.OrganizationIdentifier
              ),
              by = dplyr::join_by(
                ENTITY_NAME,
                ENTITY_ABBR,
                CRITERIATYPEAQUAHUMHLTH,
                CRITERIATYPEFRESHSALTWATER,
                CRITERIATYPE_ACUTECHRONIC,
                USE_CLASS_NAME_LOCATION_ETC
              )
            ) |>
            # join the CST source link
            dplyr::left_join(
              CriteriaSearchToolRef_Sources,
              by = c("CRIT_SOURCE_ID")
            ) |>
            dplyr::mutate(
              CST.SourceLink = paste0(SOURCE, "#page=", as.character(PDFPGNO))
            )

          # fill in TADA criteria table with CST magnitude values and other relevant CST columns
          DefineCriteriaMethodology2 <- DefineCriteriaMethodology |>
            dplyr::full_join(
              CriteriaSearchToolRef_filtered,
              by = c(
                "ATTAINS.OrganizationIdentifier",
                "ATTAINS.ParameterName",
                "TADA.CharacteristicName" = "CharacteristicName"
              )
            ) |>
            dplyr::mutate(
              ATTAINS.UseName = dplyr::if_else(
                ATTAINS.UseName.x == ATTAINS.UseName.y,
                ATTAINS.UseName.x,
                NA_character_
              )
            ) |>
            # format the criterion values to the TADA magnitude format, for cases when there's a range.
            tidyr::separate(
              col = CRITERION_VALUE,
              into = c("MagnitudeValueLower", "MagnitudeValueUpper"),
              sep = "\\s*-\\s*", # robust to spaces around the dash
              fill = "left",
              convert = TRUE,
              extra = "drop"
            ) |>
            # convert CST columns to TADA criteria column name
            dplyr::mutate(SaltFresh = CRITERIATYPEFRESHSALTWATER) |>
            dplyr::mutate(AcuteChronic = CRITERIATYPE_ACUTECHRONIC) |>
            dplyr::mutate(MagnitudeUnit = UNIT_NAME) |>
            # select relevant columns found in the TADA criteria table, append CST pollutant name and use at the end
            dplyr::select(
              dplyr::all_of(desired_cols),
              CST.StdPollutantName = STD_POLLUTANT_NAME,
              CST.Use = USE_CLASS_NAME_LOCATION_ETC,
              CST.CriteriaTypeAquaHumHlth = CRITERIATYPEAQUAHUMHLTH,
              CST.CriteriaTypeWaterOrg = CRITERIATYPE_WATERORG,
              CST.SourceLink
            ) |>
            dplyr::distinct()

          # print message to indicate we are joining CST magnitudes to user criteria table, additional review is likely needed.
          if (nrow(DefineCriteriaMethodology2) == 0) {
            message(paste(
              "TADA_DefineCriteriaMethodology: auto_assign = TRUE.",
              "No parameter(s) and/or use(s) were matched between ATTAINS and CST for your defined org_id(s). No magnitude values could be populated for your org(s)."
            ))
          }

          # We will filter out any instances of ph variation, temperature rise above ambient and any other
          # CST pollutant name which TADA analysis function may not be able to handle currently.
          # NOTE FOR DEVELOPERS: We may wish to include these pollutants back eventually if we can
          # think of a way to handle these unique cases for analysis.
          if (
            any(
              DefineCriteriaMethodology2$CST.StdPollutantName %in%
                c("PH VARIATION", "TEMPERATURE RISE ABOVE AMBIENT")
            )
          ) {
            message(paste(
              "TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'.",
              "TADA functions cannot currently handle analysis for these instances."
            ))
          }
          DefineCriteriaMethodology2 <- DefineCriteriaMethodology2 |>
            dplyr::filter(
              !CST.StdPollutantName %in%
                c("PH VARIATION", "TEMPERATURE RISE ABOVE AMBIENT")
            )

          # Now, make sure that the CST Magnitude unit matches TADA data frame unit
          TADAPriorityCharConvertRef <- utils::read.csv(
            system.file(
              "extdata",
              "TADAPriorityCharConvertRef.csv",
              package = "EPATADA"
            ),
            fileEncoding = "UTF-8-BOM"
          ) |>
            dplyr::mutate(dplyr::across(
              where(is.character),
              stringr::str_to_upper
            )) |>
            dplyr::filter(!is.na(Code))

          # identify the unit ref of the .data
          unitRef <- suppressMessages(suppressWarnings(TADA_CreateUnitRef(
            .data
          )))

          # temporarily adjust scientific notation, then restore
          .old_opts <- options(scipen = 999)
          on.exit(options(.old_opts), add = TRUE)

          # modify unitRef to have the MagnitudeUnit as the target.
          unitRef_CST <- unitRef |>
            dplyr::inner_join(
              dplyr::select(
                DefineCriteriaMethodology2,
                TADA.CharacteristicName,
                MagnitudeUnit
              ),
              by = "TADA.CharacteristicName",
              relationship = "many-to-many"
            ) |>
            dplyr::filter(
              !is.na(MagnitudeUnit),
              TADA.ResultMeasure.MeasureUnitCode != MagnitudeUnit
            ) |>
            dplyr::select(
              TADA.CharacteristicName,
              TADA.ResultMeasure.MeasureUnitCode,
              MagnitudeUnit,
              -TADA.WQXUnitConversionCoefficient,
              -TADA.WQXUnitConversionFactor
            ) |>
            dplyr::left_join(
              TADAPriorityCharConvertRef,
              by = c("MagnitudeUnit" = "Code")
            ) |>
            dplyr::distinct()

          unitRef_CST_NA <- dplyr::filter(unitRef_CST, is.na(Target.Unit))

          # print message to indicate there are values pulled in from the CST that are being converted to match those in the TADA df
          if (length(unique(unitRef_CST$TADA.CharacteristicName)) > 0) {
            message(paste(
              "Warning in TADA_DefineCriteriaMethodology: ",
              "There are",
              length(unique(unitRef_CST$TADA.CharacteristicName)),
              "TADA.CharacteristicName units that do not match with the CST autoassign MagnitudeUnit values.",
              "Converting these MagnitudeUnit Values from the CST to match the TADA.ResultMeasure.MeasureUnitCode in your dataframe.",
              "Please review these conversions."
            ))
          }
          # print message to identify those that could not be converted. Recommend users to select appropriate unit alias or convert manually.
          if (nrow(unitRef_CST_NA) > 0) {
            message(paste(
              "Warning in TADA_DefineCriteriaMethodology:",
              "There are",
              length(unique(unitRef_CST_NA$TADA.CharacteristicName)),
              "TADA.CharacteristicName with CST MagnitudeUnit values that could not be converted.",
              "Please review these CST magnitude units:",
              paste(unique(unitRef_CST_NA$MagnitudeUnit), collapse = ", "),
              "and convert to an appropriate unit found in your TADA data frame."
            ))
          }

          # convert cst units to match those found in the TADA df
          DefineCriteriaMethodology2 <- suppressWarnings(TADA_CorrectColType(
            DefineCriteriaMethodology2
          ))

          DefineCriteriaMethodology <- DefineCriteriaMethodology2 |>
            dplyr::left_join(
              unitRef_CST,
              by = c("TADA.CharacteristicName", "MagnitudeUnit"),
              relationship = "many-to-many"
            ) |>
            dplyr::mutate(
              Conversion.Factor = dplyr::if_else(
                is.na(Conversion.Factor),
                1,
                Conversion.Factor
              ),
              MagnitudeUnit = dplyr::if_else(
                is.na(Target.Unit),
                MagnitudeUnit,
                Target.Unit
              ),
              MagnitudeValueLower = round(
                Conversion.Factor * MagnitudeValueLower,
                digits = 4
              ),
              MagnitudeValueUpper = round(
                Conversion.Factor * MagnitudeValueUpper,
                digits = 4
              ),
              # Don’t infer “Yes” just because magnitudes are missing
              EquationBased = dplyr::if_else(
                dplyr::if_any(
                  dplyr::any_of(c("CST.StdPollutantName", "CST.Use")),
                  ~ !is.na(.x)
                ) &
                  dplyr::if_all(
                    dplyr::all_of(c(
                      "MagnitudeValueLower",
                      "MagnitudeValueUpper"
                    )),
                    ~ is.na(.x)
                  ),
                "Yes",
                "No",
                missing = "No"
              )
            ) |>
            dplyr::select(
              -dplyr::any_of(names(TADAPriorityCharConvertRef)),
              -TADA.ResultMeasure.MeasureUnitCode
            ) |>
            dplyr::distinct()
        }
      }

      # final formatting to ensure all column types are correct
      DefineCriteriaMethodology <- suppressWarnings(TADA_CorrectColType(
        DefineCriteriaMethodology
      )) |>
        dplyr::filter(!is.na(TADA.CharacteristicName))
    }

    # User wants to populate the criteria table using a user supplied table.
    # This option will prioritize a user-supplied table, but will include
    # all rows for any missing WQP Characteristic (or TADA.ComparableDataIdentifier)
    # generated from the auto_assign default values. Users may also append epa 304a values.
    if (!is.null(criteriaMethods)) {
      # If org_id includes the empty string placeholder (which is generated if org_id = NULL or ""), do not overwrite user-supplied orgs in criteriaMethods.
      # Only add the column if missing; preserve "" blank values.
      if (
        "" %in%
          org_id &&
          !"ATTAINS.OrganizationIdentifier" %in% names(criteriaMethods)
      ) {
        criteriaMethods$ATTAINS.OrganizationIdentifier <- "" # KW: replaced NA with ""for consistency? Other wise change back to NA_character_
      }

      criteriaMethods$ATTAINS.ParameterName <- toupper(
        criteriaMethods$ATTAINS.ParameterName
      )

      # Build a param frame from .data; choose join keys based on org_id presence
      TADA_param <- dplyr::distinct(.data[, c(
        "TADA.CharacteristicName",
        "TADA.ComparableDataIdentifier"
      )])

      if (length(org_id) == 1L && identical(org_id, "")) {
        # No org constraint: allow NA org in the join, do not expand by org_id
        join_by_cols <- c("TADA.CharacteristicName")
      } else {
        # Expand across provided org_id values
        TADA_param <- tidyr::crossing(
          TADA_param,
          ATTAINS.OrganizationIdentifier = as.character(org_id)
        )
        join_by_cols <- c(
          "ATTAINS.OrganizationIdentifier",
          "TADA.CharacteristicName"
        )
      }

      criteriaMethods <- criteriaMethods |>
        dplyr::select(-dplyr::any_of("TADA.ComparableDataIdentifier")) |>
        dplyr::full_join(TADA_param, by = join_by_cols)

      # Only filter to org_id when org_id is not the empty-string placeholder
      if (!("" %in% org_id)) {
        criteriaMethods <- criteriaMethods |>
          dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id)
      }

      # 2. Identify missing columns
      missing_cols <- setdiff(desired_cols, names(criteriaMethods))

      # 3. Add missing columns with NA values using mutate()
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          criteriaMethods <- criteriaMethods |> dplyr::mutate(!!col := NA)
        }
      }

      # Identify all unique TADA.CharacteristicName present in the data
      unique_param <- unique(.data$TADA.CharacteristicName)

      # What WQP Characteristic names did the user supplied table miss?
      non_definedCriteria <- criteriaMethods |>
        dplyr::filter(is.na(ATTAINS.ParameterName)) |>
        dplyr::filter(TADA.CharacteristicName %in% unique_param) |>
        dplyr::select(dplyr::all_of(desired_cols)) |>
        as.data.frame()

      if (nrow(non_definedCriteria) > 0 && displayUniqueId == TRUE) {
        warning(paste0(
          "Your user supplied criteriaMethods file is missing ",
          length(unique(non_definedCriteria$TADA.ComparableDataIdentifier)),
          " unique TADA.ComparableDataIdentifier(s):\n  ",
          paste0(
            unique(non_definedCriteria$TADA.ComparableDataIdentifier),
            collapse = ", "
          ),
          "\n",
          "  without an ATTAINS.ParameterName crosswalk.\n",
          "  Please review if these entries are applicable to your analysis or ignore this message if they are not relevant.\n"
        ))
      }

      if (nrow(non_definedCriteria) > 0 && displayUniqueId == FALSE) {
        warning(paste0(
          "Your user supplied criteriaMethods file is missing ",
          length(unique(non_definedCriteria$TADA.CharacteristicName)),
          " unique TADA.CharacteristicName(s) :\n  ",
          paste0(
            unique(non_definedCriteria$TADA.CharacteristicName),
            collapse = ", "
          ),
          "\n",
          "  without an ATTAINS.ParameterName crosswalk.\n",
          "  Please review if these entries are applicable to your analysis or ignore this message if they are not relevant.\n"
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

      suppressWarnings({
        for (i in seq_len(ncol(non_definedCriteria))) {
          col_name <- names(non_definedCriteria)[i]
          target_class <- desired_types[[i]]

          # Coerce non_definedCriteria if it has rows
          if (nrow(non_definedCriteria) > 0) {
            if (identical(target_class, "numeric")) {
              non_definedCriteria[[
                col_name
              ]] <- suppressWarnings(as.numeric(non_definedCriteria[[
                col_name
              ]]))
            } else if (identical(target_class, "Date")) {
              if (col_name %in% c("SeasonStartDate", "SeasonEndDate")) {
                non_definedCriteria[[
                  col_name
                ]] <- .parse_season_date(non_definedCriteria[[col_name]])
              } else {
                non_definedCriteria[[
                  col_name
                ]] <- suppressWarnings(as.Date(non_definedCriteria[[col_name]]))
              }
            } else {
              non_definedCriteria[[
                col_name
              ]] <- as.character(non_definedCriteria[[col_name]])
            }
          }

          # Coerce definedCriteria if it has rows
          if (nrow(definedCriteria) > 0) {
            if (identical(target_class, "numeric")) {
              definedCriteria[[
                col_name
              ]] <- suppressWarnings(as.numeric(definedCriteria[[col_name]]))
            } else if (identical(target_class, "Date")) {
              if (col_name %in% c("SeasonStartDate", "SeasonEndDate")) {
                definedCriteria[[
                  col_name
                ]] <- .parse_season_date(definedCriteria[[col_name]])
              } else {
                definedCriteria[[
                  col_name
                ]] <- suppressWarnings(as.Date(definedCriteria[[col_name]]))
              }
            } else {
              definedCriteria[[col_name]] <- as.character(definedCriteria[[
                col_name
              ]])
            }
          }
        }
      })

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
        dplyr::any_of(desired_cols) # NOTE: 12/16/25 changed from dplyr::select to relocate. Allow additional columns from user supplied table.
      )
    }

    # now, if a user originally supplied a MLSummaryRef, filter the dataframe back to only the relevant TADA.ComparableDataIdentifier in their reviewed MLSummaryRef
    if (!is.null(MLSummary_params)) {
      DefineCriteriaMethodology <- DefineCriteriaMethodology |>
        dplyr::filter(TADA.ComparableDataIdentifier %in% MLSummary_params)
    }

    # Display all unique TADA.ComparableDataIdentifier in the Criteria Methods list or not.
    # Helps a user identifies all WQP data if they do not fill out the reference tables when TRUE
    # FALSE is recommended if a user has gone through a step by step review process to
    # determine what they would like summarized in their final output.
    if (displayUniqueId == FALSE) {
      message(paste0(
        "TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. ",
        "Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."
      ))

      DefineCriteriaMethodology <- DefineCriteriaMethodology |>
        dplyr::mutate(TADA.ComparableDataIdentifier = NA_character_) |>
        dplyr::arrange(
          ATTAINS.OrganizationIdentifier != "USEPA",
          ATTAINS.OrganizationIdentifier,
          ATTAINS.UseName
        ) |>
        dplyr::distinct()
    }
  }

  # User wants to populate the Criteria table using the EPA304(a) criteria
  # joins the EPA304(a) criteria to the current Criteria Table.
  # safe guard when org_id can be NULL in the "all arguments are blank" branch
  if (!is.null(org_id) && "USEPA" %in% org_id) {
    message("TADA_DefineCriteriaMethodology: USEPA was included ...")
    epa304a <- utils::read.csv(
      system.file("extdata", "EPA304a_criteria_table.csv", package = "EPATADA"),
      fileEncoding = "UTF-8-BOM"
    )
    if (displayUniqueId && !missing(.data)) {
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
      dplyr::bind_rows(epa304a) |>
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

    # identify dups with NA to remove
    DefineCriteriaMethodology_dups <- DefineCriteriaMethodology |>
      dplyr::distinct(dplyr::across(-ATTAINS.UseName), .keep_all = TRUE) |>
      dplyr::filter(is.na(ATTAINS.UseName))

    # remove dups and create final criteria table
    DefineCriteriaMethodology <- DefineCriteriaMethodology |>
      dplyr::filter(!is.na(ATTAINS.UseName)) |>
      dplyr::bind_rows(DefineCriteriaMethodology_dups)
  }

  # ensure the first n columns are shown in TADA criteria table format. Additional columns are allowed for notes etc.
  DefineCriteriaMethodology <- dplyr::relocate(
    DefineCriteriaMethodology,
    dplyr::any_of(desired_cols) # NOTE: 12/16/25 changed from dplyr::select to relocate. Allow additional columns from user supplied table.
  )

  # Generates the excel function (HIGHLY Recommended for users to export)
  if (excel == TRUE) {
    # get downloads path
    downloads_path <- get_downloads_path("CriteriaMethodology.xlsx")

    # create a brand new workbook and decide on save path at the end.
    wb <- openxlsx::createWorkbook()

    # if the sheets exist, remove them then re-add them. Must do so to avoid stacking data validation rules.
    tryCatch(
      openxlsx::addWorksheet(wb, "DefineCriteriaMethodology"),
      error = function(e) {
        openxlsx::removeWorksheet(wb, "DefineCriteriaMethodology")
        openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
      }
    )

    tryCatch(openxlsx::addWorksheet(wb, "Index-Criteria"), error = function(e) {
      openxlsx::removeWorksheet(wb, "Index-Criteria")
      openxlsx::addWorksheet(wb, "Index-Criteria")
    })

    # Set visibility
    sv <- openxlsx::sheetVisibility(wb)
    sn <- names(wb)

    idx_dcm <- which(sn == "DefineCriteriaMethodology")
    if (length(idx_dcm) == 1) {
      sv[idx_dcm] <- "visible"
    }

    idx_ic <- which(sn == "Index-Criteria")
    if (length(idx_ic) == 1) {
      sv[idx_ic] <- "hidden"
    }

    openxlsx::sheetVisibility(wb) <- sv

    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")

    # Set zoom size (avoid free-variable scoping)
    set_zoom <- function(sheet_view_xml, zoom) {
      gsub('(?<=zoomScale=")[0-9]+', zoom, sheet_view_xml, perl = TRUE)
    }
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(sV, "90")
    }

    # Export DefineCriteriaMethodology dataframe into the excel spreadsheet tab
    openxlsx::writeData(
      wb,
      "DefineCriteriaMethodology",
      startCol = 1,
      x = DefineCriteriaMethodology,
      headerStyle = header_st
    )

    # Format column widths
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

    # Apply a month-day display format for season dates
    date_style <- openxlsx::createStyle(numFmt = "mmm dd")
    date_cols <- which(
      names(DefineCriteriaMethodology) %in%
        c("SeasonStartDate", "SeasonEndDate")
    )
    if (length(date_cols) > 0) {
      openxlsx::addStyle(
        wb,
        "DefineCriteriaMethodology",
        date_style,
        rows = 2:(nrow(DefineCriteriaMethodology) + 1),
        cols = date_cols,
        gridExpand = TRUE
      )
    }

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
    required_idx_cols <- c(
      "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName"
    )

    idx_df <- if (missing(.data)) {
      # fallback already created above when .data is missing
      .data
    } else {
      # select what exists, then add missing columns as NA, then reorder
      tmp <- .data |> dplyr::select(dplyr::any_of(required_idx_cols))
      miss <- setdiff(required_idx_cols, names(tmp))
      if (length(miss) > 0) {
        tmp[miss] <- NA_character_
      }
      tmp[required_idx_cols]
    }

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 6,
      startRow = 1,
      x = unique(idx_df)
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
    load(system.file(
      "extdata",
      "ATTAINSParamUseOrgRef.rda",
      package = "EPATADA"
    ))
    All.WaterTypeList <- ATTAINSParamUseOrgRef

    Org.WaterTypeList <- dplyr::filter(
      All.WaterTypeList,
      ATTAINS.OrganizationIdentifier %in% org_id
    )

    wt <- unique(Org.WaterTypeList$ATTAINS.WaterType)
    if (length(wt) == 0) {
      wt <- NA_character_
    }
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 10,
      startRow = 1,
      x = data.frame(ATTAINS.WaterType = wt)
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

    units_vec <- if (
      !missing(.data) && "TADA.ResultMeasure.MeasureUnitCode" %in% names(.data)
    ) {
      unique(.data$TADA.ResultMeasure.MeasureUnitCode)
    } else {
      NA_character_
    }
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 18,
      startRow = 1,
      x = data.frame(MagnitudeUnit = units_vec)
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

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 32, # AF
      startRow = 1,
      x = data.frame(
        EquationType = c(
          "Hardness",
          "pH",
          "pH and Temperature",
          "pH and Hardness"
        )
      )
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 33, # AG
      startRow = 1,
      x = data.frame(pHDirection = c("Above", "Below", "NA"))
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      startCol = 34, # AH
      startRow = 1,
      x = data.frame(TemperatureExtreme = c("Min", "Max", "NA"))
    )

    # Build an allowed UseName list (non-NA) from the table you’re writing
    # If none are available, you can substitute an org-specific list as a fallback.
    use_list <- sort(unique(stats::na.omit(
      DefineCriteriaMethodology$ATTAINS.UseName
    )))
    # Assuming ATTAINSParamUseOrgRef is still in scope from earlier load:
    if (length(use_list) == 0 && exists("ATTAINSParamUseOrgRef")) {
      use_list <- sort(unique(ATTAINSParamUseOrgRef$ATTAINS.UseName[
        ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier %in% org_id
      ]))
    }
    if (length(use_list) > 0) {
      openxlsx::writeData(
        wb,
        "Index-Criteria",
        startCol = 17,
        startRow = 1, # Q
        x = data.frame(ATTAINS.UseName = use_list)
      )
    }

    # ParameterName (apply validation to column 2)
    sheets <- names(wb)
    if (!("Index" %in% sheets)) {
      param_list <- sort(unique(stats::na.omit(
        DefineCriteriaMethodology$ATTAINS.ParameterName
      )))
      openxlsx::writeData(
        wb,
        "Index-Criteria",
        startCol = 16,
        startRow = 1, # P
        x = data.frame(ATTAINS.ParameterName = param_list)
      )
      param_len <- length(param_list)
      param_validation_ref <- sprintf(
        "'Index-Criteria'!$P$2:$P$%d",
        param_len + 1L
      )
    } else {
      param_validation_ref <- "'Index'!$E$2:$E$60000"
    }

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 2,
      rows = 2:1000,
      type = "list",
      value = param_validation_ref,
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    # UseName (FIXED: apply to column 3; point to the UseName list we just wrote in column Q)
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 3, # ATTAINS.UseName
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$Q$2:$Q$%d", length(use_list) + 1L), # avoids excess blank items in the dropdown
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    # TADA.ComparableDataIdentifier (FIXED: apply to column 4)
    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 4, # TADA.ComparableDataIdentifier
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

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 32, # EquationType
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AF$2:$AF$5",
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 35, # pHDirection
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AG$2:$AG$4",
      allowBlank = TRUE,
      showErrorMsg = TRUE,
      showInputMsg = TRUE
    ))

    suppressWarnings(openxlsx::dataValidation(
      wb,
      sheet = "DefineCriteriaMethodology",
      cols = 40, # TemperatureExtreme
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AH$2:$AH$4",
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
      cols = 1:ncol(DefineCriteriaMethodology),
      rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb,
      "DefineCriteriaMethodology",
      cols = 1:ncol(DefineCriteriaMethodology),
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

    # Determine actual save path
    save_path <- downloads_path

    # If overwrite = F, check if original exists yet. If not, save it as an original and create a copy.
    if (!isTRUE(overwrite)) {
      if (!file.exists(downloads_path)) {
        openxlsx::activeSheet(wb) <- "DefineCriteriaMethodology"
        openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)
        message(
          "TADA_DefineCriteriaMethodology: ",
          "overwrite = F selected but no original CriteriaMethodology.xlsx was found. Creating original version as well as a copy with timestamp."
        )
        wb <- openxlsx::loadWorkbook(downloads_path)
      }
      if (file.exists(downloads_path)) {
        base <- tools::file_path_sans_ext(downloads_path)
        ext <- tools::file_ext(downloads_path)
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        save_path <- sprintf("%s_%s.%s", base, ts, ext)
      }
    }

    # Save current workbook structure first so file exists at final path
    openxlsx::saveWorkbook(wb, save_path, overwrite = TRUE)

    # Add dictionary tabs to the final file
    .TADA_CriteriaDataDictionary(save_path)

    # Reload the updated workbook so wb now includes those tabs
    wb <- openxlsx::loadWorkbook(save_path)

    # Make "DefineCriteriaMethodology" the active sheet
    if ("activeSheet" %in% getNamespaceExports("openxlsx")) {
      openxlsx::activeSheet(wb) <- "DefineCriteriaMethodology"
    }

    # now continue any remaining edits if needed, then final save
    openxlsx::saveWorkbook(wb, save_path, overwrite = TRUE)

    if (!overwrite && save_path != downloads_path) {
      message("Saved as: ", save_path)
    }

    cat("File saved to:", gsub("/", "\\\\", save_path), "\n")
  }

  DefineCriteriaMethodology <- suppressWarnings(TADA_CorrectColType(
    DefineCriteriaMethodology
  ))
  return(DefineCriteriaMethodology)
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
#' @param downloads_path A character string to define the location of the
#' 'CriteriaMethodology.xlsx' file to include the data dictionary. Default is
#' null to find the path in the Downloads folder path.
#'
#' @return No return value; called for its side effects of creating or updating
#'   an Excel workbook in the downloads_path. The function writes or refreshes:
#'   - "DataDictionary" worksheet with columns:
#'     ColumnName, Requirement, Source, ColumnType, Description.
#'   - "AllowableValues" worksheet with columns:
#'     ColumnName, ColumnType, AllowableValues, ExampleValues.
#'
#' @seealso [TADA_DefineCriteriaMethodology()] [TADA_ParametersForAnalysis()]
#'
#' @examples
#' # Example 1: Write to a temporary path (recommended for reproducible scripts/tests)
#' tmp_xlsx <- file.path(tempdir(), "CriteriaMethodology.xlsx")
#' .TADA_CriteriaDataDictionary(tmp_xlsx)
#'
#' # Inspect created sheet names
#' openxlsx::getSheetNames(tmp_xlsx)
#'
#' # Example 2: Use the default Downloads location (may vary by OS/user)
#' # \dontrun{
#' # .TADA_CriteriaDataDictionary()
#' # }
#'
.TADA_CriteriaDataDictionary <- function(downloads_path = NULL) {
  if (is.null(downloads_path)) {
    # get downloads path
    downloads_path <- get_downloads_path("CriteriaMethodology.xlsx")
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
      "AssessPeriod",
      "AssessPeriodStartDate",
      "AssessPeriodEndDate",
      "Season",
      "SeasonStartDate",
      "SeasonEndDate",
      "DistrCount",
      "DistrPeriod",
      "DistrMinSample",
      "Notes",
      "EquationType",
      "EquationFormula",
      "pHThreshold",
      "pHDirection",
      "hardness_param_1",
      "hardness_param_2",
      "hardness_param_3",
      "hardness_param_4",
      "TemperatureExtreme",
      "pH_param_1",
      "pH_param_2",
      "pH_param_3",
      "pH_param_4",
      "pH_param_5",
      "pH_param_6",
      "pH_param_7",
      "pH_param_8",
      "pH_param_9",
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
          "It is recommended to have performed this crosswalk in TADA_ParametersForAnalysis to avoid any duplicated ",
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
      # AssessPeriod
      "Labels the assessment period of which the WQP data must be collected from. Users should define the assessment date range in the beginning and end date columns that proceeds this one.",
      # AssessPeriodStartDate
      "The start date in which WQP data will be analyzed for this parameter and use.",
      # AssessPeriodEndDate
      "The end date in which WQP data will be analyzed for this parameter and use.",
      # Season
      "Labels the season in which the standards apply for this parameter and use. Specify the start and end dates of your season in the proceeding two columns.",
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
      #TemperatureExtreme
      "For pH and Temperature equations only. Defines if the equation considers the minimum value or maximum value of the temperature component of the equation.",
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
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_6
      paste0(
        c(
          "Sixth coefficient in the typical pH- & temperature-dependent equation format: ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_7
      paste0(
        c(
          "Seventh coefficient in the typical pH- & temperature-dependent equation format: ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_8
      paste0(
        c(
          "Eigth coefficient in the typical pH- & temperature-dependent equation format: ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
          "NOTE: Equation handling in TADA is still in development."
        ),
        collapse = "\r\n"
      ),
      # pH_param_9
      paste0(
        c(
          "Ninth coefficient in the typical pH- & temperature-dependent equation format: ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*min(param_6, param_7*10^(param_8*(param_9-Temperature)))) ",
          "OR ",
          "param_5*[param_1/(1+10^(param_2-pH)) + param_3/(1+10^(param_4-pH))]*(param_6*10^(param_7*(param_8-max(Temperature,param_9))))). ",
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
      "AssessPeriod",
      "AssessPeriodStartDate",
      "AssessPeriodEndDate",
      "Season",
      "SeasonStartDate",
      "SeasonEndDate",
      "DistrCount",
      "DistrPeriod",
      "DistrMinSample",
      "Notes",
      "EquationType",
      "EquationFormula",
      "pHThreshold",
      "pHDirection",
      "hardness_param_1",
      "hardness_param_2",
      "hardness_param_3",
      "hardness_param_4",
      "TemperatureExtreme",
      "pH_param_1",
      "pH_param_2",
      "pH_param_3",
      "pH_param_4",
      "pH_param_5",
      "pH_param_6",
      "pH_param_7",
      "pH_param_8",
      "pH_param_9",
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
      "Equation"
    ),
    AllowableValues = c(
      "https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx",
      "https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx",
      "https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx",
      "",
      "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV",
      "",
      "",
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
      "Last 30 years; Last 10 years; Last 5 years; Last 3 years; Last year; NA",
      "",
      "",
      "Summer; Fall; Spring; Winter",
      "",
      "",
      "",
      "Seasonal; Annual; Semi-Annual; Quarterly; Monthly; Bi-weekly; Weekly; 10 days; NA",
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
      "Min; Max; NA",
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
      "",
      "2024-10-01",
      "2025-09-30",
      "",
      "Apr 01",
      "Jul 15",
      "5",
      "",
      "10",
      "New addition to ATTAINS in FY2026",
      "",
      paste0(
        c(
          "Hardness: 1.101672 - ln(hardness) (0.041838) * e(0.7977*ln(hardness)-3.909) ",
          "OR ",
          "pH: 0.275/(1+10^(7.204-pH)) + 39/(1+10^(pH-7.204)) ",
          "OR ",
          "pH-Temp: 0.8876*(((0.0278/(1+10^(7.688-pH)))+(1.1994/(1+10^(pH-7.688))))*(2.126*10^(0.028*(20-max(Temperature,7)))))",
          "OR ",
          "pH-Temp: 0.7249*(((0.0114/(1+10^(7.204-pH)))+(1.6181/(1+10^(pH-7.204))))*min(51.93,23.12*10^(0.036*(20-Temperature))))",
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
      "Link"
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
