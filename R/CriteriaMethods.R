#' Criteria and Methodology Template
#'
#' Assessment criteria and methodologies used to evaluate water quality vary across the nation.
#' TADA users can use this template to define the criteria and methodologies for each
#' parameter-use combination of interest for their organization.
#'
#' The table can be created manually, auto-populated from ATTAINS and the Criteria Search Tool (CST)
#' using the input WQP data frame, or built from a prefilled user-supplied criteria table template
#' or MLSummaryRef generated through the recommended Module 2 workflow using, in order:
#' [TADA_CrosswalkWQPATTAINSParam()], [TADA_CrosswalkCSTATTAINSUses()],
#' [TADA_CreateParamUseRef()], and [TADA_MLSummary()].
#'
#' The output is in a TADA compatible format and includes allowable values for each column.
#' Users may provide full criteria/methodology information or only magnitude values.
#' If criteria differ by acute vs. chronic condition, waterbody type, season, or other factors,
#' additional rows should be created for each unique case. Additional columns capture data
#' sufficiency information such as minimum sample size, assessment period dates, and seasonality.
#'
#' Allowable values for `ATTAINS.UseName`, `ATTAINS.ParameterName`, and
#' `ATTAINS.OrganizationIdentifier` can be retrieved with:
#' \preformatted{
#' ATTAINS.uses <- rExpertQuery::EQ_DomainValues("use_name", api_key = .setEQKey())
#' ATTAINS.parameters <- rExpertQuery::EQ_DomainValues("param_name", api_key = .setEQKey())
#' ATTAINS.organizations <- rExpertQuery::EQ_DomainValues("org_id", api_key = .setEQKey())
#' }
#'
#' @param .data A TADA data frame. Run all desired cleaning, processing, harmonization,
#' filtering, and censored-data handling before using this function.
#'
#' @param org_id ATTAINS organization identifier. `"USEPA"` may be used to populate EPA 304(a)
#' recommended criteria for matching `TADA.CharacteristicName` values. `"All"` returns all
#' prior ATTAINS information across organizations as separate rows. `"NULL"` returns all unique
#' prior ATTAINS information without organization labels for manual editing.
#'
#' @param criteriaMethods Optional user-supplied criteria and methodology table. Supplied values
#' take priority. Any missing parameter/use combinations from the input data will be added and
#' must be completed manually if analysis is desired.
#'
#' @param MLSummaryRef Optional spatial crosswalk used to assign unique spatial criteria to a
#' parameter, use, waterbody, or monitoring site/assessment unit. If supplied, it must contain:
#' `"ATTAINS.OrganizationIdentifier"`, `"ATTAINS.AssessmentUnitIdentifier"`,
#' `"MonitoringLocationIdentifier"`, `"MonitoringLocationTypeName"`,
#' `"TADA.ComparableDataIdentifier"`, `"ATTAINS.ParameterName"`, `"ATTAINS.UseName"`,
#' `"ATTAINS.WaterType"`, `"SaltFresh"`, `"DepthCategory"`, `"LongitudeMeasure"`,
#' `"LatitudeMeasure"`, `"IncludeOrExclude"`, and `"UniqueSpatialCriteria"`.
#' When supplied and `displayUniqueId` is not specified, IDs are retained by default.
#'
#' @param auto_assign Logical; default `FALSE`. If `TRUE`, a draft criteria/methods table is
#' generated using default assignment through
#' [TADA_CrosswalkWQPATTAINSParam()], [TADA_CrosswalkCSTATTAINSUses()],
#' [TADA_CreateParamUseRef()], and [TADA_MLSummary()]. Requires `.data` and `org_id`.
#'
#' @param displayUniqueId Logical. If `TRUE`, unique `TADA.ComparableDataIdentifier` values
#' are shown in the output to define criteria and methodology.
#'
#' @param excel Logical; if `TRUE`, writes an Excel workbook to the user's Downloads folder.
#' The file is named `CriteriaMethodology.xlsx` unless `overwrite = FALSE`, in which case a
#' timestamped copy is created. The workbook includes highlighted input cells.
#'
#' @param overwrite Logical. If `TRUE`, overwrite the Excel file if it already exists.
#'
#' @return A list containing: (1) the criteria and methodology table in TADA format,
#' (2) the Data Dictionary table, and (3) the Allowable Values table.
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # empty dataframe with colnames
#' blank_criteria <- TADA_DefineCriteriaMethodology()
#'
#' # criteria table with all unique TADA.ComparableDataIdentifier from WQP data
#' UT_criteria_comparables <- TADA_DefineCriteriaMethodology(
#'   Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", excel = TRUE
#'  )[[1]]
#'
#' # auto assign param and uses crosswalk method
#' UT_criteria_auto <- TADA_DefineCriteriaMethodology(
#'   Data_MT_MissoulaCounty,
#'   org_id = "MTDEQ",
#'   auto_assign = TRUE
#'  )[[1]]
#'
#' # retrieve criteria table from the TADACommunityHub
#' UT_criteria_CommunityHub <- TADA_GetCriteriaFile(org_id = "UTAHDWQ")
#'
#' # provide and filters a user supplied criteria table using TADACommunityHub file
#' # to only relevant param/characteristics in their dataframe
#' UT_criteria_user <- TADA_DefineCriteriaMethodology(
#'   Data_MT_MissoulaCounty,
#'   org_id = "UTAHDWQ",
#'   criteriaMethods = UT_criteria_CommunityHub
#'  )[[1]]
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
  harmonizeSynonyms = FALSE,
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
    message(
      "All arguments are blank, returning an empty dataframe with column names only."
    )
    return(blank_criteria)
  }

  if (!is.logical(auto_assign)) {
    stop("TADA_DefineCriteriaMethodology: auto_assign must be TRUE/FALSE.")
  }

  if (is.null(org_id)) {
    org_id <- character(0)
  }

  # ------------------------------------------------------------
  # 1) Build blank criteria with unique comparable identifiers
  # ------------------------------------------------------------
  unique_chars <- .data |>
    dplyr::select(TADA.ComparableDataIdentifier, TADA.CharacteristicName) |>
    dplyr::distinct()

  unique_chars <- unique_chars |>
    tidyr::crossing(ATTAINS.OrganizationIdentifier = org_id)

  # ------------------------------------------------------------
  # 2) If auto_assign = TRUE, build MLSummaryRef with CST-based summary
  # ------------------------------------------------------------
  if (isTRUE(auto_assign)) {
    # if no MLSummaryRef is provided, create it with defaults
    if (is.null(MLSummaryRef)) {
      message(paste0(
        "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected but no MLSummaryRef. Generating TADA_MLSummary with default assignment."
      ))

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

      # correct column types for any empty columns and add on all unique TADA.CharacteristicName
      MLSummaryRef <- TADA_CorrectColType(MLSummaryRef) |>
        dplyr::right_join(
          unique_chars,
          by = c(
            "ATTAINS.OrganizationIdentifier",
            "TADA.ComparableDataIdentifier"
          )
        )
    }
  }
  # ------------------------------------------------------------
  # 3) If MLSummaryRef contains CST.PollutantName (from user supplied table or auto_assign)
  # Note: user do not need to provide the CST.PollutantName crosswalk.
  # ------------------------------------------------------------
  if (!is.null(MLSummaryRef) && "CST.PollutantName" %in% names(MLSummaryRef)) {
    ATTAINSOrgToCSTEntityRef <- utils::read.csv(
      system.file(
        "extdata",
        "ATTAINSOrgToCSTEntityRef.csv",
        package = "EPATADA"
      ),
      stringsAsFactors = FALSE
    )

    internal_path <- system.file(
      "extdata",
      "cst-workbook.xlsx",
      package = "EPATADA"
    )
    if (!nzchar(internal_path) || !file.exists(internal_path)) {
      stop(
        "Internal CST workbook is missing: inst/extdata/cst-workbook.xlsx. ",
        "Please add this file to the EPATADA package."
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

    CriteriaSearchToolRef <- .tada_cst_prepare_table(CriteriaSearchToolRef)
    CriteriaSearchToolRef_Legend <- .tada_cst_prepare_table(
      CriteriaSearchToolRef_Legend
    )
    CriteriaSearchToolRef_Sources <- .tada_cst_prepare_table(
      CriteriaSearchToolRef_Sources
    )

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
      dplyr::mutate(
        CST.SourceLink = paste0(SOURCE, "#page=", as.character(PDFPGNO))
      ) |>
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
  # 4) If MLSummaryRef is supplied, use it. Else, return empty criteria table
  # ------------------------------------------------------------
  if (!is.null(MLSummaryRef)) {
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
    optional_cols <- c(
      "ATTAINS.WaterType",
      "SaltFresh",
      "DepthCategory",
      "UniqueSpatialCriteria"
    )
    for (col in optional_cols) {
      if (!col %in% names(MLSummaryRef)) {
        MLSummaryRef[[col]] <- NA_character_
      }
    }

    DefineCriteriaMethodology <- MLSummaryRef |>
      TADA_CorrectColType() |>
      dplyr::bind_rows(blank_criteria) |>
      dplyr::right_join(
        unique_chars,
        c(
          "ATTAINS.OrganizationIdentifier",
          "TADA.ComparableDataIdentifier",
          "TADA.CharacteristicName"
        )
      ) |>
      dplyr::select(dplyr::any_of(c(
        desired_cols,
        "CST.PollutantName",
        "CST.UseName",
        "CST.SourceLink"
      ))) |>
      dplyr::distinct()
  } else {
    DefineCriteriaMethodology <- unique_chars |>
      dplyr::bind_rows(blank_criteria)
  }

  # ------------------------------------------------------------
  # 5) If criteriaMethods is supplied, merge it in and prioritize
  # ------------------------------------------------------------
  if (!is.null(criteriaMethods)) {
    if (!is.null(MLSummaryRef)) {
      stop(
        "only one of MLSummaryRef or criteriaMethods is allowed as an input."
      )
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

    # drop "Flag.UserSuppliedCriteriaMatch" if it exists
    criteriaMethods2 <- criteriaMethods |>
      TADA_CorrectColType() |>
      dplyr::select(dplyr::any_of(c(
        desired_cols,
        "CST.PollutantName",
        "CST.UseName",
        "CST.SourceLink"
      ))) |>
      dplyr::distinct()

    if (harmonizeSynonyms) {
      message(
        "harmonizeSynonyms = TRUE: harmonizing your user supplied criteria table with characteristic, fraction and speciation name",
        "to match those from your WQP data."
      )
      # create the synonym table with values from the WQP .data (these should all have the correct targets)
      criteria_synonym <- TADA_GetSynonymRef(.data)

      # extract unique char, frac, speciation and units from WQP data
      unique_combos <- dplyr::select(
        .data,
        "TADA.CharacteristicName",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName",
        "TADA.ResultMeasure.MeasureUnitCode"
      ) |>
        dplyr::mutate(TADA.ResultMeasureValue = NA) |>
        dplyr::distinct()

      # join criteria table with unique combos, identifying the target char, frac and speciation
      criteria_harmonized_w_units <- criteriaMethods |>
        TADA_CorrectColType() |>
        dplyr::left_join(
          unique_combos,
          by = c(
            "TADA.CharacteristicName",
            "TADA.ResultSampleFractionText",
            "TADA.MethodSpeciationName"
          )
        )

      # harmonize the criteria table with the desired target char, frac and speciation
      criteria_harmonized <- TADA_HarmonizeSynonyms(
        .data = criteria_harmonized_w_units,
        ref = criteria_synonym
      )

      # reformat, filter by only chars found in .data
      criteriaMethods <- criteria_harmonized |>
        dplyr::filter(
          TADA.CharacteristicName %in% .data$TADA.CharacteristicName
        ) |>
        dplyr::select(dplyr::any_of(c(desired_cols))) |>
        dplyr::distinct()
    }

    criteriaMethods2 <- criteriaMethods |>
      dplyr::right_join(
        .data,
        by = c("TADA.CharacteristicName"),
        suffix = c(".crit", ".data")
      ) |>
      dplyr::mutate(
        Flag.UserSuppliedCriteriaMatch = dplyr::case_when(
          !TADA.CharacteristicName %in%
            unique(
              criteriaMethods$TADA.CharacteristicName
            ) ~ "Suspect: Characteristic not found in criteria table.",

          !is.na(TADA.ResultSampleFractionText.crit) &
            !is.na(TADA.MethodSpeciationName.crit) &
            (TADA.ResultSampleFractionText.data !=
              TADA.ResultSampleFractionText.crit |
              TADA.MethodSpeciationName.data !=
                TADA.MethodSpeciationName.crit) ~ "Suspect: Your Criteria Table's fraction and speciation does not match your WQP data for this characteristic name",

          !is.na(TADA.ResultSampleFractionText.crit) &
            is.na(TADA.MethodSpeciationName.crit) &
            TADA.ResultSampleFractionText.data !=
              TADA.ResultSampleFractionText.crit ~ "Suspect: Your Criteria Table's fraction does not match your WQP data for this characteristic name",

          is.na(TADA.ResultSampleFractionText.crit) &
            !is.na(TADA.MethodSpeciationName.crit) &
            TADA.MethodSpeciationName.data !=
              TADA.MethodSpeciationName.crit ~ "Suspect: Your Criteria Table's speciation does not match your WQP data for this characteristic name",

          TRUE ~ "Pass: Matched criteria combination"
        ),
        TADA.ResultSampleFractionText = TADA.ResultSampleFractionText.crit,
        TADA.MethodSpeciationName = TADA.MethodSpeciationName.crit,
        TADA.ComparableDataIdentifier = TADA.ComparableDataIdentifier.data
      ) |>
      dplyr::select(dplyr::any_of(c(
        desired_cols,
        "CST.PollutantName",
        "CST.UseName",
        "CST.SourceLink",
        "Flag.UserSuppliedCriteriaMatch"
      ))) |>
      dplyr::distinct()

    DefineCriteriaMethodology <- criteriaMethods2 |>
      TADA_CorrectColType() |>
      dplyr::bind_rows(blank_criteria) |>
      dplyr::select(-ATTAINS.OrganizationIdentifier) |> # drop ATTAINS.OrganizationIdentifier and re-add from unique_chars
      dplyr::right_join(
        unique_chars,
        by = c("TADA.ComparableDataIdentifier", "TADA.CharacteristicName")
      ) |>
      dplyr::select(dplyr::any_of(c(
        desired_cols,
        "CST.PollutantName",
        "CST.UseName",
        "CST.SourceLink",
        "Flag.UserSuppliedCriteriaMatch"
      ))) |>
      dplyr::distinct()
  }

  # ------------------------------------------------------------
  # 6) Final cleanup
  # ------------------------------------------------------------
  # if CST crosswalk provided, filter out those that cannot be analyzed with TADA
  if (
    "CST.PollutantName" %in%
      names(DefineCriteriaMethodology) &&
      any(
        DefineCriteriaMethodology$CST.PollutantName %in%
          c("PH VARIATION", "TEMPERATURE RISE ABOVE AMBIENT"),
        na.rm = TRUE
      )
  ) {
    message(paste(
      "TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'.",
      "TADA functions cannot currently handle analysis for these instances."
    ))

    DefineCriteriaMethodology <- DefineCriteriaMethodology |>
      dplyr::filter(
        !CST.PollutantName %in%
          c("PH VARIATION", "TEMPERATURE RISE ABOVE AMBIENT")
      )
  }

  # convert units to match WQP data
  DefineCriteriaMethodology <- .TADA_Criteria_Convert_Unit(
    .data,
    DefineCriteriaMethodology = DefineCriteriaMethodology
  ) |>
    dplyr::select(dplyr::any_of(c(
      desired_cols,
      "CST.PollutantName",
      "CST.UseName",
      "CST.SourceLink",
      "Flag.UserSuppliedCriteriaMatch"
    ))) |>
    TADA_CorrectColType()

  # if displayUniqueID = FALSE, convert TADA.ComparableDataIdentifier to NA. Only TADA.CharacteristicName is shown now
  if (isFALSE(displayUniqueId)) {
    DefineCriteriaMethodology <- DefineCriteriaMethodology |>
      dplyr::mutate(TADA.ComparableDataIdentifier = NA_character_) |>
      dplyr::distinct()
  }

  # ------------------------------------------------------------
  # 7) Excel export
  # ------------------------------------------------------------
  # Generates the Excel file (recommended)
  if (isTRUE(excel)) {
    downloads_path <- invisible(.get_downloads_path("CriteriaMethodology.xlsx"))
    wb <- openxlsx::createWorkbook()

    for (sh in c("DefineCriteriaMethodology", "Index-Criteria")) {
      if (sh %in% names(wb)) {
        openxlsx::removeWorksheet(wb, sh)
      }
      openxlsx::addWorksheet(wb, sh)
    }

    sv <- openxlsx::sheetVisibility(wb)
    sv[names(wb) == "DefineCriteriaMethodology"] <- "visible"
    sv[names(wb) == "Index-Criteria"] <- "hidden"
    openxlsx::sheetVisibility(wb) <- sv

    header_st <- openxlsx::createStyle(textDecoration = "bold")
    openxlsx::writeData(
      wb,
      "DefineCriteriaMethodology",
      DefineCriteriaMethodology,
      headerStyle = header_st
    )

    openxlsx::setColWidths(
      wb,
      "DefineCriteriaMethodology",
      cols = 1:ncol(DefineCriteriaMethodology),
      widths = "auto"
    )
    openxlsx::setColWidths(
      wb,
      "DefineCriteriaMethodology",
      cols = 1:5,
      widths = 20
    )

    date_cols <- which(
      names(DefineCriteriaMethodology) %in%
        c("SeasonStartDate", "SeasonEndDate")
    )
    if (length(date_cols)) {
      openxlsx::addStyle(
        wb,
        "DefineCriteriaMethodology",
        openxlsx::createStyle(numFmt = "mmm dd"),
        rows = 2:(nrow(DefineCriteriaMethodology) + 1),
        cols = date_cols,
        gridExpand = TRUE
      )
    }

    # --- Index-Criteria sheet ---
    if (missing(.data)) {
      .data <- data.frame(
        TADA.ComparableDataIdentifier = NA_character_,
        TADA.CharacteristicName = NA_character_,
        TADA.ResultSampleFractionText = NA_character_,
        TADA.MethodSpeciationName = NA_character_,
        TADA.ResultMeasure.MeasureUnitCode = NA_character_
      )
    }

    required_idx_cols <- c(
      "TADA.ComparableDataIdentifier",
      "TADA.CharacteristicName",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName"
    )

    idx_df <- if (missing(.data)) {
      .data
    } else {
      tmp <- .data |> dplyr::select(dplyr::any_of(required_idx_cols))
      miss <- setdiff(required_idx_cols, names(tmp))
      if (length(miss)) {
        tmp[miss] <- NA_character_
      }
      tmp[required_idx_cols]
    }

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = unique(idx_df),
      startCol = 6,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(AcuteChronic = c("A", "C", "NA")),
      startCol = 14,
      startRow = 1
    )

    load(system.file(
      "extdata",
      "ATTAINSParamUseOrgRef.rda",
      package = "EPATADA"
    ))
    wt <- unique(ATTAINSParamUseOrgRef$ATTAINS.WaterType[
      ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier %in% org_id
    ])
    if (!length(wt)) {
      wt <- NA_character_
    }
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(ATTAINS.WaterType = wt),
      startCol = 10,
      startRow = 1
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(SaltFresh = c("S", "F", "NA")),
      startCol = 11,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(
        DepthCategory = c(
          "No depth info",
          "Epilimnion-surface",
          "Surface",
          "Bottom",
          "Middle"
        )
      ),
      startCol = 12,
      startRow = 1
    )

    if (is.null(MLSummaryRef)) {
      MLSummaryRef <- data.frame(UniqueSpatialCriteria = NA_character_)
    }
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(
        UniqueSpatialCriteria = c(
          unique(MLSummaryRef$UniqueSpatialCriteria),
          "NA"
        )
      ),
      startCol = 13,
      startRow = 1
    )

    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(EquationBased = c("Yes", "No", "NA")),
      startCol = 15,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(
        MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode)
      ),
      startCol = 18,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(
        DurationUnit = c("n-hour", "n-day", "n-week", "n-month", "n-quarter")
      ),
      startCol = 20,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
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
          "mean of daily minima",
          "mean of daily maxima"
        )
      ),
      startCol = 21,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
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
      ),
      startCol = 23,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
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
      ),
      startCol = 31,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(
        EquationType = c(
          "Hardness",
          "pH",
          "pH and Temperature",
          "pH and Hardness"
        )
      ),
      startCol = 32,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(pHDirection = c("Above", "Below", "NA")),
      startCol = 33,
      startRow = 1
    )
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(
        AmmoniaEqType = c("Min Multiplier", "Max Exponent", "Overall")
      ),
      startCol = 34,
      startRow = 1
    )

    use_list <- sort(unique(stats::na.omit(
      DefineCriteriaMethodology$ATTAINS.UseName
    )))
    if (length(use_list) == 0 && exists("ATTAINSParamUseOrgRef")) {
      use_list <- sort(unique(ATTAINSParamUseOrgRef$ATTAINS.UseName[
        ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier %in% org_id
      ]))
    }
    if (length(use_list)) {
      openxlsx::writeData(
        wb,
        "Index-Criteria",
        x = data.frame(ATTAINS.UseName = use_list),
        startCol = 17,
        startRow = 1
      )
    }

    param_list <- sort(unique(stats::na.omit(
      DefineCriteriaMethodology$ATTAINS.ParameterName
    )))
    openxlsx::writeData(
      wb,
      "Index-Criteria",
      x = data.frame(ATTAINS.ParameterName = param_list),
      startCol = 16,
      startRow = 1
    )

    # validations
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 2,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$P$2:$P$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 3,
      rows = 2:1000,
      type = "list",
      value = sprintf("'Index-Criteria'!$Q$2:$Q$%d", length(use_list) + 1L)
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 4,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$F$2:$F$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 6,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$H$2:$H$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 7,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$I$2:$I$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 8,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$J$2:$J$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 9,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$K$2:$K$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 10,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$L$2:$L$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 11,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$M$2:$M$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 12,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$N$2:$N$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 13,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$O$2:$O$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 16,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$R$2:$R$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 18,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$T$2:$T$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 19,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$U$2:$U$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 21,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$W$2:$W$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 22,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$X$2:$X$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 25,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AA$2:$AA$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 29,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AE$2:$AE$1000"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 32,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AF$2:$AF$5"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 35,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AG$2:$AG$4"
    )
    openxlsx::dataValidation(
      wb,
      "DefineCriteriaMethodology",
      cols = 40,
      rows = 2:1000,
      type = "list",
      value = "'Index-Criteria'!$AH$2:$AH$4"
    )

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
    )
    openxlsx::conditionalFormatting(
      wb,
      "DefineCriteriaMethodology",
      cols = 1:ncol(DefineCriteriaMethodology),
      rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )

    openxlsx::groupColumns(
      wb,
      "DefineCriteriaMethodology",
      cols = 22:ncol(DefineCriteriaMethodology),
      hidden = FALSE,
      level = -1
    )
    openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)
  }

  DefineCriteriaMethodology <- suppressWarnings(TADA_CorrectColType(
    DefineCriteriaMethodology
  ))

  # Otherwise return all supporting tables
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_xlsx), add = TRUE)
  .TADA_CriteriaDataDictionary(tmp_xlsx)

  return(list(
    DefineCriteriaMethodology = DefineCriteriaMethodology,
    DataDictionary = openxlsx::read.xlsx(
      tmp_xlsx,
      sheet = "DataDictionary",
      startRow = 2
    ),
    AllowableValues = openxlsx::read.xlsx(
      tmp_xlsx,
      sheet = "AllowableValues",
      startRow = 2
    )
  ))
}

# internal function to convert criteria table units to ones to match WQP data
.TADA_Criteria_Convert_Unit <- function(.data, DefineCriteriaMethodology) {
  TADAPriorityCharConvertRef <- utils::read.csv(
    system.file(
      "extdata",
      "TADAPriorityCharConvertRef.csv",
      package = "EPATADA"
    ),
    fileEncoding = "UTF-8-BOM"
  ) |>
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_to_upper)) |>
    dplyr::filter(!is.na(Code))

  unitRef <- suppressMessages(suppressWarnings(TADA_CreateUnitRef(.data)))

  .old_opts <- options(scipen = 999)
  on.exit(options(.old_opts), add = TRUE)

  unitRef_CST <- unitRef |>
    dplyr::inner_join(
      dplyr::select(
        DefineCriteriaMethodology,
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
    dplyr::left_join(
      TADAPriorityCharConvertRef,
      by = c("MagnitudeUnit" = "Code")
    ) |>
    dplyr::distinct()

  unitRef_CST_NA <- dplyr::filter(unitRef_CST, is.na(Target.Unit))

  if (nrow(unitRef_CST) > 0) {
    message(paste(
      "Warning in TADA_DefineCriteriaMethodology:",
      "There are",
      length(unique(unitRef_CST$TADA.CharacteristicName)),
      "TADA.CharacteristicName units that do not match with the CST autoassign MagnitudeUnit values.",
      "Converting these MagnitudeUnit values from CST to match TADA.ResultMeasure.MeasureUnitCode.",
      "Please review these conversions."
    ))
  }

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

  DefineCriteriaMethodology <- suppressWarnings(TADA_CorrectColType(
    DefineCriteriaMethodology
  ))

  DefineCriteriaMethodology <- DefineCriteriaMethodology |>
    dplyr::left_join(
      unitRef_CST,
      by = c("TADA.CharacteristicName", "MagnitudeUnit"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      Conversion.Factor = dplyr::coalesce(Conversion.Factor, 1),
      MagnitudeUnit = dplyr::coalesce(Target.Unit, MagnitudeUnit),
      MagnitudeValueLower = round(
        Conversion.Factor * MagnitudeValueLower,
        digits = 4
      ),
      MagnitudeValueUpper = round(
        Conversion.Factor * MagnitudeValueUpper,
        digits = 4
      ),
      EquationBased = dplyr::if_else(
        dplyr::if_any(
          dplyr::any_of(c("CST.PollutantName", "CST.Use")),
          ~ !is.na(.x)
        ) &
          dplyr::if_all(
            dplyr::any_of(c("MagnitudeValueLower", "MagnitudeValueUpper")),
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
