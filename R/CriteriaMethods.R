#' Define Criteria and Methodology
#'
#' Users will need to provide the completed reference tables from
#' TADA_CreateSpatialRef. This will generate a template for users to fill out
#' and define either the full Criteria or magnitude only values associated with
#' an ATTAINS Parameter name and use name. For each Criteria/Magnitude value,
#' users will need to ensure they properly define any additional methods that will
#' correctly reflect their assessment standards for that parameter and use.
#' For example, if there are separate standards for acute versus chronic,
#' rivers versus estuary, different seasons, etc. then a user will need to create
#' additional rows to reflect this.
#'
#' Efforts have been made to pull in the EPA304a recommended standards
#' automatically from the Criteria Search Tool (CST). Users should validate this
#' final output if a user has decided to include the EPA304a standards.
#' User will need to determine if any additional adjustments are needed.
#' For example, does the crosswalk between the EPA304A.PollutantName
#' and TADA.ComparableDataIdentifier seem valid for your organization's method?
#' Is your organization only interested in providing the EPA304a recommended
#' standards for certain seasons (Fall, Summer, Spring, Winter) etc.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#'
#' @param spatialRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit.
#' 
#' @param ref "TADA"
#'
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#'
#' @export
#'
#' @examples
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
#' # Now, run TADA_CreateSpatialRef()
#' SpatialRef_UT <- TADA_CreateSpatialRef(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   waterUseParamRef = NULL, useAURef = NULL, sitesAURef = NULL,
#'   useParamRef = UseParamRef_UT,
#'   excel = FALSE
#' )
#'
#' DefineCriteriaMethodology_UT <- TADA_DefineCriteriaMethodology(
#'   Data_Nutrients_UT,
#'   spatialRef = SpatialRef_UT,
#'   excel = FALSE
#' )
#'
TADA_DefineCriteriaMethodology <- function(.data, ref = "TADA", spatialRef = NULL,
                                           excel = TRUE, overwrite = FALSE) {
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")

  # check to see if user-supplied parameter ref is a df with appropriate columns and filled out.
  if (!is.null(spatialRef) & !is.character(spatialRef)) {
    if (!is.data.frame(spatialRef)) {
      stop("TADA_DefineCriteriaMethodology: 'spatialRef' must be a data frame with seven columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, ApplyUniqueSpatialCriteria,
        ATTAINS.waterTypeCode, ATTAINS.assessmentunitidentifier, MonitoringLocationTypeName")
    }

    if (is.data.frame(spatialRef)) {
      col.names <- c(
        "ATTAINS.ParameterName",
        "ATTAINS.UseName",
        "ATTAINS.OrganizationIdentifier",
        "ApplyUniqueSpatialCriteria",
        "ATTAINS.waterTypeCode",
        "ATTAINS.assessmentunitidentifier",
        "MonitoringLocationTypeName"
      )

      ref.names <- names(spatialRef)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_DefineCriteriaMethodology: 'spatialRef' must be a data frame with seven columns:
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, ApplyUniqueSpatialCriteria,
        ATTAINS.waterTypeCode, ATTAINS.assessmentunitidentifier, MonitoringLocationTypeName")
      }
    }
  }

  # Handles Dissolved Metals Criteria and Method Splits by Acute/Chronic and Salt/Fresh
  # Need to consider cases in which some orgs may not have separate criteria splits for dissolved metals.
  metal_list <- data.frame(
    ATTAINS.ParameterName = c("ARSENIC", "ZINC")
  ) %>%
    cbind(AcuteChronic = rep(c("Acute", "Chronic", "Acute", "Chronic"), each = 2)) %>%
    cbind(SaltFresh = rep(c("Salt", "Fresh", "Fresh", "Salt"), each = 2)) %>%
    dplyr::arrange(ATTAINS.ParameterName)

  DefineCriteriaMethodology <- spatialRef %>%
    dplyr::select(
      "ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier", "ATTAINS.UseName",
      "MonitoringLocationTypeName", "ApplyUniqueSpatialCriteria", "ATTAINS.waterTypeCode"
    ) %>%
    dplyr::mutate(MonitoringLocationTypeName = dplyr::if_else( # Only include if a unique spatial criteria is applied for
      is.na(ApplyUniqueSpatialCriteria),
      as.character(NA),
      MonitoringLocationTypeName
    )) %>%
    # dplyr::filter(!dplyr::if_all(c(ApplyUniqueSpatialCriteria, ATTAINS.waterTypeCode), is.na)) %>%
    dplyr::bind_cols(
      data.frame(
        AcuteChronic = as.character(NA), SaltFresh = as.character(NA), WaterDepth = as.numeric(NA),
        BegAssessDate = as.Date(NA), EndAssessDate = as.Date(NA), Season = as.character(NA),
        MinimumSample = as.numeric(NA), EquationBased = as.character(NA),
        MagnitudeValueLower = as.character(NA), MagnitudeValueUpper = as.character(NA), MagnitudeUnit = as.character(NA)
      )
    ) %>%
    dplyr::left_join(metal_list, by = ("ATTAINS.ParameterName"), relationship = "many-to-many") %>%
    dplyr::mutate(AcuteChronic = dplyr::coalesce(AcuteChronic.x, AcuteChronic.y)) %>%
    dplyr::select(-c(AcuteChronic.x, AcuteChronic.y)) %>%
    dplyr::mutate(SaltFresh = dplyr::coalesce(SaltFresh.x, SaltFresh.y)) %>%
    dplyr::select(-c(SaltFresh.x, SaltFresh.y)) %>%
    dplyr::distinct() %>%
    dplyr::select(
      "ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier", "ATTAINS.UseName", "MonitoringLocationTypeName",
      "ATTAINS.waterTypeCode", "AcuteChronic", "SaltFresh", "WaterDepth", "BegAssessDate", "EndAssessDate",
      "Season", "MinimumSample", "ApplyUniqueSpatialCriteria",
      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit"
    )

  CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
    dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, ATTAINS.UseName = use_name, CRITERIATYPE_ACUTECHRONIC, CRITERIATYPEFRESHSALTWATER, CRITERION_VALUE, UNIT_NAME) %>%
    dplyr::mutate(ATTAINS.OrganizationIdentifier = "EPA304a")

  if ("EPA304a" %in% DefineCriteriaMethodology$ATTAINS.OrganizationIdentifier) {
    DefineCriteriaMethodology <- DefineCriteriaMethodology %>%
      dplyr::left_join(CST_param, c("EPA304A.PollutantName", "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier"), relationship = "many-to-many") %>%
      dplyr::mutate(AcuteChronic = CRITERIATYPE_ACUTECHRONIC) %>%
      dplyr::mutate(SaltFresh = CRITERIATYPEFRESHSALTWATER) %>%
      dplyr::mutate(MagnitudeValueLower = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE, "-"), stringr::str_extract(CRITERION_VALUE, "[^-]+"),
        ""
      )) %>%
      dplyr::mutate(MagnitudeValueUpper = dplyr::if_else(
        stringr::str_detect(CRITERION_VALUE, "-"), stringr::str_split(CRITERION_VALUE, "-", simplify = TRUE)[, 2],
        CRITERION_VALUE
      )) %>%
      dplyr::mutate(dplyr::across(c(MagnitudeValueLower, MagnitudeValueUpper), as.numeric)) %>%
      dplyr::mutate(dplyr::across(
        c(
          ATTAINS.waterTypeCode,
          MonitoringLocationTypeName,
          AcuteChronic, SaltFresh, Season, EquationBased,
          ApplyUniqueSpatialCriteria, # Will depend on the user's crosswalk of ML to this criteria for filtering.
        ), as.factor
      )) %>%
      dplyr::mutate(MagnitudeUnit = UNIT_NAME) %>%
      dplyr::select(-c(CRITERIATYPEFRESHSALTWATER, CRITERIATYPE_ACUTECHRONIC, CRITERION_VALUE, UNIT_NAME)) %>%
      dplyr::mutate(MagnitudeUnit = toupper(MagnitudeUnit)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(ATTAINS.OrganizationIdentifier != "EPA304a", ATTAINS.OrganizationIdentifier)
  }
  #
  # paramRef$ATTAINS.ParameterName <- as.character(paramRef$ATTAINS.ParameterName)
  # # Pulls in all the units that are found in TADA.ResultMeasure.MeasureUnitCode as unique allowable unit column
  # MagnitudeValue <- paramRef %>%
  #   dplyr::left_join(
  #     (dplyr::select(.data, "TADA.ComparableDataIdentifier", "TADA.ResultMeasure.MeasureUnitCode") %>%
  #       dplyr::distinct() %>%
  #       tidyr::drop_na(TADA.ResultMeasure.MeasureUnitCode)
  #     ),
  #     by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many"
  #   ) %>%
  #   dplyr::select(ATTAINS.ParameterName, TADA.ResultMeasure.MeasureUnitCode) %>%
  #   dplyr::distinct() %>%
  #   dplyr::right_join(DefineCriteriaMethodology, by = c("ATTAINS.ParameterName"), relationship = "many-to-many") %>%
  #   dplyr::select(TADA.ResultMeasure.MeasureUnitCode) %>%
  #   dplyr::rename(MagnitudeUnit = TADA.ResultMeasure.MeasureUnitCode)

  if (excel == TRUE) {
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "DefineCriteriaMethodology")
        openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
      }
    )

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
      "ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier", "ATTAINS.UseName", "MonitoringLocationTypeName",
      "ATTAINS.waterTypeCode", "AcuteChronic", "SaltFresh", "BegAssessDate", "EndAssessDate",
      "Season", "MinimumSample", "ApplyUniqueSpatialCriteria",
      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit"
    )

    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(wb, "DefineCriteriaMethodology", cols = 1:ncol(DefineCriteriaMethodology), widths = "auto")
    openxlsx::setColWidths(wb, sheet = "DefineCriteriaMethodology", cols = 1:5, widths = 20)

    # Write column names in the excel spreadsheet under the tab [DefineCriteriaMethodology]
    # writeData(wb, "DefineCriteriaMethodology", startCol = 1, x = par, headerStyle = header_st)
    # Export DefineCriteriaMethodology dataframe into the excel spreadsheet tab
    openxlsx::writeData(wb, "DefineCriteriaMethodology", startCol = 1, x = DefineCriteriaMethodology, headerStyle = header_st)
    # writeData(wb, "DefineCriteriaMethodology", startCol = 13, startRow = 1, x = MagnitudeValue)

    openxlsx::writeData(wb, "Index", startCol = 9, startRow = 1, x = data.frame(MonitoringLocationTypeName = c(unique(.data$MonitoringLocationTypeName), "All", "NA"))) # WQP MonitoringTypeLocationName

    openxlsx::writeData(wb, "Index", startCol = 10, startRow = 1, x = data.frame(ATTAINS.waterTypeCode = c(unique(spatialRef$ATTAINS.waterTypeCode), "All", "NA"))) # ATTAINS.waterTypeCode
    openxlsx::writeData(wb, "Index", startCol = 11, startRow = 1, x = data.frame(AcuteChronic = c("A", "C", "NA"))) # AcuteChronic
    openxlsx::writeData(wb, "Index", startCol = 12, startRow = 1, x = data.frame(AcuteChronic = c("S", "F", "NA"))) # SaltFresh

    openxlsx::writeData(wb, "Index", startCol = 13, startRow = 1, x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA"))) # Season

    openxlsx::writeData(wb, "Index", startCol = 14, startRow = 1, x = data.frame(ApplyUniqueSpatialCriteria = c(unique(spatialRef$ApplyUniqueSpatialCriteria), "NA"))) # ApplyUniqueSpatialCriteria
    openxlsx::writeData(wb, "Index", startCol = 15, startRow = 1, x = data.frame(EquationBased = c("Yes", "No", "NA"))) # EquationBased

    openxlsx::writeData(wb, "Index", startCol = 16, startRow = 1, x = data.frame(MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode))) # MagnitudeUnit

    # The list of allowable values for each column in excel tab [DefineCriteriaMethodology] will be defined by the [Index] tab
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 4, rows = 2:1000, type = "list", value = sprintf("'Index'!$I$2:$I$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # WQP MonitoringTypeLocationName
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 5, rows = 2:1000, type = "list", value = sprintf("'Index'!$J$2:$J$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ATTAINS.waterTypeCode
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 6, rows = 2:1000, type = "list", value = sprintf("'Index'!$K$2:$K$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # AcuteChronic
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 7, rows = 2:1000, type = "list", value = sprintf("'Index'!$L$2:$L$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # SaltFresh
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 11, rows = 2:1000, type = "list", value = sprintf("'Index'!$M$2:$M$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # Season
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 13, rows = 2:1000, type = "list", value = sprintf("'Index'!$N$2:$N$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # ApplyUniqueSpatialCriteria
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 14, rows = 2:1000, type = "list", value = sprintf("'Index'!$O$2:$O$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # EquationBased
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", cols = 17, rows = 2:1000, type = "list", value = sprintf("'Index'!$P$2:$P$1000"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE)) # MagnitudeUnit

    # Conditional Formatting
    openxlsx::freezePane(wb, "DefineCriteriaMethodology", firstActiveRow = 2, firstActiveCol = 4)
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology",
      cols = 4:17, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "notBlanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology",
      cols = 4:17, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "blanks", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    ) # modified cells.

    # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }

    if (overwrite == FALSE) {
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }

    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }

  return(DefineCriteriaMethodology)
}



#' Magnitude Summary
#'
#' @param .data A TADA dataframe. Users should run the appropriate data cleaning,
#' processing, harmonization and filtering functions prior to this step.
#'
#' @return A data frame with all allowable ATTAINS designated use values for an ATTAINS Parameter
#'
#' @export
#'
#' @examples
#' Data_Nutrients_UT_GetATTAINS <- load("data.Rda")
#' Data_Nutrients_Param_Ref <- TADA_CreateUseParamRef(Data_Nutrients_UT)
#'
# TADA_MagnitudeSummary <- function(.data, StandardsRef = NULL, UseAURef = NULL, overwrite = FALSE) {
#' #  # Attempt to pull in the ref files from the default Downloads location.
#' #  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
#' #  if (is.null(StandardsRef)) {
#' #    StandardsRef <- openxlsx::read.xlsx(downloads_path, sheet = "DefineCriteriaMethodology")
#' #  }
#' #  # check to see if user-supplied standards ref is a df with appropriate columns and filled out.
#' #  if (!is.null(StandardsRef) & !is.character(StandardsRef)) {
#' #    if (!is.data.frame(StandardsRef)) {
#' #      stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
#' #      ATTAINS.ParameterName,	ATTAINS.OrganizationIdentifier,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
#' #    }
#' #    if (is.data.frame(StandardsRef)) {
#' #      col.names <- c(
#' #        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName"
#' #      )
#' #      ref.names <- names(StandardsRef)
#' #      if (length(setdiff(col.names, ref.names)) > 0) {
#' #        stop("TADA_DefineStandards: 'StandardsRef' must be a data frame with at least six columns:
#' #        ATTAINS.ParameterName,	ATTAINS.OrganizationIdentifier,	ATTAINS.UseName, StandardValue,	StandardUnit,	StandardLimit")
#' #      }
#' #    }
#' #  }
#' #  wb <- openxlsx::loadWorkbook(wb, downloads_path)
#' #  tryCatch(
#' #    {
#' #      openxlsx::addWorksheet(wb, "MagnitudeExcursions")
#' #    },
#' #    error = function(e) {
#' #      openxlsx::removeWorksheet(wb, "MagnitudeExcursions")
#' #      openxlsx::addWorksheet(wb, "MagnitudeExcursions")
#' #    }
#' #  )
#' #  # Format column header
#' #  header_st <- openxlsx::createStyle(textDecoration = "Bold")
#' #  # Reference tables (required)
#' #  ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
#' #  UseParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseParamRef")
#' #  AURef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateAURef")
#' #  # Contains all AU ref columns such as Site-specific names and User defined exclusions to be joined in the TADA dataframe.
#' #  temp_AU <- .data %>%
#' #    dplyr::right_join(AURef, by = c(
#' #      "MonitoringLocationIdentifier", "MonitoringLocationName",
#' #      "LongitudeMeasure", "LatitudeMeasure", "MonitoringLocationTypeName"
#' #    ), relationship = "many-to-many")
#' #  # Magnitude Excursion Summary
#' #  TADA_MagnitudeExcursions <- StandardsRef %>%
#' #    dplyr::mutate(dplyr::across(c(MagnitudeValueLower, MagnitudeValueUpper), as.numeric)) %>%
#' #    dplyr::mutate(dplyr::across(
#' #      c(
#' #        ATTAINS.waterTypeCode,
#' #        MonitoringLocationTypeName,
#' #        AcuteChronic, SaltFresh, Season, EquationBased,
#' #        ApplyUniqueSpatialCriteria, # Will depend on the user's crosswalk of ML to this criteria for filtering.
#' #      ), as.factor
#' #    )) %>%
#' #    dplyr::left_join(temp_AU, by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many") %>%
#' #    dplyr::distinct() %>%
#' #    dplyr::mutate(across(MagnitudeValueLower, as.numeric)) %>%
#' #    dplyr::group_by(.[, c(
#' #      "TADA.ComparableDataIdentifier", "EPA304A.PollutantName", "ATTAINS.ParameterName",
#' #      "ATTAINS.OrganizationIdentifier", "ATTAINS.UseName",
#' #      "ATTAINS.assessmentunitidentifier", "MonitoringLocationIdentifier",
#' #      "MonitoringLocationTypeName.y", "ATTAINS.waterTypeCode.y", "AcuteChronic", "SaltFresh",
#' #      "BegAssessDate", "EndAssessDate",
#' #      "Season", "MinimumSample", "ApplyUniqueSpatialCriteria.y",
#' #      "EquationBased", "MagnitudeValueLower", "MagnitudeValueUpper", "MagnitudeUnit"
#' #    )]) %>%
#' #    dplyr::summarise(
#' #      n_MonitoringLocationID = length(unique(MonitoringLocationIdentifier)),
#' #      n_discrete = sum(!is.na(TADA.ResultMeasureValue)),
#' #      n_exceedance = sum(TADA.ResultMeasureValue < MagnitudeValueLower, na.rm = TRUE) + sum(TADA.ResultMeasureValue > MagnitudeValueUpper, na.rm = TRUE),
#' #      .groups = "drop"
#' #    )
#' #  if (!is.null(UseAURef)) {
#' #    # If a user provides UseAURef and UseParamRef, this creates a Use name to AU and Parameter crosswalk. This helps filter down the summary list further.
#' #    UseParamAU <- UseAURef %>%
#' #      dplyr::right_join(UseParamRef, by = c("ATTAINS.UseName", "ATTAINS.OrganizationIdentifier"), relationship = "many-to-many") %>%
#' #      dplyr::filter(!(!ATTAINS.OrganizationIdentifier %in% c("EPA304a") & is.na(ATTAINS.assessmentunitidentifier))) %>%
#' #      dplyr::select(
#' #        ATTAINS.OrganizationIdentifier, ATTAINS.assessmentunitidentifier,
#' #        ATTAINS.assessmentunitname, TADA.ComparableDataIdentifier,
#' #        EPA304A.PollutantName, ATTAINS.ParameterName, ATTAINS.UseName
#' #      )
#' #    UseParamAU2 <- UseParamAU %>%
#' #      dplyr::group_by(ATTAINS.ParameterName, ATTAINS.assessmentunitidentifier, ATTAINS.assessmentunitname) %>%
#' #      dplyr::summarize(.groups = "keep") %>%
#' #      dplyr::mutate(ATTAINS.OrganizationIdentifier = "EPA304a") %>%
#' #      stats::na.omit() %>%
#' #      dplyr::full_join(UseParamAU, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName"), relationship = "many-to-many") %>%
#' #      dplyr::mutate(
#' #        ATTAINS.assessmentunitidentifier = dplyr::coalesce(ATTAINS.assessmentunitidentifier.x, ATTAINS.assessmentunitidentifier.y),
#' #        ATTAINS.assessmentunitname = dplyr::coalesce(ATTAINS.assessmentunitname.x, ATTAINS.assessmentunitname.y)
#' #      ) %>%
#' #      dplyr::select(-c(ATTAINS.assessmentunitidentifier.x, ATTAINS.assessmentunitidentifier.y, ATTAINS.assessmentunitname.x, ATTAINS.assessmentunitname.y)) %>%
#' #      dplyr::distinct()
#' #    TADA_MagnitudeExcursions <- TADA_MagnitudeExcursions %>%
#' #      dplyr::right_join(UseParamAU2)
#' #  }
#' #  # set zoom size
#' #  set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
#' #  sV <- wb$worksheets[[8]]$sheetViews
#' #  wb$worksheets[[8]]$sheetViews <- set_zoom(90)
#' #  # Format header and bodystyle
#' #  header_st <- openxlsx::createStyle(textDecoration = "Bold")
#' #  bodyStyle <- openxlsx::createStyle(wrapText = TRUE)
#' #  # Write column names in the excel spreadsheet under the tab [DefineStandards]
#' #  # writeData(wb, "DefineStandards", startCol = 1, x = par, headerStyle = header_st)
#' #  # Export DefineStandards dataframe into the excel spreadsheet tab
#' #  openxlsx::writeData(wb, "MagnitudeExcursions", startCol = 1, x = TADA_MagnitudeExcursions, headerStyle = header_st)
#' #  # Saving of the file if overwrite = TRUE or if the file is not found in the defined folder path. If is not saved, a dataframe is still returned.
#' #  if (!is.null(downloads_path)) {
#' #    # saveWorkbook(wb, "inst/extdata/myfileRef.xlsx", overwrite = F)
#' #    downloads_path <- downloads_path
#' #  }
#' #  if (overwrite == TRUE) {
#' #    openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
#' #  }
#' #  if (overwrite == FALSE) {
#' #    warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
#' #    openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
#' #  }
#' #  cat("File saved to:", gsub("/", "\", downloads_path), "\n")
#' #  MagnitudeExcursions <- openxlsx::read.xlsx(downloads_path, sheet = "MagnitudeExcursions")
#' #  return(MagnitudeExcursions)
# }
