TADA_CrosswalkATTAINSParameterName <- function(
    .data,
    org_id = NULL,
    paramRef = NULL,
    auto_assign = "All",
    AUMLRef = NULL,
    excel = FALSE,
    overwrite = FALSE
) {
  if (missing(.data) && missing(org_id) && missing(paramRef) && missing(AUMLRef)) {
    message("All arguments are blank, returning an empty dataframe with column names only.")
    .data <- data.frame(
      TADA.CharacteristicName = NA_character_,
      TADA.ComparableDataIdentifier = NA_character_
    )
    ParametersCrosswalk <- data.frame(
      TADA.ComparableDataIdentifier = character(0),
      ATTAINS.OrganizationIdentifier = character(0),
      ATTAINS.ParameterName = character(0),
      ATTAINS.FlagParameterName = character(0),
      Flag.ParameterInput = character(0)
    )
  } else {
    if (excel == FALSE && overwrite == TRUE) {
      stop(paste0(
        "TADA_CrosswalkATTAINSParameterName: ",
        "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
        "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
      ))
    }
    
    if (!auto_assign %in% c("None", "All", "Org")) {
      stop(paste0(
        "TADA_CrosswalkATTAINSParameterName: ",
        "argument input ",
        auto_assign,
        " is not a valid entry. Please type one of 'None', 'All', 'Org' as a value."
      ))
    }
    
    if (!is.character(org_id) & is.null(org_id)) {
      org_id <- ""
      message("Proceeding function with 'org_id = NULL'. If this was not intentional, please supply a valid 'org_id'.")
    }
    
    if (tolower("all") %in% tolower(org_id)) {
      if (is.null(AUMLRef)) {
        message("org_id == 'All' was selected, no AUMLRef provided; attempting to pull domain orgs.")
        org_id <- tryCatch(
          {
            dv <- rExpertQuery::EQ_DomainValues("org_id", api_key = .setEQKey())
            if (!is.null(dv) && "code" %in% names(dv)) {
              dv[["code"]]
            } else {
              warning("EQ_DomainValues('org_id') returned no 'code' column; proceeding with empty org list.")
              character()
            }
          },
          error = function(e) {
            warning("Failed to retrieve ATTAINS org domain values: ", conditionMessage(e))
            character()
          }
        )
      } else {
        message("org_id == 'All' was selected, AUMLRef provided; using orgs found in AUMLRef.")
        org_id <- unique(stats::na.omit(AUMLRef$ATTAINS.OrganizationIdentifier))
      }
    }
    
    if (length(org_id) > 1) {
      message(paste0(
        "TADA_CrosswalkATTAINSParameterName: More than one org_name was defined in your dataframe. ",
        "Generating duplicate rows of TADA.ComparableDataIdentifier for each org."
      ))
    }
    
    if (!is.null(paramRef) & !is.character(paramRef)) {
      if (!is.data.frame(paramRef)) {
        stop(paste0(
          "TADA_CrosswalkATTAINSParameterName: 'paramRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
        ))
      }
      
      if (is.data.frame(paramRef)) {
        col.names <- c("TADA.ComparableDataIdentifier", "ATTAINS.ParameterName")
        ref.names <- names(paramRef)
        
        if (
          length(setdiff(col.names, ref.names)) > 0 &&
          !("TADA.ComparableDataIdentifier" %in% names(paramRef))
        ) {
          stop(paste0(
            "TADA_CrosswalkATTAINSParameterName: 'paramRef' must be a data frame with these 2 columns:",
            "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
          ))
        }
      }
    }
    
    if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
      paramRef <- paramRef |>
        dplyr::left_join(
          .data,
          c(
            "TADA.CharacteristicName",
            "TADA.MethodSpeciationName",
            "TADA.ResultSampleFractionText"
          )
        ) |>
        dplyr::select(
          "TADA.CharacteristicName",
          "TADA.ComparableDataIdentifier",
          "ATTAINS.OrganizationIdentifier",
          "ATTAINS.ParameterName",
          "ATTAINS.FlagParameterName"
        )
    }
    
    TADA_param <- dplyr::distinct(.data[, c("TADA.ComparableDataIdentifier"), drop = FALSE]) |>
      dplyr::distinct() |>
      dplyr::mutate(ATTAINS.OrganizationIdentifier = NA_character_) |>
      tidyr::complete(
        TADA.ComparableDataIdentifier,
        ATTAINS.OrganizationIdentifier = org_id
      ) |>
      dplyr::filter(!is.na(ATTAINS.OrganizationIdentifier)) |>
      dplyr::left_join(
        .data[, c("TADA.ComparableDataIdentifier", "TADA.CharacteristicName")],
        by = dplyr::join_by(TADA.ComparableDataIdentifier),
        relationship = "many-to-many"
      ) |>
      dplyr::distinct()
    
    load(system.file(
      "extdata",
      "ATTAINSParamUseOrgRef.rda",
      package = "EPATADA"
    ))
    
    ATTAINS_param <- ATTAINSParamUseOrgRef |>
      dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) |>
      dplyr::arrange(ATTAINS.ParameterName)
    
    if ("" %in% org_id) {
      ATTAINS_param <- ATTAINSParamUseOrgRef |>
        dplyr::mutate(ATTAINS.OrganizationIdentifier = "")
    }
    
    if (
      sum(
        !org_id[!org_id %in% c("EPA304a", "")] %in%
        TADA_GetATTAINSOrgIDsRef()[, "code"]
      ) > 0
    ) {
      warning("TADA_CrosswalkATTAINSParameterName: One or more organization identifiers entered by user is not found in ATTAINS.")
    }
    
    if (tolower(auto_assign) == tolower("None")) {
      ParametersCrosswalk <- TADA_param |>
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName
        ) |>
        dplyr::arrange(ATTAINS.OrganizationIdentifier) |>
        dplyr::mutate(
          ATTAINS.ParameterName = as.character(NA),
          ATTAINS.FlagParameterName = "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment"
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = "Default. No crosswalk was provided."
        ) |>
        dplyr::distinct()
    }
    
    if (tolower(auto_assign) == tolower("All")) {
      message(paste0(
        "TADA_CrosswalkATTAINSParameterName: auto_assign == 'All' was selected, ",
        "finding an alias ATTAINS.ParameterName match for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
      ))
      
      TADACharAliasRef <- utils::read.csv(system.file(
        "extdata",
        "TADACharAliasRef.csv",
        package = "EPATADA"
      ))
      
      TADACharAliasRef <- TADACharAliasRef |>
        dplyr::filter(
          ATTAINS.ParameterName %in% ATTAINSParamUseOrgRef$ATTAINS.ParameterName
        )
      
      ParametersCrosswalk <- TADA_param |>
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) |>
        dplyr::select(
          TADA.CharacteristicName,
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName
        ) |>
        dplyr::left_join(
          TADACharAliasRef,
          by = c("TADA.CharacteristicName" = "CharacteristicName"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName
        ) |>
        dplyr::arrange(ATTAINS.OrganizationIdentifier) |>
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "Not Applicable for Analysis." | is.na(ATTAINS.ParameterName) ~
              "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in% ATTAINSParamUseOrgRef$ATTAINS.ParameterName ~
              "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
            ATTAINS.ParameterName %in% ATTAINSParamUseOrgRef$ATTAINS.ParameterName &
              !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier, ATTAINSParamUseOrgRef$ATTAINS.ParameterName) ~
              "This ATTAINS parameter name was included in past ATTAINS assessment cycles, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier, ATTAINSParamUseOrgRef$ATTAINS.ParameterName) ~
              "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization."
          )
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = dplyr::if_else(
            !is.na(ATTAINS.ParameterName),
            "This crosswalk was provided through an alias match auto_assign = 'All', between ATTAINS.ParameterName and TADA.CharacteristicName.",
            "No crosswalk was provided and no alias matches were found."
          )
        ) |>
        dplyr::distinct()
    }
    
    if (tolower(auto_assign) == tolower("Org")) {
      message(paste0(
        "TADA_CrosswalkATTAINSParameterName: auto_assign == 'Org' was selected, finding an alias ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
      ))
      
      TADACharAliasRef <- utils::read.csv(system.file(
        "extdata",
        "TADACharAliasRef.csv",
        package = "EPATADA"
      ))
      
      TADACharAliasRef <- TADACharAliasRef |>
        dplyr::filter(
          ATTAINS.ParameterName %in% ATTAINS_param$ATTAINS.ParameterName
        )
      
      ParametersCrosswalk <- TADA_param |>
        dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) |>
        dplyr::select(
          TADA.CharacteristicName,
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName
        ) |>
        dplyr::left_join(
          TADACharAliasRef,
          by = c("TADA.CharacteristicName" = "CharacteristicName"),
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName
        ) |>
        dplyr::arrange(ATTAINS.OrganizationIdentifier) |>
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "Not Applicable for Analysis." | is.na(ATTAINS.ParameterName) ~
              "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in% ATTAINSParamUseOrgRef$ATTAINS.ParameterName ~
              "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
            ATTAINS.ParameterName %in% ATTAINSParamUseOrgRef$ATTAINS.ParameterName &
              !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier, ATTAINSParamUseOrgRef$ATTAINS.ParameterName) ~
              "This ATTAINS parameter name was included in past ATTAINS assessment cycles, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier, ATTAINSParamUseOrgRef$ATTAINS.ParameterName) ~
              "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization."
          )
        ) |>
        dplyr::mutate(
          ATTAINS.ParameterName = dplyr::if_else(
            ATTAINS.FlagParameterName ==
              "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization." |
              ATTAINS.OrganizationIdentifier == "",
            ATTAINS.ParameterName,
            NA
          )
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = dplyr::if_else(
            !is.na(ATTAINS.ParameterName),
            "This crosswalk was provided through an alias match auto_assign = 'Org', between ATTAINS.ParameterName and TADA.CharacteristicName.",
            "No crosswalk was provided and no alias matches were found for this organization."
          )
        ) |>
        dplyr::distinct()
    }
    
    if (!is.null(paramRef)) {
      paramRef <- paramRef |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName
        ) |>
        dplyr::mutate(
          Flag.ParameterInput = "This crosswalk was provided through a user supplied table"
        ) |>
        dplyr::filter(!is.na(ATTAINS.ParameterName))
      
      ParametersCrosswalk <- ParametersCrosswalk |>
        dplyr::select(
          ATTAINS.OrganizationIdentifier,
          TADA.ComparableDataIdentifier,
          ATTAINS.ParameterName,
          Flag.ParameterInput
        ) |>
        dplyr::filter(
          !TADA.ComparableDataIdentifier %in% paramRef$TADA.ComparableDataIdentifier
        ) |>
        dplyr::bind_rows(paramRef[, c(
          "ATTAINS.OrganizationIdentifier",
          "TADA.ComparableDataIdentifier",
          "ATTAINS.ParameterName",
          "Flag.ParameterInput"
        )]) |>
        dplyr::mutate(
          ATTAINS.FlagParameterName = dplyr::case_when(
            ATTAINS.ParameterName == "Not Applicable for Analysis." | is.na(ATTAINS.ParameterName) ~
              "No ATTAINS.ParameterName crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
            !ATTAINS.ParameterName %in%
              ATTAINSParamUseOrgRef$ATTAINS.ParameterName ~
              "Parameter name is not included in ATTAINS, contact ATTAINS to add ATTAINS.ParameterName name to Domain List.",
            ATTAINS.ParameterName %in%
              ATTAINSParamUseOrgRef$ATTAINS.ParameterName &
              !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(
                ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier,
                ATTAINSParamUseOrgRef$ATTAINS.ParameterName
              ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles, but not for this organization.",
            paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in%
              paste(
                ATTAINSParamUseOrgRef$ATTAINS.OrganizationIdentifier,
                ATTAINSParamUseOrgRef$ATTAINS.ParameterName
              ) ~ "This ATTAINS parameter name was included in past ATTAINS assessment cycles for this organization"
          )
        ) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.FlagParameterName,
          Flag.ParameterInput
        )
    }
    
    rm(TADA_param)
  }
  return(ParametersCrosswalk)
}



TADA_CrosswalkCSTPollutantName <- function(
    .data,
    org_id = NULL,
    paramRef = NULL,
    auto_assign = "All",
    AUMLRef = NULL
) {
  attains_crosswalk <- TADA_CrosswalkATTAINSParameterName(
    .data = .data,
    org_id = org_id,
    paramRef = paramRef,
    auto_assign = auto_assign,
    AUMLRef = AUMLRef
  )
  
  # rejoin .data to recover TADA.CharacteristicName
  data_char <- dplyr::distinct(
    .data[, c("TADA.ComparableDataIdentifier", "TADA.CharacteristicName"), drop = FALSE]
  )
  
  attains_plus_char <- attains_crosswalk |>
    dplyr::left_join(
      data_char,
      by = "TADA.ComparableDataIdentifier",
      relationship = "many-to-many"
    )
  
  # load CST reference data
  TADACharAliasRef <- utils::read.csv(
    system.file("extdata", "TADACharAliasRef.csv", package = "EPATADA"),
    stringsAsFactors = FALSE
  )
  orgs <- utils::read.csv(
    system.file("extdata", "ATTAINSOrgToCSTEntityRef.csv", package = "EPATADA"),
    stringsAsFactors = FALSE
  )
  
  cst_ref <- TADA_CST_GetCriteria() |>
    dplyr::mutate(
      POLLUTANT_NAME = toupper(POLLUTANT_NAME),
      STD_POLLUTANT_NAME = toupper(STD_POLLUTANT_NAME)
    ) |>
    dplyr::left_join(orgs, by = "ENTITY_ABBR") |>
    dplyr::left_join(
      TADACharAliasRef,
      by = c("POLLUTANT_NAME", "STD_POLLUTANT_NAME"),
      relationship = "many-to-many"
    ) |>
    dplyr::select(
      TADA.CharacteristicName = CharacteristicName,
      ATTAINS.OrganizationIdentifier,
      CST.PollutantName = POLLUTANT_NAME
    ) |>
    dplyr::distinct()
  
  out <- attains_plus_char |>
    dplyr::left_join(
      cst_ref,
      by = c("TADA.CharacteristicName", "ATTAINS.OrganizationIdentifier"),
      relationship = "many-to-many"
    ) |>
    dplyr::select(
      TADA.ComparableDataIdentifier,
      CST.PollutantName,
      ATTAINS.ParameterName,
      ATTAINS.OrganizationIdentifier,
      ATTAINS.FlagParameterName,
      Flag.ParameterInput
    ) |>
    dplyr::distinct()
  
  out
}


TADA_CrosswalkExcel <- function(
    ParametersCrosswalk,
    auto_assign = "All",
    overwrite = FALSE,
    filename = "ParamUseMLCrosswalks.xlsx"
) {
  downloads_path <- .get_downloads_path(filename)
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "ATTAINS.PriorOrgParamUseRef")
  openxlsx::addWorksheet(wb, "ParametersCrosswalk")
  openxlsx::addWorksheet(wb, "Index")
  
  sv <- openxlsx::sheetVisibility(wb)
  sn <- names(wb)
  if (length(which(sn == "ParametersCrosswalk")) == 1) sv[which(sn == "ParametersCrosswalk")] <- "visible"
  if (length(which(sn == "Index")) == 1) sv[which(sn == "Index")] <- "hidden"
  if (length(which(sn == "ATTAINS.PriorOrgParamUseRef")) == 1) sv[which(sn == "ATTAINS.PriorOrgParamUseRef")] <- "visible"
  openxlsx::sheetVisibility(wb) <- sv
  
  header_st <- openxlsx::createStyle(textDecoration = "Bold")
  openxlsx::setColWidths(wb, "ParametersCrosswalk", cols = 1:ncol(ParametersCrosswalk), widths = "auto")
  
  openxlsx::writeData(wb, "ParametersCrosswalk", x = ParametersCrosswalk, headerStyle = header_st)
  
  if (!isTRUE(overwrite) && file.exists(downloads_path)) {
    base <- tools::file_path_sans_ext(downloads_path)
    ext <- tools::file_ext(downloads_path)
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    downloads_path <- sprintf("%s_%s.%s", base, ts, ext)
  }
  
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)
  message("Saved as: ", normalizePath(downloads_path))
  
  invisible(downloads_path)
}
