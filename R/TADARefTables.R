# Used to store cached ATTAINSParamUseOrg Reference Table
ATTAINSParamUseOrgRef_Cached <- NULL



#' ATTAINS Parameter and Use Name by Organization Reference Key
#'
#' Function downloads and returns the newest available ATTAINS domain values
#' reference dataframe summarized by parameter and use 
#' listed as a cause by organizations in previous assessments.
#' This dataframe is used in TADA_CreateParamRef() and
#' TADA_CreateParamUseRef() as the basis for the pulling in prior ATTAINS
#' parameter names and use name by organization name. This helps to filter
#' selections of drop down values and summarizing impairment decisions for
#' a parameter and use combination.
#'
#' @return Dataframe of ATTAINS parameter and use name by organization
#'
#' @export

TADA_GetATTAINSParamUseOrgRef <- function() {
  # # If there is a cached table available return it
  # if (!is.null(ATTAINSParamUseOrgRef_Cached)) {
  #   return(ATTAINSParamUseOrgRef_Cached)
  # }
  # 
  # # Try to download up-to-date raw data
  # 
  # raw.data <- tryCatch(
  #   {
  #     # reads rATTAINS domain value
  #     org_id <- rATTAINS::domain_values("OrgName")[[3]]
  #     all.data <- list()
  #     
  #     for(i in 1:length(org_id)){
  #       all.data[[i]] <- rATTAINS::assessments(organization_id = org_id[i])[[2]]
  #     }
  #     
  #     all.data2 <- dplyr::bind_rows(all.data, .id = "column_label")
  #   },
  #   error = function(err) {
  #     NULL
  #   }
  # )
  # 
  # # remove intermediate variables
  # rm(org_id, all.data, all.data2)
  # 
  # # If the download failed fall back to internal data (and report it)
  # if (is.null(raw.data)) {
  #   message("Downloading latest ATTAINSParamUseOrg Reference Table failed!")
  #   message("Falling back to (possibly outdated) internal file.")
  #   return(utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA")))
  # }
  # 
  # # Creates and formats the ATTAINSParamUseOrg ref table containing parameters by use name for each org
  # use_attainments <- raw.data %>% tidyr::unnest(c(use_attainments), names_sep = ".")
  # use_parameters <- use_attainments %>% tidyr::unnest(c(parameters), names_sep = ".")
  #   
  # ATTAINSParamUseOrgRef <- use_parameters %>%
  #   dplyr::select(organization_identifier, organization_name, organization_type_text,
  #                 use_attainments.use_name, parameters.parameter_name) %>%
  #   dplyr::distinct()
  # 
  # # remove intermediate variables
  # rm(use_attainments, use_parameters)
  # 
  
  ATTAINSParamUseOrgRef <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  # Save updated table in cache
  ATTAINSParamUseOrgRef_Cached <- ATTAINSParamUseOrgRef
  
  ATTAINSParamUseOrgRef
}


# Update ATTAINSParamUseOrg Reference Table internal file
# (for internal use only)

TADA_UpdateATTAINSParamUseOrgRef <- function() {
  utils::write.csv(TADA_GetATTAINSParamUseOrgRef(), file = "inst/extdata/ATTAINSParamUseEntityRef.csv", row.names = FALSE)
}



# Used to store cached CST EPA304a Reference Table
EPA304aRef_Cached <- NULL



#' EPA304a Criteria Search Tool Reference Key
#'
#' Function downloads and returns the newest available Criteria Search Tool and
#' associated EPA 304a Criteria pollutant names as a reference dataframe. 
#' This dataframe is used in TADA_CreateParamRef() and
#' TADA_CreateParamUseRef() as the basis for the pulling in EPA304a recommended
#' pollutant name and use_name for assessment under the CWA.
#'
#' Currently only numeric priority characteristic in TADA are the focus.
#' This list can be found on the bottom of the web page:
#' https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa
#'
#' @return Dataframe of EPA304a recommended standards for a pollutant and use name.
#'
#' @export

TADA_GetEPA304aRef <- function() {
  # If there is a cached table available return it
  if (!is.null(EPA304aRef_Cached)) {
    return(EPA304aRef_Cached)
  }
  
  # Try to download up-to-date raw data
  
  raw.data <- tryCatch(
    {
      # read raw xlsx from url
      openxlsx::read.xlsx("https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx")
    },
    error = function(err) {
      NULL
    }
  )
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Criteria Search Tool Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")))
  }
  
  # Creates and formats the CST ref table below:
  
  # Find the CST row that contains the column name of dataframe - removes extraneous details
  CST.begin <- as.integer(which(rowSums(is.na(raw.data))==0)[1])
  colnames(raw.data) <- as.character(raw.data[CST.begin,])
  # import TADA unit reference for priority characteristics (characteristic specific)
  tada.char.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))

  # Pulls in column names that will be used as a reference table
  EPA304aRef <- raw.data %>%
    utils::tail(-CST.begin) %>%
    dplyr::filter(ENTITY_ABBR == "304A") %>%
    dplyr::left_join(tada.char.ref, by = c("POLLUTANT_NAME" = "CST.PollutantName"), relationship = "many-to-many") %>%
    dplyr::select(TADA.CharacteristicName, POLLUTANT_NAME, organization_name = ENTITY_ABBR,
                  use_name = USE_CLASS_NAME_LOCATION_ETC, CRITERION_VALUE,
                  CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
                  CRITERIATYPE_ACUTECHRONIC, CRITERIATYPE_WATERORG, UNIT_NAME
                  )
  
  # Remove intermediate variables
  rm(CST.begin, tada.char.ref, raw.data)  
  
  # Save updated table in cache
  EPA304aRef_Cached <- EPA304aRef
  
  EPA304aRef
}

# Update Criteria Search Tool Reference Table internal file
# (for internal use only)

TADA_UpdateEPA304aRef <- function() {
  utils::write.csv(TADA_GetEPA304aRef(), file = "inst/extdata/CST.csv", row.names = FALSE)
}


#' Nutrient Summation Reference Key
#'
#' Function downloads and returns the newest available nutrient summation
#' reference dataframe. This dataframe is used in TADA_CalculateTotalNitrogen as
#' the basis for the combinations added together to get total nitrogen. Users
#' may customize this reference table for their own dataset and use the custom
#' dataframe as an input in TADA_CalculateTotalNitrogen.
#'
#' @return Dataframe of nutrient summation combinations
#'
#' @export

TADA_GetNutrientSummationRef <- function() {
  ref <- utils::read.csv(system.file("extdata", "NPsummation_key.csv", package = "EPATADA"))
  return(ref)
}


#' Generate Unique Synonym Reference Table
#'
#' Function generates a synonym reference table containing all unique combinations of
#' TADA.CharacteristicName, TADA.ResultSampleFractionText, and TADA.MethodSpeciationName. The
#' function also joins in some TADA-specific suggested synonyms for nutrients and priority parameters.
#' These target synonyms (denoted in the reference table with the prefix "Target.") are intended to
#' help the user aggregate synonymous data that may be uploaded with slightly different metadata
#' conventions and prepare nutrient data for total N and P summations. Users can review how their
#' input data relates to target synonyms for TADA.CharacteristicName, TADA.ResultSampleFractionText,
#' and TADA.MethodSpeciationName. Once the synonym table is created, users may optionally edit the
#' target columns in the reference table to meet their needs. Additionally, the function assumes
#' the user has already removed any data containing suspect characteristic-unit-fraction-speciation
#' combinations (i.e. user has already run TADA_FlagFraction, TADA_FlagSpeciation, TADA_FlagResultUnit,
#' etc.).
#'
#' @param .data TADA dataframe. If a data frame is not provided, the function will return the default internal reference table.
#'
#' @return Synonym Reference Table unique to the input dataframe
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_6Tribes_5y)
#'
#' # Create a synonym reference table for flagged, cleaned dataframe:
#' Data_6Tribes_5yClean <- subset(Data_6Tribes_5y, !is.na(Data_6Tribes_5y$TADA.ResultMeasureValue))
#' Data_6Tribes_5yClean <- TADA_FlagFraction(Data_6Tribes_5yClean, clean = TRUE)
#' Data_6Tribes_5yClean <- TADA_FlagResultUnit(Data_6Tribes_5yClean, clean = "suspect_only")
#' Data_6Tribes_5yClean <- TADA_FlagSpeciation(Data_6Tribes_5yClean, clean = "suspect_only")
#' Data_6Tribes_5yClean <- TADA_FlagMethod(Data_6Tribes_5yClean, clean = TRUE)
#' CreateRefTable <- TADA_GetSynonymRef(Data_6Tribes_5yClean)
#'
#' # Get internal synonym reference table
#' reference <- TADA_GetSynonymRef()
TADA_GetSynonymRef <- function(.data) {
  if (missing(.data)) {
    ref <- utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv", package = "EPATADA"))
    return(ref)
  }

  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # check .data has the required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  TADA_CheckColumns(.data, expected_cols)

  if (!any(c("TADA.MethodSpeciation.Flag", "TADA.SampleFraction.Flag", "TADA.ResultUnit.Flag") %in% names(.data))) {
    print("Warning: This dataframe is missing TADA QC flagging columns, indicating that you have not yet run the TADA_FlagResultUnit, TADA_FlagFraction, or TADA_FlagSpeciation functions. It is highly recommended you run these flagging functions and remove Suspect combinations before proceeding to this step.")
  }

  # check to see if any suspect data flags exist
  check_inv <- .data[, names(.data) %in% c("TADA.MethodSpeciation.Flag", "TADA.SampleFraction.Flag", "TADA.ResultUnit.Flag")]
  check_inv <- check_inv %>%
    tidyr::pivot_longer(cols = names(check_inv), names_to = "Flag_Column") %>%
    dplyr::filter(value == "Suspect")

  if (dim(check_inv)[1] > 0) {
    check_inv <- check_inv %>%
      dplyr::group_by(Flag_Column) %>%
      dplyr::summarise("Result Count" = length(value))
    print("Warning: Your dataframe contains suspect metadata combinations in the following flag columns:")
    print(as.data.frame(check_inv))
  }

  # execute function after checks are passed
  # define raw harmonization table as an object
  harm.raw <- utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv", package = "EPATADA"))

  join.data <- merge(unique(.data[, expected_cols]),
    harm.raw,
    by = expected_cols,
    all.x = TRUE
  )

  # trim join.data to include only unique combos of char-frac-spec-unit
  unique.data <- join.data %>% dplyr::distinct()

  unique.data <- unique.data[, names(harm.raw)]

  # return unique.data
  return(unique.data)
}


#' Nutrient Summation Reference Key
#'
#' This internal reference file includes USGS only units/speciations. It was
#' created in July 2023 using the pcodes domain table from NWIS
#' (https://help.waterdata.usgs.gov/codes-and-parameters/parameters). All USGS units
#' and speciations are given a target unit and speciation that is synonymous, but
#' adheres to the WQX schema (WQX measure unit domain table).
#'
#' This reference file is used in the TADA_ConvertResultUnits() function where
#' synonymous units and speciations are harmonized before units are then also
#' harmonized/converted to WQX targets.
#'
#'
#' @return Dataframe of USGS only units and speciations and their WQX compatible
#' targets/synonyms.
#'
#' @export

TADA_GetUSGSSynonymRef <- function() {
  ref <- utils::read.csv(system.file("extdata", "USGS_units_speciation.csv", package = "EPATADA"))
  return(ref)
}
