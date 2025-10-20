# Used to store cached ATTAINSParamToWQPCharRef Reference Table
ATTAINSParamToWQPCharRef_Cached <- NULL

#' ATTAINS Parameter and WQP Characteristic Exact Match Reference Table
#'
#' Function downloads and returns the newest available crosswalk of exact
#' matches between ATTAINS.ParameterName and TADA.CharacteristicName.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated ATTAINSParamToWQPCharRef object
#'
#' @export
TADA_GetATTAINSParamToWQPCharRef <- function(charAliasType = c("All", "ATTAINS")) {

  charAliasType <- match.arg(charAliasType)

  # Pull in WQX Char Alias table.
  temp_zip <- tempfile(fileext = ".zip")

  utils::download.file("https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicAlias_CSV.zip", destfile = temp_zip, mode = "wb")

  temp_dir <- tempdir() # Create a temporary directory to extract files
  utils::unzip(temp_zip, exdir = temp_dir)

  # If you know the CSV filename
  csv_file_path <- file.path(temp_dir, "Characteristic Alias.csv")

  data <- utils::read.csv(csv_file_path)

  # remove intermediate variables
  rm(temp_zip, temp_dir, csv_file_path)

  WQX_char_alias_filtered <- data

  WQX_char_alias_filtered$Alias.Name <- toupper(WQX_char_alias_filtered$Alias.Name)

  # retrieve the ATTAINS parameter domain value from rExpertQuery
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("param_name")

  ATTAINSParamRef <- ATTAINS.raw[, "name", drop = FALSE]

  # Create the initial ATTAINS param to WQX char crosswalk
  if(charAliasType == "ATTAINS") {
    WQX_char_alias_filtered <- dplyr::filter(WQX_char_alias_filtered, Alias.Type.Name == "ATTAINS.PARAMETER")

    ATTAINSWQX2.0 <- ATTAINSParamRef |>
      dplyr::left_join(WQX_char_alias_filtered, by = c("name"  = "Alias.Name")) |>
      dplyr::select(CharacteristicName = Characteristic.Name, ATTAINS.ParameterName = name, Alias.Type.Name) |>
      dplyr::distinct()
  }

  if(charAliasType == "All") {
    ATTAINSWQX2.0 <- ATTAINSParamRef |>
      dplyr::left_join(WQX_char_alias_filtered, by = c("name"  = "Alias.Name")) |>
      dplyr::select(CharacteristicName = Characteristic.Name, ATTAINS.ParameterName = name, Alias.Type.Name) |>
      dplyr::distinct()
  }

  # remove intermediate variables
  rm(WQX_char_alias_filtered, data)

  ATTAINSParamToWQPCharRef <- ATTAINSWQX2.0

  ATTAINSParamToWQPCharRef
}

# Update CriteriaSearchToolRef Reference Table internal file
# (for internal use only)
TADA_UpdateATTAINSParamToWQPCharRef <- function() {
  utils::write.csv(TADA_GetATTAINSParamToWQPCharRef(), file = "inst/extdata/ATTAINSParamToWQPCharRef.csv", row.names = FALSE)
}

# Used to store cached CriteriaSearchToolRef Reference Table
CriteriaSearchToolRef_Cached <- NULL

#' Criteria Search Tool Reference Table
#'
#' Function downloads and returns the newest available criteria search tool from
#' OST. Table is formatted and cleaned up for easy to
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated ATTAINSParamToWQPCharRef object
#'
#' @export
TADA_GetCriteriaSearchToolRef <- function(){
  CST.raw <- openxlsx::read.xlsx("https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx")

  # Find the first row that has all values populated. This will indicate the column names of the CST data frame.
  # Note: Why not use a static row number? The CST may get new entries that may change the start of the data frames.
  first_filled_row_index <- which(rowSums(is.na(CST.raw)) == 0)[1]

  # Extract our CST column names
  CST.cols <- as.character(CST.raw[first_filled_row_index,])

  # remove rows with "legend" info (rows 1-201)
  CST <- CST.raw[-c(1:first_filled_row_index), ]

  # assign column names to the new data frame
  names(CST) <- CST.cols

  # filter the dataframe to just the CAS and pollutant numbers for our use case.
  CST <- CST |>
    dplyr::select(POLLUTANT_NAME, STD_POLLUTANT_NAME, CAS_NO) |>
    dplyr::distinct()

  # save updated table in cache
  CriteriaSearchToolRef_Cached <- CST

  # remove intermediate objects
  rm(CST.raw, first_filled_row_index, CST.cols)

  return(CST)
}


# Update CriteriaSearchToolRef Reference Table internal file
# (for internal use only)
TADA_UpdateCriteriaSearchToolRef <- function() {
  utils::write.csv(TADA_GetCriteriaSearchToolRef(), file = "inst/extdata/CriteriaSearchToolRef.csv", row.names = FALSE)
}



#' TADA Alias Methodology for ATTAINS, WQP and CST Alias Table for Review
#'
#' This function compares the current WQX Characteristic Alias table
#' of ATTAINS.ParameterNames and WQX CharacteristicNames to the logic.
#' used in this function, which looks at the percentage of words that are
#' a match between each ATTAINS parameters and WQX Characteristics (and
#' within ATTAINS and CST, and CST and WQX by CAS) as an alternative
#' way of finding additional aliases. It is recommended for the TADA
#' team to review this table and decide whether these aliases are
#' accurate, and if so, reach out to the WQX team to add these to the
#' WQX Characteristic Alias table.
#'
#' Note for Development: We should keep a reference file to indicate
#' which rows have already been reviewed during this process.In addition,
#' we can modify the 'strictness' of percent matches. Being more strict
#' can result in less potential match (false negatives) findings while
#' less strict may result in greater number of matches that shouldn't
#' be matched (false positives). Default for now is to be more strict.
#'
#' @param includeCST a Boolean value. If TRUE, this will include columns
#' for CST pollutant Name if it contains an alias between any 3 sources.
#'
#' @return a data frame consisting of potential additional ATTAINS.ParameterName
#' to WQX.CharacteristicName alias for review. TADA team will review and
#' decide if these are appropriate aliases.
#'
#' @export
#'
#' @examples
#' review2 <- TADA_AdditionalCharAliasForReview(includeCST = TRUE)
#' review <- TADA_AdditionalCharAliasForReview()
#'
TADA_AdditionalCharAliasForReview <- function(includeCST = FALSE
                                              # ATTAINS.CST.tolerance,
                                              # CST.ATTAINS.tolerance,
                                              # ATTAINS.WQX.tolerance,
                                              # WQX.ATTAINS.tolerance
) {
  # The current WQX char and ATTAINS Parameter alias table from the WQX
  ATTAINSParamToWQPCharRef <- utils::read.csv(system.file("extdata", "ATTAINSParamToWQPCharRef.csv", package = "EPATADA"))

  # ATTAINSWQX2.0_missing <- ATTAINSParameterWQPCharRef |>
  #   dplyr::group_by(name, Characteristic.Name) |>
  #   dplyr::summarise(count = sum(Description == "ATTAINS.parameter")) |>
  #   dplyr::filter(count == 0)
  #
  # ATTAINSWQX2.0_missing <- ATTAINSWQX2.0_missing |>
  #   dplyr::mutate(Description = "ATTAINS.parameter", Alias.Name = name, Alias.Type.Name = "ATTAINS.PARAMETER", Unique.Identifier = NA, Domain = "Characteristic Alias()", Last.Change.Date = NA)|>
  #   dplyr::select(Domain, Unique.Identifier, Alias.Name, Description, Characteristic.Name, Alias.Type.Name, Last.Change.Date)

  # Extracts all words from each WQX characteristic name
  raw.data <- utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))

  WQXCharacteristicRef <- raw.data |>
    dplyr::rename(
      CharacteristicName = Name,
      Char_Flag = Domain.Value.Status) |>
    # select the columns of interest from the data frame.
    dplyr::select(CharacteristicName, Char_Flag, Comparable.Name, CAS.Number)
  
  # WQX has dashes in the CAS number, remove them to match CST CAS number
  WQXCharacteristicRef$CAS.Number <- gsub("-","", WQXCharacteristicRef$CAS.Number)

  WQXCharacteristicRef2 <- WQXCharacteristicRef |>
    dplyr::mutate(
      name_words = stringr::str_split(CharacteristicName, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% c(" ", "-", "%", "--", "&", "#")) |>
    dplyr::distinct(CharacteristicName, name_words, .keep_all = TRUE)

  WQXCharacteristicRef2$name_words <- toupper(gsub("[^[:alnum:] ]", "", WQXCharacteristicRef2$name_words))
  
  # remove intermediate variable
  rm(raw.data)
  
  # Extracts all words from each ATTAINS Parameter Name
  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("param_name")

  ATTAINSParamRef <- ATTAINS.raw[, "name", drop = FALSE]

  ATTAINSParamRef2 <- ATTAINSParamRef |>
    dplyr::mutate(
      name_words = stringr::str_split(name, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% c(" ", "-", "%", "--", "&", "#")) |>
    dplyr::distinct(name, name_words, .keep_all = TRUE)

  ATTAINSParamRef2$name_words <- gsub("[^[:alnum:] ]", "", ATTAINSParamRef2$name_words)

  # remove intermediate variables
  rm(ATTAINSParamRef, ATTAINS.raw)
  
  # Extracts all words from each CST Pollutant Name
  CST <- utils::read.csv(system.file("extdata", "CriteriaSearchToolRef.csv", package = "EPATADA"))
  CST <- CST |>
    dplyr::mutate(CAS_NO = as.character(CAS_NO))

  CST2 <- CST |>
    dplyr::mutate(
      name_words = stringr::str_split(POLLUTANT_NAME, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% c(" ", "-", "%", "--", "&", "#")) |>
    dplyr::distinct(POLLUTANT_NAME, name_words, .keep_all = TRUE)

  CST2$name_words <- toupper(gsub("[^[:alnum:] ]", "", CST2$name_words))

  # Find matches by WQX char and CAS with CST pollutants
  WQX_CST_CAS_Ref <- WQXCharacteristicRef |>
    dplyr::inner_join(CST, by = c("CAS.Number" = "CAS_NO")) |>
    #dplyr::mutate(ATTAINS.ParameterName = STD_POLLUTANT_NAME) |>
    dplyr::distinct()
  
  # remove intermediate variables
  rm(CST, WQXCharacteristicRef)
  
  # Look for percent word matches 
  temp <- dplyr::right_join(WQXCharacteristicRef2, ATTAINSParamRef2, by = "name_words") |>
    dplyr::distinct(CharacteristicName, name, name_words, .keep_all = TRUE) |>
    dplyr::group_by(CharacteristicName, name) |>
    dplyr::count() |> dplyr::ungroup() |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      percent_match_WQX = n/stringr::str_count(CharacteristicName, "\\S+"),
      percent_match_ATTAINS = n/stringr::str_count(name, "\\S+")
    ) |>
    dplyr::slice_max(order_by = percent_match_WQX + percent_match_ATTAINS ) |>
    dplyr::right_join(WQXCharacteristicRef, by = "CharacteristicName") |>
    dplyr::filter(
      percent_match_WQX + percent_match_ATTAINS > 1
    )

  # less aggressive (prone to more mistake)
  temp <- temp |>
    dplyr::group_by(CharacteristicName) |>
    dplyr::mutate(
      percent_match_WQX = n/stringr::str_count(CharacteristicName, "\\S+"),
      percent_match_ATTAINS = n/stringr::str_count(name, "\\S+")
    ) |>
    dplyr::slice_max(order_by = percent_match_WQX + percent_match_ATTAINS )

  # more aggressive (too strict, can lead to missed matched)
  temp_100 <- temp |>
    dplyr::filter(
      percent_match_WQX == 1 | percent_match_ATTAINS == 1
    )
  
  # Look for percent word matches between ATTAINS and CST
  temp_ATTAINS_CST <- dplyr::right_join(CST2, ATTAINSParamRef2, by = "name_words") |>
    dplyr::distinct(POLLUTANT_NAME, name, name_words, .keep_all = TRUE) |>
    dplyr::group_by(POLLUTANT_NAME, name) |>
    dplyr::count() |> dplyr::ungroup() |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      percent_match_WQX = n/stringr::str_count(POLLUTANT_NAME, "\\S+"),
      percent_match_ATTAINS = n/stringr::str_count(name, "\\S+")
    ) |>
    dplyr::slice_max(order_by = percent_match_WQX + percent_match_ATTAINS ) |>
    dplyr::right_join(CST, by = "POLLUTANT_NAME") |>
    dplyr::filter(
      percent_match_WQX + percent_match_ATTAINS > 1
    )
  
  # less aggressive (prone to more mistake) but identifies more matches
  temp_ATTAINS_CST <- temp_ATTAINS_CST |>
    dplyr::group_by(POLLUTANT_NAME) |>
    dplyr::mutate(
      percent_match_WQX = n/stringr::str_count(POLLUTANT_NAME, "\\S+"),
      percent_match_ATTAINS = n/stringr::str_count(name, "\\S+")
    ) |>
    dplyr::slice_max(order_by = percent_match_WQX + percent_match_ATTAINS )
  
  # more aggressive (too strict, can lead to missed matched)
  temp_100_ATTAINS_CST <- temp_ATTAINS_CST |> 
    dplyr::filter(
      percent_match_WQX == 1 | percent_match_ATTAINS == 1
    )

  # Join by ATTAINS Parameter name and CAS numbers.
  temp_final <- temp_100 |>
    dplyr::full_join(
      temp_100_ATTAINS_CST,
      by = c("name")
    ) |>
    dplyr::mutate(
      CAS.Number = dplyr::coalesce(CAS.Number, CAS_NO)
    ) |>
    dplyr::full_join(
      WQX_CST_CAS_Ref,
      by = c("CAS.Number" )
    ) |>
    dplyr::mutate(
      CharacteristicName = dplyr::coalesce(CharacteristicName.x, CharacteristicName.y),
      Char_Flag = dplyr::coalesce(Char_Flag.x, Char_Flag.y),
      Comparable.Name = dplyr::coalesce(Comparable.Name.x, Comparable.Name.y),
      POLLUTANT_NAME = dplyr::coalesce(POLLUTANT_NAME.x, POLLUTANT_NAME.y),
      STD_POLLUTANT_NAME = dplyr::coalesce(STD_POLLUTANT_NAME.x, STD_POLLUTANT_NAME.y)
    ) |>
    dplyr::select(
      ATTAINS.ParameterName = name,
      CharacteristicName,
      CAS.Number, POLLUTANT_NAME, STD_POLLUTANT_NAME
    ) |>
    dplyr::distinct()
  
  # remove intermediate variables
  rm(temp, temp_100, temp_ATTAINS_CST, temp_100_ATTAINS_CST)
  
  # Compare the crosswalk methodology logic done in TADA temp dataframe (in development still) to the WQX Char Alias table
  WQX_Char_Needs_Review <- ATTAINSParamToWQPCharRef |>
    dplyr::anti_join(temp_final, by = c("ATTAINS.ParameterName", "CharacteristicName")) |>
    dplyr::select(ATTAINS.ParameterName, CharacteristicName) |>
    dplyr::filter(!is.na(CharacteristicName)) |>
    dplyr::distinct()

  # # What source to use for review? temp =
  # ATTAINWQX2.0_non_matched <- temp_100 |>
  #   #dplyr::filter(is.na(Characteristic.Name)) |>
  #   dplyr::anti_join(ATTAINSParameterWQPCharRef, by = c("name", "CharacteristicName" = "Characteristic.Name")) |>
  #   #dplyr::select(ATTAINS.ParameterName = name, CharacteristicName) |>
  #   dplyr::distinct()
  #
  # # from temp, contain 199 additional rows to add onto WQX Char ref (needs to be reviewed)
  # # (may contain more false positives) - may identify additional rows that shouldn't be an alias. Ex. See sediments.
  # ATTAINWQX2.0_non_matched2 <- ATTAINSParameterWQPCharRef |>
  #   dplyr::filter(is.na(Characteristic.Name)) |>
  #   dplyr::left_join(temp, by = c("name")) |>
  #   dplyr::select(ATTAINS.ParameterName = name, CharacteristicName) |>
  #   dplyr::distinct()

  if(includeCST == TRUE){
    # Additional ATTAINS to WQX matches using ATTAINS-WQX-CST-CAS matches using TADA methods.
    ATTAINSWQX2.0_non_matched3 <- temp_final |>
      #dplyr::filter(is.na(Characteristic.Name)) |>
      dplyr::anti_join(ATTAINSParamToWQPCharRef, by = c("ATTAINS.ParameterName", "CharacteristicName")) |>
      #dplyr::select(ATTAINS.ParameterName, CharacteristicName, CAS.Number) |>
      dplyr::distinct()
  }

  if(includeCST == FALSE){
    # Additional ATTAINS to WQX matches using ATTAINS-WQX-CST-CAS matches using TADA methods.
    ATTAINSWQX2.0_non_matched3 <- temp_final |>
      #dplyr::filter(is.na(Characteristic.Name)) |>
      dplyr::anti_join(ATTAINSParamToWQPCharRef, by = c("ATTAINS.ParameterName", "CharacteristicName")) |>
      dplyr::select(ATTAINS.ParameterName, CharacteristicName, CAS.Number) |>
      dplyr::filter(!is.na(ATTAINS.ParameterName)) |>
      #dplyr::mutate(TADA.Status == "Not Reviewed")
    dplyr::distinct()
  }
  
  # remove intermediate variable
  rm(temp_final, WQXCharacteristicRef2, ATTAINSParamRef2, CST2, WQX_CST_CAS_Ref)
  
  return(ATTAINSWQX2.0_non_matched3)
}


# Used to store cached ATTAINSOrgIDsRef Reference Table
ATTAINSOrgIDsRef_Cached <- NULL

#' ATTAINS Organization Identifier Reference Table
#'
#' Function downloads and returns the newest available crosswalk of state/tribe/territory codes
#' and their respective organization names and organization identifiers.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @return Updated sysdata.rda with updated ATTAINSOrgIDsRef object
#'
#' @export
TADA_GetATTAINSOrgIDsRef <- function() {
  # If there is a cached table available return it
  if (!is.null(ATTAINSOrgIDsRef_Cached)) {
    return(ATTAINSOrgIDsRef_Cached)
  }

  # Try to download up-to-date raw data

  raw.data <- tryCatch(
    {
      # get data from ATTAINS
      spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))
    },
    error = function(err) {
      NULL
    }
  )

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest ATTAINS Organization Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "ATTAINSOrgIDsRef.csv", package = "EPATADA")))
  }

  ATTAINSOrgIDsRef <- raw.data %>%
    dplyr::distinct()

  # Save updated table in cache
  ATTAINSOrgIDsRef_Cached <- ATTAINSOrgIDsRef

  ATTAINSOrgIDsRef
}

# Update  ATTAINS Organization Identifier Reference Table
# (for internal use only)
TADA_UpdateATTAINSOrgIDsRef <- function() {
  utils::write.csv(TADA_GetATTAINSOrgIDsRef(), file = "inst/extdata/ATTAINSOrgIDsRef.csv", row.names = FALSE)
}

# Used to store cached ATTAINSParamUseOrg Reference Table
ATTAINSParamUseOrgRef_Cached <- NULL

#' ATTAINS Parameter and Use Name by Organization Reference Key
#'
#' Function downloads and returns the newest available ATTAINS domain values
#' reference dataframe which includes all parameters and uses
#' listed as a cause by ATTAINS organizations in previous assessments.
#' This dataframe is used in TADA_CreateParamRef() and
#' TADA_CreateUseParamRef() as the basis for pulling in prior ATTAINS
#' parameter names and use names by organization name. This helps to filter
#' selections in the Excel drop down menu.
#'
#' @return Dataframe including ATTAINS parameters and uses for each organization.
#'
#' @export
#'
TADA_GetATTAINSParamUseOrgRef <- function() {
  # If there is a cached table available return it
  if (!is.null(ATTAINSParamUseOrgRef_Cached)) {
    return(ATTAINSParamUseOrgRef_Cached)
  }

  # from national download
  nat.assessments <- spsUtil::quiet(rExpertQuery::EQ_NationalExtract("assessments"))

  if (!exists("nat.assessments")) {
    message("Downloading latest ATTAINSParamUseOrg Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA")))
  }

  # considers only the latest cycle form each org, you could skip this step
  # and use params from all assessment cycles - What is preferred?

  latest.assessments <- nat.assessments %>%
    dplyr::group_by(organizationId) %>%
    # dplyr::slice_max(reportingCycle) %>%
    dplyr::select(-objectId) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  latest.params <- latest.assessments %>%
    dplyr::select(
      organizationId, organizationName,
      organizationType, parameterName,
      useName, waterType
    ) %>%
    dplyr::rename(
      ATTAINS.OrganizationIdentifier = organizationId,
      ATTAINS.OrganizationName = organizationName,
      ATTAINS.OrganizationType = organizationType,
      ATTAINS.ParameterName = parameterName,
      ATTAINS.UseName = useName,
      ATTAINS.WaterType = waterType
    ) %>%
    dplyr::distinct()

  # remove intermediate variables
  rm(nat.assessments, latest.assessments)
  # If the download failed fall back to internal data (and report it)

  ATTAINSParamUseOrgRef <- latest.params

  rm(latest.params)

  # Save updated table in cache
  ATTAINSParamUseOrgRef_Cached <- ATTAINSParamUseOrgRef

  ATTAINSParamUseOrgRef
}

# Update ATTAINSParamUseOrg Reference Table internal file
# (for internal use only)
TADA_UpdateATTAINSParamUseOrgRef <- function() {
  utils::write.csv(TADA_GetATTAINSParamUseOrgRef(), file = "inst/extdata/ATTAINSParamUseEntityRef.csv", row.names = FALSE)
}
