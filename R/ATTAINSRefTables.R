# Used to store cached ATTAINSParamToWQPCharRef Reference Table
ATTAINSParamToWQPCharRef_Cached <- NULL

#' ATTAINS Parameter and WQP Characteristic Alias Reference Table
#'
#' Function downloads and returns the newest available crosswalk of alias
#' matches between ATTAINS.ParameterName and TADA.CharacteristicName.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
#'
#' @param charAliasType A string value to indicate the WQX data source to use
#' for finding an ATTAINS parameter name to WQX characteristic name alias.
#' An alias may have been determined from another data source outside of ATTAINS
#' which has an exact spelling to an ATTAINS parameter name that could be used
#' for finding a match.
#'
#' @return Updated sysdata.rda with updated ATTAINSParamToWQPCharRef object
#'
#' @export
TADA_GetATTAINSParamToWQPCharRef <- function(
  charAliasType = c("All", "ATTAINS")
) {
  charAliasType <- match.arg(charAliasType)

  # Pull in WQX Char Alias table.
  temp_zip <- tempfile(fileext = ".zip")

  utils::download.file(
    "https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicAlias_CSV.zip",
    destfile = temp_zip,
    mode = "wb"
  )

  temp_dir <- tempdir() # Create a temporary directory to extract files
  utils::unzip(temp_zip, exdir = temp_dir)

  # If you know the CSV filename
  csv_file_path <- file.path(temp_dir, "Characteristic Alias.csv")

  data <- utils::read.csv(csv_file_path)

  WQX_char_alias_filtered <- data

  WQX_char_alias_filtered$Alias.Name <- toupper(
    WQX_char_alias_filtered$Alias.Name
  )

  # remove intermediate variables
  rm(temp_zip, temp_dir, csv_file_path)

  # retrieve the ATTAINS parameter domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))

  ATTAINSParamRef <- ATTAINS.raw[, "name", drop = FALSE]

  # Create the initial ATTAINS param to WQX char crosswalk
  if (charAliasType == "ATTAINS") {
    WQX_char_alias_filtered <- dplyr::filter(
      WQX_char_alias_filtered,
      Alias.Type.Name == "ATTAINS.PARAMETER"
    )

    ATTAINSWQX <- ATTAINSParamRef |>
      dplyr::left_join(
        WQX_char_alias_filtered,
        by = c("name" = "Alias.Name")
      ) |>
      dplyr::select(
        CharacteristicName = Characteristic.Name,
        ATTAINS.ParameterName = name,
        Alias.Type.Name
      ) |>
      dplyr::mutate(CharacteristicName = toupper(CharacteristicName)) |>
      dplyr::distinct()
  }

  if (charAliasType == "All") {
    ATTAINSWQX <- ATTAINSParamRef |>
      dplyr::left_join(
        WQX_char_alias_filtered,
        by = c("name" = "Alias.Name")
      ) |>
      dplyr::select(
        CharacteristicName = Characteristic.Name,
        ATTAINS.ParameterName = name,
        Alias.Type.Name
      ) |>
      dplyr::mutate(CharacteristicName = toupper(CharacteristicName)) |>
      dplyr::distinct()
  }

  # remove intermediate variables
  rm(WQX_char_alias_filtered, data)

  ATTAINSParamToWQPCharRef <- ATTAINSWQX

  ATTAINSParamToWQPCharRef
}

# Update ATTAINS Parameter and WQP Characteristic Crosswalk
# (for internal use only)
TADA_UpdateATTAINSParamToWQPCharRef <- function() {
  utils::write.csv(
    TADA_GetATTAINSParamToWQPCharRef(),
    file = "inst/extdata/ATTAINSParamToWQPCharRef.csv",
    row.names = FALSE
  )
}


#' TADA Alias Methodology for ATTAINS, WQP and CST Alias Table for Review
#'
#' This function compares the current WQX Characteristic Alias table
#' of ATTAINS.ParameterNames and WQX CharacteristicNames to the logic
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
#' @param displayPercent a Boolean value. If True, this will display the percent
#' match in number of words between the WQX characteristic, ATTAINS parameter
#' and CST pollutant names.
#'
#' @param WQX.ATTAINS.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with ATTAINS.WQX.tolerance which
#' defines the minimum percentage of the number of words that must be found in a
#' WQX characteristic name to an ATTAINS parameter to for it to be considered an alias match.
#'
#' @param ATTAINS.WQX.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with ATTAINS.WQX.tolerance which
#' defines the minimum percentage of the number of words that must be found in an
#' ATTAINS parameter to a WQX characteristic name for it to be considered an alias match.
#'
#' @param ATTAINS.CST.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with CST.ATTAINS.tolerance which
#' defines the minimum percentage of the number of words that must be found in an
#' ATTAINS parameter to a CST pollutant name for it to be considered an alias match.
#'
#' @param CST.ATTAINS.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with ATTAINS.CST.tolerance which
#' defines the minimum percentage of the number of words that must be found in a
#' CST pollutant name to an ATTAINS parameter to for it to be considered an alias match.
#'
#' @return a data frame consisting of potential additional ATTAINS.ParameterName
#' to WQX.CharacteristicName alias for review. TADA team will review and
#' decide if these are appropriate aliases.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' review <- TADA_AdditionalCharAliasForReview()
#' review2 <- TADA_AdditionalCharAliasForReview(includeCST = TRUE)
#'
#' review_more_strict <- TADA_AdditionalCharAliasForReview(
#'   displayPercent = TRUE,
#'   ATTAINS.WQX.tolerance = 1.0,
#'   WQX.ATTAINS.tolerance = 1.0
#' )
#'
#' review_less_strict <- TADA_AdditionalCharAliasForReview(
#'   displayPercent = TRUE,
#'   ATTAINS.WQX.tolerance = 0.5,
#'   WQX.ATTAINS.tolerance = 0.5
#' )
#' }
#'
TADA_AdditionalCharAliasForReview <- function(
  includeCST = FALSE,
  displayPercent = FALSE,
  ATTAINS.CST.tolerance = 1.00,
  CST.ATTAINS.tolerance = 1.00,
  ATTAINS.WQX.tolerance = 1.00,
  WQX.ATTAINS.tolerance = 1.00
) {
  if (
    ATTAINS.CST.tolerance > 1.00 |
      CST.ATTAINS.tolerance > 1.00 |
      ATTAINS.WQX.tolerance > 1.00 |
      WQX.ATTAINS.tolerance > 1.00
  ) {
    stop(
      "One or more tolerance defined is greater than 1.00. Tolerance cannot exceed 100%."
    )
  }

  # The current WQX char and ATTAINS Parameter alias table from the WQX
  ATTAINSParamToWQPCharRef <- utils::read.csv(system.file(
    "extdata",
    "ATTAINSParamToWQPCharRef.csv",
    package = "EPATADA"
  ))

  # Extracts all words from each WQX characteristic name
  raw.data <- TADA_GetCharacteristicRef()

  WQXCharacteristicRef <- raw.data |>
    dplyr::select(CharacteristicName, Char_Flag, Comparable.Name, CAS.Number) |>
    dplyr::distinct()

  # WQX has dashes in the CAS number, remove them to match CST CAS number
  WQXCharacteristicRef$CAS.Number <- gsub(
    "-",
    "",
    WQXCharacteristicRef$CAS.Number
  )

  WQXCharacteristicRef2 <- WQXCharacteristicRef |>
    dplyr::mutate(
      name_words = stringr::str_split(CharacteristicName, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% c(" ", "-", "%", "--", "&", "#")) |>
    dplyr::distinct(CharacteristicName, name_words, .keep_all = TRUE)

  WQXCharacteristicRef2$name_words <- toupper(gsub(
    "[^[:alnum:] ]",
    "",
    WQXCharacteristicRef2$name_words
  ))

  # remove intermediate variable
  rm(raw.data)

  # Extracts all words from each ATTAINS Parameter Name
  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("param_name")

  ATTAINSParamRef <- ATTAINS.raw[, "name", drop = FALSE]

  ATTAINSParamRef2 <- ATTAINSParamRef |>
    dplyr::mutate(name_words = stringr::str_split(name, pattern = " ")) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% c(" ", "-", "%", "--", "&", "#")) |>
    dplyr::distinct(name, name_words, .keep_all = TRUE)

  ATTAINSParamRef2$name_words <- toupper(gsub(
    "[^[:alnum:] ]",
    "",
    ATTAINSParamRef2$name_words
  ))

  # remove intermediate variables
  rm(ATTAINSParamRef, ATTAINS.raw)

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

  CST_ref <- .tada_cst_read_sheet(internal_path, target = "criteria")
  if (is.null(CST_ref)) {
    stop(
      "Failed to read 'Criteria' sheet from internal CST workbook at: ",
      internal_path
    )
  }
  CST_ref <- .tada_cst_prepare_table(CST_ref)

  CST <- CST_ref |>
    dplyr::select(POLLUTANT_NAME, STD_POLLUTANT_NAME, CAS_NO) |>
    dplyr::distinct() |>
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
    dplyr::distinct()

  # Look for percent word matches
  temp_ATTAINS_WQX <- dplyr::right_join(
    WQXCharacteristicRef2,
    ATTAINSParamRef2,
    by = "name_words",
    relationship = "many-to-many"
  ) |>
    dplyr::distinct(CharacteristicName, name, name_words, .keep_all = TRUE) |>
    dplyr::group_by(CharacteristicName, name) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      percent_match_WQX = n / stringr::str_count(CharacteristicName, "\\S+"),
      percent_match_ATTAINS_WQX = n / stringr::str_count(name, "\\S+")
    ) |>
    # ATTAINS param to WQX char must be strict, choose best match only
    dplyr::slice_max(
      order_by = percent_match_WQX + percent_match_ATTAINS_WQX
    ) |>
    dplyr::right_join(
      WQXCharacteristicRef,
      by = "CharacteristicName",
      relationship = "many-to-many"
    )

  temp_ATTAINS_WQX <- temp_ATTAINS_WQX |>
    dplyr::group_by(CharacteristicName) |>
    dplyr::mutate(
      percent_match_WQX = n / stringr::str_count(CharacteristicName, "\\S+"),
      percent_match_ATTAINS_WQX = n / stringr::str_count(name, "\\S+")
    )

  temp_ATTAINS_WQX_Final <- temp_ATTAINS_WQX |>
    dplyr::filter(
      percent_match_WQX >= WQX.ATTAINS.tolerance |
        percent_match_ATTAINS_WQX >= ATTAINS.WQX.tolerance
    )

  # Look for percent word matches between ATTAINS and CST
  temp_ATTAINS_CST <- dplyr::right_join(
    CST2,
    ATTAINSParamRef2,
    by = "name_words",
    relationship = "many-to-many"
  ) |>
    dplyr::distinct(POLLUTANT_NAME, name, name_words, .keep_all = TRUE) |>
    dplyr::group_by(POLLUTANT_NAME, name) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      percent_match_CST = n / stringr::str_count(POLLUTANT_NAME, "\\S+"),
      percent_match_ATTAINS_CST = n / stringr::str_count(name, "\\S+")
    ) |>
    dplyr::right_join(CST, by = "POLLUTANT_NAME", relationship = "many-to-many")

  # remove intermediate variables
  rm(CST, WQXCharacteristicRef)

  temp_ATTAINS_CST <- temp_ATTAINS_CST |>
    dplyr::group_by(POLLUTANT_NAME) |>
    dplyr::mutate(
      percent_match_CST = n / stringr::str_count(POLLUTANT_NAME, "\\S+"),
      percent_match_ATTAINS_CST = n / stringr::str_count(name, "\\S+")
    )

  temp_ATTAINS_CST_Final <- temp_ATTAINS_CST |>
    dplyr::filter(
      percent_match_CST >= CST.ATTAINS.tolerance |
        percent_match_ATTAINS_CST >= ATTAINS.CST.tolerance
    )

  # Join by ATTAINS Parameter name and CAS numbers.
  temp_final <- temp_ATTAINS_WQX_Final |>
    dplyr::full_join(
      temp_ATTAINS_CST_Final,
      by = c("name"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(CAS.Number = dplyr::coalesce(CAS.Number, CAS_NO)) |>
    dplyr::full_join(
      WQX_CST_CAS_Ref,
      by = c("CAS.Number"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      CharacteristicName = dplyr::coalesce(
        CharacteristicName.x,
        CharacteristicName.y
      ),
      Char_Flag = dplyr::coalesce(Char_Flag.x, Char_Flag.y),
      Comparable.Name = dplyr::coalesce(Comparable.Name.x, Comparable.Name.y),
      POLLUTANT_NAME = dplyr::coalesce(POLLUTANT_NAME.x, POLLUTANT_NAME.y),
      STD_POLLUTANT_NAME = dplyr::coalesce(
        STD_POLLUTANT_NAME.x,
        STD_POLLUTANT_NAME.y
      )
    ) |>
    dplyr::select(
      ATTAINS.ParameterName = name,
      CharacteristicName,
      CAS.Number,
      POLLUTANT_NAME,
      STD_POLLUTANT_NAME,
      percent_match_ATTAINS_CST,
      percent_match_CST,
      percent_match_ATTAINS_WQX,
      percent_match_WQX
    ) |>
    dplyr::distinct()

  # remove intermediate variables
  rm(
    temp_ATTAINS_WQX,
    temp_ATTAINS_WQX_Final,
    temp_ATTAINS_CST,
    temp_ATTAINS_CST_Final
  )

  if (includeCST == TRUE) {
    ATTAINSWQX_non_matched <- temp_final |>
      dplyr::filter(!is.na(CharacteristicName)) |>
      dplyr::anti_join(
        ATTAINSParamToWQPCharRef,
        by = c("ATTAINS.ParameterName", "CharacteristicName")
      ) |>
      dplyr::distinct()
  }

  if (includeCST == FALSE) {
    ATTAINSWQX_non_matched <- temp_final |>
      dplyr::filter(!is.na(CharacteristicName)) |>
      dplyr::anti_join(
        ATTAINSParamToWQPCharRef,
        by = c("ATTAINS.ParameterName", "CharacteristicName")
      ) |>
      dplyr::select(
        ATTAINS.ParameterName,
        CharacteristicName,
        CAS.Number,
        percent_match_ATTAINS_WQX,
        percent_match_WQX
      ) |>
      dplyr::filter(!is.na(ATTAINS.ParameterName)) |>
      dplyr::distinct()
  }

  if (displayPercent == FALSE) {
    ATTAINSWQX_non_matched <- ATTAINSWQX_non_matched |>
      dplyr::select(
        -dplyr::any_of(c(
          "percent_match_ATTAINS_CST",
          "percent_match_CST",
          "percent_match_ATTAINS_WQX",
          "percent_match_WQX"
        ))
      )
  }

  # remove intermediate variable
  rm(temp_final, WQXCharacteristicRef2, ATTAINSParamRef2, CST2, WQX_CST_CAS_Ref)

  return(ATTAINSWQX_non_matched)
}


#' TADA Alias Methodology for ATTAINS and CST Uses Alias Table for Review
#'
#' This function prioritizes matching the use name's type for the ATTAINS use_name
#' domain with the Criteria Search Tool (CST) uses. It achieves this by aligning the
#' context2 field from the ATTAINS use_name domain, which acts as a uses category,
#' with the Human Health and Aquatic Life column indicators from the CST.
#'
#' Next, this function then compares ATTAINS.UseName and CST uses by extracting
#' individual words from each use domain string and calculating the percentage
#' of words that match between each ATTAINS use and CST use. Users are advised
#' to review this uses alias table and adjust their tolerance levels as desired
#' to determine the accuracy of the crosswalk.
#'
#' Lastly, if no use matches are found between ATTAINS and the CST, but an ATTAINS
#' parameter matches a CST standard pollutant name for the organization, return
#' all CST uses for each distinct ATTAINS use name. Users must then select the
#' appropriate CST magnitude value(s) to populate for each ATTAINS parameter–use
#' combination.
#'
#' Many-to-many matches are likely and will require thorough review. Users should
#' be aware that a CST use may be duplicated for each ATTAINS.UseName. It is the
#' user's responsibility to ensure that CST uses are appropriately matched to
#' ATTAINS.UseName.
#'
#' Note for Development: We should keep a reference file to indicate
#' which rows have already been reviewed during this process.In addition,
#' we can modify the 'strictness' of percent matches. Being more strict
#' can result in less potential match (false negatives) findings while
#' less strict may result in greater number of matches that shouldn't
#' be matched (false positives). Default for now is to be more strict.
#'
#' @param displayPercent a Boolean value. If True, this will display the percent
#' match in number of words between the WQX characteristic, ATTAINS parameter
#' and CST pollutant names.
#'
#' @param ATTAINS.CST.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with CST.ATTAINS.tolerance which
#' defines the minimum percentage of the number of words that must be found in an
#' ATTAINS parameter to a CST pollutant name for it to be considered an alias match.
#'
#' @param CST.ATTAINS.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with ATTAINS.CST.tolerance which
#' defines the minimum percentage of the number of words that must be found in a
#' CST pollutant name to an ATTAINS parameter to for it to be considered an alias match.
#'
#' @return a data frame consisting of potential additional ATTAINS.ParameterName
#' to WQX.CharacteristicName alias for review. TADA team will review and
#' decide if these are appropriate aliases.
#'
#' @export
#'
#' @examples
#' uses <- TADA_UsesAliasForReview(
#'   ATTAINS.CST.tolerance = 0.50,
#'   CST.ATTAINS.tolerance = 0.15 # uses a lower value as CST uses can be very long string.
#' )
#'
TADA_UsesAliasForReview <- function(
  displayPercent = FALSE,
  ATTAINS.CST.tolerance = 1.00,
  CST.ATTAINS.tolerance = 1.00
) {
  # stop if greater than 1, must be between 0 and 1
  if (ATTAINS.CST.tolerance > 1.00 | CST.ATTAINS.tolerance > 1.00) {
    stop(
      "One or more tolerance defined is greater than 1.00. Tolerance cannot exceed 100%."
    )
  }
  # stop if less than 0, must be between 0 and 1
  if (ATTAINS.CST.tolerance < 0.00 | CST.ATTAINS.tolerance < 0.00) {
    stop(
      "One or more tolerance defined is less than 0. Tolerance cannot be negative."
    )
  }

  # retrieve the ATTAINS use_name domain value from rExpertQuery
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("use_name") |>
    dplyr::select(name, context, context2)

  # Will join ATTAINS Use with an Aquatic Life or Human Health indicator consistent with CST columns
  UsesType <- data.frame(
    context2 = c(
      rep(NA, 3),
      rep("CULTURAL_USE", 2),
      rep("DRINKINGWATER_USE", 2),
      rep("ECOLOGICAL_USE", 2),
      rep("FISHCONSUMPTION_USE", 2),
      rep("OTHER_USE", 3),
      rep("RECREATION_USE", 2)
    ),
    CRITERIATYPEAQUAHUMHLTH = c(
      "A",
      "H",
      NA_character_,
      "H",
      NA_character_,
      "H",
      NA_character_,
      "A",
      NA_character_,
      "H",
      NA_character_,
      "A",
      "H",
      NA_character_,
      "H",
      NA_character_
    )
  )

  # extract the context2 of ATTAINS.UseName for the use "category type" and applies
  # logic to match them to a Human Health or Aquatic Life indicator.
  ATTAINSUseRef <- ATTAINS.raw |>
    dplyr::left_join(
      UsesType,
      by = dplyr::join_by(context2),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      name = toupper(name),
      ATTAINS.OrganizationIdentifier = context
    ) |>
    dplyr::select(-context) |>
    dplyr::distinct()

  # extract each individual ATTAINS.UseName word
  ATTAINSUseRef2 <- ATTAINSUseRef |>
    dplyr::mutate(name_words = stringr::str_split(name, pattern = " ")) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(
      !name_words %in% c(" ", "-", "%", "--", "&", "#", "and", "or")
    ) |>
    dplyr::mutate(
      name_words = toupper(gsub("[^[:alnum:] ]", "", name_words))
    ) |>
    dplyr::filter(name_words != "") |>
    dplyr::distinct(
      ATTAINS.OrganizationIdentifier,
      name,
      name_words,
      .keep_all = TRUE
    )

  # remove intermediate variables
  rm(ATTAINS.raw)

  # pulls in CST and extract relevant columns
  CST <- TADA_CST_GetCriteria()

  # select appropriate columns from the CST
  CST <- CST |>
    dplyr::select(
      ENTITY_ABBR,
      ENTITY_NAME,
      CRITERIATYPEAQUAHUMHLTH,
      CRITERIATYPEFRESHSALTWATER,
      CRITERIATYPE_ACUTECHRONIC,
      USE_CLASS_NAME_LOCATION_ETC
    ) |>
    dplyr::mutate(
      USE_CLASS_NAME_LOCATION_ETC = toupper(USE_CLASS_NAME_LOCATION_ETC)
    ) |>
    dplyr::distinct()

  # Extracts all words from each CST Use
  CST2 <- CST |>
    dplyr::mutate(
      name_words = stringr::str_split(
        USE_CLASS_NAME_LOCATION_ETC,
        pattern = " "
      )
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(
      !name_words %in% c(" ", "-", "%", "--", "&", "#", "and", "or")
    ) |>
    dplyr::mutate(
      name_words = toupper(gsub("[^[:alnum:] ]", "", name_words))
    ) |>
    dplyr::filter(name_words != "") |>
    dplyr::distinct(USE_CLASS_NAME_LOCATION_ETC, name_words, .keep_all = TRUE)

  # match CST Entity with ATTAINS org (best guess using state/tribe name)
  ATTAINSOrgIDsRef <- utils::read.csv(system.file(
    "extdata",
    "ATTAINSOrgIDsRef.csv",
    package = "EPATADA"
  ))
  ATTAINSOrgIDsRef$name <- toupper(ATTAINSOrgIDsRef$name)
  ATTAINS_CST.org <- data.frame(unique(CST[, c(
    "ENTITY_NAME",
    "ENTITY_ABBR"
  )])) |>
    dplyr::mutate(ENTITY_NAME = toupper(ENTITY_NAME)) |>
    dplyr::left_join(ATTAINSOrgIDsRef, by = c("ENTITY_NAME" = "name")) |>
    dplyr::rename(ATTAINS.OrganizationIdentifier = code) |>
    dplyr::select(ENTITY_ABBR, ATTAINS.OrganizationIdentifier)

  # joins the ATTAINS.OrganizationIdentifier in the CST tables
  CST2 <- CST2 |>
    dplyr::mutate(ENTITY_NAME = toupper(ENTITY_NAME)) |>
    dplyr::left_join(ATTAINS_CST.org, by = "ENTITY_ABBR")

  CST <- CST |>
    dplyr::mutate(ENTITY_NAME = toupper(ENTITY_NAME)) |>
    dplyr::left_join(ATTAINS_CST.org, by = "ENTITY_ABBR")

  # matches by org id and CRITERIATYPEAQUAHUMHLTH
  temp_ATTAINS_CST <- dplyr::full_join(
    CST,
    ATTAINSUseRef,
    by = c("ATTAINS.OrganizationIdentifier", "CRITERIATYPEAQUAHUMHLTH"),
    relationship = "many-to-many"
  )

  # Look for percent word matches between ATTAINS and CST as additional matches
  temp_ATTAINS_CST2 <- dplyr::full_join(
    CST2,
    ATTAINSUseRef2,
    by = c("name_words", "ATTAINS.OrganizationIdentifier"),
    relationship = "many-to-many"
  ) |>
    dplyr::distinct(
      USE_CLASS_NAME_LOCATION_ETC,
      name,
      name_words,
      .keep_all = TRUE
    ) |>
    # drops individual words column and extract the word count
    dplyr::group_by(
      ATTAINS.OrganizationIdentifier,
      USE_CLASS_NAME_LOCATION_ETC,
      name,
      context2
    ) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      percent_match_CST = n /
        stringr::str_count(USE_CLASS_NAME_LOCATION_ETC, "\\S+"),
      percent_match_ATTAINS_CST = n / stringr::str_count(name, "\\S+")
    ) |>
    # dplyr::slice_max(
    #   order_by = percent_match_CST + percent_match_ATTAINS_CST
    # ) |>
    dplyr::right_join(
      CST, # Join to table without separated words in each string
      by = c("ATTAINS.OrganizationIdentifier", "USE_CLASS_NAME_LOCATION_ETC"),
      relationship = "many-to-many"
    )
  # dplyr::filter(percent_match_CST + percent_match_ATTAINS_CST > 0)

  # now combine the two tables
  temp_ATTAINS_CST_final <- temp_ATTAINS_CST |>
    dplyr::full_join(
      temp_ATTAINS_CST2,
      by = dplyr::join_by(
        ENTITY_ABBR,
        ENTITY_NAME,
        CRITERIATYPEAQUAHUMHLTH,
        CRITERIATYPEFRESHSALTWATER,
        CRITERIATYPE_ACUTECHRONIC,
        USE_CLASS_NAME_LOCATION_ETC,
        ATTAINS.OrganizationIdentifier,
        name,
        context2
      )
    ) |>
    dplyr::distinct()

  # remove intermediate variables
  rm(
    CST,
    ATTAINSUseRef,
    CST2,
    ATTAINSUseRef2,
    temp_ATTAINS_CST,
    temp_ATTAINS_CST2
  )

  # filter by desired tolerance level defined in the arg inputs
  ATTAINS_CST_Final <- temp_ATTAINS_CST_final |>
    dplyr::filter(
      percent_match_CST >= CST.ATTAINS.tolerance |
        percent_match_ATTAINS_CST >= ATTAINS.CST.tolerance |
        (is.na(percent_match_CST) & is.na(percent_match_ATTAINS_CST))
    )

  if (displayPercent == FALSE) {
    ATTAINS_CST_Final <- ATTAINS_CST_Final |>
      dplyr::select(
        -dplyr::any_of(c("percent_match_ATTAINS_CST", "percent_match_CST"))
      )
  }

  return(ATTAINS_CST_Final)
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
    message(
      "TADA_UpdateATTAINSOrgIDsRef: Downloading latest ATTAINS Organization ID domain options failed! Falling back to (possibly outdated) internal file."
    )
    return(utils::read.csv(system.file(
      "extdata",
      "ATTAINSOrgIDsRef.csv",
      package = "EPATADA"
    )))
  }

  ATTAINSOrgIDsRef <- raw.data |> dplyr::distinct()

  # Save updated table in cache
  ATTAINSOrgIDsRef_Cached <- ATTAINSOrgIDsRef

  ATTAINSOrgIDsRef
}

# Update  ATTAINS Organization Identifier Reference Table
# (for internal use only)
TADA_UpdateATTAINSOrgIDsRef <- function() {
  utils::write.csv(
    TADA_GetATTAINSOrgIDsRef(),
    file = "inst/extdata/ATTAINSOrgIDsRef.csv",
    row.names = FALSE
  )
}

# Used to store cached ATTAINSParamUseOrg Reference Table
ATTAINSParamUseOrgRef_Cached <- NULL

#' ATTAINS Parameter and Use Name by Organization Reference Key
#'
#' Function downloads and returns the newest available ATTAINS domain values
#' reference dataframe which includes all parameters and uses
#' listed as a cause by ATTAINS organizations in previous assessments.
#' This dataframe is used in TADA_ParametersForAnalysis() and
#' TADA_UsesForAnalysis() as the basis for pulling in prior ATTAINS
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
  nat.assessments <- spsUtil::quiet(rExpertQuery::EQ_NationalExtract(
    "assessments"
  ))

  if (!exists("nat.assessments")) {
    message("Downloading latest ATTAINSParamUseOrg Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file(
      "extdata",
      "ATTAINSParamUseEntityRef.csv",
      package = "EPATADA"
    )))
  }

  # considers only the latest cycle form each org, you could skip this step
  # and use params from all assessment cycles - What is preferred?

  latest.assessments <- nat.assessments |>
    dplyr::group_by(organizationId) |>
    # dplyr::slice_max(reportingCycle) |>
    dplyr::select(-objectId) |>
    dplyr::distinct() |>
    dplyr::ungroup()

  latest.params <- latest.assessments |>
    dplyr::select(
      organizationId,
      organizationName,
      organizationType,
      parameterName,
      useName,
      waterType
    ) |>
    dplyr::rename(
      ATTAINS.OrganizationIdentifier = organizationId,
      ATTAINS.OrganizationName = organizationName,
      ATTAINS.OrganizationType = organizationType,
      ATTAINS.ParameterName = parameterName,
      ATTAINS.UseName = useName,
      ATTAINS.WaterType = waterType
    ) |>
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
  utils::write.csv(
    TADA_GetATTAINSParamUseOrgRef(),
    file = "inst/extdata/ATTAINSParamUseEntityRef.csv",
    row.names = FALSE
  )
}
