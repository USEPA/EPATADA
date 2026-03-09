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
  ref <- utils::read.csv(system.file(
    "extdata",
    "NPsummation_key.csv",
    package = "EPATADA"
  ))
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
#' @param .data TADA dataframe. If a dataframe is not provided, the function will return the default internal reference table.
#'
#' @return Synonym Reference Table unique to the input dataframe
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_6Tribes_5y)
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
    ref <- utils::read.csv(system.file(
      "extdata",
      "HarmonizationTemplate.csv",
      package = "EPATADA"
    ))
    return(ref)
  }

  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  TADA_CheckColumns(.data, expected_cols)

  if (
    !any(
      c(
        "TADA.MethodSpeciation.Flag",
        "TADA.SampleFraction.Flag",
        "TADA.ResultUnit.Flag"
      ) %in%
        names(.data)
    )
  ) {
    print(
      "Warning: This dataframe is missing TADA QC flagging columns, indicating that you have not yet run the TADA_FlagResultUnit, TADA_FlagFraction, or TADA_FlagSpeciation functions. It is highly recommended you run these flagging functions and remove Suspect combinations before proceeding to this step."
    )
  }

  # check to see if any suspect data flags exist
  check_inv <- .data[,
    names(.data) %in%
      c(
        "TADA.MethodSpeciation.Flag",
        "TADA.SampleFraction.Flag",
        "TADA.ResultUnit.Flag"
      )
  ]
  check_inv <- check_inv |>
    tidyr::pivot_longer(cols = names(check_inv), names_to = "Flag_Column") |>
    dplyr::filter(value == "Suspect")

  if (dim(check_inv)[1] > 0) {
    check_inv <- check_inv |>
      dplyr::group_by(Flag_Column) |>
      dplyr::summarise("Result Count" = length(value))
    print(
      "Warning: Your dataframe contains suspect metadata combinations in the following flag columns:"
    )
    print(as.data.frame(check_inv))
  }

  # execute function after checks are passed
  # define raw harmonization table as an object
  harm.raw <- utils::read.csv(system.file(
    "extdata",
    "HarmonizationTemplate.csv",
    package = "EPATADA"
  ))

  join.data <- merge(
    unique(.data[, expected_cols]),
    harm.raw,
    by = expected_cols,
    all.x = TRUE
  )

  # trim join.data to include only unique combos of char-frac-spec-unit
  unique.data <- join.data |> dplyr::distinct()

  unique.data <- unique.data[, names(harm.raw)]

  # return unique.data
  return(unique.data)
}


#' Nutrient Summation Reference Key
#'
#' This internal reference file includes USGS only units/speciations. It was
#' created in July 2023 using the pcodes domain table from NWIS. All USGS units
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
  ref <- utils::read.csv(system.file(
    "extdata",
    "USGS_units_speciation.csv",
    package = "EPATADA"
  ))
  return(ref)
}


# Used to store cached TADACharAliasRef Reference Table
TADACharAliasRef_Cached <- NULL

#' ATTAINS Parameter, CST Pollutant and WQP Characteristic Alias Reference Table
#'
#' Function downloads and returns the newest available crosswalk of alias matches
#' between ATTAINS.ParameterName, CST.PollutantName and TADA.CharacteristicName
#' from the WQX Characteristic alias domain and joins in additional potential
#' matches using TADA logic between the 3 sources for review. Note: A 'review'
#' column is included which will be kept in the inst/extdata folder and must be
#' labeled as 'approved' to keep a running list of alias that has been reviewed -
#' otherwise, updating this alias table will not remember what has already been
#' reviewed.
#'
#' This function caches the table after it has been called once
#' so subsequent calls will be faster.
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
#' CST pollutant name to an ATTAINS parameter for it to be considered an alias match.
#'
#' @param CST.WQX.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with WQX.CST.tolerance which
#' defines the minimum percentage of the number of words that must be found in an
#' WQX characteristic to a CST pollutant name for it to be considered an alias match.
#'
#' @param WQX.CST.tolerance a numeric value ranging from 0 to 1 (0% to 100%).
#' Default is 100%. This value is an OR condition with CST.WQX.tolerance which
#' defines the minimum percentage of the number of words that must be found in a
#' CST pollutant name to a WQX characteristic for it to be considered an alias match.
#'
#' @param set.all.tolerance optional: default is NA, if a user specifies a numeric
#' value ranging from 0 to 1 (0% to 100%), this will populate all tolerances to
#' this value.
#'
#' @return updated rda consisting of potential additional ATTAINS.ParameterName
#' to WQX.CharacteristicName alias for review. TADA team will review and
#' decide if these are appropriate aliases.
#'
#' @export
#'
TADA_GetTADACharAliasRef <- function(
  ATTAINS.CST.tolerance = 1.00,
  CST.ATTAINS.tolerance = 1.00,
  ATTAINS.WQX.tolerance = 1.00,
  WQX.ATTAINS.tolerance = 1.00,
  CST.WQX.tolerance = 1.00,
  WQX.CST.tolerance = 1.00,
  set.all.tolerance = NA
) {
  # if set.all.tolerance is populated, populate all tolerance limits with same value.
  if (!is.na(set.all.tolerance)) {
    ATTAINS.CST.tolerance <- CST.ATTAINS.tolerance <- WQX.ATTAINS.tolerance <- ATTAINS.WQX.tolerance <- CST.WQX.tolerance <- WQX.CST.tolerance <- set.all.tolerance
  }
  # stop if greater than 1, must be between 0 and 1
  if (
    any(
      ATTAINS.CST.tolerance > 1.00,
      CST.ATTAINS.tolerance > 1.00,
      ATTAINS.WQX.tolerance > 1.00,
      WQX.ATTAINS.tolerance > 1.00,
      CST.WQX.tolerance > 1.00,
      WQX.CST.tolerance > 1.00
    )
  ) {
    stop(
      "One or more tolerance defined is greater than 1.00. Tolerance cannot exceed 100%."
    )
  }
  # stop if less than 0, must be between 0 and 1
  if (
    any(
      ATTAINS.CST.tolerance < 0.00,
      CST.ATTAINS.tolerance < 0.00,
      ATTAINS.WQX.tolerance < 0.00,
      WQX.ATTAINS.tolerance < 0.00,
      CST.WQX.tolerance < 0.00,
      WQX.CST.tolerance < 0.00
    )
  ) {
    stop(
      "One or more tolerance defined is less than 0.00. Tolerance cannot be less than 0%."
    )
  }

  # If there is a cached table available return it
  if (!is.null(TADACharAliasRef_Cached)) {
    return(TADACharAliasRef_Cached)
  }

  # pull in most recent TADACharAliasRef in EPATADA
  current_TADACharAlias <- utils::read.csv(system.file(
    "extdata",
    "TADACharAliasRef.csv",
    package = "EPATADA"
  ))

  # identifies how many WQX-ATTAINS-CST alias are already approved or rejected in the current inst/extdata csv file
  print(paste(
    "The current 'TADACharAliasRef.csv' file in EPATADA inst/extdata folder contains",
    sum(current_TADACharAlias$Status != "Needs review"),
    "that have already been reviewed (see 'Status' column as approved or rejected). These rows will not be replaced and will be returned in the new output of this function's run. Any additional rows that are found as potential alias will be appended.",
    "If you would like to made edits to this alias table, open the file and modify the Status column."
  ))

  # for word matching method, create a list of common stop words and punctuation marks to exclude
  stop_words <- c(
    "a",
    "an",
    "for",
    "and",
    "nor",
    "but",
    "or",
    "yet",
    "so",
    "the",
    "!",
    "\"",
    "#",
    "$",
    "%",
    "&",
    "'",
    "(",
    ")",
    "*",
    "+",
    ",",
    "-",
    ".",
    "/",
    ":",
    ";",
    "<",
    "=",
    ">",
    "?",
    "@",
    "[",
    "\\",
    "]",
    "^",
    "_",
    "`",
    "{",
    "|",
    "}",
    "~",
    "-",
    # any additional words added below that TADA developers can add below in their review:
    "(%)",
    "--"
  )

  # retrieve WQX, ATTAINS and CST domains
  # retrieve WQX characteristic names
  raw.data <- TADA_GetCharacteristicRef()

  WQXCharacteristicRef <- raw.data |>
    dplyr::select(CharacteristicName, Char_Flag, Comparable.Name, CAS.Number) |>
    dplyr::mutate(dplyr::across(where(is.character), toupper)) |>
    dplyr::distinct()

  # WQX has dashes in the CAS number, remove them to match CST CAS number
  WQXCharacteristicRef$CAS.Number <- gsub(
    "-",
    "",
    WQXCharacteristicRef$CAS.Number
  )

  # Extracts all words from each WQX characteristic name (remove extra)
  WQXCharacteristicRef2 <- WQXCharacteristicRef |>
    dplyr::mutate(
      name_words = stringr::str_split(CharacteristicName, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
    # fix spelling error
    dplyr::mutate(
      name_words = dplyr::if_else(
        name_words == "KJEHLDAL",
        "KJELDAHL",
        name_words
      )
    ) |>
    dplyr::distinct(CharacteristicName, name_words, .keep_all = TRUE)

  WQXCharacteristicRef2$name_words <- toupper(gsub(
    "[^[:alnum:] ]",
    "",
    WQXCharacteristicRef2$name_words
  ))

  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("param_name")

  ATTAINSParamRef <- ATTAINS.raw[, "name", drop = FALSE]

  # extracts all words from each ATTAINS Parameter Name
  ATTAINSParamRef2 <- ATTAINSParamRef |>
    dplyr::mutate(name_words = stringr::str_split(name, pattern = " ")) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
    # fix spelling error
    dplyr::mutate(
      name_words = dplyr::if_else(
        name_words == "KJEHLDAHL",
        "KJELDAHL",
        name_words
      )
    ) |>
    dplyr::distinct(name, name_words, .keep_all = TRUE)

  ATTAINSParamRef2$name_words <- toupper(gsub(
    "[^[:alnum:] ]",
    "",
    ATTAINSParamRef2$name_words
  ))

  # Extract CST Criteria from the internal workbook only; error if missing/unreadable
  CST.raw <- TADA_CST_GetCriteria()

  # extract unique relevant columns
  CST <- CST.raw |>
    dplyr::select(POLLUTANT_NAME, STD_POLLUTANT_NAME, CAS_NO) |>
    dplyr::mutate(dplyr::across(where(is.character), toupper)) |>
    dplyr::distinct() |>
    dplyr::mutate(CAS_NO = as.character(CAS_NO))

  # Extracts all words from each CST Pollutant Name
  CST2 <- CST |>
    dplyr::mutate(
      name_words = stringr::str_split(POLLUTANT_NAME, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
    # fix spelling error
    dplyr::mutate(
      name_words = dplyr::if_else(
        name_words == "KJEHLDAL",
        "KJELDAHL",
        name_words
      )
    ) |>
    dplyr::distinct(POLLUTANT_NAME, name_words, .keep_all = TRUE)

  CST2$name_words <- toupper(gsub("[^[:alnum:] ]", "", CST2$name_words))

  # remove intermediate variables
  rm(raw.data, ATTAINS.raw, CST.raw)

  # Step 1:
  # Look for percent word matches between ATTAINS and WQX
  # inner join is being used to show matches that were found as an alias.
  TADARef_ATTAINS_WQX <- dplyr::inner_join(
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
      percent_match_WQX_ATTAINS = n /
        stringr::str_count(CharacteristicName, "\\S+"),
      percent_match_ATTAINS_WQX = n / stringr::str_count(name, "\\S+")
    ) |>
    # ATTAINS param to WQX char must be strict, choose best match only using slice_max
    dplyr::slice_max(
      order_by = percent_match_WQX_ATTAINS + percent_match_ATTAINS_WQX
    ) |>
    dplyr::right_join(
      WQXCharacteristicRef,
      by = "CharacteristicName",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      (percent_match_WQX_ATTAINS >= WQX.ATTAINS.tolerance |
        percent_match_ATTAINS_WQX >= ATTAINS.WQX.tolerance) &
        percent_match_WQX_ATTAINS + percent_match_ATTAINS_WQX > 1 # minimum match tolerance set
    )

  # step 2: CST and ATTAINS
  # Look for percent word matches between ATTAINS and CST
  # inner join is being used to show matches that were found as an alias.
  TADARef_ATTAINS_CST <- dplyr::inner_join(
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
      percent_match_CST_ATTAINS = n /
        stringr::str_count(POLLUTANT_NAME, "\\S+"),
      percent_match_ATTAINS_CST = n / stringr::str_count(name, "\\S+")
    ) |>
    # Note: make ATTAINS to CST crosswalk less strict to ensure all potential parameters in ATTAINS can be populated with CST magnitude values.
    #       Removal of extra rows in criteria table may be needed by the TADA user to reflect their org's assessment needs.
    # However, if CST to ATTAINS match must be strict, choose best match only using slice_max
    # dplyr::slice_max(
    #   order_by = percent_match_CST_ATTAINS + percent_match_ATTAINS_CST
    # ) |>
    # pulls in STD_POLLUTANT_NAME
    dplyr::right_join(
      CST,
      by = "POLLUTANT_NAME",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      (percent_match_CST_ATTAINS >= CST.ATTAINS.tolerance |
        percent_match_ATTAINS_CST >= ATTAINS.CST.tolerance) &
        percent_match_CST_ATTAINS + percent_match_ATTAINS_CST > 1 # minimum match tolerance set
    )

  # step 3: CST and WQX
  # Look for percent word matches between CST and WQX
  # inner join is being used to show matches that were found as an alias.
  TADARef_CST_WQX <- dplyr::inner_join(
    WQXCharacteristicRef2,
    CST2,
    by = "name_words",
    relationship = "many-to-many"
  ) |>
    dplyr::distinct(
      CharacteristicName,
      POLLUTANT_NAME,
      name_words,
      .keep_all = TRUE
    ) |>
    dplyr::group_by(CharacteristicName, POLLUTANT_NAME) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(POLLUTANT_NAME) |>
    dplyr::mutate(
      percent_match_WQX_CST = n /
        stringr::str_count(CharacteristicName, "\\S+"),
      percent_match_CST_WQX = n / stringr::str_count(POLLUTANT_NAME, "\\S+")
    ) |>
    # If CST to WQX char must be strict, choose best match only using slice_max
    dplyr::slice_max(
      order_by = percent_match_WQX_CST + percent_match_CST_WQX
    ) |>
    # pulls in STD_POLLUTANT_NAME
    dplyr::right_join(
      CST,
      by = "POLLUTANT_NAME",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      (percent_match_WQX_CST >= WQX.CST.tolerance |
        percent_match_CST_WQX >= CST.WQX.tolerance) &
        percent_match_WQX_CST + percent_match_CST_WQX > 1 # minimum match tolerance set
    )

  # remove intermediate variables
  rm(stop_words, ATTAINSParamRef2, CST2, WQXCharacteristicRef2)

  # Step 4: pull in WQXCharAliasRef table
  WQXCharAliasRef <- TADA_GetWQXCharAliasRef()

  # WQX to ATTAINS
  WQXRef_ATTAINS_WQX <- WQXCharAliasRef |>
    dplyr::filter(Alias.Type.Name %in% c("ATTAINS.PARAMETER")) |>
    dplyr::select(
      CharacteristicName = Characteristic.Name,
      ATTAINS.ParameterName = Alias.Name,
      Last.Change.Date
    )
  # WQX to CST.Pollutant
  WQXRef_CST_WQX <- WQXCharAliasRef |>
    dplyr::filter(Alias.Type.Name %in% c("CST.POLLUTANT")) |>
    dplyr::select(
      CharacteristicName = Characteristic.Name,
      POLLUTANT_NAME = Alias.Name,
      Last.Change.Date
    ) |>
    dplyr::left_join(
      dplyr::select(WQXCharacteristicRef, CharacteristicName, CAS.Number),
      by = "CharacteristicName"
    )

  # step 4: find pairwise combos from WQX_ATTAINS and WQX_CST (Note: The WQXCharRef table is based on WQX char, thus CST_ATTAINS is a standalone table from TADA.Alias)
  # find any additional ATTAINS_WQX from TADA alias match
  ATTAINS_WQX_Final <- WQXRef_ATTAINS_WQX |>
    dplyr::mutate(
      CharacteristicName = toupper(CharacteristicName),
      ATTAINS.ParameterName = toupper(ATTAINS.ParameterName)
    ) |>
    dplyr::full_join(
      TADARef_ATTAINS_WQX,
      c("ATTAINS.ParameterName" = "name", "CharacteristicName")
    ) |>
    dplyr::select(CharacteristicName, ATTAINS.ParameterName) |>
    dplyr::distinct()

  # Now find additional CST_WQX crosswalk from TADA alias match and bind it with what is found in WQX
  CST_WQX_Final <- WQXRef_CST_WQX |>
    dplyr::mutate(
      POLLUTANT_NAME = toupper(POLLUTANT_NAME),
      CharacteristicName = toupper(CharacteristicName)
    ) |>
    dplyr::left_join(CST, by = "POLLUTANT_NAME") |>
    dplyr::full_join(
      TADARef_CST_WQX,
      c("POLLUTANT_NAME", "STD_POLLUTANT_NAME", "CharacteristicName")
    ) |>
    dplyr::select(CharacteristicName, STD_POLLUTANT_NAME, POLLUTANT_NAME) |>
    dplyr::filter(!is.na(STD_POLLUTANT_NAME)) |>
    dplyr::distinct()

  ATTAINS_WQX_CST_Final <- ATTAINS_WQX_Final |>
    # first, we join ATTAINS_WQX with CST_WQX to get ATTAINS_WQX_CST, the preliminary 3-way table.
    dplyr::full_join(
      CST_WQX_Final,
      by = "CharacteristicName",
      relationship = "many-to-many"
    ) |>
    # now, join the TADA ref table of ATTAINS_CST from the TADA.alias match method to find any additional matches.
    # recall that ATTAINS and CST is less strict in its word matches as CST is for discovery (users will review a larger list of crosswalk filtered by their org)
    dplyr::full_join(
      dplyr::select(
        TADARef_ATTAINS_CST,
        ATTAINS.ParameterName = name,
        POLLUTANT_NAME,
        STD_POLLUTANT_NAME
      ),
      by = c("ATTAINS.ParameterName"), # only join by ATTAINS as there may be additional ATTAINS and CST matches that were not matched from ATTAINS_WQX_CST table.
      relationship = "many-to-many"
    ) |>
    # show additional rows to account for pairwise combinations
    tidyr::pivot_longer(
      cols = tidyr::matches("\\.x$|\\.y$"), # Selects columns ending in .x or .y
      names_to = c(".value", "source"),
      names_sep = "\\.",
      values_drop_na = FALSE
    ) |>
    dplyr::group_by(CharacteristicName, ATTAINS.ParameterName) |>
    tidyr::fill(POLLUTANT_NAME, .direction = "downup") |>
    tidyr::fill(STD_POLLUTANT_NAME, .direction = "downup") |>
    dplyr::ungroup() |>
    # ensures all ATTAINS.ParameterName are included
    dplyr::full_join(
      ATTAINSParamRef,
      by = c("ATTAINS.ParameterName" = "name")
    ) |>
    # populate the CAS NO
    dplyr::left_join(
      dplyr::select(WQXCharacteristicRef, CharacteristicName, CAS.Number),
      by = "CharacteristicName"
    ) |>
    dplyr::left_join(CST, by = c("POLLUTANT_NAME", "STD_POLLUTANT_NAME")) |>
    dplyr::select(
      CharacteristicName,
      ATTAINS.ParameterName,
      POLLUTANT_NAME,
      STD_POLLUTANT_NAME,
      WQX_CAS_NO = CAS.Number,
      CST_CAS_NO = CAS_NO
    ) |>
    # reject mismatching CAS
    dplyr::mutate(
      Status = "Needs review",
      Status.Notes = dplyr::case_when(
        !is.na(WQX_CAS_NO) &
          !is.na(CST_CAS_NO) &
          WQX_CAS_NO != "" &
          WQX_CAS_NO != CST_CAS_NO ~ paste0(
          "TADA_GetTADACharAliasRef() recommendation: 'rejected' based on mismatching CAS_NO at tolerances of: WQX.ATTAINS = ",
          WQX.ATTAINS.tolerance,
          " ATTAINS.WQX = ",
          ATTAINS.WQX.tolerance,
          " CST.WQX = ",
          CST.WQX.tolerance,
          " WQX.CST = ",
          WQX.CST.tolerance,
          " ATTAINS.CST = ",
          ATTAINS.CST.tolerance,
          " CST.ATTAINS = ",
          CST.ATTAINS.tolerance
        ),
        WQX_CAS_NO == CST_CAS_NO ~ paste0(
          "TADA_GetTADACharAliasRef() recommendation: 'approved' based on matching CAS_NO at tolerances of: WQX.ATTAINS = ",
          WQX.ATTAINS.tolerance,
          " ATTAINS.WQX = ",
          ATTAINS.WQX.tolerance,
          " CST.WQX = ",
          CST.WQX.tolerance,
          " WQX.CST = ",
          WQX.CST.tolerance,
          " ATTAINS.CST = ",
          ATTAINS.CST.tolerance,
          " CST.ATTAINS = ",
          CST.ATTAINS.tolerance
        ),
        TRUE ~ paste0(
          "Manual 'Status' assignment needed at tolerances of: WQX.ATTAINS = ",
          WQX.ATTAINS.tolerance,
          " ATTAINS.WQX = ",
          ATTAINS.WQX.tolerance,
          " CST.WQX = ",
          CST.WQX.tolerance,
          " WQX.CST = ",
          WQX.CST.tolerance,
          " ATTAINS.CST = ",
          ATTAINS.CST.tolerance,
          " CST.ATTAINS = ",
          CST.ATTAINS.tolerance
        )
      )
    ) |>
    dplyr::distinct()

  # remove intermediate variables
  rm(
    TADARef_ATTAINS_CST,
    TADARef_ATTAINS_WQX,
    TADARef_CST_WQX,
    WQXRef_ATTAINS_WQX,
    WQXRef_CST_WQX,
    WQXCharAliasRef,
    CST,
    WQXCharacteristicRef,
    ATTAINSParamRef
  )

  # step 5: identify any current TADA.AliasMatch that have been reviewed in TADA inst/extdata already - but may not have been submitted/updated in WQX Char ref domain
  TADA_reviewed_list <- current_TADACharAlias |> # current_TADACharAlias pulled in during beginning of this function.
    dplyr::filter(Status != "Needs review") |>
    dplyr::mutate(
      WQX_CAS_NO = as.character(WQX_CAS_NO),
      CST_CAS_NO = as.character(CST_CAS_NO)
    )

  # remove matching rows from current TADACharRef in the TADA internal folder that have been approved or rejected, and bind them to the updated TADACharRef.
  TADACharAliasRef <- ATTAINS_WQX_CST_Final |>
    dplyr::mutate(
      WQX_CAS_NO = as.character(WQX_CAS_NO),
      CST_CAS_NO = as.character(CST_CAS_NO)
    ) |>
    dplyr::filter(
      !(ATTAINS.ParameterName %in%
        TADA_reviewed_list$ATTAINS.ParameterName &
        CharacteristicName %in% TADA_reviewed_list$CharacteristicName &
        POLLUTANT_NAME %in% TADA_reviewed_list$POLLUTANT_NAME &
        STD_POLLUTANT_NAME %in% TADA_reviewed_list$STD_POLLUTANT_NAME &
        WQX_CAS_NO %in% TADA_reviewed_list$WQX_CAS_NO &
        CST_CAS_NO %in% TADA_reviewed_list$CST_CAS_NO)
    ) |>
    dplyr::bind_rows(dplyr::mutate(TADA_reviewed_list))

  # Save updated table in cache
  TADACharAliasRef_Cached <- TADACharAliasRef

  # returns final table
  TADACharAliasRef
}

# Update TADACharAlias Reference Table internal file
# (for internal use only)

.TADA_UpdateTADACharAliasRef <- function(
  ATTAINS.CST.tolerance = 1.00,
  CST.ATTAINS.tolerance = 1.00,
  ATTAINS.WQX.tolerance = 1.00,
  WQX.ATTAINS.tolerance = 1.00,
  CST.WQX.tolerance = 1.00,
  WQX.CST.tolerance = 1.00,
  set.all.tolerance = NA
) {
  utils::write.csv(
    TADA_GetTADACharAliasRef(
      ATTAINS.CST.tolerance = ATTAINS.CST.tolerance,
      CST.ATTAINS.tolerance = CST.ATTAINS.tolerance,
      ATTAINS.WQX.tolerance = ATTAINS.WQX.tolerance,
      WQX.ATTAINS.tolerance = WQX.ATTAINS.tolerance,
      CST.WQX.tolerance = CST.WQX.tolerance,
      WQX.CST.tolerance = WQX.CST.tolerance,
      set.all.tolerance = set.all.tolerance
    ),
    file = "inst/extdata/TADACharAliasRef.csv",
    row.names = FALSE
  )
}


# Used to store cached TADAUsesAliasRef Reference Table
TADAUsesAliasRef_Cached <- NULL

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
#' appropriate CST magnitude value(s) to populate for each ATTAINS parameter and use
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
#' @param set.all.tolerance optional: default is NA, if a user specifies a numeric
#' value ranging from 0 to 1 (0% to 100%), this will populate all tolerances to
#' this value.
#'
#' @return a data frame consisting of potential additional ATTAINS.ParameterName
#' to WQX.CharacteristicName alias for review. TADA team will review and
#' decide if these are appropriate aliases.
#'
#' @export
#'
TADA_GetTADAUsesAliasRef <- function(
  ATTAINS.CST.tolerance = 0.15,
  CST.ATTAINS.tolerance = 0.15,
  set.all.tolerance = NA
) {
  # if set.all.tolerance is populated, populate all tolerance limits with same value.
  if (!is.na(set.all.tolerance)) {
    ATTAINS.CST.tolerance <- CST.ATTAINS.tolerance <- set.all.tolerance
  }
  # stop if greater than 1, must be between 0 and 1
  if (ATTAINS.CST.tolerance > 1.00 | CST.ATTAINS.tolerance > 1.00) {
    stop(
      "One or more tolerance defined is greater than 1.00. Tolerance cannot exceed 100%."
    )
  }
  # stop if less than 0, must be between 0 and 1
  if (ATTAINS.CST.tolerance < 0.00 | CST.ATTAINS.tolerance < 0.00) {
    stop(
      "One or more tolerance defined is less than 0.00. Tolerance cannot be less than 0%."
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

  # for word matching method, create a list of common stop words and punctuation marks to exclude
  stop_words <- c(
    "a",
    "an",
    "for",
    "and",
    "nor",
    "but",
    "or",
    "yet",
    "so",
    "the",
    "!",
    "\"",
    "#",
    "$",
    "%",
    "&",
    "'",
    "(",
    ")",
    "*",
    "+",
    ",",
    "-",
    ".",
    "/",
    ":",
    ";",
    "<",
    "=",
    ">",
    "?",
    "@",
    "[",
    "\\",
    "]",
    "^",
    "_",
    "`",
    "{",
    "|",
    "}",
    "~",
    "-",
    # any additional words added below that TADA developers can add below in their review:
    "(%)",
    "--"
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
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
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

  # Extract CST Criteria from the internal workbook only; error if missing/unreadable
  CST.raw <- TADA_CST_GetCriteria()

  # extract unique relevant columns
  CST <- CST.raw

  # select appropriate columns from the CST
  CST <- CST.raw |>
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
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
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
  ATTAINS_CST <- dplyr::full_join(
    CST,
    ATTAINSUseRef,
    by = c("ATTAINS.OrganizationIdentifier", "CRITERIATYPEAQUAHUMHLTH"),
    relationship = "many-to-many"
  )

  # Look for percent word matches between ATTAINS and CST as additional matches
  ATTAINS_CST2 <- dplyr::full_join(
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
  ATTAINS_CST_final <- ATTAINS_CST |>
    dplyr::full_join(
      ATTAINS_CST2,
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
    CST.raw,
    ATTAINSUseRef,
    CST2,
    ATTAINSUseRef2,
    ATTAINS_CST,
    ATTAINS_CST2
  )

  # filter by desired tolerance level defined in the arg inputs
  TADAUsesAliasRef <- ATTAINS_CST_final |>
    dplyr::filter(
      percent_match_CST >= CST.ATTAINS.tolerance |
        percent_match_ATTAINS_CST >= ATTAINS.CST.tolerance |
        (is.na(percent_match_CST) & is.na(percent_match_ATTAINS_CST))
    ) |>
    dplyr::mutate(
      ATTAINS.UseName = name,
      review = "New row: Needs Review",
      Last.Change.Date = NA
    ) |>
    dplyr::select(-n, -name)

  # drop percentages
  TADAUsesAliasRef <- TADAUsesAliasRef |>
    dplyr::select(
      -dplyr::any_of(c("percent_match_ATTAINS_CST", "percent_match_CST"))
    )

  # lastly, pull in the current TADAUsesAlias Ref table in TADA inst/extdata that have been reviewed.
  current_TADAUsesAlias <- utils::read.csv(system.file(
    "extdata",
    "TADAUsesAliasRef.csv",
    package = "EPATADA"
  ))

  # filter those that have been reviewed.
  current_TADAUsesAlias <- current_TADAUsesAlias |>
    # dplyr::filter( review != "New Row: Needs Review") # Note: we can probably filter it by this line instead.
    dplyr::filter(review == "APPROVED" | review == "REJECTED")

  # keep rows that exist in current TADACharRef that do not have a match with the new ref
  TADA_reviewed_list <- current_TADAUsesAlias |>
    dplyr::anti_join(
      TADAUsesAliasRef,
      by = dplyr::join_by(
        ENTITY_ABBR,
        ENTITY_NAME,
        CRITERIATYPEAQUAHUMHLTH,
        CRITERIATYPEFRESHSALTWATER,
        CRITERIATYPE_ACUTECHRONIC,
        USE_CLASS_NAME_LOCATION_ETC,
        ATTAINS.OrganizationIdentifier,
        context2,
        ATTAINS.UseName,
        review,
        Last.Change.Date
      ),
      na_matches = "na"
    )

  # return rows from current TADAUsesRef in the TADA internal folder
  TADAUsesAliasRef <- TADAUsesAliasRef |>
    dplyr::anti_join(
      TADA_reviewed_list,
      by = dplyr::join_by(
        ENTITY_ABBR,
        ENTITY_NAME,
        CRITERIATYPEAQUAHUMHLTH,
        CRITERIATYPEFRESHSALTWATER,
        CRITERIATYPE_ACUTECHRONIC,
        USE_CLASS_NAME_LOCATION_ETC,
        ATTAINS.OrganizationIdentifier,
        context2,
        ATTAINS.UseName
      )
    ) |>
    dplyr::bind_rows(TADA_reviewed_list)

  return(TADAUsesAliasRef)
}

# Update TADAUsesAlias Reference Table internal file
# (for internal use only)

.TADA_UpdateTADAUsesAliasRef <- function(
  ATTAINS.CST.tolerance = 0.15,
  CST.ATTAINS.tolerance = 0.15,
  set.all.tolerance = NA
) {
  utils::write.csv(
    TADA_GetTADAUsesAliasRef(
      ATTAINS.CST.tolerance = ATTAINS.CST.tolerance,
      CST.ATTAINS.tolerance = CST.ATTAINS.tolerance,
      set.all.tolerance = set.all.tolerance
    ),
    file = "inst/extdata/TADAUsesAliasRef.csv",
    row.names = FALSE
  )
}
