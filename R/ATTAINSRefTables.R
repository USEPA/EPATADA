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
      WQX.ATTAINS.tolerance > 1.00
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
      WQX.ATTAINS.tolerance < 0.00
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

  # retrieve WQXCharAliasRef table
  file_path <- system.file(
    "extdata",
    "WQXCharAliasRef.rda",
    package = "EPATADA"
  )
  load(file_path)

  # remove intermediate variable
  rm(file_path)

  # WQX to ATTAINS
  WQX_char_alias_filtered1 <- WQXCharAliasRef |>
    dplyr::filter(Alias.Type.Name %in% c("ATTAINS.PARAMETER")) |>
    dplyr::select(
      CharacteristicName = Characteristic.Name,
      ATTAINS.ParameterName = Alias.Name,
      Last.Change.Date
    )
  # WQX to CST.Pollutant
  WQX_char_alias_filtered2 <- WQXCharAliasRef |>
    dplyr::filter(Alias.Type.Name %in% c("CST.POLLUTANT")) |>
    dplyr::select(
      CharacteristicName = Characteristic.Name,
      POLLUTANT_NAME = Alias.Name,
      Last.Change.Date
    )
  # WQX to CST Std. pollutant
  WQX_char_alias_filtered3 <- WQXCharAliasRef |>
    dplyr::filter(Alias.Type.Name %in% c("CST.STD.POLLUTANT")) |>
    dplyr::select(
      CharacteristicName = Characteristic.Name,
      STD_POLLUTANT_NAME = Alias.Name,
      Last.Change.Date
    )
  # WQX-ATTAINS-CST initial parameter crosswalk
  WQX_char_alias_filtered <- WQX_char_alias_filtered1 |>
    dplyr::full_join(
      WQX_char_alias_filtered2,
      by = c("CharacteristicName", "Last.Change.Date"),
      relationship = "many-to-many"
    ) |>
    dplyr::full_join(
      WQX_char_alias_filtered3,
      by = c("CharacteristicName", "Last.Change.Date"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(dplyr::across(where(is.character), toupper))

  # for word matching method, create a list of common stop words and punctuation marks to exclude
  # assisted generation of stop words from EPA GenAI Tool
  stop_words <- c(
    "a",
    "about",
    "above",
    "after",
    "again",
    "against",
    "all",
    "am",
    "an",
    "and",
    "any",
    "are",
    "aren",
    "aren't",
    "as",
    "at",
    "be",
    "because",
    "been",
    "before",
    "being",
    "below",
    "between",
    "both",
    "but",
    "by",
    "can",
    "could",
    "couldn",
    "couldn't",
    "did",
    "didn",
    "didn't",
    "do",
    "does",
    "doesn",
    "doesn't",
    "doing",
    "don",
    "don't",
    "down",
    "during",
    "each",
    "few",
    "for",
    "from",
    "further",
    "had",
    "hadn",
    "hadn't",
    "has",
    "hasn",
    "hasn't",
    "have",
    "haven",
    "haven't",
    "having",
    "he",
    "her",
    "here",
    "hers",
    "herself",
    "him",
    "himself",
    "his",
    "how",
    "i",
    "if",
    "in",
    "into",
    "is",
    "isn",
    "isn't",
    "it",
    "its",
    "itself",
    "just",
    "ll",
    "m",
    "ma",
    "me",
    "more",
    "most",
    "mustn",
    "mustn't",
    "my",
    "myself",
    "needn",
    "needn't",
    "no",
    "nor",
    "not",
    "now",
    "o",
    "of",
    "off",
    "on",
    "once",
    "only",
    "or",
    "other",
    "our",
    "ours",
    "ourselves",
    "out",
    "over",
    "own",
    "re",
    "s",
    "same",
    "shan",
    "shan't",
    "she",
    "should",
    "should've",
    "shouldn",
    "shouldn't",
    "so",
    "some",
    "such",
    "t",
    "than",
    "that",
    "the",
    "their",
    "theirs",
    "them",
    "themselves",
    "then",
    "there",
    "these",
    "they",
    "this",
    "those",
    "through",
    "to",
    "too",
    "under",
    "until",
    "up",
    "very",
    "was",
    "wasn",
    "wasn't",
    "we",
    "were",
    "weren",
    "weren't",
    "what",
    "when",
    "where",
    "which",
    "while",
    "who",
    "whom",
    "why",
    "will",
    "with",
    "won",
    "won't",
    "wouldn",
    "wouldn't",
    "y",
    "you",
    "your",
    "yours",
    "yourself",
    "yourselves",
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
    "’",
    "‘",
    "“",
    "”",
    "—",
    "–",
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
    dplyr::distinct(CharacteristicName, name_words, .keep_all = TRUE)

  WQXCharacteristicRef2$name_words <- toupper(gsub(
    "[^[:alnum:] ]",
    "",
    WQXCharacteristicRef2$name_words
  ))

  # remove intermediate variable
  rm(
    WQX_char_alias_filtered1,
    WQX_char_alias_filtered2,
    WQX_char_alias_filtered3,
    raw.data
  )

  # retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("param_name")

  ATTAINSParamRef <- ATTAINS.raw[, "name", drop = FALSE]

  # extracts all words from each ATTAINS Parameter Name
  ATTAINSParamRef2 <- ATTAINSParamRef |>
    dplyr::mutate(name_words = stringr::str_split(name, pattern = " ")) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
    dplyr::distinct(name, name_words, .keep_all = TRUE)

  ATTAINSParamRef2$name_words <- toupper(gsub(
    "[^[:alnum:] ]",
    "",
    ATTAINSParamRef2$name_words
  ))

  # remove intermediate variables
  rm(ATTAINS.raw)

  # retrieve the Criteria Search Tool
  CriteriaSearchToolRef <- system.file(
    "extdata",
    "CriteriaSearchToolRef.rda",
    package = "EPATADA"
  )
  load(CriteriaSearchToolRef)

  # extract unique relevant columns
  CST <- CriteriaSearchToolRef |>
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
    dplyr::distinct(POLLUTANT_NAME, name_words, .keep_all = TRUE)

  CST2$name_words <- toupper(gsub("[^[:alnum:] ]", "", CST2$name_words))

  # Step 1:
  # Look for percent word matches between ATTAINS and WQX
  # inner join is being used to show matches that were found as an alias.
  ATTAINS_WQX <- dplyr::inner_join(
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
      percent_match_WQX_ATTAINS >= WQX.ATTAINS.tolerance |
        percent_match_ATTAINS_WQX >= ATTAINS.WQX.tolerance
    )

  # step 2: CST and ATTAINS
  # Look for percent word matches between ATTAINS and CST
  # inner join is being used to show matches that were found as an alias.
  ATTAINS_CST <- dplyr::inner_join(
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
    # If CST to ATTAINS match must be strict, choose best match only using slice_max
    # dplyr::slice_max(
    #   order_by = percent_match_CST + percent_match_ATTAINS_CST
    # ) |>
    dplyr::right_join(
      CST,
      by = "POLLUTANT_NAME",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      percent_match_CST_ATTAINS >= CST.ATTAINS.tolerance |
        percent_match_ATTAINS_CST >= ATTAINS.CST.tolerance
    )

  # step 3: CST and WQX
  # Look for percent word matches between CST and WQX
  # inner join is being used to show matches that were found as an alias.
  CST_WQX <- dplyr::inner_join(
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
    # dplyr::slice_max(
    #   order_by = percent_match_WQX_CST + percent_match_CST_WQX
    # ) |>
    dplyr::right_join(
      CST,
      by = "POLLUTANT_NAME",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      percent_match_WQX_CST >= WQX.CST.tolerance |
        percent_match_CST_WQX >= CST.WQX.tolerance
    )

  # remove intermediate variables
  rm(
    CST,
    CST2,
    ATTAINSParamRef,
    ATTAINSParamRef2,
    WQXCharacteristicRef,
    WQXCharacteristicRef2
  )

  ## step 4: join pairwise combination tables into one.
  final <- ATTAINS_CST |>
    dplyr::full_join(ATTAINS_WQX, by = "name", relationship = "many-to-many") |>
    dplyr::full_join(
      CST_WQX,
      by = c(
        "POLLUTANT_NAME",
        "STD_POLLUTANT_NAME",
        "CAS_NO",
        "CharacteristicName"
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::select(
      CharacteristicName,
      POLLUTANT_NAME,
      STD_POLLUTANT_NAME,
      ATTAINS.ParameterName = name,
      CST_CAS_NO = CAS_NO,
      WQX_CAS_NO = CAS.Number
    )

  # These are highly likely matches as they contain CAS No exact matches
  final_CAS_match <- final |>
    dplyr::filter(WQX_CAS_NO == CST_CAS_NO) |>
    dplyr::mutate(
      review = "APPROVED",
      source = "TADA.AliasMatch",
      Last.Change.Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )

  # drop unlikely matches (CAS do not match if both CAS is populated)
  final_drop <- final |>
    dplyr::filter(WQX_CAS_NO != CST_CAS_NO & WQX_CAS_NO != "")

  # create final TADA_char_alias table with matching CAS labeled as APPROVED
  final_filtered <- final |>
    # drop unlikely matches here
    dplyr::anti_join(
      final_drop,
      by = dplyr::join_by(
        CharacteristicName,
        POLLUTANT_NAME,
        STD_POLLUTANT_NAME,
        ATTAINS.ParameterName,
        CST_CAS_NO,
        WQX_CAS_NO
      )
    ) |>
    # drop those that have been approved and bind them back with correct label
    dplyr::anti_join(
      final_CAS_match,
      by = dplyr::join_by(
        CharacteristicName,
        POLLUTANT_NAME,
        STD_POLLUTANT_NAME,
        ATTAINS.ParameterName,
        CST_CAS_NO,
        WQX_CAS_NO
      )
    ) |>
    # label those that still needs review
    dplyr::mutate(
      review = "New Row: Needs Review",
      source = "TADA.AliasMatch",
      Last.Change.Date = NA
    ) |>
    # join back CAS matches with the approved label
    dplyr::bind_rows(final_CAS_match)

  # remove intermediate variables
  rm(ATTAINS_CST, ATTAINS_WQX, CST_WQX)

  # Now populate the WQXCharAliasTable that was pulled in from the beginning with CAS number, source, and review labels
  WQX_char_alias_filtered <- WQX_char_alias_filtered |>
    dplyr::left_join(WQXCharacteristicRef, by = "CharacteristicName") |>
    dplyr::left_join(CST, by = c("POLLUTANT_NAME", "STD_POLLUTANT_NAME")) |>
    dplyr::mutate(review = "APPROVED", source = "WQX.AliasMatch") |>
    dplyr::select(
      CharacteristicName,
      POLLUTANT_NAME,
      STD_POLLUTANT_NAME,
      ATTAINS.ParameterName,
      WQX_CAS_NO = CAS.Number,
      CST_CAS_NO = CAS_NO,
      source,
      review,
      Last.Change.Date
    )

  # now, get the most up to date TADACharAlias Ref table
  TADACharAliasRef <- final_filtered |>
    # find all new TADA alias match to add to the final char alias table
    dplyr::anti_join(
      WQX_char_alias_filtered,
      by = dplyr::join_by(
        CharacteristicName,
        POLLUTANT_NAME,
        STD_POLLUTANT_NAME,
        ATTAINS.ParameterName
      )
    ) |>
    # bind existing WQX char alias.
    dplyr::bind_rows(WQX_char_alias_filtered)

  # pull in most recent TADACharAliasRef in EPATADA
  current_TADACharAlias <- utils::read.csv(system.file(
    "extdata",
    "TADACharAliasRef.csv",
    package = "EPATADA"
  ))

  # step5: identify any current TADA.AliasMatch that have been reviewed in TADA inst/extdata already - but may not have been submitted/updated in WQX Char ref domain
  current_TADACharAlias <- current_TADACharAlias |>
    # dplyr::filter( review != "New Row: Needs Review") # Note: we can probably filter it by this line instead.
    dplyr::filter(
      review == "APPROVED" &
        source == "TADA.AliasMatch" |
        review == "REJECTED" & source == "TADA.AliasMatch"
    )

  # keep rows that exist in current TADACharRef that do not have a match with the new ref
  TADA_reviewed_list <- current_TADACharAlias |>
    dplyr::mutate(
      WQX_CAS_NO = as.character(WQX_CAS_NO),
      CST_CAS_NO = as.character(CST_CAS_NO)
    ) |>
    dplyr::anti_join(
      TADACharAliasRef,
      by = dplyr::join_by(
        CharacteristicName,
        ATTAINS.ParameterName,
        POLLUTANT_NAME,
        STD_POLLUTANT_NAME,
        WQX_CAS_NO,
        CST_CAS_NO,
        source,
        review,
        Last.Change.Date
      ),
      na_matches = "na"
    )

  # return rows from current TADACharRef in the TADA internal folder
  TADACharAliasRef <- TADACharAliasRef |>
    dplyr::filter(
      !(ATTAINS.ParameterName %in%
        TADA_reviewed_list$ATTAINS.ParameterName &
        CharacteristicName %in% TADA_reviewed_list$CharacteristicName &
        POLLUTANT_NAME %in% TADA_reviewed_list$POLLUTANT_NAME &
        STD_POLLUTANT_NAME %in% TADA_reviewed_list$STD_POLLUTANT_NAME)
    ) |>
    dplyr::bind_rows(dplyr::mutate(TADA_reviewed_list))
  # lastly, fill in any missing WQX Char that can be populated if an ATTAINS to CST exist and has a ATTAINS to WQX that is already defined.
  # Note: In theory, only new ATTAINS.ParameterName entries in this scenario should be the only potential of having NA remaining in this final table.
  #       as all unique ATTAINS.ParameterName were crosswalked to a WQX char at some point. Verify?
  TADACharAliasRef <- TADACharAliasRef |>
    dplyr::select(CharacteristicName, ATTAINS.ParameterName, source) |>
    dplyr::filter(source %in% "WQX.CharAlias") |>
    dplyr::full_join(
      TADACharAliasRef,
      by = c("ATTAINS.ParameterName"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      CharacteristicName = dplyr::if_else(
        is.na(CharacteristicName.y),
        CharacteristicName.x,
        CharacteristicName.y
      )
    ) |>
    dplyr::select(
      CharacteristicName,
      ATTAINS.ParameterName,
      POLLUTANT_NAME,
      STD_POLLUTANT_NAME,
      source = source.y,
      WQX_CAS_NO,
      CST_CAS_NO,
      review,
      Last.Change.Date
    ) |>
    dplyr::distinct()

  # remove intermediate variables
  rm(
    TADA_reviewed_list,
    current_TADACharAlias,
    final,
    final_CAS_match,
    final_drop,
    final_filtered
  )

  # Save updated table in cache
  TADACharAliasRef_Cached <- TADACharAliasRef

  # returns final table
  TADACharAliasRef
}

# Update TADACharAlias Reference Table internal file
# (for internal use only)

TADA_UpdateTADACharAliasRef <- function(
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
      ATTAINS.CST.tolerance = 1.00,
      CST.ATTAINS.tolerance = 1.00,
      ATTAINS.WQX.tolerance = 1.00,
      WQX.ATTAINS.tolerance = 1.00,
      CST.WQX.tolerance = 1.00,
      WQX.CST.tolerance = 1.00,
      set.all.tolerance = NA
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
#' @examples
#' uses <- TADA_UsesAliasForReview(
#'   ATTAINS.CST.tolerance = 0.50,
#'   CST.ATTAINS.tolerance = 0.15 # uses a lower value as CST uses can be very long string.
#' )
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
  # assisted generation of stop words from EPA GenAI Tool
  stop_words <- c(
    "a",
    "about",
    "above",
    "after",
    "again",
    "against",
    "all",
    "am",
    "an",
    "and",
    "any",
    "are",
    "aren",
    "aren't",
    "as",
    "at",
    "be",
    "because",
    "been",
    "before",
    "being",
    "below",
    "between",
    "both",
    "but",
    "by",
    "can",
    "could",
    "couldn",
    "couldn't",
    "did",
    "didn",
    "didn't",
    "do",
    "does",
    "doesn",
    "doesn't",
    "doing",
    "don",
    "don't",
    "down",
    "during",
    "each",
    "few",
    "for",
    "from",
    "further",
    "had",
    "hadn",
    "hadn't",
    "has",
    "hasn",
    "hasn't",
    "have",
    "haven",
    "haven't",
    "having",
    "he",
    "her",
    "here",
    "hers",
    "herself",
    "him",
    "himself",
    "his",
    "how",
    "i",
    "if",
    "in",
    "into",
    "is",
    "isn",
    "isn't",
    "it",
    "its",
    "itself",
    "just",
    "ll",
    "m",
    "ma",
    "me",
    "more",
    "most",
    "mustn",
    "mustn't",
    "my",
    "myself",
    "needn",
    "needn't",
    "no",
    "nor",
    "not",
    "now",
    "o",
    "of",
    "off",
    "on",
    "once",
    "only",
    "or",
    "other",
    "our",
    "ours",
    "ourselves",
    "out",
    "over",
    "own",
    "re",
    "s",
    "same",
    "shan",
    "shan't",
    "she",
    "should",
    "should've",
    "shouldn",
    "shouldn't",
    "so",
    "some",
    "such",
    "t",
    "than",
    "that",
    "the",
    "their",
    "theirs",
    "them",
    "themselves",
    "then",
    "there",
    "these",
    "they",
    "this",
    "those",
    "through",
    "to",
    "too",
    "under",
    "until",
    "up",
    "very",
    "was",
    "wasn",
    "wasn't",
    "we",
    "were",
    "weren",
    "weren't",
    "what",
    "when",
    "where",
    "which",
    "while",
    "who",
    "whom",
    "why",
    "will",
    "with",
    "won",
    "won't",
    "wouldn",
    "wouldn't",
    "y",
    "you",
    "your",
    "yours",
    "yourself",
    "yourselves",
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
    "’",
    "‘",
    "“",
    "”",
    "—",
    "–",
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

  # pulls in CST and extract relevant columns
  file_path <- system.file(
    "extdata",
    "CriteriaSearchToolRef.rda",
    package = "EPATADA"
  )
  load(file_path)

  CST <- CriteriaSearchToolRef

  # remove intermediate variables
  rm(file_path)

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
  ATTAINS_CST.org <- data.frame(unique(CriteriaSearchToolRef[, c(
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
  rm(CST, ATTAINSUseRef, CST2, ATTAINSUseRef2, ATTAINS_CST, ATTAINS_CST2)

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

TADA_UpdateTADAUsesAliasRef <- function(
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

  # considers only the latest cycle from each org, you could skip this step
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
