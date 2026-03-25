#' Get Nutrient Summation Reference Key
#'
#' Return the installed nutrient summation reference table used by
#' TADA_CalculateTotalNP() to identify which nitrogen and phosphorus subspecies
#' are combined into total N and total P.
#'
#' The internal file can be customized by users and supplied back to
#' TADA_CalculateTotalNP() via the `sum_ref` argument.
#'
#' @details
#' - Key columns (TADA.CharacteristicName, TADA.ResultSampleFractionText,
#'   TADA.MethodSpeciationName) are normalized: leading/trailing whitespace is
#'   trimmed and both `""` and `"NONE"` are treated as `NA`.
#' - Rows are de-duplicated after normalization.
#' - The CSV is read with `na.strings = c("", "NA")` so blanks are converted to `NA`.
#'
#' Expected columns include, at minimum:
#' - TADA.CharacteristicName
#' - TADA.ResultSampleFractionText
#' - TADA.MethodSpeciationName
#' - NutrientGroup
#' - SummationName
#' - SummationRank
#' - SummationNote
#' - SummationFractionNotes
#' - SummationSpeciationNotes
#' - SummationSpeciationConversionFactor
#'
#' @return A data.frame of nutrient summation combinations with normalized keys.
#'
#' @seealso TADA_CalculateTotalNP()
#'
#' @examples
#' ref <- TADA_GetNutrientSummationRef()
#' head(ref)
#'
#' # Use a customized version:
#' # my_ref <- ref
#' # ...edit rows/targets as needed...
#' # df_out <- TADA_CalculateTotalNP(df_in, sum_ref = my_ref)
#'
#' @export
TADA_GetNutrientSummationRef <- function() {
  ref <- utils::read.csv(
    system.file("extdata", "NPsummation_key.csv", package = "EPATADA"),
    stringsAsFactors = FALSE,
    check.names = TRUE,
    comment.char = "",
    na.strings = c("", "NA")
  )

  keys <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )

  normalize_keys <- function(df, cols) {
    df |>
      dplyr::mutate(dplyr::across(
        dplyr::any_of(cols),
        ~ {
          x <- as.character(.)
          x <- trimws(x)
          x[x == ""] <- NA_character_
          x[toupper(x) == "NONE"] <- NA_character_
          x
        }
      ))
  }

  # Ensure key columns exist
  missing <- setdiff(keys, names(ref))
  if (length(missing) > 0) {
    stop(
      "NP summation ref is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }

  # Normalize and de-duplicate
  ref <- ref |> normalize_keys(keys) |> dplyr::distinct()

  ref
}

#' Get Unique Synonym Reference Table
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
#' @export
TADA_GetSynonymRef <- function(.data = NULL) {
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )

  # Helpers for normalization
  normalize_keys <- function(df, cols) {
    df |>
      dplyr::mutate(dplyr::across(
        dplyr::any_of(cols),
        ~ {
          x <- as.character(.)
          x <- trimws(x)
          x[x == ""] <- NA_character_
          x[toupper(x) == "NONE"] <- NA_character_
          x
        }
      ))
  }
  trim_to_na <- function(df, cols) {
    df |>
      dplyr::mutate(dplyr::across(
        dplyr::any_of(cols),
        ~ {
          x <- .
          if (is.character(x)) {
            x <- trimws(x)
            x[x == ""] <- NA_character_
          }
          x
        }
      ))
  }

  # NA-safe left join (requires dplyr >= 1.1.0)
  left_join_na <- function(x, y, by) {
    dplyr::left_join(x, y, by = by, na_matches = "na")
  }

  # Load and normalize the harmonization template
  harm.raw <- utils::read.csv(
    system.file("extdata", "HarmonizationTemplate.csv", package = "EPATADA"),
    stringsAsFactors = FALSE,
    check.names = TRUE,
    comment.char = "",
    na.strings = c("", "NA")
  )
  harm.raw <- normalize_keys(harm.raw, expected_cols)
  harm.raw <- trim_to_na(harm.raw, names(harm.raw))
  harm.raw <- dplyr::distinct(harm.raw)

  # If no data supplied, return the normalized internal template
  if (is.null(.data)) {
    return(harm.raw)
  }

  # Check input columns
  TADA_CheckColumns(.data, expected_cols)

  # Warnings about QC flag columns
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
    warning(
      "This dataframe is missing TADA QC flagging columns. ",
      "Run TADA_FlagResultUnit, TADA_FlagFraction, and TADA_FlagSpeciation and remove Suspect combinations before this step."
    )
  }

  flag_cols <- intersect(
    c(
      "TADA.MethodSpeciation.Flag",
      "TADA.SampleFraction.Flag",
      "TADA.ResultUnit.Flag"
    ),
    names(.data)
  )

  if (length(flag_cols) > 0) {
    # Use a local alias to avoid ambiguity with the rlang .data pronoun
    df <- .data

    check_inv <- df |>
      dplyr::select(dplyr::all_of(flag_cols)) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "Flag_Column",
        values_to = "Flag_Value"
      ) |>
      dplyr::filter(.data$Flag_Value == "Suspect")

    if (nrow(check_inv) > 0) {
      summary_inv <- check_inv |>
        dplyr::group_by(.data$Flag_Column) |>
        dplyr::summarise(`Result Count` = dplyr::n(), .groups = "drop")

      message(
        "Warning: Your dataframe contains suspect metadata combinations in the following flag columns:"
      )
      print(as.data.frame(summary_inv))
    }
  }

  # Unique combinations from the data, normalized like the template
  combos <- .data[, expected_cols, drop = FALSE]
  combos <- dplyr::distinct(combos)
  combos <- normalize_keys(combos, expected_cols)

  # NA-aware join to pull target columns
  join.data <- left_join_na(combos, harm.raw, by = expected_cols)

  # Return unique rows aligned to template columns
  unique.data <- dplyr::distinct(join.data)
  unique.data <- unique.data[, names(harm.raw), drop = FALSE]

  unique.data
}

#' USGS Unit and Speciation Conversion Table
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
#' @return Dataframe of USGS only units and speciations and their WQX compatible
#' targets/synonyms.
#'
#' @export
TADA_GetUSGSSynonymRef <- function() {
  utils::read.csv(
    system.file("extdata", "USGS_units_speciation.csv", package = "EPATADA"),
    stringsAsFactors = FALSE,
    check.names = TRUE,
    comment.char = "",
    na.strings = c("", "NA")
  )
}

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
  if (!requireNamespace("rExpertQuery", quietly = TRUE)) {
    stop(
      "Package 'rExpertQuery' is required by TADA_GetTADACharAliasRef(). Please install it."
    )
  }

  # If set.all.tolerance is populated, populate all tolerance limits with same value
  if (!is.na(set.all.tolerance)) {
    ATTAINS.CST.tolerance <- CST.ATTAINS.tolerance <- WQX.ATTAINS.tolerance <- ATTAINS.WQX.tolerance <- CST.WQX.tolerance <- WQX.CST.tolerance <- set.all.tolerance
  }

  # Validate tolerance ranges (must be between 0 and 1)
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

  # Use EPATADA unified cache with a tolerance-keyed key
  cache_key <- paste(
    "TADACharAliasRef",
    sprintf("%.6f", ATTAINS.CST.tolerance),
    sprintf("%.6f", CST.ATTAINS.tolerance),
    sprintf("%.6f", ATTAINS.WQX.tolerance),
    sprintf("%.6f", WQX.ATTAINS.tolerance),
    sprintf("%.6f", CST.WQX.tolerance),
    sprintf("%.6f", WQX.CST.tolerance),
    if (is.na(set.all.tolerance)) "NA" else sprintf("%.6f", set.all.tolerance),
    sep = "|"
  )
  cached <- .tada_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  # CSV-only: Load previously reviewed alias decisions
  csv_path <- system.file(
    "extdata",
    "TADACharAliasRef.csv",
    package = "EPATADA"
  )
  if (!nzchar(csv_path) || !file.exists(csv_path)) {
    message(
      "TADACharAliasRef.csv not found in EPATADA inst/extdata; proceeding with an empty review list."
    )
    current_TADACharAlias <- data.frame(
      CharacteristicName = character(),
      ATTAINS.ParameterName = character(),
      POLLUTANT_NAME = character(),
      STD_POLLUTANT_NAME = character(),
      WQX_CAS_NO = character(),
      CST_CAS_NO = character(),
      Status = character(),
      Status.Notes = character(),
      stringsAsFactors = FALSE
    )
  } else {
    current_TADACharAlias <- utils::read.csv(
      csv_path,
      stringsAsFactors = FALSE,
      check.names = TRUE,
      comment.char = ""
    )
    reviewed_n <- if ("Status" %in% names(current_TADACharAlias)) {
      sum(current_TADACharAlias$Status != "Needs review", na.rm = TRUE)
    } else {
      0L
    }
    message(paste(
      "The current 'TADACharAliasRef.csv' file in EPATADA inst/extdata folder contains",
      reviewed_n,
      "that have already been reviewed (see 'Status' column as approved or rejected). These rows will not be replaced and will be returned in the new output of this function's run. Any additional rows that are found as potential alias will be appended.",
      "If you would like to make edits to this alias table, open the file and modify the Status column."
    ))
  }

  # List of stop words and punctuation to exclude during word matching
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
    ";",
    ":",
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
    "(%)",
    "--"
  )

  # Retrieve WQX, ATTAINS and CST domains
  # WQX characteristic names
  raw.data <- TADA_GetCharacteristicRef()

  WQXCharacteristicRef <- raw.data |>
    dplyr::select(dplyr::any_of(c(
      "CharacteristicName",
      "Char_Flag",
      "Comparable.Name",
      "CAS.Number"
    ))) |>
    dplyr::mutate(dplyr::across(where(is.character), toupper)) |>
    dplyr::distinct()

  # Remove dashes in CAS number to match CST CAS number
  if ("CAS.Number" %in% names(WQXCharacteristicRef)) {
    WQXCharacteristicRef$CAS.Number <- gsub(
      "-",
      "",
      WQXCharacteristicRef$CAS.Number
    )
  }

  # Extract all words from each WQX characteristic name
  WQXCharacteristicRef2 <- WQXCharacteristicRef |>
    dplyr::mutate(
      name_words = stringr::str_split(CharacteristicName, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
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

  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("param_name")
  ATTAINSParamRef <- ATTAINS.raw[, "name", drop = FALSE]

  # Extract all words from each ATTAINS Parameter Name
  ATTAINSParamRef2 <- ATTAINSParamRef |>
    dplyr::mutate(name_words = stringr::str_split(name, pattern = " ")) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
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

  # CST Criteria
  CST.raw <- TADA_CST_GetCriteria()

  CST <- CST.raw |>
    dplyr::select(POLLUTANT_NAME, STD_POLLUTANT_NAME, CAS_NO) |>
    dplyr::mutate(dplyr::across(where(is.character), toupper)) |>
    dplyr::distinct() |>
    dplyr::mutate(CAS_NO = as.character(CAS_NO))

  # Extract all words from each CST Pollutant Name
  CST2 <- CST |>
    dplyr::mutate(
      name_words = stringr::str_split(POLLUTANT_NAME, pattern = " ")
    ) |>
    tidyr::unnest(cols = c(name_words)) |>
    dplyr::filter(!name_words %in% toupper(stop_words)) |>
    dplyr::mutate(
      name_words = dplyr::if_else(
        name_words == "KJEHLDAL",
        "KJELDAHL",
        name_words
      )
    ) |>
    dplyr::distinct(POLLUTANT_NAME, name_words, .keep_all = TRUE)

  CST2$name_words <- toupper(gsub("[^[:alnum:] ]", "", CST2$name_words))

  # Remove intermediate variables
  rm(raw.data, ATTAINS.raw, CST.raw)

  # Step 1: ATTAINS and WQX percent word matches
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
        percent_match_WQX_ATTAINS + percent_match_ATTAINS_WQX > 1
    )

  # Step 2: CST and ATTAINS percent word matches
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
    dplyr::right_join(
      CST,
      by = "POLLUTANT_NAME",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      (percent_match_CST_ATTAINS >= CST.ATTAINS.tolerance |
        percent_match_ATTAINS_CST >= ATTAINS.CST.tolerance) &
        percent_match_CST_ATTAINS + percent_match_ATTAINS_CST > 1
    )

  # Step 3: CST and WQX percent word matches
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
    dplyr::slice_max(
      order_by = percent_match_WQX_CST + percent_match_CST_WQX
    ) |>
    dplyr::right_join(
      CST,
      by = "POLLUTANT_NAME",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      (percent_match_WQX_CST >= WQX.CST.tolerance |
        percent_match_CST_WQX >= CST.WQX.tolerance) &
        percent_match_WQX_CST + percent_match_CST_WQX > 1
    )

  # Remove intermediate variables
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
      dplyr::select(
        WQXCharacteristicRef,
        dplyr::any_of(c("CharacteristicName", "CAS.Number"))
      ),
      by = "CharacteristicName"
    )

  # Find additional ATTAINS_WQX from TADA alias match
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

  # Find additional CST_WQX crosswalk from TADA alias match and bind with WQX
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
    dplyr::full_join(
      CST_WQX_Final,
      by = "CharacteristicName",
      relationship = "many-to-many"
    ) |>
    dplyr::full_join(
      dplyr::select(
        TADARef_ATTAINS_CST,
        ATTAINS.ParameterName = name,
        POLLUTANT_NAME,
        STD_POLLUTANT_NAME
      ),
      by = c("ATTAINS.ParameterName"),
      relationship = "many-to-many"
    ) |>
    tidyr::pivot_longer(
      cols = tidyr::matches("\\.x$|\\.y$"),
      names_to = c(".value", "source"),
      names_sep = "\\.",
      values_drop_na = FALSE
    ) |>
    dplyr::group_by(CharacteristicName, ATTAINS.ParameterName) |>
    tidyr::fill(POLLUTANT_NAME, .direction = "downup") |>
    tidyr::fill(STD_POLLUTANT_NAME, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::full_join(
      ATTAINSParamRef,
      by = c("ATTAINS.ParameterName" = "name")
    ) |>
    dplyr::left_join(
      dplyr::select(
        WQXCharacteristicRef,
        dplyr::any_of(c("CharacteristicName", "CAS.Number"))
      ),
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

  # Remove intermediate variables
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

  # Carry forward reviewed rows in current TADACharAliasRef
  TADA_reviewed_list <- current_TADACharAlias |>
    dplyr::filter(Status != "Needs review") |>
    dplyr::mutate(
      WQX_CAS_NO = as.character(WQX_CAS_NO),
      CST_CAS_NO = as.character(CST_CAS_NO)
    )

  # Build final table: keep reviewed rows, append new candidates
  TADACharAliasRef <- ATTAINS_WQX_CST_Final |>
    dplyr::mutate(
      WQX_CAS_NO = as.character(WQX_CAS_NO),
      CST_CAS_NO = as.character(CST_CAS_NO)
    ) |>
    dplyr::anti_join(
      dplyr::mutate(
        TADA_reviewed_list,
        WQX_CAS_NO = as.character(WQX_CAS_NO),
        CST_CAS_NO = as.character(CST_CAS_NO)
      ),
      by = c(
        "CharacteristicName",
        "ATTAINS.ParameterName",
        "POLLUTANT_NAME",
        "STD_POLLUTANT_NAME",
        "WQX_CAS_NO",
        "CST_CAS_NO"
      )
    ) |>
    dplyr::bind_rows(TADA_reviewed_list)

  # Save updated table in session cache
  .tada_cache_set(cache_key, TADACharAliasRef)

  # Return final table
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
  # Optional: set a single tolerance for both directions
  if (!is.na(set.all.tolerance)) {
    ATTAINS.CST.tolerance <- CST.ATTAINS.tolerance <- set.all.tolerance
  }

  # Validate ranges
  if (ATTAINS.CST.tolerance > 1.00 || CST.ATTAINS.tolerance > 1.00) {
    stop(
      "One or more tolerance defined is greater than 1.00. Tolerance cannot exceed 100%."
    )
  }
  if (ATTAINS.CST.tolerance < 0.00 || CST.ATTAINS.tolerance < 0.00) {
    stop(
      "One or more tolerance defined is less than 0.00. Tolerance cannot be less than 0%."
    )
  }

  # Ensure rExpertQuery is available
  if (!requireNamespace("rExpertQuery", quietly = TRUE)) {
    stop(
      "Package 'rExpertQuery' is required by TADA_GetTADAUsesAliasRef(). Please install it."
    )
  }

  # Package-wide cache keyed by tolerances
  cache_key <- paste(
    "TADAUsesAliasRef",
    sprintf("%.6f", ATTAINS.CST.tolerance),
    sprintf("%.6f", CST.ATTAINS.tolerance),
    if (is.na(set.all.tolerance)) "NA" else sprintf("%.6f", set.all.tolerance),
    sep = "|"
  )
  cached <- .tada_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  # CSV-only: load previously reviewed uses alias decisions
  uses_csv <- system.file(
    "extdata",
    "TADAUsesAliasRef.csv",
    package = "EPATADA"
  )
  if (!nzchar(uses_csv) || !file.exists(uses_csv)) {
    message(
      "TADAUsesAliasRef.csv not found in EPATADA inst/extdata; proceeding with an empty review list."
    )
    current_TADAUsesAlias <- data.frame(
      ENTITY_ABBR = character(),
      ENTITY_NAME = character(),
      CRITERIATYPEAQUAHUMHLTH = character(),
      CRITERIATYPEFRESHSALTWATER = character(),
      CRITERIATYPE_ACUTECHRONIC = character(),
      USE_CLASS_NAME_LOCATION_ETC = character(),
      ATTAINS.OrganizationIdentifier = character(),
      context2 = character(),
      ATTAINS.UseName = character(),
      review = character(),
      Last.Change.Date = character(),
      stringsAsFactors = FALSE
    )
  } else {
    current_TADAUsesAlias <- utils::read.csv(
      uses_csv,
      stringsAsFactors = FALSE,
      check.names = TRUE,
      comment.char = ""
    )
    reviewed_n <- if ("review" %in% names(current_TADAUsesAlias)) {
      sum(
        current_TADAUsesAlias$review %in% c("APPROVED", "REJECTED"),
        na.rm = TRUE
      )
    } else {
      0L
    }
    message(paste(
      "The current 'TADAUsesAliasRef.csv' file in EPATADA inst/extdata contains",
      reviewed_n,
      "rows already reviewed (review == APPROVED/REJECTED).",
      "Reviewed rows will be kept; newly discovered potential aliases will be appended."
    ))
  }

  # Map ATTAINS use context2 to Human Health vs Aquatic Life (CST column)
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
    ),
    stringsAsFactors = FALSE
  )

  # Stop words and punctuation to exclude in token matching
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
    ";",
    ":",
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
    "(%)",
    "--"
  )

  # ATTAINS use_name domain
  ATTAINS.raw <- rExpertQuery::EQ_DomainValues("use_name") |>
    dplyr::select(name, context, context2)

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

  # Tokenize ATTAINS use names
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

  rm(ATTAINS.raw)

  # CST Criteria (download with fallback to package workbook handled inside helper)
  CST.raw <- tryCatch(
    TADA_CST_GetCriteria(download_only = FALSE),
    error = function(e) {
      stop(
        "Unable to retrieve CST Criteria. Ensure internet access or that the package ships inst/extdata/cst-workbook.xlsx. ",
        "Underlying error: ",
        conditionMessage(e)
      )
    }
  )

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

  # Tokenize CST uses
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

  # Match CST entity to ATTAINS org (best guess using state/tribe name)
  ATTAINSOrgIDsRef <- TADA_GetATTAINSOrgIDsRef()
  ATTAINSOrgIDsRef$name <- toupper(ATTAINSOrgIDsRef$name)
  ATTAINS_CST.org <- data.frame(unique(CST[, c(
    "ENTITY_NAME",
    "ENTITY_ABBR"
  )])) |>
    dplyr::mutate(ENTITY_NAME = toupper(ENTITY_NAME)) |>
    dplyr::left_join(ATTAINSOrgIDsRef, by = c("ENTITY_NAME" = "name")) |>
    dplyr::rename(ATTAINS.OrganizationIdentifier = code) |>
    dplyr::select(ENTITY_ABBR, ATTAINS.OrganizationIdentifier)

  # Join ATTAINS org ID into CST tables
  CST2 <- CST2 |>
    dplyr::mutate(ENTITY_NAME = toupper(ENTITY_NAME)) |>
    dplyr::left_join(ATTAINS_CST.org, by = "ENTITY_ABBR")

  CST <- CST |>
    dplyr::mutate(ENTITY_NAME = toupper(ENTITY_NAME)) |>
    dplyr::left_join(ATTAINS_CST.org, by = "ENTITY_ABBR")

  # Join by org and the Aquatic/Human-Health indicator
  ATTAINS_CST <- dplyr::full_join(
    CST,
    ATTAINSUseRef,
    by = c("ATTAINS.OrganizationIdentifier", "CRITERIATYPEAQUAHUMHLTH"),
    relationship = "many-to-many"
  )

  # Word-match between ATTAINS and CST (same org)
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
    dplyr::right_join(
      CST,
      by = c("ATTAINS.OrganizationIdentifier", "USE_CLASS_NAME_LOCATION_ETC"),
      relationship = "many-to-many"
    )

  # Combine strict type match and word-match tables
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

  rm(
    CST,
    CST.raw,
    ATTAINSUseRef,
    CST2,
    ATTAINSUseRef2,
    ATTAINS_CST,
    ATTAINS_CST2
  )

  # Apply tolerance filters
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
    dplyr::select(-n, -name) |>
    dplyr::select(
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
      Last.Change.Date,
      dplyr::everything()
    )

  # Drop percentage columns before carry-forward
  TADAUsesAliasRef <- TADAUsesAliasRef |>
    dplyr::select(
      -dplyr::any_of(c("percent_match_ATTAINS_CST", "percent_match_CST"))
    )

  # Carry forward reviewed rows from current CSV (APPROVED/REJECTED only)
  current_TADAUsesAlias_keep <- current_TADAUsesAlias |>
    dplyr::filter(review %in% c("APPROVED", "REJECTED"))

  # Keep rows from new table that are not identical to reviewed rows on core keys
  TADA_reviewed_list <- current_TADAUsesAlias_keep |>
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

  # Bind reviewed rows back so they are retained
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

  # Cache and return
  .tada_cache_set(cache_key, TADAUsesAliasRef)
  TADAUsesAliasRef
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
