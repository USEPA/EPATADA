#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



# write global variables. Gets rid of global variable NOTE in check:
utils::globalVariables(c(
  "TADA.ResultValueAboveUpperThreshold.Flag", "ActivityIdentifier", "ActivityMediaName",
  "ActivityStartDate", "TADA.ResultValueBelowUpperThreshold.Flag", "TADA.ResultValueBelowLowerThreshold.Flag", "CharacteristicName",
  "Conversion.Factor", "Count", "Description", "FieldName", "FieldValue",
  "MethodSpecationName", "MonitoringLocationIdentifier",
  "OrganizationFormalName", "OrganizationIdentifier", "ProjectDescriptionText",
  "ProjectFileUrl", "ProjectIdentifier",
  "ProjectMonitoringLocationWeightingUrl", "ProjectName",
  "QAPPApprovalAgencyName", "QAPPApprovedIndicator",
  "ResultDetectionConditionText", "ResultMeasureValue",
  "SamplingDesignTypeCode", "Source", "Status", "TADA.AggregatedContinuousData.Flag",
  "TADA.InvalidCoordinates.Flag", "TADA.PotentialDupRowIDs.Flag", "TADA.QAPPDocAvailable",
  "Target.Unit", "Type", "Value.Unit", "TADA.AnalyticalMethod.Flag",
  "TADA.MethodSpeciation.Flag", "TADA.ResultUnit.Flag",
  "TADA.SampleFraction.Flag", "YearSummarized", "where", "TADA.CharacteristicName",
  "ResultIdentifier", "TADA.ResultMeasureValue", "n_sites",
  "n_records", "statecodes_df", "STUSAB", "ActivityStartTime.Time", "numorgs", "dup_id",
  "LatitudeMeasure", "TADA.ResultMeasureValueDataTypes.Flag", "Name", "TADA.Detection_Type",
  "DetectionQuantitationLimitTypeName", "TADA.Limit_Type", "multiplier", "summ", "cf",
  "LongitudeMeasure", "TADA.CensoredData.Flag", "Censored_Count", "TADA.ResultMeasureValueDataTypes.Flag",
  "Status2", "ActivityTypeCode", "SampleCollectionEquipmentName",
  "ResultTimeBasisText", "StatisticalBaseCode", "ResultValueTypeName",
  "masked", "TADA.env", "Legend", "Fields", "desc", "WQXActivityType_Cached",
  "TADA.ActivityType.Flag", "Code", "ActivityTypeCode", "ResultCount",
  "tot_n", "MonitoringLocationName", "TADA.LatitudeMeasure",
  "TADA.LongitudeMeasure", "median", "sd", "TADA.ComparableDataIdentifier",
  "desc", "Legend", "roundRV", "TADA.DuplicateID", "maxRV", "within10",
  "AllGroups", "Domain.Value.Status", "Char_Flag", "Comparable.Name",
  "TADA.ResultMeasureValue1", "TADA.ResultSampleFractionText",
  "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode",
  "TADA.ActivityMediaName", "TADA.NutrientSummationGroup",
  "SummationName", "SummationRank", "SummationFractionNotes", "SummationSpeciationNotes",
  "SummationSpeciationConversionFactor", "SummationNote", "NutrientGroup",
  "Target.Speciation", "TADA.NearbySiteGroups", "numres", "TADA.SingleOrgDupGroupID",
  "TADA.MeasureQualifierCode.Flag", "TADA.MeasureQualifierCode.Def", "MeasureQualifierCode", "value", "Flag_Column",
  "Data_NCTCShepherdstown_HUC12", "ActivityStartDateTime", "TADA.MultipleOrgDupGroupID",
  "TADA.WQXVal.Flag", "Concat", ".", "MeasureQualifierCode.Split", "TADA.Media.Flag",
  "TADA.UseForAnalysis.Flag", "ML.Media.Flag", "TADA.DepthCategory.Flag", "ARD_Category",
  "ActivityRelativeDepthName", "TADA.ActivityDepthHeightMeasure.MeasureValue", 
  "TADA.ResultDepthHeightMeasure.MeasureValue", "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
  "TADA.ActivityDepthHeightMeasure.MeasureUnitCode", "TADA.ConsolidatedDepth",
  "TADA.ConsolidatedDepth.Unit", "DepthsPerGroup", "TADA.ActivityBottomDepthHeightMeasure.MeasureValue",
  "TADA.ConsolidatedDepth.Bottom", "DepthsByGroup", "TADA.DepthProfileAggregation.Flag",
  "N", "SecchiConversion", "YAxis.DepthUnit", "Unique.Identifier", "Domain",
  "Note.Recommendation", "Conversion.Coefficient", "Last.Change.Date", "Value", "Minimum",
  "Maximum", "TADA.NResults", "MeanResults", "TADA.CharacteristicsForDepthProfile",
  "MonitoringLocationTypeName"
))

# global variables for tribal feature layers used in TADA_OverviewMap in Utilities.R
AKAllotmentsUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0/query"
AKVillagesUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1/query"
AmericanIndianUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query"
OffReservationUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3/query"
OKTribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query"
VATribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5/query"

#' TADA_AutoClean
#'
#' Removes rows of data that are exact duplicates. Creates new columns with
#' prefix "TADA." and capitalizes fields to harmonize data. This function runs
#' "TADA_ConvertSpecialChars", "TADA_ConvertResultUnits",
#' "TADA_ConvertDepthUnits", and "TADA_IDCensoredData" functions, which perform
#' the following QA steps: remove exact duplicates, convert result values to
#' numeric, harmonize result and depth units (note: all depth-related columns
#' with populated values are converted to meters in a TADA-specific column),
#' convert text to uppercase letters, substitute outdated (deprecated) characteristic names
#' with updated names in TADA.CharacteristicName, and categorize detection limit
#' data. Original affected columns are not changed: new columns are added to the
#' end of the dataframe with the prefix "TADA." This function makes important
#' character fields uppercase so that they're interoperable with the WQX
#' validation reference tables and reduces issues with case-sensitivity when
#' joining data.TADA_AutoClean can be run as a stand alone function but is
#' primarily used by the TADA_dataRetrieval function.
#'
#' @param .data TADA dataframe
#'
#' @return Input dataframe with several added TADA-specific columns, including:
#'   TADA.ActivityMediaName, TADA.CharacteristicName, TADA.ResultMeasureValue,
#'   TADA.ResultMeasure.MeasureUnitCode, TADA.ResultMeasureValueDataTypes.Flag,
#'   TADA.LatitudeMeasure, TADA.LongitudeMeasure,
#'   TADA.ResultSampleFractionText, TADA.MethodSpeciationName, and more.
#'   Please note that the number of TADA-specific depth columns in the returned
#'   dataframe depends upon the number of depth columns with one or more results
#'   populated with a numeric value. If all depth columns contain only NA's, no
#'   conversion is necessary and no TADA depth columns are created.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Find web service URLs for each Profile using WQP User Interface (https://www.waterqualitydata.us/)
#' # Example WQP URL: https://www.waterqualitydata.us/#statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET
#'
#' # Use TADA_ReadWQPWebServices to load the Station, Project, and Phys-Chem Result profiles
#' stationProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' physchemProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&dataProfile=resultPhysChem&providers=NWIS&providers=STEWARDS&providers=STORET")
#' projectProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Project/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#'
#' # Join all three profiles using TADA_JoinWQPProfiles
#' TADAProfile <- TADA_JoinWQPProfiles(FullPhysChem = physchemProfile, Sites = stationProfile, Projects = projectProfile)
#'
#' # Run TADA_AutoClean
#' Autocleaned_TADAProfile <- TADA_AutoClean(TADAProfile)
#' }
#'
TADA_AutoClean <- function(.data) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # .data required columns
  required_cols <- c(
    "ActivityMediaName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",
    "CharacteristicName", "ResultSampleFractionText", "MethodSpeciationName",
    "DetectionQuantitationLimitMeasure.MeasureUnitCode", "ResultDetectionConditionText",
    "ResultIdentifier", "DetectionQuantitationLimitMeasure.MeasureValue",
    "LatitudeMeasure", "LongitudeMeasure"
  )

  # check .data has required columns
  TADA_CheckColumns(.data, required_cols)

  # execute function after checks are passed

  # capitalize fields with known synonyms that only differ in caps
  print("TADA_Autoclean: creating TADA-specific columns.")
  .data$TADA.CharacteristicName <- toupper(.data$CharacteristicName)
  .data$TADA.ResultSampleFractionText <- toupper(.data$ResultSampleFractionText)
  .data$TADA.MethodSpeciationName <- toupper(.data$MethodSpeciationName)
  .data$TADA.ResultMeasure.MeasureUnitCode <- toupper(.data$ResultMeasure.MeasureUnitCode)
  .data$TADA.ActivityMediaName <- toupper(.data$ActivityMediaName)
  .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode <-
    toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)

  # Remove complex biological data. Un-comment after new WQX 3.0 Profiles are released. May not be needed if implemented via WQP UI/services.
  # .data$TADA.BiologicalIntentName = toupper(.data$BiologicalIntentName)
  # TADAProfile = dplyr::filter(TADAProfile, TADA.BiologicalIntentName != "TISSUE" | "TOXICITY" | is.na(TADA.BiologicalIntentName) == TRUE)

  # run TADA_ConvertSpecialChars function
  # .data <- MeasureValueSpecialCharacters(.data)
  print("TADA_Autoclean: checking for special characters.")
  .data <- TADA_ConvertSpecialChars(.data, "ResultMeasureValue")
  .data <- TADA_ConvertSpecialChars(.data, "DetectionQuantitationLimitMeasure.MeasureValue")

  # include only in TADA_SimpleCensoredMethods
  # # Identify detection limit data
  # print("TADA_Autoclean: identifying and copying detection limit data to result value if blank.")
  # .data <- TADA_IDCensoredData(.data)

  # change latitude and longitude measures to class numeric
  .data$TADA.LatitudeMeasure <- as.numeric(.data$LatitudeMeasure)
  .data$TADA.LongitudeMeasure <- as.numeric(.data$LongitudeMeasure)

  # Automatically convert USGS only unit "meters" to "m"
  .data$TADA.ResultMeasure.MeasureUnitCode[.data$TADA.ResultMeasure.MeasureUnitCode == "meters"] <- "m"
  .data$ActivityDepthHeightMeasure.MeasureUnitCode[.data$ActivityDepthHeightMeasure.MeasureUnitCode == "meters"] <- "m"
  .data$ActivityTopDepthHeightMeasure.MeasureUnitCode[.data$ActivityTopDepthHeightMeasure.MeasureUnitCode == "meters"] <- "m"
  .data$ActivityBottomDepthHeightMeasure.MeasureUnitCode[.data$ActivityBottomDepthHeightMeasure.MeasureUnitCode == "meters"] <- "m"
  .data$ResultDepthHeightMeasure.MeasureUnitCode[.data$ResultDepthHeightMeasure.MeasureUnitCode == "meters"] <- "m"

  # Substitute updated characteristic name for deprecated names
  print("TADA_Autoclean: updating deprecated (i.e. retired) characteristic names.")
  .data <- TADA_SubstituteDeprecatedChars(.data)

  # Implement unit harmonization
  print("TADA_Autoclean: harmonizing result and depth units.")
  .data <- suppressWarnings(TADA_ConvertResultUnits(.data, transform = TRUE))
  .data <- suppressWarnings(TADA_ConvertDepthUnits(.data, unit = "m"))

  # create comparable data identifier column
  .data <- TADA_CreateComparableID(.data)

  print("NOTE: This version of the TADA package is designed to work with quantitative (numeric) data with media name: 'WATER'. TADA_AutoClean does not currently remove (filter) data with non-water media types. If desired, the user must make this specification on their own outside of package functions. Example: dplyr::filter(.data, TADA.ActivityMediaName == 'WATER')")

  .data <- TADA_OrderCols(.data)

  return(.data)
}



#' Decimal Places
#'
#' for numeric data type
#'
#' @param x Numeric data field from TADA profile
#'
#' @return Number of values to the right of the decimal point for numeric type data.
#'

TADA_DecimalPlaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' Insert Breaks
#'
#' This function inserts a new line into a string when it exceeds a
#' user-specified length. New lines are added to spaces in the string. This is
#' intended to make plot legends more readable and tidy.
#'
#' @param x A vector of strings
#' @param len The maximum character length a string can be before the function
#'   searches for the best space to insert a new line.
#'
#' @return The same vector of strings with new lines added where appropriate.
#' 
#' @export 
#'
TADA_InsertBreaks <- function(x, len = 50) {
  if (nchar(x) > len) {
    multiples <- floor(nchar(x) / len)
    lens <- seq(len, len * multiples, by = len)
    spaces <- unlist(gregexpr(" ", x))
    if (max(spaces) > len) {
      spots <- sapply(lens, function(x) spaces[min(which(spaces > x))])
      for (i in 1:length(spots)) {
        stringi::stri_sub(x, spots[i] + (i - 1), spots[i]) <- "\n "
      }
    }
  }
  return(x)
}


#' Check Type
#'
#' This function checks if the inputs to a function are of the expected type. It
#' is used at the beginning of TADA functions to ensure the
#' inputs are suitable.
#'
#' @param arg An input argument to check
#' @param type Expected class of input argument
#' @param paramName Optional name for argument to use in error message
#'
TADA_CheckType <- function(arg, type, paramName) {
  if ((type %in% class(arg)) == FALSE) {
    # if optional parameter name not specified use arg in errorMessage
    if (missing(paramName)) {
      paramName <- arg
    }
    errorMessage <- paste(paramName, " must be of class '", type, "'", sep = "")
    return(stop(errorMessage))
  }
}



#' Check Columns
#'
#' This function checks if the expected column names are in the dataframe. It is
#' used at the beginning of TADA functions to ensure the input data frame is
#' suitable (i.e. is either the full physical/chemical results profile
#' downloaded from WQP or the TADA profile template downloaded from the EPA TADA
#' webpage.)
#'
#' @param .data A dataframe
#' @param expected_cols A vector of expected column names as strings
#'
TADA_CheckColumns <- function(.data, expected_cols) {
  if (all(expected_cols %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
}



#' TADA_ConvertSpecialChars
#'
#' This function will screen a column of the user's choice for special
#' characters. It creates a NEW column that describes the content of the column
#' prior to conversion to numeric (named "TADA.COLUMN NAME DataTypes.Flag"). It
#' also creates a NEW column to hold the new, numeric format (named "TADA.COLUMN
#' NAME"). This function will successfully convert some special character
#' formats to numeric: whitespace, >, <, ~, %, and commas are removed before
#' converting a result value to numeric. Result values in the format # - # are
#' converted to an average of the two numbers. Result values
#' containing any other text or non-numeric characters become NA in
#' the newly created "TADA.COLUMN NAME" and labeled accordingly in "TADA.COLUMN
#' NAME DataTypes.Flag".
#'
#'
#' @param .data A TADA profile object
#' @param col A character column to be converted to numeric
#'
#' @return Returns the original dataframe with two new columns: the input column
#' with the prefix "TADA.", which holds the numeric form of the original column,
#' and "TADA.ResultValueDataTypes.Flag", which has text describing the type of data
#' contained within the column of interest, including "Numeric","Less Than" (<), "Greater Than" (>),
#' "Approximate Value" (~), "Text" (A-z), "Percentage" (%), "Comma-Separated Numeric" (#,###),
#' and "Numeric Range - Averaged" (# - #)
#'
#' @export
#'
#' @examples
#' data(Data_Nutrients_UT)
#' HandleSpecialChars_ResultMeasureValue <- TADA_ConvertSpecialChars(Data_Nutrients_UT, "ResultMeasureValue")
#' unique(HandleSpecialChars_ResultMeasureValue$TADA.ResultMeasureValueDataTypes.Flag)
#' HandleSpecialChars_DetLimMeasureValue <- TADA_ConvertSpecialChars(Data_Nutrients_UT, "TADA.DetectionQuantitationLimitMeasure.MeasureValue")
#' unique(HandleSpecialChars_DetLimMeasureValue$TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag)
#'
TADA_ConvertSpecialChars <- function(.data, col) {
  if (!col %in% names(.data)) {
    stop("Invalid column name specified for input dataset.")
  }

  # Define new column names
  numcol <- paste0("TADA.", col)
  flagcol <- paste0("TADA.", col, "DataTypes.Flag")

  # Create dummy columns for easy handling in function
  chars.data <- .data
  names(chars.data)[names(chars.data) == col] <- "orig"
  chars.data$masked <- chars.data$orig

  # If column is already numeric, just discern between NA and numeric
  if (is.numeric(chars.data$orig)) {
    clean.data <- chars.data %>%
      dplyr::mutate(flag = dplyr::case_when(
        is.na(masked) ~ as.character("NA - Not Available"),
        TRUE ~ as.character("Numeric")
      ))
  } else {
    chars.data$masked <- gsub(" ", "", chars.data$masked) # get rid of white space for subsequent sorting
    # Detect special characters in column and populate new flag column with descriptor
    # of the specific type of character/data type
    clean.data <- chars.data %>%
      dplyr::mutate(flag = dplyr::case_when(
        is.na(masked) ~ as.character("NA - Not Available"),
        (!is.na(suppressWarnings(as.numeric(masked)) == TRUE)) ~ as.character("Numeric"),
        (grepl("<", masked) == TRUE) ~ as.character("Less Than"),
        (grepl(">", masked) == TRUE) ~ as.character("Greater Than"),
        (grepl("~", masked) == TRUE) ~ as.character("Approximate Value"),
        (grepl("[A-Za-z]", masked) == TRUE) ~ as.character("Text"),
        (grepl("%", masked) == TRUE) ~ as.character("Percentage"),
        (grepl(",", masked) == TRUE) ~ as.character("Comma-Separated Numeric"),
        (grepl("\\d\\-\\d", masked) == TRUE) ~ as.character("Numeric Range - Averaged"),
        # because * is a special character you have to escape\\ it:
        (grepl("\\*", masked) == TRUE) ~ as.character("Approximate Value"),
        (!stringi::stri_enc_mark(masked) %in% c("ASCII")) ~ as.character("Non-ASCII Character(s)"),
        TRUE ~ "Coerced to NA"
      ))

    # Result Values that are numeric ranges with the format #-# are converted to an average of the two numbers expressed in the range.
    if (any(clean.data$flag == "Numeric Range - Averaged")) {
      numrange <- subset(clean.data, clean.data$flag %in% c("Numeric Range - Averaged"))
      notnumrange <- subset(clean.data, !clean.data$flag %in% c("Numeric Range - Averaged"))
      numrange <- numrange %>%
        tidyr::separate(masked, into = c("num1", "num2"), sep = "-", remove = TRUE) %>%
        dplyr::mutate_at(c("num1", "num2"), as.numeric)
      numrange$masked <- as.character(rowMeans(numrange[, c("num1", "num2")], na.rm = TRUE))
      numrange <- numrange[, !names(numrange) %in% c("num1", "num2")]

      clean.data <- plyr::rbind.fill(notnumrange, numrange)
    }

    # In the new TADA column, convert to numeric and remove some specific special
    # characters.
    clean.data$masked <- suppressWarnings(as.numeric(stringr::str_replace_all(
      clean.data$masked, c("<" = "", ">" = "", "~" = "", "%" = "", "\\*" = "")
    )))
  }

  # this updates the DataTypes.Flag to "NA - Not Available" if NA
  clean.data$flag <- ifelse(
    is.na(clean.data$flag),
    "NA - Not Available",
    clean.data$flag
  )

  # Rename to original column name, TADA column name, and flag column name
  names(clean.data)[names(clean.data) == "orig"] <- col
  names(clean.data)[names(clean.data) == "masked"] <- numcol
  names(clean.data)[names(clean.data) == "flag"] <- flagcol

  clean.data <- TADA_OrderCols(clean.data)

  return(clean.data)
}



#' Substitute Preferred Characteristic Name for Deprecated Names
#'
#' This function uses the WQX Characteristic domain table to substitute
#' deprecated (i.e. retired and/or invalid) Characteristic Names with the new
#' name in the TADA.CharacteristicName column. TADA_SubstituteDeprecatedChars is
#' run within TADA_AutoClean, which runs within TADA_DataRetreival and (if autoclean = TRUE)
#' in TADA_BigDataRetrieval. Therefore, deprecated characteristic names are
#' harmonized to the new name automatically upon data retrieval.
#' TADA_SubstituteDeprecatedChars can also be used by itself on a user supplied
#' dataset that is in the WQX/WQP format, if desired. This solution works for both
#' EPA WQX and USGS NWIS provided data.
#'
#' Enter ?TADA_GetCharacteristicRef() to review a list of all WQX characteristics, the including
#' deprecated names (Char_Flag). This can be used as a crosswalk between the deprecated names
#' (CharacteristicName) and their new names (Comparable.Name).
#'
#' @param .data TADA dataframe
#'
#' @return Input TADA dataframe with substituted characteristic names in
#'   TADA.CharacteristicName column. Original columns are unchanged.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download nutrient data in MT from 2022 and set autoclean = FALSE
#' df <- TADA_DataRetrieval(startDate = "2022-01-01", endDate = "2022-12-31", characteristicType = "Nutrient", statecode = "MT", applyautoclean = FALSE)
#' df2 <- TADA_SubstituteDeprecatedChars(df)
#' # in this example, "Inorganic nitrogen (nitrate and nitrite)" is a USGS NWIS characteristic that is
#' # deprecated and "Phosphate-phosphorus***retired***use Total Phosphorus, mixed forms" is a deprecated WQX
#' # name. Both are are transformed to their new names.
#' # review characteristic names before and after transformation
#' unique(df2$CharacteristicName)
#' unique(df2$TADA.CharacteristicName)
#'
#' df3 <- TADA_DataRetrieval(startDate = "2022-01-01", endDate = "2022-12-31", characteristicType = "Nutrient", statecode = "WY", applyautoclean = FALSE)
#' df4 <- TADA_SubstituteDeprecatedChars(df3)
#' unique(df4$CharacteristicName)
#' unique(df4$TADA.CharacteristicName)
#' }
#'
TADA_SubstituteDeprecatedChars <- function(.data) {
  TADA_CheckColumns(.data, expected_cols = c("CharacteristicName"))

  if ("TADA.CharacteristicName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original CharacteristicName
    .data$TADA.CharacteristicName <- toupper(.data$CharacteristicName)
  }

  # read in characteristic reference table with deprecation information, filter to deprecated terms and for "retired" in CharactersticName.
  # remove all characters after first "*" in CharacteristicName and remove any leading or trailing white space to make compatible with deprecated NWIS CharactersticName.
  nwis.table <- utils::read.csv(system.file("extdata", "WQXCharacteristicRef.csv", package = "TADA")) %>%
    dplyr::filter(
      Char_Flag == "Deprecated",
      grepl("retired", CharacteristicName)
    ) %>%
    dplyr::mutate(CharacteristicName = trimws(stringr::str_split(CharacteristicName, "\\*", simplify = T)[, 1]))

  # read in characteristic reference table with deprecation information and filter to deprecated terms.
  # join with deprecated NWIS CharacteristicName data.frame.
  ref.table <- utils::read.csv(system.file("extdata", "WQXCharacteristicRef.csv", package = "TADA")) %>%
    dplyr::filter(Char_Flag == "Deprecated") %>%
    rbind(nwis.table)

  rm(nwis.table)

  # merge to dataset
  .data <- merge(.data, ref.table, all.x = TRUE)
  # if CharacteristicName is deprecated and comparable name is not blank (NA), use the provided Comparable.Name. Otherwise, keep TADA.CharacteristicName as-is.
  .data$TADA.CharacteristicName <- ifelse(!is.na(.data$Char_Flag) & !.data$Comparable.Name %in% c(""), .data$Comparable.Name, .data$TADA.CharacteristicName)

  howmany <- length(.data$Char_Flag[!is.na(.data$Char_Flag)])

  if (howmany > 0) {
    chars <- unique(.data$CharacteristicName[!is.na(.data$Char_Flag)])
    chars <- paste0(chars, collapse = "; ")
    print(paste0(howmany, " results in your dataset have one of the following deprecated characteristic names: ", chars, ". These names have been substituted with the updated preferred names in the TADA.CharacteristicName field."))
  } else {
    print("No deprecated characteristic names found in dataset.")
  }

  .data <- .data %>% dplyr::select(-Char_Flag, -Comparable.Name)
  .data <- TADA_OrderCols(.data)
  return(.data)
}

#' Create TADA.ComparableDataIdentifier Column
#'
#' This utility function creates the TADA.ComparableDataIdentifier column by pasting
#' together TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName,
#' and TADA.ResultMeasure.MeasureUnitCode.
#'
#' @param .data TADA dataframe
#'
#' @return Input TADA dataframe with added TADA.ComparableDataIdentifier column.
#'
#' @export
#'
TADA_CreateComparableID <- function(.data) {
  TADA_CheckColumns(.data, expected_cols = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode"))
  .data$TADA.ComparableDataIdentifier <- paste(.data$TADA.CharacteristicName, .data$TADA.ResultSampleFractionText, .data$TADA.MethodSpeciationName, .data$TADA.ResultMeasure.MeasureUnitCode, sep = "_")
  return(.data)
}


#' Identify and group nearby monitoring locations (UNDER ACTIVE DEVELOPMENT)
#'
#' This function takes a TADA dataset and creates a distance matrix for all
#' sites in the dataset. It then uses the buffer input value to determine which
#' sites are within the buffer and should be identified as part of a nearby site
#' group.
#'
#' @param .data TADA dataframe OR TADA sites dataframe
#' @param dist_buffer Numeric. The maximum distance (in meters) two sites can be
#'   from one another to be considered "nearby" and grouped together.
#'
#' @return Input dataframe with a TADA.NearbySiteGroups column that indicates
#'   the nearby site groups each monitoring location belongs to.
#'
#' @export
#'
TADA_FindNearbySites <- function(.data, dist_buffer = 100) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # .data required columns
  required_cols <- c("MonitoringLocationIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure")
  # check .data has required columns
  TADA_CheckColumns(.data, required_cols)

  # create spatial dataset based on sites
  data_sf <- unique(.data[, c("MonitoringLocationIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure")])
  # convert to sf object
  data_sf <- sf::st_as_sf(data_sf,
    coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
    # Change to your CRS
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )
  # create a distance matrix in meters
  dist.mat <- data.frame(sf::st_distance(data_sf)) # Great Circle distance since in lat/lon

  row.names(dist.mat) <- data_sf$MonitoringLocationIdentifier
  colnames(dist.mat) <- data_sf$MonitoringLocationIdentifier

  # convert distances to those within buffer (1) and beyond buffer (0)
  dist.mat1 <- apply(dist.mat, c(1, 2), function(x) {
    if (x <= dist_buffer) {
      x <- 1
    } else {
      x <- 0
    }
  })

  # create empty dataframe for groups
  groups <- data.frame()

  # loop through distance matrix and extract site groups that are within the buffer distance from one another
  for (i in 1:dim(dist.mat1)[1]) {
    fsite <- rownames(dist.mat1)[i] # focal site
    dat <- data.frame(Count = dist.mat1[i, ]) # get focal site count row as a column
    dat$MonitoringLocationIdentifier <- colnames(dist.mat1) # give df site names along with counts
    sites <- dat$MonitoringLocationIdentifier[dat$Count == 1] # filter to sites within buffer
    sites1 <- sites[!sites %in% fsite] # get site list within buffer that does not include focal site
    if (length(sites1) > 0) { # if this list is greater than 0, combine sites within buffer into data frame
      df <- data.frame(MonitoringLocationIdentifier = sites, TADA.SiteGroup = paste0(sites, collapse = ", "))
      groups <- plyr::rbind.fill(groups, df)
    }
  }

  # get unique groups (since represented multiple times for each site looped through, above)
  groups <- unique(groups)

  if (dim(groups)[1] > 0) { # if there are groups of nearby sites...
    # create group ID's for easier understanding
    grp <- data.frame(TADA.SiteGroup = unique(groups$TADA.SiteGroup), TADA.SiteGroupID = paste0("Group_", 1:length(unique(groups$TADA.SiteGroup))))
    groups <- merge(groups, grp, all.x = TRUE)
    groups <- unique(groups[, !names(groups) %in% c("TADA.SiteGroup")])

    # find any sites within multiple groups
    summ_sites <- groups %>%
      dplyr::group_by(MonitoringLocationIdentifier) %>%
      dplyr::mutate(GroupCount = 1:length(MonitoringLocationIdentifier))

    # pivot wider if a site belongs to multiple groups
    groups_wide <- merge(groups, summ_sites, all.x = TRUE)
    groups_wide <- tidyr::pivot_wider(groups_wide, id_cols = "MonitoringLocationIdentifier", names_from = "GroupCount", names_prefix = "TADA.SiteGroup", values_from = "TADA.SiteGroupID")
    # merge data to site groupings
    .data <- merge(.data, groups_wide, all.x = TRUE)
  } else { # if no groups, give a TADA.NearbySiteGroups column filled with NA
    .data$TADA.NearbySiteGroups <- "No nearby sites"
    print("No nearby sites detected using input buffer distance.")
  }

  # concatenate and move site id cols to right place
  grpcols <- names(.data)[grepl("TADA.SiteGroup", names(.data))]

  .data <- .data %>% tidyr::unite(col = TADA.NearbySiteGroups, dplyr::all_of(grpcols), sep = ", ", na.rm = TRUE)
  .data$TADA.NearbySiteGroups[.data$TADA.NearbySiteGroups == ""] <- "No nearby sites"

  # order columns
  if ("ResultIdentifier" %in% names(.data)) {
    .data <- TADA_OrderCols(.data)
  }

  return(.data)
}

#' Generate a random WQP dataset
#'
#' Retrieves data for a period of time in the past 20 years using
#' TADA_DataRetrieval. This function can be used for testing functions on
#' random datasets.
#'
#' @param number_of_days Numeric. The default is 1, which will query and retrieve
#' data for a random two-day period (e.g.startDate = "2015-04-21",
#' endDate = "2015-04-22"). The user can change this number to select additional days
#' if desired.
#'
#' @param choose_random_state Boolean (TRUE or FALSE). The default is FALSE.
#' If FALSE, the function will query all data in the WQP for the number_of_days
#' specified (national query). If TRUE, the function will select a random state
#' and only retrieve data for that state.
#'
#' @return Random WQP dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- TADA_RandomTestingData(number_of_days = 1, choose_random_state = FALSE)
#' df <- TADA_RandomTestingData(number_of_days = 10, choose_random_state = TRUE)
#' }
#'
TADA_RandomTestingData <- function(number_of_days = 1, choose_random_state = FALSE) {
  # choose a random day within the last 20 years
  twenty_yrs_ago <- Sys.Date() - 20 * 365
  random_start_date <- twenty_yrs_ago + sample(20 * 365, 1)
  # choose a random start date and add any number_of_days (set that as the end date)
  end_date <- random_start_date + number_of_days

  if (choose_random_state == TRUE) {
    load(system.file("extdata", "statecodes_df.Rdata", package = "TADA"))
    state <- sample(statecodes_df$STUSAB, 1)
  }

  if (choose_random_state == FALSE) {
    state <- "null"
  }

  print(c(
    startDate = as.character(random_start_date),
    endDate = as.character(end_date),
    statecode = state
  ))

  # retrieve data
  dat <- TADA_DataRetrieval(
    startDate = as.character(random_start_date),
    endDate = as.character(end_date),
    statecode = state
  )

  if (dim(dat)[1] < 1) {
    dat <- Data_NCTCShepherdstown_HUC12
  }

  return(dat)
}

# Generate a random data retrieval dataset (internal, for testthat's)
# samples a random 90 days in the past 20 years using
# TADA_DataRetrieval. Built to use in testthat and for developer testing of new
# functions on random datasets.
TADA_RandomStateTestingSet <- function(number_of_days = 90) {
  load(system.file("extdata", "statecodes_df.Rdata", package = "TADA"))
  state <- sample(statecodes_df$STUSAB, 1)
  twenty_yrs_ago <- Sys.Date() - 20 * 365
  random_start_date <- twenty_yrs_ago + sample(20 * 365, 1)
  # changed default to 2 days instead of 90
  end_date <- random_start_date + number_of_days

  print(paste0(state, " from ", random_start_date, " to ", end_date))

  # removed state input
  dat <- TADA_DataRetrieval(startDate = as.character(random_start_date), endDate = as.character(end_date), statecode = state)

  if (dim(dat)[1] < 1) {
    dat <- Data_NCTCShepherdstown_HUC12
  }

  return(dat)
}


#' Aggregate multiple result values to a min, max, or mean
#'
#' This function groups TADA data by user-defined columns and aggregates the
#' TADA.ResultMeasureValue to a minimum, maximum, or average value.
#'
#' @param .data A TADA dataframe
#' @param grouping_cols The column names used to group the data
#' @param agg_fun The aggregation function used on the grouped data. This can
#'   either be 'min', 'max', or 'mean'.
#' @param clean Boolean. Determines whether other measurements from the group
#'   aggregation should be removed or kept in the dataframe. If clean = FALSE,
#'   additional measurements are indicated in the
#'   TADA.ResultValueAggregation.Flag as "Used in aggregation function but not
#'   selected".
#'
#' @return A TADA dataframe with aggregated values combined into one row. If the
#'   agg_fun is 'min' or 'max', the function will select the row matching the
#'   aggregation condition and flag it as the selected measurement. If the
#'   agg_fun is 'mean', the function will select a random row from the
#'   aggregated rows to represent the metadata associated with the mean value,
#'   and gives the row a unique ResultIdentifier: the original ResultIdentifier
#'   with the prefix "TADA-". Function adds a TADA.ResultValueAggregation.Flag
#'   to indicate which rows have been aggregated.
#'
#' @export
#'
#' @examples
#' # Load example dataset
#' data(Data_6Tribes_5y)
#' # Select maximum value per day, site, comparable data identifier, result detection condition, and activity type code. Clean all non-maximum measurements from grouped data.
#' Data_6Tribes_5y_agg <- TADA_AggregateMeasurements(Data_6Tribes_5y, grouping_cols = c("ActivityStartDate", "MonitoringLocationIdentifier", "TADA.ComparableDataIdentifier", "ResultDetectionConditionText", "ActivityTypeCode"), agg_fun = "max", clean = TRUE)
#'
#' # Calculate a mean value per day, site, comparable data identifier, result detection condition, and activity type code. Keep all measurements used to calculate mean measurement.
#' Data_6Tribes_5y_agg <- TADA_AggregateMeasurements(Data_6Tribes_5y, grouping_cols = c("ActivityStartDate", "MonitoringLocationIdentifier", "TADA.ComparableDataIdentifier", "ResultDetectionConditionText", "ActivityTypeCode"), agg_fun = "mean", clean = FALSE)
TADA_AggregateMeasurements <- function(.data, grouping_cols = c("ActivityStartDate", "MonitoringLocationIdentifier", "TADA.ComparableDataIdentifier", "ResultDetectionConditionText", "ActivityTypeCode"), agg_fun = c("max", "min", "mean"), clean = TRUE) {
  TADA_CheckColumns(.data, grouping_cols)
  agg_fun <- match.arg(agg_fun)

  # Find multiple values in groups
  ncount <- .data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
    dplyr::summarise(ncount = length(ResultIdentifier))

  if (max(ncount$ncount) < 2) {
    print("No rows to aggregate.")
    return(.data)
  } else {
    dat <- merge(.data, ncount, all.x = TRUE)

    if (any(is.na(dat$TADA.ResultMeasureValue))) {
      "Warning: your dataset contains one or more rows where TADA.ResultMeasureValue = NA. Recommend removing these rows before proceeding. Otherwise, the function will not consider NAs in its calculations."
    }

    dat$TADA.ResultValueAggregation.Flag <- ifelse(dat$ncount == 1, "No aggregation needed", paste0("Used in ", agg_fun, " aggregation function but not selected"))
    multiples <- dat %>% dplyr::filter(ncount > 1)

    dat <- dat %>% dplyr::select(-ncount)

    if (agg_fun == "max") {
      out <- multiples %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
        dplyr::slice_max(order_by = TADA.ResultMeasureValue, n = 1, with_ties = FALSE)
      dat$TADA.ResultValueAggregation.Flag <- ifelse(dat$ResultIdentifier %in% out$ResultIdentifier, paste0("Selected as ", agg_fun, " aggregate value"), dat$TADA.ResultValueAggregation.Flag)
    }
    if (agg_fun == "min") {
      out <- multiples %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
        dplyr::slice_min(order_by = TADA.ResultMeasureValue, n = 1, with_ties = FALSE)
      dat$TADA.ResultValueAggregation.Flag <- ifelse(dat$ResultIdentifier %in% out$ResultIdentifier, paste0("Selected as ", agg_fun, " aggregate value"), dat$TADA.ResultValueAggregation.Flag)
    }
    if (agg_fun == "mean") {
      out <- multiples %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
        dplyr::mutate(TADA.ResultMeasureValue1 = mean(TADA.ResultMeasureValue, na.rm = TRUE)) %>%
        dplyr::slice_sample(n = 1) %>%
        dplyr::mutate(TADA.ResultValueAggregation.Flag = paste0("Selected as ", agg_fun, " aggregate value, with randomly selected metadata from a row in the aggregate group"))
      out <- out %>%
        dplyr::select(-TADA.ResultMeasureValue) %>%
        dplyr::rename(TADA.ResultMeasureValue = TADA.ResultMeasureValue1) %>%
        dplyr::mutate(ResultIdentifier = paste0("TADA-", ResultIdentifier))
      dat <- plyr::rbind.fill(dat, out)
    }

    if (clean == TRUE) {
      dat <- subset(dat, !dat$TADA.ResultValueAggregation.Flag %in% c(paste0("Used in ", agg_fun, " aggregation function but not selected")))
    }

    dat <- TADA_OrderCols(dat)
    print("Aggregation results:")
    print(table(dat$TADA.ResultValueAggregation.Flag))
    return(dat)
  }
}

#' Run key flagging functions
#'
#' This is a shortcut function to run all of the most important flagging functions on a TADA dataset. See ?function documentation for TADA_FlagResultUnit, TADA_FlagFraction, TADA_FindQCActivities, and TADA_FlagSpeciation for more information.
#'
#' @param .data A TADA dataframe
#' @param remove_na Boolean, Determines whether to keep TADA.ResultMeasureValues that are NA. Defaults to TRUE.
#' @param clean Boolean. Determines whether to keep the Invalid rows in the dataset following each flagging function. Defaults to TRUE.
#'
#' @return A TADA dataframe with the following flagging columns:TADA.ResultUnit.Flag, TADA.MethodSpeciation.Flag, TADA.SampleFraction.Flag, and TADA.ActivityType.Flag
#'
#' @export
#'
#' @examples
#' # Load example dataset
#' data(Data_6Tribes_5y)
#' # Run flagging functions, keeping all rows
#' Data_6Tribes_5y_ALL <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, remove_na = FALSE, clean = FALSE)
#'
#' # Run flagging functions, removing NA's and Invalid rows
#' Data_6Tribes_5y_CLEAN <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, remove_na = TRUE, clean = TRUE)
TADA_RunKeyFlagFunctions <- function(.data, remove_na = TRUE, clean = TRUE) {
  if (remove_na == TRUE) {
    .data <- .data %>% dplyr::filter(!is.na(TADA.ResultMeasureValue))
  }

  if (clean == TRUE) {
    .data <- TADA_FlagResultUnit(.data, clean = "invalid_only")
    .data <- TADA_FlagFraction(.data, clean = TRUE)
    .data <- TADA_FlagSpeciation(.data, clean = "invalid_only")
    .data <- TADA_FindQCActivities(.data, clean = TRUE)
  } else {
    .data <- TADA_FlagResultUnit(.data, clean = "none")
    .data <- TADA_FlagFraction(.data, clean = FALSE)
    .data <- TADA_FlagSpeciation(.data, clean = "none")
    .data <- TADA_FindQCActivities(.data, clean = FALSE)
  }

  return(.data)
}

#' Get bounding box JSON
#'
#' @param bbox A bounding box from the sf function st_bbox
#' @return A string containing bounding box JSON that can be passed to an ArcGIS feature layer in the Input Geometry field
#'
#' @examples
#' \dontrun{
#' # Load example dataset
#' data(Data_6Tribes_5y)
#' # Get the bounding box of the data
#' bbox <- sf::st_bbox(c(xmin = min(Data_6Tribes_5y$TADA.LongitudeMeasure), ymin = min(Data_6Tribes_5y$TADA.LatitudeMeasure), xmax = max(Data_6Tribes_5y$TADA.LongitudeMeasure), ymax = max(Data_6Tribes_5y$TADA.LatitudeMeasure)), crs = sf::st_crs(Data_6Tribes_5y))
#' # Get a string containing the JSON of the bounding box
#' getBboxJson(bbox)
#' }
#'
getBboxJson <- function(bbox) {
  json <- paste0('{"xmin":', bbox[1], ',"ymin":', bbox[2], ',"xmax":', bbox[3], ',"ymax":', bbox[4], "}")
  return(json)
}

#' Create icon(s) to be used to represent points on a map feature layer
#' pchIcons is used within TADA_addPoints
#'
#' Uses the different plotting symbols available in R to create PNG files that can be used as markers on a map feature layer.
#'
#' @param pch Plot character code; either a single number or a vector of multiple numbers. Possible values available at http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r. Defaults to 1 (an open circle).
#' @param width Width of the plot character. Defaults to 30 pixels.
#' @param height Height of the plot character. Defaults to 30 pixels.
#' @param bg Background color of the plot character Defaults to transparent.
#' @param col Color(s) of the plot character(s). Defaults to black.
#' @param lwd Line width. Optional, defaults to NULL.
#' @return Path(s) to PNG file(s) in a temp folder on user's computer.
#'
#' @examples
#' \dontrun{
#' # Create three PNG files, a red circle, blue triangle, and yellow "X", each on a green background.
#' pchIcons(c(1, 2, 4), 40, 40, "green", c("red", "blue", "yellow"))
#' }
#'
pchIcons <- function(pch = 1,
                     width = 30,
                     height = 30,
                     bg = "transparent",
                     col = "black",
                     lwd = NULL) {
  n <- length(pch)
  files <- character(n)
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f,
      width = width,
      height = height,
      bg = bg
    )
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::points(
      .5,
      .5,
      pch = pch[i],
      col = col[i],
      cex = min(width, height) / 8,
      lwd = lwd
    )
    grDevices::dev.off()
    files[i] <- f
  }
  files
}

#' Retrieve feature layer from ArcGIS REST service
#' getFeatureLayer is used within TADA_addPolys and TADA_addPoints
#'
#' @param url URL of the layer REST service, ending with "/query". Example: https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query (American Indian Reservations)
#' @param bbox A bounding box from the sf function st_bbox; used to filter the query results. Optional; defaults to NULL.
#' @return ArcGIS feature layer
#'
#' @examples
#' \dontrun{
#' # Load example dataset
#' data(Data_Nutrients_UT)
#' # Get the bounding box of the data
#' bbox <- sf::st_bbox(c(xmin = min(Data_Nutrients_UT$TADA.LongitudeMeasure), ymin = min(Data_Nutrients_UT$TADA.LatitudeMeasure), xmax = max(Data_Nutrients_UT$TADA.LongitudeMeasure), ymax = max(Data_Nutrients_UT$TADA.LatitudeMeasure)), crs = sf::st_crs(Data_Nutrients_UT))
#' # Get the American Indian Reservations feature layer, filtered by the bounding box for the Data_Nutrients_UT example dataset
#' getFeatureLayer("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query", bbox)
#' }
#'
getFeatureLayer <- function(url, bbox = NULL) {
  if (is.null(bbox)) {
    inputGeom <- NULL
  } else {
    inputGeom <- getBboxJson(bbox)
  }
  url <- paste0(url, "?where=1%3D1&outfields=*&returnGeometry=true&geometry=", inputGeom, "&f=geojson")
  layer <- sf::read_sf(url)
  return(layer)
}


#' Get text for tribal marker popup
#' getPopup is used within TADA_addPolys and TADA_addPoints
#'
#' @param layer A map feature layer
#' @param layername Name of the layer
#' @return Vector of strings to be used as the text for the popups when clicking on a tribal marker
#'
#' @examples
#' \dontrun{
#' # Get the Oklahoma Tribal Statistical Areas feature layer
#' layer <- getFeatureLayer("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query")
#' # Get popup text for individual markers
#' getPopup(layer, "Oklahoma Tribal Statistical Areas")
#' }
#'
getPopup <- function(layer, layername) {
  text <- paste0("<strong>", layername, "</strong><p>")
  cols <-
    c(
      "TRIBE_NAME" = "Tribe Name",
      "PARCEL_NO" = "Parcel Number",
      "EPA_ID" = "EPA ID",
      "TYPE" = "Type"
    )

  for (i in seq(1, length(cols))) {
    if (names(cols[i]) %in% colnames(layer)) {
      text <- paste0(text, "<strong>", cols[i], "</strong>: ", layer[[names(cols[i])]], "<br>")
    }
  }
  return(text)
}

#' Add polygons from an ArcGIS feature layer to a leaflet map
#'
#' @param map A leaflet map
#' @param url URL of the ArcGIS REST service returning the polygon feature layer
#' @param layergroup Name of the layer group
#' @param layername Name of the layer
#' @param bbox A bounding box from the sf function st_bbox; used to filter the query results. Optional; defaults to NULL.
#' @return The original map with polygon from the feature layer added to it.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a leaflet map
#' lmap <- leaflet::leaflet() %>% leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo")
#' # Add the American Indian Reservations feature layer to the map
#' lmap <- TADA_addPolys(lmap, "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query", "Tribes", "American Indian Reservations")
#' lmap
#' }
#'
TADA_addPolys <- function(map, url, layergroup, layername, bbox = NULL) {
  layer <- getFeatureLayer(url, bbox)
  if (is.null(layer)) {
    return(map)
  }
  lbbox <- sf::st_bbox(layer)
  if (is.na(lbbox[1])) {
    return(map)
  }
  areaColumn <- "ALAND_KM"
  if (!(areaColumn %in% colnames(layer))) {
    areaColumn <- "AREA_KM"
  }

  map <-
    leaflet::addPolygons(
      map,
      data = layer,
      color = "#A0522D",
      weight = 0.35,
      smoothFactor = 0.5,
      opacity = 1.0,
      fillOpacity = 0.2,
      fillColor = ~ leaflet::colorNumeric("Oranges", layer[[areaColumn]])(layer[[areaColumn]]),
      highlightOptions = leaflet::highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      ),
      popup = getPopup(layer, layername),
      group = layergroup
    )
  return(map)
}

#' Add points from an ArcGIS feature layer to a leaflet map
#'
#' @param map A leaflet map
#' @param url URL of the ArcGIS REST service returning the points feature layer
#' @param layergroup Name of the layer group
#' @param layername Name of the layer
#' @param bbox A bounding box from the sf function st_bbox; used to filter the query results. Optional; defaults to NULL.
#' @return The original map with polygon from the feature layer added to it.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a leaflet map
#' lmap <- leaflet::leaflet() %>% leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo")
#' # Add the Virginia Federally Recognized Tribes feature layer to the map
#' lmap <- TADA_addPoints(lmap, "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5/query", "Tribes", "Virginia Federally Recognized Tribes")
#' lmap
#' }
#'
TADA_addPoints <- function(map, url, layergroup, layername, bbox = NULL) {
  layer <- getFeatureLayer(url, bbox)
  if (is.null(layer)) {
    return(map)
  }
  lbbox <- sf::st_bbox(layer)
  if (is.na(lbbox[1])) {
    return(map)
  }
  shapes <- c(2) # open triangle; for other options see http://www.statmethods.net/advgraphs/parameters.html
  iconFiles <- pchIcons(shapes, width = 20, height = 20, col = c("#CC7722"), lwd = 2)
  map <- leaflet::addMarkers(
    map,
    data = layer,
    icon = ~ leaflet::icons(
      iconUrl = iconFiles[],
      popupAnchorX = 20,
      popupAnchorY = 0
    ),
    popup = getPopup(layer, layername),
    group = layergroup
  )
  return(map)
}
