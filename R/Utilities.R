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

#' Silence print messages
#' @name quiet
#' @param x Code to silence
#' @return Function or code output with print messages silenced
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# write global variables. Gets rid of global variable NOTE in check:
utils::globalVariables(c(
  "TADA.ResultValueAboveUpperThreshold.Flag", "ActivityIdentifier", "ActivityMediaName",
  "ActivityStartDate", "TADA.ResultValueBelowUpperThreshold.Flag",
  "TADA.ResultValueBelowLowerThreshold.Flag", "CharacteristicName",
  "Conversion.Factor", "Count", "Description", "FieldName", "FieldValue",
  "MethodSpecationName", "MonitoringLocationIdentifier",
  "OrganizationFormalName", "OrganizationIdentifier", "ProjectDescriptionText",
  "ProjectFileUrl", "ProjectIdentifier",
  "ProjectMonitoringLocationWeightingUrl", "ProjectName",
  "QAPPApprovalAgencyName", "QAPPApprovedIndicator",
  "ResultDetectionConditionText", "ResultMeasureValue",
  "SamplingDesignTypeCode", "Source", "Status", "TADA.ContinuousData.Flag",
  "TADA.SuspectCoordinates.Flag", "TADA.PotentialDupRowIDs.Flag", "TADA.QAPPDocAvailable",
  "Target.Unit", "Type", "Value.Unit", "TADA.AnalyticalMethod.Flag",
  "TADA.MethodSpeciation.Flag", "TADA.ResultUnit.Flag",
  "TADA.SampleFraction.Flag", "YearSummarized", "where", "TADA.CharacteristicName",
  "ResultIdentifier", "TADA.ResultMeasureValue", "n_sites",
  "n_records", "statecodes_df", "STUSAB", "ActivityStartTime.Time", "numorgs", "dup_id",
  "LatitudeMeasure", "TADA.ResultMeasureValueDataTypes.Flag", "Name", "TADA.Detection_Type",
  "DetectionQuantitationLimitTypeName", "TADA.Limit_Type", "multiplier", "summ", "cf",
  "LongitudeMeasure", "TADA.CensoredData.Flag", "Censored_Count",
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
  "TADA.MeasureQualifierCode.Flag", "TADA.MeasureQualifierCode.Def", "MeasureQualifierCode",
  "value", "Flag_Column", "Data_NCTCShepherdstown_HUC12", "ActivityStartDateTime",
  "TADA.MultipleOrgDupGroupID", "TADA.WQXVal.Flag", "Concat", ".", "MeasureQualifierCode.Split",
  "TADA.Media.Flag", "ML.Media.Flag", "TADA.UseForAnalysis.Flag",
  "Unique.Identifier", "Domain", "Note.Recommendation", "Conversion.Coefficient",
  "Conversion.Coefficient", "Last.Change.Date", "Value", "Minimum", "Unique.Identifier",
  "Domain", "ResultMeasure.MeasureUnitCode", "Comb", "CombList",
  "TADA.Target.ResultMeasure.MeasureUnitCode", "TADA.WQXUnitConversionFactor",
  "TADA.WQXUnitConversionCoefficient", "TADA.Target.MethodSpeciationName",
  "flag", "NConvert", "MultUnits", "CharList", "CharUnit", "SingleNearbyGroup",
  "TADA.MultipleOrgDuplicate", "TADA.ResultSelectedMultipleOrgs", "Maximum",
  "OBJECTID", "GLOBALID", "assessmentunitidentifier", "index", "epsg",
  "ResultMeasure.MeasureUnitCode", "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode",
  "DetectionQuantitationLimitMeasure.MeasureUnitCode", "NCode",
  "ATTAINS.assessmentunitidentifier", "ATTAINS_AU", "TOTALAREA_MI", "TOTALAREA_KM",
  "ATTAINS_AUs", "ARD_Category", "ActivityRelativeDepthName", "DepthsByGroup",
  "DepthsPerGroup", "MeanResults", "MonitoringLocationTypeName", "N", "SecchiConversion",
  "TADA.ActivityBottomDepthHeightMeasure.MeasureValue",
  "TADA.ActivityDepthHeightMeasure.MeasureUnitCode", "TADA.ActivityDepthHeightMeasure.MeasureValue",
  "TADA.CharacteristicsForDepthProfile TADA.ConsolidatedDepth",
  "TADA.ConsolidatedDepth.Bottom TADA.ConsolidatedDepth.Unit", "TADA.DepthCategory.Flag",
  "TADA.DepthProfileAggregation.Flag", "TADA.NResults",
  "TADA.ResultDepthHeightMeasure.MeasureUnitCode", "TADA.ResultDepthHeightMeasure.MeasureValue",
  "YAxis.DepthUnit", "TADA.CharacteristicsForDepthProfile", "TADA.ConsolidatedDepth",
  "TADA.ConsolidatedDepth.Bottom", "TADA.ConsolidatedDepth.Unit", "col2rgb",
  "palette.colors", "rect", "rgb", "text", "CodeNoSpeciation", "ResultMeasure.MeasureUnitCode.Upper",
  "TADA.MonitoringLocationIdentifier", "StringA", "StringB", "MeasureUnitCode.match",
  "TADA.ActivityTopDepthHeightMeasure.MeasureValue", "group_id", "time_diff_lead", "time_diff_lag",
  "NResults", "missing.group", "TADA.PairingGroup", "TADA.PairingGroup.Rank", "timediff",
  "TADA.MonitoringLocationName", "TADA.MonitoringLocationTypeName"
  "ATTAINS.submissionid", "HorizontalCoordinateReferenceSystemDatumName", 
  "NCount", "NHD.catchmentareasqkm", "NHD.comid", "NHD.nhdplusid", "NHD.resolution", 
  "areasqkm", "assessmentUnitIdentifier", "catchmentareasqkm", "comid", 
  "featureid", "geometry", "nhdplusid", "waterTypeCode"
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
#' This function performs the following tasks:
#' 1) Creates new columns with the TADA prefix "TADA." and capitalizes all
#' letters within them so that they're interoperable with the WQX validation
#' reference tables and to reduce issues with case-sensitivity when joining
#' data: CharacteristicName, ResultSampleFractionText, MethodSpeciationName,
#' ResultMeasure.MeasureUnitCode, ActivityMediaName, and
#' DetectionQuantitationLimitMeasure.MeasureUnitCode.
#' 2) This function runs "TADA_ConvertSpecialChars" on these columns:
#' ResultMeasureValue and DetectionQuantitationLimitMeasure.MeasureValue.
#' Creates new versions of these columns with the TADA prefix "TADA."
#' 3) Converts the column type of LatitudeMeasure and LongitudeMeasure to
#' numeric (double) and creates new columns with the “TADA” prefix.
#' 4) Replace meters" with “m” in the following columns:
#' TADA.ResultMeasure.MeasureUnitCode,
#' ActivityDepthHeightMeasure.MeasureUnitCode,
#' ActivityTopDepthHeightMeasure.MeasureUnitCode,
#' ActivityBottomDepthHeightMeasure.MeasureUnitCode, and
#' ResultDepthHeightMeasure.MeasureUnitCode.
#' 5) Runs TADA_SubstituteDeprecatedChars to replace deprecated characteristic
#' names based on Water Quality Exchange (WQX) Characteristic domain table.
#' 6) Runs TADA_ConvertResultUnits to harmonize result and detection limit
#' units to WQX and TADA or user supplied target units. Enter
#' ?TADA_ConvertResultUnits and ?TADA_CreateUnitRef() into the console for more
#' details.
#' 7) Runs TADA_ConvertDepthUnits to convert the depth units to meters on the
#' following columns: ResultDepthHeightMeasure.MeasureValue,
#' ActivityDepthHeightMeasure.MeasureValue,
#' ActivityTopDepthHeightMeasure.MeasureValue,
#' and ActivityBottomDepthHeightMeasure.MeasureValue, and add new columns
#' with the “TADA” prefix.
#' 8) Runs TADA_CreateComparableID to create a comparable data group by
#' concatenating TADA.CharacteristicName, TADA.ResultSampleFractionText,
#' TADA.MethodSpeciationName, and TADA.ResultMeasure.MeasureUnitCode.
#'
#' Original columns are not changed: new columns are added to the end of the
#' dataframe with the prefix "TADA.". TADA_AutoClean can be run as a stand
#' alone function but is primarily used by the TADA_dataRetrieval function.
#'
#' @param .data TADA dataframe
#'
#' @return Input dataframe with several added TADA-specific columns, including:
#'
#' TADA.ActivityMediaName (character)
#' TADA.ResultSampleFractionText (character)
#' TADA.CharacteristicName (character)
#' TADA.MethodSpeciationName (character)
#' TADA.ComparableDataIdentifier (character)
#' TADA.ResultMeasureValue (numeric)
#' TADA.ResultMeasureValueDataTypes.Flag (character)
#' TADA.ResultMeasure.MeasureUnitCode	(character)
#' TADA.WQXResultUnitConversion	(character)
#' TADA.DetectionQuantitationLimitMeasure.MeasureValue (numeric)
#' TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag (character)
#' TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode	(character)
#' TADA.ResultDepthHeightMeasure.MeasureValue	(numeric)
#' TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag	(character)
#' TADA.ResultDepthHeightMeasure.MeasureUnitCode (character)
#' TADA.ActivityDepthHeightMeasure.MeasureValue	(numeric)
#' TADA.ActivityDepthHeightMeasure.MeasureValueDataTypes.Flag	(character)
#' TADA.ActivityDepthHeightMeasure.MeasureUnitCode (character)
#' TADA.ActivityTopDepthHeightMeasure.MeasureValue (numeric)
#' TADA.ActivityTopDepthHeightMeasure.MeasureValueDataTypes.Flag (character)
#' TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode	(character)
#' TADA.ActivityBottomDepthHeightMeasure.MeasureValue	(numeric)
#' TADA.ActivityBottomDepthHeightMeasure.MeasureValueDataTypes.Flag	(character)
#' TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode (character)
#' TADA.LatitudeMeasure	(numeric)
#' TADA.LongitudeMeasure (numeric)
#' TADA.MonitoringLocationIdentifier (character)
#' TADA.MonitoringLocationName (character)
#' TADA.MonitoringLocationTypeName (character)
#'
#' Please note that the number of TADA-specific depth columns in the returned
#' dataframe depends upon the number of depth columns with one or more results
#' populated with a numeric value. If all depth columns contain only NA's, no
#' conversion is necessary and no TADA depth columns are created.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Find web service URLs for each Profile using WQP User Interface:
#' # https://www.waterqualitydata.us/
#' 
#' # Example WQP URL: 
#' https://www.waterqualitydata.us/#statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET
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

  # check to make sure columns do not already exist and capitalize fields with known synonyms that
  # only differ in caps
  print("TADA_Autoclean: creating TADA-specific columns.")

  if ("TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original DetectionQuantitationLimitMeasure.MeasureUnitCode
    .data$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode <-
      toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  }

  if ("TADA.ActivityMediaName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original ActivityMediaName
    .data$TADA.ActivityMediaName <- toupper(.data$ActivityMediaName)
  }

  if ("TADA.CharacteristicName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original CharacteristicName
    .data$TADA.CharacteristicName <- toupper(.data$CharacteristicName)
  }

  if ("TADA.ResultSampleFractionText" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original ResultSampleFractionText
    .data$TADA.ResultSampleFractionText <- toupper(.data$ResultSampleFractionText)
  }

  if ("TADA.MethodSpeciationName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original MethodSpeciationName
    .data$TADA.MethodSpeciationName <- toupper(.data$MethodSpeciationName)
  }

  if ("TADA.ResultMeasure.MeasureUnitCode" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original ResultMeasure.MeasureUnitCode
    .data$TADA.ResultMeasure.MeasureUnitCode <- toupper(.data$ResultMeasure.MeasureUnitCode)
  }
  
  if ("TADA.MonitoringLocationIdentifier" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original MonitoringLocationIdentifier
    .data$TADA.MonitoringLocationIdentifier <- toupper(.data$MonitoringLocationIdentifier)
  }
  
  if ("TADA.MonitoringLocationName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original MonitoringLocationName
    .data$TADA.MonitoringLocationName <- toupper(.data$MonitoringLocationName)
  }
  
  if ("TADA.MonitoringLocationTypeName" %in% colnames(.data)) {
    .data <- .data
  } else {
    # create uppercase version of original MonitoringLocationName
    .data$TADA.MonitoringLocationTypeName <- toupper(.data$MonitoringLocationTypeName)
  }

  # Transform "Dissolved oxygen (DO)" characteristic name to "DISSOLVED OXYGEN SATURATION" IF
  # result unit is "%" or "% SATURATN".
  
  if(any(.data$CharacteristicName == "Dissolved oxygen (DO)")) {

  print("TADA_Autoclean: harmonizing dissolved oxygen characterisic name to DISSOLVED OXYGEN SATURATION if unit is % or % SATURATN.")

  do.units <- c("%", "% SATURATN")

  do.data <- .data %>%
    dplyr::filter((CharacteristicName == "Dissolved oxygen (DO)") & ResultMeasure.MeasureUnitCode %in% do.units) %>%
    dplyr::mutate(
      TADA.CharacteristicName = "DISSOLVED OXYGEN SATURATION",
      TADA.ResultMeasure.MeasureUnitCode = "%"
    )

  do.list <- do.data %>%
    dplyr::select(ResultIdentifier) %>%
    dplyr::pull()

  other.data <- .data %>%
    dplyr::filter(!ResultIdentifier %in% do.list)

  do.full.join <- colnames(.data)

  .data <- do.data %>%
    dplyr::full_join(other.data, by = do.full.join) %>%
    dplyr::arrange(ResultIdentifier)

  rm(do.units, do.list, do.data, other.data, do.full.join)
  }

  # Remove complex biological data. Un-comment after new WQX 3.0 Profiles are released. May not be needed if implemented via WQP UI/services.
  # .data$TADA.BiologicalIntentName = toupper(.data$BiologicalIntentName)
  # TADAProfile = dplyr::filter(TADAProfile, TADA.BiologicalIntentName != "TISSUE" | "TOXICITY" | is.na(TADA.BiologicalIntentName) == TRUE)

  # run TADA_ConvertSpecialChars function
  print("TADA_Autoclean: handling special characters and coverting TADA.ResultMeasureValue and TADA.DetectionQuantitationLimitMeasure.MeasureValue value fields to numeric.")
  .data <- TADA_ConvertSpecialChars(.data, "ResultMeasureValue")
  .data <- TADA_ConvertSpecialChars(.data, "DetectionQuantitationLimitMeasure.MeasureValue")

  # include only in TADA_SimpleCensoredMethods
  # # Identify detection limit data
  # print("TADA_Autoclean: identifying and copying detection limit data to result value if blank.")
  # .data <- TADA_IDCensoredData(.data)

  # change latitude and longitude measures to class numeric
  print("TADA_Autoclean: converting TADA.LatitudeMeasure and TADA.LongitudeMeasure fields to numeric.")
  .data$TADA.LatitudeMeasure <- as.numeric(.data$LatitudeMeasure)
  .data$TADA.LongitudeMeasure <- as.numeric(.data$LongitudeMeasure)

  # update data types (only needed if TADA_JoinWQPProfiles is used to download data). commenting out for now (these data type conversions should happen on TADA versions of these columns? review what USGS DR does)
  # join2$ActivityStartDate <- as.Date(join2$ActivityStartDate)
  # join2$ActivityEndDate <- as.Date(join2$ActivityEndDate)
  # join2$ActivityDepthHeightMeasure.MeasureValue <- as.double(join2$ActivityDepthHeightMeasure.MeasureValue)
  # join2$ResultDepthHeightMeasure.MeasureValue <- as.double(join2$ResultDepthHeightMeasure.MeasureValue)
  # join2$AnalysisStartDate <- as.Date(join2$AnalysisStartDate)
  # join2$timeZoneStart <- as.double(join2$timeZoneStart)
  # join2$timeZoneEnd <- as.double(join2$timeZoneEnd)
  # #conversion to UTC should happen on the TADA versions of these columns, ActivityStartTime.TimeZoneCode and ActivityEndTime.TimeZoneCode would also need to be edited
  # join2$ActivityStartDateTime <- as.Date.POSIXct(join2$ActivityStartDateTime, tz = "UTC")
  # join2$ActivityEndDateTime <- as.Date.POSIXct(join2$ActivityEndDateTime, tz = "UTC")
  # join2$DrainageAreaMeasure.MeasureValue <- as.double(join2$DrainageAreaMeasure.MeasureValue)
  # join2$ContributingDrainageAreaMeasure.MeasureValue <- as.double(join2$ContributingDrainageAreaMeasure.MeasureValue)
  # join2$VerticalMeasure.MeasureValue <- as.double(join2$VerticalMeasure.MeasureValue)
  # join2$VerticalAccuracyMeasure.MeasureValue <- as.double(join2$VerticalAccuracyMeasure.MeasureValue)
  # join2$WellDepthMeasure.MeasureValue <- as.double(join2$WellDepthMeasure.MeasureValue)
  # join2$WellHoleDepthMeasure.MeasureValue <- as.double(join2$WellHoleDepthMeasure.MeasureValue)

  # Automatically convert USGS only unit "meters" to "m"
  print("TADA_Autoclean: harmonizing synonymous unit names (m and meters) to m.")
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
  .data <- suppressWarnings(TADA_ConvertResultUnits(.data, transform = TRUE, ref = "tada"))
  .data <- suppressWarnings(TADA_ConvertDepthUnits(.data, unit = "m"))

  # create comparable data identifier column
  print("TADA_Autoclean: creating TADA.ComparableDataIdentifier field for use when generating visualizations and analyses.")
  .data <- TADA_CreateComparableID(.data)

  print("NOTE: This version of the TADA package is designed to work with numeric data with media name: 'WATER'. TADA_AutoClean does not currently remove (filter) data with non-water media types. If desired, the user must make this specification on their own outside of package functions. Example: dplyr::filter(.data, TADA.ActivityMediaName == 'WATER')")

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
    stop("The dataframe does not contain the required fields. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
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
#' @param percent.ave Boolean argument; default is percent.ave = TRUE. When clean = TRUE,
#' any percent range values will be averaged. When percent.ave = FALSE, percent range
#' values are not averaged, but are flagged.
#'
#' @return Returns the original dataframe with two new columns: the input column
#' with the prefix "TADA.", which holds the numeric form of the original column,
#' and "TADA.COLUMN NAME DataTypes.Flag", which has text describing the type of data
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
#'
#' HandleSpecialChars_DetLimMeasureValue <- TADA_ConvertSpecialChars(Data_Nutrients_UT, "TADA.DetectionQuantitationLimitMeasure.MeasureValue")
#' unique(HandleSpecialChars_DetLimMeasureValue$TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag)
#'
TADA_ConvertSpecialChars <- function(.data, col, percent.ave = TRUE) {
  if (!col %in% names(.data)) {
    stop("Suspect column name specified for input dataset.")
  }

  # Define new column names
  numcol <- paste0("TADA.", col)
  flagcol <- paste0("TADA.", col, "DataTypes.Flag")

  # Create dummy columns for easy handling in function
  chars.data <- .data
  names(chars.data)[names(chars.data) == col] <- "orig"
  chars.data <- chars.data %>%
    dplyr::select(-tidyselect::any_of(c(col, numcol, flagcol)))
  chars.data$masked <- chars.data$orig

  # Add percentage character to dissolved oxygen saturation ResultMeasureValue
  # so percentage and percentage - range averaged can be identified correctly
  if (col == "ResultMeasureValue") {
    do.units <- c("%", "% SATURATN")

    chars.data$masked <- ifelse(chars.data$CharacteristicName == "Dissolved oxygen (DO)" & chars.data$ResultMeasure.MeasureUnitCode %in% do.units,
      paste(chars.data$masked, "%"), chars.data$masked
    )

    # updates percentage units where NA
    chars.data$TADA.ResultMeasure.MeasureUnitCode <- ifelse(
      grepl("%", chars.data$masked), "%", chars.data$ResultMeasure.MeasureUnitCode
    )

    # TADA.ResultMeasure.MeasureUnitCode to uppercase
    chars.data$TADA.ResultMeasure.MeasureUnitCode <- toupper(chars.data$TADA.ResultMeasure.MeasureUnitCode)
  }

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
      dplyr::mutate(
        flag = dplyr::case_when(
          is.na(masked) ~ as.character("NA - Not Available"),
          (!is.na(suppressWarnings(as.numeric(masked)) == TRUE)) ~ as.character("Numeric"),
          (grepl("<", masked) == TRUE) ~ as.character("Less Than"),
          (grepl(">", masked) == TRUE) ~ as.character("Greater Than"),
          (grepl("~", masked) == TRUE) ~ as.character("Approximate Value"),
          (grepl("[A-Za-z]", masked) == TRUE) ~ as.character("Text"),
          (grepl("%", masked) == TRUE) ~ as.character("Percentage"),
          (grepl(",", masked) == TRUE) ~ as.character("Comma-Separated Numeric"),
          (grepl("\\d\\-\\d", masked) == TRUE) ~ as.character("Numeric Range - Averaged"),
          (grepl("([1-9]|[1-9][0-9]|100)-([1-9]|[1-9][0-9]|100)%", masked) == TRUE) ~ as.character("Percentage Range - Averaged"),
          # because * is a special character you have to escape\\ it:
          (grepl("\\*", masked) == TRUE) ~ as.character("Approximate Value"),
          (!stringi::stri_enc_mark(masked) %in% c("ASCII")) ~ as.character("Non-ASCII Character(s)"),
          TRUE ~ "Coerced to NA"
        ),
        flag = ifelse(flag == "Greater Than" & grepl("%", masked) & grepl("-", masked),
          "Percentage Range - Averaged", flag
        ),
        flag = ifelse(flag == "Less Than" & grepl("%", masked) & grepl("-", masked),
          "Percentage Range - Averaged", flag
        )
      )
  }

  if (percent.ave == FALSE) {
    num.range.filter <- c("Numeric Range - Averaged")
  }

  if (percent.ave == TRUE) {
    num.range.filter <- c("Numeric Range - Averaged", "Percentage Range - Averaged")
  }

  # Result Values that are numeric ranges with the format #-# are converted to an average of the two numbers expressed in the range.
  if (any(clean.data$flag %in% num.range.filter)) {
    numrange <- subset(clean.data, clean.data$flag %in% num.range.filter)
    notnumrange <- subset(clean.data, !clean.data$flag %in% num.range.filter)
    numrange <- numrange %>%
      dplyr::mutate(
        masked = stringr::str_remove(masked, "[1-9]\\)"),
        masked = stringr::str_remove(masked, "%"),
        masked = stringr::str_remove(masked, ">"),
        masked = stringr::str_remove(masked, "<")
      ) %>%
      tidyr::separate(masked, into = c("num1", "num2"), sep = "-", remove = TRUE) %>%
      dplyr::mutate_at(c("num1", "num2"), as.numeric)
    numrange$masked <- as.character(rowMeans(numrange[, c("num1", "num2")], na.rm = TRUE))
    numrange <- numrange[, !names(numrange) %in% c("num1", "num2")] %>%
      dplyr::mutate(masked = ifelse(flag == "Percentage Range - Average", paste(masked, "%", sep = ""), masked))

    clean.data <- plyr::rbind.fill(notnumrange, numrange)
  }

  # In the new TADA column, convert to numeric and remove some specific special
  # characters.
  clean.data$masked <- suppressWarnings(as.numeric(stringr::str_replace_all(
    clean.data$masked, c("<" = "", ">" = "", "~" = "", "%" = "", "\\*" = "", "1\\)" = "")
  )))

  # this updates the DataTypes.Flag to "NA - Not Available" if NA
  clean.data$flag <- ifelse(
    is.na(clean.data$flag),
    "NA - Not Available",
    clean.data$flag
  )

  # remove columns to be replaced
  clean.data <- clean.data %>%
    dplyr::select(!(tidyselect::any_of(numcol)), !(tidyselect::any_of(flagcol)))

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
#' deprecated (i.e. retired and/or suspect) Characteristic Names with the new
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

  # read in characteristic reference table with deprecation information, filter to deprecated terms and for "retired" in CharacteristicName.
  # remove all characters after first "*" in CharacteristicName and remove any leading or trailing white space to make compatible with deprecated NWIS CharacteristicName.
  nwis.table <- utils::read.csv(system.file("extdata", "WQXCharacteristicRef.csv", package = "EPATADA")) %>%
    dplyr::filter(
      Char_Flag == "Deprecated",
      grepl("retired", CharacteristicName)
    ) %>%
    dplyr::mutate(CharacteristicName = trimws(stringr::str_split(CharacteristicName, "\\*", simplify = T)[, 1]))

  # read in characteristic reference table with deprecation information and filter to deprecated terms.
  # join with deprecated NWIS CharacteristicName data.frame.
  ref.table <- utils::read.csv(system.file("extdata", "WQXCharacteristicRef.csv", package = "EPATADA")) %>%
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

#' Convert a delimited string to the format used by WQX 3.0 profiles for one-to-manys
#'
#' This utility function takes a delimited string of entities, and a delimiter (which defaults to a comma)
#' and returns a new string in the WQX 3.0 format of c("StringA","StringB")
#'
#' @param delimited_string Character argument. Should be a string delimited by the character 
#'    passed in the delimiter parameter.
#'    
#' @param delimiter Character argument The character used to delimit the string passed in 
#'    delimited_string. Defaults to a comma.
#'    
#' @return String.
#'
#' @export
#'
TADA_FormatDelimitedString <- function(delimited_string, delimiter = ",") {
  esc_chars = c("|", "^", "&", ".", "!", "?", "\\", "*", "-", "+", ">", "<")
  if (delimiter %in% esc_chars) {
    delimiter <- paste0("\\", delimiter)
  }
  return(paste0('["', gsub(delimiter, '","', delimited_string), '"]'))
}


#' Identify and group nearby monitoring locations (UNDER ACTIVE DEVELOPMENT)
#'
#' This function takes a TADA dataset and creates a distance matrix for all
#' sites in the dataset. It then uses the buffer input value to determine which
#' sites are within the buffer and should be identified as part of a nearby site
#' group.
#'
#' @param .data TADA dataframe OR TADA sites dataframe
#' 
#' @param dist_buffer Numeric. The maximum distance (in meters) two sites can be
#'   from one another to be considered "nearby" and grouped together.
#'   
#'@param org_hierarchy Vector of organization identifiers that acts as the order in which the 
#'   function should select representative metadata for grouped sites based on the organization
#'   that collected the data. If left blank, the function does not factor organization in to the
#'   metadata selection process. When a vector is provided, the metadata will first be selected by
#'   organization and the "meta_select" argument will only be applied in cases where more than
#'   one set of metadata per site grouping are available from the highest ranking organization
#'   available.
#'    
#' @param meta_select Character argument to determine how metadata should be selected if no 
#'    org_hierarchy is specified or if multiple options for metadata from the same organization 
#'    exist. Options are "oldest", which selects the metadata associated with the oldest result from
#'    the grouped nearby sites, "newest", which selects the metadata associated with the newest
#'    result from the grouped nearby sites, "count" which selects the metadata associated with the
#'    greatest number of results, and "random" which selects random metadata from the site group.
#'    The default is meta_select = "random".
#'
#' @return Input dataframe with a TADA.MonitoringLocationIdentifier column that indicates
#'   the nearby site groups each monitoring location belongs to. Related metadata, including
#'   TADA.MonitoringLocationName, TADA.LatitudeMeasure, TADA.LongitudeMeasure, and 
#'   TADA.MonitoringLocationTypeName are added to the input df.
#'   
#' @param dist_buffer Numeric. The maximum distance (in meters) two sites can 
#' be from one another to be considered "nearby" and grouped together.
#' The default is 100m. 
#'
#' @return Input dataframe with a TADA.MonitoringLocationIdentifier column that 
#' indicates the nearby site groups each monitoring location belongs to. Grouped 
#' sites are concatenated in the TADA.MonitoringLocationIdentifier column 
#' (e.g. "USGS-10010025","USGS-10010026" enclosed in square brackets []). 
#' This JSON array is the new TADA monitoring location ID for the grouped sites.
#' TADA.MonitoringLocationIdentifier can be leveraged to analyze data from 
#' nearby sites together (as the same general location).
#'
#' @export
#' 
#' @examples
#' GroupNearbySites_100m <- TADA_FindNearbySites(Data_Nutrients_UT)
#' GroupNearbySites_10m <- TADA_FindNearbySites(Data_Nutrients_UT, dist_buffer = 10)
#' 
#'
# TADA_FindNearbySites <- function(.data, dist_buffer = 100) {
#   # check .data is data.frame
#   TADA_CheckType(.data, "data.frame", "Input object")
#   
#   # .data required columns
#   required_cols <- c("MonitoringLocationIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure")
#   # check .data has required columns
#   TADA_CheckColumns(.data, required_cols)
#   
#   # create spatial dataset based on sites
#   data_sf <- .data %>%
#     dplyr::select("MonitoringLocationIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure") %>%
#     unique()
#   
#   # convert to sf object
#   data_sf <- sf::st_as_sf(data_sf,
#                           coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
#                           # Change to your CRS
#                           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   )
#   # create a distance matrix in meters
#   dist.mat <- data.frame(sf::st_distance(data_sf)) # Great Circle distance since in lat/lon
#   
#   row.names(dist.mat) <- data_sf$MonitoringLocationIdentifier
#   colnames(dist.mat) <- data_sf$MonitoringLocationIdentifier
#   
#   # convert distances to those within buffer (1) and beyond buffer (0)
#   dist.mat1 <- apply(dist.mat, c(1, 2), function(x) {
#     if (x <= dist_buffer) {
#       x <- 1
#     } else {
#       x <- 0
#     }
#   })
#   
#   # create empty dataframe for groups
#   groups <- data.frame()
#   
#   # loop through distance matrix and extract site groups that are within the buffer distance from one another
#   for (i in 1:dim(dist.mat1)[1]) {
#     fsite <- rownames(dist.mat1)[i] # focal site
#     dat <- data.frame(Count = dist.mat1[i, ]) # get focal site count row as a column
#     dat$MonitoringLocationIdentifier <- colnames(dist.mat1) # give df site names along with counts
#     sites <- dat$MonitoringLocationIdentifier[dat$Count == 1] # filter to sites within buffer
#     sites1 <- sites[!sites %in% fsite] # get site list within buffer that does not include focal site
#     if (length(sites1) > 0) { # if this list is greater than 0, combine sites within buffer into data frame
#       df <- data.frame(MonitoringLocationIdentifier = sites, TADA.MonitoringLocationIdentifier = paste0(sites, collapse = ","))
#       df[c("TADA.MonitoringLocationIdentifier")] <- lapply(df[c("TADA.MonitoringLocationIdentifier")], TADA_FormatDelimitedString)
#       groups <- plyr::rbind.fill(groups, df)
#     }
#   }
#   
#   # get unique groups (since represented multiple times for each site looped through, above)
#   groups <- unique(groups)
#   
#   if (dim(groups)[1] > 0) {
#     # create group id numbers
#     group_ids <- groups %>% 
#       dplyr::group_by(TADA.MonitoringLocationIdentifier) %>%
#       dplyr::mutate(TADA.SiteGroup = dplyr::cur_group_id()) %>%
#       dplyr::ungroup() %>%
#       dplyr::group_by(MonitoringLocationIdentifier) %>%
#       dplyr::mutate(TADA.MonitoringLocationIdentifier = paste(TADA.MonitoringLocationIdentifier, collapse = ","),
#                     TADA.SiteGroup = paste(TADA.SiteGroup, collapse = ",")) %>%
#       dplyr::distinct() %>%
#       dplyr::ungroup()
#     
#     # find any sites within multiple groups
#     summ_sites <- group_ids %>%
#       dplyr::group_by(MonitoringLocationIdentifier) %>%
#       dplyr::mutate(GroupCount = 1:length(MonitoringLocationIdentifier))
#     
#     # pivot wider if a site belongs to multiple groups
#     groups_prep <- merge(group_ids, summ_sites, all.x = TRUE)
#     groups_wide <- tidyr::pivot_wider(groups_prep, id_cols = "MonitoringLocationIdentifier", names_from = "GroupCount", names_prefix = "TADA.MonitoringLocationIdentifier", values_from = "TADA.MonitoringLocationIdentifier")
#     ids_wide <- tidyr::pivot_wider(groups_prep, id_cols = "MonitoringLocationIdentifier", names_from = "GroupCount", names_prefix = "TADA.SiteGroup", values_from = "TADA.SiteGroup")
#     # merge data to site groupings
#     .data <- merge(.data, groups_wide, all.x = TRUE)
#     .data <- merge(.data, ids_wide, all.x = TRUE)
#     
#     # concatenate and move site id cols to right place
#     grpcols <- names(.data)[grepl("TADA.MonitoringLocationIdentifier", names(.data))]
#     idcols <- names (.data)[grepl("TADA.SiteGroup", names(.data))]
#     
#     .data <- .data %>% 
#       tidyr::unite(col = TADA.MonitoringLocationIdentifier.New, dplyr::all_of(grpcols), sep = ", ", na.rm = TRUE) %>%
#       dplyr::mutate(TADA.MonitoringLocationIdentifier = ifelse(!is.na(TADA.MonitoringLocationIdentifier.New),
#                                                                TADA.MonitoringLocationIdentifier.New, TADA.MonitoringLocationIdentifier))
#       
#       .data <- .data %>% 
#         tidyr::unite(col = TADA.SiteGroup, dplyr::all_of(idcols), sep = ", ", na.rm = TRUE) %>%
#         dplyr::mutate(TADA.SiteGroup = ifelse(TADA.SiteGroup == "", "No nearby sites", TADA.SiteGroup))
#   }
#   
#   if (dim(groups)[1] == 0) { # #if no groups, give a TADA.MonitoringLocationIdentifier column filled with NA
#     print("No nearby sites detected using input buffer distance.")
#   }
#   
#   # order columns
#   if ("ResultIdentifier" %in% names(.data)) {
#     .data <- TADA_OrderCols(.data)
#   }
#   
#   return(.data)
# }
# 
#   # select and assign metadata randomly for grouped sites when meta_select equals "random"
# 
#   if(meta_select == "random") {
# 
#     select_meta <- grouped_sites %>%
#       dplyr::select(TADA.MonitoringLocationIdentifier.New1, TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure, TADA.LongitudeMeasure, TADA.MonitoringLocationTypeName) %>%
#       dplyr::distinct() %>%
#       dplyr::group_by(TADA.MonitoringLocationIdentifier.New1) %>%
#       dplyr::slice_sample(n = 1) %>%
#       dplyr::rename(TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
#                     TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
#                     TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName) %>%
#       dplyr::mutate(TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected randomly")
# 
#   }
# 
#   if(meta_select == "oldest") {
# 
#     select_meta <- grouped_sites %>%
#       dplyr::select(TADA.MonitoringLocationIdentifier.New1, TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure, TADA.LongitudeMeasure, TADA.MonitoringLocationTypeName,
#                     ActivityStartDate) %>%
#       dplyr::distinct() %>%
#       dplyr::group_by(TADA.MonitoringLocationIdentifier.New1) %>%
#       dplyr::slice_min(ActivityStartDate) %>%
#       dplyr::slice_sample(n = 1) %>%
#       dplyr::rename(TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
#                     TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
#                     TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName) %>%
#       dplyr::mutate(TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected from the oldest result available.")
# 
#   }
# 
#   if(meta_select == "newest") {
# 
#     select_meta <- grouped_sites %>%
#       dplyr::select(TADA.MonitoringLocationIdentifier.New1, TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure, TADA.LongitudeMeasure, TADA.MonitoringLocationTypeName,
#                     ActivityStartDate) %>%
#       dplyr::distinct() %>%
#       dplyr::group_by(TADA.MonitoringLocationIdentifier.New1) %>%
#       dplyr::slice_max(ActivityStartDate) %>%
#       dplyr::slice_sample(n = 1) %>%
#       dplyr::rename(TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
#                     TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
#                     TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName) %>%
#       dplyr::mutate(TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected from the newest result available.")
# 
#   }
# 
#   if(meta_select == "count") {
# 
#    select_meta <- grouped_sites %>%
#      dplyr::group_by(TADA.MonitoringLocationIdentifier.New1) %>%
#      dplyr::mutate(NCount = length(TADA.ResultMeasureValue)) %>%
#       dplyr::select(TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure, TADA.LongitudeMeasure, TADA.MonitoringLocationTypeName,
#                     NCount) %>%
#       dplyr::distinct() %>%
#       dplyr::group_by(TADA.MonitoringLocationIdentifier) %>%
#       dplyr::slice_max(NCount) %>%
#       dplyr::slice_sample(n = 1) %>%
#       dplyr::rename(TADA.MonitoringLocationName.New = TADA.MonitoringLocationName,
#                     TADA.LatitudeMeasure.New = TADA.LatitudeMeasure,
#                     TADA.LongitudeMeasure.New = TADA.LongitudeMeasure,
#                     TADA.MonitoringLocationTypeName.New = TADA.MonitoringLocationTypeName) %>%
#       dplyr::mutate(TADA.NearbySites.Flag = "This monitoring location was grouped with other nearby site(s). Metadata were selected from the newest result available.")
# 
#   }
# 
#   .data <- .data %>%
#     dplyr::full_join(select_meta, by = dplyr::join_by(TADA.MonitoringLocationIdentifier.New1))
# 
#   .data <- .data %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(TADA.MonitoringLocationName = ifelse(!ResultIdentifier %in% grouped_resultids,
#                                                        TADA.MonitoringLocationName, TADA.MonitoringLocationName.New),
#                   TADA.LatitudeMeasure = ifelse(!ResultIdentifier %in% grouped_resultids,
#                                                 TADA.LatitudeMeasure, TADA.LatitudeMeasure.New),
#                   TADA.LongitudeMeasure = ifelse(!ResultIdentifier %in% grouped_resultids,
#                                                         TADA.LongitudeMeasure, TADA.LongitudeMeasure.New),
#                   TADA.MonitoringLocationTypeName = ifelse(!ResultIdentifier %in% grouped_resultids,
#                                                            TADA.MonitoringLocationTypeName, TADA.MonitoringLocationTypeName.New),
#                   TADA.MonitoringLocationIdentifier = ifelse(TADA.MonitoringLocationIdentifier.New1 == "",
#                                                              TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationIdentifier.New1)) %>%
#     dplyr::select(-TADA.MonitoringLocationIdentifier.New1, -TADA.MonitoringLocationName.New,
#                   -TADA.LatitudeMeasure.New, -TADA.LongitudeMeasure.New,
#                   -TADA.MonitoringLocationTypeName.New)
#   }
# 
#   if (dim(groups)[1] == 0) {
# 
#     print("No nearby sites detected using input buffer distance.")
# 
#     .data <- .data %>%
#       dplyr::mutate(TADA.NearbySites.Flag = "No nearby sites detected using input buffer distance.")
#   }
# 
#     .data <- TADA_OrderCols(.data) %>%
#       dplyr::mutate(TADA.NearbySites.Flag = ifelse(is.na(TADA.NearbySites.Flag),
#                                                    "No nearby sites detected using input buffer distance.",
#                                                    TADA.NearbySites.Flag))
# 
#   return(.data)
# }


#' Get grouped monitoring stations that are near each other
#'
#' This function takes a TADA dataset that contains grouped nearby monitoring stations
#' and returns a unique dataset of the original MonitoringLocationIdentifier, the grouped
#' TADA.MonitoringLocationIdentifier, as well as the original and TADA-prefixed LongitudeMeasure,
#' LatitudeMeasure, MonitoringLocationName, and MonitoringLocationTypeName, filtered for only those
#' stations that have a nearby station.
#'
#' @param .data TADA dataframe 
#'
#' @return New dataframe with unique combinations of original and TADA MonitoringLocationIdentifier,
#' LongitudeMeasure, LatitudeMeasure, MonitoringLocationName, and MonitoringLocationTypeName.
#'
#' @export
#'
TADA_GetUniqueNearbySites <- function(.data) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  
  # .data required columns
  required_cols <- c("MonitoringLocationIdentifier", "TADA.MonitoringLocationIdentifier",
                     "MonitoringLocationName", "TADA.MonitoringLocationName",
                     "LongitudeMeasure", "TADA.LongitudeMeasure", 
                     "LatitudeMeasure", "TADA.LatitudeMeasure",
                     "MonitoringLocationTypeName", "TADA.MonitoringLocationTypeName",
                     "MonitoringLocationDescriptionText", "TADA.NearbySites.Flag")
  # check .data has required columns
  TADA_CheckColumns(.data, required_cols)
  
  # filter only for locations with nearby sites
  .data <- .data %>%
    dplyr::filter(!is.na(TADA.NearbySites.Flag),
                  TADA.NearbySites.Flag != "No nearby sites detected using input buffer distance.") %>%
    # retain only required columns
    dplyr::select(dplyr::all_of(required_cols)) %>%
    # retain only unique records
    dplyr::distinct()

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
#' @param autoclean Boolean (TRUE or FALSE). The default is TRUE.
#' If FALSE, the function will NOT apply the TADA_AutoClean as part of the
#' TADA_DataRetrieval. If TRUE, the function WILL apply TADA_AutoClean as part of
#' TADA_DataRetrieval.
#'
#' @return Random WQP dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- TADA_RandomTestingData(number_of_days = 1, choose_random_state = FALSE)
#' df <- TADA_RandomTestingData(number_of_days = 10, choose_random_state = TRUE)
#' df <- TADA_RandomTestingData(number_of_days = 5, choose_random_state = TRUE, autoclean = FALSE)
#' }
#'
TADA_RandomTestingData <- function(number_of_days = 1, choose_random_state = FALSE, autoclean = TRUE) {
  while (TRUE) {
    # choose a random day within the last 20 years
    twenty_yrs_ago <- Sys.Date() - 20 * 365
    random_start_date <- twenty_yrs_ago + sample(20 * 365, 1)
    # choose a random start date and add any number_of_days (set that as the end date)
    end_date <- random_start_date + number_of_days

    if (choose_random_state == TRUE) {
      load(system.file("extdata", "statecodes_df.Rdata", package = "EPATADA"))
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

    if (autoclean == TRUE) {
      dat <- TADA_DataRetrieval(
        startDate = as.character(random_start_date),
        endDate = as.character(end_date),
        statecode = state,
        applyautoclean = TRUE
      )
    }

    if (autoclean == FALSE) {
      dat <- TADA_DataRetrieval(
        startDate = as.character(random_start_date),
        endDate = as.character(end_date),
        statecode = state,
        applyautoclean = FALSE
      )
    }

    if (nrow(dat) > 0) {
      return(dat)
    }
  }
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
#' # Select maximum value per day, site, comparable data identifier, result detection condition,
#' # and activity type code. Clean all non-maximum measurements from grouped data.
#' Data_6Tribes_5y_agg <- TADA_AggregateMeasurements(Data_6Tribes_5y, 
#'     grouping_cols = c("ActivityStartDate", "TADA.MonitoringLocationIdentifier", 
#'                       "TADA.ComparableDataIdentifier", "ResultDetectionConditionText",
#'                       "ActivityTypeCode"),
#'     agg_fun = "max", clean = TRUE)
#'
#' # Calculate a mean value per day, site, comparable data identifier, result detection condition,
#' # and activity type code. Keep all measurements used to calculate mean measurement.
#' Data_6Tribes_5y_agg <- TADA_AggregateMeasurements(Data_6Tribes_5y,
#'   grouping_cols = c("ActivityStartDate", "TADA.MonitoringLocationIdentifier", 
#'                   "TADA.ComparableDataIdentifier", "ResultDetectionConditionText",
#'                   "ActivityTypeCode"), 
#'   agg_fun = "mean", clean = FALSE)
TADA_AggregateMeasurements <- function(.data, grouping_cols = c("ActivityStartDate", "TADA.MonitoringLocationIdentifier", "TADA.ComparableDataIdentifier", "ResultDetectionConditionText", "ActivityTypeCode"), agg_fun = c("max", "min", "mean"), clean = TRUE) {
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
#' This is a shortcut function to run all of the most important flagging functions on a TADA
#' dataset. See ?function documentation for TADA_FlagResultUnit, TADA_FlagFraction,
#' TADA_FindQCActivities, and TADA_FlagSpeciation for more information.
#'
#' @param .data A TADA dataframe
#' @param remove_na Boolean, Determines whether to keep TADA.ResultMeasureValues that are NA.
#' Defaults to TRUE.
#' @param clean Boolean. Determines whether to keep the Suspect rows in the dataset following each
#' flagging function. Defaults to TRUE.
#'
#' @return A TADA dataframe with the following flagging columns:TADA.ResultUnit.Flag,
#' TADA.MethodSpeciation.Flag, TADA.SampleFraction.Flag, and TADA.ActivityType.Flag
#'
#' @export
#'
#' @examples
#' # Load example dataset
#' data(Data_6Tribes_5y)
#' # Run flagging functions, keeping all rows
#' Data_6Tribes_5y_ALL <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, remove_na = FALSE, clean = FALSE)
#'
#' # Run flagging functions, removing NA's and Suspect rows
#' Data_6Tribes_5y_CLEAN <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, remove_na = TRUE, clean = TRUE)
TADA_RunKeyFlagFunctions <- function(.data, remove_na = TRUE, clean = TRUE) {
  if (remove_na == TRUE) {
    .data <- .data %>% dplyr::filter(!is.na(TADA.ResultMeasureValue))
  }

  if (clean == TRUE) {
    .data <- TADA_FlagResultUnit(.data, clean = "suspect_only")
    .data <- TADA_FlagFraction(.data, clean = TRUE)
    .data <- TADA_FlagSpeciation(.data, clean = "suspect_only")
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
#' getFeatureLayer is used by writeLayer to write feature layers to local files
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
#' bbox <- sf::st_bbox(
#'   c(
#'     xmin = min(Data_Nutrients_UT$TADA.LongitudeMeasure),
#'     ymin = min(Data_Nutrients_UT$TADA.LatitudeMeasure),
#'     xmax = max(Data_Nutrients_UT$TADA.LongitudeMeasure),
#'     ymax = max(Data_Nutrients_UT$TADA.LatitudeMeasure)
#'   ),
#'   crs = sf::st_crs(Data_Nutrients_UT)
#' )
#' # Get the American Indian Reservations feature layer,
#' # filtered by the bounding box for the Data_Nutrients_UT example dataset
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


#' Download a shapefile from an API and save it to a local folder, overwriting existing file if it exists
#' writeLayer is used by TADA_UpdateTribalLayers in TADAGeospatialRefLayers.R.
#'
#' @param url URL of the layer REST service, ending with "/query". Example: https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query (American Indian Reservations)
#' @param layerfilepath Local path to save the .shp file to
#'
#' @examples
#' \dontrun{
#' # Get the Oklahoma Tribal Statistical Areas feature layer and write local file to inst/extdata/OKTribe.shp
#' OKTribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query"
#' writeLayer(OKTribeUrl, "inst/extdata/OKTribe.shp")
#' }
#'
writeLayer <- function(url, layerfilepath) {
  layer <- getFeatureLayer(url)
  # Attribute names can only be up to 10 characters long when saved to .dbf as part of sf::st_write.
  # They are truncated automatically but TOTALAREA_MI and TOTALAREA_KM will not be unique after being
  # truncated, so explicitly rename them first if they exist to avoid error.
  if ("TOTALAREA_MI" %in% colnames(layer)) {
    layer <- layer %>% dplyr::rename(
      TAREA_MI = TOTALAREA_MI,
      TAREA_KM = TOTALAREA_KM
    )
  }
  sf::st_write(layer, layerfilepath, delete_layer = TRUE)
}


#' Get a shapefile from a local folder, optionally crop it by a bounding box, and return it as a sf object
#' getLayer is used within TADA_addPolys and TADA_addPoints
#'
#' @param layerfilepath Local path to the .shp file for the layer
#' @param bbox A bounding box from the sf function st_bbox; used to filter the query results. Optional; defaults to NULL.
#' @return sf object containing the layer
#'
#'
#' @examples
#' \dontrun{
#' # Load example dataset
#' data(Data_6Tribes_5y_Harmonized)
#' # Get the bounding box of the data
#' bbox <- sf::st_bbox(c(xmin = min(Data_6Tribes_5y_Harmonized$TADA.LongitudeMeasure), ymin = min(Data_6Tribes_5y_Harmonized$TADA.LatitudeMeasure), xmax = max(Data_6Tribes_5y_Harmonized$TADA.LongitudeMeasure), ymax = max(Data_6Tribes_5y_Harmonized$TADA.LatitudeMeasure)), crs = sf::st_crs(Data_6Tribes_5y_Harmonized))
#' # Get the American Indian Reservations feature layer, filtered by the bounding box for the Data_6Tribes_5y_Harmonized example dataset
#' layerfilepath <- "extdata/AmericanIndian.shp"
#' getLayer(layerfilepath, bbox)
#' }
#'
getLayer <- function(layerfilepath, bbox = NULL) {
  layer <- sf::st_read(system.file(layerfilepath, package = "EPATADA"))
  if (!(is.null(bbox))) {
    sf::sf_use_s2(FALSE)
    layer <- sf::st_make_valid(layer)
    layer <- sf::st_crop(layer, bbox)
  }
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
#' layer <- getLayer("extdata/OKTribe.shp")
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
#' @param layerfilepath Local path to the .shp file for the layer
#' @param layergroup Name of the layer group
#' @param layername Name of the layer
#' @param bbox A bounding box from the sf function st_bbox; used to filter the query results. Optional; defaults to NULL.
#' @return The original map with polygons from the feature layer added to it.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a leaflet map
#' lmap <- leaflet::leaflet() %>%
#'   leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo") %>%
#'   leaflet::addMapPane("featurelayers", zIndex = 300)
#' # Add the American Indian Reservations feature layer to the map
#' lmap <- TADA_addPolys(lmap, "extdata/AmericanIndian.shp", "Tribes", "American Indian Reservations")
#' lmap
#' }
#'
TADA_addPolys <- function(map, layerfilepath, layergroup, layername, bbox = NULL) {
  layer <- getLayer(layerfilepath, bbox)
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
      group = layergroup,
      options = leaflet::pathOptions(pane = "featurelayers")
    )
  return(map)
}

#' Add points from an ArcGIS feature layer to a leaflet map
#'
#' @param map A leaflet map
#' @param layerfilepath Local path to the .shp file for the layer
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
#' lmap <- leaflet::leaflet() %>%
#'   leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo") %>%
#'   leaflet::addMapPane("featurelayers", zIndex = 300)
#' # Add the Virginia Federally Recognized Tribes feature layer to the map
#' lmap <- TADA_addPoints(lmap, "extdata/VATribe.shp", "Tribes", "Virginia Federally Recognized Tribes")
#' lmap
#' }
#'
TADA_addPoints <- function(map, layerfilepath, layergroup, layername, bbox = NULL) {
  layer <- getLayer(layerfilepath, bbox)
  if (is.null(layer)) {
    return(map)
  }
  lbbox <- sf::st_bbox(layer)
  if (is.na(lbbox[1])) {
    return(map)
  }
  shapes <- c(2) # open triangle; for other options see https://www.geeksforgeeks.org/r-plot-pch-symbols-different-point-shapes-available-in-r/
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
    group = layergroup,
    options = leaflet::pathOptions(pane = "featurelayers")
  )
  return(map)
}

#' Create Characteristic/MeasureUnitCode/MethodSpeciation Ref
#'
#' Creates data frame of unique combinations of TADA.CharacteristicName,
#' TADA.ResultMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode, and
#' TADA.MethodSpeciationName in a TADA data frame.
#'
#' @param .data A TADA data frame.
#'
#' @return A data frame with unique combinations of TADA.CharacteristicName,
#' TADA.ResultMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode, and
#' TADA.MethodSpeciationName
#'
#' @export
#'
#' @examples
#' UniqueCharUnitSpecExample <- TADA_UniqueCharUnitSpeciation(Data_NCTCShepherdstown_HUC12)
#'
TADA_UniqueCharUnitSpeciation <- function(.data) {
  required_cols <- c(
    "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )

  # Check to see if TADA_Autoclean has been run
  if (any(required_cols %in% colnames(.data)) == FALSE) {
    print("The dataframe does not contain the required fields. Running TADA_AutoClean to create required columns.")
    .data <- TADA_AutoClean(.data)
  }

  if (all(required_cols %in% colnames(.data)) == TRUE) {
    .data <- .data
  }

  # Create df of unique codes and characteristic names(from TADA.CharacteristicName and TADA.ResultMeasure.MeasureUnitCode) in TADA data frame
  data.units.result <- .data %>%
    dplyr::select(
      TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode,
      ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName
    ) %>%
    dplyr::filter(!is.na(TADA.ResultMeasure.MeasureUnitCode)) %>%
    dplyr::distinct()

  # Create df of unique codes and characteristic names(from TADA.CharacteristicName and TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode) in TADA data frame
  data.units.det <- .data %>%
    dplyr::select(
      TADA.CharacteristicName, TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
      DetectionQuantitationLimitMeasure.MeasureUnitCode, TADA.MethodSpeciationName
    ) %>%
     dplyr::filter(!is.na(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)) %>%
    dplyr::distinct() %>%
    dplyr::rename(
      TADA.ResultMeasure.MeasureUnitCode = TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
      ResultMeasure.MeasureUnitCode = DetectionQuantitationLimitMeasure.MeasureUnitCode
    )

  # Create combined df with all unique codes (both result and det units) and characteristic names
  data.units <- data.units.result %>%
    dplyr::full_join(data.units.det, by = c(
      "TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode",
      "ResultMeasure.MeasureUnitCode", "TADA.MethodSpeciationName"
    )) %>%
    dplyr::distinct() %>%
    dplyr::group_by(TADA.CharacteristicName) %>%
    dplyr::mutate(NCode = length(unique(TADA.ResultMeasure.MeasureUnitCode))) %>%
    dplyr::filter(!is.na(TADA.ResultMeasure.MeasureUnitCode) |
      is.na(TADA.ResultMeasure.MeasureUnitCode) & NCode == 1) %>%
    dplyr::select(-NCode)
  
  return(data.units)
}


#' Create Color Palette For Use in Graphs and Maps
#'
#' Creates a consistent color palette for use in TADA visualizations. Consistent 
#' color pairings can be utilized by setting col_pair = TRUE, in which each row 
#' consists of two values for color outlines and fills. Currently, the palette 
#' is utilizing the "Okabe-Ito" palette from base R via the palette.colors
#' function. The palette includes 9 colors by default. However, additional colors
#' can be added to the palette as needed as more complex visualization functions
#' are added to the TADA package.
#'
#' @param col_pair Boolean argument. Optional argument to define consistent color
#' pairings for outlines/fills of TADA figures defined by the row values in a dataframe.
#'
#' @return A color palette based on the "Okabe-Ito" palette, extended to 15 colors,
#'  with modifications for use in mapping and graphing functions
#'
#' @export
#'
#' @examples
#' TestColorPalette <- TADA_ColorPalette()
#' TestColorPalettePairings <- TADA_ColorPalette(col_pair = TRUE)
#' TestColorPalettePairings
#'
TADA_ColorPalette <- function(col_pair = FALSE) {
  pal <- c(
    "#000000", "#835A00", "#DC851E", "#059FA4", "#56B4E9",
    "#005258", "#A1A522", "#F0E442", "#66A281", "#1E6F98",
    "#4F5900", "#813B00", "#CD758F", "#B686A1", "#999999"
  )
  
  # Defines two color columns to be used as the color pairings in a dataframe
  col1 <- c()
  col2 <- c()
  col_combo <- data.frame()
    
  # Each row defines the pairing of colors to be used if col_pair is TRUE
  if(col_pair == TRUE){
    col1 <- c(pal[5], pal[3], pal[7], pal[14])
    col2 <- c(pal[10], pal[12], pal[11], pal[2])
    col_combo <- data.frame(col1, col2)
    pal <- col_combo
  }
  
  return(pal)
}


#' View TADA Color Palette
#'
#' View a swatch of the colors in the TADA Color palette labeled by color and
#' index number. TADA developers can reference this function when deciding which
#' colors to use in TADA visualizations. TADA users can also reference this
#' palette function to create their own visually consistent figures. TADA consistent
#' color pairings when col_pair = TRUE can be viewed in a matrix format.
#'
#' @param col_pair Boolean argument. Optional argument to view consistent color
#' pairings for outlines/fills of TADA figures defined by the row values in a dataframe.
#'
#' @return A color swatch figure based on the TADA color palette.
#'
#' @export
#'
#' @examples
#' TestViewPalette <- TADA_ViewColorPalette()
#' TestViewPalettePairing <- TADA_ViewColorPalette(col_pair = TRUE)
#'
TADA_ViewColorPalette <- function(col_pair = FALSE) {
  # call TADA color palette
  pal <- TADA_ColorPalette()

  # determine length of color palette
  n <- length(pal)

  # create list of label colors, first one needs to be white to show up clearly
  label_colors <- rep("black", n)
  label_colors[1] <- "white"

  # create color swatch graphic
  graphics::par(mar = c(1, 0, 1, 0))
  swatch <- graphics::plot(1,
    type = "n", xlab = "", ylab = "", xlim = c(0.5, n + 0.5), ylim = c(0, 1),
    main = "TADA Palette", axes = FALSE
  )
  rect(1:n - 0.5, 0, n + 0.5, 1, col = pal, border = NA)
  text(x = 1:n, y = 0.5, labels = 1:n, pos = 3, col = label_colors)
  text(x = 1:n, y = 0.5 - 0.2, labels = pal, pos = 1, col = label_colors, cex = 0.7, srt = 90)

  col_combo <- TADA_ColorPalette(col_pair = TRUE)

  if(col_pair == TRUE){
  swatch <- list()
  # Create a 2 x nrow/2 plotting matrix, can handle additional color pairings, in one view, if more are added in the future.
  graphics::par(mfrow = c(2, nrow(col_combo)/2))
  # create list of label colors for pairs
  label_colors <- rep("black", 2)

    for(i in 1:nrow(col_combo)){

      one_swatch <- graphics::plot(1,
        type = "n", xlab = "", ylab = "", xlim = c(0.5, 2.5), ylim = c(0, 1),
        main = paste0("TADA Palette Pair ", i), axes = FALSE
      )
      rect(1:2 - 0.5, 0, 2 + 0.5, 1, col = as.character(col_combo[i,]), border = NA)
      #text(x = 1:2, y = 0.5 - 0.2, labels = 1:2, pos = 3, col = label_colors, cex = 0.75)
      text(x = 1:2 + 0.25, y = 0.5, labels = col_combo[i,], pos = 2, col = label_colors, cex = 0.7)

      swatch[[i]] <- one_swatch
    }
  }
  
  graphics::par(mfrow=c(1,1))
  swatch <- grDevices::recordPlot()
  
  return(swatch)
}

#' Remove NAs in Strings for Figure Titles and Axis Labels
#'
#' Returns a vector of string(s) that removes common NA strings
#' found in columns such as TADA.ComparableDataIdentifier. Can also
#' accommodate handling of certain NA texts found in any general
#' character string or a vector of strings.
#'
#' This function is meant as an internal function to remove NAs
#' from figure titles and axis labels for the TADA package.
#'
#' @param char_string Character argument. Could be a single string
#' or vector of strings that contains common "NA" strings
#' (ex: "(NA", "(NA)", "_NA", etc.)
#'
#' @return A vector string that has removed NAs from its value.
#'
#' @export
#'
#' @examples
#' # Removes NAs based on each TADA.ComparableDataIdentifier found in a dataset.
#' data(Data_Nutrients_UT)
#' UT_Titles <- TADA_CharStringRemoveNA(unique(Data_Nutrients_UT$TADA.ComparableDataIdentifier))
#'
TADA_CharStringRemoveNA <- function(char_string) {
  # Checks if data type is a character string.
  if (!is.character(char_string)) {
    stop(paste0("TADA_CharStrignRemoveNA: 'char_string' argument is not a character string."))
  }

  # Converts character string to a vector.
  title_string <- as.vector(char_string)

  # Looks through each item in the vector and removes NAs from each.
  labs <- c()
  for (i in 1:length(char_string)) {
    labs[i] <- paste0(char_string[i], collapse = " ")
    labs[i] <- gsub("_NA|\\(NA|\\(NA)", "", labs[i])
    labs[i] <- gsub("_", " ", labs[i])
    labs <- as.vector(labs)
  }

  return(labs)
}
