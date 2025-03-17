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
#' # https://www.waterqualitydata.us/#statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET
#'
#' # Use TADA_ReadWQPWebServices to load the Station, Project, and Phys-Chem Result profiles
#' stationProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' physchemProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&dataProfile=resultPhysChem&providers=NWIS&providers=STEWARDS&providers=STORET")
#' projectProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Project/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#'
#' # Join all three profiles using TADA_JoinWQPProfiles
#' TADAProfile <- TADA_JoinWQPProfiles(
#'   FullPhysChem = physchemProfile,
#'   Sites = stationProfile, Projects = projectProfile
#' )
#'
#' # Run TADA_AutoClean
#' Autocleaned_TADAProfile <- TADA_AutoClean(TADAProfile)
#' }
TADA_AutoClean <- function(.data) {
  # need to specify this or throws error when trying to bind rows. Temporary fix for larger
  # issue where data structure for all columns should be specified.
  cols <- names(.data)
  .data <- .data %>% dplyr::mutate_at(cols, as.character)
  
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
  
  if ("ActivityStartDateTime" %in% colnames(.data)) {
    .data <- .data
  } else {
    # creates ActivityStartDateTime and ActivityStartTime.TimeZoneCode_offset
    # this is only needed when dataRetrieval is not used to get WQP data
    .data <- TADA_CreateDateTime(
      .data = .data,
      date_col = "ActivityStartDate",
      time_col = "ActivityStartTime.Time",
      tz_col = "ActivityStartTime.TimeZoneCode",
      tz = "UTC"
    )
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

#' Run key flagging functions
#'
#' This is a shortcut function to run all of the most important flagging 
#' functions on a TADA dataset. See ?function documentation for 
#' TADA_FlagResultUnit, TADA_FlagFraction, TADA_FindQCActivities, 
#' TADA_FlagMeasureQualifierCode, and TADA_FlagSpeciation for more information.
#'
#' @param .data A TADA dataframe.
#' @param clean Boolean. Determines whether to keep the suspect rows (or not).
#' Defaults to TRUE.
#'
#' @return A TADA dataframe with the following flagging columns: 
#' TADA.ResultUnit.Flag, TADA.MethodSpeciation.Flag, TADA.SampleFraction.Flag, 
#' TADA.MeasureQualifierCode.Flag and TADA.ActivityType.Flag.
#'
#' @export
#'
#' @examples
#' # Run flagging functions but keep all results
#' keep_all <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, clean = FALSE)
#'
#' # Run flagging functions and remove and suspect rows
#' remove_suspect <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, clean = TRUE)
TADA_RunKeyFlagFunctions <- function(.data, clean = TRUE) {
  if (clean == TRUE) {
    .data <- TADA_FlagResultUnit(.data, clean = "suspect_only")
    .data <- TADA_FlagFraction(.data, clean = TRUE)
    .data <- TADA_FlagSpeciation(.data, clean = "suspect_only")
    .data <- TADA_FindQCActivities(.data, clean = TRUE)
    .data <- TADA_FlagMeasureQualifierCode(.data, clean = TRUE)
  } else {
    .data <- TADA_FlagResultUnit(.data, clean = "none")
    .data <- TADA_FlagFraction(.data, clean = FALSE)
    .data <- TADA_FlagSpeciation(.data, clean = "none")
    .data <- TADA_FindQCActivities(.data, clean = FALSE)
    .data <- TADA_FlagMeasureQualifierCode(.data, clean = FALSE)
  }
  
  return(.data)
}
