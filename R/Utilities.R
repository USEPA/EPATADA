#' Silence Print Messages from Code Execution
#'
#' This utility function executes the provided code while suppressing any print messages.
#' It is useful for running code quietly, especially when print statements are not needed.
#'
#' @name quiet
#' @param x Expression or code block to execute silently.
#' @return The result of the executed code, with all print messages suppressed.
#'
quiet <- function(x) {
  # Redirect output to a temporary file to suppress prints
  sink(tempfile())
  # Ensure sink is terminated on exit
  on.exit(sink())
  # Execute the code and return its result invisibly
  invisible(force(x))
}

# write global variables. Gets rid of global variable NOTE in check:
utils::globalVariables(c(
  "TADA.ResultValueAboveUpperThreshold.Flag",
  "ActivityIdentifier",
  "ActivityMediaName",
  "ActivityStartDate",
  "TADA.ResultValueBelowUpperThreshold.Flag",
  "TADA.ResultValueBelowLowerThreshold.Flag",
  "CharacteristicName",
  "Conversion.Factor",
  "Count",
  "Description",
  "FieldName",
  "FieldValue",
  "MethodSpecationName",
  "MonitoringLocationIdentifier",
  "OrganizationFormalName",
  "OrganizationIdentifier",
  "ProjectDescriptionText",
  "ProjectFileUrl",
  "ProjectIdentifier",
  "ProjectMonitoringLocationWeightingUrl",
  "ProjectName",
  "QAPPApprovalAgencyName",
  "QAPPApprovedIndicator",
  "ResultDetectionConditionText",
  "ResultMeasureValue",
  "SamplingDesignTypeCode",
  "Source",
  "Status",
  "TADA.ContinuousData.Flag",
  "TADA.SuspectCoordinates.Flag",
  "TADA.PotentialDupRowIDs.Flag",
  "TADA.QAPPDocAvailable",
  "Target.Unit",
  "Type",
  "Value.Unit",
  "TADA.AnalyticalMethod.Flag",
  "TADA.MethodSpeciation.Flag",
  "TADA.ResultUnit.Flag",
  "TADA.SampleFraction.Flag",
  "YearSummarized",
  "where",
  "TADA.CharacteristicName",
  "ResultIdentifier",
  "TADA.ResultMeasureValue",
  "n_sites",
  "n_records",
  "statecodes_df",
  "STUSAB",
  "ActivityStartTime.Time",
  "numorgs",
  "dup_id",
  "LatitudeMeasure",
  "TADA.ResultMeasureValueDataTypes.Flag",
  "Name",
  "TADA.Detection_Type",
  "DetectionQuantitationLimitTypeName",
  "TADA.Limit_Type",
  "multiplier",
  "summ",
  "cf",
  "LongitudeMeasure",
  "TADA.CensoredData.Flag",
  "Censored_Count",
  "Status2",
  "ActivityTypeCode",
  "SampleCollectionEquipmentName",
  "ResultTimeBasisText",
  "StatisticalBaseCode",
  "ResultValueTypeName",
  "masked",
  "TADA.env",
  "Legend",
  "Fields",
  "desc",
  "WQXActivityType_Cached",
  "TADA.ActivityType.Flag",
  "Code",
  "ResultCount",
  "tot_n",
  "MonitoringLocationName",
  "TADA.LatitudeMeasure",
  "TADA.LongitudeMeasure",
  "median",
  "sd",
  "TADA.ComparableDataIdentifier",
  "roundRV",
  "TADA.DuplicateID",
  "maxRV",
  "within10",
  "AllGroups",
  "Domain.Value.Status",
  "Char_Flag",
  "Comparable.Name",
  "TADA.ResultMeasureValue1",
  "TADA.ResultSampleFractionText",
  "TADA.MethodSpeciationName",
  "TADA.ResultMeasure.MeasureUnitCode",
  "TADA.ActivityMediaName",
  "TADA.NutrientSummationGroup",
  "SummationName",
  "SummationRank",
  "SummationFractionNotes",
  "SummationSpeciationNotes",
  "SummationSpeciationConversionFactor",
  "SummationNote",
  "NutrientGroup",
  "Target.Speciation",
  "TADA.NearbySiteGroups",
  "numres",
  "TADA.SingleOrgDupGroupID",
  "TADA.MeasureQualifierCode.Flag",
  "TADA.MeasureQualifierCode.Def",
  "MeasureQualifierCode",
  "value",
  "Flag_Column",
  "ActivityStartDateTime",
  "TADA.MultipleOrgDupGroupID",
  "TADA.WQXVal.Flag",
  "Concat",
  "MeasureQualifierCode.Split",
  "TADA.Media.Flag",
  "ML.Media.Flag",
  "Unique.Identifier",
  "Domain",
  "Note.Recommendation",
  "Conversion.Coefficient",
  "Last.Change.Date",
  "Value",
  "Minimum",
  "Comb",
  "CombList",
  "TADA.Target.ResultMeasure.MeasureUnitCode",
  "TADA.WQXUnitConversionFactor",
  "TADA.WQXUnitConversionCoefficient",
  "TADA.Target.MethodSpeciationName",
  "flag",
  "NConvert",
  "MultUnits",
  "CharList",
  "CharUnit",
  "SingleNearbyGroup",
  "TADA.MultipleOrgDuplicate",
  "TADA.ResultSelectedMultipleOrgs",
  "Maximum",
  "OBJECTID",
  "GLOBALID",
  "assessmentunitidentifier",
  "index",
  "epsg",
  "ResultMeasure.MeasureUnitCode",
  "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode",
  "NCode",
  "ATTAINS.AssessmentUnitIdentifier",
  "ATTAINS_AU",
  "TOTALAREA_MI",
  "TOTALAREA_KM",
  "ATTAINS_AUs",
  "ARD_Category",
  "ActivityRelativeDepthName",
  "DepthsByGroup",
  "DepthsPerGroup",
  "MeanResults",
  "MonitoringLocationTypeName",
  "N",
  "SecchiConversion",
  "TADA.ActivityBottomDepthHeightMeasure.MeasureValue",
  "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
  "TADA.ActivityDepthHeightMeasure.MeasureValue",
  "TADA.CharacteristicsForDepthProfile TADA.ConsolidatedDepth",
  "TADA.ConsolidatedDepth.Bottom TADA.ConsolidatedDepth.Unit",
  "TADA.DepthCategory.Flag",
  "TADA.DepthProfileAggregation.Flag",
  "TADA.NResults",
  "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
  "TADA.ResultDepthHeightMeasure.MeasureValue",
  "YAxis.DepthUnit",
  "TADA.CharacteristicsForDepthProfile",
  "TADA.ConsolidatedDepth",
  "TADA.ConsolidatedDepth.Bottom",
  "TADA.ConsolidatedDepth.Unit",
  "col2rgb",
  "palette.colors",
  "rect",
  "rgb",
  "text",
  "CodeNoSpeciation",
  "ResultMeasure.MeasureUnitCode.Upper",
  "TADA.MonitoringLocationIdentifier",
  "StringA",
  "StringB",
  "MeasureUnitCode.match",
  "TADA.ActivityTopDepthHeightMeasure.MeasureValue",
  "group_id",
  "time_diff_lead",
  "time_diff_lag",
  "NResults",
  "missing.group",
  "TADA.PairingGroup",
  "TADA.PairingGroup.Rank",
  "timediff",
  "TADA.MonitoringLocationName",
  "TADA.MonitoringLocationTypeName",
  "ATTAINS.SubmissionId",
  "HorizontalCoordinateReferenceSystemDatumName",
  "NCount",
  "NHD.catchmentareasqkm",
  "NHD.comid",
  "NHD.nhdplusid",
  "NHD.resolution",
  "areasqkm",
  "assessmentUnitIdentifier",
  "catchmentareasqkm",
  "comid",
  "featureid",
  "geometry",
  "nhdplusid",
  "waterTypeCode",
  "TADA.NearbySiteGroup",
  "TADA.MonitoringLocationIdentifier.New",
  "TADA.NearbySites.Flag",
  "CountSites",
  "Group",
  "Matrix",
  "n_id",
  "OrgRank",
  "rank.default",
  "Site",
  "TADA.LatitudeMeasure.New",
  "TADA.LongitudeMeasure.New",
  "TADA.MonitoringLocationName.New",
  "TADA.MonitoringLocationTypeName.New",
  "df_number",
  "ASSESSMENT_UNIT_ID",
  "ATTAINS.FlagParameterName",
  "ATTAINS.FlagUseName",
  "ATTAINS.ParameterName",
  "CRITERIATYPEAQUAHUMHLTH",
  "CRITERIATYPEFRESHSALTWATER",
  "CRITERIATYPE_ACUTECHRONIC",
  "CRITERIATYPE_WATERORG",
  "CRITERION_VALUE",
  "ENTITY_ABBR",
  "EPA304A.PollutantName",
  "IncludeOrExclude",
  " MONITORING_DATA_LINK_TEXT",
  "MONITORING_DATA_LINK_TEXT.New",
  "MS_LOCATION_ID",
  "MS_ORG_ID",
  "MonitoringDataLinkText",
  "OrgIDForURL",
  "POLLUTANT_NAME",
  "ProviderName",
  "TADA.SingleOrgDup.Flag",
  "UNIT_NAME",
  "URLencode",
  "USE_CLASS_NAME_LOCATION_ETC",
  "assessment_unit_identifier",
  "monitoring_data_link_text",
  "monitoring_location_identifier",
  "monitoring_organization_identifier",
  "monitoring_stations",
  "organization_identifier",
  "organization_identifier.y",
  "parameter",
  "use_name",
  "use_name.y",
  "ATTAINS.OrganizationIdentifier",
  "ATTAINS.ParameterName.y",
  "ATTAINS.UseName",
  "ATTAINS.UseName.x",
  "ATTAINS.UseName.y",
  "Flag.ParameterInput",
  "Flag.UseInput",
  "TADA.ComparableDataIdentifier.x",
  "TADA.ComparableDataIdentifier.y",
  "organizationId",
  "organizationName",
  "organizationType",
  "parameterName",
  "PARCEL_NO",
  "TRIBE_NAME",
  "everything",
  "resultCount",
  "tribal_area",
  "txtProgressBar",
  "Date",
  "NWIS.parameter",
  "NWIS.status",
  "NWIS.value",
  "TADA.DistanceAway.Meters",
  "agency_cd begin_date",
  "parm_cd site_no",
  "site_tp_cd",
  "site_type",
  "st_drop_geometry",
  "station_nm",
  "ApplyUniqueSpatialCriteria",
  "assessmentUnitId",
  "ATTAINS.AssessmentUnitName",
  "ATTAINS.OrganizationIdentifier",
  "ATTAINS.WaterType",
  "useName",
  "waterType",
  "TADA.AssessmentUnitStatus",
  "Flag.AssessmentNote",
  "cluster",
  "count",
  "count_nu",
  "data_type",
  "data_type_cd",
  "dec_lat_va",
  "dec_long_va",
  "end_date",
  "parameter_code",
  "parameter_name_description",
  "Statistic Type Code",
  "Statistic Type Description",
  "agency_cd",
  "begin_date",
  "parm_cd",
  "site_no",
  "stat_cd",
  "stat_type",
  "grouped.sites",
  "n",
  "nearby",
  "rainbow",
  "monitoringLocationId",
  "monitoringLocationOrgId",
  "monitoringLocationDataLink",
  "ATTAINS.OrganizationName",
  "ATTAINS.WaterType",
  "ATTAINS.MonitoringDataLinkText",
  "ATTAINS.MonitoringDataLinkText.New",
  "ATTAINS.MonitoringLocationIdentifier",
  "AssessmentUnitIdentifier",
  "DetectionQuantitationLimitMeasure.MeasureUnitCode",
  "MS_DATA_LINK",
  "OLD_ATTAINS.MonitoringLocationIdentifier",
  "Shape_Area",
  "Shape_Length",
  "TADA.AURefSource",
  "TADA.NutrientSummation.Flag",
  "assessmentunitname",
  "assmnt_joinkey",
  "catchmentistribal",
  "catchmentresolution",
  "catchmentstatecode",
  "has4bplan",
  "hasalternativeplan",
  "hasprotectionplan",
  "hastmdl",
  "huc12",
  "ircategory",
  "isassessed",
  "isimpaired",
  "isthreatened",
  "objectId",
  "on303dlist",
  "organizationid",
  "organizationname",
  "orgtype",
  "overallstatus",
  "permid_joinkey",
  "region",
  "reportingCycle",
  "reportingcycle",
  "response.code",
  "return_sf",
  "state",
  "submissionid",
  "tas303d",
  "visionpriority303d",
  "waterbodyreportlink",
  "xwalk_huc12_version",
  "xwalk_method",
  "WqxV2.FieldName",
  "auid.col",
  "ml.col",
  "type.col",
  "AggregatedActivityEndDateTime",
  "AggregatedActivityStartDateTime",
  "ATTAINS.AssessmentUnitIdentifier.y",
  "ATTAINS.WaterType.y DepthCategory",
  "DurationPeriod.x",
  "DurationValue",
  "geomean_TADA.ResultMeasureValue",
  "MagnitudeUnit",
  "MagnitudeValueLower",
  "MagnitudeValueUpper",
  "n_Aggregatedsamples",
  "n_exceedance",
  "SaltFresh",
  "TADA.ParameterInSite.Flag",
  "UniqueSpatialCriteria",
  "ATTAINS.WaterType.y",
  "DepthCategory",
  "User.WaterType",
  "ATTAINS.OrganizationId",
  "MatchMessage",
  "Mismatch",
  "Ref.WaterType",
  "Alias.Type.Name",
  "CAS_NO",
  "Char_Flag.x",
  "Char_Flag.y",
  "Characteristic.Name",
  "STD_POLLUTANT_NAME",
  "name",
  "name_words",
  "percent_match_ATTAINS",
  "percent_match_WQX",
  "Characteristic",
  "WQXcharValRef",
  "CAS.Number",
  "CAS_NO",
  "CharacteristicName.x",
  "CharacteristicName.y",
  "Comparable.Name.x",
  "Comparable.Name.y",
  "POLLUTANT_NAME.x",
  "POLLUTANT_NAME.y",
  "STD_POLLUTANT_NAME.x",
  "STD_POLLUTANT_NAME.y",
  "percent_match_ATTAINS_CST",
  "percent_match_ATTAINS_WQX",
  "percent_match_CST",
  "UserRef.AssessmentUnitIdentifier",
  "Group.n",
  "Ref.TADA.Media.Flag",
  "context2",
  "CST.STD_POLLUTANT_NAME",
  "ENTITY_NAME",
  "TADA.NearbySiteGroup.New",
  "code",
  "context",
  "ATTAINS_catchments",
  "attains.imgs",
  "attains.labels",
  "icon.labels",
  "ATTAINS.ParameterName.x",
  "Ref.AssessmentUnitIdentifier"
))

# global variables for tribal feature layers used in TADA_OverviewMap in Utilities.R
AKAllotmentsUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0/query"
AKVillagesUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1/query"
AmericanIndianUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query"
OffReservationUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3/query"
OKTribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query"
VATribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5/query"

#' Calculate Decimal Places
#'
#' This function calculates the number of decimal places in a numeric value.
#' It returns the number of digits to the right of the decimal point for numeric data.
#'
#' @param x A numeric value or vector from the TADA profile.
#'
#' @return An integer representing the number of decimal places in the numeric value.
#' If the input is an integer or a numeric value with no decimal places, the function returns 0.
TADA_DecimalPlaces <- function(x) {
  # Convert the number to a character string, remove trailing zeros, and split by the decimal point
  parts <- strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]]

  # If there is a decimal part, return its length; otherwise, return 0
  if (length(parts) > 1) {
    return(nchar(parts[[2]]))
  } else {
    return(0)
  }
}


#' Check Type
#'
#' This function checks if the inputs to a function are of the expected type. It
#' is used at the beginning of TADA functions to ensure the inputs are suitable.
#'
#' @param arg An input argument to check
#' @param type Expected class of input argument
#' @param paramName Optional name for argument to use in error message
TADA_CheckType <- function(arg, type, paramName = deparse(substitute(arg))) {
  if (!inherits(arg, type)) {
    errorMessage <- sprintf("%s must be of class '%s'", paramName, type)
    stop(errorMessage)
  }
  invisible(NULL)
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
#' @return Invisible `NULL` if all expected columns are present; otherwise, an error is thrown.
TADA_CheckColumns <- function(.data, expected_cols) {
  TADA_CheckType(.data, "data.frame", "Input object") # check .data is data.frame

  if (!is.vector(expected_cols) || !is.character(expected_cols)) {
    stop("Expected columns must be a character vector.")
  }

  missing_cols <- setdiff(expected_cols, colnames(.data))

  if (length(missing_cols) > 0) {
    stop(paste(
      "The dataframe does not contain the required field(s):",
      paste(missing_cols, collapse = ", "),
      ". Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage."
    ))
  }

  invisible(NULL)
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
#' NAME DataTypes.Flag". When clean = TRUE, rows that cannot be converted to
#' numeric are removed. When clean = FALSE, no rows are removed. Default is
#' clean = FALSE. When flaggedonly = TRUE, data frame is filtered to show only
#' rows with non-numeric result values. Default is flaggedonly = FALSE.
#'
#'
#' @param .data A TADA profile object
#' @param col A character column to be converted to numeric
#' @param clean Boolean argument; removes non-numeric result values from the
#' data frame when clean = TRUE. Default is clean = FALSE.
#' @param flaggedonly Boolean argument; filters dataframe to show only
#' non-numeric result values when flaggedonly = TRUE. Default is flaggedonly
#' = FALSE.
#' @param percent.ave Boolean argument; default is percent.ave = TRUE. When
#' clean = TRUE, any percent range values will be averaged. When
#' percent.ave = FALSE, percent range values are not averaged, but are flagged.
#'
#' @return Returns the original dataframe with two new columns: the input column
#' with the prefix "TADA.", which holds the numeric form of the original column,
#' and "TADA.COLUMN NAME DataTypes.Flag", which has text describing the type of
#' data contained within the column of interest, including "Numeric",
#' "Less Than" (<), "Greater Than" (>), "Approximate Value" (~), "Text" (A-z),
#' "Percentage" (%), "Comma-Separated Numeric" (#,###), and
#' "Numeric Range - Averaged" (# - #).
#'
#' @export
#'
#' @examples
#' HandleSpecialChars_ResultMeasureValue <-
#'   TADA_ConvertSpecialChars(Data_Nutrients_UT, "ResultMeasureValue")
#' unique(HandleSpecialChars_ResultMeasureValue$
#'   TADA.ResultMeasureValueDataTypes.Flag)
#'
#' HandleSpecialChars_DetLimMeasureValue <-
#'   TADA_ConvertSpecialChars(
#'     Data_Nutrients_UT,
#'     "TADA.DetectionQuantitationLimitMeasure.MeasureValue"
#'   )
#' unique(HandleSpecialChars_DetLimMeasureValue$
#'   TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag)
TADA_ConvertSpecialChars <- function(
  .data,
  col,
  percent.ave = TRUE,
  clean = FALSE,
  flaggedonly = FALSE
) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  if (!col %in% names(.data)) {
    stop("Suspect column name specified for input dataset.")
  }

  # check that clean and flaggedonly are not both TRUE
  if (clean == TRUE & flaggedonly == TRUE) {
    stop(
      "Function not executed because clean and flaggedonly cannot both be TRUE"
    )
  }

  if (!any(grepl("TADA.", col))) {
    # Define new column names
    numcol <- paste0("TADA.", col)
    flagcol <- paste0("TADA.", col, "DataTypes.Flag")

    # Create dummy columns for easy handling in function
    chars.data <- .data
    names(chars.data)[names(chars.data) == col] <- "orig"
    chars.data <- chars.data |>
      dplyr::select(-tidyselect::any_of(c(col, numcol, flagcol)))
    chars.data$masked <- chars.data$orig

    # Add percentage character to dissolved oxygen saturation ResultMeasureValue
    # so percentage and percentage - range averaged can be identified correctly
    if (col == "ResultMeasureValue") {
      do.units <- c("%", "% SATURATN")

      chars.data$masked <- ifelse(
        chars.data$CharacteristicName == "Dissolved oxygen (DO)" &
          chars.data$ResultMeasure.MeasureUnitCode %in% do.units,
        paste(chars.data$masked, "%"),
        chars.data$masked
      )

      # updates percentage units where NA
      chars.data$TADA.ResultMeasure.MeasureUnitCode <- ifelse(
        grepl("%", chars.data$masked),
        "%",
        chars.data$ResultMeasure.MeasureUnitCode
      )

      # TADA.ResultMeasure.MeasureUnitCode to uppercase
      chars.data$TADA.ResultMeasure.MeasureUnitCode <- toupper(
        chars.data$TADA.ResultMeasure.MeasureUnitCode
      )
    }

    # If column is already numeric, just discern between NA and numeric
    if (is.numeric(chars.data$orig)) {
      clean.data <- chars.data |>
        dplyr::mutate(
          flag = dplyr::case_when(
            is.na(masked) ~ as.character("NA - Not Available"),
            TRUE ~ as.character("Numeric")
          )
        )
    } else {
      chars.data$masked <- gsub(" ", "", chars.data$masked) # get rid of white space for subsequent sorting
      # Detect special characters in column and populate new flag column with descriptor
      # of the specific type of character/data type
      clean.data <- chars.data |>
        dplyr::mutate(
          flag = dplyr::case_when(
            is.na(masked) ~ as.character("NA - Not Available"),
            (!is.na(
              suppressWarnings(as.numeric(masked)) == TRUE
            )) ~ as.character("Numeric"),
            (grepl("<", masked) == TRUE) ~ as.character("Less Than"),
            (grepl(">", masked) == TRUE) ~ as.character("Greater Than"),
            (grepl("\\+", masked) == TRUE) ~ as.character("Greater Than"),
            (grepl("~", masked) == TRUE) ~ as.character("Approximate Value"),
            (grepl("[A-Za-z]", masked) == TRUE) ~ as.character("Text"),
            (grepl("%", masked) == TRUE) ~ as.character("Percentage"),
            (grepl(",", masked) == TRUE) ~ as.character(
              "Comma-Separated Numeric"
            ),
            (grepl("\\d\\-\\d", masked) == TRUE) ~ as.character(
              "Numeric Range - Averaged"
            ),
            (grepl("([1-9]|[1-9][0-9]|100)-([1-9]|[1-9][0-9]|100)%", masked) ==
              TRUE) ~ as.character("Percentage Range - Averaged"),
            # because * is a special character you have to escape\\ it:
            (grepl("\\*", masked) == TRUE) ~ as.character("Approximate Value"),
            (!stringi::stri_enc_mark(masked) %in% c("ASCII")) ~ as.character(
              "Non-ASCII Character(s)"
            ),
            TRUE ~ "Coerced to NA"
          ),
          flag = ifelse(
            flag == "Greater Than" & grepl("%", masked) & grepl("-", masked),
            "Percentage Range - Averaged",
            flag
          ),
          flag = ifelse(
            flag == "Less Than" & grepl("%", masked) & grepl("-", masked),
            "Percentage Range - Averaged",
            flag
          )
        )
    }

    if (percent.ave == FALSE) {
      num.range.filter <- c("Numeric Range - Averaged")
    }

    if (percent.ave == TRUE) {
      num.range.filter <- c(
        "Numeric Range - Averaged",
        "Percentage Range - Averaged"
      )
    }

    # Result Values that are numeric ranges with the format #-# are converted to an average of the two numbers expressed in the range.
    if (any(clean.data$flag %in% num.range.filter)) {
      numrange <- subset(clean.data, clean.data$flag %in% num.range.filter)
      notnumrange <- subset(clean.data, !clean.data$flag %in% num.range.filter)
      numrange <- numrange |>
        dplyr::mutate(
          masked = stringr::str_remove(masked, "[1-9]\\)"),
          masked = stringr::str_remove(masked, "%"),
          masked = stringr::str_remove(masked, ">"),
          masked = stringr::str_remove(masked, "<")
        ) |>
        tidyr::separate(
          masked,
          into = c("num1", "num2"),
          sep = "-",
          remove = TRUE
        ) |>
        dplyr::mutate_at(c("num1", "num2"), as.numeric)
      numrange$masked <- as.character(rowMeans(
        numrange[, c("num1", "num2")],
        na.rm = TRUE
      ))
      numrange <- numrange[, !names(numrange) %in% c("num1", "num2")] |>
        dplyr::mutate(
          masked = ifelse(
            flag == "Percentage Range - Average",
            paste(masked, "%", sep = ""),
            masked
          )
        )

      clean.data <- plyr::rbind.fill(notnumrange, numrange)
    }

    # In the new TADA column, convert to numeric and remove some specific special
    # characters.
    clean.data$masked <- suppressWarnings(as.numeric(stringr::str_replace_all(
      clean.data$masked,
      c(
        "<" = "",
        ">" = "",
        "~" = "",
        "%" = "",
        "\\*" = "",
        "1\\)" = "",
        "\\+" = ""
      )
    )))

    # this updates the DataTypes.Flag to "NA - Not Available" if flag is NA
    clean.data$flag <- ifelse(
      is.na(clean.data$flag),
      "NA - Not Available",
      clean.data$flag
    )

    # remove columns to be replaced
    clean.data <- clean.data |>
      dplyr::select(
        !(tidyselect::any_of(numcol)),
        !(tidyselect::any_of(flagcol))
      )

    # Rename to original column name, TADA column name, and flag column name
    names(clean.data)[names(clean.data) == "orig"] <- col
    names(clean.data)[names(clean.data) == "masked"] <- numcol
    names(clean.data)[names(clean.data) == "flag"] <- flagcol

    clean.data <- TADA_OrderCols(clean.data)
  } else {
    flagcol <- paste0(col, "DataTypes.Flag")
    numcol <- col

    clean.data <- .data

    # this updates the flagcol to "NA - Not Available" if numcol is NA
    clean.data[[flagcol]] <- ifelse(
      is.na(clean.data[[numcol]]),
      "NA - Not Available",
      clean.data[[flagcol]]
    )

    # remove columns to be replaced
    clean.data <- clean.data |>
      dplyr::select(
        !(tidyselect::any_of(numcol)),
        !(tidyselect::any_of(flagcol))
      )

    # Rename to original column name, TADA column name, and flag column name
    names(clean.data)[names(clean.data) == "orig"] <- col
    names(clean.data)[names(clean.data) == "masked"] <- numcol
    names(clean.data)[names(clean.data) == "flag"] <- flagcol

    clean.data <- TADA_OrderCols(clean.data)
  }

  if (flaggedonly == FALSE) {
    if (clean == TRUE) {
      clean.data <- clean.data |>
        dplyr::filter(
          !(!!rlang::sym(flagcol)) %in%
            c(
              "NA - Not Available",
              "Text",
              "Non-ASCII Character(s)",
              "Result Value/Unit Cannot Be Estimated From Detection Limit",
              "Coerced to NA"
            )
        )

      return(clean.data)
    }

    if (clean == FALSE) {
      return(clean.data)
    }
  }

  if (flaggedonly == TRUE) {
    clean.data <- clean.data |>
      dplyr::filter(
        !!rlang::sym(flagcol) %in%
          c(
            "NA - Not Available",
            "Text",
            "Non-ASCII Character(s)",
            "Result Value/Unit Cannot Be Estimated From Detection Limit",
            "Coerced to NA"
          )
      )
  }
}

#' Substitute Preferred Characteristic Name for Deprecated Names
#'
#' This function uses the WQX Characteristic domain table to substitute
#' deprecated (i.e. retired and/or suspect) Characteristic Names with the new
#' name in the TADA.CharacteristicName column. TADA_SubstituteDeprecatedChars is
#' run within TADA_AutoClean, which runs within TADA_DataRetrieval and
#' (if autoclean = TRUE) in TADA_BigDataRetrieval. Therefore, deprecated
#' characteristic names are harmonized to the new name automatically upon data
#' retrieval. TADA_SubstituteDeprecatedChars can also be used by itself on a
#' user supplied dataset that is in the WQX/WQP format, if desired. This
#' solution works for both EPA WQX and USGS NWIS provided data.
#'
#' Enter ?TADA_GetCharacteristicRef() to review a list of all WQX
#' characteristics, the including deprecated names (Char_Flag). This can be
#' used as a crosswalk between the deprecated names (CharacteristicName) and
#' their new names (Comparable.Name).
#'
#' @param .data TADA dataframe
#' @param quiet logical; suppress messages if TRUE
#'
#' @return Input TADA dataframe with substituted characteristic names in
#' TADA.CharacteristicName column. Original columns are unchanged.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download nutrient data in MT from 2022 and set autoclean = FALSE
#' df <- TADA_DataRetrieval(
#'   startDate = "2022-01-01",
#'   endDate = "2022-12-31",
#'   characteristicType = "Nutrient",
#'   statecode = "MT",
#'   applyautoclean = FALSE, ask = FALSE
#' )
#' df2 <- TADA_SubstituteDeprecatedChars(df)
#' # in this example, "Inorganic nitrogen (nitrate and nitrite)" is a USGS NWIS
#' # characteristic that is deprecated and
#' # "Phosphate-phosphorus***retired***use Total Phosphorus, mixed forms"
#' # is a deprecated WQX name. Both are are transformed to their new names.
#' # review characteristic names before and after transformation
#' unique(df2$CharacteristicName)
#' unique(df2$TADA.CharacteristicName)
#'
#' df3 <- TADA_DataRetrieval(
#'   startDate = "2022-01-01", endDate = "2022-12-31",
#'   characteristicType = "Nutrient", statecode = "WY", applyautoclean = FALSE,
#'   ask = FALSE
#' )
#' df4 <- TADA_SubstituteDeprecatedChars(df3)
#' unique(df4$CharacteristicName)
#' unique(df4$TADA.CharacteristicName)
#' }
TADA_SubstituteDeprecatedChars <- function(.data, quiet = FALSE) {
  # Ensure required column
  TADA_CheckColumns(.data, c("CharacteristicName"))

  # Handle empty input
  if (nrow(.data) == 0) {
    if (!quiet) {
      message(
        "The entered data frame is empty. Skipping deprecated-name substitution."
      )
    }
    return(.data)
  }

  # Ensure TADA.CharacteristicName exists (initialize uppercase)
  if (!"TADA.CharacteristicName" %in% colnames(.data)) {
    .data$TADA.CharacteristicName <- toupper(.data$CharacteristicName)
  }

  # Load the characteristic domain table
  char.table <- TADA_GetCharacteristicRef()

  # NWIS-friendly variant: trim at first '*' for retired WQX names
  nwis_table <- char.table |>
    dplyr::filter(
      Char_Flag == "Deprecated",
      grepl("retired", CharacteristicName, ignore.case = TRUE)
    ) |>
    dplyr::mutate(
      CharacteristicName = trimws(stringr::str_split(
        CharacteristicName,
        "\\*",
        simplify = TRUE
      )[, 1])
    )

  # Build reference table of deprecated names; select only needed columns and de-duplicate
  ref.table <- char.table |>
    dplyr::filter(Char_Flag %in% c("Deprecated")) |> # add "Suspect" here if desired
    dplyr::bind_rows(nwis_table) |>
    dplyr::select(CharacteristicName, Char_Flag, Comparable.Name) |>
    dplyr::distinct(CharacteristicName, .keep_all = TRUE)

  # Left-join on CharacteristicName only; preserve row order
  .data <- dplyr::left_join(.data, ref.table, by = "CharacteristicName")

  # Substitute deprecated names when Comparable.Name is present and non-empty
  .data$TADA.CharacteristicName <- ifelse(
    !is.na(.data$Char_Flag) &
      !is.na(.data$Comparable.Name) &
      nzchar(trimws(.data$Comparable.Name)),
    .data$Comparable.Name,
    .data$TADA.CharacteristicName
  )

  # Enforce uppercase for all values in TADA.CharacteristicName
  .data$TADA.CharacteristicName <- toupper(.data$TADA.CharacteristicName)

  # Reporting (respect quiet)
  total_deprecated <- sum(!is.na(.data$Char_Flag))
  changed_rows <- .data |>
    dplyr::filter(
      !is.na(Char_Flag),
      !is.na(Comparable.Name),
      nzchar(trimws(Comparable.Name))
    )
  changed_n <- nrow(changed_rows)

  if (!quiet) {
    if (changed_n > 0) {
      # Unique mapping of original -> substituted (uppercase) names
      mapping_df <- changed_rows |>
        dplyr::distinct(CharacteristicName, TADA.CharacteristicName)
      mapping_pairs <- paste0(
        mapping_df$CharacteristicName,
        " -> ",
        mapping_df$TADA.CharacteristicName
      )
      msg <- paste0(
        changed_n,
        " results in your dataset had deprecated characteristic names. ",
        "These were substituted as follows: ",
        paste(mapping_pairs, collapse = "; "),
        "."
      )
      message(msg)
    } else if (total_deprecated > 0) {
      message(
        "Deprecated characteristic names were detected, but no substitutions were applied because Comparable.Name was missing or blank."
      )
    } else {
      message("No deprecated characteristic names found in dataset.")
    }
  }

  # Clean up ref columns
  .data <- dplyr::select(.data, -Char_Flag, -Comparable.Name)
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
TADA_CreateComparableID <- function(.data) {
  # check .data is data.frame and has required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)
  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  .data$TADA.ComparableDataIdentifier <- paste(
    .data$TADA.CharacteristicName,
    .data$TADA.ResultSampleFractionText,
    .data$TADA.MethodSpeciationName,
    .data$TADA.ResultMeasure.MeasureUnitCode,
    sep = "_"
  )
  return(.data)
}

#' Convert a delimited string to the format used by WQX 3.0 profiles for
#' one-to-manys
#'
#' This utility function takes a delimited string of entities, and a delimiter
#' (which defaults to a comma) and returns a new string in the WQX 3.0 format
#' of c("StringA","StringB").
#'
#' @param delimited_string Character argument. Should be a string delimited
#' by the character passed in the delimiter parameter.
#'
#' @param delimiter Character argument The character used to delimit the
#' string passed in delimited_string. Defaults to a comma.
#'
#' @return String.
#'
#' @export
TADA_FormatDelimitedString <- function(delimited_string, delimiter = ",") {
  esc_chars <- c("|", "^", "&", ".", "!", "?", "\\", "*", "-", "+", ">", "<")
  if (delimiter %in% esc_chars) {
    delimiter <- paste0("\\", delimiter)
  }
  return(paste0('["', gsub(delimiter, '","', delimited_string), '"]'))
}


#' Generate a Random Water Quality Portal (WQP) Dataset
#'
#' This function retrieves water quality data for a randomly selected period
#' within the past 20 years using `TADA_DataRetrieval`. It can be used to test
#' functions on random datasets. The function ensures that the returned dataset
#' contains at least 10 results. If the initial random dataset contains fewer
#' than 10 results, the function automatically queries another random dataset
#' until the criteria are met.
#'
#' @param number_of_days Numeric. Specifies the number of days for which data
#' will be queried. The default is 1, which queries data for a random two-day
#' period (e.g., startDate = "2015-04-21", endDate = "2015-04-22").
#' Users can increase this number to retrieve data for more days.
#'
#' @param choose_random_state Boolean (TRUE or FALSE). Default is FALSE.
#' If FALSE, the function queries all available WQP data for the specified
#' number_of_days (national query). If TRUE, the function selects a random state
#' and retrieves data only for that state.
#'
#' @param autoclean Boolean (TRUE or FALSE). Default is TRUE.
#' If TRUE, the function applies `TADA_AutoClean` as part of the `TADA_DataRetrieval`.
#' If FALSE, the function does not apply `TADA_AutoClean`.
#'
#' @param max_attempts Numeric. Specifies the maximum number of attempts to
#' retrieve data if an error occurs. Default is 3.
#'
#' @return A data frame containing a random WQP dataset with at least 10 results,
#' or an empty data frame if data retrieval fails after the specified number of attempts.
#' If a 500 Internal Server Error or any other error occurs during data retrieval,
#' the function will retry up to `max_attempts` times. If all attempts fail,
#' an empty data frame is returned, and a message is logged indicating the failure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Retrieve a random dataset for random 2-day period
#' # across the entire nation
#' random_data_national <- TADA_RandomTestingData(
#'   number_of_days = 1,
#'   choose_random_state = FALSE
#' )
#'
#' # Example 2: Retrieve a random dataset for a 3-day period within
#' # a randomly selected state
#' random_data_state <- TADA_RandomTestingData(
#'   number_of_days = 3,
#'   choose_random_state = TRUE
#' )
#'
#' # Example 3: Retrieve a random dataset for a 5-day period
#' # within a randomly selected state without auto-cleaning
#' random_data_state_no_clean <- TADA_RandomTestingData(
#'   number_of_days = 5,
#'   choose_random_state = TRUE,
#'   autoclean = FALSE
#' )
#' }
TADA_RandomTestingData <- function(
  number_of_days = 1,
  choose_random_state = FALSE,
  autoclean = TRUE,
  max_attempts = 3
) {
  # Retrieve random data
  get_random_data <- function(
    ndays = number_of_days,
    state_choice = choose_random_state,
    ac = autoclean,
    ask = FALSE
  ) {
    # Calculate a random start date within the last 20 years
    twenty_years_ago <- Sys.Date() - 20 * 365
    random_start_date <- twenty_years_ago + sample(20 * 365, 1)
    end_date <- random_start_date + ndays

    # Determine if a random state should be selected
    if (state_choice == TRUE) {
      load(system.file("extdata", "statecodes_df.Rdata", package = "EPATADA"))
      state <- sample(statecodes_df$STUSAB, 1)
    } else {
      state <- "null"
    }

    # Print the selected date range and state code
    print(list(
      startDate = as.character(random_start_date),
      endDate = as.character(end_date),
      statecode = state
    ))

    # Attempt to retrieve data, retrying if an error occurs
    attempt <- 1
    while (attempt <= max_attempts) {
      dat <- tryCatch(
        {
          TADA_DataRetrieval(
            startDate = as.character(random_start_date),
            endDate = as.character(end_date),
            statecode = state,
            applyautoclean = ac,
            ask = FALSE
          )
        },
        httr2_http_500 = function(e) {
          # Log the occurrence of a 500 error
          message("Attempt ", attempt, ": 500 Internal Server Error occurred.")
          return(NULL) # Return NULL to indicate failure
        },
        error = function(e) {
          # Log other errors
          message("Attempt ", attempt, ": An error occurred - ", e$message)
          return(NULL) # Return NULL to indicate failure
        }
      )

      # If data retrieval was successful, return the data
      if (!is.null(dat)) {
        return(dat)
      }

      # Increment attempt counter and try a new query
      attempt <- attempt + 1
      random_start_date <- twenty_years_ago + sample(20 * 365, 1)
      end_date <- random_start_date + ndays
      if (state_choice) {
        state <- sample(statecodes_df$STUSAB, 1)
      }
    }

    # If all attempts fail, return an empty data frame
    message(
      "Failed to retrieve data after ",
      max_attempts,
      " attempts due to persistent errors."
    )
    return(data.frame())
  }

  # Internal function to ensure dataset has at least 10 results
  verify_random_data <- function() {
    repeat {
      df <- get_random_data(number_of_days, choose_random_state, autoclean)
      if (!is.null(df) && nrow(df) >= 10) break
    }
    return(df)
  }

  # Retrieve and return the verified dataset
  df <- verify_random_data()
  return(df)
}


#' Get bounding box JSON
#'
#' @param bbox A bounding box from the sf function st_bbox
#' @return A string containing bounding box JSON that can be passed to an
#' ArcGIS feature layer in the Input Geometry field
#'
#' @examples
#' \dontrun{
#' # Load example dataset
#' utils::data(Data_6Tribes_5y)
#' # Get the bounding box of the data
#' bbox <- sf::st_bbox(
#'   c(
#'     xmin = min(Data_6Tribes_5y$TADA.LongitudeMeasure),
#'     ymin = min(Data_6Tribes_5y$TADA.LatitudeMeasure),
#'     xmax = max(Data_6Tribes_5y$TADA.LongitudeMeasure),
#'     ymax = max(Data_6Tribes_5y$TADA.LatitudeMeasure)
#'   ),
#'   crs = sf::st_crs(Data_6Tribes_5y)
#' )
#' # Get a string containing the JSON of the bounding box
#' getBboxJson(bbox)
#' }
getBboxJson <- function(bbox) {
  json <- paste0(
    '{"xmin":',
    bbox[1],
    ',"ymin":',
    bbox[2],
    ',"xmax":',
    bbox[3],
    ',"ymax":',
    bbox[4],
    "}"
  )
  return(json)
}

#' Create icon(s) to be used to represent points on a map feature layer
#' pchIcons is used within TADA_addPoints
#'
#' Uses the different plotting symbols available in R to create PNG files that can be used as markers on a map feature layer.
#'
#' @param pch Plot character code; either a single number or a vector of multiple numbers. Possible values available at https://www.geeksforgeeks.org/r-plot-pch-symbols-different-point-shapes-available-in-r/. Defaults to 1 (an open circle).
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
pchIcons <- function(
  pch = 1,
  width = 30,
  height = 30,
  bg = "transparent",
  col = "black",
  lwd = NULL
) {
  n <- length(pch)
  files <- character(n)
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = width, height = height, bg = bg)
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
#' utils::data(Data_Nutrients_UT)
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
getFeatureLayer <- function(url, bbox = NULL) {
  if (is.null(bbox)) {
    inputGeom <- NULL
  } else {
    inputGeom <- getBboxJson(bbox)
  }
  url <- paste0(
    url,
    "?where=1%3D1&outfields=*&returnGeometry=true&geometry=",
    inputGeom,
    "&f=geojson"
  )
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
#' # Get the Oklahoma Tribal Statistical Areas feature layer and write
#' # local file to inst/extdata/OKTribe.shp
#' OKTribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query"
#' writeLayer(OKTribeUrl, "inst/extdata/OKTribe.shp")
#' }
writeLayer <- function(url, layerfilepath) {
  layer <- getFeatureLayer(url)
  # Attribute names can only be up to 10 characters long when saved to .dbf as part of sf::st_write.
  # They are truncated automatically but TOTALAREA_MI and TOTALAREA_KM will not be unique after being
  # truncated, so explicitly rename them first if they exist to avoid error.
  if ("TOTALAREA_MI" %in% colnames(layer)) {
    layer <- layer |>
      dplyr::rename(TAREA_MI = TOTALAREA_MI, TAREA_KM = TOTALAREA_KM)
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
#' utils::data(Data_6Tribes_5y_Harmonized)
#' # Get the bounding box of the data
#' bbox <- sf::st_bbox(
#'   c(
#'     xmin = min(Data_6Tribes_5y_Harmonized$TADA.LongitudeMeasure),
#'     ymin = min(Data_6Tribes_5y_Harmonized$TADA.LatitudeMeasure),
#'     xmax = max(Data_6Tribes_5y_Harmonized$TADA.LongitudeMeasure),
#'     ymax = max(Data_6Tribes_5y_Harmonized$TADA.LatitudeMeasure)
#'   ),
#'   crs = sf::st_crs(Data_6Tribes_5y_Harmonized)
#' )
#' # Get the American Indian Reservations feature layer,
#' # filtered by the bounding box for the Data_6Tribes_5y_Harmonized
#' # example dataset
#' layerfilepath <- "extdata/AmericanIndian.shp"
#' getLayer(layerfilepath, bbox)
#' }
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
#' getTribalPopup is used within TADA_addPolys and TADA_addPoints
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
#' getTribalPopup(layer, "Oklahoma Tribal Statistical Areas")
#' }
getTribalPopup <- function(layer, layername) {
  popups <- vector("character", nrow(layer))

  # select and rename cols
  cols <- c(
    "TRIBE_N" = "Tribe",
    "STATE" = "State",
    "REGION" = "EPA Region",
    "AWATER_M" = "Water Area (sq miles)",
    "ALAND_M" = "Land Area (sq miles)",
    "TOTALAREA_M" = "Total Area (sq miles)",
    "EPA_ID" = "EPA ID"
  )

  # create popup text for each polygon
  for (j in seq_len(nrow(layer))) {
    text <- paste0("<strong>", layername, "</strong><p>")

    for (i in seq_along(cols)) {
      col_name <- names(cols[i])

      if (col_name %in% colnames(layer)) {
        value <- layer[j, col_name, drop = TRUE]

        # if col is "REGION", process the semicolon-delimited string
        if (col_name == "REGION") {
          # split the string by semicolon, get unique values, and join them back
          value <- unique(unlist(strsplit(value, ";\\s*")))
        }

        # if the col contains an area, round the value
        if (col_name %in% c("AWATER_M", "ALAND_M", "TOTALAREA_M")) {
          # round to two decimal places
          value <- round(value, digits = 2)
        }

        value_str <- paste(value, collapse = ", ")

        text <- paste0(
          text,
          "<strong>",
          cols[i],
          "</strong>: ",
          value_str,
          "<br>"
        )
      }
    }

    popups[j] <- text
  }

  return(popups)
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
#' lmap <- leaflet::leaflet() |>
#'   leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo") |>
#'   leaflet::addMapPane("featurelayers", zIndex = 300)
#' # Add the American Indian Reservations feature layer to the map
#' lmap <- TADA_addPolys(lmap, "extdata/AmericanIndian.shp", "Tribes", "American Indian Reservations")
#' lmap
#' }
TADA_addPolys <- function(
  map,
  layerfilepath,
  layergroup,
  layername,
  bbox = NULL
) {
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

  map <- leaflet::addPolygons(
    map,
    data = layer,
    color = "#A0522D",
    weight = 0.35,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 0.2,
    fillColor = ~ leaflet::colorNumeric("Oranges", layer[[areaColumn]])(layer[[
      areaColumn
    ]]),
    highlightOptions = leaflet::highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    popup = getTribalPopup(layer, layername),
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
#' lmap <- leaflet::leaflet() |>
#'   leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo") |>
#'   leaflet::addMapPane("featurelayers", zIndex = 300)
#' # Add the Virginia Federally Recognized Tribes feature layer to the map
#' lmap <- TADA_addPoints(
#'   lmap, "extdata/VATribe.shp",
#'   "Tribes", "Virginia Federally Recognized Tribes"
#' )
#' lmap
#' }
TADA_addPoints <- function(
  map,
  layerfilepath,
  layergroup,
  layername,
  bbox = NULL
) {
  layer <- getLayer(layerfilepath, bbox)
  if (is.null(layer)) {
    return(map)
  }
  lbbox <- sf::st_bbox(layer)
  if (is.na(lbbox[1])) {
    return(map)
  }
  shapes <- c(2) # open triangle; for other options see https://www.geeksforgeeks.org/r-plot-pch-symbols-different-point-shapes-available-in-r/
  iconFiles <- pchIcons(
    shapes,
    width = 20,
    height = 20,
    col = c("#CC7722"),
    lwd = 2
  )
  map <- leaflet::addMarkers(
    map,
    data = layer,
    icon = ~ leaflet::icons(
      iconUrl = iconFiles[],
      popupAnchorX = 20,
      popupAnchorY = 0
    ),
    popup = getTribalPopup(layer, layername),
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
#' UniqueCharUnitSpecExample <-
#'   TADA_UniqueCharUnitSpeciation(Data_Nutrients_UT)
TADA_UniqueCharUnitSpeciation <- function(.data) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  required_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )

  # Check to see if TADA_Autoclean has been run
  if (any(required_cols %in% colnames(.data)) == FALSE) {
    print(
      "The dataframe does not contain the required fields. Running TADA_AutoClean to create required columns."
    )
    .data <- TADA_AutoClean(.data)
  }

  # Create df of unique codes and characteristic names(from TADA.CharacteristicName and TADA.ResultMeasure.MeasureUnitCode) in TADA data frame
  data.units.result <- .data |>
    dplyr::select(
      TADA.CharacteristicName,
      TADA.ResultMeasure.MeasureUnitCode,
      ResultMeasure.MeasureUnitCode,
      TADA.MethodSpeciationName
    ) |>
    dplyr::distinct()

  # Create df of unique codes and characteristic names(from TADA.CharacteristicName and TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode) in TADA data frame
  data.units.det <- .data |>
    dplyr::select(
      TADA.CharacteristicName,
      TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
      DetectionQuantitationLimitMeasure.MeasureUnitCode,
      TADA.MethodSpeciationName
    ) |>
    dplyr::filter(
      !is.na(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
    ) |>
    dplyr::distinct() |>
    dplyr::rename(
      TADA.ResultMeasure.MeasureUnitCode = TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
      ResultMeasure.MeasureUnitCode = DetectionQuantitationLimitMeasure.MeasureUnitCode
    )

  # Create combined df with all unique codes (both result and det units) and characteristic names
  data.units <- data.units.result |>
    dplyr::full_join(
      data.units.det,
      by = c(
        "TADA.CharacteristicName",
        "TADA.ResultMeasure.MeasureUnitCode",
        "ResultMeasure.MeasureUnitCode",
        "TADA.MethodSpeciationName"
      )
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(TADA.CharacteristicName)

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
TADA_ColorPalette <- function(col_pair = FALSE) {
  pal <- c(
    "#000000",
    "#835A00",
    "#DC851E",
    "#059FA4",
    "#56B4E9",
    "#005258",
    "#A1A522",
    "#F0E442",
    "#66A281",
    "#1E6F98",
    "#4F5900",
    "#813B00",
    "#CD758F",
    "#B686A1",
    "#999999"
  )

  # Defines two color columns to be used as the color pairings in a dataframe
  col1 <- c()
  col2 <- c()
  col_combo <- data.frame()

  # Each row defines the pairing of colors to be used if col_pair is TRUE
  if (col_pair == TRUE) {
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
  swatch <- graphics::plot(
    1,
    type = "n",
    xlab = "",
    ylab = "",
    xlim = c(0.5, n + 0.5),
    ylim = c(0, 1),
    main = "TADA Palette",
    axes = FALSE
  )
  rect(1:n - 0.5, 0, n + 0.5, 1, col = pal, border = NA)
  text(x = 1:n, y = 0.5, labels = 1:n, pos = 3, col = label_colors)
  text(
    x = 1:n,
    y = 0.5 - 0.2,
    labels = pal,
    pos = 1,
    col = label_colors,
    cex = 0.7,
    srt = 90
  )

  col_combo <- TADA_ColorPalette(col_pair = TRUE)

  if (col_pair == TRUE) {
    swatch <- list()
    # Create a 2 x nrow/2 plotting matrix, can handle additional color pairings, in one view, if more are added in the future.
    graphics::par(mfrow = c(2, nrow(col_combo) / 2))
    # create list of label colors for pairs
    label_colors <- rep("black", 2)

    for (i in 1:nrow(col_combo)) {
      one_swatch <- graphics::plot(
        1,
        type = "n",
        xlab = "",
        ylab = "",
        xlim = c(0.5, 2.5),
        ylim = c(0, 1),
        main = paste0("TADA Palette Pair ", i),
        axes = FALSE
      )
      rect(
        1:2 - 0.5,
        0,
        2 + 0.5,
        1,
        col = as.character(col_combo[i, ]),
        border = NA
      )
      # text(x = 1:2, y = 0.5 - 0.2, labels = 1:2, pos = 3, col = label_colors, cex = 0.75)
      text(
        x = 1:2 + 0.25,
        y = 0.5,
        labels = col_combo[i, ],
        pos = 2,
        col = label_colors,
        cex = 0.7
      )

      swatch[[i]] <- one_swatch
    }
  }

  graphics::par(mfrow = c(1, 1))
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
#' utils::data(Data_Nutrients_UT)
#' unique(Data_Nutrients_UT$TADA.ComparableDataIdentifier)
#' UT_Titles <- TADA_CharStringRemoveNA(unique(Data_Nutrients_UT$TADA.ComparableDataIdentifier))
#' unique(UT_Titles)
TADA_CharStringRemoveNA <- function(char_string) {
  # Checks if data type is a character string.
  if (!is.character(char_string)) {
    stop(paste0(
      "TADA_CharStrignRemoveNA: 'char_string' argument is not a character string."
    ))
  }

  # Converts character string to a vector.
  title_string <- as.vector(char_string)

  # Looks through each item in the vector and removes NAs from each.
  labs <- c()
  for (i in 1:length(char_string)) {
    labs[i] <- paste0(char_string[i], collapse = " ")
    labs[i] <- gsub("_NA|\\(NA|\\(NA)", "", labs[i])
    labs[i] <- gsub("_", " ", labs[i])
    labs[i] <- gsub("\\s+", " ", labs[i])
    labs <- as.vector(labs)
  }

  return(labs)
}


#' Create downloadable table
#'
#' This function creates a data table that can be downloaded as a .csv, .xlsx or .pdf.
#'
#' @param .data A data frame
#'
#' @return A data table with multiple download options (.csv, .xlsx or .pdf).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # return ATTAINS parameter domain values
#' TADA_TableExport(rExpertQuery::EQ_DomainValues("param_name"))
#' }
TADA_TableExport <- function(.data = NULL) {
  if (is.null(.data)) {
    stop("Input object must be of class 'data.frame'")
  }

  data <- DT::datatable(
    .data,
    extensions = c("Buttons", "FixedColumns"),
    options = list(
      paging = TRUE,
      dom = "Bfrtip",
      autoWidth = TRUE,
      pageLength = 5,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      buttons = c("copy", "csv", "excel", "pdf")
      # fixedColumns = list(leftColumns = 1
    ),
    class = "display"
  ) |>
    DT::formatStyle(columns = colnames(.data), "fontSize" = "80%")

  return(data)
}


#' Create and download .csv
#'
#' This function creates a .csv file and exports it to a user's download folder
#' with the name of the data frame as the file name.
#'
#' @param .data A data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Returns a .csv file of the example Data_Nutrients_UT TADA data frame.
#' TADA_CreateCSV(Data_Nutrients_UT)
#' }
TADA_CreateCSV <- function(.data) {
  if (!is.data.frame(.data)) {
    stop("Input object must be of class 'data.frame'")
  }

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL) # Exit the function early
  }

  df_name <- deparse(substitute(.data))

  downloads_path <- file.path(
    Sys.getenv("USERPROFILE"),
    "Downloads",
    paste0(df_name, ".csv")
  )

  utils::write.csv(.data, file = downloads_path, row.names = FALSE)

  cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
}

#' TADA_RenametoLegacy
#'
#' This function renames columns in a dataframe from WQX3.0 (beta) names to WQX2.0 (legacy) names.
#'  Water Quality Portal data are retrieved using USGS dataRetrieval service = "ResultWQX3".
#'  The purpose of this function is to aid in integrating and updating TADA dependencies
#'  developed under WQX2.0 to function with data retrieved using WQX3.0 service.
#'
#'  TADA_RenametoLegacy function calls on EPA web services to read in the documented
#'  WQX3.0 schema file (schema_outbound_wqx3.0.csv).The file crosswalks WQX3.0 column names
#'  with equivalent WQX2.0 Legacy column names across profiles (e.g., PhysChem, ActivityMetric) where appropriate.
#'  The function uses data.table::setnames() to rename columns in the dataframe
#'  by reference - in this case where there are beta names, rename to legacy names, and skip where there are no matches.
#'
#'
#' @param .data A water quality monitoring dataframe retrieved using dataRetrieval::readWQPdata using WQX3.0 Beta services
#'
#' @return A water quality monitoring dataframe with WQX2.0 Legacy column names
#'
#' @export
#'
#' @examples
#' DeWitt_wqx3 <- dataRetrieval::readWQPdata(
#'   statecode = "Illinois",
#'   countycode = "DeWitt", characteristicName = "Nitrogen",
#'   service = "ResultWQX3", dataProfile = "fullPhysChem",
#'   ignore_attributes = TRUE
#' )
#'
#' DeWitt_wqx3_withlegacynames <- EPATADA::TADA_RenametoLegacy(DeWitt_wqx3)
#'
TADA_RenametoLegacy <- function(.data) {
  ## READ WQX3.0 column name schema from EPA Water Data WQP Quick Reference Guide
  # https://www.epa.gov/waterdata/water-quality-portal-quick-reference-guide
  wqxnames <- readr::read_csv(
    "https://www.epa.gov/system/files/other-files/2025-07/schema_outbound_wqx3.0.csv",
    show_col_types = FALSE
  )

  # Process schema crosswalk table to better suit TADA elements and reduce duplicate legacy elements
  wqxnames_mod <- wqxnames |>
    dplyr::mutate(
      WqxV2.FieldName = dplyr::case_when(
        # 3.0 element ~ change to in 2.0 element
        FieldName3.0 ==
          "SampleCollectionMethod_Description" ~ "SampleCollectionMethod/MethodDescriptionText",
        FieldName3.0 ==
          "DataQuality_PrecisionValue" ~ "DataQuality/PrecisionValue",
        FieldName3.0 ==
          "DataQuality_ConfidenceIntervalValue" ~ "DataQuality/ConfidenceIntervalValue",
        FieldName3.0 ==
          "DataQuality_UpperConfidenceLimitValue" ~ "DataQuality/UpperConfidenceLimitValue",
        FieldName3.0 ==
          "DataQuality_LowerConfidenceLimitValue" ~ "DataQuality/LowerConfidenceLimitValue",
        FieldName3.0 ==
          "ResultAnalyticalMethod_Description" ~ "ResultAnalyticalMethod/MethodDescriptionText",
        FieldName3.0 == "Location_Latitude" ~ "LatitudeMeasure", # Changing to what is returned in legacy Site profile
        FieldName3.0 == "Location_Longitude" ~ "LongitudeMeasure", # Changing to what is returned in legacy Site profile
        FieldName3.0 ==
          "Location_HorzCoordReferenceSystemDatum" ~ "HorizontalCoordinateReferenceSystemDatumName", # Changing to what is returned in legacy Site profile
        FieldName3.0 == "SamplePrepMethod_Description" ~ NA, # Biological profile
        FieldName3.0 == "LabSamplePrepMethod_Description" ~ NA, # Biological profile
        FieldName3.0 == "LabSamplePrepMethod_EndTime" ~ NA, # Biological profile
        FieldName3.0 == "ProjectAttachment_FileName" ~ NA, # named BinaryObjectFileName
        FieldName3.0 == "ProjectAttachment_FileType" ~ NA, # named BinaryObjectFileTypeCode
        FieldName3.0 == "ActivityAttachment_FileName" ~ NA,
        FieldName3.0 == "ActivityAttachment_FileType" ~ NA,
        FieldName3.0 == "ResultAttachment_FileName" ~ NA,
        FieldName3.0 == "ResultAttachment_FileType" ~ NA,
        TRUE ~ WqxV2.FieldName
      )
    ) |>
    # Remove rows without a legacy name in the crosswalk table
    dplyr::filter(!is.na(WqxV2.FieldName)) |>
    # Some elements in the crosswalk table have different special characters compared to
    # elements returned with dataRetrieval
    # Using stringr to identify special characters replacing "_" with "." and "/" with "."
    dplyr::mutate(
      WqxV2.FieldName = stringr::str_replace_all(
        WqxV2.FieldName,
        c("_" = ".", "/" = ".")
      )
    )

  # Make copy of original names from dataRetrieval 3.0 query bc data.table::setnames
  # will overwrite original dataframe
  df <- data.table::copy(.data)
  beta_names_dr <- names(.data) # copy of original elements

  # Create vectors of WQX3.0 and WQX2.0 (Legacy) column names
  beta_names <- wqxnames_mod$FieldName3.0
  legacy_names <- wqxnames_mod$WqxV2.FieldName

  if (length(beta_names) != length(legacy_names)) {
    stop("`old names` and `new names` must be the same length", call. = FALSE)
  }

  df <- data.table::setnames(
    df,
    old = beta_names,
    new = legacy_names,
    skip_absent = TRUE
  )

  df <- TADA_OrderCols(df)

  return(df)
}

#' checkColNames
#'
#' This function checks column names using partial string matches. It is designed
#' to facilitate the use of user-supplied refs with differently prefixed columns
#' in Module 2 and 3 functions.
#'
#' @param .data A user-supplied ref data frame containing AssessmentUnitIdentifier,
#' MonitoringLocationIdentifier, and WaterType columns. It is permitted (but not
#' required) for these columns to use ATTAINS, TADA or other prefixes.
#' @param partial.string The character string used for partial string matching when
#' checking column names.
#'
#' @return A data frame with two columns identifying the exact column names for the
#' AssessmentUnitIdentifier, MonitoringLocationIdentifier, and WaterType columns in
#' a user-supplied ref file.
#'
checkColName <- function(.data, partial.string = NULL) {
  col.id <- dplyr::case_when(
    partial.string == "AssessmentUnitIdentifier" ~ "auid.col",
    partial.string == "MonitoringLocationIdentifier" ~ "ml.col",
    partial.string == "WaterType" ~ "type.col"
  )

  if (any(stringr::str_detect(names(.data), partial.string)) != TRUE) {
    stop(paste0(
      "TADA_CreateAUMLCrosswalk: The ",
      partial.string,
      " column is missing from the user-supplied reference (au_ref)."
    ))
  }

  if (any(stringr::str_detect(names(.data), partial.string)) != FALSE) {
    select.col <- .data |>
      dplyr::select(dplyr::contains(partial.string)) |>
      names()

    if (length(select.col) > 1) {
      stop(paste0(
        "TADA_CreateAUMLCrosswalk: There cannot be more than one ",
        partial.string,
        " column in the user-supplied reference (au_ref)."
      ))
    }

    col.lab <- data.frame(col.id, select.col)

    rm(col.id, select.col)
  }
  return(col.lab)
}

#' renameATTAINSCols
#'
#' This function adds the ATTAINS prefix and changes column name capitalization to
#' match the TADA format.
#'
#' @param .data A data frame containing columns from ATTAINS geospatial web services.
#'
#' @param return_list Boolean argument. When return_list = TRUE, the function returns
#' a list of the TADA formatted names for ATTAINS columns. When return_list = FALSE,
#' the input .data data frame is updated so column names from ATTAINS geospatial web
#' services match the TADA format. Default is return_list = FALSE.
#'
#' @param format Character argument. The format the user wants to switch the column
#' names too. When format = "tada", the ATTAINS prefix and TADA capitalization will
#' be applied. When format = "attains", TADA formatted columns will be renamed to the
#' original ATTAINS names. Default = "tada".
#'
#' @return A data frame with column name from ATTAINS geospatial web service updated
#' to match the TADA format. Or when return_list = TRUE, a list of all TADA
#' formatted ATTAINS column names.
#'
renameATTAINSCols <- function(.data, return_list = FALSE, format = "tada") {
  # List of TADA formatted column names
  attains.tada <- c(
    "ATTAINS.OrganizationIdentifier",
    "ATTAINS.SubmissionId",
    "ATTAINS.HasProtectionPlan",
    "ATTAINS.AssessmentUnitName",
    "ATTAINS.NhdPlusId",
    "ATTAINS.Tas303d",
    "ATTAINS.IsThreatened",
    "ATTAINS.State",
    "ATTAINS.On303dList",
    "ATTAINS.OrganizationName",
    "ATTAINS.Region",
    "ATTAINS.ShapeLength",
    "ATTAINS.ReportingCycle",
    "ATTAINS.AssmntJoinKey",
    "ATTAINS.HasTmdl",
    "ATTAINS.OrgType",
    "ATTAINS.PermIdJoinKey",
    "ATTAINS.CatchmentIsTribal",
    "ATTAINS.IrCategory",
    "ATTAINS.WaterbodyReportLink",
    "ATTAINS.AssessmentUnitIdentifier",
    "ATTAINS.OverallStatus",
    "ATTAINS.IsAssessed",
    "ATTAINS.IsImpaired",
    "ATTAINS.Has4bPlan",
    "ATTAINS.Huc12",
    "ATTAINS.HasAlternativePlan",
    "ATTAINS.VisionPriority303d",
    "ATTAINS.AreaSqkm",
    "ATTAINS.CatchmentAreaSqkm",
    "ATTAINS.CatchmentStateCode",
    "ATTAINS.CatchmentResolution",
    "ATTAINS.ShapeArea",
    "ATTAINS.CulturalUse",
    "ATTAINS.DrinkingWaterUse",
    "ATTAINS.EcologicalUse",
    "ATTAINS.FishConsumptionUse",
    "ATTAINS.RecreationUse",
    "ATTAINS.OtherUse",
    "ATTAINS.AlgalGrowth",
    "ATTAINS.Ammonia",
    "ATTAINS.CauseUnknown",
    "ATTAINS.CauseUnknownFishKills",
    "ATTAINS.CauseUnknownImpairedBiota",
    "ATTAINS.Dioxins",
    "ATTAINS.FishConsumptionAdvisory",
    "ATTAINS.FlowAlterations",
    "ATTAINS.HabitatAlterations",
    "ATTAINS.HydrologicAlteration",
    "ATTAINS.Mercury",
    "ATTAINS.MetalsOtherThanMercury",
    "ATTAINS.NoxiousAquaticPlants",
    "ATTAINS.NuisanceExoticSpecies",
    "ATTAINS.NuisanceNativeSpecies",
    "ATTAINS.Nutrients",
    "ATTAINS.OilAndGrease",
    "ATTAINS.OxygenDepletion",
    "ATTAINS.OtherCause",
    "ATTAINS.Pathogens",
    "ATTAINS.Pesticides",
    "ATTAINS.Pfas",
    "ATTAINS.PhAcidityCausticConditions",
    "ATTAINS.PolychlorinatedBiphenylsPcbs",
    "ATTAINS.Radiation",
    "ATTAINS.SolidsChloridesSulfates",
    "ATTAINS.Sediment",
    "ATTAINS.TasteColorAndOdor",
    "ATTAINS.Temperature",
    "ATTAINS.TotalToxics",
    "ATTAINS.ToxicInorganics",
    "ATTAINS.ToxicOrganics",
    "ATTAINS.Trash",
    "ATTAINS.Turbidity",
    "ATTAINS.CycleStatus",
    "ATTAINS.OrigFid",
    "ATTAINS.XwalkMethod",
    "ATTAINS.XwalkHuc12Version",
    "ATTAINS.Chlorine",
    "ATTAINS.Biotoxins",
    "ATTAINS.WaterType"
  )

  # List of original ATTAINS column names
  attains.orig <- c(
    "organizationid",
    "submissionid",
    "hasprotectionplan",
    "assessmentunitname",
    "nhdplusid",
    "tas303d",
    "isthreatened",
    "state",
    "on303dlist",
    "organizationname",
    "region",
    "Shape_Length",
    "reportingcycle",
    "assmnt_joinkey",
    "hastmdl",
    "orgtype",
    "permid_joinkey",
    "catchmentistribal",
    "ircategory",
    "waterbodyreportlink",
    "assessmentunitidentifier",
    "overallstatus",
    "isassessed",
    "isimpaired",
    "has4bplan",
    "huc12",
    "hasalternativeplan",
    "visionpriority303d",
    "areasqkm",
    "catchmentareasqkm",
    "catchmentstatecode",
    "catchmentresolution",
    "Shape_Area",
    "cultural_use",
    "drinkingwater_use",
    "ecological_use",
    "fishconsumption_use",
    "recreation_use",
    "other_use",
    "algal_growth",
    "ammonia",
    "cause_unknown",
    "cause_unknown_fish_kills",
    "cause_unknown_impaired_biota",
    "dioxins",
    "fish_consumption_advisory",
    "flow_alterations",
    "habitat_alterations",
    "hydrologic_alteration",
    "mercury",
    "metals_other_than_mercury",
    "noxious_aquatic_plants",
    "nuisance_exotic_species",
    "nuisance_native_species",
    "nutrients",
    "oil_and_grease",
    "oxygen_depletion",
    "other_cause",
    "pathogens",
    "pesticides",
    "pfas",
    "ph_acidity_caustic_conditions",
    "polychlorinated_biphenyls_pcbs",
    "radiation",
    "solids_chlorides_sulfates",
    "sediment",
    "taste_color_and_odor",
    "temperature",
    "total_toxics",
    "toxic_inorganics",
    "toxic_organics",
    "trash",
    "turbidity",
    "cyclestatus",
    "orig_fid",
    "xwalk_method",
    "xwalk_huc12_version",
    "chlorine",
    "biotoxins",
    "waterType"
  )

  # If return_list equals TRUE, return the list of TADA formatted column names
  if (return_list == TRUE & format == "tada") {
    attains.tada <- unique(attains.tada)

    return(attains.tada)
  }

  # If return_list equals TRUE, return the list of ATTAINS formatted column names
  if (return_list == TRUE & format == "attains") {
    attains.orig <- unique(attains.orig)
    return(attains.orig)
  }

  # If return_list equals FALSE, proceed with renaming columns
  if (return_list == FALSE) {
    # Determine which water type column exists and adjust the lists accordingly
    if ("waterTypeCode" %in% names(.data)) {
      attains.orig <- gsub("waterType", "waterTypeCode", attains.orig)
    }

    # Assign old and new name vectors based on format selected by user
    old.names <- if (format == "tada") attains.orig else attains.tada
    new.names <- if (format == "tada") attains.tada else attains.orig

    # Rename columns
    data.table::setnames(
      .data,
      old = old.names,
      new = new.names,
      skip_absent = TRUE
    )

    # Remove intermediate objects
    rm(attains.tada, attains.orig, old.names, new.names)

    # Return data frame with changed column names
    return(.data)
  }
}

#' TADA_CorrectColType
#'
#' Correct column data types for TADA, ATTAINS, and user reference data using the
#' TADA column-type reference file bundled with EPATADA. This ensures downstream
#' TADA functions operate with expected classes.
#'
#' The mapping of column names to target classes is read from:
#' inst/extdata/TADAColTypeRef.csv within the EPATADA package.
#'
#' Supported types in the reference file are:
#' - character
#' - numeric
#' - integer
#' - logical
#' - factor
#' - date
#'
#' Unrecognized or missing types are left unchanged.
#'
#' @param .data A data.frame (or tibble) containing columns required for TADA functions.
#'
#' @return A data.frame with corrected column classes.
#'
#' @examples
#' # df <- TADA_CorrectColType()
#'
#' @export
#' @importFrom utils read.csv
TADA_CorrectColType <- function(.data) {
  if (is.null(.data)) {
    return(NULL)
  }
  if (inherits(.data, "sf")) {
    return(.data)
  } # simplest safe behavior
  if (!is.data.frame(.data)) {
    warning(
      "TADA_CorrectColType: input is neither data.frame nor sf; returning unchanged"
    )
    return(.data)
  }
  ref_path <- system.file("extdata", "TADAColTypeRef.csv", package = "EPATADA")
  if (!nzchar(ref_path) || !file.exists(ref_path)) {
    stop("TADAColTypeRef.csv not found in EPATADA/extdata.")
  }

  coltype.ref <- utils::read.csv(
    ref_path,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )

  required_cols <- c("column_name", "column_type")
  if (!all(required_cols %in% names(coltype.ref))) {
    stop("TADAColTypeRef.csv must contain columns: column_name, column_type.")
  }

  # Normalize entries
  coltype.ref$column_name <- trimws(coltype.ref$column_name)
  coltype.ref$column_type <- tolower(trimws(coltype.ref$column_type))

  # Converter per type
  convert <- function(x, type) {
    switch(
      type,
      character = as.character(x),
      numeric = suppressWarnings(as.numeric(x)),
      integer = suppressWarnings(as.integer(x)),
      logical = {
        # Leave as-is if already logical; convert reasonable string/numeric representations
        if (is.logical(x)) {
          return(x)
        }
        if (is.numeric(x)) {
          return(x != 0)
        }
        if (is.character(x)) {
          lx <- trimws(tolower(x))
          map <- c(
            "true" = "TRUE",
            "t" = "TRUE",
            "y" = "TRUE",
            "yes" = "TRUE",
            "1" = "TRUE",
            "false" = "FALSE",
            "f" = "FALSE",
            "n" = "FALSE",
            "no" = "FALSE",
            "0" = "FALSE"
          )
          lx <- ifelse(lx %in% names(map), map[lx], lx)
          return(as.logical(lx))
        }
        as.logical(x)
      },
      factor = as.factor(x),
      date = {
        if (inherits(x, "Date")) {
          return(x)
        }
        if (inherits(x, "POSIXt")) {
          return(as.Date(x))
        }
        if (is.character(x)) {
          out <- suppressWarnings(as.Date(x))
          if (all(is.na(out)) && any(grepl("[:T]", x))) {
            out <- suppressWarnings(as.Date(as.POSIXct(x, tz = "UTC")))
          }
          return(out)
        }
        suppressWarnings(as.Date(x))
      },
      # Default: unknown type -> leave unchanged
      x
    )
  }

  # Columns present in both the CSV and the data
  present <- intersect(coltype.ref$column_name, names(.data))

  # Also ensure we process any ATTAINS.*Use columns even if not listed in CSV
  use_cols <- grep("^ATTAINS\\..*Use$", names(.data), value = TRUE)
  extra_use_cols <- setdiff(use_cols, present)

  # Union of CSV-present columns and ATTAINS.*Use columns
  process_cols <- union(present, extra_use_cols)

  if (length(process_cols) == 0L) {
    return(.data)
  }

  for (nm in process_cols) {
    # Skip geometry columns (sf objects)
    if (inherits(.data[[nm]], "sfc")) {
      next
    }

    # Determine target type: from CSV if present, otherwise default for ATTAINS.*Use
    if (nm %in% present) {
      target_type <- coltype.ref$column_type[coltype.ref$column_name == nm][1]
    } else {
      # Any ATTAINS.*Use not in CSV gets coerced to character
      target_type <- "character"
    }

    # Generic override: ensure any ATTAINS.*Use column ends up as character
    if (grepl("^ATTAINS\\..*Use$", nm)) {
      target_type <- "character"
    }

    old <- .data[[nm]]
    before_na <- sum(is.na(old))
    new <- try(convert(old, target_type), silent = TRUE)

    if (inherits(new, "try-error")) {
      warning(sprintf(
        "Failed to coerce column '%s' to type '%s'; leaving unchanged.",
        nm,
        target_type
      ))
      next
    }

    after_na <- sum(is.na(new))
    if (after_na > before_na) {
      warning(sprintf(
        "Coercing column '%s' to '%s' introduced %d additional NA values.",
        nm,
        target_type,
        after_na - before_na
      ))
    }

    .data[[nm]] <- new
  }

  .data
}

#' .setDefaultEQKey
#'
#' Get default EPATADA package rExpertQuery API key. For best performance and to
#' avoid server failures from rate-limits, EPATADA users should obtain their own
#' rExpertQuery API Key here: https://owapps.epa.gov/expertquery/api-key-signup
#'
#' @return Character string. The default rExpertQuery API key.
#'
.setDefaultEQKey <- function() {
  default.key <- "lfzVzpwIlKS1O4l1QmbOLUeTzxyql4QdbHVR5Yf5"

  return(default.key)
}
