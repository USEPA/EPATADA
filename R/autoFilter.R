#' Unique Field Values Table
#'
#' Function creates a table yielding the number of unique values in each field (column) returned.
#' The first column of the summary table holds the field name and the second column holds the
#' unique value count.
#'
#' @param .data TADA dataframe
#' @param display A character string denoting what fields to return in the summary table. Defaults to "key". "all" will return all fields in the dataset, "most" will return most field names except those holding numeric values or units, and "key" returns the most important columns to review. Note that if a field is completely NA, it will not be shown on the summary table.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A summary table yielding the number of unique values in each field.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' # Count table of key fields in Data_Nutrients_UT dataset
#' fieldCountUT <- TADA_FieldCounts(Data_Nutrients_UT)
#' # Count table of most fields in Data_Nutrients_UT, filtered to only
#' # AMMONIA results.
#' fieldCountUTAmmonia <- TADA_FieldCounts(Data_Nutrients_UT,
#'   display = "most", characteristicName = "AMMONIA"
#' )
TADA_FieldCounts <- function(.data, display = c("key", "most", "all"), characteristicName = "null") {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)  # Exit the function early
  }  
  
  # # run required flagging/cleaning functions
  # if ("TADA.UseForAnalysis.Flag" %in% colnames(.data)) {
  #   .data <- .data
  # } else {
  #   # create TADA.UseForAnalysis.Flag
  #   .data <- TADA_AnalysisDataFilter(.data)
  # }

  display <- match.arg(display)

  # filter to characteristic if provided
  if (!characteristicName %in% c("null")) {
    .data <- subset(.data, .data$TADA.CharacteristicName %in% c(characteristicName))

    if (dim(.data)[1] < 1) {
      stop("Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization of synonyms from different WQP data providers (USGS's NWIS and EPA's WQX).")
    }
  }

  # remove fields with only NAs from df
  df <- .data %>% dplyr::select(where(~ !all(is.na(.x))))

  if (display == "key") {
    cols <- c(
      "ActivityTypeCode",
      "TADA.ActivityType.Flag",
      # "TADA.UseForAnalysis.Flag",
      "TADA.ActivityMediaName",
      "ActivityMediaSubdivisionName",
      "TADA.MonitoringLocationTypeName",
      "OrganizationFormalName",
      "TADA.CharacteristicName",
      "SubjectTaxonomicName",
      "TADA.MeasureQualifierCode.Def",
      "HydrologicCondition",
      "HydrologicEvent",
      "BiologicalIntentName",
      "AssemblageSampledName",
      "CharacteristicNameUserSupplied",
      "SampleTissueAnatomyName",
      "CharacteristicNameUserSupplied",
      "TADA.ComparableDataIdentifier",
      "ActivityRelativeDepthName",
      "ResultStatusIdentifier",
      "ResultValueTypeName"
    )
  }
  if (display == "most") {
    cols <- c(
      "ActivityGroup",
      "OrganizationIdentifier",
      "OrganizationFormalName",
      "ActivityTypeCode",
      "ActivityMediaSubdivisionName",
      "ActivityRelativeDepthName",
      "ProjectIdentifier",
      "ProjectName",
      "TADA.MonitoringLocationIdentifier",
      "MonitoringLocationName",
      "ActivityCommentText",
      "SampleAquifer",
      "HydrologicCondition",
      "HydrologicEvent",
      "SampleCollectionMethod.MethodIdentifier",
      "SampleCollectionMethod.MethodIdentifierContext",
      "SampleCollectionMethod.MethodName",
      "SampleCollectionMethod.MethodDescriptionText",
      "SampleCollectionEquipmentName",
      "ResultDetectionConditionText",
      "ResultStatusIdentifier",
      "ResultValueTypeName",
      "DataQuality.PrecisionValue",
      "DataQuality.BiasValue",
      "DataQuality.ConfidenceIntervalValue",
      "DataQuality.UpperConfidenceLimitValue",
      "DataQuality.LowerConfidenceLimitValue",
      "ResultCommentText",
      "USGSPCode",
      "SubjectTaxonomicName",
      "SampleTissueAnatomyName",
      "ResultAnalyticalMethod.MethodIdentifier",
      "ResultAnalyticalMethod.MethodIdentifierContext",
      "ResultAnalyticalMethod.MethodName",
      "ResultAnalyticalMethod.MethodDescriptionText",
      "LaboratoryName",
      "ResultLaboratoryCommentText",
      "DetectionQuantitationLimitTypeName",
      "ProviderName",
      "MonitoringLocationDescriptionText",
      "HUCEightDigitCode",
      "HorizontalCoordinateReferenceSystemDatumName",
      "VerticalCoordinateReferenceSystemDatumName",
      "AquiferName",
      "LocalAqfrName",
      "FormationTypeText",
      "AquiferTypeName",
      "ProjectDescriptionText",
      "SamplingDesignTypeCode",
      "QAPPApprovalAgencyName",
      "TADA.ActivityMediaName",
      "TADA.CharacteristicName",
      "CharacteristicNameUserSupplied",
      "TADA.MeasureQualifierCode.Def",
      "TADA.MethodSpeciationName",
      "TADA.ResultSampleFractionText",
      "TADA.ComparableDataIdentifier",
      "TADA.MonitoringLocationTypeName",
      "AssemblageSampledName",
      "BiologicalIntentName",
      "TADA.ActivityType.Flag"
    )
  }
  if (display == "all") {
    cols <- names(df)
  }

  df <- df %>%
    dplyr::select(dplyr::contains(cols))

  # CREATE LIST OF FIELDS
  # Find count of unique values in each column
  col.names <- data.frame(Count = apply(df, 2, function(x) length(unique(x))))
  # Create "Fields" column from row names
  col.names$Fields <- row.names(col.names)
  # Remove row names
  row.names(col.names) <- NULL
  # Reorder columns
  col.names <- col.names[, c(2, 1)]

  # Reorder Count column in col.names from largest to smallest number
  col.names <- col.names %>%
    dplyr::arrange(desc(Count))

  return(col.names)
}

#' Field Values Summary Table
#'
#' Function creates a dataframe containing the relative proportions of values in a given field in a TADA dataset.
#'
#' @param .data TADA dataframe
#' @param field The field (column) the user would like to see represented in a pie chart.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A summary dataframe.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#'
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter:
#' TADA_FieldValuesTable(Data_Nutrients_UT, field = "TADA.CharacteristicName")
#'
TADA_FieldValuesTable <- function(.data, field = "null", characteristicName = "null") {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)  # Exit the function early
  }  
  
  if (!field %in% names(.data)) {
    stop("Field input does not exist in dataset. Please populate the 'field' argument with a valid field name. Enter ?TADA_FieldValuesTable in console for more information.")
  }

  # change NAs to "NA" (character string)
  .data[[field]][is.na(.data[[field]])] <- "NA"

  # filter to characteristic if provided
  if (!characteristicName %in% c("null")) {
    .data <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% characteristicName)
    if (dim(.data)[1] < 1) {
      stop("Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization.")
    }
  }

  dat <- as.data.frame(table(.data[, field]))
  names(dat) <- c("Value", "Count")
  dat <- dat %>% dplyr::arrange(desc(Count))
  return(dat)
}

#' TADA_AnalysisDataFilter
#'
#' With default settings (clean = FALSE), this function creates a TADA.UseForAnalysis.Flag
#' column which flags any data that are NOT surface water results for
#' removal (TADA.UseForAnalysis.Flag = "No") and flags surface water results
#' for use in analysis (TADA.UseForAnalysis.Flag = "Yes"). If desired, a user
#' can change the function input to clean = TRUE, and then the function will
#' filter the dataframe to remove rows that are not going to be used in analyses,
#' and retain only the media types selected by the user.Setting clean = TRUE, means
#' that all results not flagged for use in the analysis workflow will be removed
#' and the TADA.UseForAnalysis.Flag column will not be added.
#'
#' It uses MonitoringLocationTypeName, ActivityMediaName, ActivityMediaSubdivisionName,
#' AquiferName,
#' LocalAqfrName, ConstructionDateText, WellDepthMeasure.MeasureValue,
#' WellDepthMeasure.MeasureUnitCode, WellHoleDepthMeasure.MeasureValue, and
#' WellHoleDepthMeasure.MeasureUnitCode to identify samples. Users
#' can select whether sediment, groundwater and/or surface water should be included.
#' An additional column, TADA.UseForAnalysis.Flag, specifies whether each row should
#' be included in the analysis workflow and why. Setting clean = TRUE, means
#' that all results not flagged for use in the analysis workflow will be removed
#' and the TADA.UseForAnalysis.Flag column will not be added.
#'
#' @param .data A TADA profile object
#'
#' @param clean Boolean argument; TRUE removes all results not flagged for use in
#' analysis workflow. TADA.UseForAnalysis.Flag column displaying the media type (as
#' determined by this function) and "Yes"/"No" will be added when clean = FALSE.
#' Results flagged "Yes" are identified as usable for further analysis. Default = FALSE.
#'
#' @param surface_water Boolean argument; specifies whether surface water
#' results should be flagged or removed in the returned dataframe. Default is
#' surface_water = TRUE, surface water results are identified as usable for analysis.
#'
#' @param ground_water Boolean argument; specifies whether ground water
#' results should be flagged or removed in the returned data frame. Default is
#' ground_water = FALSE, ground water results are identified as not usable for analysis.
#'
#' @param sediment Boolean argument; specifies whether sediment results should
#' be flagged or removed in the returned data frame. Default is sediment = FALSE,
#' sediment results are identified as not usable for analysis.
#'
#' @param other Boolean argument; species whether "other" (uncategorized) results should be flagged
#' or removed in the returned data frame. Default is other = TRUE, other results are retained for
#' additional review by user/use in analysis.
#'
#' @return If clean = TRUE, returns the data frame with only the media types
#' selected as usable (set to TRUE in function input) by the user are returned.
#' If clean = FALSE, returns the data frame and an additional column,
#' TADA.UseForAnalysis.Flag, indicating the media type (as determined by this function)
#' and which results should be included or excluded from assessments based on user input.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(Data_6Tribes_5y_Harmonized)
#' # Returns data with ONLY surface water results retained and no TADA.UseForAnalysis.Flag column
#' Data_6Tribes_Assessment1 <- TADA_AnalysisDataFilter(Data_6Tribes_5y_Harmonized,
#'   clean = TRUE,
#'   surface_water = TRUE, ground_water = FALSE, sediment = FALSE
#' )
#'
#' # Returns dataframe with ONLY surface water results identified as usable and adds
#' # TADA.UseForAnalysis.Flag column.
#' Data_6Tribes_Assessment2 <- TADA_AnalysisDataFilter(Data_6Tribes_5y_Harmonized,
#'   clean = FALSE,
#'   surface_water = TRUE, ground_water = FALSE, sediment = FALSE
#' )
#' unique(Data_6Tribes_Assessment2$TADA.UseForAnalysis.Flag)
#' }
#'
TADA_AnalysisDataFilter <- function(.data,
                                    clean = FALSE,
                                    surface_water = TRUE,
                                    ground_water = FALSE,
                                    sediment = FALSE,
                                    other = TRUE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  
  # Check if the input data frame is empty
  if (nrow(.data) == 0) {
    message("The entered data frame is empty. The function will not run.")
    return(NULL)  # Exit the function early
  }  
  
  # *Need to add fish tissue to this function once using WQX 3.0 profiles

  # import MonitoringLocationTypeNames and TADA.Media.Flags
  sw.sitetypes <- utils::read.csv(system.file("extdata", "WQXMonitoringLocationTypeNameRef.csv", package = "EPATADA")) %>%
    dplyr::select(Name, TADA.Media.Flag) %>%
    dplyr::rename(ML.Media.Flag = TADA.Media.Flag) %>%
    dplyr::mutate(MonitoringLocationTypeName = toupper(Name)) %>%
    dplyr::select(-Name)

  # add TADA.Media.Flag column
  .data <- .data %>%
    # identify TADA.Media.Flag using ActivityMediaSubdivisionName and columns related to groundwater
    dplyr::mutate(TADA.Media.Flag = dplyr::case_when(
      ActivityMediaSubdivisionName == "Groundwater" ~ "Groundwater",
      !is.na(AquiferName) |
        !is.na(AquiferTypeName) |
        !is.na(LocalAqfrName) |
        !is.na(ConstructionDateText) |
        !is.na(WellDepthMeasure.MeasureValue) |
        !is.na(WellDepthMeasure.MeasureUnitCode) |
        !is.na(WellHoleDepthMeasure.MeasureValue) |
        !is.na(WellHoleDepthMeasure.MeasureUnitCode) ~ "Groundwater",
      ActivityMediaSubdivisionName == "Surface Water" ~ "Surface Water",
      !ActivityMediaName %in% c("WATER", "Water", "water") ~ ActivityMediaName
    )) %>%
    # add TADA.Media.Flag for additional rows based on TADA.MonitoringLocationTypeName
    dplyr::left_join(sw.sitetypes, by = "MonitoringLocationTypeName") %>%
    dplyr::mutate(
      TADA.Media.Flag = ifelse(is.na(TADA.Media.Flag),
        ML.Media.Flag, TADA.Media.Flag
      ),
      TADA.Media.Flag = toupper(TADA.Media.Flag)
    ) %>%
    dplyr::select(-ML.Media.Flag) %>%
    # set remaining NA TADA.Media.Flag to OTHER %>%
    dplyr::mutate(TADA.Media.Flag = ifelse(is.na(TADA.Media.Flag),
      "OTHER", TADA.Media.Flag
    ))

  print("TADA_AnalysisDataFilter: Identifying groundwater results.")

  if (surface_water == TRUE) {
    sur.water.flag <- "Yes"

    print("TADA_AnalysisDataFilter: Flagging surface water results to include in assessments.")
  }

  if (surface_water == FALSE) {
    sur.water.flag <- "No"

    print("TADA_AnalysisDataFilter: Flagging surface water results to exclude from assessments.")
  }


  if (ground_water == TRUE) {
    gr.water.flag <- "Yes"

    print("TADA_AnalysisDataFilter: Flagging groundwater results to include in assessments.")
  }

  if (ground_water == FALSE) {
    gr.water.flag <- "No"

    print("TADA_AnalysisDataFilter: Flagging groundwater results to exclude from assessments.")
  }

  if (sediment == TRUE) {
    sed.flag <- "Yes"

    print("TADA_AnalysisDataFilter: Flagging sediment results to include in assessments.")
  }

  if (sediment == FALSE) {
    sed.flag <- "No"

    print("TADA_AnalysisDataFilter: Flagging sediment results to exclude from assessments.")
  }

  if (other == TRUE) {
    other.flag <- "Yes"

    print("TADA_AnalysisDataFilter: Flagging other results to include in assessments.")
  }

  if (other == FALSE) {
    other.flag <- "No"

    print("TADA_AnalysisDataFilter: Flagging other results to exclude from assessments.")
  }

  # add media flag
  .data <- .data %>%
    dplyr::mutate(
      TADA.UseForAnalysis.Flag = dplyr::case_when(
        TADA.Media.Flag == "SEDIMENT" ~ paste0(sed.flag, " - ", TADA.Media.Flag),
        TADA.Media.Flag == "SURFACE WATER" ~ paste0(sur.water.flag, " - ", TADA.Media.Flag),
        TADA.Media.Flag == "GROUNDWATER" ~ paste0(gr.water.flag, " - ", TADA.Media.Flag),
        TADA.Media.Flag == "OTHER" ~ paste0(other.flag, " - ", TADA.Media.Flag),
        !TADA.Media.Flag %in% c("SEDIMENT", "SURFACE WATER", "GROUNDWATER", "OTHER") ~ paste("No - ", TADA.Media.Flag, sep = "")
      )
    )

  if (clean == TRUE) {
    .data <- .data %>%
      dplyr::filter(stringr::str_detect(TADA.UseForAnalysis.Flag, "Yes")) %>%
      dplyr::select(c(-TADA.UseForAnalysis.Flag, -TADA.Media.Flag)) %>%
      TADA_OrderCols()

    print("TADA_AnalysisDataFilter: Removing results flagged for exclusion from assessments.")

    return(.data)
  }

  if (clean == FALSE) {
    .data <- .data %>%
      dplyr::select(-TADA.Media.Flag) %>%
      TADA_OrderCols()

    print("TADA_AnalysisDataFilter: Returning all results with TADA.UseForAnalysis.Flag column indicating if result should be used for assessments.")

    return(.data)
  }
}
