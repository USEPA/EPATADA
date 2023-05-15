#' Used with multiple filter functions in this .R file
TADA.env <- new.env()


#' Generate list of field names
#'
#' Function creates a list of common WQX fields that should be reviewed when 
#' doing full data frame filtering, as well as the
#' number of unique values in each field. The list is intended to inform users
#' as to which specific fields to explore further and filter by.
#' 
#' The output will include counts for the following fields:
#' "ActivityTypeCode"
#' "TADA.ActivityMediaName"
#' "ActivityMediaSubdivisionName"
#' "ActivityCommentText"
#' "MonitoringLocationTypeName"
#' "StateName"
#' "TribalLandName"
#' "OrganizationFormalName"
#' "TADA.CharacteristicName"
#' "HydrologicCondition"
#' "HydrologicEvent"
#' "BiologicalIntentName"
#' "MeasureQualifierCode"
#' "ActivityGroup"
#' "AssemblageSampledName"
#' "ProjectName"
#' "CharacteristicNameUserSupplied"
#' "DetectionQuantitationLimitTypeName"
#' "SampleTissueAnatomyName"
#' "LaboratoryName"
#'
#' @param .data TADA dataframe
#'
#' @return A table of fields and the count of unique values in each field.
#'
#' @export
#'
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create a table of fields and count of unique values in each field:
#' Fields_Nutrients_Utah <- FilterFields(Nutrients_Utah)
#' 

FilterFields <- function(.data) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")

  # CREATE LIST OF FIELDS
  # Find count of unique values in each column
  col.names <- data.frame(Count = apply(.data, 2, function(x) length(unique(x))))
  # Create "Fields" column from row names
  col.names$Fields <- row.names(col.names)
  # Remove row names
  row.names(col.names) <- NULL
  # Reorder columns
  col.names <- col.names[, c(2, 1)]
  
  # Filter dataframe to include only recommended fields for filtering
  # This does not limit the elements that can be used in other filter functions
  # This only filters the table output of this function
  col.names <- dplyr::filter(col.names, Fields %in% c(
    "ActivityTypeCode", "TADA.ActivityMediaName",
    "ActivityMediaSubdivisionName",
    "ActivityCommentText", "MonitoringLocationTypeName",
    "StateName", "TribalLandName",
    "OrganizationFormalName", "TADA.CharacteristicName",
    "HydrologicCondition", "HydrologicEvent",
    "BiologicalIntentName", "MeasureQualifierCode",
    "ActivityGroup", "AssemblageSampledName",
    "ProjectName", "CharacteristicNameUserSupplied",
    "DetectionQuantitationLimitTypeName",
    "SampleTissueAnatomyName", "LaboratoryName"
   ))

  # Reorder Count column in col.names from largest to smallest number
  col.names <- col.names %>%
    dplyr::arrange(desc(Count))
  
  # CREATE LIST OF UNIQUE VALUES PER FIELDS FROM DATAFRAME

  # remove fields with only NAs from df
  df <- .data %>% dplyr::select(where(~ !all(is.na(.x))))

  # Get list of unique values per field
  UniqueValList <- apply(df, 2, function(x) {
    data.frame(table(list(x)))
  })

  # Reorder Freq column in each data frame from largest to smallest number
  UniqueValList <- lapply(UniqueValList, function(X) {
    X[order(-X$Freq), ]
  })

  # Rename fields
  UniqueValList <- lapply(UniqueValList, stats::setNames, c("FieldValues", "Count"))

  # Filter list to include only recommended fields for filtering (cm removed 3/23/23)
  # UniqueValList <- UniqueValList[c(
  #  "ActivityTypeCode", "TADA.ActivityMediaName",
  #  "ActivityMediaSubdivisionName",
  #  "ActivityCommentText", "MonitoringLocationTypeName",
  #  "StateName", "TribalLandName",
  #  "OrganizationFormalName", "TADA.CharacteristicName",
  #  "HydrologicCondition", "HydrologicEvent",
  #  "BiologicalIntentName", "MeasureQualifierCode",
  #  "ActivityGroup", "AssemblageSampledName",
  #  "ProjectName", "CharacteristicNameUserSupplied",
  #  "DetectionQuantitationLimitTypeName",
  #  "SampleTissueAnatomyName", "LaboratoryName"
  #)]

  TADA.env$UniqueValList <- UniqueValList
  
  print(col.names)
}


#' Generate list of unique values in a given field
#'
#' Function creates a table and pie chart of all unique values 
#' (and their counts) in the input dataframe for the field specified.
#' 
#' Any field may be specified. 
#'
#' To start, we recommend reviewing any of the following fields:
#' "ActivityTypeCode"
#' "TADA.ActivityMediaName"
#' "ActivityMediaSubdivisionName"
#' "ActivityCommentText"
#' "MonitoringLocationTypeName"
#' "StateName"
#' "TribalLandName"
#' "OrganizationFormalName"
#' "TADA.CharacteristicName"
#' "HydrologicCondition"
#' "HydrologicEvent"
#' "BiologicalIntentName"
#' "MeasureQualifierCode"
#' "ActivityGroup"
#' "AssemblageSampledName"
#' "ProjectName"
#' "CharacteristicNameUserSupplied"
#' "DetectionQuantitationLimitTypeName"
#' "SampleTissueAnatomyName"
#' "LaboratoryName"
#'
#' @param field Field name
#' @param .data Optional argument; TADA dataframe
#'
#' @return A table and pie chart of unique values in the selected field.
#'
#' @export
#'
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create table and pie chart of "Hydrologic Condition" unique values and counts:
#' FieldReview_HydrologicCondition <- FilterFieldReview(field = "HydrologicCondition", Nutrients_Utah)
#' 

FilterFieldReview <- function(field, .data) {
  # if provided, check .data is data.frame
  if (!missing(.data)) {
    checkType(.data, "data.frame", "Input object")
  }
  # execute function after checks are passed

  # refresh UniqueValList
  invisible(utils::capture.output(FilterFields(.data)))
  
  # check that input is in UniqueValList
  if (exists(field, TADA.env$UniqueValList) == FALSE) {
    stop("The field entered into FilterFieldReview is not populated (all NA) in 
         the input data frame.")
  }

  # subset UniqueValList by input
  df <- TADA.env$UniqueValList[[field]]
  df$Legend = paste0(df$FieldValues, " - ", df$Count, " results")
  
  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValues))

  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

  # create pie chart
  # look into fixing display of counts for small slices: 
  # https://stackoverflow.com/questions/28912059/labels-on-the-pie-chart-for-small-pieces-ggplot
  pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = Legend)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount),name = field) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() # remove background, grid, numeric labels
    # ggplot2::geom_text(ggplot2::aes(label = Count), color = "black", size=4, position = ggplot2::position_stack(vjust = 0.5))

  print(pie)
  print(df)
}



#' Generate list of parameters
#'
#' Function generates list of characteristics in the input dataframe, as well as
#' the number of records for each. The list is intended to inform users
#' as to which parameters to explore further and filter.
#'
#' @param .data TADA dataframe
#'
#' @return A list of unique values in TADA.CharacteristicName and their counts
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter: 
#' ParameterList <- FilterParList(Nutrients_Utah)
#' 

FilterParList <- function(.data) {
  # count the frequency of each value in TADA.CharactersticName field
  ParValueCount <- data.frame(table(list(.data$TADA.CharacteristicName)))
  
  # Reorder Freq column from largest to smallest number
  ParValueCount <- ParValueCount[order(-ParValueCount$Freq), ]
  
  # Rename fields
  ParValueCount <- stats::setNames(ParValueCount, c("TADA.CharacteristicName",
                                                    "Count"))
  print(ParValueCount)
}



#' Generate list of field names subset by parameter
#'
#' Function subsets the input dataframe by the input parameter and creates a
#' list of the fields in the subset dataframe as well as the number of unique
#' values in each field. The list is intended to inform users as to which
#' specific fields to explore further and filter (subset by a parameter).
#' 
#' Recommended fields to review and filter by for each parameter:
#' "ActivityCommentText"
#' "ActivityTypeCode"
#' "TADA.ActivityMediaName"
#' "ActivityMediaSubdivisionName"
#' "MeasureQualifierCode"
#' "MonitoringLocationTypeName"
#' "HydrologicCondition"
#' "HydrologicEvent"
#' "ResultStatusIdentifier"
#' "MethodQualifierTypeName"
#' "ResultCommentText" 
#' "ResultLaboratoryCommentText"
#' "TADA.ResultMeasure.MeasureUnitCode"
#' "TADA.ResultSampleFractionText"
#' "ResultTemperatureBasisText"
#' "ResultValueTypeName"
#' "ResultWeightBasisText"
#' "SampleCollectionEquipmentName"
#' "LaboratoryName"
#' "MethodDescriptionText"
#' "ResultParticleSizeBasisText"
#' "SampleCollectionMethod.MethodIdentifier"
#' "SampleCollectionMethod.MethodIdentifierContext"
#' "SampleCollectionMethod.MethodName"
#' "DataQuality.BiasValue"
#' "MethodSpeciationName"
#' "ResultAnalyticalMethod.MethodName"
#' "ResultAnalyticalMethod.MethodIdentifier"
#' "ResultAnalyticalMethod.MethodIdentifierContext"
#' "AssemblageSampledName"
#' "DetectionQuantitationLimitTypeName"
#'
#' @param .data TADA dataframe
#' @param parameter Characteristic name (parameter name) from the dataframe.
#'
#' @return A table of fields and the count of unique values in each field,
#' subset by a parameter.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create list of fields for parameter "AMMONIA" with number of unique values in each field:
#' AmmoniaFields <- FilterParFields(Nutrients_Utah, parameter = "AMMONIA")
#' 

FilterParFields <- function(.data, parameter) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check .data has required columns
  checkColumns(.data, "TADA.CharacteristicName")
  
  # check parameter is in .data
  if ((parameter %in% .data$TADA.CharacteristicName) == FALSE) {
    stop("Input parameter for FilterParFields function is not in the 
         input dataframe.")
  }

  # SUBSET DATAFRAME; CREATE LIST OF FIELDS
  df <- dplyr::filter(.data, TADA.CharacteristicName %in% parameter)
  # Find count of unique values in each column
  col.names <- data.frame(Count = apply(df, 2, function(x) length(unique(x))))
  # Create "Fields" column from row names
  col.names$Fields <- row.names(col.names)
  # Remove row names
  row.names(col.names) <- NULL
  # Reorder columns
  col.names <- col.names[, c(2, 1)]

  
  # Filter col.names to include only fields recommended for filtering
  # This does not limit the fields than can be entered in other filter 
  # functions. It simply subsets the output table to a more manageable 
  # number of fields for the user to review (subsets to suggestions)
  col.names <- dplyr::filter(col.names, Fields %in% c(
    "ActivityCommentText", "ActivityTypeCode",
    "TADA.ActivityMediaName", "ActivityMediaSubdivisionName",
    "MeasureQualifierCode", "MonitoringLocationTypeName",
    "HydrologicCondition", "HydrologicEvent",
    "ResultStatusIdentifier", "MethodQualifierTypeName",
    "ResultCommentText", "ResultLaboratoryCommentText",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.ResultSampleFractionText", "ResultTemperatureBasisText",
    "ResultValueTypeName", "ResultWeightBasisText",
    "SampleCollectionEquipmentName", "LaboratoryName",
    "MethodDescriptionText", "ResultParticleSizeBasisText",
    "SampleCollectionMethod.MethodIdentifier",
    "SampleCollectionMethod.MethodIdentifierContext",
    "SampleCollectionMethod.MethodName",
    "DataQuality.BiasValue", "MethodSpeciationName",
    "ResultAnalyticalMethod.MethodName",
    "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext",
    "AssemblageSampledName", "DetectionQuantitationLimitTypeName"
  ))

  
  # Reorder Count column in col.names from largest to smallest number
  col.names <- col.names %>%
    dplyr::arrange(desc(Count))
  
  
  # CREATE LIST OF UNIQUE VALUES PER FIELD FROM DATAFRAME
  # remove fields with only NAs from df (NA-only fields were removed from WQP, not .data)
  df <- df %>% dplyr::select(where(~ !all(is.na(.x))))
  # count the frequency of each value in all fields in df for selected characteristic
  ParUniqueValList <- apply(df, 2, function(x) {
    data.frame(table(list(x)))
  })
  # Reorder Freq column in each data frame from largest to smallest number
  ParUniqueValList <- lapply(ParUniqueValList, function(x) {
    x[order(-x$Freq), ]
  })
  # Rename fields
  ParUniqueValList <- lapply(ParUniqueValList, stats::setNames, 
                             c("FieldValues", "Count"))
  
  # cm removed so we do not limit users ability to enter other fields
  # Filter list to require only these fields for filtering
  #ParUniqueValList <- ParUniqueValList[c(
  #  "ActivityCommentText", "ActivityTypeCode",
  #  "TADA.ActivityMediaName", "ActivityMediaSubdivisionName",
  #  "MeasureQualifierCode", "MonitoringLocationTypeName",
  #  "HydrologicCondition", "HydrologicEvent",
  #  "ResultStatusIdentifier", "MethodQualifierTypeName",
  #  "ResultCommentText", "ResultLaboratoryCommentText",
  #  "TADA.ResultMeasure.MeasureUnitCode",
  #  "TADA.ResultSampleFractionText", "ResultTemperatureBasisText",
  #  "ResultValueTypeName", "ResultWeightBasisText",
  #  "SampleCollectionEquipmentName", "LaboratoryName",
  #  "MethodDescriptionText", "ResultParticleSizeBasisText",
  #  "SampleCollectionMethod.MethodIdentifier",
  #  "SampleCollectionMethod.MethodIdentifierContext",
  #  "SampleCollectionMethod.MethodName",
  #  "DataQuality.BiasValue", "MethodSpeciationName",
  #  "ResultAnalyticalMethod.MethodName",
  #  "ResultAnalyticalMethod.MethodIdentifier",
  #  "ResultAnalyticalMethod.MethodIdentifierContext",
  #  "AssemblageSampledName", "DetectionQuantitationLimitTypeName", 
  #  "MonitoringLocationIdentifier"
  #)]

  TADA.env$ParUniqueValList <- ParUniqueValList
  print(col.names)
}



#' Generate list of unique values in a given field subset by parameter
#'
#' Function creates a table and pie chart of unique values, and counts of those
#' values, for a chosen field in a dataframe subset by parameter.
#' 
#' Recommended fields to review and filter by for each parameter:
#' "ActivityCommentText"
#' "ActivityTypeCode"
#' "TADA.ActivityMediaName"
#' "ActivityMediaSubdivisionName"
#' "MeasureQualifierCode"
#' "MonitoringLocationTypeName"
#' "HydrologicCondition"
#' "HydrologicEvent"
#' "ResultStatusIdentifier"
#' "MethodQualifierTypeName"
#' "ResultCommentText" 
#' "ResultLaboratoryCommentText"
#' "TADA.ResultMeasure.MeasureUnitCode"
#' "TADA.ResultSampleFractionText"
#' "ResultTemperatureBasisText"
#' "ResultValueTypeName"
#' "ResultWeightBasisText"
#' "SampleCollectionEquipmentName"
#' "LaboratoryName"
#' "MethodDescriptionText"
#' "ResultParticleSizeBasisText"
#' "SampleCollectionMethod.MethodIdentifier"
#' "SampleCollectionMethod.MethodIdentifierContext"
#' "SampleCollectionMethod.MethodName"
#' "DataQuality.BiasValue"
#' "MethodSpeciationName"
#' "ResultAnalyticalMethod.MethodName"
#' "ResultAnalyticalMethod.MethodIdentifier"
#' "ResultAnalyticalMethod.MethodIdentifierContext"
#' "AssemblageSampledName"
#' "DetectionQuantitationLimitTypeName"
#'
#' @param field Field name
#' @param .data Optional argument; TADA dataframe
#' @param parameter Characteristic name (parameter name) from the dataframe.
#'
#' @return A table and pie chart of unique values in the selected field.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create table and pie chart of monitoring locations for the parameter "AMMONIA" in dataframe:
#' AmmoniaMonitoringLocations <- FilterParFieldReview(field = "MonitoringLocationIdentifier", Nutrients_Utah, parameter = "AMMONIA")
#' 

FilterParFieldReview <- function(field, .data, parameter) {
  # if provided, check .data is data.frame
  if (!missing(.data)) {
    checkType(.data, "data.frame", "Input object")
  }
  # check parameter is in .data
  if (!missing(parameter)) {
    if ((parameter %in% .data$TADA.CharacteristicName) == FALSE) {
      stop("The parameter entered into FilterParFieldReview is not in the input 
           dataframe.")
    }
  }
  
  # execute function after checks are passed
  
  # refreshes UniqueValList
  FilterParList(.data)
  FilterParFields(.data, parameter)

  invisible(utils::capture.output(FilterParFields(.data, parameter)))
  
  # check that input is in ParUniqueValList
  if (exists(field, TADA.env$ParUniqueValList) == FALSE) {
    stop("The field entered into FilterParFieldReview is NOT available 
    (or all NA) for the parameter entered in the input dataframe.")
  }

  # subset UniqueValList by input
  df <- TADA.env$ParUniqueValList[[field]]
  df$Legend = paste0(df$FieldValues, " - ", df$Count, " results")
  
  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValues))

  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

  # create pie chart
  pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = Legend)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount), name = field) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() +
    ggplot2::labs(title = parameter) #+ 
    # ggplot2::geom_text(ggplot2::aes(label = Count), color = "black", size=4, position = ggplot2::position_stack(vjust = 0.5))

  print(pie)
  print(df)
}

#' Unique Field Values Table
#'
#' Function creates a table yielding the number of unique values in each field (column) returned.
#' The first column of the summary table holds the field name and the second column holds the
#' unique value count.
#' 
#' @param .data TADA dataframe
#' @param display A character string denoting what fields to return in the summary table. Defaults to "key". "all" will return all fields in the dataset, "narrow" will return most field names except those holding numeric values or units, and "key" returns the most important columns to review. Note that if a field is completely NA, it will not be shown on the summary table.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A summary table yielding the number of unique values in each field.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' # Count table of key fields in Nutrients_Utah dataset
#' fieldCountUT <- fieldCounts(Nutrients_Utah)
#' # Count table of most fields in Nutrients_Utah, filtered to only AMMONIA results.
#' fieldCountUTAmmonia <- fieldCounts(Nutrients_Utah, display = "narrow", characteristicName = "AMMONIA")

fieldCounts <- function(.data, display = "key", characteristicName = "null"){
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  
  # filter to characteristic if provided
  if(!characteristicName%in%c("null")){
    .data = subset(.data, .data$TADA.CharacteristicName%in%c(characteristicName))
    
    if(length(.data)<1){
      stop("Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization.")
    }
  }
  
  # remove fields with only NAs from df
  df <- .data %>% dplyr::select(where(~ !all(is.na(.x))))
  
  if(display=="key"){
    cols = c("ActivityTypeCode", "TADA.ActivityMediaName",
             "ActivityMediaSubdivisionName",
             "ActivityCommentText", "MonitoringLocationTypeName",
             "StateName", "TribalLandName",
             "OrganizationFormalName", "TADA.CharacteristicName",
             "HydrologicCondition", "HydrologicEvent",
             "BiologicalIntentName", "MeasureQualifierCode",
             "ActivityGroup", "AssemblageSampledName",
             "ProjectName", "CharacteristicNameUserSupplied",
             "DetectionQuantitationLimitTypeName",
             "SampleTissueAnatomyName", "LaboratoryName")
  }
  if(display=="narrow"){
    cols = c("ResultIdentifier",
            "OrganizationIdentifier",
             "OrganizationFormalName",
             "ActivityIdentifier",
             "ActivityTypeCode",
             "ActivityMediaName",
             "ActivityMediaSubdivisionName",
             "ActivityRelativeDepthName",
             "ProjectIdentifier",
             "ProjectName",
             "ActivityConductingOrganizationText",
             "MonitoringLocationIdentifier",
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
             "MethodSpeciationName",
             "CharacteristicName",
             "ResultSampleFractionText",
             "MeasureQualifierCode",
             "ResultStatusIdentifier",
             "StatisticalBaseCode",
             "ResultValueTypeName",
             "ResultWeightBasisText",
             "ResultTimeBasisText",
             "ResultTemperatureBasisText",
             "ResultParticleSizeBasisText",
             "DataQuality.PrecisionValue",
             "DataQuality.BiasValue",
             "DataQuality.ConfidenceIntervalValue",
             "DataQuality.UpperConfidenceLimitValue",
             "DataQuality.LowerConfidenceLimitValue",
             "ResultCommentText",
             "USGSPCode",
             "ResultDepthAltitudeReferencePointText",
             "SubjectTaxonomicName",
             "SampleTissueAnatomyName",
             "BinaryObjectFileName",
             "BinaryObjectFileTypeCode",
             "ResultFileUrl",
             "ResultAnalyticalMethod.MethodIdentifier",
             "ResultAnalyticalMethod.MethodIdentifierContext",
             "ResultAnalyticalMethod.MethodName",
             "ResultAnalyticalMethod.MethodUrl",
             "ResultAnalyticalMethod.MethodDescriptionText",
             "LaboratoryName",
             "ResultLaboratoryCommentText",
             "ResultDetectionQuantitationLimitUrl",
             "DetectionQuantitationLimitTypeName",
             "LabSamplePreparationUrl",
             "ProviderName",
             "MonitoringLocationTypeName",
             "MonitoringLocationDescriptionText",
             "HUCEightDigitCode",
             "LatitudeMeasure",
             "LongitudeMeasure",
             "SourceMapScaleNumeric",
             "HorizontalCollectionMethodName",
             "HorizontalCoordinateReferenceSystemDatumName",
             "VerticalCollectionMethodName",
             "VerticalCoordinateReferenceSystemDatumName",
             "CountryCode",
             "StateCode",
             "CountyCode",
             "AquiferName",
             "LocalAqfrName",
             "FormationTypeText",
             "AquiferTypeName",
             "ConstructionDateText",
             "MethodSpecificationName",
             "ProjectDescriptionText",
             "SamplingDesignTypeCode",
             "QAPPApprovedIndicator",
             "QAPPApprovalAgencyName",
             "ProjectFileUrl",
             "ProjectMonitoringLocationWeightingUrl",
             "TADA.LatitudeMeasure",
             "TADA.LongitudeMeasure",
             "TADA.InvalidCoordinates.Flag",
             "TADA.ActivityMediaName", 
             "TADA.CharacteristicName",
             "TADA.CharacteristicGroup",
             "CharacteristicNameUserSupplied",
             "TADA.SuggestedCharacteristicName",
             "TADA.CharacteristicNameAssumptions",
             "TADA.TotalN_TotalP_CharacteristicNames_AfterSummation",
             "TADA.TotalN_TotalP_Summation_Identifier",
             "TADA.TotalN_TotalP_ComboLogic",
             "TADA.AggregatedContinuousData.Flag",
             "TADA.ResultMeasureValue",
             "TADA.ResultMeasureValueDataTypes.Flag",
             "TADA.CensoredData.Flag",
             "TADA.CensoredMethod",
             "TADA.ResultUnit.Flag",
             "TADA.MethodSpecificationName",
             "TADA.AnalyticalMethod.Flag",
             "TADA.MethodSpeciation.Flag",
             "TADA.ResultSampleFractionText",
             "TADA.SampleFraction.Flag",
             "TADA.SuggestedSampleFraction",
             "TADA.FractionAssumptions",
             "TADA.ComparableDataIdentifier",
             "TADA.RemoveReason"
    )
  }
  if(display=="all"){
    cols = names(df)
  }
  
  df = df[,names(df)%in%cols]
  
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


#' Field Values Pie Chart
#'
#' Function creates a ggplot2 pie chart showing the relative proportions of values in a given field in a TADA dataset.
#' 
#' @param .data TADA dataframe
#' @param field The field (column) the user would like to see represented in a pie chart.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A ggplot2 pie chart.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter: 
#' filterPie(Nutrients_Utah, field = "TADA.CharacteristicName")
#' 


filterPie <- function(.data,field="null",characteristicName="null"){
  
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  
  if(!field%in%names(.data)){
    stop("Field input does not exist in dataset. Please populate the 'field' argument with a valid field name. Enter ?TADA::filterPie in console for more information.")
  }
  
  # filter to characteristic if provided
  if(!characteristicName%in%c("null")){
    .data = subset(.data, .data$TADA.CharacteristicName%in%c(characteristicName))
    if(length(.data)<1){
      stop("Characteristic name(s) provided are not contained within the input dataset. Note that TADA converts characteristic names to ALL CAPS for easier harmonization.")
    }
  }
  
  dat = as.data.frame(table(.data[,field]))
  dat$Legend = paste0(dat$Var1, " - ", dat$Freq, " results")
  
  # define number of colors required for pie chart
  colorCount <- length(unique(dat$Var1))
  
  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
  
  # create pie chart
  pie <- ggplot2::ggplot(dat, ggplot2::aes(x = "", y = Freq, fill = Legend)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount), name = field) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() 
  
  return(pie)
}