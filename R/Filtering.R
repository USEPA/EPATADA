#' Generate list of field names
#' 
#' Function creates a list of the fields in the input dataframe as well as the
#' number of unique values in each field. The list is intended to inform users
#' as to which specific fields to explore further and filter.
#'
#' @param .data TADA dataframe
#'
#' @return A table of fields and the count of unique values in each field.
#' 
#' @export
#' 

FilterFields <- function(.data){  
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  
  # CREATE LIST OF FIELDS
  # Find count of unique values in each column
  col.names <- data.frame(Count = apply(.data, 2, function(x) length(unique(x))))
  # Create "FieldName" column from row names
  col.names$FieldName <- row.names(col.names) 
  # Remove row names
  row.names(col.names) <- NULL
  # Reorder columns
  col.names <- col.names[, c(2,1)]
  # Filter dataframe to include only fields for filtering
  col.names <- dplyr::filter(col.names, FieldName %in% c("ActivityTypeCode", "ActivityMediaName", 
                                                         "ActivityMediaSubdivisionName", 
                                                         "ActivityCommentText", "MonitoringLocationTypeName",
                                                         "StateName", "TribalLandName",
                                                         "OrganizationFormalName", "CharacteristicName", 
                                                         "HydrologicCondition", "HydrologicEvent",
                                                         "BiologicalIntentName", "MeasureQualifierCode", 
                                                         "ActivityGroup", "AssemblageSampledName",
                                                         "ProjectName", "CharacteristicNameUserSupplied",
                                                         "DetectionQuantitationLimitTypeName",
                                                         "SampleTissueAnatomyName", "LaboratoryName"))
  
  # CREATE LIST OF UNIQUE VALUES PER FIELD FROM DATAFRAME
  
  # remove fields with only NAs from df
  df <- .data %>% dplyr::select(where(~!all(is.na(.x))))
  
  # Get list of unique values per field
  UniqueValList <- apply(df, 2, function(x){data.frame(table(list(x)))})
  
  # Reorder Freq column in each data frame from largest to smallest number
  UniqueValList <- lapply(UniqueValList, function(X){X[order(-X$Freq),]})
  
  # Rename fields
  UniqueValList <- lapply(UniqueValList, setNames, c("FieldValue", "Count"))
  
  # Filter list to include only fields for filtering
  UniqueValList <- UniqueValList[c("ActivityTypeCode", "ActivityMediaName", 
                                   "ActivityMediaSubdivisionName", 
                                   "ActivityCommentText", "MonitoringLocationTypeName",
                                   "StateName", "TribalLandName",
                                   "OrganizationFormalName", "CharacteristicName", 
                                   "HydrologicCondition", "HydrologicEvent",
                                   "BiologicalIntentName", "MeasureQualifierCode", 
                                   "ActivityGroup", "AssemblageSampledName",
                                   "ProjectName", "CharacteristicNameUserSupplied",
                                   "DetectionQuantitationLimitTypeName",
                                   "SampleTissueAnatomyName", "LaboratoryName")]
  
  .GlobalEnv$UniqueValList <- UniqueValList
  print(col.names)
}

#' Generate list of unique values in a given field
#' 
#' Function creates a table and pie chart of unique values, and counts of those
#' values for a chosen field in a dataframe.
#'
#' @param .data TADA dataframe
#'
#' @return A table and pie chart of unique values in the selected field.
#' 
#' @export
#' 

FilterFieldReview <- function(field){
  
  # check that input is in UniqueValList
  if(exists(field, UniqueValList) == FALSE){
    stop("Input does not exist as a field in the dataset.")
  }
  # subset UniqueValList by input
  df <- UniqueValList[[field]]
  
  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValue))
  
  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
  
  # create pie chart
  pie <- ggplot2::ggplot(df, ggplot2::aes(x="", y = Count, fill = FieldValue)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount)) +
    ggplot2::geom_bar(stat="identity", width=1) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::theme_void()
  
  print(pie)
  print(df)
}

#' Generate list of parameters
#' 
#' Function generates list of characteristics in the input dataset, as well as 
#' the number of records for each. The list is intended to inform users
#' as to which parameters to explore further and filter.
#'
#' @param .data TADA dataframe
#'
#' @return 
#' 
#' @export
#' 
FilterParList <- function(.data){
  # count the frequency of each value in CharactersticName field
  ParValueCount <- data.frame(table(list(.data$CharacteristicName)))
  # Reorder Freq column from largest to smallest number
  ParValueCount <- ParValueCount[order(-ParValueCount$Freq),]
  # Rename fields
  ParValueCount <- stats::setNames(ParValueCount, c("FieldValue", "Count"))
  
  print(ParValueCount)
}

#' Generate list of field names subset by parameter
#' 
#' Function subsets the input dataframe by the input parameter and creates a 
#' list of the fields in the subset dataframe as well as the number of unique 
#' values in each field. The list is intended to inform users as to which 
#' specific fields to explore further and filter subset by a parameter.
#' 
#' @param .data TADA dataframe
#'
#' @return A table of fields and the count of unique values in each field, 
#' subset by a parameter.
#' 
#' @export
#' 

FilterParFields <- function(.data, parameter) {
  
  # check that .data object is compatible with TADA
    # check .data is of class data.frame
  if("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
    # check .data has required columns
  if(("CharacteristicName" %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # check parameter is in .data
  if((parameter %in% .data$CharacteristicName) == FALSE) {
    stop("Input parameter is not in the input dataset.")
  }
  
  # SUBSET DATAFRAME; CREATE LIST OF FIELDS
  df <- dplyr::filter(.data, CharacteristicName %in% parameter)
  # Find count of unique values in each column
  col.names <- data.frame(Count = apply(df, 2, function(x) length(unique(x))))
  # Create "FieldName" column from row names
  col.names$FieldName <- row.names(col.names) 
  # Remove row names
  row.names(col.names) <- NULL
  # Reorder columns
  col.names <- col.names[, c(2,1)]
  # Filter col.names to include only fields for filtering
  col.names <- dplyr::filter(col.names, FieldName %in% c("ActivityCommentText", "ActivityTypeCode",
                                                         "ActivityMediaName", "ActivityMediaSubdivisionName", 
                                                         "MeasureQualifierCode", "MonitoringLocationTypeName",
                                                         "HydrologicCondition", "HydrologicEvent",
                                                         "ResultStatusIdentifier", "MethodQualifierTypeName",
                                                         "ResultCommentText", "ResultLaboratoryCommentText",
                                                         "ResultMeasure.MeasureUnitCode", 
                                                         "ResultSampleFractionText", "ResultTemperatureBasisText",
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
                                                         "AssemblageSampledName", "DetectionQuantitationLimitTypeName"))
  
  # CREATE LIST OF UNIQUE VALUES PER FIELD FROM DATAFRAME
  # remove fields with only NAs from df (NA-only fields were removed from WQP, not .data) 
  df <- df %>% dplyr::select(where(~!all(is.na(.x))))
  # count the frequency of each value in all fields in df for selected characteristic
  ParUniqueValList <- apply(df, 2, function(x){data.frame(table(list(x)))})
  # Reorder Freq column in each data frame from largest to smallest number
  ParUniqueValList <- lapply(ParUniqueValList, function(x){x[order(-x$Freq),]})
  # Rename fields
  ParUniqueValList <- lapply(ParUniqueValList, stats::setNames, c("FieldValue", "Count"))
  # Filter list to include only fields for filtering
  ParUniqueValList <- ParUniqueValList[c("ActivityCommentText", "ActivityTypeCode",
                                         "ActivityMediaName", "ActivityMediaSubdivisionName", 
                                         "MeasureQualifierCode", "MonitoringLocationTypeName",
                                         "HydrologicCondition", "HydrologicEvent",
                                         "ResultStatusIdentifier", "MethodQualifierTypeName",
                                         "ResultCommentText", "ResultLaboratoryCommentText",
                                         "ResultMeasure.MeasureUnitCode", 
                                         "ResultSampleFractionText", "ResultTemperatureBasisText",
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
                                         "AssemblageSampledName", "DetectionQuantitationLimitTypeName")]
  
  
  .GlobalEnv$ParUniqueValList <- ParUniqueValList
  print(col.names)
}  

#' Generate list of unique values in a given field subset by parameter
#' 
#' Function creates a table and pie chart of unique values, and counts of those
#' values, for a chosen field in a dataframe subset by parameter.
#'
#' @param .data TADA dataframe
#'
#' @return A table and pie chart of unique values in the selected field.
#' 
#' @export
#' 

FilterParFieldReview <- function(field){
  
  # check that input is in ParUniqueValList
  if(exists(field, ParUniqueValList) == FALSE){
    stop("Input does not exist as a field in the dataset.")
  }
  # subset UniqueValList by input
  df <- ParUniqueValList[[field]]
  
  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValue))
  
  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
  
  # create pie chart
  pie <- ggplot2::ggplot(df, ggplot2::aes(x="", y = Count, fill = FieldValue)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount)) +
    ggplot2::geom_bar(stat="identity", width=1) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::theme_void()
  
  print(pie)
  print(df) 
}