#' Used with multiple filter functions in this .R file
TADA.env <- new.env()


#' Generate list of field names
#'
#' Function creates a list of common fields in the input dataframe that
#' are used for full data frame filtering, as well as the
#' number of unique values in each field. The list is intended to inform users
#' as to which specific fields to explore further and filter.
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
  
  # check that input is in UniqueValList (cm removed 3/23/23)
  # if (exists(field, TADA.env$UniqueValList) == FALSE) {
  #  stop("Input not recommened for function.")
  #}

  # subset UniqueValList by input
  df <- TADA.env$UniqueValList[[field]]
  
  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValues))

  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

  # create pie chart
  pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = FieldValues)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount),name = field) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() + # remove background, grid, numeric labels
    ggplot2::geom_text(ggplot2::aes(label = Count), color = "black", size=4, position = ggplot2::position_stack(vjust = 0.5))

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
#' specific fields to explore further and filter subset by a parameter.
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
    stop("Input parameter is not in the input dataframe.")
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
  
  #Filter col.names to include only fields recommended for filtering
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
  
  # cm removed, saving for potential future use in Shiny
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
      stop("The input parameter you entered is not in the input dataframe.")
    }
  }
  
  # execute function after checks are passed
  
  # refreshes UniqueValList
  FilterParList(.data)
  FilterParFields(.data, parameter)

  invisible(utils::capture.output(FilterParFields(.data, parameter)))
  
  # cm removed 
  # check that input is in ParUniqueValList
  if (exists(field, TADA.env$ParUniqueValList) == FALSE) {
    stop("The field you entered is not populated in the input dataframe.")
  }

  # subset UniqueValList by input
  df <- TADA.env$ParUniqueValList[[field]]

  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValues))

  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

  # create pie chart
  pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = FieldValues)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount), name = field) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() +
    ggplot2::labs(title = parameter) + 
    ggplot2::geom_text(ggplot2::aes(label = Count), color = "black", size=4, position = ggplot2::position_stack(vjust = 0.5))

  print(pie)
  print(df)
}
