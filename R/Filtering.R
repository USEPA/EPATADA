TADA.env <- new.env()

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

FilterFields <- function(.data) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")

  # CREATE LIST OF FIELDS
  # Find count of unique values in each column
  col.names <- data.frame(Count = apply(.data, 2, function(x) length(unique(x))))
  # Create "FieldName" column from row names
  col.names$FieldName <- row.names(col.names)
  # Remove row names
  row.names(col.names) <- NULL
  # Reorder columns
  col.names <- col.names[, c(2, 1)]
  # Filter dataframe to include only fields for filtering
  col.names <- dplyr::filter(col.names, FieldName %in% c(
    "ActivityTypeCode", "ActivityMediaName",
    "ActivityMediaSubdivisionName",
    "ActivityCommentText", "MonitoringLocationTypeName",
    "StateName", "TribalLandName",
    "OrganizationFormalName", "CharacteristicName",
    "HydrologicCondition", "HydrologicEvent",
    "BiologicalIntentName", "MeasureQualifierCode",
    "ActivityGroup", "AssemblageSampledName",
    "ProjectName", "CharacteristicNameUserSupplied",
    "DetectionQuantitationLimitTypeName",
    "SampleTissueAnatomyName", "LaboratoryName"
  ))

  # CREATE LIST OF UNIQUE VALUES PER FIELD FROM DATAFRAME

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
  UniqueValList <- lapply(UniqueValList, stats::setNames, c("FieldValue", "Count"))

  # Filter list to include only fields for filtering
  UniqueValList <- UniqueValList[c(
    "ActivityTypeCode", "ActivityMediaName",
    "ActivityMediaSubdivisionName",
    "ActivityCommentText", "MonitoringLocationTypeName",
    "StateName", "TribalLandName",
    "OrganizationFormalName", "CharacteristicName",
    "HydrologicCondition", "HydrologicEvent",
    "BiologicalIntentName", "MeasureQualifierCode",
    "ActivityGroup", "AssemblageSampledName",
    "ProjectName", "CharacteristicNameUserSupplied",
    "DetectionQuantitationLimitTypeName",
    "SampleTissueAnatomyName", "LaboratoryName"
  )]

  TADA.env$UniqueValList <- UniqueValList
  print(col.names)
}

#' Generate list of unique values in a given field
#'
#' Function creates a table and pie chart of unique values, and counts of those
#' values for a chosen field in a dataframe.
#'
#' @param field Field name
#' @param .data Optional argument; TADA dataframe
#'
#' @return A table and pie chart of unique values in the selected field.
#'
#' @export
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
    stop("Input does not exist as a field in the dataframe.")
  }

  # subset UniqueValList by input
  df <- TADA.env$UniqueValList[[field]]

  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValue))

  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

  # create pie chart
  pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = FieldValue)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount)) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void()

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
#' @return A list of unique characteristics and their counts
#'
#' @export
#'
FilterParList <- function(.data) {
  # count the frequency of each value in CharactersticName field
  ParValueCount <- data.frame(table(list(.data$CharacteristicName)))
  # Reorder Freq column from largest to smallest number
  ParValueCount <- ParValueCount[order(-ParValueCount$Freq), ]
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
#' @param parameter Characteristic name (parameter name) from the dataframe.
#'
#' @return A table of fields and the count of unique values in each field,
#' subset by a parameter.
#'
#' @export
#'

FilterParFields <- function(.data, parameter) {
  # check .data is data.frame
  checkType(.data, "data.frame", "Input object")
  # check .data has required columns
  checkColumns(.data, "CharacteristicName")
  
  # check parameter is in .data
  if ((parameter %in% .data$CharacteristicName) == FALSE) {
    stop("Input parameter is not in the input dataframe.")
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
  col.names <- col.names[, c(2, 1)]
  # Filter col.names to include only fields for filtering
  col.names <- dplyr::filter(col.names, FieldName %in% c(
    "ActivityCommentText", "ActivityTypeCode",
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
  ParUniqueValList <- lapply(ParUniqueValList, stats::setNames, c("FieldValue", "Count"))
  # Filter list to include only fields for filtering
  ParUniqueValList <- ParUniqueValList[c(
    "ActivityCommentText", "ActivityTypeCode",
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
    "AssemblageSampledName", "DetectionQuantitationLimitTypeName"
  )]


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

FilterParFieldReview <- function(field, .data, parameter) {
  # if provided, check .data is data.frame
  if (!missing(.data)) {
    checkType(.data, "data.frame", "Input object")
  }
  # check parameter is in .data
  if (!missing(parameter)) {
    if ((parameter %in% .data$CharacteristicName) == FALSE) {
      stop("Input parameter is not in the input dataframe.")
    }
  }
  # execute function after checks are passed

  # refresh UniqueValList
  invisible(utils::capture.output(FilterParFields(.data, parameter)))
  # check that input is in ParUniqueValList
  if (exists(field, TADA.env$ParUniqueValList) == FALSE) {
    stop("Input does not exist as a field in the dataframe.")
  }

  # subset UniqueValList by input
  df <- TADA.env$ParUniqueValList[[field]]

  # define number of colors required for pie chart
  colorCount <- length(unique(df$FieldValue))

  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

  # create pie chart
  pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = FieldValue)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount)) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void()

  print(pie)
  print(df)
}
