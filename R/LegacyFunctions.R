# # ADD LEGACY FUNCTIONS BY COPYING THEM HERE AND REMOVING THEIR ROXYGEN CONTENT BEFORE COMMENTING OUT AGAIN
# 
# 
# # TADA.env <- new.env()
# 
# 
# # Generate list of field names
# # 
# # Function creates a list of common WQX fields that should be reviewed when
# # doing full data frame filtering, as well as the
# # number of unique values in each field. The list is intended to inform users
# # as to which specific fields to explore further and filter by.
# # 
# # The output will include counts for the following fields:
# # "ActivityTypeCode"
# # "TADA.ActivityMediaName"
# # "ActivityMediaSubdivisionName"
# # "ActivityCommentText"
# # "MonitoringLocationTypeName"
# # "StateName"
# # "TribalLandName"
# # "OrganizationFormalName"
# # "TADA.CharacteristicName"
# # "HydrologicCondition"
# # "HydrologicEvent"
# # "BiologicalIntentName"
# # "MeasureQualifierCode"
# # "ActivityGroup"
# # "AssemblageSampledName"
# # "ProjectName"
# # "CharacteristicNameUserSupplied"
# # "DetectionQuantitationLimitTypeName"
# # "SampleTissueAnatomyName"
# # "LaboratoryName"
# # 
# # param .data TADA dataframe
# # 
# # return A table of fields and the count of unique values in each field.
# # examples
# # # Load example dataset:
# # data(Data_Nutrients_UT)
# # 
# # # Create a table of fields and count of unique values in each field:
# # Fields_Data_Nutrients_UT <- FilterFields(Data_Nutrients_UT)
# # 
# 
# FilterFields <- function(.data) {
#   # check .data is data.frame
#   TADA_CheckType(.data, "data.frame", "Input object")
#   
#   # CREATE LIST OF FIELDS
#   # Find count of unique values in each column
#   col.names <- data.frame(Count = apply(.data, 2, function(x) length(unique(x))))
#   # Create "Fields" column from row names
#   col.names$Fields <- row.names(col.names)
#   # Remove row names
#   row.names(col.names) <- NULL
#   # Reorder columns
#   col.names <- col.names[, c(2, 1)]
#   
#   # Filter dataframe to include only recommended fields for filtering
#   # This does not limit the elements that can be used in other filter functions
#   # This only filters the table output of this function
#   col.names <- dplyr::filter(col.names, Fields %in% c(
#     "ActivityTypeCode", "TADA.ActivityMediaName",
#     "ActivityMediaSubdivisionName",
#     "ActivityCommentText", "MonitoringLocationTypeName",
#     "StateName", "TribalLandName",
#     "OrganizationFormalName", "TADA.CharacteristicName",
#     "HydrologicCondition", "HydrologicEvent",
#     "BiologicalIntentName", "MeasureQualifierCode",
#     "ActivityGroup", "AssemblageSampledName",
#     "ProjectName", "CharacteristicNameUserSupplied",
#     "DetectionQuantitationLimitTypeName",
#     "SampleTissueAnatomyName", "LaboratoryName"
#   ))
#   
#   # Reorder Count column in col.names from largest to smallest number
#   col.names <- col.names %>%
#     dplyr::arrange(desc(Count))
#   
#   # CREATE LIST OF UNIQUE VALUES PER FIELDS FROM DATAFRAME
#   
#   # remove fields with only NAs from df
#   df <- .data %>% dplyr::select(where(~ !all(is.na(.x))))
#   
#   # Get list of unique values per field
#   UniqueValList <- apply(df, 2, function(x) {
#     data.frame(table(list(x)))
#   })
#   
#   # Reorder Freq column in each data frame from largest to smallest number
#   UniqueValList <- lapply(UniqueValList, function(X) {
#     X[order(-X$Freq), ]
#   })
#   
#   # Rename fields
#   UniqueValList <- lapply(UniqueValList, stats::setNames, c("FieldValues", "Count"))
#   
#   # Filter list to include only recommended fields for filtering (cm removed 3/23/23)
#   # UniqueValList <- UniqueValList[c(
#   #  "ActivityTypeCode", "TADA.ActivityMediaName",
#   #  "ActivityMediaSubdivisionName",
#   #  "ActivityCommentText", "MonitoringLocationTypeName",
#   #  "StateName", "TribalLandName",
#   #  "OrganizationFormalName", "TADA.CharacteristicName",
#   #  "HydrologicCondition", "HydrologicEvent",
#   #  "BiologicalIntentName", "MeasureQualifierCode",
#   #  "ActivityGroup", "AssemblageSampledName",
#   #  "ProjectName", "CharacteristicNameUserSupplied",
#   #  "DetectionQuantitationLimitTypeName",
#   #  "SampleTissueAnatomyName", "LaboratoryName"
#   #)]
#   
#   TADA.env$UniqueValList <- UniqueValList
#   
#   print(col.names)
# }
# 
# 
# # Generate list of unique values in a given field
# # 
# # Function creates a table and pie chart of all unique values
# # (and their counts) in the input dataframe for the field specified.
# # 
# # Any field may be specified.
# # 
# # To start, we recommend reviewing any of the following fields:
# # "ActivityTypeCode"
# # "TADA.ActivityMediaName"
# # "ActivityMediaSubdivisionName"
# # "ActivityCommentText"
# # "MonitoringLocationTypeName"
# # "StateName"
# # "TribalLandName"
# # "OrganizationFormalName"
# # "TADA.CharacteristicName"
# # "HydrologicCondition"
# # "HydrologicEvent"
# # "BiologicalIntentName"
# # "MeasureQualifierCode"
# # "ActivityGroup"
# # "AssemblageSampledName"
# # "ProjectName"
# # "CharacteristicNameUserSupplied"
# # "DetectionQuantitationLimitTypeName"
# # "SampleTissueAnatomyName"
# # "LaboratoryName"
# # 
# # param field Field name
# # param .data Optional argument; TADA dataframe
# # 
# # return A table and pie chart of unique values in the selected field.
# # 
# # examples
# # # Load example dataset:
# # data(Data_Nutrients_UT)
# # 
# # # Create table and pie chart of "Hydrologic Condition" unique values and counts:
# # FieldReview_HydrologicCondition <- FilterFieldReview(field = "HydrologicCondition", Data_Nutrients_UT)
# 
# 
# FilterFieldReview <- function(field, .data) {
#   # if provided, check .data is data.frame
#   if (!missing(.data)) {
#     TADA_CheckType(.data, "data.frame", "Input object")
#   }
#   # execute function after checks are passed
#   
#   # refresh UniqueValList
#   invisible(utils::capture.output(FilterFields(.data)))
#   
#   # check that input is in UniqueValList
#   if (exists(field, TADA.env$UniqueValList) == FALSE) {
#     stop("The field entered into FilterFieldReview is not populated (all NA) in 
#          the input data frame.")
#   }
#   
#   # subset UniqueValList by input
#   df <- TADA.env$UniqueValList[[field]]
#   df$Legend = paste0(df$FieldValues, " - ", df$Count, " results")
#   
#   # define number of colors required for pie chart
#   colorCount <- length(unique(df$FieldValues))
#   
#   # define color palette
#   getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
#   
#   # create pie chart
#   # look into fixing display of counts for small slices: 
#   # https://stackoverflow.com/questions/28912059/labels-on-the-pie-chart-for-small-pieces-ggplot
#   pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = Legend)) +
#     ggplot2::scale_fill_manual(values = getPalette(colorCount),name = field) +
#     ggplot2::geom_bar(stat = "identity", width = 1) +
#     ggplot2::coord_polar("y", start = 0) +
#     ggplot2::theme_void() # remove background, grid, numeric labels
#   # ggplot2::geom_text(ggplot2::aes(label = Count), color = "black", size=4, position = ggplot2::position_stack(vjust = 0.5))
#   
#   print(pie)
#   print(df)
# }
# 
# 
# 
# # Generate list of parameters
# # 
# # Function generates list of characteristics in the input dataframe, as well as
# # the number of records for each. The list is intended to inform users
# # as to which parameters to explore further and filter.
# # 
# # param .data TADA dataframe
# # 
# # return A list of unique values in TADA.CharacteristicName and their counts
# # examples
# # # Load example dataset:
# # data(Data_Nutrients_UT)
# # 
# # # Create a list of parameters in the dataset and the number of records of
# # # each parameter:
# # ParameterList <- FilterParList(Data_Nutrients_UT)
# # 
# 
# FilterParList <- function(.data) {
#   # count the frequency of each value in TADA.CharactersticName field
#   ParValueCount <- data.frame(table(list(.data$TADA.CharacteristicName)))
#   
#   # Reorder Freq column from largest to smallest number
#   ParValueCount <- ParValueCount[order(-ParValueCount$Freq), ]
#   
#   # Rename fields
#   ParValueCount <- stats::setNames(ParValueCount, c("TADA.CharacteristicName",
#                                                     "Count"))
#   print(ParValueCount)
# }
# 
# # Generate list of field names subset by parameter
# # 
# # Function subsets the input dataframe by the input parameter and creates a
# # list of the fields in the subset dataframe as well as the number of unique
# # values in each field. The list is intended to inform users as to which
# # specific fields to explore further and filter (subset by a parameter).
# # 
# # Recommended fields to review and filter by for each parameter:
# # "ActivityCommentText"
# # "ActivityTypeCode"
# # "TADA.ActivityMediaName"
# # "ActivityMediaSubdivisionName"
# # "MeasureQualifierCode"
# # "MonitoringLocationTypeName"
# # "HydrologicCondition"
# # "HydrologicEvent"
# # "ResultStatusIdentifier"
# # "MethodQualifierTypeName"
# # "ResultCommentText"
# # "ResultLaboratoryCommentText"
# # "TADA.ResultMeasure.MeasureUnitCode"
# # "TADA.ResultSampleFractionText"
# # "ResultTemperatureBasisText"
# # "ResultValueTypeName"
# # "ResultWeightBasisText"
# # "SampleCollectionEquipmentName"
# # "LaboratoryName"
# # "MethodDescriptionText"
# # "ResultParticleSizeBasisText"
# # "SampleCollectionMethod.MethodIdentifier"
# # "SampleCollectionMethod.MethodIdentifierContext"
# # "SampleCollectionMethod.MethodName"
# # "DataQuality.BiasValue"
# # "MethodSpeciationName"
# # "ResultAnalyticalMethod.MethodName"
# # "ResultAnalyticalMethod.MethodIdentifier"
# # "ResultAnalyticalMethod.MethodIdentifierContext"
# # "AssemblageSampledName"
# # "DetectionQuantitationLimitTypeName"
# # 
# # param .data TADA dataframe
# # param parameter Characteristic name (parameter name) from the dataframe.
# # 
# # return A table of fields and the count of unique values in each field,
# # subset by a parameter.
# # 
# # examples
# # # Load example dataset:
# # data(Data_Nutrients_UT)
# # 
# # # Create list of fields for parameter "AMMONIA" with number of unique values in each field:
# # AmmoniaFields <- FilterParFields(Data_Nutrients_UT, parameter = "AMMONIA")
# # 
# 
# FilterParFields <- function(.data, parameter) {
#   # check .data is data.frame
#   TADA_CheckType(.data, "data.frame", "Input object")
#   # check .data has required columns
#   TADA_CheckColumns(.data, "TADA.CharacteristicName")
#   
#   # check parameter is in .data
#   if ((parameter %in% .data$TADA.CharacteristicName) == FALSE) {
#     stop("Input parameter for FilterParFields function is not in the 
#          input dataframe.")
#   }
#   
#   # SUBSET DATAFRAME; CREATE LIST OF FIELDS
#   df <- dplyr::filter(.data, TADA.CharacteristicName %in% parameter)
#   # Find count of unique values in each column
#   col.names <- data.frame(Count = apply(df, 2, function(x) length(unique(x))))
#   # Create "Fields" column from row names
#   col.names$Fields <- row.names(col.names)
#   # Remove row names
#   row.names(col.names) <- NULL
#   # Reorder columns
#   col.names <- col.names[, c(2, 1)]
#   
#   
#   # Filter col.names to include only fields recommended for filtering
#   # This does not limit the fields than can be entered in other filter 
#   # functions. It simply subsets the output table to a more manageable 
#   # number of fields for the user to review (subsets to suggestions)
#   col.names <- dplyr::filter(col.names, Fields %in% c(
#     "ActivityCommentText", "ActivityTypeCode",
#     "TADA.ActivityMediaName", "ActivityMediaSubdivisionName",
#     "MeasureQualifierCode", "MonitoringLocationTypeName",
#     "HydrologicCondition", "HydrologicEvent",
#     "ResultStatusIdentifier", "MethodQualifierTypeName",
#     "ResultCommentText", "ResultLaboratoryCommentText",
#     "TADA.ResultMeasure.MeasureUnitCode",
#     "TADA.ResultSampleFractionText", "ResultTemperatureBasisText",
#     "ResultValueTypeName", "ResultWeightBasisText",
#     "SampleCollectionEquipmentName", "LaboratoryName",
#     "MethodDescriptionText", "ResultParticleSizeBasisText",
#     "SampleCollectionMethod.MethodIdentifier",
#     "SampleCollectionMethod.MethodIdentifierContext",
#     "SampleCollectionMethod.MethodName",
#     "DataQuality.BiasValue", "MethodSpeciationName",
#     "ResultAnalyticalMethod.MethodName",
#     "ResultAnalyticalMethod.MethodIdentifier",
#     "ResultAnalyticalMethod.MethodIdentifierContext",
#     "AssemblageSampledName", "DetectionQuantitationLimitTypeName"
#   ))
#   
#   
#   # Reorder Count column in col.names from largest to smallest number
#   col.names <- col.names %>%
#     dplyr::arrange(desc(Count))
#   
#   
#   # CREATE LIST OF UNIQUE VALUES PER FIELD FROM DATAFRAME
#   # remove fields with only NAs from df (NA-only fields were removed from WQP, not .data)
#   df <- df %>% dplyr::select(where(~ !all(is.na(.x))))
#   # count the frequency of each value in all fields in df for selected characteristic
#   ParUniqueValList <- apply(df, 2, function(x) {
#     data.frame(table(list(x)))
#   })
#   # Reorder Freq column in each data frame from largest to smallest number
#   ParUniqueValList <- lapply(ParUniqueValList, function(x) {
#     x[order(-x$Freq), ]
#   })
#   # Rename fields
#   ParUniqueValList <- lapply(ParUniqueValList, stats::setNames, 
#                              c("FieldValues", "Count"))
#   
#   # cm removed so we do not limit users ability to enter other fields
#   # Filter list to require only these fields for filtering
#   #ParUniqueValList <- ParUniqueValList[c(
#   #  "ActivityCommentText", "ActivityTypeCode",
#   #  "TADA.ActivityMediaName", "ActivityMediaSubdivisionName",
#   #  "MeasureQualifierCode", "MonitoringLocationTypeName",
#   #  "HydrologicCondition", "HydrologicEvent",
#   #  "ResultStatusIdentifier", "MethodQualifierTypeName",
#   #  "ResultCommentText", "ResultLaboratoryCommentText",
#   #  "TADA.ResultMeasure.MeasureUnitCode",
#   #  "TADA.ResultSampleFractionText", "ResultTemperatureBasisText",
#   #  "ResultValueTypeName", "ResultWeightBasisText",
#   #  "SampleCollectionEquipmentName", "LaboratoryName",
#   #  "MethodDescriptionText", "ResultParticleSizeBasisText",
#   #  "SampleCollectionMethod.MethodIdentifier",
#   #  "SampleCollectionMethod.MethodIdentifierContext",
#   #  "SampleCollectionMethod.MethodName",
#   #  "DataQuality.BiasValue", "MethodSpeciationName",
#   #  "ResultAnalyticalMethod.MethodName",
#   #  "ResultAnalyticalMethod.MethodIdentifier",
#   #  "ResultAnalyticalMethod.MethodIdentifierContext",
#   #  "AssemblageSampledName", "DetectionQuantitationLimitTypeName", 
#   #  "MonitoringLocationIdentifier"
#   #)]
#   
#   TADA.env$ParUniqueValList <- ParUniqueValList
#   print(col.names)
# }
# 
# # Generate list of unique values in a given field subset by parameter
# # 
# # Function creates a table and pie chart of unique values, and counts of those
# # values, for a chosen field in a dataframe subset by parameter.
# # 
# # Recommended fields to review and filter by for each parameter:
# # "ActivityCommentText"
# # "ActivityTypeCode"
# # "TADA.ActivityMediaName"
# # "ActivityMediaSubdivisionName"
# # "MeasureQualifierCode"
# # "MonitoringLocationTypeName"
# # "HydrologicCondition"
# # "HydrologicEvent"
# # "ResultStatusIdentifier"
# # "MethodQualifierTypeName"
# # "ResultCommentText"
# # "ResultLaboratoryCommentText"
# # "TADA.ResultMeasure.MeasureUnitCode"
# # "TADA.ResultSampleFractionText"
# # "ResultTemperatureBasisText"
# # "ResultValueTypeName"
# # "ResultWeightBasisText"
# # "SampleCollectionEquipmentName"
# # "LaboratoryName"
# # "MethodDescriptionText"
# # "ResultParticleSizeBasisText"
# # "SampleCollectionMethod.MethodIdentifier"
# # "SampleCollectionMethod.MethodIdentifierContext"
# # "SampleCollectionMethod.MethodName"
# # "DataQuality.BiasValue"
# # "MethodSpeciationName"
# # "ResultAnalyticalMethod.MethodName"
# # "ResultAnalyticalMethod.MethodIdentifier"
# # "ResultAnalyticalMethod.MethodIdentifierContext"
# # "AssemblageSampledName"
# # "DetectionQuantitationLimitTypeName"
# # 
# # param field Field name
# # param .data Optional argument; TADA dataframe
# # param parameter Characteristic name (parameter name) from the dataframe.
# # 
# # return A table and pie chart of unique values in the selected field.
# # 
# # examples
# # # Load example dataset:
# # data(Data_Nutrients_UT)
# # 
# # # Create table and pie chart of monitoring locations for the parameter "AMMONIA" in dataframe:
# # AmmoniaMonitoringLocations <- FilterParFieldReview(field = "MonitoringLocationIdentifier", Data_Nutrients_UT, parameter = "AMMONIA")
# # 
# 
# FilterParFieldReview <- function(field, .data, parameter) {
#   # if provided, check .data is data.frame
#   if (!missing(.data)) {
#     TADA_CheckType(.data, "data.frame", "Input object")
#   }
#   # check parameter is in .data
#   if (!missing(parameter)) {
#     if ((parameter %in% .data$TADA.CharacteristicName) == FALSE) {
#       stop("The parameter entered into FilterParFieldReview is not in the input 
#            dataframe.")
#     }
#   }
#   
#   # execute function after checks are passed
#   
#   # refreshes UniqueValList
#   FilterParList(.data)
#   FilterParFields(.data, parameter)
#   
#   invisible(utils::capture.output(FilterParFields(.data, parameter)))
#   
#   # check that input is in ParUniqueValList
#   if (exists(field, TADA.env$ParUniqueValList) == FALSE) {
#     stop("The field entered into FilterParFieldReview is NOT available 
#     (or all NA) for the parameter entered in the input dataframe.")
#   }
#   
#   # subset UniqueValList by input
#   df <- TADA.env$ParUniqueValList[[field]]
#   df$Legend = paste0(df$FieldValues, " - ", df$Count, " results")
#   
#   # define number of colors required for pie chart
#   colorCount <- length(unique(df$FieldValues))
#   
#   # define color palette
#   getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
#   
#   # create pie chart
#   pie <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = Legend)) +
#     ggplot2::scale_fill_manual(values = getPalette(colorCount), name = field) +
#     ggplot2::geom_bar(stat = "identity", width = 1) +
#     ggplot2::coord_polar("y", start = 0) +
#     ggplot2::theme_void() +
#     ggplot2::labs(title = parameter) #+ 
#   # ggplot2::geom_text(ggplot2::aes(label = Count), color = "black", size=4, position = ggplot2::position_stack(vjust = 0.5))
#   
#   print(pie)
#   print(df)
# }
# 
# # Check for Special Characters in Measure Value Fields - Deprecated
# # 
# # Function checks for special characters and non-numeric values in the
# # ResultMeasureValue and DetectionQuantitationLimitMeasure.MeasureValue
# # fields and appends flag columns indicating if special characters are included
# # and if so, what the special characters are. The ResultMeasureValue and
# # DetectionQuantitationLimitMeasure.MeasureValue fields are also converted to
# # class numeric. This function is deprecated, please use TADA_ConvertSpecialChars() function.
# # 
# # param .data TADA dataframe
# # 
# # return Full dataframe with column indicating presence of special characters in
# # the ResultMeasureValue and DetectionQuantitationLimitMeasure.MeasureValue
# # fields. Additionally, the ResultMeasureValue and
# # DetectionQuantitationLimitMeasure.MeasureValue fields are converted to class
# # numeric, and copies of each column are created to preserve original
# # character values.
# 
# 
# MeasureValueSpecialCharacters <- function(.data) {
#   
#   warning("This function is deprecated and does not return the correct column names. Please use TADA_ConvertSpecialChars() function instead.")
#   
#   # check .data is data.frame
#   TADA_CheckType(.data, "data.frame", "Input object")
#   
#   # .data required columns
#   required_cols <- c("ResultMeasureValue", "DetectionQuantitationLimitMeasure.MeasureValue")
#   # check .data has required columns
#   TADA_CheckColumns(.data, required_cols)
#   
#   # execute function after checks are passed
#   # define check.data
#   check.data <- .data
#   
#   # copy MeasureValue columns to MeasureValue.Original
#   check.data$ResultMeasureValue.Original <- check.data$ResultMeasureValue
#   check.data$DetectionLimitMeasureValue.Original <-
#     check.data$DetectionQuantitationLimitMeasure.MeasureValue
#   
#   # add TADA.ResultMeasureValue.Flag column
#   flag.data <- check.data %>%
#     # apply function row by row
#     dplyr::rowwise() %>%
#     # create flag column
#     dplyr::mutate(TADA.ResultMeasureValue.Flag = dplyr::case_when(
#       is.na(ResultMeasureValue.Original) ~ as.character("ND or NA"),
#       (grepl("<", ResultMeasureValue.Original) == TRUE) ~ as.character("Less Than"),
#       (grepl(">", ResultMeasureValue.Original) == TRUE) ~ as.character("Greater Than"),
#       (grepl("~", ResultMeasureValue.Original) == TRUE) ~ as.character("Approximate Value"),
#       (grepl("[A-Za-z]", ResultMeasureValue.Original) == TRUE) ~ as.character("Text"),
#       (grepl("\\d", ResultMeasureValue.Original) == TRUE) ~ as.character("Numeric"),
#       TRUE ~ "Coerced to NA"
#     ))
#   
#   # add TADA.DetectionLimitMeasureValue.Flag column
#   flag.data <- flag.data %>%
#     # apply function row by row
#     dplyr::rowwise() %>%
#     # create flag column
#     dplyr::mutate(TADA.DetectionLimitMeasureValue.Flag = dplyr::case_when(
#       is.na(DetectionLimitMeasureValue.Original) ~ as.character(NA),
#       (grepl("<", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Less Than"),
#       (grepl(">", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Greater Than"),
#       (grepl("~", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Approximate Value"),
#       (grepl("[A-Za-z]", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Text"),
#       (grepl("\\d", DetectionLimitMeasureValue.Original) == TRUE) ~ as.character("Numeric"),
#       TRUE ~ "Coerced to NA"
#     ))
#   
#   # remove special characters before converting to numeric
#   flag.data$ResultMeasureValue <- stringr::str_replace_all(
#     flag.data$ResultMeasureValue,
#     c("<" = "", ">" = "", "~" = "", "," = "")
#   )
#   flag.data$DetectionQuantitationLimitMeasure.MeasureValue <- stringr::str_replace_all(
#     flag.data$DetectionQuantitationLimitMeasure.MeasureValue,
#     c("<" = "", ">" = "", "~" = "", "," = "")
#   )
#   
#   # change measure value columns to numeric
#   # rename df
#   clean.data <- flag.data
#   # ResultMeasureValue
#   clean.data$ResultMeasureValue <- suppressWarnings(
#     as.numeric(clean.data$ResultMeasureValue)
#   )
#   # DetectionQuantitationLimitMeasure.MeasureValue
#   clean.data$DetectionQuantitationLimitMeasure.MeasureValue <-
#     suppressWarnings(as.numeric(clean.data$DetectionQuantitationLimitMeasure.MeasureValue))
#   
#   # reorder columns
#   # place flag column next to relevant fields
#   clean.data <- clean.data %>%
#     dplyr::relocate("ResultMeasureValue.Original",
#                     .after = "ResultMeasureValue"
#     ) %>%
#     dplyr::relocate("TADA.ResultMeasureValue.Flag",
#                     .after = "ResultMeasureValue.Original"
#     ) %>%
#     dplyr::relocate("DetectionLimitMeasureValue.Original",
#                     .after = "DetectionQuantitationLimitMeasure.MeasureValue"
#     ) %>%
#     dplyr::relocate("TADA.DetectionLimitMeasureValue.Flag",
#                     .after = "DetectionLimitMeasureValue.Original"
#     )
#   return(clean.data)
# }
# 
# #Identify Potential Duplicate Data Uploads
# # 
# # Identifies data records uploaded by different organizations with the same date,
# #time, characteristic name, and result value within X meters of each other and
# # flags as potential duplicates. However, it is at the discretion of the data user
# # to determine if the data records are unique or represent overlap that could cause
# # issues in the data analysis.
# # 
# # @param .data TADA dataframe
# # @param dist_buffer Numeric. The distance in meters below which two sites with 
# # measurements at the same time on the same day of the same parameter will
# # be flagged as potential duplicates.
# # 
# # @return The same TADA dataframe with two additional columns, a duplicate flag
# # column, and a distance between sites (in meters) column.
# #
# # @export
# # 
# 
# identifyPotentialDuplicates <- function(.data, dist_buffer = 100){
#   dat = .data
#   dups = dat%>%dplyr::filter(!is.na(ResultMeasureValue))%>%dplyr::mutate(roundRV = round(ResultMeasureValue,digits=2))%>%dplyr::group_by(ActivityStartDate, ActivityStartTime.Time, CharacteristicName,ResultMeasureValue)%>%dplyr::summarise(numorgs = length(unique(OrganizationIdentifier)))%>%dplyr::filter(numorgs>1)
#   dups$dup_id = seq(1:dim(dups)[1])
#   
#   tdups = dplyr::left_join(dups, dat)
#   tdups$LatitudeMeasure = as.numeric(tdups$LatitudeMeasure)
#   tdups$LongitudeMeasure = as.numeric(tdups$LongitudeMeasure)
#   
#   distances = tdups%>%dplyr::ungroup()%>%dplyr::select(dup_id,LatitudeMeasure,LongitudeMeasure)
#   dcoords = sf::st_as_sf(x = distances, coords = c("LongitudeMeasure","LatitudeMeasure"), crs="EPSG:4326")
#   
#   dists = data.frame()
#   for(i in 1:max(dcoords$dup_id)){
#     ds = subset(dcoords, dcoords$dup_id==i)
#     dist = as.numeric(sf::st_distance(ds$geometry[1],ds$geometry[2]))
#     dsdist = data.frame(dup_id = i, distance_m = as.numeric(dist))
#     dsdist$TADA.idPotentialDuplicates.Flag = ifelse(dist<=dist_buffer,"POTENTIAL DUPLICATE DATAPOINT",NA)
#     dists = rbind(dists, dsdist)
#   }
#   
#   tdups1 = merge(tdups, dists, all.x = TRUE)
#   tdups1 = tdups1[,!names(tdups1)%in%c("dup_id","numorgs")]
#   dat1 = merge(dat, tdups1, all.x = TRUE)
#   
#   return(dat1)
# }
# 
# 
# # AutoFilter
# #
# # Function can be used to autofilter and simplify a WQP dataframe.
# # After applying this function, the dataframe will only contain result values for
# # water media types or chemicals in tissue (e.g. mercury in fish tissue).
# # More complex biological data (counts and macroinvertebrates) is removed.
# # The function looks at the following fields to autofilter:
# # ActivityMediaName, ActivityMediaSubDivisionName, AssemblageSampledName
# #
# # @param .data TADA dataframe
# # @param clean Indicates whether flag columns should be appended to the data
# # (clean = FALSE), or flagged data is transformed/filtered from the
# # dataframe and no columns are appended (clean = TRUE).
# #
# # @return When clean = FALSE, flag column is appended to the dataframe. When
# # clean = TRUE, flag column is not appended and relevant rows are removed.
# #
# # 
# # AutoFilter <- function(.data, clean = TRUE) {
# #   field.names <- colnames(.data)
# #   
# #   if (TADAprofileCheck(.data) == FALSE) {
# #     stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
# #   }
# #   
# #   if (TADAprofileCheck(.data) == TRUE) {
# #     if (clean == TRUE) {
# #       # Remove all data where media name does NOT equal WATER (ignore punctuation)
# #       cleandata <- dplyr::filter(.data, ActivityMediaName == "Water")
# #       
# #       return(cleandata)
# #     }
# #     
# #     if (clean == FALSE) {
# #       # NEED TO EDIT TO ADD FLAGS, currently removes other water Water
# #       flagdata <- dplyr::filter(.data, ActivityMediaName == "water")
# #       
# #       return(flagdata)
# #     } else {
# #       stop("'clean' argument must be Boolean (TRUE or FALSE)")
# #     }
# #   }
# # }
# #
# 
# 
# 
# # TADA Profile Check
# #
# # This function checks if the column names in a dataframe include the TADA
# # profile fields. It is used at the beginning of TADA functions to ensure the
# # input data frame is suitable (i.e. is either the full physical/chemical
# # results profile downloaded from WQP or the TADA profile template downloaded
# # from the EPA TADA webpage.)
# #
# # @param .data A dataframe
# #
# # @return Boolean result indicating whether or not the input dataframe contains
# # all of the TADA profile fields.
# #
# 
# TADAprofileCheck <- function(.data) {
#   TADA.fields <- c(
#     "OrganizationIdentifier", "OrganizationFormalName",
#     "ActivityIdentifier", "ActivityTypeCode",
#     "ActivityMediaName", "ActivityMediaSubdivisionName",
#     "ActivityStartDate", "ActivityStartTime.Time",
#     "ActivityStartTime.TimeZoneCode", "ActivityEndDate",
#     "ActivityEndTime.Time", "ActivityEndTime.TimeZoneCode",
#     "ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode",
#     "ActivityDepthAltitudeReferencePointText", "ActivityTopDepthHeightMeasure.MeasureValue",
#     "ActivityTopDepthHeightMeasure.MeasureUnitCode", "ActivityBottomDepthHeightMeasure.MeasureValue",
#     "ActivityBottomDepthHeightMeasure.MeasureUnitCode", "ProjectIdentifier",
#     "ActivityConductingOrganizationText", "MonitoringLocationIdentifier",
#     "ActivityCommentText", "SampleAquifer",
#     "HydrologicCondition", "HydrologicEvent",
#     "SampleCollectionMethod.MethodIdentifier", "SampleCollectionMethod.MethodIdentifierContext",
#     "SampleCollectionMethod.MethodName", "SampleCollectionEquipmentName",
#     "ResultDetectionConditionText", "CharacteristicName",
#     "ResultSampleFractionText", "ResultMeasureValue",
#     "ResultMeasure.MeasureUnitCode", "MeasureQualifierCode",
#     "ResultStatusIdentifier", "StatisticalBaseCode",
#     "ResultValueTypeName", "ResultWeightBasisText",
#     "ResultTimeBasisText", "ResultTemperatureBasisText",
#     "ResultParticleSizeBasisText", "PrecisionValue",
#     "ResultCommentText", "USGSPCode",
#     "ResultDepthHeightMeasure.MeasureValue", "ResultDepthHeightMeasure.MeasureUnitCode",
#     "ResultDepthAltitudeReferencePointText", "SubjectTaxonomicName",
#     "SampleTissueAnatomyName", "ResultAnalyticalMethod.MethodIdentifier",
#     "ResultAnalyticalMethod.MethodIdentifierContext", "ResultAnalyticalMethod.MethodName",
#     "MethodDescriptionText", "LaboratoryName",
#     "AnalysisStartDate", "ResultLaboratoryCommentText",
#     "DetectionQuantitationLimitTypeName", "DetectionQuantitationLimitMeasure.MeasureValue",
#     "DetectionQuantitationLimitMeasure.MeasureUnitCode", "PreparationStartDate",
#     "ProviderName", "ActivityStartDateTime", "ActivityEndDateTime"
#   )
#   
#   if (("data.frame" %in% class(.data)) == FALSE) {
#     stop("Input object must be of class 'data.frame'")
#   }
#   
#   if (all(TADA.fields %in% colnames(.data)) == TRUE) {
#     TRUE
#   } else {
#     stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
#   }
# }
# 
# # Check for Potential Duplicates
# # 
# # Sometimes multiple organizations submit the exact same data set to the
# # Water Quality Portal (WQP), which can affect water quality analyses. This
# # function checks for and identifies data that is identical in all fields
# # excluding organization-specific and comment text fields. When clean = FALSE,
# # a column titled "TADA.PotentialDupRowIDs.Flag" is added to the dataframe which
# # assigns each pair of potential duplicates a unique ID for review.
# # When clean = TRUE, the function retains the first occurrence of each
# # potential duplicate in the dataframe. Default is clean = TRUE.
# # 
# # @param .data TADA dataframe
# # @param clean Boolean argument; removes potential duplicate data from
# # the dataframe when clean = TRUE. When clean = FALSE,
# # a column titled "TADA.PotentialDupRowIDs.Flag" is added to the dataframe which
# # assigns each pair of potential duplicates a unique ID for review.
# # Default is clean = TRUE.
# # @param flaggedonly Boolean argument; filters dataframe to show only potential
# # duplicate rows of data when flaggedonly = TRUE. Default is flaggedonly = FALSE.
# # 
# # @return When clean = FALSE, the following column will be added to you dataframe:
# # TADA.PotentialDupRowIDs.Flag. This column flags potential duplicate rows of data
# # in your dataframe, and assigns each potential duplicate combination a unique number
# # linking the two potential duplication rows. When clean = FALSE the first of
# # each group of potential duplicate rows will be removed from the dataframe
# # and no column is appended.
# # 
# # @export
# #
# # @examples
# # # Load example dataset:
# # data(Data_Nutrients_UT)
# # 
# # # Remove potential duplicate data from dataframe:
# # PotentialDup_clean <- PotentialDuplicateRowID(Data_Nutrients_UT)
# #
# # # Flag, but do not remove, potential duplicate data in new column titled "TADA.PotentialDupRowIDs.Flag":
# # PotentialDup_flagcolumnadded <- PotentialDuplicateRowID(Data_Nutrients_UT, clean = FALSE)
# # 
# # # Flag and review potential duplicate data only:
# # PotentialDup_reviewduplicatesonly <- PotentialDuplicateRowID(Data_Nutrients_UT, clean = FALSE, flaggedonly = TRUE)
# 
# PotentialDuplicateRowID <- function(.data, clean = TRUE, flaggedonly = FALSE) {
#   # check .data is data.frame
#   TADA_CheckType(.data, "data.frame", "Input object")
#   # check clean is boolean
#   TADA_CheckType(clean, "logical")
#   # check flaggedonly is boolean
#   TADA_CheckType(flaggedonly, "logical")
#   # check .data has required columns
#   required_cols <- c(
#     "ActivityIdentifier", "ActivityConductingOrganizationText",
#     "OrganizationFormalName", "OrganizationIdentifier",
#     "ProjectIdentifier", "ResultCommentText",
#     "ActivityCommentText"
#   )
#   TADA_CheckColumns(.data, required_cols)
#   # check that clean and flaggedonly are not both TRUE
#   if (clean == TRUE & flaggedonly == TRUE) {
#     stop("Function not executed because clean and flaggedonly cannot both be TRUE")
#   }
#   
#   # execute function after checks are passed
#   # get list of field names in .data
#   field.names <- colnames(.data)
#   # create list of fields to exclude when looking for duplicate rows
#   excluded.fields <- c(
#     "ActivityIdentifier", "ActivityConductingOrganizationText",
#     "OrganizationFormalName", "OrganizationIdentifier",
#     "ProjectIdentifier", "ResultCommentText", "ActivityCommentText"
#   )
#   # create list of fields to check for duplicates across
#   dupe.fields <- field.names[!field.names %in% excluded.fields]
#   
#   # subset list of duplicate rows
#   dupe.data <- .data[duplicated(.data[dupe.fields]), ]
#   
#   # if no potential duplicates are found
#   if (nrow(dupe.data) == 0) {
#     if (flaggedonly == FALSE) {
#       print("No potential duplicates found in your dataframe.")
#       .data <- TADA_OrderCols(.data)
#       return(.data)
#     }
#     if (flaggedonly == TRUE) {
#       print("This dataframe is empty because we did not find any potential duplicates in your dataframe")
#       dupe.data <- TADA_OrderCols(dupe.data)
#       return(dupe.data)
#     }
#   }
#   
#   # if potential duplicates are found
#   if (nrow(dupe.data) != 0) {
#     
#     # flag potential duplicates
#     dupe.data$TADA.PotentialDupRowIDs.Flag <- as.integer(seq_len(nrow(dupe.data)))
#     
#     # merge flag column into .data
#     flag.data <- merge(.data, dupe.data, by = dupe.fields, all.x = TRUE)
#     
#     # remove extraneous columns, fix field names
#     flag.data <- flag.data %>%
#       # remove ".x" suffix from column names
#       dplyr::rename_at(
#         dplyr::vars(dplyr::ends_with(".x")),
#         ~ stringr::str_replace(., "\\..$", "")
#       ) %>%
#       # remove columns with ".y" suffix
#       dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
#     
#     # flagged output, all data
#     if (clean == FALSE & flaggedonly == FALSE) {
#       flag.data <- TADA_OrderCols(flag.data)
#       return(flag.data)
#     }
#     
#     # clean output
#     if (clean == TRUE & flaggedonly == FALSE) {
#       # remove duplicate rows
#       # seperate data into 2 dataframes by TADA.PotentialDupRowIDs.Flag (no NAs and NAs)
#       dup.data <- flag.data[!is.na(flag.data$TADA.PotentialDupRowIDs.Flag), ]
#       NAdup.data <- flag.data[is.na(flag.data$TADA.PotentialDupRowIDs.Flag), ]
#       
#       nodup.data <- dup.data[!duplicated(dup.data$TADA.PotentialDupRowIDs.Flag), ]
#       
#       clean.data <- rbind(nodup.data, NAdup.data)
#       
#       # remove TADA.PotentialDupRowID column
#       clean.data <- dplyr::select(clean.data, -TADA.PotentialDupRowIDs.Flag)
#       clean.data <- TADA_OrderCols(clean.data)
#       return(clean.data)
#     }
#     
#     # flagged data, errors only
#     if (clean == FALSE & flaggedonly == TRUE) {
#       # filter to show duplicate data only
#       dup.data <- flag.data[!is.na(flag.data$TADA.PotentialDupRowIDs.Flag), ]
#       dup.data <- TADA_OrderCols(dup.data)
#       return(dup.data)
#     }
#   }
# }
