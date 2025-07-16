###################
## MODIFY RENAME COLUMN FUNCTION
##  referencing updated crosswalk table (temporary)
##
## 4/4/2025
## 6/26/2025
###################

# load most recent packages in development
library(remotes)

library(dataRetrieval) #dataRetrieval 2.7.19  OLD dataRetrieval 2.7.17.9000 #Extended Documentation: https://doi-usgs.github.io/dataRetrieval

library(devtools)
load_all()

library(tidyverse)
library(readr)
library(data.table)

## Read modified crosswalk table - linked to the EPA Water Quality Portal Quick Reference site
# https://www.epa.gov/waterdata/water-quality-portal-quick-reference-guide
# UPDATED 6/26/2025
wqxnames <- readr::read_csv("https://www.epa.gov/system/files/other-files/2025-07/schema_outbound_wqx3.0.csv")
#wqxnames <- readr::read_csv("https://www.epa.gov/system/files/other-files/2024-07/schema_outbound_wqx3.0.csv")
#wqxcrswlk_mod <- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/UpdatedSchema/Updated_2025_0626/schema_outbound_WQX3.0.csv")

## Load WQ data using beta services
result.tada <- TADA_DataRetrieval(siteid = "USGS-04024315",
                                  #startDate = "1975-01-01",
                                  #endDate = "1978-12-31",
                                  characteristicName = "Temperature, water",
                                  applyautoclean = FALSE)

##################
## PROCESS DATA
##################
# Manually changing crosswalk table to better suit TADA objectives
#  In particular - Changing the 'see Comments field' to useful names - relevant to dataRetrieval and TADA
#                - Making some legacy names NA (would have been called BinaryObjectFileName and BinaryObjectFileTypeCode)
wqxcrswlk_mod2 <- wqxcrswlk_mod |> 
  mutate(WqxV2.FieldName = case_when( #existing in 3.0 column ~ change to in 2.0 column
    FieldName3.0 == "SampleCollectionMethod_Description" ~ "SampleCollectionMethod/MethodDescriptionText",
    FieldName3.0 == "DataQuality_PrecisionValue" ~ "DataQuality/PrecisionValue",
    FieldName3.0 == "DataQuality_ConfidenceIntervalValue" ~ "DataQuality/ConfidenceIntervalValue",
    FieldName3.0 == "DataQuality_UpperConfidenceLimitValue" ~ "DataQuality/UpperConfidenceLimitValue",
    FieldName3.0 == "DataQuality_LowerConfidenceLimitValue" ~ "DataQuality/LowerConfidenceLimitValue",
    FieldName3.0 == "ResultAnalyticalMethod_Description" ~ "ResultAnalyticalMethod/MethodDescriptionText",
    FieldName3.0 == "Location_Latitude" ~ "LatitudeMeasure",  # Changing to what is returned in legacy Site profile 
    FieldName3.0 == "Location_Longitude" ~ "LongitudeMeasure", # Changing to what is returned in legacy Site profile 
    FieldName3.0 == "Location_HorzCoordReferenceSystemDatum" ~ "HorizontalCoordinateReferenceSystemDatumName", # Changing to what is returned in legacy Site profile 
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
  ))

# Remove NAs from crosswalk table 
wqxcrswlk_mod3 <- wqxcrswlk_mod2 |> 
   filter(!is.na(WqxV2.FieldName))

###############
# Create vectors of WQX3.0 and WQX2.0 (Legacy) column names
##  These need to line up and be equal length
beta_names = wqxcrswlk_mod3$FieldName3.0
legacy_names = wqxcrswlk_mod3$WqxV2.FieldName

# Check if same length
if (length(beta_names) != length(legacy_names)) {
  stop("`old names` and `new names` must be the same length", call. = FALSE)
}

###############
## Rename columns from USGS dataRetrieval pull
# There is a bug with data.table::setnames - it modifies inputs (even original file) - does not save a copy but modifies directly
# https://www.canallc.com/post/pitfalls-with-using-the-data-table-package


####################
## USGS dataRetrieval n = 63 at USGS site 04024315 for water temperature
WQPquery <- list(siteid = "USGS-04024315",
                 #startDate = "1975-01-01",
                 #endDate = "1978-12-31",
                 characteristicName = "Temperature, water"
                 
)

result.DR <- dataRetrieval::readWQPdata(WQPquery,
                                        service = "ResultWQX3", # WQX3 option "ResultWQX3",
                                        dataProfile = "fullPhysChem", #"fullPhysChem", "basicPhysChem", "narrow"
                                        ignore_attributes = TRUE)

names(result.DR)

df <- result.DR

###############
## RENAME COLUMNS FROM BETA BACK TO LEGACY
#https://stackoverflow.com/questions/29380447/using-data-tablesetnames-when-some-column-names-might-not-be-present 
# setnames is part of data.table package - https://cran.r-project.org/web/packages/data.table/data.table.pdf 
#  will change column names (no copy is made)
# setnames(data.frame, old, new) 
#  old = character names to change (I provide this as a list object - names from the schema table) 
#  new = new column names - this must be the same length as columns provided to old
#  skip_absent - skip items in old that are missing in names(x)
# Need to make sure to drop from the crosswalk table NAs for legacy names 
df <- df |> 
  data.table::setnames(beta_names,legacy_names, skip_absent = TRUE)

#df <- result.DR |> 
#  rename(any_of(stats::setNames(legacy_names,
#                                beta_names))) 

###############
## REPLACE SPECIAL CHARACTERS TO MATCH TADA FORMAT
# Only apply to subset of columns that were beta that had legacy names
# and remove column names that are not in the dataRetrieval call - see below

####
# To process data for renaming special characters - need to remove unmatching names in schema table that don't have a paterner in the dataRetrieval output
# Make column headings a vector
beta_DR <- as.data.frame(colnames(result.DR)) |> 
  rename("beta" = 1) # Give the first column the name "FieldName3.0"

# Select schema names of interest from crosswalk table
wqx_red <- wqxcrswlk_mod2 |> 
  select(FieldName3.0,WqxV2.FieldName) |> 
  mutate(WqxV3.FieldName = FieldName3.0)

# Join dataRetrieval output using beta service with modified crosswalk table - left join
test <- left_join(beta_DR, wqx_red, by = c("beta" = "FieldName3.0"))
# List of legacy names that are returned in dataRetrieval
cols_wqx2_legacy <- test$WqxV2.FieldName

# Fields to remove
 drop_fields <- c("ActivityMediaSubdivisionName","SampleAquifer","OrganizationDescriptionText","TribalCode","ElectronicAddress",
                  "Telephonic","OrganizationAddress/AddressTypeName_1","OrganizationAddress/AddressText_1","OrganizationAddress/SupplementalAddressText_1",
                  "OrganizationAddress/LocalityName_1", "OrganizationAddress/StateCode_1", "OrganizationAddress/PostalCode_1",
                  "OrganizationAddress/CountryCode_1","OrganizationAddress/CountyCode_1", "OrganizationAddress/AddressTypeName_2",
                  "OrganizationAddress/AddressText_2", "OrganizationAddress/SupplementalAddressText_2","OrganizationAddress/LocalityName_2",
                  "OrganizationAddress/StateCode_2", "OrganizationAddress/PostalCode_2", "OrganizationAddress/CountryCode_2", "OrganizationAddress/CountyCode_2",
                  "OrganizationAddress/AddressTypeName_3",
                  "OrganizationAddress/AddressText_3", "OrganizationAddress/SupplementalAddressText_3","OrganizationAddress/LocalityName_3",
                  "OrganizationAddress/StateCode_3", "OrganizationAddress/PostalCode_3", "OrganizationAddress/CountryCode_3", "OrganizationAddress/CountyCode_3",
                  "ProjectDescriptionText", "SamplingDesignTypeCode", "MeasureValue", "MeasureUnitCode", "StatisticalStratumText",
                  "LocationCategoryName","LocationStatusName", "ReferenceLocationTypeCode", "ReferenceLocationStartDate", "ReferenceLocationEndDate",
                  "ResourceCreatorName", "ResourceSubjectText", "ResourcePublisherName", "ResourceDate", "ResourceIdentifier", 
                  "CommentText", "ProjectFileUrl", "ProjectMonitoringLocationWeightingUrl", "SourceMapScaleNumeric", "HorizontalAccuracyMeasure/MeasureValue",
                  "HorizontalAccuracyMeasure/MeasureUnitCode", "HorizontalCollectionMethodName", "VerticalMeasure/MeasureValue", "VerticalMeasure/MeasureUnitCode",
                  "VerticalAccuracyMeasure/MeasureValue", "VerticalAccuracyMeasure/MeasureUnitCode", "VerticalCollectionMethodName", "VerticalCoordinateReferenceSystemDatumName",
                  "AquiferTypeName", "AquiferName", "LocalAqfrName", "FormationTypeText", "WellHoleDepthMeasure/MeasureValue", "WellHoleDepthMeasure/MeasureUnitCode",
                  "ConstructionDateText", "WellDepthMeasure/MeasureValue", "WellDepthMeasure/MeasureUnitCode", "DrainageAreaMeasure/MeasureValue", "DrainageAreaMeasure/MeasureUnitCode",
                  "ContributingDrainageAreaMeasure/MeasureValue", "ContributingDrainageAreaMeasure/MeasureUnitCode", "IndexIdentifier", "IndexTypeIdentifier", "IndexTypeIdentifierContext",
                  "IndexTypeName", "ResourceTitleName", "IndexTypeScaleText", "IndexQualifierCode", "IndexCommentText", "IndexCalculatedDate",
                  "CollectionDuration/MeasureValue", "CollectionDuration/MeasureUnitCode", "ReachLengthMeasure/MeasureValue", "ReachLengthMeasure/MeasureUnitCode", 
                  "ReachWidthMeasure/MeasureValue", "ReachWidthMeasure/MeasureUnitCode", "PassCount", "NetTypeName", "NetSurfaceAreaMeasure/MeasureValue", 
                  "NetSurfaceAreaMeasure/MeasureUnitCode", "NetMeshSizeMeasure/MeasureUnitCode","BoatSpeedMeasure/MeasureValue", "BoatSpeedMeasure/MeasureUnitCode",
                  "CurrentSpeedMeasure/MeasureValue", "NetMeshSizeMeasure/MeasureValue", "CurrentSpeedMeasure/MeasureUnitCode", "ActivityMetricType/MetricTypeIdentifier",
                  "ActivityMetricType/MetricTypeIdentifierContext", "ActivityMetricType/MetricTypeName", "see Comment field",
                  "MetricTypeCitation/MetricTypeScaleText", "MetricTypeCitation/FormulaDescriptionText", "MetricValueMeasure/MeasureValue",
                  "MetricValueMeasure/MeasureUnitCode", "MetricValueMeasure/MetricScoreNumeric", "MetricValueMeasure/MetricCommentText", "MetricValueMeasure/IndexIdentifier",
                  "CellFormName", "CellShapeName","HabitName", "VoltinismName", "TaxonomicPollutionTolerance", "TaxonomicPollutionToleranceScaleText",
                  "TrophicLevelName", "FunctionalFeedingGroupName", "FrequencyClassDescriptorCode", "FrequencyClassDescriptorUnitCode",
                  "LowerClassBoundValue", "UpperClassBoundValue", "ActivityGroupUrl")

 wqxcrswlk_legacy <- wqxcrswlk_mod3 |> 
   #filter(in_DR2.0 == "Y") |> 
   filter(!WqxV2.FieldName %in% drop_fields)

## WRITE MODIFIED CROSSWALK TABLE
# write_csv(wqxcrswlk_legacy, "C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/Temp_Crswlk/wqxcrswlk_temp_toshare.csv")
 cols_wqx2_legacy <- wqxcrswlk_legacy$WqxV2.FieldName


## Replace special characters
df2 <- df |> 
  rename_with(~ stringr::str_replace_all(., c('_' = '\\.', '/' = '\\.')), 
              .cols = all_of(cols_wqx2_legacy)) #rename_with(~ stringr::str_replace_all(., pattern = '_', replacement = '\\.'))
names(df2)




#############
## READ Crosswalk table - modified
wqxcrswlk_legacy <- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/Temp_Crswlk/wqxcrswlk_temp_toshare.csv")

###########################
## TESTING OUT FUNCTION WITH MODIFIED CROSSWALK TABLE


#' TADA_RenameColumns
#' This function renames columns in a data frame pulled from the Water Quality Portal 3.0
#'  using USGS dataRetrieval (v2.7.18) service = “ResultWQX3” back to schema names from WQX2.0 (Legacy).
#'
#' @param .data A dataframe queried from the WQP3.0 using dataRetrieval (v2.7.18)
#' @param crswlk A modified crosswalk table (dataframe) for WQX3.0 and WQX2.0 field (element)
#'
#' @returns A dataframe with column names changed to WQX2.0 Legacy names
#' @export
#'
#' @examples df_legacy <- TADA_RenameColumns(df_wqp3, crswlk)
TADA_RenameColumns <- function(.data, crswlk) {
  ## READ WQX3.0 column name schema from web services
  wqxnames <- crswlk
  
   # Create vectors of WQX3.0 and WQX2.0 (Legacy) column names that are in WQX schema crosswalk table
  beta_names = crswlk$FieldName3.0
  legacy_names = crswlk$WqxV2.FieldName
  
  if (length(beta_names) != length(legacy_names)) {
    stop("`old names` and `new names` must be the same length", call. = FALSE)
  }
  
  # Rename columns in WQP (beta) data query back to the legacy names 
  df <- data.table::setnames(.data, old = beta_names,
                             new = legacy_names, skip_absent = TRUE) 
  
  # Replace special characters in column names to match expected TADA output
  df <- df |> 
    rename_with(~ stringr::str_replace_all(., c('_' = '\\.', '/' = '\\.'))) #rename_with(~ stringr::str_replace_all(., pattern = '_', replacement = '\\.'))
  
  
  
  return(df)
}

###################
## RUN THE FUNCTION 
test <- TADA_RenameColumns(result.DR, wqxcrswlk_legacy)

names(test)
# I think it's working. 

##########################
# CHECKING FUNCTION ON OTHER DATA QUERIES USING WQP Beta - currently WQX data is not working but USGS data is
WQPquery2 <- list(siteid = "USGS-440504121435800"#, #Middle Green Lake (1980-1981)   McKenzie River at Springfield ("USGS-440508122581200")
                 #startDate = "1975-01-01",
                 #endDate = "1978-12-31",
                 #characteristicName = "Temperature, water"
                 
)

result.DR2 <- dataRetrieval::readWQPdata(WQPquery2,
                                         service = "ResultWQX3", # beta "ResultWQX3"
                                         dataProfile = "fullPhysChem",
                                         ignore_attributes = T)

test2 <- TADA_RenameColumns(result.DR2, wqxcrswlk_legacy)

## WRITE TADA OUTPUT WITH RENAMED FUNCTION
write_csv(test2, "C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/dataRetrieval_output/result.tada.beta.rename.csv")


# Rename columns missing from crosswalk table
df <- df |> 
  rename(DetectionQuantitationLimitMeasure.MeasureValue = DetectionLimit.MeasureA,
         DetectionQuantitationLimitMeasure.MeasureUnitCode = DetectionLimit.MeasureUnitA)

## CREATE VECTOR OF COLUMN NAMES IN dataRetrieval # 183 columns
results.DR.cols <- names(results.DR)

# Subset crosswalk table based on matches with dataRetrieval output by FieldName3.0
wqxcrswlk_legacy <- wqxcrswlk |> 
  filter(FieldName3.0 %in% results.DR.cols) |> 
  filter(!WqxV2.FieldName == "see Comment field") |> 
  filter(!is.na(WqxV2.FieldName))
# Vector of legacy names in dataRetrieval output that have a match
cols_wqx2_legacy <- wqxcrswlk_legacy$WqxV2.FieldName

## Replace special characters
df <- df |> 
  rename_with(~ stringr::str_replace_all(., c('_' = '\\.', '/' = '\\.')), 
              .cols = all_of(cols_wqx2_legacy)) #rename_with(~ stringr::str_replace_all(., pattern = '_', replacement = '\\.'))
names(df)
