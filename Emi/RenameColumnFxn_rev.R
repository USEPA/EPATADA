###################
## MODIFY RENAME COLUMN FUNCTION
##  referencing updated crosswalk table (temporary)
##
## 4/4/2025
###################

library(tidyverse)
library(readr)

## Read modified crosswalk table
wqxcrswlk_mod <- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/Temp_Crswlk/wqxcrswlk_temp_modified.csv")

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
#  Changing the 'see Comments field' to useful names - relevant to dataRetrieval and TADA
wqxcrswlk_mod2 <- wqxcrswlk_mod |> 
  mutate(WqxV2.FieldName = case_when(
    FieldName3.0 == "SampleCollectionMethod_Description" ~ "SampleCollectionMethod/MethodDescriptionText",
    FieldName3.0 == "DataQuality_PrecisionValue" ~ "DataQuality/PrecisionValue",
    FieldName3.0 == "DataQuality_ConfidenceIntervalValue" ~ "DataQuality/ConfidenceIntervalValue",
    FieldName3.0 == "DataQuality_UpperConfidenceLimitValue" ~ "DataQuality/UpperConfidenceLimitValue",
    FieldName3.0 == "DataQuality_LowerConfidenceLimitValue" ~ "DataQuality/LowerConfidenceLimitValue",
    FieldName3.0 == "ResultAnalyticalMethod_Description" ~ "ResultAnalyticalMethod/MethodDescriptionText",
    FieldName3.0 == "SamplePrepMethod_Description" ~ NA, # Biological profile
    FieldName3.0 == "LabSamplePrepMethod_Description" ~ NA, # Biological profile
    FieldName3.0 == "LabSamplePrepMethod_EndTime" ~ NA, # Biological profile
    TRUE ~ WqxV2.FieldName
  ))

# Remove NAs from crosswalk table 
wqxcrswlk_mod3 <- wqxcrswlk_mod2 |> 
  filter(!is.na(WqxV2.FieldName))

###############
# Create vectors of WQX3.0 and WQX2.0 (Legacy) column names
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

###############
## RENAME COLUMNS FROM BETA BACK TO LEGACY
#https://stackoverflow.com/questions/29380447/using-data-tablesetnames-when-some-column-names-might-not-be-present 
df <- result.tada |> 
  rename(any_of(setNames(beta_names,
                         legacy_names))) 

###############
## REPLACE SPECIAL CHARACTERS TO MATCH TADA FORMAT
# Only apply to subset of columns that were beta that had legacy names
wqxcrswlk_legacy <- wqxcrswlk_mod3 |> 
  filter(in_DR2.0 == "Y")

cols_wqx2_legacy <- wqxcrswlk_legacy$WqxV2.FieldName


## Replace special characters
df2 <- df |> 
  rename_with(~ stringr::str_replace_all(., c('_' = '\\.', '/' = '\\.')), 
              .cols = all_of(cols_wqx2_legacy)) #rename_with(~ stringr::str_replace_all(., pattern = '_', replacement = '\\.'))
names(df2)

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
