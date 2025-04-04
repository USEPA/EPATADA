
# load most recent packages in development
library(remotes)
## DOI-USGS dataRetrieval
# install_github("DOI-USGS/dataRetrieval",
#                build_vignettes = TRUE, 
#                build_opts = c("--no-resave-data",
#                               "--no-manual"))
library(dataRetrieval) #dataRetrieval 2.7.18  OLD dataRetrieval 2.7.17.9000 #Extended Documentation: https://doi-usgs.github.io/dataRetrieval

library(devtools)
load_all()
library(tidyverse)
library(dataRetrieval) # dataRetrieval 2.7.18

# remotes::install_github("DOI-USGS/dataRetrieval", ref = "develop", dependencies = TRUE,
# force = TRUE)

##############
## REVISED TADA FUNCTION (returns 182 variables)
result.tada <- TADA_DataRetrieval(siteid = "USGS-04024315",
                                  #startDate = "1975-01-01",
                                  #endDate = "1978-12-31",
                                  characteristicName = "Temperature, water",
                                  applyautoclean = FALSE)
# Getting error "Error: 'curl_parse_url' is not an exported object from 'namespace:curl'"


# RENAME using str_replace_all to replace underscore(_) with '.'
#result.tada2 <- result.tada |> 
#  rename_with(~ stringr::str_replace_all(., pattern = '_', replacement = '\\.'))

##################
# RENAME COLUMNS TO MATCH REQUIRED TADA_AUTOCLEAN - ADD THIS TO RENAME FUNCTION
# result.tada <- result.tada |> 
#   rename(DetectionQuantitationLimitMeasure.MeasureValue = DetectionLimit.MeasureA,
#          DetectionQuantitationLimitMeasure.MeasureUnitCode = DetectionLimit.MeasureUnitA)

#lookup <- c(ActivityStartDateTime = "Activity_StartDateTime")#, ResultAnalyticalMethod.MethodUrl = "ResultAnalyticalMethod_Description")
#result.tada <- rename(result.tada, ActivityStartDateTime = Activity.StartDateTime)
#results.tada2 <- rename(result.tada, all_of(lookup))

################
# TADA Autoclean
test2 <- TADA_AutoClean(result.tada)

################
## Check new function in package
devtools::check()


##################
## TESTING OTHER QUERIES
result.tada2 <- TADA_DataRetrieval(organization = c("CNENVSER"),# "REDLAKE_WQX","SFNOES_WQX","PUEBLO_POJOAQUE","FONDULAC_WQX","PUEBLOOFTESUQUE", "CNENVSER"
                                   startDate = "2018-01-01", 
                                   endDate = "2019-01-01", 
                                   applyautoclean = FALSE)

# Seems to time out when call using TADAModule1.Rmd
# SFNOES_WQX - runs n = 3779
# PUEBLOOFTESUQUE - runs n = 3334
# CNENVSER - runs n = 913
# Times out with REDLAKE_WQX,PUEBLO_POJOAQUE,"FONDULAC_WQX"  

TADA_dataframe <- TADA_DataRetrieval(
  startDate = "2020-01-01",
  endDate = "2020-12-31",
  characteristicName = "pH",
  countycode = "US:08:069",
  applyautoclean = TRUE
)

#############################
## TADA_AutoClean Required Fields - see Utilities.R
required_cols <- c(
  "ActivityMediaName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",
  "CharacteristicName", "ResultSampleFractionText", "MethodSpeciationName",
  "DetectionQuantitationLimitMeasure.MeasureUnitCode", "ResultDetectionConditionText",
  "ResultIdentifier", "DetectionQuantitationLimitMeasure.MeasureValue",
  "LatitudeMeasure", "LongitudeMeasure"
)

tada_cols <- colnames(result.tada)

# Which ones are missing from TADA output?
test <- setdiff(required_cols, tada_cols)
test
#[1] "ResultMeasure.MeasureUnitCode"                    
#[2] "DetectionQuantitationLimitMeasure.MeasureUnitCode"
#[3] "DetectionQuantitationLimitMeasure.MeasureValue" 

################
## THERE ARE DUPLICATES in the schema crosswalk table - not sure how to deal with these in the long run
#   Maybe can delete for now
# result.tada2 <- result.tada |> 
#   select(!starts_with("BinaryObject")) |> 
#   select(!starts_with("PreparationEnd")) |> 
#   select(!DetectionQuantitationLimitType)


######
## WRITE TADA_DataRetrieval OUTPUT
write_csv(as.data.frame(colnames(result.tada)),"C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/tadaoutput_colnames_rev.csv")
##############
## Grab TADA Template names (85 variables)
test <- TADA_GetTemplate()

# There are 15 variables in TADA template that are not in the schema
# https://www.reddit.com/r/RStudio/comments/wv177f/comparing_columns_of_two_dataframes/
diff <- setdiff(colnames(test), colnames(result.tada))
#diff <- setdiff(colnames(result.tada), colnames(test))

result.temp <- test |> 
  select(all_of(diff))

result.tada2 <- result.tada |>  
  select(all_of(diff))
  #select(colnames(TADA_GetTemplate()))
test <- as.data.frame(colnames(test))
test2 <- as.data.frame(colnames(result.tada))




####################
## Pieces of TADA_DataRetrieval()
library(tidyverse)
library(data.table)

WQPquery <- list(siteid = "USGS-04024315",
                 #startDate = "1975-01-01",
                 #endDate = "1978-12-31",
                 characteristicName = "Temperature, water"
                 
)

result.DR <- dataRetrieval::readWQPdata(WQPquery,
                                         service = "ResultWQX3",
                                         dataProfile = "fullPhysChem",
                                         ignore_attributes = TRUE)
names(result.DR)
# check if any results are available
if ((nrow(results.DR) > 0) == FALSE) {
  print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
  TADAprofile.clean <- results.DR
} else {
  TADAprofile <- results.DR
  
  # add new functionality here to change names back to old names
  TADAprofile <- TADA_RenameColumns(TADAprofile)
}

# Test code to rename specific columns
# Rename using a named vector and `all_of()`
lookup <- c(ActivityStartDateTime = "Activity_StartDateTime", ResultAnalyticalMethod.MethodUrl = "ResultAnalyticalMethod_Description")
results.DR <- rename(results.DR, all_of(lookup))

# FROM USGS Website
nameToUse <- "pH"
pHData <- readWQPdata(siteid = "USGS-04024315", 
                      characteristicName = nameToUse)

pHData_legacy <- readWQPdata(siteid = "USGS-04024315", 
                             characteristicName = nameToUse,
                             service = "Result",
                             dataProfile = "narrowResult")
names(pHData_legacy)

activity <- readWQPdata(
  siteid = "USGS-04024315",
  service = "Activity"
)

# Data profile: "Result Detection Quantitation Limit Data"  
dl_data <- readWQPdata(
  siteid = "USGS-04024315",
  service = "ResultDetectionQuantitationLimit"
)

# Data profiles: "Sample Results (narrow)" 
samp_narrow <- readWQPdata(
  siteid = "USGS-04024315",
  service = "Result",
  dataProfile = "narrowResult"
)

names(samp_narrow)
###################
## WQX 3.0 Schema
wqxnames <- readr::read_csv("https://www.epa.gov/system/files/other-files/2024-07/schema_outbound_wqx3.0.csv")

# Collapse related wqx schema variables into a single column based on conditions
# And drop columns that aren't in WQX Legacy
wqxnames_rev <- wqxnames |>
  rowwise() |>
  mutate(legacy = case_when(!is.na(FieldName2.0.PhysChem) ~ FieldName2.0.PhysChem,
                            is.na(FieldName2.0.PhysChem) & !is.na(FieldName2.0.ActivityMetric) ~ FieldName2.0.ActivityMetric,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & !is.na(FieldName2.0.Project) ~ FieldName2.0.Project,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & is.na(FieldName2.0.Project) & !is.na(FieldName2.0.SamplingActivity) ~ FieldName2.0.SamplingActivity,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & is.na(FieldName2.0.Project) & is.na(FieldName2.0.SamplingActivity) & !is.na(FieldName2.0.Site) ~ FieldName2.0.Site,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & is.na(FieldName2.0.Project) & is.na(FieldName2.0.SamplingActivity) & is.na(FieldName2.0.Site) & !is.na(FieldName2.0.QuantitationLimit) ~ FieldName2.0.QuantitationLimit,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & is.na(FieldName2.0.Project) & is.na(FieldName2.0.SamplingActivity) & is.na(FieldName2.0.Site) & is.na(FieldName2.0.QuantitationLimit) & !is.na(FieldName2.0.Organization) ~ FieldName2.0.Organization,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & is.na(FieldName2.0.Project) & is.na(FieldName2.0.SamplingActivity) & is.na(FieldName2.0.Site) & is.na(FieldName2.0.QuantitationLimit) & is.na(FieldName2.0.Organization) & !is.na(FieldName2.0.PrjMonLocWeighting) ~ FieldName2.0.PrjMonLocWeighting,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & is.na(FieldName2.0.Project) & is.na(FieldName2.0.SamplingActivity) & is.na(FieldName2.0.Site) & is.na(FieldName2.0.QuantitationLimit) & is.na(FieldName2.0.Organization) & is.na(FieldName2.0.PrjMonLocWeighting) & !is.na(FieldName2.0.BiolHabitatMetric) ~ FieldName2.0.BiolHabitatMetric,
                            is.na(FieldName2.0.PhysChem) & is.na(FieldName2.0.ActivityMetric) & is.na(FieldName2.0.Project) & is.na(FieldName2.0.SamplingActivity) & is.na(FieldName2.0.Site) & is.na(FieldName2.0.QuantitationLimit) & is.na(FieldName2.0.Organization) & is.na(FieldName2.0.PrjMonLocWeighting) & is.na(FieldName2.0.BiolHabitatMetric) & !is.na(FieldName2.0.Biological) ~ FieldName2.0.Biological,
                            TRUE ~ NA)
  ) |>
  filter(!is.na(legacy)) # from 364 column names to 286 - beta version adds 78 names

# Fix legacy names that do not match TADA template names
# Note - this will replace all / and _ with . but there maybe legacy names getting changed that do not have a TADA equivalent - so may need to fix later
wqxnames_rev2 <- wqxnames_rev |> 
  mutate(legacy_2 = str_replace_all(legacy, "/|_", ".")) #|> 
#filter(legacy_2 %in% colnames(TADA_GetTemplate()))

# Create vectors of WQX3.0 and WQX2.0 (Legacy) column names
beta_names = wqxnames_rev2$FieldName3.0
legacy_names = wqxnames_rev2$legacy_2
legacynamesdf <- as.data.frame(legacy_names)
legacynames_sub <- legacynamesdf |> 
  filter(legacy_names %in% diff)
str_legacy <- legacynames_sub$legacy_names

diff_miss <- result.temp |> 
  select(!str_legacy)


# CHECK COLUMN NAMES
library(readr)
wqx_schema <- read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/schema_outbound_WQX3.0_updated.csv")
dr_col <- data.frame(FieldName3.0 = colnames(result.DR),
                     usgs_dr_fieldname = colnames(result.DR))

test <- left_join(wqx_schema,dr_col, by = "FieldName3.0" ) |> 
  select(c(1,10:23)) |> 
  relocate(FieldName3.0, WqxV2.FieldName, usgs_dr_fieldname)

######################
## Rename with updated crosswalk table (temporary)
wqxcrswlk <- readr::read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/Temp_Crswlk/wqxcrswlk_temp.csv")
