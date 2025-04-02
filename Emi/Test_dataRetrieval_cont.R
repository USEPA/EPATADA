###############
## R Script to explore steps needed to transition from WQP2.0 to WQP3.0
##  One major step is to develop a function to rename fields (data elements)
#    from 3.0 names to legacy names and see how TADA functions work


# load most recent packages in development
library(remotes)
## DOI-USGS dataRetrieval
# remotes::install_github("DOI-USGS/dataRetrieval", ref = "develop", 
#                         dependencies = TRUE, force = TRUE)

# install_github("DOI-USGS/dataRetrieval",
#                build_vignettes = TRUE, 
#                build_opts = c("--no-resave-data",
#                               "--no-manual"))

library(dataRetrieval) #dataRetrieval 2.7.18  OLD dataRetrieval 2.7.17.9000 #Extended Documentation: https://doi-usgs.github.io/dataRetrieval

library(devtools)
load_all()
library(tidyverse)
library(data.table)

##############
## STEPS
## 1) Run DataDiscoveryRetrieval function
#     DataDiscoveryRetrieval.R was revised to use the beta 3.0 WQP data retrieval
#     Added the created TADARenameColumns function to change from beta to legacy names
#     Changes names separated with "_" to names separated with "." (how TADA breaks up words)
## 2) Run TADA_DataRetrieval query (below)

## NEED TO REVISE RENAME FUNCTION TO USE REVISED SCHEMA TABLE - THIS WILL SIMPLIFY THE CODE

##############
## REVISED TADA FUNCTION (returns 183 variables)
## 
result.tada <- TADA_DataRetrieval(siteid = "USGS-04024315",
                                  #startDate = "1975-01-01",
                                  #endDate = "1978-12-31",
                                  characteristicName = "Temperature, water",
                                  applyautoclean = FALSE)
#Error: 'curl_parse_url' is not an exported object from 'namespace:curl'

names(result.tada)

## Generate random dataset to test
result.tada.rand <- TADA_RandomTestingData(number_of_days = 1, choose_random_state = TRUE, autoclean = FALSE)
# Getting Error in `httr2::req_perform()`:
#! Failed to perform HTTP request.
#Caused by error in `curl::curl_fetch_memory()`:



####################
## USGS dataRetrieval 
WQPquery <- list(siteid = "USGS-04024315",
                 #startDate = "1975-01-01",
                 #endDate = "1978-12-31",
                 characteristicName = "Temperature, water"
                 
)

results.DR <- dataRetrieval::readWQPdata(WQPquery,
                                        service = "ResultWQX3",
                                        dataProfile = "fullPhysChem",
                                        ignore_attributes = TRUE)


# check if any results are available
if ((nrow(results.DR) > 0) == FALSE) {
  print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
  TADAprofile.clean <- results.DR
} else {
  TADAprofile <- results.DR
  
  # add new functionality here to change names back to old names
  TADAprofile <- TADA_RenameColumns(TADAprofile)
}

names(TADAprofile)

###################
## CHECK COLUMN NAMES btw schema table and USGS dataRetrieval output
## WQX 3.0 Schema - this is the old online schema - will need to update
wqxnames <- readr::read_csv("https://www.epa.gov/system/files/other-files/2024-07/schema_outbound_wqx3.0.csv")

# Need to add a FieldName2.0 column that collapses across all profiles
wqxnames_rev <- wqxnames |>
  dplyr::rowwise() |>
  dplyr::mutate(legacy = dplyr::case_when(!is.na(FieldName2.0.PhysChem) ~ FieldName2.0.PhysChem,
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

# Replace special characters in column names
# df <- df |> 
#   mutate_all(~(stringr::str_replace_all(., c('_' = '\\.', '/' = '\\.'))))

## USGS DataRetrieval 3.0 output (field names)
dr_col <- data.frame(FieldName3.0 = colnames(result.DR),
                     usgs_dr_fieldname = colnames(result.DR))

# Merge schema and usgs output
test <- left_join(dr_col, wqxnames_rev, by = "FieldName3.0") |> 
  select(c(1,2,22))


 



