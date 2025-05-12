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
library(readr)

##############
## USGS BRANCH of dataRetrieval reverting back to using Post
remotes::install_github("DOI-USGS/dataRetrieval", ref = "develop", dependencies = TRUE, force = TRUE)
library(dataRetrieval)

# Check which branch is being used - "RemoteRef"
# https://stackoverflow.com/questions/60982775/r-check-which-branch-of-a-package-was-installed-with-install-git?newreg=13421ec963844a839c360fc7ddcb6272
packageDescription('dataRetrieval')
################

# Trying the develop branch to see if the error (partial file transfer closed with outsanding read data remaining)
#  is caused by switch from Post to Get
# I don't think so...I think it's using the 3.0 service vs. 2.0
#  I can pull small sets of data using the 3.0 service
#  but larger datasets get the error message
# When i change to the 2.0 service (default), it's able to pull data (ex. CNENVSER)


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
#Error in `httr2::req_perform()`:
#! HTTP 504 Gateway Timeout.

#Error: 'curl_parse_url' is not an exported object from 'namespace:curl'

tada3.0_names <- names(result.tada)

# Write TADA output using dataRetrieval (3.0) call
write_csv(result.tada, "C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/dataRetrieval_output/result.tada.beta.csv")

## Generate random dataset to test
result.tada.rand <- TADA_RandomTestingData(number_of_days = 4, choose_random_state = TRUE, autoclean = FALSE)
# Sometimes returns Error in `httr2::req_perform()`:
#! Failed to perform HTTP request.
#Caused by error in `curl::curl_fetch_memory()`:
#! Transferred a partial file [www.waterqualitydata.us]: transfer closed with outstanding read data remaining

##################
## TESTING OTHER QUERIES
result.tada2 <- TADA_DataRetrieval(organization = c("CNENVSER"),# "REDLAKE_WQX","SFNOES_WQX","PUEBLO_POJOAQUE","FONDULAC_WQX","PUEBLOOFTESUQUE", "CNENVSER"
                                   startDate = "2018-01-01", 
                                   endDate = "2018-01-31", 
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
  applyautoclean = FALSE)





####################
## USGS dataRetrieval 
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

# NOT WORKING - there's a glitch with WQP beta - data product service calling WQX data
#  USGS data seems to work alright though...
WQPquery2 <- list(organization = "CNENVSER",
                  startDate = "2018-01-01", 
                  endDate = "2018-01-31")
result.DR2 <- dataRetrieval::readWQPdata(WQPquery2,
                                          service = "Result", # beta "ResultWQX3"
                                          dataProfile = "fullPhysChem",
                                          ignore_attributes = T)

#################
## WRITE dataRetrieval output
# Beta (3.0)
write_csv(result.DR, "C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/dataRetrieval_output/result.dr.beta.csv")

# Production (legacy) (2.0)
write_csv(result.DR2, "C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/dataRetrieval_output/result.dr.legacy.csv")


# check if any results are available
if ((nrow(result.DR) > 0) == FALSE) {
  print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
  TADAprofile.clean <- result.DR
} else {
  TADAprofile <- result.DR
  
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


 
######################
## Updated crosswalk table (temporary)
## 4/3/25
wqxcrswlk <- readr::read_csv("C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/Temp_Crswlk/wqxcrswlk_temp.csv")

# WQX2.0 LEGACY ELEMENTS
# Vector of FieldName3.0 with FieldName2.0 n = 274 variables
wqxcrswlk_legacy <- wqxcrswlk |> 
  filter(!is.na(WqxV2.FieldName))
# Vector of beta names in dataRetrieval output that have a match
wqx3_legacy <- wqxcrswlk_legacy$FieldName3.0

# dataRetrieval Output(Legacy) n = 132 elements
result.tada.legacy <- result.tada |> 
  select(any_of(wqx3_legacy))

#######################
## ADD COLUMNS TO CROSSWALK TABLE INDICATING WHETHER VARIABLE IS IN WQX2 and WQX3
wqxcrswlk_mod <- wqxcrswlk |> 
  mutate(in_DR3.0 = ifelse(FieldName3.0 %in% names(result.tada), "Y","N")) |> 
  mutate(in_DR2.0 = ifelse(FieldName3.0 %in% names(result.tada.legacy), "Y",
                           ifelse(is.na(WqxV2.FieldName) & FieldName3.0 %in% names(result.tada),"N", NA)))

############
## WRITE MODIFIED CROSSWALK TABLE
write_csv(wqxcrswlk_mod, "C:/Users/efergus/OneDrive - Environmental Protection Agency (EPA)/a_WDIB/TADA/WQP_transition/Crosswalk_tables/Temp_Crswlk/wqxcrswlk_temp_modified.csv")

## EXPLORE MANUAL MODIFICATIONS TO CROSSWALK TABLE TO ACCOMODATE TADA OUTPUT
# Vector of FieldName3.0 with NO 2.0 equivalent
wqxcrswlk_new <- wqxcrswlk |> 
  mutate(legacy_status = case_when(
    is.na(WqxV2.FieldName) ~ "NotServed",
    TRUE ~ WqxV2.FieldName
  )) |> 
  filter(legacy_status == "NotServed") |>
  select(FieldName3.0, WqxV2.FieldName, Definition, legacy_status)
wqx3_notserved <- wqxcrswlk_new$FieldName3.0

# Vector of FieldName3.0 with comment for legacy field n = 35 variables w/comments
wqxcrswlk_comment <- wqxcrswlk |> 
  filter(WqxV2.FieldName == "see Comment field") |>
  select(FieldName3.0, WqxV2.FieldName, Definition)
wqx3_comment <- wqxcrswlk_comment$FieldName3.0

# Vector of FieldName3.0 with FieldName2.0 n = 274 variables
# Subset crosswalk table based on matches with dataRetrieval output by FieldName3.0
wqxcrswlk_legacy <- wqxcrswlk |> 
  #filter(!WqxV2.FieldName == "see Comment field") |> 
  filter(!is.na(WqxV2.FieldName))
# Vector of beta names in dataRetrieval output that have a match
wqx3_legacy <- wqxcrswlk_legacy$FieldName3.0



# dataRetrieval output from WQX3 that did not have a legacy name (45 variables)
#  Changed TADA_DataRetrieval to not rename columns - so will see beta names now
result.tada.new <- result.tada |> 
  select(any_of(wqx3_notserved))

# dataRetrieval output with comment for legacy 3 n = 35
result.DR.comment <- result.DR |> 
  select(any_of(wqx3_comment))


  

##################
## MODIFY Crosswalk table changing See comment to useful names - relevant to dataRetrieval and TADA
wqxcrswlk_mod <- wqxcrswlk |> 
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
  

#######################
# MODIFY CROSSWALK TABLE FOR RENAME FXN
# Remove NAs from crosswalk table 
wqxcrswlk_mod2 <- wqxcrswlk_mod |> 
  filter(!is.na(WqxV2.FieldName))

# Create vectors of WQX3.0 and WQX2.0 (Legacy) column names
beta_names = wqxcrswlk_mod2$FieldName3.0
legacy_names = wqxcrswlk_mod2$WqxV2.FieldName

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
df <- result.DR |> 
  rename(any_of(setNames(beta_names,
                         legacy_names))) 

#####################
# Replace special characters in column names

# Only apply to subset of columns that were beta that had legacy names
## CREATE VECTOR OF COLUMN NAMES IN dataRetrieval # 183 columns
result.DR.cols <- names(result.DR)

# Subset crosswalk table based on matches with dataRetrieval output by FieldName3.0
wqxcrswlk_legacy <- wqxcrswlk |> 
  filter(FieldName3.0 %in% result.DR.cols) |> 
  filter(!WqxV2.FieldName == "see Comment field") |> 
  filter(!is.na(WqxV2.FieldName))
# Vector of legacy names in dataRetrieval output that have a match
cols_wqx2_legacy <- wqxcrswlk_legacy$WqxV2.FieldName

## Replace special characters
df <- df |> 
  rename_with(~ stringr::str_replace_all(., c('_' = '\\.', '/' = '\\.')), 
              .cols = all_of(cols_wqx2_legacy)) #rename_with(~ stringr::str_replace_all(., pattern = '_', replacement = '\\.'))
names(df)



#########
# Function to rename columns - modeled after data.table::setnames
# https://stackoverflow.com/questions/73390056/how-do-i-rename-columns-in-tidyverse-with-vectors-of-names
# rename_old_new = function(data, old, new, skip_absent = FALSE) {
#   if(skip_absent) {
#     rename(data, any_of(setNames(old, new)))
#   } else {
#     rename(data, all_of(setNames(old, new)))
#   }
# }
# 
# test <- rename_old_new(result.DR, beta_names, legacy_names, skip_absent = T)
# names(test)


# Get index of where 'see Comment field' occurs as columns in df
df_comment_index <- which(names(df) == "see Comment field")

# Select columns in df that with legacy names as see comment
DR_comment <- result.DR|> 
  select(all_of(df_comment_index))



# Show which fields are newly served in the dataRetrieval fxn
# Show which fields have a comment and see if can populate with something useful for TADA

##########################
## R SHINY - from TADA Module doc
# download TADA Shiny repository
remotes::install_github("USEPA/TADAShiny",
                        ref = "develop",
                        dependencies = TRUE
)

# launch the app locally.
TADAShiny::run_app()

