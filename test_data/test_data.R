library(dataRetrieval)
library(dplyr)
library(stringr)

#WQP URL
#https://www.waterqualitydata.us/data/Project/search?statecode=US%3A24&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Stream&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2019&startDateHi=01-01-2022&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET

# Set query parameters ####
WQPquery <- list(statecode = "US:24",
                 Sitetype = c("Lake, Reservoir, Impoundment", "Stream"), 
                 Samplemedia = c("water", "Water"),
                 characteristicName = c("Dissolved oxygen (DO)", "pH", "Temperature, water"),
                 startDate = "01-01-2019", 
                 endDate = "01-01-2022")

# Retrieve all 3 profiles ####
results.DR <- readWQPdata(WQPquery)

narrow.DR <- readWQPdata(WQPquery, dataProfile = "narrowResult")

sites.DR <- whatWQPsites(WQPquery)

projects.DR <- readWQPdata(WQPquery, service = "Project")

# Join station data to full phys/chem (results.DR) ####

join1 <- results.DR %>%
  # join stations to results
  left_join(sites.DR, by = "MonitoringLocationIdentifier") %>% 
  # remove ".x" suffix from column names
  rename_at(vars(ends_with(".x")), ~str_replace(., "\\..$","")) %>%
  # remove columns with ".y" suffix
  select_at(vars(-ends_with(".y")))

# Join Speciation column from narrow to full profile

TADAprofile <- join1 %>%
  left_join(select(narrow.DR, ActivityIdentifier, MonitoringLocationIdentifier,
                   CharacteristicName, ResultMeasureValue, 
                   MethodSpecificationName), 
            by = c("ActivityIdentifier", "MonitoringLocationIdentifier",
                   "CharacteristicName", "ResultMeasureValue"))

# Remove duplicate rows
TADAprofile <- TADAprofile[!duplicated(TADAprofile),]  
