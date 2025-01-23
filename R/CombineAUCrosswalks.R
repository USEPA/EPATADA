# First, check if an org submitted ML to AU crosswalk to ATTAINS already (only a few states have done so)
AK_crosswalk <- TADA_GetATTAINSAUSiteCrosswalk(org_id = "AKDECWQ")
# Pull in an example TADA_dataframe
AK_data <- TADA_DataRetrieval(organization = "AKDECWQ", startDate = "2020-01-01", endDate = "2022-12-31")
AK_data_attains <- TADA_GetATTAINS(AK_data)
AK_data_attains_AU <- AK_data_attains$TADA_with_ATTAINS

# Filter AK_crosswalk by MS_ORG_ID == "AKDECWQ" only
AK_crosswalk_AKDECWQ <- AK_crosswalk %>%
  dplyr::filter("AKDECWQ" == MS_ORG_ID) %>%
  dplyr::distinct()

# Filter TADA Get ATTAINS by the 3 columns and get distinct values.
AK_GetATTAINS <- AK_data_attains_AU %>%
  dplyr::select(MS_LOCATION_ID = MonitoringLocationIdentifier, MS_ORG_ID = OrganizationIdentifier, ASSESSMENT_UNIT_ID = ATTAINS.assessmentunitidentifier) %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct() 

# Find matches, 52 of the 317 rows ML to AU in TADA dataframe have matches in both user supplied and TADA_GetATTAINS()
merged <- merge(AK_crosswalk_AKDECWQ, AK_GetATTAINS)

# There are 57 matching ML (so 5 of the 57 ML to AU matches defined in the user supplied crosswalk were matched to a different AU)
matches <- intersect(AK_crosswalk_AKDECWQ$MS_LOCATION_ID, AK_GetATTAINS$MS_LOCATION_ID)

# Number of unique AU for org_id = AKDECWQ from TADA_GetATTAINS(): 108
AU_GetATTAINS <- unique(AK_data_attains_AU$ATTAINS.assessmentunitidentifier)
n_AU_GetATTAINS <- length(AU_GetATTAINS)

# Number of unique AU for org_id = AKDECWQ from TADA_GetATTAINSAUSiteCrosswalk(): 44
AU_userSupplied <- unique(AK_crosswalk_AKDECWQ[,3])
n_AU_userSupplied <- nrow(AU_userSupplied)

# Number of interesects ( only 25 out of 44 are found in TADA_GetATTAINSAUSiteCrosswalk() and 25 out of 108 found in TADA_GetATTAINS()
# So, there are 83 AU in the TADA dataframe that were not previously defined.
length(intersect(dplyr::pull(AK_crosswalk_AKDECWQ[,3]), AK_GetATTAINS$ASSESSMENT_UNIT_ID ))

# Create a final df of ML to AU crosswalk. This dataframe will use any user-supplied crosswalk first, then for any remaining ML to AU crosswalk,
# it will use the recommended one done in TADA_GetATTAINS. User should perform additional QAQC of this crosswalk though.
# The flag column indicates where the crosswalk was performed and determines where the matches were consistent.
merged_AU_ML <- AK_GetATTAINS %>%
  dplyr::mutate(Flag1 = "GetATTAINS ML to AU matches") %>%
  dplyr::full_join(AK_crosswalk_AKDECWQ, by = c("MS_LOCATION_ID" , "MS_ORG_ID" , "ASSESSMENT_UNIT_ID" )) %>%
  dplyr::mutate(Flag2 = dplyr::case_when(
    stringr::str_c(MS_LOCATION_ID, MS_ORG_ID , ASSESSMENT_UNIT_ID) 
    %in% stringr::str_c(merged$MS_LOCATION_ID, merged$MS_ORG_ID, merged$ASSESSMENT_UNIT_ID) 
    ~  "User Supplied ML to AU matches")
  ) %>%
  dplyr::mutate(Flag = dplyr::case_when(
    !is.na(Flag2) ~ "Same ML to AU matches",
    is.na(Flag1) & is.na(Flag2) ~ "User Supplied ML to AU matches",
    !is.na(Flag1) & is.na(Flag2) ~ "GetATTAINS ML to AU matches"
  )) %>%
  dplyr::filter(MS_LOCATION_ID %in% unique(AK_GetATTAINS$MS_LOCATION_ID))%>%
  dplyr::select(-Flag1, -Flag2)


