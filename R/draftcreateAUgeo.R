# testgis <- sf::read_sf("C:/Users/hmarler/OneDrive - Environmental Protection Agency (EPA)/Desktop/GIS_2020_PUEBLOOFTESUQUE/PUEBLOOFTESUQUE.shp")
#
# testdat <- Data_TribalNations_Harmonized |>
#   dplyr::filter(OrganizationFormalName == "Blackfeet Nation (Montana)")
#
# testdat <- testdat |>
#   dplyr::rename(ATTAINS.AssessmentUnitIdentifier = TADA.MonitoringLocationIdentifier)
#
# testpoints <- TADA_CreatePointAUs(testdat)
#
#
#
#
#
#
# # need to create AU batch upload file
#
#
# # need to create AU with MLs batch upload file
#
# test.nogeo <- TADA_CreatePointAUs(testdat)
#
# test.geo <- TADA_CreatePointAUs(testdat,
#                                 create_geo = TRUE,
#                                 download_geo = TRUE)
