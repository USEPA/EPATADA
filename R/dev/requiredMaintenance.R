# last ref and example data update: 6/17/25

###########################################################

# Update All TADA Reference Files

# ATTAINSRefTables.R
TADA_UpdateATTAINSOrgIDsRef()
TADA_UpdateATTAINSParamUseOrgRef() # takes a long time
TADA_UpdateATTAINSParameterWQPCharRef()
# WQPWQXRefTables.R
TADA_UpdateWQXCharValRef()
TADA_UpdateMeasureUnitRef()
TADA_UpdateDetCondRef()
TADA_UpdateDetLimitRef()
TADA_UpdateActivityTypeRef()
TADA_UpdateCharacteristicRef()
TADA_UpdateMeasureQualifierCodeRef()
TADA_UpdateMonLocTypeRef()
TADA_UpdateWQPOrgProviderRef()
# CriteriaRefTables.R
TADA_UpdateEPACSTRef()
# TADAGeospatialRefLayers.R
TADA_UpdateTribalLayers()

###########################################################

# Update Example Data

TADA_UpdateExampleData <- function() {
  # Generate Data_Nutrients_UT
  Data_Nutrients_UT <- TADA_DataRetrieval(
    statecode = "UT",
    characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
    startDate = "2020-10-01",
    endDate = "2022-09-30",
    ask = FALSE
  )
  print("Data_Nutrients_UT")
  print(dim(Data_Nutrients_UT))
  usethis::use_data(Data_Nutrients_UT,
    internal = FALSE, overwrite = TRUE,
    compress = "xz", version = 3, ascii = FALSE
  )
  rm(Data_Nutrients_UT)

  # Generate Data_6Tribes_5y.rda
  Data_6Tribes_5y <- TADA_DataRetrieval(
    organization = c(
      "REDLAKE_WQX",
      "SFNOES_WQX",
      "PUEBLO_POJOAQUE",
      "FONDULAC_WQX",
      "PUEBLOOFTESUQUE",
      "CNENVSER"
    ),
    startDate = "2018-01-01",
    endDate = "2023-01-01",
    ask = FALSE
  )
  print("Data_6Tribes_5y:")
  print(dim(Data_6Tribes_5y))
  usethis::use_data(Data_6Tribes_5y,
    internal = FALSE, overwrite = TRUE,
    compress = "xz", version = 3, ascii = FALSE
  )

  # Generate Data_6Tribes_5y_Harmonized.rda
  y <- subset(Data_6Tribes_5y, Data_6Tribes_5y$TADA.ActivityMediaName %in% c("WATER"))
  y <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, clean = TRUE)
  rm(Data_6Tribes_5y)
  y <- TADA_FlagMethod(y, clean = TRUE)
  y <- TADA_FlagAboveThreshold(y, clean = TRUE)
  y <- TADA_FlagBelowThreshold(y, clean = TRUE)
  y <- TADA_FindPotentialDuplicatesMultipleOrgs(y, dist_buffer = 100)
  y <- TADA_FindPotentialDuplicatesSingleOrg(y)
  y <- dplyr::filter(y, !(MeasureQualifierCode %in% c("D", "H", "ICA", "*")))
  y <- TADA_SimpleCensoredMethods(y,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )
  y <- dplyr::filter(y, TADA.ResultMeasureValueDataTypes.Flag != "Text" &
    TADA.ResultMeasureValueDataTypes.Flag != "NA - Not Available" &
    !is.na(TADA.ResultMeasureValue))
  Data_6Tribes_5y_Harmonized <- TADA_HarmonizeSynonyms(y)
  print("Data_6Tribes_5y_Harmonized:")
  print(dim(Data_6Tribes_5y_Harmonized))
  usethis::use_data(Data_6Tribes_5y_Harmonized,
    internal = FALSE, overwrite = TRUE,
    compress = "xz", version = 3, ascii = FALSE
  )
  rm(Data_6Tribes_5y_Harmonized)
  rm(y)

  # # Generate Data_NCTCShepherdstown_HUC12
  # Data_NCTCShepherdstown_HUC12 <- TADA_DataRetrieval(
  #   startDate = "2022-01-01",
  #   endDate = "2024-12-31",
  #   countycode = "null",
  #   huc = "02070004",
  #   siteid = "null",
  #   siteType = "null",
  #   characteristicName = "null",
  #   characteristicType = "null",
  #   sampleMedia = "null",
  #   statecode = "null",
  #   organization = "null",
  #   project = "null",
  #   applyautoclean = TRUE,
  #   ask = FALSE
  # )
  # print("Data_NCTCShepherdstown_HUC12:")
  # print(dim(Data_NCTCShepherdstown_HUC12))
  # usethis::use_data(Data_NCTCShepherdstown_HUC12, internal = FALSE, overwrite = TRUE, compress = "xz", version = 3, ascii = FALSE)
  # rm(Data_NCTCShepherdstown_HUC12)

  # Generate Data_R5_TADAPackageDemo
  Data_R5_TADAPackageDemo <- TADA_DataRetrieval(
    startDate = "2019-05-01",
    endDate = "2019-05-07",
    countycode = "null",
    huc = "null",
    siteid = "null",
    siteType = "null",
    characteristicName = "null",
    characteristicType = "null",
    sampleMedia = "null",
    statecode = c("IL", "IN", "MI", "MN", "OH", "WI"),
    organization = "null",
    project = "null",
    applyautoclean = FALSE,
    ask = FALSE
  )
  print("Data_R5_TADAPackageDemo:")
  print(dim(Data_R5_TADAPackageDemo))
  usethis::use_data(Data_R5_TADAPackageDemo, internal = FALSE, overwrite = TRUE, compress = "xz", version = 3, ascii = FALSE)
  rm(Data_R5_TADAPackageDemo)

  # Generate MODULE 3 VIGNETTE EXAMPLE DATA
  # Get data
  Data_WV <- TADA_DataRetrieval(
    startDate = "2020-03-14",
    huc = "02070004",
    applyautoclean = FALSE,
    ask = FALSE
  )
  # Remove non-surface water media
  # OPTIONAL
  Data_WV <- TADA_AnalysisDataFilter(
    Data_WV,
    clean = TRUE,
    surface_water = TRUE,
    ground_water = FALSE,
    sediment = FALSE
  )
  # Remove single org duplicates
  # REQUIRED
  Data_WV <- TADA_FindPotentialDuplicatesSingleOrg(
    Data_WV
  )
  Data_WV <- dplyr::filter(
    Data_WV,
    TADA.SingleOrgDup.Flag == "Unique"
  )
  # Run autoclean
  # REQUIRED
  Data_WV <- TADA_AutoClean(Data_WV)
  # Prepare censored results
  # REQUIRED
  Data_WV <- TADA_SimpleCensoredMethods(
    Data_WV,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )
  # Remove multiple org duplicates
  # OPTIONAL
  Data_WV <- TADA_FindPotentialDuplicatesMultipleOrgs(
    Data_WV
  )
  Data_WV <- dplyr::filter(
    Data_WV,
    TADA.ResultSelectedMultipleOrgs == "Y"
  )
  # Filter out remaining irrelevant data, NA's and empty cols
  Data_WV <- TADA_ConvertSpecialChars(Data_WV,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )
  # Remove results with QC issues
  # REQUIRED
  Data_WV <- TADA_RunKeyFlagFunctions(
    Data_WV,
    clean = TRUE
  )
  # Flag above and below threshold. Do not remove
  Data_WV <- TADA_FlagAboveThreshold(Data_WV, clean = FALSE, flaggedonly = FALSE)
  Data_WV <- TADA_FlagBelowThreshold(Data_WV, clean = FALSE, flaggedonly = FALSE)
  # Harmonize synonyms
  Data_WV <- TADA_HarmonizeSynonyms(Data_WV)
  # Save example data
  Data_HUC8_02070004_Mod1Output <- Data_WV
  print("Data_HUC8_02070004_Mod1Output:")
  print(dim(Data_HUC8_02070004_Mod1Output))
  usethis::use_data(Data_HUC8_02070004_Mod1Output,
    internal = FALSE,
    overwrite = TRUE,
    compress = "xz",
    version = 3,
    ascii = FALSE
  )
  rm(Data_HUC8_02070004_Mod1Output)
  rm(Data_WV)

  # Generate Data_MT_MissoulaCounty
  Data_MT_MissoulaCounty <- TADA_DataRetrieval(
     startDate = "2020-01-01",
     endDate = "2022-12-31",
     statecode = "MT",
     characteristicName = c(
     "Escherichia",
     "Escherichia coli",
     "pH"
     ),
     county = "Missoula County",
     ask = FALSE) %>%
     TADA_RunKeyFlagFunctions() %>%
     TADA_SimpleCensoredMethods() %>%
     TADA_HarmonizeSynonyms()

  print("Data_MT_MissoulaCounty")
  print(dim(Data_MT_MissoulaCounty))
  usethis::use_data(Data_MT_MissoulaCounty,
                    internal = FALSE, overwrite = TRUE,
                    compress = "xz", version = 3, ascii = FALSE
  )
  rm(Data_MT_MissoulaCounty)

# Generate Data_MT_AUMLRef
 # get crosswalk from ATTAINS
attains.existing.MT <- TADA_GetATTAINSAUMLCrosswalk(org_id = "MTDEQ")

# clean existing crosswalk from ATTAINS to make sure WQP monitoring location IDs pulled from ATTAINS are WQP compatible (adds org ID if missing)
clean.existing.attains.MT <- TADA_UpdateATTAINSAUMLCrosswalk(org_id = "MTDEQ")

# create example user supplied crosswalk (select a few Monitoring Locations from the tada df to use in the example for demonstration purposes)
user.supplied.cw <- clean.existing.attains.MT %>%
  dplyr::select(
    ATTAINS.AssessmentUnitIdentifier,
    ATTAINS.MonitoringLocationIdentifier,
    ATTAINS.WaterType
  ) %>%
  dplyr::filter(ATTAINS.MonitoringLocationIdentifier %in% c(
    "MDEQ_WQ_WQX-C04CKFKR05", "MDEQ_WQ_WQX-C04KNDYC01", "MDEQ_WQ_WQX-C04KNDYC02",
    "MDEQ_WQ_WQX-C04KNDYC04", "MDEQ_WQ_WQX-C04KNDYC54"
  )) %>%
  dplyr::rename(
    AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier,
    MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier
  ) %>%
  # Add an example new assessment unit for demonstration purposes.
  dplyr::bind_rows(c(
    AssessmentUnitIdentifier = "NEW:EX_MDEQ_WQ_WQX",
    MonitoringLocationIdentifier = "NARS_WQX-NWC_MT-10184",
    ATTAINS.WaterType = "LAKE, FRESHWATER"
  ))

MT.AUMLRef <- TADA_CreateAUMLCrosswalk(Data_MT_MissoulaCounty,
                                       au_ref = user.supplied.cw,
                                       org_id = "MTDEQ",
                                       add_catch = FALSE,
                                       batch_upload = TRUE
)

Data_MT_AUMLRef <- MT.AUMLRef$ATTAINS_crosswalk %>%
  dplyr::mutate(
    ATTAINS.WaterType = dplyr::case_when(
      ATTAINS.AssessmentUnitIdentifier == "NEW:EX_MDEQ_WQ_WQX" ~ "LAKE, FRESHWATER",
      TRUE ~ ATTAINS.WaterType
    )
  )

  print("Data_MT_AUMLRef")
  print(dim(Data_MT_AUMLRef))
  usethis::use_data(Data_MT_AUMLRef,
                    internal = FALSE, overwrite = TRUE,
                    compress = "xz", version = 3, ascii = FALSE
  )
  rm(attains.existing.MT, clean.existing.attains.MT, user.supplied.cw,
     MT.AUMLRef, Data_MT_AUMLRef)

  # Generate Data_MT_UseAURef

  Data_MT_UseAURef <- TADA_CreateUseAURef(AUMLRef = Data_MT_AUMLRef, org_id = "MTDEQ")

  print("Data_MT_UseAURef")
  print(dim(Data_MT_UseAURef))
  usethis::use_data(Data_MT_UseAURef,
                    internal = FALSE, overwrite = TRUE,
                    compress = "xz", version = 3, ascii = FALSE
  )
  rm(Data_MT_UseAURef)
}
###########################################################



# spell check
library(spelling)
spelling::spell_check_package(
  pkg = ".",
  vignettes = TRUE
)
# run to update spelling word list
spelling::get_wordlist()
spelling::update_wordlist()

###########################################################

# Run styler to style code
# https://style.tidyverse.org/
# See: https://styler.r-lib.org/reference/style_pkg.html
# Run the following with defaults
library(styler)
style_pkg()

###########################################################

# Run devtools check and test
devtools::test()
# devtools::check()

# more robust test for releases (includes broken link check)
devtools::check(manual = FALSE, remote = TRUE, incoming = TRUE)
