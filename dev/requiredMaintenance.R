###########################################################

# Update All TADA Reference Files

# ATTAINSRefTables.R
TADA_UpdateATTAINSOrgIDsRef()
TADA_UpdateATTAINSParamUseOrgRef()
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

## Update Example Data

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
  y <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y)
  rm(Data_6Tribes_5y)
  y <- TADA_FlagMethod(y, clean = TRUE)
  y <- TADA_FlagAboveThreshold(y, clean = TRUE)
  y <- TADA_FlagBelowThreshold(y, clean = TRUE)
  # y <- TADA_FindPotentialDuplicatesMultipleOrgs(y, dist_buffer = 100)
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

  # Generate Data_NCTCShepherdstown_HUC12
  Data_NCTCShepherdstown_HUC12 <- TADA_DataRetrieval(
    startDate = "2020-03-14",
    endDate = "null",
    countycode = "null",
    huc = "02070004",
    siteid = "null",
    siteType = "null",
    characteristicName = "null",
    characteristicType = "null",
    sampleMedia = "null",
    statecode = "null",
    organization = "null",
    project = "null",
    applyautoclean = TRUE,
    ask = FALSE
  )
  print("Data_NCTCShepherdstown_HUC12:")
  print(dim(Data_NCTCShepherdstown_HUC12))
  usethis::use_data(Data_NCTCShepherdstown_HUC12, internal = FALSE, overwrite = TRUE, compress = "xz", version = 3, ascii = FALSE)
  rm(Data_NCTCShepherdstown_HUC12)

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
  # Data_WV <- TADA_FindPotentialDuplicatesMultipleOrgs(
  #   Data_WV
  # )
  # Data_WV <- dplyr::filter(
  #   Data_WV,
  #   TADA.ResultSelectedMultipleOrgs == "Y"
  # )
  # Filter out remaining irrelevant data, NA's and empty cols
  # REQUIRED
  unique(Data_WV$TADA.ResultMeasureValueDataTypes.Flag)
  sum(is.na(Data_WV$TADA.ResultMeasureValue))
  Data_WV <- TADA_AutoFilter(Data_WV)
  unique(Data_WV$TADA.ResultMeasureValueDataTypes.Flag)
  sum(is.na(Data_WV$TADA.ResultMeasureValue))
  # Remove results with QC issues
  # REQUIRED
  Data_WV <- TADA_RunKeyFlagFunctions(
    Data_WV,
    clean = TRUE
  )
  # CM note for team discussion: Should results with NA units be dealt with now as well within TADA_AutoFilter?
  # Flag above and below threshold. Do not remove
  # OPTIONAL
  Data_WV <- TADA_FlagAboveThreshold(Data_WV, clean = FALSE, flaggedonly = FALSE)
  Data_WV <- TADA_FlagBelowThreshold(Data_WV, clean = FALSE, flaggedonly = FALSE)
  # Harmonize synonyms
  # OPTIONAL
  Data_WV <- TADA_HarmonizeSynonyms(Data_WV)
  # Review
  Data_WV <- dplyr::filter(Data_WV, TADA.CharacteristicName %in% c("ZINC", "PH", "NITRATE"))
  TADA_FieldValuesTable(Data_WV, field = "TADA.ComparableDataIdentifier")
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
}

###########################################################

# Run styler to style code
# https://style.tidyverse.org/
# See: https://styler.r-lib.org/reference/style_pkg.html
# Run the following with defaults
library(styler)
style_pkg()

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

# Run devtools check and test
devtools::test()
# devtools::check()

# more robust test for releases (includes broken link check)
devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE)
