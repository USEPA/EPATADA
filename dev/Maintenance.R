#' Update TADA Reference Files
#' @return Saves updated reference files
#' This is only needed for ref tables in WQXRefTables.R
#' and the tribal feature layers in TADAGeospatialRefLayers.R
TADA_UpdateAllRefs <- function() {
  TADA_UpdateWQXCharValRef()
  TADA_UpdateMeasureUnitRef()
  TADA_UpdateDetCondRef()
  TADA_UpdateDetLimitRef()
  TADA_UpdateActivityTypeRef()
  TADA_UpdateCharacteristicRef()
  TADA_UpdateMeasureQualifierCodeRef()
  TADA_UpdateMonLocTypeRef()
  TADA_UpdateEPACSTRef()
  TADA_UpdateWQPOrgProviderRef()
  TADA_UpdateATTAINSOrgIDsRef()
  TADA_UpdateATTAINSParamUseOrgRef()
  TADA_UpdateTribalLayers()
}

###########################################################

## FUNCTION TO UPDATE EXAMPLE DATA

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

## Find char-frac-spec-unit combos not present in TADA HarmonizationTemplate.
## Add new combinations when found to the HarmonizationTemplate.csv and
## NPsummation_key.csv (if relevant to TN or TP summation).
#
# FindSynonyms <- function() {
#   test <- TADA_RandomTestingData()
#   test1 <- TADA_RunKeyFlagFunctions(test)
#   ref <- TADA_GetSynonymRef()
#   ref_chars <- unique(ref$TADA.CharacteristicName)
#   test_chars <- unique(subset(test1, test1$TADA.CharacteristicName %in% ref_chars)[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode")])
#   test_chars_ref <- merge(test_chars, ref, all.x = TRUE)
#   new_combos <- subset(test_chars_ref, is.na(test_chars_ref$HarmonizationGroup))[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode")]
#   if (dim(new_combos)[1] > 0) {
#     print("New combinations found in random dataset test.")
#   }
#   return(new_combos)
# }

###########################################################

# TADA_OvernightTesting
#
# @return console inputs and outputs
#
# TADA_OvernightTesting <- function(){
#
#   testing_log <- file("testing_log.txt") # File name of output log
#
#   sink(testing_log, append = TRUE, type = "output") # Writing console output to log file
#   sink(testing_log, append = TRUE, type = "message")
#
#   #cat(readChar(rstudioapi::getSourceEditorContext()$path, # Writing currently opened R script to file
#   #             file.info(rstudioapi::getSourceEditorContext()$path)$size))
#
#   num_iterations=2
#   master_missing_codes_df <- data.frame(MeasureQualifierCode = NA, TADA.MeasureQualifierCode.Flag = NA)
#
#   for (i in 1:num_iterations) {
#
#     testing <- TADA_RandomTestingData()
#
#     testing2 <- TADA_FlagMeasureQualifierCode(testing)
#
#     #expect_true(all(testing2$TADA.MeasureQualifierCode.Flag != "Not Reviewed"))
#
#     #print(unique(testing2$TADA_FlagMeasureQualifierCode))
#     #print(unique(testing2$MeasureQualifierCode))
#
#     # load in ResultMeasureQualifier Flag Table
#     qc.ref <- TADA_GetMeasureQualifierCodeRef() %>%
#       dplyr::rename(MeasureQualifierCode = Code) %>%
#       dplyr::select(MeasureQualifierCode, TADA.MeasureQualifierCode.Flag)
#
#     codes = unique(testing2$MeasureQualifierCode)
#     missing_codes = codes[!codes %in% qc.ref$MeasureQualifierCode]
#
#     missing_codes_df <- data.frame(MeasureQualifierCode = missing_codes, TADA.MeasureQualifierCode.Flag = "Not Reviewed")
#
#     View(missing_codes_df)
#
#     master_missing_codes_df <- dplyr::full_join(missing_codes_df, master_missing_codes_df, by = c("MeasureQualifierCode", "TADA.MeasureQualifierCode.Flag"), copy = TRUE)
#
#     View(master_missing_codes_df)
#
#     }
#
#   master_missing_codes_distinct = master_missing_codes_df %>% dplyr::distinct()
#
#   View(master_missing_codes_distinct)
#
#   master_missing_codes_freq = as.data.frame(table(master_missing_codes_df))
#
#   View(master_missing_codes_freq)
#
#   closeAllConnections() # Close connection to log file
#
#   return(testing_log)
#
#   }

###########################################################

# # Run styler to style code
# # https://style.tidyverse.org/
# # See: https://styler.r-lib.org/reference/style_pkg.html
# # Run the following with defaults
# library(styler)
# style_pkg()

###########################################################

# un devtools check and test
devtools::check()
devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE) # more robust test for releases (includes broken link check)
devtools::test()

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

# Find Broken Links if test-URLChecker.R fails
# Run the code below:
# extract urls function
extract_urls <- function(text) {
  stringr::str_extract_all(text, "http[s]?://[^\\s\\)\\]]+") %>% unlist()
}

# clean urls function
clean_url <- function(url) {
  stringr::str_remove_all(url, "[\\\\.,\\\")]+$|[{}].*") %>%
    stringr::str_remove_all("[<>]")
}

# create lists of files to check
other_files <- c(
  system.file("README.md", package = "EPATADA"),
  system.file("DESCRIPTION", package = "EPATADA"),
  system.file("NAMESPACE", package = "EPATADA")
)

vignettes <- list.files(system.file("vignettes", package = "EPATADA"), pattern = ".Rmd", full.names = TRUE)

articles <- list.files(system.file("vignettes/articles", package = "EPATADA"), pattern = ".Rmd", full.names = TRUE)

r_files <- list.files(system.file("R", package = "EPATADA"), pattern = ".R", full.names = TRUE)

# combine file lists
files <- append(other_files, vignettes) %>%
  append(articles) %>%
  append(r_files)

# create list of urls
urls <- purrr::map(files, ~ readLines(.x)) %>%
  unlist() %>%
  extract_urls() %>%
  clean_url() %>%
  unique() %>%
  # problematic URL I can't get a response from using multiple methods (itec) and CRAN because its response is inconsistent, likely due to redirecting to mirrors (HRM 10/28/2024)
  setdiff(c(
         # url works (HRM 11/7/24), but does not provide a recognizable response code
            "https://www.itecmembers.org/attains/",
         # if included will get 500 response because this is an incomplete URL
         # additional query information is pasted in as part of geospatial functions
          "https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=",
         # page loads but does not return a response code (NA)
         "http://cran.us.r-project.org"
          ))

# retrieve http response headers from url list
headers <- urls %>%
  purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))

# extract response code from first line of header response
response_code <- sapply(headers, "[[", 1)

# create dataframe of urls and response codes
df <- data.frame(urls, response_code)

# filter for any response codes that are not successful or redirect responses
df_false <- df %>%
  dplyr::filter(!grepl("200", response_code) &
                  !grepl("301", response_code) &
                  !grepl("302", response_code))

# Review the output of df_false.
# More information about http response codes can be found here:
# [Mozilla Developer HTTP response status codes] (https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
# Replace the broken links with functional ones or remove if no acceptable substitute is available.
# Rerun code above to verify that df_false contains zero rows.


###########################################################

# # Find Characteristic/Source/Value.Unit Combinations in "WQXcharValRef.csv" with more than one row
# # open unit.ref
# unit.ref <- utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "EPATADA")) %>%
#   dplyr::filter(
#     Type == "CharacteristicUnit",
#     Status == "Accepted"
#   )
#
# # find Characteristic/Source/Value.Unit combinations with more than one row
# find.dups <- unit.ref %>%
#   dplyr::filter(Type == "CharacteristicUnit") %>%
#   dplyr::group_by(Characteristic, Source, Value.Unit) %>%
#   dplyr::mutate(Min_n = length(unique(Minimum)),
#                 Max_n = length(unique(Maximum))) %>%
#   dplyr::filter(Min_n > 1 |
#                   Max_n > 1)
#
# # create download path
# download.path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "WQXcharValRef_multiples.csv")
#
# # create csv to send to WQX team and save in test results folder
# readr::write_csv(find.dups, download.path)
#
# # review csv and send to WQX team to update the validation table
#
###########################################################
# # Precompile articles for pkdown site,  should be run after any updates to articles or updates to
# # functions that may impact articles
#
# rmarkdown::render("vignettes/articles/_TADAAssessmentUnitUseCase.Rmd",
#                   output_format = "html_document")
#
# rmarkdown::render("vignettes/articles/TADAModule1_AdvancedTraining.Rmd",
#                   output_format = "html_document")
#
# rmarkdown::render("vignettes/articles/TADAModule1_BeginnerTraining.Rmd",
#                   output_format = "html_document")
#
# rmarkdown::render("vignettes/articles/TADAModule2.Rmd",
#                   output_format = "html_document")
#
# rmarkdown::render("vignettes/articles/TADAModule3_PartA.Rmd",
#                   output_format = "html_document")
#
# rmarkdown::render("vignettes/articles/TADAWaterSciConWorkshopDemo.Rmd",
#                   output_format = "html_document")
