###########################################################

# Find char-frac-spec-unit combos not present in TADA HarmonizationTemplate.
# Add new combinations when found to the HarmonizationTemplate.csv and
# NPsummation_key.csv (if relevant to TN or TP summation).

FindSynonyms <- function() {
  test <- TADA_RandomTestingData()
  test1 <- TADA_RunKeyFlagFunctions(test)
  ref <- TADA_GetSynonymRef()
  ref_chars <- unique(ref$TADA.CharacteristicName)
  test_chars <- unique(subset(test1, test1$TADA.CharacteristicName %in% ref_chars)[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode")])
  test_chars_ref <- merge(test_chars, ref, all.x = TRUE)
  new_combos <- subset(test_chars_ref, is.na(test_chars_ref$HarmonizationGroup))[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode")]
  if (dim(new_combos)[1] > 0) {
    print("New combinations found in random dataset test.")
  }
  return(new_combos)
}

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

# Find Characteristic/Source/Value.Unit Combinations in "WQXcharValRef.csv" with more than one row
# open unit.ref
unit.ref <- utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "EPATADA")) %>%
  dplyr::filter(
    Type == "CharacteristicUnit",
    Status == "Accepted"
  )

# find Characteristic/Source/Value.Unit combinations with more than one row
find.dups <- unit.ref %>%
  dplyr::filter(Type == "CharacteristicUnit") %>%
  dplyr::group_by(Characteristic, Source, Value.Unit) %>%
  dplyr::mutate(Min_n = length(unique(Minimum)),
                Max_n = length(unique(Maximum))) %>%
  dplyr::filter(Min_n > 1 |
                  Max_n > 1)

# create download path
download.path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "WQXcharValRef_multiples.csv")

# create csv to send to WQX team and save in test results folder
readr::write_csv(find.dups, download.path)

# review csv and send to WQX team to update the validation table

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

###########################################################

# DRAFT function for overnight testing on lots of example data (incomplete)

TADA_OvernightTesting <- function(){

  testing_log <- file("testing_log.txt") # File name of output log

  sink(testing_log, append = TRUE, type = "output") # Writing console output to log file
  sink(testing_log, append = TRUE, type = "message")

  #cat(readChar(rstudioapi::getSourceEditorContext()$path, # Writing currently opened R script to file
  #             file.info(rstudioapi::getSourceEditorContext()$path)$size))

  num_iterations=2
  master_missing_codes_df <- data.frame(MeasureQualifierCode = NA, TADA.MeasureQualifierCode.Flag = NA)

  for (i in 1:num_iterations) {

    testing <- TADA_RandomTestingData()

    testing2 <- TADA_FlagMeasureQualifierCode(testing)

    #expect_true(all(testing2$TADA.MeasureQualifierCode.Flag != "Not Reviewed"))

    #print(unique(testing2$TADA_FlagMeasureQualifierCode))
    #print(unique(testing2$MeasureQualifierCode))

    # load in ResultMeasureQualifier Flag Table
    qc.ref <- TADA_GetMeasureQualifierCodeRef() %>%
      dplyr::rename(MeasureQualifierCode = Code) %>%
      dplyr::select(MeasureQualifierCode, TADA.MeasureQualifierCode.Flag)

    codes = unique(testing2$MeasureQualifierCode)
    missing_codes = codes[!codes %in% qc.ref$MeasureQualifierCode]

    missing_codes_df <- data.frame(MeasureQualifierCode = missing_codes, TADA.MeasureQualifierCode.Flag = "Not Reviewed")

    View(missing_codes_df)

    master_missing_codes_df <- dplyr::full_join(missing_codes_df, master_missing_codes_df, by = c("MeasureQualifierCode", "TADA.MeasureQualifierCode.Flag"), copy = TRUE)

    View(master_missing_codes_df)

    }

  master_missing_codes_distinct = master_missing_codes_df %>% dplyr::distinct()

  View(master_missing_codes_distinct)

  master_missing_codes_freq = as.data.frame(table(master_missing_codes_df))

  View(master_missing_codes_freq)

  closeAllConnections() # Close connection to log file

  return(testing_log)

  }
