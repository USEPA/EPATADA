# A rule-based Natural Language Processing (NLP). It relies on hand-crafted linguistic rules, dictionaries, and human
# defined expertise rather than learning from data. Rule-based systems are effective in specific, controlled domains with high precision needs.

# install.packages(c("httr2","pdftools"))
library(httr2)
library(pdftools)

# fetch pdf
pdf_url <- "https://www.epa.gov/sites/production/files/2014-12/documents/alwqs_chapter335610.pdf"
tmp <- tempfile(fileext = ".pdf")

req <- request(pdf_url) |> req_user_agent("R/httr2 pdf fetch") |> req_perform()

# save req to a temp file in binary format (efficient storage in R)
writeBin(resp_body_raw(req), tmp)

# extracts text per page
txt_vec <- pdf_text(tmp)
# select page number of text to extract
page_num <- 32
pdf_text <- paste0(txt_vec[page_num], ".")
pdf_text

# split page to distinct sentences while correcting for syntax, numbered list, E. coli abbreviation etc.
split_sentences_distinct <- function(text, dedup = TRUE) {
  dot_ph <- "<<<DOT>>>"
  sent_ph <- "<<<SENT>>>"
  s <- text

  # Normalize whitespace
  s <- gsub("[ \t]+", " ", s)
  s <- gsub("\\s*\\n\\s*", " ", s)
  s <- trimws(s)

  # Protect "E. coli" / "E.coli" (case-insensitive) - replace the dot after E
  s <- gsub(
    "(\\b[Ee])\\.(\\s*)coli\\b",
    paste0("\\1", dot_ph, "\\2coli"),
    s,
    perl = TRUE
  )

  # Protect decimal points (e.g., 2.0, 3.14)
  s <- gsub("(\\d)\\.(\\d)", paste0("\\1", dot_ph, "\\2"), s, perl = TRUE)

  # Protect number list markers like "5. " so "5." stays attached
  s <- gsub("(\\b\\d+)\\.(\\s+)", paste0("\\1", dot_ph, "\\2"), s, perl = TRUE)

  # Insert sentence delimiter after '.', '!', or '?' followed by whitespace or end
  s <- gsub("([.;!?])(\\s+|$)", paste0("\\1", sent_ph), s, perl = TRUE)

  # Split, restore dots, trim, and optionally deduplicate
  parts <- strsplit(s, sent_ph, fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nchar(parts) > 0]
  parts <- gsub(dot_ph, ".", parts, fixed = TRUE)

  if (dedup) {
    parts <- unique(parts)
  }
  return(parts)
}

# function to autoassign duration if possible
TADA_autoassign_duration <- function(
  param = "E. coli",
  magnitude = 700,
  text = pdf_text
) {
  list_of_strings <- split_sentences_distinct(text)
  return(list_of_strings)
}
TADA_autoassign_duration(pdf_text)

# runs through each sentence in pdf to identify if the param and magnitude are found as a match
#searched_string <- list()

param_words <- unlist(strsplit(trimws(param), "\\s+"))
magnitude_words <- function(x, value = magnitude) {
  re_num <- "(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d*)?"
  sapply(x, function(s) {
    loc <- gregexpr(re_num, s, perl = TRUE)[[1]]
    if (loc[1] == -1) {
      return(FALSE)
    }
    len <- attr(loc, "match.length")
    nums <- vapply(
      seq_along(loc),
      function(i) {
        raw <- substr(s, loc[i], loc[i] + len[i] - 1)
        suppressWarnings(as.numeric(gsub(",", "", raw)))
      },
      numeric(1)
    )
    any(!is.na(nums) & nums == value)
  })
}

# which sentence in pdf page contains the most likely match for the param?
# params may be more than one word, use rowSums to identify which strings contain the most matches for all param words.
param_index <- as.numeric(which.max(rowSums(sapply(
  param_words,
  grepl,
  x = list_of_strings,
  ignore.case = TRUE
))))
param_index

# which sentence in pdf page contains the most likely match for the magnitude?
# magnitude should only be one value, no rowSum is used.
magnitude_index <- as.numeric(which.max(magnitude_words(list_of_strings, 700)))
magnitude_index

# final index for most likely sentence that contains the information.
# magnitude value is prioritized. If multiple magnitude indices are found, pair it to the closest param_index.
# if multiple magnitude values are still found, search both indices?
final_index <- sapply(magnitude_index, function(x) {
  # Calculate absolute differences
  diffs <- abs(param_index - x)
  # Find the index of the minimum difference
  closest_index <- which.min(diffs)
  # Return the value from vector2 at that index
  magnitude_index[closest_index]
})

final_index <- unique(final_index)

if (
  str_detect(
    list_of_strings[final_index],
    "geometric\\s*mean|geo\\s*mean|30\\s*-?\\s*day"
  )
) {
  duration_data <- data.frame(
    ATTAINS.ParameterName = param,
    MagnitudeUnit = magnitude,
    DurationValue = 30,
    DurationMethods = "geometric mean",
    DurationUnits = "n-day"
  )
}


# Helper to OR multiple alternatives safely
pat_or <- function(x) paste0("(?:", paste(x, collapse = "|"), ")")

# Master list of regex patterns for your columns
# Inline (?i) makes matching case-insensitive by default.
WQS_PATTERNS <- list(
  DurationUnit = list(
    # Matches: 1-hr, 1 hr, 1-hour, 24-hour; allows “hr” or “hour(s)”
    "n-hour" = "(?i)\\b(?:\\d+|n)\\s*[-–—]?\\s*(?:hr|hour)s?\\b",
    # Matches: 7-day, 7 day, 7d; keeps it simple with day/d
    "n-day" = "(?i)\\b(?:\\d+|n)\\s*[-–—]?\\s*(?:day|d)s?\\b",
    # Matches: 4-week, 4 wk, 4-week(s)
    "n-week" = "(?i)\\b(?:\\d+|n)\\s*[-–—]?\\s*(?:week|wk)s?\\b",
    # Matches: 1-month, 1 mo, 1 mos, month(s)
    "n-month" = "(?i)\\b(?:\\d+|n)\\s*[-–—]?\\s*(?:month|mo|mos)s?\\b",
    # Matches: 1-quarter, 1 qtr, quarter(s)
    "n-quarter" = "(?i)\\b(?:\\d+|n)\\s*[-–—]?\\s*(?:quarter|qtr)s?\\b"
  ),

  DurationMethod = list(
    # Require “arithmetic” to avoid colliding with “geometric mean”
    "arithmetic mean" = "(?i)\\barithmetic\\s+(?:mean|average|avg)\\b",
    "arithmetic median" = "(?i)\\barithmetic\\s+median\\b|\\bmedian\\b",
    "arithmetic max" = "(?i)\\b(?:arithmetic\\s+)?(?:maximum|max)\\b|\\binstantaneous\\s+maximum\\b",
    "arithmetic min" = "(?i)\\b(?:arithmetic\\s+)?(?:minimum|min)\\b|\\binstantaneous\\s+minimum\\b",
    "arithmetic extremes" = "(?i)\\b(?:arithmetic\\s+)?extremes?\\b",

    # Common variants: “geo mean”, “g-mean”, “GM” (use with care if ‘GM’ is ambiguous in your corpus)
    "geometric mean" = "(?i)\\bgeometric\\s*mean\\b|\\bgeo\\s*mean\\b|\\bg[-\\.]?mean\\b|\\bGM\\b",

    # Rolling/running/moving means; optionally allow an n-day mention in between
    "rolling geometric mean" = "(?i)\\b(?:rolling|running|moving)\\s+(?:\\d+\\s*[-–—]?\\s*day\\s+)?(?:geometric\\s+)?mean\\b",
    "rolling arithmetic mean" = "(?i)\\b(?:rolling|running|moving)\\s+(?:\\d+\\s*[-–—]?\\s*day\\s+)?(?:arithmetic\\s+)?(?:mean|average|avg)\\b",

    # DO/temperature-style phrasing; include common acronyms where relevant
    "mean of daily minima" = "(?i)\\bmean\\s+of\\s+(?:the\\s+)?daily\\s+(?:minima|minimums|min)\\b|\\bdaily\\s+minimum\\s+mean\\b|\\b7\\s*-?\\s*day\\s+(?:average|mean)\\s+of\\s+the\\s+daily\\s+minimum\\b",
    "mean of daily maxima" = "(?i)\\bmean\\s+of\\s+(?:the\\s+)?daily\\s+(?:maxima|maximums|max)\\b|\\bdaily\\s+maximum\\s+mean\\b|\\b7\\s*-?\\s*day\\s+(?:average|mean)\\s+of\\s+the\\s+daily\\s+maximum\\b"
  ),

  FreqMethod = list(
    # “No more than X% of samples may exceed/fail/not meet…”
    "Percent of samples not meeting" = "(?i)\\b(?:no\\s+more\\s+than\\s+)?\\d+%\\s+of\\s+samples\\s+(?:may\\s+)?(?:exceed|violate|fail|not\\s+meet)\\b|\\bpercent\\s+of\\s+samples\\s+(?:exceed|violate|fail|not\\s+meet)\\b",

    # e.g., “95th percentile”, “90th percentile”
    "percentile" = "(?i)\\b\\d{1,3}(?:st|nd|rd|th)?\\s*percentile\\b|\\bpercentile\\b",

    # Counts over a 3-year period; allow “samples/exceedances/excursions” and “in/per/over”
    "n-samples in 3 years" = "(?i)\\b(?:no\\s+more\\s+than\\s+)?\\d+\\s+(?:samples|excursions|exceedances)\\s+(?:in|per|over|within)\\s+3\\s*(?:years?|yrs?)\\b",

    "n-samples in 4 years" = "(?i)\\b(?:no\\s+more\\s+than\\s+)?\\d+\\s+(?:samples|excursions|exceedances)\\s+(?:in|per|over|within)\\s+4\\s*(?:years?|yrs?)\\b",

    "n-samples in 5 years" = "(?i)\\b(?:no\\s+more\\s+than\\s+)?\\d+\\s+(?:samples|excursions|exceedances)\\s+(?:in|per|over|within)\\s+5\\s*(?:years?|yrs?)\\b",

    # Binomial test references
    "binomial test" = "(?i)\\bbinomial\\s+test\\b|\\bexact\\s+binomial\\b|\\bbinomial\\s+method\\b",

    # Generic count-based frequency statements
    "NumberNotMeeting" = "(?i)\\b(?:number|count)\\s+of\\s+(?:samples|observations)\\s+(?:not\\s+meeting|failing|exceeding)\\b|\\bnumber\\s+not\\s+meeting\\b|\\bexceedances?\\b"
  )
)


for (i in 1:length(list_of_strings)) {
  #searched_string[[i]] <- str_detect(list_of_strings[i], "geometric\\s*mean|geo\\s*mean|30\\s*-?\\s*day") # searches for the word 'geometric' followed by 'mean' ignoring whitespace or 'geo' followed by 'mean'
}
