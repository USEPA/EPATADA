# Helper to skip tests that require live internet downloads unless explicitly enabled.
# File loaded by testthat when its name starts with "helper-".
skip_if_no_live_tests <- function() {
  if (!identical(tolower(Sys.getenv("EPATADA_RUN_LIVE_TESTS", "")), "true")) {
    testthat::skip(
      "Live reference tests disabled in CI; set EPATADA_RUN_LIVE_TESTS=true to enable"
    )
  }
  if (!requireNamespace("curl", quietly = TRUE) || !curl::has_internet()) {
    testthat::skip("No internet connection available for live reference tests")
  }
}
