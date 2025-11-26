# some of the ref files have flags that need review when
# WQX domain tables are updated

# for example, see "Not Reviewed" TADA.MeasureQualifierCode.Flag in WQXMeasureQualifierCodeRef

#########################################################
# spell check
library(spelling)
spelling::spell_check_package(
  pkg = ".",
  vignettes = TRUE
)
spelling::get_wordlist()
# # run to update spelling word list
# spelling::update_wordlist() # do not run until after checking wordlist & fixing spelling issues!

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
