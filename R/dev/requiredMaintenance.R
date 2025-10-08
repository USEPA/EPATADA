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
