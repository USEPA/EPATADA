# some of the ref files have flags that need review when
# WQX domain tables are updated
# for example, see "Not Reviewed" TADA.MeasureQualifierCode.Flag in WQXMeasureQualifierCodeRef

#########################################################

# spell check
library(spelling)
spelling::spell_check_package(pkg = ".", vignettes = TRUE)
spelling::get_wordlist()
# # run to update spelling word list
# spelling::update_wordlist() # do not run until after checking wordlist & fixing spelling issues!

###########################################################

library(styler)
# Recommended styler setup to approximate air.toml
style_pkg(
  transformers = tidyverse_style(
    scope = I(c("tokens", "indention")), # let air handle the rest
    indent_by = 2,
    strict = FALSE # turn off aggressive alignment
  )
)

###########################################################

library(devtools)
# Run devtools check and test
devtools::test()
# devtools::check()
# more robust test for releases (includes broken link check)
devtools::check(manual = FALSE, remote = TRUE, incoming = TRUE)
