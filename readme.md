---
editor_options: 
  markdown: 
    wrap: 72
---

# Tools for Automated Data Assessment (TADA)

TADA is a draft R package being developed to help states, tribal
nations, and other stakeholders compile and evaluate Water Quality
Portal (WQP) data collected from surface water monitoring sites on
streams and lakes more efficiently. TADA is also being used to as
building block to support development of the TADA R Shiny application.

We encourage stakeholders to begin testing the functionality and
providing feedback. Moreover, open source software provides an avenue
for water quality program owners to develop and share code, and we
welcome your contributions! We hope to build a collaborative community
dedicated to this effort where contributors can discover, share and
build the package functionality over time.

## Water Quality Portal

In 2012, the WQP was deployed by the U.S. Geological Survey (USGS), the
U.S. Environmental Protection Agency (USEPA), and the National Water
Quality Monitoring Council to combine and serve water-quality data from
numerous sources in a standardized format. The WQP holds over 420
million water quality sample results from over 1000 federal, state,
tribal and other partners, and is the nation's largest source for single
point of access for water-quality data. Participating organizations
submit their data to the WQP using the EPA's Water Quality Exchange
(WQX), a framework designed to map their data holdings to a common data
structure.

## Installation

You can install and load the most recent version from GitHub by running:

``` r
library (remotes)
remotes::install_github("USEPA/TADA")
```

To view the vignette in RStudio, run

``` r
vignette("WQPDataHarmonization", "TADA")
```

## Dependencies

Make sure all of the packages below are installed before running the
code:

|               |
|---------------|
| data.table    |
| plyr          |
| dataRetrieval |
| dplyr         |
| ggplot2       |
| grDevices     |
| magrittr      |
| stringr       |
| utils         |
| RColorBrewer  |
| Rcpp          |
| stats         |
| remotes       |
| rmarkdown     |
| knitr         |
| testthat      |
| usethis       |
| devtools      |

## Contact

If you have any questions, please reach out to Cristina Mullin at
mullin.cristina\@epa.gov

## Disclaimer

This United States Environmental Protection Agency (EPA) GitHub project
code is provided on an "as is" basis and the user assumes responsibility
for its use. EPA has relinquished control of the information and no
longer has responsibility to protect the integrity, confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by EPA. The EPA seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by EPA or the United States Government.
