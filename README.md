## Welcome to TADA: Tools for Automated Data Analysis!

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![](https://github.com/USEPA/TADA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/TADA/actions/workflows/R-CMD-check.yaml)

We encourage you to read this package's [CONTRIBUTING](https://usepa.github.io/TADA/articles/CONTRIBUTING.html), [LICENSE](https://usepa.github.io/TADA/LICENSE.html), and [README](https://usepa.github.io/TADA/index.html) files (you are here).

Tools for Automated Data Analysis, or TADA, is a draft R package being developed to help States, Tribes, Tribal Nations, Pueblos, and other stakeholders more efficiently compile and evaluate [Water Quality Portal (WQP)](https://www.waterqualitydata.us/) data collected from surface water monitoring sites. TADA is both a stand-alone R package, and a building block to support development of the [TADA R Shiny application](https://github.com/USEPA/TADAShiny).

We encourage stakeholders to test the functionality and provide feedback. Moreover, open source software provides an avenue for water quality data originators and users to develop and share code, and we welcome your contributions! More information on how to contribute can be found in the [CONTRIBUTING](https://usepa.github.io/TADA/articles/CONTRIBUTING.html) file. This file explains how users can contribute to the R package by submitting a pull request or issue to request a change or provide feedback. We hope to build a collaborative community dedicated to this effort where contributors can discover, share and build the package functionality over time.

[More about the TADA Project](https://www.epa.gov/waterdata/TADA)

## Installation

You can install and load the most recent version of the TADA R Package from [GitHub](https://github.com/USEPA/TADA) by running:

``` r
library (remotes)
remotes::install_github("USEPA/TADA", ref = "develop", dependencies = TRUE)
```

## Water Quality Portal

In 2012, the WQP was deployed by the U.S. Geological Survey (USGS), the U.S. Environmental Protection Agency (USEPA), and the National Water Quality Monitoring Council to combine and serve water-quality data from numerous sources in a standardized format. The WQP holds over 420 million water quality sample results from over 1000 federal, state, tribal and other partners, and is the nation's largest source for single point of access for water-quality data. Participating organizations submit their data to the WQP using the EPA's Water Quality Exchange (WQX), a framework designed to map their data holdings to a common data structure.

## Open-Source Code Policy

Effective August 8, 2016, the [OMB Mandate: M-16-21; Federal Source Code Policy: Achieving Efficiency, Transparency, and Innovation through Reusable and Open Source Software](https://obamawhitehouse.archives.gov/sites/default/files/omb/memoranda/2016/m_16_21.pdf) applies to new custom-developed code created or procured by EPA consistent with the scope and applicability requirements of Office of Management and Budget's (OMB's) Federal Source Code Policy. In general, it states that all new custom-developed code by Federal Agencies should be made available and reusable as open-source code.

The EPA specific implementation of OMB Mandate M-16-21 is addressed in the [System Life Cycle Management Procedure](https://www.epa.gov/irmpoli8/policy-procedures-and-guidance-system-life-cycle-management-slcm). EPA has chosen to use GitHub as its version control system as well as its inventory of open-source code projects. EPA uses GitHub to inventory its custom-developed, open-source code and generate the necessary metadata file that is then posted to code.gov for broad reuse in compliance with OMB Mandate M-16-21.

If you have any questions or want to read more, check out the [EPA Open Source Project Repo](https://github.com/USEPA/open-source-projects) and [EPA's Interim Open Source Code Guidance](https://www.epa.gov/developers/open-source-software-and-epa-code-repository-requirements).

## License

All contributions to this project will be released under the CCO-1.0 license file dedication. By submitting a pull request or issue, you are agreeing to comply with this waiver of copyright interest.

## Disclaimer

This United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

## Contact

If you have any questions, please reach out to Cristina Mullin (mullin.cristina\@epa.gov).
