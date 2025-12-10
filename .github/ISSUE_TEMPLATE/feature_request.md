---
name: Feature request
about: Suggest an idea for this project
title: ''
labels: ''
assignees: ''
editor_options: 
  markdown: 
    wrap: 72
---

**Is your feature request related to a problem? Please describe:**

A clear and concise description of what the problem is. Ex. I'm always
frustrated when [...]

**Describe the solution you'd like:**

A clear and concise description of what you want to happen. If possible,
include an example of how you would like to see the code work:

``` r
library(TADA)
super_cool_feature(x, y)
```

**Describe alternatives you've considered:**

A clear and concise description of any alternative solutions or features
you've considered.

**Additional context:**

Add any other context or screenshots about the feature request here.

**Reminders for TADA contributors addressing this issue:**

-   [ ] Create or edit the function/code.

-   [ ] Document all code using line/inline and/or multi-line/block comments
    to describe what is does.

-   [ ] Create or edit tests in tests/testthat folder to help prevent and/or 
    troubleshoot potential future issues.

-   [ ] Create or edit the function documentation. Include working
    examples.

-   [ ] Update or add the new functionality to the appropriate vignette
    (or create new one). Make sure the vignette is included in the articles 
    section of the _pkgdown.yml

-   [ ] If function/code edits made as part of this issue impact other
    functions in the package or functionality in the shiny app, ensure
    those are updated as well.
    
-   [ ] Run .TADA_UpdateRefFiles() and .TADA_UpdateExampleData() in MaintenanceScheduled.R
    All example data files need to be documented in ExampleData.R and also included
    in the MaintenanceScheduled.R so they get re-generated correctly during the scheduled 
    routine maintenance (see maintenance-scheduled.yaml).

-   [ ] Run devtools::document() and devtools::check() and address any new notes or 
    issues before creating a pull request.    
    
-   [ ] Run spelling and styler in requiredMaintenance.R

-   [ ] If you created any new columns or made changes, update RequiredCols.R

-   [ ] Run more robust check for releases: devtools::check(manual = TRUE, 
    remote = TRUE, incoming = TRUE)
