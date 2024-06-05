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

New features and/or edits should include all the following work:

-   [ ] Create or edit the function/code.

-   [ ] Document all code using comments to describe what is does.

-   [ ] Create or edit tests in tests/testthat folder.

-   [ ] Create or edit the function documentation. Include working
    examples.

-   [ ] Update or add the new functionality to the appropriate vignette
    (or create new one).

-   [ ] If function/code edits made as part of this issue impact other
    functions in the package or functionality in the shiny app, ensure
    those are updated as well.

-   [ ] Run devtools document() and check() and address any new comments
    or issues before creating a pull request.

-   [ ] Run TADA_UpdateAllRefs(), TADA_UpdateExampleData(), and
    styler::style_pkg() in Maintenance.R. Run more robust check for
    releases: devtools::check(manual = TRUE, remote = TRUE, incoming =
    TRUE)
