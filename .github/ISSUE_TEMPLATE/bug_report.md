---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''
editor_options: 
  markdown: 
    wrap: 72
---

**Describe the bug**

A clear and concise description of what the bug is.

**To Reproduce**

Steps to reproduce the behavior:

1\. Go to '...'

2\. Click on '....'

3\. Scroll down to '....'

4\. See error

Code to reproduce the behavior:

``` r
library(EPATADA)

# Data used when you encountered the bug
df <- TADAdataRetrieval("a","b","c","d")

# TADA function and function inputs used when you encountered the bug
df2 <- EPATADA::TADA_ProblemFunction("a","b","c","d")
```

**Expected behavior**

A clear and concise description of what you expected to happen.

**Screenshots**

If applicable, add screenshots to help explain your problem.

**Additional context**

Add any other context about the problem here.

**Reminders for TADA contributors addressing this issue**

Bug fixes should include all the following work:

-   [ ] Create or edit the function/code.

-   [ ] Document all code using line/inline and/or multi-line/block comments
    to describe what is does.

-   [ ] Create or edit tests in tests/testthat folder to help prevent and/or 
    troubleshoot potential future issues.

-   [ ] Create or edit the function documentation. Include working
    examples.

-   [ ] Update or add the new functionality to the appropriate vignette
    (or create new one).

-   [ ] If function/code edits made as part of this issue impact other
    functions in the package or functionality in the shiny app, ensure
    those are updated as well.
    
-   [ ] Run TADA_UpdateAllRefs(), TADA_UpdateExampleData(), styler::style_pkg(),
    devtools::document(), and devtools::check() and address any new notes or 
    issues before creating a pull request.    

-   [ ] Run more robust check for releases: devtools::check(manual = TRUE, 
    remote = TRUE, incoming = TRUE)
