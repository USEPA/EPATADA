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
library(TADA)

# Data used when you encountered the bug
df <- TADAdataRetrieval("a","b","c","d")

# TADA function and function inputs used when you encountered the bug
df2 <- TADA_ProblemFunction("a","b","c","d")
```

**Expected behavior**

A clear and concise description of what you expected to happen.

**Screenshots**

If applicable, add screenshots to help explain your problem.

**Session Info**

Please include your session info:

``` r
sessionInfo()
#OR preferred:
devtools::session_info()
```

**Additional context**

Add any other context about the problem here.

**Reminders for TADA contributors addressing this issue**

New features should include all of the following work:

-   [ ] Create the function/code.

-   [ ] Document all code using comments to describe what is does.

-   [ ] Create tests in tests folder.

-   [ ] Create help file using roxygen2 above code.

-   [ ] Create working examples in help file (via roxygen2).

-   [ ] Add to appropriate vignette (or create new one).
