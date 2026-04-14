### Pull Request Checklist (convert PR to draft if in progress)

Merge upstream

-   [ ] Update from latest `develop`; resolve conflicts

Documentation

-   [ ] Refresh inline/block comments for clarity

-   [ ] Update roxygen docs and include examples; review help pages

Interdependencies

-   [ ] If columns were added/updated, update `RequiredCols.R`

-   [ ] Add/update vignettes for corresponding changes in functionality, list these under articles in _pkgdown.yml, and ensure added/updated vignettes run and build with proper formatting locally

-   [ ] If changes affect other package or the shiny app functions, update those impacted functions accordingly

Tests

-   [ ] Add/update tests in `tests/testthat`; review the bot's coverage report from [test-coverage](https://github.com/USEPA/EPATADA/blob/develop/.github/workflows/test-coverage.yaml) and confirm all changes are covered

-   [ ] Review the bot's spelling comment (only posted if the test fails). Run spelling::spell_check_package() locally and fix any misspellings; add approved project terms to WORDLIST with spelling::update_wordlist()

Maintenance & Data Refresh 

-   [ ] Run `.TADA_UpdateRefFiles()` and `.TADA_UpdateExampleData()` locally via `MaintenanceScheduled.R` or trigger the [Component File Update](https://github.com/USEPA/EPATADA/actions/workflows/maintenance-update.yaml) GitHub Action

-   [ ] If new example data files were added, document them in `ExampleData.R` and include them in `MaintenanceScheduled.R` for regular refresh

Final preparation

-   [ ] Run devtools::test(), devtools::check(), and devtools::document() locally; ensure tests pass and fix any errors, warnings, or notes. Add new dependencies to `DESCRIPTION` and document appropriately

-   [ ] Include a summary of the changes made and relevant context/motivation

-   [ ] Link issues to auto-close on merge (use Development sidebar or include "Closes #<issue-number>" in the PR)

-   [ ] Request review from at least one developer team member (convert PR to ready for review if it was designated as in progress)
