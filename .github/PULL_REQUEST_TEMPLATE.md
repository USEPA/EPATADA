### Pull Request Checklist

-   [ ] Ensure that your branch is up-to-date with the `develop` branch. Check for any merge conflicts and resolve them if needed.
  
-   [ ] Include a summary of the changes made and any relevant context in the pull request description.

-   [ ] Link any related issues or pull requests. Use keywords like "Closes #issue_number" to automatically close related issues when the pull request is merged.

-   [ ] Document all code using line/inline and/or multi-line/block comments to describe what it does.
    
-   [ ] Create or edit the function documentation. Include working examples. Run `devtools::document()`.

-   [ ] Update or add the new functionality to the appropriate vignette (or create a new one). Make sure the vignette is included in the articles section of the `_pkgdown.yml`.

-   [ ] Run spelling maintenance in `requiredMaintenance.R`.

-   [ ] If function/code edits made as part of this issue impact other functions in the package or functionality in the shiny app, ensure those are updated as well.

-   [ ] Run `.TADA_UpdateRefFiles()` and `.TADA_UpdateExampleData()` in `MaintenanceScheduled.R` locally or manually run the [Component File Update](https://github.com/USEPA/EPATADA/actions/workflows/maintenance-update.yaml) action.

-   [ ] If new example data files were created ensure these are documented in `ExampleData.R` and also included in the `MaintenanceScheduled.R` to allow for regular refresh.

-   [ ] If you created any new columns or made changes, update `RequiredCols.R`.

-   [ ] Ensure that any new dependencies are added to the `DESCRIPTION` file and documented appropriately.

-   [ ] Create or edit tests in the `tests/testthat` folder to help prevent and/or troubleshoot potential future issues. Ensure that all new and existing tests pass after your changes. Run `devtools::test()` and verify.

-   [ ] Run `devtools::check()` and address any new notes or issues before creating a pull request.

-   [ ] Request a review from at least one other developer on the team.
