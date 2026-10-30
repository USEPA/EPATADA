# Plan: Refactor / reorganize R code (issue #1060)

Issue: "review and update function name conventions and .R file consolidation/organization" (related discussion: #1005).
Proposed branch: long-running `refactor/1060-code-organization` off `develop`, with small task branches/PRs merged into it.

## Goals

1. File housekeeping: consolidate/rename/split `.R` files so function location is predictable.
2. Naming: more specific names as the name moves right (e.g. `TADA_CreatePointAUs` -> `TADA_CreateAUPoints`).
3. Prune redundant or duplicated code.

Naming and file-structure decisions need team sign-off (Task 0) before code moves.

## Scan summary (develop, ~39.8k lines in 26 files in `R/`, 117 exports)

| File | Lines | Observations |
|---|---|---|
| `ATTAINSCrosswalks.R` | 5366 | Largest file. Mixes crosswalk, water type, uses/parameters, summary, salt/fresh, point AU functions. **`TADA_UpdateATTAINSWaterTypes` and `TADA_ReviewATTAINSWaterTypes` are each defined twice** (lines ~4383/4925 and ~4536/5053); the later definition silently wins. |
| `GeospatialFunctions.R` | 3467 | Mix of exported TADA functions and internal `fetch*` helpers (`fetch_bbox`, `fetch_au`, `fetchATTAINS`, `fetchNHD`); includes ATTAINS crosswalk functions (`TADA_CreateATTAINSAUMLCrosswalk`, `TADA_CreateAUMLCrosswalk`, `TADA_GetATTAINSByAUID`, `TADA_CreatePointAUGeometry`). |
| `GeospatialUtilities.R` | 2341 | Map-building helpers (`add*`, `prep*`, `check*`), NHD helpers (`.nhd_*`, `.safe_fetchNHD`), and ATTAINS fetch helpers (`fetchWaterType`). Overlaps in purpose with `GeospatialFunctions.R` and `Maps.R`. |
| `Utilities.R` | 3346 | Grab bag: general utils, plus spatial/map helpers (`getBboxJson`, `getFeatureLayer`, `writeLayerIfChanged`, `readLayer`, `TADA_addPolys`, `TADA_addPoints`, `getTribalPopup`, `pchIcons`), ATTAINS column helpers (`renameATTAINSCols`), color palette functions, export/CSV helpers. |
| `WQPWQXATTAINSCSTRefs.R` | 3460 | Ref-table getters/updaters for WQP, WQX, ATTAINS, and CST; overlaps in scope with `TADARefTables.R`, `TADACommunityHub.R`, `MaintenanceScheduled.R`. Contains internal `.tada_*` cache/IO helpers. |
| `TADARefTables.R` | 1587 | More ref-table getters/updaters (`TADA_Get*Ref`, `.TADA_Update*Ref`). |
| `CriteriaMethods.R`, `CriteriaAnalysis.R`, `CriteriaComparison.R`, `TADACommunityHub.R` | 3292 / 573 / 449 / 196 | Criteria workflow split over four files with inconsistent names (`TADA_Analysis_Join_WQP_Criteria` uses underscores; most use CamelCase). |
| `TADAGeospatialRefLayers.R` | 35 | Tiny; belongs with other geospatial/ref-layer code. |
| `autoClean.R`, `autoFilter.R` | 426 / 666 | File names are lowerCamel, others UpperCamel. |
| `ExampleData.R` | 153 | Docs-only (no functions). Fine, but check if it belongs with `MaintenanceScheduled.R` or a `data.R`. |

Naming inconsistencies observed:

- Non-prefixed internal helpers mix `snake_case`, `lowerCamel`, and `.dot` prefixes: `fetch_bbox`, `fetchATTAINS`, `getMapIconLabels`, `.nhd_get_layer`, `.tada_cache_get`, `.TADA_UpdateCharAliasRef`, `ask_user`, `make_groups`, `quiet`.
- Exported names with the pattern `TADA_<Verb><Object>` vs `TADA_<Object>_<Verb>` (`TADA_Analysis_Join_WQP_Criteria`, `TADA_CST_GetCriteria`).
- AU naming: `TADA_CreatePointAUs`, `TADA_CreatePointAUGeometry`, `TADA_CreateAUMLCrosswalk`, `TADA_CreateATTAINSAUMLCrosswalk`, `TADA_GetATTAINSAUMLCrosswalk`, `TADA_UpdateATTAINSAUMLCrosswalk`: overlapping and hard to distinguish. Need to check whether any are redundant.
- Ref functions are named both `TADA_Get<X>Ref` and `TADA_CST_Get<X>`.
- Update functions: `.TADA_Update*` (internal) vs `TADA_Update*` (exported) with no clear rule.

## Tasks

Order is chosen so that low-risk, mechanical steps come first and each task is a separate PR that passes `devtools::test()` and `devtools::check()`.

### Task 0: Decide conventions (discussion, no code)

- Write a short naming/file-structure guide (add to `vignettes/CONTRIBUTING.Rmd`):
  - Exported: `TADA_<Verb><Object><Qualifier>` (specific to the right).
  - Internal: choose one style (suggest `.tada_snake_case` or `.TADA_*`) and apply everywhere.
  - Files: one topic per file, UpperCamel names, max size guideline (e.g. split above ~2000 lines).
  - Deprecation policy for renamed exports.
- Agree on target file layout (see Task 6 proposal).
- Outcome: split into sub-issues per task below.

### Task 1: Baseline and safety net

- Record the current export list, function-to-file map, and reverse dependency usage (`NAMESPACE`, `_pkgdown.yml`, vignettes, tests, `dev/`).
- Generate a script (`dev/function-inventory.R`) that lists each function, file, exported status, and call sites, so every later task can diff against it.
- Confirm tests/`R CMD check` are green on `develop`; note slow/skipped tests (network-dependent tests in `test-GeospatialFunctions.R`, `test-ATTAINSCrosswalks.R`).

### Task 2: Remove clear duplicates and dead code (no behavior change)

- Delete the duplicate first definitions of `TADA_UpdateATTAINSWaterTypes` and `TADA_ReviewATTAINSWaterTypes` in `ATTAINSCrosswalks.R` (keep the later one that is currently active; diff the two first to confirm nothing is lost, including roxygen/examples).
- Find other unused internal functions (e.g. use `lintr::object_usage_linter`, `grep` against call sites): candidates include `fill_missing_assessment_unit_id`, `.nhd_*` variants, `TADA_RandomTestingData`, `checkColName`, `.sf_layer_equal`, `.get_downloads_path`.
- Look for near-duplicate helpers: `getBboxJson` / `createBBox` / `fetch_bbox`; `fetchATTAINS` / `fetchWaterType` / `fetch_au` / `TADA_GetATTAINSByAUID`; `TADA_addPolys`/`TADA_addPoints` vs `addATTAINS`/`addWQPSites`; `TADA_CreateAUMLCrosswalk` vs `TADA_CreateATTAINSAUMLCrosswalk`; `TADA_CreateCSV` vs `TADA_TableExport`.
- Look for repeated code chunks inside long functions (input checks, column-requirement checks, `.tada_*` helpers) and move to shared internal helpers.

### Task 3: Consolidate geospatial files

- Merge/redistribute `GeospatialFunctions.R`, `GeospatialUtilities.R`, `TADAGeospatialRefLayers.R`, and the spatial helpers currently in `Utilities.R` into a topic-based set, e.g.:
  - `GeospatialSpatial.R`: `TADA_MakeSpatial`, nearby-site functions, bbox helpers.
  - `GeospatialNHD.R`: `fetchNHD`, `.nhd_*`, `.checkNHD`.
  - `GeospatialATTAINSFetch.R`: ATTAINS fetch/check helpers (`fetchATTAINS`, `fetch_au`, `fetchWaterType`, `.checkATTAINS`).
  - `GeospatialRefLayers.R`: tribal layers and `TADA_UpdateTribalLayers`.
  - `MapHelpers.R`: `add*`, `prep*`, `get*Labels`, legend/layer control (alongside or merged into `Maps.R`).
- Move ATTAINS-specific functions out of geospatial files into the ATTAINS file group (Task 4), per the issue.
- Move tests to match (`test-GeospatialFunctions.R` etc.).
- Pure moves: no signature or content changes, to keep git history/diffs reviewable (use `git mv` where whole files are renamed).

### Task 4: Split and reorganize ATTAINS code

- Break up `ATTAINSCrosswalks.R` (5.4k lines), e.g.:
  - `ATTAINSAUCrosswalk.R`: AU/ML crosswalk create/get/update.
  - `ATTAINSWaterTypes.R`: water type update/review/assign, salt/fresh indicator.
  - `ATTAINSUses.R`: parameters, uses, and uses assignment.
  - `ATTAINSPointAUs.R`: point AU functions, `TADA_MLSummary` (or keep with the crosswalk).
- Move ATTAINS ref getters/updaters from `WQPWQXATTAINSCSTRefs.R` into the matching ATTAINS files, or into a single `ATTAINSRefs.R`.

### Task 5: Consolidate ref-table and CST code

- Separate `WQPWQXATTAINSCSTRefs.R` by domain: `WQXRefs.R` (WQX/WQP), `ATTAINSRefs.R`, `CSTRefs.R`; merge `TADARefTables.R` content with the WQX/TADA alias refs, and `.tada_*` cache/IO helpers into `RefTableHelpers.R`.
- Decide where `TADACommunityHub.R`, `CriteriaMethods.R`, `CriteriaAnalysis.R`, `CriteriaComparison.R` belong (suggest a `Criteria*` group, with `TADACommunityHub.R` moved into it or renamed).
- Align `MaintenanceScheduled.R` with the ref files (updaters live next to getters, scheduled wrapper calls them).

### Task 6: Tidy `Utilities.R` and remaining files

- Split `Utilities.R` into `Utilities.R` (generic checks, type/column helpers), `UtilitiesExport.R` (CSV/table export), `ColorPalette.R`, and move spatial/ATTAINS pieces per Tasks 3-4.
- Rename `autoClean.R`/`autoFilter.R` to `AutoClean.R`/`AutoFilter.R` for consistency (case-only rename on Windows/macOS requires a two-step `git mv`).
- Decide on `ExampleData.R` and `Tables.R` placement.
- Update `_pkgdown.yml` reference index to follow new groupings.

### Task 7: Rename functions (needs Task 0 decisions)

- Build a rename table (old -> new) in `dev/function-rename-map.csv`; examples: `TADA_CreatePointAUs` -> `TADA_CreateAUPoints`, harmonize AU/crosswalk names, `TADA_Analysis_Join_WQP_Criteria` -> CamelCase equivalent, `TADA_CST_Get*` vs `TADA_Get*Ref`.
- For exported functions: keep the old name as a deprecated wrapper using `.Deprecated()` (or `lifecycle::deprecate_warn()`), with `@keywords internal` and removal target version.
- For internals: rename in one pass using the map (scripted search/replace with word boundaries), then `devtools::document()`.
- Update tests, vignettes (`vignettes/*.Rmd`), README, `_pkgdown.yml`, `dev/` scripts, and `NEWS` / version bump in `DESCRIPTION`.
- Do renames in small batches grouped by file area, one PR each.

### Task 8: Final cleanup and verification

- Re-run function inventory (Task 1) and confirm only intended changes.
- `devtools::document()`, `devtools::test()`, `devtools::check()`, `pkgdown::check_pkgdown()`, URL checker test.
- Style pass (`air format`, per `air.toml`) limited to touched files to avoid noisy diffs.
- Update `CONTRIBUTING.Rmd` and add NEWS entry summarizing renames/deprecations.

## Working practices for the long-running branch

- Merge `develop` into the branch regularly (weekly) to limit conflicts, since many contributors touch the same large files.
- Keep file moves separate from content edits (different commits/PRs) so `git blame` and reviews stay useful.
- Prefer a freeze window on the affected files while the big moves (Tasks 3-6) land; announce to the team.
- Each PR: tests pass, no exported-function removal without a deprecation shim.

## Risks / open questions

- Exported-name changes affect external users and vignettes; decide the deprecation window.
- Network-dependent tests may mask regressions; consider mocking key ATTAINS/NHD calls or running them manually before merging.
- Confirm which of the two duplicate `TADA_UpdateATTAINSWaterTypes` / `TADA_ReviewATTAINSWaterTypes` definitions is intended (the later one is the one currently loaded).
- Whether `Collate` ordering or `@include` is required after moves (none in `DESCRIPTION` currently, so should be unaffected).
- Open-source collaborators (e.g. mhweber, who offered help in the issue) could own Tasks 3-4 (geospatial/ATTAINS).
