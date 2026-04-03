#' List Criteria and Methodologies Files in TADACommunityHub
#'
#' Retrieves the complete listing of Criteria and Methodologies files from the TADACommunityHub repository.
#'
#' @param pkg The GitHub TADACommunityHub package. Ensures the package is downloaded.
#' Should not be modified unless package name reference changes.
#'
#' @return A data frame with four columns.
#'
#' @export
#'
#' @examples
#' criteriaFiles <- TADA_GetCriteriaFiles()
#'
# List available criteria files from another installed package's inst/extdata
TADA_GetCriteriaFiles <- function(pkg = "TADACommunityHub") {
  # checks if the package is installed.
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '",
      pkg,
      "' is not installed. Please install it to use this function."
    )
  }

  ext_dir <- system.file("extdata", package = pkg)
  if (!nzchar(ext_dir) || !dir.exists(ext_dir)) {
    stop("No extdata directory found in package '", pkg, "'.")
  }

  # The "default_files.xlsx" used to map display names -> ATTAINS org IDs
  default_path <- file.path(ext_dir, "default_files.xlsx")
  default_df <- NULL
  if (file.exists(default_path)) {
    default_df <- openxlsx::read.xlsx(default_path)
  } else {
    warning(
      "default_files.xlsx not found in ",
      pkg,
      "/inst/extdata; ",
      "ATTAINS.OrganizationIdentifier mapping will be NA."
    )
  }

  # Find all TADACommunityHub _criteria_crosswalk.xlsx files
  xlsx_files <- list.files(
    ext_dir,
    pattern = "_criteria_crosswalk\\.xlsx$",
    full.names = TRUE
  )

  # If no files return, return blank data frame and report issue.
  if (length(xlsx_files) == 0) {
    warning(
      "No criteria crosswalk files found in ",
      pkg,
      "/inst/extdata. Please report issue to TADACommunityHub."
    )
    return(data.frame(
      ATTAINS.OrganizationIdentifier = character(),
      display_name = character(),
      file_name = character(),
      file_path = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Build the reference table
  res <- lapply(xlsx_files, function(fp) {
    fname <- basename(fp)
    # Display name derived from filename
    display_name <- gsub("_criteria_crosswalk\\.xlsx$", "", fname)
    display_name <- gsub("_", " ", display_name)
    display_name <- tools::toTitleCase(display_name)

    data.frame(
      display_name = display_name,
      file_name = fname,
      file_path = fp,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, res)

  # Join ATTAINS org ID if default_files.xlsx is available
  if (
    !is.null(default_df) &&
      all(
        c("Display.Name", "ATTAINS.OrganizationIdentifier") %in%
          names(default_df)
      )
  ) {
    result <- merge(
      x = result,
      y = default_df[, c("Display.Name", "ATTAINS.OrganizationIdentifier")],
      by.x = "display_name",
      by.y = "Display.Name",
      all.x = TRUE,
      sort = FALSE
    )
    # Reorder columns
    result <- result[, c(
      "ATTAINS.OrganizationIdentifier",
      "display_name",
      "file_name",
      "file_path"
    )]
  } else {
    # If the mapping isn't available, fill with NA column
    result$ATTAINS.OrganizationIdentifier <- NA_character_
    result <- result[, c(
      "ATTAINS.OrganizationIdentifier",
      "display_name",
      "file_name",
      "file_path"
    )]
  }

  result
}

#' Load TADACommunityHub Criteria File
#'
#' A function to download the criteria data frame from TADACommunityHub
#'
#' @param org_id Character string. The ATTAINS organization identifier should be
#' supplied by the user consistent with the TADA package mod 3 workflow.
#'
#' @param state_tribe Character string. This is the state or tribe name used
#' that is consistent with what is found in the UI of the module 3 ShinyAnalyze
#' app (note to developers: we should review the spelling of these names to ensure
#' consistency with ATTAINS state or tribe names.)
#'
#' @param ref a data frame with four columns from [TADA_GetCriteriaFiles()]
#'
#' @param pkg The GitHub TADACommunityHub package. Ensures the package is downloaded.
#' Should not be modified unless package name reference changes.
#'
#' @return a data frame containing the TADACommunityHub TADA criteria table
#' based on the user supplied org_id or state_tribe name.
#'
#' @export
#'
#' @examples
#' loadCriteria_MTDEQ <- TADA_LoadCriteriaFile(org_id = "MTDEQ")
#'
# Load a selected criteria file (by org_id or state_tribe display name) from local inst/extdata
TADA_LoadCriteriaFile <- function(
  org_id = NULL,
  state_tribe = NULL,
  ref = NULL,
  pkg = "TADACommunityHub"
) {
  if (is.null(ref)) {
    ref <- TADA_GetCriteriaFiles(pkg = pkg)
  }

  # Only one of org_id or state_tribe should be populated
  if (all(is.null(org_id), is.null(state_tribe))) {
    stop("loadCriteria: You must provide either org_id or state_tribe.")
  }
  if (all(!is.null(org_id), !is.null(state_tribe))) {
    stop("loadCriteria: Please provide only one of org_id or state_tribe.")
  }

  # Select file_path based on which identifier was provided
  if (!is.null(state_tribe)) {
    if (!state_tribe %in% ref$display_name) {
      stop("loadCriteria: state_tribe not found (check spelling).")
    }
    file_path <- ref[ref$display_name == state_tribe, "file_path", drop = TRUE]
  } else {
    if (!org_id %in% ref$ATTAINS.OrganizationIdentifier) {
      stop("loadCriteria: org_id not found (check value).")
    }
    file_path <- stats::na.omit(ref[
      ref$ATTAINS.OrganizationIdentifier == org_id,
      "file_path",
      drop = TRUE
    ])
  }

  # If multiple matches, take the first but warn if this occurs (should not happen)
  if (length(file_path) > 1) {
    warning("Multiple matching files found; using the first.")
    file_path <- file_path[1]
  }

  if (!nzchar(file_path) || !file.exists(file_path)) {
    stop("Selected file could not be found on disk: ", file_path)
  }

  df <- openxlsx::read.xlsx(file_path)

  # create a dummy reference to a TADACommunityHub function, Rmd checks will create a warning if no reference to any functions are made from an Imports package.
  temp <- TADACommunityHub::validateATTAINSParam(df)
  rm(temp)

  return(df)
}
