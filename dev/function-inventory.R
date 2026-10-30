# Function inventory for refactor (issue #1060, Task 1).
#
# Run from the package root:  source("dev/function-inventory.R")
#
# Outputs (written to dev/inventory/):
#   function-inventory.csv  one row per function definition in R/
#   function-callsites.csv  one row per (function, file) where it is referenced
#
# Uses base R parsing only. R/, tests/ and dev/ scripts are parsed (so comments
# and strings are ignored); vignettes, README and _pkgdown.yml are text-searched.

inventory_dir <- "dev/inventory"
dir.create(inventory_dir, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists("DESCRIPTION"), dir.exists("R"))

# ---- Definitions ------------------------------------------------------------

r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)

is_function_expr <- function(e) {
  is.call(e) && identical(e[[1]], as.name("function"))
}

get_definitions <- function(path) {
  exprs <- parse(path, keep.source = TRUE)
  srcref <- attr(exprs, "srcref")
  out <- list()
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (
      is.call(e) &&
        (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
        (is.name(e[[2]]) || is.character(e[[2]])) &&
        is_function_expr(e[[3]])
    ) {
      lines <- c(srcref[[i]][1], srcref[[i]][3])
      out[[length(out) + 1]] <- data.frame(
        name = as.character(e[[2]]),
        file = basename(path),
        line_start = lines[1],
        line_end = lines[2],
        n_lines = lines[2] - lines[1] + 1,
        n_args = length(e[[3]][[2]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}

defs <- do.call(rbind, lapply(r_files, get_definitions))

# ---- Export status ----------------------------------------------------------

ns <- readLines("NAMESPACE", warn = FALSE)
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
exports <- gsub("^\"|\"$", "", exports)

defs$exported <- defs$name %in% exports
defs$n_definitions <- ave(defs$name, defs$name, FUN = length)
defs$n_definitions <- as.integer(defs$n_definitions)
defs$duplicate_def <- defs$n_definitions > 1

# Does a Rd help page exist, and is it marked internal?
rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
rd_internal <- vapply(
  rd_files,
  function(f) any(grepl("\\\\keyword\\{internal\\}", readLines(f, warn = FALSE))),
  logical(1)
)
rd_names <- sub("\\.Rd$", "", basename(rd_files))
defs$has_rd <- defs$name %in% rd_names
defs$rd_internal <- defs$name %in% rd_names[rd_internal]

# Naming style of each function
defs$name_style <- ifelse(
  grepl("^\\.", defs$name),
  "internal_dot",
  ifelse(
    grepl("^TADA_", defs$name),
    "TADA_prefix",
    ifelse(grepl("_", defs$name), "snake_case", "camelCase_or_lower")
  )
)

# ---- Call sites -------------------------------------------------------------

fn_names <- unique(defs$name)

# Token-level references from parsed R files (ignores comments and strings).
# Also counts bare symbols, so functions passed as arguments (e.g. lapply(x, f))
# are detected.
count_refs_parsed <- function(path) {
  pd <- tryCatch(
    utils::getParseData(parse(path, keep.source = TRUE)),
    error = function(e) NULL
  )
  if (is.null(pd)) {
    return(NULL)
  }
  toks <- pd$text[pd$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")]
  toks <- toks[toks %in% fn_names]
  if (!length(toks)) {
    return(NULL)
  }
  tab <- table(toks)
  data.frame(
    name = names(tab),
    file = path,
    n_refs = as.integer(tab),
    stringsAsFactors = FALSE
  )
}

# Text search for non-R-parsed files (Rmd, md, yml). Word-boundary match.
count_refs_text <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hits <- vapply(
    fn_names,
    function(fn) {
      pat <- paste0("(?<![A-Za-z0-9_.])", gsub("\\.", "\\\\.", fn), "(?![A-Za-z0-9_])")
      m <- gregexpr(pat, txt, perl = TRUE)[[1]]
      if (m[1] == -1) 0L else length(m)
    },
    integer(1)
  )
  hits <- hits[hits > 0]
  if (!length(hits)) {
    return(NULL)
  }
  data.frame(
    name = names(hits),
    file = path,
    n_refs = as.integer(hits),
    stringsAsFactors = FALSE
  )
}

list_files <- function(dirs, pattern) {
  dirs <- dirs[dir.exists(dirs)]
  if (!length(dirs)) {
    return(character())
  }
  list.files(dirs, pattern = pattern, recursive = TRUE, full.names = TRUE)
}

parsed_files <- c(
  r_files,
  list_files("tests", "\\.[Rr]$"),
  list_files("dev", "\\.[Rr]$"),
  list_files("inst", "\\.[Rr]$")
)
# Exclude this script's own output dir/this file
parsed_files <- setdiff(parsed_files, "dev/function-inventory.R")

text_files <- c(
  list_files("vignettes", "\\.(Rmd|qmd|md)$"),
  list_files("inst", "\\.(Rmd|qmd|md)$"),
  intersect(c("README.md", "_pkgdown.yml", "NAMESPACE"), list.files(all.files = TRUE))
)
text_files <- setdiff(text_files, "NAMESPACE") # exports are handled separately

parsed_refs <- do.call(rbind, lapply(parsed_files, count_refs_parsed))
text_refs <- do.call(rbind, lapply(text_files, count_refs_text))
callsites <- rbind(parsed_refs, text_refs)

# Classify where each reference lives
callsites$location <- ifelse(
  grepl("^R/", callsites$file),
  "R",
  ifelse(
    grepl("^tests/", callsites$file),
    "tests",
    ifelse(
      grepl("^vignettes/", callsites$file),
      "vignettes",
      ifelse(grepl("^dev/", callsites$file), "dev", "other")
    )
  )
)

# Remove the definition itself from R/ counts: each definition contributes one
# SYMBOL token (the assignment target), so subtract it for the defining file.
def_counts <- aggregate(
  line_start ~ name + file,
  data = transform(defs, file = file.path("R", file)),
  FUN = length
)
names(def_counts)[3] <- "n_defs"
callsites <- merge(callsites, def_counts, by = c("name", "file"), all.x = TRUE)
callsites$n_defs[is.na(callsites$n_defs)] <- 0L
callsites$n_refs <- callsites$n_refs - callsites$n_defs
callsites$n_defs <- NULL
callsites <- callsites[callsites$n_refs > 0, ]
callsites <- callsites[order(callsites$name, callsites$file), ]

# ---- Summarise per function and attach to inventory -------------------------

sum_by <- function(loc) {
  x <- callsites[callsites$location == loc, ]
  if (!nrow(x)) {
    return(setNames(integer(), character()))
  }
  tapply(x$n_refs, x$name, sum)
}

for (loc in c("R", "tests", "vignettes", "dev", "other")) {
  s <- sum_by(loc)
  defs[[paste0("refs_", loc)]] <- as.integer(ifelse(
    is.na(s[defs$name]),
    0L,
    s[defs$name]
  ))
}

# Files in R/ that call each function (semi-colon separated)
r_callers <- callsites[callsites$location == "R", ]
callers_by_fn <- tapply(basename(r_callers$file), r_callers$name, function(x) {
  paste(unique(x), collapse = ";")
})
defs$called_from_R_files <- ifelse(
  is.na(callers_by_fn[defs$name]),
  "",
  callers_by_fn[defs$name]
)

# Flags used to prioritise cleanup
defs$unreferenced <- with(
  defs,
  !exported & refs_R == 0 & refs_tests == 0 & refs_vignettes == 0 & refs_dev == 0
)
defs$exported_untested <- with(defs, exported & refs_tests == 0)

defs <- defs[order(defs$file, defs$line_start), ]

utils::write.csv(defs, file.path(inventory_dir, "function-inventory.csv"), row.names = FALSE)
utils::write.csv(callsites, file.path(inventory_dir, "function-callsites.csv"), row.names = FALSE)

# ---- Console summary --------------------------------------------------------

message("Definitions: ", nrow(defs), " in ", length(unique(defs$file)), " files")
message("Exported: ", sum(defs$exported), " (NAMESPACE lists ", length(exports), ")")
message(
  "Exports in NAMESPACE with no function defined in R/: ",
  paste(setdiff(exports, defs$name), collapse = ", ")
)
message("Duplicate definitions: ", paste(unique(defs$name[defs$duplicate_def]), collapse = ", "))
message("Unreferenced internal functions: ", sum(defs$unreferenced))
message("Exported but not referenced in tests: ", sum(defs$exported_untested))
message("Wrote ", normalizePath(inventory_dir))

invisible(defs)
