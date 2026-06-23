library(dplyr)
library(lubridate)
library(tibble)

TADA_Analysis_DurationAgg <- function(
    .data,
    durationValue,
    durationUnit,
    rolling = FALSE
    ) {
  normalize_unit <- function(u) {
    u <- tolower(trimws(u))
    u <- sub("^n-", "", u)
    u <- sub("s$", "", u)
    match.arg(u, c("hour","day","week","month","quarter"))
  }
  
  # Shift a POSIXct by n months using base seq(Date, by = "<n> months")
  add_months_by_seq <- function(t, n_months) {
    tz_out <- tryCatch(tz(t), error = function(e) NULL)
    d0 <- as.Date(t)
    d1 <- seq(d0, by = paste(n_months, "months"), length.out = 2L)[2L]
    as.POSIXct(d1, tz = if (!is.null(tz_out)) tz_out else "", origin = "1970-01-01")
  }
  
  add_step <- function(t, unit, n) {
    if (unit == "hour")    return(t + hours(n))
    if (unit == "day")     return(t + days(n))
    if (unit == "week")    return(t + weeks(n))
    if (unit == "month")   return(add_months_by_seq(t, n))
    if (unit == "quarter") return(add_months_by_seq(t, 3 * n))
    stop("Unsupported unit: ", unit)
  }

  # 1) Calendar-aligned windows
  make_calendar_windows <- function(
      .data,
      durationValue,
      durationUnit,
      week_start = 1
      ) {
    stopifnot(is.numeric(durationValue), durationValue > 0)
    unit <- normalize_unit(durationUnit)
    
    ts <- .data[["ActivityStartDateTime"]]
    if (is.null(ts)) stop('Column "ActivityStartDateTime" not found.')
    
    # Coerce to POSIXct if needed
    if (inherits(ts, "Date")) {
      ts <- as.POSIXct(ts)
    } else if (!inherits(ts, "POSIXct")) {
      if (is.character(ts)) {
        ts_parsed <- suppressWarnings(lubridate::ymd_hms(ts, quiet = TRUE))
        if (all(is.na(ts_parsed))) ts_parsed <- suppressWarnings(lubridate::ymd(ts, quiet = TRUE))
        if (all(is.na(ts_parsed))) stop('Could not parse "ActivityStartDateTime" to POSIXct.')
        ts <- ts_parsed
      } else {
        stop('"ActivityStartDateTime" must be POSIXct/Date or character parseable to a datetime.')
      }
    }
    
    ts <- ts[is.finite(ts)]
    if (length(ts) == 0) stop('No finite timestamps in "ActivityStartDateTime".')
    
    min_dt <- min(ts, na.rm = TRUE)
    max_dt <- max(ts, na.rm = TRUE)
    
    # Align first boundary to calendar boundary
    start0 <- if (unit == "week") {
      floor_date(min_dt, unit = "week", week_start = week_start)
    } else {
      floor_date(min_dt, unit = unit)
    }
    
    # Build boundaries; include final partial window
    boundaries <- c(start0)
    while (tail(boundaries, 1) < max_dt) {
      boundaries <- c(boundaries, add_step(tail(boundaries, 1), unit, durationValue))
    }
    if (length(boundaries) == 1L) {
      boundaries <- c(boundaries, add_step(boundaries, unit, durationValue))
    }
    
    windows <- tibble(
      window_start = boundaries[-length(boundaries)],
      window_end   = boundaries[-1]
    )
    
    # Branch once (no case_when)
    start_floor <- if (unit == "hour") {
      function(x) floor_date(x, "hour")
    } else {
      function(x) floor_date(x, "day")
    }
    
    end_adjust <- if (unit %in% c("month", "quarter")) {
      function(x) floor_date(x, "day") - seconds(1)  # last day 23:59:59
    } else {
      function(x) x - seconds(1)                     # one second before next boundary
    }
    
    windows <- windows |>
      mutate(
        window_start = start_floor(window_start),
        window_end   = end_adjust(window_end)
      )
    
    windows$window_start <- format(windows$window_start, "%Y-%m-%d %H:%M:%S")
    
    return(windows)
  }

  # 2) Rolling windows
  make_rolling_windows <- function(
    .data,
    durationValue,
    durationUnit,
    week_start = 1
  ) {
    stopifnot(is.numeric(durationValue), durationValue > 0)
    
    unit <- tolower(gsub("^n-", "", durationUnit))
    unit <- sub("s$", "", unit)
    unit <- match.arg(unit, c("hour","day","week","month","quarter"))
    
    ts <- .data[["ActivityStartDateTime"]]
    if (is.null(ts)) stop('Column "ActivityStartDateTime" not found.')
    tz_ref <- attr(ts, "tzone"); if (is.null(tz_ref) || tz_ref == "") tz_ref <- "UTC"
    
    # coerce to POSIXct
    if (inherits(ts, "POSIXct")) {
      # ok
    } else if (inherits(ts, "Date")) {
      ts <- as.POSIXct(ts, tz = tz_ref)
    } else if (is.character(ts)) {
      ts <- suppressWarnings(lubridate::ymd_hms(ts, tz = tz_ref, quiet = TRUE))
      if (all(is.na(ts))) ts <- suppressWarnings(lubridate::ymd(ts, tz = tz_ref, quiet = TRUE))
      if (all(is.na(ts))) stop('Could not parse "ActivityStartDateTime" to POSIXct.')
    } else if (is.numeric(ts)) {
      rng <- range(ts[is.finite(ts)], na.rm = TRUE)
      if (rng[2] > 1e12)       ts <- as.POSIXct(ts/1000, origin = "1970-01-01", tz = tz_ref)         # Unix ms
      else if (rng[2] > 1e9)   ts <- as.POSIXct(ts,       origin = "1970-01-01", tz = tz_ref)         # Unix s
      else if (rng[2] > 20000) ts <- as.POSIXct(ts*86400, origin = "1899-12-30", tz = tz_ref)         # Excel
      else                     ts <- as.POSIXct(as.Date(ts, origin = "1970-01-01"), tz = tz_ref)      # R Date numeric
    } else {
      stop('"ActivityStartDateTime" must be POSIXct/Date/character/numeric.')
    }
    
    ts <- ts[is.finite(ts)]
    if (!length(ts)) {
      return(data.frame(window_start = as.POSIXct(character()),
                        window_end   = as.POSIXct(character())))
    }
    
    min_dt <- min(ts); max_dt <- max(ts)
    
    # Build calendar-aligned period starts S and ends E, vectorized
    if (unit == "hour") {
      s0 <- lubridate::floor_date(min_dt, "hour")
      sL <- lubridate::floor_date(max_dt, "hour") + lubridate::hours(1)
      S  <- seq(s0, sL, by = "hour")
      E  <- S[-1] - lubridate::seconds(1)
      start_floor <- function(x) lubridate::floor_date(x, "hour")
    } else if (unit == "day") {
      s0 <- lubridate::floor_date(min_dt, "day")
      sL <- lubridate::floor_date(max_dt, "day") + lubridate::days(1)
      S  <- seq(s0, sL, by = "day")
      E  <- S[-1] - lubridate::seconds(1)
      start_floor <- function(x) lubridate::floor_date(x, "day")
    } else if (unit == "week") {
      s0 <- lubridate::floor_date(min_dt, "week", week_start = week_start)
      sL <- lubridate::floor_date(max_dt, "week", week_start = week_start) + lubridate::weeks(1)
      S  <- seq(s0, sL, by = "week")
      E  <- S[-1] - lubridate::seconds(1)
      start_floor <- function(x) lubridate::floor_date(x, "day")
    } else if (unit == "month") {
      d0 <- as.Date(lubridate::floor_date(min_dt, "month"))
      dL <- seq(as.Date(lubridate::floor_date(max_dt, "month")),
                by = "1 month", length.out = 2L)[2L]
      Sd <- seq(d0, dL, by = "1 month")
      S  <- as.POSIXct(Sd, tz = tz_ref)
      E  <- as.POSIXct(Sd[-1], tz = tz_ref) - lubridate::seconds(1)
      start_floor <- function(x) lubridate::floor_date(x, "day")
    } else { # quarter
      d0 <- as.Date(lubridate::floor_date(min_dt, "quarter"))
      dL <- seq(as.Date(lubridate::floor_date(max_dt, "quarter")),
                by = "3 months", length.out = 2L)[2L]
      Sd <- seq(d0, dL, by = "3 months")
      S  <- as.POSIXct(Sd, tz = tz_ref)
      E  <- as.POSIXct(Sd[-1], tz = tz_ref) - lubridate::seconds(1)
      start_floor <- function(x) lubridate::floor_date(x, "day")
    }
    
    if (length(E) < durationValue) {
      return(data.frame(window_start = as.POSIXct(character()),
                        window_end   = as.POSIXct(character())))
    }
    
    idx       <- seq.int(durationValue, length(E))
    win_start <- start_floor(S[idx + 1 - durationValue])
    win_end   <- E[idx]
    
    # Optional end adjust (identity for hour/day/week; snap to end-of-day for month/quarter)
    end_adjust <- if (unit %in% c("month", "quarter")) {
      function(x) lubridate::floor_date(x, "day") - lubridate::seconds(1)
    } else {
      function(x) x
    }
    
    windows <- data.frame(window_start = win_start, window_end = win_end, row.names = NULL)
    
    # Apply the same mutate + format as in your calendar branch
    windows <- windows |>
      dplyr::mutate(
        window_start = start_floor(window_start),
        window_end   = end_adjust(window_end)
      )
    
    windows$window_start <- format(windows$window_start, "%Y-%m-%d %H:%M:%S")
    
    return(windows)
  }

  if (isFALSE(rolling)) {
    message("TADA_Analysis_DurationAgg: rolling = FALSE was selected. Aggregating your WQP data set on a distinct calendar basis.")
    windows <- make_calendar_windows(.data, durationValue, durationUnit)
  }
  
  if (isTRUE(rolling)) {
    message("TADA_Analysis_DurationAgg: rolling = TRUE was selected. Aggregating your WQP data set on a rolling basis.")
    windows <- make_rolling_windows(.data, durationValue, durationUnit)
  }
  return(windows)
}



#' Join WQP Data with analysis windows by date range
#'
#' Assigns each observation date to all analysis windows whose date range
#' includes it. Attaches the rolling or calendar-aligned windows 
#' from `TADA_Analysis_DurationAgg` to a TADA dataset of observations.
#'
#' The function converts `ActivityStartDate` and `ActivityStartDateTime`, 
#' to a `Date` column type. It performs an inclusive range join where
#' `ActivityStartDate` falls within `[window_start_date, window_end_date]`
#' which is generated from [TADA_Analysis_DurationAgg]
#'
#' @param .data A TADA data frame. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#' @param windows A data.frame of analysis windows with columns
#' `window_start` and `window_end` produced by
#' [TADA_Analysis_DurationAgg()].
#'
#' @details
#' - The join uses `dplyr::join_by(dplyr::between(ActivityStartDate,
#'   window_start_date, window_end_date))` and is inclusive of both endpoints.
#' - Because this is a many-to-many join, a single observation date can match
#'   multiple windows if those windows overlap; the output will contain one row
#'   per match.
#' - Window start/end are reduced to dates before joining; sub-day time
#'   information is ignored for the match criterion.
#'
#' @return A data.frame containing the TADA data frame with the aggregated duration
#' columns: `window_start_date` and `window_end_date`. Rows are repeated when an
#' observation date falls into multiple windows. If no dates fall within any 
#' window ranges, the result is empty.
#'
#' @seealso [TADA_Analysis_DurationAgg] for constructing `windows`.
#'
#' @examples
#' cal_4d <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 4, durationUnit = "n-day")
#' 
#' cal_m <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 1, durationUnit = "n-month")
#' 
#' cal_3m <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")
#' 
#' roll_3m <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 3, durationUnit = "n-month", rolling = TRUE)
#' 
#' Data_MT_MissoulaCounty_Durations <- join_by_date_range(Data_MT_MissoulaCounty, cal_3m)
#' 
#' Data_MT_MissoulaCounty_Durations_roll <- join_by_date_range(Data_MT_MissoulaCounty, roll_3m)
#'
#' @export
TADA_Analysis_join_by_date_range <- function(data, windows) {
  # Ensure ActivityStartDate exists; derive from ActivityStartDateTime if needed
  if (!"ActivityStartDate" %in% names(data)) {
    if (!"ActivityStartDateTime" %in% names(data)) {
      stop('Need ActivityStartDate or ActivityStartDateTime in `data`.')
    }
    data <- dplyr::mutate(data, ActivityStartDate = as.Date(ActivityStartDateTime))
  }
  
  data <- dplyr::mutate(data, ActivityStartDate = as.Date(ActivityStartDateTime))
  
  # Reduce window bounds to Date
  win_dates <- windows |>
    dplyr::mutate(
      window_start_date = as.Date(window_start),
      window_end_date   = as.Date(window_end)
    )
  
  # Many-to-many inner join where ActivityStartDate is within [start, end]
  dplyr::inner_join(
    data,
    win_dates,
    dplyr::join_by(between(ActivityStartDate, window_start_date, window_end_date))
  )
}

#' Join a WQP + Criteria Table with Start and End Date Windows
#'
#' This data frame requires users to have already joined the WQP data frame 
#' with the TADA criteria table which can be generated from [TADA_]For each unique DurationUnit and DurationValue found in the 
#'
#' The function converts `ActivityStartDate` and `ActivityStartDateTime`, 
#' to a `Date` column type. It performs an inclusive range join where
#' `ActivityStartDate` falls within `[window_start_date, window_end_date]`
#' which is generated from [TADA_Analysis_DurationAgg]
#'
#' @param .data A TADA data frame. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#' @param windows A data.frame of analysis windows with columns
#' `window_start` and `window_end` produced by
#' [TADA_Analysis_DurationAgg()].
#'
#' @details
#' - The join uses `dplyr::join_by(dplyr::between(ActivityStartDate,
#'   window_start_date, window_end_date))` and is inclusive of both endpoints.
#' - Because this is a many-to-many join, a single observation date can match
#'   multiple windows if those windows overlap; the output will contain one row
#'   per match.
#' - Window start/end are reduced to dates before joining; sub-day time
#'   information is ignored for the match criterion.
#'
#' @return A data.frame containing the TADA data frame with the aggregated duration
#' columns: `window_start_date` and `window_end_date`. Rows are repeated when an
#' observation date falls into multiple windows. If no dates fall within any 
#' window ranges, the result is empty.
#'
#' @seealso [TADA_Analysis_DurationAgg] for constructing `windows`.
#'
#' @examples
#' cal_4d <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 4, durationUnit = "n-day")
#' 
#' cal_m <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 1, durationUnit = "n-month")
#' 
#' cal_3m <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")
#' 
#' roll_3m <- TADA_Analysis_DurationAgg(Data_MT_MissoulaCounty, 3, durationUnit = "n-month", rolling = TRUE)
#' 
#' Data_MT_MissoulaCounty_Durations <- join_by_date_range(Data_MT_MissoulaCounty, cal_3m)
#' 
#' Data_MT_MissoulaCounty_Durations_roll <- join_by_date_range(Data_MT_MissoulaCounty, roll_3m)
#'
#' @export
TADA_Analysis_Join_Windows <- function(data_w_criteria) {
  stopifnot(is.data.frame(data_w_criteria))
  req <- c("DurationValue", "DurationUnit", "DurationMethod")
  missing <- setdiff(req, names(data_w_criteria))
  if (length(missing) > 0) {
    stop("Input data is missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # Normalize unit (lowercase, remove n- prefix and trailing s)
  normalize_unit <- function(u) {
    u <- tolower(trimws(u))
    u <- sub("^n-", "", u)
    u <- sub("s$", "", u)
    u
  }
  
  # Harmonize date/time types once up front
  harmonize_datetime_cols <- function(df) {
    if ("ActivityStartDateTime" %in% names(df)) {
      ts <- df$ActivityStartDateTime
      if (!inherits(ts, "POSIXct")) {
        if (inherits(ts, "Date")) {
          ts <- as.POSIXct(ts)
        } else if (is.character(ts)) {
          ts_try <- suppressWarnings(lubridate::ymd_hms(ts, quiet = TRUE))
          if (all(is.na(ts_try))) ts_try <- suppressWarnings(lubridate::ymd(ts, quiet = TRUE))
          ts <- ts_try
        } else {
          # fall back: try as POSIXct directly
          ts <- tryCatch(as.POSIXct(ts), error = function(e) as.POSIXct(NA))
        }
      }
      df$ActivityStartDateTime <- ts
      df$ActivityStartDate <- as.Date(ts)
    } else if ("ActivityStartDate" %in% names(df)) {
      df$ActivityStartDate <- as.Date(df$ActivityStartDate)
    }
    df
  }
  
  df <- harmonize_datetime_cols(data_w_criteria)
  
  # Derive rolling flag from DurationMethod
  df$rolling <- grepl("\\brolling\\b", df$DurationMethod, ignore.case = TRUE)
  
  # Normalized unit for grouping/window generation
  df$du_norm <- normalize_unit(df$DurationUnit)
  
  # Split into valid and invalid based on duration
  is_valid <- !is.na(df$DurationValue) & !is.na(df$du_norm) & nzchar(df$du_norm)
  df_valid   <- df[is_valid, , drop = FALSE]
  df_invalid <- df[!is_valid, , drop = FALSE]
  
  # Identifier columns for splitting
  id_cols <- c("TADA.ComparableDataIdentifier",
               "TADA.CharacteristicName",
               "TADA.ResultSampleFractionText",
               "TADA.MethodSpeciationName")
  have_ids <- id_cols[id_cols %in% names(df_valid)]
  if (length(have_ids) < length(id_cols)) {
    warning("Some identifier columns are missing: ",
            paste(setdiff(id_cols, have_ids), collapse = ", "),
            ". Proceeding with available identifiers.")
  }
  
  # Build results for valid criteria
  out_valid <- NULL
  if (nrow(df_valid) > 0L) {
    # Split by Duration settings AND identifiers
    df_groups <- df_valid |>
      dplyr::group_by(
        DurationValue, du_norm, rolling,
        dplyr::across(dplyr::all_of(have_ids))
      ) |>
      dplyr::group_split()
    
    out_list <- lapply(df_groups, function(sub_df) {
      # Extract grouping values from the first row of the subgroup
      val <- sub_df$DurationValue[1]
      uni <- sub_df$du_norm[1]
      rol <- sub_df$rolling[1]
      
      # Build windows for this subgroup
      win <- TADA_Analysis_DurationAgg(
        .data         = sub_df, # use the beg and end dates 
        durationValue = val,
        durationUnit  = uni,
        rolling       = rol
      )
      
      # Join observations to windows for this subgroup
      res <- TADA_Analysis_join_by_date_range(sub_df, win)
      
      # Carry Duration metadata
      res$DurationValue  <- val
      res$DurationUnit   <- uni
      res$DurationMethod <- if (all(is.na(sub_df$DurationMethod))) NA_character_
      else sub_df$DurationMethod[which(!is.na(sub_df$DurationMethod))[1]]
      res$rolling <- rol
      
      # Carry identifier columns from the subgroup
      for (cc in have_ids) {
        res[[cc]] <- sub_df[[cc]][1]
      }
      
      res
    })
    
    out_valid <- dplyr::bind_rows(out_list)
  }
  
  # Build rows for invalid criteria: windows equal to ActivityStartDateTime
  out_invalid <- NULL
  if (nrow(df_invalid) > 0L) {
    ts_invalid <- if ("ActivityStartDateTime" %in% names(df_invalid)) df_invalid$ActivityStartDateTime else {
      if ("ActivityStartDate" %in% names(df_invalid)) as.POSIXct(df_invalid$ActivityStartDate) else as.POSIXct(NA)
    }
    ts_char <- ifelse(is.na(ts_invalid), NA_character_, format(ts_invalid, "%Y-%m-%d %H:%M:%S"))
    
    out_invalid <- df_invalid
    out_invalid$window_start       <- ts_char
    out_invalid$window_end         <- ts_invalid
    out_invalid$window_start_date  <- as.Date(ts_invalid)
    out_invalid$window_end_date    <- as.Date(ts_invalid)
  }
  
  # Final bind; ensure consistent types again on ActivityStartDate
  cast_activity_date <- function(x) {
    if ("ActivityStartDate" %in% names(x)) x$ActivityStartDate <- as.Date(x$ActivityStartDate)
    x
  }
  out_valid   <- if (!is.null(out_valid)) cast_activity_date(out_valid) else NULL
  out_invalid <- if (!is.null(out_invalid)) cast_activity_date(out_invalid) else NULL
  
  if (is.null(out_valid) && is.null(out_invalid)) {
    return(data_w_criteria)
  }
  
  out <- dplyr::bind_rows(out_valid, out_invalid)
  
  # Drop helper columns if present
  out <- dplyr::select(out, -dplyr::any_of(c("du_norm", "rolling")))
  
  return(out)
}



##### examples 

# think through how to group by parameters & uses next
cal_4d <- make_calendar_windows(Data_MT_MissoulaCounty, 4, durationUnit = "n-day")

cal_m <- make_calendar_windows(Data_MT_MissoulaCounty, 1, durationUnit = "n-month")

cal_3m <- make_calendar_windows(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")

roll_3m <- make_rolling_windows(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")

Data_MT_MissoulaCounty_Durations <- join_by_date_range(Data_MT_MissoulaCounty, cal_3m)
Data_MT_MissoulaCounty_Durations_roll <- join_by_date_range(Data_MT_MissoulaCounty, roll_3m)





filt <- dplyr::distinct(final_MT_data_criteria, TADA.ComparableDataIdentifier,
                TADA.CharacteristicName,
                TADA.ResultSampleFractionText,
                TADA.MethodSpeciationName,DurationValue, DurationUnit) |>
  tidyr::drop_na()

wqp_criteria_durations <- list()


for (i in 1:nrow(filt)) {
  filt_data <- Data_MT_MissoulaCounty |> dplyr::semi_join(filt[i,])
  
  wqp_criteria_durations[[i]] <- TADA_Analysis_DurationAgg(filt_data, filt$DurationValue[i], filt$DurationUnit[i])
}

final_df <- dplyr::bind_rows(wqp_criteria_durations[[1]],wqp_criteria_durations[[2]])


TADA_Analysis_Join_Windows2 <- function(data_w_criteria, join_back = TRUE) {
  stopifnot(is.data.frame(data_w_criteria))
  
  id_cols <- c(
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  req <- c(id_cols, "DurationValue", "DurationUnit")
  missing <- setdiff(req, names(data_w_criteria))
  if (length(missing) > 0) {
    stop("Input data is missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # Harmonize date/time columns for later use
  harmonize_datetime_cols <- function(df) {
    if ("ActivityStartDateTime" %in% names(df)) {
      ts <- df$ActivityStartDateTime
      if (!inherits(ts, "POSIXct")) {
        if (inherits(ts, "Date")) {
          ts <- as.POSIXct(ts)
        } else if (is.character(ts)) {
          ts_try <- suppressWarnings(lubridate::ymd_hms(ts, quiet = TRUE))
          if (all(is.na(ts_try))) ts_try <- suppressWarnings(lubridate::ymd(ts, quiet = TRUE))
          ts <- ts_try
        } else {
          ts <- tryCatch(as.POSIXct(ts), error = function(e) as.POSIXct(NA))
        }
      }
      df$ActivityStartDateTime <- ts
      df$ActivityStartDate <- as.Date(ts)
    } else if ("ActivityStartDate" %in% names(df)) {
      df$ActivityStartDate <- as.Date(df$ActivityStartDate)
    }
    df
  }
  df <- harmonize_datetime_cols(data_w_criteria)
  
  # Distinct combos INCLUDING NA so we can produce per-observation windows for them
  filt_all <- df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(req)))
  
  if (nrow(filt_all) == 0) return(dplyr::tibble())
  
  out_list <- vector("list", nrow(filt_all))
  
  # Helper for NA-safe equality on identifiers
  equal_or_both_na <- function(x, y) {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
  }
  
  for (i in seq_len(nrow(filt_all))) {
    combo <- filt_all[i, , drop = FALSE]
    
    dur_val  <- combo$DurationValue
    dur_unit <- combo$DurationUnit
    valid_combo <- !is.na(dur_val) & !is.na(dur_unit)
    
    if (valid_combo) {
      # Fully-specified combo: use semi_join and build windows via DurationAgg
      filt_data <- df |>
        dplyr::semi_join(combo, by = c(id_cols, "DurationValue", "DurationUnit"))
      
      if (nrow(filt_data) == 0) {
        out_list[[i]] <- dplyr::tibble()
        next
      }
      
      win <- TADA_Analysis_DurationAgg(
        .data         = filt_data,
        durationValue = dur_val,
        durationUnit  = dur_unit
      )
      
      # Ensure identifiers + duration fields are present on the window output
      for (cc in id_cols) if (!cc %in% names(win)) win[[cc]] <- combo[[cc]]
      if (!"DurationValue" %in% names(win)) win$DurationValue <- dur_val
      if (!"DurationUnit" %in% names(win))  win$DurationUnit  <- dur_unit
      
      if (isTRUE(join_back)) {
        # Optionally attach windows to observations
        win_joined <- TADA_Analysis_join_by_date_range(filt_data, win)
        for (cc in id_cols) win_joined[[cc]] <- combo[[cc]]
        win_joined$DurationValue <- dur_val
        win_joined$DurationUnit  <- dur_unit
        out_list[[i]] <- win_joined
      } else {
        out_list[[i]] <- win
      }
      
    } else {
      # NA DurationValue and/or DurationUnit: append per-observation windows
      # Filter rows by identifiers only (NA-safe)
      mask <- rep(TRUE, nrow(df))
      for (cc in id_cols) {
        mask <- mask & equal_or_both_na(df[[cc]], combo[[cc]])
      }
      # And match NA-ness of duration fields to the combo
      mask <- mask &
        (if (is.na(dur_val)) is.na(df$DurationValue) else df$DurationValue == dur_val) &
        (if (is.na(dur_unit)) is.na(df$DurationUnit) else df$DurationUnit == dur_unit)
      
      sub_na <- df[mask, , drop = FALSE]
      
      if (nrow(sub_na) == 0) {
        out_list[[i]] <- dplyr::tibble()
        next
      }
      
      # Windows equal to ActivityStartDateTime (like before)
      ts <- if ("ActivityStartDateTime" %in% names(sub_na)) {
        sub_na$ActivityStartDateTime
      } else if ("ActivityStartDate" %in% names(sub_na)) {
        as.POSIXct(sub_na$ActivityStartDate)
      } else {
        as.POSIXct(NA)
      }
      ts_char <- ifelse(is.na(ts), NA_character_, format(ts, "%Y-%m-%d %H:%M:%S"))
      
      win_na <- dplyr::tibble(
        window_start      = ts_char,
        window_end        = ts,
        window_start_date = as.Date(ts),
        window_end_date   = as.Date(ts)
      )
      
      # Carry identifiers + duration fields for this NA combo
      for (cc in id_cols) win_na[[cc]] <- combo[[cc]]
      win_na$DurationValue <- dur_val
      win_na$DurationUnit  <- dur_unit
      
      # If join_back = TRUE, these are already per-observation; just append
      out_list[[i]] <- win_na
    }
  }
  
  dplyr::bind_rows(out_list)
}
