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


TADA_Analysis_apply_criteria <- function(.data_w_criteria, join = TRUE) {
  stopifnot(is.data.frame(.data_w_criteria))
  req <- c("DurationValue", "DurationUnit", "DurationMethod")
  missing <- setdiff(req, names(.data_w_criteria))
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
  
  df <- .data_w_criteria
  
  # Derive rolling flag from DurationMethod
  df$..rolling <- grepl("\\brolling\\b", df$DurationMethod, ignore.case = TRUE)
  
  # Normalized unit for grouping/window generation
  df$..du_norm <- normalize_unit(df$DurationUnit)
  
  # Keep only usable criteria rows
  df_valid <- df[!is.na(df$DurationValue) & !is.na(df$..du_norm) & nzchar(df$..du_norm), , drop = FALSE]
  
  if (nrow(df_valid) == 0L) {
    warning("TADA_Analysis_apply_criteria: No duration aggregation start or end date performed (DurationValue/DurationUnit are all NA). Returning input data unchanged.")
    return(.data_w_criteria)
  }
  
  # Unique combinations to iterate over
  combos <- df_valid |>
    dplyr::distinct(DurationValue, ..du_norm, ..rolling)
  
  out_list <- lapply(seq_len(nrow(combos)), function(i) {
    val <- combos$DurationValue[i]
    uni <- combos$..du_norm[i]
    rol <- combos$..rolling[i]
    
    # Subset rows for this combo
    sub_df <- df_valid[df_valid$DurationValue == val &
                         df_valid$..du_norm == uni &
                         df_valid$..rolling  == rol, , drop = FALSE]
    
    # Compute windows from this subset's date range
    win <- TADA_Analysis_DurationAgg(
      .data         = sub_df,
      durationValue = val,
      durationUnit  = uni,
      rolling       = rol
    )
    
    if (isTRUE(join)) {
      res <- TADA_Analysis_join_by_date_range(sub_df, win)
      # Ensure criteria columns are present/consistent
      res$DurationValue  <- val
      res$DurationUnit   <- uni
      res$DurationMethod <- if (all(is.na(sub_df$DurationMethod))) NA_character_ else
        sub_df$DurationMethod[which(!is.na(sub_df$DurationMethod))[1]]
      res$rolling <- rol
      res
    } else {
      # Return just the windows, tagged with criteria
      win$DurationValue  <- val
      win$DurationUnit   <- uni
      win$DurationMethod <- if (all(is.na(sub_df$DurationMethod))) NA_character_ else
        sub_df$DurationMethod[which(!is.na(sub_df$DurationMethod))[1]]
      win$rolling <- rol
      win
    }
  })
  
  out <- dplyr::bind_rows(out_list)
  
  # Drop helper columns if they leaked through
  out <- dplyr::select(out, -dplyr::any_of(c("..du_norm", "..rolling")))
  
  out
}



##### examples 

# think through how to group by parameters & uses next
cal_4d <- make_calendar_windows(Data_MT_MissoulaCounty, 4, durationUnit = "n-day")

cal_m <- make_calendar_windows(Data_MT_MissoulaCounty, 1, durationUnit = "n-month")

cal_3m <- make_calendar_windows(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")

roll_3m <- make_rolling_windows(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")

Data_MT_MissoulaCounty_Durations <- join_by_date_range(Data_MT_MissoulaCounty, cal_3m)
Data_MT_MissoulaCounty_Durations_roll <- join_by_date_range(Data_MT_MissoulaCounty, roll_3m)
