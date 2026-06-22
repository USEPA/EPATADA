library(dplyr)
library(lubridate)
library(tibble)

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
  # normalize unit
  unit <- tolower(gsub("^n-", "", durationUnit)); unit <- sub("s$", "", unit)
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
  } else stop('"ActivityStartDateTime" must be POSIXct/Date/character/numeric.')
  
  ts <- ts[is.finite(ts)]
  if (!length(ts)) stop('No finite timestamps in "ActivityStartDateTime".')
  min_dt <- min(ts); max_dt <- max(ts)
  
  # build calendar-aligned period starts S, ends E = next start - 1 sec
  if (unit == "hour") {
    S <- seq(lubridate::floor_date(min_dt, "hour"),
             lubridate::floor_date(max_dt, "hour") + lubridate::hours(1),
             by = "hour")
    E <- S[-1] - lubridate::seconds(1)
    floor_start <- function(x) lubridate::floor_date(x, "hour")
  } else if (unit == "day") {
    S <- seq(lubridate::floor_date(min_dt, "day"),
             lubridate::floor_date(max_dt, "day") + lubridate::days(1),
             by = "day")
    E <- S[-1] - lubridate::seconds(1)
    floor_start <- function(x) lubridate::floor_date(x, "day")
  } else if (unit == "week") {
    s0 <- lubridate::floor_date(min_dt, "week", week_start = week_start)
    sL <- lubridate::floor_date(max_dt, "week", week_start = week_start) + lubridate::weeks(1)
    S <- seq(s0, sL, by = "week")
    E <- S[-1] - lubridate::seconds(1)
    floor_start <- function(x) lubridate::floor_date(x, "day")
  } else if (unit == "month") {
    S <- lubridate::floor_date(min_dt, "month")
    while (tail(S,1) <= lubridate::floor_date(max_dt, "month")) {
      next_s <- as.POSIXct(seq(as.Date(tail(S,1)), by = "1 month", length.out = 2)[2], tz = tz_ref)
      S <- c(S, next_s)
      if (next_s > lubridate::floor_date(max_dt, "month")) break
    }
    E <- S[-1] - lubridate::seconds(1)  # end-of-month
    floor_start <- function(x) lubridate::floor_date(x, "day")
  } else { # quarter
    S <- lubridate::floor_date(min_dt, "quarter")
    while (tail(S,1) <= lubridate::floor_date(max_dt, "quarter")) {
      next_s <- as.POSIXct(seq(as.Date(tail(S,1)), by = "3 months", length.out = 2)[2], tz = tz_ref)
      S <- c(S, next_s)
      if (next_s > lubridate::floor_date(max_dt, "quarter")) break
    }
    E <- S[-1] - lubridate::seconds(1)  # end-of-quarter
    floor_start <- function(x) lubridate::floor_date(x, "day")
  }
  
  # one window per period end, covering previous `durationValue` full periods
  if (length(E) < durationValue) return(data.frame(window_start = as.POSIXct(character()),
                                                    window_end   = as.POSIXct(character())))
  idx <- seq.int(durationValue, length(E))
  win_start <- floor_start(S[idx + 1 - durationValue])
  win_end   <- E[idx]
  attr(win_start, "tzone") <- tz_ref; attr(win_end, "tzone") <- tz_ref
  windows <- data.frame(window_start = win_start, window_end = win_end, row.names = NULL)
  
  windows$window_start <- format(windows$window_start, "%Y-%m-%d %H:%M:%S")
  
  return(windows)
}


join_by_date_range <- function(data, windows) {
  # Ensure ActivityStartDate exists; derive from ActivityStartDateTime if needed
  if (!"ActivityStartDate" %in% names(data)) {
    if (!"ActivityStartDateTime" %in% names(data)) {
      stop('Need ActivityStartDate or ActivityStartDateTime in `data`.')
    }
    data <- mutate(data, ActivityStartDate = as.Date(ActivityStartDateTime))
  }
  
  data <- EPATADA::TADA_CorrectColType(data)
  
  # Reduce window bounds to Date
  win_dates <- windows |>
    mutate(
      window_start_date = as.Date(window_start),
      window_end_date   = as.Date(window_end)
    )
  
  # Many-to-many inner join where ActivityStartDate is within [start, end]
  inner_join(
    data,
    win_dates,
    join_by(between(ActivityStartDate, window_start_date, window_end_date))
  )
}

##### examples 

# think through how to group by parameters & uses next
cal_4d <- make_calendar_windows(Data_MT_MissoulaCounty, 4, durationUnit = "n-day")

cal_m <- make_calendar_windows(Data_MT_MissoulaCounty, 1, durationUnit = "n-month")

cal_3m <- make_calendar_windows(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")

roll_3m <- make_rolling_windows(Data_MT_MissoulaCounty, 3, durationUnit = "n-month")

Data_MT_MissoulaCounty_Durations <- join_by_date_range(Data_MT_MissoulaCounty, cal_3m)
