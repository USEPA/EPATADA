library(dplyr)
library(lubridate)
library(tibble)

TADA_Analysis_DurationAgg <- function(
    .data,
    durationValue = NA,
    durationUnit = NA,
    rolling = FALSE
) {
  normalize_unit <- function(u) {
    if (is.na(u) || is.null(u) || !nzchar(trimws(as.character(u)))) return(NA_character_)
    u <- tolower(trimws(u))
    u <- sub("^n-", "", u)
    u <- sub("s$", "", u)
    match.arg(u, c("hour", "day", "week", "month", "quarter"))
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
  
  # Instantaneous case: each row is its own window
  if (is.na(durationValue) || is.na(durationUnit)) {
    ts <- .data[["ActivityStartDateTime"]]
    if (is.null(ts)) stop('Column "ActivityStartDateTime" not found.')
    
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
    
    out <- tibble::tibble(
      window_start = ts,
      window_end   = ts
    )
    
    out$window_start <- format(out$window_start, "%Y-%m-%d %H:%M:%S")
    out$window_end   <- format(out$window_end, "%Y-%m-%d %H:%M:%S")
    return(out)
  }
  
  # Coerce ActivityStartDateTime for window calculations
  ts <- .data[["ActivityStartDateTime"]]
  if (is.null(ts)) stop('Column "ActivityStartDateTime" not found.')
  
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
  unit <- normalize_unit(durationUnit)
  
  # 1) Calendar-aligned windows
  make_calendar_windows <- function(durationValue, durationUnit, week_start = 1) {
    stopifnot(is.numeric(durationValue), durationValue > 0)
    
    start0 <- if (unit == "week") {
      floor_date(min_dt, unit = "week", week_start = week_start)
    } else {
      floor_date(min_dt, unit = unit)
    }
    
    boundaries <- c(start0)
    while (tail(boundaries, 1) < max_dt) {
      boundaries <- c(boundaries, add_step(tail(boundaries, 1), unit, durationValue))
    }
    if (length(boundaries) == 1L) {
      boundaries <- c(boundaries, add_step(boundaries, unit, durationValue))
    }
    
    windows <- tibble::tibble(
      window_start = boundaries[-length(boundaries)],
      window_end   = boundaries[-1]
    )
    
    start_floor <- if (unit == "hour") {
      function(x) floor_date(x, "hour")
    } else {
      function(x) floor_date(x, "day")
    }
    
    end_adjust <- if (unit %in% c("month", "quarter")) {
      function(x) floor_date(x, "day") - seconds(1)
    } else {
      function(x) x - seconds(1)
    }
    
    windows <- windows |>
      dplyr::mutate(
        window_start = start_floor(window_start),
        window_end   = end_adjust(window_end)
      )
    
    windows$window_start <- format(windows$window_start, "%Y-%m-%d %H:%M:%S")
    windows$window_end   <- format(windows$window_end, "%Y-%m-%d %H:%M:%S")
    windows
  }
  
  # 2) Rolling windows
  make_rolling_windows <- function(durationValue, durationUnit, week_start = 1) {
    stopifnot(is.numeric(durationValue), durationValue > 0)
    
    tz_ref <- attr(ts, "tzone")
    if (is.null(tz_ref) || tz_ref == "") tz_ref <- "UTC"
    
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
    } else if (unit == "quarter") {
      d0 <- as.Date(lubridate::floor_date(min_dt, "quarter"))
      dL <- seq(as.Date(lubridate::floor_date(max_dt, "quarter")),
                by = "3 months", length.out = 2L)[2L]
      Sd <- seq(d0, dL, by = "3 months")
      S  <- as.POSIXct(Sd, tz = tz_ref)
      E  <- as.POSIXct(Sd[-1], tz = tz_ref) - lubridate::seconds(1)
      start_floor <- function(x) lubridate::floor_date(x, "day")
    }
    
    if (length(E) < durationValue) {
      return(data.frame(
        window_start = as.POSIXct(character()),
        window_end   = as.POSIXct(character())
      ))
    }
    
    idx      <- seq.int(durationValue, length(E))
    win_start <- start_floor(S[idx + 1 - durationValue])
    win_end   <- E[idx]
    
    end_adjust <- if (unit %in% c("month", "quarter")) {
      function(x) lubridate::floor_date(x, "day") - lubridate::seconds(1)
    } else {
      function(x) x
    }
    
    windows <- data.frame(window_start = win_start, window_end = win_end, row.names = NULL)
    
    windows <- windows |>
      dplyr::mutate(
        window_start = start_floor(window_start),
        window_end   = end_adjust(window_end)
      )
    
    windows$window_start <- format(windows$window_start, "%Y-%m-%d %H:%M:%S")
    windows$window_end   <- format(windows$window_end, "%Y-%m-%d %H:%M:%S")
    windows
  }
  
  if (isFALSE(rolling)) {
    message("TADA_Analysis_DurationAgg: rolling = FALSE was selected. Aggregating on a distinct calendar basis.")
    windows <- make_calendar_windows(durationValue, durationUnit)
  } else {
    message("TADA_Analysis_DurationAgg: rolling = TRUE was selected. Aggregating on a rolling basis.")
    windows <- make_rolling_windows(durationValue, durationUnit)
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
#' Data_MT_MissoulaCounty_Durations <- TADA_Analysis_join_by_date_range(Data_MT_MissoulaCounty, cal_3m)
#' 
#' Data_MT_MissoulaCounty_Durations_roll <- TADA_Analysis_join_by_date_range(Data_MT_MissoulaCounty, roll_3m)
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



#' Join a WQP + Criteria Table with Start and End Date Windows for each unique Char, fraction and speciation
#'
#' This data frame requires users to have already joined the WQP data frame 
#' with the TADA criteria table which can be generated from [TADA_Analysis_Join_WQP_Criteria]
#' For each unique DurationUnit and DurationValue found in the data frame.
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
#' Data_MT_MissoulaCounty_Durations <- TADA_Analysis_join_by_date_range(Data_MT_MissoulaCounty, cal_3m)
#' 
#' Data_MT_MissoulaCounty_Durations_roll <- TADA_Analysis_join_by_date_range(Data_MT_MissoulaCounty, roll_3m)
#'
#' @export
TADA_Analysis_Join_Windows <- function(data_w_criteria, join_back = TRUE) {
  stopifnot(is.data.frame(data_w_criteria))
  
  id_cols <- c(
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )
  
  # Only require identifier columns
  missing_ids <- setdiff(id_cols, names(data_w_criteria))
  if (length(missing_ids) > 0) {
    stop("Input data is missing required columns: ", paste(missing_ids, collapse = ", "))
  }
  
  # Add DurationValue / DurationUnit if missing
  if (!"DurationValue" %in% names(data_w_criteria)) {
    data_w_criteria$DurationValue <- NA
  }
  if (!"DurationUnit" %in% names(data_w_criteria)) {
    data_w_criteria$DurationUnit <- NA
  }
  
  # Harmonize date/time columns
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
  
  # Distinct combos including NA
  req <- c(id_cols, "DurationValue", "DurationUnit")
  filt_all <- df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(req)))
  
  if (nrow(filt_all) == 0) return(dplyr::tibble())
  
  out_list <- vector("list", nrow(filt_all))
  
  # NA-safe equality
  equal_or_both_na <- function(x, y) {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
  }
  
  for (i in seq_len(nrow(filt_all))) {
    combo <- filt_all[i, , drop = FALSE]
    
    dur_val  <- combo$DurationValue
    dur_unit <- combo$DurationUnit
    
    valid_combo <- !is.na(dur_val) && !is.na(dur_unit)
    
    mask <- rep(TRUE, nrow(df))
    for (cc in id_cols) {
      mask <- mask & equal_or_both_na(df[[cc]], combo[[cc]])
    }
    mask <- mask &
      equal_or_both_na(df$DurationValue, dur_val) &
      equal_or_both_na(df$DurationUnit, dur_unit)
    
    sub <- df[mask, , drop = FALSE]
    
    if (nrow(sub) == 0) {
      out_list[[i]] <- dplyr::tibble()
      next
    }
    
    if (valid_combo) {
      win <- TADA_Analysis_DurationAgg(
        .data = sub,
        durationValue = dur_val,
        durationUnit = dur_unit,
        rolling = FALSE
      )
      
      if (isTRUE(join_back)) {
        win_for_join <- win |>
          dplyr::select(-dplyr::any_of(c(id_cols, "DurationValue", "DurationUnit")))
        
        win_joined <- TADA_Analysis_join_by_date_range(sub, win_for_join)
        
        for (cc in id_cols) win_joined[[cc]] <- combo[[cc]]
        win_joined$DurationValue <- dur_val
        win_joined$DurationUnit  <- dur_unit
        
        out_list[[i]] <- win_joined
      } else {
        for (cc in id_cols) if (!cc %in% names(win)) win[[cc]] <- combo[[cc]]
        if (!"DurationValue" %in% names(win)) win$DurationValue <- dur_val
        if (!"DurationUnit" %in% names(win))  win$DurationUnit  <- dur_unit
        out_list[[i]] <- win
      }
      
    } else {
      ts <- sub$ActivityStartDateTime
      if (is.null(ts)) {
        stop('Column "ActivityStartDateTime" is required for NA DurationValue/DurationUnit rows.')
      }
      if (!inherits(ts, "POSIXct")) ts <- as.POSIXct(ts)
      
      win_na <- dplyr::tibble(
        window_start = format(ts, "%Y-%m-%d %H:%M:%S"),
        window_end   = format(ts, "%Y-%m-%d %H:%M:%S"),
        window_start_date = as.Date(ts),
        window_end_date   = as.Date(ts)
      )
      
      for (cc in id_cols) win_na[[cc]] <- sub[[cc]]
      win_na$DurationValue <- sub$DurationValue
      win_na$DurationUnit  <- sub$DurationUnit
      
      if (isTRUE(join_back)) {
        win_na <- dplyr::bind_cols(
          sub,
          win_na |>
            dplyr::select(-dplyr::any_of(names(sub)))
        )
      }
      
      out_list[[i]] <- win_na
    }
  }
  
  dplyr::bind_rows(out_list)
}
