# Use a packaged dataset instead of live WQP retrieval to avoid network fragility
test_that("TADA_Stats suggestions complete", {
  # Load example dataset shipped with the package
  utils::data(Data_6Tribes_5y_Harmonized, package = "EPATADA")

  check <- TADA_Stats(Data_6Tribes_5y_Harmonized)
  expect_true(all(!is.na(check$ND_Estimation_Method)))
})

# Helper: build a clean, single-group subset from random data with a mix of flags
make_clean_group <- function(
  testdat,
  target_n = 20,
  nd_frac = 0.4,
  od_frac = 0.2
) {
  sub <- testdat[!is.na(testdat$TADA.ResultMeasureValue), , drop = FALSE]
  # If too few rows, recycle to reach target_n
  if (nrow(sub) < target_n) {
    sub <- sub[rep(seq_len(nrow(sub)), length.out = target_n), , drop = FALSE]
  } else {
    sub <- sub[seq_len(target_n), , drop = FALSE]
  }

  # Single group identifier and unique ResultIdentifier
  sub$TADA.ComparableDataIdentifier <- "GroupA"
  sub$ResultIdentifier <- paste0("RID_", seq_len(nrow(sub)))

  # Ensure MonitoringLocationIdentifier and DL types are populated
  sub$TADA.MonitoringLocationIdentifier[is.na(
    sub$TADA.MonitoringLocationIdentifier
  )] <- "LocX"
  sub$DetectionQuantitationLimitTypeName[is.na(
    sub$DetectionQuantitationLimitTypeName
  )] <- "DLX"

  # Set flags with given fractions (ND < 50% to exercise KM/ROS branches)
  n <- nrow(sub)
  nd_n <- max(1L, floor(n * nd_frac))
  od_n <- max(0L, floor(n * od_frac))
  idx <- seq_len(n)
  nd_idx <- idx[seq_len(nd_n)]
  od_idx <- setdiff(idx, nd_idx)[seq_len(od_n)]
  nc_idx <- setdiff(idx, c(nd_idx, od_idx))

  sub$TADA.CensoredData.Flag <- "Not-Censored"
  if (length(nd_idx)) {
    sub$TADA.CensoredData.Flag[nd_idx] <- "Non-Detect"
  }
  if (length(od_idx)) {
    sub$TADA.CensoredData.Flag[od_idx] <- "Over-Detect"
  }

  # Ensure at least two DL types among ND to trigger KM when ND < 50%
  if (length(nd_idx) > 1) {
    dl_vals <- rep(c("DL_A", "DL_B"), length.out = length(nd_idx))
    sub$DetectionQuantitationLimitTypeName[nd_idx] <- dl_vals
  }

  sub
}

test_that("TADA_Stats computes stats correctly for a single random group", {
  set.seed(123)
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  sub <- make_clean_group(testdat, target_n = 20, nd_frac = 0.45, od_frac = 0.2)
  subf <- sub[!is.na(sub$TADA.ResultMeasureValue), , drop = FALSE]

  out <- TADA_Stats(sub)

  # structure
  expect_s3_class(out, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(out), 1L)

  # counts and percentages
  expected_location_count <- length(unique(
    subf$TADA.MonitoringLocationIdentifier
  ))
  expected_measurement_count <- length(unique(subf$ResultIdentifier))
  expected_nd_count <- sum(subf$TADA.CensoredData.Flag == "Non-Detect")
  expected_over_count <- sum(subf$TADA.CensoredData.Flag == "Over-Detect")
  total_flags <- length(subf$TADA.CensoredData.Flag)
  expected_nd_pct_raw <- (expected_nd_count / total_flags) * 100
  expected_over_pct_raw <- (expected_over_count / total_flags) * 100
  expected_nd_lvls <- length(unique(subf$DetectionQuantitationLimitTypeName[
    subf$TADA.CensoredData.Flag == "Non-Detect"
  ]))

  expect_equal(out$Location_Count, expected_location_count)
  expect_equal(out$Measurement_Count, expected_measurement_count)
  expect_equal(out$Non_Detect_Count, expected_nd_count)
  expect_equal(out$Over_Detect_Count, expected_over_count)
  expect_equal(out$Non_Detect_Pct, round(expected_nd_pct_raw, 1))
  expect_equal(out$Over_Detect_Pct, round(expected_over_pct_raw, 1))
  expect_equal(out$Non_Detect_Lvls, expected_nd_lvls)

  # fences and continuous stats (default sig_figs = 3)
  q1 <- stats::quantile(subf$TADA.ResultMeasureValue, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(subf$TADA.ResultMeasureValue, 0.75, na.rm = TRUE)
  iqr <- stats::IQR(subf$TADA.ResultMeasureValue, na.rm = TRUE)
  expect_equal(out$UpperFence, signif(q3 + 1.5 * iqr, 3))
  expect_equal(out$LowerFence, signif(q1 - 1.5 * iqr, 3))
  expect_equal(
    out$Min,
    signif(min(subf$TADA.ResultMeasureValue, na.rm = TRUE), 3)
  )
  expect_equal(
    out$Mean,
    signif(mean(subf$TADA.ResultMeasureValue, na.rm = TRUE), 3)
  )
  expect_equal(
    out$Max,
    signif(max(subf$TADA.ResultMeasureValue, na.rm = TRUE), 3)
  )

  # percentiles
  p <- function(prob) {
    stats::quantile(subf$TADA.ResultMeasureValue, prob, na.rm = TRUE)
  }
  expect_equal(out$Percentile_5th, signif(p(0.05), 3))
  expect_equal(out$Percentile_10th, signif(p(0.10), 3))
  expect_equal(out$Percentile_15th, signif(p(0.15), 3))
  expect_equal(out$Percentile_25th, signif(p(0.25), 3))
  expect_equal(out$Percentile_50th_Median, signif(p(0.50), 3))
  expect_equal(out$Percentile_75th, signif(p(0.75), 3))
  expect_equal(out$Percentile_85th, signif(p(0.85), 3))
  expect_equal(out$Percentile_95th, signif(p(0.95), 3))
  expect_equal(out$Percentile_98th, signif(p(0.98), 3))

  # ND estimation method (raw ND pct, not rounded)
  expected_method <- {
    if (expected_nd_pct_raw == 0) {
      "No non-detects to estimate"
    } else if (expected_nd_pct_raw > 80) {
      "Percent censored too high for estimation methods"
    } else if (expected_nd_pct_raw < 50 && expected_nd_lvls > 1) {
      "Kaplan-Meier"
    } else if (expected_nd_pct_raw < 50) {
      "Robust Regression Order Statistics"
    } else if (expected_measurement_count >= 50) {
      "Maximum Likelihood Estimation"
    } else {
      "Robust Regression Order Statistics"
    }
  }
  expect_equal(out$ND_Estimation_Method, expected_method)
})

test_that("TADA_Stats respects additional grouping columns on random data", {
  set.seed(124)
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  # Filter to non-NA results to avoid degenerate groups
  filtered <- testdat[!is.na(testdat$TADA.ResultMeasureValue), , drop = FALSE]
  out <- TADA_Stats(
    filtered,
    group_cols = c("TADA.MonitoringLocationIdentifier")
  )

  # All rows should represent one location per group
  expect_true(all(out$Location_Count == 1L))

  # Number of rows equals number of unique (ComparableID, Location) pairs
  expected_nrows <- nrow(unique(filtered[, c(
    "TADA.ComparableDataIdentifier",
    "TADA.MonitoringLocationIdentifier"
  )]))
  expect_equal(nrow(out), expected_nrows)
})

test_that("TADA_Stats rounding parameters modify outputs on random data", {
  set.seed(125)
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  sub <- make_clean_group(testdat, target_n = 15, nd_frac = 0.33, od_frac = 0.0)
  subf <- sub[!is.na(sub$TADA.ResultMeasureValue), , drop = FALSE]

  out <- TADA_Stats(sub, sig_figs = 4, pct_digits = 0)

  expected_mean <- signif(mean(subf$TADA.ResultMeasureValue, na.rm = TRUE), 4)
  expect_equal(out$Mean, expected_mean)

  expected_nd_count <- sum(subf$TADA.CensoredData.Flag == "Non-Detect")
  total_flags <- length(subf$TADA.CensoredData.Flag)
  expect_equal(
    out$Non_Detect_Pct,
    round((expected_nd_count / total_flags) * 100, 0)
  )
})

test_that("TADA_Stats ND_Estimation_Method logic across boundary scenarios (derived from random data)", {
  set.seed(128)
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  # Helper to finalize DF: ensure required columns and unique ResultIdentifier
  finalize_df <- function(df) {
    df$ResultIdentifier <- paste0("RID_", seq_len(nrow(df)))
    df$TADA.ComparableDataIdentifier <- "ScenarioGroup"
    df$TADA.MonitoringLocationIdentifier[is.na(
      df$TADA.MonitoringLocationIdentifier
    )] <- "LocX"
    df$DetectionQuantitationLimitTypeName[is.na(
      df$DetectionQuantitationLimitTypeName
    )] <- "DLX"
    df
  }

  base <- testdat[!is.na(testdat$TADA.ResultMeasureValue), , drop = FALSE]
  if (nrow(base) < 60) {
    base <- base[rep(seq_len(nrow(base)), length.out = 60), , drop = FALSE]
  }

  # Case A: ND_Pct > 80% => too high
  df_high <- finalize_df(base[seq_len(10), , drop = FALSE])
  df_high$TADA.CensoredData.Flag <- c(rep("Non-Detect", 9), "Not-Censored")
  out_high <- TADA_Stats(df_high)
  expect_equal(
    out_high$ND_Estimation_Method,
    "Percent censored too high for estimation methods"
  )

  # Case B: 50% <= ND_Pct <= 80% and Measurement_Count >= 50 => MLE
  df_mle <- finalize_df(base[seq_len(60), , drop = FALSE])
  df_mle$TADA.CensoredData.Flag <- c(
    rep("Non-Detect", 45),
    rep("Not-Censored", 15)
  ) # 75% ND
  df_mle$DetectionQuantitationLimitTypeName[
    df_mle$TADA.CensoredData.Flag == "Non-Detect"
  ] <- "DL1"
  out_mle <- TADA_Stats(df_mle)
  expect_equal(out_mle$Measurement_Count, 60L)
  expect_equal(out_mle$Non_Detect_Pct, round(75, 1))
  expect_equal(out_mle$ND_Estimation_Method, "Maximum Likelihood Estimation")

  # Case C: 50% <= ND_Pct <= 80% and Measurement_Count < 50 => ROS
  df_ros <- finalize_df(base[seq_len(40), , drop = FALSE])
  df_ros$TADA.CensoredData.Flag <- c(
    rep("Non-Detect", 30),
    rep("Not-Censored", 10)
  ) # 75% ND
  df_ros$DetectionQuantitationLimitTypeName[
    df_ros$TADA.CensoredData.Flag == "Non-Detect"
  ] <- "DL1"
  out_ros <- TADA_Stats(df_ros)
  expect_equal(out_ros$Measurement_Count, 40L)
  expect_equal(out_ros$Non_Detect_Pct, round(75, 1))
  expect_equal(
    out_ros$ND_Estimation_Method,
    "Robust Regression Order Statistics"
  )

  # Case D: ND_Pct == 0 => No estimation
  df_none <- finalize_df(base[seq_len(20), , drop = FALSE])
  df_none$TADA.CensoredData.Flag <- rep("Not-Censored", nrow(df_none))
  out_none <- TADA_Stats(df_none)
  expect_equal(out_none$ND_Estimation_Method, "No non-detects to estimate")

  # Case E: ND_Pct < 50 and ND levels > 1 => KM
  df_km <- finalize_df(base[seq_len(20), , drop = FALSE])
  df_km$TADA.CensoredData.Flag <- c(
    rep("Non-Detect", 9),
    rep("Not-Censored", 11)
  ) # 45% ND
  dl_vals <- c("DL_A", "DL_B")
  df_km$DetectionQuantitationLimitTypeName[
    df_km$TADA.CensoredData.Flag == "Non-Detect"
  ] <- rep(dl_vals, length.out = 9)
  out_km <- TADA_Stats(df_km)
  expect_equal(out_km$ND_Estimation_Method, "Kaplan-Meier")
})

test_that("TADA_Stats calls TADA_IDCensoredData when TADA.CensoredData.Flag is missing (single group)", {
  set.seed(126)
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  # Use a subset and remove the flag
  sub <- testdat[!is.na(testdat$TADA.ResultMeasureValue), , drop = FALSE]
  sub <- sub[seq_len(min(50L, nrow(sub))), , drop = FALSE] # more rows so single group has enough records
  sub$TADA.CensoredData.Flag <- NULL

  # Filter to a single comparable group so TADA_Stats produces exactly one row
  one_grp <- sub$TADA.ComparableDataIdentifier[1]
  sub <- sub[sub$TADA.ComparableDataIdentifier == one_grp, , drop = FALSE]

  # Mock TADA_IDCensoredData to create the missing flag based on value relative to median
  testthat::with_mocked_bindings(
    TADA_IDCensoredData = function(.data) {
      med <- stats::median(.data$TADA.ResultMeasureValue, na.rm = TRUE)
      .data$TADA.CensoredData.Flag <- ifelse(
        .data$TADA.ResultMeasureValue < med,
        "Non-Detect",
        "Not-Censored"
      )
      .data$DetectionQuantitationLimitTypeName[
        .data$TADA.CensoredData.Flag == "Non-Detect"
      ] <- "DL_MOCK"
      .data
    },
    {
      out <- TADA_Stats(sub)
      expect_equal(nrow(out), 1L)

      # Expected ND count and pct (raw, not rounded) based on the same rule
      nd_count <- sum(
        sub$TADA.ResultMeasureValue <
          stats::median(sub$TADA.ResultMeasureValue, na.rm = TRUE)
      )
      total <- nrow(sub)
      nd_pct_raw <- (nd_count / total) * 100

      expect_equal(out$Non_Detect_Count, nd_count)
      expect_equal(out$Non_Detect_Pct, round(nd_pct_raw, 1))

      # Use out's summarised fields for ND levels and measurement count
      nd_lvls_out <- out$Non_Detect_Lvls
      measurement_count_out <- out$Measurement_Count

      # Mirror ND_Estimation_Method logic from TADA_Stats
      expected_method <- if (nd_pct_raw == 0) {
        "No non-detects to estimate"
      } else if (nd_pct_raw > 80) {
        "Percent censored too high for estimation methods"
      } else if (nd_pct_raw < 50 && nd_lvls_out > 1) {
        "Kaplan-Meier"
      } else if (nd_pct_raw < 50) {
        "Robust Regression Order Statistics"
      } else if (measurement_count_out >= 50) {
        "Maximum Likelihood Estimation"
      } else {
        "Robust Regression Order Statistics"
      }
      expect_equal(out$ND_Estimation_Method, expected_method)
    }
  )
})
