# --- Test helpers ---
make_day_df <- function(bins, durations) {
  data.frame(
    bin = bins,
    duration = durations
  )
}

make_warn_df <- function(dates) {
  data.frame(
    date = dates,
    long_dur_feeder = NA_character_,
    long_dur_drinker = NA_character_,
    stringsAsFactors = FALSE
  )
}

mock_cfg <- list(
  large_intake_visit_feed = 2000,
  large_intake_visit_wat = 1800
)

# --- Tests for qc_long_dur ---
test_that("qc_long_dur flags long feeder visits correctly", {
  comb <- list("2025-01-01" = make_day_df(bins = c(1, 2, 3), durations = c(1900, 2100, 2200)))
  warn <- make_warn_df("2025-01-01")

  result <- qc_long_dur(comb, warn, cfg = mock_cfg, verbose = FALSE, type = "feed")
  expect_equal(result$long_dur_feeder, "2; 3")
})

test_that("qc_long_dur flags long water visits correctly", {
  comb <- list("2025-01-02" = make_day_df(bins = c(1, 2), durations = c(1700, 1900)))
  warn <- make_warn_df("2025-01-02")

  result <- qc_long_dur(comb, warn, cfg = mock_cfg, verbose = FALSE, type = "water")
  expect_equal(result$long_dur_drinker, "2")
})

test_that("qc_long_dur does not flag if durations are below threshold", {
  comb <- list("2025-01-03" = make_day_df(bins = c(1, 2), durations = c(1000, 1500)))
  warn <- make_warn_df("2025-01-03")

  result <- qc_long_dur(comb, warn, cfg = mock_cfg, verbose = FALSE, type = "feed")
  expect_true(is.na(result$long_dur_feeder))
})

test_that("qc_long_dur handles empty days and unmatched warn date", {
  comb <- list("2025-01-04" = make_day_df(bins = integer(0), durations = numeric(0)))
  warn <- make_warn_df("2025-01-05")  # mismatched date

  result <- qc_long_dur(comb, warn, cfg = mock_cfg, verbose = FALSE, type = "water")
  expect_true(is.na(result$long_dur_drinker))
})

# --- Tests for qc_all_long_durations ---
test_that("qc_all_long_durations handles feed and water input", {
  feed_data <- list("2025-01-06" = make_day_df(bins = c(1), durations = c(2500)))
  water_data <- list("2025-01-06" = make_day_df(bins = c(2), durations = c(1900)))
  warn <- make_warn_df("2025-01-06")

  result <- qc_all_long_durations(feed = feed_data, water = water_data, warn = warn, cfg = mock_cfg)

  expect_equal(result$long_dur_feeder, "1")
  expect_equal(result$long_dur_drinker, "2")
})

test_that("qc_all_long_durations skips NULL inputs", {
  warn <- make_warn_df("2025-01-07")
  result <- qc_all_long_durations(feed = NULL, water = NULL, warn = warn, cfg = mock_cfg)

  expect_true(all(is.na(result$long_dur_feeder)))
  expect_true(all(is.na(result$long_dur_drinker)))
})

test_that("qc_long_dur verbose prints output for long durations", {
  comb <- list("2025-01-08" = make_day_df(bins = c(1), durations = c(2500)))
  warn <- make_warn_df("2025-01-08")

  expect_output(
    qc_long_dur(comb, warn, cfg = mock_cfg, verbose = TRUE, type = "feed"),
    regexp = "LONG DURATION VISITS DETECTED"
  )
})
