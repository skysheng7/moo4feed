# Test suite for qc_negatives()
test_that("qc_negatives() identifies negative durations correctly", {
  comb <- list(
    "2025-05-01" = data.frame(bin = c(1, 2), duration = c(-5, 10), intake = c(1, 1)),
    "2025-05-02" = data.frame(bin = c(3), duration = c(20), intake = c(2))
  )

  warn <- data.frame(date = c("2025-05-01", "2025-05-02"), negative_visit_bins = NA_character_)
  cfg <- list(calibration_error = 0.5)

  result <- qc_negatives(comb, warn, verbose = FALSE, cfg = cfg, bin_col = "bin", dur_col = "duration", intake_col = "intake")
  expect_equal(result$negative_visit_bins[result$date == "2025-05-01"], "1")
  expect_true(is.na(result$negative_visit_bins[result$date == "2025-05-02"]))
})

test_that("qc_negatives() identifies significant negative intakes correctly", {
  comb <- list(
    "2025-05-01" = data.frame(bin = c(1, 2), duration = c(5, 10), intake = c(-1, -0.2)),
    "2025-05-02" = data.frame(bin = c(3), duration = c(20), intake = c(2))
  )

  warn <- data.frame(date = c("2025-05-01", "2025-05-02"), negative_visit_bins = NA_character_)
  cfg <- list(calibration_error = 0.5)

  result <- qc_negatives(comb, warn, verbose = FALSE, cfg = cfg, bin_col = "bin", dur_col = "duration", intake_col = "intake")
  expect_equal(result$negative_visit_bins[result$date == "2025-05-01"], "1")
  expect_true(is.na(result$negative_visit_bins[result$date == "2025-05-02"]))
})

test_that("qc_negatives() combines duration and intake negative warnings correctly", {
  comb <- list(
    "2025-05-01" = data.frame(bin = c(1, 2, 3), duration = c(-5, 10, 15), intake = c(1, -1.2, -0.1)),
    "2025-05-02" = data.frame(bin = c(4), duration = c(20), intake = c(2))
  )

  warn <- data.frame(date = c("2025-05-01", "2025-05-02"), negative_visit_bins = NA_character_)
  cfg <- list(calibration_error = 0.5)

  result <- qc_negatives(comb, warn, verbose = FALSE, cfg = cfg, bin_col = "bin", dur_col = "duration", intake_col = "intake")
  expect_equal(result$negative_visit_bins[result$date == "2025-05-01"], "1; 2")
  expect_true(is.na(result$negative_visit_bins[result$date == "2025-05-02"]))
})

test_that("qc_negatives() handles empty data correctly", {
  comb <- list(
    "2025-05-01" = data.frame(bin = integer(0), duration = numeric(0), intake = numeric(0)),
    "2025-05-02" = data.frame(bin = c(1), duration = c(20), intake = c(2))
  )

  warn <- data.frame(date = c("2025-05-01", "2025-05-02"), negative_visit_bins = NA_character_)
  cfg <- list(calibration_error = 0.5)

  result <- qc_negatives(comb, warn, verbose = FALSE, cfg = cfg, bin_col = "bin", dur_col = "duration", intake_col = "intake")
  expect_true(is.na(result$negative_visit_bins[result$date == "2025-05-01"]))
  expect_true(is.na(result$negative_visit_bins[result$date == "2025-05-02"]))
})

test_that("qc_negatives() verbose mode outputs correctly", {
  comb <- list(
    "2025-05-01" = data.frame(bin = c(1), duration = c(-5), intake = c(-1))
  )

  warn <- data.frame(date = "2025-05-01", negative_visit_bins = NA_character_)
  cfg <- list(calibration_error = 0.5)

  expect_output(
    qc_negatives(comb, warn, verbose = TRUE, cfg = cfg, bin_col = "bin", dur_col = "duration", intake_col = "intake"),
    regexp = "NEGATIVE DURATION VALUES DETECTED|SIGNIFICANT NEGATIVE INTAKE VALUES DETECTED"
  )
})
