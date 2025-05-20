# ----------------------------------------------------------------------------- #
# Helper assertions                                                             #
# ----------------------------------------------------------------------------- #

test_that("assert_scalar_num() behaves correctly", {
  expect_silent(assert_scalar_num(10, "foo"))                           # normal, positive
  expect_silent(assert_scalar_num(NA, "foo", allow_na = TRUE))          # NA allowed
  expect_silent(assert_scalar_num(-3, "foo", positive = FALSE))         # negative allowed
  expect_error(assert_scalar_num(-3, "foo"), "positive", fixed = TRUE)  # negative forbidden
  expect_error(assert_scalar_num("a", "foo"), "numeric scalar", fixed = TRUE)
})

test_that("assert_scalar_int() behaves correctly", {
  expect_silent(assert_scalar_int(5L, "bar"))                                  # integer ok
  expect_silent(assert_scalar_int(NA_integer_, "bar", allow_na = TRUE))        # NA allowed
  expect_error(assert_scalar_int(5.1, "bar"), "must be an integer", fixed = TRUE)  # non-integer
  expect_error(assert_scalar_int(-2L, "bar"), "positive", fixed = TRUE)        # sign check
})

test_that("assert_int_vec() behaves correctly", {
  expect_silent(assert_int_vec(c(1L, 2L, 3L), "vec"))
  expect_error(assert_int_vec(c(1, NA, 3), "vec"), "no NAs", fixed = TRUE)
  expect_error(assert_int_vec(c("a", "b"), "vec"), "integer vector", fixed = TRUE)
})

# ----------------------------------------------------------------------------- #
# qc_config()                                                                   #
# ----------------------------------------------------------------------------- #

test_that("qc_config() returns a complete, named list with defaults", {
  cfg <- qc_config()
  expected_names <- c(
    "high_dur_feed", "high_dur_water",
    "large_intake_visit_feed", "large_intake_visit_water",
    "large_intake_rate_feed", "large_intake_rate_water",
    "low_visit_threshold", "total_cows_expected",
    "low_feed_intake", "high_feed_intake",
    "low_wat_intake", "high_wat_intake",
    "replacement_threshold", "calibration_error",
    "bin_offset", "bins_feed", "bins_wat"
  )
  expect_type(cfg, "list")
  expect_setequal(names(cfg), expected_names)
})

test_that("qc_config() accepts overrides & `...` extras", {
  cfg <- qc_config(
    high_dur_feed = 1500,
    total_cows_expected = 120L,
    my_extra = "hello"
  )
  expect_equal(cfg$high_dur_feed, 1500)
  expect_equal(cfg$total_cows_expected, 120L)
  expect_equal(cfg$my_extra, "hello")
})

test_that("qc_config() works with negative bin_offset (allowed)", {
  cfg <- qc_config(
    bin_offset = -50L,
    bins_feed  = c(1L, 2L),
    bins_wat   = c(1L, 2L)
  )
  expect_equal(cfg$bin_offset, -50L)
})

test_that("qc_config() allows NA for total_cows_expected", {
  expect_silent(qc_config(total_cows_expected = NA))
})

# ----------------------------------------------------------------------------- #
# qc_config() – edge & error handling                                           #
# ----------------------------------------------------------------------------- #

test_that("low/high intake relationship checks", {
  expect_error(
    qc_config(low_feed_intake = 80, high_feed_intake = 75),
    "low_feed_intake"
  )
  expect_error(
    qc_config(low_wat_intake = 200, high_wat_intake = 180),
    "low_wat_intake"
  )
})

test_that("calibration_error must be positive", {
  expect_error(qc_config(calibration_error = -0.1), "positive")
})

test_that("low_visit_threshold must be integer", {
  expect_error(qc_config(low_visit_threshold = 9.5), "integer")
})

test_that("high_dur_* must be positive numerics", {
  expect_error(qc_config(high_dur_feed = -10), "positive")
  expect_error(qc_config(high_dur_water = "abc"), "numeric")
})

test_that("overlapping bins after offset are detected", {
  expect_error(
    qc_config(
      bin_offset = 0L,
      bins_feed  = c(1L, 2L, 3L),
      bins_wat   = c(3L, 4L)
    ),
    "overlap"
  )
})


# ----------------------------------------------------------------------------- #
# qc_warning_skeleton() – normal & edge cases                                   #
# ----------------------------------------------------------------------------- #
# Normal and Edge Case Tests
test_that("qc_warning_skeleton works correctly under normal conditions", {
  comb <- list(
    "2024-05-01" = data.frame(),
    "2024-05-02" = data.frame()
  )

  # Normal case: feed and water
  result <- qc_warning_skeleton(comb, has_feed = TRUE, has_water = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("long_dur_feeder", "long_dur_drinker") %in% names(result)))
  expect_true(all(c("large_intake_feed_visit", "large_intake_water_visit") %in% names(result)))
  expect_false(any(c("large_intake_visit_feed", "large_intake_rate_feed",
                    "large_intake_visit_water", "large_intake_rate_water") %in% names(result)))

  # Normal case: only feed
  result_feed_only <- qc_warning_skeleton(comb, has_feed = TRUE, has_water = FALSE)
  expect_true("large_intake_feed_visit" %in% names(result_feed_only))
  expect_false("large_intake_water_visit" %in% names(result_feed_only))
  expect_false(any(c("large_intake_visit_feed", "large_intake_rate_feed") %in% names(result_feed_only)))

  # Normal case: only water
  result_water_only <- qc_warning_skeleton(comb, has_feed = FALSE, has_water = TRUE)
  expect_true("large_intake_water_visit" %in% names(result_water_only))
  expect_false("large_intake_feed_visit" %in% names(result_water_only))
  expect_false(any(c("large_intake_visit_water", "large_intake_rate_water") %in% names(result_water_only)))
})

# Edge case: Single-day input
test_that("qc_warning_skeleton handles single-day inputs correctly", {
  comb <- list("2024-05-01" = data.frame())
  result <- qc_warning_skeleton(comb, has_feed = TRUE, has_water = TRUE)
  expect_equal(nrow(result), 1)
})

# Error Handling Test
test_that("qc_warning_skeleton handles empty input lists appropriately", {
  empty_comb <- list()
  expect_error(qc_warning_skeleton(empty_comb), "The input list is empty!")
})
