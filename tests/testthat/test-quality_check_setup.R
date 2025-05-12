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
    "large_intake_feeder", "large_intake_drinker",
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
