# ----------------------------------------------------------------------------- #
# check_intake()                                                                #
# ----------------------------------------------------------------------------- #

test_that("check_intake() flags cows with low and high intake correctly", {
  intake_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02")),
    cow = c("A", "B", "C"),
    feeding_intake = c(20, 80, 50)
  )

  warn_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_
  )

  set_id_col2("cow")
  cfg <- qc_config(low_feed_intake = 30, high_feed_intake = 70)

  updated_warn <- check_intake(intake_df, warn_df, type = "feeding", cfg = cfg)

  expect_equal(updated_warn$low_daily_feed_intake_cows[1], "A, 20")
  expect_equal(updated_warn$high_daily_feed_intake_cows[1], "B, 80")
  expect_true(is.na(updated_warn$low_daily_feed_intake_cows[2]))
  expect_true(is.na(updated_warn$high_daily_feed_intake_cows[2]))
})

test_that("check_intake() handles non-dataframe and empty input", {
  warn <- tibble::tibble(
    date = as.Date(character()),
    low_daily_feed_intake_cows = character(),
    high_daily_feed_intake_cows = character(),
    low_daily_water_intake_cows = character(),
    high_daily_water_intake_cows = character()
  )

  set_id_col2("cow")
  cfg <- qc_config()

  # Not a data frame input
  expect_error(check_intake("not_a_df", warn, type = "feeding", cfg = cfg), 
               regexp = "`df`.*data frame")

  expect_error(check_intake(tibble::tibble(), "not_a_df", type = "feeding", cfg = cfg), 
               regexp = "`warn`.*data frame")

  expect_error(check_intake(tibble::tibble(), warn, type = "feeding", cfg = "not_a_list"), 
               regexp = "`cfg`.*list")

  # Empty input (should not error)
  empty_df <- tibble::tibble(date = as.Date(character()), cow = character(), feeding_intake = numeric())
  expect_silent(check_intake(empty_df, warn, type = "feeding", cfg = cfg))
})