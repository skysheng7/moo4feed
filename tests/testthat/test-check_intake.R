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