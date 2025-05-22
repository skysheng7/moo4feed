# ----------------------------------------------------------------------------- #
# merge_feed_water_summary()                                                    #
# ----------------------------------------------------------------------------- #

test_that("merge_feed_water_summary() merges feed and water summaries and applies checks", {
  feed_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01")),
    cow = c("A", "B"),
    intake = c(20, 80),
    duration = c(200, 300)
  )

  drink_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01")),
    cow = c("A", "B"),
    intake = c(50, 200),
    duration = c(100, 150)
  )

  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  set_id_col2("cow")
  cfg <- qc_config(low_feed_intake = 30, high_feed_intake = 70,
                   low_wat_intake = 60, high_wat_intake = 180)

  result <- merge_feed_water_summary(feed_df, drink_df, warn_df, cfg = cfg)

  expect_named(result, c("summary", "warn"))
  expect_s3_class(result$summary, "data.frame")
  expect_s3_class(result$warn, "data.frame")
  expect_true("A" %in% result$summary$cow)
  expect_equal(result$warn$low_daily_feed_intake_cows[1], "A, 20")
  expect_equal(result$warn$high_daily_feed_intake_cows[1], "B, 80")
  expect_equal(result$warn$high_daily_water_intake_cows[1], "B, 200") 
})