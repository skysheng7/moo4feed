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

test_that("merge_feed_water_summary() handles NULL inputs and bad types", {
  warn <- tibble::tibble(
    date = as.Date("2024-01-01"),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  cfg <- qc_config()
  set_id_col2("cow")
  set_intake_col2("intake")
  set_duration_col2("duration")

  # feed and water are NULL — should return NULL summary
  result <- merge_feed_water_summary(feed = NULL, water = NULL, warn = warn, cfg = cfg)
  expect_null(result$summary)
  expect_s3_class(result$warn, "data.frame")

  # Invalid feed input
  expect_error(merge_feed_water_summary(feed = "not_a_df", water = NULL, warn = warn, cfg = cfg),
               "`feed` must be a data frame or NULL")

  # Invalid warn input
  expect_error(merge_feed_water_summary(feed = NULL, water = NULL, warn = "not_a_df", cfg = cfg),
               "`warn` must be a data frame")
})