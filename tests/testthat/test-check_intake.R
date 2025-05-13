# -----------------------------------------------------------------------------#
# ---------------------------- Tests for check_intake ------------------------#
# -----------------------------------------------------------------------------#

test_that("check_intake detects low feeding intake", {
  intake_data <- data.frame(
    date = as.Date("2025-01-01"),
    Cow = "C1"
  )
  intake_data[["Feeding_Intake(kg)"]] <- 3

  warning_data <- data.frame(
    date = as.Date("2025-01-01"),
    low_daily_feed_intake_cows = "",
    high_daily_feed_intake_cows = "",
    low_daily_water_intake_cows = "",
    high_daily_water_intake_cows = ""
  )

  result <- check_intake(
    intake_data, warning_data,
    type = "feeding", limit = "low",
    feed_intake_low_bar = 5, feed_intake_high_bar = 50,
    water_intake_low_bar = 1, water_intake_high_bar = 100
  )

  expect_equal(result$low_daily_feed_intake_cows[1], "Cow  C1 ,  3 kg")
})


test_that("check_intake detects high feeding intake", {
  intake_data <- data.frame(
    date = as.Date("2025-01-01"),
    Cow = "C2"
  )
  intake_data[["Feeding_Intake(kg)"]] <- 60

  warning_data <- data.frame(
    date = as.Date("2025-01-01"),
    low_daily_feed_intake_cows = "",
    high_daily_feed_intake_cows = "",
    low_daily_water_intake_cows = "",
    high_daily_water_intake_cows = ""
  )

  result <- check_intake(
    intake_data, warning_data,
    type = "feeding", limit = "high",
    feed_intake_low_bar = 5, feed_intake_high_bar = 50,
    water_intake_low_bar = 1, water_intake_high_bar = 100
  )

  expect_equal(result$high_daily_feed_intake_cows[1], "Cow  C2 ,  60 kg")
})


test_that("check_intake does not flag normal intake", {
  intake_data <- data.frame(
    date = as.Date("2025-01-01"),
    Cow = "C3"
  )
  intake_data[["Feeding_Intake(kg)"]] <- 25

  warning_data <- data.frame(
    date = as.Date("2025-01-01"),
    low_daily_feed_intake_cows = "",
    high_daily_feed_intake_cows = "",
    low_daily_water_intake_cows = "",
    high_daily_water_intake_cows = ""
  )

  result_low <- check_intake(
    intake_data, warning_data,
    type = "feeding", limit = "low",
    feed_intake_low_bar = 5, feed_intake_high_bar = 50,
    water_intake_low_bar = 1, water_intake_high_bar = 100
  )

  result_high <- check_intake(
    intake_data, warning_data,
    type = "feeding", limit = "high",
    feed_intake_low_bar = 5, feed_intake_high_bar = 50,
    water_intake_low_bar = 1, water_intake_high_bar = 100
  )

  expect_equal(result_low$low_daily_feed_intake_cows[1], "")
  expect_equal(result_high$high_daily_feed_intake_cows[1], "")
})
