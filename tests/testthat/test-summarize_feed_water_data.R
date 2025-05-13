# -----------------------------------------------------------------------------#
# --------------------- Tests for summarize_feed_water_data ------------------#
# -----------------------------------------------------------------------------#

test_that("summarize_feed_water_data outputs correct structure", {
  test_data <- data.frame(
    Intake = c(1.1, 2.3, 3.5),
    Duration = c(120, 90, 60),
    date = as.Date(c("2025-01-01", "2025-01-01", "2025-01-02")),
    Cow = c("C1", "C1", "C2")
  )

  result <- summarize_feed_water_data(test_data, type = "Feeding")

  expect_type(result, "list")
  expect_named(result, c("intake", "duration", "visits"))
  expect_s3_class(result$intake, "data.frame")
  expect_true(all(c("date", "Cow", "Feeding_Intake(kg)") %in% names(result$intake)))
})

test_that("summarize_feed_water_data throws error on invalid type", {
  test_data <- data.frame(
    Intake = 1.1,
    Duration = 120,
    date = as.Date("2025-01-01"),
    Cow = "C1"
  )

  expect_error(
    summarize_feed_water_data(test_data, type = "Invalid"),
    "The type should be either 'Feeding' or 'Drinking'"
  )
})
