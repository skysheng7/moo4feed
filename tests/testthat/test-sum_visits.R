# ----------------------------------------------------------------------------- #
# sum_visits()                                                   #
# ----------------------------------------------------------------------------- #

test_that("sum_visits() correctly summarizes intake, duration, and visits", {
  df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    cow = c(1, 1, 2),
    intake = c(10, 5, 8),
    duration = c(100, 150, 120)
  )

  set_id_col2("cow")
  set_intake_col2("intake")
  set_duration_col2("duration")

  result <- sum_visits(df, type = "feed")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "cow", "feed_intake", "feed_duration", "feed_visits"))
  expect_equal(nrow(result), 2)
  expect_equal(result$feed_visits[1], 2)
})