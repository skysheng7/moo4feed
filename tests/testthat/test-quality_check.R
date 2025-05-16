# Test suite for qc function with realistic data structure
test_that("qc function behaves correctly under normal conditions with realistic data", {
  feed <- list(
    "2020-10-31" = data.frame(
      transponder = c(12200060, 11954040),
      cow = c(5114, 4070),
      bin = c(13, 24),
      start = lubridate::ymd_hms(c("2020-10-31 00:03:24", "2020-10-31 00:05:32"), tz = tz2()),
      end = lubridate::ymd_hms(c("2020-10-31 00:04:01", "2020-10-31 00:09:25"), tz = tz2()),
      duration = c(37, 233),
      startweight = c(18.7, 8.6),
      endweight = c(19.4, 7.4),
      intake = c(-0.7, 1.2),
      date = as.Date("2020-10-31")
    )
  )

  water <- list(
    "2020-10-31" = data.frame(
      transponder = c(12199974, 12706601),
      cow = c(5028, 7010),
      bin = c(12, 30),
      start = lubridate::ymd_hms(c("2020-10-31 00:10:00", "2020-10-31 00:12:00"), tz = tz2()),
      end = lubridate::ymd_hms(c("2020-10-31 00:11:00", "2020-10-31 00:13:00"), tz = tz2()),
      duration = c(60, 60),
      startweight = c(5.0, 7.5),
      endweight = c(4.5, 7.0),
      intake = c(0.5, 0.5),
      date = as.Date("2020-10-31")
    )
  )

  cfg <- qc_config(total_cows_expected = 2)
  set_id_col2("cow")
  set_start_col2("start")
  set_end_col2("end")

  # Normal case: both feed and water
  result <- qc(feed = feed, water = water, cfg = cfg, verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("warnings", "feed", "water", "combined") %in% names(result)))
  expect_equal(nrow(result$warnings), 1)
  expect_equal(result$warnings$total_cows, 4)
  expect_true(result$warnings$missing_cow == "")

  # Normal case: feed only
  result_feed_only <- qc(feed = feed, water = NULL, cfg = cfg, verbose = FALSE)
  expect_null(result_feed_only$water)
  expect_equal(result_feed_only$feed, feed)
  expect_equal(result_feed_only$warnings$total_cows, 2)

  # Normal case: water only
  result_water_only <- qc(feed = NULL, water = water, cfg = cfg, verbose = FALSE)
  expect_null(result_water_only$feed)
  expect_equal(result_water_only$water, water)
  expect_equal(result_water_only$warnings$total_cows, 2)
})

# Edge case: Empty feed and water input
test_that("qc handles error when feed and water are NULL", {
  expect_error(qc(feed = NULL, water = NULL, cfg = qc_config(), verbose = FALSE),
               regexp = "`feed` and `water` can't all be NULL")
})
