# Test suite for qc function with realistic data structure
test_that("qc function behaves correctly under normal conditions with realistic data", {
  feed <- list(
    "2020-10-31" = data.frame(
      transponder = c(12200060, 11954040),
      cow = c(5114, 4070),
      bin = c(13, 24),
      start = lubridate::ymd_hms(c("2020-10-31 00:03:24", "2020-10-31 00:05:32"), tz = tz2()),
      end = lubridate::ymd_hms(c("2020-10-31 00:04:34", "2020-10-31 00:15:32"), tz = tz2()),
      duration = c(70, 600),
      start_weight = c(18.7, 8.6),
      end_weight = c(19.4, 7.4),
      intake = c(-0.7, 1.2),
      date = as.Date("2020-10-31"),
      rate = c(-0.7/70, 1.2/600)
    )
  )

  feed_expect <- list(
    "2020-10-31" = data.frame(
      transponder = c(11954040),
      cow = c(4070),
      bin = c(24),
      start = lubridate::ymd_hms(c("2020-10-31 00:05:32"), tz = tz2()),
      end = lubridate::ymd_hms(c("2020-10-31 00:15:32"), tz = tz2()),
      duration = c(600),
      start_weight = c(8.6),
      end_weight = c(7.4),
      intake = c(1.2),
      date = as.Date("2020-10-31"),
      rate = c(1.2/600)
    )
  )

  water <- list(
    "2020-10-31" = data.frame(
      transponder = c(12199974, 12706601),
      cow = c(5028, 7010),
      bin = c(12, 30),
      start = lubridate::ymd_hms(c("2020-10-31 00:10:00", "2020-10-31 00:12:00"), tz = tz2()),
      end = lubridate::ymd_hms(c("2020-10-31 00:10:50", "2020-10-31 00:12:50"), tz = tz2()),
      duration = c(50, 50),
      start_weight = c(5.0, 7.5),
      end_weight = c(4.5, 7.0),
      intake = c(0.5, 0.5),
      date = as.Date("2020-10-31"),
      rate = c(0.5/50, 0.5/50)
    )
  )

  cfg <- qc_config(total_cows_expected = 2)

  # Normal case: both feed and water
  result <- qc(feed = feed,
               water = water,
               cfg = cfg,
               verbose = FALSE,
               id_col = "cow",
               start_col = "start",
               end_col = "end",
               bin_col = "bin",
               dur_col = "duration",
               intake_col = "intake",
               start_weight_col = "start_weight",
               end_weight_col = "end_weight")
  expect_type(result, "list")
  expect_true(all(c("warnings", "feed", "water", "combined") %in% names(result)))
  expect_equal(nrow(result$warnings), 1)
  expect_equal(result$warnings$total_cows, 4L)
  expect_true(is.na(result$warnings$missing_cow))

  # Normal case: feed only
  result_feed_only <- qc(feed = feed,
                         water = NULL,
                         cfg = cfg,
                         verbose = FALSE,
                         id_col = "cow",
                         start_col = "start",
                         end_col = "end",
                         bin_col = "bin",
                         dur_col = "duration",
                         intake_col = "intake",
                         start_weight_col = "start_weight",
                         end_weight_col = "end_weight")
  expect_null(result_feed_only$water)
  
  # Compare data frames carefully, considering column order and row order
  expect_equal(names(result_feed_only$feed), names(feed_expect))
  expect_equal(ncol(result_feed_only$feed[[1]]), ncol(feed_expect[[1]]))
  
  expect_equal(result_feed_only$feed[[1]]$start_weight, feed_expect[[1]]$start_weight)
  expect_equal(result_feed_only$feed[[1]]$end_weight, feed_expect[[1]]$end_weight)
  expect_equal(result_feed_only$feed[[1]]$intake, feed_expect[[1]]$intake)
  expect_equal(result_feed_only$feed[[1]]$rate, feed_expect[[1]]$rate)
  
  expect_equal(result_feed_only$warnings[["total_cows"]][1], 2)

  # Normal case: water only
  result_water_only <- qc(feed = NULL,
                          water = water,
                          cfg = cfg,
                          verbose = FALSE,
                          id_col = "cow",
                          start_col = "start",
                          end_col = "end",
                          bin_col = "bin",
                          dur_col = "duration",
                          intake_col = "intake",
                          start_weight_col = "start_weight",
                          end_weight_col = "end_weight")
  expect_null(result_water_only$feed)
  expect_equal(result_water_only$water[[1]]$start_weight, water[[1]]$start_weight)
  expect_equal(result_water_only$water[[1]]$end_weight, water[[1]]$end_weight)
  expect_equal(result_water_only$water[[1]]$intake, water[[1]]$intake)
  expect_equal(result_water_only$water[[1]]$rate, water[[1]]$rate)
  expect_equal(result_water_only$warnings[["total_cows"]][1], 2)
})

# Edge case: Empty feed and water input
test_that("qc handles error when feed and water are NULL", {
  expect_error(qc(feed = NULL, water = NULL, cfg = qc_config(), verbose = FALSE),
               regexp = "`feed` and `water` can't all be NULL")
})
