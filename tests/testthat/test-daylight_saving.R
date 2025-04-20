# ------------------------- test for dst_switch_day() -------------------------#
test_that("dst_switch_day returns correct structure and values", {
  # Run the function
  result <- dst_switch_day(c(2020, 2021), tz = "America/Vancouver")

  # Check it's a data frame
  expect_s3_class(result, "data.frame")

  # Check the columns
  expect_named(result, c("year", "spring", "fall", "spring_next_day", "fall_next_day"))

  # Check number of rows (one per year)
  expect_equal(nrow(result), 2)

  # Check values for known years
  expect_equal(as.character(result$spring[1]), "2020-03-08")
  expect_equal(as.character(result$fall[1]),   "2020-11-01")
  expect_equal(as.character(result$spring_next_day[1]), "2020-03-09")
  expect_equal(as.character(result$fall_next_day[1]),   "2020-11-02")
  expect_equal(as.character(result$spring[2]), "2021-03-14")
  expect_equal(as.character(result$fall[2]),   "2021-11-07")
  expect_equal(as.character(result$spring_next_day[2]), "2021-03-15")
  expect_equal(as.character(result$fall_next_day[2]),   "2021-11-08")

  # Check years match input
  expect_equal(result$year, c(2020, 2021))
})

test_that("returns empty data frame with warning in no-DST time zone", {
  expect_warning(
    result <- dst_switch_day(2020, tz = "Etc/UTC"),
    "No DST transitions found for the given years and time zone."
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "spring", "fall", "spring_next_day", "fall_next_day"))
  expect_equal(nrow(result), 0)
})

test_that("errors on non-numeric or invalid year input", {
  expect_error(dst_switch_day("2020", tz = "America/Vancouver"), "must be a numeric vector", fixed = TRUE)
  expect_error(dst_switch_day(c(1800, 2020), tz = "America/Vancouver"), "must be four-digit numbers greater than 1907.", fixed = TRUE)
  expect_error(dst_switch_day(numeric(0), tz = "America/Vancouver"), "must contain at least one year.", fixed = TRUE)
})

test_that("errors on invalid time zone", {
  expect_error(dst_switch_day(2020, tz = "Invalid/Timezone"), "must be a valid time zone name", fixed = TRUE)
})

test_that("handles single year input", {
  result <- dst_switch_day(2017, tz = "America/Vancouver")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "spring", "fall", "spring_next_day", "fall_next_day"))
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 2017)
  expect_equal(as.character(result$spring[1]), "2017-03-12")
  expect_equal(as.character(result$fall[1]), "2017-11-05")
})



# ------------------------- tests for dst_switch_hm() -------------------------#
test_that("detects DST spring forward correctly", {
# Known DST spring forward in Vancouver (skips 2am -> 3am)
result <- dst_switch_hm("2023-03-12", tz = "America/Vancouver")
expect_s3_class(result, "POSIXct")
expect_equal(format(result, "%H:%M"), "01:59")
})

test_that("detects DST fall back correctly", {
  # Known DST fall back in Vancouver (repeats 1am hour)
  result <- dst_switch_hm("2012-11-04", tz = "America/Vancouver")
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%H:%M"), "00:59")  # Repeats 1:00–2:00
})

test_that("returns NULL with warning when no DST transition occurs", {
  expect_warning({
    result <- dst_switch_hm("2020-07-01", tz = "Etc/UTC")
  }, "No DST transition detected", fixed = TRUE)
  expect_null(result)
})

test_that("handles Date object as input", {
  date_obj <- as.Date("2020-03-08")
  result <- dst_switch_hm(date_obj, tz = "America/Vancouver")
  expect_s3_class(result, "POSIXct")
})

test_that("errors on invalid date format", {
  expect_error(
    dst_switch_hm("bad-date", tz = "America/Vancouver"),
    "must be a valid Date object or string", fixed = TRUE
  )
})

test_that("errors on invalid time zone", {
  expect_error(
    dst_switch_hm("2020-03-08", tz = "Mars/BaseTime"),
    "must be a valid time zone name", fixed = TRUE
  )
})

test_that("errors on invalid interval (too small, not integer, or too large)", {
  expect_error(dst_switch_hm("2020-03-08", interval = 0))
  expect_error(dst_switch_hm("2020-03-08", interval = 60))
  expect_error(dst_switch_hm("2020-03-08", interval = 1.5))
  expect_error(dst_switch_hm("2020-03-08", interval = "1"))
})


# ----------------------- tests for get_dst_switch_info() ---------------------#
test_that("Normal case: Vancouver timezone, 2021–2022", {
  result <- get_dst_switch_info(years = 2021:2022, tz = "America/Vancouver")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("spring", "fall", "spring_time", "fall_time") %in% colnames(result)))
  expect_true(all(!is.na(result$spring_time)))
  expect_true(all(!is.na(result$fall_time)))
  expect_s3_class(result$spring_time, "POSIXct")
  expect_s3_class(result$fall_time, "POSIXct")
})

test_that("Edge case: timezone without DST", {
  expect_warning(result <- get_dst_switch_info(years = 2020:2021, tz = "Etc/UTC"))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("spring_time", "fall_time") %in% colnames(result)))
})


# --------------------- tests for daylight_saving_adjust() --------------------#
# get daylight saving days in a dataframe first
dst_info <- get_dst_switch_info(years = 2021, tz = "America/Vancouver")

# test for normal cases when it's not a daylight saving change day
df <- data.frame(Start = c("01:30:00", "02:30:00", "04:00:00"),
                                   End = c("02:00:00", "03:00:00", "04:30:00"))
test_that("normal case: when it's not daylight saving day, return original df", {
  result <- daylight_saving_adjust(df,
                         date = "2021-9-1",
                         start_col = "Start",
                         end_col = "End",
                         dst_df = dst_info,
                         tz = "America/Vancouver")

  expect_equal(as.data.frame(result), df, ignore_attr = TRUE)
})

# test for edge case user did not enter the column names of start and end time
df <- data.frame(Start = c("01:30:00", "02:30:00", "04:00:00"),
                 End = c("02:00:00", "03:00:00", "04:30:00"))
test_that("edge case: when user did not enter the column names of start and end time", {
  result <- daylight_saving_adjust(df,
                                   date = "2021-9-1",
                                   dst_df = dst_info,
                                   tz = "America/Vancouver")

  expect_equal(as.data.frame(result), df, ignore_attr = TRUE)
})

# test for edge case user did not enter the column names of start and end time,
# but the column name in the user's dataframe is not the same as the function default
df <- data.frame(Start_time = c("01:30:00", "02:30:00", "04:00:00"),
                 End_time = c("02:00:00", "03:00:00", "04:30:00"))
test_that("edge case: when user did not enter the column names of start and end time", {
  expect_error(
    result <- daylight_saving_adjust(df,
                                     date = "2021-9-1",
                                     dst_df = dst_info,
                                     tz = "America/Vancouver"),
    "not found", fixed = TRUE
  )

})

# test for normal case when it's spring daylight saving change day
df <- data.frame(Start_time = c("01:30:00", "02:30:00", "04:00:00"),
                 End_time = c("02:00:00", "03:00:00", "04:30:00"))
test_that("edge case: when user did not enter the column names of start and end time", {
  expect_error(
    result <- daylight_saving_adjust(df,
                                     date = "2021-9-1",
                                     dst_df = dst_info,
                                     tz = "America/Vancouver"),
    "not found", fixed = TRUE
  )

})







