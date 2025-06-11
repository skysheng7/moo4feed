test_that("convert_times_to_minutes works with basic datetime vectors", {
  # Test basic functionality with POSIXct datetime
  dt1 <- lubridate::as_datetime("2023-01-01 00:30:00", tz = tz2())
  dt2 <- lubridate::as_datetime("2023-01-01 01:15:00", tz = tz2())
  dt3 <- lubridate::as_datetime("2023-01-01 12:00:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2, dt3)
  result <- convert_times_to_minutes(datetime_vec)
  
  expect_equal(result, c(30, 75, 720))
  expect_type(result, "double")
  expect_length(result, 3)
})

test_that("convert_times_to_minutes works with reference_date as Date object", {
  # Test with reference date as Date object
  dt1 <- lubridate::as_datetime("2023-01-02 01:30:00", tz = tz2())
  dt2 <- lubridate::as_datetime("2023-01-02 02:15:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2)
  ref_date <- as.Date("2023-01-02")
  result <- convert_times_to_minutes(datetime_vec, reference_date = ref_date)
  
  expect_equal(result, c(90, 135))
})

test_that("convert_times_to_minutes works with reference_date as character string", {
  # Test with reference date as character string
  dt1 <- lubridate::as_datetime("2023-01-01 02:00:00", tz = tz2())
  dt2 <- lubridate::as_datetime("2023-01-01 03:30:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2)
  result <- convert_times_to_minutes(datetime_vec, reference_date = "2023-01-01")
  
  expect_equal(result, c(120, 210))
})

test_that("convert_times_to_minutes handles empty vectors", {
  result <- convert_times_to_minutes(c())
  expect_equal(result, numeric(0))
  expect_length(result, 0)
})

test_that("convert_times_to_minutes handles NA values correctly", {
  # Test with some NA values
  dt1 <- lubridate::as_datetime("2023-01-01 01:00:00", tz = tz2())
  dt2 <- NA
  dt3 <- lubridate::as_datetime("2023-01-01 02:00:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2, dt3)
  result <- convert_times_to_minutes(datetime_vec)
  
  expect_equal(result[1], 60)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 120)
  expect_length(result, 3)
})

test_that("convert_times_to_minutes handles NA at beginning of vector", {
  # Test with NA at beginning, should use first non-NA datetime
  dt1 <- NA
  dt2 <- lubridate::as_datetime("2023-01-01 01:30:00", tz = tz2())
  dt3 <- lubridate::as_datetime("2023-01-01 02:00:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2, dt3)
  result <- convert_times_to_minutes(datetime_vec)
  
  expect_true(is.na(result[1]))
  expect_equal(result[2], 90)
  expect_equal(result[3], 120)
})

test_that("convert_times_to_minutes works across different dates", {
  # Test spanning multiple days
  dt1 <- lubridate::as_datetime("2023-01-01 23:30:00", tz = tz2())
  dt2 <- lubridate::as_datetime("2023-01-02 01:00:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2)
  result <- convert_times_to_minutes(datetime_vec)
  
  # First datetime is 23:30 = 1410 minutes from midnight of 2023-01-01
  # Second datetime is 25 hours = 1500 minutes from midnight of 2023-01-01
  expect_equal(result[1], 1410)
  expect_equal(result[2], 1500)
})

test_that("convert_times_to_minutes handles timezone consistency", {
  # Test with timezone awareness
  dt1 <- lubridate::as_datetime("2023-01-01 01:00:00", tz = tz2())
  dt2 <- lubridate::as_datetime("2023-01-01 02:00:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2)
  result <- convert_times_to_minutes(datetime_vec)
  
  expect_equal(result, c(60, 120))
})

test_that("convert_times_to_minutes works with character datetime inputs", {
  # Test conversion from character strings
  datetime_vec <- c("2023-01-01 01:30:00", "2023-01-01 02:45:00")
  result <- convert_times_to_minutes(datetime_vec)
  
  expect_equal(result, c(90, 165))
})

test_that("convert_times_to_minutes errors when all values are NA with no reference_date", {
  datetime_vec <- c(NA, NA, NA)
  
  expect_error(
    convert_times_to_minutes(datetime_vec),
    "Failed to convert datetime_vec to datetime objects, returned all NA values"
  )
})

test_that("convert_times_to_minutes errors with invalid datetime conversion", {
  datetime_vec <- c("invalid-datetime", "also-invalid")
  
  expect_error(
    convert_times_to_minutes(datetime_vec),
    "Failed to convert datetime_vec to datetime objects"
  )
})

test_that("convert_times_to_minutes errors with invalid reference_date", {
  dt1 <- lubridate::as_datetime("2023-01-01 01:00:00", tz = tz2())
  datetime_vec <- c(dt1)
  
  expect_error(
    convert_times_to_minutes(datetime_vec, reference_date = "invalid-date"),
    "Invalid reference_date provided"
  )
})

test_that("convert_times_to_minutes works with single datetime value", {
  dt1 <- lubridate::as_datetime("2023-01-01 01:30:00", tz = tz2())
  result <- convert_times_to_minutes(c(dt1))
  
  expect_equal(result, 90)
  expect_length(result, 1)
})

test_that("convert_times_to_minutes handles mixed valid and invalid dates with reference", {
  # Test with valid reference date but some NA values in datetime_vec
  dt1 <- lubridate::as_datetime("2023-01-01 01:00:00", tz = tz2())
  dt2 <- NA
  dt3 <- lubridate::as_datetime("2023-01-01 02:30:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2, dt3)
  ref_date <- as.Date("2023-01-01")
  result <- convert_times_to_minutes(datetime_vec, reference_date = ref_date)
  
  expect_equal(result[1], 60)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 150)
})

test_that("convert_times_to_minutes maintains datetime order consistency", {
  # Test that order is preserved
  dt1 <- lubridate::as_datetime("2023-01-01 03:00:00", tz = tz2())
  dt2 <- lubridate::as_datetime("2023-01-01 01:00:00", tz = tz2())
  dt3 <- lubridate::as_datetime("2023-01-01 02:00:00", tz = tz2())
  
  datetime_vec <- c(dt1, dt2, dt3)
  result <- convert_times_to_minutes(datetime_vec)
  
  # Should maintain input order, not sort by time
  expect_equal(result, c(180, 60, 120))
}) 