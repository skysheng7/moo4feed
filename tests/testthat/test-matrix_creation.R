test_that("create_time_sequence works correctly", {
  # Create test data with explicit timezone
  test_data <- data.frame(
    Start = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:05:00"), tz = "UTC"),
    End = as.POSIXct(c("2024-01-01 10:02:00", "2024-01-01 10:07:00"), tz = "UTC")
  )
  
  # Test function
  result <- create_time_sequence(test_data)
  
  # Check results
  expect_type(result, "double")
  expect_length(result, 421) # 7 minutes * 60 seconds + 1
  expect_equal(min(result), as.POSIXct("2024-01-01 10:00:00", tz = "UTC"))
  expect_equal(max(result), as.POSIXct("2024-01-01 10:07:00", tz = "UTC"))
})

test_that("prepare_time_cow_matrix works correctly", {
  # Create test data with explicit timezone
  test_data <- data.frame(
    Cow = c("Cow1", "Cow2", "Cow1"),
    Start = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:00", "2024-01-01 10:05:00"), tz = "UTC"),
    End = as.POSIXct(c("2024-01-01 10:02:00", "2024-01-01 10:02:00", "2024-01-01 10:07:00"), tz = "UTC")
  )
  
  time_seq <- create_time_sequence(test_data)
  
  # Test function
  result <- prepare_time_cow_matrix(test_data, time_seq)
  
  # Check results
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3) # Time + 2 cows
  expect_equal(nrow(result), length(time_seq))
  expect_equal(colnames(result), c("Time", "Cow1", "Cow2"))
  expect_true(all(result[, 2:3] == 0)) # All values should be 0 initially
})

test_that("prepare_time_bin_matrix works correctly", {
  # Create test data with explicit timezone
  test_matrix <- data.frame(
    Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:01:00"), tz = "UTC"),
    Cow1 = c(0, 0),
    Cow2 = c(0, 0)
  )
  
  # Test function
  result <- prepare_time_bin_matrix(test_matrix)
  
  # Check results
  expect_s3_class(result, "data.frame")
  expect_equal(result, test_matrix) # Should return the same matrix
})

test_that("prepare_time_feed_matrix works correctly", {
  # Create test data with explicit timezone
  time_seq <- seq(
    as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 10:01:00", tz = "UTC"),
    by = "sec"
  )
  
  # Test function
  result <- prepare_time_feed_matrix(time_seq, min_feed_bin = 1, max_feed_bin = 3)
  
  # Check results
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 4) # Time + 3 bins
  expect_equal(nrow(result), length(time_seq))
  expect_equal(colnames(result), c("Time", "1", "2", "3"))
  expect_true(all(is.na(result[, 2:4]))) # All values should be NA initially
}) 