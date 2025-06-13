test_that("empty_synch_matrix works correctly", {
  # Create test data
  test_data <- list(
    "2024-01-01" = data.frame(
      Cow = c("Cow1", "Cow2"),
      Start = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:00"), tz = "UTC"),
      End = as.POSIXct(c("2024-01-01 10:02:00", "2024-01-01 10:02:00"), tz = "UTC"),
      Bin = c(1, 2),
      Startweight = c(100, 100),
      Endweight = c(95, 95)
    )
  )
  
  # Test feed type
  result_feed <- empty_synch_matrix(test_data, min_feed_bin = 1, max_feed_bin = 2, type = "feed")
  
  # Check results
  expect_type(result_feed, "list")
  expect_length(result_feed, 3)
  expect_named(result_feed, c("synch_master_cow", "synch_master_bin", "synch_master_feed"))
  expect_length(result_feed$synch_master_cow, 1)
  expect_length(result_feed$synch_master_bin, 1)
  expect_length(result_feed$synch_master_feed, 1)
  
  # Test drink type
  result_drink <- empty_synch_matrix(test_data, min_feed_bin = 1, max_feed_bin = 2, type = "drink")
  
  # Check results
  expect_type(result_drink, "list")
  expect_length(result_drink, 2)
  expect_named(result_drink, c("synch_master_cow", "synch_master_bin"))
})

test_that("matrix_initialize works correctly", {
  # Create test data
  test_data <- list(
    "2024-01-01" = data.frame(
      Cow = c("Cow1", "Cow2"),
      Start = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:00"), tz = "UTC"),
      End = as.POSIXct(c("2024-01-01 10:02:00", "2024-01-01 10:02:00"), tz = "UTC"),
      Bin = c(1, 2),
      Startweight = c(100, 100),
      Endweight = c(95, 95)
    )
  )
  
  # Test feed type
  result_feed <- matrix_initialize(test_data, min_feed_bin = 1, max_feed_bin = 2, type = "feed")
  
  # Check results
  expect_type(result_feed, "list")
  expect_length(result_feed, 3)
  expect_named(result_feed, c("synch_master_cow", "synch_master_bin", "synch_master_feed"))
  
  # Check matrix values
  expect_true(all(result_feed$synch_master_cow[[1]][, 2:3] == 1))
  expect_equal(result_feed$synch_master_bin[[1]][, 2], rep(1, nrow(result_feed$synch_master_bin[[1]])))
  expect_equal(result_feed$synch_master_bin[[1]][, 3], rep(2, nrow(result_feed$synch_master_bin[[1]])))
})

test_that("process_cur_synch works correctly", {
  # Create test data
  test_matrix <- data.frame(
    Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:01:00"), tz = "UTC"),
    "1" = c(NA, 95),
    "2" = c(100, NA)
  )
  
  # Test function
  result <- process_cur_synch(test_matrix, total_feed_bin = 2)
  
  # Check results
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 4) # Time + 2 bins + totalFeed
  expect_equal(nrow(result), 2)
  expect_false(any(is.na(result[, 2:3]))) # No NAs in bin columns
  expect_true(all(result$totalFeed >= 0)) # Total feed should be non-negative
})

test_that("matrix_process works correctly", {
  # Create test data
  test_data <- list(
    "2024-01-01" = data.frame(
      Cow = c("Cow1", "Cow2"),
      Start = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:00"), tz = "UTC"),
      End = as.POSIXct(c("2024-01-01 10:02:00", "2024-01-01 10:02:00"), tz = "UTC"),
      Bin = c(1, 2),
      Startweight = c(100, 100),
      Endweight = c(95, 95)
    )
  )
  
  # Test function
  result <- matrix_process(test_data, min_feed_bin = 1, max_feed_bin = 2, total_feed_bin = 2)
  
  # Check results
  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, c("synch_master_cow2", "synch_master_bin2", "synch_master_feed2"))
  
  # Check derived columns
  expect_true("total_cow_num" %in% colnames(result$synch_master_cow2[[1]]))
  expect_true("total_bin_occupied" %in% colnames(result$synch_master_cow2[[1]]))
  expect_true("empty_bin_num" %in% colnames(result$synch_master_cow2[[1]]))
  expect_true("date" %in% colnames(result$synch_master_cow2[[1]]))
  expect_true("totalFeed" %in% colnames(result$synch_master_feed2[[1]]))
}) 