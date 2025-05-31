test_that("unique_bin_visits handles both feed and water data correctly", {
  # Create test data
  test_dates <- c("2023-01-01", "2023-01-02")
  
  # Feed data
  feed_day1 <- tibble::tibble(
    cow = c("1001", "1001", "1002", "1003"),
    bin = c(1, 2, 1, 3)
  )
  
  feed_day2 <- tibble::tibble(
    cow = c("1001", "1002", "1002", "1004"),
    bin = c(2, 1, 3, 2)
  )
  
  feed_data <- list(
    "2023-01-01" = feed_day1,
    "2023-01-02" = feed_day2
  )
  
  # Water data
  water_day1 <- tibble::tibble(
    cow = c("1001", "1002", "1003"),
    bin = c(101, 102, 101)  # Using bin_offset of 100
  )
  
  water_day2 <- tibble::tibble(
    cow = c("1001", "1003", "1004"),
    bin = c(101, 102, 103)
  )
  
  water_data <- list(
    "2023-01-01" = water_day1,
    "2023-01-02" = water_day2
  )
  
  # Set global variables for testing
  old_id_col <- id_col2()
  old_bin_col <- bin_col2()
  old_bin_offset <- bin_offset2()
  old_bins_feed <- bins_feed2()
  old_bins_wat <- bins_wat2()
  
  # Set test values
  set_id_col2("cow")
  set_bin_col2("bin")
  set_bin_offset2(100)
  set_bins_feed2(1:5)
  set_bins_wat2(1:5)
  
  # Test with both feed and water data
  result <- unique_bin_visits(
    feed = feed_data,
    water = water_data
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "cow", "unique_feed_bins_visited", 
                         "unique_water_bins_visited", "total_bins_visited"))
  
  # Check rows count (4 cows over 2 days, but not all cows appear each day)
  expect_equal(nrow(result), 7)  # 3 cows on day 1, 4 cows on day 2
  
  # Check specific values for day 1
  day1_data <- result[result$date == "2023-01-01", ]
  expect_equal(nrow(day1_data), 3)
  
  # Cow 1001 visited 2 feed bins and 1 water bin on day 1
  cow1001_day1 <- day1_data[day1_data$cow == "1001", ]
  expect_equal(cow1001_day1$unique_feed_bins_visited, 2)
  expect_equal(cow1001_day1$unique_water_bins_visited, 1)
  expect_equal(cow1001_day1$total_bins_visited, 3)
  
  # Check specific values for day 2
  day2_data <- result[result$date == "2023-01-02", ]
  expect_equal(nrow(day2_data), 4)
  
  # Cow 1004 visited 1 feed bin and 1 water bin on day 2
  cow1004_day2 <- day2_data[day2_data$cow == "1004", ]
  expect_equal(cow1004_day2$unique_feed_bins_visited, 1)
  expect_equal(cow1004_day2$unique_water_bins_visited, 1)
  expect_equal(cow1004_day2$total_bins_visited, 2)
  
  # Test with return_list = TRUE
  result_list <- unique_bin_visits(
    feed = feed_data,
    water = water_data,
    return_list = TRUE
  )
  
  expect_type(result_list, "list")
  expect_named(result_list, test_dates)
  expect_s3_class(result_list[[1]], "data.frame")
  
  # Restore original global variables
  set_id_col2(old_id_col)
  set_bin_col2(old_bin_col)
  set_bin_offset2(old_bin_offset)
  set_bins_feed2(old_bins_feed)
  set_bins_wat2(old_bins_wat)
})

test_that("unique_bin_visits handles feed-only data correctly", {
  # Create test data - feed only
  feed_day1 <- tibble::tibble(
    cow = c("1001", "1001", "1002"),
    bin = c(1, 2, 1)
  )
  
  feed_day2 <- tibble::tibble(
    cow = c("1001", "1003"),
    bin = c(2, 3)
  )
  
  feed_data <- list(
    "2023-01-01" = feed_day1,
    "2023-01-02" = feed_day2
  )
  
  # Set global variables for testing
  old_id_col <- id_col2()
  old_bin_col <- bin_col2()
  old_bins_feed <- bins_feed2()
  
  # Set test values
  set_id_col2("cow")
  set_bin_col2("bin")
  set_bins_feed2(1:5)
  
  # Test with feed data only
  result <- unique_bin_visits(feed = feed_data)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_setequal(names(result), c("date", "cow", "unique_feed_bins_visited", 
                                  "unique_water_bins_visited", "total_bins_visited"))
  
  # Check specific values
  expect_equal(nrow(result), 4)  # 2 cows on day 1, 2 unique cows on day 2
  
  # Check that water bin visits are all 0
  expect_true(all(result$unique_water_bins_visited == 0))
  
  # Check that total_bins_visited equals unique_feed_bins_visited
  expect_equal(result$total_bins_visited, result$unique_feed_bins_visited)
  
  # Restore original global variables
  set_id_col2(old_id_col)
  set_bin_col2(old_bin_col)
  set_bins_feed2(old_bins_feed)
})

test_that("unique_bin_visits handles water-only data correctly", {
  # Create test data - water only
  water_day1 <- tibble::tibble(
    cow = c("1001", "1002", "1002"),
    bin = c(101, 102, 103)  # Using bin_offset of 100
  )
  
  water_day2 <- tibble::tibble(
    cow = c("1002", "1003"),
    bin = c(101, 102)
  )
  
  water_data <- list(
    "2023-01-01" = water_day1,
    "2023-01-02" = water_day2
  )
  
  # Set global variables for testing
  old_id_col <- id_col2()
  old_bin_col <- bin_col2()
  old_bin_offset <- bin_offset2()
  old_bins_wat <- bins_wat2()
  
  # Set test values
  set_id_col2("cow")
  set_bin_col2("bin")
  set_bin_offset2(100)
  set_bins_wat2(1:5)
  
  # Test with water data only
  result <- unique_bin_visits(water = water_data)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_setequal(names(result), c("date", "cow", "unique_feed_bins_visited", 
                                  "unique_water_bins_visited", "total_bins_visited"))
  
  # Check specific values
  expect_equal(nrow(result), 4)  # 2 cows on day 1, 2 unique cows on day 2
  
  # Check that feed bin visits are all 0
  expect_true(all(result$unique_feed_bins_visited == 0))
  
  # Check that total_bins_visited equals unique_water_bins_visited
  expect_equal(result$total_bins_visited, result$unique_water_bins_visited)
  
  # Cow 1002 visited 2 water bins on day 1
  cow1002_day1 <- result[result$date == "2023-01-01" & result$cow == "1002", ]
  expect_equal(cow1002_day1$unique_water_bins_visited, 2)
  
  # Restore original global variables
  set_id_col2(old_id_col)
  set_bin_col2(old_bin_col)
  set_bin_offset2(old_bin_offset)
  set_bins_wat2(old_bins_wat)
})

test_that("unique_bin_visits handles empty data correctly", {
  # Create empty data frames
  empty_feed_day <- tibble::tibble(
    cow = character(0),
    bin = integer(0)
  )
  
  empty_water_day <- tibble::tibble(
    cow = character(0),
    bin = integer(0)
  )
  
  feed_data <- list(
    "2023-01-01" = empty_feed_day
  )
  
  water_data <- list(
    "2023-01-01" = empty_water_day
  )
  
  # Set global variables for testing
  old_id_col <- id_col2()
  old_bin_col <- bin_col2()
  
  # Set test values
  set_id_col2("cow")
  set_bin_col2("bin")
  
  # Test with empty data
  result <- unique_bin_visits(
    feed = feed_data,
    water = water_data
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "cow", "unique_feed_bins_visited", 
                         "unique_water_bins_visited", "total_bins_visited"))
  
  # Empty data should result in an empty result with the right structure
  expect_equal(nrow(result), 0)
  
  # Restore original global variables
  set_id_col2(old_id_col)
  set_bin_col2(old_bin_col)
})

test_that("unique_bin_visits handles mixed empty/non-empty data", {
  # Create test data
  feed_day1 <- tibble::tibble(
    cow = c("1001", "1002"),
    bin = c(1, 2)
  )
  
  # Empty water data for day 1
  empty_water_day <- tibble::tibble(
    cow = character(0),
    bin = integer(0)
  )
  
  # Empty feed data for day 2
  empty_feed_day <- tibble::tibble(
    cow = character(0),
    bin = integer(0)
  )
  
  water_day2 <- tibble::tibble(
    cow = c("1001", "1003"),
    bin = c(101, 102)  # Using bin_offset of 100
  )
  
  feed_data <- list(
    "2023-01-01" = feed_day1,
    "2023-01-02" = empty_feed_day
  )
  
  water_data <- list(
    "2023-01-01" = empty_water_day,
    "2023-01-02" = water_day2
  )
  
  # Set global variables for testing
  old_id_col <- id_col2()
  old_bin_col <- bin_col2()
  old_bin_offset <- bin_offset2()
  old_bins_feed <- bins_feed2()
  old_bins_wat <- bins_wat2()
  
  # Set test values
  set_id_col2("cow")
  set_bin_col2("bin")
  set_bin_offset2(100)
  set_bins_feed2(1:5)
  set_bins_wat2(1:5)
  
  # Test with mixed data
  result <- unique_bin_visits(
    feed = feed_data,
    water = water_data
  )
  
  # Check structure and values
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)  # 2 cows on day 1, 2 cows on day 2
  
  # Check day 1 (feed only)
  day1_data <- result[result$date == "2023-01-01", ]
  expect_equal(nrow(day1_data), 2)
  expect_true(all(day1_data$unique_water_bins_visited == 0))
  
  # Check day 2 (water only)
  day2_data <- result[result$date == "2023-01-02", ]
  expect_equal(nrow(day2_data), 2)
  expect_true(all(day2_data$unique_feed_bins_visited == 0))
  
  # Restore original global variables
  set_id_col2(old_id_col)
  set_bin_col2(old_bin_col)
  set_bin_offset2(old_bin_offset)
  set_bins_feed2(old_bins_feed)
  set_bins_wat2(old_bins_wat)
})

test_that("unique_bin_visits handles different date formats correctly", {
  # Create test data with different date formats
  feed_data <- list(
    "2023/01/01" = tibble::tibble(
      cow = c("1001", "1002"),
      bin = c(1, 2)
    ),
    "2023-01-02" = tibble::tibble(
      cow = c("1002", "1003"),
      bin = c(2, 3)
    )
  )
  
  water_data <- list(
    "2023/01/01" = tibble::tibble(
      cow = c("1001", "1003"),
      bin = c(101, 102)
    ),
    "2023-01-03" = tibble::tibble(  # Different date than feed data
      cow = c("1001", "1004"),
      bin = c(101, 103)
    )
  )
  
  # Set global variables for testing
  old_id_col <- id_col2()
  old_bin_col <- bin_col2()
  old_bin_offset <- bin_offset2()
  old_bins_feed <- bins_feed2()
  old_bins_wat <- bins_wat2()
  
  # Set test values
  set_id_col2("cow")
  set_bin_col2("bin")
  set_bin_offset2(100)
  set_bins_feed2(1:5)
  set_bins_wat2(1:5)
  
  # Test with mixed date formats
  result <- unique_bin_visits(
    feed = feed_data,
    water = water_data
  )
  
  # Check dates - should have 3 unique dates
  unique_dates <- unique(result$date)
  expect_equal(length(unique_dates), 3)
  expect_true(all(c("2023/01/01", "2023-01-02", "2023-01-03") %in% unique_dates))
  
  # Check each date has the right data
  day1_data <- result[result$date == "2023/01/01", ]
  expect_equal(nrow(day1_data), 3)  # 3 cows: 1001, 1002, 1003
  
  day2_data <- result[result$date == "2023-01-02", ]
  expect_equal(nrow(day2_data), 2)  # 2 cows: 1002, 1003
  expect_true(all(day2_data$unique_water_bins_visited == 0))  # No water data
  
  day3_data <- result[result$date == "2023-01-03", ]
  expect_equal(nrow(day3_data), 2)  # 2 cows: 1001, 1004
  expect_true(all(day3_data$unique_feed_bins_visited == 0))  # No feed data
  
  # Restore original global variables
  set_id_col2(old_id_col)
  set_bin_col2(old_bin_col)
  set_bin_offset2(old_bin_offset)
  set_bins_feed2(old_bins_feed)
  set_bins_wat2(old_bins_wat)
})

test_that("unique_bin_visits throws appropriate errors", {
  # Test error when both feed and water are NULL
  expect_error(
    unique_bin_visits(feed = NULL, water = NULL),
    "At least one of `feed` or `water` must be provided."
  )
})

test_that("unique_bin_visits handles custom column names", {
  # Create test data with custom column names
  feed_data <- list(
    "2023-01-01" = tibble::tibble(
      animal_id = c("1001", "1002"),
      feeder_bin = c(1, 2)
    )
  )
  
  water_data <- list(
    "2023-01-01" = tibble::tibble(
      animal_id = c("1001", "1003"),
      feeder_bin = c(101, 102)
    )
  )
  
  # Test with custom column names
  result <- unique_bin_visits(
    feed = feed_data,
    water = water_data,
    id_col = "animal_id",
    bin_col = "feeder_bin",
    bin_offset = 100,
    bins_feed = 1:5,
    bins_wat = 1:5
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "animal_id", "unique_feed_bins_visited", 
                         "unique_water_bins_visited", "total_bins_visited"))
  
  # Check specific values
  expect_equal(nrow(result), 3)  # 3 animals
  
  # Cow 1001 visited both feed and water
  cow1001 <- result[result$animal_id == "1001", ]
  expect_equal(cow1001$unique_feed_bins_visited, 1)
  expect_equal(cow1001$unique_water_bins_visited, 1)
  expect_equal(cow1001$total_bins_visited, 2)
}) 