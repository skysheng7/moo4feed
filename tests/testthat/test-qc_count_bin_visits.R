test_that("count_visits_per_bin handles normal case correctly", {
  # Test data
  test_data <- tibble::tibble(
    cow = c("1", "1", "2", "2", "3"),
    bin = c(1, 1, 2, 3, 3)
  )
  
  # Without bins_all parameter
  result1 <- count_visits_per_bin(
    data = test_data,
    bin_col = "bin"
  )
  
  expect_equal(nrow(result1), 3)  # 3 unique bins
  expect_equal(result1$visit_freq[result1$bin == 1], 2)
  expect_equal(result1$visit_freq[result1$bin == 2], 1)
  expect_equal(result1$visit_freq[result1$bin == 3], 2)
  
  # With bins_all parameter, including non-visited bins
  result2 <- count_visits_per_bin(
    data = test_data,
    bin_col = "bin",
    bins_all = 1:5
  )
  
  expect_equal(nrow(result2), 5)  # 5 unique bins (incl. zeros)
  expect_equal(result2$visit_freq[result2$bin == 1], 2)
  expect_equal(result2$visit_freq[result2$bin == 4], 0)
  expect_equal(result2$visit_freq[result2$bin == 5], 0)
})

test_that("count_visits_per_bin handles edge cases correctly", {
  # Test with empty data
  empty_result <- count_visits_per_bin(
    data = tibble::tibble(cow = character(0), bin = integer(0))
  )
  expect_equal(nrow(empty_result), 0)
  expect_equal(ncol(empty_result), 2)
  expect_true("bin" %in% names(empty_result))
  expect_true("visit_freq" %in% names(empty_result))
  
  # Test with NULL data
  null_result <- count_visits_per_bin(data = NULL)
  expect_equal(nrow(null_result), 0)
  expect_equal(ncol(null_result), 2)
  
  # Test with single row
  single_row_data <- tibble::tibble(cow = "1", bin = 5)
  single_result <- count_visits_per_bin(data = single_row_data)
  expect_equal(nrow(single_result), 1)
  expect_equal(single_result$bin, 5)
  expect_equal(single_result$visit_freq, 1)
  
  # Test with bins_all but no matching bins
  no_match_data <- tibble::tibble(cow = c("1", "2"), bin = c(10, 11))
  no_match_result <- count_visits_per_bin(
    data = no_match_data,
    bins_all = 1:5
  )
  expect_equal(nrow(no_match_result), 7)  # 5 from bins_all + 2 from data
  expect_true(all(no_match_result$visit_freq[no_match_result$bin %in% 1:5] == 0))
  expect_equal(no_match_result$visit_freq[no_match_result$bin == 10], 1)
})

test_that("count_visits_per_bin respects custom bin column names", {
  # Test data with custom column name
  test_data <- tibble::tibble(
    cow = c("1", "2", "3"),
    feeder = c(5, 6, 5)  # Using "feeder" instead of "bin"
  )
  
  result <- count_visits_per_bin(
    data = test_data,
    bin_col = "feeder"
  )
  
  expect_equal(nrow(result), 2)
  expect_true("feeder" %in% names(result))
  expect_equal(result$visit_freq[result$feeder == 5], 2)
  expect_equal(result$visit_freq[result$feeder == 6], 1)
})

test_that("count_visits_per_cow_bin returns correct counts for normal case", {
  # Test data
  test_data <- tibble::tibble(
    cow = c("1", "1", "1", "2", "2", "3"),
    bin = c(1, 1, 2, 2, 3, 3)
  )
  
  result <- count_visits_per_cow_bin(
    data = test_data,
    id_col = "cow",
    bin_col = "bin"
  )
  
  expect_equal(nrow(result), 5)  # 5 cow-bin combinations
  
  # Check specific counts
  cow1_bin1 <- result[result$cow == "1" & result$bin == 1, ]
  expect_equal(cow1_bin1$visit_freq, 2)
  
  cow1_bin2 <- result[result$cow == "1" & result$bin == 2, ]
  expect_equal(cow1_bin2$visit_freq, 1)
  
  cow2_bin2 <- result[result$cow == "2" & result$bin == 2, ]
  expect_equal(cow2_bin2$visit_freq, 1)
})

test_that("count_visits_per_cow_bin handles edge cases correctly", {
  # Test with empty data
  empty_result <- count_visits_per_cow_bin(
    data = tibble::tibble(cow = character(0), bin = integer(0))
  )
  expect_equal(nrow(empty_result), 0)
  expect_equal(ncol(empty_result), 3)
  expect_true("cow" %in% names(empty_result))
  expect_true("bin" %in% names(empty_result))
  expect_true("visit_freq" %in% names(empty_result))
  
  # Test with NULL data
  null_result <- count_visits_per_cow_bin(data = NULL)
  expect_equal(nrow(null_result), 0)
  expect_equal(ncol(null_result), 3)
  
  # Test with single row
  single_row_data <- tibble::tibble(cow = "1", bin = 5)
  single_result <- count_visits_per_cow_bin(data = single_row_data)
  expect_equal(nrow(single_result), 1)
  expect_equal(single_result$cow, "1")
  expect_equal(single_result$bin, 5)
  expect_equal(single_result$visit_freq, 1)
})

test_that("count_visits_per_cow_bin respects custom column names", {
  # Test data with custom column names
  test_data <- tibble::tibble(
    animal_id = c("A", "A", "B", "C"),
    feeder = c(1, 2, 1, 2)
  )
  
  result <- count_visits_per_cow_bin(
    data = test_data,
    id_col = "animal_id",
    bin_col = "feeder"
  )
  
  expect_equal(nrow(result), 4)
  expect_true("animal_id" %in% names(result))
  expect_true("feeder" %in% names(result))
  expect_equal(result$visit_freq[result$animal_id == "A" & result$feeder == 1], 1)
  expect_equal(result$visit_freq[result$animal_id == "A" & result$feeder == 2], 1)
})

test_that("count_unique_bins_visited_per_cow handles all bin types correctly", {
  # Test data with various bin types
  test_data <- tibble::tibble(
    cow = c("1", "1", "1", "2", "2", "3", "4"),
    bin = c(1, 2, 3, 1, 4, 5, 10)  # Bin 10 is not in our test set
  )
  
  # Test with "all" bin_type
  result_all <- count_unique_bins_visited_per_cow(
    data = test_data,
    all_bins = 1:5,
    bin_type = "all",
    id_col = "cow",
    bin_col = "bin"
  )
  
  expect_equal(nrow(result_all), 3)  # 3 cows with bins in the specified range
  expect_true("unique_bins_visited" %in% names(result_all))
  expect_equal(result_all$unique_bins_visited[result_all$cow == "1"], 3)
  expect_equal(result_all$unique_bins_visited[result_all$cow == "2"], 2)
  expect_equal(result_all$unique_bins_visited[result_all$cow == "3"], 1)
  
  # Cow 4 has no valid bins in our range so shouldn't appear
  expect_false("4" %in% result_all$cow)
  
  # Test with "feed" bin_type
  result_feed <- count_unique_bins_visited_per_cow(
    data = test_data,
    all_bins = 1:3,  # Only considering bins 1-3 as feed bins
    bin_type = "feed",
    id_col = "cow",
    bin_col = "bin"
  )
  
  expect_equal(nrow(result_feed), 2)  # Only cows 1 and 2 visited feed bins
  expect_true("unique_feed_bins_visited" %in% names(result_feed))
  expect_equal(result_feed$unique_feed_bins_visited[result_feed$cow == "1"], 3)
  expect_equal(result_feed$unique_feed_bins_visited[result_feed$cow == "2"], 1)
  
  # Test with "water" bin_type
  result_water <- count_unique_bins_visited_per_cow(
    data = test_data,
    all_bins = 4:5,  # Only considering bins 4-5 as water bins
    bin_type = "water",
    id_col = "cow",
    bin_col = "bin"
  )
  
  expect_equal(nrow(result_water), 2)  # Only cows 2 and 3 visited water bins
  expect_true("unique_water_bins_visited" %in% names(result_water))
  expect_equal(result_water$unique_water_bins_visited[result_water$cow == "2"], 1)
  expect_equal(result_water$unique_water_bins_visited[result_water$cow == "3"], 1)
})

test_that("count_unique_bins_visited_per_cow handles edge cases correctly", {
  # Test with empty data
  empty_result <- count_unique_bins_visited_per_cow(
    data = tibble::tibble(cow = character(0), bin = integer(0)),
    all_bins = 1:5,
    bin_type = "all"
  )
  expect_equal(nrow(empty_result), 0)
  expect_equal(ncol(empty_result), 2)
  expect_true("cow" %in% names(empty_result))
  expect_true("unique_bins_visited" %in% names(empty_result))
  
  # Test with empty data but different bin types
  empty_result_feed <- count_unique_bins_visited_per_cow(
    data = tibble::tibble(cow = character(0), bin = integer(0)),
    all_bins = 1:5,
    bin_type = "feed"
  )
  expect_true("unique_feed_bins_visited" %in% names(empty_result_feed))
  
  empty_result_water <- count_unique_bins_visited_per_cow(
    data = tibble::tibble(cow = character(0), bin = integer(0)),
    all_bins = 1:5,
    bin_type = "water"
  )
  expect_true("unique_water_bins_visited" %in% names(empty_result_water))
  
  # Test with NULL data
  null_result <- count_unique_bins_visited_per_cow(
    data = NULL,
    all_bins = 1:5,
    bin_type = "all"
  )
  expect_equal(nrow(null_result), 0)
  
  # Test with data where no bins match the all_bins parameter
  no_match_data <- tibble::tibble(
    cow = c("1", "2", "3"),
    bin = c(10, 11, 12)  # None match all_bins = 1:5
  )
  no_match_result <- count_unique_bins_visited_per_cow(
    data = no_match_data,
    all_bins = 1:5,
    bin_type = "all"
  )
  expect_equal(nrow(no_match_result), 0)  # No cow visited any bin in the specified range
})

test_that("count_unique_bins_visited_per_cow respects custom column names", {
  # Test data with custom column names
  test_data <- tibble::tibble(
    animal_id = c("A", "A", "A", "B", "B", "C"),
    feeder = c(1, 2, 3, 2, 4, 5)
  )
  
  result <- count_unique_bins_visited_per_cow(
    data = test_data,
    all_bins = 1:5,
    bin_type = "all",
    id_col = "animal_id",
    bin_col = "feeder"
  )
  
  expect_equal(nrow(result), 3)
  expect_true("animal_id" %in% names(result))
  expect_equal(result$unique_bins_visited[result$animal_id == "A"], 3)
  expect_equal(result$unique_bins_visited[result$animal_id == "B"], 2)
  expect_equal(result$unique_bins_visited[result$animal_id == "C"], 1)
}) 