test_that("count_visits_per_bin returns correct visit counts", {
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
  
  # Test with empty data
  empty_result <- count_visits_per_bin(
    data = tibble::tibble(cow = character(0), bin = integer(0))
  )
  expect_equal(nrow(empty_result), 0)
})

test_that("count_visits_per_cow_bin returns correct counts", {
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
  
  # Test with empty data
  empty_result <- count_visits_per_cow_bin(
    data = tibble::tibble(cow = character(0), bin = integer(0))
  )
  expect_equal(nrow(empty_result), 0)
})

test_that("count_unique_bins_visited_per_cow handles different bin types", {
  # Already covered in test-unique_bin_visits.R, but adding additional tests
  
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
  
  # Cow 4 has no valid bins in our range so shouldn't appear
  expect_false("4" %in% result_all$cow)
}) 