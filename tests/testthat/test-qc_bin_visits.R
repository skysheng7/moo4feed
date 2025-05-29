# ----------------------------------------------------------------------------- #
# qc_bin_visits() – normal cases                                                  #
# ----------------------------------------------------------------------------- #

test_that("qc_bin_visits() correctly identifies bins with no visits", {
  # Create test data with bins 1 and 2 visited, but 3 and 4 not visited
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 1, 2, 2),
      bin = c(1, 2, 1, 2)
    )
  )
  
  # Warning dataframe initialized with date
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  # Configuration with default low visit threshold (10)
  cfg <- qc_config()
  
  # Test with verbose = FALSE to avoid console output during tests
  result <- qc_bin_visits(
    test_data,
    warn_df,
    cfg = cfg,
    id_col = "cow",
    bin_col = "bin",
    all_bins = 1:4,
    verbose = FALSE
  )
  
  # Expect bins 3 and 4 to be identified as never visited
  expect_equal(result$bins_never_visited[1], "3; 4")
})

test_that("qc_bin_visits() correctly identifies bins with low traffic", {
  # Create test data with bin 1 having high traffic (12 visits),
  # bin 2 having low traffic (5 visits), and bins 3-4 not visited
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(rep(1, 6), rep(2, 6), rep(3, 5)),
      bin = c(rep(1, 6), rep(1, 6), rep(2, 5))
    )
  )
  
  # Warning dataframe initialized with date
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  # Configuration with default low visit threshold (10)
  cfg <- qc_config(low_visit_threshold = 10)
  
  # Test with verbose = FALSE to avoid console output during tests
  result <- qc_bin_visits(
    test_data,
    warn_df,
    cfg = cfg,
    id_col = "cow",
    bin_col = "bin",
    all_bins = 1:4,
    verbose = FALSE
  )
  
  # Expect bins 3 and 4 to be never visited
  expect_equal(result$bins_never_visited[1], "3; 4")
  
  # Expect bin 2 to have low traffic
  expect_equal(result$bins_low_traffic[1], "2")
})

test_that("qc_bin_visits() works with multiple days of data", {
  # Create test data with two days
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 1, 2),
      bin = c(1, 2, 1)
    ),
    "2024-01-02" = data.frame(
      cow = c(1, 2, 2),
      bin = c(1, 2, 3)
    )
  )
  
  # Warning dataframe initialized with dates
  warn_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    bins_never_visited = c(NA_character_, NA_character_),
    bins_low_traffic = c(NA_character_, NA_character_)
  )
  
  # Configuration with custom low visit threshold
  cfg <- qc_config(low_visit_threshold = 2)
  
  # Test with verbose = FALSE
  result <- qc_bin_visits(
    test_data,
    warn_df,
    cfg = cfg,
    id_col = "cow",
    bin_col = "bin",
    all_bins = 1:4,
    verbose = FALSE
  )
  
  # Check expectations for day 1
  expect_equal(result$bins_never_visited[1], "3; 4")
  expect_equal(result$bins_low_traffic[1], "2")
  
  # Check expectations for day 2
  expect_equal(result$bins_never_visited[2], "4")
  expect_equal(result$bins_low_traffic[2], "1; 2; 3")
})

test_that("qc_bin_visits() works with custom bin column names", {
  # Create test data with custom column name for bin
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 1, 2),
      feeder_id = c(1, 2, 1)
    )
  )
  
  # Warning dataframe initialized with date
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  # Test with custom bin column name
  result <- qc_bin_visits(
    test_data,
    warn_df,
    id_col = "cow",
    bin_col = "feeder_id",
    all_bins = 1:3,
    verbose = FALSE
  )
  
  # Expect bin 3 to be never visited
  expect_equal(result$bins_never_visited[1], "3")
})

# ----------------------------------------------------------------------------- #
# qc_bin_visits() – edge cases                                                  #
# ----------------------------------------------------------------------------- #

test_that("qc_bin_visits() handles empty input list", {
  empty_list <- list()
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  result <- qc_bin_visits(empty_list, warn_df, verbose = FALSE)
  
  # Should return the warning dataframe unchanged
  expect_equal(result, warn_df)
})

test_that("qc_bin_visits() handles empty data frames", {
  # Create test data with an empty data frame
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = character(),
      bin = integer()
    )
  )
  
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  result <- qc_bin_visits(test_data, warn_df, verbose = FALSE)
  
  # Warning dataframe should remain unchanged
  expect_equal(result, warn_df)
})

test_that("qc_bin_visits() handles missing dates in warning dataframe", {
  # Create test data
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 2),
      bin = c(1, 2)
    )
  )
  
  # Warning dataframe with different date
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-02"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  result <- qc_bin_visits(test_data, warn_df, verbose = FALSE)
  
  # Warning dataframe should remain unchanged since no matching date
  expect_equal(result, warn_df)
})

test_that("qc_bin_visits() handles when no bins have issues", {
  # Create test data where all bins are visited adequately
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = rep(1:5, each = 3),
      bin = rep(1:3, times = 5)
    )
  )
  
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  # With low threshold of 5
  cfg <- qc_config(low_visit_threshold = 5)
  
  result <- qc_bin_visits(
    test_data,
    warn_df,
    cfg = cfg,
    all_bins = 1:3,
    verbose = FALSE
  )
  
  # No bins should be identified as problematic
  expect_true(is.na(result$bins_never_visited[1]))
  expect_true(is.na(result$bins_low_traffic[1]))
})

# ----------------------------------------------------------------------------- #
# qc_bin_visits() – verbose output                                               #
# ----------------------------------------------------------------------------- #

test_that("qc_bin_visits() produces correct messages when verbose=TRUE", {
  # Create test data
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 1, 2),
      bin = c(1, 1, 1) # bin 1 has 3 visits, bin 2-4 have none
    )
  )
  
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  # Should capture messages about never-visited bins
  expect_message(
    qc_bin_visits(
      test_data,
      warn_df,
      all_bins = 1:4,
      verbose = TRUE
    ),
    "bins were never visited"
  )
  
  # Create data for low traffic message
  low_traffic_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 2, 3, 4, 5),
      bin = c(1, 2, 2, 2, 2) # bin 1 has 1 visit, bin 2 has 4 visits
    )
  )
  
  # Should capture messages about low-traffic bins
  expect_message(
    qc_bin_visits(
      low_traffic_data,
      warn_df,
      cfg = qc_config(low_visit_threshold = 5),
      all_bins = 1:2,
      verbose = TRUE
    ),
    "bins has low cow traffic"
  )
})

# ----------------------------------------------------------------------------- #
# Integration with count_visits_per_bin()                                        #
# ----------------------------------------------------------------------------- #

test_that("qc_bin_visits() correctly integrates with count_visits_per_bin function", {
  # Create test data with specific visit pattern
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 1, 2, 2, 3, 3),
      bin = c(1, 1, 2, 2, 3, 3) # bins 1-3 each have 2 visits, bin 4 has none
    )
  )
  
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    bins_never_visited = NA_character_,
    bins_low_traffic = NA_character_
  )
  
  # Configuration with threshold of 3 visits
  cfg <- qc_config(low_visit_threshold = 3)
  
  # Run the function
  result <- qc_bin_visits(
    test_data,
    warn_df,
    cfg = cfg,
    id_col = "cow",
    bin_col = "bin",
    all_bins = 1:4,
    verbose = FALSE
  )
  
  # With our visit pattern and threshold of 3:
  # - bin 4 should be never visited
  # - bins 1, 2, and 3 should have low traffic (2 visits each)
  expect_equal(result$bins_never_visited[1], "4")
  expect_equal(result$bins_low_traffic[1], "1; 2; 3")
}) 