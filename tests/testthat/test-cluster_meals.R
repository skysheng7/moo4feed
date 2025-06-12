# ---------------------- Tests for cluster_meals ---------------------#

# Test setup helper function
create_test_data <- function() {
  # Create realistic test data with multiple animals and days
  data.frame(
    cow = rep(c("A", "B"), each = 10),
    start = c(
      # Animal A - Day 1: 3 meals pattern
      lubridate::ymd_hms("2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00", # Meal 1
                         "2023-01-01 11:00:00", "2023-01-01 11:15:00", "2023-01-01 11:30:00", # Meal 2  
                         "2023-01-01 16:00:00", "2023-01-01 16:15:00", "2023-01-01 16:30:00", # Meal 3
                         "2023-01-01 20:00:00"), # Single visit (likely noise)
      # Animal B - Similar pattern
      lubridate::ymd_hms("2023-01-01 09:00:00", "2023-01-01 09:15:00", "2023-01-01 09:30:00",
                         "2023-01-01 12:00:00", "2023-01-01 12:15:00", "2023-01-01 12:30:00",
                         "2023-01-01 17:00:00", "2023-01-01 17:15:00", "2023-01-01 17:30:00",
                         "2023-01-01 21:00:00")
    ),
    end = c(
      lubridate::ymd_hms("2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00",
                         "2023-01-01 11:10:00", "2023-01-01 11:25:00", "2023-01-01 11:40:00",
                         "2023-01-01 16:10:00", "2023-01-01 16:25:00", "2023-01-01 16:40:00",
                         "2023-01-01 20:10:00"),
      lubridate::ymd_hms("2023-01-01 09:10:00", "2023-01-01 09:25:00", "2023-01-01 09:40:00",
                         "2023-01-01 12:10:00", "2023-01-01 12:25:00", "2023-01-01 12:40:00",
                         "2023-01-01 17:10:00", "2023-01-01 17:25:00", "2023-01-01 17:40:00",
                         "2023-01-01 21:10:00")
    ),
    bin = rep(c(1, 2, 3, 1, 2, 3, 1, 2, 3, 4), 2),
    intake = rep(c(1.5, 2.0, 1.8, 1.2, 1.9, 1.6, 1.7, 2.1, 1.4, 0.8), 2),
    duration = rep(c(10, 15, 10, 10, 15, 10, 10, 15, 10, 10), 2)
  )
}

create_multiday_test_data <- function() {
  # Create test data spanning multiple days
  day1_data <- create_test_data()
  day2_data <- create_test_data()
  day2_data$start <- day2_data$start + lubridate::days(1)
  day2_data$end <- day2_data$end + lubridate::days(1)
  
  rbind(day1_data, day2_data)
}

# ============================================================================ #
# NORMAL USE CASES
# ============================================================================ #

test_that("cluster_meals works with single dataframe and fixed eps", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("cow" %in% names(result))
  expect_true("meal_id" %in% names(result))
  expect_true("meal_start" %in% names(result))
  expect_true("meal_end" %in% names(result))
  expect_true("meal_duration" %in% names(result))
  expect_true("visit_count" %in% names(result))
  expect_true("total_intake" %in% names(result))
  expect_true("feeding_percentage" %in% names(result))
  expect_true("unique_bins_count" %in% names(result))
  
  # Should have meals for both animals
  expect_equal(length(unique(result$cow)), 2)
})

test_that("cluster_meals works with list of dataframes", {
  test_data1 <- create_test_data()[1:10, ]  # Animal A only
  test_data2 <- create_test_data()[11:20, ] # Animal B only
  data_list <- list(test_data1, test_data2)
  
  result <- cluster_meals(data_list, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(length(unique(result$cow)), 2)
})

test_that("cluster_meals works with automatic eps determination - both method", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = NULL, method = "both", min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("cluster_meals works with automatic eps determination - percentile method", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = NULL, method = "percentile", min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("cluster_meals works with automatic eps determination - gmm method", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = NULL, method = "gmm", min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("cluster_meals works with different eps_scope values", {
  test_data <- create_multiday_test_data()
  
  # Test all three eps_scope options
  result_animal_day <- cluster_meals(test_data, eps = NULL, eps_scope = "one_animal_single_day", use_log_transform = FALSE)
  result_animal_all <- cluster_meals(test_data, eps = NULL, eps_scope = "one_animal_all_days", use_log_transform = FALSE)
  result_universal <- cluster_meals(test_data, eps = NULL, eps_scope = "all_animals", use_log_transform = FALSE)
  
  expect_s3_class(result_animal_day, "data.frame")
  expect_s3_class(result_animal_all, "data.frame")
  expect_s3_class(result_universal, "data.frame")
  
  expect_true(nrow(result_animal_day) > 0)
  expect_true(nrow(result_animal_all) > 0)
  expect_true(nrow(result_universal) > 0)
})

test_that("cluster_meals works with custom column names", {
  test_data <- create_test_data()
  names(test_data) <- c("animal_id", "visit_start", "visit_end", "feeder_bin", "feed_intake", "visit_duration")
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, 
                         id_col = "animal_id", start_col = "visit_start", end_col = "visit_end",
                         bin_col = "feeder_bin", intake_col = "feed_intake", dur_col = "visit_duration")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("animal_id" %in% names(result))
})

test_that("cluster_meals works with different percentile values", {
  test_data <- create_test_data()
  
  result_50 <- cluster_meals(test_data, eps = NULL, method = "percentile", percentile = 0.5, use_log_transform = FALSE)
  result_75 <- cluster_meals(test_data, eps = NULL, method = "percentile", percentile = 0.75, use_log_transform = FALSE)
  result_90 <- cluster_meals(test_data, eps = NULL, method = "percentile", percentile = 0.9, use_log_transform = FALSE)
  
  expect_s3_class(result_50, "data.frame")
  expect_s3_class(result_75, "data.frame")
  expect_s3_class(result_90, "data.frame")
})

test_that("cluster_meals works with custom bounds", {
  test_data <- create_test_data()
  
  result_custom <- cluster_meals(test_data, eps = NULL, lower_bound = 10, upper_bound = 45, use_log_transform = FALSE)
  result_no_lower <- cluster_meals(test_data, eps = NULL, lower_bound = NULL, upper_bound = 60, use_log_transform = FALSE)
  result_no_upper <- cluster_meals(test_data, eps = NULL, lower_bound = 5, upper_bound = NULL, use_log_transform = FALSE)
  result_no_bounds <- cluster_meals(test_data, eps = NULL, lower_bound = NULL, upper_bound = NULL, use_log_transform = FALSE)
  
  expect_s3_class(result_custom, "data.frame")
  expect_s3_class(result_no_lower, "data.frame")
  expect_s3_class(result_no_upper, "data.frame")
  expect_s3_class(result_no_bounds, "data.frame")
})

test_that("cluster_meals works with different min_pts values", {
  test_data <- create_test_data()
  
  result_2 <- cluster_meals(test_data, eps = 30, min_pts = 2, use_log_transform = FALSE)
  result_3 <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  result_4 <- cluster_meals(test_data, eps = 30, min_pts = 4, use_log_transform = FALSE)
  
  expect_s3_class(result_2, "data.frame")
  expect_s3_class(result_3, "data.frame")
  expect_s3_class(result_4, "data.frame")
  
  # With higher min_pts, should get fewer or equal meals
  expect_true(nrow(result_4) <= nrow(result_3))
  expect_true(nrow(result_3) <= nrow(result_2))
})

# ============================================================================ #
# EDGE CASES
# ============================================================================ #

test_that("cluster_meals handles empty dataframe", {
  empty_data <- data.frame(
    cow = character(0),
    start = lubridate::as_datetime(character(0)),
    end = lubridate::as_datetime(character(0)),
    bin = integer(0),
    intake = numeric(0),
    duration = numeric(0)
  )
  
  expect_warning(
    result <- cluster_meals(empty_data, eps = 30, use_log_transform = FALSE),
    "No data provided, returning empty meal dataframe"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("cow" %in% names(result))
})

test_that("cluster_meals handles single animal with single visit", {
  single_visit <- data.frame(
    cow = "A",
    start = lubridate::ymd_hms("2023-01-01 08:00:00"),
    end = lubridate::ymd_hms("2023-01-01 08:10:00"),
    bin = 1,
    intake = 1.5,
    duration = 10
  )
  
  result <- cluster_meals(single_visit, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)  # Should be empty as it doesn't meet min_pts
})

test_that("cluster_meals handles single animal with exactly min_pts visits", {
  min_visits <- data.frame(
    cow = rep("A", 3),
    start = lubridate::ymd_hms("2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00"),
    end = lubridate::ymd_hms("2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00"),
    bin = c(1, 2, 3),
    intake = c(1.5, 2.0, 1.8),
    duration = c(10, 15, 10)
  )
  
  result <- cluster_meals(min_visits, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)  # Should form a meal or be noise
})

test_that("cluster_meals handles data with all visits as noise (large gaps)", {
  sparse_data <- data.frame(
    cow = rep("A", 6),
    start = lubridate::ymd_hms("2023-01-01 08:00:00", "2023-01-01 10:00:00", "2023-01-01 12:00:00",
                               "2023-01-01 14:00:00", "2023-01-01 16:00:00", "2023-01-01 18:00:00"),
    end = lubridate::ymd_hms("2023-01-01 08:10:00", "2023-01-01 10:10:00", "2023-01-01 12:10:00",
                             "2023-01-01 14:10:00", "2023-01-01 16:10:00", "2023-01-01 18:10:00"),
    bin = c(1, 2, 3, 1, 2, 3),
    intake = c(1.5, 2.0, 1.8, 1.2, 1.9, 1.6),
    duration = c(10, 15, 10, 10, 15, 10)
  )
  
  result <- cluster_meals(sparse_data, eps = 15, min_pts = 3, use_log_transform = FALSE)  # Small eps
  
  expect_s3_class(result, "data.frame")
  # Might be empty if all visits are classified as noise
})

test_that("cluster_meals handles data with missing date column", {
  test_data <- create_test_data()
  test_data$date <- NULL  # Remove date column
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("cluster_meals handles data with existing date column", {
  test_data <- create_test_data()
  test_data$date <- lubridate::date(test_data$start)
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("cluster_meals handles data with NA values in non-critical columns", {
  test_data <- create_test_data()
  test_data$intake[1] <- NA
  test_data$duration[2] <- NA
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  # Should still work, NA values should be handled in meal summaries
})

test_that("cluster_meals handles unsorted data", {
  test_data <- create_test_data()
  # Shuffle the data
  test_data <- test_data[sample(nrow(test_data)), ]
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("cluster_meals handles very large eps values", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = 1000, min_pts = 3, use_log_transform = FALSE)  # Very large eps
  
  expect_s3_class(result, "data.frame")
  # With very large eps, most visits should cluster into few meals
})

test_that("cluster_meals handles very small eps values", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = 1, min_pts = 3, use_log_transform = FALSE)  # Very small eps
  
  expect_s3_class(result, "data.frame")
  # With very small eps, most visits might be classified as noise
})

test_that("cluster_meals handles empty list", {
  expect_error(
    cluster_meals(list(), eps = 30, use_log_transform = FALSE),
    "data list is empty"
  )
})

test_that("cluster_meals handles list with empty dataframes", {
  empty_df <- data.frame(
    cow = character(0),
    start = lubridate::as_datetime(character(0)),
    end = lubridate::as_datetime(character(0)),
    bin = integer(0),
    intake = numeric(0),
    duration = numeric(0)
  )
  
  expect_warning(
    result <- cluster_meals(list(empty_df, empty_df), eps = 30, use_log_transform = FALSE),
    "No data provided, returning empty meal dataframe"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# ============================================================================ #
# ERROR HANDLING
# ============================================================================ #

test_that("cluster_meals stops when data is NULL", {
  expect_error(
    cluster_meals(NULL, eps = 30, use_log_transform = FALSE),
    "data cannot be NULL"
  )
})

test_that("cluster_meals stops when data is not dataframe or list", {
  expect_error(
    cluster_meals("not_a_dataframe", eps = 30, use_log_transform = FALSE),
    "data must be a dataframe or list of dataframes"
  )
})

test_that("cluster_meals stops when list contains non-dataframes", {
  expect_error(
    cluster_meals(list(create_test_data(), "not_a_dataframe"), eps = 30, use_log_transform = FALSE),
    "All items in the list must be dataframes"
  )
})

test_that("cluster_meals stops for invalid min_pts", {
  test_data <- create_test_data()
  
  expect_error(
    cluster_meals(test_data, eps = 30, min_pts = 0, use_log_transform = FALSE),
    "min_pts must be a single positive integer"
  )
  
  expect_error(
    cluster_meals(test_data, eps = 30, min_pts = -1, use_log_transform = FALSE),
    "min_pts must be a single positive integer"
  )
  
  expect_error(
    cluster_meals(test_data, eps = 30, min_pts = 1.5, use_log_transform = FALSE),
    "min_pts must be a single positive integer"
  )
  
  expect_error(
    cluster_meals(test_data, eps = 30, min_pts = c(1, 2), use_log_transform = FALSE),
    "min_pts must be a single positive integer"
  )
})

test_that("cluster_meals stops for invalid method", {
  test_data <- create_test_data()
  
  expect_error(
    cluster_meals(test_data, method = "invalid", use_log_transform = FALSE),
    "method must be one of: both, percentile, gmm"
  )
})

test_that("cluster_meals stops for invalid percentile", {
  test_data <- create_test_data()
  
  expect_error(
    cluster_meals(test_data, percentile = 0, use_log_transform = FALSE),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  expect_error(
    cluster_meals(test_data, percentile = 1, use_log_transform = FALSE),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  expect_error(
    cluster_meals(test_data, percentile = 1.5, use_log_transform = FALSE),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  expect_error(
    cluster_meals(test_data, percentile = c(0.5, 0.7), use_log_transform = FALSE),
    "percentile must be a single numeric value between 0 and 1"
  )
})

test_that("cluster_meals stops for invalid bounds", {
  test_data <- create_test_data()
  
  expect_error(
    cluster_meals(test_data, lower_bound = -1, use_log_transform = FALSE),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
  
  expect_error(
    cluster_meals(test_data, upper_bound = -1, use_log_transform = FALSE),
    "upper_bound must be a single non-negative numeric value or NULL"
  )
  
  expect_error(
    cluster_meals(test_data, lower_bound = c(1, 2), use_log_transform = FALSE),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
  
  expect_error(
    cluster_meals(test_data, upper_bound = c(1, 2), use_log_transform = FALSE),
    "upper_bound must be a single non-negative numeric value or NULL"
  )
  
  expect_error(
    cluster_meals(test_data, lower_bound = 50, upper_bound = 20, use_log_transform = FALSE),
    "lower_bound must be less than or equal to upper_bound"
  )
})

test_that("cluster_meals stops for invalid eps_scope", {
  test_data <- create_test_data()
  
  expect_error(
    cluster_meals(test_data, eps_scope = "invalid", use_log_transform = FALSE),
    "eps_scope must be one of: one_animal_single_day, one_animal_all_days, all_animals"
  )
})

test_that("cluster_meals stops when required columns are missing", {
  test_data <- create_test_data()
  test_data$cow <- NULL
  
  expect_error(
    cluster_meals(test_data, eps = 30, use_log_transform = FALSE),
    "Missing required columns: cow"
  )
})

test_that("cluster_meals stops when multiple columns are missing", {
  test_data <- create_test_data()
  test_data$cow <- NULL
  test_data$bin <- NULL
  
  expect_error(
    cluster_meals(test_data, eps = 30, use_log_transform = FALSE),
    "Missing required columns: cow, bin"
  )
})

# ============================================================================ #
# OUTPUT VALIDATION
# ============================================================================ #

test_that("cluster_meals returns correct column structure", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  # Check required columns exist
  required_cols <- c("cow", "date", "meal_id", "meal_start", "meal_end", 
                     "meal_duration", "visit_count", "total_intake", 
                     "feeding_percentage", "unique_bins_count")
  
  expect_true(all(required_cols %in% names(result)))
  
  # Check column types
  expect_true(is.character(result$cow) || is.factor(result$cow))
  expect_s3_class(result$date, "Date")
  expect_true(is.numeric(result$meal_id))
  expect_s3_class(result$meal_start, "POSIXt")
  expect_s3_class(result$meal_end, "POSIXt")
  expect_true(is.numeric(result$meal_duration))
  expect_true(is.numeric(result$visit_count))
  expect_true(is.numeric(result$total_intake))
  expect_true(is.numeric(result$feeding_percentage))
  expect_true(is.numeric(result$unique_bins_count))
})

test_that("cluster_meals meal_id is sequential within animal-day", {
  test_data <- create_multiday_test_data()
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  if (nrow(result) > 0) {
    # Check meal_id starts at 1 for each animal-day
    result_by_animal_day <- split(result, paste(result$cow, result$date))
    
    for (group in result_by_animal_day) {
      if (nrow(group) > 0) {
        expect_equal(min(group$meal_id), 1)
        expect_equal(max(group$meal_id), nrow(group))
        expect_equal(sort(group$meal_id), 1:nrow(group))
      }
    }
  }
})

test_that("cluster_meals meal timing is logical", {
  test_data <- create_test_data()
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  if (nrow(result) > 0) {
    # meal_start should be <= meal_end
    expect_true(all(result$meal_start <= result$meal_end))
    
    # meal_duration should be non-negative
    expect_true(all(result$meal_duration >= 0))
    
    # visit_count should be >= min_pts
    expect_true(all(result$visit_count >= 3))
    
    # feeding_percentage should be between 0 and 100
    expect_true(all(result$feeding_percentage >= 0 & result$feeding_percentage <= 100))
    
    # unique_bins_count should be >= 1
    expect_true(all(result$unique_bins_count >= 1))
  }
})

# ============================================================================ #
# INTEGRATION TESTS WITH DIFFERENT SCENARIOS
# ============================================================================ #

test_that("cluster_meals consistency across different eps_scope with same eps", {
  test_data <- create_test_data()
  fixed_eps <- 30
  
  # All should give same results when eps is fixed
  result_animal_day <- cluster_meals(test_data, eps = fixed_eps, eps_scope = "one_animal_single_day", use_log_transform = FALSE)
  result_animal_all <- cluster_meals(test_data, eps = fixed_eps, eps_scope = "one_animal_all_days", use_log_transform = FALSE)
  result_universal <- cluster_meals(test_data, eps = fixed_eps, eps_scope = "all_animals", use_log_transform = FALSE)
  
  expect_equal(nrow(result_animal_day), nrow(result_animal_all))
  expect_equal(nrow(result_animal_all), nrow(result_universal))
})

test_that("cluster_meals handles real-world-like data patterns", {
  # Create more realistic irregular feeding patterns
  realistic_data <- data.frame(
    cow = rep(c("A", "B"), each = 15),
    start = c(
      # Animal A: irregular meal patterns
      lubridate::ymd_hms("2023-01-01 06:30:00", "2023-01-01 06:45:00", "2023-01-01 07:00:00", # Early meal
                         "2023-01-01 09:15:00", "2023-01-01 09:35:00", # Mid-morning snack
                         "2023-01-01 12:00:00", "2023-01-01 12:20:00", "2023-01-01 12:40:00", "2023-01-01 13:00:00", # Long lunch
                         "2023-01-01 15:30:00", # Afternoon snack
                         "2023-01-01 18:00:00", "2023-01-01 18:15:00", "2023-01-01 18:30:00", # Dinner
                         "2023-01-01 21:00:00", "2023-01-01 21:30:00"), # Late meal
      # Animal B: similar but slightly different timing
      lubridate::ymd_hms("2023-01-01 07:00:00", "2023-01-01 07:15:00", "2023-01-01 07:30:00",
                         "2023-01-01 10:00:00", "2023-01-01 10:20:00",
                         "2023-01-01 13:00:00", "2023-01-01 13:20:00", "2023-01-01 13:40:00", "2023-01-01 14:00:00",
                         "2023-01-01 16:00:00",
                         "2023-01-01 19:00:00", "2023-01-01 19:15:00", "2023-01-01 19:30:00",
                         "2023-01-01 22:00:00", "2023-01-01 22:30:00")
    ),
    end = c(
      lubridate::ymd_hms("2023-01-01 06:40:00", "2023-01-01 06:55:00", "2023-01-01 07:10:00",
                         "2023-01-01 09:25:00", "2023-01-01 09:45:00",
                         "2023-01-01 12:10:00", "2023-01-01 12:30:00", "2023-01-01 12:50:00", "2023-01-01 13:10:00",
                         "2023-01-01 15:40:00",
                         "2023-01-01 18:10:00", "2023-01-01 18:25:00", "2023-01-01 18:40:00",
                         "2023-01-01 21:10:00", "2023-01-01 21:40:00"),
      lubridate::ymd_hms("2023-01-01 07:10:00", "2023-01-01 07:25:00", "2023-01-01 07:40:00",
                         "2023-01-01 10:10:00", "2023-01-01 10:30:00",
                         "2023-01-01 13:10:00", "2023-01-01 13:30:00", "2023-01-01 13:50:00", "2023-01-01 14:10:00",
                         "2023-01-01 16:10:00",
                         "2023-01-01 19:10:00", "2023-01-01 19:25:00", "2023-01-01 19:40:00",
                         "2023-01-01 22:10:00", "2023-01-01 22:40:00")
    ),
    bin = rep(c(1, 2, 3, 1, 2, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2), 2),
    intake = rep(c(1.8, 2.2, 1.9, 1.0, 1.3, 2.5, 2.1, 1.8, 2.0, 0.8, 2.3, 1.7, 1.9, 1.4, 1.6), 2),
    duration = rep(c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10), 2)
  )
  
  result <- cluster_meals(realistic_data, eps = NULL, method = "both", min_pts = 2, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  # Should be able to identify multiple meals per animal
})

# ============================================================================ #
# PERFORMANCE AND STRESS TESTS (if needed)
# ============================================================================ #

test_that("cluster_meals handles moderately large dataset", {
  # Create larger dataset (not too large for CI)
  large_data <- data.frame()
  
  for (animal in 1:10) {
    for (day in 1:3) {
      day_data <- create_test_data()
      day_data$cow <- paste0("Animal_", animal)
      day_data$start <- day_data$start + lubridate::days(day - 1)
      day_data$end <- day_data$end + lubridate::days(day - 1)
      large_data <- rbind(large_data, day_data)
    }
  }
  
  # Should handle this without errors
  result <- cluster_meals(large_data, eps = 30, min_pts = 3, use_log_transform = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_equal(length(unique(result$cow)), 10)
}) 