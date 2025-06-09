test_that("cluster_meals works with single dataframe", {
  # Create sample data with clear meal patterns
  sample_data <- data.frame(
    cow = rep(c(1001, 1002), each = 12),
    start = c(
      # Cow 1001: 3 meals with 4 visits each
      as.POSIXct("2023-01-01 08:00:00") + c(0, 300, 600, 900),      # Meal 1
      as.POSIXct("2023-01-01 12:00:00") + c(0, 180, 360, 540),      # Meal 2  
      as.POSIXct("2023-01-01 18:00:00") + c(0, 240, 480, 720),      # Meal 3
      # Cow 1002: 2 meals with 6 visits each
      as.POSIXct("2023-01-01 09:00:00") + c(0, 120, 240, 360, 480, 600), # Meal 1
      as.POSIXct("2023-01-01 15:00:00") + c(0, 150, 300, 450, 600, 750)  # Meal 2
    ),
    end = c(
      as.POSIXct("2023-01-01 08:00:00") + c(180, 480, 780, 1080),
      as.POSIXct("2023-01-01 12:00:00") + c(120, 300, 480, 660),
      as.POSIXct("2023-01-01 18:00:00") + c(180, 420, 660, 900),
      as.POSIXct("2023-01-01 09:00:00") + c(90, 210, 330, 450, 570, 690),
      as.POSIXct("2023-01-01 15:00:00") + c(120, 270, 420, 570, 720, 870)
    ),
    bin = sample(1:10, 24, replace = TRUE),
    duration = rep(c(180, 120, 150), length.out = 24),
    intake = runif(24, 0.5, 3.0)
  )
  
  # Test with automatic eps determination
  result <- cluster_meals(sample_data)
  
  # Basic structure tests
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Check required columns
  expected_cols <- c("cow", "date", "meal_id", "meal_start", "meal_end", 
                    "meal_duration", "visit_count", "total_intake", 
                    "feeding_duration", "unique_bins_count")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check data types
  expect_type(result$cow, "integer")
  expect_s3_class(result$date, "Date")
  expect_type(result$meal_id, "integer")
  expect_s3_class(result$meal_start, "POSIXct")
  expect_s3_class(result$meal_end, "POSIXct")
  expect_type(result$meal_duration, "double")
  expect_type(result$visit_count, "integer")
  expect_type(result$total_intake, "double")
  expect_type(result$feeding_duration, "double")
  expect_type(result$unique_bins_count, "integer")
  
  # Check that we have meals for both cows
  expect_true(1001 %in% result$cow)
  expect_true(1002 %in% result$cow)
  
  # Check that meal_ids are sequential within each cow
  for (cow_id in unique(result$cow)) {
    cow_meals <- result[result$cow == cow_id, ]
    cow_meals <- cow_meals[order(cow_meals$meal_start), ]
    expect_equal(cow_meals$meal_id, seq_len(nrow(cow_meals)))
  }
})

test_that("cluster_meals works with list of dataframes", {
  # Create two dataframes for different days
  df1 <- data.frame(
    cow = rep(1001, 6),
    start = as.POSIXct("2023-01-01 08:00:00") + c(0, 300, 600, 3600, 3900, 4200),
    end = as.POSIXct("2023-01-01 08:00:00") + c(180, 480, 780, 3780, 4080, 4380),
    bin = sample(1:5, 6, replace = TRUE),
    duration = rep(180, 6),
    intake = runif(6, 1, 2)
  )
  
  df2 <- data.frame(
    cow = rep(1002, 6),
    start = as.POSIXct("2023-01-02 09:00:00") + c(0, 240, 480, 2400, 2640, 2880),
    end = as.POSIXct("2023-01-02 09:00:00") + c(120, 360, 600, 2520, 2760, 3000),
    bin = sample(1:5, 6, replace = TRUE),
    duration = rep(120, 6),
    intake = runif(6, 0.5, 1.5)
  )
  
  data_list <- list("2023-01-01" = df1, "2023-01-02" = df2)
  
  result <- cluster_meals(data_list)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(1001 %in% result$cow)
  expect_true(1002 %in% result$cow)
  
  # Check that we have data from both dates
  expect_true(as.Date("2023-01-01") %in% result$date)
  expect_true(as.Date("2023-01-02") %in% result$date)
})

test_that("cluster_meals handles custom column names", {
  sample_data <- data.frame(
    animal_id = rep(1001, 6),
    visit_start = as.POSIXct("2023-01-01 08:00:00") + c(0, 300, 600, 3600, 3900, 4200),
    visit_end = as.POSIXct("2023-01-01 08:00:00") + c(180, 480, 780, 3780, 4080, 4380),
    feeder_bin = sample(1:5, 6, replace = TRUE),
    visit_duration = rep(180, 6),
    feed_intake = runif(6, 1, 2)
  )
  
  result <- cluster_meals(sample_data,
                         id_col = "animal_id",
                         start_col = "visit_start", 
                         end_col = "visit_end",
                         bin_col = "feeder_bin",
                         dur_col = "visit_duration",
                         intake_col = "feed_intake")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(1001 %in% result$cow)
})

test_that("cluster_meals handles custom eps parameter", {
  sample_data <- data.frame(
    cow = rep(1001, 8),
    start = as.POSIXct("2023-01-01 08:00:00") + c(0, 300, 600, 900, 3600, 3900, 4200, 4500),
    end = as.POSIXct("2023-01-01 08:00:00") + c(180, 480, 780, 1080, 3780, 4080, 4380, 4680),
    bin = sample(1:5, 8, replace = TRUE),
    duration = rep(180, 8),
    intake = runif(8, 1, 2)
  )
  
  # Test with small eps (should create more meals)
  result_small <- cluster_meals(sample_data, eps = 10)
  
  # Test with large eps (should create fewer meals)  
  result_large <- cluster_meals(sample_data, eps = 60)
  
  expect_s3_class(result_small, "data.frame")
  expect_s3_class(result_large, "data.frame")
  
  # With larger eps, we should generally have fewer or equal meals
  if (nrow(result_small) > 0 && nrow(result_large) > 0) {
    expect_true(nrow(result_large) <= nrow(result_small))
  }
})

test_that("cluster_meals handles edge cases", {
  # Test with insufficient visits (less than min_pts)
  small_data <- data.frame(
    cow = 1001,
    start = as.POSIXct("2023-01-01 08:00:00"),
    end = as.POSIXct("2023-01-01 08:03:00"),
    bin = 1,
    duration = 180,
    intake = 1.5
  )
  
  result_small <- cluster_meals(small_data, min_pts = 3)
  expect_s3_class(result_small, "data.frame")
  expect_equal(nrow(result_small), 0) # Should be empty due to noise treatment
  
  # Test with empty dataframe
  empty_data <- data.frame(
    cow = integer(0),
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0)),
    bin = integer(0),
    duration = numeric(0),
    intake = numeric(0)
  )
  
  result_empty <- cluster_meals(empty_data)
  expect_s3_class(result_empty, "data.frame")
  expect_equal(nrow(result_empty), 0)
})

test_that("cluster_meals validates input parameters", {
  sample_data <- data.frame(
    cow = 1001,
    start = as.POSIXct("2023-01-01 08:00:00"),
    end = as.POSIXct("2023-01-01 08:03:00"),
    bin = 1,
    duration = 180,
    intake = 1.5
  )
  
  # Test NULL data
  expect_error(cluster_meals(NULL), "data cannot be NULL")
  
  # Test invalid data type
  expect_error(cluster_meals("not_a_dataframe"), "data must be a dataframe or list of dataframes")
  
  # Test missing required columns
  incomplete_data <- data.frame(cow = 1001, start = as.POSIXct("2023-01-01 08:00:00"))
  expect_error(cluster_meals(incomplete_data), "Missing required columns")
})

test_that("determine_optimal_eps function works correctly", {
  # Test with various time patterns
  
  # Closely spaced visits
  close_times <- c(0, 5, 10, 15, 60, 65, 70, 75)
  eps1 <- moo4feed:::determine_optimal_eps(close_times)
  expect_type(eps1, "double")
  expect_true(eps1 >= 5 && eps1 <= 120)
  
  # Widely spaced visits  
  wide_times <- c(0, 120, 240, 360, 480)
  eps2 <- moo4feed:::determine_optimal_eps(wide_times)
  expect_type(eps2, "double")
  expect_true(eps2 >= 5 && eps2 <= 120)
  
  # Single time point
  single_time <- c(60)
  eps3 <- moo4feed:::determine_optimal_eps(single_time)
  expect_equal(eps3, 30) # Default fallback
  
  # Empty vector
  empty_times <- numeric(0)
  eps4 <- moo4feed:::determine_optimal_eps(empty_times)
  expect_equal(eps4, 30) # Default fallback
})

test_that("cluster_meals produces consistent meal statistics", {
  # Create data with known meal structure
  sample_data <- data.frame(
    cow = rep(1001, 6),
    start = as.POSIXct("2023-01-01 08:00:00") + c(0, 300, 600, 3600, 3900, 4200),
    end = as.POSIXct("2023-01-01 08:00:00") + c(180, 480, 780, 3780, 4080, 4380),
    bin = c(1, 2, 1, 3, 3, 4),
    duration = c(180, 180, 180, 180, 180, 180),
    intake = c(1.0, 1.5, 1.2, 0.8, 1.1, 0.9)
  )
  
  result <- cluster_meals(sample_data, eps = 15) # Force tight clustering
  
  if (nrow(result) > 0) {
    # Check that meal statistics make sense
    for (i in seq_len(nrow(result))) {
      meal <- result[i, ]
      
      # Meal duration should be non-negative
      expect_true(meal$meal_duration >= 0)
      
      # Visit count should be positive
      expect_true(meal$visit_count > 0)
      
      # Total intake should be non-negative
      expect_true(meal$total_intake >= 0)
      
      # Feeding duration should be non-negative
      expect_true(meal$feeding_duration >= 0)
      
      # Unique bins count should be between 1 and visit count
      expect_true(meal$unique_bins_count >= 1)
      expect_true(meal$unique_bins_count <= meal$visit_count)
      
      # Meal end should be >= meal start
      expect_true(meal$meal_end >= meal$meal_start)
    }
  }
})

test_that("cluster_meals handles different min_pts values", {
  sample_data <- data.frame(
    cow = rep(1001, 8),
    start = as.POSIXct("2023-01-01 08:00:00") + c(0, 300, 600, 900, 3600, 3900, 4200, 4500),
    end = as.POSIXct("2023-01-01 08:00:00") + c(180, 480, 780, 1080, 3780, 4080, 4380, 4680),
    bin = sample(1:5, 8, replace = TRUE),
    duration = rep(180, 8),
    intake = runif(8, 1, 2)
  )
  
  # Test with min_pts = 2
  result_min2 <- cluster_meals(sample_data, min_pts = 2, eps = 20)
  
  # Test with min_pts = 4  
  result_min4 <- cluster_meals(sample_data, min_pts = 4, eps = 20)
  
  expect_s3_class(result_min2, "data.frame")
  expect_s3_class(result_min4, "data.frame")
  
  # Higher min_pts should generally result in fewer meals (more restrictive)
  if (nrow(result_min2) > 0 && nrow(result_min4) > 0) {
    expect_true(nrow(result_min4) <= nrow(result_min2))
  }
}) 