# -----------------------------------------------------------------------------#
# ---------------------- Tests for cluster_meals_cow_day ---------------------#
# -----------------------------------------------------------------------------#

# Test helper function to create sample data
create_sample_animal_day <- function(n_visits = 5, animal_id = "A001", date_val = "2023-01-01") {
  start_times <- lubridate::as_datetime(paste(date_val, c("08:00:00", "08:15:00", "08:30:00", "12:00:00", "12:15:00")[1:n_visits]))
  end_times <- start_times + lubridate::minutes(5)
  
  data.frame(
    cow = rep(animal_id, n_visits),
    start = start_times,
    end = end_times,
    bin = rep(c(1, 2), length.out = n_visits),
    intake = runif(n_visits, 0.5, 3.0),
    duration = rep(300, n_visits),  # 5 minutes in seconds
    date = as.Date(rep(date_val, n_visits)),
    stringsAsFactors = FALSE
  )
}

# Tests for Normal Operation
test_that("cluster_meals_cow_day works with typical data", {
  # Create sample data with clear meal patterns
  animal_day_df <- create_sample_animal_day(5)
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 60,  # 60 minutes
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)
  
  # Check required columns exist
  expected_cols <- c("cow", "date", "meal_id", "meal_start", "meal_end", 
                     "meal_duration", "visit_count", "total_intake", 
                     "feeding_percentage", "unique_bins_count")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check data types
  expect_type(result$meal_id, "integer")
  expect_s3_class(result$meal_start, "POSIXct")
  expect_s3_class(result$meal_end, "POSIXct")
  expect_type(result$meal_duration, "double")
  expect_type(result$visit_count, "integer")
  expect_type(result$total_intake, "double")
  expect_type(result$feeding_percentage, "double")
  expect_type(result$unique_bins_count, "integer")
  
  # Check meal_id is sequential
  expect_equal(result$meal_id, seq_len(nrow(result)))
  
  # Check logical constraints
  expect_true(all(result$meal_duration >= 0))
  expect_true(all(result$visit_count >= 1))
  expect_true(all(result$total_intake >= 0))
  expect_true(all(result$feeding_percentage >= 0 & result$feeding_percentage <= 100))
  expect_true(all(result$unique_bins_count >= 1))
  expect_true(all(result$meal_start <= result$meal_end))
})

test_that("cluster_meals_cow_day creates correct number of meals", {
  # Create data with obvious 2 meal pattern (morning and afternoon)
  animal_day_df <- data.frame(
    cow = rep("A001", 6),
    start = lubridate::as_datetime(c(
      "2023-01-01 08:00:00", "2023-01-01 08:10:00", "2023-01-01 08:20:00",
      "2023-01-01 14:00:00", "2023-01-01 14:10:00", "2023-01-01 14:20:00"
    )),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 08:15:00", "2023-01-01 08:25:00",
      "2023-01-01 14:05:00", "2023-01-01 14:15:00", "2023-01-01 14:25:00"
    )),
    bin = rep(1, 6),
    intake = rep(1.5, 6),
    duration = rep(300, 6),
    date = as.Date(rep("2023-01-01", 6))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 30,  # 30 minutes - should create 2 meals
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # Should create 2 meals
  expect_equal(nrow(result), 2)
  expect_equal(result$visit_count, c(3, 3))
  expect_equal(result$meal_id, c(1, 2))
  
  # Check meal timing
  expect_true(lubridate::hour(result$meal_start[1]) == 8)
  expect_true(lubridate::hour(result$meal_start[2]) == 14)
})

test_that("cluster_meals_cow_day calculates meal statistics correctly", {
  # Create precise test data
  animal_day_df <- data.frame(
    cow = "TEST",
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:10:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:05:00", "2023-01-01 08:15:00")),
    bin = c(1, 2),
    intake = c(2.0, 3.0),
    duration = c(300, 600),  # 5 and 10 minutes
    date = as.Date(c("2023-01-01", "2023-01-01"))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 30,
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(result$visit_count, 2)
  expect_equal(result$total_intake, 5.0)
  expect_equal(result$unique_bins_count, 2)
  expect_equal(result$meal_duration, 15 * 60)  # 15 minutes in seconds
  
  # Check feeding percentage: (300 + 600) seconds / (15*60) seconds * 100
  expected_feeding_percentage <- (900 / (15 * 60)) * 100
  expect_equal(result$feeding_percentage, expected_feeding_percentage)
})

# Tests for Edge Cases
test_that("cluster_meals_cow_day returns empty for insufficient visits", {
  # Test with fewer visits than min_pts
  animal_day_df <- create_sample_animal_day(2)
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 60,
    min_pts = 3,  # More than available visits
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # Should return empty dataframe with correct structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  
  # Check required columns exist even in empty result
  expected_cols <- c("cow", "date", "meal_id", "meal_start", "meal_end", 
                     "meal_duration", "visit_count", "total_intake", 
                     "feeding_percentage", "unique_bins_count")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("cluster_meals_cow_day handles exactly min_pts visits", {
  # Test edge case where nrow == min_pts (now should work since condition changed to <)
  animal_day_df <- create_sample_animal_day(3)
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 60,
    min_pts = 3,  # Exactly the number of visits
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # This should now work since condition changed from <= to <
  expect_true(nrow(result) >= 1)
})

test_that("cluster_meals_cow_day handles all visits as noise", {
  # Create data where visits are too far apart to cluster
  animal_day_df <- data.frame(
    cow = "A001",
    start = lubridate::as_datetime(c(
      "2023-01-01 08:00:00", "2023-01-01 10:00:00", 
      "2023-01-01 12:00:00", "2023-01-01 14:00:00"
    )),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 10:05:00", 
      "2023-01-01 12:05:00", "2023-01-01 14:05:00"
    )),
    bin = rep(1, 4),
    intake = rep(1.0, 4),
    duration = rep(300, 4),
    date = as.Date(rep("2023-01-01", 4))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 30,  # 30 minutes - too small to cluster visits 2 hours apart
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # Should return empty because all visits are treated as noise
  expect_equal(nrow(result), 0)
})

test_that("cluster_meals_cow_day handles single visit per meal", {
  # Test with min_pts = 1 (single visits can form meals)
  animal_day_df <- data.frame(
    cow = "A001",
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 12:00:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:05:00", "2023-01-01 12:05:00")),
    bin = c(1, 2),
    intake = c(1.5, 2.0),
    duration = c(300, 300),
    date = as.Date(rep("2023-01-01", 2))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 60,
    min_pts = 1,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  expect_equal(nrow(result), 2)
  expect_true(all(result$visit_count == 1))
})

test_that("cluster_meals_cow_day handles unsorted data", {
  # Create data that's not sorted by start time
  animal_day_df <- data.frame(
    cow = rep("A001", 4),
    start = lubridate::as_datetime(c(
      "2023-01-01 12:00:00", "2023-01-01 08:00:00",
      "2023-01-01 08:10:00", "2023-01-01 12:10:00"
    )),
    end = lubridate::as_datetime(c(
      "2023-01-01 12:05:00", "2023-01-01 08:05:00",
      "2023-01-01 08:15:00", "2023-01-01 12:15:00"
    )),
    bin = rep(1, 4),
    intake = rep(1.5, 4),
    duration = rep(300, 4),
    date = as.Date(rep("2023-01-01", 4))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 30,
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # Should still work correctly and create 2 meals
  expect_equal(nrow(result), 2)
  
  # Meals should be ordered by start time
  expect_true(result$meal_start[1] < result$meal_start[2])
  expect_equal(result$meal_id, c(1, 2))
})

test_that("cluster_meals_cow_day handles zero meal duration", {
  # Create data where meal start and end are the same
  animal_day_df <- data.frame(
    cow = "A001",
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:00:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:00:00")),
    bin = c(1, 2),
    intake = c(1.5, 2.0),
    duration = c(0, 0),
    date = as.Date(rep("2023-01-01", 2))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 60,
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(result$meal_duration, 0)
  expect_equal(result$feeding_percentage, 0)  # Should handle division by zero
})

test_that("cluster_meals_cow_day handles NA values in intake and duration", {
  # Create data with NA values
  animal_day_df <- data.frame(
    cow = rep("A001", 3),
    start = lubridate::as_datetime(c(
      "2023-01-01 08:00:00", "2023-01-01 08:10:00", "2023-01-01 08:20:00"
    )),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 08:15:00", "2023-01-01 08:25:00"
    )),
    bin = rep(1, 3),
    intake = c(1.5, NA, 2.0),  # One NA value
    duration = c(300, 600, NA),  # One NA value
    date = as.Date(rep("2023-01-01", 3))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 30,
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(result$total_intake, 3.5)  # Should ignore NA and sum 1.5 + 2.0
  expect_equal(result$feeding_percentage, (900 / (25 * 60)) * 100)  # Should ignore NA duration
})

test_that("cluster_meals_cow_day handles NA values in start times", {
  # Create data with NA values in start times
  animal_day_df <- data.frame(
    cow = rep("A001", 4),
    start = c(
      lubridate::as_datetime("2023-01-01 08:00:00"),
      NA,  # NA value
      lubridate::as_datetime("2023-01-01 08:20:00"),
      lubridate::as_datetime("2023-01-01 08:30:00")
    ),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 08:15:00",
      "2023-01-01 08:25:00", "2023-01-01 08:35:00"
    )),
    bin = rep(1, 4),
    intake = rep(1.5, 4),
    duration = rep(300, 4),
    date = as.Date(rep("2023-01-01", 4))
  )
  
  # Should inform about NA values and remove them
  expect_message(
    result <- cluster_meals_cow_day(
      animal_day_df = animal_day_df,
      eps = 30,
      min_pts = 2,
      id_col = "cow",
      start_col = "start",
      end_col = "end",
      bin_col = "bin",
      intake_col = "intake",
      dur_col = "duration"
    ),
    "Found 1 NA values in start times for clustering"
  )
  
  # Should still create a meal with the remaining 3 valid visits
  expect_equal(nrow(result), 1)
  expect_equal(result$visit_count, 3)
})

test_that("cluster_meals_cow_day returns empty when all start times are NA", {
  # Create data where all start times are NA
  animal_day_df <- data.frame(
    cow = rep("A001", 3),
    start = rep(as.POSIXct(NA), 3),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 08:15:00", "2023-01-01 08:25:00"
    )),
    bin = rep(1, 3),
    intake = rep(1.5, 3),
    duration = rep(300, 3),
    date = as.Date(rep("2023-01-01", 3))
  )
  
  # Should error because no valid start times exist for conversion
  expect_error(
    cluster_meals_cow_day(
      animal_day_df = animal_day_df,
      eps = 30,
      min_pts = 2,
      id_col = "cow",
      start_col = "start",
      end_col = "end",
      bin_col = "bin",
      intake_col = "intake",
      dur_col = "duration"
    ),
    "Failed to convert datetime_vec to datetime objects"
  )
})

# Tests for Error Handling
test_that("cluster_meals_cow_day stops when eps is NULL", {
  animal_day_df <- create_sample_animal_day(5)
  
  expect_error(
    cluster_meals_cow_day(
      animal_day_df = animal_day_df,
      eps = NULL,  # This should trigger error
      min_pts = 2,
      id_col = "cow",
      start_col = "start",
      end_col = "end",
      bin_col = "bin",
      intake_col = "intake",
      dur_col = "duration"
    ),
    "eps should be determined before calling cluster_meals_cow_day"
  )
})

test_that("cluster_meals_cow_day stops when required columns are missing", {
  # Create data with missing column
  animal_day_df <- data.frame(
    cow = rep("A001", 3),
    start = lubridate::as_datetime(c(
      "2023-01-01 08:00:00", "2023-01-01 08:10:00", "2023-01-01 08:20:00"
    )),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 08:15:00", "2023-01-01 08:25:00"
    )),
    # Missing bin, intake, and duration columns
    date = as.Date(rep("2023-01-01", 3))
  )
  
  expect_error(
    cluster_meals_cow_day(
      animal_day_df = animal_day_df,
      eps = 30,
      min_pts = 2,
      id_col = "cow",
      start_col = "start",
      end_col = "end",
      bin_col = "bin",
      intake_col = "intake",
      dur_col = "duration"
    ),
    "Missing required columns: bin, intake, duration"
  )
})

test_that("cluster_meals_cow_day stops when date column is missing", {
  # Create data without date column
  animal_day_df <- data.frame(
    cow = rep("A001", 3),
    start = lubridate::as_datetime(c(
      "2023-01-01 08:00:00", "2023-01-01 08:10:00", "2023-01-01 08:20:00"
    )),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 08:15:00", "2023-01-01 08:25:00"
    )),
    bin = rep(1, 3),
    intake = rep(1.5, 3),
    duration = rep(300, 3)
    # Missing date column
  )
  
  expect_error(
    cluster_meals_cow_day(
      animal_day_df = animal_day_df,
      eps = 30,
      min_pts = 2,
      id_col = "cow",
      start_col = "start",
      end_col = "end",
      bin_col = "bin",
      intake_col = "intake",
      dur_col = "duration"
    ),
    "Missing required columns: date"
  )
})

# Tests for Custom Column Names
test_that("cluster_meals_cow_day works with custom column names", {
  # Create data with different column names
  animal_day_df <- data.frame(
    animal_id = rep("A001", 3),
    begin_time = lubridate::as_datetime(c(
      "2023-01-01 08:00:00", "2023-01-01 08:10:00", "2023-01-01 08:20:00"
    )),
    finish_time = lubridate::as_datetime(c(
      "2023-01-01 08:05:00", "2023-01-01 08:15:00", "2023-01-01 08:25:00"
    )),
    feeder = rep(1, 3),
    feed_intake = rep(1.5, 3),
    feeding_time = rep(300, 3),
    date = as.Date(rep("2023-01-01", 3))
  )
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 30,
    min_pts = 2,
    id_col = "animal_id",
    start_col = "begin_time",
    end_col = "finish_time",
    bin_col = "feeder",
    intake_col = "feed_intake",
    dur_col = "feeding_time"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("animal_id" %in% names(result))
  expect_true("meal_start" %in% names(result))
  expect_true("meal_end" %in% names(result))
  expect_true("meal_duration" %in% names(result))
  expect_true("visit_count" %in% names(result))
  expect_true("total_intake" %in% names(result))
  expect_true("feeding_percentage" %in% names(result))
  expect_true("unique_bins_count" %in% names(result))
})

# Test for very large eps value
test_that("cluster_meals_cow_day handles very large eps values", {
  animal_day_df <- create_sample_animal_day(5)
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 1440,  # 24 hours - should cluster everything
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # All visits should be in one big meal
  expect_equal(nrow(result), 1)
  expect_equal(result$visit_count, 5)
})

# Test for very small eps value
test_that("cluster_meals_cow_day handles very small eps values", {
  animal_day_df <- create_sample_animal_day(5)
  
  result <- cluster_meals_cow_day(
    animal_day_df = animal_day_df,
    eps = 1,  # 1 minute - very small
    min_pts = 2,
    id_col = "cow",
    start_col = "start",
    end_col = "end",
    bin_col = "bin",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  # With visits 15 minutes apart, should be all noise with eps=1
  expect_equal(nrow(result), 0)
}) 