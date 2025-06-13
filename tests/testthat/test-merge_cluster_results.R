# -----------------------------------------------------------------------------#
# ------------------------ Tests for merge_cluster_results -------------------#
# -----------------------------------------------------------------------------#

test_that("merge_cluster_results works for single dataframe input", {
  # Create sample visit data
  visit_data <- data.frame(
    cow = c(1, 1, 1, 2, 2),
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:05:00", 
                                    "2023-01-01 12:00:00", "2023-01-01 09:00:00", 
                                    "2023-01-01 09:30:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:03:00", "2023-01-01 08:08:00", 
                                  "2023-01-01 12:05:00", "2023-01-01 09:05:00", 
                                  "2023-01-01 09:35:00")),
    intake = c(1.5, 2.0, 1.8, 1.2, 1.6)
  )
  
  # Create sample meal results
  meal_results <- data.frame(
    cow = c(1, 1, 2),
    date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    meal_id = c(1, 2, 1),
    meal_start = lubridate::as_datetime(c("2023-01-01 08:03:30", "2023-01-01 11:58:00", 
                                         "2023-01-01 08:58:00")),
    meal_end = lubridate::as_datetime(c("2023-01-01 08:10:00", "2023-01-01 12:10:00", 
                                       "2023-01-01 09:40:00")),
    meal_duration = c(720, 720, 2520),
    total_intake = c(3.5, 1.8, 2.8),
    visit_count = c(2, 1, 2)
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  
  # Check new columns exist
  expected_cols <- c("meal_id", "meal_start", "meal_end", "meal_duration", 
                    "total_intake", "visit_count")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that visits were assigned correctly
  expect_equal(sum(result$meal_id > 0), 4) # 4 visits should be assigned to meals
  expect_equal(sum(result$meal_id == 0), 1) # 1 visit should be outlier
})

test_that("merge_cluster_results works for list of dataframes input", {
  # Create sample visit data as list
  visit_data <- list(
    "2023-01-01" = data.frame(
      cow = c(1, 1),
      start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:05:00")),
      end = lubridate::as_datetime(c("2023-01-01 08:03:00", "2023-01-01 08:08:00")),
      intake = c(1.5, 2.0)
    ),
    "2023-01-02" = data.frame(
      cow = c(2, 2),
      start = lubridate::as_datetime(c("2023-01-02 09:00:00", "2023-01-02 09:30:00")),
      end = lubridate::as_datetime(c("2023-01-02 09:05:00", "2023-01-02 09:35:00")),
      intake = c(1.2, 1.6)
    )
  )
  
  # Create sample meal results
  meal_results <- data.frame(
    cow = c(1, 2),
    date = as.Date(c("2023-01-01", "2023-01-02")),
    meal_id = c(1, 1),
    meal_start = lubridate::as_datetime(c("2023-01-01 07:58:00", "2023-01-02 08:58:00")),
    meal_end = lubridate::as_datetime(c("2023-01-01 08:10:00", "2023-01-02 09:40:00")),
    meal_duration = c(720, 2520),
    total_intake = c(3.5, 2.8),
    visit_count = c(2, 2)
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # Check structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, is.data.frame)))
  
  # Check that each dataframe has the expected columns
  expected_cols <- c("meal_id", "meal_start", "meal_end", "meal_duration", 
                    "total_intake", "visit_count")
  expect_true(all(expected_cols %in% names(result[[1]])))
  expect_true(all(expected_cols %in% names(result[[2]])))
})

test_that("merge_cluster_results handles visits with no matching meals (outliers)", {
  # Create visit data with no matching meals
  visit_data <- data.frame(
    cow = c(1, 1),
    start = lubridate::as_datetime(c("2023-01-01 15:00:00", "2023-01-01 16:00:00")),
    end = lubridate::as_datetime(c("2023-01-01 15:03:00", "2023-01-01 16:03:00")),
    intake = c(1.5, 2.0)
  )
  
  # Create meal results that don't overlap with visits
  meal_results <- data.frame(
    cow = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::as_datetime("2023-01-01 08:00:00"),
    meal_end = lubridate::as_datetime("2023-01-01 08:30:00"),
    meal_duration = 1800,
    total_intake = 3.5,
    visit_count = 2
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # All visits should be outliers
  expect_true(all(result$meal_id == 0))
  expect_true(all(is.na(result$meal_start)))
  expect_true(all(is.na(result$meal_end)))
  expect_true(all(is.na(result$meal_duration)))
  expect_true(all(is.na(result$total_intake)))
  expect_true(all(is.na(result$visit_count)))
})

test_that("merge_cluster_results handles empty visit data", {
  # Create empty visit data
  visit_data <- data.frame(
    cow = integer(0),
    start = lubridate::as_datetime(character(0)),
    end = lubridate::as_datetime(character(0)),
    intake = numeric(0)
  )
  
  # Create sample meal results
  meal_results <- data.frame(
    cow = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::as_datetime("2023-01-01 08:00:00"),
    meal_end = lubridate::as_datetime("2023-01-01 08:30:00"),
    meal_duration = 1800,
    total_intake = 3.5,
    visit_count = 2
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  
  # Check that new columns exist even in empty dataframe
  expected_cols <- c("meal_id", "meal_start", "meal_end", "meal_duration", 
                    "total_intake", "visit_count", "date")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("merge_cluster_results handles empty meal results", {
  # Create sample visit data
  visit_data <- data.frame(
    cow = c(1, 1),
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:05:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:03:00", "2023-01-01 08:08:00")),
    intake = c(1.5, 2.0)
  )
  
  # Create empty meal results
  meal_results <- data.frame(
    cow = integer(0),
    date = as.Date(character(0)),
    meal_id = integer(0),
    meal_start = lubridate::as_datetime(character(0)),
    meal_end = lubridate::as_datetime(character(0)),
    meal_duration = numeric(0),
    total_intake = numeric(0),
    visit_count = integer(0)
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # All visits should be outliers since no meals exist
  expect_true(all(result$meal_id == 0))
  expect_true(all(is.na(result$meal_start)))
})

test_that("merge_cluster_results handles empty list input", {
  # Create empty list
  visit_data <- list()
  
  # Create sample meal results
  meal_results <- data.frame(
    cow = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::as_datetime("2023-01-01 08:00:00"),
    meal_end = lubridate::as_datetime("2023-01-01 08:30:00"),
    meal_duration = 1800,
    total_intake = 3.5,
    visit_count = 2
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("merge_cluster_results handles list with empty dataframes", {
  # Create list with empty dataframes
  visit_data <- list(
    "2023-01-01" = data.frame(
      cow = integer(0),
      start = lubridate::as_datetime(character(0)),
      end = lubridate::as_datetime(character(0)),
      intake = numeric(0)
    ),
    "2023-01-02" = data.frame(
      cow = integer(0),
      start = lubridate::as_datetime(character(0)),
      end = lubridate::as_datetime(character(0)),
      intake = numeric(0)
    )
  )
  
  # Create sample meal results
  meal_results <- data.frame(
    cow = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::as_datetime("2023-01-01 08:00:00"),
    meal_end = lubridate::as_datetime("2023-01-01 08:30:00"),
    meal_duration = 1800,
    total_intake = 3.5,
    visit_count = 2
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(nrow(result[[1]]), 0)
  expect_equal(nrow(result[[2]]), 0)
  
  # Check that columns are properly initialized even in empty dataframes
  expected_cols <- c("meal_id", "meal_start", "meal_end", "meal_duration", 
                    "total_intake", "visit_count", "date")
  expect_true(all(expected_cols %in% names(result[[1]])))
  expect_true(all(expected_cols %in% names(result[[2]])))
})

test_that("merge_cluster_results handles custom column names", {
  # Create visit data with custom column names
  visit_data <- data.frame(
    animal_id = c(1, 1),
    begin_time = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:05:00")),
    finish_time = lubridate::as_datetime(c("2023-01-01 08:03:00", "2023-01-01 08:08:00")),
    intake = c(1.5, 2.0)
  )
  
  # Create meal results with custom column names
  meal_results <- data.frame(
    animal_id = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::as_datetime("2023-01-01 07:58:00"),
    meal_end = lubridate::as_datetime("2023-01-01 08:10:00"),
    meal_duration = 720,
    total_intake = 3.5,
    visit_count = 2
  )
  
  result <- merge_cluster_results(visit_data, meal_results, 
                                 id_col = "animal_id", 
                                 start_col = "begin_time", 
                                 end_col = "finish_time")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$meal_id > 0)) # Both visits should be assigned
})

test_that("merge_cluster_results adds date column when missing", {
  # Create visit data without date column
  visit_data <- data.frame(
    cow = c(1, 1),
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:05:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:03:00", "2023-01-01 08:08:00")),
    intake = c(1.5, 2.0)
  )
  
  meal_results <- data.frame(
    cow = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::as_datetime("2023-01-01 07:58:00"),
    meal_end = lubridate::as_datetime("2023-01-01 08:10:00"),
    meal_duration = 720,
    total_intake = 3.5,
    visit_count = 2
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  expect_true("date" %in% names(result))
  expect_s3_class(result$date, "Date")
})

test_that("merge_cluster_results handles multiple meals per animal-day", {
  # Create visit data
  visit_data <- data.frame(
    cow = c(1, 1, 1, 1),
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:05:00",
                                    "2023-01-01 12:00:00", "2023-01-01 12:05:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:03:00", "2023-01-01 08:08:00",
                                  "2023-01-01 12:03:00", "2023-01-01 12:08:00")),
    intake = c(1.5, 2.0, 1.8, 1.2)
  )
  
  # Create meal results with multiple meals for same animal-day
  meal_results <- data.frame(
    cow = c(1, 1),
    date = as.Date(c("2023-01-01", "2023-01-01")),
    meal_id = c(1, 2),
    meal_start = lubridate::as_datetime(c("2023-01-01 07:58:00", "2023-01-01 11:58:00")),
    meal_end = lubridate::as_datetime(c("2023-01-01 08:10:00", "2023-01-01 12:10:00")),
    meal_duration = c(720, 720),
    total_intake = c(3.5, 3.0),
    visit_count = c(2, 2)
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # Check that visits are assigned to correct meals
  expect_equal(length(unique(result$meal_id[result$meal_id > 0])), 2)
  expect_true(all(result$meal_id > 0)) # All visits should be assigned
})

# -----------------------------------------------------------------------------#
# ---------------------------- Error Handling Tests -------------------------#
# -----------------------------------------------------------------------------#

test_that("merge_cluster_results throws error for NULL inputs", {
  visit_data <- data.frame(cow = 1, start = Sys.time(), end = Sys.time())
  meal_results <- data.frame(cow = 1, date = Sys.Date(), meal_id = 1, 
                            meal_start = Sys.time(), meal_end = Sys.time(),
                            meal_duration = 100, total_intake = 1, visit_count = 1)
  
  expect_error(merge_cluster_results(NULL, meal_results), 
               "Both visit_data and meal_results must be provided")
  expect_error(merge_cluster_results(visit_data, NULL), 
               "Both visit_data and meal_results must be provided")
})

test_that("merge_cluster_results throws error for non-dataframe meal_results", {
  visit_data <- data.frame(cow = 1, start = Sys.time(), end = Sys.time())
  
  expect_error(merge_cluster_results(visit_data, "not_a_dataframe"), 
               "meal_results must be a dataframe")
  expect_error(merge_cluster_results(visit_data, list(a = 1)), 
               "meal_results must be a dataframe")
})

test_that("merge_cluster_results throws error for invalid visit_data structure", {
  meal_results <- data.frame(cow = 1, date = Sys.Date(), meal_id = 1, 
                            meal_start = Sys.time(), meal_end = Sys.time(),
                            meal_duration = 100, total_intake = 1, visit_count = 1)
  
  expect_error(merge_cluster_results("not_valid", meal_results), 
               "visit_data must be a dataframe or list of dataframes")
  expect_error(merge_cluster_results(123, meal_results), 
               "visit_data must be a dataframe or list of dataframes")
})

test_that("merge_cluster_results throws error for list with non-dataframes", {
  meal_results <- data.frame(cow = 1, date = Sys.Date(), meal_id = 1, 
                            meal_start = Sys.time(), meal_end = Sys.time(),
                            meal_duration = 100, total_intake = 1, visit_count = 1)
  
  invalid_list <- list(
    data.frame(cow = 1, start = Sys.time(), end = Sys.time()),
    "not_a_dataframe"
  )
  
  expect_error(merge_cluster_results(invalid_list, meal_results), 
               "All items in visit_data list must be dataframes")
})

test_that("merge_cluster_results throws error for missing required columns in visit_data", {
  # Missing start column
  visit_data <- data.frame(cow = 1, end = Sys.time())
  meal_results <- data.frame(cow = 1, date = Sys.Date(), meal_id = 1, 
                            meal_start = Sys.time(), meal_end = Sys.time(),
                            meal_duration = 100, total_intake = 1, visit_count = 1)
  
  expect_error(merge_cluster_results(visit_data, meal_results), 
               "Missing required columns in visit_data: start")
  
  # Missing multiple columns
  visit_data2 <- data.frame(cow = 1)
  expect_error(merge_cluster_results(visit_data2, meal_results), 
               "Missing required columns in visit_data")
})

test_that("merge_cluster_results throws error for missing required columns in meal_results", {
  visit_data <- data.frame(cow = 1, start = Sys.time(), end = Sys.time())
  
  # Missing meal_id column
  meal_results <- data.frame(cow = 1, date = Sys.Date(), 
                            meal_start = Sys.time(), meal_end = Sys.time(),
                            meal_duration = 100, total_intake = 1, visit_count = 1)
  
  expect_error(merge_cluster_results(visit_data, meal_results), 
               "Missing required columns in meal_results: meal_id")
  
  # Missing multiple columns
  meal_results2 <- data.frame(cow = 1, date = Sys.Date())
  expect_error(merge_cluster_results(visit_data, meal_results2), 
               "Missing required columns in meal_results")
})

# -----------------------------------------------------------------------------#
# ---------------------------- Edge Cases Tests -----------------------------#
# -----------------------------------------------------------------------------#

test_that("merge_cluster_results handles visits spanning multiple days", {
  # Create visit that spans midnight
  visit_data <- data.frame(
    cow = 1,
    start = lubridate::as_datetime("2023-01-01 23:58:00"),
    end = lubridate::as_datetime("2023-01-02 00:02:00"),
    intake = 1.5
  )
  
  # Create meal on first day
  meal_results <- data.frame(
    cow = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::as_datetime("2023-01-01 23:55:00"),
    meal_end = lubridate::as_datetime("2023-01-02 00:05:00"),
    meal_duration = 600,
    total_intake = 1.5,
    visit_count = 1
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # Visit should be assigned to meal (date is extracted from start time)
  expect_equal(result$meal_id[1], 1)
})

test_that("merge_cluster_results handles different timezone formats", {
  # Create visit data with timezone
  visit_data <- data.frame(
    cow = 1,
    start = lubridate::with_tz(lubridate::as_datetime("2023-01-01 08:00:00"), "UTC"),
    end = lubridate::with_tz(lubridate::as_datetime("2023-01-01 08:03:00"), "UTC"),
    intake = 1.5
  )
  
  # Create meal results with different timezone
  meal_results <- data.frame(
    cow = 1,
    date = as.Date("2023-01-01"),
    meal_id = 1,
    meal_start = lubridate::with_tz(lubridate::as_datetime("2023-01-01 07:58:00"), "America/Vancouver"),
    meal_end = lubridate::with_tz(lubridate::as_datetime("2023-01-01 08:10:00"), "America/Vancouver"),
    meal_duration = 720,
    total_intake = 1.5,
    visit_count = 1
  )
  
  # Should not error (lubridate handles timezone conversion)
  result <- merge_cluster_results(visit_data, meal_results)
  expect_s3_class(result, "data.frame")
})

test_that("merge_cluster_results handles overlapping meals correctly", {
  # Create visit data
  visit_data <- data.frame(
    cow = 1,
    start = lubridate::as_datetime("2023-01-01 08:00:00"),
    end = lubridate::as_datetime("2023-01-01 08:03:00"),
    intake = 1.5
  )
  
  # Create overlapping meals (visit could match both)
  meal_results <- data.frame(
    cow = c(1, 1),
    date = as.Date(c("2023-01-01", "2023-01-01")),
    meal_id = c(1, 2),
    meal_start = lubridate::as_datetime(c("2023-01-01 07:58:00", "2023-01-01 07:59:00")),
    meal_end = lubridate::as_datetime(c("2023-01-01 08:10:00", "2023-01-01 08:05:00")),
    meal_duration = c(720, 360),
    total_intake = c(1.5, 1.5),
    visit_count = c(1, 1)
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # Should assign to first matching meal
  expect_equal(result$meal_id[1], 1)
})

test_that("merge_cluster_results handles large datasets efficiently", {
  # Create larger dataset to test performance
  n_visits <- 1000
  visit_data <- data.frame(
    cow = rep(1:10, each = 100),
    start = lubridate::as_datetime("2023-01-01 08:00:00") + 
            lubridate::minutes(seq(0, n_visits-1) * 5),
    end = lubridate::as_datetime("2023-01-01 08:00:00") + 
          lubridate::minutes(seq(0, n_visits-1) * 5 + 3),
    intake = runif(n_visits, 0.5, 3.0)
  )
  
  # Create meal results
  meal_results <- data.frame(
    cow = rep(1:10, each = 5),
    date = as.Date("2023-01-01"),
    meal_id = rep(1:5, 10),
    meal_start = lubridate::as_datetime("2023-01-01 08:00:00") + 
                 lubridate::hours(rep(c(0, 4, 8, 12, 16), 10)),
    meal_end = lubridate::as_datetime("2023-01-01 08:00:00") + 
               lubridate::hours(rep(c(2, 6, 10, 14, 18), 10)),
    meal_duration = rep(7200, 50),
    total_intake = runif(50, 10, 30),
    visit_count = rep(20, 50)
  )
  
  # Should complete without error
  start_time <- Sys.time()
  result <- merge_cluster_results(visit_data, meal_results)
  end_time <- Sys.time()
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), n_visits)
  
  # Should complete in reasonable time (less than 10 seconds for 1000 visits)
  expect_lt(as.numeric(end_time - start_time), 10)
})

test_that("merge_cluster_results preserves original column order and data types", {
  # Create visit data with specific column order and types
  visit_data <- data.frame(
    extra_col = c("A", "B"),
    cow = c(1L, 1L),
    start = lubridate::as_datetime(c("2023-01-01 08:00:00", "2023-01-01 08:05:00")),
    end = lubridate::as_datetime(c("2023-01-01 08:03:00", "2023-01-01 08:08:00")),
    intake = c(1.5, 2.0),
    another_col = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  meal_results <- data.frame(
    cow = 1L,
    date = as.Date("2023-01-01"),
    meal_id = 1L,
    meal_start = lubridate::as_datetime("2023-01-01 07:58:00"),
    meal_end = lubridate::as_datetime("2023-01-01 08:10:00"),
    meal_duration = 720.0,
    total_intake = 3.5,
    visit_count = 2L
  )
  
  result <- merge_cluster_results(visit_data, meal_results)
  
  # Check that original columns are preserved
  expect_true("extra_col" %in% names(result))
  expect_true("another_col" %in% names(result))
  
  # Check data types are preserved
  expect_type(result$extra_col, "character")
  expect_type(result$cow, "integer")
  expect_type(result$another_col, "logical")
  
  # Check new columns have correct types
  expect_type(result$meal_id, "integer")
  expect_s3_class(result$meal_start, "POSIXct")
  expect_type(result$meal_duration, "double")
}) 