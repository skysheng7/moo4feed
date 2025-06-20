# Tests for synch_feed_drink_combined.R

# Helper function to create toy synchronicity data
create_toy_synch_data <- function() {
  list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 0, 1),
      `2` = c(0, 1, 1),
      check.names = FALSE
    ),
    day2 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-02 10:00:00"), 
                 lubridate::ymd_hms("2023-01-02 10:00:01"), by = "sec"),
      `1` = c(1, 0),
      `2` = c(1, 0),
      `3` = c(0, 1),
      check.names = FALSE
    )
  )
}

# Tests for total_animals_present function
test_that("total_animals_present works with normal input", {
  toy_data <- create_toy_synch_data()
  result <- total_animals_present(toy_data, bins_feed = 1:5, bins_wat = 101:103)
  
  # Check structure
  expect_equal(length(result), 2)
  expect_equal(names(result), c("day1", "day2"))
  
  # Check day1 calculations
  day1_result <- result[[1]]
  expect_true("total_animal_num" %in% names(day1_result))
  expect_true("total_bin_occupied" %in% names(day1_result))
  expect_true("empty_bin_num" %in% names(day1_result))
  
  # Check values for day1
  expected_totals <- c(1, 1, 2)  # sum of animals at each time point
  expect_equal(day1_result$total_animal_num, expected_totals)
  expect_equal(day1_result$total_bin_occupied, expected_totals)
  expect_equal(day1_result$empty_bin_num, c(7, 7, 6))  # 8 total bins - animals present
  
  # Check day2 calculations
  day2_result <- result[[2]]
  expected_totals_day2 <- c(2, 1)  # sum of animals at each time point
  expect_equal(day2_result$total_animal_num, expected_totals_day2)
  expect_equal(day2_result$total_bin_occupied, expected_totals_day2)
  expect_equal(day2_result$empty_bin_num, c(6, 7))  # 8 total bins - animals present
})

test_that("total_animals_present handles single animal correctly", {
  single_animal_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 1,
      check.names = FALSE
    )
  )
  
  result <- total_animals_present(single_animal_data, bins_feed = 1:3, bins_wat = 101:102)
  
  expect_equal(result[[1]]$total_animal_num, 1)
  expect_equal(result[[1]]$total_bin_occupied, 1)
  expect_equal(result[[1]]$empty_bin_num, 4)  # 5 total bins - 1 occupied
})

test_that("total_animals_present handles no animals present", {
  no_animals_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(0, 0),
      `2` = c(0, 0),
      check.names = FALSE
    )
  )
  
  result <- total_animals_present(no_animals_data, bins_feed = 1:4, bins_wat = 101:103)
  
  expect_equal(result[[1]]$total_animal_num, c(0, 0))
  expect_equal(result[[1]]$total_bin_occupied, c(0, 0))
  expect_equal(result[[1]]$empty_bin_num, c(7, 7))  # all 7 bins empty
})

test_that("total_animals_present preserves original list names", {
  toy_data <- create_toy_synch_data()
  names(toy_data) <- c("custom_day1", "custom_day2")
  
  result <- total_animals_present(toy_data, bins_feed = 1:3, bins_wat = 101:102)
  
  expect_equal(names(result), c("custom_day1", "custom_day2"))
})

test_that("total_animals_present handles missing list names", {
  toy_data <- create_toy_synch_data()
  names(toy_data) <- NULL
  
  result <- total_animals_present(toy_data, bins_feed = 1:3, bins_wat = 101:102)
  
  expect_equal(length(result), 2)
  # Names should be NULL when input has no names
  expect_null(names(result))
})

test_that("total_animals_present handles NA values correctly", {
  na_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, NA, 1),
      `2` = c(NA, 1, 1),
      check.names = FALSE
    )
  )
  
  result <- total_animals_present(na_data, bins_feed = 1:3, bins_wat = 101:102)
  
  # rowSums with na.rm = TRUE should handle NAs correctly
  expected_totals <- c(1, 1, 2)  # NA values ignored in sum
  expect_equal(result[[1]]$total_animal_num, expected_totals)
})

# Error handling tests
test_that("total_animals_present errors for NULL or empty input", {
  expect_error(total_animals_present(NULL, bins_feed = 1:3, bins_wat = 101:102), 
               "Input feed_drink_synch_master_animal cannot be NULL or empty")
  
  expect_error(total_animals_present(list(), bins_feed = 1:3, bins_wat = 101:102), 
               "Input feed_drink_synch_master_animal cannot be NULL or empty")
})

test_that("total_animals_present errors for non-numeric bins", {
  toy_data <- create_toy_synch_data()
  
  expect_error(total_animals_present(toy_data, bins_feed = c("a", "b"), bins_wat = 101:102), 
               "bins_feed and bins_wat must be numeric")
  
  expect_error(total_animals_present(toy_data, bins_feed = 1:3, bins_wat = c("x", "y")), 
               "bins_feed and bins_wat must be numeric")
})

test_that("total_animals_present errors for empty bins", {
  toy_data <- create_toy_synch_data()
  
  expect_error(total_animals_present(toy_data, bins_feed = numeric(0), bins_wat = 101:102), 
               "bins_feed and bins_wat cannot be empty")
  
  expect_error(total_animals_present(toy_data, bins_feed = 1:3, bins_wat = numeric(0)), 
               "bins_feed and bins_wat cannot be empty")
})

test_that("total_animals_present errors for invalid data frame structure", {
  invalid_data <- list(
    day1 = "not a data frame"
  )
  
  expect_error(total_animals_present(invalid_data, bins_feed = 1:3, bins_wat = 101:102), 
               "Each element in feed_drink_synch_master_animal must be a data frame")
})

test_that("total_animals_present errors for missing Time column", {
  no_time_data <- list(
    day1 = data.frame(
      `1` = c(1, 0),
      `2` = c(0, 1),
      check.names = FALSE
    )
  )
  
  expect_error(total_animals_present(no_time_data, bins_feed = 1:3, bins_wat = 101:102), 
               "Each data frame must have a 'Time' column")
})

test_that("total_animals_present errors for insufficient columns", {
  insufficient_cols_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00")
    )
  )
  
  expect_error(total_animals_present(insufficient_cols_data, bins_feed = 1:3, bins_wat = 101:102), 
               "Each data frame must have at least one animal column")
})

test_that("total_animals_present works with different bin ranges", {
  toy_data <- create_toy_synch_data()
  
  # Test with large bin ranges
  result1 <- total_animals_present(toy_data, bins_feed = 1:10, bins_wat = 101:110)
  expect_equal(result1[[1]]$empty_bin_num[1], 19)  # 20 total bins - 1 occupied
  
  # Test with minimal bins
  result2 <- total_animals_present(toy_data, bins_feed = 1, bins_wat = 101)
  expect_equal(result2[[1]]$empty_bin_num[1], 1)  # 2 total bins - 1 occupied
})

test_that("total_animals_present handles edge case with all animals present", {
  all_animals_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 1,
      `2` = 1,
      check.names = FALSE
    )
  )
  
  # This should error because bins_wat is empty
  expect_error(total_animals_present(all_animals_data, bins_feed = 1:2, bins_wat = integer(0)), 
               "bins_feed and bins_wat cannot be empty")
  
  # Test with proper bins
  result <- total_animals_present(all_animals_data, bins_feed = 1, bins_wat = 101)
  expect_equal(result[[1]]$total_animal_num, 2)
  expect_equal(result[[1]]$empty_bin_num, 0)  # All bins occupied
})

# Tests for delete_inactive_time function
test_that("delete_inactive_time works with normal input", {
  # Create animal data with some inactive periods
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:04"), by = "sec"),
      `1` = c(1, 0, 0, 1, 0),
      `2` = c(0, 0, 1, 0, 0),
      total_animal_num = c(1, 0, 1, 1, 0),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:04"), by = "sec"),
      `1` = c(1, 0, 0, 2, 0),
      `2` = c(0, 0, 3, 0, 0),
      check.names = FALSE
    )
  )
  
  result <- delete_inactive_time(animal_data, bin_data)
  
  # Check structure
  expect_equal(length(result), 2)
  expect_equal(length(result[[1]]), 1)  # One day
  expect_equal(length(result[[2]]), 1)  # One day
  
  # Check that inactive periods are removed (should have 3 rows instead of 5)
  expect_equal(nrow(result[[1]][[1]]), 3)
  expect_equal(nrow(result[[2]][[1]]), 3)
  
  # Check that the right rows are kept (those with total_animal_num > 0)
  expect_equal(result[[1]][[1]]$total_animal_num, c(1, 1, 1))
})

test_that("delete_inactive_time preserves list names", {
  animal_data <- list(
    custom_day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 1,
      total_animal_num = 1,
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    custom_day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 1,
      check.names = FALSE
    )
  )
  
  result <- delete_inactive_time(animal_data, bin_data)
  
  expect_equal(names(result[[1]]), "custom_day1")
  expect_equal(names(result[[2]]), "custom_day1")
})

test_that("delete_inactive_time handles all inactive periods", {
  # Create data where no animals are ever active
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(0, 0, 0),
      `2` = c(0, 0, 0),
      total_animal_num = c(0, 0, 0),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(0, 0, 0),
      `2` = c(0, 0, 0),
      check.names = FALSE
    )
  )
  
  result <- delete_inactive_time(animal_data, bin_data)
  
  # Should return empty data frames with same structure
  expect_equal(nrow(result[[1]][[1]]), 0)
  expect_equal(nrow(result[[2]][[1]]), 0)
  expect_equal(ncol(result[[1]][[1]]), 4)  # Same number of columns
  expect_equal(ncol(result[[2]][[1]]), 3)
})

test_that("delete_inactive_time handles all active periods", {
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 0, 1),
      `2` = c(0, 1, 1),
      total_animal_num = c(1, 1, 2),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 0, 2),
      `2` = c(0, 3, 4),
      check.names = FALSE
    )
  )
  
  result <- delete_inactive_time(animal_data, bin_data)
  
  # Should keep all rows since all periods are active
  expect_equal(nrow(result[[1]][[1]]), 3)
  expect_equal(nrow(result[[2]][[1]]), 3)
  expect_equal(result[[1]][[1]]$total_animal_num, c(1, 1, 2))
})

test_that("delete_inactive_time handles multiple days", {
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(1, 0),
      total_animal_num = c(1, 0),
      check.names = FALSE
    ),
    day2 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-02 10:00:00"), 
                 lubridate::ymd_hms("2023-01-02 10:00:01"), by = "sec"),
      `1` = c(0, 1),
      total_animal_num = c(0, 1),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(1, 0),
      check.names = FALSE
    ),
    day2 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-02 10:00:00"), 
                 lubridate::ymd_hms("2023-01-02 10:00:01"), by = "sec"),
      `1` = c(0, 2),
      check.names = FALSE
    )
  )
  
  result <- delete_inactive_time(animal_data, bin_data)
  
  expect_equal(length(result[[1]]), 2)
  expect_equal(length(result[[2]]), 2)
  expect_equal(nrow(result[[1]][[1]]), 1)  # day1: only first row kept
  expect_equal(nrow(result[[1]][[2]]), 1)  # day2: only second row kept
})

# Error handling tests for delete_inactive_time
test_that("delete_inactive_time errors for NULL or empty input", {
  animal_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                       `1` = 1, total_animal_num = 1, check.names = FALSE))
  bin_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                    `1` = 1, check.names = FALSE))
  
  expect_error(delete_inactive_time(NULL, bin_data), 
               "Input feed_drink_synch_master_animal cannot be NULL or empty")
  
  expect_error(delete_inactive_time(animal_data, NULL), 
               "Input feed_drink_synch_master_bin cannot be NULL or empty")
  
  expect_error(delete_inactive_time(list(), bin_data), 
               "Input feed_drink_synch_master_animal cannot be NULL or empty")
  
  expect_error(delete_inactive_time(animal_data, list()), 
               "Input feed_drink_synch_master_bin cannot be NULL or empty")
})

test_that("delete_inactive_time errors for mismatched lengths", {
  animal_data <- list(
    day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                     `1` = 1, total_animal_num = 1, check.names = FALSE),
    day2 = data.frame(Time = lubridate::ymd_hms("2023-01-02 10:00:00"), 
                     `1` = 1, total_animal_num = 1, check.names = FALSE)
  )
  
  bin_data <- list(
    day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                     `1` = 1, check.names = FALSE)
  )
  
  expect_error(delete_inactive_time(animal_data, bin_data), 
               "feed_drink_synch_master_animal and feed_drink_synch_master_bin must have the same length")
})

test_that("delete_inactive_time errors for invalid data frame structure", {
  animal_data <- list(day1 = "not a data frame")
  bin_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                    `1` = 1, check.names = FALSE))
  
  expect_error(delete_inactive_time(animal_data, bin_data), 
               "Each element must be a data frame")
})

test_that("delete_inactive_time errors for missing total_animal_num column", {
  animal_data <- list(
    day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                     `1` = 1, check.names = FALSE)
  )
  
  bin_data <- list(
    day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                     `1` = 1, check.names = FALSE)
  )
  
  expect_error(delete_inactive_time(animal_data, bin_data), 
               "Animal data frames must have a 'total_animal_num' column \\(run total_animals_present first\\)")
})

test_that("delete_inactive_time errors for mismatched row counts", {
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 0, 1),
      total_animal_num = c(1, 0, 1),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(1, 0),
      check.names = FALSE
    )
  )
  
  expect_error(delete_inactive_time(animal_data, bin_data), 
               "Corresponding animal and bin data frames must have the same number of rows")
})

# Tests for add_date function
test_that("add_date works with normal input", {
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 0, 1),
      `2` = c(0, 1, 1),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 0, 2),
      `2` = c(0, 3, 4),
      check.names = FALSE
    )
  )
  
  result <- add_date(animal_data, bin_data)
  
  # Check structure
  expect_equal(length(result), 2)
  expect_equal(length(result[[1]]), 1)
  expect_equal(length(result[[2]]), 1)
  
  # Check that date column was added
  expect_true("date" %in% names(result[[1]][[1]]))
  expect_true("date" %in% names(result[[2]][[1]]))
  
  # Check date values
  expected_date <- lubridate::date(lubridate::ymd_hms("2023-01-01 10:00:00"))
  expect_equal(result[[1]][[1]]$date, rep(expected_date, 3))
  expect_equal(result[[2]][[1]]$date, rep(expected_date, 3))
  
  # Check that list names were updated to date
  expect_equal(names(result[[1]]), "2023-01-01")
  expect_equal(names(result[[2]]), "2023-01-01")
})

test_that("add_date handles multiple days", {
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(1, 0),
      check.names = FALSE
    ),
    day2 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-02 15:30:00"), 
                 lubridate::ymd_hms("2023-01-02 15:30:01"), by = "sec"),
      `1` = c(0, 1),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(1, 0),
      check.names = FALSE
    ),
    day2 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-02 15:30:00"), 
                 lubridate::ymd_hms("2023-01-02 15:30:01"), by = "sec"),
      `1` = c(0, 2),
      check.names = FALSE
    )
  )
  
  result <- add_date(animal_data, bin_data)
  
  # Check that both days are processed
  expect_equal(length(result[[1]]), 2)
  expect_equal(length(result[[2]]), 2)
  
  # Check dates for each day
  expect_equal(result[[1]][[1]]$date, rep(lubridate::ymd("2023-01-01"), 2))
  expect_equal(result[[1]][[2]]$date, rep(lubridate::ymd("2023-01-02"), 2))
  
  # Check list names
  expect_equal(names(result[[1]]), c("2023-01-01", "2023-01-02"))
  expect_equal(names(result[[2]]), c("2023-01-01", "2023-01-02"))
})

test_that("add_date handles empty data frames", {
  animal_data <- list(
    day1 = data.frame(
      Time = as.POSIXct(character(0)),
      `1` = numeric(0),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = as.POSIXct(character(0)),
      `1` = numeric(0),
      check.names = FALSE
    )
  )
  
  result <- add_date(animal_data, bin_data)
  
  # Check that date column is added even for empty data frames
  expect_true("date" %in% names(result[[1]][[1]]))
  expect_true("date" %in% names(result[[2]][[1]]))
  
  # Check that empty data frames have 0 rows
  expect_equal(nrow(result[[1]][[1]]), 0)
  expect_equal(nrow(result[[2]][[1]]), 0)
  
  # Check that names are set for empty data frames
  expect_equal(names(result[[1]]), "empty_1")
  expect_equal(names(result[[2]]), "empty_1")
})

test_that("add_date handles single time point", {
  animal_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-03-15 14:25:30"),
      `1` = 1,
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-03-15 14:25:30"),
      `1` = 5,
      check.names = FALSE
    )
  )
  
  result <- add_date(animal_data, bin_data)
  
  expect_equal(result[[1]][[1]]$date, lubridate::ymd("2023-03-15"))
  expect_equal(result[[2]][[1]]$date, lubridate::ymd("2023-03-15"))
  expect_equal(names(result[[1]]), "2023-03-15")
  expect_equal(names(result[[2]]), "2023-03-15")
})

test_that("add_date handles data spanning multiple dates within one day entry", {
  # This tests edge case where one list element spans midnight
  animal_data <- list(
    day1 = data.frame(
      Time = c(lubridate::ymd_hms("2023-01-01 23:59:59"), 
               lubridate::ymd_hms("2023-01-02 00:00:01")),
      `1` = c(1, 0),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = c(lubridate::ymd_hms("2023-01-01 23:59:59"), 
               lubridate::ymd_hms("2023-01-02 00:00:01")),
      `1` = c(1, 0),
      check.names = FALSE
    )
  )
  
  result <- add_date(animal_data, bin_data)
  
  # Should have dates for both days
  expected_dates <- c(lubridate::ymd("2023-01-01"), lubridate::ymd("2023-01-02"))
  expect_equal(result[[1]][[1]]$date, expected_dates)
  
  # List name should be based on first date
  expect_equal(names(result[[1]]), "2023-01-01")
})

# Error handling tests for add_date
test_that("add_date errors for NULL or empty input", {
  animal_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                       `1` = 1, check.names = FALSE))
  bin_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                    `1` = 1, check.names = FALSE))
  
  expect_error(add_date(NULL, bin_data), 
               "Input feed_drink_synch_master_animal cannot be NULL or empty")
  
  expect_error(add_date(animal_data, NULL), 
               "Input feed_drink_synch_master_bin cannot be NULL or empty")
  
  expect_error(add_date(list(), bin_data), 
               "Input feed_drink_synch_master_animal cannot be NULL or empty")
  
  expect_error(add_date(animal_data, list()), 
               "Input feed_drink_synch_master_bin cannot be NULL or empty")
})

test_that("add_date errors for mismatched lengths", {
  animal_data <- list(
    day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                     `1` = 1, check.names = FALSE),
    day2 = data.frame(Time = lubridate::ymd_hms("2023-01-02 10:00:00"), 
                     `1` = 1, check.names = FALSE)
  )
  
  bin_data <- list(
    day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                     `1` = 1, check.names = FALSE)
  )
  
  expect_error(add_date(animal_data, bin_data), 
               "feed_drink_synch_master_animal and feed_drink_synch_master_bin must have the same length")
})

test_that("add_date errors for invalid data frame structure", {
  animal_data <- list(day1 = "not a data frame")
  bin_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                    `1` = 1, check.names = FALSE))
  
  expect_error(add_date(animal_data, bin_data), 
               "Each element must be a data frame")
})

test_that("add_date errors for missing Time column", {
  animal_data <- list(
    day1 = data.frame(`1` = 1, check.names = FALSE)
  )
  
  bin_data <- list(
    day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                     `1` = 1, check.names = FALSE)
  )
  
  expect_error(add_date(animal_data, bin_data), 
               "Both animal and bin data frames must have a 'Time' column")
})

test_that("add_date errors for mismatched row counts", {
  animal_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 0, 1),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(1, 0),
      check.names = FALSE
    )
  )
  
  expect_error(add_date(animal_data, bin_data), 
               "Corresponding animal and bin data frames must have the same number of rows")
})

test_that("add_date errors for non-POSIXct Time column", {
  animal_data <- list(
    day1 = data.frame(
      Time = c("2023-01-01 10:00:00", "2023-01-01 10:00:01"),
      `1` = c(1, 0),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    day1 = data.frame(
      Time = c("2023-01-01 10:00:00", "2023-01-01 10:00:01"),
      `1` = c(1, 0),
      check.names = FALSE
    )
  )
  
  expect_error(add_date(animal_data, bin_data), 
               "Time columns must be POSIXct")
})

# Tests for bin_update function
test_that("bin_update works with normal input", {
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:03"), by = "sec"),
      `1` = c(1, 2, 0, 1),      # feed bins
      `2` = c(0, 0, 101, 102),  # water bins
      date = rep(lubridate::ymd("2023-01-01"), 4),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1:6, bins_wat = 101:105)
  
  # Check structure
  expect_equal(length(result), 1)
  expect_equal(names(result), "2023-01-01")
  
  # Check feed bin updates (bins 1-6 should get +200 offset)
  expect_equal(result[[1]]$`1`, c(201, 202, 0, 201))  # 1->201, 2->202
  
  # Check water bin updates (101->207, 102->208)
  expect_equal(result[[1]]$`2`, c(0, 0, 207, 208))
})

test_that("bin_update handles different feed bin value offsets", {
  bin_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 5,   # value <= 6: +200
      `2` = 10,  # value <= 18: +202  
      `3` = 25,  # value > 18: +204
      date = lubridate::ymd("2023-01-01"),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = c(5, 10, 25), bins_wat = 101)
  
  # Check different offsets based on bin VALUE, not position
  expect_equal(result[[1]]$`1`, 205)  # 5 + 200 (value <= 6)
  expect_equal(result[[1]]$`2`, 212)  # 10 + 202 (value <= 18)
  expect_equal(result[[1]]$`3`, 229)  # 25 + 204 (value > 18)
})

test_that("bin_update handles water bin hard-coded mapping", {
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:04"), by = "sec"),
      `1` = c(101, 102, 103, 104, 105),
      date = rep(lubridate::ymd("2023-01-01"), 5),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1, bins_wat = 101:105)
  
  # Water bins should map: 101->207, 102->208, 103->221, 104->222, 105->235 (hard-coded)
  expected_mapping <- c(207, 208, 221, 222, 235)
  expect_equal(result[[1]]$`1`, expected_mapping)
})

test_that("bin_update preserves non-matching bin numbers", {
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(1, 999, 101),  # 999 should remain unchanged
      date = rep(lubridate::ymd("2023-01-01"), 3),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1:2, bins_wat = 101:102)
  
  # 1->201, 999 unchanged, 101->207
  expect_equal(result[[1]]$`1`, c(201, 999, 207))
})

test_that("bin_update handles zero values correctly", {
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec"),
      `1` = c(0, 1, 0),
      `2` = c(101, 0, 0),
      date = rep(lubridate::ymd("2023-01-01"), 3),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1, bins_wat = 101)
  
  # Zeros should remain unchanged, only 1->201 and 101->207
  expect_equal(result[[1]]$`1`, c(0, 201, 0))
  expect_equal(result[[1]]$`2`, c(207, 0, 0))
})

test_that("bin_update handles multiple days", {
  bin_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 1,
      date = lubridate::ymd("2023-01-01"),
      check.names = FALSE
    ),
    day2 = data.frame(
      Time = lubridate::ymd_hms("2023-01-02 10:00:00"),
      `1` = 101,
      date = lubridate::ymd("2023-01-02"),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1, bins_wat = 101)
  
  expect_equal(length(result), 2)
  expect_equal(names(result), c("2023-01-01", "2023-01-02"))
  expect_equal(result[[1]]$`1`, 201)  # feed bin
  expect_equal(result[[2]]$`1`, 207)  # water bin
})

test_that("bin_update preserves Time and date columns", {
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:01"), by = "sec"),
      `1` = c(1, 101),
      date = rep(lubridate::ymd("2023-01-01"), 2),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1, bins_wat = 101)
  
  # Time and date columns should be unchanged
  expect_equal(result[[1]]$Time, bin_data[[1]]$Time)
  expect_equal(result[[1]]$date, bin_data[[1]]$date)
  
  # Only bin numbers should change
  expect_equal(result[[1]]$`1`, c(201, 207))
})

test_that("bin_update handles empty data frames", {
  bin_data <- list(
    day1 = data.frame(
      Time = as.POSIXct(character(0)),
      `1` = numeric(0),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1:5, bins_wat = 101:105)
  
  expect_equal(nrow(result[[1]]), 0)
  expect_equal(ncol(result[[1]]), 2)  # Time and `1` columns
})

test_that("bin_update handles data without date column", {
  bin_data <- list(
    custom_day = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 1,
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = 1, bins_wat = 101)
  
  # Should preserve original name since no date column
  expect_equal(names(result), "custom_day")
  expect_equal(result[[1]]$`1`, 201)
})

# Error handling tests for bin_update
test_that("bin_update errors for NULL or empty input", {
  expect_error(bin_update(NULL, bins_feed = 1:3, bins_wat = 101:102), 
               "Input feed_drink_synch_master_bin cannot be NULL or empty")
  
  expect_error(bin_update(list(), bins_feed = 1:3, bins_wat = 101:102), 
               "Input feed_drink_synch_master_bin cannot be NULL or empty")
})

test_that("bin_update errors for non-numeric bins", {
  bin_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                    `1` = 1, check.names = FALSE))
  
  expect_error(bin_update(bin_data, bins_feed = c("a", "b"), bins_wat = 101:102), 
               "bins_feed and bins_wat must be numeric")
  
  expect_error(bin_update(bin_data, bins_feed = 1:3, bins_wat = c("x", "y")), 
               "bins_feed and bins_wat must be numeric")
})

test_that("bin_update errors for empty bins", {
  bin_data <- list(day1 = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"), 
                                    `1` = 1, check.names = FALSE))
  
  expect_error(bin_update(bin_data, bins_feed = numeric(0), bins_wat = 101:102), 
               "bins_feed and bins_wat cannot be empty")
  
  expect_error(bin_update(bin_data, bins_feed = 1:3, bins_wat = numeric(0)), 
               "bins_feed and bins_wat cannot be empty")
})

test_that("bin_update errors for invalid data frame structure", {
  bin_data <- list(day1 = "not a data frame")
  
  expect_error(bin_update(bin_data, bins_feed = 1:3, bins_wat = 101:102), 
               "Each element must be a data frame")
})

test_that("bin_update errors for missing Time column", {
  bin_data <- list(
    day1 = data.frame(`1` = 1, check.names = FALSE)
  )
  
  expect_error(bin_update(bin_data, bins_feed = 1:3, bins_wat = 101:102), 
               "Each data frame must have a 'Time' column")
})

test_that("bin_update works with edge case bin ranges", {
  bin_data <- list(
    day1 = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      `1` = 30,  # This would be value 30 > 18, so +204
      date = lubridate::ymd("2023-01-01"),
      check.names = FALSE
    )
  )
  
  # Test with 30 feed bins to check value logic
  result <- bin_update(bin_data, bins_feed = 1:30, bins_wat = 101)
  
  # Bin 30 has value 30 > 18, so should get +204 offset
  expect_equal(result[[1]]$`1`, 234)  # 30 + 204
})

test_that("bin_update handles complex mixed bin scenario", {
  bin_data <- list(
    day1 = data.frame(
      Time = seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                 lubridate::ymd_hms("2023-01-01 10:00:05"), by = "sec"),
      `1` = c(3, 0, 10, 101, 999, 20),  # mix of feed, water, and unmapped
      date = rep(lubridate::ymd("2023-01-01"), 6),
      check.names = FALSE
    )
  )
  
  result <- bin_update(bin_data, bins_feed = c(3, 10, 20), bins_wat = 101)
  
  # Expected: 3->203 (value 3 <= 6, +200), 0->0, 10->212 (value 10 <= 18, +202), 101->207, 999->999, 20->224 (value 20 > 18, +204)
  expect_equal(result[[1]]$`1`, c(203, 0, 212, 207, 999, 224))
})

# Tests for feed_drink_matrix_process function
test_that("feed_drink_matrix_process works with normal input", {
  toy_data <- list(
    day1 = data.frame(
      cow = c(1, 1, 2, 2),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:03", 
                                   "2023-01-01 10:00:00", "2023-01-01 10:00:04")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:01", "2023-01-01 10:00:04", 
                                 "2023-01-01 10:00:02", "2023-01-01 10:00:05")),
      bin = c(1, 101, 2, 102)
    )
  )
  
  result <- feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105)
  
  # Check structure
  expect_equal(length(result), 2)
  expect_true(is.list(result[[1]]))  # animal matrices
  expect_true(is.list(result[[2]]))  # bin matrices
  
  # Check names are date-based
  expect_equal(names(result[[1]]), "2023-01-01")
  expect_equal(names(result[[2]]), "2023-01-01")
  
  # Check that animal data has required columns
  animal_df <- result[[1]][[1]]
  expect_true("total_animal_num" %in% names(animal_df))
  expect_true("total_bin_occupied" %in% names(animal_df))
  expect_true("empty_bin_num" %in% names(animal_df))
  expect_true("date" %in% names(animal_df))
  
  # Check that bin data has updated bin numbers and date
  bin_df <- result[[2]][[1]]
  expect_true("date" %in% names(bin_df))
  
  # Verify bin numbers were updated (feed bins: 1->201, 2->202; water bins: 101->207, 102->208)
  unique_bins <- unique(unlist(bin_df[, !names(bin_df) %in% c("Time", "date")]))
  unique_bins <- unique_bins[unique_bins != 0]  # Remove zeros
  expect_true(all(unique_bins %in% c(201, 202, 207, 208)))
})

test_that("feed_drink_matrix_process handles multiple days", {
  toy_data <- list(
    day1 = data.frame(
      cow = 1,
      start = lubridate::ymd_hms("2023-01-01 10:00:00"),
      end = lubridate::ymd_hms("2023-01-01 10:00:01"),
      bin = 1
    ),
    day2 = data.frame(
      cow = 1,
      start = lubridate::ymd_hms("2023-01-02 10:00:00"),
      end = lubridate::ymd_hms("2023-01-02 10:00:01"),
      bin = 2
    )
  )
  
  result <- feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105)
  
  # Should have separate entries for each day
  expect_equal(length(result[[1]]), 2)
  expect_equal(length(result[[2]]), 2)
  expect_equal(names(result[[1]]), c("2023-01-01", "2023-01-02"))
  expect_equal(names(result[[2]]), c("2023-01-01", "2023-01-02"))
})

test_that("feed_drink_matrix_process removes inactive time", {
  toy_data <- list(
    day1 = data.frame(
      cow = c(1, 1),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:03")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:03")),
      bin = c(1, 2)
    )
  )
  
  result <- feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105)
  
  # Should have 2 rows (one for each feeding event)
  expect_equal(nrow(result[[1]][[1]]), 2)
  expect_equal(nrow(result[[2]][[1]]), 2)
  
  # All remaining rows should have total_animal_num > 0
  expect_true(all(result[[1]][[1]]$total_animal_num > 0))
})

test_that("feed_drink_matrix_process handles single animal", {
  toy_data <- list(
    day1 = data.frame(
      cow = c(1, 1),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      bin = c(1, 101)
    )
  )
  
  result <- feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105)
  
  # Check structure
  expect_equal(length(result), 2)
  expect_equal(ncol(result[[1]][[1]]), 6)  # Time + cow1 + total_animal_num + total_bin_occupied + empty_bin_num + date
  
  # Check that total_animal_num is correct (1 when active, removed when 0)
  expect_true(all(result[[1]][[1]]$total_animal_num == 1))
})

test_that("feed_drink_matrix_process integrates all steps correctly", {
  toy_data <- list(
    day1 = data.frame(
      cow = c(1, 1, 2, 2, 1),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:03", 
                                   "2023-01-01 10:00:01", "2023-01-01 10:00:02",
                                   "2023-01-01 10:00:04")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:01", "2023-01-01 10:00:03", 
                                 "2023-01-01 10:00:02", "2023-01-01 10:00:02",
                                 "2023-01-01 10:00:04")),
      bin = c(1, 101, 3, 3, 2)
    )
  )
  
  result <- feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105)
  
  animal_df <- result[[1]][[1]]
  bin_df <- result[[2]][[1]]
  
  # Check that we have some data (exact number depends on overlaps)
  expect_true(nrow(animal_df) > 0)
  expect_true(nrow(bin_df) > 0)
  
  # Check that animal counts are calculated
  expect_true("total_animal_num" %in% names(animal_df))
  expect_true("total_bin_occupied" %in% names(animal_df))
  
  # Check bin updates: 1->201, 2->202, 3->203, 101->207
  unique_bins <- unique(unlist(bin_df[, !names(bin_df) %in% c("Time", "date")]))
  unique_bins <- unique_bins[unique_bins != 0]
  expect_true(all(unique_bins %in% c(201, 202, 203, 207)))
  
  # Check date column exists
  expect_true("date" %in% names(animal_df))
  expect_true("date" %in% names(bin_df))
  expect_true(all(animal_df$date == lubridate::ymd("2023-01-01")))
})

# Error handling tests for feed_drink_matrix_process
test_that("feed_drink_matrix_process errors for NULL or empty input", {
  expect_error(feed_drink_matrix_process(NULL), 
               "Input all_comb cannot be NULL or empty")
  
  expect_error(feed_drink_matrix_process(list()), 
               "Input all_comb cannot be NULL or empty")
})

test_that("feed_drink_matrix_process errors for non-list input", {
  expect_error(feed_drink_matrix_process("not a list"), 
               "all_comb must be a list")
  
  expect_error(feed_drink_matrix_process(data.frame(x = 1)), 
               "Each element in all_comb must be a data frame")
})

test_that("feed_drink_matrix_process errors for invalid data frame structure", {
  toy_data <- list(
    animal1 = "not a data frame"
  )
  
  expect_error(feed_drink_matrix_process(toy_data), 
               "Each element in all_comb must be a data frame")
})

test_that("feed_drink_matrix_process errors for missing required columns", {
  toy_data <- list(
    day1 = data.frame(
      cow = 1,
      start = lubridate::ymd_hms("2023-01-01 10:00:00")
      # Missing end and bin columns
    )
  )
  
  expect_error(feed_drink_matrix_process(toy_data), 
               "Each data frame must have columns:")
})

test_that("feed_drink_matrix_process errors for empty data frames", {
  # Create empty data frame with required structure but no rows
  toy_data <- list(
    day1 = data.frame(
      cow = numeric(0),
      start = lubridate::ymd_hms(character(0)),
      end = lubridate::ymd_hms(character(0)),
      bin = numeric(0)
    )
  )
  
  # Empty data frames should cause an error since they can't create time sequences
  expect_error(feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105),
               "cur_data is empty")
})

test_that("feed_drink_matrix_process preserves animal names", {
  toy_data <- list(
    day1 = data.frame(
      cow = c(123, 456),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:00")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:00")),
      bin = c(1, 2)
    )
  )
  
  result <- feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105)
  
  # Check that animal names are preserved in column names
  animal_cols <- setdiff(names(result[[1]][[1]]), c("Time", "total_animal_num", "total_bin_occupied", "empty_bin_num", "date"))
  expect_true("123" %in% animal_cols)
  expect_true("456" %in% animal_cols)
}) 