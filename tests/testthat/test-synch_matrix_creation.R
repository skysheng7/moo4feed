# Tests for synch_matrix_creation.R
# Testing internal functions for creating time sequences and empty matrices

# ============================================================================ #
# create_time_sequence() tests
# ============================================================================ #

test_that("create_time_sequence works with default (sec) resolution", {
  toy_data <- data.frame(
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:00:05")
  )
  
  result <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
  
  expect_true(lubridate::is.POSIXct(result))
  expect_equal(length(result), 6)  # 0,1,2,3,4,5 seconds
  expect_equal(as.numeric(difftime(result[2], result[1], units = "secs")), 1)
})

test_that("create_time_sequence works with minute resolution", {
  toy_data <- data.frame(
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:05:00")
  )
  
  result <- create_time_sequence(toy_data, start_col = "start", 
                                 end_col = "end", resolution = "min")
  
  expect_equal(length(result), 6)  # 0,1,2,3,4,5 minutes
  expect_equal(as.numeric(difftime(result[2], result[1], units = "mins")), 1)
})

test_that("create_time_sequence handles case-insensitive resolution", {
  toy_data <- data.frame(
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:00:02")
  )
  
  result1 <- create_time_sequence(toy_data, start_col = "start", 
                                  end_col = "end", resolution = "SEC")
  result2 <- create_time_sequence(toy_data, start_col = "start", 
                                  end_col = "end", resolution = " Min ")
  
  expect_equal(length(result1), 3)
  expect_equal(length(result2), 1)
})

test_that("create_time_sequence handles multiple rows correctly", {
  toy_data <- data.frame(
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:03")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:05"))
  )
  
  result <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
  
  expect_equal(length(result), 6)  # From 10:00:00 to 10:00:05
  expect_equal(min(result), lubridate::ymd_hms("2023-01-01 10:00:00"))
  expect_equal(max(result), lubridate::ymd_hms("2023-01-01 10:00:05"))
})

test_that("create_time_sequence errors on invalid inputs", {
  # Not a data frame
  expect_error(
    create_time_sequence(list(), start_col = "start", end_col = "end"),
    "`cur_data` must be a data frame"
  )
  
  # Missing columns
  toy_data <- data.frame(x = 1)
  expect_error(
    create_time_sequence(toy_data, start_col = "start", end_col = "end"),
    "Missing required columns"
  )
  
  # Non-POSIXct columns
  toy_data <- data.frame(start = "2023-01-01", end = "2023-01-02")
  expect_error(
    create_time_sequence(toy_data, start_col = "start", end_col = "end"),
    "Start and end columns must be POSIXct"
  )
  
  # Empty data frame
  toy_data <- data.frame(
    start = lubridate::ymd_hms(character(0)),
    end = lubridate::ymd_hms(character(0))
  )
  expect_error(
    create_time_sequence(toy_data, start_col = "start", end_col = "end"),
    "`cur_data` is empty"
  )
})

test_that("create_time_sequence handles NA and infinite values", {
  # NA values mixed with valid values are handled by na.rm
  toy_data <- data.frame(
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", NA)),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:05", "2023-01-01 10:00:10"))
  )
  # With na.rm=TRUE, this should work fine
  result <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
  expect_true(lubridate::is.POSIXct(result))
  
  # All NA values lead to Inf/-Inf which triggers error
  toy_data <- data.frame(
    start = lubridate::ymd_hms(NA),
    end = lubridate::ymd_hms(NA)
  )
  expect_error(
    suppressWarnings(
      create_time_sequence(toy_data, start_col = "start", end_col = "end")
    ),
    "Start/End times cannot be NA or infinite"
  )
})

test_that("create_time_sequence validates end time after start time", {
  toy_data <- data.frame(
    start = lubridate::ymd_hms("2023-01-01 10:00:05"),
    end = lubridate::ymd_hms("2023-01-01 10:00:00")
  )
  expect_error(
    create_time_sequence(toy_data, start_col = "start", end_col = "end"),
    "End time cannot be earlier than start time"
  )
})

test_that("create_time_sequence works with same start and end time", {
  toy_data <- data.frame(
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:00:00")
  )
  
  result <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
  expect_equal(length(result), 1)
})

# ============================================================================ #
# prepare_time_animal_matrix() tests
# ============================================================================ #

test_that("prepare_time_animal_matrix creates correct matrix structure", {
  toy_data <- data.frame(
    cow = c(1, 2, 3),
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:00:02")
  )
  
  time_seq <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
  result <- prepare_time_animal_matrix(toy_data, time_seq, id_col = "cow")
  
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 4)  # Time + 3 animals
  expect_equal(nrow(result), length(time_seq))
  expect_equal(colnames(result), c("Time", "1", "2", "3"))
  expect_true(all(result[, -1] == 0))  # All initialized to 0
  expect_equal(result$Time, time_seq)
})

test_that("prepare_time_animal_matrix sorts animal IDs", {
  toy_data <- data.frame(
    cow = c(5, 2, 8, 1),
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:00:02")
  )
  
  time_seq <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
  result <- prepare_time_animal_matrix(toy_data, time_seq, id_col = "cow")
  
  expect_equal(colnames(result)[-1], c("1", "2", "5", "8"))
})

test_that("prepare_time_animal_matrix handles single animal", {
  toy_data <- data.frame(
    cow = 1,
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:00:02")
  )
  
  time_seq <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
  result <- prepare_time_animal_matrix(toy_data, time_seq, id_col = "cow")
  
  expect_equal(ncol(result), 2)  # Time + 1 animal
})

test_that("prepare_time_animal_matrix errors on invalid inputs", {
  # Not a data frame
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                  lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
  expect_error(
    prepare_time_animal_matrix(list(), time_seq, id_col = "cow"),
    "`cur_data` must be a data frame"
  )
  
  # Missing id column
  toy_data <- data.frame(x = 1)
  expect_error(
    prepare_time_animal_matrix(toy_data, time_seq, id_col = "cow"),
    "Missing required column: cow"
  )
  
  # Non-POSIXct time sequence
  toy_data <- data.frame(cow = 1)
  expect_error(
    prepare_time_animal_matrix(toy_data, c(1, 2, 3), id_col = "cow"),
    "`dateTime_seq` must be date time data type: POSIXct"
  )
  
  # Empty time sequence
  expect_error(
    prepare_time_animal_matrix(toy_data, lubridate::ymd_hms(character(0)), id_col = "cow"),
    "`dateTime_seq` cannot be empty"
  )
  
  # No animals in data
  toy_data <- data.frame(cow = numeric(0))
  expect_error(
    prepare_time_animal_matrix(toy_data, time_seq, id_col = "cow"),
    "No animals found in `cur_data`"
  )
})

# ============================================================================ #
# prepare_time_feed_matrix() tests
# ============================================================================ #

test_that("prepare_time_feed_matrix creates correct matrix structure", {
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                  lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
  bins_feed <- 1:3
  
  result <- prepare_time_feed_matrix(time_seq, bins_feed = bins_feed)
  
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 4)  # Time + 3 bins
  expect_equal(nrow(result), length(time_seq))
  expect_equal(colnames(result), c("Time", "1", "2", "3"))
  expect_true(all(is.na(result[, -1])))  # All initialized to NA
  expect_equal(result$Time, time_seq)
})

test_that("prepare_time_feed_matrix handles non-sequential bins", {
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                  lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
  bins_feed <- c(1, 5, 10)
  
  result <- prepare_time_feed_matrix(time_seq, bins_feed = bins_feed)
  
  # Should create bins from min to max with step 1
  expect_equal(ncol(result), 11)  # Time + bins 1 through 10
  expect_equal(colnames(result)[2:11], as.character(1:10))
})

test_that("prepare_time_feed_matrix handles single bin", {
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                  lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
  
  result <- prepare_time_feed_matrix(time_seq, bins_feed = 5)
  
  expect_equal(ncol(result), 2)  # Time + 1 bin
})

test_that("prepare_time_feed_matrix errors on invalid inputs", {
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                  lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
  
  # Non-POSIXct time sequence
  expect_error(
    prepare_time_feed_matrix(c(1, 2, 3), bins_feed = 1:3),
    "`dateTime_seq` must be POSIXct"
  )
  
  # Empty time sequence
  expect_error(
    prepare_time_feed_matrix(lubridate::ymd_hms(character(0)), bins_feed = 1:3),
    "`dateTime_seq` cannot be empty"
  )
  
  # Non-numeric bins
  expect_error(
    prepare_time_feed_matrix(time_seq, bins_feed = c("a", "b")),
    "`bins_feed` must be numeric"
  )
  
  # Empty bins
  expect_error(
    prepare_time_feed_matrix(time_seq, bins_feed = numeric(0)),
    "`bins_feed` cannot be empty"
  )
})

# ============================================================================ #
# prepare_time_bin_matrix() tests
# ============================================================================ #

test_that("prepare_time_bin_matrix returns input unchanged", {
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                  lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
  
  input_matrix <- data.frame(
    Time = time_seq,
    animal1 = c(0, 1, 0),
    animal2 = c(1, 0, 1)
  )
  
  result <- prepare_time_bin_matrix(input_matrix)
  
  expect_identical(result, input_matrix)
})

test_that("prepare_time_bin_matrix validates input", {
  # Not a data frame
  expect_error(
    prepare_time_bin_matrix(list()),
    "`animal_time_matrix` must be a data frame"
  )
  
  # Missing Time column
  input_matrix <- data.frame(animal1 = c(0, 1, 0))
  expect_error(
    prepare_time_bin_matrix(input_matrix),
    "`animal_time_matrix` must have a 'Time' column"
  )
  
  # No animal columns
  input_matrix <- data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00"))
  expect_error(
    prepare_time_bin_matrix(input_matrix),
    "`animal_time_matrix` must have at least one animal column"
  )
})

test_that("prepare_time_bin_matrix works with multiple columns", {
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
                  lubridate::ymd_hms("2023-01-01 10:00:05"), by = "sec")
  
  input_matrix <- data.frame(
    Time = time_seq,
    animal1 = rep(0, 6),
    animal2 = rep(1, 6),
    animal3 = rep(0, 6)
  )
  
  result <- prepare_time_bin_matrix(input_matrix)
  
  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 4)
  expect_identical(result, input_matrix)
})
