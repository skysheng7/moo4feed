# Test synch_pair_analysis ----------------------------------------------------

test_that("synch_pair_analysis works with single day input", {
  # Create toy data
  toy_data <- data.frame(
    animal = c(1, 2, 3, 1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:01",
      "2023-01-01 10:00:05", "2023-01-01 10:00:10",
      "2023-01-01 10:00:11"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:03", "2023-01-01 10:00:04",
      "2023-01-01 10:00:08", "2023-01-01 10:00:13",
      "2023-01-01 10:00:14"
    ), tz = "UTC"),
    bin = c(1, 2, 3, 1, 2),
    start_weight = c(10.5, 8.3, 9.1, 10.2, 8.0),
    end_weight = c(10.2, 8.1, 8.9, 9.9, 7.8)
  )
  
  # Process matrices
  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = 1:3)
  
  # Analyze pairs
  result <- synch_pair_analysis(matrices, type = "feed",
                                id_col = "animal")
  
  expect_type(result, "list")
  expect_named(result, c("bout", "total_time", "avg_duration"))
  
  # Check matrices structure
  expect_true(is.matrix(result$bout))
  expect_true(is.matrix(result$total_time))
  expect_true(is.matrix(result$avg_duration))
  
  # Check dimensions
  expect_equal(nrow(result$bout), 3)
  expect_equal(ncol(result$bout), 3)
  
  # Check animal IDs in names
  expect_equal(rownames(result$bout), c("1", "2", "3"))
  expect_equal(colnames(result$bout), c("1", "2", "3"))
})

test_that("synch_pair_analysis works with multi-day input", {
  # Create toy data with multiple days
  toy_data <- data.frame(
    animal = c(1, 2, 1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:01",
      "2023-01-02 10:00:00", "2023-01-02 10:00:01"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:03", "2023-01-01 10:00:04",
      "2023-01-02 10:00:03", "2023-01-02 10:00:04"
    ), tz = "UTC"),
    bin = c(1, 2, 1, 2),
    start_weight = c(10.5, 8.3, 10.2, 8.0),
    end_weight = c(10.2, 8.1, 9.9, 7.8)
  )
  
  # Add date column to force multi-day processing
  toy_data$date <- as.Date(toy_data$start)
  
  # Create multi-day matrices manually by splitting by date
  day1_data <- toy_data[toy_data$date == as.Date("2023-01-01"), ]
  day2_data <- toy_data[toy_data$date == as.Date("2023-01-02"), ]
  
  matrices1 <- matrix_process(day1_data, type = "feed",
                             id_col = "animal", start_col = "start",
                             end_col = "end", bin_col = "bin",
                             start_weight_col = "start_weight",
                             end_weight_col = "end_weight",
                             bins_feed = 1:2)
  
  matrices2 <- matrix_process(day2_data, type = "feed",
                             id_col = "animal", start_col = "start",
                             end_col = "end", bin_col = "bin",
                             start_weight_col = "start_weight",
                             end_weight_col = "end_weight",
                             bins_feed = 1:2)
  
  # Combine into multi-day format
  multi_day_matrices <- list(
    synch_master_animal2 = list(
      day1 = matrices1$synch_master_animal2,
      day2 = matrices2$synch_master_animal2
    )
  )
  
  result <- synch_pair_analysis(multi_day_matrices, type = "feed",
                                id_col = "animal")
  
  expect_type(result, "list")
  expect_named(result, c("bout", "total_time", "avg_duration"))
  
  # For multi-day, each component should be a list
  expect_true(is.list(result$bout))
  expect_equal(length(result$bout), 2)
})

test_that("synch_pair_analysis detects co-occurrence correctly", {
  # Create data where animals 1 and 2 overlap, but 3 doesn't
  toy_data <- data.frame(
    animal = c(1, 2, 3),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",  # Overlaps with animal 1
      "2023-01-01 10:00:10"   # No overlap
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:05",
      "2023-01-01 10:00:03",
      "2023-01-01 10:00:12"
    ), tz = "UTC"),
    bin = c(1, 2, 3),
    start_weight = c(10.5, 8.3, 9.1),
    end_weight = c(10.2, 8.1, 8.9)
  )
  
  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = 1:3)
  
  result <- synch_pair_analysis(matrices, type = "feed",
                                id_col = "animal")
  
  # Animals 1 and 2 should have co-occurrence
  expect_true(result$bout["1", "2"] > 0)
  expect_true(result$total_time["1", "2"] > 0)
  
  # Animals 1 and 3 should have no co-occurrence
  expect_equal(result$bout["1", "3"], 0)
  expect_equal(result$total_time["1", "3"], 0)
  
  # Animals 2 and 3 should have no co-occurrence
  expect_equal(result$bout["2", "3"], 0)
  expect_equal(result$total_time["2", "3"], 0)
})

test_that("synch_pair_analysis works with different resolutions", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:00"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:05",
      "2023-01-01 10:00:05"
    ), tz = "UTC"),
    bin = c(1, 2),
    start_weight = c(10.5, 8.3),
    end_weight = c(10.2, 8.1)
  )
  
  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = 1:2)
  
  # Test with sec resolution
  result_sec <- synch_pair_analysis(matrices, type = "feed",
                                    resolution = "sec",
                                    id_col = "animal")
  expect_true(is.matrix(result_sec$bout))
  
  # Test with min resolution
  result_min <- synch_pair_analysis(matrices, type = "feed",
                                    resolution = "min",
                                    id_col = "animal")
  expect_true(is.matrix(result_min$bout))
})

test_that("synch_pair_analysis handles whitespace and case in parameters", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:01"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:03", "2023-01-01 10:00:04"
    ), tz = "UTC"),
    bin = c(1, 2),
    start_weight = c(10.5, 8.3),
    end_weight = c(10.2, 8.1)
  )
  
  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = 1:2)
  
  # Should handle case and whitespace
  expect_no_error(
    synch_pair_analysis(matrices, type = " FEED ", resolution = " SEC ",
                       id_col = "animal")
  )
})

test_that("synch_pair_analysis errors on NULL matrix_data", {
  expect_error(
    synch_pair_analysis(NULL, type = "feed"),
    "matrix_data.*cannot be NULL or empty"
  )
})

test_that("synch_pair_analysis errors on empty matrix_data", {
  expect_error(
    synch_pair_analysis(list(), type = "feed"),
    "matrix_data.*cannot be NULL or empty"
  )
})

test_that("synch_pair_analysis errors on missing synch_master_animal2", {
  bad_data <- list(other_component = data.frame(x = 1))
  
  expect_error(
    synch_pair_analysis(bad_data, type = "feed"),
    "must contain 'synch_master_animal2'"
  )
})

test_that("synch_pair_analysis handles single animal", {
  toy_data <- data.frame(
    animal = c(1, 1),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:05"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:03", "2023-01-01 10:00:08"
    ), tz = "UTC"),
    bin = c(1, 1),
    start_weight = c(10.5, 10.2),
    end_weight = c(10.2, 9.9)
  )
  
  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = 1)
  
  result <- synch_pair_analysis(matrices, type = "feed",
                                id_col = "animal")
  
  # Should return 1x1 matrices with zeros (no pairs possible)
  expect_equal(dim(result$bout), c(1, 1))
  expect_equal(result$bout[1, 1], 0)
})

# Test process_all_pairs_one_day -----------------------------------------------

test_that("process_all_pairs_one_day works correctly", {
  # Create a simple animal matrix
  animal_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",
      "2023-01-01 10:00:02"
    ), tz = "UTC"),
    `1` = c(1, 1, 0),
    `2` = c(1, 0, 0),
    check.names = FALSE
  )
  
  result <- process_all_pairs_one_day(animal_matrix, "sec", "animal")
  
  expect_type(result, "list")
  expect_named(result, c("bout", "total_time", "avg_duration"))
  expect_true(is.matrix(result$bout))
  expect_equal(dim(result$bout), c(2, 2))
})

test_that("process_all_pairs_one_day errors on non-data.frame input", {
  expect_error(
    process_all_pairs_one_day(list(a = 1), "sec", "animal"),
    "animal_matrix must be a data frame"
  )
})

test_that("process_all_pairs_one_day errors on missing Time column", {
  bad_matrix <- data.frame(x = 1, y = 2)
  
  expect_error(
    process_all_pairs_one_day(bad_matrix, "sec", "animal"),
    "animal_matrix must have 'Time' column"
  )
})

test_that("process_all_pairs_one_day errors on no animal columns", {
  bad_matrix <- data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC"))
  
  expect_error(
    process_all_pairs_one_day(bad_matrix, "sec", "animal"),
    "No animal columns found"
  )
})

test_that("process_all_pairs_one_day handles single animal correctly", {
  animal_matrix <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC"),
    `1` = 1,
    check.names = FALSE
  )
  
  result <- process_all_pairs_one_day(animal_matrix, "sec", "animal")
  
  expect_equal(dim(result$bout), c(1, 1))
  expect_equal(result$bout[1, 1], 0)
})

# Test extract_pair_activity ---------------------------------------------------

test_that("extract_pair_activity extracts correct time points", {
  animal_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",
      "2023-01-01 10:00:02",
      "2023-01-01 10:00:03"
    ), tz = "UTC"),
    `1` = c(1, 1, 0, 1),
    `2` = c(1, 0, 1, 1),
    check.names = FALSE
  )
  
  result <- extract_pair_activity(animal_matrix, "1", "2")
  
  expect_true(lubridate::is.POSIXct(result))
  # Both active at times 1 and 4
  expect_equal(length(result), 2)
  expect_equal(result[1], animal_matrix$Time[1])
  expect_equal(result[2], animal_matrix$Time[4])
})

test_that("extract_pair_activity returns empty vector when no overlap", {
  animal_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01"
    ), tz = "UTC"),
    `1` = c(1, 0),
    `2` = c(0, 1),
    check.names = FALSE
  )
  
  result <- extract_pair_activity(animal_matrix, "1", "2")
  
  expect_equal(length(result), 0)
  expect_true(lubridate::is.POSIXct(result))
})

test_that("extract_pair_activity handles numeric animal IDs", {
  animal_matrix <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC"),
    `1` = 1,
    `2` = 1,
    check.names = FALSE
  )
  
  result <- extract_pair_activity(animal_matrix, 1, 2)
  
  expect_equal(length(result), 1)
})

test_that("extract_pair_activity errors on missing animal", {
  animal_matrix <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC"),
    `1` = 1,
    check.names = FALSE
  )
  
  expect_error(
    extract_pair_activity(animal_matrix, "1", "999"),
    "Animal 999 not found"
  )
  
  expect_error(
    extract_pair_activity(animal_matrix, "999", "1"),
    "Animal 999 not found"
  )
})

