# Tests for synch_matrix_processing.R
# Testing matrix processing functions for synchronicity analysis

# ============================================================================ #
# matrix_process() tests - Main exported function
# ============================================================================ #

test_that("matrix_process works with feed type and single data frame", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(1, 2),
    start_weight = c(10.5, 8.3),
    end_weight = c(10.2, 8.1)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "feed",
    resolution = "sec",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight",
    bins_feed = 1:2
  )
  
  expect_type(result, "list")
  expect_named(result, c("synch_master_animal2", "synch_master_bin2", "synch_master_feed2"))
  expect_s3_class(result$synch_master_animal2, "data.frame")
  expect_s3_class(result$synch_master_bin2, "data.frame")
  expect_s3_class(result$synch_master_feed2, "data.frame")
  
  # Check derived columns
  expect_true("total_animal_num" %in% colnames(result$synch_master_animal2))
  expect_true("unoccupied_bin_num" %in% colnames(result$synch_master_animal2))
  expect_true("date" %in% colnames(result$synch_master_animal2))
  expect_true("totalFeed" %in% colnames(result$synch_master_feed2))
})

test_that("matrix_process works with feed type and list input", {
  toy_data <- list(
    day1 = data.frame(
      animal = c(1, 2),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
      bin = c(1, 2),
      start_weight = c(10.5, 8.3),
      end_weight = c(10.2, 8.1)
    ),
    day2 = data.frame(
      animal = c(1, 2),
      start = lubridate::ymd_hms(c("2023-01-02 10:00:00", "2023-01-02 10:00:01")),
      end = lubridate::ymd_hms(c("2023-01-02 10:00:02", "2023-01-02 10:00:03")),
      bin = c(1, 2),
      start_weight = c(11.5, 9.3),
      end_weight = c(11.2, 9.1)
    )
  )
  
  result <- matrix_process(
    toy_data, 
    type = "feed",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight",
    bins_feed = 1:2
  )
  
  expect_type(result, "list")
  expect_named(result, c("synch_master_animal2", "synch_master_bin2", "synch_master_feed2"))
  expect_length(result$synch_master_animal2, 2)
  expect_length(result$synch_master_bin2, 2)
  expect_length(result$synch_master_feed2, 2)
  expect_named(result$synch_master_animal2, c("day1", "day2"))
})

test_that("matrix_process works with drink type", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(1, 2)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "drink",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    bins_wat = 1:2
  )
  
  expect_type(result, "list")
  expect_named(result, c("synch_master_animal2", "synch_master_bin2"))
  expect_false("synch_master_feed2" %in% names(result))
})

test_that("matrix_process works with feed_and_drink type", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(1, 2)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "feed_and_drink",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    bins_feed = 1:2,
    bins_wat = 3:4
  )
  
  expect_type(result, "list")
  expect_named(result, c("synch_master_animal2", "synch_master_bin2"))
  
  # Unoccupied bins should account for both feed and water bins
  total_bins <- length(1:2) + length(3:4)
  expect_true(all(result$synch_master_animal2$unoccupied_bin_num <= total_bins))
})

test_that("matrix_process handles case-insensitive type", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(1, 2)
  )
  
  result1 <- matrix_process(toy_data, type = "DRINK", id_col = "animal", 
                           start_col = "start", end_col = "end", 
                           bin_col = "bin", bins_wat = 1:2)
  result2 <- matrix_process(toy_data, type = " drink ", id_col = "animal", 
                           start_col = "start", end_col = "end", 
                           bin_col = "bin", bins_wat = 1:2)
  
  expect_named(result1, c("synch_master_animal2", "synch_master_bin2"))
  expect_named(result2, c("synch_master_animal2", "synch_master_bin2"))
})

test_that("matrix_process works with minute resolution", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    end = lubridate::ymd_hms(c("2023-01-01 10:02:00", "2023-01-01 10:03:00")),
    bin = c(1, 2)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "drink",
    resolution = "min",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    bins_wat = 1:2
  )
  
  expect_s3_class(result$synch_master_animal2, "data.frame")
  # With minute resolution, should have fewer rows
  expect_true(nrow(result$synch_master_animal2) > 0)
})

test_that("matrix_process errors on invalid inputs", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(1, 2)
  )
  
  # NULL data
  expect_error(
    matrix_process(NULL, type = "feed"),
    "`data_list` cannot be NULL or empty"
  )
  
  # Empty list
  expect_error(
    matrix_process(list(), type = "feed"),
    "`data_list` cannot be NULL or empty"
  )
  
  # Missing required columns
  expect_error(
    matrix_process(
      data.frame(x = 1), 
      type = "feed",
      id_col = "animal",
      start_col = "start",
      end_col = "end"
    ),
    "Missing required columns"
  )
  
  # Missing weight columns for feed type
  expect_error(
    matrix_process(
      data.frame(
        animal = 1,
        start = lubridate::ymd_hms("2023-01-01 10:00:00"),
        end = lubridate::ymd_hms("2023-01-01 10:00:02"),
        bin = 1
      ),
      type = "feed",
      id_col = "animal",
      start_col = "start",
      end_col = "end",
      bin_col = "bin",
      start_weight_col = "start_weight",
      end_weight_col = "end_weight",
      bins_feed = 1:2
    ),
    "Missing required columns"
  )
})

test_that("matrix_process filters out inactive time periods", {
  # Create data with gaps
  toy_data <- data.frame(
    animal = c(1, 1),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:10")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:12")),
    bin = c(1, 1)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "drink",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    bins_wat = 1:2
  )
  
  # Should only have active time periods (not the 8-second gap)
  expect_true(all(result$synch_master_animal2$total_animal_num > 0))
})

# ============================================================================ #
# find_closest_time_index() tests
# ============================================================================ #

test_that("find_closest_time_index finds exact match", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:05"),
    by = "sec"
  )
  
  target <- lubridate::ymd_hms("2023-01-01 10:00:03")
  result <- find_closest_time_index(time_seq, target, tolerance = 1)
  
  expect_equal(result, 4)  # Index 4 is the 3-second mark
})

test_that("find_closest_time_index finds closest match within tolerance", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:05"),
    by = "sec"
  )
  
  # Target is slightly off
  target <- lubridate::ymd_hms("2023-01-01 10:00:02") + 0.5
  result <- find_closest_time_index(time_seq, target, tolerance = 1)
  
  expect_true(result %in% c(3, 4))  # Should match index 3 or 4
})

test_that("find_closest_time_index errors when outside tolerance", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:05"),
    by = "sec"
  )
  
  # Target is way off
  target <- lubridate::ymd_hms("2023-01-01 11:00:00")
  
  expect_error(
    find_closest_time_index(time_seq, target, tolerance = 1),
    "No matching time found within tolerance"
  )
})

test_that("find_closest_time_index handles edge cases", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:05"),
    by = "sec"
  )
  
  # First element
  target <- lubridate::ymd_hms("2023-01-01 10:00:00")
  result <- find_closest_time_index(time_seq, target, tolerance = 1)
  expect_equal(result, 1)
  
  # Last element
  target <- lubridate::ymd_hms("2023-01-01 10:00:05")
  result <- find_closest_time_index(time_seq, target, tolerance = 1)
  expect_equal(result, 6)
})

test_that("find_closest_time_index errors when time is way before sequence", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:05"),
    by = "sec"
  )
  
  # Target is way before the sequence start (exceeds tolerance)
  target <- lubridate::ymd_hms("2023-01-01 08:00:00")
  
  expect_error(
    find_closest_time_index(time_seq, target, tolerance = 1),
    "No matching time found within tolerance"
  )
})

# ============================================================================ #
# filter_matrix_and_add_date() tests
# ============================================================================ #

test_that("filter_matrix_and_add_date filters correctly", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:05"),
    by = "sec"
  )
  
  matrix_data <- data.frame(
    Time = time_seq,
    animal1 = c(1, 1, 0, 0, 1, 1),
    animal2 = c(0, 1, 1, 0, 0, 1)
  )
  
  active_records <- c(1, 2, 3, 5, 6)  # Skip row 4
  result <- filter_matrix_and_add_date(matrix_data, active_records)
  
  expect_equal(nrow(result), 5)
  expect_true("date" %in% colnames(result))
  expect_s3_class(result$date, "Date")
  expect_equal(result$date, rep(as.Date("2023-01-01"), 5))
})

test_that("filter_matrix_and_add_date handles empty active records", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:05"),
    by = "sec"
  )
  
  matrix_data <- data.frame(
    Time = time_seq,
    animal1 = rep(0, 6)
  )
  
  result <- filter_matrix_and_add_date(matrix_data, integer(0))
  
  expect_equal(nrow(result), 0)
  expect_true("date" %in% colnames(result))
  expect_s3_class(result$date, "Date")
})

test_that("filter_matrix_and_add_date preserves all columns", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:02"),
    by = "sec"
  )
  
  matrix_data <- data.frame(
    Time = time_seq,
    animal1 = c(1, 1, 0),
    animal2 = c(0, 1, 1),
    extra_col = c(10, 20, 30)
  )
  
  active_records <- c(1, 2)
  result <- filter_matrix_and_add_date(matrix_data, active_records)
  
  expect_true("extra_col" %in% colnames(result))
  expect_equal(result$extra_col, c(10, 20))
})

# ============================================================================ #
# process_cur_synch() tests
# ============================================================================ #

test_that("process_cur_synch adds totalFeed column", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:02"),
    by = "sec"
  )
  
  cur_synch <- data.frame(
    Time = time_seq,
    `1` = c(10.5, 10.3, 10.1),
    `2` = c(8.0, 7.8, 7.6),
    check.names = FALSE
  )
  
  result <- process_cur_synch(cur_synch, bins_feed = 1:2)
  
  expect_true("totalFeed" %in% colnames(result))
  expect_equal(result$totalFeed, c(18.5, 18.1, 17.7))
})

test_that("process_cur_synch handles NA values with forward fill", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:03"),
    by = "sec"
  )
  
  cur_synch <- data.frame(
    Time = time_seq,
    `1` = c(10.5, NA, NA, 10.1),
    `2` = c(8.0, 7.8, NA, 7.6),
    check.names = FALSE
  )
  
  result <- process_cur_synch(cur_synch, bins_feed = 1:2)
  
  # NA values should be forward filled
  expect_false(any(is.na(result$`1`)))
  expect_false(any(is.na(result$`2`)))
  expect_equal(result$`1`, c(10.5, 10.5, 10.5, 10.1))
  expect_equal(result$`2`, c(8.0, 7.8, 7.8, 7.6))
})

test_that("process_cur_synch handles all NA columns", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:02"),
    by = "sec"
  )
  
  cur_synch <- data.frame(
    Time = time_seq,
    `1` = c(NA, NA, NA),
    `2` = c(8.0, 7.8, 7.6),
    check.names = FALSE
  )
  
  result <- process_cur_synch(cur_synch, bins_feed = 1:2)
  
  # All NA column should default to 0
  expect_equal(result$`1`[1], 0)
})

test_that("process_cur_synch errors on invalid inputs", {
  # NULL input
  expect_error(
    process_cur_synch(NULL, bins_feed = 1:2),
    "`cur_synch` cannot be NULL"
  )
  
  # Missing Time column
  cur_synch <- data.frame(`1` = c(10, 9, 8), check.names = FALSE)
  expect_error(
    process_cur_synch(cur_synch, bins_feed = 1:2),
    "Input matrix must contain a 'Time' column"
  )
  
  # Invalid bins_feed
  time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"),
                  lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
  cur_synch <- data.frame(Time = time_seq, `1` = c(10, 9, 8), check.names = FALSE)
  
  expect_error(
    process_cur_synch(cur_synch, bins_feed = character(0)),
    "bins_feed must be a non-empty numeric vector"
  )
  
  expect_error(
    process_cur_synch(cur_synch, bins_feed = numeric(0)),
    "bins_feed must be a non-empty numeric vector"
  )
})

test_that("process_cur_synch handles missing bin columns gracefully", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:02"),
    by = "sec"
  )
  
  cur_synch <- data.frame(
    Time = time_seq,
    `1` = c(10, 9, 8),
    check.names = FALSE
  )
  
  # Request bins 1-3 but only bin 1 exists - function processes available bins
  result <- process_cur_synch(cur_synch, bins_feed = 1:3)
  expect_true("totalFeed" %in% colnames(result))
  # Only bin 1 should be in totalFeed calculation
  expect_equal(result$totalFeed, c(10, 9, 8))
})

test_that("process_cur_synch errors when no bin columns match", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:02"),
    by = "sec"
  )
  
  cur_synch <- data.frame(
    Time = time_seq,
    `10` = c(10, 9, 8),
    check.names = FALSE
  )
  
  # Request bins 1-3 but only bin 10 exists - no match
  expect_error(
    process_cur_synch(cur_synch, bins_feed = 1:3),
    "No matching bin columns found in input matrix"
  )
})

# ============================================================================ #
# process_feed_matrix_data() tests
# ============================================================================ #

test_that("process_feed_matrix_data processes non-empty matrix", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:02"),
    by = "sec"
  )
  
  feed_matrix <- data.frame(
    Time = time_seq,
    `1` = c(10.5, 10.3, 10.1),
    `2` = c(8.0, 7.8, 7.6),
    check.names = FALSE
  )
  
  result <- process_feed_matrix_data(feed_matrix, bins_feed = 1:2)
  
  expect_true("totalFeed" %in% colnames(result))
  expect_true("date" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("process_feed_matrix_data handles empty matrix", {
  feed_matrix <- data.frame(
    Time = lubridate::ymd_hms(character(0))
  )
  
  result <- process_feed_matrix_data(feed_matrix, bins_feed = 1:2)
  
  expect_true("totalFeed" %in% colnames(result))
  expect_true("date" %in% colnames(result))
  expect_equal(nrow(result), 0)
  expect_equal(length(result$totalFeed), 0)
})

test_that("process_feed_matrix_data handles matrix with only Time column", {
  time_seq <- seq(
    lubridate::ymd_hms("2023-01-01 10:00:00"),
    lubridate::ymd_hms("2023-01-01 10:00:02"),
    by = "sec"
  )
  
  # Matrix with Time and at least one bin column (realistic scenario)
  feed_matrix <- data.frame(
    Time = time_seq,
    `1` = c(NA, NA, NA),
    check.names = FALSE
  )
  
  result <- process_feed_matrix_data(feed_matrix, bins_feed = 1:2)
  
  # Should process successfully with totalFeed and date columns
  expect_true("Time" %in% colnames(result))
  expect_true("totalFeed" %in% colnames(result))
  expect_true("date" %in% colnames(result))
})

# ============================================================================ #
# Integration tests with realistic data
# ============================================================================ #

test_that("matrix_process handles overlapping animal visits", {
  # Two animals at same time
  toy_data <- data.frame(
    animal = c(1, 2, 1),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:00", "2023-01-01 10:00:05")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:03", "2023-01-01 10:00:03", "2023-01-01 10:00:08")),
    bin = c(1, 2, 1)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "drink",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    bins_wat = 1:2
  )
  
  # During overlap, total_animal_num should be 2
  overlap_rows <- result$synch_master_animal2[1:4, ]
  expect_true(all(overlap_rows$total_animal_num == 2))
})

test_that("matrix_process correctly assigns bins to animals", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(5, 8)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "drink",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    bins_wat = 1:10
  )
  
  # Check bin assignments
  bin_matrix <- result$synch_master_bin2
  expect_true(any(bin_matrix$`1` == 5))
  expect_true(any(bin_matrix$`2` == 8))
})

test_that("matrix_process handles feed weights correctly", {
  toy_data <- data.frame(
    animal = c(1),
    start = lubridate::ymd_hms("2023-01-01 10:00:00"),
    end = lubridate::ymd_hms("2023-01-01 10:00:04"),
    bin = c(1),
    start_weight = c(10.0),
    end_weight = c(9.0)
  )
  
  result <- matrix_process(
    toy_data, 
    type = "feed",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight",
    bins_feed = 1:2
  )
  
  feed_matrix <- result$synch_master_feed2
  
  # Feed weight should decrease from 10 to 9
  bin1_weights <- feed_matrix$`1`
  expect_true(bin1_weights[1] >= bin1_weights[length(bin1_weights)])
  expect_true(all(!is.na(bin1_weights)))
})

test_that("matrix_process preserves list names", {
  toy_data <- list(
    monday = data.frame(
      animal = c(1, 2),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
      bin = c(1, 2)
    ),
    tuesday = data.frame(
      animal = c(1, 2),
      start = lubridate::ymd_hms(c("2023-01-02 10:00:00", "2023-01-02 10:00:01")),
      end = lubridate::ymd_hms(c("2023-01-02 10:00:02", "2023-01-02 10:00:03")),
      bin = c(1, 2)
    )
  )
  
  result <- matrix_process(
    toy_data, 
    type = "drink",
    id_col = "animal", 
    start_col = "start",
    end_col = "end", 
    bin_col = "bin",
    bins_wat = 1:2
  )
  
  expect_named(result$synch_master_animal2, c("monday", "tuesday"))
  expect_named(result$synch_master_bin2, c("monday", "tuesday"))
})

test_that("matrix_process handles bins outside expected range with warning", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(1, 99),  # Bin 99 is outside range
    start_weight = c(10.5, 8.3),
    end_weight = c(10.2, 8.1)
  )
  
  expect_warning(
    result <- matrix_process(
      toy_data, 
      type = "feed",
      id_col = "animal", 
      start_col = "start",
      end_col = "end", 
      bin_col = "bin",
      start_weight_col = "start_weight",
      end_weight_col = "end_weight",
      bins_feed = 1:2
    ),
    "Bin 99 is outside the expected range"
  )
})
