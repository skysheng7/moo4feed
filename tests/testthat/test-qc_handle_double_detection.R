# -----------------------------------------------------------------------------#
# Tests for qc_handle_double_detection.R                                       #
# -----------------------------------------------------------------------------#

# Helper functions --------------------------------------------------------

create_test_data <- function() {
  # Create test feed data
  feed <- list()
  feed[["2023-01-01"]] <- data.frame(
    cow = c(1, 2, 2, 3),
    bin = c(1, 2, 3, 4),
    start = lubridate::ymd_hms(
      c(
        "2023-01-01 08:00:00",
        "2023-01-01 08:30:00",
        "2023-01-01 08:45:00", # Cow 2 at different bins with overlapping times
        "2023-01-01 09:00:00"
      ),
      tz = "UTC"
    ),
    end = lubridate::ymd_hms(
      c(
        "2023-01-01 08:15:00",
        "2023-01-01 08:50:00",
        "2023-01-01 09:00:00",
        "2023-01-01 09:15:00"
      ),
      tz = "UTC"
    ),
    duration = c(900, 1200, 900, 900),
    stringsAsFactors = FALSE
  )
  
  feed[["2023-01-02"]] <- data.frame(
    cow = c(4, 5, 5),
    bin = c(1, 2, 1),
    start = lubridate::ymd_hms(
      c(
        "2023-01-02 10:00:00",
        "2023-01-02 10:30:00",
        "2023-01-02 10:05:00" # Different cow at same bin with overlapping times
      ),
      tz = "UTC"
    ),
    end = lubridate::ymd_hms(
      c(
        "2023-01-02 10:15:00",
        "2023-01-02 10:45:00",
        "2023-01-02 10:20:00"
      ),
      tz = "UTC"
    ),
    duration = c(900, 900, 900),
    stringsAsFactors = FALSE
  )
  
  # Create test water data
  water <- list()
  water[["2023-01-01"]] <- data.frame(
    cow = c(1, 2),
    bin = c(101, 102),
    start = lubridate::ymd_hms(
      c(
        "2023-01-01 08:10:00",
        "2023-01-01 08:40:00"
      ),
      tz = "UTC"
    ),
    end = lubridate::ymd_hms(
      c(
        "2023-01-01 08:20:00",
        "2023-01-01 08:55:00"
      ),
      tz = "UTC"
    ),
    duration = c(600, 900),
    stringsAsFactors = FALSE
  )
  
  water[["2023-01-02"]] <- data.frame(
    cow = c(4, 6, 6),
    bin = c(101, 102, 103),
    start = lubridate::ymd_hms(
      c(
        "2023-01-02 10:10:00",
        "2023-01-02 10:30:00",
        "2023-01-02 10:40:00" # Cow 6 at different bins with overlapping times
      ),
      tz = "UTC"
    ),
    end = lubridate::ymd_hms(
      c(
        "2023-01-02 10:20:00",
        "2023-01-02 10:50:00",
        "2023-01-02 10:55:00"
      ),
      tz = "UTC"
    ),
    duration = c(600, 1200, 900),
    stringsAsFactors = FALSE
  )
  
  return(list(feed = feed, water = water))
}

# Tests for handle_double_detection_cow ----------------------------------

test_that("handle_double_detection_cow correctly adjusts overlapping times for same animal", {
  test_data <- create_test_data()
  feed <- test_data$feed
  
  # Process with handle_double_detection_cow
  result <- handle_double_detection_cow(feed)
  
  # Check that cow 2's overlapping visit times were adjusted
  # The end time of the first visit should be 1 second before the start of the second
  cow2_day1 <- result[["2023-01-01"]][result[["2023-01-01"]]$cow == 2, ]
  cow2_day1 <- cow2_day1[order(cow2_day1$start), ]
  
  expected_end_time <- lubridate::ymd_hms("2023-01-01 08:44:59", tz = "UTC")
  expect_equal(cow2_day1$end[1], expected_end_time)
  
  # Check that other records weren't modified
  expect_equal(result[["2023-01-01"]]$end[1], feed[["2023-01-01"]]$end[1])
  expect_equal(result[["2023-01-01"]]$end[4], feed[["2023-01-01"]]$end[4])
})

test_that("handle_double_detection_cow handles single-record dataframes", {
  # Create a list with a single-record dataframe
  single_record <- list()
  single_record[["2023-01-03"]] <- data.frame(
    cow = 1,
    bin = 1,
    start = lubridate::ymd_hms("2023-01-03 08:00:00", tz = "UTC"),
    end = lubridate::ymd_hms("2023-01-03 08:15:00", tz = "UTC"),
    duration = 900,
    stringsAsFactors = FALSE
  )
  
  # Process with handle_double_detection_cow
  result <- handle_double_detection_cow(single_record)
  
  # Check that the record wasn't modified
  expect_equal(result[["2023-01-03"]], single_record[["2023-01-03"]])
})

test_that("handle_double_detection_cow handles empty dataframes", {
  # Create a list with an empty dataframe
  empty_df <- list()
  empty_df[["2023-01-04"]] <- data.frame(
    cow = character(),
    bin = integer(),
    start = lubridate::ymd_hms(character(), tz = "UTC"),
    end = lubridate::ymd_hms(character(), tz = "UTC"),
    duration = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process with handle_double_detection_cow
  result <- handle_double_detection_cow(empty_df)
  
  # Check that the empty dataframe remains unchanged
  expect_equal(nrow(result[["2023-01-04"]]), 0)
})

test_that("handle_double_detection_cow works with custom column names", {
  # Create test data with custom column names
  custom_data <- list()
  custom_data[["2023-01-05"]] <- data.frame(
    animal_id = c(1, 1),
    bin = c(1, 2),
    start_time = lubridate::ymd_hms(
      c(
        "2023-01-05 08:00:00",
        "2023-01-05 08:05:00" # Overlapping visit
      ),
      tz = "UTC"
    ),
    end_time = lubridate::ymd_hms(
      c(
        "2023-01-05 08:10:00",
        "2023-01-05 08:15:00"
      ),
      tz = "UTC"
    ),
    duration = c(600, 600),
    stringsAsFactors = FALSE
  )
  
  # Process with handle_double_detection_cow using custom column names
  result <- handle_double_detection_cow(
    custom_data,
    id_col = "animal_id",
    start_col = "start_time",
    end_col = "end_time"
  )
  
  # Check that the end time was adjusted
  expected_end_time <- lubridate::ymd_hms("2023-01-05 08:04:59", tz = "UTC")
  expect_equal(result[["2023-01-05"]]$end_time[1], expected_end_time)
})

# Tests for handle_double_detection_bin ----------------------------------

test_that("handle_double_detection_bin correctly adjusts overlapping times for same bin", {
  test_data <- create_test_data()
  feed <- test_data$feed
  
  # Process with handle_double_detection_bin
  result <- handle_double_detection_bin(feed)
  
  # Check that cow 5's visit time at bin 1 (which overlaps with cow 4) was adjusted
  day2_data <- result[["2023-01-02"]]
  cow5_bin1 <- day2_data[day2_data$cow == 5 & day2_data$bin == 1, ]
  cow4_bin1 <- day2_data[day2_data$cow == 4 & day2_data$bin == 1, ]
  
  # Get row with earlier start time (should be cow 4)
  bin1_sorted <- day2_data[day2_data$bin == 1, ]
  bin1_sorted <- bin1_sorted[order(bin1_sorted$start), ]
  first_row <- bin1_sorted[1, ]
  
  # End time of first visit at bin 1 should be adjusted to 1 second before start of second visit
  expected_end_time <- bin1_sorted$start[2] - lubridate::seconds(1)
  expect_equal(first_row$end, expected_end_time)
})

test_that("handle_double_detection_bin handles single-record dataframes", {
  # Create a list with a single-record dataframe
  single_record <- list()
  single_record[["2023-01-03"]] <- data.frame(
    cow = 1,
    bin = 1,
    start = lubridate::ymd_hms("2023-01-03 08:00:00", tz = "UTC"),
    end = lubridate::ymd_hms("2023-01-03 08:15:00", tz = "UTC"),
    duration = 900,
    stringsAsFactors = FALSE
  )
  
  # Process with handle_double_detection_bin
  result <- handle_double_detection_bin(single_record)
  
  # Check that the record wasn't modified
  expect_equal(result[["2023-01-03"]], single_record[["2023-01-03"]])
})

test_that("handle_double_detection_bin handles empty dataframes", {
  # Create a list with an empty dataframe
  empty_df <- list()
  empty_df[["2023-01-04"]] <- data.frame(
    cow = character(),
    bin = integer(),
    start = lubridate::ymd_hms(character(), tz = "UTC"),
    end = lubridate::ymd_hms(character(), tz = "UTC"),
    duration = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process with handle_double_detection_bin
  result <- handle_double_detection_bin(empty_df)
  
  # Check that the empty dataframe remains unchanged
  expect_equal(nrow(result[["2023-01-04"]]), 0)
})

test_that("handle_double_detection_bin works with custom column names", {
  # Create test data with custom column names
  custom_data <- list()
  custom_data[["2023-01-05"]] <- data.frame(
    animal_id = c(1, 2),
    feeder_bin = c(1, 1),
    start_time = lubridate::ymd_hms(
      c(
        "2023-01-05 08:00:00",
        "2023-01-05 08:05:00" # Same bin, different animals, overlapping times
      ),
      tz = "UTC"
    ),
    end_time = lubridate::ymd_hms(
      c(
        "2023-01-05 08:10:00",
        "2023-01-05 08:15:00"
      ),
      tz = "UTC"
    ),
    duration = c(600, 600),
    stringsAsFactors = FALSE
  )
  
  # Process with handle_double_detection_bin using custom column names
  result <- handle_double_detection_bin(
    custom_data,
    bin_col = "feeder_bin",
    start_col = "start_time",
    end_col = "end_time"
  )
  
  # Check that the end time was adjusted
  expected_end_time <- lubridate::ymd_hms("2023-01-05 08:04:59", tz = "UTC")
  expect_equal(result[["2023-01-05"]]$end_time[1], expected_end_time)
})

# Tests for update_duration ----------------------------------------------

test_that("update_duration correctly recalculates durations", {
  # Create test data with incorrect durations
  test_data <- list()
  test_data[["2023-01-06"]] <- data.frame(
    cow = c(1, 2),
    bin = c(1, 2),
    start = lubridate::ymd_hms(
      c(
        "2023-01-06 08:00:00",
        "2023-01-06 09:00:00"
      ),
      tz = "UTC"
    ),
    end = lubridate::ymd_hms(
      c(
        "2023-01-06 08:15:00",
        "2023-01-06 09:30:00"
      ),
      tz = "UTC"
    ),
    duration = c(1000, 2000), # Incorrect durations
    stringsAsFactors = FALSE
  )
  
  # Process with update_duration
  result <- update_duration(test_data)
  
  # Check that durations were correctly recalculated
  expected_durations <- c(
    as.numeric(difftime(test_data[["2023-01-06"]]$end[1], test_data[["2023-01-06"]]$start[1], units = "secs")),
    as.numeric(difftime(test_data[["2023-01-06"]]$end[2], test_data[["2023-01-06"]]$start[2], units = "secs"))
  )
  
  expect_equal(result[["2023-01-06"]]$duration, expected_durations)
})

test_that("update_duration handles empty dataframes", {
  # Create a list with an empty dataframe
  empty_df <- list()
  empty_df[["2023-01-07"]] <- data.frame(
    cow = character(),
    bin = integer(),
    start = lubridate::ymd_hms(character(), tz = "UTC"),
    end = lubridate::ymd_hms(character(), tz = "UTC"),
    duration = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process with update_duration
  result <- update_duration(empty_df)
  
  # Check that the empty dataframe remains unchanged
  expect_equal(nrow(result[["2023-01-07"]]), 0)
})

test_that("update_duration works with custom column names", {
  # Create test data with custom column names
  custom_data <- list()
  custom_data[["2023-01-08"]] <- data.frame(
    animal_id = c(1, 2),
    feeder_bin = c(1, 2),
    start_time = lubridate::ymd_hms(
      c(
        "2023-01-08 08:00:00",
        "2023-01-08 09:00:00"
      ),
      tz = "UTC"
    ),
    end_time = lubridate::ymd_hms(
      c(
        "2023-01-08 08:15:00",
        "2023-01-08 09:30:00"
      ),
      tz = "UTC"
    ),
    visit_duration = c(1000, 2000), # Incorrect durations
    stringsAsFactors = FALSE
  )
  
  # Process with update_duration using custom column names
  result <- update_duration(
    custom_data,
    start_col = "start_time",
    end_col = "end_time",
    dur_col = "visit_duration"
  )
  
  # Check that durations were correctly recalculated
  expected_durations <- c(
    as.numeric(difftime(custom_data[["2023-01-08"]]$end_time[1], custom_data[["2023-01-08"]]$start_time[1], units = "secs")),
    as.numeric(difftime(custom_data[["2023-01-08"]]$end_time[2], custom_data[["2023-01-08"]]$start_time[2], units = "secs"))
  )
  
  expect_equal(result[["2023-01-08"]]$visit_duration, expected_durations)
})

# Tests for split_feed_water ---------------------------------------------

test_that("split_feed_water correctly separates feed and water data", {
  # Create combined test data
  test_data <- create_test_data()
  feed <- test_data$feed
  water <- test_data$water
  
  # Combine feed and water data
  comb <- combine_feed_water(feed, water)
  
  # Split the combined data
  result <- split_feed_water(comb, feed, water)
  
  # Check that feed data was correctly split
  expect_equal(nrow(result$feed[["2023-01-01"]]), nrow(feed[["2023-01-01"]]))
  expect_equal(nrow(result$feed[["2023-01-02"]]), nrow(feed[["2023-01-02"]]))
  
  # Check that water data was correctly split
  expect_equal(nrow(result$water[["2023-01-01"]]), nrow(water[["2023-01-01"]]))
  expect_equal(nrow(result$water[["2023-01-02"]]), nrow(water[["2023-01-02"]]))
  
  # Check that bins were correctly identified
  expect_true(all(result$feed[["2023-01-01"]]$bin < 100))
  expect_true(all(result$water[["2023-01-01"]]$bin >= 100))
})

test_that("split_feed_water handles NULL inputs correctly", {
  # Test with feed NULL
  test_data <- create_test_data()
  water <- test_data$water
  comb <- water
  
  result <- split_feed_water(comb, NULL, water)
  expect_null(result$feed)
  expect_equal(result$water, water)
  
  # Test with water NULL
  feed <- test_data$feed
  comb <- feed
  
  result <- split_feed_water(comb, feed, NULL)
  expect_equal(result$feed, feed)
  expect_null(result$water)
})

test_that("split_feed_water works with custom bin_col and bin_offset", {
  # Create test data with custom column names
  test_data <- create_test_data()
  feed <- test_data$feed
  water <- test_data$water
  
  # Rename bin column
  for (i in seq_along(feed)) {
    names(feed[[i]])[names(feed[[i]]) == "bin"] <- "feeder_bin"
  }
  for (i in seq_along(water)) {
    names(water[[i]])[names(water[[i]]) == "bin"] <- "feeder_bin"
  }
  
  # Combine feed and water data
  comb <- combine_feed_water(feed, water)
  
  # Split with custom column name and offset
  result <- split_feed_water(comb, feed, water, bin_col = "feeder_bin", bin_offset = 100)
  
  # Check that feed and water were correctly separated
  expect_equal(nrow(result$feed[["2023-01-01"]]), nrow(feed[["2023-01-01"]]))
  expect_equal(nrow(result$water[["2023-01-01"]]), nrow(water[["2023-01-01"]]))
})

# Tests for handle_all_double_detections ---------------------------------

test_that("handle_all_double_detections integrates all correction steps", {
  test_data <- create_test_data()
  feed <- test_data$feed
  water <- test_data$water
  
  # Process with handle_all_double_detections
  result <- handle_all_double_detections(feed, water)
  
  # Check that all components are present
  expect_true(!is.null(result$feed))
  expect_true(!is.null(result$water))
  expect_true(!is.null(result$combined))
  
  # Check that overlapping times were adjusted for cow 2 feeding event
  cow2_feed <- result$feed[["2023-01-01"]][result$feed[["2023-01-01"]]$cow == 2, ]
  cow2_feed <- cow2_feed[order(cow2_feed$start), ]
  expected_end_time <- lubridate::ymd_hms("2023-01-01 08:39:59", tz = "UTC")
  expect_equal(cow2_feed$end[1], expected_end_time)
  
  # Check that durations were updated
  first_visit_duration <- as.numeric(difftime(cow2_feed$end[1], cow2_feed$start[1], units = "secs"))
  expect_equal(cow2_feed$duration[1], first_visit_duration)

  # Check that overlapping times were adjusted for cow 2 drinking event
  cow2_wat <- result$water[["2023-01-01"]][result$water[["2023-01-01"]]$cow == 2, ]
  cow2_wat <- cow2_wat[order(cow2_wat$start), ]
  expected_end_time <- lubridate::ymd_hms("2023-01-01 08:44:59", tz = "UTC")
  expect_equal(cow2_wat$end, expected_end_time)
})

test_that("handle_all_double_detections works with feed data only", {
  test_data <- create_test_data()
  feed <- test_data$feed
  
  # Process with handle_all_double_detections (feed only)
  result <- handle_all_double_detections(feed = feed, water = NULL)
  
  # Check that feed data was processed
  expect_true(!is.null(result$feed))
  expect_true(is.null(result$water))
  expect_true(!is.null(result$combined))
  
  # Check structure matches original
  expect_equal(names(result$feed), names(feed))
  expect_equal(length(result$feed), length(feed))
})

test_that("handle_all_double_detections works with water data only", {
  test_data <- create_test_data()
  water <- test_data$water
  
  # Process with handle_all_double_detections (water only)
  result <- handle_all_double_detections(feed = NULL, water = water)
  
  # Check that water data was processed
  expect_true(is.null(result$feed))
  expect_true(!is.null(result$water))
  expect_true(!is.null(result$combined))
  
  # Check structure matches original
  expect_equal(names(result$water), names(water))
  expect_equal(length(result$water), length(water))
})

test_that("handle_all_double_detections throws error with both NULL inputs", {
  expect_error(
    handle_all_double_detections(feed = NULL, water = NULL),
    "Both feed and water cannot be NULL."
  )
})

test_that("handle_all_double_detections works with custom column names", {
  test_data <- create_test_data()
  feed <- test_data$feed
  water <- test_data$water
  
  # Rename columns
  for (i in seq_along(feed)) {
    names(feed[[i]])[names(feed[[i]]) == "cow"] <- "animal_id"
    names(feed[[i]])[names(feed[[i]]) == "bin"] <- "feeder_bin"
    names(feed[[i]])[names(feed[[i]]) == "start"] <- "start_time"
    names(feed[[i]])[names(feed[[i]]) == "end"] <- "end_time"
    names(feed[[i]])[names(feed[[i]]) == "duration"] <- "visit_duration"
  }
  
  for (i in seq_along(water)) {
    names(water[[i]])[names(water[[i]]) == "cow"] <- "animal_id"
    names(water[[i]])[names(water[[i]]) == "bin"] <- "feeder_bin"
    names(water[[i]])[names(water[[i]]) == "start"] <- "start_time"
    names(water[[i]])[names(water[[i]]) == "end"] <- "end_time"
    names(water[[i]])[names(water[[i]]) == "duration"] <- "visit_duration"
  }
  
  # Process with custom column names
  result <- handle_all_double_detections(
    feed = feed,
    water = water,
    id_col = "animal_id",
    bin_col = "feeder_bin",
    start_col = "start_time",
    end_col = "end_time",
    dur_col = "visit_duration",
    bin_offset = 100
  )
  
  # Check that results contain the custom column names
  expect_true("animal_id" %in% names(result$feed[["2023-01-01"]]))
  expect_true("feeder_bin" %in% names(result$feed[["2023-01-01"]]))
  expect_true("start_time" %in% names(result$feed[["2023-01-01"]]))
  expect_true("end_time" %in% names(result$feed[["2023-01-01"]]))
  expect_true("visit_duration" %in% names(result$feed[["2023-01-01"]]))
}) 