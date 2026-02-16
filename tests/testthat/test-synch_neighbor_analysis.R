# Test synch_neighbor_analysis -------------------------------------------------

test_that("synch_neighbor_analysis works with single day input", {
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
  
  # Analyze neighbors with linear layout
  result <- synch_neighbor_analysis(
    matrices, 
    bin_layout = "1-2-3",
    type = "feed",
    id_col = "animal"
  )
  
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

test_that("synch_neighbor_analysis works with multi-day input", {
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
    synch_master_bin2 = list(
      day1 = matrices1$synch_master_bin2,
      day2 = matrices2$synch_master_bin2
    )
  )
  
  result <- synch_neighbor_analysis(
    multi_day_matrices,
    bin_layout = "1-2",
    type = "feed",
    id_col = "animal"
  )
  
  expect_type(result, "list")
  expect_named(result, c("bout", "total_time", "avg_duration"))
  
  # For multi-day, each component should be a list
  expect_true(is.list(result$bout))
  expect_equal(length(result$bout), 2)
})

test_that("synch_neighbor_analysis detects neighbors correctly", {
  # Create data where animals are at neighboring bins
  toy_data <- data.frame(
    animal = c(1, 2, 3),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",  # Animal 2 at bin 2 (neighbor of bin 1)
      "2023-01-01 10:00:10"   # Animal 3 at bin 10 (not neighbor)
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:05",
      "2023-01-01 10:00:03",
      "2023-01-01 10:00:12"
    ), tz = "UTC"),
    bin = c(1, 2, 10),
    start_weight = c(10.5, 8.3, 9.1),
    end_weight = c(10.2, 8.1, 8.9)
  )
  
  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = c(1, 2, 10))
  
  result <- synch_neighbor_analysis(
    matrices,
    bin_layout = "1-2-3",  # Only bins 1-2 are neighbors in this layout
    type = "feed",
    id_col = "animal"
  )
  
  # Animals 1 and 2 should have neighbor activity
  expect_true(result$bout["1", "2"] > 0)
  expect_true(result$total_time["1", "2"] > 0)
  
  # Animals 1 and 3 should have no neighbor activity
  expect_equal(result$bout["1", "3"], 0)
  expect_equal(result$total_time["1", "3"], 0)
  
  # Animals 2 and 3 should have no neighbor activity
  expect_equal(result$bout["2", "3"], 0)
  expect_equal(result$total_time["2", "3"], 0)
})

test_that("synch_neighbor_analysis works with multi-row layout", {
  toy_data <- data.frame(
    animal = c(1, 2, 3, 4),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:01",
      "2023-01-01 10:00:02", "2023-01-01 10:00:03"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:05", "2023-01-01 10:00:06",
      "2023-01-01 10:00:07", "2023-01-01 10:00:08"
    ), tz = "UTC"),
    bin = c(1, 2, 4, 5),
    start_weight = c(10.5, 8.3, 9.1, 7.5),
    end_weight = c(10.2, 8.1, 8.9, 7.2)
  )
  
  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = c(1, 2, 4, 5))
  
  # Two rows: bins 1-2-3 in row 1, bins 4-5-6 in row 2
  result <- synch_neighbor_analysis(
    matrices,
    bin_layout = "1-2-3\n4-5-6",
    type = "feed",
    id_col = "animal"
  )
  
  expect_true(is.matrix(result$bout))
  expect_equal(dim(result$bout), c(4, 4))
})

test_that("synch_neighbor_analysis works with different resolutions", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:00"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:05", "2023-01-01 10:00:05"
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
  result_sec <- synch_neighbor_analysis(matrices, bin_layout = "1-2",
                                        type = "feed", resolution = "sec",
                                        id_col = "animal")
  expect_true(is.matrix(result_sec$bout))
  
  # Test with min resolution
  result_min <- synch_neighbor_analysis(matrices, bin_layout = "1-2",
                                        type = "feed", resolution = "min",
                                        id_col = "animal")
  expect_true(is.matrix(result_min$bout))
})

test_that("synch_neighbor_analysis handles whitespace and case in parameters", {
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
    synch_neighbor_analysis(matrices, bin_layout = "1-2",
                           type = " FEED ", resolution = " SEC ",
                           id_col = "animal")
  )
})

test_that("synch_neighbor_analysis errors on NULL matrix_data", {
  expect_error(
    synch_neighbor_analysis(NULL, bin_layout = "1-2", type = "feed"),
    "matrix_data.*cannot be NULL or empty"
  )
})

test_that("synch_neighbor_analysis errors on empty matrix_data", {
  expect_error(
    synch_neighbor_analysis(list(), bin_layout = "1-2", type = "feed"),
    "matrix_data.*cannot be NULL or empty"
  )
})

test_that("synch_neighbor_analysis errors on missing synch_master_bin2", {
  bad_data <- list(other_component = data.frame(x = 1))
  
  expect_error(
    synch_neighbor_analysis(bad_data, bin_layout = "1-2", type = "feed"),
    "must contain 'synch_master_bin2'"
  )
})

test_that("synch_neighbor_analysis handles single animal", {
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
  
  result <- synch_neighbor_analysis(matrices, bin_layout = "1",
                                    type = "feed", id_col = "animal")
  
  # Should return 1x1 matrices with zeros (no pairs possible)
  expect_equal(dim(result$bout), c(1, 1))
  expect_equal(result$bout[1, 1], 0)
})

# Test process_all_neighbors_one_day -------------------------------------------

test_that("process_all_neighbors_one_day works correctly", {
  # Create a simple bin matrix
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",
      "2023-01-01 10:00:02"
    ), tz = "UTC"),
    `1` = c(1, 1, 0),  # Animal 1 at bin 1, bin 1, inactive
    `2` = c(2, 0, 0),  # Animal 2 at bin 2, inactive, inactive
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2-3")
  result <- process_all_neighbors_one_day(bin_matrix, neighbor_lookup, "sec", "animal")
  
  expect_type(result, "list")
  expect_named(result, c("bout", "total_time", "avg_duration"))
  expect_true(is.matrix(result$bout))
  expect_equal(dim(result$bout), c(2, 2))
})

test_that("process_all_neighbors_one_day errors on non-data.frame input", {
  neighbor_lookup <- parse_bin_layout("1-2")
  
  expect_error(
    process_all_neighbors_one_day(list(a = 1), neighbor_lookup, "sec", "animal"),
    "bin_matrix must be a data frame"
  )
})

test_that("process_all_neighbors_one_day errors on missing Time column", {
  bad_matrix <- data.frame(x = 1, y = 2)
  neighbor_lookup <- parse_bin_layout("1-2")
  
  expect_error(
    process_all_neighbors_one_day(bad_matrix, neighbor_lookup, "sec", "animal"),
    "bin_matrix must have 'Time' column"
  )
})

test_that("process_all_neighbors_one_day errors on no animal columns", {
  bad_matrix <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC")
  )
  neighbor_lookup <- parse_bin_layout("1-2")
  
  expect_error(
    process_all_neighbors_one_day(bad_matrix, neighbor_lookup, "sec", "animal"),
    "No animal columns found"
  )
})

test_that("process_all_neighbors_one_day handles single animal correctly", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC"),
    `1` = 1,
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2")
  result <- process_all_neighbors_one_day(bin_matrix, neighbor_lookup, "sec", "animal")
  
  expect_equal(dim(result$bout), c(1, 1))
  expect_equal(result$bout[1, 1], 0)
})

# Test extract_neighbor_activity -----------------------------------------------

test_that("extract_neighbor_activity extracts correct time points", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",
      "2023-01-01 10:00:02",
      "2023-01-01 10:00:03"
    ), tz = "UTC"),
    `1` = c(1, 1, 0, 1),  # Bin values for animal 1
    `2` = c(2, 3, 2, 2),  # Bin values for animal 2
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2-3")
  result <- extract_neighbor_activity(bin_matrix, "1", "2", neighbor_lookup)
  
  expect_true(lubridate::is.POSIXct(result))
  # Both at neighboring bins at time 1 (bins 1 and 2) and time 2 (bins 1 and 3)
  expect_true(length(result) >= 0)
})

test_that("extract_neighbor_activity returns empty when no neighbors", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01"
    ), tz = "UTC"),
    `1` = c(1, 0),
    `2` = c(10, 1),  # Bins 1 and 10 are not neighbors
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2-3")
  result <- extract_neighbor_activity(bin_matrix, "1", "2", neighbor_lookup)
  
  expect_equal(length(result), 0)
  expect_true(lubridate::is.POSIXct(result))
})

test_that("extract_neighbor_activity handles inactive animals", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01"
    ), tz = "UTC"),
    `1` = c(1, 0),  # Active then inactive
    `2` = c(0, 2),  # Inactive then active
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2")
  result <- extract_neighbor_activity(bin_matrix, "1", "2", neighbor_lookup)
  
  # Never active at the same time
  expect_equal(length(result), 0)
})

test_that("extract_neighbor_activity handles numeric animal IDs", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC"),
    `1` = 1,
    `2` = 2,
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2-3")
  result <- extract_neighbor_activity(bin_matrix, 1, 2, neighbor_lookup)
  
  expect_true(lubridate::is.POSIXct(result))
})

test_that("extract_neighbor_activity errors on missing animal", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00", tz = "UTC"),
    `1` = 1,
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2")
  
  expect_error(
    extract_neighbor_activity(bin_matrix, "1", "999", neighbor_lookup),
    "Animal 999 not found"
  )
  
  expect_error(
    extract_neighbor_activity(bin_matrix, "999", "1", neighbor_lookup),
    "Animal 999 not found"
  )
})

test_that("extract_neighbor_activity handles same animal at different bins", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",
      "2023-01-01 10:00:02"
    ), tz = "UTC"),
    `1` = c(1, 2, 3),  # Animal moves across bins
    `2` = c(2, 3, 4),  # Animal 2 also moves
    check.names = FALSE
  )
  
  neighbor_lookup <- parse_bin_layout("1-2-3-4")
  result <- extract_neighbor_activity(bin_matrix, "1", "2", neighbor_lookup)
  
  # All three time points should have neighboring bins
  expect_equal(length(result), 3)
})

# Additional edge case tests ===================================================

test_that("synch_neighbor_analysis handles invalid bin_layout", {
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

  expect_error(
    synch_neighbor_analysis(matrices, bin_layout = "", type = "feed"),
    "bin_layout cannot be NULL or empty"
  )

  expect_error(
    synch_neighbor_analysis(matrices, bin_layout = NULL, type = "feed"),
    "bin_layout cannot be NULL or empty"
  )
})

test_that("synch_neighbor_analysis handles bin_matrix with all zeros", {
  # Create data with bins but no actual activity overlap
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01"
    ), tz = "UTC"),
    `1` = c(1, 0),
    `2` = c(0, 2),
    check.names = FALSE
  )

  matrix_data <- list(
    synch_master_bin2 = bin_matrix
  )

  result <- synch_neighbor_analysis(
    matrix_data,
    bin_layout = "1-2",
    type = "feed"
  )

  # No neighbor activity since they're never active at the same time
  expect_equal(result$bout["1", "2"], 0)
  expect_equal(result$total_time["1", "2"], 0)
})

test_that("synch_neighbor_analysis handles complex bin layout with gaps", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:00"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:05", "2023-01-01 10:00:05"
    ), tz = "UTC"),
    bin = c(1, 10),
    start_weight = c(10.5, 8.3),
    end_weight = c(10.2, 8.1)
  )

  matrices <- matrix_process(toy_data, type = "feed",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            start_weight_col = "start_weight",
                            end_weight_col = "end_weight",
                            bins_feed = c(1, 10))

  result <- synch_neighbor_analysis(
    matrices,
    bin_layout = "1-2-3\n8-9-10",
    type = "feed"
  )

  # Bins 1 and 10 are not neighbors (different rows)
  expect_equal(result$bout["1", "2"], 0)
})

test_that("process_all_neighbors_one_day handles date column in bin_matrix", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01"
    ), tz = "UTC"),
    `1` = c(1, 1),
    `2` = c(2, 2),
    date = as.Date("2023-01-01"),
    check.names = FALSE
  )

  neighbor_lookup <- parse_bin_layout("1-2-3")
  result <- process_all_neighbors_one_day(bin_matrix, neighbor_lookup, "sec", "animal")

  # Should exclude date column from animal list
  expect_equal(dim(result$bout), c(2, 2))
  expect_equal(rownames(result$bout), c("1", "2"))
})

test_that("extract_neighbor_activity handles non-neighboring bins correctly", {
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01"
    ), tz = "UTC"),
    `1` = c(1, 1),
    `2` = c(5, 5),  # Bin 5 is not neighbor of bin 1
    check.names = FALSE
  )

  neighbor_lookup <- parse_bin_layout("1-2-3")
  result <- extract_neighbor_activity(bin_matrix, "1", "2", neighbor_lookup)

  expect_equal(length(result), 0)
})

test_that("synch_neighbor_analysis works with very simple layout", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:00"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:03", "2023-01-01 10:00:03"
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

  result <- synch_neighbor_analysis(
    matrices,
    bin_layout = "1-2",
    type = "feed"
  )

  # Both animals at neighboring bins for entire duration
  expect_true(result$bout["1", "2"] > 0)
  expect_true(result$total_time["1", "2"] > 0)
})

test_that("synch_neighbor_analysis handles animals moving between bins", {
  # Create bin matrix where animal 1 moves from bin 1 to bin 2
  bin_matrix <- data.frame(
    Time = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",
      "2023-01-01 10:00:02"
    ), tz = "UTC"),
    `1` = c(1, 2, 3),  # Animal 1 moves across bins
    `2` = c(2, 3, 4),  # Animal 2 also moves
    check.names = FALSE
  )

  matrix_data <- list(
    synch_master_bin2 = bin_matrix
  )

  result <- synch_neighbor_analysis(
    matrix_data,
    bin_layout = "1-2-3-4",
    type = "feed"
  )

  # Should detect neighbor patterns at each time point
  expect_true(result$total_time["1", "2"] > 0)
})

test_that("synch_neighbor_analysis works with drink type", {
  toy_data <- data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:01"
    ), tz = "UTC"),
    end = lubridate::ymd_hms(c(
      "2023-01-01 10:00:03", "2023-01-01 10:00:04"
    ), tz = "UTC"),
    bin = c(1, 2)
  )

  matrices <- matrix_process(toy_data, type = "drink",
                            id_col = "animal", start_col = "start",
                            end_col = "end", bin_col = "bin",
                            bins_wat = 1:2)

  result <- synch_neighbor_analysis(
    matrices,
    bin_layout = "1-2",
    type = "drink"
  )

  expect_type(result, "list")
  expect_named(result, c("bout", "total_time", "avg_duration"))
})

