# Tests for synch_synchronicity_analysis.R

# Helper function to create test data
create_test_animal_data <- function() {
  list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01", "2023-01-01 10:00:03")),
      "1" = c(1, 1, 0),
      "2" = c(1, 1, 1),
      total_animal_num = c(2, 2, 1),
      check.names = FALSE
    )
  )
}

create_test_bin_data <- function() {
  list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01", "2023-01-01 10:00:03")),
      "1" = c(201, 201, 0),
      "2" = c(202, 202, 203),
      total_animal_num = c(2, 2, 1),
      check.names = FALSE
    )
  )
}

# Tests for empty_animal_matrix
test_that("empty_animal_matrix works with valid input", {
  test_data <- data.frame(cow = c(1, 2, 3), other = c("a", "b", "c"))
  result <- moo4feed:::empty_animal_matrix(test_data, id_col = "cow")
  
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(names(result), c("empty_matrix", "animal_num"))
  
  expect_equal(result$animal_num, 3)
  expect_equal(dim(result$empty_matrix), c(3, 3))
  expect_equal(rownames(result$empty_matrix), c("1", "2", "3"))
  expect_equal(colnames(result$empty_matrix), c("1", "2", "3"))
  expect_true(all(result$empty_matrix == 0))
})

test_that("empty_animal_matrix handles single animal", {
  test_data <- data.frame(cow = 1, other = "a")
  result <- moo4feed:::empty_animal_matrix(test_data, id_col = "cow")
  
  expect_equal(result$animal_num, 1)
  expect_equal(dim(result$empty_matrix), c(1, 1))
  expect_equal(rownames(result$empty_matrix), "1")
  expect_equal(colnames(result$empty_matrix), "1")
})

test_that("empty_animal_matrix handles duplicate animals", {
  test_data <- data.frame(cow = c(1, 1, 2, 2), other = c("a", "b", "c", "d"))
  result <- moo4feed:::empty_animal_matrix(test_data, id_col = "cow")
  
  expect_equal(result$animal_num, 2)
  expect_equal(dim(result$empty_matrix), c(2, 2))
})

test_that("empty_animal_matrix errors on invalid input", {
  expect_error(moo4feed:::empty_animal_matrix(NULL), "`master_data` must be a data frame")
  expect_error(moo4feed:::empty_animal_matrix("not a df"), "`master_data` must be a data frame")
  expect_error(moo4feed:::empty_animal_matrix(data.frame()), "Column 'cow' not found")
  expect_error(moo4feed:::empty_animal_matrix(data.frame(x = 1), id_col = "cow"), "Column 'cow' not found")
  expect_error(moo4feed:::empty_animal_matrix(data.frame(cow = character(0)), id_col = "cow"), "`master_data` cannot be empty")
})

# NEW TESTS FOR COVERAGE
test_that("empty_animal_matrix handles zero animals found", {
  # Data frame with rows but no valid animal IDs
  test_data <- data.frame(cow = character(0), other = character(0))
  expect_error(moo4feed:::empty_animal_matrix(test_data, id_col = "cow"), "`master_data` cannot be empty")
})

# Tests for calculate_bout_duration
test_that("calculate_bout_duration works with continuous time", {
  test_data <- data.frame(
    Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01", "2023-01-01 10:00:02")),
    cow1 = c(1, 1, 1),
    cow2 = c(1, 1, 1)
  )
  
  result <- moo4feed:::calculate_bout_duration(test_data)
  
  expect_true("bout" %in% names(result))
  expect_true("duration" %in% names(result))
  expect_equal(result$bout, c(1, 1, 1))
  expect_equal(result$duration, c(1, 2, 3))
})

test_that("calculate_bout_duration works with gaps", {
  test_data <- data.frame(
    Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01", "2023-01-01 10:00:03")),
    cow1 = c(1, 1, 1),
    cow2 = c(1, 1, 1)
  )
  
  result <- moo4feed:::calculate_bout_duration(test_data)
  
  expect_equal(result$bout, c(1, 1, 2))
  expect_equal(result$duration, c(1, 2, 1))
})

test_that("calculate_bout_duration handles single row", {
  test_data <- data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
    cow1 = 1,
    cow2 = 1
  )
  
  result <- moo4feed:::calculate_bout_duration(test_data)
  
  expect_equal(result$bout, 1)
  expect_equal(result$duration, 1)
})

test_that("calculate_bout_duration sorts by time", {
  test_data <- data.frame(
    Time = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    cow1 = c(1, 1, 1),
    cow2 = c(1, 1, 1)
  )
  
  result <- moo4feed:::calculate_bout_duration(test_data)
  
  # Should be sorted and continuous
  expect_equal(result$bout, c(1, 1, 1))
  expect_equal(result$duration, c(1, 2, 3))
  expect_true(all(result$Time == sort(result$Time)))
})

test_that("calculate_bout_duration errors on invalid input", {
  expect_error(moo4feed:::calculate_bout_duration(NULL), "`cur_worksheet` must be a data frame")
  expect_error(moo4feed:::calculate_bout_duration("not a df"), "`cur_worksheet` must be a data frame")
  expect_error(moo4feed:::calculate_bout_duration(data.frame()), "`cur_worksheet` must have a 'Time' column")
  expect_error(moo4feed:::calculate_bout_duration(data.frame(x = 1)), "`cur_worksheet` must have a 'Time' column")
  expect_error(moo4feed:::calculate_bout_duration(data.frame(Time = "not a time")), "'Time' column must be POSIXct")
})

# NEW TESTS FOR COVERAGE
test_that("calculate_bout_duration handles empty data frame", {
  test_data <- data.frame(Time = lubridate::ymd_hms(character(0)))
  expect_error(moo4feed:::calculate_bout_duration(test_data), "`cur_worksheet` cannot be empty")
})

# Tests for paired_synchronicity_analysis
test_that("paired_synchronicity_analysis works with valid data", {
  animal_data <- create_test_animal_data()
  bin_data <- create_test_bin_data()
  
  result <- moo4feed:::paired_synchronicity_analysis(animal_data, bin_data, 2)
  
  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_equal(names(result), c("paired_bout", "paired_total_time", "paired_average_dur"))
  
  # Each result should be a list with one element (one date)
  expect_equal(length(result$paired_bout), 1)
  expect_equal(names(result$paired_bout), "2023-01-01")
  
  # Each matrix should be 2x2
  expect_equal(dim(result$paired_bout[["2023-01-01"]]), c(2, 2))
  expect_equal(dim(result$paired_total_time[["2023-01-01"]]), c(2, 2))
  expect_equal(dim(result$paired_average_dur[["2023-01-01"]]), c(2, 2))
})

test_that("paired_synchronicity_analysis handles empty data", {
  empty_data <- list("2023-01-01" = data.frame())
  
  result <- moo4feed:::paired_synchronicity_analysis(empty_data, empty_data, 2)
  
  expect_equal(length(result$paired_bout), 1)
  expect_true(all(result$paired_bout[["2023-01-01"]] == 0))
})

test_that("paired_synchronicity_analysis handles no multiple animals", {
  # Data where total_animal_num is never > 1
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(1, 0),
      "2" = c(0, 1),
      total_animal_num = c(1, 1),
      check.names = FALSE
    )
  )
  bin_data <- create_test_bin_data()
  
  result <- moo4feed:::paired_synchronicity_analysis(animal_data, bin_data, 2)
  
  # Should have zero matrices
  expect_true(all(result$paired_bout[["2023-01-01"]] == 0))
  expect_true(all(result$paired_total_time[["2023-01-01"]] == 0))
  expect_true(all(result$paired_average_dur[["2023-01-01"]] == 0))
})

test_that("paired_synchronicity_analysis errors on invalid input", {
  expect_error(moo4feed:::paired_synchronicity_analysis(NULL, list(), 2), "`synch_master_animal` cannot be NULL or empty")
  expect_error(moo4feed:::paired_synchronicity_analysis(list(), list(), -1), "`synch_master_animal` cannot be NULL or empty")
  expect_error(moo4feed:::paired_synchronicity_analysis(list(a = data.frame()), list(a = data.frame()), "not a number"), "`animal_num` must be a positive number")
})

# NEW TESTS FOR COVERAGE
test_that("paired_synchronicity_analysis handles data without total_animal_num column", {
  # Data without total_animal_num should still work
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(1, 1),
      "2" = c(1, 1),
      check.names = FALSE
    )
  )
  bin_data <- create_test_bin_data()
  
  result <- moo4feed:::paired_synchronicity_analysis(animal_data, bin_data, 2)
  
  expect_type(result, "list")
  expect_equal(length(result), 3)
})

# Tests for neighbor_synchronicity_analysis  
test_that("neighbor_synchronicity_analysis works with valid data", {
  animal_data <- create_test_animal_data()
  bin_data <- create_test_bin_data()
  
  result <- moo4feed:::neighbor_synchronicity_analysis(animal_data, bin_data, 2)
  
  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_equal(names(result), c("neighbor_bout", "neighbor_total_time", "neighbor_average_dur"))
  
  # Each result should be a list with one element (one date)
  expect_equal(length(result$neighbor_bout), 1)
  expect_equal(names(result$neighbor_bout), "2023-01-01")
  
  # Each matrix should be 2x2
  expect_equal(dim(result$neighbor_bout[["2023-01-01"]]), c(2, 2))
  expect_equal(dim(result$neighbor_total_time[["2023-01-01"]]), c(2, 2))
  expect_equal(dim(result$neighbor_average_dur[["2023-01-01"]]), c(2, 2))
})

test_that("neighbor_synchronicity_analysis detects adjacent bins", {
  # Create data where animals are at adjacent bins (201, 202)
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(1, 1),
      "2" = c(1, 1),
      total_animal_num = c(2, 2),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(201, 201),
      "2" = c(202, 202),  # Adjacent to bin 201
      total_animal_num = c(2, 2),
      check.names = FALSE
    )
  )
  
  result <- moo4feed:::neighbor_synchronicity_analysis(animal_data, bin_data, 2)
  
  # Should detect neighboring behavior
  expect_true(result$neighbor_bout[["2023-01-01"]][1, 2] > 0)
  expect_true(result$neighbor_total_time[["2023-01-01"]][1, 2] > 0)
})

test_that("neighbor_synchronicity_analysis ignores non-adjacent bins", {
  # Create data where animals are at non-adjacent bins (201, 203)
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(1, 1),
      "2" = c(1, 1),
      total_animal_num = c(2, 2),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(201, 201),
      "2" = c(203, 203),  # Not adjacent to bin 201
      total_animal_num = c(2, 2),
      check.names = FALSE
    )
  )
  
  result <- moo4feed:::neighbor_synchronicity_analysis(animal_data, bin_data, 2)
  
  # Should not detect neighboring behavior
  expect_equal(result$neighbor_bout[["2023-01-01"]][1, 2], 0)
  expect_equal(result$neighbor_total_time[["2023-01-01"]][1, 2], 0)
})

test_that("neighbor_synchronicity_analysis errors on invalid input", {
  expect_error(moo4feed:::neighbor_synchronicity_analysis(NULL, list(), 2), "`synch_master_animal` cannot be NULL or empty")
  expect_error(moo4feed:::neighbor_synchronicity_analysis(list(a = data.frame()), NULL, 2), "`synch_master_bin` cannot be NULL or empty")
  expect_error(moo4feed:::neighbor_synchronicity_analysis(list(a = 1), list(a = 1, b = 2), 2), "`synch_master_animal` and `synch_master_bin` must have the same length")
  expect_error(moo4feed:::neighbor_synchronicity_analysis(list(), list(), -1), "`synch_master_animal` cannot be NULL or empty")
})

# NEW TESTS FOR COVERAGE
test_that("neighbor_synchronicity_analysis handles empty data frame", {
  empty_data <- list("2023-01-01" = data.frame())
  
  result <- moo4feed:::neighbor_synchronicity_analysis(empty_data, empty_data, 2)
  
  expect_equal(length(result$neighbor_bout), 1)
  expect_true(all(result$neighbor_bout[["2023-01-01"]] == 0))
})

test_that("neighbor_synchronicity_analysis handles data with fewer than 2 animals", {
  # Data with only one animal column
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(1, 1),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
      "1" = c(201, 201),
      check.names = FALSE
    )
  )
  
  result <- moo4feed:::neighbor_synchronicity_analysis(animal_data, bin_data, 2)
  
  # Should return zero matrices since no pairs can be formed
  expect_true(all(result$neighbor_bout[["2023-01-01"]] == 0))
})

# Tests for synchronicity_matrix_process
test_that("synchronicity_matrix_process works with valid data", {
  animal_data <- create_test_animal_data()
  bin_data <- create_test_bin_data()
  
  result <- moo4feed::synchronicity_matrix_process(animal_data, bin_data)
  
  expect_type(result, "list")
  expect_equal(length(result), 6)
  expect_equal(names(result), c("paired_bout", "paired_total_time", "paired_average_dur", 
                               "neighbor_bout", "neighbor_total_time", "neighbor_average_dur"))
  
  # Each component should be a list with matrices
  for (component in names(result)) {
    expect_equal(length(result[[component]]), 1)
    expect_equal(names(result[[component]]), "2023-01-01")
    expect_equal(dim(result[[component]][["2023-01-01"]]), c(2, 2))
  }
})

test_that("synchronicity_matrix_process handles multiple dates", {
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      "1" = 1, "2" = 1,
      total_animal_num = 2,
      check.names = FALSE
    ),
    "2023-01-02" = data.frame(
      Time = lubridate::ymd_hms("2023-01-02 10:00:00"),
      "1" = 1, "2" = 1,
      total_animal_num = 2,
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      "1" = 201, "2" = 202,
      total_animal_num = 2,
      check.names = FALSE
    ),
    "2023-01-02" = data.frame(
      Time = lubridate::ymd_hms("2023-01-02 10:00:00"),
      "1" = 201, "2" = 202,
      total_animal_num = 2,
      check.names = FALSE
    )
  )
  
  result <- moo4feed::synchronicity_matrix_process(animal_data, bin_data)
  
  # Should have 2 dates for each component
  for (component in names(result)) {
    expect_equal(length(result[[component]]), 2)
    expect_equal(names(result[[component]]), c("2023-01-01", "2023-01-02"))
  }
})

test_that("synchronicity_matrix_process handles many animals", {
  # Test with 4 animals
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      "1" = 1, "2" = 1, "3" = 1, "4" = 1,
      total_animal_num = 4,
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
      "1" = 201, "2" = 202, "3" = 203, "4" = 204,
      total_animal_num = 4,
      check.names = FALSE
    )
  )
  
  result <- moo4feed::synchronicity_matrix_process(animal_data, bin_data)
  
  # Should have 4x4 matrices
  for (component in names(result)) {
    expect_equal(dim(result[[component]][["2023-01-01"]]), c(4, 4))
  }
})

test_that("synchronicity_matrix_process errors on invalid input", {
  expect_error(moo4feed::synchronicity_matrix_process(NULL, list()), "`synch_master_animal` cannot be NULL or empty")
  expect_error(moo4feed::synchronicity_matrix_process(list(a = data.frame()), NULL), "`synch_master_bin` cannot be NULL or empty")
  expect_error(moo4feed::synchronicity_matrix_process(list(a = 1), list(a = 1, b = 2)), "`synch_master_animal` and `synch_master_bin` must have the same length")
  
  # Test with no animals
  empty_data <- list("2023-01-01" = data.frame(Time = lubridate::ymd_hms("2023-01-01 10:00:00")))
  expect_error(moo4feed::synchronicity_matrix_process(empty_data, empty_data), "No animals found")
  
  # Test with only one animal
  single_animal <- list("2023-01-01" = data.frame(
    Time = lubridate::ymd_hms("2023-01-01 10:00:00"),
    "1" = 1,
    check.names = FALSE
  ))
  expect_error(moo4feed::synchronicity_matrix_process(single_animal, single_animal), "At least 2 animals are required")
})

# NEW TESTS FOR COVERAGE
test_that("synchronicity_matrix_process handles all empty datasets", {
  # All datasets are NULL or empty
  empty_data <- list(
    "2023-01-01" = NULL,
    "2023-01-02" = data.frame()
  )
  
  expect_error(moo4feed::synchronicity_matrix_process(empty_data, empty_data), "No animals found")
})

test_that("synchronicity_matrix_process handles mixed empty and valid datasets", {
  # Some empty, some valid data
  mixed_data <- list(
    "2023-01-01" = data.frame(),  # Empty
    "2023-01-02" = data.frame(    # Valid
      Time = lubridate::ymd_hms("2023-01-02 10:00:00"),
      "1" = 1, "2" = 1,
      total_animal_num = 2,
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    "2023-01-01" = data.frame(),
    "2023-01-02" = data.frame(
      Time = lubridate::ymd_hms("2023-01-02 10:00:00"),
      "1" = 201, "2" = 202,
      total_animal_num = 2,
      check.names = FALSE
    )
  )
  
  result <- moo4feed::synchronicity_matrix_process(mixed_data, bin_data)
  
  expect_type(result, "list")
  expect_equal(length(result), 6)
})

# Integration tests
test_that("synchronicity analysis integrates with feed_drink_matrix_process output", {
  skip_if_not_installed("lubridate")
  
  # Create realistic interval data
  toy_data <- list(
    day1 = data.frame(
      cow = c(1, 1, 2, 2),
      start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:03", 
                                   "2023-01-01 10:00:00", "2023-01-01 10:00:04")),
      end = lubridate::ymd_hms(c("2023-01-01 10:00:01", "2023-01-01 10:00:04", 
                                 "2023-01-01 10:00:02", "2023-01-01 10:00:05")),
      bin = c(1, 2, 3, 1)
    )
  )
  
  # Process through feed_drink_matrix_process
  processed_matrices <- moo4feed::feed_drink_matrix_process(
    toy_data,
    id_col = "cow", start_col = "start", end_col = "end", bin_col = "bin",
    bins_feed = 1:3, bins_wat = 101:102
  )
  
  # Should be able to run synchronicity analysis on the output
  result <- moo4feed::synchronicity_matrix_process(
    processed_matrices[[1]], 
    processed_matrices[[2]]
  )
  
  expect_type(result, "list")
  expect_equal(length(result), 6)
  
  # Verify matrix dimensions match number of animals
  for (component in names(result)) {
    expect_equal(dim(result[[component]][[1]]), c(2, 2))
  }
})

test_that("symmetry properties are maintained", {
  animal_data <- create_test_animal_data()
  bin_data <- create_test_bin_data()
  
  result <- moo4feed::synchronicity_matrix_process(animal_data, bin_data)
  
  # All matrices should be symmetric
  for (component in names(result)) {
    matrix_data <- result[[component]][["2023-01-01"]]
    expect_true(all(matrix_data == t(matrix_data)), info = paste("Component", component, "is not symmetric"))
  }
})

test_that("edge case: animals never overlap", {
  # Create data where animals are never active at same time
  animal_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01", 
                                  "2023-01-01 10:00:02", "2023-01-01 10:00:03")),
      "1" = c(1, 1, 0, 0),
      "2" = c(0, 0, 1, 1),
      total_animal_num = c(1, 1, 1, 1),
      check.names = FALSE
    )
  )
  
  bin_data <- list(
    "2023-01-01" = data.frame(
      Time = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01", 
                                  "2023-01-01 10:00:02", "2023-01-01 10:00:03")),
      "1" = c(201, 201, 0, 0),
      "2" = c(0, 0, 202, 202),
      total_animal_num = c(1, 1, 1, 1),
      check.names = FALSE
    )
  )
  
  result <- moo4feed::synchronicity_matrix_process(animal_data, bin_data)
  
  # All synchronicity measures should be zero
  for (component in names(result)) {
    matrix_data <- result[[component]][["2023-01-01"]]
    # Diagonal can be ignored, check off-diagonal elements
    expect_equal(matrix_data[1, 2], 0, info = paste("Component", component, "should be zero"))
    expect_equal(matrix_data[2, 1], 0, info = paste("Component", component, "should be zero"))
  }
}) 