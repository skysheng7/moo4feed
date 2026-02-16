# Test create_empty_pair_matrix ------------------------------------------------

test_that("create_empty_pair_matrix creates correct matrix structure", {
  animal_ids <- c("1", "2", "3")
  mat <- create_empty_pair_matrix(animal_ids)
  
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), 3)
  expect_equal(ncol(mat), 3)
  expect_equal(rownames(mat), animal_ids)
  expect_equal(colnames(mat), animal_ids)
  expect_true(all(mat == 0))
})

test_that("create_empty_pair_matrix works with numeric IDs", {
  animal_ids <- c(101, 102, 103)
  mat <- create_empty_pair_matrix(animal_ids)
  
  expect_equal(rownames(mat), as.character(animal_ids))
  expect_equal(colnames(mat), as.character(animal_ids))
})

test_that("create_empty_pair_matrix works with single animal", {
  animal_ids <- c("1")
  mat <- create_empty_pair_matrix(animal_ids)
  
  expect_equal(dim(mat), c(1, 1))
  expect_equal(mat[1, 1], 0)
})

test_that("create_empty_pair_matrix errors on empty input", {
  expect_error(
    create_empty_pair_matrix(character(0)),
    "animal_ids cannot be empty"
  )
})

# Test calculate_bout_duration -------------------------------------------------

test_that("calculate_bout_duration works with single time point (sec)", {
  time_vec <- lubridate::ymd_hms("2023-01-01 10:00:00")
  result <- calculate_bout_duration(time_vec, "sec")
  
  expect_equal(result$bout, 1)
  expect_equal(result$total_time, 1)
  expect_equal(result$avg_duration, 1)
})

test_that("calculate_bout_duration works with single time point (min)", {
  time_vec <- lubridate::ymd_hms("2023-01-01 10:00:00")
  result <- calculate_bout_duration(time_vec, "min")
  
  expect_equal(result$bout, 1)
  expect_equal(result$total_time, 1/60)
  expect_equal(result$avg_duration, 1/60)
})

test_that("calculate_bout_duration works with continuous sequence (sec)", {
  time_vec <- lubridate::ymd_hms(c(
    "2023-01-01 10:00:00",
    "2023-01-01 10:00:01",
    "2023-01-01 10:00:02"
  ))
  result <- calculate_bout_duration(time_vec, "sec")
  
  expect_equal(result$bout, 1)
  expect_equal(result$total_time, 3)
  expect_equal(result$avg_duration, 3)
})

test_that("calculate_bout_duration detects multiple bouts (sec)", {
  time_vec <- lubridate::ymd_hms(c(
    "2023-01-01 10:00:00",
    "2023-01-01 10:00:01",
    "2023-01-01 10:00:05",  # Gap > 1 sec, new bout
    "2023-01-01 10:00:06"
  ))
  result <- calculate_bout_duration(time_vec, "sec")
  
  expect_equal(result$bout, 2)
  expect_equal(result$total_time, 4)
  expect_equal(result$avg_duration, 2)
})

test_that("calculate_bout_duration works with minute resolution", {
  time_vec <- lubridate::ymd_hms(c(
    "2023-01-01 10:00:00",
    "2023-01-01 10:01:00",
    "2023-01-01 10:02:00",
    "2023-01-01 10:05:00"  # Gap > 60 sec, new bout
  ))
  result <- calculate_bout_duration(time_vec, "min")
  
  expect_equal(result$bout, 2)
  expect_equal(result$total_time, 4)
  expect_equal(result$avg_duration, 2)
})

test_that("calculate_bout_duration handles empty time vector", {
  time_vec <- lubridate::ymd_hms(character(0))
  result <- calculate_bout_duration(time_vec, "sec")
  
  expect_equal(result$bout, 0)
  expect_equal(result$total_time, 0)
  expect_equal(result$avg_duration, 0)
})

test_that("calculate_bout_duration handles unsorted time vector", {
  time_vec <- lubridate::ymd_hms(c(
    "2023-01-01 10:00:02",
    "2023-01-01 10:00:00",
    "2023-01-01 10:00:01"
  ))
  result <- calculate_bout_duration(time_vec, "sec")
  
  expect_equal(result$bout, 1)
  expect_equal(result$total_time, 3)
})

test_that("calculate_bout_duration handles whitespace in resolution", {
  time_vec <- lubridate::ymd_hms("2023-01-01 10:00:00")
  result <- calculate_bout_duration(time_vec, " SEC ")
  
  expect_equal(result$bout, 1)
})

test_that("calculate_bout_duration errors on non-POSIXct input", {
  expect_error(
    calculate_bout_duration(c(1, 2, 3), "sec"),
    "time_vector must be POSIXct"
  )
})

# Test parse_bin_layout --------------------------------------------------------

test_that("parse_bin_layout works with single row layout", {
  layout <- "1-2-3"
  result <- parse_bin_layout(layout)
  
  expect_true(is.list(result))
  expect_equal(names(result), c("1", "2", "3"))
  expect_equal(result[["1"]], 2)
  expect_equal(result[["2"]], c(1, 3))
  expect_equal(result[["3"]], 2)
})

test_that("parse_bin_layout works with multi-row layout", {
  layout <- "1-2-3\n4-5-6"
  result <- parse_bin_layout(layout)
  
  expect_equal(names(result), c("1", "2", "3", "4", "5", "6"))
  # Row 1 neighbors
  expect_equal(result[["1"]], 2)
  expect_equal(result[["2"]], c(1, 3))
  expect_equal(result[["3"]], 2)
  # Row 2 neighbors
  expect_equal(result[["4"]], 5)
  expect_equal(result[["5"]], c(4, 6))
  expect_equal(result[["6"]], 5)
})

test_that("parse_bin_layout handles uneven rows", {
  layout <- "1-2\n3-4-5"
  result <- parse_bin_layout(layout)
  
  expect_equal(names(result), c("1", "2", "3", "4", "5"))
  expect_equal(result[["1"]], 2)
  expect_equal(result[["2"]], 1)
  expect_equal(result[["3"]], 4)
  expect_equal(result[["4"]], c(3, 5))
  expect_equal(result[["5"]], 4)
})

test_that("parse_bin_layout handles single bin", {
  layout <- "1"
  result <- parse_bin_layout(layout)
  
  expect_equal(names(result), "1")
  expect_equal(length(result[["1"]]), 0)  # No neighbors
})

test_that("parse_bin_layout handles extra whitespace", {
  layout <- "  1 - 2 - 3  \n  4 - 5  "
  result <- parse_bin_layout(layout)
  
  expect_equal(names(result), c("1", "2", "3", "4", "5"))
})

test_that("parse_bin_layout errors on NULL input", {
  expect_error(
    parse_bin_layout(NULL),
    "bin_layout cannot be NULL or empty"
  )
})

test_that("parse_bin_layout errors on empty string", {
  expect_error(
    parse_bin_layout(""),
    "bin_layout cannot be NULL or empty"
  )
})

test_that("parse_bin_layout errors on only whitespace", {
  expect_error(
    parse_bin_layout("\n\n"),
    "bin_layout must contain at least one row"
  )
})

# Test is_neighbour ------------------------------------------------------------

test_that("is_neighbour detects horizontal neighbors", {
  layout <- "1-2-3"
  lookup <- parse_bin_layout(layout)
  
  expect_true(is_neighbour(1, 2, lookup))
  expect_true(is_neighbour(2, 1, lookup))
  expect_true(is_neighbour(2, 3, lookup))
  expect_false(is_neighbour(1, 3, lookup))  # Not adjacent
})

test_that("is_neighbour detects non-neighbors across rows", {
  layout <- "1-2\n3-4"
  lookup <- parse_bin_layout(layout)
  
  # Bins in different rows are not neighbors
  expect_false(is_neighbour(1, 3, lookup))
  expect_false(is_neighbour(2, 4, lookup))
  expect_false(is_neighbour(1, 4, lookup))
})

test_that("is_neighbour returns FALSE for same bin", {
  layout <- "1-2-3"
  lookup <- parse_bin_layout(layout)
  
  expect_false(is_neighbour(1, 1, lookup))
  expect_false(is_neighbour(2, 2, lookup))
})

test_that("is_neighbour handles NA values", {
  layout <- "1-2-3"
  lookup <- parse_bin_layout(layout)
  
  expect_false(is_neighbour(NA, 2, lookup))
  expect_false(is_neighbour(1, NA, lookup))
  expect_false(is_neighbour(NA, NA, lookup))
})

test_that("is_neighbour handles non-existent bins", {
  layout <- "1-2-3"
  lookup <- parse_bin_layout(layout)
  
  expect_false(is_neighbour(1, 99, lookup))
  expect_false(is_neighbour(99, 1, lookup))
  expect_false(is_neighbour(99, 100, lookup))
})

test_that("is_neighbour works with complex layout", {
  layout <- "1-2-3\n4-5\n6-7-8-9"
  lookup <- parse_bin_layout(layout)

  # Row 1
  expect_true(is_neighbour(1, 2, lookup))
  expect_true(is_neighbour(2, 3, lookup))
  # Row 2
  expect_true(is_neighbour(4, 5, lookup))
  # Row 3
  expect_true(is_neighbour(6, 7, lookup))
  expect_true(is_neighbour(7, 8, lookup))
  expect_true(is_neighbour(8, 9, lookup))
  # Cross-row (should be FALSE)
  expect_false(is_neighbour(3, 4, lookup))
  expect_false(is_neighbour(5, 6, lookup))
})

# Additional edge case tests ===================================================

test_that("calculate_bout_duration handles very large time gaps", {
  time_vec <- lubridate::ymd_hms(c(
    "2023-01-01 10:00:00",
    "2023-01-01 10:00:01",
    "2023-01-01 12:00:00"  # 2 hour gap
  ))
  result <- calculate_bout_duration(time_vec, "sec")

  expect_equal(result$bout, 2)
  expect_equal(result$total_time, 3)
  expect_equal(result$avg_duration, 1.5)
})

test_that("calculate_bout_duration handles duplicate timestamps", {
  time_vec <- lubridate::ymd_hms(c(
    "2023-01-01 10:00:00",
    "2023-01-01 10:00:00",  # Duplicate
    "2023-01-01 10:00:01"
  ))
  result <- calculate_bout_duration(time_vec, "sec")

  expect_equal(result$bout, 1)
  expect_equal(result$total_time, 3)
})

test_that("calculate_bout_duration errors on invalid resolution", {
  time_vec <- lubridate::ymd_hms("2023-01-01 10:00:00")

  expect_error(
    calculate_bout_duration(time_vec, "hour"),
    "'arg' should be one of"
  )
})

test_that("create_empty_pair_matrix handles mixed character and numeric IDs", {
  animal_ids <- c(1, 10, 100)
  mat <- create_empty_pair_matrix(animal_ids)

  expect_equal(rownames(mat), c("1", "10", "100"))
  expect_equal(colnames(mat), c("1", "10", "100"))
})

test_that("parse_bin_layout handles layout with gaps in bin numbers", {
  layout <- "1-5-10"
  result <- parse_bin_layout(layout)

  expect_equal(names(result), c("1", "5", "10"))
  expect_equal(result[["1"]], 5)
  expect_equal(result[["5"]], c(1, 10))
  expect_equal(result[["10"]], 5)
})

test_that("parse_bin_layout handles layout with single bin per row", {
  layout <- "1\n2\n3"
  result <- parse_bin_layout(layout)

  expect_equal(names(result), c("1", "2", "3"))
  expect_equal(length(result[["1"]]), 0)  # No neighbors
  expect_equal(length(result[["2"]]), 0)
  expect_equal(length(result[["3"]]), 0)
})

test_that("is_neighbour handles numeric bin IDs", {
  layout <- "1-2-3"
  lookup <- parse_bin_layout(layout)

  # Test with numeric inputs
  expect_true(is_neighbour(1, 2, lookup))
  expect_true(is_neighbour(2, 3, lookup))
  expect_false(is_neighbour(1, 3, lookup))
})

test_that("is_neighbour handles zero as bin number", {
  layout <- "0-1-2"
  lookup <- parse_bin_layout(layout)

  expect_true(is_neighbour(0, 1, lookup))
  expect_true(is_neighbour(1, 2, lookup))
  expect_false(is_neighbour(0, 2, lookup))
})

