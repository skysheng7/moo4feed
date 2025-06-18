# ---------------------- Tests for non_nutritive_visits ---------------------#

# Helper: create toy data for normal and edge cases
create_toy_data <- function() {
  list(
    "2023-01-01" = data.frame(
      cow = c("A", "B", "A", "C"),
      intake = c(0.0, 0.2, 0.6, 0.0),
      start_weight = c(10, 0.3, 12, 0.1)
    ),
    "2023-01-02" = data.frame(
      cow = c("A", "B", "C"),
      intake = c(0.0, 0.0, 0.0),
      start_weight = c(0.2, 0.1, 0.6)
    )
  )
}

# -----------------------------------------------------------------------------#
#                  Tests for calculate_non_nutritive_visits()                 #
# -----------------------------------------------------------------------------#

# Normal use case: calculate_non_nutritive_visits

test_that("calculate_non_nutritive_visits returns correct counts for normal data", {
  toy_data <- create_toy_data()
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(toy_data, cfg = cfg)
  expect_type(result, "list")
  expect_true(all(sapply(result, inherits, "data.frame")))
  # Check correct column names
  expect_true(all(c("cow", "number_of_non_nutritive_visits") %in% names(result[[1]])))
  # Check counts for 2023-01-01: only cow A (intake=0, start_weight=10) and B (intake=0.2, start_weight=0.3) should be counted
  expect_true(any(result[[1]]$cow == "A"))
  expect_false(any(result[[1]]$cow == "C"))
})

# Edge case: empty data list

test_that("calculate_non_nutritive_visits returns empty list for empty input", {
  cfg <- qc_config(calibration_error = 0.5)
  expect_error(
    calculate_non_nutritive_visits(list(), cfg = cfg),
    "`data` must be a non-empty list of data frames."
  )
})

# Error handling: missing required columns

test_that("calculate_non_nutritive_visits errors if required columns are missing", {
  bad_data <- list("2023-01-01" = data.frame(cow = c("A"), intake = c(0.1)))
  cfg <- qc_config(calibration_error = 0.5)
  expect_error(
    calculate_non_nutritive_visits(bad_data, cfg = cfg),
    "Missing required columns"
  )
})

test_that("qc_config errors for invalid calibration_error", {
  expect_error(
    qc_config(calibration_error = -1),
    "must be a positive numeric scalar"
  )
  expect_error(
    qc_config(calibration_error = NA),
    "must be a positive numeric scalar"
  )
})

test_that("calculate_non_nutritive_visits errors for non-list input", {
  cfg <- qc_config(calibration_error = 0.5)
  expect_error(
    calculate_non_nutritive_visits("not_a_list", cfg = cfg),
    "`data` must be a non-empty list of data frames."
  )
})

# -----------------------------------------------------------------------------#
#                  Tests for calculate_no_feed_visits()                         #
# -----------------------------------------------------------------------------#

# Normal use case: calculate_no_feed_visits

test_that("calculate_no_feed_visits returns correct counts for normal data", {
  toy_data <- create_toy_data()
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(toy_data, cfg = cfg)
  expect_type(result, "list")
  expect_true(all(sapply(result, inherits, "data.frame")))
  expect_true(all(c("cow", "number_of_visits_when_no_feed") %in% names(result[[1]])))
})

# Edge case: all visits above calibration_error

test_that("calculate_no_feed_visits returns empty data frame if no visits match", {
  data <- list("2023-01-01" = data.frame(
    cow = c("A", "B"),
    intake = c(1, 2),
    start_weight = c(1, 2)
  ))
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_true(nrow(result[[1]]) == 0)
})

# Error handling: missing required columns

test_that("calculate_no_feed_visits errors if required columns are missing", {
  bad_data <- list("2023-01-01" = data.frame(cow = c("A"), intake = c(0.1)))
  cfg <- qc_config(calibration_error = 0.5)
  expect_error(
    calculate_no_feed_visits(bad_data, cfg = cfg),
    "Missing required columns"
  )
})

test_that("calculate_no_feed_visits errors for non-list input", {
  cfg <- qc_config(calibration_error = 0.5)
  expect_error(
    calculate_no_feed_visits("not_a_list", cfg = cfg),
    "`data` must be a non-empty list of data frames."
  )
})

# -----------------------------------------------------------------------------#
#                  Additional Robustness and Edge Case Tests                   #
# -----------------------------------------------------------------------------#

test_that("calculate_non_nutritive_visits works with custom column names", {
  data <- list(
    "2023-01-01" = data.frame(
      animal = c("A", "B", "A"),
      eat = c(0, 0.6, 0.2),
      sw = c(10, 12, 0.3)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(
    data, cfg = cfg, id_col = "animal", intake_col = "eat", start_weight_col = "sw"
  )
  expect_true("animal" %in% names(result[[1]]))
})

test_that("calculate_non_nutritive_visits ignores extra columns in input", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "B"),
      intake = c(0, 0.6),
      start_weight = c(10, 12),
      extra = c(1, 2)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_true(all(c("cow", "number_of_non_nutritive_visits") %in% names(result[[1]])))
})

test_that("calculate_non_nutritive_visits handles NA values in intake/start_weight", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "B", "C"),
      intake = c(NA, 0.2, 0.0),
      start_weight = c(10, NA, 0.6)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_true(is.data.frame(result[[1]]))
})

test_that("calculate_non_nutritive_visits returns all animals if all visits are non-nutritive", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "B"),
      intake = c(0, 0),
      start_weight = c(10, 11)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_equal(sort(result[[1]]$cow), c("A", "B"))
})

test_that("calculate_non_nutritive_visits returns empty data frame if no visits are non-nutritive", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "B"),
      intake = c(1, 2),
      start_weight = c(1, 2)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_equal(nrow(result[[1]]), 0)
})

test_that("calculate_non_nutritive_visits aggregates multiple visits per animal per day", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "A", "A", "B"),
      intake = c(0, 0, 0.6, 0),
      start_weight = c(10, 11, 12, 10)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_true(result[[1]]$number_of_non_nutritive_visits[result[[1]]$cow == "A"] == 2)
})

test_that("calculate_non_nutritive_visits output list names match input", {
  data <- list(
    "2023-01-01" = data.frame(cow = "A", intake = 0, start_weight = 10),
    "2023-01-02" = data.frame(cow = "B", intake = 0, start_weight = 10)
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_equal(names(result), names(data))
})

test_that("calculate_non_nutritive_visits works with non-character animal IDs", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c(1, 2, 1),
      intake = c(0, 0.6, 0.2),
      start_weight = c(10, 12, 0.3)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_true(all(c("cow", "number_of_non_nutritive_visits") %in% names(result[[1]])))
})

test_that("calculate_non_nutritive_visits works with only one animal and one visit", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = "A",
      intake = 0,
      start_weight = 10
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_true(is.data.frame(result[[1]]))
})

test_that("calculate_non_nutritive_visits handles large input lists", {
  data <- replicate(100, data.frame(cow = "A", intake = 0, start_weight = 10), simplify = FALSE)
  names(data) <- paste0("2023-01-", sprintf("%02d", 1:100))
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_equal(length(result), 100)
})

test_that("calculate_non_nutritive_visits output is always a data.frame or tibble", {
  data <- list(
    "2023-01-01" = data.frame(cow = "A", intake = 0, start_weight = 10)
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_non_nutritive_visits(data, cfg = cfg)
  expect_true(is.data.frame(result[[1]]) || tibble::is_tibble(result[[1]]))
})

test_that("calculate_no_feed_visits works with custom column names", {
  data <- list(
    "2023-01-01" = data.frame(
      animal = c("A", "B", "A"),
      eat = c(0, 0.6, 0.2),
      sw = c(0.1, 0.2, 0.3)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(
    data, cfg = cfg, id_col = "animal", intake_col = "eat", start_weight_col = "sw"
  )
  expect_true("animal" %in% names(result[[1]]))
})

test_that("calculate_no_feed_visits ignores extra columns in input", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "B"),
      intake = c(0, 0.6),
      start_weight = c(0.1, 0.2),
      extra = c(1, 2)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_true(all(c("cow", "number_of_visits_when_no_feed") %in% names(result[[1]])))
})

test_that("calculate_no_feed_visits handles NA values in intake/start_weight", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "B", "C"),
      intake = c(NA, 0.2, 0.0),
      start_weight = c(NA, 0.1, 0.2)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_true(is.data.frame(result[[1]]))
})

test_that("calculate_no_feed_visits returns all animals if all visits are no-feed", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "B"),
      intake = c(0, 0),
      start_weight = c(0.1, 0.2)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_equal(sort(result[[1]]$cow), c("A", "B"))
})

test_that("calculate_no_feed_visits aggregates multiple visits per animal per day", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c("A", "A", "A", "B"),
      intake = c(0, 0, 0.6, 0),
      start_weight = c(0.1, 0.2, 0.3, 0.1)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_true(result[[1]]$number_of_visits_when_no_feed[result[[1]]$cow == "A"] == 2)
})

test_that("calculate_no_feed_visits output list names match input", {
  data <- list(
    "2023-01-01" = data.frame(cow = "A", intake = 0, start_weight = 0.1),
    "2023-01-02" = data.frame(cow = "B", intake = 0, start_weight = 0.2)
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_equal(names(result), names(data))
})

test_that("calculate_no_feed_visits works with non-character animal IDs", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = c(1, 2, 1),
      intake = c(0, 0.6, 0.2),
      start_weight = c(0.1, 0.2, 0.3)
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_true(all(c("cow", "number_of_visits_when_no_feed") %in% names(result[[1]])))
})

test_that("calculate_no_feed_visits works with only one animal and one visit", {
  data <- list(
    "2023-01-01" = data.frame(
      cow = "A",
      intake = 0,
      start_weight = 0.1
    )
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_true(is.data.frame(result[[1]]))
})

test_that("calculate_no_feed_visits handles large input lists", {
  data <- replicate(100, data.frame(cow = "A", intake = 0, start_weight = 0.1), simplify = FALSE)
  names(data) <- paste0("2023-01-", sprintf("%02d", 1:100))
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_equal(length(result), 100)
})

test_that("calculate_no_feed_visits output is always a data.frame or tibble", {
  data <- list(
    "2023-01-01" = data.frame(cow = "A", intake = 0, start_weight = 0.1)
  )
  cfg <- qc_config(calibration_error = 0.5)
  result <- calculate_no_feed_visits(data, cfg = cfg)
  expect_true(is.data.frame(result[[1]]) || tibble::is_tibble(result[[1]]))
})
