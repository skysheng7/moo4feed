test_that("unmerge_data works with normal data", {
  # Create a test data frame with multiple dates
  test_df <- data.frame(
    date = c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02", "2023-01-03"),
    cow = c(1, 2, 3, 4, 5),
    intake = c(10, 20, 30, 40, 50)
  )
  
  # Run the function
  result <- unmerge_data(test_df)
  
  # Test basic functionality
  expect_type(result, "list")
  expect_length(result, 3) # 3 unique dates
  expect_named(result, c("2023-01-01", "2023-01-02", "2023-01-03"))
  
  # Test contents of each data frame in the list
  expect_equal(nrow(result[["2023-01-01"]]), 2)
  expect_equal(nrow(result[["2023-01-02"]]), 2)
  expect_equal(nrow(result[["2023-01-03"]]), 1)
  
  # Check that the first data frame contains correct data
  expect_equal(result[["2023-01-01"]]$cow, c(1, 2))
  expect_equal(result[["2023-01-01"]]$intake, c(10, 20))
  
  # Check that the second data frame contains correct data
  expect_equal(result[["2023-01-02"]]$cow, c(3, 4))
  expect_equal(result[["2023-01-02"]]$intake, c(30, 40))
  
  # Check that the third data frame contains correct data
  expect_equal(result[["2023-01-03"]]$cow, 5)
  expect_equal(result[["2023-01-03"]]$intake, 50)
})

test_that("unmerge_data works with custom date column name", {
  # Create a test data frame with a custom date column name
  test_df <- data.frame(
    custom_date = c("2023-01-01", "2023-01-01", "2023-01-02"),
    cow = c(1, 2, 3),
    intake = c(10, 20, 30)
  )
  
  # Run the function with custom date column
  result <- unmerge_data(test_df, date_col = "custom_date")
  
  # Test basic functionality
  expect_type(result, "list")
  expect_length(result, 2) # 2 unique dates
  expect_named(result, c("2023-01-01", "2023-01-02"))
  
  # Test contents of each data frame in the list
  expect_equal(nrow(result[["2023-01-01"]]), 2)
  expect_equal(nrow(result[["2023-01-02"]]), 1)
})

test_that("unmerge_data works with date objects", {
  # Create a test data frame with Date objects
  test_df <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02")),
    cow = c(1, 2, 3),
    intake = c(10, 20, 30)
  )
  
  # Run the function
  result <- unmerge_data(test_df)
  
  # Test basic functionality
  expect_type(result, "list")
  expect_length(result, 2) # 2 unique dates
  
  # Names should be Date objects converted to character
  expected_names <- as.character(as.Date(c("2023-01-01", "2023-01-02")))
  expect_named(result, expected_names)
})

test_that("unmerge_data works with a single date", {
  # Create a test data frame with only one date
  test_df <- data.frame(
    date = c("2023-01-01", "2023-01-01", "2023-01-01"),
    cow = c(1, 2, 3),
    intake = c(10, 20, 30)
  )
  
  # Run the function
  result <- unmerge_data(test_df)
  
  # Test basic functionality
  expect_type(result, "list")
  expect_length(result, 1) # Only 1 unique date
  expect_named(result, "2023-01-01")
  
  # Test contents of the data frame in the list
  expect_equal(nrow(result[["2023-01-01"]]), 3)
  expect_equal(result[["2023-01-01"]]$cow, c(1, 2, 3))
  expect_equal(result[["2023-01-01"]]$intake, c(10, 20, 30))
})

test_that("unmerge_data works with an empty data frame with date column", {
  # Create an empty data frame with date column
  test_df <- data.frame(
    date = character(0),
    cow = integer(0),
    intake = numeric(0)
  )
  
  # Run the function
  result <- unmerge_data(test_df)
  
  # Test result
  expect_type(result, "list")
  expect_length(result, 0) # Empty list
})

test_that("unmerge_data handles tibbles properly", {
  skip_if_not_installed("tibble")
  
  # Create a test tibble
  test_tbl <- tibble::tibble(
    date = c("2023-01-01", "2023-01-01", "2023-01-02"),
    cow = c(1, 2, 3),
    intake = c(10, 20, 30)
  )
  
  # Run the function
  result <- unmerge_data(test_tbl)
  
  # Test basic functionality
  expect_type(result, "list")
  expect_length(result, 2) # 2 unique dates
  expect_named(result, c("2023-01-01", "2023-01-02"))
})

test_that("unmerge_data raises an error for non-data frame input", {
  # Test with a vector
  expect_error(
    unmerge_data(c(1, 2, 3)),
    "`df` must be a data frame."
  )
  
  # Test with a list that is not a data frame
  expect_error(
    unmerge_data(list(a = 1, b = 2)),
    "`df` must be a data frame."
  )
  
  # Test with NULL
  expect_error(
    unmerge_data(NULL),
    "`df` must be a data frame."
  )
})

test_that("unmerge_data raises an error when date column is not found", {
  # Create a test data frame without the expected date column
  test_df <- data.frame(
    not_date = c("2023-01-01", "2023-01-02"),
    cow = c(1, 2),
    intake = c(10, 20)
  )
  
  # Test with default date column
  expect_error(
    unmerge_data(test_df),
    "Date column date not found in the data frame."
  )
  
  # Test with custom date column that doesn't exist
  expect_error(
    unmerge_data(test_df, date_col = "missing_column"),
    "Date column missing_column not found in the data frame."
  )
})

test_that("unmerge_data preserves all columns", {
  # Create a test data frame with multiple columns
  test_df <- data.frame(
    date = c("2023-01-01", "2023-01-01", "2023-01-02"),
    cow = c(1, 2, 3),
    intake = c(10, 20, 30),
    pen = c("A", "B", "C"),
    weight = c(500, 550, 600)
  )
  
  # Run the function
  result <- unmerge_data(test_df)
  
  # Check that all columns are preserved
  expect_equal(colnames(result[["2023-01-01"]]), c("date", "cow", "intake", "pen", "weight"))
  expect_equal(colnames(result[["2023-01-02"]]), c("date", "cow", "intake", "pen", "weight"))
  
  # Check all values are preserved
  expect_equal(result[["2023-01-01"]]$pen, c("A", "B"))
  expect_equal(result[["2023-01-01"]]$weight, c(500, 550))
  expect_equal(result[["2023-01-02"]]$pen, "C")
  expect_equal(result[["2023-01-02"]]$weight, 600)
})

test_that("unmerge_data handles data frames with a single row", {
  # Create a test data frame with a single row
  test_df <- data.frame(
    date = "2023-01-01",
    cow = 1,
    intake = 10
  )
  
  # Run the function
  result <- unmerge_data(test_df)
  
  # Test basic functionality
  expect_type(result, "list")
  expect_length(result, 1) # 1 unique date
  expect_named(result, "2023-01-01")
  
  # Test contents of the data frame in the list
  expect_equal(nrow(result[["2023-01-01"]]), 1)
  expect_equal(result[["2023-01-01"]]$cow, 1)
  expect_equal(result[["2023-01-01"]]$intake, 10)
}) 