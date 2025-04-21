


# ---------------------- test for keep_bins() ----------------------#

test_that("keeps specified bins using default column", {
  df <- data.frame(bin = 1:5, value = 11:15)
  out <- keep_bins(df, bins = 2:4)
  expect_s3_class(out, "data.frame")
  expect_equal(out, df[2:4, , drop = FALSE])
})

test_that("keeps specified bins using a custom column name", {
  df <- data.frame(mybin = c(10, 20, 30), x = 1:3, stringsAsFactors = FALSE)
  out <- keep_bins(df, bins = c(20, 30), bin_col = "mybin")
  expect_equal(out, df[2:3, , drop = FALSE])
})

test_that("returns zero‐row data frame when no bins match", {
  df <- data.frame(bin = 1:3, x = 4:6)
  out <- keep_bins(df, bins = 5, bin_col = "bin")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), ncol(df))
})

test_that("errors if `df` is not a data frame", {
  expect_error(
    keep_bins(1:5, bins = 2:4),
    "`df` must be a data frame"
  )
})

test_that("errors if `bin_col` is not a single string", {
  df <- data.frame(bin = 1:3)
  expect_error(
    keep_bins(df, bins = 1, bin_col = 1),
    "`bin_col` must be a single character string"
  )
  expect_error(
    keep_bins(df, bins = 1, bin_col = c("a", "b")),
    "`bin_col` must be a single character string"
  )
})

test_that("errors if requested column is missing", {
  df <- data.frame(other = 1:3)
  expect_error(
    keep_bins(df, bins = 1:2, bin_col = "bin"),
    "`df` must contain a column named 'bin'"
  )
})

test_that("errors if the column is not numeric", {
  df <- data.frame(bin = c("a", "b"), x = 1:2, stringsAsFactors = FALSE)
  expect_error(
    keep_bins(df, bins = 1),
    "Column 'bin' must be numeric"
  )
})

test_that("errors if `bins` is not numeric", {
  df <- data.frame(bin = 1:3)
  expect_error(
    keep_bins(df, bins = c("a", "b")),
    "`bins` must be a numeric vector"
  )
})


# ---------------------- test for delete_rows() ----------------------#

test_that("removes specified values for a character column", {
  df <- data.frame(
    cow   = c("A", "B", "C", "A"),
    value = 1:4,
    stringsAsFactors = FALSE
  )
  out <- delete_rows(df, "cow", c("A", "C"))
  # only rows with cow == "B" remain
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$cow, "B")
  expect_equal(out$value, 2L)
})

test_that("removes specified values for a numeric column", {
  df <- data.frame(
    bin   = 1:5,
    value = 11:15
  )
  out <- delete_rows(df, "bin", c(2, 4))
  expect_equal(out$bin, c(1, 3, 5))
  expect_equal(out$value, c(11, 13, 15))
})

test_that("returns unchanged data frame when nothing matches", {
  df <- data.frame(x = letters[1:3], y = 1:3, stringsAsFactors = FALSE)
  out <- delete_rows(df, "x", "z")
  expect_equal(out, df)
})

test_that("returns zero‑row data frame when all match", {
  df <- data.frame(id = c("a", "b"), v = 1:2, stringsAsFactors = FALSE)
  out <- delete_rows(df, "id", c("a", "b"))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), ncol(df))
})

test_that("errors if `df` is not a data frame", {
  expect_error(
    delete_rows(1:5, "x", 1),
    "`df` must be a data frame"
  )
})

test_that("errors if `col_name` is not a single string", {
  df <- data.frame(x = 1:3)
  expect_error(
    delete_rows(df, c("x", "y"), 1),
    "`col_name` must be a single character string"
  )
  expect_error(
    delete_rows(df, 123, 1),
    "`col_name` must be a single character string"
  )
})

test_that("errors if requested column is missing", {
  df <- data.frame(a = 1:3)
  expect_error(
    delete_rows(df, "b", 1),
    "`df` must contain a column named 'b'"
  )
})

test_that("errors if `to_delete` is not a vector", {
  df <- data.frame(a = 1:3)
  bad <- data.frame(x = 1)
  expect_error(
    delete_rows(df, "a", bad),
    "`to_delete` must be a vector"
  )
})

test_that("errors on type mismatch between column and `to_delete`", {
  df_char <- data.frame(a = letters[1:3], stringsAsFactors = FALSE)
  expect_error(
    delete_rows(df_char, "a", 1:2),
    "Type mismatch"
  )
  df_num <- data.frame(a = 1:3)
  expect_error(
    delete_rows(df_num, "a", c("1", "2")),
    "Type mismatch"
  )
})
