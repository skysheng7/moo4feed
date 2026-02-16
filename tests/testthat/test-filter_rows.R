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
    cow = c("A", "B", "C", "A"),
    value = 1:4,
    stringsAsFactors = FALSE
  )
  out <- delete_rows(df, c("A", "C"), "cow")
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
  out <- delete_rows(df, c(2, 4), "bin")
  expect_equal(out$bin, c(1, 3, 5))
  expect_equal(out$value, c(11, 13, 15))
})

test_that("returns unchanged data frame when nothing matches", {
  df <- data.frame(x = letters[1:3], y = 1:3, stringsAsFactors = FALSE)
  out <- delete_rows(df, "z", "x")
  expect_equal(out, df)
})

test_that("returns zero‑row data frame when all match", {
  df <- data.frame(id = c("a", "b"), v = 1:2, stringsAsFactors = FALSE)
  out <- delete_rows(df, c("a", "b"), "id")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), ncol(df))
})

test_that("errors if `df` is not a data frame", {
  expect_error(
    delete_rows(1:5, 1, "x"),
    "`df` must be a data frame"
  )
})

test_that("errors if `col_name` is not a single string", {
  df <- data.frame(x = 1:3)
  expect_error(
    delete_rows(df, 1, c("x", "y")),
    "`col_name` must be a single character string"
  )
  expect_error(
    delete_rows(df, 1, 123),
    "`col_name` must be a single character string"
  )
})

test_that("errors if requested column is missing", {
  df <- data.frame(a = 1:3)
  expect_error(
    delete_rows(df, 1, "b"),
    "`df` must contain a column named 'b'"
  )
})

test_that("errors if `to_delete` is not a vector", {
  df <- data.frame(a = 1:3)
  bad <- data.frame(x = 1)
  expect_error(
    delete_rows(df, bad, "a"),
    "`to_delete` must be a vector"
  )
})

test_that("errors on type mismatch between column and `to_delete`", {
  df_char <- data.frame(a = letters[1:3], stringsAsFactors = FALSE)
  expect_error(
    delete_rows(df_char, 1:2, "a"),
    "Type mismatch"
  )
  df_num <- data.frame(a = 1:3)
  expect_error(
    delete_rows(df_num, c("1", "2"), "a"),
    "Type mismatch"
  )
})



# -------------------------- tests for rename_bins() --------------------------#

test_that("shifts specified bins correctly with default `bin` column", {
  df <- data.frame(bin = 1:5, value = 10:14)
  out <- rename_bins(df, bins = 2:4, bin_offset = 100)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("bin", "value"))
  expect_equal(out$bin, c(1, 102, 103, 104, 5))
  expect_equal(out$value, df$value)
})

test_that("shifts specified bins correctly with custom column name", {
  df <- data.frame(Bin = 1:5, value = 20:24)
  out <- rename_bins(df, bins = c(1, 5), bin_offset = 50, bin_col = "Bin")
  expect_equal(out$Bin, c(51, 2, 3, 4, 55))
  expect_equal(out$value, df$value)
})

test_that("no bins match → returns data unchanged", {
  df <- data.frame(bin = 1:3, x = 4:6)
  out <- rename_bins(df, bins = 10:20, bin_offset = 5)
  expect_equal(out, df)
})

test_that("all bins match → all shifted", {
  df <- data.frame(bin = 1:3, y = 7:9)
  out <- rename_bins(df, bins = 1:3, bin_offset = 10)
  expect_equal(out$bin, c(11, 12, 13))
  expect_equal(out$y, df$y)
})

test_that("errors if `df` is not a data frame", {
  expect_error(
    rename_bins(1:5, bins = 1, bin_offset = 1),
    "`df` must be a data frame"
  )
})

test_that("errors if `bin_col` is not a single string", {
  df <- data.frame(bin = 1:3)
  expect_error(
    rename_bins(df, bins = 1, bin_offset = 1, bin_col = c("a", "b")),
    "`bin_col` must be a single character string"
  )
  expect_error(
    rename_bins(df, bins = 1, bin_offset = 1, bin_col = 1),
    "`bin_col` must be a single character string"
  )
})

test_that("errors if column not found", {
  df <- data.frame(notbin = 1:3)
  expect_error(
    rename_bins(df, bins = 1, bin_offset = 1, bin_col = "bin"),
    "`df` must contain a column named 'bin'"
  )
})

test_that("errors if column is not numeric", {
  df <- data.frame(bin = letters[1:3], z = 1:3, stringsAsFactors = FALSE)
  expect_error(
    rename_bins(df, bins = 1, bin_offset = 1),
    "Column 'bin' must be numeric"
  )
})

test_that("errors if `bins` is not numeric", {
  df <- data.frame(bin = 1:3)
  expect_error(
    rename_bins(df, bins = c("x", "y"), bin_offset = 1),
    "`bins` must be a numeric vector"
  )
})

test_that("errors if `bin_offset` is not a single numeric", {
  df <- data.frame(bin = 1:3)
  expect_error(
    rename_bins(df, bins = 1, bin_offset = c(1, 2)),
    "`bin_offset` must be a single numeric value"
  )
  expect_error(
    rename_bins(df, bins = 1, bin_offset = "1"),
    "`bin_offset` must be a single numeric value"
  )
})
