# Tests for synch_matrix_creation.R
library(testthat)
library(lubridate)

test_that("create_time_sequence works for normal input", {
  df <- data.frame(Start = ymd_hms("2023-01-01 00:00:00"), End = ymd_hms("2023-01-01 00:00:05"))
  seq <- create_time_sequence(df)
  expect_equal(length(seq), 6)
  expect_true(all(lubridate::is.POSIXct(seq)))
})

test_that("create_time_sequence errors for bad input", {
  expect_error(create_time_sequence(data.frame()), "must have 'Start' and 'End' columns")
  expect_error(create_time_sequence(data.frame(Start=1, End=2)), "POSIXct")
  expect_error(create_time_sequence(data.frame(Start=ymd_hms("2023-01-01 00:00:00"), End=ymd_hms("2022-12-31 23:59:59"))), "earlier")
})

test_that("prepare_time_cow_matrix works for normal input", {
  df <- data.frame(Cow = c(1,2), Start = ymd_hms("2023-01-01 00:00:00"), End = ymd_hms("2023-01-01 00:00:01"))
  seq <- create_time_sequence(df)
  mat <- prepare_time_cow_matrix(df, seq)
  expect_equal(nrow(mat), length(seq))
  expect_equal(colnames(mat)[1], "Time")
  expect_equal(sort(colnames(mat)[-1]), c("1","2"))
})

test_that("prepare_time_cow_matrix errors for bad input", {
  expect_error(prepare_time_cow_matrix(data.frame(), ymd_hms("2023-01-01 00:00:00")), "Cow")
  expect_error(prepare_time_cow_matrix(data.frame(Cow=1), 1:5), "POSIXct")
})

test_that("prepare_time_bin_matrix returns input and checks structure", {
  df <- data.frame(Time=ymd_hms("2023-01-01 00:00:00"), `1`=0)
  expect_equal(prepare_time_bin_matrix(df), df)
  expect_error(prepare_time_bin_matrix(data.frame()), "Time")
})

test_that("prepare_time_feed_matrix works for normal input", {
  seq <- ymd_hms("2023-01-01 00:00:00") + 0:2
  mat <- prepare_time_feed_matrix(seq, 1, 3)
  expect_equal(nrow(mat), 3)
  expect_equal(colnames(mat), c("Time", "1", "2", "3"))
  expect_true(all(is.na(mat[,2:4])))
})

test_that("prepare_time_feed_matrix errors for bad input", {
  expect_error(prepare_time_feed_matrix(1:5, 1, 3), "POSIXct")
  expect_error(prepare_time_feed_matrix(ymd_hms("2023-01-01 00:00:00"), 3, 1), "greater")
}) 