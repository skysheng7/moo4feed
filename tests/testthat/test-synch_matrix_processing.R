# Tests for synch_matrix_processing.R
library(testthat)
library(lubridate)
library(zoo)

# Helper: minimal valid data
minimal_data <- function() {
  data.frame(
    Cow = 1,
    Start = ymd_hms("2023-01-01 00:00:00"),
    End = ymd_hms("2023-01-01 00:00:01"),
    Bin = 1,
    Startweight = 10,
    Endweight = 12
  )
}

test_that("empty_synch_matrix works for feed", {
  dl <- list(day1 = minimal_data())
  res <- empty_synch_matrix(dl, min_feed_bin = 1, max_feed_bin = 2, type = "feed")
  expect_true(all(c("synch_master_cow", "synch_master_bin", "synch_master_feed") %in% names(res)))
  expect_equal(nrow(res$synch_master_cow[[1]]), 2)
  expect_equal(ncol(res$synch_master_feed[[1]]), 3)
})

test_that("empty_synch_matrix errors for bad input", {
  expect_error(empty_synch_matrix(list(), 1, 2, "feed"), "empty")
  expect_error(empty_synch_matrix(list(day1 = minimal_data()), NULL, 2, "feed"), "must be provided")
  expect_error(empty_synch_matrix(list(day1 = minimal_data()), 2, 1, "feed"), "<=")
  expect_error(empty_synch_matrix(list(day1 = minimal_data()), 1, 2, "badtype"), "Type must be one of")
})

test_that("matrix_initialize works for feed", {
  dl <- list(day1 = minimal_data())
  res <- matrix_initialize(dl, min_feed_bin = 1, max_feed_bin = 2, type = "feed")
  expect_true(all(c("synch_master_cow", "synch_master_bin", "synch_master_feed") %in% names(res)))
  expect_equal(nrow(res$synch_master_cow[[1]]), 2)
})

test_that("matrix_initialize errors for bad input", {
  expect_error(matrix_initialize(list(), 1, 2, "feed"), "empty")
  expect_error(matrix_initialize(list(day1 = minimal_data()), NULL, 2, "feed"), "must be provided")
  expect_error(matrix_initialize(list(day1 = minimal_data()), 2, 1, "feed"), "<=")
  expect_error(matrix_initialize(list(day1 = minimal_data()), 1, 2, "badtype"), "Type must be one of")
})

test_that("process_cur_synch fills NAs and computes totalFeed", {
  seq <- ymd_hms("2023-01-01 00:00:00") + 0:2
  mat <- data.frame(Time = seq, `1` = c(NA, 2, 3), `2` = c(1, NA, 3), check.names = FALSE)
  res <- process_cur_synch(mat, total_feed_bin = 2)
  expect_true(!any(is.na(res[1,2:3])))
  expect_true(all(!is.na(res$totalFeed)))
  expect_equal(res$totalFeed[1], sum(res[1,2:3]))
})

test_that("process_cur_synch errors for bad input", {
  expect_error(process_cur_synch(NULL, 2), "NULL")
  expect_error(process_cur_synch(data.frame(), 2), "empty")
  expect_error(process_cur_synch(data.frame(Time=1), 2), "at least one bin")
  expect_error(process_cur_synch(data.frame(Time=1, X=1), -1), "at least one bin")
})

test_that("matrix_process works for feed", {
  dl <- list(day1 = minimal_data())
  res <- matrix_process(dl, min_feed_bin = 1, max_feed_bin = 1, total_feed_bin = 1, type = "feed")
  expect_true(all(c("synch_master_cow2", "synch_master_bin2", "synch_master_feed2") %in% names(res)))
  expect_equal(nrow(res$synch_master_cow2[[1]]), 2)
  expect_true("total_cow_num" %in% names(res$synch_master_cow2[[1]]))
  expect_true("totalFeed" %in% names(res$synch_master_feed2[[1]]))
})

test_that("matrix_process errors for bad input", {
  expect_error(matrix_process(list(), 1, 2, 1, "feed"), "empty")
  expect_error(matrix_process(list(day1 = minimal_data()), NULL, 2, 1, "feed"), "must be provided")
  expect_error(matrix_process(list(day1 = minimal_data()), 2, 1, 1, "feed"), "<=")
  expect_error(matrix_process(list(day1 = minimal_data()), 1, 2, 1, "badtype"), "Type must be one of")
}) 