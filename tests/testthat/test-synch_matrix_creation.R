# Tests for synch_matrix_creation.R

test_that("create_time_sequence works for normal input", {
  df <- data.frame(start = lubridate::ymd_hms("2023-01-01 00:00:00"), 
                   end = lubridate::ymd_hms("2023-01-01 00:00:05"))
  seq <- create_time_sequence(df, start_col = "start", end_col = "end")
  expect_equal(length(seq), 6)
  expect_true(all(lubridate::is.POSIXct(seq)))
})

test_that("create_time_sequence errors for bad input", {
  expect_error(create_time_sequence(data.frame(), start_col = "start", end_col = "end"), "Missing required columns")
  expect_error(create_time_sequence(data.frame(start=1, end=2), start_col = "start", end_col = "end"), "POSIXct")
  expect_error(create_time_sequence(data.frame(start=lubridate::ymd_hms("2023-01-01 00:00:00"), 
                                              end=lubridate::ymd_hms("2022-12-31 23:59:59")), 
                                   start_col = "start", end_col = "end"), "earlier")
})

test_that("prepare_time_animal_matrix works for normal input", {
  df <- data.frame(cow = c(1,2), 
                   start = lubridate::ymd_hms("2023-01-01 00:00:00"), 
                   end = lubridate::ymd_hms("2023-01-01 00:00:01"))
  seq <- create_time_sequence(df, start_col = "start", end_col = "end")
  mat <- prepare_time_animal_matrix(df, seq, id_col = "cow")
  expect_equal(nrow(mat), length(seq))
  expect_equal(colnames(mat)[1], "Time")
  expect_equal(sort(colnames(mat)[-1]), c("1","2"))
})

test_that("prepare_time_animal_matrix errors for bad input", {
  expect_error(prepare_time_animal_matrix(data.frame(), lubridate::ymd_hms("2023-01-01 00:00:00"), id_col = "cow"), "Missing required column")
  expect_error(prepare_time_animal_matrix(data.frame(cow=1), 1:5, id_col = "cow"), "POSIXct")
})

test_that("prepare_time_bin_matrix returns input and checks structure", {
  df <- data.frame(Time=lubridate::ymd_hms("2023-01-01 00:00:00"), `1`=0, check.names = FALSE)
  expect_equal(prepare_time_bin_matrix(df), df)
  expect_error(prepare_time_bin_matrix(data.frame()), "Time")
})

test_that("prepare_time_feed_matrix works for normal input", {
  seq <- lubridate::ymd_hms("2023-01-01 00:00:00") + 0:2
  mat <- prepare_time_feed_matrix(seq, bins_feed = 1:3)
  expect_equal(nrow(mat), 3)
  expect_equal(colnames(mat), c("Time", "1", "2", "3"))
  expect_true(all(is.na(mat[,2:4])))
})

test_that("prepare_time_feed_matrix errors for bad input", {
  expect_error(prepare_time_feed_matrix(1:5, bins_feed = 1:3), "POSIXct")
  expect_error(prepare_time_feed_matrix(lubridate::ymd_hms("2023-01-01 00:00:00"), bins_feed = c()), "bins_feed must be numeric")
})

test_that("create_time_sequence works with different column names", {
  df <- data.frame(start_time = lubridate::ymd_hms("2023-01-01 00:00:00"), 
                   end_time = lubridate::ymd_hms("2023-01-01 00:00:03"))
  seq <- create_time_sequence(df, start_col = "start_time", end_col = "end_time")
  expect_equal(length(seq), 4)
  expect_true(all(lubridate::is.POSIXct(seq)))
})

test_that("prepare_time_animal_matrix works with different animal ID columns", {
  df <- data.frame(animal_id = c(101, 102), 
                   start = lubridate::ymd_hms("2023-01-01 00:00:00"), 
                   end = lubridate::ymd_hms("2023-01-01 00:00:01"))
  seq <- create_time_sequence(df, start_col = "start", end_col = "end")
  mat <- prepare_time_animal_matrix(df, seq, id_col = "animal_id")
  expect_equal(colnames(mat), c("Time", "101", "102"))
})

test_that("prepare_time_feed_matrix works with non-sequential bins", {
  seq <- lubridate::ymd_hms("2023-01-01 00:00:00") + 0:1
  mat <- prepare_time_feed_matrix(seq, bins_feed = c(1, 5, 10))
  expect_equal(nrow(mat), 2)
  expect_equal(colnames(mat), c("Time", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
})

test_that("create_time_sequence handles single time point", {
  df <- data.frame(start = lubridate::ymd_hms("2023-01-01 00:00:00"), 
                   end = lubridate::ymd_hms("2023-01-01 00:00:00"))
  seq <- create_time_sequence(df, start_col = "start", end_col = "end")
  expect_equal(length(seq), 1)
  expect_equal(seq[1], lubridate::ymd_hms("2023-01-01 00:00:00"))
}) 