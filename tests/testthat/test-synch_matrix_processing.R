# Tests for synch_matrix_processing.R

# Helper: minimal valid data
minimal_data <- function() {
  data.frame(
    cow = 1,
    start = lubridate::ymd_hms("2023-01-01 00:00:00"),
    end = lubridate::ymd_hms("2023-01-01 00:00:01"),
    bin = 1,
    start_weight = 10,
    end_weight = 12
  )
}

# Helper: more complex data for better testing
complex_data <- function() {
  data.frame(
    cow = c(1, 2, 1),
    start = lubridate::ymd_hms(c("2023-01-01 00:00:00", "2023-01-01 00:00:01", "2023-01-01 00:00:10")),
    end = lubridate::ymd_hms(c("2023-01-01 00:00:02", "2023-01-01 00:00:03", "2023-01-01 00:00:12")),
    bin = c(1, 2, 1),
    start_weight = c(10, 8, 9),
    end_weight = c(12, 9, 11)
  )
}

test_that("empty_synch_matrix works for feed", {
  dl <- list(day1 = minimal_data())
  res <- empty_synch_matrix(dl, type = "feed", bins_feed = 1:2, 
                           id_col = "cow", start_col = "start", end_col = "end", 
                           bin_col = "bin")
  expect_true(all(c("synch_master_animal", "synch_master_bin", "synch_master_feed") %in% names(res)))
  expect_equal(nrow(res$synch_master_animal[[1]]), 2)
  expect_equal(ncol(res$synch_master_feed[[1]]), 3)
})

test_that("empty_synch_matrix works for drink", {
  dl <- list(day1 = minimal_data())
  res <- empty_synch_matrix(dl, type = "drink", bins_wat = 1:2,
                           id_col = "cow", start_col = "start", end_col = "end", 
                           bin_col = "bin")
  expect_true(all(c("synch_master_animal", "synch_master_bin") %in% names(res)))
  expect_false("synch_master_feed" %in% names(res))
})

test_that("empty_synch_matrix errors for bad input", {
  expect_error(empty_synch_matrix(list(), type = "feed"), "empty")
  expect_error(empty_synch_matrix(list(day1 = minimal_data()), type = "badtype"), "Type must be one of")
  expect_error(empty_synch_matrix(list(day1 = data.frame()), type = "feed"), "Missing required columns")
})

test_that("matrix_initialize works for feed", {
  dl <- list(day1 = minimal_data())
  res <- matrix_initialize(dl, type = "feed", bins_feed = 1:2,
                          id_col = "cow", start_col = "start", end_col = "end",
                          bin_col = "bin", start_weight_col = "start_weight", 
                          end_weight_col = "end_weight")
  expect_true(all(c("synch_master_animal", "synch_master_bin", "synch_master_feed") %in% names(res)))
  expect_equal(nrow(res$synch_master_animal[[1]]), 2)
})

test_that("matrix_initialize works for drink", {
  dl <- list(day1 = minimal_data())
  res <- matrix_initialize(dl, type = "drink", bins_wat = 1:2,
                          id_col = "cow", start_col = "start", end_col = "end",
                          bin_col = "bin")
  expect_true(all(c("synch_master_animal", "synch_master_bin") %in% names(res)))
  expect_false("synch_master_feed" %in% names(res))
})

test_that("matrix_initialize errors for bad input", {
  expect_error(matrix_initialize(list(), type = "feed"), "empty")
  expect_error(matrix_initialize(list(day1 = minimal_data()), type = "badtype"), "Type must be one of")
  expect_error(matrix_initialize(list(day1 = data.frame(cow = 1)), type = "feed"), "Missing required columns")
})

test_that("process_cur_synch fills NAs and computes totalFeed", {
  time_seq <- lubridate::ymd_hms("2023-01-01 00:00:00") + 0:2
  mat <- data.frame(Time = time_seq, `1` = c(NA, 2, 3), `2` = c(1, NA, 3), check.names = FALSE)
  res <- process_cur_synch(mat, bins_feed = 1:2)
  expect_true(!any(is.na(res[1,2:3])))
  expect_true(all(!is.na(res$totalFeed)))
  expect_equal(res$totalFeed[1], sum(res[1,2:3]))
})

test_that("process_cur_synch handles all-NA columns", {
  time_seq <- lubridate::ymd_hms("2023-01-01 00:00:00") + 0:2
  mat <- data.frame(Time = time_seq, `1` = c(NA, NA, NA), `2` = c(1, NA, 3), check.names = FALSE)
  res <- process_cur_synch(mat, bins_feed = 1:2)
  expect_true(!is.na(res$totalFeed[1]))
  expect_equal(res[1, "1"], 0)  # Should default to 0 for all-NA column
})

test_that("process_cur_synch errors for bad input", {
  expect_error(process_cur_synch(NULL, bins_feed = 1:2), "NULL")
  expect_error(process_cur_synch(data.frame(), bins_feed = 1:2), "empty")
  expect_error(process_cur_synch(data.frame(Time=1), bins_feed = 1:2), "at least one bin")
  expect_error(process_cur_synch(data.frame(Time=1, X=1), bins_feed = c()), "non-empty")
})

test_that("matrix_process works for feed", {
  dl <- list(day1 = minimal_data())
  res <- matrix_process(dl, type = "feed", bins_feed = 1:2,
                       id_col = "cow", start_col = "start", end_col = "end",
                       bin_col = "bin", start_weight_col = "start_weight", 
                       end_weight_col = "end_weight")
  expect_true(all(c("synch_master_animal2", "synch_master_bin2", "synch_master_feed2") %in% names(res)))
  expect_equal(nrow(res$synch_master_animal2[[1]]), 2)
  expect_true("total_animal_num" %in% names(res$synch_master_animal2[[1]]))
  expect_true("totalFeed" %in% names(res$synch_master_feed2[[1]]))
})

test_that("matrix_process works for drink", {
  dl <- list(day1 = minimal_data())
  res <- matrix_process(dl, type = "drink", bins_wat = 1:2,
                       id_col = "cow", start_col = "start", end_col = "end",
                       bin_col = "bin")
  expect_true(all(c("synch_master_animal2", "synch_master_bin2") %in% names(res)))
  expect_false("synch_master_feed2" %in% names(res))
  expect_true("total_animal_num" %in% names(res$synch_master_animal2[[1]]))
})

test_that("matrix_process errors for bad input", {
  expect_error(matrix_process(list(), type = "feed"), "empty")
  expect_error(matrix_process(list(day1 = minimal_data()), type = "badtype"), "Type must be one of")
})

test_that("matrix_initialize handles out-of-range bins gracefully", {
  # Create data with bin outside the expected range
  bad_data <- data.frame(
    cow = 1,
    start = lubridate::ymd_hms("2023-01-01 00:00:00"),
    end = lubridate::ymd_hms("2023-01-01 00:00:01"),
    bin = 99,  # Outside range 1:2
    start_weight = 10,
    end_weight = 12
  )
  dl <- list(day1 = bad_data)
  
  expect_warning(
    res <- matrix_initialize(dl, type = "feed", bins_feed = 1:2,
                            id_col = "cow", start_col = "start", end_col = "end",
                            bin_col = "bin", start_weight_col = "start_weight", 
                            end_weight_col = "end_weight"),
    "outside the expected range"
  )
  expect_true("synch_master_feed" %in% names(res))
})

test_that("complex data processing works correctly", {
  dl <- list(day1 = complex_data())
  res <- matrix_process(dl, type = "feed", bins_feed = 1:3,
                       id_col = "cow", start_col = "start", end_col = "end",
                       bin_col = "bin", start_weight_col = "start_weight", 
                       end_weight_col = "end_weight")
  
  # Should have processed multiple animals and time points
  expect_gt(nrow(res$synch_master_animal2[[1]]), 2)
  expect_true("date" %in% names(res$synch_master_animal2[[1]]))
  expect_true("date" %in% names(res$synch_master_bin2[[1]]))
  expect_true("date" %in% names(res$synch_master_feed2[[1]]))
})

test_that("empty_synch_matrix works with custom column names", {
  custom_data <- data.frame(
    animal_id = 1,
    start_time = lubridate::ymd_hms("2023-01-01 00:00:00"),
    end_time = lubridate::ymd_hms("2023-01-01 00:00:01"),
    feeder_bin = 1
  )
  dl <- list(day1 = custom_data)
  
  res <- empty_synch_matrix(dl, type = "feed", bins_feed = 1:2,
                           id_col = "animal_id", start_col = "start_time", 
                           end_col = "end_time", bin_col = "feeder_bin")
  expect_true(all(c("synch_master_animal", "synch_master_bin", "synch_master_feed") %in% names(res)))
  expect_equal(colnames(res$synch_master_animal[[1]]), c("Time", "1"))
}) 