test_that("total_cows_present calculates correct totals", {
  # Create test data
  test_data <- list(
    data.frame(
      Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01")),
      Cow1 = c(1, 0),
      Cow2 = c(1, 1),
      Cow3 = c(0, 1)
    )
  )
  
  # Test function
  result <- total_cows_present(test_data, total_fed_wat_bin = 5)
  
  # Check results
  expect_equal(result[[1]]$total_cow_num, c(2, 2))
  expect_equal(result[[1]]$total_bin_occupied, c(2, 2))
  expect_equal(result[[1]]$empty_bin_num, c(3, 3))
})

test_that("delete_inactive_time removes periods with no activity", {
  # Create test data
  test_cow_data <- list(
    data.frame(
      Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01", "2024-01-01 10:00:02")),
      Cow1 = c(1, 0, 1),
      Cow2 = c(1, 0, 0),
      total_cow_num = c(2, 0, 1)
    )
  )
  
  test_bin_data <- list(
    data.frame(
      Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01", "2024-01-01 10:00:02")),
      Cow1 = c(1, 0, 2),
      Cow2 = c(2, 0, 0)
    )
  )
  
  # Test function
  result <- delete_inactive_time(test_cow_data, test_bin_data)
  
  # Check results
  expect_equal(nrow(result[[1]][[1]]), 2)  # Should have 2 rows (removed middle row)
  expect_equal(nrow(result[[2]][[1]]), 2)  # Should have 2 rows (removed middle row)
  expect_equal(result[[1]][[1]]$total_cow_num, c(2, 1))
})

test_that("add_date adds correct date information", {
  # Create test data
  test_cow_data <- list(
    data.frame(
      Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01")),
      Cow1 = c(1, 1),
      Cow2 = c(1, 0)
    )
  )
  
  test_bin_data <- list(
    data.frame(
      Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01")),
      Cow1 = c(1, 2),
      Cow2 = c(2, 0)
    )
  )
  
  # Test function
  result <- add_date(test_cow_data, test_bin_data)
  
  # Check results
  expect_equal(result[[1]][[1]]$date, rep(as.Date("2024-01-01"), 2))
  expect_equal(result[[2]][[1]]$date, rep(as.Date("2024-01-01"), 2))
  expect_equal(names(result[[1]])[1], "2024-01-01")
  expect_equal(names(result[[2]])[1], "2024-01-01")
})

test_that("bin_update correctly maps bin numbers", {
  # Create test data
  test_data <- list(
    data.frame(
      Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01")),
      Cow1 = c(101, 1),  # Water bin and feed bin
      Cow2 = c(102, 2)   # Water bin and feed bin
    )
  )
  
  # Test function
  result <- bin_update(test_data)
  
  # Check results
  expect_equal(result[[1]]$Cow1, c(207, 201))  # Water bin 101 -> 207, feed bin 1 -> 201
  expect_equal(result[[1]]$Cow2, c(208, 202))  # Water bin 102 -> 208, feed bin 2 -> 202
})

test_that("feed_drink_matrix_process processes data correctly", {
  # Create test data
  test_data <- list(
    data.frame(
      Time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01")),
      Start = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01")),
      End = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:00:01")),
      Cow = c("Cow1", "Cow2"),
      Bin = c(101, 1)
    )
  )
  
  # Test function
  result <- feed_drink_matrix_process(test_data, total_fed_wat_bin = 5)
  
  # Check results
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(c("Time", "date") %in% names(result[[1]][[1]])))
  expect_true(all(c("Time", "date") %in% names(result[[2]][[1]])))
})

test_that("total_cows_present handles edge cases", {
  # Test empty input
  expect_error(total_cows_present(list(), total_fed_wat_bin = 10), "Input list is empty")
  
  # Test invalid total_fed_wat_bin
  expect_error(total_cows_present(list(data.frame()), total_fed_wat_bin = 0), "total_fed_wat_bin must be positive")
  expect_error(total_cows_present(list(data.frame()), total_fed_wat_bin = "10"), "total_fed_wat_bin must be numeric")
  
  # Test missing Time column
  expect_error(
    total_cows_present(list(data.frame(Cow1 = 1)), total_fed_wat_bin = 10),
    "Each data frame must contain a 'Time' column"
  )
  
  # Test normal operation
  sample_data <- list(
    data.frame(
      Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
      Cow1 = c(1, 0, 1),
      Cow2 = c(0, 1, 1)
    )
  )
  result <- total_cows_present(sample_data, total_fed_wat_bin = 10)
  expect_equal(length(result), 1)
  expect_true(all(c("total_cow_num", "total_bin_occupied", "empty_bin_num") %in% names(result[[1]])))
})

test_that("delete_inactive_time handles edge cases", {
  # Test empty input
  expect_error(delete_inactive_time(list(), list()), "Input lists cannot be empty")
  
  # Test mismatched list lengths
  expect_error(
    delete_inactive_time(list(data.frame()), list(data.frame(), data.frame())),
    "Cow and bin data lists must have the same length"
  )
  
  # Test missing required columns
  expect_error(
    delete_inactive_time(
      list(data.frame(Time = Sys.time())),
      list(data.frame(Time = Sys.time()))
    ),
    "Cow data must contain 'Time' and 'total_cow_num' columns"
  )
  
  # Test normal operation
  sample_cow_data <- list(
    data.frame(
      Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
      Cow1 = c(1, 0, 1),
      Cow2 = c(0, 1, 1),
      total_cow_num = c(1, 1, 2)
    )
  )
  sample_bin_data <- list(
    data.frame(
      Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
      Cow1 = c(1, 0, 1),
      Cow2 = c(0, 1, 1)
    )
  )
  result <- delete_inactive_time(sample_cow_data, sample_bin_data)
  expect_equal(length(result), 2)
  expect_equal(length(result[[1]]), 1)
  expect_equal(length(result[[2]]), 1)
})

test_that("add_date handles edge cases", {
  # Test empty input
  expect_error(add_date(list(), list()), "Input lists cannot be empty")
  
  # Test mismatched list lengths
  expect_error(
    add_date(list(data.frame()), list(data.frame(), data.frame())),
    "Cow and bin data lists must have the same length"
  )
  
  # Test missing Time column
  expect_error(
    add_date(
      list(data.frame(Cow1 = 1)),
      list(data.frame(Cow1 = 1))
    ),
    "Both cow and bin data must contain a 'Time' column"
  )
  
  # Test normal operation
  sample_cow_data <- list(
    data.frame(
      Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
      Cow1 = c(1, 0, 1),
      Cow2 = c(0, 1, 1)
    )
  )
  sample_bin_data <- list(
    data.frame(
      Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
      Cow1 = c(1, 0, 1),
      Cow2 = c(0, 1, 1)
    )
  )
  result <- add_date(sample_cow_data, sample_bin_data)
  expect_equal(length(result), 2)
  expect_true("date" %in% names(result[[1]][[1]]))
  expect_true("date" %in% names(result[[2]][[1]]))
})

test_that("bin_update handles edge cases", {
  # Test empty input
  expect_error(bin_update(list()), "Input list is empty")
  
  # Test missing Time column
  expect_error(
    bin_update(list(data.frame(Cow1 = 1))),
    "Data must contain a 'Time' column"
  )
  
  # Test normal operation
  sample_data <- list(
    data.frame(
      Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
      Cow1 = c(101, 1, 2),
      Cow2 = c(102, 3, 4)
    )
  )
  result <- bin_update(sample_data)
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$Cow1[1], 207)  # Water bin 101 -> 207
  expect_equal(result[[1]]$Cow1[2], 201)  # Feed bin 1 -> 201
})

test_that("feed_drink_matrix_process handles edge cases", {
  # Test empty input
  expect_error(feed_drink_matrix_process(list(), total_fed_wat_bin = 10), "Input list is empty")
  
  # Test invalid total_fed_wat_bin
  expect_error(feed_drink_matrix_process(list(data.frame()), total_fed_wat_bin = 0), "total_fed_wat_bin must be positive")
  expect_error(feed_drink_matrix_process(list(data.frame()), total_fed_wat_bin = "10"), "total_fed_wat_bin must be numeric")
  
  # Test missing required columns
  expect_error(
    feed_drink_matrix_process(
      list(data.frame(Time = Sys.time())),
      total_fed_wat_bin = 10
    ),
    "Data frame 1 is missing required columns"
  )
  
  # Test normal operation
  sample_data <- list(
    data.frame(
      Start = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
      End = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3) + 60,
      Cow = c("Cow1", "Cow2", "Cow1"),
      Bin = c(101, 1, 2)
    )
  )
  result <- feed_drink_matrix_process(sample_data, total_fed_wat_bin = 10)
  expect_equal(length(result), 2)
  expect_true(all(c("Time", "date") %in% names(result[[1]][[1]])))
  expect_true(all(c("Time", "date") %in% names(result[[2]][[1]])))
}) 