# Tests for cluster_gap_utils.R functions

test_that("calculate_gaps works with normal input", {
  start_times <- c(60, 120, 180, 300)  # 1:00, 2:00, 3:00, 5:00
  end_times <- c(90, 150, 210, 330)    # 1:30, 2:30, 3:30, 5:30
  
  result <- calculate_gaps(start_times, end_times)
  
  expect_type(result, "double")
  expect_length(result, 3)
  expect_equal(result, c(30, 30, 90))  # gaps: 30min, 30min, 90min
})

test_that("calculate_gaps handles sorted input correctly", {
  # Already sorted
  start_times <- c(60, 120, 180)
  end_times <- c(90, 150, 210)
  
  result <- calculate_gaps(start_times, end_times)
  expect_equal(result, c(30, 30))
  
  # Unsorted input should be sorted internally
  start_times_unsorted <- c(180, 60, 120)
  end_times_unsorted <- c(210, 90, 150)
  
  result_unsorted <- calculate_gaps(start_times_unsorted, end_times_unsorted)
  expect_equal(result_unsorted, c(30, 30))
})

test_that("calculate_gaps removes negative gaps", {
  # Create overlapping visits
  start_times <- c(60, 80, 180)   # Second visit starts before first ends
  end_times <- c(100, 120, 210)  # Overlapping visits
  
  result <- calculate_gaps(start_times, end_times)
  
  # Should only return positive gaps, negative gap (80-100 = -20) removed
  expect_length(result, 1)
  expect_equal(result, 60)  # gap from 120 to 180
})

test_that("calculate_gaps handles edge case with length <= 1", {
  # Empty input
  expect_equal(calculate_gaps(numeric(0), numeric(0)), numeric(0))
  
  # Single visit
  expect_equal(calculate_gaps(60, 90), numeric(0))
})

test_that("calculate_gaps throws error for mismatched lengths", {
  start_times <- c(60, 120)
  end_times <- c(90)  # Different length
  
  expect_error(calculate_gaps(start_times, end_times), 
               "start_times and end_times must have the same length")
})

test_that("calculate_gaps handles all negative gaps", {
  # All visits overlap
  start_times <- c(60, 70, 80)
  end_times <- c(100, 110, 120)  # All overlapping
  
  result <- calculate_gaps(start_times, end_times)
  expect_equal(result, numeric(0))  # All gaps removed
})

test_that("calculate_gaps_by_animal works with normal input", {
  # Create test data with multiple animals and dates
  data <- data.frame(
    cow = c(1, 1, 1, 2, 2),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:30:00", 
                                 "2023-01-01 09:00:00", "2023-01-01 10:00:00", 
                                 "2023-01-01 10:45:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 08:45:00", 
                               "2023-01-01 09:15:00", "2023-01-01 10:15:00", 
                               "2023-01-01 11:00:00"))
  )
  
  result <- calculate_gaps_by_animal(data)
  
  expect_type(result, "double")
  expect_length(result, 3)  # 2 gaps for cow 1, 1 gap for cow 2
  expect_true(all(result >= 0))  # All gaps should be non-negative
})

test_that("calculate_gaps_by_animal handles custom column names", {
  data <- data.frame(
    animal_id = c(1, 1),
    visit_start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:30:00")),
    visit_end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 08:45:00"))
  )
  
  result <- calculate_gaps_by_animal(data, 
                                   id_col = "animal_id", 
                                   start_col = "visit_start", 
                                   end_col = "visit_end")
  
  expect_type(result, "double")
  expect_length(result, 1)
  expect_equal(result, 15)  # 15 minute gap
})

test_that("calculate_gaps_by_animal adds date column when missing", {
  data <- data.frame(
    cow = c(1, 1),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:30:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 08:45:00"))
  )
  
  # Function should not error when date column is missing
  expect_no_error(calculate_gaps_by_animal(data))
})

test_that("calculate_gaps_by_animal respects existing date column", {
  data <- data.frame(
    cow = c(1, 1),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:30:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 08:45:00")),
    date = as.Date(c("2023-01-01", "2023-01-01"))
  )
  
  result <- calculate_gaps_by_animal(data)
  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("calculate_gaps_by_animal handles multiple dates", {
  data <- data.frame(
    cow = c(1, 1, 1, 1),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:30:00",
                                 "2023-01-02 08:00:00", "2023-01-02 08:30:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 08:45:00",
                               "2023-01-02 08:15:00", "2023-01-02 08:45:00"))
  )
  
  result <- calculate_gaps_by_animal(data)
  
  # Should calculate gaps within each date separately
  expect_length(result, 2)  # One gap per date
  expect_true(all(result == 15))  # Both gaps should be 15 minutes
})

test_that("calculate_gaps_by_animal handles edge case with nrow <= 1", {
  # Empty data
  data <- data.frame(
    cow = integer(0),
    start = lubridate::as_datetime(character(0)),
    end = lubridate::as_datetime(character(0))
  )
  
  result <- calculate_gaps_by_animal(data)
  expect_equal(result, numeric(0))
  
  # Single row
  data_single <- data.frame(
    cow = 1,
    start = lubridate::ymd_hms("2023-01-01 08:00:00"),
    end = lubridate::ymd_hms("2023-01-01 08:15:00")
  )
  
  result_single <- calculate_gaps_by_animal(data_single)
  expect_equal(result_single, numeric(0))
})

test_that("calculate_gaps_by_animal groups by animal and date correctly", {
  # Mix data from different animals and dates
  data <- data.frame(
    cow = c(1, 2, 1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:00:00",
                                 "2023-01-01 08:30:00", "2023-01-01 08:30:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 08:15:00",
                               "2023-01-01 08:45:00", "2023-01-01 08:45:00"))
  )
  
  result <- calculate_gaps_by_animal(data)
  
  # Should get one gap per animal (since they're on the same date)
  expect_length(result, 2)
  expect_true(all(result == 15))
})

test_that("calculate_gaps_by_animal handles overlapping visits", {
  data <- data.frame(
    cow = c(1, 1, 1),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:10:00", 
                                 "2023-01-01 08:45:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:20:00", "2023-01-01 08:25:00", 
                               "2023-01-01 09:00:00"))  # First two overlap
  )
  
  result <- calculate_gaps_by_animal(data)
  
  # Should remove negative gap and only return positive gaps
  expect_length(result, 1)
  expect_equal(result, 20)  # gap from 08:25 to 08:45
})

test_that("calculate_gaps_by_animal handles single visit per animal-date", {
  data <- data.frame(
    cow = c(1, 2, 3),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 09:00:00", 
                                 "2023-01-01 10:00:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 09:15:00", 
                               "2023-01-01 10:15:00"))
  )
  
  result <- calculate_gaps_by_animal(data)
  
  # No gaps should be calculated (each animal has only one visit)
  expect_equal(result, numeric(0))
})

test_that("calculate_gaps_by_animal works with default global variables", {
  # Test that function works with default column names from global variables
  data <- data.frame(
    cow = c(1, 1),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:30:00")),
    end = lubridate::ymd_hms(c("2023-01-01 08:15:00", "2023-01-01 08:45:00"))
  )
  
  # Should use id_col2(), start_col2(), end_col2() defaults
  expect_no_error(calculate_gaps_by_animal(data))
  
  result <- calculate_gaps_by_animal(data)
  expect_length(result, 1)
  expect_equal(result, 15)
})

test_that("calculate_gaps_by_animal handles mixed data types correctly", {
  # Test with different datetime formats that lubridate should handle
  data <- data.frame(
    cow = c(1, 1),
    start = c(lubridate::ymd_hms("2023-01-01 08:00:00"), 
              lubridate::ymd_hms("2023-01-01 08:30:00")),
    end = c(lubridate::ymd_hms("2023-01-01 08:15:00"), 
            lubridate::ymd_hms("2023-01-01 08:45:00"))
  )
  
  result <- calculate_gaps_by_animal(data)
  expect_length(result, 1)
  expect_equal(result, 15)
})

test_that("calculate_gaps_by_animal maintains precision for small gaps", {
  # Test with small gaps (seconds)
  data <- data.frame(
    cow = c(1, 1),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:00:30")),
    end = lubridate::ymd_hms(c("2023-01-01 08:00:10", "2023-01-01 08:00:40"))
  )
  
  result <- calculate_gaps_by_animal(data)
  expect_length(result, 1)
  expect_equal(result, 20/60)  # 20 seconds = 20/60 minutes
})

test_that("calculate_gaps works with normal cases", {
  # Normal case with proper gaps
  start_times <- c(10, 30, 60)
  end_times <- c(20, 40, 70)
  
  result <- calculate_gaps(start_times, end_times)
  expect_equal(result, c(10, 20))  # gaps are 30-20=10, 60-40=20
})

test_that("calculate_gaps handles edge cases", {
  # Empty input
  expect_equal(calculate_gaps(numeric(0), numeric(0)), numeric(0))
  
  # Single visit
  expect_equal(calculate_gaps(10, 20), numeric(0))
  
  # Two visits
  start_times <- c(10, 30)
  end_times <- c(20, 40)
  result <- calculate_gaps(start_times, end_times)
  expect_equal(result, 10)  # 30-20=10
})

test_that("calculate_gaps handles overlapping visits", {
  # Overlapping visits (negative gaps should be removed)
  start_times <- c(10, 25, 60)  # second visit starts before first ends
  end_times <- c(30, 35, 70)
  
  result <- calculate_gaps(start_times, end_times)
  expect_equal(result, 25)  # only 60-35=25, negative gap 25-30=-5 is removed
})

test_that("calculate_gaps handles unsorted input", {
  # Unsorted start times should be sorted internally
  start_times <- c(60, 10, 30)
  end_times <- c(70, 20, 40)
  
  result <- calculate_gaps(start_times, end_times)
  expect_equal(result, c(10, 20))  # Should be sorted: 10-20, 30-40, 60-70
})

test_that("calculate_gaps validates input", {
  # Different length vectors
  expect_error(
    calculate_gaps(c(10, 20), c(15)),
    "start_times and end_times must have the same length"
  )
})

test_that("calculate_gaps_by_animal works with normal data", {
  # Create test data
  data <- data.frame(
    cow = c("A", "A", "A", "B", "B"),
    start = as.POSIXct(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:30:00", 
      "2023-01-01 11:00:00",
      "2023-01-01 09:00:00",
      "2023-01-01 09:45:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 10:15:00",
      "2023-01-01 10:45:00",
      "2023-01-01 11:15:00",
      "2023-01-01 09:20:00",
      "2023-01-01 10:00:00"
    ))
  )
  
  result <- calculate_gaps_by_animal(data)
  
  # Should have gaps from both animals
  expect_type(result, "double")
  expect_true(length(result) >= 2)  # At least one gap per animal
})

test_that("calculate_gaps_by_animal handles edge cases", {
  # Empty data
  empty_data <- data.frame(
    cow = character(0),
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0))
  )
  result <- calculate_gaps_by_animal(empty_data)
  expect_equal(result, numeric(0))
  
  # Single row
  single_data <- data.frame(
    cow = "A",
    start = as.POSIXct("2023-01-01 10:00:00"),
    end = as.POSIXct("2023-01-01 10:15:00")
  )
  result <- calculate_gaps_by_animal(single_data)
  expect_equal(result, numeric(0))
})

test_that("calculate_gaps_by_animal works across multiple dates", {
  # Data spanning multiple dates
  data <- data.frame(
    cow = c("A", "A", "A", "A"),
    start = as.POSIXct(c(
      "2023-01-01 10:00:00",
      "2023-01-01 11:00:00",
      "2023-01-02 09:00:00",  # Different date
      "2023-01-02 10:00:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 10:15:00",
      "2023-01-01 11:15:00",
      "2023-01-02 09:15:00",
      "2023-01-02 10:15:00"
    ))
  )
  
  result <- calculate_gaps_by_animal(data)
  
  # Should have 2 gaps (one per date), not a gap across dates
  expect_equal(length(result), 2)
})

test_that("calculate_gaps_by_animal works with custom column names", {
  # Test with different column names
  data <- data.frame(
    animal_id = c("A", "A"),
    begin_time = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:30:00")),
    finish_time = as.POSIXct(c("2023-01-01 10:15:00", "2023-01-01 10:45:00"))
  )
  
  result <- calculate_gaps_by_animal(
    data, 
    id_col = "animal_id", 
    start_col = "begin_time", 
    end_col = "finish_time"
  )
  
  expect_type(result, "double")
  expect_equal(length(result), 1)
  expect_equal(result, 15)  # 30-15=15 minutes gap
})

test_that("calculate_gaps_by_animal adds date column when missing", {
  # Data without date column
  data <- data.frame(
    cow = c("A", "A"),
    start = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:30:00")),
    end = as.POSIXct(c("2023-01-01 10:15:00", "2023-01-01 10:45:00"))
  )
  
  # Should work even without date column
  result <- calculate_gaps_by_animal(data)
  expect_type(result, "double")
  expect_equal(length(result), 1)
})

test_that("calculate_gaps_by_animal preserves existing date column", {
  # Data with existing date column
  data <- data.frame(
    cow = c("A", "A"),
    start = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:30:00")),
    end = as.POSIXct(c("2023-01-01 10:15:00", "2023-01-01 10:45:00")),
    date = as.Date(c("2023-01-01", "2023-01-01"))
  )
  
  # Should work with existing date column
  result <- calculate_gaps_by_animal(data)
  expect_type(result, "double")
  expect_equal(length(result), 1)
})

test_that("calculate_gaps_by_animal handles multiple animals correctly", {
  # Multiple animals, ensure gaps aren't calculated between animals
  data <- data.frame(
    cow = c("A", "B", "A", "B"),
    start = as.POSIXct(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:05:00",  # Animal B starts during A's visit
      "2023-01-01 10:30:00",  # Animal A's second visit
      "2023-01-01 10:35:00"   # Animal B's second visit
    )),
    end = as.POSIXct(c(
      "2023-01-01 10:15:00",
      "2023-01-01 10:20:00",
      "2023-01-01 10:45:00",
      "2023-01-01 10:50:00"
    ))
  )
  
  result <- calculate_gaps_by_animal(data)
  
  # Should have exactly 2 gaps (one per animal)
  expect_equal(length(result), 2)
  # Gap for A: 10:30 - 10:15 = 15 minutes
  # Gap for B: 10:35 - 10:20 = 15 minutes
  expect_true(all(result == 15))
})

test_that("calculate_gaps_by_animal works with large datasets", {
  # Test performance with larger dataset
  n <- 1000
  data <- data.frame(
    cow = rep(c("A", "B", "C"), length.out = n),
    start = as.POSIXct("2023-01-01 00:00:00") + 
            cumsum(runif(n, 300, 1800)), # 5-30 min intervals
    end = as.POSIXct("2023-01-01 00:00:00") + 
          cumsum(runif(n, 300, 1800)) + runif(n, 60, 300) # 1-5 min durations
  )
  
  # Should complete without errors
  result <- calculate_gaps_by_animal(data)
  expect_type(result, "double")
  expect_true(length(result) > 0)
})
