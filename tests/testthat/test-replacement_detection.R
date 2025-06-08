# -----------------------------------------------------------------------------#
#                         Setup: Common test variables                          #
# -----------------------------------------------------------------------------#

# Helper function to create test data with controlled replacement events
make_test_data <- function(ids, bins, starts_chr, ends_chr) {
  stopifnot(length(ids) == length(bins),
            length(ids) == length(starts_chr),
            length(ids) == length(ends_chr))

  start_time <- lubridate::ymd_hms(starts_chr, tz = tz2())
  end_time   <- lubridate::ymd_hms(ends_chr, tz = tz2())

  df <- data.frame(
    transponder = ids + 1000L,          # dummy but unique
    cow         = as.character(ids),     # will be renamed below
    bin         = bins,
    start       = start_time,
    end         = end_time,
    duration    = as.integer(difftime(end_time, start_time, units = "secs")),
    startweight = 10,
    endweight   = 10,
    intake      = 0,
    date        = lubridate::date(start_time),
    stringsAsFactors = FALSE
  )

  # Rename to match package-wide column helpers
  names(df)[match("cow",   names(df))] <- id_col2()
  names(df)[match("bin",   names(df))] <- bin_col2()
  names(df)[match("start", names(df))] <- start_col2()
  names(df)[match("end",   names(df))] <- end_col2()

  df
}

# Create test data with known replacement events
day1 <- make_test_data(
  ids   = c(1, 2, 3, 4, 5),
  bins  = c(10, 10, 11, 11, 12),
  starts_chr = c(
    "2025-05-01 11:00:00",
    "2025-05-01 11:05:15", # 15s after cow 1 left (replacement)
    "2025-05-01 11:00:00",
    "2025-05-01 11:05:30", # 30s after cow 3 left (not replacement)
    "2025-05-01 11:00:00"
  ),
  ends_chr = c(
    "2025-05-01 11:05:00",
    "2025-05-01 11:10:00",
    "2025-05-01 11:05:00",
    "2025-05-01 11:10:00",
    "2025-05-01 11:05:00"
  )
)

day2 <- make_test_data(
  ids   = c(1, 2, 3, 4),
  bins  = c(10, 10, 11, 11),
  starts_chr = c(
    "2025-05-02 11:00:00",
    "2025-05-02 11:05:20", # 20s after cow 1 left (replacement)
    "2025-05-02 11:00:00",
    "2025-05-02 11:05:10"  # 10s after cow 3 left (replacement)
  ),
  ends_chr = c(
    "2025-05-02 11:05:00",
    "2025-05-02 11:10:00",
    "2025-05-02 11:05:00",
    "2025-05-02 11:10:00"
  )
)

# Create test data with alibi cases
day_with_alibi <- make_test_data(
  ids   = c(1, 2, 2, 3),
  bins  = c(10, 10, 11, 11),
  starts_chr = c(
    "2025-05-03 11:00:00",
    "2025-05-03 11:05:15", # 15s after cow 1 left (replacement)
    "2025-05-03 11:04:00", # cow 2 is at bin 11  when cow 1 left (alibi) 
    "2025-05-03 11:05:10"  # 8s after cow 2 left (replacement)
  ),
  ends_chr = c(
    "2025-05-03 11:05:00",
    "2025-05-03 11:10:00",
    "2025-05-03 11:05:02", # cow 2 is at bin 11  when cow 1 left (alibi) 
    "2025-05-03 11:10:00"
  )
)

# Create test data for multi-day
test_comb <- list(
  "2025-05-01" = day1,
  "2025-05-02" = day2,
  "2025-05-03" = day_with_alibi
)

# Generate single-day test data for existing tests
replacements <- record_replacement_day(day1)
empty_replacements <- data.frame(
  reactor_cow = character(),
  bin = integer(),
  time = as.POSIXct(character()),
  date = as.Date(character()),
  actor_cow = character(),
  bout_interval = lubridate::as.duration(numeric())
)

# -----------------------------------------------------------------------------#
#                  Tests for record_replacement_days()                         #
# -----------------------------------------------------------------------------#

test_that("Normal case: detects replacements correctly across multiple days", {
  result <- record_replacement_days(test_comb)

  expect_type(result, "list")
  expect_length(result, length(test_comb))
  expect_true(all(sapply(result, is.data.frame)))
})

test_that("Edge case: handles empty input gracefully", {
  empty_list <- list()
  result <- record_replacement_days(empty_list)

  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("Error handling: data_list is not a list", {
  expect_error(
    record_replacement_days("not_a_list"),
    "must be a named list"
  )
})

# -----------------------------------------------------------------------------#
#                  Tests using custom test dataset                             #
# -----------------------------------------------------------------------------#

test_that("record_replacement_day correctly identifies replacements with test data", {
  replacements_day1 <- record_replacement_day(day1)
  
  expect_s3_class(replacements_day1, "data.frame")
  expect_equal(nrow(replacements_day1), 1) # Only one replacement in day1
  expect_equal(replacements_day1$reactor_cow, "1")
  expect_equal(replacements_day1$actor_cow, "2")
  expect_equal(replacements_day1$bin, 10)
  expect_equal(replacements_day1$time, lubridate::ymd_hms("2025-05-01 11:05:00", tz = tz2()))
})

test_that("record_replacement_days correctly processes multiple days", {
  result <- record_replacement_days(test_comb)
  
  expect_type(result, "list")
  expect_length(result, 3)
  
  # Day 1 should have 1 replacement
  expect_equal(nrow(result[[1]]), 1)
  
  # Day 2 should have 2 replacements
  expect_equal(nrow(result[[2]]), 2)
  
  # Day 3 should have 1 valid replacement (one filtered due to alibi)
  expect_equal(nrow(result[[3]]), 1)
  expect_equal(result[[3]]$actor_cow, "3") # Only cow 3 replacement should remain
})

test_that("check_alibi_day correctly filters out actor cows with alibis", {
  # First detect replacements
  day3_replacements <- record_replacement_day(day_with_alibi)
  
  # Should find 2 initial replacements
  expect_equal(nrow(day3_replacements), 2)
  
  # After checking alibis, should only have 1 valid replacement
  valid_replacements <- check_alibi_day(day3_replacements, day_with_alibi)
  expect_equal(nrow(valid_replacements), 1)
  expect_equal(valid_replacements$actor_cow, "3")
})

test_that("Threshold parameter controls replacement detection", {
  # Create custom config with different threshold
  custom_cfg <- qc_config(replacement_threshold = 15) # Stricter threshold
  
  # Should detect fewer replacements with stricter threshold
  result_strict <- record_replacement_days(test_comb, cfg = custom_cfg)
  
  # Day 2 should have fewer valid replacements (only 1 instead of 2)
  expect_equal(nrow(result_strict[[2]]), 1)
})

test_that("check_alibi_days returns empty list when no replacements", {
  empty_replacements_list <- list(
    "2025-05-01" = empty_replacements,
    "2025-05-02" = empty_replacements
  )
  
  result <- check_alibi_days(empty_replacements_list, test_comb[1:2])
  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(nrow(result[[1]]), 0)
  expect_equal(nrow(result[[2]]), 0)
})

test_that("record_replacement_day handles varying input column names", {
  # Rename columns in our test data
  day_custom_cols <- day1
  names(day_custom_cols)[names(day_custom_cols) == id_col2()] <- "animal_id"
  names(day_custom_cols)[names(day_custom_cols) == bin_col2()] <- "feeder"
  names(day_custom_cols)[names(day_custom_cols) == start_col2()] <- "time_start"
  names(day_custom_cols)[names(day_custom_cols) == end_col2()] <- "time_end"
  
  # Should work with custom column names
  result <- record_replacement_day(
    day_custom_cols,
    id_col = "animal_id",
    bin_col = "feeder",
    start_col = "time_start",
    end_col = "time_end"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

# ---------------------- Tests for check_alibi_days() ---------------------- #

test_that("Normal case: validates replacements across multiple days", {
  replacements <- record_replacement_days(test_comb)
  valid_replacements <- check_alibi_days(replacements, test_comb)

  expect_type(valid_replacements, "list")
  expect_length(valid_replacements, length(test_comb))
  expect_true(all(sapply(valid_replacements, is.data.frame)))
})

test_that("Edge case: empty replacements return empty list", {
  empty_replacements_list <- list()
  valid_replacements <- check_alibi_days(empty_replacements_list, test_comb)

  expect_type(valid_replacements, "list")
  expect_length(valid_replacements, 0)
})

test_that("Error handling: mismatched lists cause error", {
  replacements <- record_replacement_days(test_comb)
  mismatched_data <- test_comb[1] # length mismatch

  expect_error(
    check_alibi_days(replacements, mismatched_data),
    "must be the same length"
  )
})

# ---------------------- Tests for record_replacement_day() (internal) ---------------------- #

test_that("Internal helper correctly identifies replacements on one day", {
  result <- record_replacement_day(day1)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("reactor_cow", "bin", "time", "date", "actor_cow", "bout_interval"))
})

test_that("Edge case: no replacements found on a quiet day", {
  empty_day <- day1[0, ] # Empty data frame
  result <- record_replacement_day(empty_day)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("Error handling: incorrect data format for single day detection", {
  incorrect_data <- data.frame(wrong_col = 1:3)
  expect_error(
    record_replacement_day(data.frame(wrong_col = 1:3)),
    "must include columns"
  )
})

# ---------------------- Tests for check_alibi_day() (internal) ---------------------- #

test_that("Internal helper correctly filters valid replacements", {
  replacements_day1 <- record_replacement_day(day1)
  valid_replacements <- check_alibi_day(replacements_day1, day1)

  expect_s3_class(valid_replacements, "data.frame")
  expect_named(valid_replacements, c("reactor_cow", "bin", "time", "date", "actor_cow", "bout_interval"))
})

test_that("Edge case: no replacements provided returns empty frame", {
  empty_replacements_df <- replacements[0, ]
  result <- check_alibi_day(empty_replacements_df, day1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("Error handling: incorrect input structure triggers error", {
  incorrect_replacements <- data.frame(wrong_col = 1:2)
  expect_error(
    check_alibi_day(incorrect_replacements, day1),
    "must include actor_cow"
  )
})

# ---------------------- Tests for day2 data ---------------------- #

test_that("record_replacement_day correctly identifies replacements for day2", {
  # Process day2 data
  replacements_day2 <- record_replacement_day(day2)
  
  # Check basic structure
  expect_s3_class(replacements_day2, "data.frame")
  expect_equal(nrow(replacements_day2), 2) # Should have 2 replacements
  
  # Sort by bin to ensure consistent testing order
  replacements_day2 <- replacements_day2[order(replacements_day2$bin, replacements_day2$time), ]
  
  # Test first replacement (bin 10)
  expect_equal(replacements_day2$reactor_cow[1], "1")
  expect_equal(replacements_day2$actor_cow[1], "2")
  expect_equal(replacements_day2$bin[1], 10)
  expect_equal(replacements_day2$time[1], lubridate::ymd_hms("2025-05-02 11:05:00", tz = tz2()))
  expect_equal(replacements_day2$date[1], as.Date("2025-05-02"))
  
  # Test second replacement (bin 11)
  expect_equal(replacements_day2$reactor_cow[2], "3")
  expect_equal(replacements_day2$actor_cow[2], "4")
  expect_equal(replacements_day2$bin[2], 11)
  expect_equal(replacements_day2$time[2], lubridate::ymd_hms("2025-05-02 11:05:00", tz = tz2()))
  expect_equal(replacements_day2$date[2], as.Date("2025-05-02"))
  
  # Check interval durations
  expect_true(replacements_day2$bout_interval[1] <= lubridate::as.duration("20s"))
  expect_true(replacements_day2$bout_interval[2] <= lubridate::as.duration("10s"))
})

# ---------------------- Tests for day_with_alibi data ---------------------- #

test_that("record_replacement_day correctly identifies initial replacements for day_with_alibi", {
  # Process alibi day data (before alibi checking)
  replacements_alibi <- record_replacement_day(day_with_alibi)
  
  # Check basic structure
  expect_s3_class(replacements_alibi, "data.frame")
  expect_equal(nrow(replacements_alibi), 2) # Should have 2 initial replacements
  
  # Sort by bin to ensure consistent testing order
  replacements_alibi <- replacements_alibi[order(replacements_alibi$bin, replacements_alibi$time), ]
  
  # Test first replacement (bin 10)
  expect_equal(replacements_alibi$reactor_cow[1], "1")
  expect_equal(replacements_alibi$actor_cow[1], "2")
  expect_equal(replacements_alibi$bin[1], 10)
  expect_equal(replacements_alibi$time[1], lubridate::ymd_hms("2025-05-03 11:05:00", tz = tz2()))
  
  # Test second replacement (bin 11)
  expect_equal(replacements_alibi$reactor_cow[2], "2")
  expect_equal(replacements_alibi$actor_cow[2], "3") 
  expect_equal(replacements_alibi$bin[2], 11)
  expect_equal(replacements_alibi$time[2], lubridate::ymd_hms("2025-05-03 11:05:02", tz = tz2()))
})

test_that("check_alibi_day correctly filters replacements with alibi", {
  # Get initial replacements
  replacements_alibi <- record_replacement_day(day_with_alibi)
  
  # Apply alibi check
  valid_replacements <- check_alibi_day(replacements_alibi, day_with_alibi)
  
  # Should only have one valid replacement (cow 2 has an alibi)
  expect_equal(nrow(valid_replacements), 1)
  
  # The remaining replacement should be the one with actor cow 3
  expect_equal(valid_replacements$reactor_cow, "2")
  expect_equal(valid_replacements$actor_cow, "3")
  expect_equal(valid_replacements$bin, 11)
  expect_equal(valid_replacements$time, lubridate::ymd_hms("2025-05-03 11:05:02", tz = tz2()))
})

test_that("Full pipeline correctly processes day_with_alibi", {
  # Create single-day test combo
  single_day_test <- list("2025-05-03" = day_with_alibi)
  
  # Run through complete pipeline
  result <- record_replacement_days(single_day_test)
  
  # Check structure
  expect_type(result, "list")
  expect_length(result, 1)
  expect_s3_class(result[[1]], "data.frame")
  
  # Check content - should only have one replacement after alibi filtering
  expect_equal(nrow(result[[1]]), 1)
  expect_equal(result[[1]]$reactor_cow, "2")
  expect_equal(result[[1]]$actor_cow, "3")
  expect_equal(result[[1]]$bin, 11)
  
  # Validate complete pipeline with all test days
  all_days_result <- record_replacement_days(test_comb)
  expect_equal(nrow(all_days_result[[1]]), 1) # Day 1: 1 replacement
  expect_equal(nrow(all_days_result[[2]]), 2) # Day 2: 2 replacements  
  expect_equal(nrow(all_days_result[[3]]), 1) # Day 3: 1 valid replacement after alibi check
})

# ---------------------- Tests with custom configuration ---------------------- #

test_that("Strict threshold correctly filters replacements", {
  # Create custom configs with different thresholds
  strict_cfg <- qc_config(replacement_threshold = 11) # Very strict
  moderate_cfg <- qc_config(replacement_threshold = 15) # Moderate
  relaxed_cfg <- qc_config(replacement_threshold = 30) # More relaxed
  
  # Process day2 with different configs
  strict_result <- record_replacement_day(day2, cfg = strict_cfg)
  moderate_result <- record_replacement_day(day2, cfg = moderate_cfg)
  relaxed_result <- record_replacement_day(day2, cfg = relaxed_cfg)
  
  # Strict should only catch the fastest replacement (10s)
  expect_equal(nrow(strict_result), 1)
  expect_equal(strict_result$bin, 11)
  
  # Moderate should catch bin 11 (10s) but not bin 10 (20s)
  expect_equal(nrow(moderate_result), 1)
  expect_equal(moderate_result$bin, 11)
  
  # Relaxed should catch both replacements
  expect_equal(nrow(relaxed_result), 2)
  expect_setequal(relaxed_result$bin, c(10, 11))
})
