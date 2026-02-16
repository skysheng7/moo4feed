# ----------------------------------------------------------------------------- #
# Synthetic data helpers                                                        #
# ----------------------------------------------------------------------------- #

make_feed_visits <- function(bin, start_times, start_weights, end_weights) {
  n <- length(bin)
  df <- data.frame(
    date = if (n == 0) character(0) else rep("2025-01-15", n),
    bin = bin,
    start = as.POSIXct(start_times, tz = "America/Vancouver"),
    end = as.POSIXct(start_times, tz = "America/Vancouver") + 300,
    start_weight = start_weights,
    end_weight = end_weights,
    stringsAsFactors = FALSE
  )
  colnames(df) <- c("date", bin_col2(), start_col2(), end_col2(),
                    start_weight_col2(), end_weight_col2())
  df
}

make_feed_list <- function(...) {
  lst <- list(...)
  names(lst) <- paste0("2025-01-", sprintf("%02d", seq_along(lst)))
  lst
}

# ----------------------------------------------------------------------------- #
# Happy path - Individual bin additions                                        #
# ----------------------------------------------------------------------------- #

test_that("detect_feed_additions detects weight jumps at single bin", {
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00", "2025-01-15 10:00:00"),
    start_weights = c(50, 45, 55),  # Jump from 40 to 55 = 15 kg increase
    end_weights = c(45, 40, 50)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$weight_increase[1], 15)
  expect_equal(result[[bin_col2()]][1], "B1")
})

test_that("detect_feed_additions includes bin_weight_after_fill column", {
  visits <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 60),  # 10 kg increase (next_start_weight - end_weight = 60 - 45 = 15)
    end_weights = c(45, 55)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )

  expect_true("bin_weight_after_fill" %in% names(result))
  # bin_weight_after_fill should be the next_start_weight (60)
  expect_equal(result$bin_weight_after_fill[1], 60)
})

test_that("bin_weight_after_fill reflects total bin weight after addition", {
  # This tests the key fix: bin_weight_after_fill is the total weight in bin
  # after feed was added, not just the amount added
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00", "2025-01-15 10:00:00"),
    start_weights = c(100, 90, 85),  # Start with 100, ends with 90, then starts at 85 (refill!)
    end_weights = c(95, 80, 75)      # Second visit ends at 80, third starts at 85 = 5kg added
  )
  # Wait - need to show actual refill
  visits2 <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(30, 80),  # Residual 30, ate 10, ended at 20. Then refilled to 80 (added 60kg)
    end_weights = c(20, 70)
  )

  result <- detect_feed_additions(
    visits2,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$weight_increase[1], 60)  # Amount added = 80 - 20 = 60
  expect_equal(result$bin_weight_after_fill[1], 80)  # Total in bin after fill = 80
})

test_that("bin_weight_after_fill uses max when feed added during visit", {
  # Critical test: Farmer adds feed DURING an animal's visit
  # Visit 1: Animal visits bin, starts with 100kg, eats down to 20kg
  # Visit 2: Animal starts eating at 80kg (60kg was added between visits)
  #          BUT farmer adds more feed during this visit
  #          Animal ends at 90kg (higher than start!)
  # The TRUE bin weight after fill is 90kg (the end weight), not 80kg (start weight)
  
  visits <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(100, 80),  # Jump detected: 80 - 20 = 60kg added
    end_weights = c(20, 90)      # End weight HIGHER than start (feed added during visit)
  )
  
  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(result$weight_increase[1], 60)  # Initial jump: 80 - 20
  # bin_weight_after_fill should be 90 (max of next_start=80 and next_end=90)
  expect_equal(result$bin_weight_after_fill[1], 90)
})

test_that("detect_feed_additions works with list input", {
  day1 <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 60),  # 10 kg increase
    end_weights = c(45, 55)
  )

  data_list <- make_feed_list(day1)

  result <- detect_feed_additions(
    data_list,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )

  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 1)
})

# ----------------------------------------------------------------------------- #
# Happy path - Aggregated all-bin events                                       #
# ----------------------------------------------------------------------------- #

test_that("detect_feed_additions aggregates multi-bin events", {
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B2", "B2", "B3", "B3"),
    start_times = c(
      "2025-01-15 08:00:00", "2025-01-15 09:00:00",  # B1
      "2025-01-15 08:05:00", "2025-01-15 09:05:00",  # B2 (within 10 min)
      "2025-01-15 08:10:00", "2025-01-15 09:10:00"   # B3 (within 10 min)
    ),
    start_weights = c(50, 55, 50, 55, 50, 55),  # All show 10 kg increase
    end_weights = c(45, 50, 45, 50, 45, 50)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 600,
    min_bins_for_group = 3,
    aggregate_all_bin = TRUE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$bins_filled[1], 3)
  expect_equal(result$avg_weight_increase[1], 10)
  expect_equal(result$min_weight_increase[1], 10)
  expect_equal(result$max_weight_increase[1], 10)
  expect_true("event_id" %in% names(result))
})

test_that("min and max weight increase are correctly calculated", {
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B2", "B2", "B3", "B3"),
    start_times = c(
      "2025-01-15 08:00:00", "2025-01-15 09:00:00",  # B1
      "2025-01-15 08:05:00", "2025-01-15 09:05:00",  # B2 (within 10 min)
      "2025-01-15 08:10:00", "2025-01-15 09:10:00"   # B3 (within 10 min)
    ),
    start_weights = c(50, 55, 40, 50, 60, 85),  # B1: +5, B2: +10, B3: +25
    end_weights = c(45, 50, 35, 45, 55, 80)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 600,
    min_bins_for_group = 3,
    aggregate_all_bin = TRUE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$bins_filled[1], 3)
  expect_equal(result$avg_weight_increase[1], (10 + 15 + 30) / 3)
  expect_equal(result$min_weight_increase[1], 10)
  expect_equal(result$max_weight_increase[1], 30)
})

test_that("aggregation filters out events with too few bins", {
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B2", "B2"),
    start_times = c(
      "2025-01-15 08:00:00", "2025-01-15 09:00:00",  # B1
      "2025-01-15 08:05:00", "2025-01-15 09:05:00"   # B2
    ),
    start_weights = c(50, 60, 50, 60),
    end_weights = c(45, 55, 45, 55)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    min_bins_for_group = 3,  # Require at least 3 bins
    aggregate_all_bin = TRUE
  )

  expect_equal(nrow(result), 0)  # Should have no events
})

test_that("aggregation separates events by time gap", {
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B1", "B2", "B2", "B2", "B3", "B3", "B3"),
    start_times = c(
      "2025-01-15 08:00:00", "2025-01-15 09:00:00", "2025-01-15 20:00:00",  # B1
      "2025-01-15 08:05:00", "2025-01-15 09:05:00", "2025-01-15 20:05:00",  # B2
      "2025-01-15 08:10:00", "2025-01-15 09:10:00", "2025-01-15 20:10:00"   # B3
    ),
    start_weights = c(50, 60, 70, 50, 60, 70, 50, 60, 70),
    end_weights = c(45, 55, 65, 45, 55, 65, 45, 55, 65)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 600,  # 10 minutes
    min_bins_for_group = 3,
    aggregate_all_bin = TRUE
  )

  expect_equal(nrow(result), 2)  # Two separate events
  expect_equal(result$bins_filled, c(3, 3))
})

# ----------------------------------------------------------------------------- #
# Edge cases                                                                    #
# ----------------------------------------------------------------------------- #

test_that("no feed additions when weight increases below threshold", {
  visits <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 52),  # Only 2 kg increase
    end_weights = c(48, 50)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("empty input returns empty result with correct structure", {
  visits <- make_feed_visits(
    bin = character(0),
    start_times = character(0),
    start_weights = numeric(0),
    end_weights = numeric(0)
  )

  result <- detect_feed_additions(
    visits,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 0)
  expect_true(all(c("date", bin_col2(), "time", "weight_increase", "bin_weight_after_fill") %in% names(result)))
})

test_that("single visit per bin returns no additions", {
  visits <- make_feed_visits(
    bin = c("B1", "B2", "B3"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:05:00", "2025-01-15 08:10:00"),
    start_weights = c(50, 50, 50),
    end_weights = c(45, 45, 45)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("negative weight changes are ignored", {
  visits <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 30),  # Decrease, not increase
    end_weights = c(45, 25)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("empty day in list returns empty dataframe for that day", {
  day1 <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 60),
    end_weights = c(45, 55)
  )

  day2 <- make_feed_visits(
    bin = character(0),
    start_times = character(0),
    start_weights = numeric(0),
    end_weights = numeric(0)
  )

  data_list <- make_feed_list(day1, day2)

  result <- detect_feed_additions(
    data_list,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result[[1]]), 1)
  expect_equal(nrow(result[[2]]), 0)
})

# ----------------------------------------------------------------------------- #
# Parameter validation                                                          #
# ----------------------------------------------------------------------------- #

test_that("invalid min_weight_increase throws error", {
  visits <- make_feed_visits(
    bin = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50,
    end_weights = 45
  )

  expect_error(
    detect_feed_additions(visits, min_weight_increase = -5),
    "min_weight_increase must be a positive number"
  )

  expect_error(
    detect_feed_additions(visits, min_weight_increase = "five"),
    "min_weight_increase must be a positive number"
  )
})

test_that("invalid max_bin_time_gap throws error", {
  visits <- make_feed_visits(
    bin = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50,
    end_weights = 45
  )

  expect_error(
    detect_feed_additions(visits, max_bin_time_gap = 0),
    "max_bin_time_gap must be a positive number"
  )
})

test_that("invalid min_bins_for_group throws error", {
  visits <- make_feed_visits(
    bin = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50,
    end_weights = 45
  )

  expect_error(
    detect_feed_additions(visits, min_bins_for_group = 0),
    "min_bins_for_group must be at least 1"
  )
})

test_that("NULL data throws error", {
  expect_error(
    detect_feed_additions(NULL),
    "data cannot be NULL"
  )
})

test_that("missing required columns throws error", {
  incomplete_visits <- data.frame(
    bin = "B1",
    start = as.POSIXct("2025-01-15 08:00:00", tz = "America/Vancouver")
  )
  colnames(incomplete_visits)[1] <- bin_col2()

  expect_error(
    detect_feed_additions(incomplete_visits),
    "Missing required columns"
  )
})

# ----------------------------------------------------------------------------- #
# Same-bin aggregation within time window                                       #
# ----------------------------------------------------------------------------- #

test_that("multiple additions to same bin within time gap are aggregated", {

  # Scenario: Farmer adds feed to B1 three times in quick succession

# Visit 1: ends with 10kg
  # (farmer adds 8kg -> 18kg)
  # Visit 2: starts 18kg, ends 15kg
  # (farmer adds 15kg -> 30kg)
  # Visit 3: starts 30kg, ends 28kg
  # (farmer adds 7kg -> 35kg)
  # Visit 4: starts 35kg
  #
  # Three detected jumps: +8, +15, +7 = 30kg total
  # Should aggregate to: time=visit2 start, weight=30, bin_weight_after_fill=35

  visits <- make_feed_visits(
    bin = c("B1", "B1", "B1", "B1"),
    start_times = c(
      "2025-01-15 08:00:00",
      "2025-01-15 08:01:00",  # 1 min after - detects +8kg jump
      "2025-01-15 08:02:00",  # 1 min after - detects +15kg jump
      "2025-01-15 08:03:00"   # 1 min after - detects +7kg jump
    ),
    start_weights = c(10, 18, 30, 35),
    end_weights = c(10, 15, 28, 33)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 300,  # 5 minutes - all additions within this window
    aggregate_all_bin = FALSE
  )

  # Should have 1 aggregated event, not 3 separate ones
  expect_equal(nrow(result), 1)
  expect_equal(result[[bin_col2()]][1], "B1")
  # Total weight: 8 + 15 + 7 = 30
  expect_equal(result$weight_increase[1], 30)
  # bin_weight_after_fill should be from the LAST addition: 35
  expect_equal(result$bin_weight_after_fill[1], 35)
})

test_that("same-bin aggregation uses first time, sum weight, and final bin_weight", {
  # More explicit test of output field semantics
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B1"),
    start_times = c(
      "2025-01-15 08:00:00",
      "2025-01-15 08:05:00",  # First jump detected here
      "2025-01-15 08:10:00"   # Second jump detected here
    ),
    start_weights = c(20, 50, 80),  # +30 then +30 = 60 total
    end_weights = c(15, 45, 70)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 600,  # 10 minutes
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 1)
  # time should be the FIRST detection time (08:05:00)
  expect_equal(
    as.character(result$time[1]),
    as.character(as.POSIXct("2025-01-15 08:05:00", tz = "America/Vancouver"))
  )
  # weight_increase should be SUM: 35 + 35 = 70 (50-15 + 80-45)
  expect_equal(result$weight_increase[1], 70)
  # bin_weight_after_fill should be FINAL: 80
  expect_equal(result$bin_weight_after_fill[1], 80)
})

test_that("additions to same bin OUTSIDE time gap remain separate", {
  # Two additions to B1 that are 2 hours apart
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B1"),
    start_times = c(
      "2025-01-15 08:00:00",
      "2025-01-15 08:05:00",  # First addition
      "2025-01-15 10:10:00"   # Second addition - 2 hours later
    ),
    start_weights = c(20, 50, 80),
    end_weights = c(15, 40, 70)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 600,  # 10 minutes - second addition is outside this
    aggregate_all_bin = FALSE
  )

  # Should have 2 separate events
  expect_equal(nrow(result), 2)
  expect_equal(result$weight_increase[1], 35)  # 50 - 15
  expect_equal(result$bin_weight_after_fill[1], 50)
  expect_equal(result$weight_increase[2], 40)  # 80 - 40
  expect_equal(result$bin_weight_after_fill[2], 80)
})

test_that("multiple bins each with multiple rapid additions", {
  # B1 gets 3 rapid additions, B2 gets 2 rapid additions
  visits <- make_feed_visits(
    bin = c("B1", "B1", "B1", "B1", "B2", "B2", "B2"),
    start_times = c(
      # B1: 4 visits with 3 jumps
      "2025-01-15 08:00:00",
      "2025-01-15 08:01:00",
      "2025-01-15 08:02:00",
      "2025-01-15 08:03:00",
      # B2: 3 visits with 2 jumps
      "2025-01-15 08:00:30",
      "2025-01-15 08:01:30",
      "2025-01-15 08:02:30"
    ),
    start_weights = c(
      10, 20, 35, 50,  # B1: +10, +15, +15 = 40 total
      15, 30, 45       # B2: +15, +15 = 30 total
    ),
    end_weights = c(
      8, 18, 33, 48,   # B1 end weights
      12, 28, 42       # B2 end weights
    )
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 300,
    aggregate_all_bin = FALSE
  )

  # Should have 2 events: one per bin
  expect_equal(nrow(result), 2)

  # Find B1 and B2 rows
  b1_row <- which(result[[bin_col2()]] == "B1")
  b2_row <- which(result[[bin_col2()]] == "B2")

  # B1: total = 12 + 17 + 17 = 46, final bin_weight = 50
  expect_equal(result$weight_increase[b1_row], 12 + 17 + 17)
  expect_equal(result$bin_weight_after_fill[b1_row], 50)

  # B2: total = 18 + 17 = 35, final bin_weight = 45
  expect_equal(result$weight_increase[b2_row], 18 + 17)
  expect_equal(result$bin_weight_after_fill[b2_row], 45)
})

test_that("single addition per bin is unchanged by same-bin aggregation", {
  # Just one addition to B1 - should work as before
  visits <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 60),
    end_weights = c(45, 55)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    max_bin_time_gap = 300,
    aggregate_all_bin = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$weight_increase[1], 15)  # 60 - 45
  expect_equal(result$bin_weight_after_fill[1], 60)
})

test_that("same-bin aggregation works correctly with list input", {
  # Day 1 has multiple rapid additions
  day1 <- make_feed_visits(
    bin = c("B1", "B1", "B1"),
    start_times = c(
      "2025-01-15 08:00:00",
      "2025-01-15 08:01:00",
      "2025-01-15 08:02:00"
    ),
    start_weights = c(10, 25, 40),
    end_weights = c(8, 22, 35)
  )

  data_list <- make_feed_list(day1)

  result <- detect_feed_additions(
    data_list,
    min_weight_increase = 5,
    max_bin_time_gap = 300,
    aggregate_all_bin = FALSE
  )

  expect_true(is.list(result))
  expect_equal(nrow(result[[1]]), 1)  # Aggregated into 1 event
  # Total: 17 + 18 = 35
  expect_equal(result[[1]]$weight_increase[1], 17 + 18)
  expect_equal(result[[1]]$bin_weight_after_fill[1], 40)
})

test_that("empty input with aggregate_all_bin = TRUE returns correct structure", {
  visits <- make_feed_visits(
    bin = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 52),  # Only 2 kg increase - below threshold
    end_weights = c(48, 50)
  )

  result <- detect_feed_additions(
    visits,
    min_weight_increase = 5,
    aggregate_all_bin = TRUE
  )

  expect_equal(nrow(result), 0)
  expect_true(all(c("date", "event_id", "event_start", "event_end", 
                    "bins_filled", "avg_weight_increase", 
                    "min_weight_increase", "max_weight_increase") %in% names(result)))
})
