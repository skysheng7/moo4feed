# ----------------------------------------------------------------------------- #
# Synthetic data helpers                                                        #
# ----------------------------------------------------------------------------- #

make_availability_visits <- function(cow_ids, bin_ids, start_times, start_weights) {
  n <- length(cow_ids)
  df <- data.frame(
    date = if (n == 0) character(0) else rep("2025-01-15", n),
    cow = cow_ids,
    bin = bin_ids,
    start = as.POSIXct(start_times, tz = "America/Vancouver"),
    start_weight = start_weights,
    stringsAsFactors = FALSE
  )
  colnames(df) <- c("date", id_col2(), bin_col2(), start_col2(), start_weight_col2())
  df
}

make_feed_additions <- function(bin_ids, times, weight_increases, bin_weight_after_fills = NULL) {
  n <- length(bin_ids)
  # If bin_weight_after_fill not provided, use weight_increase as default
  # (assumes bin was empty before fill - for backward compatibility)
  if (is.null(bin_weight_after_fills)) {
    bin_weight_after_fills <- weight_increases
  }
  df <- data.frame(
    date = if (n == 0) character(0) else rep("2025-01-15", n),
    bin = bin_ids,
    time = as.POSIXct(times, tz = "America/Vancouver"),
    weight_increase = weight_increases,
    bin_weight_after_fill = bin_weight_after_fills,
    stringsAsFactors = FALSE
  )
  colnames(df)[2] <- bin_col2()
  df
}

# ----------------------------------------------------------------------------- #
# Happy path                                                                    #
# ----------------------------------------------------------------------------- #

test_that("calculate_feed_availability matches visits to feed additions", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 40)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"), # Before visits
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_true("visits" %in% names(result))
  expect_true("daily_summary" %in% names(result))
  expect_equal(result$visits$feed_added_weight, c(60, 60))
  expect_equal(result$visits$bin_weight_after_fill, c(60, 60))
  expect_false(is.na(result$visits$feed_addition_time[1]))
  expect_false(is.na(result$visits$feed_addition_time[2]))
})

test_that("calculate_feed_availability calculates percentages correctly using bin_weight_after_fill", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(60, 30) # 100% and 50% of feed remaining
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)  # bin was empty, so total = amount added
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_equal(result$visits$pct_feed_remaining, c(100, 50))
})

test_that("percentage uses bin_weight_after_fill not weight_increase", {
  # Critical test: bin had residual feed before addition
  # bin_weight_after_fill = 80 (total after fill)
  # weight_increase = 50 (amount added)
  # start_weight = 60
  # Correct: 60/80 = 75%, NOT 60/50 = 120% (capped at 100)

  visits <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(60)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(50),  # Amount added
    bin_weight_after_fills = c(80)  # Total in bin after addition (had 30 residual)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  # Should be 60/80 = 75%, not 60/50 = 100% (capped)
  expect_equal(result$visits$pct_feed_remaining[1], 75)
})

test_that("calculate_feed_availability caps percentage at 100%", {
  visits <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(70) # More than bin_weight_after_fill
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_equal(result$visits$pct_feed_remaining[1], 100) # Capped
})

test_that("calculate_feed_availability matches to most recent feed addition for bin", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1, 1),
    bin_ids = c("B1", "B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 11:00:00", "2025-01-15 14:00:00"),
    start_weights = c(60, 50, 40)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1", "B1"),
    times = c("2025-01-15 07:00:00", "2025-01-15 10:00:00"),
    weight_increases = c(60, 50),
    bin_weight_after_fills = c(60, 50)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  # First visit uses first addition, second and third use second addition
  expect_equal(result$visits$feed_added_weight, c(60, 50, 50))
  expect_equal(result$visits$bin_weight_after_fill, c(60, 50, 50))
  expect_equal(result$visits$pct_feed_remaining, c(100, 100, 80))
})

test_that("calculate_feed_availability creates correct daily summary", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1, 1),
    bin_ids = c("B1", "B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00", "2025-01-15 10:00:00"),
    start_weights = c(60, 45, 30) # 100%, 75%, 50%
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_equal(result$daily_summary$mean_pct_feed_remaining[1], 75) # (100+75+50)/3
  expect_equal(result$daily_summary$median_pct_feed_remaining[1], 75)
  expect_equal(result$daily_summary$total_visits_analyzed[1], 3)
})

test_that("different bins track their own feed amounts correctly", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1, 1, 1),
    bin_ids = c("B1", "B2", "B1", "B2"),
    start_times = c(
      "2025-01-15 08:00:00", "2025-01-15 08:05:00",
      "2025-01-15 09:00:00", "2025-01-15 09:05:00"
    ),
    start_weights = c(60, 40, 30, 20)
  )

  # B1 gets 60kg, B2 gets 50kg
  feed_additions <- make_feed_additions(
    bin_ids = c("B1", "B2"),
    times = c("2025-01-15 07:00:00", "2025-01-15 07:05:00"),
    weight_increases = c(60, 50),
    bin_weight_after_fills = c(60, 50)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  # B1 visits should use bin_weight_after_fill=60: 60/60=100%, 30/60=50%
  # B2 visits should use bin_weight_after_fill=50: 40/50=80%, 20/50=40%
  expect_equal(result$visits$feed_added_weight[1], 60) # B1 first visit
  expect_equal(result$visits$feed_added_weight[2], 50) # B2 first visit
  expect_equal(result$visits$feed_added_weight[3], 60) # B1 second visit
  expect_equal(result$visits$feed_added_weight[4], 50) # B2 second visit

  expect_equal(result$visits$pct_feed_remaining[1], 100) # B1: 60/60
  expect_equal(result$visits$pct_feed_remaining[2], 80) # B2: 40/50
  expect_equal(result$visits$pct_feed_remaining[3], 50) # B1: 30/60
  expect_equal(result$visits$pct_feed_remaining[4], 40) # B2: 20/50
})

test_that("calculate_feed_availability works with list input", {
  visits <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(60)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )

  visit_list <- list("2025-01-15" = visits)
  addition_list <- list("2025-01-15" = feed_additions)

  result <- calculate_feed_availability(visit_list, addition_list)

  expect_true(is.list(result$visits))
  expect_true(is.list(result$daily_summary))
  expect_equal(length(result$visits), 1)
  expect_equal(length(result$daily_summary), 1)
})

# ----------------------------------------------------------------------------- #
# Edge cases                                                                    #
# ----------------------------------------------------------------------------- #

test_that("no feed additions returns NA for all visits", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    start_weights = c(50, 40)
  )

  feed_additions <- make_feed_additions(
    bin_ids = character(0),
    times = character(0),
    weight_increases = numeric(0)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_true(all(is.na(result$visits$feed_addition_time)))
  expect_true(all(is.na(result$visits$pct_feed_remaining)))
})

test_that("visit before any feed addition returns NA", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-15 06:00:00", "2025-01-15 09:00:00"), # First before addition
    start_weights = c(50, 40)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 08:00:00"),
    weight_increases = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_true(is.na(result$visits$feed_addition_time[1]))
  expect_true(is.na(result$visits$pct_feed_remaining[1]))
  expect_false(is.na(result$visits$feed_addition_time[2]))
})

test_that("zero bin_weight_after_fill returns NA percentage", {
  visits <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(50)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(0), # Zero feed added
    bin_weight_after_fills = c(0) # Zero bin weight
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_true(is.na(result$visits$pct_feed_remaining[1]))
})

test_that("empty visits returns empty result with correct structure", {
  visits <- make_availability_visits(
    cow_ids = integer(0),
    bin_ids = character(0),
    start_times = character(0),
    start_weights = numeric(0)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_equal(nrow(result$visits), 0)
  expect_true(all(c(
    "feed_addition_time", "feed_added_weight", "bin_weight_after_fill",
    "pct_feed_remaining"
  ) %in% names(result$visits)))
})

test_that("all visits before feed additions returns empty daily summary", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-15 05:00:00", "2025-01-15 06:00:00"),
    start_weights = c(50, 40)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 08:00:00"), # After all visits
    weight_increases = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_equal(nrow(result$daily_summary), 0)
})

test_that("multiple animals tracked separately in daily summary", {
  visits <- make_availability_visits(
    cow_ids = c(1, 1, 2, 2),
    bin_ids = c("B1", "B1", "B1", "B1"),
    start_times = c(
      "2025-01-15 08:00:00", "2025-01-15 09:00:00",
      "2025-01-15 08:30:00", "2025-01-15 09:30:00"
    ),
    start_weights = c(60, 30, 45, 15)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_equal(nrow(result$daily_summary), 2)
  expect_equal(result$daily_summary$mean_pct_feed_remaining[
    result$daily_summary[[id_col2()]] == 1
  ], 75) # (100+50)/2
  expect_equal(result$daily_summary$mean_pct_feed_remaining[
    result$daily_summary[[id_col2()]] == 2
  ], 50) # (75+25)/2
})

test_that("single visit per animal returns NA for SD", {
  visits <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(60)
  )

  feed_additions <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 07:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )

  result <- calculate_feed_availability(visits, feed_additions)

  expect_true(is.na(result$daily_summary$sd_pct_feed_remaining[1]))
})

test_that("empty day in list returns empty dataframes for that day", {
  day1 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(60)
  )

  day2 <- make_availability_visits(
    cow_ids = integer(0),
    bin_ids = character(0),
    start_times = character(0),
    start_weights = numeric(0)
  )

  additions1 <- make_feed_additions(c("B1"), c("2025-01-15 07:00:00"), c(60))
  additions2 <- make_feed_additions(character(0), character(0), numeric(0))

  visit_list <- list("2025-01-15" = day1, "2025-01-16" = day2)
  addition_list <- list("2025-01-15" = additions1, "2025-01-16" = additions2)

  result <- calculate_feed_availability(visit_list, addition_list)

  expect_equal(nrow(result$visits[[1]]), 1)
  expect_equal(nrow(result$visits[[2]]), 0)
})

# ----------------------------------------------------------------------------- #
# Input validation                                                              #
# ----------------------------------------------------------------------------- #

test_that("NULL visit_data throws error", {
  feed_additions <- make_feed_additions(c("B1"), c("2025-01-15 07:00:00"), c(60))

  expect_error(
    calculate_feed_availability(NULL, feed_additions),
    "visit_data cannot be NULL"
  )
})

test_that("NULL feed_addition_data throws error", {
  visits <- make_availability_visits(
    cow_ids = 1,
    bin_ids = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50
  )

  expect_error(
    calculate_feed_availability(visits, NULL),
    "feed_addition_data cannot be NULL"
  )
})

test_that("feed_addition_data with event_id (aggregated) throws error", {
  visits <- make_availability_visits(
    cow_ids = 1,
    bin_ids = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50
  )

  # Aggregated feed data (has event_id) - should be rejected
  aggregated_events <- data.frame(
    date = "2025-01-15",
    event_id = 1,
    event_start = as.POSIXct("2025-01-15 07:00:00", tz = "America/Vancouver"),
    event_end = as.POSIXct("2025-01-15 07:10:00", tz = "America/Vancouver"),
    bins_filled = 3,
    avg_weight_increase = 60
  )

  expect_error(
    calculate_feed_availability(visits, aggregated_events),
    "appears to be aggregated"
  )
})

test_that("missing required columns in visit_data throws error", {
  incomplete_visits <- data.frame(
    cow = 1,
    bin = "B1"
  )
  colnames(incomplete_visits)[1] <- id_col2()

  feed_additions <- make_feed_additions(c(1), c("2025-01-15 07:00:00"), c(60))

  expect_error(
    calculate_feed_availability(incomplete_visits, feed_additions),
    "Missing required columns in visit_data"
  )
})

test_that("missing required columns in feed_addition_data throws error", {
  visits <- make_availability_visits(
    cow_ids = 1,
    bin_ids = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50
  )

  incomplete_additions <- data.frame(
    bin = "B1",
    time = as.POSIXct("2025-01-15 07:00:00", tz = "America/Vancouver")
    # Missing weight_increase
  )
  colnames(incomplete_additions)[1] <- bin_col2()

  expect_error(
    calculate_feed_availability(visits, incomplete_additions),
    "Missing columns"
  )
})



test_that("mismatched day names throws error", {
  visits_list <- list("2025-01-15" = make_availability_visits(
    1, "B1",
    "2025-01-15 08:00:00", 50
  ))
  additions_list <- list("2025-01-16" = make_feed_additions("B1", "2025-01-16 07:00:00", 60, 60))

  expect_error(
    calculate_feed_availability(visits_list, additions_list),
    "matching day names"
  )
})

test_that("missing bin_weight_after_fill column throws error", {
  visits <- make_availability_visits(
    cow_ids = 1,
    bin_ids = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50
  )

  # Old-style feed additions without bin_weight_after_fill
  old_additions <- data.frame(
    date = "2025-01-15",
    time = as.POSIXct("2025-01-15 07:00:00", tz = "America/Vancouver"),
    weight_increase = 60
  )
  old_additions[[bin_col2()]] <- "B1"

  expect_error(
    calculate_feed_availability(visits, old_additions),
    "bin_weight_after_fill"
  )
})

test_that("invalid feed_addition_data type throws error", {
  visits <- make_availability_visits(
    cow_ids = 1,
    bin_ids = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50
  )

  # Empty list - not valid
  expect_error(
    calculate_feed_availability(visits, list()),
    "must be a data frame or named list of data frames"
  )
})

test_that("feed_addition_data missing weight_increase throws error", {
  visits <- make_availability_visits(
    cow_ids = 1,
    bin_ids = "B1",
    start_times = "2025-01-15 08:00:00",
    start_weights = 50
  )

  # Missing weight_increase column
  incomplete_additions <- data.frame(
    date = "2025-01-15",
    time = as.POSIXct("2025-01-15 07:00:00", tz = "America/Vancouver"),
    bin_weight_after_fill = 60
  )
  incomplete_additions[[bin_col2()]] <- "B1"

  expect_error(
    calculate_feed_availability(visits, incomplete_additions),
    "Missing columns"
  )
})

# ----------------------------------------------------------------------------- #
# Multi-day cross-day feed addition matching                                   #
# ----------------------------------------------------------------------------- #

test_that("visits early in day 2 match feed additions from previous day", {
  # Day 1: Feed added at 20:00 (8 PM)
  visits1 <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 10:00:00"),
    start_weights = c(60, 50)
  )
  visits1$date <- "2025-01-15"

  additions1 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 20:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions1$date <- "2025-01-15"

  # Day 2: Early morning visits (before feed added on day 2)
  # Feed added later at 12:00 (noon)
  visits2 <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-16 06:00:00", "2025-01-16 14:00:00"),
    start_weights = c(40, 55)
  )
  visits2$date <- "2025-01-16"

  additions2 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-16 12:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions2$date <- "2025-01-16"

  visit_list <- list("2025-01-15" = visits1, "2025-01-16" = visits2)
  addition_list <- list("2025-01-15" = additions1, "2025-01-16" = additions2)

  result <- calculate_feed_availability(visit_list, addition_list)

  # Day 2, first visit (06:00) should match day 1 feed addition (20:00)
  expect_equal(
    as.character(result$visits[["2025-01-16"]]$feed_addition_time[1]),
    as.character(as.POSIXct("2025-01-15 20:00:00", tz = "America/Vancouver"))
  )
  expect_equal(result$visits[["2025-01-16"]]$feed_added_weight[1], 60)
  expect_equal(result$visits[["2025-01-16"]]$pct_feed_remaining[1], 40 / 60 * 100)

  # Day 2, second visit (14:00) should match day 2 feed addition (12:00)
  expect_equal(
    as.character(result$visits[["2025-01-16"]]$feed_addition_time[2]),
    as.character(as.POSIXct("2025-01-16 12:00:00", tz = "America/Vancouver"))
  )
  expect_equal(result$visits[["2025-01-16"]]$feed_added_weight[2], 60)
  expect_equal(result$visits[["2025-01-16"]]$pct_feed_remaining[2], 55 / 60 * 100)
})

test_that("multiple bins track cross-day correctly", {
  # Day 1: B1 and B2 both get feed
  visits1 <- make_availability_visits(
    cow_ids = c(1, 2),
    bin_ids = c("B1", "B2"),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00"),
    start_weights = c(60, 50)
  )
  visits1$date <- "2025-01-15"

  additions1 <- make_feed_additions(
    bin_ids = c("B1", "B2"),
    times = c("2025-01-15 19:00:00", "2025-01-15 19:30:00"),
    weight_increases = c(60, 50),
    bin_weight_after_fills = c(60, 50)
  )
  additions1$date <- "2025-01-15"

  # Day 2: Early visits to both bins before day 2 feed additions
  visits2 <- make_availability_visits(
    cow_ids = c(1, 2, 1, 2),
    bin_ids = c("B1", "B2", "B1", "B2"),
    start_times = c(
      "2025-01-16 05:00:00", "2025-01-16 05:30:00",
      "2025-01-16 13:00:00", "2025-01-16 13:30:00"
    ),
    start_weights = c(30, 25, 55, 45)
  )
  visits2$date <- "2025-01-16"

  additions2 <- make_feed_additions(
    bin_ids = c("B1", "B2"),
    times = c("2025-01-16 11:00:00", "2025-01-16 11:30:00"),
    weight_increases = c(60, 50),
    bin_weight_after_fills = c(60, 50)
  )
  additions2$date <- "2025-01-16"

  visit_list <- list("2025-01-15" = visits1, "2025-01-16" = visits2)
  addition_list <- list("2025-01-15" = additions1, "2025-01-16" = additions2)

  result <- calculate_feed_availability(visit_list, addition_list)

  # Day 2 early visits should match day 1 feed additions
  expect_equal(result$visits[["2025-01-16"]]$feed_added_weight[1], 60) # B1 early
  expect_equal(result$visits[["2025-01-16"]]$feed_added_weight[2], 50) # B2 early

  # Day 2 later visits should match day 2 feed additions
  expect_equal(result$visits[["2025-01-16"]]$feed_added_weight[3], 60) # B1 later
  expect_equal(result$visits[["2025-01-16"]]$feed_added_weight[4], 50) # B2 later
})

test_that("first day not affected when no previous day exists", {
  # Day 1: should work normally without previous day
  visits1 <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-15 06:00:00", "2025-01-15 10:00:00"),
    start_weights = c(50, 60)
  )
  visits1$date <- "2025-01-15"

  additions1 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 08:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions1$date <- "2025-01-15"

  visit_list <- list("2025-01-15" = visits1)
  addition_list <- list("2025-01-15" = additions1)

  result <- calculate_feed_availability(visit_list, addition_list)

  # First visit before addition should be NA
  expect_true(is.na(result$visits[["2025-01-15"]]$feed_addition_time[1]))
  expect_true(is.na(result$visits[["2025-01-15"]]$pct_feed_remaining[1]))

  # Second visit after addition should match day 1 addition
  expect_false(is.na(result$visits[["2025-01-15"]]$feed_addition_time[2]))
  expect_equal(result$visits[["2025-01-15"]]$pct_feed_remaining[2], 100)
})

test_that("no feed additions on previous day still checks current day", {
  # Day 1: no feed additions
  visits1 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(50)
  )
  visits1$date <- "2025-01-15"

  additions1 <- make_feed_additions(
    bin_ids = character(0),
    times = character(0),
    weight_increases = numeric(0)
  )
  additions1$date <- character(0)

  # Day 2: has feed addition
  visits2 <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-16 06:00:00", "2025-01-16 10:00:00"),
    start_weights = c(50, 60)
  )
  visits2$date <- "2025-01-16"

  additions2 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-16 08:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions2$date <- "2025-01-16"

  visit_list <- list("2025-01-15" = visits1, "2025-01-16" = visits2)
  addition_list <- list("2025-01-15" = additions1, "2025-01-16" = additions2)

  result <- calculate_feed_availability(visit_list, addition_list)

  # Day 2 first visit before addition should be NA (no prev day feed)
  expect_true(is.na(result$visits[["2025-01-16"]]$feed_addition_time[1]))

  # Day 2 second visit should match day 2 addition
  expect_false(is.na(result$visits[["2025-01-16"]]$feed_addition_time[2]))
  expect_equal(result$visits[["2025-01-16"]]$pct_feed_remaining[2], 100)
})

test_that("non-consecutive days handled correctly", {
  # Day 1: Jan 15
  visits1 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(60)
  )
  visits1$date <- "2025-01-15"

  additions1 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 20:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions1$date <- "2025-01-15"

  # Day 3: Jan 17 (skipping Jan 16)
  # Early visit should NOT match Jan 15 (not previous day)
  visits3 <- make_availability_visits(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B1"),
    start_times = c("2025-01-17 06:00:00", "2025-01-17 14:00:00"),
    start_weights = c(40, 55)
  )
  visits3$date <- "2025-01-17"

  additions3 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-17 12:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions3$date <- "2025-01-17"

  visit_list <- list("2025-01-15" = visits1, "2025-01-17" = visits3)
  addition_list <- list("2025-01-15" = additions1, "2025-01-17" = additions3)

  result <- calculate_feed_availability(visit_list, addition_list)

  # Jan 17 first visit should be NA (no previous day Jan 16 in data)
  expect_true(is.na(result$visits[["2025-01-17"]]$feed_addition_time[1]))
  expect_true(is.na(result$visits[["2025-01-17"]]$pct_feed_remaining[1]))

  # Jan 17 second visit should match Jan 17 addition
  expect_false(is.na(result$visits[["2025-01-17"]]$feed_addition_time[2]))
})

test_that("missing previous day with gap in data", {
  # Day 1: Jan 10
  visits1 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-10 08:00:00"),
    start_weights = c(60)
  )
  visits1$date <- "2025-01-10"

  additions1 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-10 20:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions1$date <- "2025-01-10"

  # Day 2: Jan 11 (has previous day Jan 10)
  visits2 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-11 06:00:00"),
    start_weights = c(50)
  )
  visits2$date <- "2025-01-11"

  additions2 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-11 12:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions2$date <- "2025-01-11"

  visit_list <- list("2025-01-10" = visits1, "2025-01-11" = visits2)
  addition_list <- list("2025-01-10" = additions1, "2025-01-11" = additions2)

  result <- calculate_feed_availability(visit_list, addition_list)

  # Jan 11 early visit should match Jan 10 feed addition
  expect_equal(
    as.character(result$visits[["2025-01-11"]]$feed_addition_time[1]),
    as.character(as.POSIXct("2025-01-10 20:00:00", tz = "America/Vancouver"))
  )
  expect_equal(result$visits[["2025-01-11"]]$pct_feed_remaining[1], 50 / 60 * 100)
})

test_that("out of order day names still find correct previous day", {
  # Data provided out of chronological order
  # Day 2: Jan 16
  visits2 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-16 06:00:00"),
    start_weights = c(50)
  )
  visits2$date <- "2025-01-16"

  additions2 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-16 12:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions2$date <- "2025-01-16"

  # Day 1: Jan 15 (comes after in list)
  visits1 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(60)
  )
  visits1$date <- "2025-01-15"

  additions1 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 20:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions1$date <- "2025-01-15"

  # List is out of order: Jan 16 first, then Jan 15
  visit_list <- list("2025-01-16" = visits2, "2025-01-15" = visits1)
  addition_list <- list("2025-01-16" = additions2, "2025-01-15" = additions1)

  result <- calculate_feed_availability(visit_list, addition_list)

  # Jan 16 should still find Jan 15 as previous day (by date, not list order)
  expect_equal(
    as.character(result$visits[["2025-01-16"]]$feed_addition_time[1]),
    as.character(as.POSIXct("2025-01-15 20:00:00", tz = "America/Vancouver"))
  )
  expect_equal(result$visits[["2025-01-16"]]$pct_feed_remaining[1], 50 / 60 * 100)
})

test_that("unparseable day names fallback gracefully", {
  # Use non-date day names like "day1", "day2"
  visits1 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-15 08:00:00"),
    start_weights = c(60)
  )
  visits1$date <- "day1"  # Non-parseable

  additions1 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-15 20:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions1$date <- "day1"

  visits2 <- make_availability_visits(
    cow_ids = c(1),
    bin_ids = c("B1"),
    start_times = c("2025-01-16 06:00:00"),
    start_weights = c(50)
  )
  visits2$date <- "day2"  # Non-parseable

  additions2 <- make_feed_additions(
    bin_ids = c("B1"),
    times = c("2025-01-16 12:00:00"),
    weight_increases = c(60),
    bin_weight_after_fills = c(60)
  )
  additions2$date <- "day2"

  visit_list <- list("day1" = visits1, "day2" = visits2)
  addition_list <- list("day1" = additions1, "day2" = additions2)

  # Should not error, just won't find previous day
  result <- calculate_feed_availability(visit_list, addition_list)

  # Day2 early visit should be NA (can't determine previous day from "day1")
  expect_true(is.na(result$visits[["day2"]]$feed_addition_time[1]))
})
