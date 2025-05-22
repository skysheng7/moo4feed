# ----------------------------------------------------------------------------- #
# feed_water_summary()                                                    #
# ----------------------------------------------------------------------------- #

test_that("feed_water_summary() merges feed and water summaries and applies checks", {
  feed_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01")),
    cow = c("A", "B"),
    intake = c(20, 80),
    duration = c(200, 300)
  )

  drink_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01")),
    cow = c("A", "B"),
    intake = c(50, 200),
    duration = c(100, 150)
  )

  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  set_id_col2("cow")
  cfg <- qc_config(low_feed_intake = 30, high_feed_intake = 70,
                   low_wat_intake = 60, high_wat_intake = 180)

  result <- feed_water_summary(feed_df, drink_df, warn_df, cfg = cfg)

  expect_named(result, c("summary", "warn"))
  expect_s3_class(result$summary, "data.frame")
  expect_s3_class(result$warn, "data.frame")
  expect_true("A" %in% result$summary$cow)
  expect_equal(result$warn$low_daily_feed_intake_cows[1], "A, 20")
  expect_equal(result$warn$high_daily_feed_intake_cows[1], "B, 80")
  expect_equal(result$warn$high_daily_water_intake_cows[1], "B, 200") 
})

test_that("feed_water_summary() handles NULL inputs and bad types", {
  warn <- tibble::tibble(
    date = as.Date("2024-01-01"),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  cfg <- qc_config()
  set_id_col2("cow")
  set_intake_col2("intake")
  set_duration_col2("duration")

  # Invalid feed input
  expect_error(feed_water_summary(feed = "not_a_df", water = NULL, warn = warn, cfg = cfg),
               "`feed` must be a data frame")

  # Invalid warn input
  expect_error(feed_water_summary(feed = NULL, water = NULL, warn = "not_a_df", cfg = cfg),
               "`warn` must be a data frame")
})

test_that("feed_water_summary handles lists of data frames", {
  # Setup test data
  feed_list <- list(
    "2024-01-01" = tibble::tibble(
      cow = c("A", "B"),
      intake = c(20, 80),
      duration = c(200, 300),
      date = as.Date(c("2024-01-01", "2024-01-01"))
    ),
    "2024-01-02" = tibble::tibble(
      cow = c("A", "C"),
      intake = c(25, 85),
      duration = c(210, 310),
      date = as.Date(c("2024-01-02", "2024-01-02"))
    )
  )

  water_list <- list(
    "2024-01-01" = tibble::tibble(
      cow = c("A", "B"),
      intake = c(50, 200),
      duration = c(100, 150),
      date = as.Date(c("2024-01-01", "2024-01-01"))
    ),
    "2024-01-02" = tibble::tibble(
      cow = c("A", "C"),
      intake = c(55, 210),
      duration = c(110, 160),
      date = as.Date(c("2024-01-02", "2024-01-02"))
    )
  )

  warn <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  # Test with lists
  result <- feed_water_summary(
    feed_list, 
    water_list, 
    warn,
    cfg = qc_config(),
    id_col = "cow",
    intake_col = "intake",
    dur_col = "duration"
  )

  # Assertions
  expect_true(is.list(result))
  expect_true(all(c("summary", "warn") %in% names(result)))
  expect_equal(nrow(result$summary), 4)  # 3 cows * 2 days
  expect_equal(ncol(result$summary), 8)  # date, cow, feed_intake, feed_duration, feed_visits, water_intake, water_duration, water_visits
  
  # Check if dates are preserved
  expect_equal(
    sort(unique(result$summary$date)), 
    as.Date(c("2024-01-01", "2024-01-02"))
  )
  
  # Check if all cows are present
  expect_equal(
    sort(unique(result$summary$cow)),
    c("A", "B", "C")
  )
  
  # Test error handling
  expect_error(
    feed_water_summary(
      list("2024-01-01" = "not a data frame"),
      water_list,
      warn
    ),
    "All elements in `feed` list must be data frames"
  )
  
  expect_error(
    feed_water_summary(
      feed_list,
      list("2024-01-01" = "not a data frame"),
      warn
    ),
    "All elements in `water` list must be data frames"
  )
  
  # Test mixing data frame and list inputs
  feed_df <- do.call(rbind, lapply(names(feed_list), function(date) {
    df <- feed_list[[date]]
    df$date <- as.Date(date)
    return(df)
  }))
  
  mixed_result <- feed_water_summary(
    feed_df,
    water_list,
    warn,
    cfg = qc_config(),
    id_col = "cow",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  expect_equal(
    result$summary,
    mixed_result$summary
  )
})



test_that("feed_water_summary() works with single data frames", {
  feed_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01")),
    cow = c("A", "B"),
    intake = c(20, 80),
    duration = c(200, 300)
  )

  water_df <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01")),
    cow = c("A", "B"),
    intake = c(50, 200),
    duration = c(100, 150)
  )

  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  set_id_col2("cow")
  cfg <- qc_config(low_feed_intake = 30, high_feed_intake = 70,
                   low_wat_intake = 60, high_wat_intake = 180)

  result <- feed_water_summary(feed_df, water_df, warn_df, cfg = cfg)

  expect_named(result, c("summary", "warn"))
  expect_s3_class(result$summary, "data.frame")
  expect_s3_class(result$warn, "data.frame")
  expect_true("A" %in% result$summary$cow)
  expect_equal(result$warn$low_daily_feed_intake_cows[1], "A, 20")
  expect_equal(result$warn$high_daily_feed_intake_cows[1], "B, 80")
  expect_equal(result$warn$high_daily_water_intake_cows[1], "B, 200") 
})

test_that("feed_water_summary() handles NULL inputs and bad types", {
  warn <- tibble::tibble(
    date = as.Date("2024-01-01"),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  cfg <- qc_config()
  set_id_col2("cow")
  set_intake_col2("intake")
  set_duration_col2("duration")

  # feed and water are NULL — should error
  expect_error(
    feed_water_summary(feed = NULL, water = NULL, warn = warn, cfg = cfg),
    "`water` and `feed` cannot both be NULL"
  )

  # Invalid feed input
  expect_error(
    feed_water_summary(feed = "not_a_df", water = NULL, warn = warn, cfg = cfg),
    "`feed` must be a data frame or a list of data frames"
  )

  # Invalid warn input
  expect_error(
    feed_water_summary(feed = NULL, water = NULL, warn = "not_a_df", cfg = cfg),
    "`warn` must be a data frame"
  )
})

test_that("feed_water_summary handles lists of data frames", {
  # Setup test data
  feed_list <- list(
    "2024-01-01" = tibble::tibble(
      date = as.Date("2024-01-01"),
      cow = c("A", "B"),
      intake = c(20, 80),
      duration = c(200, 300)
    ),
    "2024-01-02" = tibble::tibble(
      date = as.Date("2024-01-02"),
      cow = c("A", "C"),
      intake = c(25, 85),
      duration = c(210, 310)
    )
  )

  water_list <- list(
    "2024-01-01" = tibble::tibble(
      date = as.Date("2024-01-01"),
      cow = c("A", "B"),
      intake = c(50, 200),
      duration = c(100, 150)
    ),
    "2024-01-02" = tibble::tibble(
      date = as.Date("2024-01-02"),
      cow = c("A", "C"),
      intake = c(55, 210),
      duration = c(110, 160)
    )
  )

  warn <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  # Test with lists
  result <- feed_water_summary(
    feed_list, 
    water_list, 
    warn,
    cfg = qc_config(),
    id_col = "cow",
    intake_col = "intake",
    dur_col = "duration"
  )

  # Assertions
  expect_true(is.list(result))
  expect_true(all(c("summary", "warn") %in% names(result)))
  expect_equal(nrow(result$summary), 4) 
  expect_equal(ncol(result$summary), 8)  # date, cow, feed_intake, feed_duration, feed_visits, water_intake, water_duration, water_visits
  
  # Check if dates are preserved
  expect_equal(
    sort(unique(result$summary$date)), 
    as.Date(c("2024-01-01", "2024-01-02"))
  )
  
  # Check if all cows are present
  expect_equal(
    sort(unique(result$summary$cow)),
    c("A", "B", "C")
  )
  
  # Test error handling for missing date column
  expect_error(
    feed_water_summary(
      list("2024-01-01" = tibble::tibble(cow = "A")),
      water_list,
      warn
    ),
    "All data frames in `feed` list must have a 'date' column"
  )
  
  expect_error(
    feed_water_summary(
      feed_list,
      list("2024-01-01" = tibble::tibble(cow = "A")),
      warn
    ),
    "All data frames in `water` list must have a 'date' column"
  )
  
  # Test error handling for invalid data frame elements
  expect_error(
    feed_water_summary(
      list("2024-01-01" = "not a data frame"),
      water_list,
      warn
    ),
    "All elements in `feed` list must be data frames"
  )
  
  expect_error(
    feed_water_summary(
      feed_list,
      list("2024-01-01" = "not a data frame"),
      warn
    ),
    "All elements in `water` list must be data frames"
  )
  
  # Test mixing data frame and list inputs
  feed_df <- merge_list_df(feed_list)
  
  mixed_result <- feed_water_summary(
    feed_df,
    water_list,
    warn,
    cfg = qc_config(),
    id_col = "cow",
    intake_col = "intake",
    dur_col = "duration"
  )
  
  expect_equal(
    result$summary,
    mixed_result$summary
  )
})

test_that("feed_water_summary handles multiple visits per cow per day", {
  # Setup test data with multiple visits per cow per day
  feed_list <- list(
    "2024-01-01" = tibble::tibble(
      date = rep(as.Date("2024-01-01"), 6),
      cow = c("A", "A", "A", "B", "B", "C"),  # Cow A: 3 visits, B: 2 visits, C: 1 visit
      intake = c(10, 15, 20, 40, 35, 70),     # Total: A=45, B=75, C=70
      duration = c(50, 75, 100, 150, 125, 250) # Total: A=225, B=275, C=250
    ),
    "2024-01-02" = tibble::tibble(
      date = rep(as.Date("2024-01-02"), 7),
      cow = c("A", "A", "B", "B", "B", "C", "C"), # A: 2 visits, B: 3 visits, C: 2 visits
      intake = c(12, 18, 30, 35, 40, 65, 75),     # Total: A=30, B=105, C=140
      duration = c(60, 80, 120, 130, 140, 220, 240) # Total: A=140, B=390, C=460
    ),
    "2024-01-03" = tibble::tibble(
      date = rep(as.Date("2024-01-03"), 6),
      cow = c("A", "B", "B", "C", "C", "C"),  # A: 1 visit, B: 2 visits, C: 3 visits
      intake = c(25, 45, 50, 60, 65, 70),     # Total: A=25, B=95, C=195
      duration = c(90, 160, 170, 200, 210, 220) # Total: A=90, B=330, C=630
    )
  )

  water_list <- list(
    "2024-01-01" = tibble::tibble(
      date = rep(as.Date("2024-01-01"), 6),
      cow = c("A", "A", "B", "B", "C", "C"),  # Each cow: 2 visits
      intake = c(20, 35, 80, 90, 150, 160),   # Total: A=55, B=170, C=310
      duration = c(30, 35, 70, 75, 100, 110)  # Total: A=65, B=145, C=210
    ),
    "2024-01-02" = tibble::tibble(
      date = rep(as.Date("2024-01-02"), 6),
      cow = c("A", "B", "B", "C", "C", "C"),  # A: 1 visit, B: 2 visits, C: 3 visits
      intake = c(30, 85, 95, 140, 150, 160),  # Total: A=30, B=180, C=450
      duration = c(40, 80, 85, 95, 100, 105)  # Total: A=40, B=165, C=300
    ),
    "2024-01-03" = tibble::tibble(
      date = rep(as.Date("2024-01-03"), 6),
      cow = c("A", "A", "A", "B", "C", "C"),  # A: 3 visits, B: 1 visit, C: 2 visits
      intake = c(15, 20, 25, 100, 155, 165),  # Total: A=60, B=100, C=320
      duration = c(25, 30, 35, 90, 105, 115)  # Total: A=90, B=90, C=220
    )
  )

  warn <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    low_daily_feed_intake_cows = NA_character_,
    high_daily_feed_intake_cows = NA_character_,
    low_daily_water_intake_cows = NA_character_,
    high_daily_water_intake_cows = NA_character_
  )

  # Test with lists
  result <- feed_water_summary(
    feed_list, 
    water_list, 
    warn,
    cfg = qc_config(
      low_feed_intake = 50,   # Will flag some of A's days
      high_feed_intake = 100, # Will flag some of B and C's days
      low_wat_intake = 40,    # Will flag some of A's days
      high_wat_intake = 300   # Will flag some of C's days
    ),
    id_col = "cow",
    intake_col = "intake",
    dur_col = "duration"
  )

  # Test dimensions
  expect_equal(nrow(result$summary), 9)  # 3 cows * 3 days
  expect_equal(ncol(result$summary), 8)  # date, cow, feed_intake, feed_duration, feed_visits, water_intake, water_duration, water_visits

  # Test visit counts and sums for cow A on day 1
  day1_cow_a <- result$summary[result$summary$date == as.Date("2024-01-01") & result$summary$cow == "A", ]
  expect_equal(day1_cow_a$feed_visits, 3)  # Cow A had 3 feed visits on day 1
  expect_equal(day1_cow_a$water_visits, 2) # Cow A had 2 water visits on day 1
  expect_equal(day1_cow_a$feed_intake, 45)  # 10 + 15 + 20
  expect_equal(day1_cow_a$water_intake, 55) # 20 + 35
  expect_equal(day1_cow_a$feed_duration, 225)  # 50 + 75 + 100
  expect_equal(day1_cow_a$water_duration, 65)  # 30 + 35

  # Test visit counts and sums for cow B on day 2
  day2_cow_b <- result$summary[result$summary$date == as.Date("2024-01-02") & result$summary$cow == "B", ]
  expect_equal(day2_cow_b$feed_visits, 3)  # Cow B had 3 feed visits on day 2
  expect_equal(day2_cow_b$water_visits, 2) # Cow B had 2 water visits on day 2
  expect_equal(day2_cow_b$feed_intake, 105)  # 30 + 35 + 40
  expect_equal(day2_cow_b$water_intake, 180) # 85 + 95
  expect_equal(day2_cow_b$feed_duration, 390)  # 120 + 130 + 140
  expect_equal(day2_cow_b$water_duration, 165) # 80 + 85

  # Test visit counts and sums for cow C on day 3
  day3_cow_c <- result$summary[result$summary$date == as.Date("2024-01-03") & result$summary$cow == "C", ]
  expect_equal(day3_cow_c$feed_visits, 3)  # Cow C had 3 feed visits on day 3
  expect_equal(day3_cow_c$water_visits, 2) # Cow C had 2 water visits on day 3
  expect_equal(day3_cow_c$feed_intake, 195)  # 60 + 65 + 70
  expect_equal(day3_cow_c$water_intake, 320) # 155 + 165
  expect_equal(day3_cow_c$feed_duration, 630)  # 200 + 210 + 220
  expect_equal(day3_cow_c$water_duration, 220) # 105 + 115

  # Test warnings
  # Day 1
  expect_true(grepl("A, 45", result$warn$low_daily_feed_intake_cows[1])) # A's feed intake < 50
  expect_true(grepl("C, 310", result$warn$high_daily_water_intake_cows[1])) # C's water intake > 300

  # Day 2
  expect_true(grepl("A, 30", result$warn$low_daily_feed_intake_cows[2])) # A's feed intake < 50
  expect_true(grepl("B, 105; C, 140", result$warn$high_daily_feed_intake_cows[2])) # Both B and C > 100
  expect_true(grepl("C, 450", result$warn$high_daily_water_intake_cows[2])) # C's water intake > 300

  # Day 3
  expect_true(grepl("A, 25", result$warn$low_daily_feed_intake_cows[3])) # A's feed intake < 50
  expect_true(grepl("C, 195", result$warn$high_daily_feed_intake_cows[3])) # C > 100
  expect_true(grepl("C, 320", result$warn$high_daily_water_intake_cows[3])) # C's water intake > 300
})