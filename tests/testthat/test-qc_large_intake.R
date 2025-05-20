# Test suite for qc_large_intake()

test_that("qc_large_intake() identifies large intake values correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3),
      intake = c(10, 5, 3),
      duration = c(100, 100, 100)
    ),
    "2025-05-02" = data.frame(
      bin = c(4),
      intake = c(2),
      duration = c(100)
    )
  )

  warn <- data.frame(
    date = c("2025-05-01", "2025-05-02"),
    large_intake_feed_visit = NA_character_
  )
  cfg <- list(large_intake_visit_feed = 8)

  result <- qc_large_intake(
    comb, warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration",
    type = "feed"
  )
  expect_equal(result$large_intake_feed_visit[result$date == "2025-05-01"], "1")
  expect_true(is.na(result$large_intake_feed_visit[result$date == "2025-05-02"]))
})

test_that("qc_large_intake() identifies rapid intake rates correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3, 4),
      intake = c(10, 10, 5, 5),
      duration = c(100, 1000, 100, 1000)
    ),
    "2025-05-02" = data.frame(
      bin = c(5),
      intake = c(5),
      duration = c(1000)
    )
  )

  warn <- data.frame(
    date = c("2025-05-01", "2025-05-02"),
    large_intake_feed_visit = NA_character_
  )
  cfg <- list(
    large_intake_visit_feed = 8,
    large_intake_rate_feed = 0.01
  )

  result <- qc_large_intake(
    comb, warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration",
    type = "feed"
  )
  # Bin 1: large intake (10 > 8) and rapid rate (0.1 > 0.01)
  # Bin 2: large intake (10 > 8) but not rapid rate (0.01 = 0.01)
  # Bin 3: rapid rate (0.05 > 0.01) but not large intake (5 < 8)
  # Bin 4: neither condition met
  expect_equal(result$large_intake_feed_visit[result$date == "2025-05-01"], "1; 2")
  expect_true(is.na(result$large_intake_feed_visit[result$date == "2025-05-02"]))
})

test_that("qc_large_intake() combines large intake and rapid rate warnings correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3, 4),
      intake = c(10, 10, 5, 3),
      duration = c(100, 1000, 100, 100)
    ),
    "2025-05-02" = data.frame(
      bin = c(5),
      intake = c(2),
      duration = c(100)
    )
  )

  warn <- data.frame(
    date = c("2025-05-01", "2025-05-02"),
    large_intake_feed_visit = NA_character_
  )
  cfg <- list(
    large_intake_visit_feed = 8,
    large_intake_rate_feed = 0.01
  )

  result <- qc_large_intake(
    comb, warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration",
    type = "feed"
  )
  # Bin 1: large intake (10 > 8) and rapid rate (0.1 > 0.01)
  # Bin 2: large intake (10 > 8) but not rapid rate (0.01 = 0.01)
  # Bin 3: rapid rate (0.05 > 0.01) but not large intake (5 < 8)
  # Bin 4: neither condition met
  expect_equal(result$large_intake_feed_visit[result$date == "2025-05-01"], "1; 2")
  expect_true(is.na(result$large_intake_feed_visit[result$date == "2025-05-02"]))
})

test_that("qc_large_intake() handles water data correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3),
      intake = c(35, 35, 20),
      duration = c(100, 1000, 100)
    )
  )

  warn <- data.frame(
    date = "2025-05-01",
    large_intake_water_visit = NA_character_
  )
  cfg <- list(
    large_intake_visit_water = 30,
    large_intake_rate_water = 0.3
  )

  result <- qc_large_intake(
    comb, warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration",
    type = "water"
  )
  # Bin 1: large intake (35 > 30) and rapid rate (0.35 > 0.3)
  # Bin 2: large intake (35 > 30) but not rapid rate (0.035 < 0.3)
  # Bin 3: neither condition met
  expect_equal(result$large_intake_water_visit[result$date == "2025-05-01"], "1; 2")
})

test_that("qc_large_intake() handles empty data correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = integer(0),
      intake = numeric(0),
      duration = numeric(0)
    ),
    "2025-05-02" = data.frame(
      bin = c(1),
      intake = c(5),
      duration = c(100)
    )
  )

  warn <- data.frame(
    date = c("2025-05-01", "2025-05-02"),
    large_intake_feed_visit = NA_character_
  )
  cfg <- list(
    large_intake_visit_feed = 8,
    large_intake_rate_feed = 0.01
  )

  result <- qc_large_intake(
    comb, warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration",
    type = "feed"
  )
  expect_true(is.na(result$large_intake_feed_visit[result$date == "2025-05-01"]))
  expect_true(is.na(result$large_intake_feed_visit[result$date == "2025-05-02"]))
})

test_that("qc_large_intake() verbose mode outputs correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2),
      intake = c(10, 5),
      duration = c(100, 100)
    )
  )

  warn <- data.frame(
    date = "2025-05-01",
    large_intake_feed_visit = NA_character_
  )
  cfg <- list(
    large_intake_visit_feed = 8,
    large_intake_rate_feed = 0.01
  )

  expect_output(
    qc_large_intake(
      comb, warn, verbose = TRUE, cfg = cfg,
      bin_col = "bin", intake_col = "intake", dur_col = "duration",
      type = "feed"
    ),
    regexp = "LARGE INTAKE VALUES DETECTED|RAPID INTAKE RATES DETECTED"
  )
})

test_that("qc_all_large_intakes() handles both feed and water data correctly", {
  feed <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2),
      intake = c(10, 5),
      duration = c(100, 100)
    )
  )
  water <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2),
      intake = c(35, 20),
      duration = c(100, 100)
    )
  )

  warn <- data.frame(
    date = "2025-05-01",
    large_intake_feed_visit = NA_character_,
    large_intake_water_visit = NA_character_
  )
  cfg <- list(
    large_intake_visit_feed = 8,
    large_intake_rate_feed = 0.01,
    large_intake_visit_water = 30,
    large_intake_rate_water = 0.3
  )

  result <- qc_all_large_intakes(
    feed = feed, water = water, warn = warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration"
  )
  # Feed bin 1: large intake (10 > 8) and rapid rate (0.1 > 0.01)
  # Water bin 1: large intake (35 > 30) and rapid rate (0.35 > 0.3)
  expect_equal(result$large_intake_feed_visit[result$date == "2025-05-01"], "1")
  expect_equal(result$large_intake_water_visit[result$date == "2025-05-01"], "1")
})

test_that("qc_all_large_intakes() handles NULL inputs correctly", {
  feed <- list(
    "2025-05-01" = data.frame(
      bin = c(1),
      intake = c(10),
      duration = c(100)
    )
  )

  warn <- data.frame(
    date = "2025-05-01",
    large_intake_feed_visit = NA_character_,
    large_intake_water_visit = NA_character_
  )
  cfg <- list(
    large_intake_visit_feed = 8,
    large_intake_rate_feed = 0.01
  )

  # Test with NULL water
  result <- qc_all_large_intakes(
    feed = feed, water = NULL, warn = warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration"
  )
  expect_equal(result$large_intake_feed_visit[result$date == "2025-05-01"], "1")
  expect_true(is.na(result$large_intake_water_visit[result$date == "2025-05-01"]))

  # Test with NULL feed
  result <- qc_all_large_intakes(
    feed = NULL, water = feed, warn = warn, verbose = FALSE, cfg = cfg,
    bin_col = "bin", intake_col = "intake", dur_col = "duration"
  )
  expect_true(is.na(result$large_intake_feed_visit[result$date == "2025-05-01"]))
  expect_true(is.na(result$large_intake_water_visit[result$date == "2025-05-01"]))
}) 