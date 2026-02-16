# ----------------------------------------------------------------------------- #
# Synthetic data helpers                                                        #
# ----------------------------------------------------------------------------- #

make_meal_visits <- function(cow_ids, meal_ids, intake_vals, start_weight_vals) {
  n <- length(cow_ids)
  df <- data.frame(
    date = if (n == 0) character(0) else rep("2025-01-15", n),
    cow = cow_ids,
    meal_id = meal_ids,
    meal_start = if (n == 0) structure(numeric(0), class = c("POSIXct", "POSIXt"), tzone = "America/Vancouver") else rep(as.POSIXct("2025-01-15 08:00:00", tz = "America/Vancouver"), n),
    intake = intake_vals,
    start_weight = start_weight_vals,
    stringsAsFactors = FALSE
  )
  colnames(df) <- c("date", id_col2(), "meal_id", "meal_start",
                    intake_col2(), start_weight_col2())
  df
}

# ----------------------------------------------------------------------------- #
# Happy path                                                                    #
# ----------------------------------------------------------------------------- #

test_that("meal_non_nutritive_summary correctly classifies visit types", {
  # Animal 1: 2 meals, first has 1 non-nutritive, second has 1 empty bin
  visits <- make_meal_visits(
    cow_ids = c(1, 1, 1, 1, 1),
    meal_ids = c(1, 1, 1, 2, 2),
    intake_vals = c(5, 0.05, 5, 0.05, 5),  # calibration_error = 0.1
    start_weight_vals = c(50, 40, 45, 0.05, 50)  # Second visit non-nutritive, fourth empty
  )

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(nrow(result), 1)
  expect_equal(result[[id_col2()]][1], 1)
  expect_equal(result$total_non_nutritive_visits[1], 1)
  expect_equal(result$total_empty_bin_visits[1], 1)
  expect_equal(result$total_meals[1], 2)
})

test_that("meal_non_nutritive_summary calculates correct statistics", {
  # Animal 1: Meal 1 has 2 non-nutritive, Meal 2 has 1 non-nutritive
  visits <- make_meal_visits(
    cow_ids = c(1, 1, 1, 1, 1, 1),
    meal_ids = c(1, 1, 1, 2, 2, 2),
    intake_vals = c(5, 0.05, 0.05, 5, 0.05, 5),
    start_weight_vals = c(50, 40, 45, 50, 40, 45)  # All have feed available
  )

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(result$mean_non_nutritive_per_meal[1], 1.5)  # (2 + 1) / 2
  expect_equal(result$median_non_nutritive_per_meal[1], 1.5)
  expect_equal(result$total_non_nutritive_visits[1], 3)
})

test_that("meal_non_nutritive_summary works with list input", {
  day1 <- make_meal_visits(
    cow_ids = c(1, 1),
    meal_ids = c(1, 1),
    intake_vals = c(5, 0.05),
    start_weight_vals = c(50, 40)
  )

  data_list <- list("2025-01-15" = day1)

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(data_list, cfg)

  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 1)
})

test_that("meal_non_nutritive_summary handles multiple animals", {
  visits <- make_meal_visits(
    cow_ids = c(1, 1, 2, 2),
    meal_ids = c(1, 1, 1, 1),
    intake_vals = c(5, 0.05, 0.05, 5),
    start_weight_vals = c(50, 40, 0.05, 50)
  )

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(nrow(result), 2)
  expect_equal(result$total_non_nutritive_visits[result[[id_col2()]] == 1], 1)
  expect_equal(result$total_empty_bin_visits[result[[id_col2()]] == 2], 1)
})

# ----------------------------------------------------------------------------- #
# Edge cases                                                                    #
# ----------------------------------------------------------------------------- #

test_that("outlier visits (meal_id == 0) are excluded", {
  visits <- make_meal_visits(
    cow_ids = c(1, 1, 1),
    meal_ids = c(0, 1, 1),  # First visit is outlier
    intake_vals = c(0.05, 5, 0.05),
    start_weight_vals = c(40, 50, 40)
  )

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(result$total_non_nutritive_visits[1], 1)  # Only counts meal_id > 0
})

test_that("empty dataframe returns empty result with correct structure", {
  visits <- make_meal_visits(
    cow_ids = integer(0),
    meal_ids = integer(0),
    intake_vals = numeric(0),
    start_weight_vals = numeric(0)
  )

  cfg <- qc_config()

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(nrow(result), 0)
  expect_true(all(c("date", id_col2(), "mean_non_nutritive_per_meal",
                    "total_meals") %in% names(result)))
})

test_that("all outlier visits (meal_id == 0) returns empty result", {
  visits <- make_meal_visits(
    cow_ids = c(1, 1, 1),
    meal_ids = c(0, 0, 0),  # All outliers
    intake_vals = c(5, 5, 5),
    start_weight_vals = c(50, 50, 50)
  )

  cfg <- qc_config()

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(nrow(result), 0)
})

test_that("all nutritive visits results in zero non-nutritive counts", {
  visits <- make_meal_visits(
    cow_ids = c(1, 1, 1),
    meal_ids = c(1, 1, 1),
    intake_vals = c(5, 5, 5),  # All have intake > calibration_error
    start_weight_vals = c(50, 50, 50)
  )

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(result$total_non_nutritive_visits[1], 0)
  expect_equal(result$total_empty_bin_visits[1], 0)
  expect_equal(result$mean_non_nutritive_per_meal[1], 0)
})

test_that("single meal per animal returns NA for SD", {
  visits <- make_meal_visits(
    cow_ids = c(1, 1),
    meal_ids = c(1, 1),  # Only one meal
    intake_vals = c(5, 0.05),
    start_weight_vals = c(50, 40)
  )

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_true(is.na(result$sd_non_nutritive_per_meal[1]))
  expect_true(is.na(result$sd_empty_bin_per_meal[1]))
})

test_that("exactly at calibration threshold is classified as non-nutritive/empty", {
  visits <- make_meal_visits(
    cow_ids = c(1, 1),
    meal_ids = c(1, 1),
    intake_vals = c(0.1, 0.1),  # Exactly at threshold
    start_weight_vals = c(50, 0.1)  # First non-nutritive, second empty
  )

  cfg <- qc_config(calibration_error = 0.1)

  result <- meal_non_nutritive_summary(visits, cfg)

  expect_equal(result$total_non_nutritive_visits[1], 1)
  expect_equal(result$total_empty_bin_visits[1], 1)
})

# ----------------------------------------------------------------------------- #
# Input validation                                                              #
# ----------------------------------------------------------------------------- #

test_that("missing meal columns throws error", {
  incomplete_visits <- data.frame(
    cow = 1,
    intake = 5,
    start_weight = 50
  )
  colnames(incomplete_visits)[1] <- id_col2()

  cfg <- qc_config()

  expect_error(
    meal_non_nutritive_summary(incomplete_visits, cfg),
    "Data must have meal labels"
  )
})

test_that("missing required columns throws error", {
  incomplete_visits <- data.frame(
    cow = 1,
    meal_id = 1,
    meal_start = as.POSIXct("2025-01-15 08:00:00", tz = "America/Vancouver")
  )
  colnames(incomplete_visits)[1] <- id_col2()

  cfg <- qc_config()

  expect_error(
    meal_non_nutritive_summary(incomplete_visits, cfg),
    "Missing required columns"
  )
})

test_that("NULL data throws error", {
  cfg <- qc_config()

  expect_error(
    meal_non_nutritive_summary(NULL, cfg),
    "data cannot be NULL"
  )
})

test_that("data without meal_label_visits throws informative error", {
  visits <- data.frame(
    cow = 1,
    intake = 5,
    start_weight = 50
  )
  colnames(visits)[1] <- id_col2()

  cfg <- qc_config()

  expect_error(
    meal_non_nutritive_summary(visits, cfg),
    "meal_label_visits"
  )
})
