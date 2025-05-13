# -----------------------------------------------------------------------------#
# --------------------- Tests for merge_feed_water_summary -------------------#
# -----------------------------------------------------------------------------#

test_that("merge_feed_water_summary works with feed and drink data", {
  dir.create("data/results", recursive = TRUE, showWarnings = FALSE)

  master_f <- data.frame(
    Intake = c(10, 20, 5),  # C1 total = 15
    Duration = c(100, 200, 50),
    date = as.Date(rep("2025-01-01", 3)),
    Cow = c("C1", "C2", "C1")
  )

  master_d <- data.frame(
    Intake = c(5, 10, 3),  # C1 total = 8
    Duration = c(50, 80, 20),
    date = as.Date(rep("2025-01-01", 3)),
    Cow = c("C1", "C2", "C1")
  )

  Insentec_warning <- data.frame(
    date = as.Date("2025-01-01"),
    low_daily_feed_intake_cows = "",
    high_daily_feed_intake_cows = "",
    low_daily_water_intake_cows = "",
    high_daily_water_intake_cows = ""
  )

  result <- merge_feed_water_summary(
    master_f = master_f,
    master_d = master_d,
    Insentec_warning = Insentec_warning,
    feed_intake_low_bar = 16,
    feed_intake_high_bar = 25,
    water_intake_low_bar = 9,
    water_intake_high_bar = 15
  )

  expect_true("Insentec_final_summary" %in% names(result))
  expect_true(is.data.frame(result$Insentec_final_summary))
  expect_s3_class(result$Insentec_warning, "data.frame")

  expect_match(result$Insentec_warning$low_daily_feed_intake_cows[1], "Cow  C1 ,  15 kg", fixed = TRUE)
  expect_match(result$Insentec_warning$low_daily_water_intake_cows[1], "Cow  C1 ,  8 kg", fixed = TRUE)

  expected_cols <- c(
    "Feeding_Intake(kg)", "Feeding_Duration(s)", "Feeding_Visits",
    "Drinking_Intake(kg)", "Drinking_Duration(s)", "Drinking_Visits"
  )
  actual_cols <- colnames(result$Insentec_final_summary)
  missing_cols <- setdiff(expected_cols, actual_cols)
  expect_equal(length(missing_cols), 0, info = paste("Missing:", paste(missing_cols, collapse = ", ")))
})
