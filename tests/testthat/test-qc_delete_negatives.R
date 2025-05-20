# Test suite for qc_delete_negatives()

test_that("qc_delete_negatives() removes records with negative duration or intake", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3, 4),
      duration = c(-5, 10, 15, 20),
      intake = c(1, -2, 3, 4),
      start_weight = c(10, 10, 10, 10),
      end_weight = c(9, 12, 7, 6)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  # Should only keep bin 4 (positive duration and intake)
  expect_equal(nrow(result[["2025-05-01"]]), 1)
  expect_equal(result[["2025-05-01"]]$bin, 4)
})

test_that("qc_delete_negatives() handles negative weights correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3),
      duration = c(10, 10, 10),
      intake = c(1, 2, 3),
      start_weight = c(-5, 10, 10),
      end_weight = c(4, -2, 7)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  # Check that negative weights are set to zero
  expect_equal(result[["2025-05-01"]]$start_weight[1], 0)
  expect_equal(result[["2025-05-01"]]$end_weight[2], 0)
  
  # Check that intake is recalculated correctly
  expect_equal(result[["2025-05-01"]]$intake[1], 0)  # 0 - 4 = -4, but negative intake is removed
  expect_equal(result[["2025-05-01"]]$intake[2], 10) # 10 - 0 = 10
  expect_equal(result[["2025-05-01"]]$intake[3], 3)  # 10 - 7 = 3
})

test_that("qc_delete_negatives() calculates rates correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3),
      duration = c(10, 0, 5),
      intake = c(5, 3, 10),
      start_weight = c(10, 10, 10),
      end_weight = c(5, 7, 0)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  # Check rate calculations
  expect_equal(result[["2025-05-01"]]$rate[1], 0.5)  # 5/10
  expect_equal(result[["2025-05-01"]]$rate[2], 0)    # infinite rate set to 0
  expect_equal(result[["2025-05-01"]]$rate[3], 2)    # 10/5
})

test_that("qc_delete_negatives() handles empty data correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = integer(0),
      duration = numeric(0),
      intake = numeric(0),
      start_weight = numeric(0),
      end_weight = numeric(0)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  expect_equal(nrow(result[["2025-05-01"]]), 0)
})

test_that("qc_delete_negatives() handles multiple days correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2),
      duration = c(-5, 10),
      intake = c(1, -2),
      start_weight = c(10, 10),
      end_weight = c(9, 12)
    ),
    "2025-05-02" = data.frame(
      bin = c(3, 4),
      duration = c(15, 20),
      intake = c(3, 4),
      start_weight = c(10, 10),
      end_weight = c(7, 6)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  # First day should be empty (all records have negative values)
  expect_equal(nrow(result[["2025-05-01"]]), 0)
  
  # Second day should keep both records
  expect_equal(nrow(result[["2025-05-02"]]), 2)
  expect_equal(result[["2025-05-02"]]$bin, c(3, 4))
})

test_that("qc_delete_negatives() handles all negative values correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2, 3),
      duration = c(-5, -10, -15),
      intake = c(-1, -2, -3),
      start_weight = c(-10, -10, -10),
      end_weight = c(-9, -8, -7)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  # All records should be removed
  expect_equal(nrow(result[["2025-05-01"]]), 0)
})

test_that("qc_delete_negatives() handles zero duration correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1, 2),
      duration = c(0, 10),
      intake = c(5, 5),
      start_weight = c(10, 10),
      end_weight = c(5, 5)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  # Both records should be kept, but first record should have rate = 0
  expect_equal(nrow(result[["2025-05-01"]]), 2)
  expect_equal(result[["2025-05-01"]]$rate[1], 0)
  expect_equal(result[["2025-05-01"]]$rate[2], 0.5)
})

test_that("qc_delete_negatives() handles missing days correctly", {
  comb <- list(
    "2025-05-01" = data.frame(
      bin = c(1),
      duration = c(10),
      intake = c(5),
      start_weight = c(10),
      end_weight = c(5)
    )
  )

  result <- qc_delete_negatives(
    comb,
    dur_col = "duration",
    intake_col = "intake",
    start_weight_col = "start_weight",
    end_weight_col = "end_weight"
  )

  # Should process the single day correctly
  expect_equal(nrow(result[["2025-05-01"]]), 1)
  expect_equal(result[["2025-05-01"]]$rate[1], 0.5)
}) 