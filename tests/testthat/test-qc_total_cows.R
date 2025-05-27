# ----------------------------------------------------------------------------- #
# Synthetic data helpers                                                        #
# ----------------------------------------------------------------------------- #
make_warn_df <- function(dates) {
  data.frame(
    date         = dates,
    total_cows   = as.integer(NA),   # character on entry (matches real skeleton)
    missing_cow  = "No",
    stringsAsFactors = FALSE
  )
}

make_comb_list <- function(lst) {
  # `lst` is a named list of integer vectors (cow IDs) keyed by date
  lapply(lst, function(ids) {
    df <- data.frame(
      ids,
      stringsAsFactors = FALSE
    )
    colnames(df) <- id_col2()
    df
  })
}

# ----------------------------------------------------------------------------- #
# Happy path                                                                    #
# ----------------------------------------------------------------------------- #
test_that("qc_total_cows() populates counts & respects expected value", {
  comb <- make_comb_list(list(
    "2025-05-01" = c(1, 2, 2),   # 2 unique cows
    "2025-05-02" = c(1, 3, 2)       # 2 unique cows
  ))
  warn <- make_warn_df(c("2025-05-01", "2025-05-02"))

  cfg  <- qc_config(total_cows_expected = 3L)    # expect 3 cows/day

  out  <- qc_total_cows(comb, warn, cfg)

  expect_equal(out$total_cows, c(2L, 3L))        # counts recorded, coerced to int
  expect_equal(out$missing_cow, c("Yes", "No")) # both below expectation
})

# ----------------------------------------------------------------------------- #
# Edge cases                                                                    #
# ----------------------------------------------------------------------------- #
test_that("dates present in warn but absent/empty in comb stay unchanged", {
  comb <- make_comb_list(list(
    "2025-05-01" = integer(0)     # empty day
  ))
  warn <- make_warn_df(c("2025-05-01", "2025-05-03"))   # 05-03 not in comb

  cfg  <- qc_config(total_cows_expected = NA)    # no expectation

  out  <- qc_total_cows(comb, warn, cfg)

  # Day with empty data → total_cows remains NA
  expect_true(is.na(out$total_cows[out$date == "2025-05-01"]))
  # Day absent from comb → untouched
  expect_true(is.na(out$total_cows[out$date == "2025-05-03"]))
  # No rows flagged because expectation is NA
  expect_equal(unique(out$missing_cow), "No")
})

test_that("no missing_cow flag when observed ≥ expected", {
  comb <- make_comb_list(list("2025-05-04" = c(1, 2, 3, 4)))
  warn <- make_warn_df("2025-05-04")

  cfg  <- qc_config(total_cows_expected = 4L)

  out  <- qc_total_cows(comb, warn, cfg)

  expect_equal(out$total_cows, 4L)
  expect_equal(out$missing_cow, "No")
})

test_that("multiple days with mixed outcomes handled correctly", {
  comb <- make_comb_list(list(
    "2025-05-05" = c(1, 1, 2),   # 2 cows (below expected)
    "2025-05-06" = c(1, 2, 3),   # 3 cows (meets expected)
    "2025-05-07" = c(1, 4, 5, 6) # 4 cows (above expected)
  ))
  warn <- make_warn_df(c("2025-05-05", "2025-05-06", "2025-05-07"))

  cfg <- qc_config(total_cows_expected = 3L)

  out <- qc_total_cows(comb, warn, cfg)

  expect_equal(out$total_cows, c(2L, 3L, 4L))
  expect_equal(out$missing_cow, c("Yes", "No", "No"))
})
