

# ----------------------------------------------------------------------------- #
# Helpers                                                                       #
# ----------------------------------------------------------------------------- #

tz_local <- tz2()   # package helper returns e.g. "UTC" or farm TZ

make_day_full <- function(ids, bins, starts_chr, ends_chr) {
  stopifnot(length(ids) == length(bins),
            length(ids) == length(starts_chr),
            length(ids) == length(ends_chr))

  start_time <- lubridate::ymd_hms(starts_chr, tz = tz_local)
  end_time   <- lubridate::ymd_hms(ends_chr,   tz = tz_local)

  df <- data.frame(
    transponder = ids + 1000L,          # dummy but unique
    cow         = ids,                      # will be renamed below
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

make_warn_df <- function(dates) {
  data.frame(
    date                  = dates,
    double_detection_bins = NA_character_,
    stringsAsFactors      = FALSE
  )
}

# ----------------------------------------------------------------------------- #
# qc_detect_all_double_detections()                                             #
# ----------------------------------------------------------------------------- #
test_that("qc_detect_all_double_detections() flags correct bins", {
  day <- make_day_full(
    ids   = c(1, 1, 2, 3, 4),
    bins  = c(10, 11, 10, 12, 12),
    starts_chr = c(
      "2025-05-01 11:00:00",
      "2025-05-01 11:03:00", # overlaps with first visit (same cow)
      "2025-05-01 11:06:00",
      "2025-05-01 11:00:00",
      "2025-05-01 11:05:00"  # overlaps at same bin 12 (diff cows)
    ),
    ends_chr = c(
      "2025-05-01 11:05:00",
      "2025-05-01 11:10:00",
      "2025-05-01 11:08:00",
      "2025-05-01 11:06:00",
      "2025-05-01 11:07:00"
    )
  )

  problematic <- qc_detect_all_double_detections(day, verbose = FALSE)
  expect_setequal(problematic, c(10, 12))
})

test_that("qc_detect_all_double_detections() returns empty vector when no overlaps", {
  day <- make_day_full(
    ids   = c(1, 1, 2),
    bins  = c(10, 10, 11),
    starts_chr = c(
      "2025-05-02 08:00:00",
      "2025-05-02 08:10:00",  # back-to-back, no overlap
      "2025-05-02 08:00:00"
    ),
    ends_chr = c(
      "2025-05-02 08:05:00",
      "2025-05-02 08:15:00",
      "2025-05-02 08:05:00"
    )
  )

  problematic <- qc_detect_all_double_detections(day, verbose = FALSE)
  expect_length(problematic, 0)
})

# ----------------------------------------------------------------------------- #
# qc_double_detection()                                                         #
# ----------------------------------------------------------------------------- #
test_that("qc_double_detection() populates warning frame correctly", {
  comb <- list(
    "2025-05-01" = make_day_full(
      ids   = c(1, 1, 2, 3, 4),
      bins  = c(10, 11, 10, 12, 12),
      starts_chr = c(
        "2025-05-01 11:00:00",
        "2025-05-01 11:03:00",
        "2025-05-01 11:06:00",
        "2025-05-01 11:00:00",
        "2025-05-01 11:05:00"
      ),
      ends_chr = c(
        "2025-05-01 11:05:00",
        "2025-05-01 11:10:00",
        "2025-05-01 11:08:00",
        "2025-05-01 11:06:00",
        "2025-05-01 11:07:00"
      )
    ),
    "2025-05-02" = make_day_full(
      ids   = c(5, 6),
      bins  = c(20, 21),
      starts_chr = c(
        "2025-05-02 09:00:00",
        "2025-05-02 09:10:00"
      ),
      ends_chr = c(
        "2025-05-02 09:05:00",
        "2025-05-02 09:15:00"
      )
    )
  )

  warn <- make_warn_df(c("2025-05-01", "2025-05-02"))
  result <- qc_double_detection(comb, warn, verbose = FALSE)

  expect_equal(result$double_detection_bins[result$date == "2025-05-01"], "10; 12")
  expect_true(is.na(result$double_detection_bins[result$date == "2025-05-02"]))
})

test_that("qc_double_detection() leaves rows untouched when comb is empty", {
  warn <- make_warn_df("2025-05-03")
  result <- qc_double_detection(list(), warn, verbose = FALSE)
  expect_true(is.na(result$double_detection_bins))
})
