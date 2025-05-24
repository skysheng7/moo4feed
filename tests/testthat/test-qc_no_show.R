# ----------------------------------------------------------------------------- #
# qc_no_show() – normal cases                                                    #
# ----------------------------------------------------------------------------- #

test_that("qc_no_show() correctly identifies cows that disappeared after noon", {
  # Create test data
  test_data <- list(
    "2024-01-01" = data.frame(
      cow = c(1, 1, 2, 2, 3),
      end = lubridate::ymd_hms(
        c(
          "2024-01-01 10:00:00",  # Cow 1 last seen before noon
          "2024-01-01 11:00:00",
          "2024-01-01 13:00:00",  # Cow 2 seen after noon
          "2024-01-01 15:00:00",
          "2024-01-01 19:00:00"   # Cow 3 seen after noon
        ),
        tz = "UTC"
      )
    )
  )
  
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    cows_disappeared_after_noon = NA_character_
  )
  
  # Test with verbose = FALSE to avoid console output during tests
  result <- qc_no_show(
    test_data,
    warn_df,
    id_col = "cow",
    end_col = "end",
    tz = "UTC",
    verbose = FALSE
  )
  
  # Check cow warnings
  expect_equal(result$cows_disappeared_after_noon[1], "1, 11:00:00")
})

# ----------------------------------------------------------------------------- #
# qc_no_show() – edge cases                                                     #
# ----------------------------------------------------------------------------- #

test_that("qc_no_show() handles empty data frames correctly", {
  empty_data <- list(
    "2024-01-01" = data.frame(
      cow = character(),
      end = lubridate::ymd_hms(character(), tz = "UTC")
    )
  )
  
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    cows_disappeared_after_noon = NA_character_
  )
  
  result <- qc_no_show(empty_data, warn_df, tz = "UTC", verbose = FALSE)
  expect_true(all(is.na(result[1, -1])))  # All columns except date should be NA
})

test_that("qc_no_show() handles single-record data frames", {
  single_record <- list(
    "2024-01-01" = data.frame(
      cow = 1,
      end = lubridate::ymd_hms("2024-01-01 10:00:00", tz = "UTC")
    )
  )
  
  warn_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    cows_disappeared_after_noon = NA_character_
  )
  
  result <- qc_no_show(single_record, warn_df, tz = "UTC", verbose = FALSE)
  expect_equal(result$cows_disappeared_after_noon[1], "1, 10:00:00")
})

# ----------------------------------------------------------------------------- #
# qc_no_show() – error handling                                                 #
# ----------------------------------------------------------------------------- #

test_that("qc_no_show() validates inputs correctly", {
  warn_df <- tibble::tibble(date = as.Date("2024-01-01"))
  
  # Test empty list
  expect_error(
    qc_no_show(list(), warn_df),
    "`comb` must be a non-empty list of data frames"
  )
  
  # Test list with non-data frame items
  invalid_list <- list(
    "2024-01-01" = data.frame(cow = 1),
    "2024-01-02" = "not a data frame"
  )
  expect_error(
    qc_no_show(invalid_list, warn_df),
    "All elements in `comb` list must be data frames"
  )
  
  # Test non-data frame warn
  expect_error(
    qc_no_show(list(data.frame()), "not_a_df"),
    "`warn` must be a data frame"
  )
})

# ----------------------------------------------------------------------------- #
# qc_determine_last_seen() tests                                                #
# ----------------------------------------------------------------------------- #

test_that("qc_determine_last_seen() finds correct last seen times", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    end = lubridate::ymd_hms(
      c(
        "2024-01-01 10:00:00",
        "2024-01-01 11:00:00",
        "2024-01-01 12:00:00",
        "2024-01-01 13:00:00"
      ),
      tz = "UTC"
    )
  )
  
  # Using unquoted symbols directly as per updated function
  id_sym <- rlang::sym("id")
  end_sym <- rlang::sym("end")
  result <- qc_determine_last_seen(df, id_sym, end_sym)
  
  expect_equal(nrow(result), 2)  # One row per unique ID
  expect_equal(
    result$end,
    lubridate::ymd_hms(c("2024-01-01 11:00:00", "2024-01-01 13:00:00"), tz = "UTC")
  )
})

# ----------------------------------------------------------------------------- #
# qc_extract_warnings() tests                                                   #
# ----------------------------------------------------------------------------- #

test_that("qc_extract_warnings() formats warnings correctly", {
  df <- data.frame(
    id = c(1, 2, 3),
    end = lubridate::ymd_hms(
      c(
        "2024-01-01 10:00:00",
        "2024-01-01 11:00:00",
        "2024-01-01 13:00:00"
      ),
      tz = "UTC"
    )
  )
  
  cutoff <- lubridate::ymd_hms("2024-01-01 12:00:00", tz = "UTC")
  
  # Using unquoted symbols directly as per updated function
  id_sym <- rlang::sym("id")
  end_sym <- rlang::sym("end")
  result <- qc_extract_warnings(df, id_sym, end_sym, cutoff)
  expect_equal(result, "1, 10:00:00; 2, 11:00:00")
  
  # Test with no warnings
  late_cutoff <- lubridate::ymd_hms("2024-01-01 09:00:00", tz = "UTC")
  result_no_warnings <- qc_extract_warnings(df, id_sym, end_sym, late_cutoff)
  expect_true(is.na(result_no_warnings))
}) 