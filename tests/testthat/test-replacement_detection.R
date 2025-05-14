# -----------------------------------------------------------------------------#
#                         Setup: Common test variables                          #
# -----------------------------------------------------------------------------#

single_day <- all_fed[[1]]
replacements <- record_replacement_day(single_day)
empty_replacements <- data.frame(
  Reactor_cow = character(),
  Bin = integer(),
  Time = as.POSIXct(character()),
  date = as.Date(character()),
  Actor_cow = character(),
  Bout_interval = lubridate::as.duration(numeric())
)

# -----------------------------------------------------------------------------#
#                  Tests for record_replacement_days()                         #
# -----------------------------------------------------------------------------#

test_that("Normal case: detects replacements correctly across multiple days", {
  result <- record_replacement_days(all_fed)

  expect_type(result, "list")
  expect_length(result, length(all_fed))
  expect_true(all(sapply(result, is.data.frame)))
})

test_that("Edge case: handles empty input gracefully", {
  empty_list <- list()
  result <- record_replacement_days(empty_list)

  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("Error handling: data_list is not a list", {
  expect_error(
    record_replacement_days("not_a_list"),
    "data_list.*list"
  )
})

# ---------------------- Tests for check_alibi_days() ---------------------- #

test_that("Normal case: validates replacements across multiple days", {
  replacements <- record_replacement_days(all_fed)
  valid_replacements <- check_alibi_days(replacements, all_fed)

  expect_type(valid_replacements, "list")
  expect_length(valid_replacements, length(all_fed))
  expect_true(all(sapply(valid_replacements, is.data.frame)))
})

test_that("Edge case: empty replacements return empty list", {
  empty_replacements <- list()
  valid_replacements <- check_alibi_days(empty_replacements, all_fed)

  expect_type(valid_replacements, "list")
  expect_length(valid_replacements, 0)
})

test_that("Error handling: mismatched lists cause error", {
  replacements <- record_replacement_days(all_fed)
  mismatched_data <- all_fed[1] # length mismatch

  expect_error(
    check_alibi_days(replacements, mismatched_data),
    "must be the same length"
  )
})

# ---------------------- Tests for record_replacement_day() (internal) ---------------------- #

test_that("Internal helper correctly identifies replacements on one day", {
  single_day <- all_fed[[1]]
  result <- record_replacement_day(single_day)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("Reactor_cow", "Bin", "Time", "date", "Actor_cow", "Bout_interval"))
})

test_that("Edge case: no replacements found on a quiet day", {
  single_day_empty <- single_day[0, ] # Empty data frame
  result <- record_replacement_day(single_day_empty)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("Error handling: incorrect data format for single day detection", {
  incorrect_data <- data.frame(wrong_col = 1:3)
  expect_error(
    record_replacement_day(data.frame(wrong_col = 1:3)),
    "must include id.*columns"
  )
})

# ---------------------- Tests for check_alibi_day() (internal) ---------------------- #

test_that("Internal helper correctly filters valid replacements", {
  single_day <- all_fed[[1]]
  replacements <- record_replacement_day(single_day)
  valid_replacements <- check_alibi_day(replacements, single_day)

  expect_s3_class(valid_replacements, "data.frame")
  expect_named(valid_replacements, c("Reactor_cow", "Bin", "Time", "date", "Actor_cow", "Bout_interval"))
})

test_that("Edge case: no replacements provided returns empty frame", {
  empty_replacements <- replacements[0, ]
  result <- check_alibi_day(empty_replacements, single_day)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("Error handling: incorrect input structure triggers error", {
  incorrect_replacements <- data.frame(wrong_col = 1:2)
  expect_error(
    check_alibi_day(incorrect_replacements, single_day),
    "must include Actor_cow"
  )
})
