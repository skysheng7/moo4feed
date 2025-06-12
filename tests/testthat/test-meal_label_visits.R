test_that("meal_label_visits works for a single dataframe (normal case)", {
  df <- data.frame(
    animal = c(1, 1, 1, 2, 2),
    start = as.POSIXct(c('2024-01-01 08:00', '2024-01-01 08:10', '2024-01-01 09:00', '2024-01-01 08:30', '2024-01-01 09:30')),
    end = as.POSIXct(c('2024-01-01 08:05', '2024-01-01 08:15', '2024-01-01 09:05', '2024-01-01 08:35', '2024-01-01 09:35')),
    bin = c(1, 1, 2, 1, 2),
    intake = c(2, 3, 1, 2, 3),
    duration = c(300, 300, 300, 300, 300)
  )
  out <- meal_label_visits(df, id_col = 'animal', start_col = 'start', end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration')
  expect_true(all(c('meal_id', 'meal_start', 'meal_end', 'meal_duration', 'total_intake', 'visit_count') %in% names(out)))
  expect_equal(nrow(out), nrow(df))
  expect_type(out$meal_id, 'integer')
})

test_that("meal_label_visits works for a list of dataframes", {
  df1 <- data.frame(
    animal = 1,
    start = as.POSIXct('2024-01-01 08:00'),
    end = as.POSIXct('2024-01-01 08:05'),
    bin = 1,
    intake = 2,
    duration = 300
  )
  df2 <- data.frame(
    animal = 2,
    start = as.POSIXct('2024-01-01 09:00'),
    end = as.POSIXct('2024-01-01 09:05'),
    bin = 2,
    intake = 3,
    duration = 300
  )
  out <- meal_label_visits(list(df1, df2), id_col = 'animal', start_col = 'start', end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration')
  expect_true(is.list(out))
  expect_equal(length(out), 2)
  expect_true(all(c('meal_id', 'meal_start', 'meal_end', 'meal_duration', 'total_intake', 'visit_count') %in% names(out[[1]])))
})

test_that("meal_label_visits handles empty dataframe", {
  df <- data.frame(animal = integer(0), start = as.POSIXct(character(0)), end = as.POSIXct(character(0)), bin = integer(0), intake = numeric(0), duration = numeric(0))
  out <- meal_label_visits(df, id_col = 'animal', start_col = 'start', end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration')
  expect_equal(nrow(out), 0)
  expect_true(all(c('meal_id', 'meal_start', 'meal_end', 'meal_duration', 'total_intake', 'visit_count') %in% names(out)))
})

test_that("meal_label_visits errors on missing required columns", {
  df <- data.frame(animal = 1, start = as.POSIXct('2024-01-01 08:00'))
  expect_error(meal_label_visits(df, id_col = 'animal', start_col = 'start', end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration'))
})

test_that("meal_label_visits assigns meal_id 0 for all outliers (no meals)", {
  df <- data.frame(
    animal = 1,
    start = as.POSIXct('2024-01-01 08:00'),
    end = as.POSIXct('2024-01-01 08:05'),
    bin = 1,
    intake = 2,
    duration = 300
  )
  # Use min_pts > nrow(df) to force all outliers
  out <- meal_label_visits(df, min_pts = 10, id_col = 'animal', start_col = 'start', end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration')
  expect_true(all(out$meal_id == 0))
})

test_that("meal_label_visits works with custom column names", {
  df <- data.frame(
    id = c(1, 1),
    s = as.POSIXct(c('2024-01-01 08:00', '2024-01-01 08:10')),
    e = as.POSIXct(c('2024-01-01 08:05', '2024-01-01 08:15')),
    b = c(1, 1),
    i = c(2, 3),
    d = c(300, 300)
  )
  out <- meal_label_visits(df, id_col = 'id', start_col = 's', end_col = 'e', bin_col = 'b', intake_col = 'i', dur_col = 'd')
  expect_equal(nrow(out), 2)
  expect_true(all(c('meal_id', 'meal_start', 'meal_end', 'meal_duration', 'total_intake', 'visit_count') %in% names(out)))
})

test_that("meal_label_visits works with different timezones", {
  df <- data.frame(
    animal = 1,
    start = as.POSIXct('2024-01-01 08:00', tz = 'America/New_York'),
    end = as.POSIXct('2024-01-01 08:05', tz = 'America/New_York'),
    bin = 1,
    intake = 2,
    duration = 300
  )
  out <- meal_label_visits(df, id_col = 'animal', start_col = 'start', end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration', tz = 'America/New_York')
  expect_true(all(attr(out$meal_start, 'tzone') == 'America/New_York'))
})

test_that("meal_label_visits returns correct structure for minimal input", {
  df <- data.frame(
    animal = 1,
    start = as.POSIXct('2024-01-01 08:00'),
    end = as.POSIXct('2024-01-01 08:05'),
    bin = 1,
    intake = 2,
    duration = 300
  )
  out <- meal_label_visits(df, id_col = 'animal', start_col = 'start', end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration')
  expect_equal(nrow(out), 1)
  expect_true(all(c('meal_id', 'meal_start', 'meal_end', 'meal_duration', 'total_intake', 'visit_count') %in% names(out)))
}) 