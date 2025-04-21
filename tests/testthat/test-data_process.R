# ---------------------- test for file_name_processing() ----------------------#
test_that("Normal case: typical filenames with embedded dates", {
  input <- c(" my/path/feed/VR200715.DAT ", "my/path/feed/VR200716.DAT")
  expected_dates <- c("200715", "200716")
  result <- file_name_processing(input, col_name = "Feed_dir")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("Feed_dir", "date"))
  expect_equal(result$date, expected_dates)
  expect_equal(trimws(result$Feed_dir), trimws(input))
})

test_that("Edge case: file names with no digits return NA", {
  input <- c("feed/VRfile.DAT", "   no_digits_here  ")
  result <- file_name_processing(input, col_name = "Feed_dir")

  expect_equal(result$date, c(NA_character_, NA_character_))
})

test_that("Edge case: empty input returns empty data frame", {
  input <- character(0)
  result <- file_name_processing(input, col_name = "Feed_dir")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Feed_dir", "date"))
})

test_that("Edge case: multiple digit blocks only picks the first", {
  input <- c("mypath/folder123/weird/VR20220101_extra_999.DAT")
  result <- file_name_processing(input, col_name = "Feed_dir")

  expect_equal(result$date, "20220101")  # Should match the first digit block in file name
})

test_that("Edge case: underscores and special characters are handled", {
  input <- c("a b.c/VR201230.DAT", "x/y z.VW220101.csv")
  result <- file_name_processing(input, col_name = "dir")

  expect_equal(result$date, c("201230", "220101"))
})

test_that("Error handling: file_names is not character", {
  expect_error(
    file_name_processing(123, "dir"),
    "`file_names` must be a character vector."
  )
})

test_that("Error handling: col_name is not a single character string", {
  expect_error(
    file_name_processing(c("a.txt"), c("x", "y")),
    "`col_name` must be a single character string."
  )
  expect_error(
    file_name_processing(c("a.txt"), 123),
    "`col_name` must be a single character string."
  )
})



# ---------------------- test for file_name_processing() ----------------------#

test_that("Normal case: matching dates between feed and water", {
  feed_files  <- c("feed/VR200715.DAT", "feed/VR200716.DAT", "feed/VR200717.DAT")
  water_files <- c("water/VW200715.DAT", "water/VW200717.DAT", "water/VW200718.DAT")

  res <- compare_files(feed_files, water_files)

  expect_type(res, "list")
  expect_named(res, c("feed", "water"))

  # Common dates are 200715 and 200717
  expect_equal(res$feed,  c("feed/VR200715.DAT", "feed/VR200717.DAT"))
  expect_equal(res$water, c("water/VW200715.DAT", "water/VW200717.DAT"))
})

test_that("Edge case: no common dates returns empty vectors", {
  feed_files  <- c("feed/VR200715.DAT", "feed/VR200716.DAT")
  water_files <- c("water/VW200718.DAT", "water/VW200719.DAT")

  res <- compare_files(feed_files, water_files)

  expect_equal(res$feed,  character(0))
  expect_equal(res$water, character(0))
})

test_that("Edge case: one side empty returns empty vectors", {
  res1 <- compare_files(character(0), c("water/VW200715.DAT"))
  expect_equal(res1$feed,  character(0))
  expect_equal(res1$water, character(0))

  res2 <- compare_files(c("feed/VR200715.DAT"), character(0))
  expect_equal(res2$feed,  character(0))
  expect_equal(res2$water, character(0))

  res3 <- compare_files(character(0), character(0))
  expect_equal(res3$feed,  character(0))
  expect_equal(res3$water, character(0))
})

test_that("Error handling: inputs must be character vectors", {
  expect_error(
    compare_files(123, c("water/VW200715.DAT")),
    "`file_names.f` must be a character vector\\."
  )
})

test_that("Handles list inputs by unlisting", {
  # Feed as list, water as character vector
  feed_list  <- list("feed/VR200715.DAT", "feed/VR200716.DAT", "feed/VR200717.DAT")
  water_vec  <- c("water/VW200715.DAT", "water/VW200717.DAT")
  res1 <- compare_files(feed_list, water_vec)
  expect_equal(res1$feed,  c("feed/VR200715.DAT", "feed/VR200717.DAT"))
  expect_equal(res1$water, c("water/VW200715.DAT", "water/VW200717.DAT"))

  # Feed as character vector, water as list
  feed_vec <- c("feed/VR200715.DAT", "feed/VR200716.DAT")
  water_list <- list("water/VW200715.DAT", "water/VW200716.DAT")
  res2 <- compare_files(feed_vec, water_list)
  expect_equal(res2$feed,  c("feed/VR200715.DAT", "feed/VR200716.DAT"))
  expect_equal(res2$water, c("water/VW200715.DAT", "water/VW200716.DAT"))

  # Both as lists
  both_lists_feed  <- list("feed/VR200801.DAT", "feed/VR200802.DAT")
  both_lists_water <- list("water/VW200801.DAT", "water/VW200803.DAT")
  res3 <- compare_files(both_lists_feed, both_lists_water)
  expect_equal(res3$feed,  "feed/VR200801.DAT")
  expect_equal(res3$water, "water/VW200801.DAT")
})



# ---------------------- test for get_date_range() ----------------------#

test_that("single file returns its own date token", {
  expect_equal(
    get_date_range("feed/VR200715.DAT"),
    "200715"
  )
})

test_that("multiple unordered files yield the correct min‐max span", {
  files <- c("feed/VR200720.DAT", "feed/VR200715.DAT", "feed/VR200716.DAT")
  expect_equal(
    get_date_range(files),
    "200715_200720"
  )
})

test_that("works when dates are already sorted", {
  files <- c("water/VW200801.DAT", "water/VW200803.DAT")
  expect_equal(
    get_date_range(files),
    "200801_200803"
  )
})

test_that("empty input vector gives NA with a warning", {
  expect_warning(
    out <- get_date_range(character(0)),
    "`df` has no rows; returning NA_character_"
  )
  expect_true(is.na(out))
})

test_that("non‐character input errors out", {
  expect_error(
    get_date_range(123),
    "`file_names` must be a character vector"
  )
})

test_that("files with no digits produce NA (and a warning)", {
  # file_name_processing will yield date = NA for both,
  # helper will see zero non‐NA dates → warning + NA
  expect_warning(
    out <- get_date_range(c("foo.txt", "bar.doc")),
    "column is NA"
  )
  expect_true(is.na(out))
})
