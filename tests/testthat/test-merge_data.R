# -----------------------------------------------------------------------------#
# ---------------------------- Tests for combine_feed_water ------------------#
# -----------------------------------------------------------------------------#

test_that("combine_feed_water works for normal case", {
  fed_list <- list(
    "2024-01-01" = data.frame(cow = 1:2, intake = c(10, 20)),
    "2024-01-02" = data.frame(cow = 3:4, intake = c(30, 40))
  )
  wat_list <- list(
    "2024-01-01" = data.frame(cow = 1:2, intake = c(5, 15)),
    "2024-01-02" = data.frame(cow = 3:4, intake = c(25, 35))
  )

  combined <- combine_feed_water(fed_list, wat_list)

  expect_length(combined, 2)
  expect_named(combined, c("2024-01-01", "2024-01-02"))
  expect_true(all(c("cow", "intake") %in% names(combined[[1]])))
  expect_true(all(c("cow", "intake") %in% names(combined[[2]])))
  expect_equal(nrow(combined[[1]]), 4)  # two rows from feed + two rows from water
})

test_that("combine_feed_water throws error if lists are not lists", {
  expect_error(combine_feed_water(1, list()), "Both `all_fed` and `all_wat` must be lists")
  expect_error(combine_feed_water(list(), "wrong"), "Both `all_fed` and `all_wat` must be lists")
})

test_that("combine_feed_water throws error if list lengths differ", {
  fed_list <- list("2024-01-01" = data.frame(x = 1))
  wat_list <- list(
    "2024-01-01" = data.frame(x = 1),
    "2024-01-02" = data.frame(x = 2)
  )
  expect_error(combine_feed_water(fed_list, wat_list), "The lengths of `all_fed` and `all_wat` must be the same")
})

test_that("combine_feed_water throws error if names do not match", {
  fed_list <- list("2024-01-01" = data.frame(x = 1))
  wat_list <- list("2024-01-02" = data.frame(x = 1))
  expect_error(combine_feed_water(fed_list, wat_list), "`all_fed` and `all_wat` must have identical names")
})

# -----------------------------------------------------------------------------#
# ---------------------------- Tests for merge_list_df ------------------------#
# -----------------------------------------------------------------------------#

test_that("merge_list_df works for normal case", {
  data_list <- list(
    data.frame(cow = 1:2, feed = c(10, 20)),
    data.frame(cow = 3:4, feed = c(30, 40))
  )

  merged <- merge_list_df(data_list)

  expect_s3_class(merged, "data.frame")
  expect_equal(nrow(merged), 4)
  expect_named(merged, c("cow", "feed"))
})

test_that("merge_list_df throws error for non-list input", {
  expect_error(merge_list_df(1), "`data_list` must be a list")
  expect_error(merge_list_df(data.frame(x = 1:3)), "`data_list` must be a list")
})

test_that("merge_list_df throws error for empty list input", {
  expect_error(merge_list_df(list()), "`data_list` is empty; nothing to merge")
})
