test_that("synch_pairs_to_df works with single day data", {
  # Create simple matrices
  animal_ids <- c("1", "2", "3")
  bout_mat <- matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(animal_ids, animal_ids))
  time_mat <- bout_mat
  avg_mat <- bout_mat
  
  # Fill upper triangle
  bout_mat[1, 2] <- 2
  bout_mat[1, 3] <- 1
  bout_mat[2, 3] <- 3
  
  time_mat[1, 2] <- 100
  time_mat[1, 3] <- 50
  time_mat[2, 3] <- 150
  
  avg_mat[1, 2] <- 50
  avg_mat[1, 3] <- 50
  avg_mat[2, 3] <- 50
  
  synch_results <- list(
    bout = bout_mat,
    total_time = time_mat,
    avg_duration = avg_mat
  )
  
  # Convert to df
  result <- synch_pairs_to_df(synch_results, sort_by = NULL)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_named(result, c("animal1", "animal2", "bouts", "total_time", "avg_duration"))
  
  # Check values
  expect_equal(result$animal1, c("1", "1", "2"))
  expect_equal(result$animal2, c("2", "3", "3"))
  expect_equal(result$bouts, c(2, 1, 3))
  expect_equal(result$total_time, c(100, 50, 150))
  expect_equal(result$avg_duration, c(50, 50, 50))
})


test_that("synch_pairs_to_df works with multi-day data", {
  # Create matrices for 2 days
  animal_ids <- c("1", "2")
  
  # Day 1 - fill upper triangle [1,2]
  bout_day1 <- matrix(c(0, 0, 2, 0), nrow = 2,
                      dimnames = list(animal_ids, animal_ids))
  time_day1 <- matrix(c(0, 0, 100, 0), nrow = 2,
                      dimnames = list(animal_ids, animal_ids))
  avg_day1 <- matrix(c(0, 0, 50, 0), nrow = 2,
                     dimnames = list(animal_ids, animal_ids))
  
  # Day 2 - fill upper triangle [1,2]
  bout_day2 <- matrix(c(0, 0, 3, 0), nrow = 2,
                      dimnames = list(animal_ids, animal_ids))
  time_day2 <- matrix(c(0, 0, 150, 0), nrow = 2,
                      dimnames = list(animal_ids, animal_ids))
  avg_day2 <- matrix(c(0, 0, 50, 0), nrow = 2,
                     dimnames = list(animal_ids, animal_ids))
  
  synch_results <- list(
    bout = list(day1 = bout_day1, day2 = bout_day2),
    total_time = list(day1 = time_day1, day2 = time_day2),
    avg_duration = list(day1 = avg_day1, day2 = avg_day2)
  )
  
  result <- synch_pairs_to_df(synch_results, sort_by = NULL)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_named(result, c("animal1", "animal2", "day", "bouts", "total_time", "avg_duration"))
  
  # Check values
  expect_equal(result$animal1, c("1", "1"))
  expect_equal(result$animal2, c("2", "2"))
  expect_equal(result$day, c("day1", "day2"))
  expect_equal(result$bouts, c(2, 3))
  expect_equal(result$total_time, c(100, 150))
})


test_that("synch_pairs_to_df filters by min_time", {
  animal_ids <- c("1", "2", "3")
  bout_mat <- matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(animal_ids, animal_ids))
  time_mat <- bout_mat
  avg_mat <- bout_mat
  
  # Fill with different time values
  time_mat[1, 2] <- 100
  time_mat[1, 3] <- 50
  time_mat[2, 3] <- 10
  
  bout_mat[1, 2] <- 2
  bout_mat[1, 3] <- 1
  bout_mat[2, 3] <- 1
  
  avg_mat[1, 2] <- 50
  avg_mat[1, 3] <- 50
  avg_mat[2, 3] <- 10
  
  synch_results <- list(
    bout = bout_mat,
    total_time = time_mat,
    avg_duration = avg_mat
  )
  
  # Filter with min_time = 30
  result <- synch_pairs_to_df(synch_results, min_time = 30, sort_by = NULL)
  
  expect_equal(nrow(result), 2)  # Only pairs with time > 30
  expect_equal(result$total_time, c(100, 50))
})


test_that("synch_pairs_to_df sorts correctly", {
  animal_ids <- c("1", "2", "3")
  bout_mat <- matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(animal_ids, animal_ids))
  time_mat <- bout_mat
  avg_mat <- bout_mat
  
  time_mat[1, 2] <- 50
  time_mat[1, 3] <- 150
  time_mat[2, 3] <- 100
  
  bout_mat[1, 2] <- 1
  bout_mat[1, 3] <- 3
  bout_mat[2, 3] <- 2
  
  avg_mat[1, 2] <- 50
  avg_mat[1, 3] <- 50
  avg_mat[2, 3] <- 50
  
  synch_results <- list(
    bout = bout_mat,
    total_time = time_mat,
    avg_duration = avg_mat
  )
  
  # Sort by total_time descending (default)
  result_desc <- synch_pairs_to_df(synch_results, sort_by = "total_time", decreasing = TRUE)
  expect_equal(result_desc$total_time, c(150, 100, 50))
  
  # Sort by total_time ascending
  result_asc <- synch_pairs_to_df(synch_results, sort_by = "total_time", decreasing = FALSE)
  expect_equal(result_asc$total_time, c(50, 100, 150))
  
  # Sort by bouts
  result_bouts <- synch_pairs_to_df(synch_results, sort_by = "bouts", decreasing = TRUE)
  expect_equal(result_bouts$bouts, c(3, 2, 1))
})


test_that("synch_pairs_to_df handles single animal", {
  animal_ids <- "1"
  bout_mat <- matrix(0, nrow = 1, ncol = 1,
                     dimnames = list(animal_ids, animal_ids))
  time_mat <- bout_mat
  avg_mat <- bout_mat
  
  synch_results <- list(
    bout = bout_mat,
    total_time = time_mat,
    avg_duration = avg_mat
  )
  
  result <- synch_pairs_to_df(synch_results)
  
  # Should return empty data frame with correct columns
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("animal1", "animal2", "bouts", "total_time", "avg_duration"))
})


test_that("synch_pairs_to_df handles no pairs above threshold", {
  animal_ids <- c("1", "2", "3")
  bout_mat <- matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(animal_ids, animal_ids))
  time_mat <- bout_mat
  avg_mat <- bout_mat
  
  # All times are 0
  synch_results <- list(
    bout = bout_mat,
    total_time = time_mat,
    avg_duration = avg_mat
  )
  
  result <- synch_pairs_to_df(synch_results, min_time = 0)
  
  # Should return empty data frame
  expect_equal(nrow(result), 0)
  expect_named(result, c("animal1", "animal2", "bouts", "total_time", "avg_duration"))
})


test_that("synch_pairs_to_df validates inputs", {
  # Not a list
  expect_error(synch_pairs_to_df("not a list"),
               "must be a list")
  
  # Missing elements
  expect_error(synch_pairs_to_df(list(bout = matrix())),
               "must contain elements")
  
  # Invalid min_time
  expect_error(synch_pairs_to_df(
    list(bout = matrix(0, 2, 2), total_time = matrix(0, 2, 2), avg_duration = matrix(0, 2, 2)),
    min_time = -1
  ), "must be a single non-negative number")
  
  expect_error(synch_pairs_to_df(
    list(bout = matrix(0, 2, 2), total_time = matrix(0, 2, 2), avg_duration = matrix(0, 2, 2)),
    min_time = c(1, 2)
  ), "must be a single non-negative number")
  
  # Invalid decreasing
  expect_error(synch_pairs_to_df(
    list(bout = matrix(0, 2, 2), total_time = matrix(0, 2, 2), avg_duration = matrix(0, 2, 2)),
    decreasing = "yes"
  ), "must be TRUE or FALSE")
  
  # Invalid sort_by column
  animal_ids <- c("1", "2")
  bout_mat <- matrix(0, nrow = 2, ncol = 2, dimnames = list(animal_ids, animal_ids))
  expect_error(
    synch_pairs_to_df(
      list(bout = bout_mat, total_time = bout_mat, avg_duration = bout_mat),
      sort_by = "invalid_column"
    ),
    "must be one of"
  )
})


test_that("synch_neighbor_compare works with single day data", {
  # Create co-occurrence matrices
  animal_ids <- c("1", "2", "3")
  
  # Pair results (total co-occurrence)
  pair_bout <- matrix(0, nrow = 3, ncol = 3, dimnames = list(animal_ids, animal_ids))
  pair_time <- pair_bout
  pair_avg <- pair_bout
  
  pair_time[1, 2] <- 100
  pair_time[1, 3] <- 80
  pair_time[2, 3] <- 60
  
  pair_results <- list(
    bout = pair_bout,
    total_time = pair_time,
    avg_duration = pair_avg
  )
  
  # Neighbor results
  neighbor_bout <- matrix(0, nrow = 3, ncol = 3, dimnames = list(animal_ids, animal_ids))
  neighbor_time <- neighbor_bout
  neighbor_avg <- neighbor_bout
  
  neighbor_time[1, 2] <- 50  # 50% of time as neighbors
  neighbor_time[1, 3] <- 80  # 100% of time as neighbors
  neighbor_time[2, 3] <- 30  # 50% of time as neighbors
  
  neighbor_results <- list(
    bout = neighbor_bout,
    total_time = neighbor_time,
    avg_duration = neighbor_avg
  )
  
  result <- synch_neighbor_compare(pair_results, neighbor_results, sort_by = NULL)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_named(result, c("animal1", "animal2", "cooccurrence_time",
                         "neighbor_time", "neighbor_ratio"))
  
  # Check values
  expect_equal(result$cooccurrence_time, c(100, 80, 60))
  expect_equal(result$neighbor_time, c(50, 80, 30))
  expect_equal(result$neighbor_ratio, c(0.5, 1.0, 0.5))
})


test_that("synch_neighbor_compare works with multi-day data", {
  animal_ids <- c("1", "2")
  
  # Day 1 - fill upper triangle [1,2]
  pair_time_day1 <- matrix(c(0, 0, 100, 0), nrow = 2,
                           dimnames = list(animal_ids, animal_ids))
  neighbor_time_day1 <- matrix(c(0, 0, 50, 0), nrow = 2,
                               dimnames = list(animal_ids, animal_ids))
  
  # Day 2 - fill upper triangle [1,2]
  pair_time_day2 <- matrix(c(0, 0, 80, 0), nrow = 2,
                           dimnames = list(animal_ids, animal_ids))
  neighbor_time_day2 <- matrix(c(0, 0, 60, 0), nrow = 2,
                               dimnames = list(animal_ids, animal_ids))
  
  pair_results <- list(
    bout = list(day1 = pair_time_day1, day2 = pair_time_day2),
    total_time = list(day1 = pair_time_day1, day2 = pair_time_day2),
    avg_duration = list(day1 = pair_time_day1, day2 = pair_time_day2)
  )
  
  neighbor_results <- list(
    bout = list(day1 = neighbor_time_day1, day2 = neighbor_time_day2),
    total_time = list(day1 = neighbor_time_day1, day2 = neighbor_time_day2),
    avg_duration = list(day1 = neighbor_time_day1, day2 = neighbor_time_day2)
  )
  
  result <- synch_neighbor_compare(pair_results, neighbor_results, sort_by = NULL)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_named(result, c("animal1", "animal2", "day", "cooccurrence_time",
                         "neighbor_time", "neighbor_ratio"))
  
  # Check values
  expect_equal(result$day, c("day1", "day2"))
  expect_equal(result$cooccurrence_time, c(100, 80))
  expect_equal(result$neighbor_time, c(50, 60))
  expect_equal(result$neighbor_ratio, c(0.5, 0.75))
})


test_that("synch_neighbor_compare filters by min_cooccurrence", {
  animal_ids <- c("1", "2", "3")
  
  pair_time <- matrix(0, nrow = 3, ncol = 3, dimnames = list(animal_ids, animal_ids))
  pair_time[1, 2] <- 100
  pair_time[1, 3] <- 50
  pair_time[2, 3] <- 10
  
  neighbor_time <- matrix(0, nrow = 3, ncol = 3, dimnames = list(animal_ids, animal_ids))
  neighbor_time[1, 2] <- 50
  neighbor_time[1, 3] <- 25
  neighbor_time[2, 3] <- 5
  
  pair_results <- list(
    bout = pair_time,
    total_time = pair_time,
    avg_duration = pair_time
  )
  
  neighbor_results <- list(
    bout = neighbor_time,
    total_time = neighbor_time,
    avg_duration = neighbor_time
  )
  
  # Filter pairs with co-occurrence < 30
  result <- synch_neighbor_compare(pair_results, neighbor_results,
                                   min_cooccurrence = 30, sort_by = NULL)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$cooccurrence_time, c(100, 50))
})


test_that("synch_neighbor_compare sorts correctly", {
  animal_ids <- c("1", "2", "3")
  
  pair_time <- matrix(0, nrow = 3, ncol = 3, dimnames = list(animal_ids, animal_ids))
  pair_time[1, 2] <- 100
  pair_time[1, 3] <- 100
  pair_time[2, 3] <- 100
  
  neighbor_time <- matrix(0, nrow = 3, ncol = 3, dimnames = list(animal_ids, animal_ids))
  neighbor_time[1, 2] <- 50  # ratio 0.5
  neighbor_time[1, 3] <- 80  # ratio 0.8
  neighbor_time[2, 3] <- 20  # ratio 0.2
  
  pair_results <- list(
    bout = pair_time,
    total_time = pair_time,
    avg_duration = pair_time
  )
  
  neighbor_results <- list(
    bout = neighbor_time,
    total_time = neighbor_time,
    avg_duration = neighbor_time
  )
  
  # Sort by ratio descending
  result_desc <- synch_neighbor_compare(pair_results, neighbor_results,
                                        sort_by = "neighbor_ratio", decreasing = TRUE)
  expect_equal(result_desc$neighbor_ratio, c(0.8, 0.5, 0.2))
  
  # Sort by ratio ascending
  result_asc <- synch_neighbor_compare(pair_results, neighbor_results,
                                       sort_by = "neighbor_ratio", decreasing = FALSE)
  expect_equal(result_asc$neighbor_ratio, c(0.2, 0.5, 0.8))
  
  # Sort by neighbor_time
  result_time <- synch_neighbor_compare(pair_results, neighbor_results,
                                        sort_by = "neighbor_time", decreasing = TRUE)
  expect_equal(result_time$neighbor_time, c(80, 50, 20))
})


test_that("synch_neighbor_compare handles zero division", {
  animal_ids <- c("1", "2")
  
  # Co-occurrence time is 0 (shouldn't happen in practice but test robustness)
  pair_time <- matrix(c(0, 0, 0, 0), nrow = 2,
                      dimnames = list(animal_ids, animal_ids))
  neighbor_time <- matrix(c(0, 0, 0, 0), nrow = 2,
                          dimnames = list(animal_ids, animal_ids))
  
  pair_results <- list(
    bout = pair_time,
    total_time = pair_time,
    avg_duration = pair_time
  )
  
  neighbor_results <- list(
    bout = neighbor_time,
    total_time = neighbor_time,
    avg_duration = neighbor_time
  )
  
  # Should return empty df (no pairs above min_cooccurrence = 0)
  result <- synch_neighbor_compare(pair_results, neighbor_results, min_cooccurrence = 0)
  expect_equal(nrow(result), 0)
})


test_that("synch_neighbor_compare validates inputs", {
  animal_ids <- c("1", "2")
  valid_mat <- matrix(0, nrow = 2, ncol = 2, dimnames = list(animal_ids, animal_ids))
  valid_results <- list(bout = valid_mat, total_time = valid_mat, avg_duration = valid_mat)
  
  # Not lists
  expect_error(synch_neighbor_compare("not a list", valid_results),
               "must be lists")
  
  # Missing elements
  expect_error(synch_neighbor_compare(list(bout = valid_mat), valid_results),
               "must contain")
  
  # Mismatched single vs multi-day
  multi_results <- list(
    bout = list(day1 = valid_mat),
    total_time = list(day1 = valid_mat),
    avg_duration = list(day1 = valid_mat)
  )
  expect_error(synch_neighbor_compare(valid_results, multi_results),
               "must both be single-day or both be multi-day")
  
  # Different number of days
  multi_results2 <- list(
    bout = list(day1 = valid_mat, day2 = valid_mat),
    total_time = list(day1 = valid_mat, day2 = valid_mat),
    avg_duration = list(day1 = valid_mat, day2 = valid_mat)
  )
  multi_results3 <- list(
    bout = list(day1 = valid_mat),
    total_time = list(day1 = valid_mat),
    avg_duration = list(day1 = valid_mat)
  )
  expect_error(synch_neighbor_compare(multi_results2, multi_results3),
               "must have the same number of days")
  
  # Invalid min_cooccurrence
  expect_error(synch_neighbor_compare(valid_results, valid_results, min_cooccurrence = -1),
               "must be a single non-negative number")
  
  # Invalid sort_by
  valid_mat[1, 2] <- 10
  valid_results2 <- list(bout = valid_mat, total_time = valid_mat, avg_duration = valid_mat)
  expect_error(synch_neighbor_compare(valid_results2, valid_results2, sort_by = "invalid"),
               "must be one of")
})


test_that("extract_upper_triangle handles edge cases", {
  # Test with mismatched dimensions
  mat1 <- matrix(0, nrow = 2, ncol = 2)
  mat2 <- matrix(0, nrow = 3, ncol = 3)
  
  expect_error(
    moo4feed:::extract_upper_triangle(mat1, mat2, mat1, 0),
    "same dimensions"
  )
  
  # Test with mismatched names
  mat1 <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("1", "2"), c("1", "2")))
  mat2 <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("1", "3"), c("1", "3")))
  
  expect_error(
    moo4feed:::extract_upper_triangle(mat1, mat2, mat1, 0),
    "identical row and column names"
  )
})


test_that("compare_matrices handles edge cases", {
  # Test with mismatched dimensions
  mat1 <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("1", "2"), c("1", "2")))
  mat2 <- matrix(0, nrow = 3, ncol = 3, dimnames = list(c("1", "2", "3"), c("1", "2", "3")))
  
  expect_error(
    moo4feed:::compare_matrices(mat1, mat2, 0),
    "same dimensions"
  )
  
  # Test with mismatched animal IDs
  mat1 <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("1", "2"), c("1", "2")))
  mat2 <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("1", "3"), c("1", "3")))
  
  expect_error(
    moo4feed:::compare_matrices(mat1, mat2, 0),
    "same animal IDs"
  )
  
  # Test single animal
  mat1 <- matrix(0, nrow = 1, ncol = 1, dimnames = list("1", "1"))
  result <- moo4feed:::compare_matrices(mat1, mat1, 0)
  
  expect_equal(nrow(result), 0)
  expect_named(result, c("animal1", "animal2", "cooccurrence_time",
                         "neighbor_time", "neighbor_ratio"))
})
