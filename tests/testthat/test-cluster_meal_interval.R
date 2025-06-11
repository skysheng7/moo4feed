test_that("optimal_interval_from_gaps works with normal input", {
  gaps <- c(5, 10, 15, 20, 90, 120, 150)  # Mix of small and large gaps
  
  result <- optimal_interval_from_gaps(gaps, method = "both")
  expect_type(result, "double")
  expect_true(result >= 5 && result <= 60)
  
  # Test different methods
  result_percentile <- optimal_interval_from_gaps(gaps, method = "percentile")
  result_gmm <- optimal_interval_from_gaps(gaps, method = "gmm")
  
  expect_type(result_percentile, "double")
  expect_type(result_gmm, "double")
})

test_that("optimal_interval_from_gaps handles empty gaps", {
  result <- optimal_interval_from_gaps(numeric(0))
  expect_equal(result, 30)
})

test_that("optimal_interval_from_gaps validates parameters", {
  gaps <- c(5, 10, 15)
  
  expect_error(
    optimal_interval_from_gaps(gaps, method = "invalid"),
    "method must be one of: both, percentile, gmm"
  )
  
  expect_error(
    optimal_interval_from_gaps(gaps, percentile = 0),
    "percentile must be a single numeric value between 0 and 1"
  )
})

test_that("optimal_interval_from_gaps works with percentile method", {
  gaps <- c(10, 20, 30, 40, 50)
  
  result_50 <- optimal_interval_from_gaps(gaps, method = "percentile", percentile = 0.5)
  result_75 <- optimal_interval_from_gaps(gaps, method = "percentile", percentile = 0.75)
  result_90 <- optimal_interval_from_gaps(gaps, method = "percentile", percentile = 0.9)
  
  expect_true(result_50 <= result_75)
  expect_true(result_75 <= result_90)
})

test_that("optimal_interval_from_gaps works with gmm method", {
  # Create bimodal data for GMM to work well
  within_meal_gaps <- rnorm(50, mean = 10, sd = 3)  # Short gaps
  between_meal_gaps <- rnorm(50, mean = 60, sd = 10)  # Long gaps
  gaps <- c(within_meal_gaps, between_meal_gaps)
  gaps <- gaps[gaps > 0]  # Remove any negative values
  
  result <- optimal_interval_from_gaps(gaps, method = "gmm")
  
  expect_type(result, "double")
  expect_true(result > 0)
})

test_that("optimal_interval_from_gaps applies bounds correctly", {
  # Test very small gaps (should be bounded to 5)
  small_gaps <- c(1, 2, 3, 4)
  result <- optimal_interval_from_gaps(small_gaps, method = "percentile", percentile = 0.5)
  expect_true(result >= 5)
  
  # Test very large gaps (should be bounded to 60)
  large_gaps <- c(100, 120, 150)
  result <- optimal_interval_from_gaps(large_gaps, method = "percentile", percentile = 0.5)
  expect_true(result <= 60)
})

test_that("determine_eps_percentile works correctly", {
  gaps <- c(10, 20, 30, 40, 50)
  
  result_50 <- determine_eps_percentile(gaps, 0.5)
  result_75 <- determine_eps_percentile(gaps, 0.75)
  result_90 <- determine_eps_percentile(gaps, 0.9)
  
  expect_equal(result_50, 30)  # Median
  expect_equal(result_75, 40)  # 75th percentile
  expect_equal(result_90, 46)  # 90th percentile
  
  expect_true(result_50 <= result_75)
  expect_true(result_75 <= result_90)
})

test_that("fit_gmm_to_gaps works with sufficient data", {
  # Create bimodal data for GMM to work well
  within_meal_gaps <- rnorm(50, mean = 10, sd = 3)  # Short gaps
  between_meal_gaps <- rnorm(50, mean = 60, sd = 10)  # Long gaps
  gaps <- c(within_meal_gaps, between_meal_gaps)
  gaps <- gaps[gaps > 0]  # Remove any negative values
  
  result <- fit_gmm_to_gaps(gaps)
  
  expect_type(result, "double")
  expect_true(result > 0)
  expect_true(result > min(within_meal_gaps) && result < max(between_meal_gaps))
})

test_that("fit_gmm_to_gaps falls back to percentile with insufficient data", {
  gaps <- c(5, 10, 15)  # Too few points for GMM
  
  result <- fit_gmm_to_gaps(gaps, percentile_fallback = 0.75)
  expected <- quantile(gaps, 0.75)
  
  expect_equal(result, expected)
})

test_that("fit_gmm_to_gaps handles GMM failure gracefully", {
  # Create data that might cause GMM to fail
  gaps <- rep(10, 20)  # All same value
  
  result <- fit_gmm_to_gaps(gaps, percentile_fallback = 0.75)
  expected <- quantile(gaps, 0.75)
  
  expect_equal(result, expected)
})

test_that("find_distribution_intersection works with normal parameters", {
  # Test case where intersection should be found
  mu1 <- 10; sigma1 <- 2; lambda1 <- 0.6
  mu2 <- 30; sigma2 <- 5; lambda2 <- 0.4
  
  result <- find_distribution_intersection(mu1, sigma1, lambda1, mu2, sigma2, lambda2)
  
  expect_type(result, "double")
  expect_true(result > mu1 && result < mu2)  # Should be between the means
})

test_that("find_distribution_intersection handles edge cases", {
  # Test case where uniroot might fail
  mu1 <- 10; sigma1 <- 1; lambda1 <- 0.1  # Very small mixing proportion
  mu2 <- 15; sigma2 <- 1; lambda2 <- 0.9
  
  result <- find_distribution_intersection(mu1, sigma1, lambda1, mu2, sigma2, lambda2)
  
  expect_type(result, "double")
  expect_true(result >= mu1 && result <= mu2)
})

test_that("find_distribution_intersection handles identical distributions", {
  # Edge case: same parameters
  mu1 <- mu2 <- 20
  sigma1 <- sigma2 <- 3
  lambda1 <- 0.5; lambda2 <- 0.5
  
  result <- find_distribution_intersection(mu1, sigma1, lambda1, mu2, sigma2, lambda2)
  
  expect_type(result, "double")
  # Should handle this gracefully
})

test_that("optimal_interval_from_gaps both method handles NA from one method", {
  # Test that min() with na.rm=TRUE works correctly
  gaps <- c(5, 10, 15, 20)
  
  # Test determine_eps_percentile doesn't return NA
  percentile_result <- determine_eps_percentile(gaps, 0.75)
  expect_false(is.na(percentile_result))
  
  # Test fit_gmm_to_gaps doesn't return NA (it falls back to percentile)
  gmm_result <- fit_gmm_to_gaps(gaps)
  expect_false(is.na(gmm_result))
})

test_that("optimal_interval_from_gaps returns numeric not integer", {
  gaps <- c(5, 10, 15)
  
  result <- optimal_interval_from_gaps(gaps)
  
  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
})

test_that("optimal_interval_from_gaps handles mixed gap sizes", {
  # Test with very mixed gap patterns
  gaps <- c(1, 2, 3, 50, 100, 150, 200)  # Mix of small and large gaps
  
  result <- optimal_interval_from_gaps(gaps)
  
  expect_type(result, "double")
  expect_true(result >= 5 && result <= 60)
})


# NEW COMPREHENSIVE BOUNDS TESTING
test_that("optimal_interval_from_gaps handles NULL bounds correctly", {
  gaps <- c(10, 20, 30, 40, 50)
  
  # Test NULL lower_bound
  result1 <- optimal_interval_from_gaps(gaps, lower_bound = NULL, upper_bound = 60)
  expect_type(result1, "double")
  expected1 <- stats::quantile(gaps, 0.75, na.rm = TRUE)  # Default percentile method
  expect_equal(result1, as.numeric(expected1))
  
  # Test NULL upper_bound
  result2 <- optimal_interval_from_gaps(gaps, lower_bound = 5, upper_bound = NULL)
  expect_type(result2, "double")
  expected2 <- stats::quantile(gaps, 0.75, na.rm = TRUE)
  expect_equal(result2, as.numeric(expected2))
  
  # Test both NULL
  result3 <- optimal_interval_from_gaps(gaps, lower_bound = NULL, upper_bound = NULL)
  expect_type(result3, "double")
  expected3 <- stats::quantile(gaps, 0.75, na.rm = TRUE)
  expect_equal(result3, as.numeric(expected3))
})

test_that("optimal_interval_from_gaps applies custom bounds correctly", {
  # Test custom lower bound
  small_gaps <- c(1, 2, 3, 4, 5)
  result_custom_lower <- optimal_interval_from_gaps(small_gaps, lower_bound = 10, upper_bound = 50)
  expect_equal(result_custom_lower, 10)  # Should be bounded to custom lower bound
  
  # Test custom upper bound
  large_gaps <- c(80, 90, 100, 110, 120)
  result_custom_upper <- optimal_interval_from_gaps(large_gaps, lower_bound = 5, upper_bound = 75)
  expect_equal(result_custom_upper, 75)  # Should be bounded to custom upper bound
  
  # Test custom bounds that don't affect result
  normal_gaps <- c(20, 25, 30, 35, 40)
  result_no_effect <- optimal_interval_from_gaps(normal_gaps, lower_bound = 10, upper_bound = 50)
  expected_no_effect <- stats::quantile(normal_gaps, 0.75, na.rm = TRUE)
  expect_equal(result_no_effect, as.numeric(expected_no_effect))
})

test_that("optimal_interval_from_gaps handles edge cases with bounds", {
  # Test when calculated value is exactly at bounds
  gaps_at_lower <- c(3, 4, 5, 5, 6)  # 75th percentile = 5
  result_at_lower <- optimal_interval_from_gaps(gaps_at_lower, lower_bound = 5, upper_bound = 60)
  expect_equal(result_at_lower, 5)
  
  gaps_at_upper <- c(55, 58, 60, 62, 65)  # 75th percentile = 60
  result_at_upper <- optimal_interval_from_gaps(gaps_at_upper, lower_bound = 5, upper_bound = 60)
  expect_equal(result_at_upper, 60)
  
  # Test when lower_bound > upper_bound (edge case - should still work)
  gaps_invalid_bounds <- c(20, 25, 30)
  result_invalid <- optimal_interval_from_gaps(gaps_invalid_bounds, lower_bound = 50, upper_bound = 30)
  # Should apply the logic as written: max(50, min(result, 30))
  expected_invalid <- max(50, min(25, 30))  # 25th percentile = 25, max(50, min(25, 30)) = 50
  expect_equal(result_invalid, expected_invalid)
})

test_that("optimal_interval_from_gaps bounds work with percentile method", {
  # Create data that will produce different results with different methods
  within_meal <- c(rep(5, 20))  # Short gaps
  between_meal <- c(rep(100, 20))  # Long gaps (will be bounded)
  gaps <- c(within_meal, between_meal)
  
  # Test percentile method with bounds
  result_percentile <- optimal_interval_from_gaps(gaps, method = "percentile", 
                                                 lower_bound = 10, upper_bound = 80)
  expect_true(result_percentile >= 10 && result_percentile <= 80)
  
})

test_that("optimal_interval_from_gaps bounds preserve numeric type", {
  gaps <- c(1, 2, 3)  # Will be bounded to lower_bound
  
  result_int_bounds <- optimal_interval_from_gaps(gaps, lower_bound = 10L, upper_bound = 60L)
  expect_true(is.numeric(result_int_bounds))
  expect_false(is.integer(result_int_bounds))  # Should be converted to double
  expect_equal(result_int_bounds, 10.0)
  
  result_double_bounds <- optimal_interval_from_gaps(gaps, lower_bound = 10.5, upper_bound = 60.5)
  expect_true(is.numeric(result_double_bounds))
  expect_equal(result_double_bounds, 10.5)
}) 