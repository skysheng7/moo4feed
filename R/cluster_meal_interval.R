
#' Determine optimal interval (i.e., eps) parameter for DBSCAN clustering
#'
#' @param start_times Numeric vector of visit start times (minutes from midnight)
#' @param end_times Numeric vector of visit end times (minutes from midnight)
#'
#' @return Optimal eps value in minutes
#' @keywords internal
#' @noRd
optimal_interval <- function(start_times, end_times) {
  
  if (length(start_times) <= 1) {
    return(30) # Default fallback
  }
  
  # Calculate inter-visit gaps (from end of previous visit to start of current visit)
  if (length(start_times) != length(end_times)) {
    stop("start_times and end_times must have the same length")
  }
  
  # Sort by start times and get corresponding end times
  sorted_indices <- order(start_times)
  sorted_start_times <- start_times[sorted_indices]
  sorted_end_times <- end_times[sorted_indices]
  
  # Calculate gaps between end of previous visit and start of current visit
  gaps <- numeric(length(sorted_start_times) - 1)
  for (i in 2:length(sorted_start_times)) {
    gaps[i - 1] <- sorted_start_times[i] - sorted_end_times[i - 1]
  }
  
  # Remove negative gaps (overlapping visits)
  gaps <- gaps[gaps >= 0]
  
  if (length(gaps) == 0) {
    return(30) # Default fallback
  }
  
  # Method 1: 75th percentile of gaps
  percentile_eps <- determine_eps_percentile(gaps)
  
  # Method 2: Gaussian mixture modeling
  gmm_eps <- determine_eps_gmm(gaps)
  
  # Take the minimum of the two methods to be conservative
  optimal_eps <- min(percentile_eps, gmm_eps, na.rm = TRUE)
  
  # Apply reasonable bounds
  optimal_eps <- max(5, min(optimal_eps, 60)) # Between 5 and 60 minutes
  
  return(as.numeric(optimal_eps))
}

#' Determine eps using percentile-based method
#'
#' @param gaps Numeric vector of inter-visit gaps
#'
#' @return Eps value based on 75th percentile of gaps
#' @keywords internal
#' @noRd
determine_eps_percentile <- function(gaps) {
  if (length(gaps) == 0) {
    return(30)
  }
  
  # Use 75th percentile of gaps
  percentile_eps <- stats::quantile(gaps, 0.75, na.rm = TRUE)
  return(as.numeric(percentile_eps))
}

#' Determine eps using Gaussian mixture modeling
#'
#' @param gaps Numeric vector of inter-visit gaps
#'
#' @return Eps value based on intersection of within-meal and between-meal distributions
#' @keywords internal
#' @noRd
determine_eps_gmm <- function(gaps) {
  
  if (length(gaps) < 10) {
    # Not enough data for GMM, fall back to percentile
    return(determine_eps_percentile(gaps))
  }
  
  # Try to fit 2-component Gaussian mixture model
  tryCatch({
    # Fit 2-component normal mixture
    mix_fit <- mixtools::normalmixEM(gaps, k = 2, verb = FALSE, 
                                    maxit = 1000, epsilon = 1e-08)
    
    # Extract parameters for the two components
    mu1 <- mix_fit$mu[1]
    mu2 <- mix_fit$mu[2]
    sigma1 <- mix_fit$sigma[1]
    sigma2 <- mix_fit$sigma[2]
    lambda1 <- mix_fit$lambda[1]
    lambda2 <- mix_fit$lambda[2]
    
    # Ensure component 1 is the within-meal (smaller mean) distribution
    if (mu1 > mu2) {
    # Swap components
    temp_mu <- mu1; mu1 <- mu2; mu2 <- temp_mu
    temp_sigma <- sigma1; sigma1 <- sigma2; sigma2 <- temp_sigma
    temp_lambda <- lambda1; lambda1 <- lambda2; lambda2 <- temp_lambda
    }
    
    # Find intersection point between the two distributions
    intersection_eps <- find_distribution_intersection(mu1, sigma1, lambda1, 
                                                    mu2, sigma2, lambda2)
    
    return(intersection_eps)
  }, error = function(e) {
    # If GMM fails, fall back to percentile method
    return(determine_eps_percentile(gaps))
  })
}

#' Find intersection point between two normal distributions in a mixture
#'
#' @param mu1 Mean of first distribution (within-meal)
#' @param sigma1 Standard deviation of first distribution
#' @param lambda1 Mixing proportion of first distribution
#' @param mu2 Mean of second distribution (between-meal)
#' @param sigma2 Standard deviation of second distribution  
#' @param lambda2 Mixing proportion of second distribution
#'
#' @return Intersection point between the two weighted distributions
#' @keywords internal
#' @noRd
find_distribution_intersection <- function(mu1, sigma1, lambda1, mu2, sigma2, lambda2) {
  
  # Define the difference function between weighted densities
  diff_func <- function(x) {
    density1 <- lambda1 * stats::dnorm(x, mean = mu1, sd = sigma1)
    density2 <- lambda2 * stats::dnorm(x, mean = mu2, sd = sigma2)
    return(density1 - density2)
  }
  
  # Find intersection point between the distributions
  # Search in the range between the two means
  search_range <- c(mu1, mu2)
  
  tryCatch({
    # Use uniroot to find where the difference is zero
    intersection <- stats::uniroot(diff_func, interval = search_range, 
                                  extendInt = "yes", tol = 1e-6)
    return(intersection$root)
  }, error = function(e) {
    # If uniroot fails, use a conservative estimate
    # Use the point closer to the within-meal distribution
    return(mu1 + 0.5 * (mu2 - mu1))
  })
} 