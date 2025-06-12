#' Determine optimal interval from gaps directly
#'
#' Wrapper function that calculates optimal eps from pre-calculated gaps.
#' This is used internally when gaps have already been calculated properly by animal.
#'
#' @param gaps Numeric vector of inter-visit gaps in minutes
#' @param method Character string specifying the method to use
#' @param percentile Numeric value between 0 and 1 for percentile method
#' @param lower_bound Numeric value for lower bound of the optimal interval
#' @param upper_bound Numeric value for upper bound of the optimal interval
#' @param use_log_transform Logical indicating whether to use log transformation for GMM fitting
#' @param log_multiplier Numeric value for multiplier of log transformation
#' @param log_offset Numeric value for offset of log transformation
#'
#' @return Optimal eps value in minutes
#' @keywords internal
#' @noRd
optimal_interval_from_gaps <- function(gaps, method = "both", percentile = 0.9, lower_bound = 5, upper_bound = 60, use_log_transform = TRUE, log_multiplier = 20, log_offset = 1) {
  
  if (length(gaps) == 0) {
    warning("There is only 1 row of data in the provided dataframe, no gaps between visits available for analysis, returning default interval of 30 minutes")
    return(30)
  }
  
  # Validate method parameter
  valid_methods <- c("both", "percentile", "gmm")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }
  
  # Validate percentile parameter
  if (!is.numeric(percentile) || length(percentile) != 1 || percentile <= 0 || percentile >= 1) {
    stop("percentile must be a single numeric value between 0 and 1")
  }
  
  # Choose method(s) to use
  if (method == "percentile") {
    optimal_eps <- determine_eps_percentile(gaps, percentile)
  } else if (method == "gmm") {
    optimal_eps <- fit_gmm_to_gaps(gaps, percentile, use_log_transform = use_log_transform, log_multiplier = log_multiplier, log_offset = log_offset)
  } else { # method == "both"
    percentile_eps <- determine_eps_percentile(gaps, percentile)
    gmm_eps <- fit_gmm_to_gaps(gaps, percentile, use_log_transform = use_log_transform, log_multiplier = log_multiplier, log_offset = log_offset)
    # Take the minimum of the two methods to be conservative
    optimal_eps <- min(percentile_eps, gmm_eps, na.rm = TRUE)
  }
  
  # Apply reasonable bounds
  if (!is.null(lower_bound) && !is.null(upper_bound)) {
    optimal_eps <- max(lower_bound, min(optimal_eps, upper_bound)) # Between lower_bound and upper_bound
  } else {
    optimal_eps <- optimal_eps
  }
  
  return(as.numeric(optimal_eps))
}

#' Determine eps using percentile-based method
#'
#' @param gaps Numeric vector of inter-visit gaps
#' @param percentile Numeric value between 0 and 1 specifying which percentile to use
#'
#' @return Eps value based on specified percentile of gaps
#' @keywords internal
#' @noRd
determine_eps_percentile <- function(gaps, percentile = 0.9) {
  
  # Use specified percentile of gaps
  percentile_eps <- stats::quantile(gaps, percentile, na.rm = TRUE)
  return(as.numeric(percentile_eps))
}

#' Fit 2-component Gaussian Mixture Model to gaps
#'
#' Helper function that fits a 2-component GMM to inter-visit gaps and returns
#' structured results including fitted parameters and metadata.
#'
#' @param gaps Numeric vector of inter-visit gaps
#' @param use_log_transform Logical indicating whether to log-transform gaps before fitting GMM
#' @param percentile_fallback Numeric value between 0 and 1 for percentile fallback if GMM fails
#' @param log_multiplier Numeric value for multiplier of log transformation
#' @param log_offset Numeric value for offset of log transformation
#'
#' @return List containing:
#'   \itemize{
#'     \item fit_successful: Logical indicating if GMM fitting was successful
#'     \item use_log_transform: Logical indicating if log transformation was used
#'     \item mu1, mu2: Means of the two components (in appropriate space)
#'     \item sigma1, sigma2: Standard deviations of the two components (in appropriate space)
#'     \item lambda1, lambda2: Mixing proportions of the two components
#'     \item intersection_point: Intersection point between the two distributions (in original scale)
#'     \item fallback_eps: Percentile-based fallback value
#'   }
#' @keywords internal
#' @noRd
fit_gmm_model <- function(gaps, use_log_transform = TRUE, log_multiplier = 20, log_offset = 1, percentile_fallback = 0.9) {
  
  # Initialize return structure
  result <- list(
    fit_successful = FALSE,
    use_log_transform = use_log_transform,
    mu1 = NA, mu2 = NA,
    sigma1 = NA, sigma2 = NA,
    lambda1 = NA, lambda2 = NA,
    intersection_point = NA,
    fallback_eps = stats::quantile(gaps, percentile_fallback, na.rm = TRUE)
  )
  
  if (length(gaps) < 10) {
    return(result)
  }
  
  # Try to fit 2-component Gaussian mixture model
  tryCatch({
    if (use_log_transform) {
      # Log-transform gaps (add small constant to avoid negative numbers)
      log_gaps <- log(log_multiplier*gaps + log_offset)

      # Fit 2-component normal mixture to log-transformed data with custom starting values
      captured_output <- utils::capture.output({
        mix_fit <- mixtools::normalmixEM(log_gaps, k = 2, verb = FALSE, 
                                        maxit = 2000, epsilon = 1e-08)
      }, type = "output")
      
      # Extract parameters for the two components (in log space)
      mu1_log <- mix_fit$mu[1]
      mu2_log <- mix_fit$mu[2]
      sigma1_log <- mix_fit$sigma[1]
      sigma2_log <- mix_fit$sigma[2]
      lambda1 <- mix_fit$lambda[1]
      lambda2 <- mix_fit$lambda[2]
      
      # Ensure component 1 is the within-meal (smaller mean in log space) distribution
      if (mu1_log > mu2_log) {
        # Swap components
        temp_mu <- mu1_log; mu1_log <- mu2_log; mu2_log <- temp_mu
        temp_sigma <- sigma1_log; sigma1_log <- sigma2_log; sigma2_log <- temp_sigma
        temp_lambda <- lambda1; lambda1 <- lambda2; lambda2 <- temp_lambda
      }
      
      # Store parameters
      result$mu1 <- mu1_log
      result$mu2 <- mu2_log
      result$sigma1 <- sigma1_log
      result$sigma2 <- sigma2_log
      result$lambda1 <- lambda1
      result$lambda2 <- lambda2
      
      # Find intersection point between the two distributions in log space
      intersection_log <- find_distribution_intersection(mu1_log, sigma1_log, lambda1, 
                                                        mu2_log, sigma2_log, lambda2)
      
      # return log-transformed intersection point
      result$intersection_point <- intersection_log
      
    } else {
      # Fit 2-component normal mixture to log-transformed data with custom starting values
      captured_output <- utils::capture.output({
        mix_fit <- mixtools::normalmixEM(gaps, k = 2, verb = FALSE, 
                                        maxit = 2000, epsilon = 1e-08)
      }, type = "output")
      
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
      
      # Store parameters
      result$mu1 <- mu1
      result$mu2 <- mu2
      result$sigma1 <- sigma1
      result$sigma2 <- sigma2
      result$lambda1 <- lambda1
      result$lambda2 <- lambda2
      
      # Find intersection point between the two distributions
      result$intersection_point <- find_distribution_intersection(mu1, sigma1, lambda1, 
                                                                 mu2, sigma2, lambda2)
    }
    
    result$fit_successful <- TRUE
    
  }, error = function(e) {
    # GMM fitting failed, result already has fit_successful = FALSE
  })
  
  return(result)
}

#' Fit Gaussian mixture model to gaps
#'
#' Fits a 2-component Gaussian mixture model to inter-visit gaps to determine
#' optimal eps for clustering. Falls back to percentile method if GMM fails.
#' Can optionally use log-transformation to better handle highly skewed distributions.
#'
#' @param gaps Numeric vector of inter-visit gaps
#' @param percentile_fallback Numeric value between 0 and 1 for percentile fallback
#' @param use_log_transform Logical indicating whether to log-transform gaps before fitting GMM
#' @param log_multiplier Numeric value for multiplier of log transformation
#' @param log_offset Numeric value for offset of log transformation
#'
#' @return Optimal eps value based on GMM intersection or percentile fallback
#' @keywords internal
#' @noRd
fit_gmm_to_gaps <- function(gaps, percentile_fallback = 0.9, use_log_transform = TRUE, log_multiplier = 20, log_offset = 1) {
  
  # Use helper function to fit GMM
  gmm_result <- fit_gmm_model(gaps, use_log_transform, log_multiplier, log_offset, percentile_fallback)
  
  if (gmm_result$fit_successful) {
    if (gmm_result$use_log_transform) {
      return((exp(gmm_result$intersection_point) - log_offset)/log_multiplier)
    } else {
      return(gmm_result$intersection_point)
    }
  } else {
    # Fall back to percentile method
    return(gmm_result$fallback_eps)
  }
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
