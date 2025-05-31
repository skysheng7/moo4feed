# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#


#' Build a configuration list for data quality-control
#'
#' This helper centralises **every threshold** used by [qc()].
#' It lets you define what should be flagged as *abnormal* in your
#' feed–water data: extra-long bouts, unrealistically large intakes, bins with
#' too few visits, etc.  Each argument has a sensible default, so you normally
#' override only the handful you care about—then pass the resulting list to
#' `qc(cfg = qc_config(...))`.
#'
#' @param high_dur_feed  Numeric. Seconds that define a *long* feeding visit.
#' @param high_dur_water Numeric. Seconds that define a *long* drinking visit.
#' @param large_intake_visit_feed  Numeric. Kilograms that flag a single, unusually
#'   large **feed** intake event.
#' @param large_intake_visit_water Numeric. Litres that flag a single, unusually
#'   large **water** intake event.
#' @param large_intake_rate_feed  Numeric. Kilograms/s considered a
#'   rapid feed-intake rate.
#' @param large_intake_rate_water Numeric. Litres/s considered a
#'   rapid water-intake rate.
#' @param low_visit_threshold Integer. Bins visited fewer than this number of
#'   times per day are flagged as *low traffic*.
#' @param total_cows_expected Integer. Expected herd size; if `NA` (default) the
#'   pipeline counts unique IDs automatically.
#' @param low_feed_intake  Numeric. kg/day.  Daily feed intake flagged as *low*
#'  for a animal (default **35**).
#' @param high_feed_intake Numeric. kg/day. Daily feed intake flagged as *high*
#'  for an animal (default **75**).
#' @param low_wat_intake   Numeric. L/day. Daily water intake flagged as *low*
#'  for an animal (default **60**).
#' @param high_wat_intake  Numeric. L/day. Daily water intake flagged as *high*
#'  for an animal (default **180**).
#' @param replacement_threshold Numeric. Seconds. Time gap to classify replacement
#'   behaviour (default **26 s**).
#' @param calibration_error Numeric. Allowed feeder calibration error (default **0.5** kg/L).
#' @param ... Reserved for future or project-specific tweaks. Named elements
#'   here are appended to the returned list.
#'
#' @return A named list consumed by [qc()] and its internal `qc_*()`
#'   helpers.
#'
#' @examples
#' # Use all defaults
#' cfg <- qc_config()
#' cfg
#'
#' # Tighten the "long feeding visit" threshold
#' cfg2 <- qc_config(high_dur_feed = 1800)
#' cfg2
#'
#' @export
qc_config <- function(
    high_dur_feed           = 2000,
    high_dur_water          = 1800,
    large_intake_visit_feed       = 8,     # kg per bout
    large_intake_visit_water      = 30,     # L per bout
    large_intake_rate_feed  = 0.008,  # kg/s
    large_intake_rate_water = 0.35,  # L/s
    low_visit_threshold     = 10,
    total_cows_expected     = NA,
    low_feed_intake         = 35,
    high_feed_intake        = 75,
    low_wat_intake          = 60,
    high_wat_intake         = 180,
    replacement_threshold   = 26,
    calibration_error       = 0.5,
    ...
) {

  ## ------------------------------------------------------------------------ ##
  ## Validate inputs                                                          ##
  ## ------------------------------------------------------------------------ ##
  assert_scalar_num(high_dur_feed,           "high_dur_feed")
  assert_scalar_num(high_dur_water,          "high_dur_water")
  assert_scalar_num(large_intake_visit_feed,     "large_intake_visit_feed")
  assert_scalar_num(large_intake_visit_water,    "large_intake_visit_water")
  assert_scalar_num(large_intake_rate_feed,  "large_intake_rate_feed")
  assert_scalar_num(large_intake_rate_water, "large_intake_rate_water")
  assert_scalar_int(low_visit_threshold,     "low_visit_threshold")
  assert_scalar_int(total_cows_expected,     "total_cows_expected", allow_na = TRUE)
  assert_scalar_num(low_feed_intake,         "low_feed_intake")
  assert_scalar_num(high_feed_intake,        "high_feed_intake")
  assert_scalar_num(low_wat_intake,          "low_wat_intake")
  assert_scalar_num(high_wat_intake,         "high_wat_intake")
  assert_scalar_num(replacement_threshold,   "replacement_threshold")
  assert_scalar_num(calibration_error,       "calibration_error", positive = TRUE)

  if (low_feed_intake >= high_feed_intake)
    stop("`low_feed_intake` must be smaller than `high_feed_intake`.", call. = FALSE)

  if (low_wat_intake >= high_wat_intake)
    stop("`low_wat_intake` must be smaller than `high_wat_intake`.", call. = FALSE)

  ## ------------------------------------------------------------------------ ##
  ## Build configuration list                                                 ##
  ## ------------------------------------------------------------------------ ##
  c(
    list(
      high_dur_feed           = high_dur_feed,
      high_dur_water          = high_dur_water,
      large_intake_visit_feed       = large_intake_visit_feed,
      large_intake_visit_water      = large_intake_visit_water,
      large_intake_rate_feed  = large_intake_rate_feed,
      large_intake_rate_water = large_intake_rate_water,
      low_visit_threshold     = low_visit_threshold,
      total_cows_expected     = total_cows_expected,
      low_feed_intake         = low_feed_intake,
      high_feed_intake        = high_feed_intake,
      low_wat_intake          = low_wat_intake,
      high_wat_intake         = high_wat_intake,
      replacement_threshold   = replacement_threshold,
      calibration_error       = calibration_error
    ),
    list(...)
  )
}


# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

assert_scalar_num <- function(x, name, allow_na = FALSE, positive = TRUE) {
  if (allow_na && is.na(x)) return(invisible())
  if (!is.numeric(x) || length(x) != 1L || (positive && x < 0) || is.na(x)) {
    stop(sprintf("`%s` must be a %s numeric scalar%s.",
                 name,
                 if (positive) "positive" else "",
                 if (allow_na) " (or NA)" else ""),
         call. = FALSE)
  }
}

assert_int_vec <- function(v, name) {
  if (!is.numeric(v) || any(is.na(v))) {
    stop(sprintf("`%s` must be an integer vector with no NAs.", name), call. = FALSE)
  }
}

assert_scalar_int <- function(x, name, allow_na = FALSE, positive = TRUE) {
  assert_scalar_num(x, name, allow_na, positive)
  if (allow_na && is.na(x)) return(invisible())
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    stop(sprintf("`%s` must be an integer.", name), call. = FALSE)
  }
}

#' Create an empty warnings dataframe with one row per day
#'
#' @inheritParams qc
#' @param comb      List of daily data frames (feed, water or combined).
#' @param has_feed  Logical.  Whether *feed*‑specific columns are needed.
#' @param has_water Logical.  Whether *water*‑specific columns are needed.
#'
#' @return A dataframe with pre‑defined character columns ready for QC modules to
#'   populate.
#' @keywords internal
#' @noRd
qc_warning_skeleton <- function(comb,
                                has_feed  = TRUE,
                                has_water = TRUE) {
  # Ensure the input list is not empty
  if (length(comb) == 0) {
    stop("The input list is empty!")
  }

  # Get date list
  date_list <- names(comb)

  # Create the initial data frame
  warn_df <- tibble::tibble(date = date_list)

  # Adding additional columns with default values (blank)
  general_columns <- c(
    "total_cows", "missing_cow", "double_detection_bins",
    "negative_visit_bins", "cows_disappeared_after_noon",
    "bins_never_visited", "bins_low_traffic"
  )

  feed_columns <- c(
    "long_dur_feeder", "large_intake_feed_visit",
    "low_daily_feed_intake_cows",
    "high_daily_feed_intake_cows",
    "feed_add_time_no_found"
  )

  wat_columns <- c(
    "long_dur_drinker", "large_intake_water_visit",
    "low_daily_water_intake_cows",
    "high_daily_water_intake_cows"
  )

  if (has_feed && has_water) {
    warning_columns <- c(general_columns, feed_columns, wat_columns)
  } else if (has_feed) {
    warning_columns <- c(general_columns, feed_columns)
  } else if (has_water) {
    warning_columns <- c(general_columns, wat_columns)
  }

  # Add warning columns to the data frame
  num_cols <- c("total_cows")
  char_cols <- setdiff(warning_columns, num_cols)

  for (col in warning_columns) {
    warn_df[[col]] <- if (col %in% num_cols) NA_integer_ else NA_character_
  }

  return(warn_df)
}
