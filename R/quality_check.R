# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Run full quality-control check on feeder and drinker data
#'
#' @param feed  A list of daily **feed** data frames named by date, or `NULL`
#'  if you don't have feeder data
#' @param water A list of daily **water** data frames named by date, or `NULL`
#'  if you don't have water data
#' @param cfg   A configuration list created by [qc_config()].
#' @param tz    Time-zone for all analysis related to timestamps.  Defaults to [tz2()].
#' @param verbose Logical. If TRUE, print details of data where errors were detected
#'
#' @return A list with four elements:
#' \describe{
#'   \item{`warnings`}{a tidy data frame with one row per day and one column
#'                    per warning code.}
#'   \item{`feed`}{cleaned feed list (or `NULL`).}
#'   \item{`water`}{cleaned water list (or `NULL`).}
#'   \item{`combined`}{merged feed + water list (after all fixes).}
#' }
#'
#' @examples
#' cfg <- qc_config(high_dur_feed = 2500, low_visit_threshold = 5)
#' out <- qc(feed = all_fed, water = all_wat, cfg = cfg)
#' out$warnings
#' @export
qc <- function(feed      = NULL,
               water     = NULL,
               cfg       = qc_config(),
               tz        = tz2(),
               verbose = TRUE) {

  # --- 0. combine ----------------------------------------------------------
  if (!is.null(feed) && !is.null(water)) {
    comb <- combine_feed_water(feed, water)
  } else if (!is.null(feed)) {
    comb <- feed
  } else if (!is.null(water)) {
    comb <- water
  } else {
    stop("`feed` and `water` can't all be NULL. One of them needs to be a list of dataframe.")
  }

  # --- 1. prepare warning skeleton ----------------------------------------
  warn  <- qc_warning_skeleton(comb, tz = tz,
                               has_feed  = !is.null(feed),
                               has_water = !is.null(water))

  # --- 2. run QC modules ---------------------------------------------------
  warn <- qc_total_cows(comb, warn, cfg)
  #warn <- qc_double_detection(comb, warn, verbose = verbose)
  #warn <- qc_negatives(comb, warn, verbose = verbose, cfg)


  # --- 4. return -----------------------------------------------------------
  list(warnings = warn,
       feed     = feed,
       water    = water,
       combined = comb)
}








