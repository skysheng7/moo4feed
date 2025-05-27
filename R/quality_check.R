# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Run full quality-control check on feeder and drinker data
#'
#' @inheritParams set_global_cols
#' @inheritParams process_all_feed
#' @param feed  A list of daily **feed** data frames named by date, or `NULL`
#'  if you don't have feeder data
#' @param water A list of daily **water** data frames named by date, or `NULL`
#'  if you don't have water data
#' @param cfg   A configuration list created by [qc_config()].
#' @param verbose Logical. If TRUE, print details of data where errors were detected
#' @param tz Time zone string for date-time operations
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
#' cfg <- qc_config(high_dur_feed = 2500, low_visit_threshold = 5, total_cows_expected=48)
#' out <- qc(feed = all_fed, water = all_wat, cfg = cfg, verbose = FALSE)
#' out$warnings
#' @export
qc <- function(feed      = NULL,
               water     = NULL,
               cfg       = qc_config(),
               id_col    = id_col2(),
               start_col = start_col2(),
               end_col   = end_col2(),
               bin_col   = bin_col2(),
               dur_col = duration_col2(),
               intake_col = intake_col2(),
               start_weight_col = start_weight_col2(),
               end_weight_col = end_weight_col2(),
               tz = tz2(),
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
  warn  <- qc_warning_skeleton(comb,
                               has_feed  = !is.null(feed),
                               has_water = !is.null(water))

  # --- 2. run QC modules ---------------------------------------------------
  warn <- qc_total_cows(comb, warn, cfg = cfg, id_col = id_col)

  warn <- qc_double_detection(comb,
                              warn=warn,
                              verbose = verbose,
                              id_col = id_col,
                              start_col = start_col,
                              end_col   = end_col,
                              bin_col   = bin_col)

  warn <- qc_negatives(comb,
                       warn=warn,
                       verbose = verbose,
                       cfg = cfg,
                       bin_col = bin_col,
                       dur_col = dur_col,
                       intake_col = intake_col)

  warn <- qc_all_large_intakes(feed = feed,
                              water = water,
                              warn = warn,
                              cfg = cfg,
                              verbose = verbose,
                              bin_col = bin_col,
                              intake_col = intake_col,
                              dur_col = dur_col)

  warn <- qc_no_show(comb,
                     warn = warn,
                     id_col = id_col,
                     end_col = end_col,
                     tz = tz,
                     verbose = verbose)

  # --- 3. delete negatives -------------------------------------------------
  comb <- qc_delete_negatives(comb,
                          dur_col = dur_col,
                          intake_col = intake_col,
                          start_weight_col = start_weight_col,
                          end_weight_col = end_weight_col)
  if (!is.null(feed)) {
    feed <- qc_delete_negatives(feed,
                            dur_col = dur_col,
                            intake_col = intake_col,
                            start_weight_col = start_weight_col,
                            end_weight_col = end_weight_col)
  } 

  if (!is.null(water)) {
    water <- qc_delete_negatives(water,
                            dur_col = dur_col,
                            intake_col = intake_col,
                            start_weight_col = start_weight_col,
                            end_weight_col = end_weight_col)
  } 

  # --- 4. return -----------------------------------------------------------
  list(warnings = warn,
       feed     = feed,
       water    = water,
       combined = comb)
}








