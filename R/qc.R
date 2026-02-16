# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Run full quality-control check on feeder and drinker data
#'
#' @description
#' This function performs comprehensive quality control checks on feeder and drinker data,
#' identifying and handling various data quality issues.
#' 
#' @details 
#' 1. Combines feed and water data (if both are provided)
#' 2. Checks for expected number of cows and reports missing animals
#' 3. Detects and optionally fixes double detections (when the same cow is detected at different bins at the same time)
#' 4. Flags negative values in duration, intake, or weight measurements
#' 5. Removes records with negative durations or intakes
#' 6. Identifies abnormally large feed/water intakes based on thresholds
#' 7. Flags cows that did not feed or drink after noon (potentially lost ear tag or due to illness)
#' 8. Flgas bins with very low traffic or no visits at all
#' 
#' Use this function after initial data processing with `process_all_feed()` and 
#' `process_all_water()` to clean your data and prepare it for analysis.
#' Customize thresholds using `qc_config()` to match your specific study parameters.
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
#' @param fix_double_detections Logical. If TRUE, applies corrections to double detection issues
#'   by adjusting end times of overlapping bouts. Default is TRUE.
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
               bins_feed = bins_feed2(),
               bins_wat = bins_wat2(),
               bin_offset = bin_offset2(),
               verbose = TRUE,
               fix_double_detections = TRUE) {

  # --- 0. combine ----------------------------------------------------------
  if (!is.null(feed) && !is.null(water)) {
    comb <- combine_feed_water(feed, water)
    all_bins <- c(bins_feed, bin_offset + bins_wat)
  } else if (!is.null(feed)) {
    comb <- feed
    all_bins <- bins_feed
  } else if (!is.null(water)) {
    comb <- water
    all_bins <- bin_offset + bins_wat
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

  warn <- qc_bin_visits(comb,
                        warn = warn,
                        cfg = cfg,
                        id_col = id_col,
                        bin_col = bin_col,
                        all_bins = all_bins,
                        verbose = verbose)

  # --- 3. Fix double detections if requested ------------------------------
  if (fix_double_detections) {
    fixed_data <- handle_all_double_detections(
      feed = feed,
      water = water,
      id_col = id_col,
      bin_col = bin_col,
      start_col = start_col,
      end_col = end_col,
      dur_col = dur_col,
      bin_offset = bin_offset
    )
    
    feed <- fixed_data$feed
    water <- fixed_data$water
    comb <- fixed_data$combined
  }

  # --- 4. delete negatives -------------------------------------------------
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

  # --- 5. return -----------------------------------------------------------
  list(warnings = warn,
       feed     = feed,
       water    = water,
       combined = comb)
}








