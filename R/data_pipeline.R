# -----------------------------------------------------------------------------#
# ------------------ External user-facing functions ---------------------------#
# -----------------------------------------------------------------------------#


#' Process feeder data
#'
#' Reads, cleans, and filters a feeder file:
#' 1. Safely read the CSV / DAT file
#' 2. Rename columns
#' 3. Drop unwanted cows & transponders
#' 4. Keep only specified bins
#' 5. Subset to desired columns
#'
#' @inheritParams process_data_generic
#'
#' @return A cleaned feeder data frame.
#'
#' @examples
#' \dontrun{
#' process_feeder(
#'   file        = "path/to/my/file/feed.csv",
#'   col_names   = c("cow", "transponder", "bin", "value"),
#'   id_col      = "cow"
#'   drop_ids    = c("A", "C"),
#'   trans_col   = "transponder",
#'   drop_trans = c("0000"),
#'   bin_col     = "bin"
#'   bins        = 1:30,
#'   select_cols = c("cow", "bin", "value"),
#'   sep         = ","
#'   header      = TRUE
#' )
#'}
#' @export
process_feeder <- function(file,
                           col_names,
                           id_col      = "cow",
                           drop_ids    = NULL,
                           trans_col   = "transponder",
                           drop_trans  = NULL,
                           bin_col     = "bin",
                           bins,
                           select_cols,
                           sep         = ",",
                           header      = FALSE) {
  process_data_generic(
    file         = file,
    col_names    = col_names,
    id_col       = id_col,
    drop_ids     = drop_ids,
    trans_col    = trans_col,
    drop_trans   = drop_trans,
    bin_col      = bin_col,
    bins         = bins,
    select_cols  = select_cols,
    sep          = sep,
    header       = header
  )
}


#' Process water data
#'
#' Reads, cleans, filters, and offsets a water file:
#' 1. Safely read the CSV
#' 2. Rename columns
#' 3. Drop unwanted cows & transponders
#' 4. Keep only specified bins
#' 5. Subset to desired columns
#' 6. Add an offset to bin IDs, so that water bin ID differs from feed bin ID
#'
#' @inheritParams process_data_generic
#' @param bin_offset  Single numeric value to add to each kept bin ID (default: `100`).
#'
#' @return A cleaned water data frame with shifted bin IDs.
#'
#' @examples
#' \dontrun{
#' process_water(
#'   file        = "path/to/my/file/water.csv",
#'   col_names   = c("cow", "transponder", "bin", "value"),
#'   id_col      = "cow"
#'   drop_ids    = c("A", "C"),
#'   trans_col   = "transponder",
#'   drop_trans  = c("0000"),
#'   bin_col     = "bin"
#'   bins        = 1:5,
#'   select_cols = c("cow", "bin", "value")
#'   bin_offset  = 100,
#'   sep         = ","
#'   header      = TRUE
#' )
#' }
#'
#' @export
process_water <- function(file,
                          col_names,
                          id_col      = "cow",
                          drop_ids    = NULL,
                          trans_col   = "transponder",
                          drop_trans  = NULL,
                          bin_col     = "bin",
                          bins,
                          select_cols,
                          bin_offset  = 100,
                          sep         = ",",
                          header      = FALSE) {

  out <- process_data_generic(
    file         = file,
    col_names    = col_names,
    id_col       = id_col,
    drop_ids     = drop_ids,
    trans_col    = trans_col,
    drop_trans   = drop_trans,
    bin_col      = bin_col,
    bins         = bins,
    select_cols  = select_cols,
    sep          = sep,
    header       = header
  )

  return(rename_bins(out, bins = bins, bin_offset = bin_offset, bin_col = bin_col))
}





# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Generic data‐processing pipeline
#'
#' @description
#' Internal helper that:
#' 1. Safely reads a file
#' 2. Renames its columns
#' 3. Drops rows by animal ID and/or transponder
#' 4. Keeps only specified bins
#' 5. Subsets to desired columns
#'
#' @inheritParams read_data_safely
#' @param col_names    Character vector of names to assign to the raw data columns.
#' @param id_col       Single string: column name for animal IDs (default: `"cow"`).
#' @param drop_ids     Vector: values in `id_col` to remove (default: `NULL`).
#' @param trans_col    Single string: column name for transponder IDs (default: `"transponder"`).
#' @param drop_trans   Vector: values in `trans_col` to remove (default: `NULL`).
#' @param bin_col      Single string: column name for bin IDs (default: `"bin"`).
#' @param bins         Numeric vector: bin IDs to keep (e.g. `2:4` or `c(1,5)`).
#' @param select_cols  Character vector: columns to retain in the final output.
#'
#' @return A processed data frame.
process_data_generic <- function(
    file,
    col_names,
    id_col      = "cow",
    drop_ids    = NULL,
    trans_col   = "transponder",
    drop_trans  = NULL,
    bin_col     = "bin",
    bins,
    select_cols,
    sep         = ",",
    header      = FALSE
) {

  # ------ error handling ---------#
  if (!is.character(file) || length(file) != 1) {
    stop("`file` must be a single character string path.")
  }
  if (!is.character(col_names)) {
    stop("`col_names` must be a character vector.")
  }
  if (!is.character(id_col) || length(id_col) != 1) {
    stop("`id_col` must be a single character string.")
  }
  if (!is.character(trans_col) || length(trans_col) != 1) {
    stop("`trans_col` must be a single character string.")
  }
  if (!is.character(bin_col) || length(bin_col) != 1) {
    stop("`bin_col` must be a single character string.")
  }
  if (!is.numeric(bins)) {
    stop("`bins` must be a numeric vector.")
  }
  if (!is.character(select_cols)) {
    stop("`select_cols` must be a character vector.")
  }
  if (!all(select_cols %in% col_names)) {
    stop("the `select_cols` vector contains columns that do not exist in this dataframe")
  }

  # ------ main logic ---------#
  df <- read_data_safely(file, sep = sep, header = header)
  if (nrow(df) == 0L) return(df)

  # 1) assign column names
  colnames(df) <- col_names

  # 2) drop unwanted IDs
  if (!is.null(drop_ids))   df <- delete_rows(df, drop_ids,   id_col)
  if (!is.null(drop_trans)) df <- delete_rows(df, drop_trans, trans_col)

  # 3) keep only selected bins
  df <- keep_bins(df, bins = bins, bin_col = bin_col)

  # 4) subset to desired columns
  df <- df[, select_cols, drop = FALSE]

  return(df)
}


