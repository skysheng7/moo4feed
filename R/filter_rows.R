# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Remove rows by matching values in a specified column
#'
#' Filters out any rows where the values in `col_name` appear in `to_delete`.
#'
#' @param df A data frame containing a column named `col_name`.
#' @param col_name A single string giving the name of the column to filter on.
#' @param to_delete A vector of values to remove; **must** have the same type
#'   data type as `df[[col_name]]`. Default is "cow".
#'
#' @return A data frame identical to `df` but with all rows where
#'   `df[[col_name]] %in% to_delete` dropped.
#'
#' @examples
#' df <- data.frame(
#'   cow         = c("A", "B", "C"),
#'   transponder = c("X1", "X2", "X3"),
#'   Value       = 1:3,
#'   stringsAsFactors = FALSE
#' )
#' # drop cows "A" and "C"
#' delete_rows(df, c("A", "C"), "cow")
#'
#' # drop transponder "X2"
#' delete_rows(df, c("X2"), "transponder")
#'
#' @export
delete_rows <- function(df, to_delete, col_name="cow") {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  if (!is.character(col_name) || length(col_name) != 1) {
    stop("`col_name` must be a single character string.")
  }
  if (!col_name %in% names(df)) {
    stop(sprintf("`df` must contain a column named '%s'.", col_name))
  }
  if (!is.vector(to_delete)) {
    stop("`to_delete` must be a vector.")
  }

  col_data <- df[[col_name]]

  # allow any combination of integer/double as numeric
  if (!(
    (is.numeric(col_data) && is.numeric(to_delete)) ||
    identical(typeof(col_data), typeof(to_delete))
  )) {
    stop(
      sprintf(
        "Type mismatch: column '%s' has data type '%s' but `to_delete` has data type '%s'.",
        col_name, typeof(col_data), typeof(to_delete)
      )
    )
  }

  df[!col_data %in% to_delete, , drop = FALSE]
}



#' Keep only rows for specified bins
#'
#' Filters to rows where the column recording bin IDs matches one of the values in `bins`.
#'
#' @param df A data frame containing a column of bin IDs.
#' @param bins A numeric vector of bin IDs to keep. You can supply individual
#'   values (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).
#' @param bin_col A single string naming the column of bin IDs (default: `"bin"`).
#'
#' @return A data frame identical to `df` but only with rows where
#'   `df[[bin_col]] %in% bins`.
#'
#' @examples
#' df <- data.frame(Bin = 1:5, Value = 11:15)
#' keep_bins(df, bins = 2:4, bin_col = "Bin")
#' #   Bin Value
#' # 2   2    12
#' # 3   3    13
#' # 4   4    14
#'
#' keep_bins(df, bins = c(1, 5), bin_col = "Bin")
#' #   Bin Value
#' # 1   1    11
#' # 5   5    15
#'
#' @export
keep_bins <- function(df, bins, bin_col = "bin") {
  # -- 1) df must be a data frame
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  # -- 2) bin_col must exist
  if (!is.character(bin_col) || length(bin_col) != 1) {
    stop("`bin_col` must be a single character string.")
  }
  if (!bin_col %in% names(df)) {
    stop(sprintf("`df` must contain a column named '%s'.", bin_col))
  }

  # -- 3) both df[[bin_col]] and bins must be numeric
  col_data <- df[[bin_col]]
  if (!is.numeric(col_data)) {
    stop(sprintf("Column '%s' must be numeric.", bin_col))
  }
  if (!is.numeric(bins)) {
    stop("`bins` must be a numeric vector (e.g. c(1,3) or 2:4).")
  }

  # -- 4) perform filtering, always return a data frame
  df[col_data %in% bins, , drop = FALSE]
}


#' Shift specified bin IDs by an offset
#'
#' Rename bin ID by adding an amount defined in `bin_offset` to each bin's
#' original ID  in `df[[bin_col]]`. Only rename bins that matches one of the
#' ID in the vector `bins`, preserving all other rows unchanged.
#'
#' We wish to do this because sometimes water bin and feed bin share the same
#' ID, and we want to differentiate water bins from feed bins.
#'
#' @param df A data frame containing a column of bin IDs.
#' @param bins A numeric vector of the ID of bins that we are interested in renaing.
#'   You can supply individual values (e.g. `c(2, 4, 6)`) or a sequence (e.g. `3:5`).
#' @param bin_offset A single numeric value to add to each matching bin ID.
#' @param bin_col A single string naming the column recording bin IDs (default: `"bin"`).
#'
#' @return A data frame identical to `df` but with `df[[bin_col]]` updated:
#'   for each row where `df[[bin_col]] %in% bins`, the new value is
#'   `old_value + bin_offset`.
#'
#' @examples
#' df <- data.frame(Bin = 1:5, Value = 10:14)
#' # shift bins 2,3,4 by +100 → become 102,103,104
#' rename_bins(df, bins = 2:4, bin_offset = 100, bin_col = "Bin")
#'
#' @export
rename_bins <- function(df, bins, bin_offset, bin_col = "bin") {
  # -- 1) df must be a data frame
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  # -- 2) bin_col must exist
  if (!is.character(bin_col) || length(bin_col) != 1) {
    stop("`bin_col` must be a single character string. `bin_col` is a single string naming the column recording bin IDs (default: `bin`)")
  }
  if (!bin_col %in% names(df)) {
    stop(sprintf("`df` must contain a column named '%s'.", bin_col))
  }

  # -- 3) df[[bin_col]] and bins must be numeric
  col_data <- df[[bin_col]]
  if (!is.numeric(col_data)) {
    stop(sprintf("Column '%s' must be numeric.", bin_col))
  }
  if (!is.numeric(bins)) {
    stop("`bins` must be a numeric vector (e.g. c(2,4) or 3:5).")
  }

  # -- 4) bin_offset must be a single numeric
  if (!is.numeric(bin_offset) || length(bin_offset) != 1) {
    stop("`bin_offset` must be a single numeric value.")
  }

  # -- 5) perform the shift
  mask <- col_data %in% bins
  df[[bin_col]][mask] <- df[[bin_col]][mask] + bin_offset

  return(df)
}


