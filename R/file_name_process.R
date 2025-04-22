# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Process file names and extract date tokens
#'
#' Extracts date tokens from file names. The date is assumed to be embedded as digits
#' in the filename in the format of yymmdd (year, month, day). The code will
#' automatically extract the continuous digits out of the filename
#' (e.g. `"VR200715"` → `"200715"`; `"rmdh20250101"` → `"rmdh20250101"`).
#'
#' @param file_names A character vector of file names.
#' @param col_name A single character string specifying the column name for the
#'   file names in the output data frame. Default is "dir".
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{<col_name>}{Original cleaned file names (underscores instead of "/", " " or ".").}
#'   \item{date}{Extracted date tokens as character strings (e.g. `"200715"`), or `NA` if
#'     extraction failed.}
#' }
#' Returns an empty data frame with those two columns if `file_names` is empty.
#'
#' @examples
#' file_name_processing(
#'  file_names = c(" feed/VR200715.DAT ", "feed/VR200716.DAT"),
#'  col_name = "Feed_dir"
#' )
#'
#' @export
file_name_processing <- function(file_names, col_name = "dir") {
  # --- Error handling ---
  if (!is.character(file_names)) {
    stop("`file_names` must be a character vector.")
  }
  if (!is.character(col_name) || length(col_name) != 1) {
    stop("`col_name` must be a single character string.")
  }

  # --- Handle empty input ---
  if (length(file_names) == 0L) {
    df <- data.frame(stringsAsFactors = FALSE)
    df[[col_name]] <- character(0)
    df[["date"]] <- character(0)
    return(df)
  }

  # --- Main processing ---
  # Step 1: Trim and replace letters, slashes, spaces, and dots with "_"
  clean_names <- gsub("[a-zA-Z/ .]+", "_", trimws(basename(file_names)), perl = TRUE)

  # Step 2: Extract digits using regex
  match_locs <- regexpr("\\d+", clean_names)
  dates <- regmatches(clean_names, match_locs)

  # Step 3: Replace "" (from no match) with NA
  dates[match_locs == -1L] <- NA_character_

  # --- Build output ---
  df <- data.frame(
    temp = trimws(file_names),
    date = dates,
    stringsAsFactors = FALSE
  )
  colnames(df) <- c(col_name, "date")

  return(df)
}


#' Compare feed and water file names by shared dates
#'
#' Identifies the subset of feed and water file names that share the same
#' extracted dates, returning only those with matching date tokens.
#'
#' @param file_names_feed A character vector of feed file names.
#' @param file_names_water A character vector of water file names.
#'
#' @return A list with components:
#' \describe{
#'   \item{feed}{Character vector of feed file names that have matching water data.}
#'   \item{water}{Character vector of water file names that have matching feed data.}
#' }
#' If no common dates are found, both components are empty character vectors.
#'
#' @examples
#' compare_files(
#'   c("feed/VR200715.DAT", "feed/VR200716.DAT"),
#'   c("water/VW200715.DAT", "water/VW200717.DAT")
#' )
#' # returned list:
#' # feed = c("feed/VR200715.DAT")
#' # water = c("water/VW200715.DAT")
#'
#' @export
compare_files <- function(file_names_feed, file_names_water) {
  # --- Normalize inputs: allow lists by unlisting ---
  if (is.list(file_names_feed)) {
    file_names_feed <- unlist(file_names_feed, use.names = FALSE)
  }
  if (is.list(file_names_water)) {
    file_names_water <- unlist(file_names_water, use.names = FALSE)
  }

  # --- Error handling ---
  if (!is.character(file_names_feed)) {
    stop("`file_names_feed` must be a character vector.")
  }
  if (!is.character(file_names_water)) {
    stop("`file_names_water` must be a character vector.")
  }

  # --- Extract dates via helper ---
  feed_df  <- file_name_processing(file_names_feed, "feed_name")
  water_df <- file_name_processing(file_names_water, "water_name")

  # --- Find intersection of dates ---
  common_dates <- base::intersect(feed_df$date, water_df$date)
  if (length(common_dates) == 0L) {
    return(list(feed  = character(0),
                water = character(0)))
  }

  # --- Filter original names by common dates ---
  feed_matches  <- feed_df$feed_name[ feed_df$date  %in% common_dates ]
  water_matches <- water_df$water_name[ water_df$date %in% common_dates ]

  return(list(feed  = feed_matches,
              water = water_matches))
}


#' Get the overall date range for a set of files
#'
#' Processes a character vector of file names, extracts embedded date tokens
#' (via [file_name_processing()]), and computes the full date span
#' using [get_date_range_helper()].
#'
#' @inheritParams file_name_processing
#'
#' @return A single character string:
#' \describe{
#'   \item{`"YYYY-MM-DD"`}{when all dates in `df$date` are the same}
#'   \item{`"YYYY-MM-DD_YYYY-MM-DD"`}{otherwise, concatenation of start and end dates}
#' }
#' Returns `NA_character_` (with a warning) if `df` has zero rows.
#'
#' @examples
#' get_date_range(c("feed/VR200720.DAT", "feed/VR200715.DAT", "feed/VR200716.DAT"))
#' # returns:
#' # "200715_200720"
#'
#' @export
get_date_range <- function(file_names) {
  df <- file_name_processing(file_names)
  return(get_date_range_helper(df))
}



# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Get date range string for dataset
#'
#' Computes the overall date range from the `date` column of the input data frame,
#' returning it as a single string. If all dates are identical, returns that single date.
#'
#' @param df A data frame with a `date` column, it can be of different data types like string or Date, but must be in consistent format (e.g., "yymmdd", ""yyyy-mm-dd").
#'
#' @return A single character string:
#' \describe{
#'   \item{`"YYYY-MM-DD"`}{when all dates in `df$date` are the same}
#'   \item{`"YYYY-MM-DD_YYYY-MM-DD"`}{otherwise, concatenation of start and end dates}
#' }
#' Returns `NA_character_` (with a warning) if `df` has zero rows.
#'
#' @details
#' - Throws an error if `df` is not a data frame or lacks a `date` column.\cr
get_date_range_helper <- function(df) {
  # --- Error handling ---
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  if (!"date" %in% names(df)) {
    stop("`df` must contain a `date` column.")
  }

  # --- Handle zero-row case ---
  if (nrow(df) == 0L) {
    warning("`df` has no rows; returning NA_character_.")
    return(NA_character_)
  }

  if (all(is.na(df$date))) {
    warning("all values in `date` column is NA")
    return(NA_character_)
  }

  # --- Prepare dates ---
  dates <- if (inherits(df$date, "POSIXt")) {
    as.Date(df$date)
  } else {
    df$date
  }

  # --- Compute start and end ---
  start <- min(dates, na.rm = TRUE)
  end   <- max(dates, na.rm = TRUE)

  # --- Return only the date_range string ---
  if (identical(start, end)) {
    return(as.character(start))
  } else {
    return(paste0(as.character(start), "_", as.character(end)))
  }
}


