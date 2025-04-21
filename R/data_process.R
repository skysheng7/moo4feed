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
#' @param file_names.f A character vector of feed file names.
#' @param file_names.w A character vector of water file names.
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
compare_files <- function(file_names.f, file_names.w) {
  # --- Normalize inputs: allow lists by unlisting ---
  if (is.list(file_names.f)) {
    file_names.f <- unlist(file_names.f, use.names = FALSE)
  }
  if (is.list(file_names.w)) {
    file_names.w <- unlist(file_names.w, use.names = FALSE)
  }

  # --- Error handling ---
  if (!is.character(file_names.f)) {
    stop("`file_names.f` must be a character vector.")
  }
  if (!is.character(file_names.w)) {
    stop("`file_names.w` must be a character vector.")
  }

  # --- Extract dates via helper ---
  feed_df  <- file_name_processing(file_names.f, "feed_name")
  water_df <- file_name_processing(file_names.w, "water_name")

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
