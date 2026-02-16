# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Combine Feeder and Water Data by Date
#'
#' Takes two lists of data frames (feeder and water) and combines them by row-binding
#' each corresponding pair. The combined data frames are returned in a new list, grouped by date.
#'
#' @param all_fed A list of data frames containing feeder visit data.
#' @param all_wat A list of data frames containing water visit data.
#'
#' @return A list of combined feed and water data frames, named by date.
#'
#' @examples
#' fed_list <- list(
#'   "2024-01-01" = data.frame(cow = 1:2, intake = c(10, 20)),
#'   "2024-01-02" = data.frame(cow = 3:4, intake = c(30, 40))
#' )
#' wat_list <- list(
#'   "2024-01-01" = data.frame(cow = 1:2, intake = c(5, 15)),
#'   "2024-01-02" = data.frame(cow = 3:4, intake = c(25, 35))
#' )
#'
#' combine_feed_water(fed_list, wat_list)
#'
#' @export
combine_feed_water <- function(all_fed, all_wat) {
  # 1) Validate inputs
  if (!is.list(all_fed) || !is.list(all_wat)) {
    stop("Both `all_fed` and `all_wat` must be lists of data frames.")
  }
  if (length(all_fed) != length(all_wat)) {
    stop("The lengths of `all_fed` and `all_wat` must be the same.")
  }
  if (!identical(names(all_fed), names(all_wat))) {
    stop("`all_fed` and `all_wat` must have identical names (each data frame is named by date) in the same order.")
  }

  # 2) Combine each corresponding pair
  out <- vector("list", length(all_fed))
  for (i in seq_along(all_fed)) {
    out[[i]] <- rbind(all_fed[[i]], all_wat[[i]])
    names(out)[i] <- names(all_fed)[i]
  }

  return(out)
}


#' Merge a List of Data Frames
#'
#' Takes a list of data frames and row-binds them into a single consolidated
#' master data frame.
#'
#' @param data_list A list of data frames to merge.
#'
#' @return A single merged data frame.
#'
#' @examples
#' data_list <- list(
#'   data.frame(cow = 1:2, feed = c(10, 20)),
#'   data.frame(cow = 3:4, feed = c(30, 40))
#' )
#'
#' merge_list_df(data_list)
#'
#' @export
merge_list_df <- function(data_list) {
  # 1) Validate input
  if (!is.list(data_list) || inherits(data_list, "data.frame")) {
    stop("`data_list` must be a list of data frames.")
  }
  if (length(data_list) == 0) {
    stop("`data_list` is empty; nothing to merge.")
  }

  # 2) Merge
  out <- do.call(rbind, data_list)

  return(out)
}
