# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Safely Read Data from a File
#'
#' Attempts to read a delimited file (comma by default) into a data frame.
#' If the file does not exist, is a directory, is empty, or an error occurs
#' during reading, this function returns an empty data frame (0 rows, 0 columns).
#'
#' @param file A single string giving the path to the file.
#' @param sep  Field separator; passed to `read.table()`. Defaults to `","`.
#' @param header Logical; does the file have a header row? Defaults to `FALSE`.
#'
#' @return A data frame of the file’s contents, or an empty data frame if
#'   the file is missing, is a directory, is empty, or cannot be read.
#'
#' @examples
#' # create a small CSV and read it
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(data.frame(a = 1:3, b = 4:6), tmp, row.names = FALSE)
#' read_data_safely(tmp)
#' unlink(tmp)
#'
#' @export
read_data_safely <- function(file, sep = ",", header = FALSE) {
  # 1) Validate input
  if (!is.character(file) || length(file) != 1) {
    stop("`file` must be a single character string.")
  }

  # 2) File must exist, not be a directory, and have nonzero size
  info <- file.info(file)
  if (isTRUE(info$isdir)) {
    message("Warning: this path is to a folder instead of a file")
    return(data.frame())
  }
  if (is.na(info$size) || info$size == 0) {
    message(paste0("Warning: File missing, or the file is empty; returning an empty data frame: ", file, ". You can ignore this warning if an empty file is expected."))
    return(data.frame())
  }

  # 3) Try to read (suppress any low‑level warnings)
  out <- tryCatch({
    df <- suppressWarnings(
      utils::read.table(
        file,
        header          = header,
        sep             = sep,
        stringsAsFactors = FALSE
      )
    )
    # treat zero‐row frames as “empty”
    if (nrow(df) == 0L) {
      message(paste0("Warning: File has no data rows; returning an empty data frame, : ", file, ". You can ignore this warning if an empty file is expected."))
      return(data.frame())
    }
    df
  }, error = function(e) {
    message("Error reading '", file, "': ", e$message, " Returning empty data frame.")
    data.frame()
  })

  return(out)
}


