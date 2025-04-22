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
                           select_cols = NULL,
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
                          select_cols = NULL,
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

#' Process a batch of feeder files
#'
#' @inherit process_all description details return seealso
#'
#' @inheritParams process_all
#'
#' @examples
#' \dontrun{
#' res <- process_all_feed(
#'   files       = c("path/to/file/VR210102.DAT", "path/to/file/VW210605.DAT"),
#'   col_names   = c("cow", "transponder", "bin", "start", "end"),
#'   bins        = 1:30,
#'   select_cols = c("cow", "bin", "start", "end"),
#'   sep         = ",",
#'   header      = TRUE,
#'   tz          = "UTC"
#' )
#' }
#'
#' @export
process_all_feed <- function(
    files,
    col_names,
    id_col      = "cow",
    drop_ids    = NULL,
    trans_col   = "transponder",
    drop_trans  = NULL,
    bin_col     = "bin",
    bins,
    select_cols = NULL,
    sep         = ",",
    header      = FALSE,
    daylight_change_duration = 60,
    tz          = Sys.timezone()
) {
  process_all(
    files    = files,
    file_type = "feed",
    col_names = col_names,
    id_col    = id_col,
    drop_ids  = drop_ids,
    trans_col = trans_col,
    drop_trans = drop_trans,
    bin_col   = bin_col,
    bins      = bins,
    select_cols = select_cols,
    sep       = sep,
    header    = header,
    daylight_change_duration = daylight_change_duration,
    tz        = tz
  )
}

#' Process a batch of water files
#'
#' @inherit process_all description details return seealso
#'
#' @inheritParams process_all
#'
#' @examples
#' \dontrun{
#' res <- process_all_water(
#'   files       = c("path/to/file/VW200306.DAT", "path/to/file/VW200307.DAT"),
#'   col_names   = c("cow", "transponder", "bin", "start", "end"),
#'   bins        = 1:5,
#'   select_cols = c("cow", "bin", "start", "end"),
#'   bin_offset  = 100,
#'   sep         = ",",
#'   header      = TRUE,
#'   tz          = "UTC"
#' )
#' }
#'
#'
#' @export
process_all_water <- function(
    files,
    col_names,
    id_col      = "cow",
    drop_ids    = NULL,
    trans_col   = "transponder",
    drop_trans  = NULL,
    bin_col     = "bin",
    bins,
    select_cols = NULL,
    bin_offset  = 100,
    sep         = ",",
    header      = FALSE,
    daylight_change_duration = 60,
    tz          = Sys.timezone()
) {
  process_all(
    files    = files,
    file_type = "water",
    col_names = col_names,
    id_col    = id_col,
    drop_ids  = drop_ids,
    trans_col = trans_col,
    drop_trans = drop_trans,
    bin_col   = bin_col,
    bins      = bins,
    select_cols = select_cols,
    bin_offset = bin_offset,
    sep       = sep,
    header    = header,
    daylight_change_duration = daylight_change_duration,
    tz        = tz
  )
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
#' @param select_cols  Character vector: columns to retain in the final output. Default is null, select all columns.
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
    select_cols = NULL,
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
  if (is.null(select_cols)) {
    select_cols = col_names
  }
  if (!is.character(select_cols)) {
    stop("`select_cols` must be a character vector or NULL")
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

#' Process multiple files with time adjustment
#'
#' @description
#' Reads, cleans, and time‑adjusts a batch of files, returning a named list
#' of data frames keyed by file date (YYYY‑MM‑DD).
#'
#' @inheritParams daylight_saving_adjust
#' @inheritParams process_water
#' @param files  Character vector of all the file paths.
#' @param file_type a character string indicating if this is feeder (i.e., "feed") data or water ("water") data. Default is "feed"
#'
#' @return
#' A **named** list of data frames, one per input file, named by date (YYYY‑MM‑DD).
#'
#' @details
#' Steps:
#' 1. **Validate** all inputs (types, `file_type` ∈ `"feed","water"`).
#' 2. **Extract** a date information from each filename and parse it via `lubridate::ymd()`.
#' 3. **Fetch** Daylight Saving Time (DST) switch table for the relevant years (`dst_switch_day(years, tz)`).
#' 4. **Loop** over each file:
#'    4.1. Call either `process_feeder()` or `process_water()` to do:
#'          4.1.1. Safely read the CSV / DAT file
#'          4.1.2. Rename columns
#'          4.1.3. Drop unwanted cows & transponders
#'          4.1.4. Keep only specified bins
#'          4.1.5. Subset to desired columns
#'    4.2. Drop any rows with `NA`.
#'    4.3. Call `daylight_saving_adjust()` to adjust timestamps for daylight saving change days.
#'    4.4. Standardize the columns recording start and end time of each event to be in the format of "yyyy-mm-dd hh:mm:ss".
#'    4.5. Store processed dataframe the output list and name it by the date.
#'
#' @seealso
#'  - \code{\link{process_feeder}}
#'  - \code{\link{process_water}}
#'  - \code{\link{file_name_processing}}
#'  - \code{\link{dst_switch_day}}
#'  - \code{\link{daylight_saving_adjust}}
process_all <- function(
    files,
    file_type   = "feed",
    col_names,
    id_col      = "cow",
    drop_ids    = NULL,
    trans_col   = "transponder",
    start_col   = "start",
    end_col     = "end",
    drop_trans  = NULL,
    bin_col     = "bin",
    bins,
    select_cols = NULL,
    bin_offset  = 100,
    sep         = ",",
    header      = FALSE,
    daylight_change_duration=60,
    tz = Sys.timezone()
) {
  # ---- input validation ----
  if (!is.character(files) || length(files) < 1) {
    stop("`files` must be a nonempty character vector of file paths.")
  }
  file_type <- match.arg(trimws(tolower(file_type)), c("feed","water"))
  if (!is.character(col_names)) {
    stop("`col_names` must be a character vector that matches either `feed` or `water`.")
  }

  # 2) Extract dates + build DST lookup -------------------------------------
  date_meta <- file_name_processing(files, col_name = "path")
  date_meta$date <- lubridate::ymd(date_meta$date, tz = tz)
  years         <- unique(lubridate::year(date_meta$date))
  dst_df        <- dst_switch_day(years = years, tz = tz)

  # 3) Pre‑allocate output --------------------------------------------------
  out <- vector("list", length(files))
  names(out) <- rep(NA_character_, length(files))

  # 4) Loop over each file --------------------------------------------------
  for (i in seq_along(files)) {
    path <- date_meta$path[i]
    date <- as.character(date_meta$date[i])

    # 4.1) Single‑file processing
    if (file_type == "feed") {
      df <- process_feeder(
        file        = path,
        col_names   = col_names,
        id_col      = id_col,
        drop_ids    = drop_ids,
        trans_col   = trans_col,
        drop_trans  = drop_trans,
        bin_col     = bin_col,
        bins        = bins,
        select_cols = select_cols,
        sep         = sep,
        header      = header
      )
    } else {  # water
      df <- process_water(
        file        = path,
        col_names   = col_names,
        id_col      = id_col,
        drop_ids    = drop_ids,
        trans_col   = trans_col,
        drop_trans  = drop_trans,
        bin_col     = bin_col,
        bins        = bins,
        select_cols = select_cols,
        bin_offset  = bin_offset,
        sep         = sep,
        header      = header
      )
    }

    # 4.2) Drop NAs
    df <- stats::na.omit(df)

    # 4.3) Trim raw times and apply DST adjustment
    if (nrow(df) > 0L) {
      df[[start_col]] <- trimws(df[[start_col]])
      df[[end_col]]   <- trimws(df[[end_col]])

      df <- daylight_saving_adjust(
        data_frame               = df,
        date                     = date,
        start_col                = start_col,
        end_col                  = end_col,
        dst_df                   = dst_df,
        daylight_change_duration = daylight_change_duration,
        tz                       = tz
      )

      df[[start_col]] <- lubridate::ymd_hms(paste(date, df[[start_col]]), tz = tz)
      df[[end_col]]   <- lubridate::ymd_hms(paste(date, df[[end_col]]),   tz = tz)
    }

    # 4.4) Store and name by date
    out[[i]]      <- df
    names(out)[i] <- date
  }

  return(out)
}


