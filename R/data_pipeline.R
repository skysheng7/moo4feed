# -----------------------------------------------------------------------------#
# ------------------ External user-facing functions ---------------------------#
# -----------------------------------------------------------------------------#


#' Process 1 feeder data file
#'
#' Reads, cleans, and filters a feeder file:
#' 1. Safely read the CSV / DAT file
#' 2. Rename columns
#' 3. Drop unwanted cows & transponders
#' 4. Keep only specified bins
#' 5. Subset to desired columns
#'
#' @inheritParams read_data_safely
#' @param col_names
#'   A character vector of column names to assign when `header = FALSE`.
#'   This vector must match the number of columns in the raw data.
#'   If `header = TRUE`, the file’s existing column names are used and
#'   `col_names` is ignored.
#' @param id_col       What's the name of the column recording animal ID? This should be
#'  a Single string. (default: `"cow"`).
#' @param drop_ids     Which animals do you wish to drop? This should be a vector
#'  indicating values in `id_col` that you wish to remove (default: `NULL`, so remove nothing).
#' @param trans_col    What's the name of the column recording transponder ID for each visit?
#'  This should be a single string. (default: `"transponder"`).
#' @param drop_trans   Which transponders do you wish to delete because they are not part
#'  of your study? This should be a vector indicating values in `trans_col` that
#'  you wish to remove (default: `NULL`, so remove nothing).
#' @param bin_col      What's the name of the column recording the ID of the bin for each visit?
#'  This should be a single string. (default: `"bin"`).
#' @param bins         Which bins are included in your study? This should be a
#'  numeric vector indicating bin IDs to keep (e.g. `2:4` or `c(1,5)`).
#' @param select_cols  Which columns in the dataframe do you wish to keep in your
#'  original data frame after cleaning? This should be a character vector indicating
#'  columns to retain in the final output. Default is `NULL`, so we select all columns.
#'
#' @return A cleaned feeder data frame.
#'
#' @examples
#' # Create a toy feeder data frame
#' original <- data.frame(
#'   cow         = c("A","B","C","A"),
#'   transponder = c("X1","X2","X3","X2"),
#'   bin         = c(1,2,3,2),
#'   value       = c(10,20,30,40),
#'   stringsAsFactors = FALSE
#' )
#' print(original)
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(original, tmp, row.names = FALSE)
#'
#' # Drop cow "A" and transponder "X2", keep bins 2:3, select only cow, bin, value
#' process_feeder(
#'   file        = tmp,
#'   drop_ids    = "A",
#'   drop_trans  = "X2",
#'   bins        = 2:3,
#'   select_cols = c("cow","bin","value"),
#'   header = TRUE
#' )
#' unlink(tmp)
#'
#' @export
process_feeder <- function(file,
                           col_names   = NULL,
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


#' Process 1 water data file
#'
#' Reads, cleans, filters, and offsets a water file:
#' 1. Safely read the CSV
#' 2. Rename columns
#' 3. Drop unwanted cows & transponders
#' 4. Keep only specified bins
#' 5. Subset to desired columns
#' 6. Add an offset to bin IDs, so that water bin ID differs from feed bin ID
#'
#' @inheritParams process_feeder
#' @param bin_offset A single numeric value to add to each matching bin ID. Default is 100.
#'
#' @return A cleaned water data frame with shifted bin IDs.
#'
#' @examples
#' # create a toy data file
#' original <- data.frame(
#'   cow         = c("D","E","F","D"),
#'   transponder = c("Y1","Y2","Y3","Y2"),
#'   bin         = c(5,6,7,6),
#'   value       = c(50,60,70,80),
#'   stringsAsFactors = FALSE
#' )
#' print(original)
#' tmp <- tempfile(fileext = ".csv")
#' write.table(original, tmp, sep = ",", row.names = FALSE, col.names = FALSE)
#'
#' # Drop nothing, keep bins 5:7, offset +100, select cow, bin, value
#' process_water(
#'   file        = tmp,
#'   col_names   = c("cow", "transponder", "bin", "value"),
#'   drop_ids    = NULL,
#'   drop_trans  = NULL,
#'   bins        = 5:7,
#'   select_cols = c("cow","bin","value"),
#'   bin_offset  = 100,
#'   header = FALSE
#' )
#' unlink(tmp)
#'
#' @export
process_water <- function(file,
                          col_names   = NULL,
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
#' @description
#' Reads, cleans, and time‑adjusts a batch of files, returning a named list
#' of data frames keyed by file date (YYYY‑MM‑DD).
#'
#' @inheritParams daylight_saving_adjust
#' @inheritParams process_water
#' @param files  What are the files you wish to process? This should be a character
#'  vector of all the file paths.
#' @param adjust_dst Do you want to apply the function ([daylight_saving_adjust()])
#'  I designed to adjust timestamp for dates affected by Daylight Saving Time
#'  changes or not? This should be logical, default is TRUE. The timestamp adjustment
#'  would only be applied if `adjust_dst` is TRUE and `tz` is set to be a timezone
#'  in North America.
#'
#' @return
#' A **named** list of data frames, one per input file, named by date (YYYY‑MM‑DD).
#'
#' @details
#' Steps:
#' 1. **Validate** all inputs (types, `file_type` ∈ `"feed","water"`).
#' 2. **Extract** a date information from each filename and parse it via [lubridate::ymd()].
#' 3. **Fetch** Daylight Saving Time (DST) switch table for the relevant years ([dst_switch_day()]).
#' 4. **Loop** over each file:
#'    - Call either [process_feeder()] or [process_water()] to do:
#'          - Safely read the CSV / DAT file
#'          - Rename columns
#'          - Drop unwanted cows & transponders
#'          - Keep only specified bins
#'          - Subset to desired columns
#'    - Drop any rows with `NA`.
#'    - Call [daylight_saving_adjust()] to adjust timestamps for daylight saving change days.
#'    - Standardize the columns recording start and end time of each event to be in the format of "yyyy-mm-dd hh:mm:ss".
#'    - Store processed dataframe the output list and name it by the date.
#'
#' @seealso
#'  - \code{\link{process_feeder}}
#'  - \code{\link{process_water}}
#'  - \code{\link{file_name_processing}}
#'  - \code{\link{dst_switch_day}}
#'  - \code{\link{daylight_saving_adjust}}
#'
#' @examples
#' tmp <- tempdir()
#' # create two CSVs in a temporary directory
#' files <- file.path(tmp, paste0("VR2022010", 1:3, ".csv"))
#' for (i in seq_along(files)) {
#'   write.csv(
#'     data.frame(
#'       cow         = c("A", "B", "C"),
#'       transponder = c("X1","X2","X3"),
#'       bin         = i + 0:2,
#'       start       = c("01:00:00", "02:00:00", "03:00:00"),
#'       end         = c("01:05:00", "02:06:01", "03:03:00")
#'     ),
#'     file      = files[i],
#'     row.names = FALSE
#'    )
#' }
#'
#' res <- process_all_feed(
#' files       = files,
#' bins        = 1:10,
#' select_cols = c("cow","bin","start","end"),
#' sep         = ",",
#' header      = TRUE,
#' tz          = "America/Vancouver")
#'
#' res
#'
#' @export
process_all_feed <- function(
    files,
    col_names   = NULL,
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
    tz          = Sys.timezone(),
    adjust_dst  = TRUE
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
    tz        = tz,
    adjust_dst = adjust_dst
  )
}

#' Process a batch of water files
#'
#' @inherit process_all_feed description details return seealso
#'
#' @inheritParams process_all_feed
#' @param bin_offset A single numeric value to add to each matching bin ID. Default is 100.
#'
#' @examples
#' # 1) create three small water‐data CSVs in a temporary directory
#' tmp   <- tempdir()
#' files <- file.path(tmp, paste0("VW2023042", 0:3, ".csv"))
#' for (i in seq_along(files)) {
#'   toy <- data.frame(
#'     cow         = c("A", "B", "C"),
#'     transponder = c("W1", "W2", "W3"),
#'     bin         = i + c(1, 2, 3),
#'     start       = c("06:00:00", "07:00:00", "08:00:00"),
#'     end         = c("06:05:00", "07:05:00", "08:05:00"),
#'     stringsAsFactors = FALSE
#'   )
#'   write.csv(toy, files[i], row.names = FALSE)
#' }
#'
#' # 2) process them in batch, shifting water‐bin IDs by +100
#' res <- process_all_water(
#'   files       = files,
#'   bins        = 2:4,
#'   select_cols = c("cow", "bin", "start", "end"),
#'   bin_offset  = 100,
#'   sep         = ",",
#'   header      = TRUE,
#'   tz          = "America/Vancouver"
#' )
#' res
#'
#' @export
process_all_water <- function(
    files,
    col_names   = NULL,
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
    tz          = Sys.timezone(),
    adjust_dst  = TRUE
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
    tz        = tz,
    adjust_dst = adjust_dst
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
#' @inheritParams process_feeder
#'
#' @return A processed data frame.
#'
#' @noRd
process_data_generic <- function(
    file,
    col_names   = NULL,
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
  if (!header) {
    if (is.null(col_names) || !is.character(col_names)) {
      stop("`col_names` must be a character vector when header = FALSE.")
    }
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


  # ------ main logic ---------#
  df <- read_data_safely(file, sep = sep, header = header)

  # — Assign column names if needed —
  if (!header) {
    if (length(col_names) != ncol(df)) {
      stop("Length of `col_names` must equal number of columns in the file.")
    }
    colnames(df) <- col_names
  }

  # — Early exit for empty data —
  if (nrow(df) == 0L) {
    if (!is.null(select_cols)) {
      empty <- as.data.frame(matrix(ncol = length(select_cols), nrow = 0))
      names(empty) <- select_cols
    } else{
      empty <- df
    }

    return(empty)
  }

  # — Determine select_cols default —
  if (is.null(select_cols)) {
    select_cols <- colnames(df)
  }
  if (!is.character(select_cols)) {
    stop("`select_cols` must be a character vector or NULL.")
  }
  if (!all(select_cols %in% colnames(df))) {
    stop("Some `select_cols` are not present in the data.frame.")
  }


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
#' @inherit process_all_feed description details return seealso
#'
#' @inheritParams process_all_feed
#' @param file_type a character string indicating if this is feeder (i.e., "feed")
#'  data or water ("water") data. Default is "feed"
#'
#' @noRd
process_all <- function(
    files,
    file_type   = "feed",
    col_names   = NULL,
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
    tz = Sys.timezone(),
    adjust_dst  = TRUE
) {
  # ---- input validation ----
  if (!is.character(files) || length(files) < 1) {
    stop("`files` must be a nonempty character vector of file paths.")
  }
  file_type <- match.arg(trimws(tolower(file_type)), c("feed","water"))
  if (!header) {
    if (is.null(col_names) || !is.character(col_names)) {
      stop("`col_names` must be a character vector when header = FALSE.")
    }
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

      # only adjust daylight saving changes if timezone is set in north america, and user set adjust_dst to TRUE
      if ((nrow(dst_df) > 0) && adjust_dst && is_north_american_tz(tz)){
        df <- daylight_saving_adjust(
          data_frame               = df,
          date                     = date,
          start_col                = start_col,
          end_col                  = end_col,
          dst_df                   = dst_df,
          daylight_change_duration = daylight_change_duration,
          tz                       = tz
        )

      }

      df[[start_col]] <- lubridate::ymd_hms(paste(date, df[[start_col]]), tz = tz)
      df[[end_col]]   <- lubridate::ymd_hms(paste(date, df[[end_col]]),   tz = tz)
    }

    # 4.4) Store and name by date
    out[[i]]      <- df
    names(out)[i] <- date
  }

  return(out)
}

#' Check if a timezone is in North America
#'
#' This internal helper function checks whether a given timezone string
#' corresponds to a North American region. It currently matches timezones
#' that start with "America/" or "Canada/", which covers most zones in the
#' United States, Canada, and Mexico.
#'
#' @inheritParams get_dst_switch_info
#'
#' @return A logical value indicating whether the timezone is in North America.
#'
#' @noRd
is_north_american_tz <- function(tz) {
  grepl("^America/|^Canada/", tz)
}
