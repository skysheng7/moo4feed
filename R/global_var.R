utils::globalVariables(c(
  "season", "fall", "spring", "start", "end", "overlap", "n", "rate",
  "date", "feed_intake", "feed_duration", "feed_visits",
  "water_intake", "water_duration", "water_visits", "warning_str", "unique_feed_bins_visited",
  "unique_water_bins_visited", "visit_freq", "next_cow", "time_dif", "actor_at_another_bin", 
  "date_str", ".data", "outlier", "intake_col", "duration_col", "gaps", "cluster", "meal_duration",
  "total_feeding_duration", "component", "density", "gap_minutes", "hcl.colors", "meal_id_factor", 
  "time_of_day", "x"
))

# internal environment that stores the run-time global variable options
the <- new.env(parent = emptyenv())

## default global variables ----------------------------------------------------
the$tz          <- "America/Vancouver"
the$id_col      <- "cow"
the$trans_col   <- "transponder"
the$start_col   <- "start"
the$end_col     <- "end"
the$bin_col     <- "bin"
the$dur_col     <- "duration"
the$intake_col     <- "intake"
the$start_weight_col     <- "start_weight"
the$end_weight_col     <- "end_weight"
the$bin_offset  <- 100
the$bins_feed   <- 1:30
the$bins_wat    <- 1:5
the$bin_layout  <- c(1, 2, 3, 4, 5, 6, 101, 102, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 103, 104, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 105)

# -- tz -------------------------------------------------------------

#' Get the timezone currently set as global variable
#'
#' @return A single character string stating the timezone (e.g., `"America/Vancouver"`).
#' @examples
#' tz2()
#' @export
tz2 <- function() the$tz

#' Set a new timezone which will be used as a global variable governing the processing of timestamp data
#'
#' @param new_tz A valid time zone name (default is the time zone of your current
#' physical location, retrived via [Sys.timezone()]), used to determine DST rules.
#' Use `OlsonNames()` to see all valid options.
#'
#' @return Called for its side-effects
#' @examples
#' # Originally timezone was default set to "America/Vancouver"
#' tz2()
#' # Now let's change timezone (`tz`) to be "UTC"
#' set_tz2("UTC")
#' # check if timezone is set properly
#' tz2()
#' @export
set_tz2 <- function(new_tz = Sys.timezone()) {
  old <- the$tz
  the$tz <- new_tz
  invisible(old)
}


# -- id_col ---------------------------------------------------------------

#' Get the name of the column recording animal ID
#'
#' What's the name of the column recording animal ID? This should be
#'  a Single string. (default: `"cow"`).
#'
#' @return A single character string stating the name of the column recording
#'  animal ID (e.g., `"cow"`).
#' @examples
#' id_col2()
#' @export
id_col2 <- function() the$id_col

#' Set the name of the column recording animal ID as global variable
#'
#' @param new_name What's the name of the column recording animal ID? This should be
#'  a Single string. (default: `"cow"`).
#'
#' @return Called for its side-effects
#' @examples
#' # set global variable `id_col` as "animal_id"
#' set_id_col2("animal_id")
#' # check if we set it up correctly
#' id_col2()
#' @export
set_id_col2 <- function(new_name = "cow") {
  old <- the$id_col
  the$id_col <- new_name
  invisible(old)
}

# -- trans_col ------------------------------------------------------------

#' Get the name of the column recording transponder ID for each visit
#'
#' What's the name of the column recording transponder ID for each visit? This
#' should be a single string. (default: `"transponder"`).
#'
#' @return A single character string stating the name of the column recording
#'  transponder ID (e.g., `"transponder"`).
#' @examples
#' trans_col2()
#' @export
trans_col2 <- function() the$trans_col

#' Set the name of the column recording transponder ID as global variable
#'
#' @param new_name What's the name of the column recording transponder ID for each visit? This
#' should be a single character string. (default: `"transponder"`).
#'
#' @return Called for its side-effects
#' @examples
#' # set global variable `trans_col` as "tag_id"
#' set_trans_col2("tag_id")
#' # check if `trans_col` is set up correctly
#' trans_col2()
#' @export
set_trans_col2 <- function(new_name = "transponder") {
  old <- the$trans_col
  the$trans_col <- new_name
  invisible(old)
}

# -- start_col ------------------------------------------------------------

#' Get the name of the column recording the start time of an event
#'
#' Name of the column recording the start time of an event, e.g.: start_col = "start"
#'
#' @return A single character string indicating name of the column recording start
#'  time of an event (e.g., `"start"`).
#' @examples
#' start_col2()
#' @export
start_col2 <- function() the$start_col

#' Set the name of the column recording the start time of an event as global variable
#'
#' Name of the column recording the start time of an event, e.g.: start_col = "start"
#'
#' @param new_name A single character string naming the column that stores the
#'   start time of each visit/event.
#'
#' @return Called for its side-effects
#' @examples
#' # set global variable `start_col` as "start_time"
#' set_start_col2("start_time")
#' # check if `start_col` is set up correctly
#' start_col2()
#' @export
set_start_col2 <- function(new_name = "start") {
  old <- the$start_col
  the$start_col <- new_name
  invisible(old)
}

# -- end_col --------------------------------------------------------------

#' Get the name of the column recording the end time of an event
#'
#' Name of the column recording the end time of an event, e.g.: `end_col = "end"`.
#'
#' @return A single character string indicating the name of the column recording
#'   the end time of an event (e.g., `"end"`).
#' @examples
#' end_col2()
#' @export
end_col2 <- function() the$end_col

#' Set the name of the column recording the end time of an event as a global variable
#'
#' Name of the column recording the end time of an event, e.g.: `end_col = "end"`.
#'
#' @param new_name A single character string naming the column that stores the
#'   end time of each visit/event.
#'
#' @return Called for its side-effects.
#' @examples
#' # set global variable `end_col` to "end_time"
#' set_end_col2("end_time")
#' # check if `end_col` is set up correctly
#' end_col2()
#' @export
set_end_col2 <- function(new_name = "end") {
  old <- the$end_col
  the$end_col <- new_name
  invisible(old)
}


# -- bin_col --------------------------------------------------------------

#' Get the name of the column recording the ID of the bin for each visit
#'
#' What's the name of the column recording the ID of the bin for each visit?
#' This should be a single string. (default: `"bin"`).
#'
#' @return A single character string stating the name of the column recording the
#'  bin ID (e.g., `"bin"`).
#' @examples
#' bin_col2()
#' @export
bin_col2 <- function() the$bin_col

#' Set the name of the column recording bin ID as global variable
#'
#' @param new_name What's the name of the column recording the ID of the bin
#'  for each visit? This should be a single string. (default: `"bin"`).
#'
#' @return Called for its side-effects
#' @examples
#' # set global variable `bin_col` as "feeder_bin"
#' set_bin_col2("feeder_bin")
#' # check if `bin_col` is set up correctly
#' bin_col2()
#' @export
set_bin_col2 <- function(new_name = "bin") {
  old <- the$bin_col
  the$bin_col <- new_name
  invisible(old)
}

# -- bin_offset -----------------------------------------------------------

#' Get the numeric **bin offset** which was set as global variable `bin_offset`
#'
#' The global variable `bin_offset` will be used to rename bin ID by adding an
#' amount defined in `bin_offset` to each bin's original ID  in `df[[bin_col]]`.
#' We wish to do this because sometimes water bin and feed bin share the same
#' ID, and we want to differentiate water bins from feed bins using this `bin_offset`.
#'
#' @return A single numeric value to add to each matching bin ID. Default is 100.
#' @examples
#' bin_offset2()
#' @export
bin_offset2 <- function() the$bin_offset

#' Set the numeric **bin offset**
#'
#' The global variable `bin_offset` will be used to rename bin ID by adding an
#' amount defined in `bin_offset` to each bin's original ID  in `df[[bin_col]]`.
#' We wish to do this because sometimes water bin and feed bin share the same
#' ID, and we want to differentiate water bins from feed bins using this `bin_offset`.
#'
#' @param new_offset A single numeric value to add to each matching bin ID. Default is 100.
#'
#' @return Called for its side-effects
#' @examples
#' # set global variable `bin_offset` as 50
#' set_bin_offset2(50)
#' # check if `bin_offset` is set up correctly
#' bin_offset2()
#' @export
set_bin_offset2 <- function(new_offset = 100) {
  old <- the$bin_offset
  the$bin_offset <- new_offset
  invisible(old)
}

# -- bins_feed ------------------------------------------------------------

#' Get the vector of all feed bins included in your study
#'
#' Which feed bins are included in your study for analysis? This should be
#' a numeric vector of bin IDs to keep. You can supply individual
#' values (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).
#'
#' @return An integer vector indicating feed bin IDs (default `1:30`).
#' @examples
#' bins_feed2()
#' @export
bins_feed2 <- function() the$bins_feed

#' Set the vector of all feed bins included in your study as global variable
#'
#' Which feed bins are included in your study for analysis? This should be
#' a numeric vector of bin IDs to keep. You can supply individual
#' values (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).
#'
#' @param new_bins An integer vector of valid feed bin ID
#'
#' @return Called for its side-effects
#' @examples
#' # set global variable `bins_feed` as 1:20
#' set_bins_feed2(1:20)
#' # check if `bins_feed` is set up correctly
#' bins_feed2()
#'
#' @export
set_bins_feed2 <- function(new_bins = 1:30) {
  old <- the$bins_feed
  the$bins_feed <- new_bins
  invisible(old)
}

# -- bins_wat -------------------------------------------------------------

#' Get the vector of all water bins included in your study
#'
#' Which water bins are included in your study for analysis? This should be
#' a numeric vector of bin IDs to keep. You can supply individual
#' values (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).
#'
#' @return An integer vector (default `1:5`).
#' @examples
#' bins_wat2()
#' @export
bins_wat2 <- function() the$bins_wat

#' Set the vector of all water bins included in your study as global variable
#'
#' Which water bins are included in your study for analysis? This should be
#' a numeric vector of bin IDs to keep. You can supply individual
#' values (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).
#'
#' @param new_bins An integer vector of valid water bin ID
#'
#' @return Called for its side-effects
#' @examples
#' # set global variable `bins_wat` as 1:3
#' set_bins_wat2(1:3)
#' # check if `bins_wat` is set up correctly
#' bins_wat2()
#'
#' @export
set_bins_wat2 <- function(new_bins = 1:5) {
  old <- the$bins_wat
  the$bins_wat <- new_bins
  invisible(old)
}

# -- bin_layout -----------------------------------------------------------

#' Get the physical layout order of bins for spatial analysis
#'
#' Returns the physical arrangement order of feed and water bins as they are
#' positioned in the facility. This is used for neighbor analysis and spatial
#' synchronicity studies where the physical proximity of bins matters.
#'
#' @return An integer vector indicating the physical order of bins from left to right
#'   (or following the physical layout). Feed bins are numbered 1-30, water bins 101-105.
#'   **Note**: Water bins use updated IDs to avoid conflicts with feed bin numbering.
#' @examples
#' bin_layout2()
#' @export
bin_layout2 <- function() the$bin_layout

#' Set the physical layout order of bins as global variable
#'
#' Define the physical arrangement order of feed and water bins as they are
#' positioned in the facility. This is used for neighbor analysis and spatial
#' synchronicity studies where the physical proximity of bins matters.
#'
#' @param new_layout An integer vector specifying the physical order of bins.
#'   Should include all feed bins (from bins_feed2()) and water bins (from bins_wat2())
#'   in their actual physical arrangement order. **Important**: Use the updated bin IDs
#'   (e.g., water bins should be 101, 102, etc., not 1, 2) to avoid conflicts between
#'   feed and water bin numbering systems.
#'
#' @return Called for its side-effects
#' @examples
#' # Set a custom physical layout using updated bin IDs
#' set_bin_layout2(c(1, 2, 101, 3, 4, 102, 5, 6))
#' # Check if bin_layout is set up correctly
#' bin_layout2()
#'
#' @export
set_bin_layout2 <- function(new_layout = c(1, 2, 3, 4, 5, 6, 101, 102, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 103, 104, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 105)) {
  
  # Input validation
  if (!is.numeric(new_layout)) {
    stop("`new_layout` must be a numeric vector", call. = FALSE)
  }
  
  if (length(new_layout) == 0) {
    stop("`new_layout` cannot be empty", call. = FALSE)
  }
  
  # Check for duplicate bin IDs
  if (any(duplicated(new_layout))) {
    duplicates <- new_layout[duplicated(new_layout)]
    stop("Duplicate bin IDs found in layout: ", paste(unique(duplicates), collapse = ", "), 
         ". Each bin ID must appear only once in the physical layout.", call. = FALSE)
  }
  
  # Check for potential conflicts between feed and water bin IDs
  feed_bins <- bins_feed2()
  water_bins <- bins_wat2()
  
  # Warn if there are overlapping IDs between feed and water bins in the layout
  feed_in_layout <- intersect(new_layout, feed_bins)
  water_in_layout <- intersect(new_layout, water_bins)
  overlapping_ids <- intersect(feed_in_layout, water_in_layout)
  
  if (length(overlapping_ids) > 0) {
    warning("Bin IDs ", paste(overlapping_ids, collapse = ", "), 
            " appear in both feed and water bin lists. Make sure you're using updated bin IDs ",
            "(e.g., water bins should be 101+, not 1-5) to avoid conflicts.", call. = FALSE)
  }
  
  # Provide helpful message about missing bins
  all_bins <- c(feed_bins, water_bins)
  missing_bins <- setdiff(all_bins, new_layout)
  extra_bins <- setdiff(new_layout, all_bins)
  
  if (length(missing_bins) > 0) {
    message("Note: The following bins from your feed/water bin lists are not included in the layout: ", 
            paste(missing_bins, collapse = ", "))
  }
  
  if (length(extra_bins) > 0) {
    message("Note: The following bins in the layout are not in your feed/water bin lists: ", 
            paste(extra_bins, collapse = ", "))
  }
  
  old <- the$bin_layout
  the$bin_layout <- new_layout
  invisible(old)
}

# -- duration_col --------------------------------------------------------------

#' Get the name of the column recording visit duration
#'
#' What's the name of the column recording the duration for each visit?
#' Default is "duration".
#'
#' @return A single character string stating the duration column name.
#' @examples
#' duration_col2()
#' @export
duration_col2 <- function() the$dur_col

#' Set the name of the duration column as global variable
#'
#' @param new_name A single string indicating the new duration column name.
#'
#' @return Called for its side-effects
#' @examples
#' set_duration_col2("visit_duration")
#' duration_col2()
#' @export
set_duration_col2 <- function(new_name = "duration") {
  old <- the$dur_col
  the$dur_col <- new_name
  invisible(old)
}

# -- intake_col ----------------------------------------------------------------

#' Get the name of the column recording feed/water intake
#'
#' What's the name of the column recording the intake per visit?
#' Default is "intake".
#'
#' @return A single character string stating the intake column name.
#' @examples
#' intake_col2()
#' @export
intake_col2 <- function() the$intake_col

#' Set the name of the intake column as global variable
#'
#' @param new_name A single string indicating the new intake column name.
#'
#' @return Called for its side-effects
#' @examples
#' set_intake_col2("feed_intake")
#' intake_col2()
#' @export
set_intake_col2 <- function(new_name = "intake") {
  old <- the$intake_col
  the$intake_col <- new_name
  invisible(old)
}

# -- start_weight_col ----------------------------------------------------------

#' Get the name of the column recording start weight
#'
#' What's the name of the column recording the start weight per visit?
#' Default is "start_weight".
#'
#' @return A single character string stating the start weight column name.
#' @examples
#' start_weight_col2()
#' @export
start_weight_col2 <- function() the$start_weight_col

#' Set the name of the start weight column as global variable
#'
#' @param new_name A single string indicating the new start weight column name.
#'
#' @return Called for its side-effects
#' @examples
#' set_start_weight_col2("initial_weight")
#' start_weight_col2()
#' @export
set_start_weight_col2 <- function(new_name = "start_weight") {
  old <- the$start_weight_col
  the$start_weight_col <- new_name
  invisible(old)
}

# -- end_weight_col ------------------------------------------------------------

#' Get the name of the column recording end weight
#'
#' What's the name of the column recording the end weight per visit?
#' Default is "end_weight".
#'
#' @return A single character string stating the end weight column name.
#' @examples
#' end_weight_col2()
#' @export
end_weight_col2 <- function() the$end_weight_col

#' Set the name of the end weight column as global variable
#'
#' @param new_name A single string indicating the new end weight column name.
#'
#' @return Called for its side-effects
#' @examples
#' set_end_weight_col2("final_weight")
#' end_weight_col2()
#' @export
set_end_weight_col2 <- function(new_name = "end_weight") {
  old <- the$end_weight_col
  the$end_weight_col <- new_name
  invisible(old)
}

#' Set multiple global variables at once
#'
#' This function allows users to set multiple global variables simultaneously.
#' Each parameter defaults to its current global value if unspecified.
#'
#' @param tz Timezone (default current global value from [tz2()])
#' @param id_col Animal ID column name (default current global value from [id_col2()])
#' @param trans_col Transponder column name (default current global value from [trans_col2()])
#' @param start_col Start time column name (default current global value from [start_col2()])
#' @param end_col End time column name (default current global value from [end_col2()])
#' @param bin_col Bin ID column name (default current global value from [bin_col2()])
#' @param dur_col Duration column name (default current global value from [duration_col2()])
#' @param intake_col Intake column name (default current global value from [intake_col2()])
#' @param start_weight_col Start weight column name (default current global value from [start_weight_col2()])
#' @param end_weight_col End weight column name (default current global value from [end_weight_col2()])
#' @param bin_offset Numeric bin offset (default current global value from [bin_offset2()])
#' @param bins_feed Integer vector of feed bins (default current global value from [bins_feed2()])
#' @param bins_wat Integer vector of water bins (default current global value from [bins_wat2()])
#' @param bin_layout Integer vector of physical bin layout (default current global value from [bin_layout2()])
#'
#' @return Called for its side-effects
#' @examples
#' set_global_cols(tz = "UTC", id_col = "animal_id", dur_col = "visit_duration")
#' @export
set_global_cols <- function(tz = tz2(),
                            id_col = id_col2(),
                            trans_col = trans_col2(),
                            start_col = start_col2(),
                            end_col = end_col2(),
                            bin_col = bin_col2(),
                            dur_col = duration_col2(),
                            intake_col = intake_col2(),
                            start_weight_col = start_weight_col2(),
                            end_weight_col = end_weight_col2(),
                            bin_offset = bin_offset2(),
                            bins_feed = bins_feed2(),
                            bins_wat = bins_wat2(),
                            bin_layout = bin_layout2()) {
  set_tz2(tz)
  set_id_col2(id_col)
  set_trans_col2(trans_col)
  set_start_col2(start_col)
  set_end_col2(end_col)
  set_bin_col2(bin_col)
  set_duration_col2(dur_col)
  set_intake_col2(intake_col)
  set_start_weight_col2(start_weight_col)
  set_end_weight_col2(end_weight_col)
  set_bin_offset2(bin_offset)
  set_bins_feed2(bins_feed)
  set_bins_wat2(bins_wat)
  set_bin_layout2(bin_layout)

  invisible(NULL)
}
