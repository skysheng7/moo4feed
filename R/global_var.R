utils::globalVariables(c(
  "season", "fall", "spring", "start", "end", "overlap", "n", "rate",
  "date", "feed_intake", "feed_duration", "feed_visits",
  "water_intake", "water_duration", "water_visits", "warning_str", "unique_feed_bins_visited",
  "unique_water_bins_visited", "visit_freq"
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
#' @param tz Timezone (default current global value)
#' @param id_col Animal ID column name (default current global value)
#' @param trans_col Transponder column name (default current global value)
#' @param start_col Start time column name (default current global value)
#' @param end_col End time column name (default current global value)
#' @param bin_col Bin ID column name (default current global value)
#' @param dur_col Duration column name (default current global value)
#' @param intake_col Intake column name (default current global value)
#' @param start_weight_col Start weight column name (default current global value)
#' @param end_weight_col End weight column name (default current global value)
#' @param bin_offset Numeric bin offset (default current global value)
#' @param bins_feed Integer vector of feed bins (default current global value)
#' @param bins_wat Integer vector of water bins (default current global value)
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
                            bins_wat = bins_wat2()) {
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

  invisible(NULL)
}
