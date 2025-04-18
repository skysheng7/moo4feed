#' Detect Daylight Saving Time (DST) Change Dates
#'
#' Returns a data frame with the start ("Spring") and end ("Fall") dates of Daylight Saving Time for each year in the input range.
#'
#' @param years A numeric vector specifying the range of years to evaluate. Use the format `c(start_year, end_year)`, e.g., `years = c(2020, 2021)`..
#' @param tz A valid time zone name (default is the time zone of your current physical location), used to determine DST rules.
#'
#' @return A data frame with columns: Year, Spring (DST start for global north), and Fall (DST end for global south).
#' @export
#'
#' @examples
#' dst_switch(c(2020, 2021), tz = "America/Vancouver")
#' # Returns a data frame like:
#' #   Year    Spring      Fall
#' #   2020 2020-03-08 2020-11-01
#' #   2021 2021-03-14 2021-11-07
dst_switch <- function(years = c(2020, 2021), tz = Sys.timezone()){
  start_date <- paste0(as.character(min(years)), "-01-01")
  end_date <- paste0(as.character(max(years)), "-12-31")
  date_seq <- seq(lubridate::ymd(start_date),
                  lubridate::ymd(end_date),
                  by = "1 day")
  date_seq_tz <- lubridate::force_tz(date_seq, tz = tz)
  dst_status <- lubridate::dst(date_seq_tz)
  dst_change <- date_seq[which(diff(dst_status) != 0)]

  dst_df <- data.frame(date = dst_change) |>
    dplyr::mutate(Year = lubridate::year(date),
                  Season = dplyr::if_else((lubridate::month(date)<9), "Spring", "Fall")) |>
    tidyr::pivot_wider(names_from = Season,
                       values_from = date)

  return(dst_df)
}


