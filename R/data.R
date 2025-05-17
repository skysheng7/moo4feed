#' Cattle feeding behavior and visit record data
#'
#' An example dataset containing feeding behavior and visit records for cattle
#' over a three-day period (2020-10-31 to 2020-11-02). Each day's data is stored
#' as a separate data frame within a list. This is the cleaned data output from
#' the vignettes (i.e., "Articles" listed on the package website) "Data cleaning" code.
#'
#' @format A list of 3 data frames, one for each date (2020-10-31, 2020-11-01, 2020-11-02),
#' with each data frame containing the following 10 variables:
#' \describe{
#'   \item{transponder}{integer, unique electronic ID for each bin}
#'   \item{cow}{integer, animal ID number}
#'   \item{bin}{integer, feeding bin location number}
#'   \item{start}{POSIXct, timestamp when feeding event started}
#'   \item{end}{POSIXct, timestamp when feeding event ended}
#'   \item{duration}{integer, duration of feeding event in seconds}
#'   \item{start_weight}{numeric, weight of feed (kg) at start of feeding event}
#'   \item{end_weight}{numeric, weight of feed (kg) at end of feeding event}
#'   \item{intake}{numeric, amount of feed consumed (kg) during the event
#'   (calculated as start_weight - end_weight)}
#'   \item{date}{Date, calendar date of the feeding event}
#' }
#'
#' @details The dataset contains detailed feeding behavior for multiple cattle
#' over three consecutive days.Each entry represents a distinct feeding event
#' where an animal visited a feed bin.
#'
#' @source Collected using an Insentec automatic feeder at University of British Columbia
#' Dairy Education and Research Centre from October 31 to November 2, 2020.
#'
#' @examples
#' # Access data for the first day
#' first_day <- all_fed[["2020-10-31"]]
#'
#' # Calculate average intake per feeding event
#' mean(first_day$intake)
#'
#' # Count feeding events by cow on November 1
#' table(all_fed[["2020-11-01"]]$cow)
"all_fed"


#' Cattle water drinking behavior and visit record data
#'
#' An example dataset containing water drinking behavior and visit records for cattle
#' over a three-day period (2020-10-31 to 2020-11-02). Each day's data is stored
#' as a separate data frame within a list. This is the cleaned data output from
#' the vignettes (i.e., "Articles" listed on the package website) "Data cleaning" code.
#'
#' @format A list of 3 data frames, one for each date (2020-10-31, 2020-11-01, 2020-11-02),
#' with each data frame containing the following 10 variables:
#' \describe{
#'   \item{transponder}{integer, unique electronic ID for each bin}
#'   \item{cow}{integer, animal ID number}
#'   \item{bin}{numeric, water bin location number}
#'   \item{start}{POSIXct, timestamp when drinking event started}
#'   \item{end}{POSIXct, timestamp when drinking event ended}
#'   \item{duration}{integer, duration of drinking event in seconds}
#'   \item{start_weight}{numeric, weight of water (kg) at start of drinking event}
#'   \item{end_weight}{numeric, weight of water (kg) at end of drinking event}
#'   \item{intake}{numeric, amount of water consumed (kg) during the event
#'   (calculated as start_weight - end_weight)}
#'   \item{date}{Date, calendar date of the drinking event}
#' }
#'
#' @details The dataset contains detailed water drinking behavior for multiple cattle
#' over three consecutive days. Each entry represents a distinct drinking event
#' where an animal visited a water bin.
#'
#' @source Collected using an Insentec automatic waterer at University of British Columbia
#' Dairy Education and Research Centre from October 31 to November 2, 2020.
#'
#' @examples
#' # Access data for the first day
#' first_day <- all_wat[["2020-10-31"]]
#'
#' # Calculate average water intake per drinking event
#' mean(first_day$intake)
#'
#' # Count drinking events by cow on November 1
#' table(all_wat[["2020-11-01"]]$cow)
"all_wat"
