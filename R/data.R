#' Cattle feeding behavior and visit record data
#'
#' An example dataset containing feeding behavior and visit records for cattle
#' over a two-day period (2020-10-31 to 2020-11-01). Each day's data is stored
#' as a separate data frame within a list. This is the cleaned data output from
#' the vignettes (i.e., "Articles" listed on the package website) "Data cleaning" code.
#'
#' @format A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
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
#' over two consecutive days. Each entry represents a distinct feeding event
#' where an animal visited a feed bin.
#'
#' @source Collected using an Insentec automatic feeder at University of British Columbia
#' Dairy Education and Research Centre from October 31 to November 1, 2020.
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
#' over a two-day period (2020-10-31 to 2020-11-01). Each day's data is stored
#' as a separate data frame within a list. This is the cleaned data output from
#' the vignettes (i.e., "Articles" listed on the package website) "Data cleaning" code.
#'
#' @format A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
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
#' over two consecutive days. Each entry represents a distinct drinking event
#' where an animal visited a water bin.
#'
#' @source Collected using an Insentec automatic waterer at University of British Columbia
#' Dairy Education and Research Centre from October 31 to November 1, 2020.
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

#' Cattle feeding behavior data after quality check and outlier removal
#'
#' A dataset containing feeding visit records for cattle 
#' over a two-day period (2020-10-31 to 2020-11-01). This dataset is the result of 
#' applying quality control procedures and KNN-based outlier removal to the raw [all_fed] data.
#'
#' @format A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
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
#'   \item{rate}{numeric, how fast the cow was eating (kg/s)}
#' }
#'
#' @details This dataset is the result of applying quality control checks using the 
#' [qc()] function to the raw [all_fed] data. The cleaning process includes removing 
#' invalid records, fixing double detections, and ensuring all intake and duration 
#' values are plausible. Details are as follows:
#' 
#' @source [all_fed]
#'
#' @examples
#' # Access quality-checked data for the first day
#' first_day <- clean_feed[["2020-10-31"]]
#'
#' # Count feeding events by cow on November 1 after quality control
#' table(clean_feed[["2020-11-01"]]$cow)
"clean_feed"

#' Cattle drinking behavior data after quality check and outlier removal
#'
#' A dataset containing drinking visit records for cattle 
#' over a two-day period (2020-10-31 to 2020-11-01). This dataset is the result of 
#' applying quality control procedures and KNN-based outlier removal to the raw [all_wat] data.
#'
#' @format A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
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
#'   \item{rate}{numeric, how fast the cow was drinking (L/s)}
#' }
#'
#' @details This dataset is the result of applying quality control checks using the 
#' [qc()] function to the raw [all_wat] data. The cleaning process includes removing 
#' invalid records, fixing double detections, and ensuring all intake and duration 
#' values are plausible.
#' 
#' @source [all_wat]
#'
#' @examples
#' # Access quality-checked data for the first day
#' first_day <- clean_water[["2020-10-31"]]
#'
#' # Count drinking events by cow on November 1 after quality control
#' table(clean_water[["2020-11-01"]]$cow)
"clean_water"

#' Combined feeding and drinking behavior data with outliers removed
#'
#' A fully cleaned dataset containing both feeding and drinking behavior data for cattle
#' over a two-day period (2020-10-31 to 2020-11-01). This dataset is the result of
#' applying quality control procedures and KNN-based outlier removal to both feed and water data,
#' then combining them into a single dataset for integrated analysis.
#'
#' @format A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
#' with each data frame containing the following 11 variables:
#' \describe{
#'   \item{transponder}{integer, unique electronic ID for each bin}
#'   \item{cow}{integer, animal ID number}
#'   \item{bin}{numeric, bin location number (feed or water bin)}
#'   \item{start}{POSIXct, timestamp when the visit started}
#'   \item{end}{POSIXct, timestamp when the visit ended}
#'   \item{duration}{integer, duration of the visit in seconds}
#'   \item{start_weight}{numeric, weight of feed/water at start of visit}
#'   \item{end_weight}{numeric, weight of feed/water at end of visit}
#'   \item{intake}{numeric, amount consumed (kg or L) during the visit}
#'   \item{date}{Date, calendar date of the visit}
#'   \item{intake}{numeric, how fast the cow was eating/drinking (kg/s or L/s)}
#' }
#'
#' @details This dataset represents the final, fully cleaned dataset after applying multiple
#' layers of quality control. First, the [qc()] function was used to fix basic issues like
#' double detections and negative values. Then, advanced outlier detection using K-Nearest 
#' Neighbors (KNN) was applied through the [knn_clean_feed()] and [knn_clean_water()] functions
#' to remove improbable data points. Finally, the cleaned feed and water datasets
#' were combined using [combine_feed_water()]. This dataset provides a comprehensive view of
#' both feeding and drinking behaviors in a single, analysis-ready format.
#' 
#' The KNN outlier detection emphasizes on removing data points with high rate and intake, and do not
#' punish too much for data points with long duration. Because based on our experience, it's likely for 
#' a cow to have very long durations per visit, but very unlikely to have large intake in a short time, 
#' so we flagged outliers to catch visits with large intake and high rate.
#' 
#' @source [clean_feed] and [clean_water] after KNN outlier removal
#'
#' @examples
#' # Access combined data for the first day
#' first_day <- clean_comb[["2020-10-31"]]
#'
#' head(first_day)
"clean_comb"
