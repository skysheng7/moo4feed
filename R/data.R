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

#' Cattle feeding behavior data after quality check and outlier removal
#'
#' A dataset containing feeding visit records for cattle 
#' over a three-day period (2020-10-31 to 2020-11-02). This dataset is the result of 
#' applying quality control procedures and KNN-based outlier removal to the raw [all_fed] data.
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
#' over a three-day period (2020-10-31 to 2020-11-02). This dataset is the result of 
#' applying quality control procedures and KNN-based outlier removal to the raw [all_wat] data.
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
#' over a three-day period (2020-10-31 to 2020-11-02). This dataset is the result of
#' applying quality control procedures and KNN-based outlier removal to both feed and water data,
#' then combining them into a single dataset for integrated analysis.
#'
#' @format A list of 3 data frames, one for each date (2020-10-31, 2020-11-01, 2020-11-02),
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

#' Daily summary of cattle feeding and drinking behavior
#'
#' A summarized dataset containing daily feed and water intake metrics for cattle 
#' over a three-day period (2020-10-31 to 2020-11-02). This dataset aggregates the 
#' quality-checked `clean_feed` and `clean_water` data to provide daily totals per animal.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{date}{Date, calendar date of the records}
#'   \item{cow}{integer, animal ID number}
#'   \item{feed_intake}{numeric, total daily feed intake (kg)}
#'   \item{feed_duration}{integer, total daily feeding duration (seconds)}
#'   \item{feed_visits}{integer, total number of feeding visits per day}
#'   \item{water_intake}{numeric, total daily water intake (L)}
#'   \item{water_duration}{integer, total daily drinking duration (seconds)}
#'   \item{water_visits}{integer, total number of drinking visits per day}
#' }
#'
#' @details This dataset is created using the [feed_water_summary()] function, which 
#' aggregates the quality-checked feeding and drinking data to produce daily summaries 
#' for each cow. The summary provides valuable metrics for monitoring feed and water intake 
#' patterns, identifying potential health issues, and conducting nutritional research.
#'
#' @source [clean_feed] and [clean_water]
#'
#' @examples
#' # Calculate average daily feed intake across all cows all days
#' mean(summary_df$feed_intake)
#'
#' # Find cows with the highest feed intake on a specific day
#' high_intake <- subset(summary_df, date == "2020-11-01")
#' head(high_intake[order(high_intake$feed_intake, decreasing = TRUE), ])
"summary_df"

#' Quality control warnings and issues for cattle feeding and drinking data
#'
#' A dataset containing all warnings and issues identified during quality control 
#' procedures applied to feeding and drinking data. This dataset is generated by the
#' [qc()] function and updated by the [feed_water_summary()] function with additional
#' warnings related to daily intake patterns.
#'
#' @format A tibble with one row per day and the following columns:
#' \describe{
#'   \item{date}{character, calendar date of the records in "YYYY-MM-DD" format}
#'   \item{total_cows}{integer, number of unique cows detected on that day}
#'   \item{missing_cow}{character, "Yes" if expected number of cows is not present, NA otherwise}
#'   \item{double_detection_bins}{character, semicolon-separated list of bin IDs where double 
#'   detection issues occurred (when the same cow is detected at different bins at the same time)}
#'   \item{negative_visit_bins}{character, semicolon-separated list of bin IDs with negative 
#'   duration or intake values}
#'   \item{cows_disappeared_after_noon}{character, semicolon-separated list of cow IDs that 
#'   did not appear after 12:00 noon}
#'   \item{bins_never_visited}{character, semicolon-separated list of bin IDs that had no visits}
#'   \item{bins_low_traffic}{character, semicolon-separated list of bin IDs with fewer than the 
#'   threshold number of visits}
#'   \item{long_dur_feeder}{character, semicolon-separated list of feeding visits with 
#'   abnormally long durations}
#'   \item{large_intake_feed_visit}{character, semicolon-separated list of feeding visits with 
#'   abnormally large intake amounts}
#'   \item{low_daily_feed_intake_cows}{character, semicolon-separated list of cow IDs with 
#'   daily feed intake below the threshold}
#'   \item{high_daily_feed_intake_cows}{character, semicolon-separated list of cow IDs with 
#'   daily feed intake above the threshold}
#'   \item{feed_add_time_no_found}{character, times when feed addition could not be identified}
#'   \item{long_dur_drinker}{character, semicolon-separated list of drinking visits with 
#'   abnormally long durations}
#'   \item{large_intake_water_visit}{character, semicolon-separated list of drinking visits with 
#'   abnormally large intake amounts}
#'   \item{low_daily_water_intake_cows}{character, semicolon-separated list of cow IDs with 
#'   daily water intake below the threshold}
#'   \item{high_daily_water_intake_cows}{character, semicolon-separated list of cow IDs with 
#'   daily water intake above the threshold}
#' }
#'
#' @details This dataset documents all quality control issues found when processing
#' raw feeding and drinking data. It is initially generated by the [qc()] function
#' which identifies issues like double detections, negative intake values, and abnormal
#' durations. It is further updated by the [feed_water_summary()] function to include
#' warnings about daily intake patterns such as abnormally high or low daily feed
#' and water consumption. The warning dataset is essential for data validation and
#' can help identify potential equipment malfunctions, animal health issues, or data
#' collection problems.
#'
#' @source Generated from quality control checks on [all_fed] and [all_wat]
#'
#' @examples
#' # Check which days had bins with negative values
#' warning[!is.na(warning$negative_visit_bins), c("date", "negative_visit_bins")]
#' 
"warning"

#' Unique bin visit patterns by cow
#'
#' A dataset containing the unique bin visit patterns for each cow across the three-day period
#' (2020-10-31 to 2020-11-02). This dataset is the result of applying the [unique_bin_visits()]
#' function to the quality-checked feeding and drinking data.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{date}{Date, calendar date of the records}
#'   \item{cow}{integer, animal ID number}
#'   \item{unique_feed_bins_visited}{integer, number of unique feed bins visited by the cow on that day}
#'   \item{unique_water_bins_visited}{integer, number of unique water bins visited by the cow on that day}
#'   \item{total_bins_visited}{integer, total number of unique bins (feed + water) visited by the cow on that day}
#' }
#'
#' @details This dataset quantifies the exploratory behavior of each cow by counting how many
#' different feeding and drinking stations they visited on each day. Cows with higher values
#' tend to be more exploratory, visiting many different bins, while those with lower values
#' may be more habitual, preferring to use fewer bins consistently.
#'
#' @source Generated by applying [unique_bin_visits()] to [clean_feed] and [clean_water]
#'
#' @examples
#' # Calculate average number of unique bins visited per cow
#' avg_bins <- aggregate(total_bins_visited ~ cow, bin_visits, mean)
#'
#' # Find the most exploratory cow (visits the most unique bins)
#' most_exploratory <- avg_bins[which.max(avg_bins$total_bins_visited), ]
#' most_exploratory
"bin_visits"

#' Replacement events between cows
#'
#' A dataset containing validated replacement events where one cow (actor) replaces another cow (reactor)
#' at a feeding bin across the three-day period (2020-10-31 to 2020-11-02). This dataset is the result
#' of applying the [record_replacement_days()] function to the quality-checked feeding data.
#'
#' @format A list of 3 data frames, one for each date (2020-10-31, 2020-11-01, 2020-11-02),
#' with each data frame containing the following variables:
#' \describe{
#'   \item{reactor_cow}{integer, ID of the cow that was replaced (had to leave the bin)}
#'   \item{bin}{integer, feeding bin location number where the replacement occurred}
#'   \item{time}{POSIXct, timestamp when the replacement event occurred}
#'   \item{date}{Date, calendar date of the replacement event}
#'   \item{actor_cow}{integer, ID of the cow that initiated the replacement (took over the bin)}
#'   \item{bout_interval}{difftime, time gap in seconds between when the reactor cow left and 
#'   the actor cow arrived}
#' }
#'
#' @details A replacement event is defined as an instance where one cow (reactor) leaves a bin and 
#' another cow (actor) enters the same bin within a short time threshold (default: 26 seconds).
#' This dataset only includes validated replacements where the actor cow did not have an "alibi" 
#' (i.e., was not recorded at another bin at the same time), increasing confidence that these
#' represent true social displacement events.
#'
#' @source Generated by applying [record_replacement_days()] to [clean_feed]
#'
#' @examples
#' # Access replacement events for the first day
#' first_day_replacements <- replacements[[1]]
#'
#' # Count the number of times each cow was replaced (as reactor)
#' reactor_counts <- table(first_day_replacements$reactor_cow)
#' head(sort(reactor_counts, decreasing = TRUE))
#'
#' # Count the number of times each cow replaced others (as actor)
#' actor_counts <- table(first_day_replacements$actor_cow)
#' head(sort(actor_counts, decreasing = TRUE))
"replacements"
