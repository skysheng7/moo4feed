# Package index

## Getting Started

Core functions for reading and processing data.

- [`read_data_safely()`](https://skysheng7.github.io/moo4feed/reference/read_data_safely.md)
  : Safely Read Data from a File
- [`process_all_feed()`](https://skysheng7.github.io/moo4feed/reference/process_all_feed.md)
  : Process a batch of feeder files
- [`process_all_water()`](https://skysheng7.github.io/moo4feed/reference/process_all_water.md)
  : Process a batch of water files
- [`process_feeder()`](https://skysheng7.github.io/moo4feed/reference/process_feeder.md)
  : Process 1 feeder data file
- [`process_water()`](https://skysheng7.github.io/moo4feed/reference/process_water.md)
  : Process 1 water data file

## Data Cleaning and Preparation

### Data Quality Checks

Check and validate feeding and drinking visits.

- [`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md)
  : Build a configuration list for data quality-control
- [`qc()`](https://skysheng7.github.io/moo4feed/reference/qc.md) : Run
  full quality-control check on feeder and drinker data
- [`knn_outlier_detection()`](https://skysheng7.github.io/moo4feed/reference/knn_outlier_detection.md)
  : Detect outliers using k-nearest neighbors (KNN) method
- [`knn_clean_feed()`](https://skysheng7.github.io/moo4feed/reference/knn_clean_feed.md)
  : Process multiple days of feeding data and remove outliers using KNN
- [`knn_clean_water()`](https://skysheng7.github.io/moo4feed/reference/knn_clean_water.md)
  : Process multiple days of water data and remove outliers using KNN
- [`viz_outliers()`](https://skysheng7.github.io/moo4feed/reference/viz_outliers.md)
  : Visualize outliers from KNN outlier detection

### Data Filtering

Filter data relevant to animals and sensors in your study.

- [`delete_rows()`](https://skysheng7.github.io/moo4feed/reference/delete_rows.md)
  : Remove rows by matching values in a specified column
- [`keep_bins()`](https://skysheng7.github.io/moo4feed/reference/keep_bins.md)
  : Keep only rows for specified bins
- [`rename_bins()`](https://skysheng7.github.io/moo4feed/reference/rename_bins.md)
  : Shift specified bin IDs by an offset

### Time Adjustments

Handle Daylight Saving Time changes for North America.

- [`daylight_saving_adjust()`](https://skysheng7.github.io/moo4feed/reference/daylight_saving_adjust.md)
  : Adjust Time Stamp for Daylight Saving Time (DST) Transitions in
  North America
- [`get_dst_switch_info()`](https://skysheng7.github.io/moo4feed/reference/get_dst_switch_info.md)
  : Get Daylight Saving Time (DST) Switch Dates and Exact Transition
  Times
- [`dst_switch_day()`](https://skysheng7.github.io/moo4feed/reference/dst_switch_day.md)
  : Detect Daylight Saving Time (DST) Change Dates
- [`dst_switch_hm()`](https://skysheng7.github.io/moo4feed/reference/dst_switch_hm.md)
  : Detect the Exact Time (hour and minute; hm) of Daylight Saving Time
  (DST) Change

### File Management

Extract date information from file names and compare files.

- [`file_name_processing()`](https://skysheng7.github.io/moo4feed/reference/file_name_processing.md)
  : Process file names and extract date tokens
- [`compare_files()`](https://skysheng7.github.io/moo4feed/reference/compare_files.md)
  : Compare feed and water file names by shared dates
- [`get_date_range()`](https://skysheng7.github.io/moo4feed/reference/get_date_range.md)
  : Get the overall date range for a set of files
- [`cap_first()`](https://skysheng7.github.io/moo4feed/reference/cap_first.md)
  : Capitalize the First Letter of a String
- [`lower_first()`](https://skysheng7.github.io/moo4feed/reference/lower_first.md)
  : De-capitalize the First Letter of a String to Lower Case

### Data Integration

Merge and unmerge feeding and drinking data

- [`combine_feed_water()`](https://skysheng7.github.io/moo4feed/reference/combine_feed_water.md)
  : Combine Feeder and Water Data by Date
- [`merge_list_df()`](https://skysheng7.github.io/moo4feed/reference/merge_list_df.md)
  : Merge a List of Data Frames
- [`unmerge_data()`](https://skysheng7.github.io/moo4feed/reference/unmerge_data.md)
  : Un-merge a combined data frame into a list of data frames by date

## Story of Every Cow

### Feed and Water Summary

Summarize daily feed/water intake, duration, and visit counts.

- [`feed_water_summary()`](https://skysheng7.github.io/moo4feed/reference/feed_water_summary.md)
  : Summarize and check for abnormal feed & water intake

### Bin visit summary

Analyze bin visit patterns.

- [`unique_bin_visits()`](https://skysheng7.github.io/moo4feed/reference/unique_bin_visits.md)
  : Analyze Bin Visit Patterns For Each Cow

### Non-nutritive Visit Analysis

Functions for analyzing non-nutritive and no-feed visits per animal per
day.

- [`calculate_non_nutritive_visits()`](https://skysheng7.github.io/moo4feed/reference/calculate_non_nutritive_visits.md)
  : Calculate non-nutritive visits per animal per day
- [`calculate_no_feed_visits()`](https://skysheng7.github.io/moo4feed/reference/calculate_no_feed_visits.md)
  : Calculate visits with no feed available per animal per day

### Feed Addition Detection

Detect when feed bins are refilled and track feed availability.

- [`detect_feed_additions()`](https://skysheng7.github.io/moo4feed/reference/detect_feed_additions.md)
  : Detect Feed Addition Times
- [`calculate_feed_availability()`](https://skysheng7.github.io/moo4feed/reference/calculate_feed_availability.md)
  : Calculate Percentage of Feed Remaining at Each Visit

### Replacement Behavior Analysis

Functions to identify replacement behavior across multiple days, and
validate those events by checking whether the actor cow was already
present at another bin.

- [`record_replacement_days()`](https://skysheng7.github.io/moo4feed/reference/record_replacement_days.md)
  : Identify and validate replacement events across multiple days

### Meal clustering

Cluster feeding visits into meals.

- [`meal_interval()`](https://skysheng7.github.io/moo4feed/reference/meal_interval.md)
  : Calculate optimal interval between feeding visits for meal
  clustering
- [`viz_eps_percentile()`](https://skysheng7.github.io/moo4feed/reference/viz_eps_percentile.md)
  : Visualize gap distribution and percentile-based optimal interval
  (eps) selection
- [`viz_eps_gmm()`](https://skysheng7.github.io/moo4feed/reference/viz_eps_gmm.md)
  : Visualize gap distribution with GMM fit and optimal interval (eps)
- [`cluster_meals()`](https://skysheng7.github.io/moo4feed/reference/cluster_meals.md)
  : Cluster feeding visits into meals using DBSCAN
- [`meal_label_visits()`](https://skysheng7.github.io/moo4feed/reference/meal_label_visits.md)
  : Cluster feeding visits into meals and label each visit
- [`merge_cluster_results()`](https://skysheng7.github.io/moo4feed/reference/merge_cluster_results.md)
  : Merge meal clustering results with original visit data
- [`viz_meal_clusters()`](https://skysheng7.github.io/moo4feed/reference/viz_meal_clusters.md)
  : Visualize meal clustering results as timeline plots
- [`combine_animal_plots()`](https://skysheng7.github.io/moo4feed/reference/combine_animal_plots.md)
  : Combine multiple days for one animal with pagination
- [`combine_date_plots()`](https://skysheng7.github.io/moo4feed/reference/combine_date_plots.md)
  : Combine multiple animals for one date with pagination
- [`extract_plots()`](https://skysheng7.github.io/moo4feed/reference/extract_plots.md)
  : Extract subset of plots by animal and/or date

### Within-Meal Behavior Analysis

Analyze behavior patterns within identified meals including visit types
and actor/reactor roles.

- [`meal_non_nutritive_summary()`](https://skysheng7.github.io/moo4feed/reference/meal_non_nutritive_summary.md)
  : Analyze Non-Nutritive and Empty Bin Visits Within Meals
- [`meal_replacement_roles()`](https://skysheng7.github.io/moo4feed/reference/meal_replacement_roles.md)
  : Analyze Within-Meal Actor/Reactor Roles
- [`meal_replacement_roles_summary()`](https://skysheng7.github.io/moo4feed/reference/meal_replacement_roles_summary.md)
  : Summarize Actor/Reactor Roles Per Animal Per Day

## Configuration and Customization

### Global Variables

Configure global variables to customize for your data structure.

- [`set_global_cols()`](https://skysheng7.github.io/moo4feed/reference/set_global_cols.md)
  : Set multiple global variables at once

- [`tz2()`](https://skysheng7.github.io/moo4feed/reference/tz2.md) : Get
  the timezone currently set as global variable

- [`set_tz2()`](https://skysheng7.github.io/moo4feed/reference/set_tz2.md)
  : Set a new timezone which will be used as a global variable governing
  the processing of timestamp data

- [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md)
  : Get the name of the column recording animal ID

- [`set_id_col2()`](https://skysheng7.github.io/moo4feed/reference/set_id_col2.md)
  : Set the name of the column recording animal ID as global variable

- [`trans_col2()`](https://skysheng7.github.io/moo4feed/reference/trans_col2.md)
  : Get the name of the column recording transponder ID for each visit

- [`set_trans_col2()`](https://skysheng7.github.io/moo4feed/reference/set_trans_col2.md)
  : Set the name of the column recording transponder ID as global
  variable

- [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md)
  : Get the name of the column recording the start time of an event

- [`set_start_col2()`](https://skysheng7.github.io/moo4feed/reference/set_start_col2.md)
  : Set the name of the column recording the start time of an event as
  global variable

- [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md)
  : Get the name of the column recording the end time of an event

- [`set_end_col2()`](https://skysheng7.github.io/moo4feed/reference/set_end_col2.md)
  : Set the name of the column recording the end time of an event as a
  global variable

- [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md)
  : Get the name of the column recording the ID of the bin for each
  visit

- [`set_bin_col2()`](https://skysheng7.github.io/moo4feed/reference/set_bin_col2.md)
  : Set the name of the column recording bin ID as global variable

- [`bin_offset2()`](https://skysheng7.github.io/moo4feed/reference/bin_offset2.md)
  :

  Get the numeric **bin offset** which was set as global variable
  `bin_offset`

- [`set_bin_offset2()`](https://skysheng7.github.io/moo4feed/reference/set_bin_offset2.md)
  :

  Set the numeric **bin offset**

- [`bins_feed2()`](https://skysheng7.github.io/moo4feed/reference/bins_feed2.md)
  : Get the vector of all feed bins included in your study

- [`set_bins_feed2()`](https://skysheng7.github.io/moo4feed/reference/set_bins_feed2.md)
  : Set the vector of all feed bins included in your study as global
  variable

- [`bins_wat2()`](https://skysheng7.github.io/moo4feed/reference/bins_wat2.md)
  : Get the vector of all water bins included in your study

- [`set_bins_wat2()`](https://skysheng7.github.io/moo4feed/reference/set_bins_wat2.md)
  : Set the vector of all water bins included in your study as global
  variable

- [`bin_layout2()`](https://skysheng7.github.io/moo4feed/reference/bin_layout2.md)
  : Get the physical layout order of bins for spatial analysis

- [`set_bin_layout2()`](https://skysheng7.github.io/moo4feed/reference/set_bin_layout2.md)
  : Set the physical layout order of bins as global variable

- [`duration_col2()`](https://skysheng7.github.io/moo4feed/reference/duration_col2.md)
  : Get the name of the column recording visit duration

- [`set_duration_col2()`](https://skysheng7.github.io/moo4feed/reference/set_duration_col2.md)
  : Set the name of the duration column as global variable

- [`intake_col2()`](https://skysheng7.github.io/moo4feed/reference/intake_col2.md)
  : Get the name of the column recording feed/water intake

- [`set_intake_col2()`](https://skysheng7.github.io/moo4feed/reference/set_intake_col2.md)
  : Set the name of the intake column as global variable

- [`start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/start_weight_col2.md)
  : Get the name of the column recording start weight

- [`set_start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/set_start_weight_col2.md)
  : Set the name of the start weight column as global variable

- [`end_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/end_weight_col2.md)
  : Get the name of the column recording end weight

- [`set_end_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/set_end_weight_col2.md)
  : Set the name of the end weight column as global variable

## Synchronicity Analysis

Functions for analyzing synchronous feeding and drinking behavior
between animals.

- [`matrix_process()`](https://skysheng7.github.io/moo4feed/reference/matrix_process.md)
  : Process matrices and add derived columns
- [`synch_pair_analysis()`](https://skysheng7.github.io/moo4feed/reference/synch_pair_analysis.md)
  : Analyze pair-wise co-occurrence patterns
- [`synch_neighbor_analysis()`](https://skysheng7.github.io/moo4feed/reference/synch_neighbor_analysis.md)
  : Analyze pair-wise spatial neighbor patterns
- [`synch_pairs_to_df()`](https://skysheng7.github.io/moo4feed/reference/synch_pairs_to_df.md)
  : Convert synchronicity matrices to pair-level data frame
- [`synch_neighbor_compare()`](https://skysheng7.github.io/moo4feed/reference/synch_neighbor_compare.md)
  : Compare neighbor time to total co-occurrence time

## Example Data

Example datasets and functions for demonstration.

- [`all_fed`](https://skysheng7.github.io/moo4feed/reference/all_fed.md)
  : Cattle feeding behavior and visit record data

- [`all_wat`](https://skysheng7.github.io/moo4feed/reference/all_wat.md)
  : Cattle water drinking behavior and visit record data

- [`clean_feed`](https://skysheng7.github.io/moo4feed/reference/clean_feed.md)
  : Cattle feeding behavior data after quality check and outlier removal

- [`clean_water`](https://skysheng7.github.io/moo4feed/reference/clean_water.md)
  : Cattle drinking behavior data after quality check and outlier
  removal

- [`clean_comb`](https://skysheng7.github.io/moo4feed/reference/clean_comb.md)
  : Combined feeding and drinking behavior data with outliers removed

- [`moo4feed_example()`](https://skysheng7.github.io/moo4feed/reference/moo4feed_example.md)
  :

  Access Example Data Files Shipped with **moo4feed**
