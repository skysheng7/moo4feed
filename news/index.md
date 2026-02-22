# Changelog

## moo4feed 0.1.0

### Initial Release

This is the first public release of moo4feed, an R package for deriving
individual animal traits from feeding and drinking data.

#### Features

- **Data Processing Pipeline**: Core functions for reading and
  processing feeding and drinking data
  - [`read_data_safely()`](https://skysheng7.github.io/moo4feed/reference/read_data_safely.md),
    [`process_all_feed()`](https://skysheng7.github.io/moo4feed/reference/process_all_feed.md),
    [`process_all_water()`](https://skysheng7.github.io/moo4feed/reference/process_all_water.md)
  - [`process_feeder()`](https://skysheng7.github.io/moo4feed/reference/process_feeder.md),
    [`process_water()`](https://skysheng7.github.io/moo4feed/reference/process_water.md)
- **Data Quality Checks**: Comprehensive quality control functions
  - [`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md),
    [`qc()`](https://skysheng7.github.io/moo4feed/reference/qc.md) for
    configurable quality checks
  - [`knn_outlier_detection()`](https://skysheng7.github.io/moo4feed/reference/knn_outlier_detection.md),
    [`knn_clean_feed()`](https://skysheng7.github.io/moo4feed/reference/knn_clean_feed.md),
    [`knn_clean_water()`](https://skysheng7.github.io/moo4feed/reference/knn_clean_water.md)
  - [`viz_outliers()`](https://skysheng7.github.io/moo4feed/reference/viz_outliers.md)
    for visualization
- **Data Cleaning and Preparation**
  - Data filtering:
    [`delete_rows()`](https://skysheng7.github.io/moo4feed/reference/delete_rows.md),
    [`keep_bins()`](https://skysheng7.github.io/moo4feed/reference/keep_bins.md),
    [`rename_bins()`](https://skysheng7.github.io/moo4feed/reference/rename_bins.md)
  - Time adjustments:
    [`daylight_saving_adjust()`](https://skysheng7.github.io/moo4feed/reference/daylight_saving_adjust.md),
    [`get_dst_switch_info()`](https://skysheng7.github.io/moo4feed/reference/get_dst_switch_info.md)
  - File management:
    [`file_name_processing()`](https://skysheng7.github.io/moo4feed/reference/file_name_processing.md),
    [`compare_files()`](https://skysheng7.github.io/moo4feed/reference/compare_files.md),
    [`get_date_range()`](https://skysheng7.github.io/moo4feed/reference/get_date_range.md)
  - Data integration:
    [`combine_feed_water()`](https://skysheng7.github.io/moo4feed/reference/combine_feed_water.md),
    [`merge_list_df()`](https://skysheng7.github.io/moo4feed/reference/merge_list_df.md),
    [`unmerge_data()`](https://skysheng7.github.io/moo4feed/reference/unmerge_data.md)
- **Behavioral Analysis**
  - Feed and water summary:
    [`feed_water_summary()`](https://skysheng7.github.io/moo4feed/reference/feed_water_summary.md)
  - Bin visit analysis:
    [`unique_bin_visits()`](https://skysheng7.github.io/moo4feed/reference/unique_bin_visits.md)
  - Non-nutritive visit analysis:
    [`calculate_non_nutritive_visits()`](https://skysheng7.github.io/moo4feed/reference/calculate_non_nutritive_visits.md),
    [`calculate_no_feed_visits()`](https://skysheng7.github.io/moo4feed/reference/calculate_no_feed_visits.md)
  - Feed addition detection:
    [`detect_feed_additions()`](https://skysheng7.github.io/moo4feed/reference/detect_feed_additions.md),
    [`calculate_feed_availability()`](https://skysheng7.github.io/moo4feed/reference/calculate_feed_availability.md)
  - Replacement behavior:
    [`record_replacement_days()`](https://skysheng7.github.io/moo4feed/reference/record_replacement_days.md)
- **Meal Clustering**
  - Meal interval calculation:
    [`meal_interval()`](https://skysheng7.github.io/moo4feed/reference/meal_interval.md)
  - Visualization tools:
    [`viz_eps_percentile()`](https://skysheng7.github.io/moo4feed/reference/viz_eps_percentile.md),
    [`viz_eps_gmm()`](https://skysheng7.github.io/moo4feed/reference/viz_eps_gmm.md),
    [`viz_meal_clusters()`](https://skysheng7.github.io/moo4feed/reference/viz_meal_clusters.md)
  - Clustering functions:
    [`cluster_meals()`](https://skysheng7.github.io/moo4feed/reference/cluster_meals.md),
    [`meal_label_visits()`](https://skysheng7.github.io/moo4feed/reference/meal_label_visits.md),
    [`merge_cluster_results()`](https://skysheng7.github.io/moo4feed/reference/merge_cluster_results.md)
  - Within-meal analysis:
    [`meal_non_nutritive_summary()`](https://skysheng7.github.io/moo4feed/reference/meal_non_nutritive_summary.md),
    [`meal_replacement_roles()`](https://skysheng7.github.io/moo4feed/reference/meal_replacement_roles.md),
    [`meal_replacement_roles_summary()`](https://skysheng7.github.io/moo4feed/reference/meal_replacement_roles_summary.md)
- **Synchronicity Analysis**
  - Matrix processing:
    [`matrix_process()`](https://skysheng7.github.io/moo4feed/reference/matrix_process.md)
  - Pair analysis:
    [`synch_pair_analysis()`](https://skysheng7.github.io/moo4feed/reference/synch_pair_analysis.md),
    [`synch_pairs_to_df()`](https://skysheng7.github.io/moo4feed/reference/synch_pairs_to_df.md)
  - Neighbor analysis:
    [`synch_neighbor_analysis()`](https://skysheng7.github.io/moo4feed/reference/synch_neighbor_analysis.md),
    [`synch_neighbor_compare()`](https://skysheng7.github.io/moo4feed/reference/synch_neighbor_compare.md)
- **Configuration System**
  - Global variable management with getter/setter functions
  - [`set_global_cols()`](https://skysheng7.github.io/moo4feed/reference/set_global_cols.md)
    for batch configuration
  - Individual setters:
    [`set_tz2()`](https://skysheng7.github.io/moo4feed/reference/set_tz2.md),
    [`set_id_col2()`](https://skysheng7.github.io/moo4feed/reference/set_id_col2.md),
    [`set_start_col2()`](https://skysheng7.github.io/moo4feed/reference/set_start_col2.md),
    etc.
- **Example Data**
  - `all_fed`, `all_wat`: Raw example datasets
  - `clean_feed`, `clean_water`, `clean_comb`: Cleaned example datasets
  - [`moo4feed_example()`](https://skysheng7.github.io/moo4feed/reference/moo4feed_example.md):
    Function to access package example files
- **Documentation**
  - 8 comprehensive vignettes covering the complete workflow
  - Package website with full documentation:
    <https://skysheng7.github.io/moo4feed/>
  - Code coverage tracking with codecov
  - Continuous integration with GitHub Actions

#### Infrastructure

- MIT license
- Comprehensive test suite with testthat
- GitHub Actions workflows for R CMD check, test coverage, and pkgdown
  deployment
- Semantic versioning adopted
