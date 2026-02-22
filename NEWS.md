# moo4feed 0.1.0

## Initial Release

This is the first public release of moo4feed, an R package for deriving individual animal traits from feeding and drinking data.

### Features

* **Data Processing Pipeline**: Core functions for reading and processing feeding and drinking data
  * `read_data_safely()`, `process_all_feed()`, `process_all_water()`
  * `process_feeder()`, `process_water()`

* **Data Quality Checks**: Comprehensive quality control functions
  * `qc_config()`, `qc()` for configurable quality checks
  * `knn_outlier_detection()`, `knn_clean_feed()`, `knn_clean_water()`
  * `viz_outliers()` for visualization

* **Data Cleaning and Preparation**
  * Data filtering: `delete_rows()`, `keep_bins()`, `rename_bins()`
  * Time adjustments: `daylight_saving_adjust()`, `get_dst_switch_info()`
  * File management: `file_name_processing()`, `compare_files()`, `get_date_range()`
  * Data integration: `combine_feed_water()`, `merge_list_df()`, `unmerge_data()`

* **Behavioral Analysis**
  * Feed and water summary: `feed_water_summary()`
  * Bin visit analysis: `unique_bin_visits()`
  * Non-nutritive visit analysis: `calculate_non_nutritive_visits()`, `calculate_no_feed_visits()`
  * Feed addition detection: `detect_feed_additions()`, `calculate_feed_availability()`
  * Replacement behavior: `record_replacement_days()`

* **Meal Clustering**
  * Meal interval calculation: `meal_interval()`
  * Visualization tools: `viz_eps_percentile()`, `viz_eps_gmm()`, `viz_meal_clusters()`
  * Clustering functions: `cluster_meals()`, `meal_label_visits()`, `merge_cluster_results()`
  * Within-meal analysis: `meal_non_nutritive_summary()`, `meal_replacement_roles()`, `meal_replacement_roles_summary()`

* **Synchronicity Analysis**
  * Matrix processing: `matrix_process()`
  * Pair analysis: `synch_pair_analysis()`, `synch_pairs_to_df()`
  * Neighbor analysis: `synch_neighbor_analysis()`, `synch_neighbor_compare()`

* **Configuration System**
  * Global variable management with getter/setter functions
  * `set_global_cols()` for batch configuration
  * Individual setters: `set_tz2()`, `set_id_col2()`, `set_start_col2()`, etc.

* **Example Data**
  * `all_fed`, `all_wat`: Raw example datasets
  * `clean_feed`, `clean_water`, `clean_comb`: Cleaned example datasets
  * `moo4feed_example()`: Function to access package example files

* **Documentation**
  * 8 comprehensive vignettes covering the complete workflow
  * Package website with full documentation: https://skysheng7.github.io/moo4feed/
  * Code coverage tracking with codecov
  * Continuous integration with GitHub Actions

### Infrastructure

* MIT license
* Comprehensive test suite with testthat
* GitHub Actions workflows for R CMD check, test coverage, and pkgdown deployment
* Semantic versioning adopted
