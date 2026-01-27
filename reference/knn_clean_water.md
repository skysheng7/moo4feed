# Process multiple days of water data and remove outliers using KNN

Process multiple days of water data and remove outliers using KNN

## Usage

``` r
knn_clean_water(
  water_data,
  k = 50,
  threshold_percentile = 99.9,
  custom_scaling = list(rate = 20, intake = 1, duration = 0.01),
  intake_col = intake_col2(),
  duration_col = duration_col2(),
  remove_outliers = FALSE,
  date_col = "date"
)
```

## Arguments

- water_data:

  A list of daily water data frames or a single data frame.

- k:

  Integer. Number of nearest neighbors to consider (default: 50). Will
  be automatically adjusted if it exceeds the number of rows in the
  data.

- threshold_percentile:

  Numeric. Percentile threshold for outlier detection. Points with
  average distances above this percentile are considered outliers. Must
  be between 0 and 100. Default is 99.

- custom_scaling:

  A named list with scaling factors for input variables. Default is
  NULL, which means no scaling is applied (all factors = 1).

- intake_col:

  Character. Name of the column containing intake data (default: from
  global_var.R).

- duration_col:

  Character. Name of the column containing duration data (default: from
  global_var.R).

- remove_outliers:

  Logical. Whether to remove outliers from the data frame.

- date_col:

  Character. Name of the date column if water_data is a list that needs
  to be unmerged (default: "date").

## Value

If input is a list: a list of data frames with outliers detected. If
input is a data frame: a data frame with outliers detected. If
remove_outliers=TRUE, returns data with outliers removed.
