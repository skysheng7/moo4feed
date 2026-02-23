# Process multiple days of feeding data and remove outliers using KNN

Process multiple days of feeding data and remove outliers using KNN

## Usage

``` r
knn_clean_feed(
  feed_data,
  k = 50,
  threshold_percentile = 99.936,
  custom_scaling = NULL,
  intake_col = intake_col2(),
  duration_col = duration_col2(),
  remove_outliers = FALSE,
  date_col = "date"
)
```

## Arguments

- feed_data:

  A list of daily feeding data frames or a single data frame.

- k:

  Integer. Number of nearest neighbors to consider (default: 50). Will
  be automatically adjusted if it exceeds the number of rows in the
  data.

- threshold_percentile:

  Numeric. Percentile threshold for outlier detection. Points with
  average distances above this percentile are considered outliers. Must
  be between 0 and 100. Default is 99.

- custom_scaling:

  A named list with scaling factors for input variables (e.g., list(rate
  = 10, intake = 2, duration = 0.5)). If NULL (default), min-max scaling
  is applied to normalize all variables to a 0-1 range, ensuring equal
  contribution to distance calculations.

- intake_col:

  Character. Name of the column containing intake data (default: from
  global_var.R).

- duration_col:

  Character. Name of the column containing duration data (default: from
  global_var.R).

- remove_outliers:

  Logical. Whether to remove outliers from the data frame.

- date_col:

  Character. Name of the date column if feed_data is a list that needs
  to be unmerged (default: "date").

## Value

If input is a list: a list of data frames with outliers detected. If
input is a data frame: a data frame with outliers detected. If
remove_outliers=TRUE, returns data with outliers removed.

## Details

When `custom_scaling` is NULL, the function automatically applies
min-max scaling to normalize all variables (duration, intake, rate) to a
0-1 range. This ensures equal contribution of each variable to the
distance calculation in the KNN algorithm. When `custom_scaling` is
provided, those scaling factors are used instead.
