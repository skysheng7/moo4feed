# Detect outliers using k-nearest neighbors (KNN) method

This function identifies outliers in feeding or drinking data using the
K-nearest neighbors (KNN) algorithm. It's based on the idea that
outliers will have larger average distances to their k-nearest
neighbors.

## Usage

``` r
knn_outlier_detection(
  df,
  k = 50,
  threshold_percentile = 99,
  custom_scaling = NULL,
  intake_col = intake_col2(),
  duration_col = duration_col2(),
  remove_outliers = FALSE
)
```

## Arguments

- df:

  A data frame containing feeding or drinking data.

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

## Value

A data frame with the same structure as the input, with an additional
column 'outlier' indicating whether each row is an outlier ("Y") or not
("N"). If remove_outliers=TRUE, returns the data frame with outliers
removed and the outlier column dropped.

## Examples

``` r
# Detect outliers in feeding data
cleaned_feed <- knn_outlier_detection(all_fed[[1]], threshold_percentile = 99.936)
#> Warning: NAs or Inf values detected in rate calculation. These rows will be automatically marked as outliers.
cleaned_feed[which(cleaned_feed$outlier == "Y"), ]
#>      transponder  cow bin               start                 end duration
#> 318     12706614 7023  12 2020-10-31 03:45:10 2020-10-31 03:54:15      545
#> 748     12200070 5124   1 2020-10-31 07:36:44 2020-10-31 08:06:42     1798
#> 1761    12706613 7022  26 2020-10-31 13:12:53 2020-10-31 13:12:53        0
#> 3367    12200069 5123  14 2020-10-31 22:12:23 2020-10-31 22:41:59     1776
#>      start_weight end_weight intake       date        rate outlier
#> 318          32.6       15.6   17.0 2020-10-31 0.031192661       Y
#> 748          30.7       25.5    5.2 2020-10-31 0.002892102       Y
#> 1761         26.8       26.9   -0.1 2020-10-31        -Inf       Y
#> 3367         34.1       27.6    6.5 2020-10-31 0.003659910       Y
```
