# Keep only rows for specified bins

Filters to rows where the column recording bin IDs matches one of the
values in `bins`. This is useful when your raw data files contains
recordings from multiple bins including bins that are not used in your
study. You can use this function to keep only data recorded from bins
included in your study.

## Usage

``` r
keep_bins(df, bins, bin_col = bin_col2())
```

## Arguments

- df:

  A data frame containing a column of bin IDs.

- bins:

  Which bins are included in your study for analysis? This should be a
  numeric vector of bin IDs to keep. You can supply individual values
  (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).

- bin_col:

  What's the name of the column recording your bin ID? This is a single
  string naming the column of bin IDs (default: `"bin"`).

## Value

A data frame identical to `df` but only with rows where
`df[[bin_col]] %in% bins`.

## Examples

``` r
df <- data.frame(Bin = 1:5, Value = 11:15)
print(df)
#>   Bin Value
#> 1   1    11
#> 2   2    12
#> 3   3    13
#> 4   4    14
#> 5   5    15
keep_bins(df, bins = 2:4, bin_col = "Bin")
#>   Bin Value
#> 2   2    12
#> 3   3    13
#> 4   4    14
#   Bin Value
# 2   2    12
# 3   3    13
# 4   4    14

keep_bins(df, bins = c(1, 5), bin_col = "Bin")
#>   Bin Value
#> 1   1    11
#> 5   5    15
#   Bin Value
# 1   1    11
# 5   5    15
```
