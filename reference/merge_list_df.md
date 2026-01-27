# Merge a List of Data Frames

Takes a list of data frames and row-binds them into a single
consolidated master data frame.

## Usage

``` r
merge_list_df(data_list)
```

## Arguments

- data_list:

  A list of data frames to merge.

## Value

A single merged data frame.

## Examples

``` r
data_list <- list(
  data.frame(cow = 1:2, feed = c(10, 20)),
  data.frame(cow = 3:4, feed = c(30, 40))
)

merge_list_df(data_list)
#>   cow feed
#> 1   1   10
#> 2   2   20
#> 3   3   30
#> 4   4   40
```
