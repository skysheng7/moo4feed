# Analyze Bin Visit Patterns For Each Cow

This function analyzes how cows interact with feed and water bins across
multiple days. For each day and each cow, it calculates:

- The number of unique feed bins visited

- The number of unique water bins visited

- The total number of unique bins visited

## Usage

``` r
unique_bin_visits(
  feed = NULL,
  water = NULL,
  id_col = id_col2(),
  bin_col = bin_col2(),
  bin_offset = bin_offset2(),
  bins_feed = bins_feed2(),
  bins_wat = bins_wat2(),
  return_list = FALSE
)
```

## Arguments

- feed:

  A list of daily **feed** data frames named by date, or `NULL` if you
  don't have feeder data

- water:

  A list of daily **water** data frames named by date, or `NULL` if you
  don't have water data

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- bin_offset:

  Numeric bin offset (default current global value from
  [`bin_offset2()`](https://skysheng7.github.io/moo4feed/reference/bin_offset2.md))

- bins_feed:

  Integer vector of feed bins (default current global value from
  [`bins_feed2()`](https://skysheng7.github.io/moo4feed/reference/bins_feed2.md))

- bins_wat:

  Integer vector of water bins (default current global value from
  [`bins_wat2()`](https://skysheng7.github.io/moo4feed/reference/bins_wat2.md))

- return_list:

  Logical. Whether to return a list of dataframes or a single dataframe.

## Value

A dataframe with columns for date, cow ID, number of unique feed bins
visited, number of unique water bins visited, and total unique bins
visited.

## Examples

``` r
# With separate feed and water data lists
bin_visit_stats <- unique_bin_visits(feed = all_fed, 
                                     water = all_wat,
                                     id_col = "cow",
                                     bin_col = "bin",
                                     bin_offset = 100,
                                     bins_feed = 1:30,
                                     bins_wat = 1:30,
                                     return_list = FALSE)

# View the results
head(bin_visit_stats)
#>                    date  cow unique_feed_bins_visited unique_water_bins_visited
#> 2020-10-31.1 2020-10-31 2074                       20                         4
#> 2020-10-31.2 2020-10-31 3150                       22                         4
#> 2020-10-31.3 2020-10-31 4001                       19                         3
#> 2020-10-31.4 2020-10-31 4044                       25                         4
#> 2020-10-31.5 2020-10-31 4070                       24                         4
#> 2020-10-31.6 2020-10-31 4072                       24                         4
#>              total_bins_visited
#> 2020-10-31.1                 24
#> 2020-10-31.2                 26
#> 2020-10-31.3                 22
#> 2020-10-31.4                 29
#> 2020-10-31.5                 28
#> 2020-10-31.6                 28
```
