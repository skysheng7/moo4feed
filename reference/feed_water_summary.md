# Summarize and check for abnormal feed & water intake

This function summarizes daily feed and water intake, visit duration,
and counts, and updates intake warnings (if input warning dataframe
`warn` is not `NULL`). If you don't want to update warnings, set
`warn = NULL`, and only the summary will be returned.

## Usage

``` r
feed_water_summary(
  feed = NULL,
  water = NULL,
  warn = NULL,
  cfg = qc_config(),
  id_col = id_col2(),
  intake_col = intake_col2(),
  dur_col = duration_col2()
)
```

## Arguments

- feed:

  Either a data frame containing all feed visits across days, or a list
  of data frames grouped by date. If providing a list, each data frame
  must have a 'date' column.

- water:

  Either a data frame containing all water visits across days, or a list
  of data frames grouped by date. If providing a list, each data frame
  must have a 'date' column.

- warn:

  Warning data frame to update

- cfg:

  A configuration list created by
  [`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md).

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- intake_col:

  Intake column name (default current global value from
  [`intake_col2()`](https://skysheng7.github.io/moo4feed/reference/intake_col2.md))

- dur_col:

  Duration column name (default current global value from
  [`duration_col2()`](https://skysheng7.github.io/moo4feed/reference/duration_col2.md))

## Value

A list of:

- `summary`: merged data frame of daily intake, duration, and visit
  counts

- `warn`: updated warnings

## Examples

``` r
# Example with single data frames
feed_df <- tibble::tibble(
  date = as.Date(c("2024-01-01", "2024-01-01")),
  cow = c("A", "B"),
  intake = c(20, 80),
  duration = c(200, 300)
)

water_df <- tibble::tibble(
  date = as.Date(c("2024-01-01", "2024-01-01")),
  cow = c("A", "B"),
  intake = c(50, 200),
  duration = c(100, 150)
)

# Example with lists of data frames
feed_list <- list(
  "2024-01-01" = tibble::tibble(
    date = as.Date("2024-01-01"),
    cow = c("A", "B"),
    intake = c(20, 80),
    duration = c(200, 300)
  ),
  "2024-01-02" = tibble::tibble(
    date = as.Date("2024-01-02"),
    cow = c("A", "C"),
    intake = c(25, 85),
    duration = c(210, 310)
  )
)

water_list <- list(
  "2024-01-01" = tibble::tibble(
    date = as.Date("2024-01-01"),
    cow = c("A", "B"),
    intake = c(50, 200),
    duration = c(100, 150)
  ),
  "2024-01-02" = tibble::tibble(
    date = as.Date("2024-01-02"),
    cow = c("A", "C"),
    intake = c(55, 210),
    duration = c(110, 160)
  )
)

warn <- tibble::tibble(
  date = as.Date(c("2024-01-01", "2024-01-02")),
  low_daily_feed_intake_cows = NA_character_,
  high_daily_feed_intake_cows = NA_character_,
  low_daily_water_intake_cows = NA_character_,
  high_daily_water_intake_cows = NA_character_
)

cfg <- qc_config()

# Using single data frames
feed_water_summary(feed_df, 
                  water_df, 
                  warn, 
                  cfg, 
                  id_col = "cow", 
                  intake_col = "intake", 
                  dur_col = "duration")
#> $summary
#> # A tibble: 2 × 8
#>   date       cow   feed_intake feed_duration feed_visits water_intake
#>   <date>     <chr>       <dbl>         <dbl>       <int>        <dbl>
#> 1 2024-01-01 A              20           200           1           50
#> 2 2024-01-01 B              80           300           1          200
#> # ℹ 2 more variables: water_duration <dbl>, water_visits <int>
#> 
#> $warn
#> # A tibble: 2 × 5
#>   date       low_daily_feed_intake_cows high_daily_feed_intake_cows
#>   <date>     <chr>                      <chr>                      
#> 1 2024-01-01 A, 20                      B, 80                      
#> 2 2024-01-02 NA                         NA                         
#> # ℹ 2 more variables: low_daily_water_intake_cows <chr>,
#> #   high_daily_water_intake_cows <chr>
#> 
                  
# Using lists of data frames
feed_water_summary(feed_list, 
                  water_list, 
                  warn, 
                  cfg, 
                  id_col = "cow", 
                  intake_col = "intake", 
                  dur_col = "duration")
#> $summary
#> # A tibble: 4 × 8
#>   date       cow   feed_intake feed_duration feed_visits water_intake
#>   <date>     <chr>       <dbl>         <dbl>       <int>        <dbl>
#> 1 2024-01-01 A              20           200           1           50
#> 2 2024-01-01 B              80           300           1          200
#> 3 2024-01-02 A              25           210           1           55
#> 4 2024-01-02 C              85           310           1          210
#> # ℹ 2 more variables: water_duration <dbl>, water_visits <int>
#> 
#> $warn
#> # A tibble: 2 × 5
#>   date       low_daily_feed_intake_cows high_daily_feed_intake_cows
#>   <date>     <chr>                      <chr>                      
#> 1 2024-01-01 A, 20                      B, 80                      
#> 2 2024-01-02 A, 25                      C, 85                      
#> # ℹ 2 more variables: low_daily_water_intake_cows <chr>,
#> #   high_daily_water_intake_cows <chr>
#> 
```
