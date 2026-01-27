# Combined feeding and drinking behavior data with outliers removed

A fully cleaned dataset containing both feeding and drinking behavior
data for cattle over a two-day period (2020-10-31 to 2020-11-01). This
dataset is the result of applying quality control procedures and
KNN-based outlier removal to both feed and water data, then combining
them into a single dataset for integrated analysis.

## Usage

``` r
clean_comb
```

## Format

A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
with each data frame containing the following 11 variables:

- transponder:

  integer, unique electronic ID for each bin

- cow:

  integer, animal ID number

- bin:

  numeric, bin location number (feed or water bin)

- start:

  POSIXct, timestamp when the visit started

- end:

  POSIXct, timestamp when the visit ended

- duration:

  integer, duration of the visit in seconds

- start_weight:

  numeric, weight of feed/water at start of visit

- end_weight:

  numeric, weight of feed/water at end of visit

- intake:

  numeric, amount consumed (kg or L) during the visit

- date:

  Date, calendar date of the visit

- intake:

  numeric, how fast the cow was eating/drinking (kg/s or L/s)

## Source

[clean_feed](https://skysheng7.github.io/moo4feed/reference/clean_feed.md)
and
[clean_water](https://skysheng7.github.io/moo4feed/reference/clean_water.md)
after KNN outlier removal

## Details

This dataset represents the final, fully cleaned dataset after applying
multiple layers of quality control. First, the
[`qc()`](https://skysheng7.github.io/moo4feed/reference/qc.md) function
was used to fix basic issues like double detections and negative values.
Then, advanced outlier detection using K-Nearest Neighbors (KNN) was
applied through the
[`knn_clean_feed()`](https://skysheng7.github.io/moo4feed/reference/knn_clean_feed.md)
and
[`knn_clean_water()`](https://skysheng7.github.io/moo4feed/reference/knn_clean_water.md)
functions to remove improbable data points. Finally, the cleaned feed
and water datasets were combined using
[`combine_feed_water()`](https://skysheng7.github.io/moo4feed/reference/combine_feed_water.md).
This dataset provides a comprehensive view of both feeding and drinking
behaviors in a single, analysis-ready format.

The KNN outlier detection emphasizes on removing data points with high
rate and intake, and do not punish too much for data points with long
duration. Because based on our experience, it's likely for a cow to have
very long durations per visit, but very unlikely to have large intake in
a short time, so we flagged outliers to catch visits with large intake
and high rate.

## Examples

``` r
# Access combined data for the first day
first_day <- clean_comb[["2020-10-31"]]

head(first_day)
#> # A tibble: 6 × 11
#>   transponder   cow   bin start               end                 duration
#>         <int> <int> <dbl> <dttm>              <dttm>                 <dbl>
#> 1    12448407  6020     1 2020-10-31 00:26:12 2020-10-31 00:27:36       84
#> 2    11954014  4044     1 2020-10-31 01:17:43 2020-10-31 01:22:13      270
#> 3    11954042  4072     1 2020-10-31 01:37:30 2020-10-31 01:37:52       22
#> 4    12200070  5124     1 2020-10-31 06:05:49 2020-10-31 06:07:52      123
#> 5    12448407  6020     1 2020-10-31 06:08:02 2020-10-31 06:09:44      102
#> 6    21292850  6069     1 2020-10-31 06:09:55 2020-10-31 06:12:05      130
#> # ℹ 5 more variables: start_weight <dbl>, end_weight <dbl>, intake <dbl>,
#> #   date <date>, rate <dbl>
```
