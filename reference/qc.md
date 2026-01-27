# Run full quality-control check on feeder and drinker data

This function performs comprehensive quality control checks on feeder
and drinker data, identifying and handling various data quality issues.

## Usage

``` r
qc(
  feed = NULL,
  water = NULL,
  cfg = qc_config(),
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  dur_col = duration_col2(),
  intake_col = intake_col2(),
  start_weight_col = start_weight_col2(),
  end_weight_col = end_weight_col2(),
  tz = tz2(),
  bins_feed = bins_feed2(),
  bins_wat = bins_wat2(),
  bin_offset = bin_offset2(),
  verbose = TRUE,
  fix_double_detections = TRUE
)
```

## Arguments

- feed:

  A list of daily **feed** data frames named by date, or `NULL` if you
  don't have feeder data

- water:

  A list of daily **water** data frames named by date, or `NULL` if you
  don't have water data

- cfg:

  A configuration list created by
  [`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md).

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- end_col:

  End time column name (default current global value from
  [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md))

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- dur_col:

  Duration column name (default current global value from
  [`duration_col2()`](https://skysheng7.github.io/moo4feed/reference/duration_col2.md))

- intake_col:

  Intake column name (default current global value from
  [`intake_col2()`](https://skysheng7.github.io/moo4feed/reference/intake_col2.md))

- start_weight_col:

  Start weight column name (default current global value from
  [`start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/start_weight_col2.md))

- end_weight_col:

  End weight column name (default current global value from
  [`end_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/end_weight_col2.md))

- tz:

  Time zone string for date-time operations

- bins_feed:

  Integer vector of feed bins (default current global value from
  [`bins_feed2()`](https://skysheng7.github.io/moo4feed/reference/bins_feed2.md))

- bins_wat:

  Integer vector of water bins (default current global value from
  [`bins_wat2()`](https://skysheng7.github.io/moo4feed/reference/bins_wat2.md))

- bin_offset:

  Numeric bin offset (default current global value from
  [`bin_offset2()`](https://skysheng7.github.io/moo4feed/reference/bin_offset2.md))

- verbose:

  Logical. If TRUE, print details of data where errors were detected

- fix_double_detections:

  Logical. If TRUE, applies corrections to double detection issues by
  adjusting end times of overlapping bouts. Default is TRUE.

## Value

A list with four elements:

- `warnings`:

  a tidy data frame with one row per day and one column per warning
  code.

- `feed`:

  cleaned feed list (or `NULL`).

- `water`:

  cleaned water list (or `NULL`).

- `combined`:

  merged feed + water list (after all fixes).

## Details

1.  Combines feed and water data (if both are provided)

2.  Checks for expected number of cows and reports missing animals

3.  Detects and optionally fixes double detections (when the same cow is
    detected at different bins at the same time)

4.  Flags negative values in duration, intake, or weight measurements

5.  Removes records with negative durations or intakes

6.  Identifies abnormally large feed/water intakes based on thresholds

7.  Flags cows that did not feed or drink after noon (potentially lost
    ear tag or due to illness)

8.  Flgas bins with very low traffic or no visits at all

Use this function after initial data processing with
[`process_all_feed()`](https://skysheng7.github.io/moo4feed/reference/process_all_feed.md)
and
[`process_all_water()`](https://skysheng7.github.io/moo4feed/reference/process_all_water.md)
to clean your data and prepare it for analysis. Customize thresholds
using
[`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md)
to match your specific study parameters.

## Examples

``` r
cfg <- qc_config(high_dur_feed = 2500, low_visit_threshold = 5, total_cows_expected=48)
out <- qc(feed = all_fed, water = all_wat, cfg = cfg, verbose = FALSE)
out$warnings
#> # A tibble: 2 × 16
#>   date       total_cows missing_cow double_detection_bins negative_visit_bins
#>   <chr>           <int> <chr>       <chr>                 <chr>              
#> 1 2020-10-31         47 Yes         30                    6; 12; 13          
#> 2 2020-11-01         47 Yes         3; 5; 6; 18           6; 12; 13; 30      
#> # ℹ 11 more variables: cows_disappeared_after_noon <chr>,
#> #   bins_never_visited <chr>, bins_low_traffic <chr>, long_dur_feeder <chr>,
#> #   large_intake_feed_visit <chr>, low_daily_feed_intake_cows <chr>,
#> #   high_daily_feed_intake_cows <chr>, long_dur_drinker <chr>,
#> #   large_intake_water_visit <chr>, low_daily_water_intake_cows <chr>,
#> #   high_daily_water_intake_cows <chr>
```
