# Calculate Percentage of Feed Remaining at Each Visit

Calculates the percentage of feed remaining at each visit based on the
most recent feed addition for that specific bin. Requires per-bin feed
addition data from
[`detect_feed_additions()`](https://skysheng7.github.io/moo4feed/reference/detect_feed_additions.md).
Returns both visit-level data with feed percentages and daily summary
statistics.

## Usage

``` r
calculate_feed_availability(
  visit_data,
  feed_addition_data,
  id_col = id_col2(),
  bin_col = bin_col2(),
  start_col = start_col2(),
  start_weight_col = start_weight_col2()
)
```

## Arguments

- visit_data:

  A named list of daily data frames or a single data frame containing
  visit records.

- feed_addition_data:

  A named list of daily data frames or a single data frame containing
  feed addition events from
  [`detect_feed_additions()`](https://skysheng7.github.io/moo4feed/reference/detect_feed_additions.md).
  Must have `aggregate_all_bin = FALSE` to get per-bin feed additions.

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- start_weight_col:

  Start weight column name (default current global value from
  [`start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/start_weight_col2.md))

## Value

A list with two elements:

- `visits` - Visit-level data with added columns:

  - `feed_addition_time` - Time when feed was added to this bin

  - `feed_added_weight` - Weight of feed added to this bin (kg)

  - `pct_feed_remaining` - Percentage of feed remaining at visit start

- `daily_summary` - Named list (or data frame) with columns:

  - `date` - Date

  - `[id_col]` - Animal identifier

  - `mean_pct_feed_remaining` - Average percentage across visits

  - `median_pct_feed_remaining` - Median percentage across visits

  - `sd_pct_feed_remaining` - Standard deviation of percentage

  - `total_visits_analyzed` - Number of visits analyzed

## Details

The function matches each visit to the most recent feed addition for
that specific bin. Feed percentages are calculated as:
`(start_weight / bin_weight_after_fill) * 100`, capped at 100%.

The `bin_weight_after_fill` represents the total bin weight immediately
after feed was added, which accounts for any residual feed that was
already in the bin. This is more accurate than using just the amount of
feed added. For example, if a bin had 10kg residual and the farmer added
25kg in total (possibly across multiple rapid additions),
`bin_weight_after_fill` would be 35kg, not 25kg.

**Important**: When calling
[`detect_feed_additions()`](https://skysheng7.github.io/moo4feed/reference/detect_feed_additions.md),
multiple rapid additions to the same bin (within `max_bin_time_gap`) are
automatically aggregated into a single event. The
`bin_weight_after_fill` from that aggregation represents the bin weight
after the final addition, which is exactly what this function needs for
accurate percentage calculations.

Visits that occur before any feed addition to that bin will have NA
values.

## Examples

``` r
# Create sample visit data
visits <- data.frame(
  date = "2024-01-01",
  cow = c("A", "A", "B", "B"),
  bin = c(1, 1, 2, 2),
  start = as.POSIXct(c(
    "2024-01-01 08:00:00",
    "2024-01-01 10:00:00",
    "2024-01-01 08:05:00",
    "2024-01-01 10:05:00"
  ), tz = "UTC"),
  end = as.POSIXct(c(
    "2024-01-01 08:10:00",
    "2024-01-01 10:10:00",
    "2024-01-01 08:15:00",
    "2024-01-01 10:15:00"
  ), tz = "UTC"),
  start_weight = c(50, 45, 50, 43),
  end_weight = c(45, 40, 43, 38)
)

# Detect per-bin feed additions
feed_additions <- detect_feed_additions(
  data = visits,
  aggregate_all_bin = FALSE
)

# Calculate feed availability at each visit
availability <- calculate_feed_availability(
  visit_data = visits,
  feed_addition_data = feed_additions
)

# Access visit-level data
visit_pct <- availability$visits

# Access daily summaries
daily_pct <- availability$daily_summary
```
