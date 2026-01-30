# Detect Feed Addition Times

Detects when feed bins are refilled by identifying significant weight
increases between consecutive visits at the same bin.

The function performs aggregation in two stages:

**Stage 1: Within-bin aggregation (ALWAYS performed)**: When a farmer
adds feed to the same bin multiple times in quick succession (within
`max_bin_time_gap` seconds), these additions are automatically grouped
into a single feed event per bin. The returned `weight_increase` is the
total amount added, and `bin_weight_after_fill` is the bin weight after
the final addition. This stage always runs regardless of the
`aggregate_all_bin` setting.

**Stage 2: Across-bin aggregation (only when
`aggregate_all_bin = TRUE`)**: After within-bin aggregation, if
`aggregate_all_bin = TRUE`, the function groups individual bin additions
into multi-bin feed events when multiple bins are refilled within a time
window.

## Usage

``` r
detect_feed_additions(
  data,
  min_weight_increase = 5,
  max_bin_time_gap = 3600,
  min_bins_for_group = 3,
  aggregate_all_bin = TRUE,
  bin_col = bin_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  start_weight_col = start_weight_col2(),
  end_weight_col = end_weight_col2()
)
```

## Arguments

- data:

  A named list of daily data frames or a single data frame containing
  visit records with bin weights.

- min_weight_increase:

  Numeric. Minimum weight increase (in kg) to consider as a feed
  addition. Default is 5 kg.

- max_bin_time_gap:

  Numeric. Maximum time gap (in seconds) between bin additions to group
  them into the same feed event. Default is 3600 seconds (1 hour).

- min_bins_for_group:

  Integer. Minimum number of bins that must show feed additions within
  the time window to classify as an "all-bin" feed event. Default is 3.

- aggregate_all_bin:

  Logical. If TRUE, groups individual bin additions into multi-bin feed
  events. If FALSE, returns individual bin additions. Default is TRUE.

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- end_col:

  End time column name (default current global value from
  [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md))

- start_weight_col:

  Start weight column name (default current global value from
  [`start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/start_weight_col2.md))

- end_weight_col:

  End weight column name (default current global value from
  [`end_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/end_weight_col2.md))

## Value

If `aggregate_all_bin = FALSE`, returns a data frame (or named list of
data frames) with columns:

- `date` - Date of the feed addition

- `[bin_col]` - Bin identifier

- `time` - Timestamp of the FIRST detected addition in this event

- `weight_increase` - Total amount of feed added across all additions in
  this event (kg). If multiple rapid additions occurred, this is the
  sum.

- `bin_weight_after_fill` - Total bin weight after the FINAL addition in
  this event (kg). This includes any residual feed that was already in
  the bin before additions began.

If `aggregate_all_bin = TRUE`, returns a data frame (or named list of
data frames) with columns:

- `date` - Date of the feed event

- `event_id` - Unique identifier for the feed event

- `event_start` - Earliest addition time in the event

- `event_end` - Latest addition time in the event

- `bins_filled` - Number of bins refilled in the event

- `avg_weight_increase` - Average feed added across bins (kg)

## Note

**Multiple rapid additions to the same bin**: When a farmer adds feed to
the same bin multiple times in quick succession (e.g., bin 1 gets 10kg,
then 10 seconds later 5kg, then 10 seconds later another 10kg), these
are **always** automatically grouped into one feed event per bin,
regardless of the `aggregate_all_bin` setting. The
`bin_weight_after_fill` will be the weight after the final addition
(e.g., 35kg if the bin had 10kg residual before the additions), not just
the sum of what was added (25kg). This within-bin aggregation happens
BEFORE any across-bin aggregation.

The function correctly handles cases where different animals visit the
same bin. Feed additions are detected based on weight changes between
consecutive visits at each bin, regardless of which animal made the
visits.

## Examples

``` r
# Create sample visit data
visits <- data.frame(
  date = "2024-01-01",
  cow = c("A", "A", "B", "B", "C"),
  bin = c(1, 1, 2, 2, 3),
  start = as.POSIXct(c(
    "2024-01-01 08:00:00",
    "2024-01-01 09:00:00",
    "2024-01-01 08:05:00",
    "2024-01-01 09:05:00",
    "2024-01-01 08:10:00"
  ), tz = "UTC"),
  end = as.POSIXct(c(
    "2024-01-01 08:10:00",
    "2024-01-01 09:10:00",
    "2024-01-01 08:15:00",
    "2024-01-01 09:15:00",
    "2024-01-01 08:20:00"
  ), tz = "UTC"),
  start_weight = c(50, 45, 50, 43, 50),
  end_weight = c(45, 40, 43, 38, 45)
)

# Detect per-bin additions (with within-bin aggregation)
additions <- detect_feed_additions(
  data = visits,
  min_weight_increase = 5,
  max_bin_time_gap = 600,
  aggregate_all_bin = FALSE
)

# Detect all-bin feed events (aggregated across multiple bins)
feed_events <- detect_feed_additions(
  data = visits,
  min_weight_increase = 5,
  max_bin_time_gap = 3600,
  min_bins_for_group = 2,
  aggregate_all_bin = TRUE
)
```
