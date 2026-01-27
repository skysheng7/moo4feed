# Build a configuration list for data quality-control

This helper centralises **every threshold** used by
[`qc()`](https://skysheng7.github.io/moo4feed/reference/qc.md). It lets
you define what should be flagged as *abnormal* in your feed–water data:
extra-long bouts, unrealistically large intakes, bins with too few
visits, etc. Each argument has a sensible default, so you normally
override only the handful you care about—then pass the resulting list to
`qc(cfg = qc_config(...))`.

## Usage

``` r
qc_config(
  high_dur_feed = 2000,
  high_dur_water = 1800,
  large_intake_visit_feed = 8,
  large_intake_visit_water = 30,
  large_intake_rate_feed = 0.008,
  large_intake_rate_water = 0.35,
  low_visit_threshold = 10,
  total_cows_expected = NA,
  low_feed_intake = 35,
  high_feed_intake = 75,
  low_wat_intake = 60,
  high_wat_intake = 180,
  replacement_threshold = 26,
  calibration_error = 0.5,
  ...
)
```

## Arguments

- high_dur_feed:

  Numeric. Seconds that define a *long* feeding visit.

- high_dur_water:

  Numeric. Seconds that define a *long* drinking visit.

- large_intake_visit_feed:

  Numeric. Kilograms that flag a single, unusually large **feed** intake
  event.

- large_intake_visit_water:

  Numeric. Litres that flag a single, unusually large **water** intake
  event.

- large_intake_rate_feed:

  Numeric. Kilograms/s considered a rapid feed-intake rate.

- large_intake_rate_water:

  Numeric. Litres/s considered a rapid water-intake rate.

- low_visit_threshold:

  Integer. Bins visited fewer than this number of times per day are
  flagged as *low traffic*.

- total_cows_expected:

  Integer. Expected herd size; if `NA` (default) the pipeline counts
  unique IDs automatically.

- low_feed_intake:

  Numeric. kg/day. Daily feed intake flagged as *low* for a animal
  (default **35**).

- high_feed_intake:

  Numeric. kg/day. Daily feed intake flagged as *high* for an animal
  (default **75**).

- low_wat_intake:

  Numeric. L/day. Daily water intake flagged as *low* for an animal
  (default **60**).

- high_wat_intake:

  Numeric. L/day. Daily water intake flagged as *high* for an animal
  (default **180**).

- replacement_threshold:

  Numeric. Seconds. Time gap to classify replacement behaviour (default
  **26 s**).

- calibration_error:

  Numeric. Allowed feeder calibration error (default **0.5** kg/L).

- ...:

  Reserved for future or project-specific tweaks. Named elements here
  are appended to the returned list.

## Value

A named list consumed by
[`qc()`](https://skysheng7.github.io/moo4feed/reference/qc.md) and its
internal `qc_*()` helpers.

## Examples

``` r
# Use all defaults
cfg <- qc_config()
cfg
#> $high_dur_feed
#> [1] 2000
#> 
#> $high_dur_water
#> [1] 1800
#> 
#> $large_intake_visit_feed
#> [1] 8
#> 
#> $large_intake_visit_water
#> [1] 30
#> 
#> $large_intake_rate_feed
#> [1] 0.008
#> 
#> $large_intake_rate_water
#> [1] 0.35
#> 
#> $low_visit_threshold
#> [1] 10
#> 
#> $total_cows_expected
#> [1] NA
#> 
#> $low_feed_intake
#> [1] 35
#> 
#> $high_feed_intake
#> [1] 75
#> 
#> $low_wat_intake
#> [1] 60
#> 
#> $high_wat_intake
#> [1] 180
#> 
#> $replacement_threshold
#> [1] 26
#> 
#> $calibration_error
#> [1] 0.5
#> 

# Tighten the "long feeding visit" threshold
cfg2 <- qc_config(high_dur_feed = 1800)
cfg2
#> $high_dur_feed
#> [1] 1800
#> 
#> $high_dur_water
#> [1] 1800
#> 
#> $large_intake_visit_feed
#> [1] 8
#> 
#> $large_intake_visit_water
#> [1] 30
#> 
#> $large_intake_rate_feed
#> [1] 0.008
#> 
#> $large_intake_rate_water
#> [1] 0.35
#> 
#> $low_visit_threshold
#> [1] 10
#> 
#> $total_cows_expected
#> [1] NA
#> 
#> $low_feed_intake
#> [1] 35
#> 
#> $high_feed_intake
#> [1] 75
#> 
#> $low_wat_intake
#> [1] 60
#> 
#> $high_wat_intake
#> [1] 180
#> 
#> $replacement_threshold
#> [1] 26
#> 
#> $calibration_error
#> [1] 0.5
#> 
```
