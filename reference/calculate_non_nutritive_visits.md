# Calculate non-nutritive visits per animal per day

Calculates the number of non-nutritive visits for each animal on each
day. A non-nutritive visit occurs when an animal visits a bin that has
feed available (more than the calibration error) but does not eat
anything.

## Usage

``` r
calculate_non_nutritive_visits(
  data,
  cfg = qc_config(),
  sort = 0,
  id_col = id_col2(),
  intake_col = intake_col2(),
  start_weight_col = start_weight_col2()
)
```

## Arguments

- data:

  A named list of daily data frames (one per day), each containing
  visit-level data.

- cfg:

  A configuration list created by
  [`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md).

- sort:

  Numeric. How to sort results by visit frequency:

  - `0` (default) - no sorting

  - `1` - ascending (lowest to highest)

  - `-1` - descending (highest to lowest)

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- intake_col:

  Intake column name (default current global value from
  [`intake_col2()`](https://skysheng7.github.io/moo4feed/reference/intake_col2.md))

- start_weight_col:

  Start weight column name (default current global value from
  [`start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/start_weight_col2.md))

## Value

A named list of data frames, one per day, each containing columns for
animal ID and the count of non-nutritive visits. If `sort` is specified,
results are sorted by the visit count column.

## Examples

``` r
toy_data <- list(
  "2023-01-01" = data.frame(
    cow = c("1", "2", "3", "1"),
    intake = c(0.2, 0.3, 0.4, 0.0),
    start_weight = c(10, 15, 20, 12)
  )
)
cfg <- qc_config(calibration_error = 0.5)

# No sorting (default)
result <- calculate_non_nutritive_visits(toy_data, cfg = cfg)
result[[1]]
#> # A tibble: 3 × 2
#>   cow   number_of_non_nutritive_visits
#>   <chr>                          <int>
#> 1 1                                  2
#> 2 2                                  1
#> 3 3                                  1

# Sort descending (highest to lowest)
result_desc <- calculate_non_nutritive_visits(toy_data, cfg = cfg, sort = -1)
result_desc[[1]]
#> # A tibble: 3 × 2
#>   cow   number_of_non_nutritive_visits
#>   <chr>                          <int>
#> 1 1                                  2
#> 2 2                                  1
#> 3 3                                  1

# Sort ascending (lowest to highest)
result_asc <- calculate_non_nutritive_visits(toy_data, cfg = cfg, sort = 1)
result_asc[[1]]
#> # A tibble: 3 × 2
#>   cow   number_of_non_nutritive_visits
#>   <chr>                          <int>
#> 1 2                                  1
#> 2 3                                  1
#> 3 1                                  2
```
