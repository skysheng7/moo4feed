# Compare neighbor time to total co-occurrence time

Compares the time pairs spend at neighboring bins to their total
co-occurrence time (regardless of location). Calculates the proportion
of co-occurrence time that is spent as spatial neighbors, which
indicates pairs that prefer to be close to each other when
feeding/drinking together.

## Usage

``` r
synch_neighbor_compare(
  pair_results,
  neighbor_results,
  min_cooccurrence = 0,
  sort_by = "neighbor_ratio",
  decreasing = TRUE
)
```

## Arguments

- pair_results:

  List output from
  [`synch_pair_analysis()`](https://skysheng7.github.io/moo4feed/reference/synch_pair_analysis.md),
  containing total co-occurrence matrices

- neighbor_results:

  List output from
  [`synch_neighbor_analysis()`](https://skysheng7.github.io/moo4feed/reference/synch_neighbor_analysis.md),
  containing neighbor-specific matrices

- min_cooccurrence:

  Numeric, minimum co-occurrence time threshold to include a pair
  (default 0). Use this to filter out pairs with negligible total
  interaction.

- sort_by:

  Character, column name to sort by. Common options: "neighbor_ratio"
  (default), "neighbor_time", "cooccurrence_time", or NULL for no
  sorting.

- decreasing:

  Logical, if TRUE (default) sorts in descending order, if FALSE sorts
  ascending

## Value

Data frame with columns:

- `animal1`: Character, first animal ID in pair

- `animal2`: Character, second animal ID in pair

- `cooccurrence_time`: Numeric, total time together anywhere

- `neighbor_time`: Numeric, time together at neighboring bins

- `neighbor_ratio`: Numeric, neighbor_time / cooccurrence_time

- `day`: Character, day identifier (only for multi-day input)

The neighbor_ratio ranges from 0 (never neighbors) to 1 (always at
neighboring bins when together). Only includes pairs where
cooccurrence_time \> min_cooccurrence.

## Examples

``` r
# Create toy data with animals at different bins
toy_data <- data.frame(
  animal = c(1, 2, 3, 1, 2),
  start = lubridate::ymd_hms(c(
    "2023-01-01 10:00:00", "2023-01-01 10:00:01",
    "2023-01-01 10:00:05", "2023-01-01 10:00:10",
    "2023-01-01 10:00:11"
  )),
  end = lubridate::ymd_hms(c(
    "2023-01-01 10:00:03", "2023-01-01 10:00:04",
    "2023-01-01 10:00:08", "2023-01-01 10:00:13",
    "2023-01-01 10:00:14"
  )),
  bin = c(1, 2, 3, 1, 2),
  start_weight = c(10.5, 8.3, 9.1, 10.2, 8.0),
  end_weight = c(10.2, 8.1, 8.9, 9.9, 7.8)
)

# Process matrices
matrices <- matrix_process(toy_data, type = "feed",
                          id_col = "animal", start_col = "start",
                          end_col = "end", bin_col = "bin",
                          start_weight_col = "start_weight",
                          end_weight_col = "end_weight",
                          bins_feed = 1:3)

# Analyze both total and neighbor patterns
pair_results <- synch_pair_analysis(matrices, type = "feed",
                                   id_col = "animal")
neighbor_results <- synch_neighbor_analysis(matrices,
                                           bin_layout = "1-2-3",
                                           type = "feed",
                                           id_col = "animal")

# Compare neighbor preference
comparison <- synch_neighbor_compare(pair_results, neighbor_results)
print(comparison)
#>   animal1 animal2 cooccurrence_time neighbor_time neighbor_ratio
#> 1       1       2                 6             6              1
```
