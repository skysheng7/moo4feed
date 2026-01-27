# Analyze pair-wise spatial neighbor patterns

Analyzes how long each pair of animals spend at neighboring bins
(spatial proximity). Uses bin layout configuration to determine which
bins are spatial neighbors. For each pair of animals, calculates the
number of bouts they spent as neighbors, total time as neighbors, and
average duration per bout.

## Usage

``` r
synch_neighbor_analysis(
  matrix_data,
  bin_layout = bin_layout2(),
  type = c("feed", "drink", "feed_and_drink"),
  resolution = c("sec", "min"),
  id_col = id_col2()
)
```

## Arguments

- matrix_data:

  Output from
  [`matrix_process()`](https://skysheng7.github.io/moo4feed/reference/matrix_process.md),
  either a single day's matrices or a list of matrices for multiple
  days. Must contain `synch_master_bin2` component with Time column and
  animal ID columns (values are bin numbers).

- bin_layout:

  Character string representing spatial layout of bins, with rows
  separated by `\n` and bins within rows separated by "-". Example:
  `1-2-3\n4-5-6` represents first row has bin 1, 2, 3, second row has
  bin 4, 5, 6. Default uses
  [`bin_layout2()`](https://skysheng7.github.io/moo4feed/reference/bin_layout2.md).

- type:

  Character, one of "feed", "drink", "feed_and_drink" to specify the
  type of activity being analyzed

- resolution:

  Character, either "sec" (default) or "min" for time resolution.
  Affects bout detection threshold and time calculations.

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

## Value

List with three elements, each containing animal × animal matrices (one
per day if multi-day input, or single matrix if single day input). Only
upper triangle of matrices is filled:

- **bout**: Number of distinct bouts each pair spent as neighbors

- **total_time**: Total time (seconds or minutes) each pair spent as
  neighbors

- **avg_duration**: Average duration per bout for each pair

Matrix rows and columns are labeled with animal IDs. Pairs that were
never neighbors have value 0.

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

# Analyze neighbor patterns with linear layout
neighbor_results <- synch_neighbor_analysis(
  matrices, 
  bin_layout = "1-2-3",
  type = "feed",
  id_col = "animal"
)

```
