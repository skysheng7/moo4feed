# Analyze pair-wise co-occurrence patterns

Analyzes how long each pair of animals spend feeding or drinking
together simultaneously, regardless of their spatial location. For each
pair of animals, calculates the number of bouts they spent together,
total time together, and average duration per bout.

## Usage

``` r
synch_pair_analysis(
  matrix_data,
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
  days. Must contain `synch_master_animal2` component with Time column
  and animal ID columns.

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

- **bout**: Number of distinct bouts each pair spent together

- **total_time**: Total time (seconds or minutes) each pair spent
  together

- **avg_duration**: Average duration per bout for each pair

Matrix rows and columns are labeled with animal IDs. Pairs that never
interacted have value 0.

## Examples

``` r
# Create toy data with multiple animals
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

# Analyze pair-wise co-occurrence
pair_results <- synch_pair_analysis(matrices, type = "feed",
                                   id_col = "animal")

```
