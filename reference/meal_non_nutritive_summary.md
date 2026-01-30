# Analyze Non-Nutritive and Empty Bin Visits Within Meals

Analyzes visits within meals to identify and summarize non-nutritive
visits (no feed intake but feed available) and empty bin visits (no feed
available). Input data must already have meal labels from
[`meal_label_visits()`](https://skysheng7.github.io/moo4feed/reference/meal_label_visits.md).

## Usage

``` r
meal_non_nutritive_summary(
  data,
  cfg = qc_config(),
  id_col = id_col2(),
  intake_col = intake_col2(),
  start_weight_col = start_weight_col2()
)
```

## Arguments

- data:

  A named list of daily data frames or a single data frame containing
  visit records with meal labels. Must be output from
  [`meal_label_visits()`](https://skysheng7.github.io/moo4feed/reference/meal_label_visits.md),
  which adds the required `meal_id` and `meal_start` columns.

- cfg:

  A configuration list created by
  [`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md).
  Used to determine calibration error threshold for classifying visits.

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

A data frame (or named list of data frames) with columns:

- `date` - Date

- `[id_col]` - Animal identifier

- `mean_non_nutritive_per_meal` - Average non-nutritive visits per meal

- `median_non_nutritive_per_meal` - Median non-nutritive visits per meal

- `sd_non_nutritive_per_meal` - Standard deviation of non-nutritive
  visits

- `mean_empty_bin_per_meal` - Average empty bin visits per meal

- `median_empty_bin_per_meal` - Median empty bin visits per meal

- `sd_empty_bin_per_meal` - Standard deviation of empty bin visits

- `total_non_nutritive_visits` - Total non-nutritive visits

- `total_empty_bin_visits` - Total empty bin visits

- `total_meals` - Total number of meals

## Examples

``` r
# Create sample visit data with meal labels
visits <- data.frame(
  date = "2024-01-01",
  cow = c("A", "A", "A", "B", "B"),
  bin = c(1, 2, 3, 1, 2),
  start_weight = c(50, 0.01, 45, 48, 0.02),
  intake = c(2.5, 0.01, 0.02, 3.0, 0.01),
  meal_id = c(1, 1, 1, 1, 1),
  meal_start = as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
)

# Analyze non-nutritive and empty bin visits
meal_visits <- meal_non_nutritive_summary(
  data = visits,
  cfg = qc_config()
)
```
