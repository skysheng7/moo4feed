# Merge meal clustering results with original visit data

Merges the meal-level results from
[`cluster_meals()`](https://skysheng7.github.io/moo4feed/reference/cluster_meals.md)
with the original feeding visit data to assign meal information to each
individual visit. Visits not assigned to any meal (outliers) are labeled
with meal_id = 0.

## Usage

``` r
merge_cluster_results(
  visit_data,
  meal_results,
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  tz = tz2()
)
```

## Arguments

- visit_data:

  Original feeding visit data (dataframe or list of dataframes)

- meal_results:

  Results from
  [`cluster_meals()`](https://skysheng7.github.io/moo4feed/reference/cluster_meals.md)
  function

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- end_col:

  End time column name (default current global value from
  [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md))

- tz:

  Timezone (default current global value from
  [`tz2()`](https://skysheng7.github.io/moo4feed/reference/tz2.md))

## Value

Same structure as input visit_data (dataframe or list of dataframes)
with original visit data plus meal information columns:

- meal_id:

  Sequential meal number within animal-day (0 for outliers)

- meal_start:

  Start time of the meal this visit belongs to (NA for outliers)

- meal_end:

  End time of the meal this visit belongs to (NA for outliers)

- meal_duration:

  Total duration of the meal this visit belongs to (NA for outliers)

- total_intake:

  Total intake of the meal this visit belongs to (NA for outliers)

- visit_count:

  Number of visits in the meal this visit belongs to (NA for outliers)

## Details

The function matches visits to meals based on animal ID, date, and
whether the visit time falls within the meal's start and end times.
Visits that don't match any meal are considered outliers and assigned
meal_id = 0.

## Examples

``` r
# Cluster meals first (all_fed is included in the package)
meals <- cluster_meals(all_fed[[1]])
#> ℹ Calculating optimal meal interval for all animals...
#> ⠙ ■■■■■■■■■■■■■■■■                 23/47 [ 49%] | ETA:  1s
#> ⠹ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   45/47 [ 96%] | ETA:  0s
#> ⠹ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  47/47 [100%] | ETA:  0s

# Merge results with original data
merged_data <- merge_cluster_results(all_fed[[1]], meals)

# Check how many visits were assigned vs. outliers
table(merged_data$meal_id == 0)
#> 
#> FALSE  TRUE 
#>  2939   544 
```
