# Cluster feeding visits into meals and label each visit

One-stop function to cluster feeding visits into meals and label each
visit with its meal information. This function first calls
[`cluster_meals()`](https://skysheng7.github.io/moo4feed/reference/cluster_meals.md)
to identify meals, then merges the meal information back to the original
visit data using
[`merge_cluster_results()`](https://skysheng7.github.io/moo4feed/reference/merge_cluster_results.md).

## Usage

``` r
meal_label_visits(
  data,
  eps = NULL,
  min_pts = 2,
  method = "gmm",
  percentile = 0.93,
  eps_scope = "all_animals",
  lower_bound = 5,
  upper_bound = 60,
  use_log_transform = TRUE,
  log_multiplier = 20,
  log_offset = 1,
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  intake_col = intake_col2(),
  dur_col = duration_col2(),
  tz = tz2()
)
```

## Arguments

- data:

  Feeding visit data (dataframe or list of dataframes)

- eps:

  DBSCAN epsilon parameter (maximum time gap in minutes between visits
  in same meal). If NULL (default), the parameter is automatically
  determined using statistical methods.

- min_pts:

  DBSCAN minimum points parameter (minimum visits to form a dense
  cluster). Default is 2. This follows the DBSCAN recommendation of
  setting min_pts to D + 1 where D is the number of dimensions (we have
  only 1 dimension: time, so min_pts = 1 + 1 = 2).

- method:

  Character string specifying the automatic eps determination method
  when eps=NULL. Options are "gmm" (default), "percentile", or "both".

- percentile:

  Numeric value between 0 and 1 specifying which percentile to use for
  automatic eps determination when method="percentile" or "both".
  Default is 0.93.

- eps_scope:

  Character string specifying the scope for automatic eps determination
  when eps=NULL. Options are:

  - "all_animals" (default): calculate an universal optimal interval
    (eps) for all animals across all days

  - "one_animal_all_days": calculate optimal interval (eps) differently
    for different animals, but within each animal, we use the same eps
    across all days

  - "one_animal_single_day": calculate optimal interval (eps)
    differently for different animals, and calculate different eps for
    each day within the same animal

- lower_bound:

  Numeric value for lower bound of the optimal interval, if NULL, no
  lower bound is applied. Default is 5.

- upper_bound:

  Numeric value for upper bound of the optimal interval, if NULL, no
  upper bound is applied. Default is 60.

- use_log_transform:

  Logical indicating whether to use log transformation for GMM fitting.
  Default is TRUE. Log transformation often provides better separation
  of within-meal and between-meal gaps.

- log_multiplier:

  Numeric value for multiplier of log transformation. Default is 20.

- log_offset:

  Numeric value for offset of log transformation. Default is 1.

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

- intake_col:

  Intake column name (default current global value from
  [`intake_col2()`](https://skysheng7.github.io/moo4feed/reference/intake_col2.md))

- dur_col:

  Duration column name (default current global value from
  [`duration_col2()`](https://skysheng7.github.io/moo4feed/reference/duration_col2.md))

- tz:

  Timezone (default current global value from
  [`tz2()`](https://skysheng7.github.io/moo4feed/reference/tz2.md))

## Value

Same structure as input data (dataframe or list of dataframes) with
additional columns:

- meal_id: Sequential meal number within animal-day (0 for outliers)

- meal_start: Start time of the meal this visit belongs to (NA for
  outliers)

- meal_end: End time of the meal this visit belongs to (NA for outliers)

- meal_duration: Total duration of the meal this visit belongs to (NA
  for outliers)

- total_intake: Total intake of the meal this visit belongs to (NA for
  outliers)

- visit_count: Number of visits in the meal this visit belongs to (NA
  for outliers)

## Details

This function is a convenience wrapper for
[`cluster_meals()`](https://skysheng7.github.io/moo4feed/reference/cluster_meals.md)
and
[`merge_cluster_results()`](https://skysheng7.github.io/moo4feed/reference/merge_cluster_results.md).
It clusters feeding visits into meals and then labels each visit with
its meal assignment and summary statistics.

## Examples

``` r
# Create a toy dataset
toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]

# Cluster and label meals
labeled <- meal_label_visits(toy_data, id_col = 'cow', start_col = 'start', 
end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration',
tz = 'America/Vancouver')
#> ℹ Calculating optimal meal interval for all animals...
head(labeled)
#>     transponder  cow bin               start                 end duration
#> 31     12200060 5114  13 2020-10-31 00:03:24 2020-10-31 00:04:01       37
#> 35     12200060 5114  17 2020-10-31 00:04:24 2020-10-31 00:12:57      513
#> 36     12200060 5114  15 2020-10-31 00:13:13 2020-10-31 00:13:19        6
#> 69     12200060 5114   4 2020-10-31 00:17:13 2020-10-31 00:34:51     1058
#> 454    12200060 5114  25 2020-10-31 06:30:13 2020-10-31 06:32:25      132
#> 545    12200060 5114  26 2020-10-31 06:32:29 2020-10-31 06:51:43     1154
#>     start_weight end_weight intake       date meal_id          meal_start
#> 31          18.7       19.4   -0.7 2020-10-31       1 2020-10-31 00:03:24
#> 35          14.1       12.3    1.8 2020-10-31       1 2020-10-31 00:03:24
#> 36          23.0       23.0    0.0 2020-10-31       2 2020-10-31 00:13:13
#> 69          11.6        7.7    3.9 2020-10-31       2 2020-10-31 00:13:13
#> 454         46.5       45.7    0.8 2020-10-31       3 2020-10-31 06:30:13
#> 545         89.8       83.1    6.7 2020-10-31       3 2020-10-31 06:30:13
#>                meal_end meal_duration total_intake visit_count
#> 31  2020-10-31 00:12:57           573          1.1           2
#> 35  2020-10-31 00:12:57           573          1.1           2
#> 36  2020-10-31 00:34:51          1298          3.9           2
#> 69  2020-10-31 00:34:51          1298          3.9           2
#> 454 2020-10-31 06:51:43          1290          7.5           2
#> 545 2020-10-31 06:51:43          1290          7.5           2
```
