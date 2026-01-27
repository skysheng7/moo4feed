# Calculate optimal interval between feeding visits for meal clustering

This function calculates the optimal interval between feeding visits for
meal clustering by analyzing the gaps between feeding visits for each
animal within each day. The function determines an appropriate time
threshold that can be used to cluster visits into meals based on
temporal proximity.

- If you wish to calculate optimal interval for a single animal across
  multiple days, make sure data belongs to a single animal across
  multiple days.

- If you wish to calculate optimal interval for all animals across all
  days, make sure data recorded all animals across all days.

- If you wish to calculate optimal interval for a single animal on a
  single day, make sure data belongs to a single animal and only 1 day.

## Usage

``` r
meal_interval(
  data,
  method = "gmm",
  percentile = 0.93,
  lower_bound = 5,
  upper_bound = 60,
  use_log_transform = TRUE,
  log_multiplier = 20,
  log_offset = 1,
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  tz = tz2()
)
```

## Arguments

- data:

  A single dataframe or list of dataframes containing feeding visit data

- method:

  Character string specifying the method for eps determination. Options
  are "gmm" (default), "percentile", or "both".

  - "percentile": Uses the specified percentile of inter-visit gaps to
    determine the optimal interval between feeding visits for meal
    clustering

  - "gmm": Uses Gaussian mixture modeling to identify the optimal
    interval between feeding visits for meal clustering

  - "both": Uses both methods and returns the minimum (more
    conservative)

- percentile:

  Numeric value between 0 and 1 specifying which percentile to use for
  eps determination when method="percentile" or "both". Default is 0.93.

- lower_bound:

  Numeric value for lower bound of the optimal interval, if NULL, no
  lower bound is applied.

- upper_bound:

  Numeric value for upper bound of the optimal interval, if NULL, no
  upper bound is applied.

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

- tz:

  Timezone (default current global value from
  [`tz2()`](https://skysheng7.github.io/moo4feed/reference/tz2.md))

## Value

A numeric value representing the optimal eps parameter in minutes. This
value is bounded between lower_bound and upper_bound for practical meal
clustering.

## Details

The function processes feeding visit data to calculate time gaps between
consecutive visits for each animal within each day. These gaps are then
analyzed using statistical methods to determine an optimal time
threshold for clustering visits into meals.

The analysis considers:

- Inter-visit gaps within each animal-day combination

- Statistical distribution of gap durations

- Conservative bounds (lower_bound-upper_bound) for practical
  application

When method="both", the function uses both percentile and Gaussian
mixture modeling approaches and returns the minimum value to be more
conservative in meal definitions.

## See also

[`cluster_meals()`](https://skysheng7.github.io/moo4feed/reference/cluster_meals.md)
for using the eps parameter in meal clustering

## Examples

``` r
# Calculate optimal eps using default method (GMM)
meal_interval(all_fed[[1]])
#> [1] 25.64307

# Use only percentile method with 80th percentile
meal_interval(all_fed[[1]], method = "percentile", percentile = 0.8)
#> [1] 5

# Use only Gaussian mixture modeling
meal_interval(all_fed[[1]], method = "gmm")
#> [1] 25.64314

# Work with list of dataframes
meal_interval(all_fed, method = "both", percentile = 0.93)
#> [1] 14.59333
```
