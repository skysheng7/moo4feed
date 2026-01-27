# Visualize gap distribution and percentile-based optimal interval (eps) selection

Creates a histogram visualization of inter-visit gaps with the optimal
eps value (determined using percentile method) highlighted as a vertical
line. This helps you understand how the percentile method selects the
optimal interval for meal clustering.

- If you wish to calculate optimal interval for a single animal across
  multiple days, make sure data belongs to a single animal across
  multiple days.

- If you wish to calculate optimal interval for all animals across all
  days, make sure data recorded all animals across all days.

- If you wish to calculate optimal interval for a single animal on a
  single day, make sure data belongs to a single animal and only 1 day.

## Usage

``` r
viz_eps_percentile(
  data,
  percentile = 0.93,
  lower_bound = NULL,
  upper_bound = NULL,
  bins = 100,
  colors = grDevices::hcl.colors(2, "Set 3"),
  title_prefix =
    "Distribution of time gap between visits & \noptimal meal interval (eps)",
  xlim = 20,
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  tz = tz2()
)
```

## Arguments

- data:

  A single dataframe or list of dataframes containing feeding visit data

- percentile:

  Numeric value between 0 and 1 specifying which percentile to use for
  eps determination. Default is 0.93.

- lower_bound:

  Numeric value for lower bound of the optimal interval, if NULL, no
  lower bound is applied.

- upper_bound:

  Numeric value for upper bound of the optimal interval, if NULL, no
  upper bound is applied.

- bins:

  Number of bins for the histogram. Default is 100.

- colors:

  Character vector of 2 colors to use. Default uses
  `grDevices::hcl.colors(2, "Set 3")`.

- title_prefix:

  Character string for plot title prefix. Default is "Distribution of
  time gap between visits & optimal meal interval (eps)".

- xlim:

  Numeric value for x-axis limit. Default is 20.

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

A ggplot2 object showing the gap distribution histogram with optimal eps
line

## Details

The function internally calls
[`meal_interval()`](https://skysheng7.github.io/moo4feed/reference/meal_interval.md)
with method="percentile" to determine the optimal eps value, then
creates a histogram of all inter-visit gaps with the optimal eps marked
as a vertical dashed line.

## Examples

``` r
# Create toy dataset
toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]
# Visualize with 93rd percentile
plot <- viz_eps_percentile(toy_data, id_col = "cow", 
                           start_col = "start", end_col = "end", 
                           tz = "America/Vancouver", 
                           percentile = 0.93)
```
