# Visualize gap distribution with GMM fit and optimal interval (eps)

Creates a histogram visualization of inter-visit gaps with Gaussian
Mixture Model (GMM) component distributions overlaid and the optimal
interval (eps) value highlighted. This helps you understand how the GMM
method identifies distinct gap patterns and selects the optimal interval
for meal clustering.

- If you wish to calculate optimal interval for a single animal across
  multiple days, make sure data belongs to a single animal across
  multiple days.

- If you wish to calculate optimal interval for all animals across all
  days, make sure data recorded all animals across all days.

- If you wish to calculate optimal interval for a single animal on a
  single day, make sure data belongs to a single animal and only 1 day.

## Usage

``` r
viz_eps_gmm(
  data,
  lower_bound = NULL,
  upper_bound = NULL,
  bins = 100,
  colors = grDevices::hcl.colors(4, "Set 3"),
  title_prefix =
    "Distribution of time gap between visits \n& GMM-based meal interval (eps)",
  show_components = TRUE,
  xlim = 10,
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

- lower_bound:

  Numeric value for lower bound of the optimal interval, if NULL, no
  lower bound is applied.

- upper_bound:

  Numeric value for upper bound of the optimal interval, if NULL, no
  upper bound is applied.

- bins:

  Number of bins for the histogram.

- colors:

  Character vector of colors to use. Default uses
  `grDevices::hcl.colors(4, "Set 3")`.

- title_prefix:

  Character string for plot title prefix. Default is "Distribution of
  time gap between visits & GMM-based meal interval (eps)".

- show_components:

  Logical indicating whether to show individual GMM components. Default
  is TRUE.

- xlim:

  Numeric value for x-axis limit.

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

A ggplot2 object showing the gap distribution histogram with GMM fit and
optimal eps line

## Details

The function internally calls
[`meal_interval()`](https://skysheng7.github.io/moo4feed/reference/meal_interval.md)
with method="gmm" to determine the optimal eps value and fits a
2-component Gaussian mixture model to visualize the separation between
within-meal and between-meal gaps.

By default, the function uses log transformation for GMM fitting
(`use_log_transform = TRUE`), which often provides better separation
between within-meal and between-meal gaps due to the typically
right-skewed nature of gap distributions. When log transformation is
used, the component distributions are displayed as log-normal
distributions in the original scale.

If GMM fitting fails or there are insufficient data points (\< 10), the
function falls back to percentile method with a warning.

## Examples

``` r
toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]

# Visualize with GMM method (default uses log transformation)
plot <- viz_eps_gmm(toy_data, id_col = "cow", start_col = "start", 
                   end_col = "end", tz = "America/Vancouver",
                   use_log_transform = FALSE)
#> ℹ Calculating time gaps between visits...
#> ℹ Fitting Gaussian Mixture Model to identify meal intervals...
#> ✔ Visualization complete!
```
