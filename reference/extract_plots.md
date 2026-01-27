# Extract subset of plots by animal and/or date

Extracts a subset of plots from the nested list structure based on
specified animals and/or dates.

## Usage

``` r
extract_plots(plot_list, animals = NULL, dates = NULL)
```

## Arguments

- plot_list:

  Nested list of plots from
  [`viz_meal_clusters()`](https://skysheng7.github.io/moo4feed/reference/viz_meal_clusters.md)

- animals:

  Character vector. Animal IDs to extract (default: NULL for all
  animals)

- dates:

  Character vector or Date vector. Dates to extract in "YYYY-MM-DD"
  format (default: NULL for all dates)

## Value

Nested list with same structure as input but containing only specified
subsets

## Examples

``` r
toy_data <- all_fed[[1]][which(all_fed[[1]]$cow %in% c(5114, 4070, 7010, 5028, 6020, 6030)),]
labeled <- meal_label_visits(toy_data, id_col = 'cow', start_col = 'start', 
end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration',
tz = 'America/Vancouver')

# Customize colors and text
p <- viz_meal_clusters(labeled, id_col = 'cow', start_col = 'start')

# Extract plots for specific animals and dates
subset_plots <- extract_plots(p, animals = c("5114"))
```
