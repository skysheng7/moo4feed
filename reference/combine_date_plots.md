# Combine multiple animals for one date with pagination

Takes plots for multiple animals on one date and creates paginated plots
with a user-defined number of plots per page to avoid overcrowding.

## Usage

``` r
combine_date_plots(
  plot_list,
  date,
  plots_per_page = 4,
  method = c("vertical", "grid"),
  title_prefix = NULL
)
```

## Arguments

- plot_list:

  Nested list of plots from
  [`viz_meal_clusters()`](https://skysheng7.github.io/moo4feed/reference/viz_meal_clusters.md)

- date:

  Character or Date. Date to combine plots for (format: "YYYY-MM-DD")

- plots_per_page:

  Integer. Number of plots to include per page (default: 4)

- method:

  Character. Method for combining plots: "vertical" or "grid" (default:
  "vertical")

- title_prefix:

  Character. Prefix for page titles (default: NULL)

## Value

Named list of combined ggplot/patchwork objects, with names "1", "2",
"3", etc.

## Examples

``` r
toy_data <- all_fed[[1]][which(all_fed[[1]]$cow %in% c(5114, 4070, 7010, 5028, 6020, 6030)),]
labeled <- meal_label_visits(toy_data, id_col = 'cow', start_col = 'start', 
end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration',
tz = 'America/Vancouver')

# Customize colors and text
p <- viz_meal_clusters(labeled, id_col = 'cow', start_col = 'start')

combined_plot <- combine_date_plots(p, date = "2020-10-31")
```
