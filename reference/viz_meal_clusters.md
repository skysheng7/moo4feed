# Visualize meal clustering results as timeline plots

Creates timeline plots showing meal assignments over time. Points
represent individual feeding visits, colored by meal_id. Outliers
(meal_id = 0) are shown in a customizable color. The function can handle
both single dataframes and lists of dataframes from
[`merge_cluster_results()`](https://skysheng7.github.io/moo4feed/reference/merge_cluster_results.md).

## Usage

``` r
viz_meal_clusters(
  data,
  point_size = 2,
  point_alpha = 0.7,
  ncol_facet = 1,
  date_format = "%Y-%m-%d",
  time_breaks = "4 hours",
  time_labels = "%H",
  color_palette = "Set 3",
  outlier_color = "grey50",
  title_prefix = "Animal",
  text_size = 12,
  title_size = NULL,
  id_col = id_col2(),
  start_col = start_col2(),
  tz = tz2()
)
```

## Arguments

- data:

  Merged visit data with meal assignments from
  [`merge_cluster_results()`](https://skysheng7.github.io/moo4feed/reference/merge_cluster_results.md).
  Can be a single dataframe or list of dataframes.

- point_size:

  Numeric. Size of the points representing visits (default: 2).

- point_alpha:

  Numeric. Transparency of points, between 0 and 1 (default: 0.7).

- ncol_facet:

  Numeric. Number of columns for faceting when creating overview plots
  (default: 1).

- date_format:

  Character. Format for date labels (default: "%Y-%m-%d").

- time_breaks:

  Character. Time axis breaks, passed to
  [`ggplot2::scale_x_datetime()`](https://ggplot2.tidyverse.org/reference/scale_date.html)
  (default: "4 hours").

- time_labels:

  Character. Time axis label format (default: "%H").

- color_palette:

  Character. Color palette name for meal colors. Options include any
  palette from
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
  such as "Set 3", "Dark 3", "Pastel 1", etc. (default: "Set 3").

- outlier_color:

  Character. Color for outlier points (meal_id = 0) (default: "grey50").

- title_prefix:

  Character. Prefix for plot titles. Set to NULL or "" for no title
  (default: "Animal"). Will be followed by the animal ID (e.g., "Animal
  123").

- text_size:

  Numeric. Base text size for all plot text elements (default: 12).

- title_size:

  Numeric. Size of plot titles when creating overview plots. If NULL,
  uses text_size + 2 (default: NULL).

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- tz:

  Timezone (default current global value from
  [`tz2()`](https://skysheng7.github.io/moo4feed/reference/tz2.md))

## Value

If ≤5 animal-day combinations: Single ggplot object with faceted plots.
If \>5 combinations: Nested list structure:

- plots:

  List of plots organized by animal then date

## Details

The function creates timeline plots with:

- X-axis: Time of day (hours)

- Y-axis: Date (vertical for single plots, horizontal for multiple
  stacked plots)

- Points: Individual feeding visits

- Colors: meal_id using categorical color palette (customizable outlier
  color for meal_id = 0)

- Title: Customizable prefix + animal ID (e.g., "Animal XX")

## Examples

``` r
# Create toy visit data with meal assignments
toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]

# Cluster and label meals
labeled <- meal_label_visits(toy_data, id_col = 'cow', start_col = 'start', 
end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration',
tz = 'America/Vancouver')
#> ℹ Calculating optimal meal interval for all animals...

# Customize colors and text
p <- viz_meal_clusters(labeled, id_col = 'cow', start_col = 'start',
                       color_palette = "Dark 3", text_size = 14, 
                       title_prefix = "Cow")
```
