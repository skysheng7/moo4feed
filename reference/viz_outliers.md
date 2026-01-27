# Visualize outliers from KNN outlier detection

This function creates a scatter plot visualizing the results of KNN
outlier detection. It allows users to plot any two variables against
each other (e.g., intake vs. duration, rate vs. intake, etc.), with
outliers highlighted in a different color.

## Usage

``` r
viz_outliers(
  data,
  x_var = duration_col2(),
  y_var = intake_col2(),
  x_lab = NULL,
  y_lab = NULL,
  jitter_amount = 0.2,
  alpha = 0.7,
  title = "Outlier Detection Results",
  regular_color = "lightblue",
  outlier_color = "orange"
)
```

## Arguments

- data:

  A data frame containing outlier detection results, or a list of such
  data frames. If a list is provided, the data will be merged before
  visualization.

- x_var:

  Character. Name of the column to display on the x-axis. Default is
  [`duration_col2()`](https://skysheng7.github.io/moo4feed/reference/duration_col2.md).

- y_var:

  Character. Name of the column to display on the y-axis. Default is
  [`intake_col2()`](https://skysheng7.github.io/moo4feed/reference/intake_col2.md).

- x_lab:

  Character. Label for the x-axis. Default is NULL, which uses the value
  of x_var.

- y_lab:

  Character. Label for the y-axis. Default is NULL, which uses the value
  of y_var.

- jitter_amount:

  Numeric. Amount of jitter to add to the points to prevent overplotting
  (default: 0.2).

- alpha:

  Numeric. Transparency level for the points, between 0 (completely
  transparent) and 1 (opaque). Default is 0.7.

- title:

  Character. Plot title (default: "Outlier Detection Results").

- regular_color:

  Character. Color for regular (non-outlier) points (default:
  "lightblue").

- outlier_color:

  Character. Color for outlier points (default: "orange").

## Value

A ggplot object that can be further customized or printed.

## Examples

``` r
# Create a toy dataset with some normal feeding data and a few outliers
set.seed(123)
# Generate 100 normal feeding events
df_feed <- data.frame(
  cow = rep(1:10, each = 10),
  duration = runif(100, 100, 300),
  intake = runif(100, 5, 15),
  bin = sample(1:5, 100, replace = TRUE),
  outlier = rep("N", 100)
)

# Add 5 outlier events
df_outliers <- data.frame(
  cow = sample(1:10, 5),
  duration = c(500, 600, 150, 700, 100),
  intake = c(35, 40, 45, 5, 50),
  bin = sample(1:5, 5, replace = TRUE),
  outlier = rep("Y", 5)
)

# Combine the normal and outlier data
df_combined <- rbind(df_feed, df_outliers)

# Visualize intake vs. duration
p1 <- viz_outliers(df_combined, x_var = "duration", y_var = "intake")

# Visualize with custom labels and title
p2 <- viz_outliers(df_combined, 
             x_var = "duration",
             y_var = "intake",
             x_lab = "Feeding Duration (seconds)", 
             y_lab = "Feed Intake (kg)",
             title = "Feed Intake Outlier Analysis")
```
