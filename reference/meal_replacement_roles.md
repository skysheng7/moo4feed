# Analyze Within-Meal Actor/Reactor Roles

Analyzes the percentage of visits within each meal where an animal
entered as an actor (replaced another animal) or left as a reactor (was
replaced by another animal). Returns meal-level detail showing role
percentages.

## Usage

``` r
meal_replacement_roles(
  visit_data,
  replacement_data,
  time_tolerance = 1,
  id_col = id_col2(),
  bin_col = bin_col2(),
  start_col = start_col2(),
  end_col = end_col2()
)
```

## Arguments

- visit_data:

  A named list of daily data frames or a single data frame containing
  visit records with meal labels.

- replacement_data:

  A named list of daily data frames or a single data frame containing
  replacement events from
  [`record_replacement_days()`](https://skysheng7.github.io/moo4feed/reference/record_replacement_days.md).

- time_tolerance:

  Numeric. Time tolerance in seconds for matching visits to replacement
  events. Default is 1 second.

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- end_col:

  End time column name (default current global value from
  [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md))

## Value

A data frame (or named list of data frames) with columns:

- `date` - Date

- `[id_col]` - Animal identifier

- `meal_id` - Meal identifier

- `total_visits_in_meal` - Total visits in the meal

- `actor_visits` - Number of visits where animal entered as actor

- `reactor_visits` - Number of visits where animal left as reactor

- `actor_reactor_visits` - Number of visits with both roles

- `pct_actor` - Percentage of visits as actor

- `pct_reactor` - Percentage of visits as reactor

- `pct_actor_reactor` - Percentage of visits with both roles

## Examples

``` r
# Create sample visit data with meal labels
visits <- data.frame(
  date = "2024-01-01",
  cow = c("A", "A", "B", "B"),
  bin = c(1, 2, 1, 2),
  start = as.POSIXct(c(
    "2024-01-01 08:00:00",
    "2024-01-01 08:15:00",
    "2024-01-01 08:10:00",
    "2024-01-01 08:20:00"
  ), tz = "UTC"),
  end = as.POSIXct(c(
    "2024-01-01 08:10:00",
    "2024-01-01 08:20:00",
    "2024-01-01 08:15:00",
    "2024-01-01 08:25:00"
  ), tz = "UTC"),
  meal_id = c(1, 1, 1, 1),
  meal_start = as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
)

# Create replacement data
replacements <- data.frame(
  actor_cow = "B",
  reactor_cow = "A",
  bin = 1,
  time = as.POSIXct("2024-01-01 08:10:00", tz = "UTC")
)

# Analyze actor/reactor roles within meals
meal_roles <- meal_replacement_roles(
  visit_data = visits,
  replacement_data = replacements
)
```
