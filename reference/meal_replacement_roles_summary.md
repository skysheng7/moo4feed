# Summarize Actor/Reactor Roles Per Animal Per Day

Takes the output from
[`meal_replacement_roles()`](https://skysheng7.github.io/moo4feed/reference/meal_replacement_roles.md)
and calculates daily summary statistics (mean, median, SD) of the role
percentages across meals for each animal.

## Usage

``` r
meal_replacement_roles_summary(meal_roles_data, id_col = id_col2())
```

## Arguments

- meal_roles_data:

  A data frame or named list of data frames from
  [`meal_replacement_roles()`](https://skysheng7.github.io/moo4feed/reference/meal_replacement_roles.md).

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

## Value

A data frame (or named list of data frames) with columns:

- `date` - Date

- `[id_col]` - Animal identifier

- `mean_pct_actor` - Average percentage of actor visits across meals

- `median_pct_actor` - Median percentage of actor visits across meals

- `sd_pct_actor` - Standard deviation of actor percentage

- `mean_pct_reactor` - Average percentage of reactor visits across meals

- `median_pct_reactor` - Median percentage of reactor visits across
  meals

- `sd_pct_reactor` - Standard deviation of reactor percentage

- `mean_pct_actor_reactor` - Average percentage of dual-role visits

- `median_pct_actor_reactor` - Median percentage of dual-role visits

- `sd_pct_actor_reactor` - Standard deviation of dual-role percentage

- `total_meals` - Total number of meals analyzed

## Examples

``` r
# Create sample meal roles data
meal_roles <- data.frame(
  date = "2024-01-01",
  cow = c("A", "A", "B", "B"),
  meal_id = c(1, 2, 1, 2),
  total_visits_in_meal = c(5, 4, 6, 3),
  actor_visits = c(2, 1, 3, 1),
  reactor_visits = c(1, 2, 2, 1),
  actor_reactor_visits = c(0, 1, 1, 0),
  pct_actor = c(40, 25, 50, 33),
  pct_reactor = c(20, 50, 33, 33),
  pct_actor_reactor = c(0, 25, 17, 0)
)

# Summarize per animal per day
daily_summary <- meal_replacement_roles_summary(meal_roles)
```
