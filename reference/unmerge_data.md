# Un-merge a combined data frame into a list of data frames by date

This function does the opposite of
[merge_list_df](https://skysheng7.github.io/moo4feed/reference/merge_list_df.md)
by splitting a combined data frame into a list of data frames, with each
data frame containing data for a single date. The list elements will be
named by the date.

## Usage

``` r
unmerge_data(df, date_col = "date")
```

## Arguments

- df:

  A data frame containing a date column.

- date_col:

  Character. Name of the column containing date information (default:
  "date"). Values under this column should be in YYYY-MM-DD format.

## Value

A named list of data frames, with each data frame containing data for a
single date. The names of the list elements are the dates.

## Examples

``` r
# create a combined data frame
combined_df <- data.frame(   
  date = c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02"),
  cow = c(1, 2, 3, 4),
  intake = c(10, 20, 30, 40)
)

# Un-merge a combined data frame
df_list <- unmerge_data(combined_df)

# Access data for a specific date
names(df_list)
#> [1] "2023-01-01" "2023-01-02"
```
