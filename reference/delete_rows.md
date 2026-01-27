# Remove rows by matching values in a specified column

Filters out any rows where the values in `col_name` appear in
`to_delete`. This function is helpful for when you want to delete
information about certain animals by their ID, or certain
sensors/transponders/insentec bins.

## Usage

``` r
delete_rows(df, to_delete, col_name = id_col2())
```

## Arguments

- df:

  A data frame containing a column named `col_name`.

- to_delete:

  What's the animal ID/transponders that you wish to delete? This should
  be a vector of values to remove; **must** have the same data type as
  `df[[col_name]]`.

- col_name:

  What's the name of the column you wish to filter your data on? This
  should be a single string giving the name of the column you are
  interested in. Default is "cow".

## Value

A data frame identical to `df` but with all rows where
`df[[col_name]] %in% to_delete` dropped.

## Examples

``` r
df <- data.frame(
  cow = c("A", "B", "C"),
  transponder = c("X1", "X2", "X3"),
  Value = 1:3,
  stringsAsFactors = FALSE
)
print(df)
#>   cow transponder Value
#> 1   A          X1     1
#> 2   B          X2     2
#> 3   C          X3     3
# drop data about cows "A" and "C" from the dataframe
delete_rows(df, c("A", "C"), "cow")
#>   cow transponder Value
#> 2   B          X2     2

# drop data recorded by transponder "X2" from the dataframe
delete_rows(df, c("X2"), "transponder")
#>   cow transponder Value
#> 1   A          X1     1
#> 3   C          X3     3
```
