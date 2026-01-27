# Process 1 feeder data file

Reads, cleans, and filters a feeder file:

1.  Safely read the CSV / DAT file

2.  Rename columns

3.  Drop unwanted cows & transponders

4.  Keep only specified bins

5.  Subset to desired columns

## Usage

``` r
process_feeder(
  file,
  col_names = NULL,
  id_col = id_col2(),
  drop_ids = NULL,
  trans_col = trans_col2(),
  drop_trans = NULL,
  bin_col = bin_col2(),
  bins = bins_feed2(),
  select_cols = NULL,
  sep = ",",
  header = FALSE
)
```

## Arguments

- file:

  What's the path to your data file? This should be a single string
  giving the path to the file.

- col_names:

  A character vector of column names to assign when `header = FALSE`.
  This vector must match the number of columns in the raw data. If
  `header = TRUE`, the file’s existing column names are used and
  `col_names` is ignored.

- id_col:

  What's the name of the column recording animal ID? This should be a
  Single string. (default: `"cow"`).

- drop_ids:

  Which animals do you wish to drop? This should be a vector indicating
  values in `id_col` that you wish to remove (default: `NULL`, so remove
  nothing).

- trans_col:

  What's the name of the column recording transponder ID for each visit?
  This should be a single string. (default: `"transponder"`).

- drop_trans:

  Which transponders do you wish to delete because they are not part of
  your study? This should be a vector indicating values in `trans_col`
  that you wish to remove (default: `NULL`, so remove nothing).

- bin_col:

  What's the name of the column recording the ID of the bin for each
  visit? This should be a single string. (default: `"bin"`).

- bins:

  Which feed bins are included in your study? This should be a numeric
  vector indicating bin IDs to keep (e.g. `2:4` or `c(1,5)`). Default is
  1:30.

- select_cols:

  Which columns in the dataframe do you wish to keep in your original
  data frame after cleaning? This should be a character vector
  indicating columns to retain in the final output. Default is `NULL`,
  so we select all columns.

- sep:

  Field separator; passed to
  [`read.table()`](https://rdrr.io/r/utils/read.table.html). Defaults to
  `","` for comma delimited files like `.csv` and `.DAT`.

- header:

  Logical; does your data file have a header row (i.e., column names)?
  Defaults to `FALSE`. If yor file contains column names at the top,
  please set this to `TRUE`.

## Value

A cleaned feeder data frame.

## Examples

``` r
# Create a toy feeder data frame
original <- data.frame(
  cow = c("A", "B", "C", "A"),
  transponder = c("X1", "X2", "X3", "X2"),
  bin = c(1, 2, 3, 2),
  value = c(10, 20, 30, 40),
  stringsAsFactors = FALSE
)
print(original)
#>   cow transponder bin value
#> 1   A          X1   1    10
#> 2   B          X2   2    20
#> 3   C          X3   3    30
#> 4   A          X2   2    40
tmp <- tempfile(fileext = ".csv")
write.csv(original, tmp, row.names = FALSE)

# Drop cow "A" and transponder "X2", keep bins 2:3, select only cow, bin, value
process_feeder(
  file = tmp,
  drop_ids = "A",
  drop_trans = "X2",
  bins = 2:3,
  select_cols = c("cow", "bin", "value"),
  header = TRUE
)
#>   cow bin value
#> 3   C   3    30
unlink(tmp)
```
