# Process 1 water data file

Reads, cleans, filters, and offsets a water file:

1.  Safely read the CSV

2.  Rename columns

3.  Drop unwanted cows & transponders

4.  Keep only specified bins

5.  Subset to desired columns

6.  Add an offset to bin IDs, so that water bin ID differs from feed bin
    ID

## Usage

``` r
process_water(
  file,
  col_names = NULL,
  id_col = id_col2(),
  drop_ids = NULL,
  trans_col = trans_col2(),
  drop_trans = NULL,
  bin_col = bin_col2(),
  bins = bins_wat2(),
  select_cols = NULL,
  bin_offset = bin_offset2(),
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

  Which water bins are included in your study for analysis? This should
  be a numeric vector of bin IDs to keep. You can supply individual
  values (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`). Default is set
  to 1:5

- select_cols:

  Which columns in the dataframe do you wish to keep in your original
  data frame after cleaning? This should be a character vector
  indicating columns to retain in the final output. Default is `NULL`,
  so we select all columns.

- bin_offset:

  A single numeric value to add to each matching bin ID. Default is 100.

- sep:

  Field separator; passed to
  [`read.table()`](https://rdrr.io/r/utils/read.table.html). Defaults to
  `","` for comma delimited files like `.csv` and `.DAT`.

- header:

  Logical; does your data file have a header row (i.e., column names)?
  Defaults to `FALSE`. If yor file contains column names at the top,
  please set this to `TRUE`.

## Value

A cleaned water data frame with shifted bin IDs.

## Examples

``` r
# create a toy data file
original <- data.frame(
  cow = c("D", "E", "F", "D"),
  transponder = c("Y1", "Y2", "Y3", "Y2"),
  bin = c(5, 6, 7, 6),
  value = c(50, 60, 70, 80),
  stringsAsFactors = FALSE
)
print(original)
#>   cow transponder bin value
#> 1   D          Y1   5    50
#> 2   E          Y2   6    60
#> 3   F          Y3   7    70
#> 4   D          Y2   6    80
tmp <- tempfile(fileext = ".csv")
write.table(original, tmp, sep = ",", row.names = FALSE, col.names = FALSE)

# Drop nothing, keep bins 5:7, offset +100, select cow, bin, value
process_water(
  file = tmp,
  col_names = c("cow", "transponder", "bin", "value"),
  drop_ids = NULL,
  drop_trans = NULL,
  bins = 5:7,
  select_cols = c("cow", "bin", "value"),
  bin_offset = 100,
  header = FALSE
)
#>   cow bin value
#> 1   D 105    50
#> 2   E 106    60
#> 3   F 107    70
#> 4   D 106    80
unlink(tmp)
```
