# Shift specified bin IDs by an offset

Rename bin ID by adding an amount defined in `bin_offset` to each bin's
original ID in `df[[bin_col]]`. Only rename bins that matches one of the
ID in the vector `bins`, preserving all other rows unchanged. We wish to
do this because sometimes water bin and feed bin share the same ID, and
we want to differentiate water bins from feed bins using this
`bin_offset`.

## Usage

``` r
rename_bins(df, bins, bin_offset = bin_offset2(), bin_col = bin_col2())
```

## Arguments

- df:

  A data frame containing a column of bin IDs.

- bins:

  Which bins do you wish to rename? This should be a numeric vector of
  the ID of bins that we are interested in renaming. You can supply
  individual values (e.g. `c(2, 4, 6)`) or a sequence (e.g. `3:5`).

- bin_offset:

  A single numeric value to add to each matching bin ID. Default is 100.

- bin_col:

  What's the name of the column noting the ID of the bin in each visit?
  This should be a single string naming the column recording bin IDs
  (default: `"bin"`).

## Value

A data frame identical to `df` but with `df[[bin_col]]` updated: for
each row where `df[[bin_col]] %in% bins`, the new value is
`old_value + bin_offset`.

## Examples

``` r
df <- data.frame(Bin = 1:5, Value = 10:14)
print(df)
#>   Bin Value
#> 1   1    10
#> 2   2    11
#> 3   3    12
#> 4   4    13
#> 5   5    14
# shift bins 2,3,4 by +100 → become 102,103,104
rename_bins(df, bins = 2:4, bin_offset = 100, bin_col = "Bin")
#>   Bin Value
#> 1   1    10
#> 2 102    11
#> 3 103    12
#> 4 104    13
#> 5   5    14
```
