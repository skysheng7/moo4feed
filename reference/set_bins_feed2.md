# Set the vector of all feed bins included in your study as global variable

Which feed bins are included in your study for analysis? This should be
a numeric vector of bin IDs to keep. You can supply individual values
(e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).

## Usage

``` r
set_bins_feed2(new_bins = 1:30)
```

## Arguments

- new_bins:

  An integer vector of valid feed bin ID

## Value

Called for its side-effects

## Examples

``` r
# set global variable `bins_feed` as 1:20
set_bins_feed2(1:20)
# check if `bins_feed` is set up correctly
bins_feed2()
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
```
