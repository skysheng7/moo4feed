# Set the vector of all water bins included in your study as global variable

Which water bins are included in your study for analysis? This should be
a numeric vector of bin IDs to keep. You can supply individual values
(e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).

## Usage

``` r
set_bins_wat2(new_bins = 1:5)
```

## Arguments

- new_bins:

  An integer vector of valid water bin ID

## Value

Called for its side-effects

## Examples

``` r
# set global variable `bins_wat` as 1:3
set_bins_wat2(1:3)
# check if `bins_wat` is set up correctly
bins_wat2()
#> [1] 1 2 3
```
