# Set the numeric **bin offset**

The global variable `bin_offset` will be used to rename bin ID by adding
an amount defined in `bin_offset` to each bin's original ID in
`df[[bin_col]]`. We wish to do this because sometimes water bin and feed
bin share the same ID, and we want to differentiate water bins from feed
bins using this `bin_offset`.

## Usage

``` r
set_bin_offset2(new_offset = 100)
```

## Arguments

- new_offset:

  A single numeric value to add to each matching bin ID. Default is 100.

## Value

Called for its side-effects

## Examples

``` r
# set global variable `bin_offset` as 50
set_bin_offset2(50)
# check if `bin_offset` is set up correctly
bin_offset2()
#> [1] 50
```
