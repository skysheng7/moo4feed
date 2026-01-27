# Get the numeric **bin offset** which was set as global variable `bin_offset`

The global variable `bin_offset` will be used to rename bin ID by adding
an amount defined in `bin_offset` to each bin's original ID in
`df[[bin_col]]`. We wish to do this because sometimes water bin and feed
bin share the same ID, and we want to differentiate water bins from feed
bins using this `bin_offset`.

## Usage

``` r
bin_offset2()
```

## Value

A single numeric value to add to each matching bin ID. Default is 100.

## Examples

``` r
bin_offset2()
#> [1] 100
```
