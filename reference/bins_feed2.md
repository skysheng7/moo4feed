# Get the vector of all feed bins included in your study

Which feed bins are included in your study for analysis? This should be
a numeric vector of bin IDs to keep. You can supply individual values
(e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).

## Usage

``` r
bins_feed2()
```

## Value

An integer vector indicating feed bin IDs (default `1:30`).

## Examples

``` r
bins_feed2()
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30
```
