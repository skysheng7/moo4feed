# Get the vector of all water bins included in your study

Which water bins are included in your study for analysis? This should be
a numeric vector of bin IDs to keep. You can supply individual values
(e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`).

## Usage

``` r
bins_wat2()
```

## Value

An integer vector (default `1:5`).

## Examples

``` r
bins_wat2()
#> [1] 1 2 3 4 5
```
