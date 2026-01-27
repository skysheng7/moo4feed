# Compare feed and water file names by shared dates

Idealy we want to have both feeder and water data for each day. This
function identifies the subset of feed and water file names that share
the same extracted dates, returning only files from dates when both
feeder and water data exist.

## Usage

``` r
compare_files(file_names_feed, file_names_water)
```

## Arguments

- file_names_feed:

  A character vector of feed file names.

- file_names_water:

  A character vector of water file names.

## Value

A list with components:

- feed:

  Character vector of feed file names that have matching water data.

- water:

  Character vector of water file names that have matching feed data.

If no common dates are found, both components are empty character
vectors.

## Examples

``` r
compare_files(
  c("feed/VR200715.DAT", "feed/VR200716.DAT"),
  c("water/VW200715.DAT", "water/VW200717.DAT")
)
#> $feed
#> [1] "feed/VR200715.DAT"
#> 
#> $water
#> [1] "water/VW200715.DAT"
#> 
```
