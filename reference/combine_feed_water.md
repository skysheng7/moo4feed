# Combine Feeder and Water Data by Date

Takes two lists of data frames (feeder and water) and combines them by
row-binding each corresponding pair. The combined data frames are
returned in a new list, grouped by date.

## Usage

``` r
combine_feed_water(all_fed, all_wat)
```

## Arguments

- all_fed:

  A list of data frames containing feeder visit data.

- all_wat:

  A list of data frames containing water visit data.

## Value

A list of combined feed and water data frames, named by date.

## Examples

``` r
fed_list <- list(
  "2024-01-01" = data.frame(cow = 1:2, intake = c(10, 20)),
  "2024-01-02" = data.frame(cow = 3:4, intake = c(30, 40))
)
wat_list <- list(
  "2024-01-01" = data.frame(cow = 1:2, intake = c(5, 15)),
  "2024-01-02" = data.frame(cow = 3:4, intake = c(25, 35))
)

combine_feed_water(fed_list, wat_list)
#> $`2024-01-01`
#>   cow intake
#> 1   1     10
#> 2   2     20
#> 3   1      5
#> 4   2     15
#> 
#> $`2024-01-02`
#>   cow intake
#> 1   3     30
#> 2   4     40
#> 3   3     25
#> 4   4     35
#> 
```
