# Set the name of the column recording the start time of an event as global variable

Name of the column recording the start time of an event, e.g.: start_col
= "start"

## Usage

``` r
set_start_col2(new_name = "start")
```

## Arguments

- new_name:

  A single character string naming the column that stores the start time
  of each visit/event.

## Value

Called for its side-effects

## Examples

``` r
# set global variable `start_col` as "start_time"
set_start_col2("start_time")
# check if `start_col` is set up correctly
start_col2()
#> [1] "start_time"
```
