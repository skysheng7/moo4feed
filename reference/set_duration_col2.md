# Set the name of the duration column as global variable

Set the name of the duration column as global variable

## Usage

``` r
set_duration_col2(new_name = "duration")
```

## Arguments

- new_name:

  A single string indicating the new duration column name.

## Value

Called for its side-effects

## Examples

``` r
set_duration_col2("visit_duration")
duration_col2()
#> [1] "visit_duration"
```
