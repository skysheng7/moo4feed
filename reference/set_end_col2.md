# Set the name of the column recording the end time of an event as a global variable

Name of the column recording the end time of an event, e.g.:
`end_col = "end"`.

## Usage

``` r
set_end_col2(new_name = "end")
```

## Arguments

- new_name:

  A single character string naming the column that stores the end time
  of each visit/event.

## Value

Called for its side-effects.

## Examples

``` r
# set global variable `end_col` to "end_time"
set_end_col2("end_time")
# check if `end_col` is set up correctly
end_col2()
#> [1] "end_time"
```
