# Set the name of the column recording bin ID as global variable

Set the name of the column recording bin ID as global variable

## Usage

``` r
set_bin_col2(new_name = "bin")
```

## Arguments

- new_name:

  What's the name of the column recording the ID of the bin for each
  visit? This should be a single string. (default: `"bin"`).

## Value

Called for its side-effects

## Examples

``` r
# set global variable `bin_col` as "feeder_bin"
set_bin_col2("feeder_bin")
# check if `bin_col` is set up correctly
bin_col2()
#> [1] "feeder_bin"
```
