# Set the name of the column recording animal ID as global variable

Set the name of the column recording animal ID as global variable

## Usage

``` r
set_id_col2(new_name = "cow")
```

## Arguments

- new_name:

  What's the name of the column recording animal ID? This should be a
  Single string. (default: `"cow"`).

## Value

Called for its side-effects

## Examples

``` r
# set global variable `id_col` as "animal_id"
set_id_col2("animal_id")
# check if we set it up correctly
id_col2()
#> [1] "animal_id"
```
