# Set the name of the column recording transponder ID as global variable

Set the name of the column recording transponder ID as global variable

## Usage

``` r
set_trans_col2(new_name = "transponder")
```

## Arguments

- new_name:

  What's the name of the column recording transponder ID for each visit?
  This should be a single character string. (default: `"transponder"`).

## Value

Called for its side-effects

## Examples

``` r
# set global variable `trans_col` as "tag_id"
set_trans_col2("tag_id")
# check if `trans_col` is set up correctly
trans_col2()
#> [1] "tag_id"
```
