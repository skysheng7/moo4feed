# Identify and validate replacement events across multiple days

This function detects potential replacement behaviors in feeding or
drinking bins across multiple days of data, and validates them by
checking if actor cows were simultaneously engaged elsewhere (i.e., have
an alibi). A replacement is defined as instances where the time interval
between one cow leaving and the next entering a bin is \< 26 seconds.

## Usage

``` r
record_replacement_days(
  comb,
  cfg = qc_config(),
  id_col = id_col2(),
  bin_col = bin_col2(),
  start_col = start_col2(),
  end_col = end_col2()
)
```

## Arguments

- comb:

  List of daily data frames (feed, water or combined).

- cfg:

  A configuration list created by
  [`qc_config()`](https://skysheng7.github.io/moo4feed/reference/qc_config.md).

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- end_col:

  End time column name (default current global value from
  [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md))

## Value

A named list of data frames, one per day, containing validated
replacement events.

## Details

This function first calls internal function `record_replacement_day()`
and applies it across all elements of `comb` to identify replacements.
It then calls internal function `check_alibi_days()` to validate those
events by removing actor cows with an alibi.

## Examples

``` r
# Use example data from the built-in all_fed dataset
valid_replacements <- record_replacement_days(all_fed)
head(valid_replacements[[1]])
#>   reactor_cow bin                time       date actor_cow bout_interval
#> 1        5124   1 2020-10-31 06:07:52 2020-10-31      6020           10s
#> 2        6020   1 2020-10-31 06:09:44 2020-10-31      6069           11s
#> 3        6069   1 2020-10-31 06:12:05 2020-10-31      5124           10s
#> 4        5124   1 2020-10-31 06:13:37 2020-10-31      5067           10s
#> 5        7010   1 2020-10-31 06:20:15 2020-10-31      7018           11s
#> 6        7018   1 2020-10-31 06:20:58 2020-10-31      7010            9s
```
