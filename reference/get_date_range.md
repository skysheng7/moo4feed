# Get the overall date range for a set of files

What are the start and end date of your study? This function takes in a
vector of file names, and export a character string indicating the date
range of your study.

## Usage

``` r
get_date_range(file_names)
```

## Arguments

- file_names:

  What are the path to all the data files that you wish to process? This
  should be a character vector of file names.

## Value

A single character string in one of the following format:

- `"YYMMDD"`:

  when all dates in `df$date` are the same

- `"YYMMDD_YYMMDD"`:

  otherwise, concatenation of start and end dates

Returns `NA_character_` (with a warning) if `df` has zero rows.

## Examples

``` r
get_date_range(c("feed/VR200720.DAT", "feed/VR200715.DAT", "feed/VR200716.DAT"))
#> [1] "200715_200720"
```
