# Process file names and extract date tokens

Extracts date tokens from file names. The date is assumed to be embedded
as digits in the filename in the format of `yymmdd` (year, month, day).
The code will automatically extract the continuous digits out of the
filename (e.g. `"VR200715"` → `"200715"`; `"rmdh20250101"` →
`"20250101"`).

## Usage

``` r
file_name_processing(file_names, col_name = "dir")
```

## Arguments

- file_names:

  What are the path to all the data files that you wish to process? This
  should be a character vector of file names.

- col_name:

  We wil put all your file names into a column stored in a dataframe.
  What name do you want to give to that column? This should be a single
  character string. Default is "dir".

## Value

A data frame with columns:

- \<col_name\>:

  Original cleaned file names.

- date:

  Extracted date tokens as character strings (e.g. `"200715"`), or `NA`
  if extraction failed.

Returns an empty data frame with those two columns if `file_names` is
empty.

## Examples

``` r
file_name_processing(
  file_names = c(" feed/VR200715.DAT ", "feed/VR200716.DAT", "feed/VR200718.DAT"),
  col_name = "Feed_dir"
)
#>            Feed_dir   date
#> 1 feed/VR200715.DAT 200715
#> 2 feed/VR200716.DAT 200716
#> 3 feed/VR200718.DAT 200718
```
