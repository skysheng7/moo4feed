# Safely Read Data from a File

Attempts to read a file (comma delimited by default) into a data frame.
Please note files with the extension `.csv`, and `.DAT` are both comma
delimited files. If the file does not exist, is a directory, is empty,
or an error occurs during reading, this function returns an empty data
frame (0 rows, 0 columns).

## Usage

``` r
read_data_safely(file, sep = ",", header = FALSE)
```

## Arguments

- file:

  What's the path to your data file? This should be a single string
  giving the path to the file.

- sep:

  Field separator; passed to
  [`read.table()`](https://rdrr.io/r/utils/read.table.html). Defaults to
  `","` for comma delimited files like `.csv` and `.DAT`.

- header:

  Logical; does your data file have a header row (i.e., column names)?
  Defaults to `FALSE`. If yor file contains column names at the top,
  please set this to `TRUE`.

## Value

A data frame of the file’s contents, or an empty data frame if the file
is missing, is a directory, is empty, or cannot be read.

## Examples

``` r
# create a small CSV and read it
tmp <- tempfile(fileext = ".csv")
write.csv(data.frame(a = 1:3, b = 4:6), tmp, row.names = FALSE)
read_data_safely(tmp)
#>   V1 V2
#> 1  a  b
#> 2  1  4
#> 3  2  5
#> 4  3  6
unlink(tmp)
```
