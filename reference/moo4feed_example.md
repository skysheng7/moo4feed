# Access Example Data Files Shipped with **moo4feed**

A lightweight helper that exposes the data files located in the
package’s `inst/extdata/` directory.

- If `path` is `NULL` (default) the function **lists** every file
  available in that directory, making it easy to discover the bundled
  examples.

- If `path` is a single file name (e.g., `"VR201102.DAT"`), the function
  returns the **absolute path** to that file so it can be opened with
  functions such as
  [`read_data_safely()`](https://skysheng7.github.io/moo4feed/reference/read_data_safely.md)
  or base‐R I/O utilities.

## Usage

``` r
moo4feed_example(path = NULL)
```

## Arguments

- path:

  *Optional.* A single character string giving the name of an example
  file to retrieve. Use `NULL` (the default) to list all available
  example files.

## Value

- **`path = NULL`** — a character vector of file names contained in the
  package’s `extdata` directory.

- **`path` is a file name** — a single character string with the full
  path to that file.

## Details

Internally this is a thin wrapper around
[`base::system.file()`](https://rdrr.io/r/base/system.file.html), so it
inherits its behaviour—most notably the `mustWork = TRUE` argument,
which triggers an error if the requested file does not exist.

## Examples

``` r
# List every example data file shipped with the package
moo4feed_example()
#> [1] "VR201031.DAT" "VR201101.DAT" "VW201031.DAT" "VW201101.DAT"

# Retrieve the full path to one specific file
file_path <- moo4feed_example("VR201101.DAT")
file_path
#> [1] "/home/runner/work/_temp/Library/moo4feed/extdata/VR201101.DAT"
```
