# Set the physical layout order of bins as global variable

Define the physical arrangement order of feed and water bins as they are
positioned in the facility. This is used for neighbor analysis and
spatial synchronicity studies where the physical proximity of bins
matters.

## Usage

``` r
set_bin_layout2(new_layout = "1-2-3\n4-5-6")
```

## Arguments

- new_layout:

  A character string specifying the physical layout of bins. Should
  include feed bins (from
  [`bins_feed2()`](https://skysheng7.github.io/moo4feed/reference/bins_feed2.md))
  and water bins (from
  [`bins_wat2()`](https://skysheng7.github.io/moo4feed/reference/bins_wat2.md))
  in their actual physical arrangement. **Important**: Use the updated
  bin IDs (e.g., water bins should be 101, 102, etc., not 1, 2) to avoid
  conflicts between feed and water bin numbering systems.

## Value

Called for its side-effects

## Details

The layout should be specified as a string where:

- Bins within the same row are separated by "-"

- Different rows are separated by `\n` (newline)

- Only bins in the same row are considered spatial neighbors
  (left/right)

- Bins in different rows are never neighbors, even if they're vertically
  aligned

## Examples

``` r
# Single row layout
set_bin_layout2("1-2-101-3-4-102-5-6")
#> Note: Bin offset is set to 100. During data processing, water bins 1-5 will be transformed to 101-105.
#> Note: The following bins from your feed/water bin lists are not included in the layout. Please ignore this message if this is intended. Otherwise you need to update your bin layout: 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 103, 104, 105

# Multiple row layout (3 rows)
set_bin_layout2("1-2-3-4-5\n6-7-8-9-10-11\n12-13-14")
#> Note: Bin offset is set to 100. During data processing, water bins 1-5 will be transformed to 101-105.
#> Note: The following bins from your feed/water bin lists are not included in the layout. Please ignore this message if this is intended. Otherwise you need to update your bin layout: 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 101, 102, 103, 104, 105

# Check if bin_layout is set up correctly
bin_layout2()
#> [1] "1-2-3-4-5\n6-7-8-9-10-11\n12-13-14"
```
