# Get the physical layout order of bins for spatial analysis

Returns the physical arrangement order of feed and water bins as they
are positioned in the facility. This is used for neighbor analysis and
spatial synchronicity studies where the physical proximity of bins
matters.

## Usage

``` r
bin_layout2()
```

## Value

A character string representing the physical bin layout. **Note**: Water
bins use updated IDs to avoid conflicts with feed bin numbering.

## Details

The layout is specified as a string where:

- Bins within the same row are separated by "-"

- Different rows are separated by `\n` (newline)

- Only bins in the same row are considered spatial neighbors

## Examples

``` r
bin_layout2()
#> [1] "1-2-3-4-5-6-101-102-7-8-9-10-11-12-13-14-15-16-17-18-103-104-19-20-21-22-23-24-25-26-27-28-29-30-105"
```
