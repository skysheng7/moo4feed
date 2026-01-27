# Cattle drinking behavior data after quality check and outlier removal

A dataset containing drinking visit records for cattle over a two-day
period (2020-10-31 to 2020-11-01). This dataset is the result of
applying quality control procedures and KNN-based outlier removal to the
raw [all_wat](https://skysheng7.github.io/moo4feed/reference/all_wat.md)
data.

## Usage

``` r
clean_water
```

## Format

A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
with each data frame containing the following 10 variables:

- transponder:

  integer, unique electronic ID for each bin

- cow:

  integer, animal ID number

- bin:

  numeric, water bin location number

- start:

  POSIXct, timestamp when drinking event started

- end:

  POSIXct, timestamp when drinking event ended

- duration:

  integer, duration of drinking event in seconds

- start_weight:

  numeric, weight of water (kg) at start of drinking event

- end_weight:

  numeric, weight of water (kg) at end of drinking event

- intake:

  numeric, amount of water consumed (kg) during the event (calculated as
  start_weight - end_weight)

- date:

  Date, calendar date of the drinking event

- rate:

  numeric, how fast the cow was drinking (L/s)

## Source

[all_wat](https://skysheng7.github.io/moo4feed/reference/all_wat.md)

## Details

This dataset is the result of applying quality control checks using the
[`qc()`](https://skysheng7.github.io/moo4feed/reference/qc.md) function
to the raw
[all_wat](https://skysheng7.github.io/moo4feed/reference/all_wat.md)
data. The cleaning process includes removing invalid records, fixing
double detections, and ensuring all intake and duration values are
plausible.

## Examples

``` r
# Access quality-checked data for the first day
first_day <- clean_water[["2020-10-31"]]

# Count drinking events by cow on November 1 after quality control
table(clean_water[["2020-11-01"]]$cow)
#> 
#> 2074 3150 4001 4044 4070 4072 4080 5028 5041 5042 5058 5061 5067 5100 5114 5120 
#>   11   17    4    7    5   12   20    8   12   16   12   12   11   10   11   11 
#> 5123 5124 5135 5137 5139 5145 6005 6020 6027 6028 6030 6033 6042 6050 6055 6069 
#>   15   22   14   10   10   11    7   14   10   12    7   13   10    8   10   10 
#> 6084 6090 6121 6126 6129 7010 7018 7019 7022 7023 7024 7027 7030 7033 7043 
#>    9   14   24   22    5   10   10    9   19    9   10   10   11   22   12 
```
