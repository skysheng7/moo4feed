# Cattle water drinking behavior and visit record data

An example dataset containing water drinking behavior and visit records
for cattle over a two-day period (2020-10-31 to 2020-11-01). Each day's
data is stored as a separate data frame within a list. This is the
cleaned data output from the vignettes (i.e., "Articles" listed on the
package website) "Data cleaning" code.

## Usage

``` r
all_wat
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

## Source

Collected using an Insentec automatic waterer at University of British
Columbia Dairy Education and Research Centre from October 31 to November
1, 2020.

## Details

The dataset contains detailed water drinking behavior for multiple
cattle over two consecutive days. Each entry represents a distinct
drinking event where an animal visited a water bin.

## Examples

``` r
# Access data for the first day
first_day <- all_wat[["2020-10-31"]]

# Calculate average water intake per drinking event
mean(first_day$intake)
#> [1] 8.46451

# Count drinking events by cow on November 1
table(all_wat[["2020-11-01"]]$cow)
#> 
#> 2074 3150 4001 4044 4070 4072 4080 5028 5041 5042 5058 5061 5067 5100 5114 5120 
#>   11   17    4    8    5   13   20    8   12   17   12   12   11   10   11   11 
#> 5123 5124 5135 5137 5139 5145 6005 6020 6027 6028 6030 6033 6042 6050 6055 6069 
#>   15   22   14   10   10   11    7   14   10   12    7   14   10    8   11   11 
#> 6084 6090 6121 6126 6129 7010 7018 7019 7022 7023 7024 7027 7030 7033 7043 
#>   10   14   24   23    5   10   10    9   19    9   10   10   11   22   12 
```
