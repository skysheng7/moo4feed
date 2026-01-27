# Cattle feeding behavior and visit record data

An example dataset containing feeding behavior and visit records for
cattle over a two-day period (2020-10-31 to 2020-11-01). Each day's data
is stored as a separate data frame within a list. This is the cleaned
data output from the vignettes (i.e., "Articles" listed on the package
website) "Data cleaning" code.

## Usage

``` r
all_fed
```

## Format

A list of 2 data frames, one for each date (2020-10-31, 2020-11-01),
with each data frame containing the following 10 variables:

- transponder:

  integer, unique electronic ID for each bin

- cow:

  integer, animal ID number

- bin:

  integer, feeding bin location number

- start:

  POSIXct, timestamp when feeding event started

- end:

  POSIXct, timestamp when feeding event ended

- duration:

  integer, duration of feeding event in seconds

- start_weight:

  numeric, weight of feed (kg) at start of feeding event

- end_weight:

  numeric, weight of feed (kg) at end of feeding event

- intake:

  numeric, amount of feed consumed (kg) during the event (calculated as
  start_weight - end_weight)

- date:

  Date, calendar date of the feeding event

## Source

Collected using an Insentec automatic feeder at University of British
Columbia Dairy Education and Research Centre from October 31 to November
1, 2020.

## Details

The dataset contains detailed feeding behavior for multiple cattle over
two consecutive days. Each entry represents a distinct feeding event
where an animal visited a feed bin.

## Examples

``` r
# Access data for the first day
first_day <- all_fed[["2020-10-31"]]

# Calculate average intake per feeding event
mean(first_day$intake)
#> [1] 0.7443296

# Count feeding events by cow on November 1
table(all_fed[["2020-11-01"]]$cow)
#> 
#> 2074 3150 4001 4044 4070 4072 4080 5028 5041 5042 5058 5061 5067 5100 5114 5120 
#>   48   62   42   61   47   53   81   66   94  128   62   71   80   92   56   78 
#> 5123 5124 5135 5137 5139 5145 6005 6020 6027 6028 6030 6033 6042 6050 6055 6069 
#>   86   89   53   80   72   79   78   78  107   65   88   85   46   72  123   88 
#> 6084 6090 6121 6126 6129 7010 7018 7019 7022 7023 7024 7027 7030 7033 7043 
#>   68   79   69   67   40   56   90   93  105   74   73  144  136   85   57 
```
