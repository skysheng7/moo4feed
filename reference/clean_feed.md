# Cattle feeding behavior data after quality check and outlier removal

A dataset containing feeding visit records for cattle over a two-day
period (2020-10-31 to 2020-11-01). This dataset is the result of
applying quality control procedures and KNN-based outlier removal to the
raw [all_fed](https://skysheng7.github.io/moo4feed/reference/all_fed.md)
data.

## Usage

``` r
clean_feed
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

- rate:

  numeric, how fast the cow was eating (kg/s)

## Source

[all_fed](https://skysheng7.github.io/moo4feed/reference/all_fed.md)

## Details

This dataset is the result of applying quality control checks using the
[`qc()`](https://skysheng7.github.io/moo4feed/reference/qc.md) function
to the raw
[all_fed](https://skysheng7.github.io/moo4feed/reference/all_fed.md)
data. The cleaning process includes removing invalid records, fixing
double detections, and ensuring all intake and duration values are
plausible. Details are as follows:

## Examples

``` r
# Access quality-checked data for the first day
first_day <- clean_feed[["2020-10-31"]]

# Count feeding events by cow on November 1 after quality control
table(clean_feed[["2020-11-01"]]$cow)
#> 
#> 2074 3150 4001 4044 4070 4072 4080 5028 5041 5042 5058 5061 5067 5100 5114 5120 
#>   48   62   41   61   46   53   78   64   93  124   61   71   80   91   54   77 
#> 5123 5124 5135 5137 5139 5145 6005 6020 6027 6028 6030 6033 6042 6050 6055 6069 
#>   84   88   52   78   70   78   77   76  104   61   84   78   46   71  121   86 
#> 6084 6090 6121 6126 6129 7010 7018 7019 7022 7023 7024 7027 7030 7033 7043 
#>   66   77   68   65   39   56   88   88  104   73   71  141  134   82   56 
```
