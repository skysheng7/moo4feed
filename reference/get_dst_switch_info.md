# Get Daylight Saving Time (DST) Switch Dates and Exact Transition Times

Generate a data frame recording both the dates and exact times (hour and
minute) when DST transitions occurs in the specified timezone, default
is the timezone your computer is located at.

## Usage

``` r
get_dst_switch_info(years = c(2020, 2021), tz = tz2(), interval = 1)
```

## Arguments

- years:

  A numeric vector specifying the range of years to evaluate. Use the
  format `c(start_year, end_year)`, e.g., `years = c(2020, 2021)`. Must
  contain at least one numeric value.

- tz:

  A valid time zone name (default is "America/Vancouver"), used to
  determine DST rules. Use
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) to see all
  valid options.

- interval:

  An integer (1–59) specifying the time resolution in minutes used to
  detect DST changes (default: 1, meaning 1 minute). You almost never
  need to change this.

## Value

A data frame with columns:

- year:

  The year of DST transitions

- spring:

  Date when DST begins ("spring forward")

- fall:

  Date when DST ends ("fall back")

- spring_next_day:

  The day after spring DST change

- fall_next_day:

  The day after fall DST change

- spring_time:

  Exact timestamp just before DST starts

- fall_time:

  Exact timestamp just before DST ends

Returns an empty data frame with only column names if no transitions are
found.

## Details

Internally calls the internal functions
[`dst_switch_day()`](https://skysheng7.github.io/moo4feed/reference/dst_switch_day.md)
to get the switch dates, and
[`dst_switch_hm()`](https://skysheng7.github.io/moo4feed/reference/dst_switch_hm.md)
to detect the transition times for each spring and fall date. If
transition times cannot be detected, `NA` values are returned in the
corresponding columns.

## Examples

``` r
dst_full <- get_dst_switch_info(years = 2021:2022, tz = "America/Vancouver")
```
