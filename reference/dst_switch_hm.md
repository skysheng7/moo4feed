# Detect the Exact Time (hour and minute; hm) of Daylight Saving Time (DST) Change

Identifies the precise local time when a DST transition occurs on a
given date in a specified time zone.

## Usage

``` r
dst_switch_hm(date, tz = tz2(), interval = 1)
```

## Arguments

- date:

  A string in "YYYY-MM-DD" format or a Date object.

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

A POSIXct object indicating the time just before the DST transition. If
no DST change is detected on the specified date, the function returns
`NULL` with a warning.

## Details

The function detects DST changes using differences in DST status across
time intervals during the given date. It handles "skipped" or
"duplicated" times (as occurs during spring and fall transitions) using
both `dst()` and `NA` time gaps. In the case of ambiguous or
unresolvable transitions, `NULL` is returned with a warning.

## Examples

``` r
dst_switch_hm("2020-03-08", tz = "America/Vancouver")
#> [1] "2020-03-08 01:59:00 PST"
dst_switch_hm("2020-11-01", tz = "America/Vancouver")
#> [1] "2020-11-01 00:59:00 PDT"
dst_switch_hm("2020-07-01", tz = "Etc/UTC") # No DST change
#> Warning: No DST transition detected on this date in the specified time zone.
#> NULL
```
