# Set a new timezone which will be used as a global variable governing the processing of timestamp data

Set a new timezone which will be used as a global variable governing the
processing of timestamp data

## Usage

``` r
set_tz2(new_tz = Sys.timezone())
```

## Arguments

- new_tz:

  A valid time zone name (default is the time zone of your current
  physical location, retrived via
  [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html)), used to
  determine DST rules. Use
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) to see all
  valid options.

## Value

Called for its side-effects

## Examples

``` r
# Originally timezone was default set to "America/Vancouver"
tz2()
#> [1] "UTC"
# Now let's change timezone (`tz`) to be "UTC"
set_tz2("UTC")
# check if timezone is set properly
tz2()
#> [1] "UTC"
```
