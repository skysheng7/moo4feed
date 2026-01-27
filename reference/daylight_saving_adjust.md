# Adjust Time Stamp for Daylight Saving Time (DST) Transitions in North America

Please note this function's logic currently is based on**North America**
only. This function adjusts a dataframe of visit events (e.g., feed or
water visits) based on DST changes. It detects whether the current date
is a DST transition day (in spring or fall), or the day after the spring
DST change, and applies the appropriate time correction logic. The
adjustment logic for each case is as follows:

## Usage

``` r
daylight_saving_adjust(
  data_frame,
  date,
  start_col = start_col2(),
  end_col = end_col2(),
  dst_df,
  daylight_change_duration = 60,
  tz = tz2()
)
```

## Arguments

- data_frame:

  A data frame with two columns representing the start and end time of
  each visit, formatted as "HH:MM:SS".

- date:

  A string in "YYYY-MM-DD" format or a Date object.

- start_col:

  Name of the column recording the start time of an event (quoted),
  e.g.: start_col = "start"

- end_col:

  Name of the column recording the end time of an event (quoted). e.g.:
  end_col = "end"

- dst_df:

  A data frame created by
  [`get_dst_switch_info`](https://skysheng7.github.io/moo4feed/reference/get_dst_switch_info.md),
  containing DST change dates and times for each year.

- daylight_change_duration:

  How many minutes does the clock jump or fall back on the day of
  daylight saving change? This should be an integer for the duration in
  minutes (default = 60).

- tz:

  A valid time zone name (default is "America/Vancouver"), used to
  determine DST rules. Use
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) to see all
  valid options.

## Value

A data frame adjusted for daylight saving time, or the original data if
no adjustment applies.

## Details

**Fall DST Change Day (e.g., 1st Sunday of November):**

- North American clocks fall back from 2:00 AM to 1:00 AM, repeating the
  hour from 1:00 to 2:00 AM.

- Some sensors do not repeat the 1–2 AM hour but instead continue
  recording 2–3 AM as if time never changed.

- This function:

  - Removes visits that occur in the ambiguous hour (2:00 AM–3:00 AM, as
    recorded).

  - Shifts all visits after 3:00 AM back by 1 hour to align with the
    post-fallback clock time.

**Spring DST Change Day (e.g., 2nd Sunday of March):**

- North American clocks in the spring will jump forward from 2:00 AM to
  3:00 AM, skipping the hour between 2–3 AM.

- Some sensors continue recording local time, resulting in inconsistent
  timestamps.

- This function:

  - Removes visits that start before the DST jump (2:00 AM) and end
    during the skipped hour (2:00–3:00 AM).

  - Shifts all visits that occur after 2:00 AM forward by 1 hour to
    align with the new clock time.

**Day After Spring DST Change:**

- On the day after spring DST change, sensors may still include
  late-night entries from the previous (DST) day, due to time
  "over-spill"

- This function:

  - Identifies the first time point where time resets, based on the
    start time column.

  - Removes all rows all entries from the previous day if there is data
    spillover due to DST change.

## Examples

``` r
dst_info <- get_dst_switch_info(years = 2021, tz = "America/Vancouver")
df <- data.frame(
  Start = c("01:30:00", "02:30:00", "04:00:00"),
  End = c("02:00:00", "03:00:00", "04:30:00")
)
print(df)
#>      Start      End
#> 1 01:30:00 02:00:00
#> 2 02:30:00 03:00:00
#> 3 04:00:00 04:30:00
daylight_saving_adjust(df,
  date = "2021-11-07",
  start_col = "Start",
  end_col = "End",
  dst_df = dst_info,
  tz = "America/Vancouver"
)
#>      Start       End
#> 1 3H 0M 0S 3H 30M 0S
```
