# Process a batch of feeder files

Reads, cleans, and time‑adjusts a batch of files, returning a named list
of data frames keyed by file date (YYYY‑MM‑DD).

## Usage

``` r
process_all_feed(
  files,
  col_names = NULL,
  id_col = id_col2(),
  drop_ids = NULL,
  trans_col = trans_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  drop_trans = NULL,
  bin_col = bin_col2(),
  bins = bins_feed2(),
  select_cols = NULL,
  sep = ",",
  header = FALSE,
  daylight_change_duration = 60,
  tz = tz2(),
  adjust_dst = TRUE
)
```

## Arguments

- files:

  What are the files you wish to process? This should be a character
  vector of all the file paths.

- col_names:

  A character vector of column names to assign when `header = FALSE`.
  This vector must match the number of columns in the raw data. If
  `header = TRUE`, the file’s existing column names are used and
  `col_names` is ignored.

- id_col:

  What's the name of the column recording animal ID? This should be a
  Single string. (default: `"cow"`).

- drop_ids:

  Which animals do you wish to drop? This should be a vector indicating
  values in `id_col` that you wish to remove (default: `NULL`, so remove
  nothing).

- trans_col:

  What's the name of the column recording transponder ID for each visit?
  This should be a single string. (default: `"transponder"`).

- start_col:

  Name of the column recording the start time of an event (quoted),
  e.g.: start_col = "start"

- end_col:

  Name of the column recording the end time of an event (quoted). e.g.:
  end_col = "end"

- drop_trans:

  Which transponders do you wish to delete because they are not part of
  your study? This should be a vector indicating values in `trans_col`
  that you wish to remove (default: `NULL`, so remove nothing).

- bin_col:

  What's the name of the column recording the ID of the bin for each
  visit? This should be a single string. (default: `"bin"`).

- bins:

  Which feed bins are included in your study for analysis? This should
  be a numeric vector of bin IDs to keep. You can supply individual
  values (e.g. `c(1, 3, 5)`) or a sequence (e.g. `2:4`). Default is set
  to 1:30

- select_cols:

  Which columns in the dataframe do you wish to keep in your original
  data frame after cleaning? This should be a character vector
  indicating columns to retain in the final output. Default is `NULL`,
  so we select all columns.

- sep:

  Field separator; passed to
  [`read.table()`](https://rdrr.io/r/utils/read.table.html). Defaults to
  `","` for comma delimited files like `.csv` and `.DAT`.

- header:

  Logical; does your data file have a header row (i.e., column names)?
  Defaults to `FALSE`. If yor file contains column names at the top,
  please set this to `TRUE`.

- daylight_change_duration:

  How many minutes does the clock jump or fall back on the day of
  daylight saving change? This should be an integer for the duration in
  minutes (default = 60).

- tz:

  A valid time zone name (default is "America/Vancouver"), used to
  determine DST rules. Use
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) to see all
  valid options.

- adjust_dst:

  Do you want to apply the function
  ([`daylight_saving_adjust()`](https://skysheng7.github.io/moo4feed/reference/daylight_saving_adjust.md))
  I designed to adjust timestamp for dates affected by Daylight Saving
  Time changes or not? This should be logical, default is TRUE. The
  timestamp adjustment would only be applied if `adjust_dst` is TRUE and
  `tz` is set to be a timezone in North America.

## Value

A **named** list of data frames, one per input file, named by date
(YYYY‑MM-DD). Within each datafarme, there is processed `date` column.

## Details

Steps:

1.  **Validate** all inputs (types, `file_type` ∈ `"feed","water"`).

2.  **Extract** a date information from each filename and parse it via
    [`lubridate::ymd()`](https://lubridate.tidyverse.org/reference/ymd.html).

3.  **Fetch** Daylight Saving Time (DST) switch table for the relevant
    years
    ([`dst_switch_day()`](https://skysheng7.github.io/moo4feed/reference/dst_switch_day.md)).

4.  **Loop** over each file:

    - Call either
      [`process_feeder()`](https://skysheng7.github.io/moo4feed/reference/process_feeder.md)
      or
      [`process_water()`](https://skysheng7.github.io/moo4feed/reference/process_water.md)
      to do: - Safely read the CSV / DAT file - Rename columns - Drop
      unwanted cows & transponders - Keep only specified bins - Subset
      to desired columns

    - Drop any rows with `NA`.

    - Call
      [`daylight_saving_adjust()`](https://skysheng7.github.io/moo4feed/reference/daylight_saving_adjust.md)
      to adjust timestamps for daylight saving change days.

    - Standardize the columns recording start and end time of each event
      to be in the format of "yyyy-mm-dd hh:mm:ss".

    - Store processed dataframe the output list and name it by the date.

## See also

- [`process_feeder`](https://skysheng7.github.io/moo4feed/reference/process_feeder.md)

- [`process_water`](https://skysheng7.github.io/moo4feed/reference/process_water.md)

- [`file_name_processing`](https://skysheng7.github.io/moo4feed/reference/file_name_processing.md)

- [`dst_switch_day`](https://skysheng7.github.io/moo4feed/reference/dst_switch_day.md)

- [`daylight_saving_adjust`](https://skysheng7.github.io/moo4feed/reference/daylight_saving_adjust.md)

## Examples

``` r
tmp <- tempdir()
# create two CSVs in a temporary directory
files <- file.path(tmp, paste0("VR2022010", 1:3, ".csv"))
for (i in seq_along(files)) {
  write.csv(
    data.frame(
      cow         = c("A", "B", "C"),
      transponder = c("X1", "X2", "X3"),
      bin         = i + 0:2,
      start       = c("01:00:00", "02:00:00", "03:00:00"),
      end         = c("01:05:00", "02:06:01", "03:03:00")
    ),
    file = files[i],
    row.names = FALSE
  )
}

res <- process_all_feed(
  files       = files,
  bins        = 1:10,
  select_cols = c("cow", "bin", "start", "end"),
  sep         = ",",
  header      = TRUE,
  tz          = "America/Vancouver"
)

res
#> $`2022-01-01`
#>   cow bin               start                 end       date
#> 1   A   1 2022-01-01 01:00:00 2022-01-01 01:05:00 2022-01-01
#> 2   B   2 2022-01-01 02:00:00 2022-01-01 02:06:01 2022-01-01
#> 3   C   3 2022-01-01 03:00:00 2022-01-01 03:03:00 2022-01-01
#> 
#> $`2022-01-02`
#>   cow bin               start                 end       date
#> 1   A   2 2022-01-02 01:00:00 2022-01-02 01:05:00 2022-01-02
#> 2   B   3 2022-01-02 02:00:00 2022-01-02 02:06:01 2022-01-02
#> 3   C   4 2022-01-02 03:00:00 2022-01-02 03:03:00 2022-01-02
#> 
#> $`2022-01-03`
#>   cow bin               start                 end       date
#> 1   A   3 2022-01-03 01:00:00 2022-01-03 01:05:00 2022-01-03
#> 2   B   4 2022-01-03 02:00:00 2022-01-03 02:06:01 2022-01-03
#> 3   C   5 2022-01-03 03:00:00 2022-01-03 03:03:00 2022-01-03
#> 
```
