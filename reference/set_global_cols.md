# Set multiple global variables at once

This function allows users to set multiple global variables
simultaneously. Each parameter defaults to its current global value if
unspecified.

## Usage

``` r
set_global_cols(
  tz = tz2(),
  id_col = id_col2(),
  trans_col = trans_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  dur_col = duration_col2(),
  intake_col = intake_col2(),
  start_weight_col = start_weight_col2(),
  end_weight_col = end_weight_col2(),
  bin_offset = bin_offset2(),
  bins_feed = bins_feed2(),
  bins_wat = bins_wat2(),
  bin_layout = bin_layout2()
)
```

## Arguments

- tz:

  Timezone (default current global value from
  [`tz2()`](https://skysheng7.github.io/moo4feed/reference/tz2.md))

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- trans_col:

  Transponder column name (default current global value from
  [`trans_col2()`](https://skysheng7.github.io/moo4feed/reference/trans_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- end_col:

  End time column name (default current global value from
  [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md))

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- dur_col:

  Duration column name (default current global value from
  [`duration_col2()`](https://skysheng7.github.io/moo4feed/reference/duration_col2.md))

- intake_col:

  Intake column name (default current global value from
  [`intake_col2()`](https://skysheng7.github.io/moo4feed/reference/intake_col2.md))

- start_weight_col:

  Start weight column name (default current global value from
  [`start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/start_weight_col2.md))

- end_weight_col:

  End weight column name (default current global value from
  [`end_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/end_weight_col2.md))

- bin_offset:

  Numeric bin offset (default current global value from
  [`bin_offset2()`](https://skysheng7.github.io/moo4feed/reference/bin_offset2.md))

- bins_feed:

  Integer vector of feed bins (default current global value from
  [`bins_feed2()`](https://skysheng7.github.io/moo4feed/reference/bins_feed2.md))

- bins_wat:

  Integer vector of water bins (default current global value from
  [`bins_wat2()`](https://skysheng7.github.io/moo4feed/reference/bins_wat2.md))

- bin_layout:

  Character string of physical bin layout with rows separated by `\n`
  and bins within rows separated by "-" (default current global value
  from
  [`bin_layout2()`](https://skysheng7.github.io/moo4feed/reference/bin_layout2.md))

## Value

Called for its side-effects

## Examples

``` r
set_global_cols(tz = "UTC", id_col = "animal_id", dur_col = "visit_duration")
#> Warning: Bin IDs 1, 2, 3 appear in both feed and water bin lists. Make sure you're using updated bin IDs (e.g., water bins should be 101+, not 1-5) to avoid conflicts.
#> Note: The following bins from your feed/water bin lists are not included in the layout: 15, 16, 17, 18, 19, 20
```
