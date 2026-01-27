# Process matrices and add derived columns

Main function to process synchronization matrices from feeding and
drinking data. Creates time-based matrices tracking animal activity, bin
usage, and optionally feed amounts. Adds derived columns like total
number of animals active, total bins occupied, and date information. Can
handle both single data frame input or list of data frames.

## Usage

``` r
matrix_process(
  data_list,
  type = c("feed", "drink", "feed_and_drink"),
  resolution = c("sec", "min"),
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  start_weight_col = start_weight_col2(),
  end_weight_col = end_weight_col2(),
  bins_feed = bins_feed2(),
  bins_wat = bins_wat2()
)
```

## Arguments

- data_list:

  List of data frames to process or single data frame

- type:

  Character, one of 'feed', 'drink', 'feed_and_drink'

- resolution:

  Character, either "sec" (default) or "min" for time resolution

- id_col:

  Animal ID column name (default current global value from
  [`id_col2()`](https://skysheng7.github.io/moo4feed/reference/id_col2.md))

- start_col:

  Start time column name (default current global value from
  [`start_col2()`](https://skysheng7.github.io/moo4feed/reference/start_col2.md))

- end_col:

  End time column name (default current global value from
  [`end_col2()`](https://skysheng7.github.io/moo4feed/reference/end_col2.md))

- bin_col:

  Bin ID column name (default current global value from
  [`bin_col2()`](https://skysheng7.github.io/moo4feed/reference/bin_col2.md))

- start_weight_col:

  Start weight column name (default current global value from
  [`start_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/start_weight_col2.md))

- end_weight_col:

  End weight column name (default current global value from
  [`end_weight_col2()`](https://skysheng7.github.io/moo4feed/reference/end_weight_col2.md))

- bins_feed:

  Integer vector of feed bins (default current global value from
  [`bins_feed2()`](https://skysheng7.github.io/moo4feed/reference/bins_feed2.md))

- bins_wat:

  Integer vector of water bins (default current global value from
  [`bins_wat2()`](https://skysheng7.github.io/moo4feed/reference/bins_wat2.md))

## Value

List containing processed matrices (structure depends on input type and
format):

**For feed type ("feed"):**

- **synch_master_animal2**: Time × Animal activity matrix

  - Rows: Time points (filtered to active periods only)

  - Columns: Time + Animal IDs + derived columns

  - Values in each cell: 1 = animal present, 0 = animal not present

  - Derived columns: `total_animal_num`, `unoccupied_bin_num`, `date`

- **synch_master_bin2**: Time × Animal bin occupancy matrix

  - Rows: Time points (filtered to active periods only)

  - Columns: Time + Animal IDs + date column only

  - Values in each cell: Bin number animal is using (0 = not active)

  - Derived columns: `date` only

- **synch_master_feed2**: Time × Bin feed amount matrix

  - Rows: Time points (filtered to active periods only)

  - Columns: Time + Bin numbers + derived columns

  - Values in each cell: Feed weight at each bin

  - Derived columns: `totalFeed`, `date`

**For drink/feed_and_drink types ("drink", "feed_and_drink"):**

- **synch_master_animal2**: Same as above but for drinking activity

- **synch_master_bin2**: Same as above but for water bin occupancy

**Note:** If input is a single data frame, returns individual matrices.
If input is a list, returns lists of matrices (one per date).

## Examples

``` r
# Create toy data with more realistic structure
toy_data <- list(
  day1 = data.frame(
    animal = c(1, 2),
    start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
    end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
    bin = c(1, 2),
    start_weight = c(10.5, 8.3),
    end_weight = c(10.2, 8.1)
  )
)


# Process with bin
result <- matrix_process(toy_data, type = "feed", 
                                  id_col = "animal", start_col = "start", 
                                  end_col = "end", bin_col = "bin",
                                  start_weight_col = "start_weight",
                                  end_weight_col = "end_weight",
                                  bins_feed = 1:2)
names(result)
#> [1] "synch_master_animal2" "synch_master_bin2"    "synch_master_feed2"  
```
