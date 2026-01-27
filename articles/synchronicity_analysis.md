# 5. Synchronicity Analysis

``` r
library(moo4feed)
library(ggplot2)
library(dplyr)
```

## 1. 🚨 Important: Set Your Global Variables First!

Before analyzing synchronicity patterns, configure global variables to
match your data structure:

``` r
# Configure global variables for your data structure
set_global_cols(
  id_col = "cow",           # Animal ID column name
  start_col = "start",      # Visit start time column
  end_col = "end",          # Visit end time column
  bin_col = "bin",          # Bin/feeder ID column
  start_weight_col = "start_weight",  # Start weight column
  end_weight_col = "end_weight",      # End weight column
  tz = "America/Vancouver", # Your timezone
  bins_feed = 1:30,         # Feed bin IDs
  bins_wat = 1:5,           # Water bin IDs
  bin_layout = "1-2-3-4-5-6-101-102-7-8-9-10-11-12-13-14-15-16-17-18-103-104-19-20-21-22-23-24-25-26-27-28-29-30-105"
)

# Verify configuration
cat("✅ Global variables configured:\n")
#> ✅ Global variables configured:
cat("ID column:", id_col2(), "\n")
#> ID column: cow
cat("Start time column:", start_col2(), "\n")
#> Start time column: start
cat("End time column:", end_col2(), "\n")
#> End time column: end
cat("Bin column:", bin_col2(), "\n")
#> Bin column: bin
cat("Timezone:", tz2(), "\n")
#> Timezone: America/Vancouver
cat("Bin layout:", bin_layout2(), "\n")
#> Bin layout: 1-2-3-4-5-6-101-102-7-8-9-10-11-12-13-14-15-16-17-18-103-104-19-20-21-22-23-24-25-26-27-28-29-30-105
```

## 2. Introduction to Synchronicity Analysis

Synchronicity in animal behavior refers to animals performing the same
activity at the same time. Understanding synchronicity helps researchers
identify social bonds and competition patterns. We will analyze two
types of synchronicity: pair-wise co-occurrence and spatial neighbor
proximity.

> 1.  **Pair-wise co-occurrence**: How often do 2 animals feed or drink
>     simultaneously, regardless of location (they can be right next to
>     each other or be physically far apart)?
> 2.  **Spatial neighbor proximity**: How often do 2 animals feed or
>     drink right next to each other (spatially close)?

## 3. Prerequisites

This tutorial assumes you have completed data cleaning from **Tutorial
1: Data Cleaning**. You need cleaned feeding or drinking data.

## 4. Data Preparation

``` r
# Load cleaned example data
data(clean_feed)
data(clean_water)

# If using your own data from previous tutorials:
# clean_feed <- your_cleaned_feed_data
# clean_water <- your_cleaned_water_data
```

## 5. Creating Time-Based Activity Matrices

Before analyzing synchronicity, we need to transform visit-based data
into time-based matrices. The
[`matrix_process()`](https://skysheng7.github.io/moo4feed/reference/matrix_process.md)
function creates matrices where:

- **Rows** represent time points (by second or minute)
- **Columns** represent individual animals
- **Values** indicate activity status at each time point

[`matrix_process()`](https://skysheng7.github.io/moo4feed/reference/matrix_process.md)
creates three types of matrices:

1.  **Animal activity matrix** (`synch_master_animal2`): Shows which
    animals are feeding or drinking (i.e., active) at each time point (1
    = active, 0 = inactive)
2.  **Bin occupancy matrix** (`synch_master_bin2`): Shows which bin each
    animal is using at each time point (0 = not using any bin)
3.  **Feed weight matrix** (`synch_master_feed2`): For feed data only,
    shows feed weight at each bin over time

The matrices are filtered to include only time points when at least one
animal is feeding or drinking, to reduce output data file size.

### Process Feed Data Matrices

``` r
# Process feed data to create time-based matrices
# Resolution "sec" means one row per second (more detailed but will generate larger files)
# Resolution "min" means one row per minute (less detailed but will generate smaller files)
feed_matrices <- matrix_process(
  data_list = clean_feed,
  type = "feed",
  resolution = "sec",  # Try "min" for faster processing with less detail
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  start_weight_col = start_weight_col2(),
  end_weight_col = end_weight_col2(),
  bins_feed = bins_feed2()
)
```

### Process Water Data Matrices

``` r
# Process water data similarly
water_matrices <- matrix_process(
  data_list = clean_water,
  type = "drink",
  resolution = "sec",
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  bins_wat = bins_wat2()
)
```

## 6. Pair-wise Co-Occurrence Analysis

Pair-wise analysis examines how much time every 2 animals spend feeding
or drinking simultaneously, regardless of their spatial location.

### Analyze Feed Synchronicity

``` r
# Analyze pair-wise co-occurrence for feeding
pair_feed_results <- synch_pair_analysis(
  matrix_data = feed_matrices,
  type = "feed",
  resolution = "sec",
  id_col = id_col2()
)

# Examine the structure
cat("Pair analysis results:\n")
#> Pair analysis results:
names(pair_feed_results)
#> [1] "bout"         "total_time"   "avg_duration"

# Look at bout counts for the first day
cat("\nBout matrix (first 5 animals, first day):\n")
#> 
#> Bout matrix (first 5 animals, first day):
bout_matrix_day1 <- pair_feed_results$bout[[1]]
print(bout_matrix_day1[1:5, 1:5])
#>      2074 3150 4001 4044 4070
#> 2074    0   31   11    7    5
#> 3150    0    0    8   15   28
#> 4001    0    0    0   32   23
#> 4044    0    0    0    0   36
#> 4070    0    0    0    0    0

# Look at total time together
cat("\nTotal time together matrix (seconds, first 5 animals, first day):\n")
#> 
#> Total time together matrix (seconds, first 5 animals, first day):
time_matrix_day1 <- pair_feed_results$total_time[[1]]
print(time_matrix_day1[1:5, 1:5])
#>      2074 3150 4001 4044 4070
#> 2074    0 4693 1564 1374 1176
#> 3150    0    0 1010 1637 3766
#> 4001    0    0    0 2709 2297
#> 4044    0    0    0    0 4237
#> 4070    0    0    0    0    0

# Look at average duration per bout
cat("\nAverage duration per bout (seconds, first 5 animals, first day):\n")
#> 
#> Average duration per bout (seconds, first 5 animals, first day):
avg_matrix_day1 <- pair_feed_results$avg_duration[[1]]
print(avg_matrix_day1[1:5, 1:5])
#>      2074     3150     4001      4044      4070
#> 2074    0 151.3871 142.1818 196.28571 235.20000
#> 3150    0   0.0000 126.2500 109.13333 134.50000
#> 4001    0   0.0000   0.0000  84.65625  99.86957
#> 4044    0   0.0000   0.0000   0.00000 117.69444
#> 4070    0   0.0000   0.0000   0.00000   0.00000
```

### Interpreting Pair-wise Co-Occurrence Results

The results contain three matrices for each day:

- **bout**: Number of distinct feeding or drinking bouts each pair spent
  together. A new bout starts when animals stop feeding or drinking
  together for more than 1 second (or 1 minute if using “min”
  resolution).
- **total_time**: Total time (in seconds or minutes) each pair spent
  feeding or drinking simultaneously.
- **avg_duration**: Average duration per bout for each pair.

**Note**: Only the upper triangle of each matrix is filled (values above
the diagonal) because the matrix is symmetrical.

### Finding Most Synchronized Pairs

``` r
# Find pairs with the most time spent together (first day)
# Convert upper triangle to data frame for easier analysis
pair_df <- data.frame(
  animal1 = character(),
  animal2 = character(),
  bouts = numeric(),
  total_time = numeric(),
  avg_duration = numeric()
)

# Extract upper triangle values
animal_ids <- rownames(time_matrix_day1)
for (i in 1:(length(animal_ids) - 1)) {
  for (j in (i + 1):length(animal_ids)) {
    if (time_matrix_day1[i, j] > 0) {  # Only include pairs with activity
      pair_df <- rbind(pair_df, data.frame(
        animal1 = animal_ids[i],
        animal2 = animal_ids[j],
        bouts = bout_matrix_day1[i, j],
        total_time = time_matrix_day1[i, j],
        avg_duration = avg_matrix_day1[i, j]
      ))
    }
  }
}

# Sort by total time together
pair_df <- pair_df[order(-pair_df$total_time), ]

cat("Top 10 most synchronized feeding pairs (first day):\n")
#> Top 10 most synchronized feeding pairs (first day):
print(head(pair_df, 10))
#>      animal1 animal2 bouts total_time avg_duration
#> 616     5123    5124    62       9260    149.35484
#> 627     5123    6042    52       8882    170.80769
#> 624     5123    6028    75       8470    112.93333
#> 671     5124    7027   112       7594     67.80357
#> 1016    6126    7022    58       7344    126.62069
#> 653     5124    6028    70       7314    104.48571
#> 634     5123    6126    55       7261    132.01818
#> 55      3150    5058    54       7202    133.37037
#> 61      3150    5123    42       7095    168.92857
#> 815     6020    6129    50       6772    135.44000
```

### Analyze Water Drinking Synchronicity

``` r
# Analyze pair-wise co-occurrence for drinking
pair_water_results <- synch_pair_analysis(
  matrix_data = water_matrices,
  type = "drink",
  resolution = "sec",
  id_col = id_col2()
)
```

## 7. Spatial Neighbor Proximity Analysis

Neighbor analysis examines how much time animals spend at spatially
adjacent bins. This requires the bin layout configuration you set
earlier.

### Understanding Bin Layout

The bin layout string defines the spatial arrangement of bins: - Rows
are separated by `\n` (newline) - Bins within a row are separated by
`-` - Only bins in the same row are considered neighbors (left/right
adjacency)

Example: `"1-2-3-4-5-6-101-102-7-8-9-10"` means bins 1, 2, 3, 4, 5, 6,
101, 102, 7, 8, 9, 10 are in a single row. - Bin 1 is neighbor to bin 2
only - Bin 5 is neighbor to bins 4 and 6

``` r
cat("Current bin layout:\n")
#> Current bin layout:
cat(bin_layout2(), "\n")
#> 1-2-3-4-5-6-101-102-7-8-9-10-11-12-13-14-15-16-17-18-103-104-19-20-21-22-23-24-25-26-27-28-29-30-105
```

### Analyze Feed Neighbor Synchronicity

``` r
# Analyze spatial neighbor patterns for feeding
neighbor_feed_results <- synch_neighbor_analysis(
  matrix_data = feed_matrices,
  bin_layout = bin_layout2(),
  type = "feed",
  resolution = "sec",
  id_col = id_col2()
)

# Examine the structure
cat("Neighbor analysis results:\n")
#> Neighbor analysis results:
names(neighbor_feed_results)
#> [1] "bout"         "total_time"   "avg_duration"

# Look at bout counts for neighbors (first day)
cat("\nNeighbor bout matrix (first 5 animals, first day):\n")
#> 
#> Neighbor bout matrix (first 5 animals, first day):
neighbor_bout_matrix <- neighbor_feed_results$bout[[1]]
print(neighbor_bout_matrix[1:5, 1:5])
#>      2074 3150 4001 4044 4070
#> 2074    0    4    0    0    0
#> 3150    0    0    2    0    1
#> 4001    0    0    0    0    3
#> 4044    0    0    0    0    3
#> 4070    0    0    0    0    0

# Look at total time as neighbors
cat("\nTotal time as neighbors (seconds, first 5 animals, first day):\n")
#> 
#> Total time as neighbors (seconds, first 5 animals, first day):
neighbor_time_matrix <- neighbor_feed_results$total_time[[1]]
print(neighbor_time_matrix[1:5, 1:5])
#>      2074 3150 4001 4044 4070
#> 2074    0  879    0    0    0
#> 3150    0    0  271    0  298
#> 4001    0    0    0    0  511
#> 4044    0    0    0    0  530
#> 4070    0    0    0    0    0
```

### Comparing Co-Occurrence vs. Neighbor Patterns

``` r
# Compare the same pair in both analyses
example_animal1 <- rownames(time_matrix_day1)[1]
example_animal2 <- rownames(time_matrix_day1)[2]

cat(sprintf("Comparison for animals %s and %s (first day):\n
Co-occurrence (feeding at same time, any bins): %d bouts, %d seconds total, %.1f seconds avg
Neighbor proximity (feeding at adjacent bins): %d bouts, %d seconds total, %.1f seconds avg\n",
  example_animal1, example_animal2,
  bout_matrix_day1[example_animal1, example_animal2],
  time_matrix_day1[example_animal1, example_animal2],
  avg_matrix_day1[example_animal1, example_animal2],
  neighbor_bout_matrix[example_animal1, example_animal2],
  neighbor_time_matrix[example_animal1, example_animal2],
  neighbor_feed_results$avg_duration[[1]][example_animal1, example_animal2]
))
#> Comparison for animals 2074 and 3150 (first day):
#> 
#> Co-occurrence (feeding at same time, any bins): 31 bouts, 4693 seconds total, 151.4 seconds avg
#> Neighbor proximity (feeding at adjacent bins): 4 bouts, 879 seconds total, 219.8 seconds avg
```

### Finding Pairs That Prefer Neighboring Bins

``` r
# Find pairs with high neighbor time relative to total co-occurrence time
neighbor_df <- data.frame(
  animal1 = character(),
  animal2 = character(),
  cooccurrence_time = numeric(),
  neighbor_time = numeric(),
  neighbor_ratio = numeric()
)

# Extract pairs with both types of activity
for (i in 1:(length(animal_ids) - 1)) {
  for (j in (i + 1):length(animal_ids)) {
    co_time <- time_matrix_day1[i, j]
    nb_time <- neighbor_time_matrix[i, j]

    if (co_time > 0) {  # Only pairs with some co-occurrence
      neighbor_df <- rbind(neighbor_df, data.frame(
        animal1 = animal_ids[i],
        animal2 = animal_ids[j],
        cooccurrence_time = co_time,
        neighbor_time = nb_time,
        neighbor_ratio = nb_time / co_time  # Proportion of time as neighbors
      ))
    }
  }
}

# Sort by neighbor ratio
neighbor_df <- neighbor_df[order(-neighbor_df$neighbor_ratio), ]

cat("Top 10 pairs with highest neighbor preference (first day):\n(Higher ratio = more time at neighboring bins when feeding together)\n")
#> Top 10 pairs with highest neighbor preference (first day):
#> (Higher ratio = more time at neighboring bins when feeding together)
print(head(neighbor_df, 10))
#>      animal1 animal2 cooccurrence_time neighbor_time neighbor_ratio
#> 1059    7022    7030               317           212      0.6687697
#> 765     5145    6090               646           397      0.6145511
#> 315     5028    5145               589           297      0.5042445
#> 341     5041    5042              6209          3068      0.4941214
#> 337     5028    7027              1329           642      0.4830700
#> 527     5100    5145               547           255      0.4661792
#> 230     4072    5120              3905          1772      0.4537772
#> 689     5135    6121              5369          2374      0.4421680
#> 238     4072    6020              3025          1326      0.4383471
#> 59      3150    5114              5182          2206      0.4257044
```

## 8. Identifying Social Bonds

Animals that consistently spend high amounts of time feeding together
may have social bonds:

``` r
# Calculate average co-occurrence time across all days
avg_time_matrix <- pair_feed_results$total_time[[1]] * 0  # Initialize with zeros

for (day_matrix in pair_feed_results$total_time) {
  avg_time_matrix <- avg_time_matrix + day_matrix
}

# Divide by number of days
avg_time_matrix <- avg_time_matrix / length(pair_feed_results$total_time)

# Find consistently synchronized pairs
cat("Pairs with consistently high synchronicity (average > 100 seconds/day):\n")
#> Pairs with consistently high synchronicity (average > 100 seconds/day):
for (i in 1:(nrow(avg_time_matrix) - 1)) {
  for (j in (i + 1):ncol(avg_time_matrix)) {
    if (avg_time_matrix[i, j] > 100) {
      cat(rownames(avg_time_matrix)[i], "-",
          colnames(avg_time_matrix)[j], ":",
          round(avg_time_matrix[i, j], 1), "seconds/day\n")
    }
  }
}
#> 2074 - 3150 : 3325 seconds/day
#> 2074 - 4001 : 1392.5 seconds/day
#> 2074 - 4044 : 918 seconds/day
#> 2074 - 4070 : 691 seconds/day
#> 2074 - 4072 : 1867 seconds/day
#> 2074 - 4080 : 3161 seconds/day
#> 2074 - 5028 : 2163.5 seconds/day
#> 2074 - 5041 : 2672.5 seconds/day
#> 2074 - 5042 : 2274 seconds/day
#> 2074 - 5058 : 3794 seconds/day
#> 2074 - 5061 : 1428 seconds/day
#> 2074 - 5067 : 2755.5 seconds/day
#> 2074 - 5100 : 2690 seconds/day
#> 2074 - 5114 : 1747 seconds/day
#> 2074 - 5120 : 3578 seconds/day
#> 2074 - 5123 : 3132.5 seconds/day
#> 2074 - 5124 : 3289.5 seconds/day
#> 2074 - 5135 : 1568.5 seconds/day
#> 2074 - 5137 : 670.5 seconds/day
#> 2074 - 5139 : 4295 seconds/day
#> 2074 - 5145 : 2638 seconds/day
#> 2074 - 6005 : 4022.5 seconds/day
#> 2074 - 6020 : 1986.5 seconds/day
#> 2074 - 6027 : 1944.5 seconds/day
#> 2074 - 6028 : 2984.5 seconds/day
#> 2074 - 6030 : 3014.5 seconds/day
#> 2074 - 6033 : 2900 seconds/day
#> 2074 - 6042 : 2748 seconds/day
#> 2074 - 6050 : 2120.5 seconds/day
#> 2074 - 6055 : 2225 seconds/day
#> 2074 - 6069 : 2838 seconds/day
#> 2074 - 6084 : 1762 seconds/day
#> 2074 - 6090 : 2550.5 seconds/day
#> 2074 - 6121 : 3131.5 seconds/day
#> 2074 - 6126 : 3351.5 seconds/day
#> 2074 - 6129 : 2695 seconds/day
#> 2074 - 7010 : 3410.5 seconds/day
#> 2074 - 7018 : 2221.5 seconds/day
#> 2074 - 7019 : 1142.5 seconds/day
#> 2074 - 7022 : 3368 seconds/day
#> 2074 - 7023 : 2302.5 seconds/day
#> 2074 - 7024 : 1051.5 seconds/day
#> 2074 - 7027 : 1320.5 seconds/day
#> 2074 - 7030 : 1802.5 seconds/day
#> 2074 - 7033 : 1581 seconds/day
#> 2074 - 7043 : 2763.5 seconds/day
#> 3150 - 4001 : 2038 seconds/day
#> 3150 - 4044 : 3089.5 seconds/day
#> 3150 - 4070 : 3520.5 seconds/day
#> 3150 - 4072 : 3997 seconds/day
#> 3150 - 4080 : 2960.5 seconds/day
#> 3150 - 5028 : 3615.5 seconds/day
#> 3150 - 5041 : 5021.5 seconds/day
#> 3150 - 5042 : 5247 seconds/day
#> 3150 - 5058 : 7040.5 seconds/day
#> 3150 - 5061 : 5180.5 seconds/day
#> 3150 - 5067 : 5033.5 seconds/day
#> 3150 - 5100 : 4261 seconds/day
#> 3150 - 5114 : 4666 seconds/day
#> 3150 - 5120 : 5206.5 seconds/day
#> 3150 - 5123 : 6570 seconds/day
#> 3150 - 5124 : 4620 seconds/day
#> 3150 - 5135 : 3470 seconds/day
#> 3150 - 5137 : 3237 seconds/day
#> 3150 - 5139 : 4154 seconds/day
#> 3150 - 5145 : 1915 seconds/day
#> 3150 - 6005 : 2530 seconds/day
#> 3150 - 6020 : 1816 seconds/day
#> 3150 - 6027 : 4009 seconds/day
#> 3150 - 6028 : 3719.5 seconds/day
#> 3150 - 6030 : 2050.5 seconds/day
#> 3150 - 6033 : 6123.5 seconds/day
#> 3150 - 6042 : 3390 seconds/day
#> 3150 - 6050 : 5571 seconds/day
#> 3150 - 6055 : 2967 seconds/day
#> 3150 - 6069 : 4426 seconds/day
#> 3150 - 6084 : 5079.5 seconds/day
#> 3150 - 6090 : 3670 seconds/day
#> 3150 - 6121 : 5270.5 seconds/day
#> 3150 - 6126 : 2833 seconds/day
#> 3150 - 6129 : 3423.5 seconds/day
#> 3150 - 7010 : 3917.5 seconds/day
#> 3150 - 7018 : 3805 seconds/day
#> 3150 - 7019 : 5786.5 seconds/day
#> 3150 - 7022 : 3869.5 seconds/day
#> 3150 - 7023 : 4664.5 seconds/day
#> 3150 - 7024 : 1763 seconds/day
#> 3150 - 7027 : 3601.5 seconds/day
#> 3150 - 7030 : 3684 seconds/day
#> 3150 - 7033 : 4518.5 seconds/day
#> 3150 - 7043 : 4893.5 seconds/day
#> 4001 - 4044 : 2577.5 seconds/day
#> 4001 - 4070 : 2233.5 seconds/day
#> 4001 - 4072 : 1746 seconds/day
#> 4001 - 4080 : 3054 seconds/day
#> 4001 - 5028 : 1752.5 seconds/day
#> 4001 - 5041 : 2080.5 seconds/day
#> 4001 - 5042 : 1880 seconds/day
#> 4001 - 5058 : 1966 seconds/day
#> 4001 - 5061 : 1899.5 seconds/day
#> 4001 - 5067 : 556.5 seconds/day
#> 4001 - 5100 : 1080 seconds/day
#> 4001 - 5114 : 1956.5 seconds/day
#> 4001 - 5120 : 1128 seconds/day
#> 4001 - 5123 : 2917.5 seconds/day
#> 4001 - 5124 : 535.5 seconds/day
#> 4001 - 5135 : 1690.5 seconds/day
#> 4001 - 5137 : 2038 seconds/day
#> 4001 - 5139 : 1571.5 seconds/day
#> 4001 - 5145 : 1341 seconds/day
#> 4001 - 6005 : 1180 seconds/day
#> 4001 - 6020 : 2088.5 seconds/day
#> 4001 - 6027 : 2187.5 seconds/day
#> 4001 - 6028 : 3173 seconds/day
#> 4001 - 6030 : 794 seconds/day
#> 4001 - 6033 : 947.5 seconds/day
#> 4001 - 6042 : 2125.5 seconds/day
#> 4001 - 6050 : 1068.5 seconds/day
#> 4001 - 6055 : 1069.5 seconds/day
#> 4001 - 6069 : 2327.5 seconds/day
#> 4001 - 6084 : 3874 seconds/day
#> 4001 - 6090 : 851.5 seconds/day
#> 4001 - 6121 : 955.5 seconds/day
#> 4001 - 6126 : 908 seconds/day
#> 4001 - 6129 : 2757 seconds/day
#> 4001 - 7010 : 1774.5 seconds/day
#> 4001 - 7018 : 1501.5 seconds/day
#> 4001 - 7019 : 2406.5 seconds/day
#> 4001 - 7022 : 1426.5 seconds/day
#> 4001 - 7023 : 2932 seconds/day
#> 4001 - 7024 : 548 seconds/day
#> 4001 - 7027 : 542 seconds/day
#> 4001 - 7030 : 1621.5 seconds/day
#> 4001 - 7033 : 1185 seconds/day
#> 4001 - 7043 : 3434.5 seconds/day
#> 4044 - 4070 : 3702 seconds/day
#> 4044 - 4072 : 1394.5 seconds/day
#> 4044 - 4080 : 1497.5 seconds/day
#> 4044 - 5028 : 3228.5 seconds/day
#> 4044 - 5041 : 3608.5 seconds/day
#> 4044 - 5042 : 3761 seconds/day
#> 4044 - 5058 : 3966 seconds/day
#> 4044 - 5061 : 4012 seconds/day
#> 4044 - 5067 : 2198 seconds/day
#> 4044 - 5100 : 2327.5 seconds/day
#> 4044 - 5114 : 3958 seconds/day
#> 4044 - 5120 : 1953 seconds/day
#> 4044 - 5123 : 4228.5 seconds/day
#> 4044 - 5124 : 1977 seconds/day
#> 4044 - 5135 : 3601 seconds/day
#> 4044 - 5137 : 3000.5 seconds/day
#> 4044 - 5139 : 1029 seconds/day
#> 4044 - 5145 : 1551 seconds/day
#> 4044 - 6005 : 2004 seconds/day
#> 4044 - 6020 : 4113 seconds/day
#> 4044 - 6027 : 2723.5 seconds/day
#> 4044 - 6028 : 3308 seconds/day
#> 4044 - 6030 : 872 seconds/day
#> 4044 - 6033 : 2361 seconds/day
#> 4044 - 6042 : 3968.5 seconds/day
#> 4044 - 6050 : 4854 seconds/day
#> 4044 - 6055 : 2007.5 seconds/day
#> 4044 - 6069 : 3681 seconds/day
#> 4044 - 6084 : 6436 seconds/day
#> 4044 - 6090 : 1159 seconds/day
#> 4044 - 6121 : 3528.5 seconds/day
#> 4044 - 6126 : 1174 seconds/day
#> 4044 - 6129 : 3370.5 seconds/day
#> 4044 - 7010 : 2184 seconds/day
#> 4044 - 7018 : 3531 seconds/day
#> 4044 - 7019 : 3416 seconds/day
#> 4044 - 7022 : 3530.5 seconds/day
#> 4044 - 7023 : 2881.5 seconds/day
#> 4044 - 7024 : 3291.5 seconds/day
#> 4044 - 7027 : 1718.5 seconds/day
#> 4044 - 7030 : 2984 seconds/day
#> 4044 - 7033 : 1781 seconds/day
#> 4044 - 7043 : 3002.5 seconds/day
#> 4070 - 4072 : 1549.5 seconds/day
#> 4070 - 4080 : 2724 seconds/day
#> 4070 - 5028 : 3652 seconds/day
#> 4070 - 5041 : 3812 seconds/day
#> 4070 - 5042 : 2926.5 seconds/day
#> 4070 - 5058 : 2880 seconds/day
#> 4070 - 5061 : 2696.5 seconds/day
#> 4070 - 5067 : 1322 seconds/day
#> 4070 - 5100 : 2311 seconds/day
#> 4070 - 5114 : 3049.5 seconds/day
#> 4070 - 5120 : 2389 seconds/day
#> 4070 - 5123 : 4290 seconds/day
#> 4070 - 5124 : 1480.5 seconds/day
#> 4070 - 5135 : 3935 seconds/day
#> 4070 - 5137 : 3249 seconds/day
#> 4070 - 5139 : 1054.5 seconds/day
#> 4070 - 5145 : 408.5 seconds/day
#> 4070 - 6005 : 1350.5 seconds/day
#> 4070 - 6020 : 1981 seconds/day
#> 4070 - 6027 : 1754.5 seconds/day
#> 4070 - 6028 : 3022.5 seconds/day
#> 4070 - 6030 : 1200.5 seconds/day
#> 4070 - 6033 : 2844.5 seconds/day
#> 4070 - 6042 : 2771 seconds/day
#> 4070 - 6050 : 3351 seconds/day
#> 4070 - 6055 : 2012.5 seconds/day
#> 4070 - 6069 : 1831 seconds/day
#> 4070 - 6084 : 3496 seconds/day
#> 4070 - 6090 : 1480 seconds/day
#> 4070 - 6121 : 3063.5 seconds/day
#> 4070 - 6126 : 1676 seconds/day
#> 4070 - 6129 : 2754.5 seconds/day
#> 4070 - 7010 : 2537 seconds/day
#> 4070 - 7018 : 1781.5 seconds/day
#> 4070 - 7019 : 4314.5 seconds/day
#> 4070 - 7022 : 2333.5 seconds/day
#> 4070 - 7023 : 2660 seconds/day
#> 4070 - 7024 : 926 seconds/day
#> 4070 - 7027 : 1785.5 seconds/day
#> 4070 - 7030 : 3645.5 seconds/day
#> 4070 - 7033 : 1488.5 seconds/day
#> 4070 - 7043 : 2787 seconds/day
#> 4072 - 4080 : 1655.5 seconds/day
#> 4072 - 5028 : 2677 seconds/day
#> 4072 - 5041 : 4520 seconds/day
#> 4072 - 5042 : 1809 seconds/day
#> 4072 - 5058 : 2090 seconds/day
#> 4072 - 5061 : 2146 seconds/day
#> 4072 - 5067 : 2447.5 seconds/day
#> 4072 - 5100 : 3445.5 seconds/day
#> 4072 - 5114 : 2764 seconds/day
#> 4072 - 5120 : 3374.5 seconds/day
#> 4072 - 5123 : 4809 seconds/day
#> 4072 - 5124 : 4247 seconds/day
#> 4072 - 5135 : 2737.5 seconds/day
#> 4072 - 5137 : 2012.5 seconds/day
#> 4072 - 5139 : 2025 seconds/day
#> 4072 - 5145 : 2667.5 seconds/day
#> 4072 - 6005 : 2089.5 seconds/day
#> 4072 - 6020 : 1765 seconds/day
#> 4072 - 6027 : 3758.5 seconds/day
#> 4072 - 6028 : 2888.5 seconds/day
#> 4072 - 6030 : 3172 seconds/day
#> 4072 - 6033 : 1354.5 seconds/day
#> 4072 - 6042 : 2097 seconds/day
#> 4072 - 6050 : 1141.5 seconds/day
#> 4072 - 6055 : 1368 seconds/day
#> 4072 - 6069 : 2655.5 seconds/day
#> 4072 - 6084 : 3960 seconds/day
#> 4072 - 6090 : 2367 seconds/day
#> 4072 - 6121 : 1931 seconds/day
#> 4072 - 6126 : 1947.5 seconds/day
#> 4072 - 6129 : 2318.5 seconds/day
#> 4072 - 7010 : 2621.5 seconds/day
#> 4072 - 7018 : 2820.5 seconds/day
#> 4072 - 7019 : 2782.5 seconds/day
#> 4072 - 7022 : 3131 seconds/day
#> 4072 - 7023 : 4561.5 seconds/day
#> 4072 - 7024 : 1148.5 seconds/day
#> 4072 - 7027 : 2653.5 seconds/day
#> 4072 - 7030 : 1759.5 seconds/day
#> 4072 - 7033 : 3762 seconds/day
#> 4072 - 7043 : 3422.5 seconds/day
#> 4080 - 5028 : 1617 seconds/day
#> 4080 - 5041 : 2930 seconds/day
#> 4080 - 5042 : 1600 seconds/day
#> 4080 - 5058 : 2840 seconds/day
#> 4080 - 5061 : 1118.5 seconds/day
#> 4080 - 5067 : 1627 seconds/day
#> 4080 - 5100 : 1862 seconds/day
#> 4080 - 5114 : 1219.5 seconds/day
#> 4080 - 5120 : 3053.5 seconds/day
#> 4080 - 5123 : 4871.5 seconds/day
#> 4080 - 5124 : 4195 seconds/day
#> 4080 - 5135 : 1958.5 seconds/day
#> 4080 - 5137 : 1193 seconds/day
#> 4080 - 5139 : 3040.5 seconds/day
#> 4080 - 5145 : 2486.5 seconds/day
#> 4080 - 6005 : 2165.5 seconds/day
#> 4080 - 6020 : 2696 seconds/day
#> 4080 - 6027 : 3405 seconds/day
#> 4080 - 6028 : 3403 seconds/day
#> 4080 - 6030 : 2583.5 seconds/day
#> 4080 - 6033 : 3284 seconds/day
#> 4080 - 6042 : 1930.5 seconds/day
#> 4080 - 6050 : 1412.5 seconds/day
#> 4080 - 6055 : 1796 seconds/day
#> 4080 - 6069 : 2082 seconds/day
#> 4080 - 6084 : 2938.5 seconds/day
#> 4080 - 6090 : 1659 seconds/day
#> 4080 - 6121 : 1631.5 seconds/day
#> 4080 - 6126 : 3231.5 seconds/day
#> 4080 - 6129 : 3217.5 seconds/day
#> 4080 - 7010 : 4313 seconds/day
#> 4080 - 7018 : 2403.5 seconds/day
#> 4080 - 7019 : 2991.5 seconds/day
#> 4080 - 7022 : 2886.5 seconds/day
#> 4080 - 7023 : 3397.5 seconds/day
#> 4080 - 7024 : 2670.5 seconds/day
#> 4080 - 7027 : 1999 seconds/day
#> 4080 - 7030 : 2226 seconds/day
#> 4080 - 7033 : 1733.5 seconds/day
#> 4080 - 7043 : 3293 seconds/day
#> 5028 - 5041 : 6041.5 seconds/day
#> 5028 - 5042 : 3334 seconds/day
#> 5028 - 5058 : 2152.5 seconds/day
#> 5028 - 5061 : 3438.5 seconds/day
#> 5028 - 5067 : 3784 seconds/day
#> 5028 - 5100 : 4999.5 seconds/day
#> 5028 - 5114 : 4118 seconds/day
#> 5028 - 5120 : 5615.5 seconds/day
#> 5028 - 5123 : 3388 seconds/day
#> 5028 - 5124 : 1351 seconds/day
#> 5028 - 5135 : 3248.5 seconds/day
#> 5028 - 5137 : 3643.5 seconds/day
#> 5028 - 5139 : 3553 seconds/day
#> 5028 - 5145 : 719 seconds/day
#> 5028 - 6005 : 2745.5 seconds/day
#> 5028 - 6020 : 1919.5 seconds/day
#> 5028 - 6027 : 2246 seconds/day
#> 5028 - 6028 : 2424 seconds/day
#> 5028 - 6030 : 1901 seconds/day
#> 5028 - 6033 : 4028.5 seconds/day
#> 5028 - 6042 : 4437 seconds/day
#> 5028 - 6050 : 3605.5 seconds/day
#> 5028 - 6055 : 2846 seconds/day
#> 5028 - 6069 : 3860 seconds/day
#> 5028 - 6084 : 4641.5 seconds/day
#> 5028 - 6090 : 2041 seconds/day
#> 5028 - 6121 : 2554.5 seconds/day
#> 5028 - 6126 : 2991 seconds/day
#> 5028 - 6129 : 2735 seconds/day
#> 5028 - 7010 : 2937 seconds/day
#> 5028 - 7018 : 3035 seconds/day
#> 5028 - 7019 : 3554 seconds/day
#> 5028 - 7022 : 3008.5 seconds/day
#> 5028 - 7023 : 3829 seconds/day
#> 5028 - 7024 : 1970.5 seconds/day
#> 5028 - 7027 : 2551 seconds/day
#> 5028 - 7030 : 3932.5 seconds/day
#> 5028 - 7033 : 2391 seconds/day
#> 5028 - 7043 : 3780 seconds/day
#> 5041 - 5042 : 5393 seconds/day
#> 5041 - 5058 : 3550.5 seconds/day
#> 5041 - 5061 : 5801.5 seconds/day
#> 5041 - 5067 : 4686 seconds/day
#> 5041 - 5100 : 4483.5 seconds/day
#> 5041 - 5114 : 5434 seconds/day
#> 5041 - 5120 : 6556.5 seconds/day
#> 5041 - 5123 : 4124.5 seconds/day
#> 5041 - 5124 : 3632.5 seconds/day
#> 5041 - 5135 : 4457.5 seconds/day
#> 5041 - 5137 : 4105 seconds/day
#> 5041 - 5139 : 4893 seconds/day
#> 5041 - 5145 : 1184.5 seconds/day
#> 5041 - 6005 : 2559.5 seconds/day
#> 5041 - 6020 : 2608 seconds/day
#> 5041 - 6027 : 2100 seconds/day
#> 5041 - 6028 : 4105 seconds/day
#> 5041 - 6030 : 2631.5 seconds/day
#> 5041 - 6033 : 4879 seconds/day
#> 5041 - 6042 : 5093 seconds/day
#> 5041 - 6050 : 3736.5 seconds/day
#> 5041 - 6055 : 2819.5 seconds/day
#> 5041 - 6069 : 5015.5 seconds/day
#> 5041 - 6084 : 5651.5 seconds/day
#> 5041 - 6090 : 3255 seconds/day
#> 5041 - 6121 : 2975 seconds/day
#> 5041 - 6126 : 2913 seconds/day
#> 5041 - 6129 : 3137 seconds/day
#> 5041 - 7010 : 4681.5 seconds/day
#> 5041 - 7018 : 2500 seconds/day
#> 5041 - 7019 : 4597.5 seconds/day
#> 5041 - 7022 : 4520.5 seconds/day
#> 5041 - 7023 : 4619 seconds/day
#> 5041 - 7024 : 1718 seconds/day
#> 5041 - 7027 : 2258 seconds/day
#> 5041 - 7030 : 5140.5 seconds/day
#> 5041 - 7033 : 3659.5 seconds/day
#> 5041 - 7043 : 4974.5 seconds/day
#> 5042 - 5058 : 4020.5 seconds/day
#> 5042 - 5061 : 5356.5 seconds/day
#> 5042 - 5067 : 3600 seconds/day
#> 5042 - 5100 : 3298.5 seconds/day
#> 5042 - 5114 : 4703 seconds/day
#> 5042 - 5120 : 3856.5 seconds/day
#> 5042 - 5123 : 3457.5 seconds/day
#> 5042 - 5124 : 2882 seconds/day
#> 5042 - 5135 : 4140.5 seconds/day
#> 5042 - 5137 : 2705.5 seconds/day
#> 5042 - 5139 : 3118.5 seconds/day
#> 5042 - 5145 : 963 seconds/day
#> 5042 - 6005 : 2670.5 seconds/day
#> 5042 - 6020 : 2732 seconds/day
#> 5042 - 6027 : 2101 seconds/day
#> 5042 - 6028 : 3494.5 seconds/day
#> 5042 - 6030 : 1435.5 seconds/day
#> 5042 - 6033 : 4301.5 seconds/day
#> 5042 - 6042 : 3031.5 seconds/day
#> 5042 - 6050 : 4364.5 seconds/day
#> 5042 - 6055 : 2649.5 seconds/day
#> 5042 - 6069 : 4743 seconds/day
#> 5042 - 6084 : 5637 seconds/day
#> 5042 - 6090 : 2051 seconds/day
#> 5042 - 6121 : 3491 seconds/day
#> 5042 - 6126 : 1266.5 seconds/day
#> 5042 - 6129 : 2640.5 seconds/day
#> 5042 - 7010 : 2599 seconds/day
#> 5042 - 7018 : 3082.5 seconds/day
#> 5042 - 7019 : 4527.5 seconds/day
#> 5042 - 7022 : 3683 seconds/day
#> 5042 - 7023 : 2736 seconds/day
#> 5042 - 7024 : 1377.5 seconds/day
#> 5042 - 7027 : 2060.5 seconds/day
#> 5042 - 7030 : 3758 seconds/day
#> 5042 - 7033 : 2701.5 seconds/day
#> 5042 - 7043 : 4091.5 seconds/day
#> 5058 - 5061 : 4457.5 seconds/day
#> 5058 - 5067 : 4200 seconds/day
#> 5058 - 5100 : 2270.5 seconds/day
#> 5058 - 5114 : 3759.5 seconds/day
#> 5058 - 5120 : 2830 seconds/day
#> 5058 - 5123 : 5828.5 seconds/day
#> 5058 - 5124 : 4113 seconds/day
#> 5058 - 5135 : 3546 seconds/day
#> 5058 - 5137 : 3041.5 seconds/day
#> 5058 - 5139 : 2887 seconds/day
#> 5058 - 5145 : 2246.5 seconds/day
#> 5058 - 6005 : 3765.5 seconds/day
#> 5058 - 6020 : 2575 seconds/day
#> 5058 - 6027 : 2796 seconds/day
#> 5058 - 6028 : 5533.5 seconds/day
#> 5058 - 6030 : 2010 seconds/day
#> 5058 - 6033 : 4368 seconds/day
#> 5058 - 6042 : 4907 seconds/day
#> 5058 - 6050 : 6263 seconds/day
#> 5058 - 6055 : 2249.5 seconds/day
#> 5058 - 6069 : 4351.5 seconds/day
#> 5058 - 6084 : 4549.5 seconds/day
#> 5058 - 6090 : 3549 seconds/day
#> 5058 - 6121 : 5606.5 seconds/day
#> 5058 - 6126 : 2777.5 seconds/day
#> 5058 - 6129 : 3309.5 seconds/day
#> 5058 - 7010 : 3122.5 seconds/day
#> 5058 - 7018 : 3445 seconds/day
#> 5058 - 7019 : 4792.5 seconds/day
#> 5058 - 7022 : 3598 seconds/day
#> 5058 - 7023 : 3398.5 seconds/day
#> 5058 - 7024 : 2063.5 seconds/day
#> 5058 - 7027 : 2800.5 seconds/day
#> 5058 - 7030 : 3573 seconds/day
#> 5058 - 7033 : 3050 seconds/day
#> 5058 - 7043 : 2824.5 seconds/day
#> 5061 - 5067 : 3329 seconds/day
#> 5061 - 5100 : 3029 seconds/day
#> 5061 - 5114 : 5712.5 seconds/day
#> 5061 - 5120 : 3336 seconds/day
#> 5061 - 5123 : 2973 seconds/day
#> 5061 - 5124 : 2106.5 seconds/day
#> 5061 - 5135 : 4186.5 seconds/day
#> 5061 - 5137 : 3061 seconds/day
#> 5061 - 5139 : 1700 seconds/day
#> 5061 - 5145 : 1678.5 seconds/day
#> 5061 - 6005 : 1951.5 seconds/day
#> 5061 - 6020 : 2673 seconds/day
#> 5061 - 6027 : 2902.5 seconds/day
#> 5061 - 6028 : 4015 seconds/day
#> 5061 - 6030 : 1203.5 seconds/day
#> 5061 - 6033 : 4037 seconds/day
#> 5061 - 6042 : 4365 seconds/day
#> 5061 - 6050 : 4809.5 seconds/day
#> 5061 - 6055 : 3630.5 seconds/day
#> 5061 - 6069 : 3910 seconds/day
#> 5061 - 6084 : 5776 seconds/day
#> 5061 - 6090 : 4179 seconds/day
#> 5061 - 6121 : 3942 seconds/day
#> 5061 - 6126 : 750 seconds/day
#> 5061 - 6129 : 2618.5 seconds/day
#> 5061 - 7010 : 2234 seconds/day
#> 5061 - 7018 : 2359.5 seconds/day
#> 5061 - 7019 : 5573.5 seconds/day
#> 5061 - 7022 : 3067.5 seconds/day
#> 5061 - 7023 : 2750.5 seconds/day
#> 5061 - 7024 : 982.5 seconds/day
#> 5061 - 7027 : 1470.5 seconds/day
#> 5061 - 7030 : 4495 seconds/day
#> 5061 - 7033 : 2912 seconds/day
#> 5061 - 7043 : 2850 seconds/day
#> 5067 - 5100 : 4132.5 seconds/day
#> 5067 - 5114 : 4196.5 seconds/day
#> 5067 - 5120 : 2705 seconds/day
#> 5067 - 5123 : 3282.5 seconds/day
#> 5067 - 5124 : 4571 seconds/day
#> 5067 - 5135 : 2212 seconds/day
#> 5067 - 5137 : 2790.5 seconds/day
#> 5067 - 5139 : 4803.5 seconds/day
#> 5067 - 5145 : 2289 seconds/day
#> 5067 - 6005 : 2915 seconds/day
#> 5067 - 6020 : 4752.5 seconds/day
#> 5067 - 6027 : 2168 seconds/day
#> 5067 - 6028 : 2267.5 seconds/day
#> 5067 - 6030 : 2580 seconds/day
#> 5067 - 6033 : 3970 seconds/day
#> 5067 - 6042 : 4328 seconds/day
#> 5067 - 6050 : 3541 seconds/day
#> 5067 - 6055 : 2367.5 seconds/day
#> 5067 - 6069 : 4982.5 seconds/day
#> 5067 - 6084 : 5791.5 seconds/day
#> 5067 - 6090 : 2656.5 seconds/day
#> 5067 - 6121 : 3516 seconds/day
#> 5067 - 6126 : 3267 seconds/day
#> 5067 - 6129 : 1278 seconds/day
#> 5067 - 7010 : 2665 seconds/day
#> 5067 - 7018 : 4581.5 seconds/day
#> 5067 - 7019 : 2821.5 seconds/day
#> 5067 - 7022 : 4090 seconds/day
#> 5067 - 7023 : 2671 seconds/day
#> 5067 - 7024 : 2406 seconds/day
#> 5067 - 7027 : 4369 seconds/day
#> 5067 - 7030 : 2369.5 seconds/day
#> 5067 - 7033 : 3008 seconds/day
#> 5067 - 7043 : 3062.5 seconds/day
#> 5100 - 5114 : 3947 seconds/day
#> 5100 - 5120 : 4726.5 seconds/day
#> 5100 - 5123 : 3653.5 seconds/day
#> 5100 - 5124 : 4258.5 seconds/day
#> 5100 - 5135 : 4124.5 seconds/day
#> 5100 - 5137 : 1859 seconds/day
#> 5100 - 5139 : 3636 seconds/day
#> 5100 - 5145 : 365 seconds/day
#> 5100 - 6005 : 2476 seconds/day
#> 5100 - 6020 : 4654 seconds/day
#> 5100 - 6027 : 2137.5 seconds/day
#> 5100 - 6028 : 2394.5 seconds/day
#> 5100 - 6030 : 3643.5 seconds/day
#> 5100 - 6033 : 3747.5 seconds/day
#> 5100 - 6042 : 4778.5 seconds/day
#> 5100 - 6050 : 2761 seconds/day
#> 5100 - 6055 : 3618 seconds/day
#> 5100 - 6069 : 3882 seconds/day
#> 5100 - 6084 : 4911.5 seconds/day
#> 5100 - 6090 : 3315 seconds/day
#> 5100 - 6121 : 3819.5 seconds/day
#> 5100 - 6126 : 4019 seconds/day
#> 5100 - 6129 : 2466 seconds/day
#> 5100 - 7010 : 2875.5 seconds/day
#> 5100 - 7018 : 3295 seconds/day
#> 5100 - 7019 : 2988 seconds/day
#> 5100 - 7022 : 3107.5 seconds/day
#> 5100 - 7023 : 2825 seconds/day
#> 5100 - 7024 : 1880 seconds/day
#> 5100 - 7027 : 4611 seconds/day
#> 5100 - 7030 : 3229 seconds/day
#> 5100 - 7033 : 3187 seconds/day
#> 5100 - 7043 : 3010 seconds/day
#> 5114 - 5120 : 4026.5 seconds/day
#> 5114 - 5123 : 2619.5 seconds/day
#> 5114 - 5124 : 2944.5 seconds/day
#> 5114 - 5135 : 3726.5 seconds/day
#> 5114 - 5137 : 3821 seconds/day
#> 5114 - 5139 : 3712.5 seconds/day
#> 5114 - 5145 : 1470 seconds/day
#> 5114 - 6005 : 2040 seconds/day
#> 5114 - 6020 : 3231.5 seconds/day
#> 5114 - 6027 : 2726 seconds/day
#> 5114 - 6028 : 2989.5 seconds/day
#> 5114 - 6030 : 1157 seconds/day
#> 5114 - 6033 : 3577.5 seconds/day
#> 5114 - 6042 : 3711 seconds/day
#> 5114 - 6050 : 4363.5 seconds/day
#> 5114 - 6055 : 3460.5 seconds/day
#> 5114 - 6069 : 3644 seconds/day
#> 5114 - 6084 : 5863.5 seconds/day
#> 5114 - 6090 : 2926 seconds/day
#> 5114 - 6121 : 3311.5 seconds/day
#> 5114 - 6126 : 1648.5 seconds/day
#> 5114 - 6129 : 2857 seconds/day
#> 5114 - 7010 : 4545 seconds/day
#> 5114 - 7018 : 4162.5 seconds/day
#> 5114 - 7019 : 4228.5 seconds/day
#> 5114 - 7022 : 3040 seconds/day
#> 5114 - 7023 : 2491 seconds/day
#> 5114 - 7024 : 880.5 seconds/day
#> 5114 - 7027 : 3068 seconds/day
#> 5114 - 7030 : 5190 seconds/day
#> 5114 - 7033 : 2595 seconds/day
#> 5114 - 7043 : 3079.5 seconds/day
#> 5120 - 5123 : 3701.5 seconds/day
#> 5120 - 5124 : 3772 seconds/day
#> 5120 - 5135 : 1453.5 seconds/day
#> 5120 - 5137 : 3491 seconds/day
#> 5120 - 5139 : 5665.5 seconds/day
#> 5120 - 5145 : 1233 seconds/day
#> 5120 - 6005 : 3460 seconds/day
#> 5120 - 6020 : 1908.5 seconds/day
#> 5120 - 6027 : 4363.5 seconds/day
#> 5120 - 6028 : 3284.5 seconds/day
#> 5120 - 6030 : 3685.5 seconds/day
#> 5120 - 6033 : 4472 seconds/day
#> 5120 - 6042 : 2325.5 seconds/day
#> 5120 - 6050 : 2275.5 seconds/day
#> 5120 - 6055 : 2705.5 seconds/day
#> 5120 - 6069 : 3359 seconds/day
#> 5120 - 6084 : 3192.5 seconds/day
#> 5120 - 6090 : 3446.5 seconds/day
#> 5120 - 6121 : 1633.5 seconds/day
#> 5120 - 6126 : 3797 seconds/day
#> 5120 - 6129 : 5763.5 seconds/day
#> 5120 - 7010 : 4724 seconds/day
#> 5120 - 7018 : 2910.5 seconds/day
#> 5120 - 7019 : 3601.5 seconds/day
#> 5120 - 7022 : 3934.5 seconds/day
#> 5120 - 7023 : 4912.5 seconds/day
#> 5120 - 7024 : 2164.5 seconds/day
#> 5120 - 7027 : 1439.5 seconds/day
#> 5120 - 7030 : 3356.5 seconds/day
#> 5120 - 7033 : 5865 seconds/day
#> 5120 - 7043 : 5703 seconds/day
#> 5123 - 5124 : 7331.5 seconds/day
#> 5123 - 5135 : 6137.5 seconds/day
#> 5123 - 5137 : 3330.5 seconds/day
#> 5123 - 5139 : 3031 seconds/day
#> 5123 - 5145 : 4086.5 seconds/day
#> 5123 - 6005 : 4375.5 seconds/day
#> 5123 - 6020 : 4876 seconds/day
#> 5123 - 6027 : 4368.5 seconds/day
#> 5123 - 6028 : 7145 seconds/day
#> 5123 - 6030 : 2304.5 seconds/day
#> 5123 - 6033 : 3094 seconds/day
#> 5123 - 6042 : 5493 seconds/day
#> 5123 - 6050 : 5238.5 seconds/day
#> 5123 - 6055 : 1374.5 seconds/day
#> 5123 - 6069 : 4030.5 seconds/day
#> 5123 - 6084 : 4304 seconds/day
#> 5123 - 6090 : 2220 seconds/day
#> 5123 - 6121 : 4794 seconds/day
#> 5123 - 6126 : 4763.5 seconds/day
#> 5123 - 6129 : 3923 seconds/day
#> 5123 - 7010 : 3107 seconds/day
#> 5123 - 7018 : 3546.5 seconds/day
#> 5123 - 7019 : 3972.5 seconds/day
#> 5123 - 7022 : 7121.5 seconds/day
#> 5123 - 7023 : 6313 seconds/day
#> 5123 - 7024 : 2743.5 seconds/day
#> 5123 - 7027 : 4619 seconds/day
#> 5123 - 7030 : 2936 seconds/day
#> 5123 - 7033 : 3838 seconds/day
#> 5123 - 7043 : 3963 seconds/day
#> 5124 - 5135 : 4488 seconds/day
#> 5124 - 5137 : 2377 seconds/day
#> 5124 - 5139 : 4190.5 seconds/day
#> 5124 - 5145 : 3358.5 seconds/day
#> 5124 - 6005 : 4516 seconds/day
#> 5124 - 6020 : 6799 seconds/day
#> 5124 - 6027 : 4704 seconds/day
#> 5124 - 6028 : 4635 seconds/day
#> 5124 - 6030 : 4567.5 seconds/day
#> 5124 - 6033 : 3858.5 seconds/day
#> 5124 - 6042 : 4011 seconds/day
#> 5124 - 6050 : 5109.5 seconds/day
#> 5124 - 6055 : 2551.5 seconds/day
#> 5124 - 6069 : 2979 seconds/day
#> 5124 - 6084 : 3941.5 seconds/day
#> 5124 - 6090 : 3992 seconds/day
#> 5124 - 6121 : 4832.5 seconds/day
#> 5124 - 6126 : 5549 seconds/day
#> 5124 - 6129 : 2692.5 seconds/day
#> 5124 - 7010 : 5084.5 seconds/day
#> 5124 - 7018 : 4411.5 seconds/day
#> 5124 - 7019 : 2609 seconds/day
#> 5124 - 7022 : 5966.5 seconds/day
#> 5124 - 7023 : 4287 seconds/day
#> 5124 - 7024 : 4204 seconds/day
#> 5124 - 7027 : 5981 seconds/day
#> 5124 - 7030 : 2342.5 seconds/day
#> 5124 - 7033 : 3696.5 seconds/day
#> 5124 - 7043 : 2171.5 seconds/day
#> 5135 - 5137 : 2860.5 seconds/day
#> 5135 - 5139 : 889.5 seconds/day
#> 5135 - 5145 : 467.5 seconds/day
#> 5135 - 6005 : 2388.5 seconds/day
#> 5135 - 6020 : 4127 seconds/day
#> 5135 - 6027 : 1576.5 seconds/day
#> 5135 - 6028 : 5042.5 seconds/day
#> 5135 - 6030 : 1414 seconds/day
#> 5135 - 6033 : 3165.5 seconds/day
#> 5135 - 6042 : 3977 seconds/day
#> 5135 - 6050 : 5573.5 seconds/day
#> 5135 - 6055 : 1580.5 seconds/day
#> 5135 - 6069 : 3948 seconds/day
#> 5135 - 6084 : 4669 seconds/day
#> 5135 - 6090 : 2203.5 seconds/day
#> 5135 - 6121 : 4173 seconds/day
#> 5135 - 6126 : 1546 seconds/day
#> 5135 - 6129 : 1713 seconds/day
#> 5135 - 7010 : 2090.5 seconds/day
#> 5135 - 7018 : 2240 seconds/day
#> 5135 - 7019 : 4003 seconds/day
#> 5135 - 7022 : 4472 seconds/day
#> 5135 - 7023 : 2208.5 seconds/day
#> 5135 - 7024 : 1315.5 seconds/day
#> 5135 - 7027 : 3732 seconds/day
#> 5135 - 7030 : 4256.5 seconds/day
#> 5135 - 7033 : 1272 seconds/day
#> 5135 - 7043 : 2954.5 seconds/day
#> 5137 - 5139 : 2086.5 seconds/day
#> 5137 - 5145 : 604 seconds/day
#> 5137 - 6005 : 1993 seconds/day
#> 5137 - 6020 : 2008.5 seconds/day
#> 5137 - 6027 : 663.5 seconds/day
#> 5137 - 6028 : 2800.5 seconds/day
#> 5137 - 6030 : 1316 seconds/day
#> 5137 - 6033 : 1686.5 seconds/day
#> 5137 - 6042 : 1932 seconds/day
#> 5137 - 6050 : 3204.5 seconds/day
#> 5137 - 6055 : 2615.5 seconds/day
#> 5137 - 6069 : 2977.5 seconds/day
#> 5137 - 6084 : 3476 seconds/day
#> 5137 - 6090 : 1113 seconds/day
#> 5137 - 6121 : 1885 seconds/day
#> 5137 - 6126 : 1479 seconds/day
#> 5137 - 6129 : 2942.5 seconds/day
#> 5137 - 7010 : 2638.5 seconds/day
#> 5137 - 7018 : 2049 seconds/day
#> 5137 - 7019 : 3736 seconds/day
#> 5137 - 7022 : 2947.5 seconds/day
#> 5137 - 7023 : 3688 seconds/day
#> 5137 - 7024 : 304.5 seconds/day
#> 5137 - 7027 : 1703.5 seconds/day
#> 5137 - 7030 : 2887 seconds/day
#> 5137 - 7033 : 2075.5 seconds/day
#> 5137 - 7043 : 2688.5 seconds/day
#> 5139 - 5145 : 1818.5 seconds/day
#> 5139 - 6005 : 2238.5 seconds/day
#> 5139 - 6020 : 3307.5 seconds/day
#> 5139 - 6027 : 2870 seconds/day
#> 5139 - 6028 : 2001 seconds/day
#> 5139 - 6030 : 2808.5 seconds/day
#> 5139 - 6033 : 4131.5 seconds/day
#> 5139 - 6042 : 2330 seconds/day
#> 5139 - 6050 : 1989.5 seconds/day
#> 5139 - 6055 : 2245 seconds/day
#> 5139 - 6069 : 3363 seconds/day
#> 5139 - 6084 : 3311 seconds/day
#> 5139 - 6090 : 2462 seconds/day
#> 5139 - 6121 : 2691.5 seconds/day
#> 5139 - 6126 : 3614 seconds/day
#> 5139 - 6129 : 3099 seconds/day
#> 5139 - 7010 : 4483.5 seconds/day
#> 5139 - 7018 : 3799 seconds/day
#> 5139 - 7019 : 2385.5 seconds/day
#> 5139 - 7022 : 3871 seconds/day
#> 5139 - 7023 : 3441.5 seconds/day
#> 5139 - 7024 : 1360.5 seconds/day
#> 5139 - 7027 : 2300 seconds/day
#> 5139 - 7030 : 3197.5 seconds/day
#> 5139 - 7033 : 3182.5 seconds/day
#> 5139 - 7043 : 2946.5 seconds/day
#> 5145 - 6005 : 2823.5 seconds/day
#> 5145 - 6020 : 2170.5 seconds/day
#> 5145 - 6027 : 2771 seconds/day
#> 5145 - 6028 : 2143.5 seconds/day
#> 5145 - 6030 : 1862 seconds/day
#> 5145 - 6033 : 1312.5 seconds/day
#> 5145 - 6042 : 2274.5 seconds/day
#> 5145 - 6050 : 1711 seconds/day
#> 5145 - 6055 : 1461 seconds/day
#> 5145 - 6069 : 2087 seconds/day
#> 5145 - 6084 : 2469.5 seconds/day
#> 5145 - 6090 : 1629 seconds/day
#> 5145 - 6121 : 1683.5 seconds/day
#> 5145 - 6126 : 1857 seconds/day
#> 5145 - 6129 : 2172.5 seconds/day
#> 5145 - 7010 : 2043 seconds/day
#> 5145 - 7018 : 3509 seconds/day
#> 5145 - 7019 : 673.5 seconds/day
#> 5145 - 7022 : 2925.5 seconds/day
#> 5145 - 7023 : 3026 seconds/day
#> 5145 - 7024 : 2278 seconds/day
#> 5145 - 7027 : 2048.5 seconds/day
#> 5145 - 7030 : 1269 seconds/day
#> 5145 - 7033 : 1878.5 seconds/day
#> 5145 - 7043 : 1331 seconds/day
#> 6005 - 6020 : 2458 seconds/day
#> 6005 - 6027 : 1394 seconds/day
#> 6005 - 6028 : 2065 seconds/day
#> 6005 - 6030 : 3041.5 seconds/day
#> 6005 - 6033 : 2687 seconds/day
#> 6005 - 6042 : 2970.5 seconds/day
#> 6005 - 6050 : 3810.5 seconds/day
#> 6005 - 6055 : 2542.5 seconds/day
#> 6005 - 6069 : 3334.5 seconds/day
#> 6005 - 6084 : 1623 seconds/day
#> 6005 - 6090 : 2586.5 seconds/day
#> 6005 - 6121 : 2933 seconds/day
#> 6005 - 6126 : 3029.5 seconds/day
#> 6005 - 6129 : 4306.5 seconds/day
#> 6005 - 7010 : 3026 seconds/day
#> 6005 - 7018 : 2755.5 seconds/day
#> 6005 - 7019 : 1455.5 seconds/day
#> 6005 - 7022 : 3623 seconds/day
#> 6005 - 7023 : 3677 seconds/day
#> 6005 - 7024 : 2116.5 seconds/day
#> 6005 - 7027 : 2285 seconds/day
#> 6005 - 7030 : 1574.5 seconds/day
#> 6005 - 7033 : 2339 seconds/day
#> 6005 - 7043 : 3860.5 seconds/day
#> 6020 - 6027 : 3096 seconds/day
#> 6020 - 6028 : 4889.5 seconds/day
#> 6020 - 6030 : 3641.5 seconds/day
#> 6020 - 6033 : 2806.5 seconds/day
#> 6020 - 6042 : 5057 seconds/day
#> 6020 - 6050 : 4324.5 seconds/day
#> 6020 - 6055 : 1138 seconds/day
#> 6020 - 6069 : 4478.5 seconds/day
#> 6020 - 6084 : 6724 seconds/day
#> 6020 - 6090 : 1945.5 seconds/day
#> 6020 - 6121 : 3168.5 seconds/day
#> 6020 - 6126 : 4654.5 seconds/day
#> 6020 - 6129 : 4057 seconds/day
#> 6020 - 7010 : 3246 seconds/day
#> 6020 - 7018 : 4415 seconds/day
#> 6020 - 7019 : 1200 seconds/day
#> 6020 - 7022 : 7192.5 seconds/day
#> 6020 - 7023 : 3562 seconds/day
#> 6020 - 7024 : 3525 seconds/day
#> 6020 - 7027 : 4278 seconds/day
#> 6020 - 7030 : 1510 seconds/day
#> 6020 - 7033 : 2024.5 seconds/day
#> 6020 - 7043 : 2585 seconds/day
#> 6027 - 6028 : 3020 seconds/day
#> 6027 - 6030 : 2988.5 seconds/day
#> 6027 - 6033 : 3139.5 seconds/day
#> 6027 - 6042 : 1349.5 seconds/day
#> 6027 - 6050 : 3079.5 seconds/day
#> 6027 - 6055 : 2553.5 seconds/day
#> 6027 - 6069 : 1822.5 seconds/day
#> 6027 - 6084 : 3368 seconds/day
#> 6027 - 6090 : 3968 seconds/day
#> 6027 - 6121 : 2202.5 seconds/day
#> 6027 - 6126 : 2474.5 seconds/day
#> 6027 - 6129 : 2967.5 seconds/day
#> 6027 - 7010 : 2664.5 seconds/day
#> 6027 - 7018 : 3723 seconds/day
#> 6027 - 7019 : 3073.5 seconds/day
#> 6027 - 7022 : 3511 seconds/day
#> 6027 - 7023 : 4519 seconds/day
#> 6027 - 7024 : 4361.5 seconds/day
#> 6027 - 7027 : 3144.5 seconds/day
#> 6027 - 7030 : 2806.5 seconds/day
#> 6027 - 7033 : 3689 seconds/day
#> 6027 - 7043 : 2974.5 seconds/day
#> 6028 - 6030 : 2068.5 seconds/day
#> 6028 - 6033 : 2419.5 seconds/day
#> 6028 - 6042 : 4528.5 seconds/day
#> 6028 - 6050 : 4472 seconds/day
#> 6028 - 6055 : 636 seconds/day
#> 6028 - 6069 : 4625 seconds/day
#> 6028 - 6084 : 5503.5 seconds/day
#> 6028 - 6090 : 2617.5 seconds/day
#> 6028 - 6121 : 4047.5 seconds/day
#> 6028 - 6126 : 3382 seconds/day
#> 6028 - 6129 : 3685 seconds/day
#> 6028 - 7010 : 2374 seconds/day
#> 6028 - 7018 : 1722 seconds/day
#> 6028 - 7019 : 3751 seconds/day
#> 6028 - 7022 : 6296.5 seconds/day
#> 6028 - 7023 : 3587.5 seconds/day
#> 6028 - 7024 : 1498.5 seconds/day
#> 6028 - 7027 : 2809 seconds/day
#> 6028 - 7030 : 3276.5 seconds/day
#> 6028 - 7033 : 3062 seconds/day
#> 6028 - 7043 : 2614.5 seconds/day
#> 6030 - 6033 : 1366 seconds/day
#> 6030 - 6042 : 1262.5 seconds/day
#> 6030 - 6050 : 1669.5 seconds/day
#> 6030 - 6055 : 1079.5 seconds/day
#> 6030 - 6069 : 2759 seconds/day
#> 6030 - 6084 : 2078.5 seconds/day
#> 6030 - 6090 : 3709.5 seconds/day
#> 6030 - 6121 : 2205 seconds/day
#> 6030 - 6126 : 2716.5 seconds/day
#> 6030 - 6129 : 3945.5 seconds/day
#> 6030 - 7010 : 2361 seconds/day
#> 6030 - 7018 : 2980 seconds/day
#> 6030 - 7019 : 648 seconds/day
#> 6030 - 7022 : 4134 seconds/day
#> 6030 - 7023 : 3633.5 seconds/day
#> 6030 - 7024 : 2834 seconds/day
#> 6030 - 7027 : 1815.5 seconds/day
#> 6030 - 7030 : 1113 seconds/day
#> 6030 - 7033 : 3330 seconds/day
#> 6030 - 7043 : 2896.5 seconds/day
#> 6033 - 6042 : 3713.5 seconds/day
#> 6033 - 6050 : 4188 seconds/day
#> 6033 - 6055 : 3345.5 seconds/day
#> 6033 - 6069 : 4014.5 seconds/day
#> 6033 - 6084 : 4318.5 seconds/day
#> 6033 - 6090 : 2826.5 seconds/day
#> 6033 - 6121 : 4390 seconds/day
#> 6033 - 6126 : 2798.5 seconds/day
#> 6033 - 6129 : 1454 seconds/day
#> 6033 - 7010 : 4408 seconds/day
#> 6033 - 7018 : 3390 seconds/day
#> 6033 - 7019 : 4369.5 seconds/day
#> 6033 - 7022 : 2928 seconds/day
#> 6033 - 7023 : 3158.5 seconds/day
#> 6033 - 7024 : 2630.5 seconds/day
#> 6033 - 7027 : 3608.5 seconds/day
#> 6033 - 7030 : 4587 seconds/day
#> 6033 - 7033 : 1958 seconds/day
#> 6033 - 7043 : 3487 seconds/day
#> 6042 - 6050 : 3857 seconds/day
#> 6042 - 6055 : 2710 seconds/day
#> 6042 - 6069 : 4263 seconds/day
#> 6042 - 6084 : 5199 seconds/day
#> 6042 - 6090 : 2594 seconds/day
#> 6042 - 6121 : 4238 seconds/day
#> 6042 - 6126 : 5060 seconds/day
#> 6042 - 6129 : 2095.5 seconds/day
#> 6042 - 7010 : 2282.5 seconds/day
#> 6042 - 7018 : 2862 seconds/day
#> 6042 - 7019 : 2799.5 seconds/day
#> 6042 - 7022 : 2950 seconds/day
#> 6042 - 7023 : 2077.5 seconds/day
#> 6042 - 7024 : 2069.5 seconds/day
#> 6042 - 7027 : 3607 seconds/day
#> 6042 - 7030 : 3536.5 seconds/day
#> 6042 - 7033 : 2491 seconds/day
#> 6042 - 7043 : 2313.5 seconds/day
#> 6050 - 6055 : 2565 seconds/day
#> 6050 - 6069 : 3635.5 seconds/day
#> 6050 - 6084 : 5247.5 seconds/day
#> 6050 - 6090 : 2914.5 seconds/day
#> 6050 - 6121 : 6243 seconds/day
#> 6050 - 6126 : 1848.5 seconds/day
#> 6050 - 6129 : 2296.5 seconds/day
#> 6050 - 7010 : 2685.5 seconds/day
#> 6050 - 7018 : 3506 seconds/day
#> 6050 - 7019 : 4326.5 seconds/day
#> 6050 - 7022 : 4023 seconds/day
#> 6050 - 7023 : 2620 seconds/day
#> 6050 - 7024 : 2492 seconds/day
#> 6050 - 7027 : 3842.5 seconds/day
#> 6050 - 7030 : 3871.5 seconds/day
#> 6050 - 7033 : 2277.5 seconds/day
#> 6050 - 7043 : 1920 seconds/day
#> 6055 - 6069 : 1733.5 seconds/day
#> 6055 - 6084 : 2312.5 seconds/day
#> 6055 - 6090 : 2877 seconds/day
#> 6055 - 6121 : 2640.5 seconds/day
#> 6055 - 6126 : 1748.5 seconds/day
#> 6055 - 6129 : 1725.5 seconds/day
#> 6055 - 7010 : 3597 seconds/day
#> 6055 - 7018 : 3926.5 seconds/day
#> 6055 - 7019 : 3185 seconds/day
#> 6055 - 7022 : 713.5 seconds/day
#> 6055 - 7023 : 2091 seconds/day
#> 6055 - 7024 : 1898.5 seconds/day
#> 6055 - 7027 : 2463.5 seconds/day
#> 6055 - 7030 : 3504.5 seconds/day
#> 6055 - 7033 : 845 seconds/day
#> 6055 - 7043 : 1952.5 seconds/day
#> 6069 - 6084 : 6136 seconds/day
#> 6069 - 6090 : 1936.5 seconds/day
#> 6069 - 6121 : 4111 seconds/day
#> 6069 - 6126 : 2923 seconds/day
#> 6069 - 6129 : 3479.5 seconds/day
#> 6069 - 7010 : 2062.5 seconds/day
#> 6069 - 7018 : 3701.5 seconds/day
#> 6069 - 7019 : 3437 seconds/day
#> 6069 - 7022 : 5451 seconds/day
#> 6069 - 7023 : 3596 seconds/day
#> 6069 - 7024 : 2552 seconds/day
#> 6069 - 7027 : 2498.5 seconds/day
#> 6069 - 7030 : 2548.5 seconds/day
#> 6069 - 7033 : 2800 seconds/day
#> 6069 - 7043 : 4687 seconds/day
#> 6084 - 6090 : 1796 seconds/day
#> 6084 - 6121 : 3527 seconds/day
#> 6084 - 6126 : 2428 seconds/day
#> 6084 - 6129 : 4640 seconds/day
#> 6084 - 7010 : 2313.5 seconds/day
#> 6084 - 7018 : 5035.5 seconds/day
#> 6084 - 7019 : 5023 seconds/day
#> 6084 - 7022 : 4600.5 seconds/day
#> 6084 - 7023 : 3373.5 seconds/day
#> 6084 - 7024 : 3091 seconds/day
#> 6084 - 7027 : 3991 seconds/day
#> 6084 - 7030 : 4197 seconds/day
#> 6084 - 7033 : 3383 seconds/day
#> 6084 - 7043 : 4305.5 seconds/day
#> 6090 - 6121 : 3244.5 seconds/day
#> 6090 - 6126 : 2537 seconds/day
#> 6090 - 6129 : 2867 seconds/day
#> 6090 - 7010 : 2738 seconds/day
#> 6090 - 7018 : 3992.5 seconds/day
#> 6090 - 7019 : 2752.5 seconds/day
#> 6090 - 7022 : 2529.5 seconds/day
#> 6090 - 7023 : 2926 seconds/day
#> 6090 - 7024 : 1925.5 seconds/day
#> 6090 - 7027 : 1052.5 seconds/day
#> 6090 - 7030 : 2905.5 seconds/day
#> 6090 - 7033 : 3149.5 seconds/day
#> 6090 - 7043 : 2672 seconds/day
#> 6121 - 6126 : 2554 seconds/day
#> 6121 - 6129 : 661.5 seconds/day
#> 6121 - 7010 : 2946 seconds/day
#> 6121 - 7018 : 4288.5 seconds/day
#> 6121 - 7019 : 4617 seconds/day
#> 6121 - 7022 : 3563 seconds/day
#> 6121 - 7023 : 2739.5 seconds/day
#> 6121 - 7024 : 2337.5 seconds/day
#> 6121 - 7027 : 3871.5 seconds/day
#> 6121 - 7030 : 3813.5 seconds/day
#> 6121 - 7033 : 1392 seconds/day
#> 6121 - 7043 : 1316.5 seconds/day
#> 6126 - 6129 : 3154 seconds/day
#> 6126 - 7010 : 3302.5 seconds/day
#> 6126 - 7018 : 1051 seconds/day
#> 6126 - 7019 : 1298 seconds/day
#> 6126 - 7022 : 4910.5 seconds/day
#> 6126 - 7023 : 3748.5 seconds/day
#> 6126 - 7024 : 2373.5 seconds/day
#> 6126 - 7027 : 3040.5 seconds/day
#> 6126 - 7030 : 1989 seconds/day
#> 6126 - 7033 : 1965 seconds/day
#> 6126 - 7043 : 2232 seconds/day
#> 6129 - 7010 : 3672 seconds/day
#> 6129 - 7018 : 2752.5 seconds/day
#> 6129 - 7019 : 2635.5 seconds/day
#> 6129 - 7022 : 4708 seconds/day
#> 6129 - 7023 : 5633 seconds/day
#> 6129 - 7024 : 1608 seconds/day
#> 6129 - 7027 : 691 seconds/day
#> 6129 - 7030 : 2073.5 seconds/day
#> 6129 - 7033 : 4622.5 seconds/day
#> 6129 - 7043 : 5024.5 seconds/day
#> 7010 - 7018 : 3584 seconds/day
#> 7010 - 7019 : 2444 seconds/day
#> 7010 - 7022 : 3010.5 seconds/day
#> 7010 - 7023 : 4200.5 seconds/day
#> 7010 - 7024 : 2020 seconds/day
#> 7010 - 7027 : 3045.5 seconds/day
#> 7010 - 7030 : 3156 seconds/day
#> 7010 - 7033 : 2309.5 seconds/day
#> 7010 - 7043 : 3560 seconds/day
#> 7018 - 7019 : 2409 seconds/day
#> 7018 - 7022 : 2687.5 seconds/day
#> 7018 - 7023 : 2684 seconds/day
#> 7018 - 7024 : 3021 seconds/day
#> 7018 - 7027 : 3222.5 seconds/day
#> 7018 - 7030 : 2997.5 seconds/day
#> 7018 - 7033 : 2621 seconds/day
#> 7018 - 7043 : 2350 seconds/day
#> 7019 - 7022 : 2039.5 seconds/day
#> 7019 - 7023 : 3874.5 seconds/day
#> 7019 - 7024 : 1090.5 seconds/day
#> 7019 - 7027 : 2054.5 seconds/day
#> 7019 - 7030 : 4784 seconds/day
#> 7019 - 7033 : 3581.5 seconds/day
#> 7019 - 7043 : 3699.5 seconds/day
#> 7022 - 7023 : 4690.5 seconds/day
#> 7022 - 7024 : 3361.5 seconds/day
#> 7022 - 7027 : 3262 seconds/day
#> 7022 - 7030 : 2294.5 seconds/day
#> 7022 - 7033 : 3544 seconds/day
#> 7022 - 7043 : 3198.5 seconds/day
#> 7023 - 7024 : 1969.5 seconds/day
#> 7023 - 7027 : 1997.5 seconds/day
#> 7023 - 7030 : 2550.5 seconds/day
#> 7023 - 7033 : 4497.5 seconds/day
#> 7023 - 7043 : 5800.5 seconds/day
#> 7024 - 7027 : 2324.5 seconds/day
#> 7024 - 7030 : 976.5 seconds/day
#> 7024 - 7033 : 1664 seconds/day
#> 7024 - 7043 : 2202 seconds/day
#> 7027 - 7030 : 2887.5 seconds/day
#> 7027 - 7033 : 1598 seconds/day
#> 7027 - 7043 : 1269.5 seconds/day
#> 7030 - 7033 : 1928 seconds/day
#> 7030 - 7043 : 2322 seconds/day
#> 7033 - 7043 : 3949.5 seconds/day
```

## 9. Detecting Avoidance

Low synchronicity despite being in the same herd might indicate
avoidance:

``` r
# Find pairs with very low co-occurrence (potential avoidance)
cat("Pairs with minimal co-occurrence (< 10 seconds total across all days):\n")
#> Pairs with minimal co-occurrence (< 10 seconds total across all days):
low_sync_count <- 0

if (length(pair_feed_results$total_time) > 1) {
  total_time_matrix <- pair_feed_results$total_time[[1]] * 0

  for (day_matrix in pair_feed_results$total_time) {
    total_time_matrix <- total_time_matrix + day_matrix
  }

  for (i in 1:(nrow(total_time_matrix) - 1)) {
    for (j in (i + 1):ncol(total_time_matrix)) {
      if (total_time_matrix[i, j] < 10 && total_time_matrix[i, j] >= 0) {
        low_sync_count <- low_sync_count + 1
      }
    }
  }

  cat("Total pairs with low synchronicity:", low_sync_count, "\n")
}
#> Total pairs with low synchronicity: 0
```

## 10. Code Cheatsheet

``` r
# Copy and modify these code blocks for your own analysis!

# ---- SETUP: Global Variables (REQUIRED FIRST!) ----
library(moo4feed)
library(dplyr)

# Set up your column names and bin configuration
set_global_cols(
  id_col = "cow",           # Your animal ID column
  start_col = "start",      # Visit start time column
  end_col = "end",          # Visit end time column
  bin_col = "bin",          # Bin/feeder ID column
  start_weight_col = "start_weight",  # Start weight column (feed only)
  end_weight_col = "end_weight",      # End weight column (feed only)
  tz = "America/Vancouver", # Your timezone
  bins_feed = 1:30,         # Your feed bin IDs
  bins_wat = 1:5,           # Your water bin IDs
  # Define your barn's bin layout (rows separated by \n, bins by -)
  bin_layout = "1-2-3-4-5-6-101-102-7-8-9-10-11-12"
)

# ---- STEP 1: Create Time-Based Matrices ----
# For feed data
feed_matrices <- matrix_process(
  data_list = your_clean_feed,   # Your cleaned feed data
  type = "feed",
  resolution = "sec",              # Try: "min" for faster processing
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  start_weight_col = start_weight_col2(),
  end_weight_col = end_weight_col2(),
  bins_feed = bins_feed2()
)

# For water data
water_matrices <- matrix_process(
  data_list = your_clean_water,   # Your cleaned water data
  type = "drink",
  resolution = "sec",
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  bins_wat = bins_wat2()
)

# ---- STEP 2: Pair-wise Co-Occurrence Analysis ----
# Analyze feeding synchronicity
pair_feed_results <- synch_pair_analysis(
  matrix_data = feed_matrices,
  type = "feed",
  resolution = "sec",              # Must match matrix_process resolution
  id_col = id_col2()
)

# Analyze drinking synchronicity
pair_water_results <- synch_pair_analysis(
  matrix_data = water_matrices,
  type = "drink",
  resolution = "sec",
  id_col = id_col2()
)

# Access results (one matrix per day if multi-day input)
bout_counts <- pair_feed_results$bout           # Number of bouts together
total_times <- pair_feed_results$total_time    # Total time together
avg_durations <- pair_feed_results$avg_duration # Average bout duration

# For single day: bout_counts is a matrix
# For multi-day: bout_counts[[1]] is first day's matrix

# ---- STEP 3: Spatial Neighbor Analysis ----
# Analyze neighbor patterns for feeding
neighbor_feed_results <- synch_neighbor_analysis(
  matrix_data = feed_matrices,
  bin_layout = bin_layout2(),      # Use your configured layout
  type = "feed",
  resolution = "sec",
  id_col = id_col2()
)

# Analyze neighbor patterns for drinking
neighbor_water_results <- synch_neighbor_analysis(
  matrix_data = water_matrices,
  bin_layout = bin_layout2(),
  type = "drink",
  resolution = "sec",
  id_col = id_col2()
)

# Access neighbor results
neighbor_bouts <- neighbor_feed_results$bout
neighbor_times <- neighbor_feed_results$total_time
neighbor_avg <- neighbor_feed_results$avg_duration

# ---- STEP 4: Extract and Analyze Specific Pairs ----
# Get results for first day
day1_matrix <- pair_feed_results$total_time[[1]]

# Find value for a specific pair (e.g., animals "5120" and "6084")
pair_time <- day1_matrix["5120", "6084"]  # Time they spent together

# Find all pairs with high synchronicity
threshold <- 100  # seconds
high_sync_pairs <- which(day1_matrix > threshold, arr.ind = TRUE)
high_sync_pairs <- high_sync_pairs[high_sync_pairs[,1] < high_sync_pairs[,2], ]  # Upper triangle only

# ---- STEP 5: Compare Across Days ----
# Sum synchronicity across all days for each pair
if (length(pair_feed_results$total_time) > 1) {
  total_matrix <- pair_feed_results$total_time[[1]] * 0  # Initialize

  for (day_matrix in pair_feed_results$total_time) {
    total_matrix <- total_matrix + day_matrix
  }

  # Average across days
  avg_matrix <- total_matrix / length(pair_feed_results$total_time)
}

# ---- BONUS: Quick Summary Statistics ----
# Total synchronicity in the herd (first day)
day1_total <- sum(day1_matrix[upper.tri(day1_matrix)])
cat("Total co-occurrence time:", day1_total, "seconds\n")

# Number of active pairs (first day)
n_active_pairs <- sum(day1_matrix[upper.tri(day1_matrix)] > 0)
cat("Number of pairs with co-occurrence:", n_active_pairs, "\n")

# Average synchronicity per active pair
avg_sync <- day1_total / n_active_pairs
cat("Average time per active pair:", round(avg_sync, 1), "seconds\n")
```

------------------------------------------------------------------------

**🦦 Ollie’s Final Words**: *“Synchronicity analysis opens a window into
the social world of your animals! Remember that feeding together doesn’t
always mean friendship—it could also be feeding pattern overlap. Combine
synchronicity data with other behavioral measures for a complete picture
of social dynamics!”*

**Next Steps**: Use synchronicity results to identify animals for
detailed behavioral observation, detect changes in social structure over
time, or validate social network models.
