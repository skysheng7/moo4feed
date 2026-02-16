# 7. Feed Availability Analysis

``` r
library(moo4feed)
library(ggplot2)
library(dplyr)
```

## 1. Set Your Global Variables First!

Before analyzing feed availability patterns, configure global variables
to match your data structure:

``` r
# Configure global variables for your data structure
set_global_cols(
  id_col = "cow",           # Animal ID column name
  start_col = "start",      # Visit start time column
  end_col = "end",          # Visit end time column
  bin_col = "bin",          # Bin/feeder ID column
  intake_col = "intake",    # Feed intake amount column
  dur_col = "duration",     # Visit duration column
  start_weight_col = "start_weight",  # Bin weight at visit start
  end_weight_col = "end_weight",      # Bin weight at visit end
  tz = "America/Vancouver"  # Your timezone
)
```

## 2. Introduction to Feed Availability Analysis

Understanding when feed is added to bins and how much feed is available
when animals visit each bin can help researchers and farmers better
track feed management on daily basis, and identify animals that may be
disadvantaged.

- **Monitor feed management**: Identify when bins are refilled
  throughout the day, the frequency of feed additions, and the amount of
  feed added to each bin.
- **Identify disadvantaged animals**: Animals visiting when bins are
  nearly empty may be disadvantaged

### What We’ll Learn

This tutorial demonstrates how to:

> 1.  **Detect feed addition events** - Identify when bins are refilled
>     based on weight increases
> 2.  **Calculate feed availability** - Determine the percentage of feed
>     remaining at each visit
> 3.  **Summarize feeding conditions** - Analyze how feed availability
>     varies across animals

## 3. Prerequisites

This tutorial assumes completion of previous data processing steps in
[**Tutorial 1: Data
Cleaning**](https://skysheng7.github.io/moo4feed/articles/data_cleaning.md).

Your data must include **start_weight** and **end_weight** columns
representing the bin weight at the beginning and end of each visit.

## 4. Data Preparation

``` r
# Load cleaned example data
data(clean_feed)

# If you're using your own data from previous tutorials, use this instead:
# clean_feed <- your_cleaned_feed_data     # From your cleaning results

# Quick peek at our data structure
cat("Feed data structure:\n")
#> Feed data structure:
head(clean_feed[[1]], 3)  # First day, first 3 rows
#> # A tibble: 3 × 11
#>   transponder   cow   bin start               end                 duration
#>         <int> <int> <dbl> <dttm>              <dttm>                 <dbl>
#> 1    12448407  6020     1 2020-10-31 00:26:12 2020-10-31 00:27:36       84
#> 2    11954014  4044     1 2020-10-31 01:17:43 2020-10-31 01:22:13      270
#> 3    11954042  4072     1 2020-10-31 01:37:30 2020-10-31 01:37:52       22
#> # ℹ 5 more variables: start_weight <dbl>, end_weight <dbl>, intake <dbl>,
#> #   date <date>, rate <dbl>

cat("\nTotal days of feed data:", length(clean_feed), "\n")
#> 
#> Total days of feed data: 2
```

## 5. Detecting Feed Addition Events

Feed additions are detected by identifying significant weight increases
between consecutive visits at the same bin. When the bin weight at the
start of a visit is much higher than the bin weight at the end of the
previous visit, feed must have been added in between.

### Understanding Feed Addition Detection

The
[`detect_feed_additions()`](https://skysheng7.github.io/moo4feed/reference/detect_feed_additions.md)
function performs aggregation in two stages:

**Stage 1: Within-bin aggregation (always performed)** When a farmer
adds feed to the same bin multiple times in quick succession (within
`max_bin_time_gap` seconds), these additions are automatically grouped
into a single feed event per bin.

**Stage 2: Across-bin aggregation (optional)** If
`aggregate_all_bin = TRUE`, individual bin additions are grouped into
multi-bin feed events when multiple bins are refilled within a time
window.

### Detect Per-Bin Feed Additions

For calculating feed availability at each visit, we need per-bin
additions (not aggregated across bins):

``` r
# Detect feed additions for each bin separately
feed_additions <- detect_feed_additions(
  data = clean_feed,
  min_weight_increase = 5,      # Minimum kg increase to count as addition

  max_bin_time_gap = 3600,      # Group rapid additions within 1 hour
  aggregate_all_bin = FALSE     # Keep per-bin additions (required for availability calc)
)

# Examine the first day's feed additions
cat("Feed additions detected on first day:\n")
#> Feed additions detected on first day:
head(feed_additions[[1]])
#>         date                time weight_increase bin_weight_after_fill bin
#> 1 2020-10-31 2020-10-31 06:04:40            34.6                  40.0  21
#> 2 2020-10-31 2020-10-31 06:04:51            27.5                  41.6  15
#> 3 2020-10-31 2020-10-31 06:04:53            31.5                  35.0   2
#> 4 2020-10-31 2020-10-31 06:05:00            30.4                  35.1   7
#> 5 2020-10-31 2020-10-31 06:05:16            33.3                  40.2   3
#> 6 2020-10-31 2020-10-31 06:05:20            49.1                  54.0  23

cat("\nFeed additions per day:\n")
#> 
#> Feed additions per day:
sapply(feed_additions, nrow)
#> 2020-10-31 2020-11-01 
#>         60         58
```

Each feed addition event contains:

- **`date`**: Date of the feed addition
- **`bin`**: Bin identifier where feed was added
- **`time`**: Timestamp of the first detected addition in this event
- **`weight_increase`**: Total amount of feed added (kg)
- **`bin_weight_after_fill`**: Total bin weight after the final addition
  (kg)

### Detect Aggregated Feed Events (Optional)

To identify coordinated feeding events where multiple bins are refilled
together:

``` r
# Detect multi-bin feed events
feed_events <- detect_feed_additions(
  data = clean_feed,
  min_weight_increase = 5,
  max_bin_time_gap = 3600,
  min_bins_for_group = 3,       # At least 3 bins filled to count as event
  aggregate_all_bin = TRUE      # Aggregate across bins
)

# Examine the aggregated events
cat("Aggregated feed events on first day:\n")
#> Aggregated feed events on first day:
head(feed_events[[1]])
#>         date event_id         event_start           event_end bins_filled
#> 1 2020-10-31        1 2020-10-31 06:04:40 2020-10-31 06:26:34          30
#> 2 2020-10-31        3 2020-10-31 15:46:59 2020-10-31 16:13:30          29
#>   avg_weight_increase
#> 1            35.48333
#> 2            54.24828

cat("\nMulti-bin feed events per day:\n")
#> 
#> Multi-bin feed events per day:
sapply(feed_events, nrow)
#> 2020-10-31 2020-11-01 
#>          2          2
```

Aggregated events contain:

- **`event_id`**: Unique identifier for the feed event
- **`event_start`**: Earliest addition time in the event
- **`event_end`**: Latest addition time in the event
- **`bins_filled`**: Number of bins refilled in the event
- **`avg_weight_increase`**: Average feed added across bins (kg)

## 6. Calculating Feed Availability at Each Visit

Once we have per-bin feed additions, we can calculate the percentage of
feed remaining when each animal visits. This helps identify animals that
consistently visit when bins are nearly empty.

### Calculate Feed Availability

``` r
# Calculate feed availability for each visit
availability <- calculate_feed_availability(
  visit_data = clean_feed,
  feed_addition_data = feed_additions  # Must use aggregate_all_bin = FALSE
)

# The function returns a list with two elements:
# 1. visits - visit-level data with feed percentages
# 2. daily_summary - summary statistics per animal per day
```

### Visit-Level Results

``` r
# Examine visit-level data from first day
visits_with_availability <- availability$visits[[1]]

cat("Visit data with feed availability (first day):\n")
#> Visit data with feed availability (first day):
head(visits_with_availability[, c("cow", "bin", "start", "start_weight",
                                   "feed_addition_time", "bin_weight_after_fill",
                                   "pct_feed_remaining")])
#> # A tibble: 6 × 7
#>     cow   bin start               start_weight feed_addition_time 
#>   <int> <dbl> <dttm>                     <dbl> <dttm>             
#> 1  6020     1 2020-10-31 00:26:12          3.8 NA                 
#> 2  4044     1 2020-10-31 01:17:43          3.5 NA                 
#> 3  4072     1 2020-10-31 01:37:30          2.6 NA                 
#> 4  5124     1 2020-10-31 06:05:49         51.1 2020-10-31 06:05:49
#> 5  6020     1 2020-10-31 06:08:02         50.7 2020-10-31 06:05:49
#> 6  6069     1 2020-10-31 06:09:55         50.1 2020-10-31 06:05:49
#> # ℹ 2 more variables: bin_weight_after_fill <dbl>, pct_feed_remaining <dbl>
```

New columns added to visit data:

- **`feed_addition_time`**: When feed was last added to this bin
- **`feed_added_weight`**: Weight of feed added to this bin (kg)
- **`bin_weight_after_fill`**: Bin weight after feed was added (kg)
- **`pct_feed_remaining`**: Percentage of feed remaining at visit start

### Daily Summary Statistics

``` r
# Examine daily summary from first day
daily_summary <- availability$daily_summary[[1]]

cat("Daily feed availability summary (first day):\n")
#> Daily feed availability summary (first day):
print(daily_summary)
#>          date  cow mean_pct_feed_remaining median_pct_feed_remaining
#> 1  2020-10-31 2074                59.70264                  53.96419
#> 2  2020-10-31 3150                62.22631                  67.39741
#> 3  2020-10-31 4001                68.03947                  73.08782
#> 4  2020-10-31 4044                56.13479                  52.74473
#> 5  2020-10-31 4070                44.92761                  37.19187
#> 6  2020-10-31 4072                48.04099                  51.27479
#> 7  2020-10-31 4080                63.09911                  68.87136
#> 8  2020-10-31 5028                60.78203                  59.76409
#> 9  2020-10-31 5041                57.02087                  59.54471
#> 10 2020-10-31 5042                67.86248                  66.59316
#> 11 2020-10-31 5058                57.86662                  56.14495
#> 12 2020-10-31 5061                62.95931                  66.66667
#> 13 2020-10-31 5067                48.39032                  49.34334
#> 14 2020-10-31 5100                48.51567                  45.58824
#> 15 2020-10-31 5114                66.52040                  61.71367
#> 16 2020-10-31 5120                68.29555                  68.00000
#> 17 2020-10-31 5123                54.79362                  53.77550
#> 18 2020-10-31 5124                45.58264                  35.25557
#> 19 2020-10-31 5135                52.79437                  50.58824
#> 20 2020-10-31 5137                55.22316                  62.12375
#> 21 2020-10-31 5139                62.07438                  57.59689
#> 22 2020-10-31 5145                59.39041                  54.60823
#> 23 2020-10-31 6005                60.36736                  70.20003
#> 24 2020-10-31 6020                52.37488                  47.73333
#> 25 2020-10-31 6027                48.87948                  46.25965
#> 26 2020-10-31 6028                57.33349                  51.74098
#> 27 2020-10-31 6030                51.48407                  57.45297
#> 28 2020-10-31 6033                67.03657                  64.68452
#> 29 2020-10-31 6042                42.32230                  21.15385
#> 30 2020-10-31 6050                54.71779                  45.02413
#> 31 2020-10-31 6055                51.10938                  49.06782
#> 32 2020-10-31 6069                53.50303                  45.76923
#> 33 2020-10-31 6084                47.67986                  50.24876
#> 34 2020-10-31 6090                61.26137                  59.74541
#> 35 2020-10-31 6121                52.37876                  41.57808
#> 36 2020-10-31 6126                56.00286                  60.62429
#> 37 2020-10-31 6129                66.57368                  67.26652
#> 38 2020-10-31 7010                61.66005                  57.62653
#> 39 2020-10-31 7018                45.43502                  40.41667
#> 40 2020-10-31 7019                62.37043                  60.06737
#> 41 2020-10-31 7022                54.38005                  62.11765
#> 42 2020-10-31 7023                58.92741                  61.82085
#> 43 2020-10-31 7024                59.27357                  64.83593
#> 44 2020-10-31 7027                52.26362                  49.81007
#> 45 2020-10-31 7030                55.72576                  51.53096
#> 46 2020-10-31 7033                58.77407                  63.11637
#> 47 2020-10-31 7043                70.72020                  73.95683
#>    sd_pct_feed_remaining total_visits_analyzed
#> 1               28.59389                    45
#> 2               26.29525                    52
#> 3               18.72861                    49
#> 4               24.65588                    58
#> 5               29.56021                    54
#> 6               26.01743                    71
#> 7               27.59500                    74
#> 8               19.37911                    41
#> 9               24.58661                    86
#> 10              15.68005                   114
#> 11              28.22823                    64
#> 12              21.14296                    47
#> 13              31.37286                    69
#> 14              23.67638                    75
#> 15              20.92599                    29
#> 16              18.98159                    57
#> 17              29.98161                    64
#> 18              34.79831                    83
#> 19              28.80174                    51
#> 20              29.48564                    54
#> 21              23.33456                    42
#> 22              21.54808                    66
#> 23              30.24233                    78
#> 24              25.29912                    59
#> 25              21.73737                    76
#> 26              23.09591                    90
#> 27              31.66523                    79
#> 28              20.81142                    68
#> 29              35.47483                    51
#> 30              26.44361                    64
#> 31              18.62885                    80
#> 32              30.60316                    77
#> 33              26.40060                    71
#> 34              22.30088                    74
#> 35              26.63942                    44
#> 36              29.19119                    58
#> 37              15.22923                    38
#> 38              22.77105                    36
#> 39              24.15284                    67
#> 40              17.49352                    76
#> 41              25.36602                    67
#> 42              26.75882                    75
#> 43              20.23474                    56
#> 44              28.30130                   140
#> 45              20.74939                   140
#> 46              21.26778                    71
#> 47              22.58502                    51
```

The daily summary provides per animal:

- **`mean_pct_feed_remaining`**: Average percentage across visits
- **`median_pct_feed_remaining`**: Median percentage across visits
- **`sd_pct_feed_remaining`**: Standard deviation of percentage
- **`total_visits_analyzed`**: Number of visits with valid feed data

## 7. Analyzing Feed Availability Patterns

### Identify Potentially Disadvantaged Animals

Animals consistently visiting when little feed remains may be
competitively disadvantaged:

``` r
# Combine all daily summaries
all_summaries <- do.call(rbind, availability$daily_summary)

# Find animals with lowest average feed availability
low_availability <- all_summaries |>
  dplyr::group_by(cow) |>
  dplyr::summarise(
    overall_mean_pct = mean(mean_pct_feed_remaining, na.rm = TRUE),
    overall_median_pct = median(median_pct_feed_remaining, na.rm = TRUE),
    days_analyzed = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(overall_mean_pct)

cat("Animals with lowest average feed availability:\n")
#> Animals with lowest average feed availability:
head(low_availability, 5)
#> # A tibble: 5 × 4
#>     cow overall_mean_pct overall_median_pct days_analyzed
#>   <int>            <dbl>              <dbl>         <int>
#> 1  6055             41.4               43.3             2
#> 2  7018             44.4               40.6             2
#> 3  6042             45.0               28.6             2
#> 4  5100             45.2               42.5             2
#> 5  6084             45.6               42.4             2

cat("\nAnimals with highest average feed availability:\n")
#> 
#> Animals with highest average feed availability:
tail(low_availability, 5)
#> # A tibble: 5 × 4
#>     cow overall_mean_pct overall_median_pct days_analyzed
#>   <int>            <dbl>              <dbl>         <int>
#> 1  3150             62.5               66.5             2
#> 2  7043             64.7               69.3             2
#> 3  5042             64.9               64.8             2
#> 4  4001             66.5               69.9             2
#> 5  6033             68.7               66.9             2
```

### Visualize Feed Availability Distribution

``` r
# Distribution of feed availability across all visits
all_visits <- do.call(rbind, availability$visits)

# Filter to visits with valid feed percentage
valid_visits <- all_visits |>
  dplyr::filter(!is.na(pct_feed_remaining))

# Create histogram
ggplot(valid_visits, aes(x = pct_feed_remaining)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Feed Availability at Visits",
    x = "Percentage of Feed Remaining (%)",
    y = "Number of Visits"
  ) +
  theme_minimal()
```

![](feed_availability_analysis_files/figure-html/viz-availability-1.png)

### Compare Feed Availability by Animal

``` r
# Boxplot of feed availability by animal (top 10 animals by visit count)
top_animals <- valid_visits |>
  dplyr::count(cow, sort = TRUE) |>
  head(10) |>
  dplyr::pull(cow)

valid_visits |>
  dplyr::filter(cow %in% top_animals) |>
  ggplot(aes(x = reorder(cow, pct_feed_remaining, FUN = median),
             y = pct_feed_remaining)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(
    title = "Feed Availability by Animal",
    x = "Animal ID",
    y = "Percentage of Feed Remaining (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](feed_availability_analysis_files/figure-html/viz-by-animal-1.png)

## 8. Summary

This tutorial demonstrated feed availability analysis:

- **Feed addition detection**: Identified when bins are refilled based
  on weight increases
- **Per-visit availability**: Calculated the percentage of feed
  remaining at each visit
- **Daily summaries**: Aggregated feed availability statistics per
  animal per day
- **Pattern identification**: Found animals that may be competitively
  disadvantaged

## 9. Code Cheatsheet

``` r
#' Copy and modify these code blocks for your own analysis!

# ---- SETUP: Global Variables (REQUIRED FIRST!) ----
library(moo4feed)
library(ggplot2)
library(dplyr)

# Set up your column names and timezone (modify these!)
set_global_cols(
  id_col = "cow",                     # Your animal ID column
  start_col = "start",                # Visit start time column
  end_col = "end",                    # Visit end time column
  bin_col = "bin",                    # Bin/feeder ID column
  intake_col = "intake",              # Feed intake amount column
  dur_col = "duration",               # Visit duration column
  start_weight_col = "start_weight",  # Bin weight at visit start
  end_weight_col = "end_weight",      # Bin weight at visit end
  tz = "America/Vancouver"            # Your timezone
)

# ---- STEP 1: Load Your Data ----
# Use the example data:
data(clean_feed)

# Or use your own cleaned data from Tutorial 1:
# clean_feed <- your_cleaned_feed_data

# ---- STEP 2: Detect Feed Additions (Per-Bin) ----
# This is required for calculating feed availability
feed_additions <- detect_feed_additions(
  data = clean_feed,
  min_weight_increase = 5,      # Minimum kg to count as addition
  max_bin_time_gap = 3600,      # Group additions within 1 hour (seconds)
  aggregate_all_bin = FALSE     # Keep per-bin (REQUIRED for availability)
)

# Check results
head(feed_additions[[1]])

# ---- STEP 3: Detect Aggregated Feed Events (Optional) ----
# Use this to identify coordinated multi-bin feeding events
feed_events <- detect_feed_additions(
  data = clean_feed,
  min_weight_increase = 5,
  max_bin_time_gap = 3600,
  min_bins_for_group = 3,       # At least 3 bins to count as event
  aggregate_all_bin = TRUE      # Aggregate across bins
)

# Check results
head(feed_events[[1]])

# ---- STEP 4: Calculate Feed Availability ----
availability <- calculate_feed_availability(
  visit_data = clean_feed,
  feed_addition_data = feed_additions  # From Step 2 (aggregate_all_bin = FALSE)
)

# Access visit-level data with feed percentages
visits_with_pct <- availability$visits

# Access daily summaries per animal
daily_summaries <- availability$daily_summary

# View first day results
head(visits_with_pct[[1]])
print(daily_summaries[[1]])

# ---- STEP 5: Analyze Patterns ----
# Combine all summaries
all_summaries <- do.call(rbind, availability$daily_summary)

# Find animals with lowest feed availability
low_availability <- all_summaries |>
  dplyr::group_by(cow) |>
  dplyr::summarise(
    overall_mean_pct = mean(mean_pct_feed_remaining, na.rm = TRUE),
    days_analyzed = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(overall_mean_pct)

print(low_availability)

# ---- STEP 6: Visualize Results ----
# Combine all visits
all_visits <- do.call(rbind, availability$visits)

# Histogram of feed availability
ggplot(all_visits, aes(x = pct_feed_remaining)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Feed Availability at Visits",
    x = "Percentage of Feed Remaining (%)",
    y = "Number of Visits"
  ) +
  theme_minimal()
```

------------------------------------------------------------------------
