# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**moo4feed** is an R package for extracting individual-level traits from raw feeding and drinking data collected through precision livestock farming systems. The package supports animal welfare research by enabling reproducible, scalable analysis workflows.

## Development Commands

### Setup and Installation
```r
# Load package during development
devtools::load_all()
```

### Testing
```r
# IMPORTANT: Always run load_all() before testing
devtools::load_all()

# Run all tests
devtools::test()
# or
testthat::test()

# Run tests with coverage
covr::package_coverage()
```

**Critical Testing Note**: Never test individual files with `testthat::test_file()`. Always use `load_all()` first, then run all tests together with `testthat::test()`.

### Documentation
```r
# Generate documentation from roxygen2 comments
devtools::document()

# Build and preview pkgdown site
pkgdown::build_site()
```

### Code Quality
```r
# Check package for CRAN submission standards
devtools::check()

# Style code following tidyverse guidelines
styler::style_pkg()
```

## Architecture

### Global Variable System (`R/global_var.R`)

The package uses a runtime environment (`the`) to store configurable global variables. This system allows users to customize column names, bin configurations, and timezone settings without modifying code.

**Key Functions:**
- `set_global_cols()` - Set multiple globals at once
- Individual getters: `id_col2()`, `start_col2()`, `end_col2()`, `bin_col2()`, `tz2()`, `bins_feed2()`, `bins_wat2()`, `bin_layout2()`
- Individual setters: `set_id_col2()`, `set_start_col2()`, `set_end_col2()`, `set_bin_col2()`, `set_tz2()`, `set_bins_feed2()`, `set_bins_wat2()`, `set_bin_layout2()`

**Usage Pattern**: All functions should accept column name parameters with defaults from these global functions (e.g., `id_col = id_col2()`). Use `@inheritParams` to avoid documentation duplication.

### Configuration System (`R/quality_check_setup.R`)

The `qc_config()` function centralizes all quality control thresholds:
- Duration thresholds for feed/water visits
- Intake thresholds (visit-level and daily)
- Traffic thresholds for bins
- Replacement behavior thresholds
- Calibration error tolerances

Pass configuration objects to QC functions rather than hardcoding thresholds.

### Data Processing Pipeline (`R/data_pipeline.R`)

Core workflow:
1. **Single File Processing**: `process_feeder()` / `process_water()`
   - Read CSV/DAT files safely
   - Rename columns
   - Filter unwanted animals/transponders
   - Keep specified bins
   - Offset water bin IDs to avoid conflicts with feed bins

2. **Batch Processing**: `process_all_feed()` / `process_all_water()`
   - Process multiple files
   - Extract dates from filenames
   - Apply daylight saving time adjustments (North American timezones)
   - Standardize timestamps to POSIXct format
   - Return named list keyed by date (YYYY-MM-DD)

### Synchronicity Analysis

**Matrix Creation** (`R/synch_matrix_creation.R`): Creates time-animal matrices where rows represent time points (seconds or minutes) and columns represent individual animals.

**Matrix Processing** (`R/synch_matrix_processing.R`): Processes matrices to identify synchronous behavior patterns.

**Analysis Types**:
- **Pair Analysis** (`R/synch_pair_analysis.R`): Analyzes pairwise synchronicity between animals
- **Neighbor Analysis** (`R/synch_neighbor_analysis.R`): Analyzes synchronicity based on physical bin proximity using `bin_layout2()`

### Meal Clustering

The package identifies meals from raw visit data using clustering algorithms:
- `cluster_meals()` - Main function using DBSCAN or gap-based methods
- `cluster_meals_cow_day()` - Per-animal, per-day clustering
- `meal_optimal_interval()` - Determines optimal inter-meal intervals
- Helper utilities in `R/cluster_gap_utils.R` and `R/cluster_time_utils.R`

### Quality Control (`R/qc.R` and `R/qc_*.R`)

Modular QC system with individual checks:
- `qc_total_cows()` - Verify expected herd size
- `qc_double_detection()` / `qc_handle_double_detection()` - Handle simultaneous detections
- `qc_negatives()` - Flag negative intakes
- `qc_long_dur()` - Flag unusually long visits
- `qc_large_intake()` - Flag abnormally large intakes
- `qc_bin_visits()` - Check bin traffic patterns
- `qc_check_intake()` - Validate daily intake ranges

Use `qc()` as the main wrapper that orchestrates all checks.

## Code Style and Conventions

### Critical Rules

1. **Never use `library()` or `require()`** - R packages access dependencies via namespace
2. **Never use `source()`** - All code must be in `R/` directory
3. **Never hardcode file paths** - Use `system.file()` for package files
4. **Never write files to disk within functions**
5. **Never modify `man/` or `NAMESPACE` directly** - These are auto-generated
6. **Never hardcode column names** - Use global variable functions from `global_var.R`
7. **Use `lubridate` for all datetime operations** - When using `ymd_hms()`, always pass `tz2()` for timezone

### Documentation Standards

**Exported Functions** (`@export`):
- One-sentence title
- Thorough description
- Document all parameters with `@param`
- Use `@inheritParams` for recursive parameter inheritance
- Include `@return` describing output
- Provide working `@examples` with toy datasets
- Use `[package::function()]` for automatic linking

**Internal Functions** (`@noRd`):
- Add `@keywords internal`
- Still document parameters and returns for maintainer clarity

### Parameter Validation

For predefined choices, use: `match.arg(trimws(tolower(parameter_name)))` to enable case-insensitive, whitespace-tolerant input (e.g., accepting "SEC", "sec", " Min ").

### Terminology

- Use "animal" not "cow" in all documentation and comments (for generalizability)

### Testing Requirements

- Write tests for every new function covering:
  - Normal use cases
  - Edge cases
  - Error handling
- Maintain >90% code coverage
- Test files follow pattern: `tests/testthat/test-function_name.R`

### Function Organization

- One function (or tightly related functions) per file in `R/`
- Keep functions focused on single tasks
- Break complex operations into small internal helper functions
- Use consistent snake_case naming

### Performance

- Prefer vectorized operations and native pipes (`|>`) over loops
- Consider memory usage for large datasets
- Use `@inheritParams` to reduce documentation overhead

## Package Structure

```
R/                      # Source code
├── global_var.R        # Global variable system (always reference this)
├── quality_check_setup.R  # QC configuration
├── data_pipeline.R     # Core processing pipeline
├── qc*.R              # Quality control modules
├── cluster*.R         # Meal clustering functions
├── synch*.R           # Synchronicity analysis
└── utils.R            # General utilities

data/                   # Processed datasets (.rda)
data-raw/              # Dataset creation scripts
inst/extdata/          # Raw example files (CSV, Excel)
tests/testthat/        # Test files (test-*.R pattern)
man/                   # Auto-generated documentation (DO NOT EDIT)
```

## Dependencies

Minimize new dependencies. Current imports:
- `dbscan`, `dplyr`, `FNN`, `ggplot2`, `lubridate`, `mixtools`, `patchwork`, `rlang`, `tibble`, `tidyr`, `zoo`

Prefer `Imports` over `Depends`. Consider suggesting optional packages (`Suggests`) rather than requiring them.

## Development Workflow

When adding new functions:
1. Add to appropriate file in `R/` (create new file if needed)
2. Use global variables from `global_var.R` with defaults
3. Write roxygen2 documentation
4. Write comprehensive tests in `tests/testthat/`
5. Run `load_all()` then `test()` to verify
6. Run `document()` to update `man/` and `NAMESPACE`
7. Run `check()` to ensure CRAN compliance
8. List new exported functions in `.github/workflows/pkgdown.yaml`

## Key Reference Files

- `R/qc_total_cows.R` - Example of well-structured function following all conventions
- `R/global_var.R` - Global variable system implementation
- `R/quality_check_setup.R` - Configuration pattern
- `.cursor/rules/my-cursor-rules.mdc` - Detailed development rules
