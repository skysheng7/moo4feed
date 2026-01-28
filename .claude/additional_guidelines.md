# Additional Claude Code Guidelines for moo4feed

This document supplements `.claude/claude_instructions.md` with additional context discovered through codebase analysis.

## Package Overview

**moo4feed** is an R package for processing animal feeding and drinking behavior data from electronic feeders/drinkers. The package converts raw visit logs into biologically meaningful metrics like meal patterns, intake rates, and synchronous behavior.

## Architecture Patterns

### 1. Global Variable System (`global_var.R`)

The package uses a sophisticated environment-based global variable system stored in `the` object:

* **Column name accessors**: `id_col2()`, `start_col2()`, `end_col2()`, `bin_col2()`, `duration_col2()`, `intake_col2()`, `start_weight_col2()`, `end_weight_col2()`, `trans_col2()`
* **Column name setters**: `set_id_col2()`, `set_start_col2()`, etc.
* **Configuration accessors**: `tz2()`, `bin_offset2()`, `bins_feed2()`, `bins_wat2()`, `bin_layout2()`
* **Batch setter**: `set_global_cols()` to set multiple variables at once

**Key patterns:**
* ALWAYS use these accessor functions instead of hard-coded column names
* Use `rlang::sym()` to convert column names to symbols for dplyr operations: `dplyr::filter(!!rlang::sym(id_col2()) == value)`
* When documenting parameters, use `@inheritParams` to avoid duplicating documentation

### 2. Configuration System (`quality_check_setup.R`)

Quality control functions use a centralized configuration pattern:

* **`qc_config()`**: Creates a named list with all QC thresholds
* Parameters include: `high_dur_feed`, `high_dur_water`, `large_intake_visit_feed`, `large_intake_visit_water`, `low_visit_threshold`, `total_cows_expected`, etc.
* Pass config objects to functions: `qc(data, cfg = qc_config(...))`
* Internal validation helpers: `assert_scalar_num()`, `assert_scalar_int()`, `assert_int_vec()`

**Pattern to follow:**
```r
my_function <- function(data, cfg = qc_config(), id_col = id_col2()) {
  # Use cfg$threshold_name to access thresholds
  # Use id_col for column name
}
```

### 3. Data Structure Conventions

* **List of dataframes by date**: Most functions operate on `list(date1 = df1, date2 = df2, ...)`
* **Names are date strings**: e.g., `"2020-10-31"`, `"2020-11-01"`
* **Standard columns**: All dataframes have consistent structure with columns accessed via global variable functions
* **Tibbles preferred**: Use `tibble::tibble()` for new dataframes

### 4. Function Organization Patterns

Functions are organized with clear section headers:

```r
# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Exported function
#' @export
public_function <- function() { }

# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Internal helper
#' @noRd
#' @keywords internal
internal_helper <- function() { }
```

### 5. Piping and Data Manipulation

* **Use native pipe**: `|>` (not magrittr `%>%`)
* **Vectorized operations**: Prefer vectorized operations over loops
* **dplyr standard practice**: Use dplyr verbs with unquoted column names or `!!rlang::sym()` for dynamic columns

### 6. Testing Conventions

* **Test file naming**: `test-function_name.R` (e.g., `test-qc_total_cows.R`)
* **Helper functions**: Create synthetic data helpers at top of test files (e.g., `make_warn_df()`, `make_comb_list()`)
* **Test structure**:
  ```r
  # Happy path tests
  test_that("function does X correctly", { ... })

  # Edge cases
  test_that("handles empty input", { ... })
  test_that("handles NA values", { ... })
  ```
* **Coverage goal**: >90% code coverage
* **Test execution**: MUST run `devtools::load_all()` before `testthat::test_file()` or `testthat::test_check()`

### 7. Documentation Patterns

**Exported functions must have:**
* Title (one-sentence summary)
* `@description` (optional, for more detail)
* `@param` for each parameter
* `@inheritParams` to reuse parameter docs
* `@return` describing what's returned
* `@examples` with working code (create toy datasets if needed)
* `@export` tag
* `@details` (optional, for algorithm explanation)
* `@seealso` (optional, to link related functions)

**Internal functions should have:**
* `@noRd` instead of `@export`
* `@keywords internal`
* Still document `@param` and `@return` for maintainability

**Cross-references:**
* Use `[function_name()]` for same package: `[qc_config()]`
* Use `[package::function()]` for other packages: `[dplyr::filter()]`

### 8. _pkgdown.yml Management

**CRITICAL**: Every time you create a new exported function (with `@export`), you MUST add it to `_pkgdown.yml` under the appropriate `reference:` section. Example:

```yaml
- title: Data Cleaning and Preparation
- subtitle: Data Quality Checks
  contents:
  - qc_config
  - qc
  - new_function_name  # ← ADD NEW FUNCTIONS HERE
```

### 9. Example Data

The package includes example datasets:
* `all_fed`: Raw feeding data (list of 2 days)
* `all_wat`: Raw water data (list of 2 days)
* `clean_feed`: QC'd feeding data
* `clean_water`: QC'd water data
* `clean_comb`: Combined cleaned data
* `moo4feed_example()`: Access raw files in `inst/extdata/`

Use these in examples and vignettes.

### 10. Vignette Style

* **Narrative approach**: Uses "Ollie the Otter" as narrator
* **Step-by-step**: Clear numbered sections
* **Code chunks**: Always show both code and output
* **Real data**: Uses package example data
* **Visual aids**: Includes plots and comics

### 11. Common Helper Utilities

From `utils.R`:
* `read_data_safely()`: Safely read CSV/DAT files, returns empty df on error
* `moo4feed_example()`: Access example data files

### 12. Quality Control Module Pattern

QC functions follow a consistent pattern:
1. Accept `comb` (data), `warn` (warnings df), `cfg` (config), and global var parameters
2. Iterate through each day: `for (i in seq_along(comb))`
3. Extract date and find index: `date <- names(comb)[i]; day_idx <- which(warn$date == date)`
4. Check for empty/missing: `if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) next`
5. Perform checks and update `warn` dataframe
6. Return updated `warn`

See `qc_total_cows.R` as reference implementation.

## Common Mistakes to Avoid

1. ❌ **Hard-coding column names** like `df$cow` → ✅ Use `df[[id_col2()]]` or `!!rlang::sym(id_col2())`
2. ❌ **Using `library()` or `require()`** → ✅ Use `package::function()` notation
3. ❌ **Creating functions without tests** → ✅ Always write comprehensive tests
4. ❌ **Forgetting to add new functions to `_pkgdown.yml`** → ✅ Update immediately after creating exported functions
5. ❌ **Using magrittr pipe `%>%`** → ✅ Use native pipe `|>`
6. ❌ **Writing "cow" in docs/comments** → ✅ Use "animal" for generalization
7. ❌ **Testing with `Rscript -e "testthat::test_file()"`** → ✅ Use `devtools::load_all()` first
8. ❌ **Using `ad_datetime()` without timezone** → ✅ Always pass `tz2()` parameter

## Additional Reminders

* **DRY principle**: Extract repeated code into small helper functions
* **No file I/O in functions**: Functions should not write files to disk
* **Never modify `man/` or `NAMESPACE` directly**: These are generated by roxygen2
* **lubridate for dates**: Use lubridate package for all datetime operations
* **Semantic versioning**: Follow semver principles in DESCRIPTION
* **Backward compatibility**: Maintain when possible
