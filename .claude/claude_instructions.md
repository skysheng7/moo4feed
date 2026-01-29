You are an expert in R package development, with a focus on creating maintainable, well-documented, and user-friendly packages following the tidyverse principles and CRAN submission standards.

## Key Principles:
- Write clean, efficient R code that follows tidyverse style guidelines.
- Write DRY (Don't Repeat Yorself) code, write small functions for code that you keep using in multiple places.
- Create thorough documentation with roxygen2 for all exported functions.
- In documentation and comments, DO NOT say "cow", just say "animal" because I wish to generalize to data processing for all animals.
- Everytime you create a new function, always write tests covering normal use cases, edge cases, error handling; and make sure > 90% code coverage.
- Use global variables from @global_var.R (never hard-code column names)and configuration systems defined in @quality_check_setup.R.
- DO NOT write files to disk within functions.
- DO NOT modify `man/` or `NAMESPACE`.
- Use `lubridate` package to handle all date time related processing.
- If you ever use `ad_datetime()` function, you must pass in `tz2()` as a input parameter to set timezone.
- Running tests in R package development is different from regular test with R script. If you need to run any test to test your new funciton, you MUST run `load_all()` first then use `testthat::test()` to test all functions at once. DO NOT test one script at a time, DO NOT ever try to run `cd /Users/skysheng/Desktop/moo4feed && Rscript -e "testthat::test_file()`!
- **CRITICAL**: Every time you create a new public-facing function (one with `@export` tag), you MUST add it to [@_pkgdown.yml](mdc:_pkgdown.yml) under the appropriate section in the `reference:` list. This ensures the function appears in the package documentation website.
- Follow semantic versioning principles for package versioning.
- Maintain backward compatibility when possible.

## Markdown Formatting:
- Always use `*` for bullet points (not `-` or other markers)
- **CRITICAL**: Always include a blank line before starting a bullet point list to ensure proper rendering
- Include a blank line after bullet point lists before continuing with regular text

## Package Structure and Organization:
- `R/`: directory contains organized code, with one function (or related functions) per file.
- `data/`: Processed datasets accessible to users (.rda files)
- `data-raw/`: Scripts that create data in data/ (processing/cleaning code)
- `inst/extdata/`: Raw example files (CSV, Excel)
- `tests/testthat/`: Test files using test-function_name.R pattern
- Use consistent naming conventions for functions (snake_case) and classes.
- Keep functions focused on single tasks; complex operations should be broken down to multiple small internal help functions.

## Global Variables and Configuration System:
- Always use the global variable system defined in @global_var.R:
  - Access column names with functions like `id_col2()`, `start_col2()`, `end_col2()`, etc.
  - Use `tz2()` for timezone settings
  - Access bin configurations with `bins_feed2()`, `bins_wat2()`, `bin_offset2()`
  - Use `@inheritParams` to avoid documentation duplication
- Always use the configuration system from [quality_check_setup.R](mdc:R/quality_check_setup.R) : 
  - Create configurations with `qc_config()` for quality control functions
  - Pass configuration objects to functions that need thresholds or customzied settings

## Documentation Standards:

### For Exported (User-Facing) Functions:
- Begin with a one-sentence title that summarizes the function
- Include a thorough description paragraph (can use `@description` tag)
- Document each parameter with `@param name description`
- Use `@inheritParams function_name` to inherit parameter docs from elsewhere (inheritance is recursive)
- Include `@return` to clearly describe what the function returns
- Provide working examples in `@examples` blocks that run without errors. Always create toy dataset to demonstrate the example code.
- Include `@export` tag to make the function available to users
- (Optional): Use `@details` to write detailed logics of your functions
- (Optional): Use `@seealso` to link relevant functions
- Use `[package::function()]` when referring to specific functions for automatic URL linking

### For Internal Helper Functions:
- Use `@noRd` instead of `@export` to prevent generating man pages for internal functions
- Use `@keywords internal` for clarity
- Still document parameters and return values the same way as exported functions for maintainer clarity

## Error Handling and Validation:
- Validate function inputs early with clear error messages
- Implement informative warning and message functions
- Return consistent output structures (tibble/data.frame, lists, etc.)
- **Parameter Validation Pattern**: For parameters with predefined choices, always use `match.arg(trimws(tolower(parameter_name)))` to ensure case-insensitive, whitespace-tolerant validation. This makes the API more user-friendly by accepting inputs like "SEC", "sec", " Min ", etc.

## Performance Considerations:
- Use vectorized operations (native pipes: `|>` ) over loops when possible
- Consider memory usage for large datasets

Dependencies:
- Minimize dependencies to reduce maintenance burden
- Use Imports rather than Depends when possible
- Always specify minimum version requirements for critical dependencies
- Consider suggesting optional packages rather than requiring them

## Key Conventions:
1. **DO NOT use `library()` or `require()` in any code you write**
2. **DO NOT use `source()` in any code**
3. **DO NOT hard-code file paths**; use `system.file()` instead
11. Use [qc_total_cows.R](mdc:R/qc_total_cows.R) as an example code script

## Multi-Agent Workflow for New Features

When implementing new features in this R package, I have created 5 different agents sequentially to ensure comprehensive, high-quality implementation:

1. r-package-developer
2. r-test-writer
3. r-package-code-reviewer
4. testing-engineer
5. master-coordinator

When you are implementing a new feature, you should use the master-coordinator agent to orchestrate the workflow between the other 4 agents.

