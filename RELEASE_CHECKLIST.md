# Release Checklist for moo4feed v0.1.0

## Status: Ready for Release

All tests pass: ✅ (2615 tests, 0 failures)

## Pre-Release Checklist

### 1. Version and Documentation Updates ✅

* [x] Updated DESCRIPTION version from `0.0.0.9000` to `0.1.0`
* [x] Created NEWS.md with comprehensive release notes
* [ ] Review and finalize NEWS.md content
* [ ] Update README.md if needed (currently looks good)

### 2. Code Quality Checks

* [x] All tests pass (2615 tests, 0 failures)
* [ ] Run full R CMD check: `devtools::check()`
  * Should have 0 errors, 0 warnings, 0 notes
* [ ] Check test coverage: `covr::package_coverage()`
  * Target: >90% coverage
* [ ] Review linter output: `lintr::lint_package()`

### 3. Documentation

* [ ] Rebuild documentation: `devtools::document()`
* [ ] Build and preview pkgdown site: `pkgdown::build_site()`
* [ ] Verify all exported functions are documented
* [ ] Check that all functions in R/ are listed in _pkgdown.yml
* [ ] Build vignettes: `devtools::build_vignettes()`

### 4. Git and Version Control

* [ ] Commit all changes
* [ ] Review git diff to ensure no unintended changes
* [ ] Verify CI/CD workflows are passing on GitHub
  * R-CMD-check.yaml
  * test-coverage.yaml
  * pkgdown.yaml

## Release Process

### Step 1: Commit Changes

```bash
cd /Users/skysheng/Desktop/github/moo4feed
git add DESCRIPTION NEWS.md R/data_pipeline.R tests/testthat/test-data_pipeline.R
git commit -m "Prepare for v0.1.0 release

- Update version to 0.1.0
- Add comprehensive NEWS.md
- Finalize data_pipeline updates"
```

### Step 2: Run Final Checks

```bash
# In R console
devtools::check()
devtools::build_vignettes()
pkgdown::build_site()
```

### Step 3: Push to GitHub

```bash
git push origin main
```

Wait for all GitHub Actions to complete successfully before proceeding.

### Step 4: Create Git Tag

```bash
git tag -a v0.1.0 -m "Release version 0.1.0

First public release of moo4feed package.
See NEWS.md for full details."

git push origin v0.1.0
```

### Step 5: Create GitHub Release

1. Go to https://github.com/skysheng7/moo4feed/releases
2. Click "Draft a new release"
3. Choose tag: `v0.1.0`
4. Release title: `moo4feed v0.1.0 - Initial Release`
5. Description: Copy from NEWS.md (formatted for GitHub)
6. Check "Set as the latest release"
7. Click "Publish release"

### Step 6: Post-Release Tasks

* [ ] Verify release appears on GitHub
* [ ] Check that pkgdown site is updated
* [ ] Test installation from GitHub:
  ```r
  devtools::install_github("skysheng7/moo4feed@v0.1.0")
  ```
* [ ] Announce release (if applicable)

## Post-Release: Start Development Version

After the release is complete, bump the version to development:

```bash
# In R console
usethis::use_dev_version()
```

This will update DESCRIPTION to `0.1.0.9000` and add an entry to NEWS.md.

## GitHub Release Description Template

Use this template when creating the GitHub release:

---

# moo4feed v0.1.0 - Initial Release

This is the first public release of **moo4feed**, an R package for deriving individual animal traits from feeding and drinking data.

## 🎉 Highlights

* Complete data processing pipeline for feeding and drinking behavior data
* Comprehensive quality control and data cleaning tools
* Advanced behavioral analysis including meal clustering and synchronicity
* 8 detailed vignettes covering the complete workflow
* Full documentation site: https://skysheng7.github.io/moo4feed/

## 📦 Installation

```r
# Install from GitHub
devtools::install_github("skysheng7/moo4feed@v0.1.0")

# Install with vignettes
devtools::install_github("skysheng7/moo4feed@v0.1.0", 
                         dependencies = TRUE, 
                         build_vignettes = TRUE)
```

## 🚀 Features

### Data Processing
* Core functions: `read_data_safely()`, `process_all_feed()`, `process_all_water()`
* Quality checks: `qc_config()`, `qc()`, `knn_outlier_detection()`
* Data cleaning: `delete_rows()`, `keep_bins()`, `daylight_saving_adjust()`

### Behavioral Analysis
* Feed/water summary: `feed_water_summary()`
* Meal clustering: `cluster_meals()`, `meal_interval()`
* Non-nutritive visits: `calculate_non_nutritive_visits()`
* Replacement behavior: `record_replacement_days()`
* Synchronicity: `synch_pair_analysis()`, `synch_neighbor_analysis()`

### Configuration
* Flexible global variable system with getter/setter functions
* Customizable quality control thresholds

### Documentation
* 8 comprehensive vignettes
* Full function reference
* Example datasets included

## 📖 Documentation

* **Package website**: https://skysheng7.github.io/moo4feed/
* **Vignettes**: See articles section on the website
* **Issues**: https://github.com/skysheng7/moo4feed/issues

## 🙏 Acknowledgments

This package was developed at the University of British Columbia with funding from NSERC Discovery Grant (RGPIN-2021-02848).

See full details in [NEWS.md](https://github.com/skysheng7/moo4feed/blob/main/NEWS.md).

---

## Notes

* Current uncommitted changes:
  * `R/data_pipeline.R` - needs review and commit
  * `tests/testthat/test-data_pipeline.R` - needs review and commit
  * `DESCRIPTION` - version updated to 0.1.0
  * `NEWS.md` - newly created

* All CI/CD workflows are configured:
  * R CMD check
  * Test coverage (codecov)
  * pkgdown deployment

* The package has comprehensive test coverage with 2615 passing tests.
