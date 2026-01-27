# Contributing to moo4feed

Contributions are welcome, and they are greatly appreciated! Every
little bit helps, and credit will always be given.

Please also read our [Code of
Conduct](https://skysheng7.github.io/moo4feed/CODE_OF_CONDUCT.md) before
contributing.

## Table of Contents

- [Ways to Contribute](#ways-to-contribute)
- [For Internal Collaborators](#for-internal-collaborators)
- [For External Contributors](#for-external-contributors)
- [Common Development Guidelines](#common-development-guidelines)
- [Documentation Guidelines](#documentation-guidelines)
- [Writing Tests](#writing-tests)
- [Pull Request Guidelines](#pull-request-guidelines)

## Ways to Contribute

You can contribute in many ways:

- [Report bugs](#report-bugs)  
- [Fix bugs](#fix-bugs)  
- [Implement features](#implement-features)  
- [Write documentation](#documentation-guidelines)  
- [Write tests](#writing-tests)
- [Submit feedback](#submit-feedback)

### Report bugs

Report bugs at <https://github.com/skysheng7/moo4feed/issues>.

If you are reporting a bug, please follow the template guidelines. The
more detailed your report, the easier and thus faster we can help you.

### Fix bugs

Look through the GitHub issues for bugs. Anything labelled with `bug`
and `help wanted` is open to whoever wants to implement it. When you
decide to work on such an issue, please assign yourself to it and add a
comment that you’ll be working on it. If you see another issue without
the `help wanted` label, just post a comment—the maintainers are usually
happy for any support they can get.

### Implement features

Look through the GitHub issues for features. Anything labelled with
`enhancement` and `help wanted` is open to contribute. As for [fix
bugs](#fix-bugs), please assign yourself and comment that you’ll be
working on it. If another enhancement catches your fancy but lacks the
`help wanted` label, just post a comment—the maintainers appreciate
extra hands.

### Submit feedback

The best way to send feedback is to file an issue at
<https://github.com/skysheng7/moo4feed/issues>. If your feedback fits an
issue template, please use it. Remember this is a volunteer-driven
project—everyone has limited time.

------------------------------------------------------------------------

# For Internal Collaborators

If you’re already added as a collaborator to the GitHub repository:

## Getting Started

1.  Clone the repository directly:

    ``` shell
    git clone https://github.com/skysheng7/moo4feed.git
    ```

2.  Create a new branch for local development:

    Move to `main` branch if you are not there already:

    ``` shell
    git checkout main
    ```

    Create a new branch with a descriptive name using `fix-` or `feat-`
    prefix:

    ``` shell
    git checkout -b fix-name-of-your-bugfix
    ```

    > 💡 Make sure `<fix-name-of-your-bugfix>` matches your branch name.

    Now your terminal should end with your new branch name
    `(fix-name-of-your-bugfix)`.

3.  Double click on `moo4feed` R project object in the root directory on
    your local computer.

    This will open a new R studio window showing the R package. There
    should be a `Git` panel on the top right side. You can double check
    after you click on `Git`, there should be a drop down button named
    after your new branch name `(fix-name-of-your-bugfix)`.

4.  Make your changes and follow the [common development
    guidelines](#common-development-guidelines).

    If you want to create new functions please follow the guidelines in
    [Writing Tests](#writing-tests).

5.  Commit and push your changes regularly to your current branch:

    ``` shell
    git add .
    git commit -m "fix: summarize your changes"
    git push -u origin <fix-name-of-your-bugfix>
    ```

    - Try to make small, focused commits (one logical change per commit)
    - Use [semantic commit
      messages](https://www.conventionalcommits.org/)
    - the `git push -u origin <fix-name-of-your-bugfix>` only needs to
      be run once. `-u` will tells Git to remember the remote branch so
      you can just run `git push` or `git pull` next time (instead of
      specifying the branch again). `origin` refers to your remote
      repository.

6.  Style your code using tidyverse style

    If you have created/modified 1 or a few new R scripts, run:

    ``` r
    styler::style_file("path/to/your/r/scripts")
    ```

    If you wish to style an entire directory, run:

    ``` r
    styler::style_dir("path/to/your/folder")
    ```

    Remember to commit and push your changes.

7.  Open a Pull Request

    > ⚠️ **Heads‑up:** If you generated the site locally with
    > [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html),  
    > delete the resulting `pkgdown/` folder **before** opening a PR to
    > `main`.

    - Go to the repository on GitHub
    - Navigate to your branch
    - Click “Open Pull Request”, or “Compare & Pull Request” banner
    - Fill out the PR description
    - Assign reviewers if needed
    - 3 Github actions will be triggered to \[1\] check if your new
      changes passes R CMD checks, \[2\] pkgdown website can be built
      and \[3\] calculate code coverage.

    > 🚀 Only open a PR when your branch passes all tests and checks
    > without errors/warnings/notes.

------------------------------------------------------------------------

# For External Contributors

If you do **not** have direct push access to the repo:

1.  **Fork** the repository → <https://github.com/skysheng7/moo4feed>

2.  **Clone** your fork:

    ``` bash
    git clone git@github.com:your‑username/moo4feed.git
    cd moo4feed
    ```

3.  (Optional) Create a feature branch:

    ``` bash
    git checkout -b feat‑cool‑thing
    ```

4.  Now follow Steps 3–7 in the [Internal Collaborators
    workflow](#for-internal-collaborators)

    open the R Project, develop, style, test, etc.

5.  Push your changes to your forked repository or new branch → open a
    PR from your fork back to `skysheng7/moo4feed`:

    - click “Compare & pull request” on GitHub and fill in the details.

    > 🚀 Open the PR only after all checks pass with no
    > errors/warnings/notes.

------------------------------------------------------------------------

# Common Development Guidelines

These guidelines apply to both internal and external contributors:

## Development Setup

1.  Install dependencies and load the package:

    ``` r
    # Install any missing dependencies
    install.packages("remotes")
    remotes::install_deps(dependencies = TRUE)
    ```

    ``` r
    # Load code without sourcing individual R files and functions
    devtools::load_all()
    ```

    > ⚠️ **Never use [`source()`](https://rdrr.io/r/base/source.html)
    > calls** to run your R scripts in R package development.

2.  When adding a new package dependency:

    ``` r
    usethis::use_package("pkgName")
    ```

    > ⚠️ **Never use [`library()`](https://rdrr.io/r/base/library.html)
    > calls** in R package development.

3.  Run tests and checks regularly:

    ``` r
    # Run all tests
    devtools::test()

    # Run full R CMD check (what CRAN uses)
    devtools::check() 
    ```

    > ⚠️ **Important:** Always run `check()` and make sure there are 0
    > errors, 0 warnings and 0 notes before making a commit or
    > submitting a PR!

## Local Development Tips

- Use `devtools::load_all()` instead of
  [`source()`](https://rdrr.io/r/base/source.html) for proper namespace
  handling

- Add packages via
  [`usethis::use_package()`](https://usethis.r-lib.org/reference/use_package.html)
  rather than [`library()`](https://rdrr.io/r/base/library.html) calls

- Run `devtools::check()` regularly to catch issues early

- Avoid hard-coding file paths—the directory structure shifts as the
  package moves from source to bundle to build. Instead, obtain paths
  programmatically with
  `system.file("folder-name", package = "moo4feed")`.

- Build the pkgdown site (if applicable) with:

  ``` r
  pkgdown::build_site()
  ```

  > ⚠️ **Note:** If you built the **pkgdown** site locally using the
  > code above,  
  > please delete the `pkgdown/` directory before submitting a pull
  > request to the `main` branch.
  >
  > The `main` branch of the `moo4feed` repository uses a GitHub Action
  > to automatically rebuild the site.  
  > Therefore, the `pkgdown/` directory should not be committed — the
  > site will be regenerated automatically on another branch `gh-pages`.

------------------------------------------------------------------------

# Documentation Guidelines

This package uses **roxygen2** for documentation. You can help by
writing or improving docs:

## Function Documentation Standards

**For Exported (User-Facing) Functions:**

- Begin with a one-sentence title that summarizes the function
- Include a thorough description paragraph (can use `@description` tag)
- Document each parameter with `@param name description`
- Use `@inheritParams function_name` to inherit parameter docs from
  elsewhere (inheritance is recursive)
- Include `@return` to clearly describe what the function returns
- Provide working examples in `@examples` blocks
- Include `@export` tag to make the function available to users
- (Optional): Use `@details` to write detailed logics of your functions
- (Optionall): use `@seealso` to link relevant functions
- Use `[package::function()]` when you refer to a specific function, the
  link to this function’s URL will be automatically inserted when we
  build the website for our package.

**For Internal Helper Functions:**

- Use `@noRd` instead of `@export` to prevent generating man pages for
  internal functions
- Still document parameters and return values the same way as instructed
  above for maintainer clarity

## Testing Documentation

- All `@examples` must run without errors. Test by running:

  ``` r
  devtools::document()
  devtools::run_examples()
  ```

## Building Documentation

- After updating `.R` files and roxygen comments, rebuild documentation:

  ``` r
  devtools::document()
  ```

- Inspect the generated files in the `man/` directory to ensure quality

- You can also inspect it on the website by re-building the pkgdown site
  with:

  ``` r
  pkgdown::build_site()
  ```

------------------------------------------------------------------------

# Writing Tests

Our functions can always benefit from having more tests!

We use `testthat` R package for testing. Follow these steps:

1.  Find existing tests in the `tests/testthat` directory

2.  When creating a new function:

    ``` r
    # Create new R script for your function
    usethis::use_r("your_new_function_name")

    # Create associated test file
    usethis::use_test("your_new_function_name")
    ```

    This creates:

    - `R/your_new_function_name.R` for the function
    - `tests/testthat/test-your_new_function_name.R` for tests

3.  For each function, write at least 3 tests covering:

    - Normal use cases
    - Edge cases
    - Error handling

4.  Run the entire test suite:

    ``` r
    devtools::test()
    ```

5.  Check test coverage:

    ``` r
    covr::package_coverage()
    ```

    We aim for 80-95% code coverage.

------------------------------------------------------------------------

# Pull Request Guidelines

Before submitting a pull request, please ensure:

1.  You’ve written or updated tests and all tests pass with
    `devtools::test()`

2.  Documentation is updated with correct roxygen2 blocks and examples
    run without error

3.  You’ve run `devtools::check()` and addressed any errors, warnings,
    or notes

4.  Commit messages follow Conventional Commits format (e.g.,
    `fix: ...`, `feat: ...`)

5.  Your branch is up to date with main and can be merged cleanly

Thank you for helping make moo4feed better!
