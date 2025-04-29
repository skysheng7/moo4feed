# Contributing

Contributions are welcome, and they are greatly appreciated! Every little bit  
helps, and credit will always be given.

Please also read our [Code of Conduct](CODE_OF_CONDUCT.md) before contributing.

## Example Contributions

You can contribute in many ways, for example:

* [Report bugs](#report-bugs)  
* [Fix bugs](#fix-bugs)  
* [Implement features](#implement-features)  
* [Write documentation](#write-documentation)  
* [Submit feedback](#submit-feedback)

## Report bugs

Report bugs at <https://github.com/skysheng7/moo4feed/issues>.

**If you are reporting a bug, please follow the template guidelines. The more  
detailed your report, the easier and thus faster we can help you.**

## Fix bugs

Look through the GitHub issues for bugs. Anything labelled with `bug` and  
`help wanted` is open to whoever wants to implement it. When you decide to work  
on such an issue, please assign yourself to it and add a comment that you’ll be  
working on it. If you see another issue without the `help wanted` label, just  
post a comment—the maintainers are usually happy for any support they can get.

## Implement features

Look through the GitHub issues for features. Anything labelled with  
`enhancement` and `help wanted` is open to contribute. As for [fix bugs](#fix-bugs),  
please assign yourself and comment that you’ll be working on it. If another  
enhancement catches your fancy but lacks the `help wanted` label, just post a  
comment—the maintainers appreciate extra hands.

## Write documentation

This package uses **roxygen2** for documentation. You can help by writing or improving docs:

- Ensure all roxygen2 blocks use correct tags and syntax.
- **Examples** in `@examples` must run without errors. Test by running:
  ```r
  devtools::document()
  devtools::run_examples()
  ```
- Use `@inheritParams <function_name>` to inherit parameter docs for parameters documented elsewhere; inheritance is recursive.
- Mark internal helper functions with `@noRd` to suppress generation of man pages.
- After updating `.R` and `roxygen` comments, rebuild docs with:
  ```r
  devtools::document()
  ```
  and inspect the resulting files under man/.

## Submit feedback

The best way to send feedback is to file an issue at
<https://github.com/skysheng7/moo4feed/issues>. If your feedback fits an issue
template, please use it. Remember this is a volunteer-driven project—everyone
has limited time.

## Get started

Ready to contribute? Here’s how to set up moo4feed for local development.

1. Fork the <https://github.com/skysheng7/moo4feed>
   repository on GitHub.
   
2. Clone your fork locally

  ```shell
  git clone git@github.com:your_name_here/moo4feed.git
  ```

3. Create a branch for local development using the default branch (typically `main`)
   as a starting point. Use `fix` or `feat` as a prefix for your branch name.

  ```shell
  git checkout main
  git checkout -b <fix-name-of-your-bugfix>
  ```
  > 💡 Make sure <fix-name-of-your-bugfix> matches your branch name.

  Now you can make your changes locally.

   
4.	Install dependencies and load the package:
	
  ```r
  # Install any missing dependencies
  install.packages("remotes")
  remotes::install_deps(dependencies = TRUE)
  ```
  
  ```r
  # Load code without sourcing individual files
  devtools::load_all()
  ```

5.	When you need to add a new package dependency, use:

  ```r
  usethis::use_package("pkgName")
  ```

  **Never use `library()` calls** in R package development.

6.	If you make any changes to functions under `/R`, please run tests and checks package health frequently:

  ```r
  # if you only want to run all tests
  devtools::test()
  ```
  
  ```r
  # if you want to run entire R CMD check, this is what CRAN runs
  devtools::check() 
  ```
  
  Call these functions regularly to catch errors and warnings early.
  
  > ⚠️ **Important:** Please **ALWAYS** run `check()` and make sure there is 0 error, 0 warning and 0 note, before you make a commit or submit a pull request!

7. Commit your changes and push your branch to GitHub. Please use [semantic commit messages](https://www.conventionalcommits.org/).

    ```shell
    git add .
    git commit -m "fix: summarize your changes"
    git push -u origin <fix-name-of-your-bugfix>
    ```
    
  > 💡 Make sure <fix-name-of-your-bugfix> matches your branch name.

8. Open the link displayed in the terminal after pushing your branch

  This link will direct you to GitHub where you can submit a Pull Request (PR) for review.
  

## Local development tips
- Use `devtools::load_all()` instead of `source()` so that namespace wiring, NAMESPACE exports, and dependencies are handled correctly.
- Add packages via `usethis::use_package()` (which updates DESCRIPTION) rather than `library()` in your code.
- Run `devtools::check()` regularly to monitor package health (errors, warnings, notes).
- Build the pkgdown site (if applicable) with:
  ```r
  pkgdown::build_site()
  ```

## Pull request guidelines

Before you submit a pull request, please ensure:

	1.	You’ve written or updated tests under `tests/` and all tests pass with `devtools::test()`.
	
	2.	Documentation is updated: `roxygen2` blocks generate the correct `.Rd` files, and examples run without error.
	
	3.	You’ve run `devtools::check()` and addressed any errors, warnings, or notes.
	
	4.	Commit messages follow Conventional Commits (e.g. fix: …, feat: …).
	
	5.	Your branch is up to date with main and can be merged cleanly.

Thank you for helping make moo4feed better!

