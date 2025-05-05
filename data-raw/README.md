# 📂 `old_scripts/` – Legacy Insentec Analysis Code

This folder holds **two “generations” of code** I wrote while developing our Insentec‐HOBO data‑analysis workflow.\
Everything here is kept for **historical reference only**—new development should **not** modify these files directly.

| Generation | File(s) | Purpose | Status |
|---------------------|-----------------|-----------------|-----------------|
| **1** | `master_script_HOBO_Insentec_combined_Sockeye_v12.R` | \~5,000‑line monolith that performed the entire pipeline end‑to‑end. | *Frozen* – never touch except for archaeology. |
| **2** | `01‑helpers‑initial data process.R`<br>`02‑helpers‑Insentec warning.R`<br>`03‑helpers‑Insentec summary.R`<br>`04‑helpers‑synchronicity matrix.R`<br>`05‑helpers‑replacement.R`<br>`06‑helpers‑non‑nutritive.R`<br>`globals.R` | First refactor of Gen‑1: extracted \~30‑50 helper functions into seven scripts and wrapped them in an RStudio Project. **No unit tests** but every function has documentation. | *Source* for the forthcoming package. |

------------------------------------------------------------------------

## How collaborators should use this directory

1.  **Ignore Generation 1**
    *The giant master script is here only so we can trace old logic when necessary.*

2.  **Mine Generation 2 for functions**

    -   Your task is to **move/reshape** the useful functions into the new package’s `R/` directory.\
    -   Follow the convention for organizing functions described in [*R Packages* §6.1 “Organize functions into files”](https://r-pkgs.org/code.html).
        -   Aim for **2‑4 closely‑related functions per file**.\
        -   Exception: a single very large or self‑contained function can live in its own file.\
    -   Add **roxygen2 docstrings** and create minimal **unit tests** as you go.

3.  **Do not overwrite these originals**
    Commit new, cleaned‑up versions in the package repo instead.
    
4.  **Refactor freely and creatively**
    Feel free to rename functions, expand parameters, improve docs, or split big chunks into smaller, clearer helpers—whatever makes the codebase stronger.
