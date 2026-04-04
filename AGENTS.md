# Agent Development Guidelines for extractox

This document provides essential guidelines for AI coding agents working on the `extractox` R package.

## Project Overview

`extractox` is a CRAN-published R package (v1.2.0) that extracts toxicological and chemical information from various databases (CompTox, ICE, IRIS, PubChem, etc.). It uses `httr2` for HTTP requests with retry logic and handles EPA server SSL compatibility issues via `condathis`.

**Key Dependencies:** cli, condathis, curl, fs, httr2, janitor, pingr, readxl, rlang, rvest, webchem, withr  
**Test Framework:** testthat 3.x  
**R Version:** >= 4.1  
**Documentation:** roxygen2 (7.3.2), pkgdown

---

## Build, Test, and Lint Commands

### Pre-commit Workflow
Use `just pcommit` to run the complete pre-commit checklist:
- Formats code with styler
- Cleans DESCRIPTION
- Updates documentation (roxygen2)
- Runs examples
- Renders README.Rmd
- Runs R CMD check
- Builds pkgdown site

### Individual Commands

**Load package (interactive development):**
```r
devtools::load_all()
```

**Run all tests:**
```r
devtools::test()
# OR
testthat::test_local()
```

**Run a single test file:**
```r
testthat::test_file("tests/testthat/test-comptox.R")
```

**Run specific test by name:**
```r
testthat::test_file("tests/testthat/test-comptox.R", filter = "Valid chemical name")
```

**Code formatting:**
```r
styler::style_pkg()
# OR for single file:
styler::style_file("R/extr_comptox.R")
```

**Check package (R CMD check):**
```r
devtools::check()
# With manual and remote checks:
devtools::check(manual = TRUE, remote = TRUE)
```

**Build documentation:**
```r
devtools::document()
```

**Build pkgdown site:**
```r
pkgdown::build_site()
```

**Render README:**
```r
rmarkdown::render("README.Rmd", output_format = "md_document")
```

---

## Code Style Guidelines

### General Style
- **Indentation:** 2 spaces (NO tabs)
- **Line endings:** Auto-append newline at EOF
- **Trailing whitespace:** Strip from all lines
- **Style guide:** Tidyverse style (enforced by styler)
- **Encoding:** UTF-8

### Imports and Dependencies
- Use `::` notation for all external package calls (e.g., `httr2::request()`, `cli::cli_warn()`)
- Never use `library()` or `require()` in package code (only in tests)
- All imports must be declared in DESCRIPTION
- Use `requireNamespace()` for conditional package checks (see `write_dataframes_to_excel()` for example)

### Roxygen Documentation
- Use roxygen2 markdown: `Roxygen: list(markdown = TRUE)` in DESCRIPTION
- Document all exported functions with:
  - `@title` or first line as title
  - `@description` or paragraph after title
  - `@param` for each parameter
  - `@return` describing return value
  - `@export` for exported functions
  - `@examples` with `\donttest{}` wrapper for API calls
- Internal functions: use `@keywords internal` and `@noRd`
- Use `# nolint` to suppress linter warnings on specific lines when necessary

### Naming Conventions
- **Functions:** Snake_case with descriptive prefixes
  - Exported extraction functions: `extr_*()` (e.g., `extr_comptox()`, `extr_iris()`)
  - Check/validation functions: `check_*()` (e.g., `check_status_code()`, `check_internet()`)
  - Utility helpers: descriptive names (e.g., `search_and_match()`, `download_db()`)
- **Variables:** Snake_case (e.g., `status_code`, `ids_not_found`, `http_date`)
- **Constants:** ALL_CAPS_SNAKE_CASE (if needed)
- **Column names:** Always use `janitor::clean_names()` to standardize column names from external sources

### Function Design
- **Verbose parameter:** All main exported functions should support `verbose = TRUE` parameter for user feedback
- **Error handling:** Use `with_graceful_exit()` wrapper for exported functions to return NULL on errors instead of throwing
- **Return values:** Use `invisible()` for functions called for side effects
- Use named parameters for clarity
- Keep functions focused on single responsibilities

### Error Handling and Messaging
- **Errors:** Use `cli::cli_abort()` for fatal errors
- **Warnings:** Use `cli::cli_warn()` for non-fatal issues
- **Info messages:** Use `cli::cli_alert_info()` for status updates (when `verbose = TRUE`)
- **Error wrapping:** Use `tryCatch()` with specific error handlers
- **Graceful degradation:** Use `with_graceful_exit(.f, ..., what = "description")` pattern for exported wrappers
- **Conditional messages:** Gate verbose messages with `if (isTRUE(verbose))`
- **CLI syntax:** Use cli's inline markup (e.g., `{.field {ids_not_found}}`, `{.url {url}}`, `{.pkg openxlsx}`)

### HTTP Requests (httr2)
- Always use retry logic: `httr2::req_retry(max_tries = 5, backoff = httr2_backoff)`
- Check status codes: use `check_status_code(resp, verbose)` helper
- For EPA servers: Use `check_need_libcurl_condathis()` and conditionally use `condathis_run_retry()` with curl
- HTTP date extraction: Use `httr2::resp_date(req)` when available
- Use `httr2::req_url_query()` for URL parameters
- Handle SSL verification via `set_ssl()` helper when needed

### Data Processing
- Always use `janitor::clean_names()` on data frames from external sources
- Add `date_downloaded` column to track when data was retrieved
- Use `query` column to track original search terms
- Handle missing data by creating NA rows for unmatched queries (see `search_and_match()`)
- Validate inputs early in functions

### Testing Conventions
- Place `library(testthat)` at the top of test files
- Use `Sys.sleep(5)` between test blocks to avoid rate limiting
- Always include `skip_on_cran()` for tests that make API calls or require network
- Use `skip_if_offline()` for network-dependent tests
- Wrap first test attempt in `suppressWarnings()` to warm up caching
- Validate data structure: `expect_true(is.list(out))`, `expect_true(all(unlist(lapply(out, is.data.frame))))`
- Check column names: `expect_equal(names(out), col_names)`
- Test both success and failure cases
- Use `setup.R` in tests/testthat/ for shared test setup (uses `withr` for temp directories)

### Caching
- Use `tools::R_user_dir()` for persistent cache directory
- Cache functions are in `R/cache.R`
- Respect user's cache settings

### Type Safety
- Use type checking in validation functions
- Use `is.*()` functions for type checks (e.g., `is.list()`, `is.data.frame()`, `is.character()`)
- Use `isTRUE()` and `isFALSE()` for safe logical checks
- Check for NULL: use `is.null()` explicitly

---

## Special Considerations

### OpenSSL/libcurl Compatibility
Modern libcurl (>= 7.78.0) with OpenSSL causes issues with EPA servers. The package:
- Detects problematic configurations with `check_need_libcurl_condathis()`
- Uses `condathis` to create isolated environment with compatible curl (7.78.0)
- Automatically switches to `condathis_run_retry()` for EPA API calls when needed

### API Rate Limiting
- Include reasonable delays between API calls (see `Sys.sleep()` in tests)
- Use exponential backoff with jitter (see `httr2_backoff()` and `condathis_run_retry()`)

---

## Files to Never Modify Directly
- `man/*.Rd` - Generated by roxygen2
- `NAMESPACE` - Generated by roxygen2
- `README.md` - Generated from README.Rmd
- `docs/*` - Generated by pkgdown

---

## Git Workflow
- Never run git commands with `-i` flag (not supported)
- CI runs on push to main/master/dev branches
- CI skips tests and examples (`--no-tests`, `--no-examples`)
- pkgdown site auto-deploys to gh-pages branch

---

## Additional Resources
- Package website: https://c1au6i0.github.io/extractox/
- GitHub repo: https://github.com/c1au6i0/extractox
- CRAN page: https://cran.r-project.org/package=extractox
