## Test environments

* Local: Debian GNU/Linux 13 (trixie), R 4.5.0, x86_64-pc-linux-gnu
* Win-builder: R-devel (via devtools::check_win_devel())

## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE is:

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Claudio Zanettini <claudio.zanettini@gmail.com>'
    New submission
    Package was archived on CRAN
    Possibly misspelled words in DESCRIPTION:
      PubChem (14:5)
      Tox (2:16)
      Toxicogenomics (12:17)
    CRAN repository db overrides:
      X-CRAN-Comment: Archived on 2025-08-25 for policy violation.
      On Internet access.

This NOTE is expected for a resubmission of a previously archived package.
The "possibly misspelled words" are all legitimate domain-specific terms
(PubChem is a database name, Tox and Toxicogenomics are established
toxicology terminology).

## Response to previous archival (Internet access policy violation)

The package was archived on 2025-08-25 for "policy violation. On Internet access."
The following changes have been made to address this:

1. **All examples that make internet/API calls are wrapped in `\donttest{}`.**
   Previously, `extr_monograph()` had its `@examples` block wrapped in plain `{}`
   instead of `\donttest{}`. This has been corrected. All other exported functions
   already used `\donttest{}` correctly.

2. **All tests skip on CRAN** via `skip_on_cran()` and `skip_if_offline()` in
   every test file. No internet access is required during `R CMD check --as-cran`.

3. **`devtools::check()` with `--run-donttest`** completed successfully with
   0 errors, 0 warnings.

## revdepcheck results

There are currently no downstream dependencies for this package.
