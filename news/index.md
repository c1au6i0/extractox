# Changelog

## extractox 1.2.1

### Breaking Changes

- `extr_ctd` and `extr_tetramer` have been **removed** from the package.
  The CTD website now requires an ALTCHA CAPTCHA challenge that prevents
  automated API access. All related source, documentation, and tests
  have been deleted.

### New Features

- **Graceful exit on error**: All exported HTTP-facing functions
  (`extr_comptox`, `extr_ice`, `extr_iris`, `extr_pprtv`,
  `extr_casrn_from_cid`, `extr_chem_info`, `extr_pubchem_fema`,
  `extr_pubchem_ghs`) now return `NULL` with a
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
  message on failure instead of throwing an error. This is implemented
  via the new internal helper `with_graceful_exit()`.
- **Jittered exponential backoff**: All `httr2` requests now use
  `req_retry(max_tries = 5, backoff = httr2_backoff)` with a jittered
  exponential backoff strategy (capped at 30 s, ±20% jitter).
  `condathis`/curl code paths use an equivalent `condathis_run_retry()`
  helper.

### Bug Fixes

- Fixed
  [`condathis::run()`](https://luciorq.github.io/condathis/reference/run.html)
  calls in `extr_comptox_openssl_()` and `extr_iris_openssl_()`: errors
  are now caught via `tryCatch` and re-thrown with a clear message
  rather than causing an uncontrolled crash.
- Fixed `extr_chem_info`: now aborts with an informative message when
  PubChem returns an empty response, rather than failing silently
  downstream.
- Fixed the `httr2_backoff()` function signature to match the
  single-argument `backoff` callback contract required by
  [`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html).
- Restored the correct CompTox batch export endpoint URL in
  `extr_comptox_out()`.
- Fixed a typo (`s.` → `.`) in the `DESCRIPTION` field
  ([\#50](https://github.com/c1au6i0/extractox/issues/50)).
- Updated test expectations for `extr_ice` and `extr_casrn_from_cid` to
  match current upstream API response sizes.
- Updated `extr_comptox` tests to reflect a CompTox API change:
  `comptox_main_data` now returns 63 columns instead of 64.
- Updated `extr_casrn_from_cid` test to reflect current PubChem data
  (row count updated from 42 to 29).
- Fixed `extr_tox` test: replaced incorrect `expect_no_warning()` with
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html); the
  function intentionally warns via `with_graceful_exit()` when an
  individual sub-service (e.g. ICE) fails transiently. The data.frame
  check is now resilient to `NULL` elements from transiently unavailable
  services.
- Fixed `extr_monograph` test: corrected `expect()` call to
  `expect_equal()` when checking the count of missing values.
- Added missing `skip_on_cran()` and `skip_if_offline()` guards to
  `extr_monograph` tests that make live API calls.
- Renamed test file `test_monograph.R` to `test-monograph.R` to follow
  the `test-*.R` convention required for automatic discovery by
  testthat.

## extractox 1.2.0

CRAN release: 2025-07-15

### Major Changes & Enhancements

- Automated Server Compatibility: Functions connecting to EPA servers
  (e.g., `extr_comptox`, `extr_iris`, `extr_pprtv`) now automatically
  handle connection issues that can occur on systems with modern
  `libcurl` and `OpenSSL`. The functions detect problematic
  configurations and use the
  [condathis](https://github.com/luciorq/condathis) package to perform
  requests with a compatible version of `curl` in an isolated
  environment. This ensures reliability without requiring manual user
  intervention.

### Minor Enhancements

- `extr_chem_info` now accepts the argument `domain` to specify the
  PubChem domain (`substance` or `compound`).
- `extr_tox` now accepts a `delay` parameter to control the delay
  between requests, helping to avoid rate-limiting errors.
- Request Throttling\*\*: Added a `delay` parameter to PubChem functions
  to allow for a pause between requests, helping to avoid rate-limiting
  errors.
- Verbose Option: A `verbose` option has been added to several functions
  for more detailed output during execution.

### Bug Fixes

- Testing: Corrected various tests, including a fix for an incorrect row
  count expectation in the PubChem test.

- Fixed `extr_chem_info` duplicated name in outout columns
  ([\#47](https://github.com/c1au6i0/extractox/issues/47)).

- Function Cleanup: Refined the `extr_comptox` function, removing
  unnecessary requirements and cleaning up the code.

## extractox 1.0.0

CRAN release: 2025-01-07

### Bug Fixes

- Fixed `extr_iris` extracting the correct number of chemicals without
  repetition ([\#15](https://github.com/c1au6i0/extractox/issues/15)).
- Fixed `extr_comtox` working when a single `download_items` different
  from `DTXCID` is selected
  ([\#17](https://github.com/c1au6i0/extractox/issues/17)).
- Fixed `extr_casrn_from_cid` failure when no results are found.

### New Features

- Added `extr_ice_assay_names` to retrieve ICE assay names
  ([\#16](https://github.com/c1au6i0/extractox/issues/16)).
- Added `extr_monograph` to check if a substance is listed in WHO IARC
  monograph and return its details
  ([\#19](https://github.com/c1au6i0/extractox/issues/19)).
- Added `extr_pprtv` to extract information from the EPA Provisional
  Peer-Reviewed Toxicity Values database
  ([\#20](https://github.com/c1au6i0/extractox/issues/20)). Introduced
  `save_to_cache` and `load_from_cache` functions to avoid
  re-downloading the file each time. See `force` argument.

### Other Breaking Changes

- Removed `cancer_types` argument from `extr_iris`. Database returns a
  dataframe with different columns based on `request` arguments.
- Removed `stop_at_warning` argument from `extr_casrn_from_cid`. Now
  warns and returns a dataframe with NA if no IDs are found.
- `extr_tox` now returns a longer list of dataframes, including the
  outputs of `extr_monograph` and `extr_pprtv`.

### Enhancements and Fixes

- Added `verbose` argument to all `extr_` functions
  ([\#18](https://github.com/c1au6i0/extractox/issues/18)).
- Unified behavior across all `extr_` functions when chemicals are not
  found ([\#30](https://github.com/c1au6i0/extractox/issues/30)-#35):
  - For all functions except `extr_comptox`, a `query` column reports
    the IDs searched. In `extr_comptox`, this info is in the
    `main_sheet` element. For `extr_ice`, `query` values contain all IDs
    found.
  - Results now contain rows with NA values for all columns (except
    `query`).
  - `extr_pprtv` and `extr_monograph` use `save_and_match` to output
    results with NA for missing IDs.
- Improved and extended all unit tests.
- `extr_comptox` now outputs a list of dataframes with clean names.
- Fixed `extr_ctd` column names: `pub_med_ids` or `pub_med_i_ds` are now
  `pubmed_ids`.
- Introduced `extr_pubchem_section_` internal function to fetch FEMA and
  GHS info, avoiding repeated code.
- Introduced `check_na_warn` internal function to generate warnings for
  missing IDs.
- Created `with_extr_sandbox` to handle cache for CRAN examples.

## extractox 0.1.0

CRAN release: 2024-12-03

- Initial CRAN submission.
