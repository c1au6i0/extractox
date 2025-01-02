# extractox 1.0.0

## Bug Fixes
* Fixed `extr_iris` extracting the correct number of chemicals without 
  repetition (#15).
* Fixed `extr_comtox` working when a single `download_items` different from 
  `DTXCID` is selected (#17).
* Fixed `extr_casrn_from_cid` failure when no results are found.

## New Features
* Added `extr_ice_assay_names` to retrieve ICE assay names (#16).
* Added `extr_monograph` to check if a substance is listed in WHO IARC 
  monograph and return its details (#19).
* Added `extr_pprtv` to extract information from the EPA Provisional 
  Peer-Reviewed Toxicity Values database (#20). Introduced `save_to_cache` 
  and `load_from_cache` functions to avoid re-downloading the file each time. 
  See `force` argument.

## Other Breaking Changes
* Removed `cancer_types` argument from `extr_iris`. Database returns a 
  dataframe with different columns based on `request` arguments.
* Removed `stop_at_warning` argument from `extr_casrn_from_cid`. Now warns 
  and returns a dataframe with NA if no IDs are found.
* `extr_tox` now returns a longer list of dataframes, including the outputs 
  of `extr_monograph` and `extr_pprtv`.

## Enhancements and Fixes
* Added `verbose` argument to all `extr_` functions (#18).
* Unified behavior across all `extr_` functions when chemicals are not found 
  (#30-#35):
   - For all functions except `extr_comptox`, a `query` column reports the IDs 
     searched. In `extr_comptox`, this info is in the `main_sheet` element. For 
     `extr_ice`, `query` values contain all IDs found.
   - Results now contain rows with NA values for all columns (except `query`).
   - `extr_pprtv` and `extr_monograph` use `save_and_match` to output results 
     with NA for missing IDs.
* Improved and extended all unit tests.
* `extr_comptox` now outputs a list of dataframes with clean names.
* Fixed `extr_ctd` column names: `pub_med_ids` or `pub_med_i_ds` are now 
  `pubmed_ids`.
* Introduced `extr_pubchem_section_` internal function to fetch FEMA and GHS 
  info, avoiding repeated code.
* Introduced `check_na_warn` internal function to generate warnings for 
  missing IDs.
* Created `with_extr_sandbox` to handle cache for CRAN examples.


# extractox 0.1.0

* Initial CRAN submission.
