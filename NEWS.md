
# extractox 0.2.0 (dev)

## Bug Fixes
* Fixed `extr_iris` extracts the correct number of chemicals with no repetition 
   (#15}.
* Fixed `extr_comtox` works when a single `download_items` different than 
   `DTXCID` is selected (#17).
* Fixed `extr_casrn_from_cid`: when no results function was faling.

## New Features
* `extr_ice_assay_names` retrieves ice assay names (#16).
* `extr_monograph` checks if a substance is listed in WHO IARC monograph and 
   returns the details of it (#19).
* `extr_pprtv` extracts information from EPA Provisional Peer-Reviewed Toxicity Values 
   database (#20)

## Other Breaking Changes
* The argument `cancer_types` in `extr_iris`  was removed. The database returns
   a dataframe with different columns depending on the request args, and implementing
   a way around that is much more tedious than just filter the resulting dataframe
   after. Plus it's unlikely that someone wouldn't want to know other non-cancer
   tox assessments type
*  `extr_casrn_from_cid` lost the argument `stop_at_warning`. To be consistent
   with other functions now it warns and returns data.frame with NA if no 
   ids are found.

## Enhancements and Fixes
* All the `extr_` functions have gained a `verbose` argument (#18).
* All the `extr_` functions now behave similarly when ids are not founds 
  (#30, #31, #32, #33, #34, #35), 
   
   - for all the functions but `extr_comptox`, a  column `query` reports the ids searched. 
     For `extr_comptox` that info  can be found in the the element `main_sheet` of
     the result. Note that for `extr_ice` given the nature of the request
     the `query` values contains all the ids found (not single id).
   - now the results  contains rows with NAs values for all columns except
     `query`

* Fixed `extr_comptox` now outputs a list of dataframes with "clean" names.
* Fixed `extr_ctd` names: column `pub_med_ids` or `pub_med_i_ds` are now `pubmed_ids`.

*Breaking change*: note that now `extr_tox` runs also `extr_monograph` and so output one more dataframe.

# extractox 0.1.0

* Initial CRAN submission.
