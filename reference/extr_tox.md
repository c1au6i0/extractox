# Extract Toxicological Information from Multiple Databases

This wrapper function retrieves toxicological information for specified
chemicals by calling several external functions to query multiple
databases, including PubChem, the Integrated Chemical Environment (ICE),
CompTox Chemicals Dashboard, and the Integrated Risk Information System
(IRIS) and other.

## Usage

``` r
extr_tox(casrn, verbose = TRUE, force = TRUE, delay = 2)
```

## Arguments

- casrn:

  A character vector of CAS Registry Numbers (CASRN) representing the
  chemicals of interest.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- force:

  Logical indicating whether to force a fresh download of the EPA PPRTV
  database. Default is TRUE.

- delay:

  Numeric value indicating the delay in seconds between requests to
  avoid overwhelming the server. Default is 3 seconds.

## Value

A list of data frames containing toxicological information retrieved
from each database:

- who_iarc_monographs:

  Lists if any, the WHO IARC monographs related to that chemical.

- pprtv:

  Risk assessment data from the EPA PPRTV

- ghs_dat:

  Toxicity data from PubChem's Globally Harmonized System (GHS)
  classification.

- ice_dat:

  Assay data from the Integrated Chemical Environment (ICE) database.

- iris:

  Risk assessment data from the IRIS database.

- comptox_list:

  List of dataframe with toxicity information from the CompTox Chemicals
  Dashboard.

## Details

Specifically, this function:

- Calls
  [`extr_monograph`](https://c1au6i0.github.io/extractox/reference/extr_monograph.md)
  to return monographs informations from WHO IARC.

- Calls
  [`extr_pubchem_ghs`](https://c1au6i0.github.io/extractox/reference/extr_pubchem_ghs.md)
  to retrieve GHS classification data from PubChem.

- Calls
  [`extr_ice`](https://c1au6i0.github.io/extractox/reference/extr_ice.md)
  to gather assay data from the ICE database.

- Calls
  [`extr_iris`](https://c1au6i0.github.io/extractox/reference/extr_iris.md)
  to retrieve risk assessment information from the IRIS database.

- Calls
  [`extr_comptox`](https://c1au6i0.github.io/extractox/reference/extr_comptox.md)
  to retrieve data from the CompTox Chemicals Dashboard.

## Examples

``` r
# \donttest{
condathis::with_sandbox_dir({ # this is to write on tempdir as for CRAN policies # nolint
  Sys.sleep(4) # To avoid overwhelming the server
  extr_tox(casrn = c("100-00-5", "107-02-8"), delay = 4)
})
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying 100-00-5. 
#> OK (HTTP 200).
#> 
#> Querying 7474. 
#> OK (HTTP 200).
#> 
#> Querying 107-02-8. 
#> OK (HTTP 200).
#> 
#> Querying 7847. 
#> OK (HTTP 200).
#> 
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CompTox...
#> ℹ Getting info from CompTox...
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to ICE database...
#> ℹ Request succeeded with status code: 200
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Quering 100-00-5 to EPA IRIS database...
#> ℹ Quering 107-02-8 to EPA IRIS database...
#> Warning: Chemical 100-00-5 not found!
#> ℹ Extracting WHO IARC monographs...
#> Last updated: 2024-11-29 5:08pm (CET)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Downloading data from <https://cfpub.epa.gov/ncea/pprtv/atoz.cfm>.
#> ℹ Saving data in the cache /tmp/RtmpLe08kZ/tmp-cache1bc145bb8beb/R/extractox/epa_pprtvs.rds.
#> ℹ Extracting EPA PPRTVs.
# }
```
