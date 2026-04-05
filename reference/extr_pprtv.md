# Extract Data from EPA PPRTVs

Extracts data for specified identifiers (CASRN or chemical names) from
the EPA's Provisional Peer-Reviewed Toxicity Values (PPRTVs) database.
The function retrieves and processes data, with options to use cached
files or force a fresh download.

## Usage

``` r
extr_pprtv(
  ids,
  search_type = "casrn",
  verbose = TRUE,
  force = TRUE,
  get_all = FALSE
)
```

## Arguments

- ids:

  Character vector of identifiers to search (e.g., CASRN or chemical
  names).

- search_type:

  Character string specifying the type of identifier: "casrn" or "name".
  Default is "casrn". If `search_type` is "name", the function performs
  a partial match search for the chemical name. NOTE: Since partial
  mached is use, multiple seraches might match the same chemical,
  therefore chemical ids might not be uniques.

- verbose:

  Logical indicating whether to display progress messages. Default is
  TRUE.

- force:

  Logical indicating whether to force a fresh download of the database.
  Default is TRUE.

- get_all:

  Logical. If TRUE ignore all the other ignore `ids`, `search_type`, set
  `force = TRUE` and get the all dataset. This is was introduced for
  debugging purposes.

## Value

A data frame with extracted information matching the specified
identifiers, or NULL if no matches are found.

## See also

[EPA
PPRTVs](https://www.epa.gov/pprtv/provisional-peer-reviewed-toxicity-values-pprtvs-assessments)
\# nolint

## Examples

``` r
# \donttest{
condathis::with_sandbox_dir({ # this is to write on tempdir as for CRAN policies # nolint

  # Extract data for a specific CASRN
  Sys.sleep(4) # Sleep to avoid overwhelming the server
  extr_pprtv(ids = "107-02-8", search_type = "casrn", verbose = TRUE)

  Sys.sleep(4) # Sleep to avoid overwhelming the server
  # Extract data for a chemical name
  out <- extr_pprtv(
    ids = "Acrolein", search_type = "name", verbose = TRUE,
    force = TRUE
  )
  print(out)

  Sys.sleep(3) # Sleep to avoid overwhelming the server
  # Extract data for multiple identifiers
  out2 <- extr_pprtv(
    ids = c("107-02-8", "79-10-7", "42576-02-3"),
    search_type = "casrn",
    verbose = TRUE,
    force = TRUE
  )
  print(out2)
})
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Downloading data from <https://cfpub.epa.gov/ncea/pprtv/atoz.cfm>.
#> ℹ Saving data in the cache /tmp/RtmputFl4Z/tmp-cache1e2d1310b5aa/R/extractox/epa_pprtvs.rds.
#> ℹ Extracting EPA PPRTVs.
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Downloading data from <https://cfpub.epa.gov/ncea/pprtv/atoz.cfm>.
#> ℹ Overwriting cache.
#> ℹ Extracting EPA PPRTVs.
#>   pprtv_substance_id chemical    casrn last_revision
#> 1               1555 Acrolein 107-02-8          2002
#>                                          pprtv_assessment
#> 1 https://cfpub.epa.gov/ncea/pprtv/documents/Acrolein.pdf
#>                                                                 iris_link
#> 1 https://cfpub.epa.gov/ncea/iris2/chemicalLanding.cfm?substance_nmbr=364
#>   rf_c_value rf_d_value           woe               date_downloaded    query
#> 1   See IRIS   See IRIS Not available Sun, 05 Apr 2026 17:12:38 GMT Acrolein
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Downloading data from <https://cfpub.epa.gov/ncea/pprtv/atoz.cfm>.
#> ℹ Overwriting cache.
#> ℹ Extracting EPA PPRTVs.
#>   pprtv_substance_id     chemical      casrn last_revision
#> 1               1555     Acrolein   107-02-8          2002
#> 2               1594      Bifenox 42576-02-3          2006
#> 3               1556 Acrylic Acid    79-10-7          2010
#>                                             pprtv_assessment
#> 1    https://cfpub.epa.gov/ncea/pprtv/documents/Acrolein.pdf
#> 2     https://cfpub.epa.gov/ncea/pprtv/documents/Bifenox.pdf
#> 3 https://cfpub.epa.gov/ncea/pprtv/documents/AcrylicAcid.pdf
#>                                                                 iris_link
#> 1 https://cfpub.epa.gov/ncea/iris2/chemicalLanding.cfm?substance_nmbr=364
#> 2                                                           Not available
#> 3   https://cfpub.epa.gov/ncea/iris2/chemicalLanding.cfm?substance_nmbr=2
#>      rf_c_value         rf_d_value
#> 1      See IRIS           See IRIS
#> 2 Not available 9 x 10-3 mg/kg-day
#> 3      See IRIS           See IRIS
#>                                                       woe
#> 1                                           Not available
#> 2 Inadequate information to assess carcinogenic potential
#> 3 Inadequate information to assess carcinogenic potential
#>                 date_downloaded      query
#> 1 Sun, 05 Apr 2026 17:12:44 GMT   107-02-8
#> 2 Sun, 05 Apr 2026 17:12:44 GMT 42576-02-3
#> 3 Sun, 05 Apr 2026 17:12:44 GMT    79-10-7
# }
```
