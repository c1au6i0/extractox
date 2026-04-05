# Extract FEMA from PubChem

This function retrieves FEMA (Flavor and Extract Manufacturers
Association) flavor profile information for a list of CAS Registry
Numbers (CASRN) from the PubChem database using the `webchem` package.

## Usage

``` r
extr_pubchem_fema(casrn, verbose = TRUE, delay = 0)
```

## Arguments

- casrn:

  A vector of CAS Registry Numbers (CASRN) as atomic vectors.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- delay:

  A numeric value indicating the delay (in seconds) between API
  requests. This controls the time between successive PubChem queries.
  Default is 0. See Details for more info.

## Value

A data frame containing the FEMA flavor profile information for each
CASRN. If no information is found for a particular CASRN, the output
will include a row indicating this.

## Details

The function performs two queries to PubChem:

1.  The first query retrieves the PubChem Compound Identifier (CID) for
    each IUPAC name.

2.  The second query retrieves additional information using the obtained
    CIDs. In cases of multiple rapid successive requests, the PubChem
    server may deny access. Introducing a delay between requests (using
    the `delay` parameter) can help prevent this issue.

## See also

[PubChem](https://pubchem.ncbi.nlm.nih.gov/)

## Examples

``` r
# \donttest{
extr_pubchem_fema(c("83-67-0", "1490-04-6"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying 83-67-0. 
#> OK (HTTP 200).
#> 
#> Querying 5429. 
#> Not Found (HTTP 404).
#> 
#> Warning: FEMA Flavor Profile for 83-67-0 not found!
#> Querying 1490-04-6. 
#> OK (HTTP 200).
#> 
#> Querying 1254. 
#> OK (HTTP 200).
#> 
#>    cid     casrn  IUPAC_name     result
#> 1 5429   83-67-0 Theobromine       <NA>
#> 2 1254 1490-04-6     Menthol Mint, Cool
#>                                           source_name source_id
#> 1                                                <NA>      <NA>
#> 2 Flavor and Extract Manufacturers Association (FEMA)      2665
#>                                other     query
#> 1 FEMA Flavor Profile info not found   83-67-0
#> 2                               <NA> 1490-04-6
# }
```
