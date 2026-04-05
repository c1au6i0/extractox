# Retrieve CASRN for PubChem CIDs

This function retrieves the CASRN for a given set of PubChem Compound
Identifiers (CID). It queries PubChem through the `webchem` package and
extracts the CASRN from the depositor-supplied synonyms.

## Usage

``` r
extr_casrn_from_cid(pubchem_ids, verbose = TRUE)
```

## Arguments

- pubchem_ids:

  A numeric vector of PubChem CIDs. These are unique identifiers for
  chemical compounds in the PubChem database.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

## Value

A data frame containing the CID, CASRN, and IUPAC name of the compound.
The returned data frame includes three columns:

- CID:

  The PubChem Compound Identifier.

- casrn:

  The corresponding CASRN of the compound.

- iupac_name:

  The IUPAC name of the compound.

- query:

  The pubchem_id queried.

## See also

[PubChem](https://pubchem.ncbi.nlm.nih.gov/)

## Examples

``` r
# \donttest{
# Example with formaldehyde and aflatoxin
cids <- c(712, 14434) # CID for formaldehyde and aflatoxin B1
extr_casrn_from_cid(cids)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Querying pubchem_ids.
#> # A tibble: 34 × 6
#>    cid   iupac_name   casrn      source_name                     source_id query
#>    <chr> <chr>        <chr>      <chr>                           <chr>     <chr>
#>  1 712   Formaldehyde 50-00-0    Australian Industrial Chemical… as_380    712  
#>  2 712   Formaldehyde 50-00-0    CAMEO Chemicals                 22034     712  
#>  3 712   Formaldehyde 50-00-0    CAMEO Chemicals                 769       712  
#>  4 712   Formaldehyde 50-00-0    CAMEO Chemicals                 17291     712  
#>  5 712   Formaldehyde 50-00-0    CAS Common Chemistry            50-00-0   712  
#>  6 712   Formaldehyde 30525-89-4 CAS Common Chemistry            30525-89… 712  
#>  7 712   Formaldehyde 50-00-0    ChemIDplus                      00000500… 712  
#>  8 712   Formaldehyde 50-00-0    DHS Chemical Facility Anti-Ter… dhs_Form… 712  
#>  9 712   Formaldehyde 50-00-0    DrugBank                        DB03843   712  
#> 10 712   Formaldehyde 50-00-0    DTP/NCI                         NSC 2988… 712  
#> # ℹ 24 more rows
# }
```
