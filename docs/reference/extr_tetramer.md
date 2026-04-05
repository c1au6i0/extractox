# Extract Tetramer Data from the CTD API

This function queries the Comparative Toxicogenomics Database API to
retrieve tetramer data based on chemicals, diseases, genes, or other
categories.

## Usage

``` r
extr_tetramer(
  chem,
  disease = "",
  gene = "",
  go = "",
  input_term_search_type = "directAssociations",
  qt_match_type = "equals",
  verify_ssl = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- chem:

  A string indicating the chemical identifiers such as CAS number or
  IUPAC name of the chemical.

- disease:

  A string indicating a disease term. Default is an empty string.

- gene:

  A string indicating a gene symbol. Default is an empty string.

- go:

  A string indicating a Gene Ontology term. Default is an empty string.

- input_term_search_type:

  A string specifying the search method to use. Options are
  "hierarchicalAssociations" or "directAssociations". Default is
  "directAssociations".

- qt_match_type:

  A string specifying the query type match method. Options are "equals"
  or "contains". Default is "equals".

- verify_ssl:

  Boolean to control if SSL should be verified or not. Default is FALSE.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- ...:

  Any other arguments to be supplied to `req_option` and thus to
  `libcurl`.

## Value

A data frame containing the queried tetramer data in CSV format.

## References

- Comparative Toxicogenomics Database: <https://ctdbase.org>

- Davis, A. P., Grondin, C. J., Johnson, R. J., Sciaky, D., McMorran,
  R., Wiegers, T. C., & Mattingly, C. J. (2019). The Comparative
  Toxicogenomics Database: update 2019. Nucleic acids research, 47(D1),
  D948–D954.
  [doi:10.1093/nar/gky868](https://doi.org/10.1093/nar/gky868)

- Davis, A. P., Wiegers, T. C., Wiegers, J., Wyatt, B., Johnson, R. J.,
  Sciaky, D., Barkalow, F., Strong, M., Planchart, A., &
  Mattingly, C. J. (2023). CTD tetramers: A new online tool that
  computationally links curated chemicals, genes, phenotypes, and
  diseases to inform molecular mechanisms for environmental health.
  Toxicological Sciences, 195(2), 155–168.
  [doi:10.1093/toxsci/kfad069](https://doi.org/10.1093/toxsci/kfad069)

## See also

[Comparative Toxicogenomics Database](https://ctdbase.org)

## Examples

``` r
# \donttest{
tetramer_data <- extr_tetramer(
  chem = c("50-00-0", "ethanol"),
  disease = "",
  gene = "",
  go = "",
  input_term_search_type = "directAssociations",
  qt_match_type = "equals"
)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CTD database for tetramer data for
#> 50-00-0...
#> ℹ Request succeeded with status code: 200
#> ℹ Sending request to CTD database for tetramer data for
#> ethanol...
#> ℹ Request succeeded with status code: 200
str(tetramer_data)
#> 'data.frame':    54564 obs. of  10 variables:
#>  $ chemical               : chr  "Formaldehyde" "Formaldehyde" "Formaldehyde" "Formaldehyde" ...
#>  $ chemical_id            : chr  "D005557" "D005557" "D005557" "D005557" ...
#>  $ gene                   : chr  "IL1B" "IL1B" "IFNG" "IL6" ...
#>  $ gene_id                : int  3553 3553 3458 3569 3569 3569 3569 7124 7124 7124 ...
#>  $ phenotype              : chr  "apoptotic process" "apoptotic process" "apoptotic process" "inflammatory response" ...
#>  $ phenotype_id           : chr  "GO:0006915" "GO:0006915" "GO:0006915" "GO:0006954" ...
#>  $ disease                : chr  "Inflammation" "Hyperalgesia" "Dermatitis, Allergic Contact" "Inflammation" ...
#>  $ disease_id             : chr  "D007249" "D006930" "D017449" "D007249" ...
#>  $ evidence_strength_score: int  4860 4320 3900 2376 2376 2376 2376 2016 2016 2016 ...
#>  $ query                  : chr  "50-00-0" "50-00-0" "50-00-0" "50-00-0" ...
# }
```
