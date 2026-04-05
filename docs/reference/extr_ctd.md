# Extract Data from the CTD API

This function queries the Comparative Toxicogenomics Database API to
retrieve data related to chemicals, diseases, genes, or other
categories.

## Usage

``` r
extr_ctd(
  input_terms,
  category = "chem",
  report_type = "genes_curated",
  input_term_search_type = "directAssociations",
  action_types = NULL,
  ontology = NULL,
  verify_ssl = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- input_terms:

  A character vector of input terms such as CAS numbers or IUPAC names.

- category:

  A string specifying the category of data to query. Valid options are
  "all", "chem", "disease", "gene", "go", "pathway", "reference", and
  "taxon". Default is "chem".

- report_type:

  A string specifying the type of report to return. Default is
  "genes_curated". Valid options include:

  "cgixns"

  :   Curated chemical-gene interactions. Requires at least one
      `action_types` parameter.

  "chems"

  :   All chemical associations.

  "chems_curated"

  :   Curated chemical associations.

  "chems_inferred"

  :   Inferred chemical associations.

  "genes"

  :   All gene associations.

  "genes_curated"

  :   Curated gene associations.

  "genes_inferred"

  :   Inferred gene associations.

  "diseases"

  :   All disease associations.

  "diseases_curated"

  :   Curated disease associations.

  "diseases_inferred"

  :   Inferred disease associations.

  "pathways_curated"

  :   Curated pathway associations.

  "pathways_inferred"

  :   Inferred pathway associations.

  "pathways_enriched"

  :   Enriched pathway associations.

  "phenotypes_curated"

  :   Curated phenotype associations.

  "phenotypes_inferred"

  :   Inferred phenotype associations.

  "go"

  :   All Gene Ontology (GO) associations. Requires at least one
      `ontology` parameter.

  "go_enriched"

  :   Enriched GO associations. Requires at least one `ontology`
      parameter.

- input_term_search_type:

  A string specifying the search method to use. Options are
  "hierarchicalAssociations" or "directAssociations". Default is
  "directAssociations".

- action_types:

  An optional character vector specifying one or more interaction types
  for filtering results. Default is "ANY". Other acceptable inputs are
  "abundance", "activity", "binding", "cotreatment", "expression",
  "folding", "localization", "metabolic processing"... See
  https://ctdbase.org/tools/batchQuery.go for a full list.

- ontology:

  An optional character vector specifying one or more ontologies for
  filtering GO reports. Default NULL.

- verify_ssl:

  Boolean to control of SSL should be verified or not.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- ...:

  Any other arguments to be supplied to `req_option` and thus to
  `libcurl`.

## Value

A data frame containing the queried data in CSV format.

## References

- Davis, A. P., Grondin, C. J., Johnson, R. J., Sciaky, D., McMorran,
  R., Wiegers, T. C., & Mattingly, C. J. (2019). The Comparative
  Toxicogenomics Database: update 2019. Nucleic acids research, 47(D1),
  D948–D954.
  [doi:10.1093/nar/gky868](https://doi.org/10.1093/nar/gky868)

## See also

[Comparative Toxicogenomics Database](https://ctdbase.org)

## Examples

``` r
# \donttest{
input_terms <- c("50-00-0", "64-17-5", "methanal", "ethanol")
dat <- extr_ctd(
  input_terms = input_terms,
  category = "chem",
  report_type = "genes_curated",
  input_term_search_type = "directAssociations",
  action_types = "ANY",
  ontology = c("go_bp", "go_cc")
)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CTD database...
#> ℹ Request succeeded with status code: 200
str(dat)
#> 'data.frame':    30140 obs. of  9 variables:
#>  $ chemical_name: chr  "Formaldehyde" "Formaldehyde" "Formaldehyde" "Formaldehyde" ...
#>  $ chemical_id  : chr  "D005557" "D005557" "D005557" "D005557" ...
#>  $ casrn        : chr  "50-00-0" "50-00-0" "50-00-0" "50-00-0" ...
#>  $ gene_symbol  : chr  "A4GALT" "AAK1" "AAMDC" "ABCA11P" ...
#>  $ gene_id      : int  53947 22848 28971 79963 26154 10351 5243 8714 10057 5825 ...
#>  $ organism     : chr  "Homo sapiens" "Homo sapiens" "Homo sapiens" "Homo sapiens" ...
#>  $ organism_id  : int  9606 9606 9606 9606 9606 9606 9606 9606 9606 9606 ...
#>  $ pubmed_ids   : chr  "27664576" "23649840" "23649840" "20655997" ...
#>  $ query        : chr  "50-00-0" "50-00-0" "50-00-0" "50-00-0" ...

# Get expresssion data
dat2 <- extr_ctd(
  input_terms = input_terms,
  report_type = "cgixns",
  category = "chem",
  action_types = "expression"
)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CTD database...
#> ℹ Request succeeded with status code: 200

str(dat2)
#> 'data.frame':    36312 obs. of  11 variables:
#>  $ chemical_name: chr  "Formaldehyde" "Formaldehyde" "Formaldehyde" "Formaldehyde" ...
#>  $ chemical_id  : chr  "D005557" "D005557" "D005557" "D005557" ...
#>  $ casrn        : chr  "50-00-0" "50-00-0" "50-00-0" "50-00-0" ...
#>  $ gene_symbol  : chr  "A4GALT" "AAK1" "AAMDC" "ABCA11P" ...
#>  $ gene_id      : int  53947 22848 28971 79963 26154 10351 5243 8714 10057 5825 ...
#>  $ organism     : chr  "Homo sapiens" "Homo sapiens" "Homo sapiens" "Homo sapiens" ...
#>  $ organism_id  : int  9606 9606 9606 9606 9606 9606 9606 9606 9606 9606 ...
#>  $ pubmed_ids   : chr  "Formaldehyde results in decreased expression of A4GALT mRNA" "Formaldehyde results in increased expression of AAK1 mRNA" "Formaldehyde results in increased expression of AAMDC mRNA" "Formaldehyde results in decreased expression of ABCA11P mRNA" ...
#>  $ query        : chr  "decreases^expression" "increases^expression" "increases^expression" "decreases^expression" ...
#>  $ NA           : chr  "27664576" "23649840" "23649840" "20655997" ...
#>  $ NA           : chr  "50-00-0" "50-00-0" "50-00-0" "50-00-0" ...
# }
```
