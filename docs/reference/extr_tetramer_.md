# Extract Tetramer Data from the CTD API

This function queries the Comparative Toxicogenomics Database API to
retrieve tetramer data based on chemicals, diseases, genes, or other
categories. Function used internally.

## Usage

``` r
extr_tetramer_(
  chem,
  disease = "",
  gene = "",
  go = "",
  input_term_search_type = "directAssociations",
  qt_match_type = "equals",
  verify_ssl = FALSE,
  ...
)
```

## Arguments

- chem:

  A string indicating the ONE chemical identifier such as CAS number or
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

- ...:

  Any other arguments to be supplied to `req_option` and thus to
  `libcurl`.

## Value

A data frame containing the queried tetramer data in CSV format.
