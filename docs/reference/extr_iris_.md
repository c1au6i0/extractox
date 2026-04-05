# Extract Data from EPA IRIS Database

The `extr_iris` function sends a request to the EPA IRIS database to
search for information based on a specified keywords and cancer types.
It retrieves and parses the HTML content from the response. Note that if
`keywords` is not provide all dataset are retrieved.

## Usage

``` r
extr_iris_(
  casrn = NULL,
  cancer_types = c("non_cancer", "cancer"),
  verify_ssl = FALSE,
  ...
)
```

## Arguments

- casrn:

  A single character string specifying the CASRN for the search.

- cancer_types:

  A character vector specifying the types of cancer to include in the
  search. Must be either "non_cancer" or "cancer".

- verify_ssl:

  Boolean to control of SSL should be verified or not.

- ...:

  Any other arguments to be supplied to `req_option` and thus to
  `libcurl`.

## Value

A data frame containing the extracted data.

## See also

[EPA IRIS database](https://cfpub.epa.gov/ncea/iris/search/)

## Examples

``` r
if (FALSE) { #\donttest{
extr_iris("1332-21-4")
} # }
```
