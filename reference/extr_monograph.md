# Retrieve WHO IARC Monograph Information

This function returns information regarding Monographs from the World
Health Organization (WHO) International Agency for Research on Cancer
(IARC) based on CAS Registry Number or Name of the chemical. Note that
the data is not fetched dynamically from the website, but has retrieved
and copy hasbeen saved as internal data in the package.

## Usage

``` r
extr_monograph(ids, search_type = "casrn", verbose = TRUE, get_all = FALSE)
```

## Arguments

- ids:

  A character vector of IDs to search for.

- search_type:

  A character string specifying the type of search to perform. Valid
  options are "casrn" (CAS Registry Number) and "name" . (name of the
  chemical). If `search_type` is "casrn", the function filters . by the
  CAS Registry Number. If `search_type` is "name", the function performs
  a partial match search for the chemical name.

- verbose:

  A logical value indicating whether to print detailed messages. .
  Default is TRUE.

- get_all:

  Logical. If TRUE ignore all the other ignore `ids`, `search_type`, set
  `force = TRUE` and get the all dataset. This is was introduced for
  debugging purposes.

## Value

A data frame containing the relevant information from the WHO IARC, .
including Monograph `volume`, `volume_publication_year`,
`evaluation_year`, . and `additional_information` where the chemical was
described.

## See also

<https://monographs.iarc.who.int/list-of-classifications/>

## Examples

``` r
# \donttest{
dat <- extr_monograph(search_type = "casrn", ids = c("105-74-8", "120-58-1"), verbose = FALSE)
str(dat)
#> 'data.frame':    2 obs. of  8 variables:
#>  $ casrn                  : chr  "105-74-8" "120-58-1"
#>  $ agent                  : chr  "Lauroyl peroxide" "Isosafrole"
#>  $ group                  : chr  "3" "3"
#>  $ volume                 : chr  "36, Sup 7, 71" "10, Sup 7"
#>  $ volume_publication_year: chr  "1999" "1987"
#>  $ evaluation_year        : int  1998 1987
#>  $ additional_information : chr  "" ""
#>  $ query                  : chr  "105-74-8" "120-58-1"

# Example usage for name search
dat2 <- extr_monograph(
  search_type = "name",
  ids = c("Aloe", "Schistosoma", "Styrene"),
  verbose = FALSE
)
str(dat2)
#> 'data.frame':    8 obs. of  8 variables:
#>  $ casrn                  : chr  "" "" "" "" ...
#>  $ agent                  : chr  "Aloe vera, whole leaf extract" "Schistosoma haematobium (infection with)" "Schistosoma japonicum (infection with)" "Schistosoma mansoni (infection with)" ...
#>  $ group                  : chr  "2B" "1" "2B" "3" ...
#>  $ volume                 : chr  "108" "61, 100B" "61" "61" ...
#>  $ volume_publication_year: chr  "2016" "2012" "1994" "1994" ...
#>  $ evaluation_year        : int  2013 2009 1994 1994 2018 1987 1987 2018
#>  $ additional_information : chr  "" "" "" "" ...
#>  $ query                  : chr  "Aloe" "Schistosoma" "Schistosoma" "Schistosoma" ...
# }
```
