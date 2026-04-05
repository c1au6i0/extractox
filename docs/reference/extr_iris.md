# Extract Data from EPA IRIS Database

The `extr_iris` function sends a request to the EPA IRIS database to
search for information based on a specified keywords and cancer types.
It retrieves and parses the HTML content from the response.

## Usage

``` r
extr_iris(casrn = NULL, verbose = TRUE, delay = 0)
```

## Arguments

- casrn:

  A vector CASRN for the search.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- delay:

  Numeric value indicating the delay in seconds between requests to
  avoid overwhelming the server. Default is 0 seconds.

## Value

A data frame containing the extracted data.

## Examples

``` r
# \donttest{
Sys.sleep(3) # To avoid rate limiting due to previous examples
extr_iris(casrn = c("1332-21-4", "50-00-0"), delay = 2)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Quering 1332-21-4 to EPA IRIS database...
#> ℹ Quering 50-00-0 to EPA IRIS database...
#> # A tibble: 4 × 9
#>   chemical_name casrn     exposure_route assessment_type critical_effect_or_tu…¹
#>   <chr>         <chr>     <chr>          <chr>           <chr>                  
#> 1 Asbestos      1332-21-4 Inhalation     Cancer          Lung cancer and mesoth…
#> 2 Formaldehyde  50-00-0   Oral           Noncancer       Reduced weight gain, h…
#> 3 Formaldehyde  50-00-0   Inhalation     Cancer          Nasopharyngeal cancer,…
#> 4 Formaldehyde  50-00-0   Inhalation     Noncancer       Decreased pulmonary fu…
#> # ℹ abbreviated name: ¹​critical_effect_or_tumor_type
#> # ℹ 4 more variables: woe_characterization <chr>, toxicity_value_type <chr>,
#> #   toxicity_value <chr>, query <chr>
# }
```
