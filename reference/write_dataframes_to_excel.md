# Write Dataframes to Excel

This function creates an Excel file with each dataframe in a list as a
separate sheet.

## Usage

``` r
write_dataframes_to_excel(df_list, filename)
```

## Arguments

- df_list:

  A named list of dataframes to write to the Excel file.

- filename:

  The name of the Excel file to create.

## Value

No return value. The function prints a message indicating the completion
of the Excel file writing.

## Examples

``` r
# \donttest{
tox_dat <- extr_comptox("50-00-0")
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CompTox...
#> ℹ Getting info from CompTox...
temp_file <- tempfile(fileext = ".xlsx")
write_dataframes_to_excel(tox_dat, filename = temp_file)
#> ℹ Excel file written in /tmp/RtmpLe08kZ/file1bc136d38557.xlsx...
# }
```
