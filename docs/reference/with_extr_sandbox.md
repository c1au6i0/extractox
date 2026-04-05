# Run Code in a Temporary Sandbox Environment

This function creates a temporary directory and sets it as
`R_USER_CACHE_DIR` before executing the provided code block. It is used
for testing or running code without affecting the user's default cache
directory as required by CRAN for the examples .This function is not
designed to be used by package users. Shamelessly "inspired" by some
@luciorq code.

## Usage

``` r
with_extr_sandbox(code, temp_dir = tempdir())
```

## Arguments

- code:

  The code to be executed inside the sandbox. Should be an expression.

- temp_dir:

  A temporary directory created using `temdir()`.

## Value

The result of the executed code.

## Examples

``` r
with_extr_sandbox(Sys.getenv("R_USER_CACHE_DIR"))
#> [1] "/tmp/RtmpMbdMYU"
with_extr_sandbox(tools::R_user_dir("extractox", "cache"))
#> [1] "/tmp/RtmpMbdMYU/R/extractox"
```
