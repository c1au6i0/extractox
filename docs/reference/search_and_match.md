# Search and Match Data

This function searches for matches in a dataframe based on a given list
of ids and search type, then combines the results into a single
dataframe, making sure that NA rows are added for any missing ids. The
column `query` is a the end of the dataframe.

## Usage

``` r
search_and_match(dat, ids, search_type, col_names, chemical_col = "chemical")
```

## Arguments

- dat:

  The dataframe to be searched.

- ids:

  A vector of ids to search for.

- search_type:

  The type of search: "casrn" or "name".

- col_names:

  Column names to be used when creating a new dataframe in case of no
  matches.

- chemical_col:

  The name of the column in dat where chemical names are stored.

## Value

A dataframe with search results.

## Details

This function is used in `extr_pprtv` and `extr_monograph`.

## See also

[`extr_pprtv`](https://c1au6i0.github.io/extractox/reference/extr_pprtv.md),
[`extr_monograph`](https://c1au6i0.github.io/extractox/reference/extr_monograph.md)
