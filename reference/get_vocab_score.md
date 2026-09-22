# Calculate the vocabulary checklist score of a cdi spreadsheet This function was used to generate the current seedlings cdi spreadsheet as retrieved by `get_cdi_spreadsheet()`.

Calculate the vocabulary checklist score of a cdi spreadsheet This
function was used to generate the current seedlings cdi spreadsheet as
retrieved by
[`get_cdi_spreadsheet()`](http://bergelsonlab.com/blabr/reference/get_cdi_spreadsheet.md).

## Usage

``` r
get_vocab_score(data, cdi_type, remove_incomplete = T)
```

## Arguments

- data:

  a dataframe of the original cdi csv

- cdi_type:

  Either wg or ws

- remove_incomplete:

  whether to remove any incomplete cdi forms

## Value

New dataframe with vocab score
