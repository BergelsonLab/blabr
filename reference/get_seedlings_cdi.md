# Get the seedlings CDI table from the BLAB_DATA [repo](https://github.com/BergelsonLab/cdi_spreadsheet) using `get_cdi_spreadsheet()`, but wrangled to include summary values and with options for which form to retrieve the dataframe. The norm conversion table was downloaded from https://github.com/langcog/wordbank-shiny/tree/main/apps/scoring/norms/percentiles/English%20Percentiles

Get the seedlings CDI table from the BLAB_DATA
[repo](https://github.com/BergelsonLab/cdi_spreadsheet) using
[`get_cdi_spreadsheet()`](http://bergelsonlab.com/blabr/reference/get_cdi_spreadsheet.md),
but wrangled to include summary values and with options for which form
to retrieve the dataframe. The norm conversion table was downloaded from
https://github.com/langcog/wordbank-shiny/tree/main/apps/scoring/norms/percentiles/English%20Percentiles

## Usage

``` r
get_seedlings_cdi(
  version = NULL,
  table = c("summary", "wordlevel", "raw"),
  justWord = TRUE
)
```

## Arguments

- version:

  version tag to checkout

- table:

  Which subset of the output to include? All table include demographic
  related items: "ResponseID", "SubjectNumber", "subj", "month",
  "Date_Completed", "AgeMonthUncorrected", "SeedlingsFinalSample",
  "Relation_to_child", "Child_gender"

  - "summary" (default): each row is one CDI form, including aggregated
    score for for each section of the CDI (CDIcomp, game gestures, later
    gestures). If a section does not have an aggregate (i.e. first
    signs, starting to talk), will include them as separate items with
    binary values (1 or 0).

  - "wordlevel": each row is an item on one cdi form, with an "item"
    column for the name of the item and a "response" column.

  - "raw": this is the rawest form of the data as saved in BLAB_DATA,
    with one row for each CDI form, including demographic columns, items
    columns, and CDIcomp, CDIprod. Is not affected by `justWord`
    parameters.

- justWord:

  Should the data include only vocabulary checklist item? If `TRUE`
  (default), returns only `CDIcomp` and `CDIprod` related for `summary`
  table and only vocabulary items for `wordlevel` table. If `FALSE`,
  will include gestures-related items. If you select `raw` table, all
  items will be included regardless of this variable.

## Value

A dataframe of the wrangled CDI output according to the parameters
