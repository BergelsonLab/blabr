# Clean up raw CDI output (item + summary) for any project administered through WebCDI.

Clean up raw CDI output (item + summary) for any project administered
through WebCDI.

## Usage

``` r
wrangle_web_cdi(
  cdi_df,
  form = c("WG", "WS"),
  table = c("summary", "wordlevel", "raw"),
  withDemographic = FALSE,
  justWord = TRUE,
  rename = TRUE
)
```

## Arguments

- form:

  Which kind of cdi form is this (`WG` or `WS`). Will only include
  columns from each form.

- table:

  Which subset of the output to include? All table include id-related
  columns: "study_name", "subject_id", "repeat_num".

  - "summary" (default): each row is one cdi form, including aggregated
    score for each form (e.g. total words produced, total number of
    first gestures, etc.).

  - "wordlevel": each row is an item on one cdi form, with an "item"
    column for the name of the item and a "response" column.

  - "raw": this is the rawest form of the data as downloaded from
    webcdi, with id-related columns added, each row is one cdi form,
    including columns for each item and columns for aggregated score, as
    well as metadata columns (e.g. date created, webcdi link, admin id,
    etc.). Is not affected by `withDemographic` and `justWord`
    parameters.

- withDemographic:

  Should the data include answers to demographic questions (e.g. sibling
  count, birth order, caregive info, etc.)? These columns are not
  treated as item, so if you select `wordlevel_long` table, the
  demographic answers will be included in every row. If you select `raw`
  table, demographic columns will always be included. `FALSE` is
  default.

- justWord:

  Should the data include only vocabulary checklist item? If `TRUE`
  (default), returns only `Words Produced` and `Words Understood`
  related columns for `summary` table and vocabulary items for
  `wordlevel` table. If `FALSE`, will include gestures-related items for
  `WG` form and sentence-related items for `WS` form. If you select
  `raw` table, all items will be included regardless of this variable.

- rename:

  For `WS` form, do we standardize the name of the word-related summary
  item to be the same as `WG` form. If `TRUE` (default), the three
  summary columns in WS named `Total Produced`,
  `Total Produced Percentile-sex`, and `Total Produced Percentile-both`
  will changed to `Words Produced`, `Words Produced Percentile-sex` and
  `Words Produced Percentile-both`. Otherwise, they will remain
  unchanged

- filepath:

  Dataframe of the csv file downloaded from WebCDI (you should use the
  csv with both item level and sumary data)

## Value

A dataframe of the wrangled CDI output according to the parameters
