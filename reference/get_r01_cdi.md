# Load CDI output from all RO1 studies and SemPhonD

Load CDI output from all RO1 studies and SemPhonD

## Usage

``` r
get_r01_cdi(
  table = c("summary", "wordlevel", "raw"),
  study = NULL,
  withDemographic = FALSE,
  justWord = TRUE,
  version = NULL
)
```

## Arguments

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

- study:

  Which study(s) to include among (`WFR`, `CLF`, `PBS`, `MISCOM`,
  `ProsPr`, `PreFunc`, `SemPhonD`, or `SPARKED`)? Provide either a
  string (if only selecting one study) or a vector of strings (for
  multiple studies) If `NULL` (default), will include all studies.

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

- version:

  version tag to checkout

## Details

Clone BLAB-private
[r01_cdi_spreadsheet](https://github.com/bergelsonlab/r01_cdi_spreadsheet.git)
repo to `~/BLAB_DATA` once before using this function.
