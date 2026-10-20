# Convert summary scores in CDIs subsections into percentage of items selected over total number of items

Convert summary scores in CDIs subsections into percentage of items
selected over total number of items

## Usage

``` r
add_cdi_percentage_score(df, is_seedlings = TRUE)
```

## Arguments

- df:

  A wrangled CDI dataframe (either from WebCDI output or from the
  SEEDLingS CDI spreadsheet)

- is_seedlings:

  A boolean indicating whether the dataframe is from the SEEDLingS CDI
  spreadsheet (`TRUE`) or from WebCDI output (`FALSE`), since these two
  formats have different column names. Default is `TRUE`.

## Value

A dataframe with a new column with percentage scores for each
subsection. The new column is added right after the original column

## Details

This function takes the summary score of each subsection and divide it
by the total number of items in that subsection, returning a percentage
score. It will apply this calculation to any column that matches the
name of a summary column (either from WebCDI output or from the
SEEDLingS CDI spreadsheet). E.g., for column `CDIcomp` in the SEEDLingS
CDI spreadsheet, the new column will be called `CDIcomp-percentage`,
while for column `Words Produced` in a WebCDI output, it will be named
`Words Produced Percentage`, to stay consistent with other columns
naming convention. The function uses a lookup table to determine the
total number of items in each subsection, which is stored in a CSV file
within the package.
