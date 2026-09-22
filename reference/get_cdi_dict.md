# Get the legends for the item columns for CDI forms, with which category they belong to. Items include the vocabulary checklist items, gesture items, and sentence items. These two csv (for WG and WS) was downloaded from WebCDI's GitHub repository: https://github.com/langcog/web-cdi/tree/master/webcdi/cdi_form_csv/cdi_forms, specifically the files `[English_WG].csv` and `[English_WS].csv`.

Get the legends for the item columns for CDI forms, with which category
they belong to. Items include the vocabulary checklist items, gesture
items, and sentence items. These two csv (for WG and WS) was downloaded
from WebCDI's GitHub repository:
https://github.com/langcog/web-cdi/tree/master/webcdi/cdi_form_csv/cdi_forms,
specifically the files `[English_WG].csv` and `[English_WS].csv`.

## Usage

``` r
get_cdi_dict(form = c("WG", "WS"))
```

## Arguments

- form:

  Which kind of cdi form is this (`WG` or `WS`)?
