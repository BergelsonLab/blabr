% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_bl.R
\name{big_aggregate}
\alias{big_aggregate}
\title{Add aggregations columns into an all_basiclevel tibble}
\usage{
big_aggregate(x, exclude = NULL, output = NULL, exclude_chi = FALSE)
}
\arguments{
\item{x}{an all_basiclevel dataframe}

\item{exclude}{columns to exclude from the final output}

\item{output}{an output path to save the result to}
}
\value{
a tibble containing the all_basicalevel data
}
\description{
Add aggregations columns into an all_basiclevel tibble
}
\examples{
\dontrun{
# get all the aggregations
all_bl <- get_all_basiclevel()
big_agg_allbl <- big_aggregate(all_bl)

# return everything except the 'type_token_ratio' and 'prop_dad' columns

bigagg_allbl_reduced <- big_aggregate(all_bl, exclude=c('type_token_ratio', 'prop_dad'))
}
}
