% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_cdi_spreadsheet}
\alias{get_cdi_spreadsheet}
\title{Get the CDI spreadsheet for SEEDLingS babies}
\usage{
get_cdi_spreadsheet(branch = NULL, commit = NULL, type = "feather")
}
\arguments{
\item{branch}{git branch to pull from}

\item{commit}{git commit to pull}

\item{type}{"feather" or "csv". defaults to "feather"}
}
\value{
a tibble containing the SEEDLingS CDI spreadsheet
}
\description{
Get the CDI spreadsheet for SEEDLingS babies
}
\examples{
cdi <- get_cdi_spreadsheet()
}
