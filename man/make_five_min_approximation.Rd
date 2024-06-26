% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lena.R
\name{make_five_min_approximation}
\alias{make_five_min_approximation}
\title{Approximate LENA's 5min.csv output}
\usage{
make_five_min_approximation(its_xml)
}
\arguments{
\item{its_xml}{XML object created by \code{rlena::read_its_file}.}
}
\value{
a tibble with at least these four columns: interval_start,
interval_end, AWC.Actual, CTC.Actual, CWC.Actual
}
\description{
Approximate LENA's 5min.csv output
}
\examples{
# Download the example ITS file (code from rlena's README)
url <- paste0("https://cdn.rawgit.com/HomeBankCode/lena-its-tools/",
              "master/Example/e20160420_165405_010572.its")
tmp <- tempfile()
download.file(url, tmp)
its <- rlena::read_its_file(tmp)
make_five_min_approximation(its_xml = its)
}
