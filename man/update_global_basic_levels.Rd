% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_basic_level.r
\name{update_global_basic_levels}
\alias{update_global_basic_levels}
\title{Updates global basic levels in all_basiclevel_na}
\usage{
update_global_basic_levels(version = NULL)
}
\arguments{
\item{version}{Version tag of the \code{all_basiclevel} repository.

Loads from the "all_basiclevel" repository:
\itemize{
\item all_basiclevel_na.csv,
\item object_dict and annotid_disambiguation used for mapping objects/tokens to
their global basic level.
}

Then adds a global_bl column to all_basiclevel_na if it can.

If it can, it will return all_basiclevel_na with the new global_bl column.

If it can't, it will throw an error and provide instructions on how to update
the global basic level mappings.}
}
\description{
Updates global basic levels in all_basiclevel_na
}