% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_global_bl_mappings}
\alias{get_global_bl_mappings}
\title{Get the global basic level spreadsheets}
\usage{
get_global_bl_mappings(version = NULL)
}
\arguments{
\item{version}{version tag to checkout}
}
\value{
list of object_dict and
}
\description{
They are used to map every token in all_basiclevel_na to its global basic
level, see \code{map_global_basic_level} and \code{update_global_basic_levels}
}
\examples{
global_bl_mapping <- get_global_bl_mappings(version = '0.3.2')
}
