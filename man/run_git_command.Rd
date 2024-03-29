% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git_sync.R
\name{run_git_command}
\alias{run_git_command}
\title{Run a git command in a dataset repository}
\usage{
run_git_command(repo, command, return_output = FALSE)
}
\arguments{
\item{repo}{dataset name: 'all_basiclevel', 'reliability', etc.}

\item{command}{what you would have after \code{git} on the command line except
for the -C \<folder\> part - the function will do that part for you.}

\item{return_output}{boolean, whether to return the printed output of the
command as string}
}
\value{
string with output if return_output is TRUE, else NULL
}
\description{
Run a git command in a dataset repository
}
\examples{
\dontrun{
run_git_command('all_basiclevel', 'status')
}
}
