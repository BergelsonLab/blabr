% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eye_tracking.R
\name{tag_low_data_trials}
\alias{tag_low_data_trials}
\title{Tag low-data trials based on fixation timeseries}
\usage{
tag_low_data_trials(
  fixation_timeseries,
  window_column = NULL,
  t_start = NULL,
  t_end = NULL,
  t_step = 20,
  min_fraction
)
}
\arguments{
\item{fixation_timeseries}{A dataframe containing fixation timeseries data. It must minimally contain the following columns:
\itemize{
\item \code{recording_id}: Identifier for the recording session.
\item \code{trial_index}: Index or identifier for the trial.
\item \code{is_good_timepoint}: Logical vector indicating valid time points (\code{TRUE} or \code{FALSE}).
\item If \code{window_column} is provided: a column with that name indicating time bins within the window of interest. Values should be \code{"Y"} or \code{"N"}.
}}

\item{window_column}{(Optional) A string specifying the name of the column that indicates (using the factor label \code{"Y"}) the time bins that belong to the window being tested for insufficient data. Either \code{window_column} or both \code{t_start} and \code{t_end} must be supplied.}

\item{t_start}{(Optional) Numeric value specifying the lower bound of the window of interest in milliseconds from the target onset. Must be provided along with \code{t_end} if \code{window_column} is not supplied.}

\item{t_end}{(Optional) Numeric value specifying the upper bound of the window of interest in milliseconds from the target onset. Must be provided along with \code{t_start} if \code{window_column} is not supplied.}

\item{t_step}{The time step in milliseconds. Must match the one used in \code{fixations_to_timeseries}.}

\item{min_fraction}{Numeric value between \code{0} and \code{1} indicating the minimum fraction of the window that must contain valid data for the trial to be considered "high-data". For example, \code{min_fraction = 1/3} requires at least one-third of the window to have valid data. \strong{This parameter must be specified.}}
}
\value{
The input dataframe with an additional logical column \code{is_trial_low_data}, indicating whether each trial is considered low-data (\code{TRUE}) or not (\code{FALSE}).
}
\description{
Identifies and marks trials with insufficient data based on the proportion of time points within a specified time window that contain valid data. A trial is considered "low-data" if less than \code{min_fraction} of the time window contains valid data.
}
\details{
Time points counted as having data meet both of the following criteria:
\itemize{
\item \code{is_good_timepoint} is \code{TRUE}. This column is typically created with a condition like \code{mutate(is_good_timepoint = some_condition)}. The definition can vary between studies.
\item The time bin is within the window of interest, indicated by the \code{window_column} or specified by \code{t_start} and \code{t_end}.
}

\strong{Usage Requirements:}
\itemize{
\item \strong{Either} \code{window_column} \strong{or both} \code{t_start} \strong{and} \code{t_end} \strong{must be supplied.}
\itemize{
\item If \code{window_column} is provided, the function uses it to identify time bins within the window of interest.
\item If \code{t_start} and \code{t_end} are provided:
\itemize{
\item The function checks if a column named \verb{window_\{t_start\}_\{t_end\}ms} exists in \code{fixation_timeseries}.
\itemize{
\item If it exists, the function stops and suggests using \code{window_column = "window_{t_start}_{t_end}ms"} instead.
\item If it does not exist, the function calls \code{assign_time_windows()} to create the required window column and proceeds.
}
\item The temporary \verb{which_window_\{t_start\}_\{t_end\}ms} column created by \code{assign_time_windows()} is dropped after use.
}
}
}

The function calculates the minimum number of time points with valid data required for a trial to be considered "high-data", based on the \code{min_fraction} and the duration of the time window (\code{t_end - t_start}). It then tags each trial by adding a new column \code{is_trial_low_data}, which is \code{TRUE} for low-data trials and \code{FALSE} otherwise.
}
