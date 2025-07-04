% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{prepare_data}
\alias{prepare_data}
\title{Compute User Sessions from Event Data}
\usage{
prepare_data(
  data,
  actor,
  time,
  action,
  order,
  time_threshold = 900,
  custom_format = NULL,
  is_unix_time = FALSE,
  unix_time_unit = "seconds",
  unused_fn = dplyr::first
)
}
\arguments{
\item{data}{A \code{data.frame} or containing the action/event data.}

\item{actor}{A \code{character} string giving the name of the column that
represents a user/actor identifier. If not provided and neither \code{time} nor
\code{order} is specified, the entire dataset is treated as a single session.}

\item{time}{A \code{character} string giving the name of the column representing
timestamps of the action events.}

\item{action}{A \code{character} string giving the name of the column holding
the information about the action taken.}

\item{order}{A \code{character} string giving the name of a column with sequence
numbers or non-unique orderable values that indicate order within an \code{actor}
group, if not present it will be ordered with all the data if no \code{actor} is
available, used when widening the data. If both \code{actor} and \code{time} are
specified, then the sequence order should be specified such that it
determines the order of events within \code{actor} and each session.}

\item{time_threshold}{An \code{integer} specifying the time threshold in seconds
for creating new time-based sessions. Defaults to 900 seconds.}

\item{custom_format}{A \code{character} string giving the format used to
parse the \code{time} column.}

\item{is_unix_time}{A \code{logical} value indicating whether the \code{time} column
is in Unix time. The default is \code{FALSE}.}

\item{unix_time_unit}{A \code{character} string giving the Unix time unit when
\code{is_unix_time} is \code{TRUE}. The default is \code{"seconds"}. Valid options are
\code{"seconds"}, \code{"milliseconds"}, or \code{"microseconds"}.}

\item{unused_fn}{How to handle extra columns when pivoting to wide format.
See \code{\link[tidyr:pivot_wider]{tidyr::pivot_wider()}}. The default is to keep all columns and to
use the first value.}
}
\value{
A \code{tna_data} object, which is a \code{list} with the following elements:
\itemize{
\item \code{long_data}: The processed data in long format.
\item \code{sequence_data}: The processed data on the sequences in wide format,
with actions/events as different variables structured with sequences.
\item \code{meta_data}: Other variables from the original data in wide format.
\item \code{statistics}: A \code{list} containing summary statistics: total
sessions, total actions, unique users, time range (if applicable), and
top sessions and user by activities.
}
}
\description{
Processes a dataset to create user sessions based on time gaps,
ordering columns, or actor groupings. It supports different ways to
understand order in user behavior and provides flexibility when
widening the data.
}
\examples{
results <- prepare_data(
  group_regulation_long, actor = "Actor", time = "Time", action = "Action"
)
print(results$sequence_data)
print(results$meta_data)
print(results$statistics)

data_ordered <- tibble::tibble(
   user = c("A", "A", "A", "B", "B", "C", "C", "C"),
   order = c(1, 2, 3, 1, 2, 1, 2, 3),
   action = c(
     "view", "click", "add_cart", "view",
     "checkout", "view", "click", "share"
   )
)
results_ordered <- prepare_data(
  data_ordered, actor = "user", order = "order", action = "action"
)
print(results_ordered$sequence_data)
print(results_ordered$meta_data)
print(results_ordered$statistics)

data_single_session <- tibble::tibble(
  action = c(
    "view", "click", "add_cart", "view",
    "checkout", "view", "click", "share"
   )
)
results_single <- prepare_data(data_single_session, action = "action")
print(results_single$sequence_data)
print(results_single$meta_data)
print(results_single$statistics)

}
\seealso{
Other data: 
\code{\link{import_data}()},
\code{\link{print.tna_data}()},
\code{\link{simulate.tna}()}
}
\concept{data}
