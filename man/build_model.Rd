% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R, R/clusters.R
\name{build_model}
\alias{build_model}
\alias{build_model.default}
\alias{build_model.matrix}
\alias{build_model.stslist}
\alias{build_model.data.frame}
\alias{build_model.tna_data}
\alias{tna}
\alias{ftna}
\alias{ctna}
\alias{group_tna}
\alias{group_ftna}
\alias{group_ctna}
\title{Build a Transition Network Analysis Model}
\usage{
build_model(x, type = "relative", scaling = character(0L), ...)

\method{build_model}{default}(x, type = "relative", scaling = character(0L), inits, ...)

\method{build_model}{matrix}(x, type = "relative", scaling = character(0L), inits, ...)

\method{build_model}{stslist}(
  x,
  type = "relative",
  scaling = character(0L),
  cols = seq(1, ncol(x)),
  ...
)

\method{build_model}{data.frame}(
  x,
  type = "relative",
  scaling = character(0L),
  cols = seq(1, ncol(x)),
  ...
)

\method{build_model}{tna_data}(x, type = "relative", scaling = character(0), ...)

tna(x, scaling = character(0L), ...)

ftna(x, scaling = character(0L), ...)

ctna(x, scaling = character(0L), ...)

group_tna(x, scaling = character(0L), ...)

group_ftna(x, scaling = character(0L), ...)

group_ctna(x, scaling = character(0L), ...)
}
\arguments{
\item{x}{A \code{stslist} (from \code{TraMineR}), \code{data.frame}, a \code{matrix}, or
a \code{tna_data} object (see \code{\link[=prepare_data]{prepare_data()}}).
For \code{stslist} and \code{data.frame} objects \code{x}
should describe a sequence of events or states to be used for building the
Markov model.  If \code{x} is a matrix, it is assumed that the element on row
\code{i} and column \code{j} is the weight of the edge representing the transition
from state \code{i} to state \code{j}. If \code{x} is a \code{data.frame}, then
it must be in wide format (see \code{cols} on how to define columns for the
time points).}

\item{type}{A \code{character} string describing the weight matrix type.
Currently supports the following types:
\itemize{
\item \code{"relative"} for relative frequencies (probabilities, the default)
\item \code{"frequency"} for frequencies.
\item \code{"co-occurrence"} for co-occurrences.
}}

\item{scaling}{A \code{character} vector describing how to scale the weights
defined by \code{type}. When a vector is provided, the scaling options are
applied in the respective order. For example, \code{c("rank", "minmax")} would
first compute the ranks, then scale them to the unit interval using
min-max normalization. An empty vector corresponds to no scaling.
Currently supports the following options:
\itemize{
\item \code{"minmax"} performs min-max normalization to scale the weights to the
unit interval. Note that if the smallest weight is positive, it
will be zero after scaling.
\item \code{"max"} Multiplies the weights by the reciprocal of the largest weight
to scale the weights to the unit interval. This options preserves
positive ranks, unlike \code{"minmax"} when all weights are positive.
\item \code{"rank"} Computes the ranks of the weights using \code{\link[=rank]{rank()}} with
\code{ties.method = "average"}.
}}

\item{...}{Ignored.}

\item{inits}{An optional \code{numeric} vector of initial state probabilities
for each state. Can be provided only if \code{x} is a \code{matrix}. The vector will
be scaled to unity.}

\item{cols}{An \code{integer}/\code{character} vector giving the indices/names of the
columns that should be considered as sequence data.
Defaults to all columns, i.e., \code{seq(1, ncol(x))}.}
}
\value{
An object of class \code{tna} which is a \code{list} containing the
following elements:
\itemize{
\item \code{weights}: An adjacency \code{matrix} of the model (weight matrix).
\item \code{inits}: A \code{numeric} vector of initial values for each state.
For \code{matrix} type \code{x}, this element will be \code{NULL} if \code{inits} is not
directly provided
\item \code{labels}: A \code{character} vector of the state labels, or \code{NULL} if
there are no labels.
\item \code{data}: The original sequence data that has been converted to an
internal format used by the package when \code{x} is a \code{stslist} or a
\code{data.frame} object. Otherwise \code{NULL}.
}
}
\description{
This function constructs a transition network analysis (TNA) model from a
given sequence. It takes a sequence of events or states and builds a Markov
model. It extracts the transition probabilities
and initial probabilities from the model and stores them in a \code{list} along
with the state labels. Additionally, it creates a transition matrix with
zero diagonal entries (without loops). Also accepts matrices of transition
probabilities and initial state probabilities directly.
}
\examples{
model <- build_model(engagement)
print(model)

model <- tna(engagement)

model <- ftna(engagement)

model <- ctna(engagement)

model <- group_tna(engagement, group = gl(2, 100))

model <- group_ftna(engagement, group = gl(2, 100))

model <- group_ctna(engagement, group = gl(2, 100))

}
\seealso{
Core functions
\code{\link{centralities}()},
\code{\link{plot.tna}()},
\code{\link{plot.tna_centralities}()},
\code{\link{plot_compare}()}
}
\concept{core}
