% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{as.igraph.matrix}
\alias{as.igraph.matrix}
\title{Coerce a Weight Matrix into an \code{igraph} Object.}
\usage{
\method{as.igraph}{matrix}(x, directed = TRUE, ...)
}
\arguments{
\item{x}{The object to convert.}

\item{directed}{A \code{logical} value. If \code{TRUE}, assumes that the graph is
directed and undirected otherwise.}

\item{...}{Additional arguments. None currently.}
}
\value{
An \code{igraph} object.
}
\description{
Coerce a Weight Matrix into an \code{igraph} Object.
}
\seealso{
Helper functions
\code{\link{as.igraph.group_tna}()},
\code{\link{as.igraph.tna}()}
}
\concept{helpers}
