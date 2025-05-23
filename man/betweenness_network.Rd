% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betweenness.R
\name{betweenness_network}
\alias{betweenness_network}
\alias{betweenness_network.tna}
\title{Build and Visualize a Network with Edge Betweenness}
\usage{
betweenness_network(x, ...)

\method{betweenness_network}{tna}(x, ...)
}
\arguments{
\item{x}{A \code{tna} object.}

\item{...}{Ignored.}
}
\value{
A \code{tna} object where the edge weights are edge betweenness values.
}
\description{
This function builds a network from a transition matrix in a \code{tna} object
and computes edge betweenness for the network.
}
\examples{
model <- tna(group_regulation)
betweenness_network(model)

}
\seealso{
Centrality measure functions
\code{\link{centralities}()},
\code{\link{plot.group_tna_centralities}()},
\code{\link{plot.tna_centralities}()},
\code{\link{print.group_tna_centralities}()},
\code{\link{print.tna_centralities}()}
}
\concept{centralities}
