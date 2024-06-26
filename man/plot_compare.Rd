% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_compare}
\alias{plot_compare}
\title{Plot the difference network between two models}
\usage{
plot_compare(x, y, ...)
}
\arguments{
\item{x}{An object of class \code{tna}. It will be the principal model.}

\item{y}{An object of class \code{tna}. It will be the model subtracted from the
principal model.}

\item{...}{Additional arguments passed to \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}
}
\value{
A \code{qgraph} object displaying the difference network between the
two models.
}
\description{
Plots the difference network between model \code{x} and model \code{y}. The edges are
computed from subtracting the two models. The pie chart is the difference in
initial probabilities between model \code{x} and model \code{y}. Green color indicates
that \code{x}is greater than \code{y}and red indicates otherwise.
}
\examples{
tna_model_1 <- build_tna(engagement[engagement[, 1] == "Active", ])
tna_model_2 <- build_tna(engagement[engagement[, 1] != "Active", ])
plot_compare(tna_model_1, tna_model_2)

}
