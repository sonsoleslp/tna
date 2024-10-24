% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pruning.R
\name{tna_disparity}
\alias{tna_disparity}
\title{Apply Disparity Filter to Transition Matrix in a tna Object}
\usage{
tna_disparity(x, cluster = 1, alpha = 0.5)
}
\arguments{
\item{x}{A \code{tna} object, which contains transition matrices and other relevant data.}

\item{cluster}{A numeric value specifying the cluster for which the transition matrix
should be extracted and processed. Defaults to \code{1}.}

\item{alpha}{A numeric value representing the significance level for the
disparity filter. Defaults to \code{0.5}.}
}
\value{
A modified \code{tna} object with the disparity-filtered transition matrix
for the specified cluster.
}
\description{
The \code{disparity} function applies a disparity filter to the transition matrix
of a specified cluster within a \code{tna} object and returns a modified \code{tna}
object with the filtered transition matrix.
}
\details{
This function extracts the transition matrix of the specified cluster from
the \code{tna} object, applies the disparity filter from the \code{backbone} package
with the specified \code{alpha}, and then multiplies the filtered result with the
original transition matrix. The result is returned as a new \code{tna} object with
updated transition data.

The function also uses the \code{onlyif} helper to conditionally pass the \code{colors}
attribute of the \code{tna} object if it is not \code{NULL}.
}
\examples{
\dontrun{
# Assuming 'tna_model' is a valid `tna` object
modified_tna <- disparity(tna_model)
}
}
\seealso{
Evaluation and validation functions
\code{\link{bootstrap_tna}()},
\code{\link{compare_tna_networks}()},
\code{\link{prune}()}
}
\concept{evaluation}
