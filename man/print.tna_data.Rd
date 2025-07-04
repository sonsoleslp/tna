% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.tna_data}
\alias{print.tna_data}
\title{Print a TNA Data Object}
\usage{
\method{print}{tna_data}(x, data = "sequence", ...)
}
\arguments{
\item{x}{A \code{tna_data} object.}

\item{data}{A \code{character} string that defines the data to be printed
tibble. Accepts either \code{"sequence"} (default) for wide format sequence data,
\code{"meta"}, for the wide format metadata, or \code{"long"} for the long format
data.}

\item{...}{Arguments passed to the tibble \code{print} method.}
}
\value{
\code{x} (invisibly).
}
\description{
Print a TNA Data Object
}
\examples{
res <- prepare_data(group_regulation_long, action = "Action", actor = "Actor",
time = "Time")
print(res, which = "sequence")
print(res, which = "meta")
print(res, which = "long")

}
\seealso{
Other data: 
\code{\link{import_data}()},
\code{\link{prepare_data}()},
\code{\link{simulate.tna}()}
}
\concept{data}
