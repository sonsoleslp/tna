% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.summary.group_tna}
\alias{print.summary.group_tna}
\title{Print the summary of a grouped Transition Network Analysis Model}
\usage{
\method{print}{summary.group_tna}(x, ...)
}
\arguments{
\item{x}{A \code{summary.group_tna} object.}

\item{...}{Arguments passed to \code{\link[=print.summary.tna]{print.summary.tna()}}.}
}
\value{
\code{x} (invisibly).
}
\description{
Print the summary of a grouped Transition Network Analysis Model
}
\examples{
model <- group_model(engagement_mmm)
print(summary(model))

}
\seealso{
Cluster-related functions
\code{\link{bootstrap}()},
\code{\link{centralities}()},
\code{\link{cliques}()},
\code{\link{communities}()},
\code{\link{deprune}()},
\code{\link{estimate_cs}()},
\code{\link{group_model}()},
\code{\link{hist.group_tna}()},
\code{\link{mmm_stats}()},
\code{\link{plot.group_tna}()},
\code{\link{plot.group_tna_centralities}()},
\code{\link{plot.group_tna_cliques}()},
\code{\link{plot.group_tna_communities}()},
\code{\link{plot.group_tna_stability}()},
\code{\link{plot_compare.group_tna}()},
\code{\link{plot_mosaic.group_tna}()},
\code{\link{plot_mosaic.tna_data}()},
\code{\link{print.group_tna}()},
\code{\link{print.group_tna_bootstrap}()},
\code{\link{print.group_tna_centralities}()},
\code{\link{print.group_tna_cliques}()},
\code{\link{print.group_tna_communities}()},
\code{\link{print.group_tna_stability}()},
\code{\link{print.summary.group_tna_bootstrap}()},
\code{\link{prune}()},
\code{\link{pruning_details}()},
\code{\link{rename_groups}()},
\code{\link{reprune}()},
\code{\link{summary.group_tna}()},
\code{\link{summary.group_tna_bootstrap}()}
}
\concept{clusters}
