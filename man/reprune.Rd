% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pruning.R
\name{reprune}
\alias{reprune}
\alias{reprune.group_tna}
\title{Restore Previous Pruning of a Transition Network Analysis Model}
\usage{
reprune(x, ...)

\method{reprune}{group_tna}(x, ...)
}
\arguments{
\item{x}{A \code{tna} or \code{group_tna} object.}

\item{...}{Ignored.}
}
\value{
A \code{tna} or \code{group_tna} object that has not been pruned. The previous
pruning result can be reactivated with \code{\link[=reprune]{reprune()}}.
}
\description{
Restore Previous Pruning of a Transition Network Analysis Model
}
\examples{
model <- tna(group_regulation)
pruned_model <- prune(model, method = "threshold", threshold = 0.1)
depruned_model <- deprune(pruned_model) # restore original model
repruned_model <- reprune(depruned_model) # reapply the previous pruning

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
\code{\link{print.summary.group_tna}()},
\code{\link{print.summary.group_tna_bootstrap}()},
\code{\link{prune}()},
\code{\link{pruning_details}()},
\code{\link{rename_groups}()},
\code{\link{summary.group_tna}()},
\code{\link{summary.group_tna_bootstrap}()}
}
\concept{clusters}
