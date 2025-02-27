% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusters.R
\name{mmm_stats}
\alias{mmm_stats}
\title{Retrieve statistics from a mixture Markov model (MMM)}
\usage{
mmm_stats(x, use_t_dist = TRUE, level = 0.05)
}
\arguments{
\item{x}{A \code{mhmm} object.}

\item{use_t_dist}{A \code{logical} value. If \code{TRUE} (the default), the
t-distribution is used to compute confidence intervals.}

\item{level}{A \code{numeric} value representing the significance level for
hypothesis testing and confidence intervals. Defaults to \code{0.05}.}
}
\value{
A \code{data.frame} object.
}
\description{
Retrieve statistics from a mixture Markov model (MMM)
}
\examples{
mmm_stats(engagement_mmm)

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
\code{\link{reprune}()},
\code{\link{summary.group_tna}()},
\code{\link{summary.group_tna_bootstrap}()}
}
\concept{clusters}
