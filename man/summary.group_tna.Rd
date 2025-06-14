% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.group_tna}
\alias{summary.group_tna}
\title{Calculate Summary of Network Metrics for a grouped Transition Network}
\usage{
\method{summary}{group_tna}(object, combined = TRUE, ...)
}
\arguments{
\item{object}{A \code{group_tna} object.}

\item{combined}{A logical indicating whether the summary results should be
combined into a single data frame for all clusters (defaults to \code{TRUE})}

\item{...}{Ignored}
}
\value{
A \code{summary.group_tna} object which is a \code{list} of \code{list}s or a
combined \code{data.frame}  containing the following network metrics:
\itemize{
\item \code{node_count}: The total number of nodes.
\item \code{edge_count}: The total number of edges.
\item \code{network_Density}: The density of the network.
\item \code{mean_distance}: The mean shortest path length.
\item \code{mean_out_strength}: The mean out-strength of nodes.
\item \code{sd_out_strength}: The standard deviation of out-strength.
\item \code{mean_in_strength}: The mean in-strength of nodes.
\item \code{sd_in_strength}: The standard deviation of in-strength.
\item \code{mean_out_degree}: The mean out-degree of nodes.
\item \code{sd_out_degree}: The standard deviation of out-degree.
\item \code{centralization_out_degree}: The centralization of out-degree.
\item \code{centralization_in_degree}: The centralization of in-degree.
\item \code{reciprocity}: The reciprocity of the network.
}
}
\description{
This function calculates a variety of network metrics for a \code{tna} object.
It computes key metrics such as node and edge counts, network density,
mean distance, strength measures, degree centrality, and reciprocity.
}
\details{
The function extracts the \code{igraph} network for each cluster and
computes the following network metrics:
\itemize{
\item Node count: Total number of nodes in the network.
\item Edge count: Total number of edges in the network.
\item Network density: Proportion of possible edges that
are present in the network.
\item Mean distance: The average shortest path length between nodes.
\item Mean and standard deviation of out-strength and in-strength: Measures
of the total weight of outgoing and incoming edges for each node.
\item Mean and standard deviation of out-degree: The number of outgoing
edges from each node.
\item Centralization of out-degree and in-degree: Measures of how
centralized the network is based on the degrees of nodes.
\item Reciprocity: The proportion of edges that are reciprocated
(i.e., mutual edges between nodes).
}
}
\examples{
group <- c(rep("High", 1000), rep("Low", 1000))
model <- group_model(group_regulation, group = group)
summary(model)

}
\seealso{
Basic functions
\code{\link{build_model}()},
\code{\link{hist.group_tna}()},
\code{\link{hist.tna}()},
\code{\link{plot.group_tna}()},
\code{\link{plot.tna}()},
\code{\link{plot_frequencies}()},
\code{\link{plot_frequencies.group_tna}()},
\code{\link{plot_mosaic}()},
\code{\link{plot_mosaic.group_tna}()},
\code{\link{plot_mosaic.tna_data}()},
\code{\link{print.group_tna}()},
\code{\link{print.summary.group_tna}()},
\code{\link{print.summary.tna}()},
\code{\link{print.tna}()},
\code{\link{summary.tna}()},
\code{\link{tna-package}}
}
\concept{basic}
