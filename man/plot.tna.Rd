% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.tna}
\alias{plot.tna}
\title{Plot a Transition Network Analysis Model}
\usage{
\method{plot}{tna}(
  x,
  labels,
  colors,
  pie,
  cut,
  show_pruned = TRUE,
  pruned_edge_color = "pink",
  edge.color = NA,
  edge.labels = TRUE,
  edge.label.position = 0.65,
  layout = "circle",
  layout_args = list(),
  scale_nodes,
  scaling_factor = 0.5,
  mar = rep(5, 4),
  theme = "colorblind",
  ...
)
}
\arguments{
\item{x}{A \code{tna} object from \code{\link[=tna]{tna()}}.}

\item{labels}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{colors}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{pie}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{cut}{Edge color and width emphasis cutoff value. The default is
the median of the edge weights. See \code{\link[qgraph:qgraph]{qgraph::qgraph()}} for details.}

\item{show_pruned}{A \code{logical} value indicating if pruned edges removed by
\code{\link[=prune]{prune()}} should be shown in the plot.  The default is \code{TRUE}, and the
edges are drawn as dashed with a different color to distinguish them.}

\item{pruned_edge_color}{A \code{character} string for the color to use for
pruned edges when \code{show_pruned = TRUE}. The default is \code{"pink"}.}

\item{edge.color}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{edge.labels}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{edge.label.position}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{layout}{One of the following:
\itemize{
\item A \code{character} string describing a \code{qgraph} layout (e.g., \code{"circle"})
or the name of a \code{igraph} layout function (e.g., \code{"layout_on_grid"}).
\item A \code{matrix} of node positions to use, with a row for each node and
\code{x} and \code{y} columns for the node positions.
\item A layout function from \code{igraph}.
}}

\item{layout_args}{A \code{list} of arguments to pass to the \code{igraph} layout
function when \code{layout} is a function or a character string that specifies
a function name.}

\item{scale_nodes}{A \code{character} string giving the name of a centrality
measure to scale the node size by. See \code{\link[=centralities]{centralities()}} for valid names.
If missing (the default), uses default \code{\link[qgraph:qgraph]{qgraph::qgraph()}} scaling.
Overrides \code{vsize} provided via \code{...}.}

\item{scaling_factor}{A \code{numeric} value specifying how strongly to scale
the nodes when \code{scale_nodes} is provided. Values
between 0 and 1 will result in smaller differences and values larger
than 1 will result in greater differences. The default is \code{0.5}.}

\item{mar}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{theme}{See \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}

\item{...}{Additional arguments passed to \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}
}
\value{
A \code{qgraph} plot of the transition network.
}
\description{
This function plots a transition network analysis (TNA) model using
the \code{qgraph} package. The nodes in the graph represent states, with node
sizes corresponding to initial state probabilities. Edge labels represent
the edge weights of the network.
}
\examples{
model <- tna(group_regulation)
plot(model)

}
\seealso{
Basic functions
\code{\link{build_model}()},
\code{\link{hist.group_tna}()},
\code{\link{hist.tna}()},
\code{\link{plot.group_tna}()},
\code{\link{plot_frequencies}()},
\code{\link{plot_frequencies.group_tna}()},
\code{\link{plot_mosaic}()},
\code{\link{plot_mosaic.group_tna}()},
\code{\link{plot_mosaic.tna_data}()},
\code{\link{print.group_tna}()},
\code{\link{print.summary.group_tna}()},
\code{\link{print.summary.tna}()},
\code{\link{print.tna}()},
\code{\link{summary.group_tna}()},
\code{\link{summary.tna}()},
\code{\link{tna-package}}
}
\concept{basic}
