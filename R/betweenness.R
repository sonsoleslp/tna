#' Build and Visualize a Network with Edge Betweenness
#'
#' This function builds a network from a transition matrix in a `tna` object
#' and computes edge betweenness for the network. Optionally, it visualizes the
#' network using the `qgraph` package, with the edge thickness representing the
#' edge betweenness values.
#'
#' @export
#' @param x A `tna` object containing transition matrices and
#' associated metadata.
#' @param ... Ignored.
#'
#' @details
#' The function first converts the transition matrix for the specified cluster
#' into a directed graph using the `igraph` package. It then calculates the
#' edge betweenness of the graph, which is a measure of how often an edge lies
#' on the shortest paths between pairs of nodes.
#'
#' If `plot = TRUE`, the function uses `qgraph` to visualize the network,
#' where edge thickness is proportional to edge betweenness, node colors are
#' derived from the `tna` object, and `Pie` values from the `tna` object are
#' displayed on the nodes.
#'
#' The layout of the network can be customized via the `layout` parameter,
#' which can either be a predefined layout from `qgraph` or a user-specified
#' matrix of node positions.
#'
#' @return A `tna` object where edge betweenness represents the edge weights.
#'
#' @examples
#' model <- tna(group_regulation)
#' betweenness_network(model)
#'
betweenness_network <- function(x, ...) {
  UseMethod("betweenness_network")
}

#' @rdname betweenness_network
#' @export
betweenness_network.tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna")
  weights <- x$weights
  g <- as.igraph(x)
  betweenness <- igraph::edge_betweenness(g, directed = TRUE)
  weights[weights > 0] <- betweenness
  build_model_(
    weights = weights,
    type = "betweenness",
    inits = x$inits,
    labels = x$labels,
    data = x$data
  )
}
