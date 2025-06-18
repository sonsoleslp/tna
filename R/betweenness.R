#' Build and Visualize a Network with Edge Betweenness
#'
#' This function builds a network from a transition matrix in a `tna` object
#' and computes edge betweenness for the network.
#'
#' @export
#' @family centralities
#' @param x A `tna` object.
#' @param directed A `logical` value. If `TRUE`, the network is considered
#' directed.
#' @return A `tna` object where the edge weights are edge betweenness values.
#' @examples
#' model <- tna(group_regulation)
#' betweenness_network(model)
#'
betweenness_network <- function(x, directed) {
  UseMethod("betweenness_network")
}

#' @rdname betweenness_network
#' @export
betweenness_network.tna <- function(x, directed) {
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
