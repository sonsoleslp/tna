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
#' @inheritParams centralities
#' @return A `tna` object where the edge weights are edge betweenness values.
#' @examples
#' model <- tna(group_regulation)
#' betweenness_network(model)
#'
betweenness_network <- function(x, directed = TRUE, invert = TRUE) {
  UseMethod("betweenness_network")
}

#' @rdname betweenness_network
#' @export
betweenness_network.tna <- function(x, directed = TRUE, invert = TRUE) {
  check_missing(x)
  check_class(x, "tna")
  g <- as.igraph(x)
  w <- ifelse_(invert, 1.0 / igraph::E(g)$weight, igraph::E(g)$weight)
  igraph::E(g)$weight <- w
  betweenness <- igraph::edge_betweenness(g, directed = directed)
  weights <- t(x$weights)
  weights[weights > 0] <- betweenness
  build_model_(
    weights = t(weights),
    type = "betweenness",
    inits = x$inits,
    labels = x$labels,
    data = x$data
  )
}
