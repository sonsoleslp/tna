#' Identify Cliques in a Transition Network
#'
#' This function identifies cliques of a specified size in a transition network.
#' It searches for cliques—complete subgraphs where every pair of nodes is
#' connected—of size `n` in the transition matrix for the specified cluster
#' in the `tna` object.
#'
#' @export
#' @param x An object of type `tna`
#' @param cluster An integer specifying which cluster to analyze.
#' Defaults to `1`.
#' @param size An `integer` specifying the size of the cliques to identify.
#' Defaults to `3` (triads).
#' @param threshold A `numeric` value that sets the minimum edge weight
#' for an edge to be considered in the clique. Edges below this value
#' are ignored. Defaults to `0`.
#' @return A `cliques` object which is a `list` of two elements:
#'
#'   * `weights` is a `matrix` of the edge weights in the clique.
#'   * `inits` is a `numeric` vector of initial weights for the clique.
#'
#' @examples
#' \dontrun{
#' # Find  3-cliques (triads) in the first cluster
#' cliques_result <- cliques(my_tna_object, cluster = 1, size = 3)
#'
#' # Find 4-cliques in the second cluster
#' cliques_result <- cliques(my_tna_object, cluster = 2, size = 4)
#' }
#'
cliques <- function(x, cluster = 1, size = 3, threshold = 0) {
  weights <- x$weights[[cluster]]
  labels <- x$labels
  inits <- x$inits[[cluster]]
  mat1 <- mat2 <- weights
  # TODO previous sum_weights could be implemented here
  mat1[upper.tri(mat1) | mat1 < threshold] <- 0
  mat2[lower.tri(mat2) | mat2 < threshold] <- 0
  g1 <- igraph::graph_from_adjacency_matrix(
    mat1,
    mode = "undirected",
    weighted = TRUE
  )
  g2 <- igraph::graph_from_adjacency_matrix(
    mat2,
    mode = "undirected",
    weighted = TRUE
  )
  cliq1 <- igraph::cliques(g1, min = size, max = size)
  cliq2 <- igraph::cliques(g2, min = size, max = size)
  nodes1 <- lapply(cliq1, function(y) which(labels %in% attr(y, "names")))
  nodes2 <- lapply(cliq2, function(y) which(labels %in% attr(y, "names")))
  common <- intersect(nodes1, nodes2)
  structure(
    list(
      weights = lapply(
        common,
        function(y) weights[y, y]
      ),
      inits = lapply(
        common,
        function(y) inits[y]
      )
    ),
    class = "tna_cliques",
    labels = labels,
    threshold = threshold,
    cluster = cluster,
    size = size,
    colors = attr(x$seq[[cluster]], "colors")
  )
}