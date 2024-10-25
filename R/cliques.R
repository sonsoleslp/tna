#' Identify Cliques in a Transition Network
#'
#' This function identifies cliques of a specified size in a transition network.
#' It searches for cliques—complete subgraphs where every pair of nodes is
#' connected—of size `n` in the transition matrix for the specified cluster
#' in the `tna` object.
#'
#' @export
#' @param x A `tna` object.
#' @param size An `integer` specifying the size of the cliques to identify.
#' Defaults to `3` (triads).
#' @param threshold A `numeric` value that sets the minimum edge weight
#' for an edge to be considered in the clique. Edges below this value
#' are ignored. Defaults to `0`.
#' @param sum_weights A `logical` value specifying whether the sum of the
#' weights should be above the `threshold` instead of individual weights of the
#' directed edges. Defaults to `FALSE`.
#' @param ... Ignored.
#' @return A `tna_cliques` object which is a `list` of two elements:
#'
#'   * `weights` is a `matrix` of the edge weights in the clique.
#'   * `inits` is a `numeric` vector of initial weights for the clique.
#'
#' @examples
#' model <- tna(engagement)
#'
#' # Find  2-cliques (dyads)
#' cliq <- cliques(model, size = 2)
#'
cliques <- function(x, ...) {
  UseMethod("cliques")
}

#' @rdname cliques
#' @export
cliques.tna <- function(x, size = 3, threshold = 0, sum_weights = FALSE, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  weights <- x$weights
  labels <- x$labels
  inits <- x$inits
  clique_idx <- integer(0)
  if (sum_weights) {
    mat <- weights + t(weights)
    mat[upper.tri(mat) | mat < threshold] <- 0
    g <- igraph::graph_from_adjacency_matrix(
      mat,
      mode = "undirected",
      weighted = TRUE
    )
    cliq <- igraph::cliques(g, min = size, max = size)
    cliq_idx <- lapply(cliq, function(y) which(labels %in% attr(y, "names")))
  } else {
    mat1 <- mat2 <- weights
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
    cliq_idx <- intersect(nodes1, nodes2)
  }
  structure(
    list(
      weights = lapply(
        cliq_idx,
        function(y) weights[y, y]
      ),
      inits = lapply(
        cliq_idx,
        function(y) inits[y]
      )
    ),
    class = "tna_cliques",
    size = size,
    labels = labels,
    threshold = threshold,
    sum_weights = sum_weights,
    colors = attr(x$data, "colors")
  )
}