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
#' Defaults to `2` (dyads).
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
cliques.tna <- function(x, size = 2, threshold = 0, sum_weights = FALSE, ...) {
  check_tna(x)
  stopifnot_(
    checkmate::test_int(x = size, lower = 2),
    "Argument {.arg size} must be a single {.cls integer}
    between 2 and {nodes(x)}."
  )
  check_nonnegative(threshold, type = "numeric")
  check_flag(sum_weights)
  weights <- x$weights
  labels <- x$labels
  inits <- x$inits
  clique_idx <- integer(0)
  mat <- weights
  mat[mat < threshold] <- 0
  if (sum_weights) {
    g <- igraph::graph_from_adjacency_matrix(
      mat,
      mode = "plus",
      weighted = TRUE
    )
    cliq <- igraph::cliques(g, min = size, max = size)
    cliq_idx <- lapply(cliq, function(y) which(labels %in% attr(y, "names")))
  } else {
    g1 <- igraph::graph_from_adjacency_matrix(
      mat,
      mode = "upper",
      weighted = TRUE
    )
    g2 <- igraph::graph_from_adjacency_matrix(
      mat,
      mode = "lower",
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