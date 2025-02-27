#' Identify Cliques in a Transition Network
#'
#' This function identifies cliques of a specified size in a transition network.
#' It searches for cliques—complete subgraphs where every pair of nodes is
#' connected—of size `n` in the transition matrix for the specified cluster
#' in the `tna` object.
#'
#' @export
#' @rdname cliques
#' @param x A `tna` or a `group_tna` object.
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
#' If `x` is a `group_tna` object, a `group_tna_cliques` object is returned
#' instead, which is a `list` or `tna_cliques` objects.
#'
#' @examples
#' model <- tna(group_regulation)
#'
#' # Find  2-cliques (dyads)
#' cliq <- cliques(model, size = 2)
#'
cliques <- function(x, ...) {
  UseMethod("cliques")
}

#' @export
#' @rdname cliques
cliques.tna <- function(x, size = 2, threshold = 0, sum_weights = FALSE, ...) {
  check_missing(x)
  check_class(x, "tna")
  check_values(threshold, type = "numeric")
  check_flag(sum_weights)
  stopifnot_(
    checkmate::test_int(x = size, lower = 2),
    "Argument {.arg size} must be a single {.cls integer}
    between 2 and {nodes(x)}."
  )
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

#' @export
#' @family clusters
#' @rdname cliques
cliques.group_tna <- function(x, size = 2, threshold = 0,
                              sum_weights = FALSE, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  structure(
    lapply(
      x,
      function(i) {
        cliques.tna(
          i,
          size = size,
          threshold = threshold,
          sum_weights = sum_weights,
          ...
        )
      }
    ),
    class = "group_tna_cliques"
  )
}
