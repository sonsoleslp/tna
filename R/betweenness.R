#' Build and Visualize a Network with Edge Betweenness
#'
#' This function builds a network from a transition matrix in a `tna` object
#' and computes edge betweenness for the network.
#'
#' The returned model remembers the model it was built from (its type and
#' scaling) and the values of `directed` and `invert`. The resampling
#' methods [permutation_test()], [bootstrap()], and [estimate_cs()] use
#' this information: every resample rebuilds the
#' original transition network from the resampled sequences and then
#' recomputes edge betweenness. For example, `permutation_test()` applied to
#' two edge betweenness networks tests the differences in edge betweenness
#' between the two groups. As for any network of a grouped model with
#' scaled edge weights, the edge betweenness networks of such a model are
#' not resampled.
#'
#' @export
#' @family centralities
#' @param x A `tna` or a `group_tna` object.
#' @param directed A `logical` value. If `TRUE`, the network is considered
#' directed.
#' @inheritParams centralities
#' @return A `tna` object where the edge weights are edge betweenness values.
#'   For a `group_tna` object, a `group_tna` object of such networks, one per
#'   group.
#' @examples
#' model <- tna(group_regulation)
#' betweenness_network(model)
#'
#' # Compare edge betweenness between groups
#' models <- group_tna(
#'   group_regulation,
#'   group = rep(c("first", "second"), each = 1000)
#' )
#' # Small number of iterations for CRAN
#' permutation_test(betweenness_network(models), iter = 20)
#'
betweenness_network <- function(x, directed = TRUE, invert = TRUE) {
  UseMethod("betweenness_network")
}

#' @rdname betweenness_network
#' @export
betweenness_network.tna <- function(x, directed = TRUE, invert = TRUE) {
  check_missing(x)
  check_class(x, "tna")
  check_flag(directed)
  check_flag(invert)
  type <- attr(x, "type")
  stopifnot_(
    !identical(type, "betweenness"),
    "Argument {.arg x} is already an edge betweenness network.",
    class = "tna_error_betweenness"
  )
  params <- attr(x, "params")
  params$betweenness <- list(
    type = type,
    scaling = attr(x, "scaling"),
    directed = directed,
    invert = invert
  )
  build_model_(
    weights = edge_betweenness_weights(x$weights, directed, invert),
    type = "betweenness",
    inits = x$inits,
    labels = x$labels,
    data = x$data,
    params = params
  )
}

#' @rdname betweenness_network
#' @export
betweenness_network.group_tna <- function(x, directed = TRUE, invert = TRUE) {
  check_missing(x)
  check_class(x, "group_tna")
  x[] <- lapply(
    x,
    betweenness_network.tna,
    directed = directed,
    invert = invert
  )
  attr(x, "type") <- "betweenness"
  x
}

#' Compute Edge Betweenness for a Weight Matrix
#'
#' @param weights A `matrix` of edge weights.
#' @inheritParams betweenness_network
#' @return A `matrix` of the same dimensions as `weights` where each positive
#'   edge weight is replaced by the edge betweenness of that edge.
#' @noRd
edge_betweenness_weights <- function(weights, directed, invert) {
  g <- as.igraph(weights)
  w <- ifelse_(invert, 1.0 / igraph::E(g)$weight, igraph::E(g)$weight)
  igraph::E(g)$weight <- w
  betweenness <- igraph::edge_betweenness(g, directed = directed)
  # igraph orders the edges by row of the adjacency matrix
  out <- t(weights)
  out[out > 0] <- betweenness
  t(out)
}

#' Get the Source Model of an Edge Betweenness Network
#'
#' @param params The `params` attribute of a `tna` object of type
#'   `"betweenness"`.
#' @return A `list` with the elements `type`, `scaling`, `directed`, and
#'   `invert` of the model the edge betweenness network was built from.
#' @noRd
betweenness_source <- function(params) {
  source <- params$betweenness
  stopifnot_(
    !is.null(source),
    c(
      "The edge betweenness network does not record the model it was
       built from.",
      `i` = "Rebuild it with {.fn betweenness_network} (tna 1.5.0 or
             later) to resample it."
    ),
    class = "tna_error_betweenness"
  )
  source
}
