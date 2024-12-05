#' Community Detection for Transition Networks
#'
#' This function detects communities within the transition networks
#' (represented by the `tna` object).
#' It uses various algorithms to find communities in the graph representation
#' of transitions and returns a `list` of communities for each cluster or a
#' specified cluster. If multiple transition matrices exist, the function
#' iterates over each cluster in the `tna` object to find communities using
#' different algorithms. The function uses the `igraph` package to convert
#' the transition matrices into graphs and then applies community detection
#' algorithms (e.g., Walktrap, Fast Greedy, Label Propagation, Infomap,
#' Edge Betweenness, Leading Eigenvector, and Spin Glass).
#'
#' @export
#' @family patterns
#' @rdname communities
#' @param x A `tna` or a `group_tna` object.
#' @param gamma A `numeric` value depicting a parameter that affects the
#' behavior of certain algorithms like the Spin Glass method. Defaults to `1`.
#' @param ... Ignored.
#' @return An object of class `tna_communities` which is a `list` with an
#'   element for each cluster containing:
#'
#'   * `counts`: A `list` with the number of communities found
#'     by each algorithm.
#'   * `assignments`: A `data.frame` where each row corresponds to a
#'     node and each column to a community detection algorithm,
#'     with color-coded community assignments.
#'
#' If `x` is a `group_tna` object, a `group_tna_communities` object is returned
#' instead, which is a `list` of `tna_communities` objects.
#'
#' @examples
#' model <- tna(engagement)
#' comm <- communities(model)
#'
communities <- function(x, ...) {
  UseMethod("communities")
}

#' @export
#' @rdname communities
communities.tna <- function(x, gamma = 1, ...) {
  check_missing(x)
  check_class(x, "tna")
  stopifnot_(
    checkmate::test_number(x = gamma),
    "Argument {.arg gamma} must be a single {.cls numeric} value."
  )
  g <- as.igraph(x)
  g_un <- igraph::as_undirected(
    g,
    mode = "collapse",
    edge.attr.comb = list(weight = "sum")
  )
  communities <- list()
  mapping <- list()

  # Walktrap algorithm
  communities$walktrap <- igraph::cluster_walktrap(
    g,
    weights = igraph::E(g)$weight
  )
  mapping$walktrap <- igraph::membership(communities$walktrap) |>
    as.numeric()

  # Fast greedy algorithm (requires undirected graph)

  communities$fast_greedy <- igraph::cluster_fast_greedy(
    g_un,
    weights = igraph::E(g_un)$weight
  )
  mapping$fast_greedy <- igraph::membership(communities$fast_greedy) |>
    as.numeric()

  # Label propagation algorithm
  communities$label_prop <- igraph::cluster_label_prop(
    g,
    weights = igraph::E(g)$weight
  )
  mapping$label_prop <- igraph::membership(communities$label_prop) |>
    as.numeric()

  # Infomap algorithm
  communities$infomap <- igraph::cluster_infomap(
    g,
    e.weights = igraph::E(g)$weight
  )
  mapping$infomap <- igraph::membership(communities$infomap) |>
    as.numeric()

  # Edge betweenness algorithm
  # TODO warning? supressing for now...
  communities$edge_betweenness <- suppressWarnings(igraph::cluster_edge_betweenness(
    g,
    weights = igraph::E(g)$weight
  ))
  mapping$edge_betweenness <-
    igraph::membership(communities$edge_betweenness) |>
    as.numeric()

  # Leading eigenvector algorithm
  communities$leading_eigen <- igraph::cluster_leading_eigen(
    g_un,
    weights = igraph::E(g_un)$weight
  )
  mapping$leading_eigen <- igraph::membership(communities$leading_eigen) |>
    as.numeric()

  # Spin glass algorithm (requires undirected graph)
  communities$spinglass <- igraph::cluster_spinglass(
    g_un,
    weights = igraph::E(g_un)$weight,
    gamma = gamma
  )
  mapping$spinglass <- igraph::membership(communities$spinglass) |>
    as.numeric()

  structure(
    list(
      counts = lengths(communities),
      assignments = as.data.frame(
        c(
          list(node = igraph::V(g)$name),
          mapping
        )
      )
    ),
    class = "tna_communities",
    tna = x
  )
}

#' @export
#' @family clusters
#' @rdname communities
communities.group_tna <- function(x, gamma = 1, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  structure(
    lapply(x, \(i) communities.tna(i, gamma = gamma, ...)),
    class = "group_tna_communities"
  )
}
