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
#' @param x A `tna` object that contains transition matrices.
#' @param cluster An optional argument specifying which cluster to analyze.
#' If `NULL`, the function will analyze all clusters in `x`.
#' @param gamma A numeric parameter that affects the behavior of certain
#' algorithms like the Spin Glass method. Defaults to `1`.
#' @return An object of class `tna_communities` which is a `list` with an
#'   element for each cluster containing:
#'
#'   * `counts`: A `list` with the number of communities found
#'   by each algorithm.
#'   * `assignments`: A `data.frame` where each row corresponds to a
#'   node and each column to a community detection algorithm, with color-coded
#'   community assignments.
#'
#' @examples
#' \dontrun{
#' # Detect communities for all clusters
#' communities <- community_detection(tna_model)
#'
#' # Detect communities for a specific cluster
#' communities <- community_detection(tna_model, cluster = 1)
#' }
#'
find_communities <- function(x, ...) {
  UseMethod("find_communities")
}

#' @rdname find_communities
#' @export
find_communities.tna <- function(x, cluster = NULL, gamma = 1) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  n_clust <- length(x$weights)
  out <- vector(mode = "list", length = n_clust)
  if (n_clust > 1L & missing(cluster)) {
    for (i in seq_len(n_clust)) {
      g <- igraph::graph_from_adjacency_matrix(
        x$weights[[i]],
        mode = "directed",
        weighted = TRUE
      )
      out[[i]] <- find_communities_(g, gamma = gamma)
    }
  } else {
    cluster <- ifelse_(is.null(cluster), 1L, cluster)
    g <- igraph::graph_from_adjacency_matrix(
      x$weights[[cluster]],
      mode = "directed",
      weighted = TRUE
    )
    out[[1]] <- find_communities_(g, gamma = gamma)
  }
  structure(
    out,
    class = "tna_communities",
    cluster = cluster,
    tna = x
  )
}

#' Internal Community Detection Function
#'
#' @param g An `igraph` graph object
#' @inheritParams find_communities
#' @noRd
find_communities_ <- function(g, gamma = 1) {
  # Find communities using different algorithms and assign to named objects
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
  g_undirected <- igraph::as.undirected(
    g,
    mode = "collapse",
    edge.attr.comb = list(weight = "sum")
  )
  communities$fast_greedy <- igraph::cluster_fast_greedy(
    g_undirected,
    weights = igraph::E(g_undirected)$weight
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
  # TODO warning?
  communities$edge_betweenness <- igraph::cluster_edge_betweenness(
    g,
    weights = igraph::E(g)$weight
  )
  mapping$edge_betweenness <-
    igraph::membership(communities$edge_betweenness) |>
    as.numeric()

  # Leading eigenvector algorithm
  # TODO how should the graph be converted to undirected
  g_un <- igraph::as.undirected(g, mode = "collapse")
  communities$leading_eigen <- igraph::cluster_leading_eigen(
    g_un,
    weights = igraph::E(g_un)$weight
  )
  mapping$leading_eigen <- igraph::membership(communities$leading_eigen) |>
    as.numeric()

  # Spin glass algorithm (requires undirected graph)
  communities$spinglass <- igraph::cluster_spinglass(
    g_undirected,
    weights = igraph::E(g_undirected)$weight,
    gamma = gamma
  )
  mapping$spinglass <- igraph::membership(communities$spinglass) |>
    as.numeric()

  list(
    counts = lengths(communities),
    assignments = as.data.frame(
      c(
        list(node = igraph::V(g)$name),
        mapping
      )
    )
  )

}
