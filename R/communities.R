#' Community Detection for Transition Networks
#'
#' This function detects communities within the transition networks (represented
#' by the `tna` object).
#' It uses various algorithms to find communities in the graph representation of
#' transitions and returns a list of communities for each cluster or a specified cluster.
#'
#' @param x A `tna` object that contains transition matrices.
#' @param cluster An optional argument specifying which cluster to analyze.
#' If `NULL`, the function will analyze all clusters in `x`.
#' @param gamma A numeric parameter that affects the behavior of certain algorithms
#' like the Spin Glass method. Defaults to `1`.
#' @details
#' If multiple transition matrices exist, the function iterates over each cluster
#' in the `tna` object to find communities using different algorithms.
#' The function uses the `igraph` package to convert the transition matrices into
#' graphs and then applies community detection algorithms (e.g., Walktrap, Fast
#' Greedy, Label Propagation, Infomap, Edge Betweenness, Leading Eigenvector,
#' and Spin Glass).
#'
#' @return A list where each element represents the result of community detection
#' for a specific cluster. Each cluster's result is returned by the internal
#' `find_communities` function, which provides community counts and assignments
#' across different algorithms.
#' @family patterns
#' @examples
#' \dontrun{
#' # Detect communities for all clusters
#' communities <- community_detection(tna_model)
#'
#' # Detect communities for a specific cluster
#' communities <- community_detection(tna_model, cluster = 1)
#' }
#' @export
find_communities <- function(x, ...) {
  UseMethod("find_communities")
}

#' @rdname find_communities
#' @export
find_communities.tna <- function(x, cluster, gamma = 1) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  n_clust <- length(x$transits)
  out <- vector(mode = "list", length = n_clust)
  if (n_clust > 1L & is.missing(cluster)) {
    for (clust in seq_len(n_clust)) {
        #info_(paste0("Finding communities for ", clus))
        g <- igraph::graph_from_adjacency_matrix(
          x$transits[[clust]],
          mode = "directed",
          weighted = TRUE
        )
        out[[clust]] <- find_communities(g, gamma = gamma)
    }
  } else {
    clust <- ifelse_(is.missing(cluster), 1L, cluster)
    #info_(paste0("Finding communities for: ", clus))
    g <- igraph::graph_from_adjacency_matrix(
      x$transits[[clus]],
      mode = "directed",
      weighted = TRUE
    )
    out[[clust]] <- find_communities_(g, gamma = gamma)
  }
  structure(
    out,
    class = "communities"
  )
}

#' Internal Community Detection Function
#'
#' This function detects communities in a given graph using various algorithms
#' provided by the `igraph` package.
#'
#' @param g An `igraph` object representing the transition network.
#' @param gamma A numeric parameter that influences the Spin Glass algorithm. Defaults to `1`.

#' @details
#' This function applies multiple community detection algorithms (e.g., Walktrap,
#' Fast Greedy, Label Propagation, Infomap, Edge Betweenness, Leading Eigenvector,
#' and Spin Glass) to the input graph `g`. It then constructs a dataframe of
#' community assignments and a list of the number of communities detected by each algorithm.
#'
#' @return A `list` containing:
#'
#'   * `community_counts`: A `list` with the number of communities found
#'   by each algorithm.
#'   * `community_assignments`: A `data.frame` where each row corresponds to a
#'   node and each column to a community detection algorithm, with color-coded
#'   community assignments.
#' }
#'
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
    e.weights = igraph::E(g)$weight)
  mapping$infomap <- igraph::membership(communities$infomap) |>
    as.numeric()

  # Edge betweenness algorithm
  communities$edge_betweenness <- igraph::cluster_edge_betweenness(
    g,
    weights = igraph::E(g)$weight
  )
  mapping$edge_betweenness <-
    igraph::membership(communities$edge_betweenness) |>
    as.numeric()

  # Leading eigenvector algorithm
  communities$leading_eigen <- igraph::cluster_leading_eigen(
    g,
    weights = igraph::E(g)$weight
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

  # Print the number of communities found by each algorithm
  # info_("Number of communities found by each algorithm:\n")
  # info_(paste(result$community_counts, collapse = ", "))

  # Display the community assignments dataframe
  # info_("\nCommunity assignments:\n")
  # print_(result$community_assignments)

}
