# cliques <- function(x, cluster = 1, n = 3, weight_threshold = 0,
#                     sum_weights = FALSE, minimum = 0.00001,
#                     mar = c(5,5,5,5), plot = TRUE, show_loops = FALSE, ...) {
#   # Extract necessary components from the Model
#   stopifnot_(
#     is_tna(x),
#     "Argument {.arg x} must be a {.cls tna} object."
#   )
#
#   weighted_matrix <- x$transits[[cluster]]
#   node_colors <- x$colors
#   node_labels <- x$labels
#   Pie <- x$inits[[cluster]]
#
#   num_nodes <- nrow(weighted_matrix)
#   cliques <- list()
#   clique_count <- 0
#
#
#   # Generate all possible n-combinations of nodes
#   node_combinations <- combn(1:num_nodes, n)
#
#   # Check each combination
#   for (i in 1:ncol(node_combinations)) {
#     nodes <- node_combinations[,i]
#     if (is_clique(nodes, sum_weights, weighted_matrix, weight_threshold)) {
#       clique_count <- clique_count + 1
#
#       # Extract submatrix for the clique
#       clique_matrix <- weighted_matrix[nodes, nodes]
#
#       # Optionally show loops in the plot, but never consider them in the computation
#       if (!show_loops) {
#         diag(clique_matrix) <- 0  # Remove self-loops from the matrix if show_loops is FALSE
#       }
#
#       # Set row and column names
#       rownames(clique_matrix) <- colnames(clique_matrix) <- node_labels[nodes]
#       clique_colors <- node_colors[match(rownames(clique_matrix), node_labels)]
#
#       cliques[[clique_count]] <- list(
#         matrix = clique_matrix,
#         # model = build_tna(clique_matrix, inits = Pie[nodes], colors = clique_colors),
#         pie_values = Pie[nodes]
#       )
#     }
#   }
#
#   if (clique_count == 0) {
#     warning_(paste("No", n, "-cliques found in the network."))
#   } else {
#     cat("Number of", n, "-cliques:", clique_count, "\n")
#     cat("Cliques:\n")
#
#     for (i in 1:length(cliques)) {
#       cat("\nClique", i, ":\n")
#       print(round(cliques[[i]]$matrix, 3))
#
#       if (plot) {
#         clique_matrix <- cliques[[i]]$matrix
#         clique_colors <- node_colors[match(rownames(clique_matrix), node_labels)]
#         clique_pie <- cliques[[i]]$pie_values
#
#         # Default plot parameters
#         plot_params <- list(
#           labels = colnames(clique_matrix),
#           edge.labels = TRUE,
#           directed = sum_weights,
#           edge.label.cex = 1.82,
#           mar = mar,
#           minimum = minimum,
#           theme = "colorblind",
#           cut = 0.01,
#           vsize = 25,
#           color = clique_colors,
#           pie = clique_pie
#         )
#
#         # Override default parameters with any provided in ...
#         plot_params <- modifyList(plot_params, list(...))
#
#         # Call qgraph with all parameters
#         do.call(qgraph::qgraph, c(list(clique_matrix), plot_params))
#       }
#     }
#   }
#
#   return(list(count = clique_count, cliques = cliques))
# }

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
    threshold = threshold
  )
}