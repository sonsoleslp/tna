#' #' Find Dyads in a Transition Network
#' #'
#' #' This function identifies and visualizes dyads (sets of two nodes forming a pair)
#' #' in a transition network from a `tna` object. It can handle both directed
#' #' and undirected networks, and it allows for customizable visualization.
#' #'
#' #' @param x A `tna` object that contains transition matrices and associated node metadata.
#' #' @param cluster An integer specifying which cluster to analyze. Defaults to `1`.
#' #' @param weight_threshold A numeric value that sets the minimum weight for an
#' #' edge to be considered. Edges below this threshold are ignored. Defaults to `0`.
#' #' @param directed A logical value indicating whether the network is directed.
#' #' Defaults to `TRUE`.
#' #' @param sum_weights A logical value specifying whether to sum weights for
#' #' directed edges when calculating triangle weights. Defaults to `FALSE`.
#' #' @param plot A logical value specifying whether to plot the dyads or not
#' #' @param layout The network layout for `qgraph`.
#' #' @param minimum A numeric value for the minimum edge weight to be displayed
#' #' in the visualization. Defaults to `0.05`.
#' #' @param mar A numeric vector specifying the margins for the visualization plot.
#' #' @param ... Additional parameters passed to the `qgraph` function for plotting.
#' #' @family patterns
#' #' @details
#' #' This function loops through all possible pairs of nodes in the network
#' #' and checks whether the edges between them form a complete dyad
#' #' The `weight_threshold` parameter allows filtering out weak edges, and the
#' #' `sum_weights` option sums directed edge weights.
#' #'
#' #' For each detected dyad, the function stores the nodes, their weights,
#' #' the submatrix corresponding to the dyad, and the initial state
#' #' probabilities (`Pie` values) for the nodes. It then visualizes each dyad
#' #' using the `qgraph` package, allowing customization of the plot via additional
#' #' arguments.
#' #' @author
#' #' Mohammed Saqr (\email{mohammed.saqr@uef.fi})
#' #' @return A list with two elements:
#' #' \itemize{
#' #'   \item `count`: The total number of dyads found.
#' #'   \item `dyads`: A list of detected dyads, where each element contains:
#' #'     \itemize{
#' #'       \item `nodes`: The labels of the nodes forming the dyad
#' #'       \item `weights`: The edge weights between the nodes.
#' #'       \item `matrix`: The submatrix representing the connections between the
#' #'       nodes in the dyad
#' #'       \item `pie_values`: The initial state probabilities (`Pie` values) for
#' #'       the nodes in the dyad.
#' #'     }
#' #' }
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Find and visualize dyads in the first cluster
#' #' dyads <- find_dyads(tna_model, cluster = 1)
#' #' }
#' #'
#' #' @seealso `qgraph`
#' #' @export
#' find_dyads <- function(x, cluster = 1, weight_threshold = 0, directed = TRUE,
#'                        plot = FALSE, layout = "spring", sum_weights = FALSE, minimum = 0.05,
#'                        mar = c(10,10,10,10), ...) {
#'   stopifnot_(
#'     is_tna(x),
#'     "Argument {.arg x} must be a {.cls tna} object."
#'   )
#'   # Extract necessary components from the Model
#'   weighted_matrix <- x$transits[[cluster]]
#'   node_colors <- x$colors
#'   node_labels <- x$labels
#'   Pie <- x$inits[[cluster]]
#'
#'   n <- nrow(weighted_matrix)
#'   dyads <- list()
#'   dyad_count <- 0
#'
#'   for (i in 1:(n-1)) {
#'     for (j in (i+1):n) {
#'       if (weighted_matrix[i,j] > weight_threshold && weighted_matrix[j,i] > weight_threshold) {
#'         dyad_count <- dyad_count + 1
#'
#'         if (sum_weights) {
#'           weights <- c(sum(weighted_matrix[i,j], weighted_matrix[j,i]))
#'         } else {
#'           weights <- c(weighted_matrix[i,j], weighted_matrix[j,i])
#'         }
#'
#'         nodes <- node_labels[c(i, j)]
#'         dyad_colors <- node_colors[match(nodes, node_labels)]
#'
#'         dyads[[dyad_count]] <- list(
#'           nodes = nodes,
#'           weights = weights,
#'           model = build_tna(weighted_matrix[c(i,j), c(i,j)], inits = Pie[c(i, j)], colors = dyad_colors),
#'           matrix = weighted_matrix[c(i,j), c(i,j)],
#'           pie_values = Pie[c(i, j)]  # Extract the corresponding Pie values
#'         )
#'       }
#'     }
#'   }
#'
#'   if (dyad_count == 0) {
#'     warning_("No mutual dyads found in the network.")
#'   } else {
#'     info_("Number of mutual dyads: ", dyad_count, "\n")
#'     ol_(id = "dyads")
#'     for (i in 1:length(dyads)) {
#'       li_("Dyad ", i, "\n")
#'       ul_(id = paste0("dyad ",i))
#'       li_("Nodes: ", paste(dyads[[i]]$nodes, collapse = ", "), "\n")
#'       li_("Weights: ", paste(round(dyads[[i]]$weights, 3), collapse = ", "), "\n")
#'
#'       cli_end_(id = paste0("dyad ",i))
#'     }
#'     cli_end_(id = "dyads")
#'     if (plot){
#'       for (i in 1:length(dyads)) {
#'         # Plot the dyad using qgraph with custom colors and Pie values
#'         plot(dyads[[i]]$model, layout = layout, directed = directed,
#'              minimum = minimum, mar = mar, ...)
#'       }
#'     }
#'
#'   }
#'
#'   return(list(count = dyad_count, dyads = dyads))
#' }
#'
#'
#' #' Find Triads in a Transition Network
#' #'
#' #' This function identifies and visualizes triads (sets of three nodes forming a triangle)
#' #' in a transition network from a `tna` object. It can handle both directed
#' #'and undirected networks, and it allows for customizable visualization.
#' #'
#' #' @param x A `tna` object that contains transition matrices and associated node metadata.
#' #' @param cluster An integer specifying which cluster to analyze. Defaults to `1`.
#' #' @param weight_threshold A numeric value that sets the minimum weight for an
#' #' edge to be considered. Edges below this threshold are ignored. Defaults to `0`.
#' #' @param directed A logical value indicating whether the network is directed.
#' #' Defaults to `TRUE`.
#' #' @param plot A logical value specifying whether to plot the triads or not
#' #' @param sum_weights A logical value specifying whether to sum weights for
#' #' directed edges when calculating triangle weights. Defaults to `FALSE`.
#' #' @param minimum A numeric value for the minimum edge weight to be displayed
#' #' in the visualization. Defaults to `0.00001`.
#' #' @param mar A numeric vector specifying the margins for the visualization plot.
#' #' @param ... Additional parameters passed to the `qgraph` function for plotting.
#' #' @family patterns
#' #' @details
#' #' This function loops through all possible sets of three nodes in the network
#' #' and checks whether the edges between them form a complete triangle.
#' #' The `weight_threshold` parameter allows filtering out weak edges, and the
#' #' `sum_weights` option sums directed edge weights.
#' #'
#' #' For each detected triangle, the function stores the nodes, their weights,
#' #' the submatrix corresponding to the triangle, and the initial state
#' #' probabilities (`Pie` values) for the nodes. It then visualizes each triangle
#' #' using the `qgraph` package, allowing customization of the plot via additional
#' #' arguments.
#' #' @author
#' #' Mohammed Saqr (\email{mohammed.saqr@uef.fi})
#' #' @return A list with two elements:
#' #' \itemize{
#' #'   \item `count`: The total number of triangles found.
#' #'   \item `triangles`: A list of detected triangles, where each element contains:
#' #'     \itemize{
#' #'       \item `nodes`: The labels of the nodes forming the triangle.
#' #'       \item `weights`: The edge weights between the nodes.
#' #'       \item `matrix`: The submatrix representing the connections between the
#' #'       nodes in the triangle.
#' #'       \item `pie_values`: The initial state probabilities (`Pie` values) for
#' #'       the nodes in the triangle.
#' #'     }
#' #' }
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Find and visualize triads in the first cluster
#' #' triads <- find_triads(tna_model, cluster = 1)
#' #' }
#' #'
#' #' @seealso `qgraph`
#' #' @export
#' find_triads <- function(x, cluster = 1, weight_threshold = 0, directed = TRUE,
#'                         sum_weights = FALSE, plot = FALSE, minimum = 0.05,
#'                         mar = rep(5, 4), ...) {
#'   stopifnot_(
#'     is_tna(x),
#'     "Argument {.arg x} must be a {.cls tna} object."
#'   )
#'   # Extract necessary components from the Model
#'   weighted_matrix <- x$transits[[cluster]]
#'   node_colors <- x$colors
#'   node_labels <- x$labels
#'   Pie <- x$inits[[cluster]]
#'
#'   n <- nrow(weighted_matrix)
#'   triangles <- list()
#'   triangle_count <- 0
#'
#'   for (i in 1:(n-2)) {
#'     for (j in (i+1):(n-1)) {
#'       for (k in (j+1):n) {
#'         if (directed) {
#'           if ((weighted_matrix[i,j] > weight_threshold &&
#'                weighted_matrix[j,i] > weight_threshold) &&
#'               (weighted_matrix[j,k] > weight_threshold &&
#'                weighted_matrix[k,j] > weight_threshold) &&
#'               (weighted_matrix[k,i] > weight_threshold &&
#'                weighted_matrix[i,k] > weight_threshold)) {
#'
#'             triangle_count <- triangle_count + 1
#'
#'             if (sum_weights) {
#'               weights <- c(sum(weighted_matrix[i,j], weighted_matrix[j,i]),
#'                            sum(weighted_matrix[j,k], weighted_matrix[k,j]),
#'                            sum(weighted_matrix[k,i], weighted_matrix[i,k]))
#'             } else {
#'               weights <- c(weighted_matrix[i,j], weighted_matrix[j,k], weighted_matrix[k,i])
#'             }
#'
#'             nodes <- node_labels[c(i, j, k)]
#'
#'             triangle_colors <- node_colors[match(nodes, node_labels)]
#'
#'             triangles[[triangle_count]] <- list(
#'               nodes = nodes,
#'               weights = weights,
#'               model = build_tna(weighted_matrix[c(i,j,k), c(i,j,k)], inits = Pie[c(i, j, k)], colors = triangle_colors),
#'               matrix = weighted_matrix[c(i,j,k), c(i,j,k)],
#'               pie_values = Pie[c(i, j, k)]  # Extract the corresponding Pie values
#'             )
#'           }
#'         } else {  # undirected
#'           if (weighted_matrix[i,j] > weight_threshold &&
#'               weighted_matrix[j,i] > weight_threshold &&
#'               weighted_matrix[j,k] > weight_threshold &&
#'               weighted_matrix[k,j] > weight_threshold &&
#'               weighted_matrix[k,i] > weight_threshold &&
#'               weighted_matrix[i,k] > weight_threshold) {
#'
#'             triangle_count <- triangle_count + 1
#'
#'             if (sum_weights) {
#'               weights <- c(sum(weighted_matrix[i,j], weighted_matrix[j,i]),
#'                            sum(weighted_matrix[j,k], weighted_matrix[k,j]),
#'                            sum(weighted_matrix[k,i], weighted_matrix[i,k]))
#'             } else {
#'               weights <- c(weighted_matrix[i,j], weighted_matrix[j,k], weighted_matrix[k,i])
#'             }
#'
#'             nodes <- node_labels[c(i, j, k)]
#'             triangle_colors <- node_colors[match(nodes, node_labels)]
#'
#'             triangles[[triangle_count]] <- list(
#'               nodes = nodes,
#'               weights = weights,
#'               model = build_tna(weighted_matrix[c(i,j,k), c(i,j,k)], inits = Pie[c(i, j, k)], colors = triangle_colors),
#'               matrix = weighted_matrix[c(i,j,k), c(i,j,k)],
#'               pie_values = Pie[c(i, j, k)]  # Extract the corresponding Pie values
#'             )
#'           }
#'         }
#'       }
#'     }
#'   }
#'
#'   if (triangle_count == 0) {
#'     warning_("No triangles found in the network.")
#'   } else {
#'     info_(paste0("Number of triangles: ", triangle_count, "\n"))
#'     ol_(id = "triangles")
#'     for (i in 1:length(triangles)) {
#'       li_("Triangle", i, ":\n")
#'       ul_(id = paste0("triangle_",i))
#'       li_("Nodes: ", paste(triangles[[i]]$nodes, collapse = ", "), "\n")
#'       li_("Weights: ", paste(round(triangles[[i]]$weights, 3), collapse = ", "), "\n")
#'
#'       cli_end_(id = paste0("triangle_", i))
#'     }
#'     cli_end_(id = "triangles")
#'     if (plot) {
#'       # Plot the triangle using qgraph with custom colors and Pie values
#'       for (i in 1:length(triangles)) {
#'         plot(triangles[[i]]$model, directed = directed, minimum = minimum, mar = mar, ...)
#'       }
#'     }
#'   }
#'
#'   return(list(count = triangle_count, triangles = triangles))
#' }
#'
#'
#' #' Find Dyads Across All Clusters
#' #'
#' #' This function identifies dyads (sets of two nodes) in all transition matrices
#' #' for each cluster within a `tna` object. It loops through each cluster and
#' #' applies the `find_dyads` function to detect dyads.
#' #'
#' #' @param x A `tna` object containing transition matrices for multiple clusters.
#' #' @param ... Additional arguments passed to the `find_dyads` function.
#' #'
#' #' @details
#' #' The function iterates over the transition matrices stored in the `tna` object
#' #' for each cluster. For each cluster, it prints a message indicating the cluster
#' #' being processed and then applies the `find_dyads` function to detect dyads
#' #' in that cluster.
#' #'
#' #' @return A list where each element contains the dyads identified for a specific
#' #' cluster. Each cluster's result is the output of the `find_dyads` function.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Find dyads across all clusters in a `tna` object
#' #' dyads_result <- find_dyads_clusters(tna_model)
#' #' }
#' #' @seealso `find_dyads`
#' #' @family patterns
#' #' @author
#' #' Sonsoles López-Pernas (\email{sonsoles.lopez@uef.fi})
#' #' @export
#' find_dyads_clusters <- function(x, ...) {
#'   result <- list()
#'   matrices <- x$transits
#'
#'   for (clus in seq_along(matrices)) {
#'     info_(paste0("Dyads for cluster ", clus,"\n"))
#'     result[[clus]] <- find_dyads(x, cluster = clus, ...)
#'   }
#' }
#'
#'
#' #' Find Triads Across All Clusters
#' #'
#' #' This function identifies triangles or triads (sets of thre nodes) in all
#' #' transition matrices for each cluster within a `tna` object. It loops through
#' #' each cluster and applies the `find_triads` function to detect triads
#' #'
#' #' @param x A `tna` object containing transition matrices for multiple clusters.
#' #' @param ... Additional arguments passed to the `find_triads` function.
#' #'
#' #' @details
#' #' The function iterates over the transition matrices stored in the `tna` object
#' #' for each cluster. For each cluster, it prints a message indicating the cluster
#' #' being processed and then applies the `find_triads` function to detect triads in
#' #' that cluster.
#' #'
#' #' @return A list where each element contains the triads identified for a specific
#' #' cluster. Each cluster's result is the output of the `find_triads` function.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Find triads across all clusters in a `tna` object
#' #' triads_result <- find_triads_clusters(tna_model)
#' #' }
#' #' @family patterns
#' #' @seealso `find_triads`
#' #' @author
#' #' Sonsoles López-Pernas (\email{sonsoles.lopez@uef.fi})
#' #' @export
#' find_triads_clusters <- function(x, ...) {
#'   result <- list()
#'   matrices <- x$transits
#'
#'   for (clus in seq_along(matrices)) {
#'     info_(paste0("Triads for cluster ", clus, "\n"))
#'     result[[clus]] <- find_triads(x, cluster = clus, ...)
#'   }
#' }



# # Helper function to check if edge meets threshold criteria and ignore loops in computation
# meets_threshold <- function(weight1, weight2, i, j, sum_weights, weight_threshold) {
#   if (i == j) return(FALSE)  # Never consider loops in the computation
#   if (sum_weights) {
#     return(weight1 + weight2 > weight_threshold)
#   } else {
#     return(weight1 > weight_threshold && weight2 > weight_threshold)
#   }
# }
#
# # Helper function to check if a set of nodes forms a clique, excluding loops from the computation
# is_clique <- function(nodes, sum_weights, weighted_matrix, weight_threshold) {
#   for (i in 1:(length(nodes)-1)) {
#     for (j in (i+1):length(nodes)) {
#       if (!meets_threshold(weighted_matrix[nodes[i], nodes[j]],
#                            weighted_matrix[nodes[j], nodes[i]],
#                            nodes[i], nodes[j], sum_weights, weight_threshold)) {
#         return(FALSE)
#       }
#     }
#   }
#   return(TRUE)
# }

#' Identify and Visualize Cliques in a Transition Network
#'
#' This function identifies cliques of a specified size in a weighted transition network within a `tna` object. It allows for customizable visualization of the cliques and can optionally display loops in the network.
#'
#' @param x An object of type `tna`
#' @param cluster An integer specifying which cluster to analyze. Defaults to `1`.
#' @param n An integer specifying the size of the cliques to identify. Defaults to `3` (triads).
#' @param weight_threshold A numeric value that sets the minimum edge weight for an edge to be considered in the clique. Edges below this threshold are ignored. Defaults to `0`.
#' @param sum_weights A logical value specifying whether to sum the weights of directed edges when determining cliques. Defaults to `FALSE`.
#' @param minimum A numeric value for the minimum edge weight to be displayed in the visualization. Defaults to `0.00001`.
#' @param mar A numeric vector specifying the margins for the visualization plot. Defaults to `c(5,5,5,5)`.
#' @param plot A logical value indicating whether to visualize the identified cliques using the `qgraph` package. Defaults to `TRUE`.
#' @param show_loops A logical value indicating whether self-loops (edges from a node to itself) should be displayed in the visualizations. Defaults to `FALSE`.
#' @param ... Additional parameters passed to the `qgraph` plotting function for customization.
#'
#' @details
#' This function searches for cliques—complete subgraphs where every pair of nodes is connected—of size `n` in the transition matrix for the specified cluster in the `tna` object. The function loops over all combinations of nodes, checking if they form a valid clique based on the edge weights and the provided threshold.
#'
#' If any cliques are found, the function can optionally visualize them using the `qgraph` package. The visualization can be customized with additional arguments passed through the `...` parameter, such as node sizes, edge thickness, and more. The function also allows controlling whether to display self-loops in the visualization with the `show_loops` parameter.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item `count`: The total number of cliques found.
#'   \item `cliques`: A list of detected cliques, where each element contains:
#'     \itemize{
#'       \item `matrix`: The submatrix representing the connections between the nodes in the clique.
#'       \item `pie_values`: The initial state probabilities (`Pie` values) for the nodes in the clique.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Find and visualize 3-cliques (triads) in the first cluster
#' cliques_result <- cliques(my_tna_object, cluster = 1, n = 3)
#'
#' # Find and visualize 4-cliques in the second cluster without showing loops
#' cliques_result <- cliques(my_tna_object, cluster = 2, n = 4, show_loops = FALSE)
#' }
#' @seealso `qgraph`
#' @export
cliques <- function(x, cluster = 1, n = 3, weight_threshold = 0,
                    sum_weights = FALSE, minimum = 0.00001,
                    mar = c(5,5,5,5), plot = TRUE, show_loops = FALSE, ...) {
  # Extract necessary components from the Model
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )

  weighted_matrix <- x$transits[[cluster]]
  node_colors <- x$colors
  node_labels <- x$labels
  Pie <- x$inits[[cluster]]

  num_nodes <- nrow(weighted_matrix)
  cliques <- list()
  clique_count <- 0


  # Generate all possible n-combinations of nodes
  node_combinations <- combn(1:num_nodes, n)

  # Check each combination
  for (i in 1:ncol(node_combinations)) {
    nodes <- node_combinations[,i]
    if (is_clique(nodes, sum_weights, weighted_matrix, weight_threshold)) {
      clique_count <- clique_count + 1

      # Extract submatrix for the clique
      clique_matrix <- weighted_matrix[nodes, nodes]

      # Optionally show loops in the plot, but never consider them in the computation
      if (!show_loops) {
        diag(clique_matrix) <- 0  # Remove self-loops from the matrix if show_loops is FALSE
      }

      # Set row and column names
      rownames(clique_matrix) <- colnames(clique_matrix) <- node_labels[nodes]
      clique_colors <- node_colors[match(rownames(clique_matrix), node_labels)]

      cliques[[clique_count]] <- list(
        matrix = clique_matrix,
        # model = build_tna(clique_matrix, inits = Pie[nodes], colors = clique_colors),
        pie_values = Pie[nodes]
      )
    }
  }

  if (clique_count == 0) {
    warning_(paste("No", n, "-cliques found in the network."))
  } else {
    cat("Number of", n, "-cliques:", clique_count, "\n")
    cat("Cliques:\n")

    for (i in 1:length(cliques)) {
      cat("\nClique", i, ":\n")
      print(round(cliques[[i]]$matrix, 3))

      if (plot) {
        clique_matrix <- cliques[[i]]$matrix
        clique_colors <- node_colors[match(rownames(clique_matrix), node_labels)]
        clique_pie <- cliques[[i]]$pie_values

        # Default plot parameters
        plot_params <- list(
          labels = colnames(clique_matrix),
          edge.labels = TRUE,
          directed = sum_weights,
          edge.label.cex = 1.82,
          mar = mar,
          minimum = minimum,
          theme = "colorblind",
          cut = 0.01,
          vsize = 25,
          color = clique_colors,
          pie = clique_pie
        )

        # Override default parameters with any provided in ...
        plot_params <- modifyList(plot_params, list(...))

        # Call qgraph with all parameters
        do.call(qgraph::qgraph, c(list(clique_matrix), plot_params))
      }
    }
  }

  return(list(count = clique_count, cliques = cliques))
}

cliques2 <- function(x, cluster = 1, n = 3, weight_threshold = 0,
                                sum_weights = FALSE, minimum = 0.00001,
                                show_loops = FALSE, ...) {
  weights <- x$weights[[cluster]]
  labels <- x$labels
  inits <- x$inits[[cluster]]
  mat1 <- mat2 <- weights
  mat1[upper.tri(mat1)] <- 0
  mat2[lower.tri(mat2)] <- 0

  g1 <- igraph::graph_from_adjacency_matrix(mat1, mode = "undirected")
  g2 <- igraph::graph_from_adjacency_matrix(mat2, mode = "undirected")

  cliq1 <- igraph::cliques(g1, min = n, max = n)
  cliq2 <- igraph::cliques(g2, min = n, max = n)
}