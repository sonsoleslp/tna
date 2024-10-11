#' Find Dyads in a Transition Network
#'
#' This function identifies and visualizes dyads (sets of two nodes forming a pair)
#' in a transition network from a `tna` object. It can handle both directed
#' and undirected networks, and it allows for customizable visualization.
#'
#' @param x A `tna` object that contains transition matrices and associated node metadata.
#' @param cluster An integer specifying which cluster to analyze. Defaults to `1`.
#' @param weight_threshold A numeric value that sets the minimum weight for an
#' edge to be considered. Edges below this threshold are ignored. Defaults to `0`.
#' @param directed A logical value indicating whether the network is directed.
#' Defaults to `FALSE`.
#' @param sum_weights A logical value specifying whether to sum weights for
#' directed edges when calculating triangle weights. Defaults to `FALSE`.
#' @param plot A logical value specifying whether to plot the dyads or not
#' @param layout The network layout for `qgraph`.
#' @param minimum A numeric value for the minimum edge weight to be displayed
#' in the visualization. Defaults to `0.05`.
#' @param mar A numeric vector specifying the margins for the visualization plot.
#' @param ... Additional parameters passed to the `qgraph` function for plotting.
#' @family patterns
#' @details
#' This function loops through all possible pairs of nodes in the network
#' and checks whether the edges between them form a complete dyad
#' The `weight_threshold` parameter allows filtering out weak edges, and the
#' `sum_weights` option sums directed edge weights.
#'
#' For each detected dyad, the function stores the nodes, their weights,
#' the submatrix corresponding to the dyad, and the initial state
#' probabilities (`Pie` values) for the nodes. It then visualizes each dyad
#' using the `qgraph` package, allowing customization of the plot via additional
#' arguments.
#' @author
#' Mohammed Saqr (\email{mohammed.saqr@uef.fi})
#' @return A list with two elements:
#' \itemize{
#'   \item `count`: The total number of dyads found.
#'   \item `dyads`: A list of detected dyads, where each element contains:
#'     \itemize{
#'       \item `nodes`: The labels of the nodes forming the dyad
#'       \item `weights`: The edge weights between the nodes.
#'       \item `matrix`: The submatrix representing the connections between the
#'       nodes in the dyad
#'       \item `pie_values`: The initial state probabilities (`Pie` values) for
#'       the nodes in the dyad.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Find and visualize dyads in the first cluster
#' dyads <- find_dyads(tna_model, cluster = 1)
#' }
#'
#' @seealso `qgraph`
#' @export
find_dyads <- function(x, cluster = 1, weight_threshold = 0, directed = FALSE,
                       plot = FALSE, layout = "spring", sum_weights = FALSE, minimum = 0.05,
                       mar = c(10,10,10,10), ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  # Extract necessary components from the Model
  weighted_matrix <- x$transits[[cluster]]
  node_colors <- x$colors
  node_labels <- x$labels
  Pie <- x$inits[[cluster]]

  n <- nrow(weighted_matrix)
  dyads <- list()
  dyad_count <- 0

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (weighted_matrix[i,j] > weight_threshold && weighted_matrix[j,i] > weight_threshold) {
        dyad_count <- dyad_count + 1

        if (sum_weights) {
          weights <- c(sum(weighted_matrix[i,j], weighted_matrix[j,i]))
        } else {
          weights <- c(weighted_matrix[i,j], weighted_matrix[j,i])
        }

        nodes <- node_labels[c(i, j)]
        dyad_colors <- node_colors[match(nodes, node_labels)]

        dyads[[dyad_count]] <- list(
          nodes = nodes,
          weights = weights,
          model = build_tna(weighted_matrix[c(i,j), c(i,j)], inits = Pie[c(i, j)], colors = dyad_colors),
          matrix = weighted_matrix[c(i,j), c(i,j)],
          pie_values = Pie[c(i, j)]  # Extract the corresponding Pie values
        )
      }
    }
  }

  if (dyad_count == 0) {
    warning_("No mutual dyads found in the network.")
  } else {
    info_("Number of mutual dyads: ", dyad_count, "\n")
    ol_(id = "dyads")
    for (i in 1:length(dyads)) {
      li_("Dyad ", i, "\n")
      ul_(id = paste0("dyad ",i))
      li_("Nodes: ", paste(dyads[[i]]$nodes, collapse = ", "), "\n")
      li_("Weights: ", paste(round(dyads[[i]]$weights, 3), collapse = ", "), "\n")

      cli_end_(id = paste0("dyad ",i))
    }
    cli_end_(id = "dyads")
    if (plot){
      for (i in 1:length(dyads)) {
        # Plot the dyad using qgraph with custom colors and Pie values
        plot(dyads[[i]]$model, layout = layout, ...)
      }
    }

  }

  return(list(count = dyad_count, dyads = dyads))
}


#' Find Triads in a Transition Network
#'
#' This function identifies and visualizes triads (sets of three nodes forming a triangle)
#' in a transition network from a `tna` object. It can handle both directed
#'and undirected networks, and it allows for customizable visualization.
#'
#' @param x A `tna` object that contains transition matrices and associated node metadata.
#' @param cluster An integer specifying which cluster to analyze. Defaults to `1`.
#' @param weight_threshold A numeric value that sets the minimum weight for an
#' edge to be considered. Edges below this threshold are ignored. Defaults to `0`.
#' @param directed A logical value indicating whether the network is directed.
#' Defaults to `FALSE`.
#' @param plot A logical value specifying whether to plot the triads or not
#' @param sum_weights A logical value specifying whether to sum weights for
#' directed edges when calculating triangle weights. Defaults to `FALSE`.
#' @param minimum A numeric value for the minimum edge weight to be displayed
#' in the visualization. Defaults to `0.00001`.
#' @param mar A numeric vector specifying the margins for the visualization plot.
#' @param ... Additional parameters passed to the `qgraph` function for plotting.
#' @family patterns
#' @details
#' This function loops through all possible sets of three nodes in the network
#' and checks whether the edges between them form a complete triangle.
#' The `weight_threshold` parameter allows filtering out weak edges, and the
#' `sum_weights` option sums directed edge weights.
#'
#' For each detected triangle, the function stores the nodes, their weights,
#' the submatrix corresponding to the triangle, and the initial state
#' probabilities (`Pie` values) for the nodes. It then visualizes each triangle
#' using the `qgraph` package, allowing customization of the plot via additional
#' arguments.
#' @author
#' Mohammed Saqr (\email{mohammed.saqr@uef.fi})
#' @return A list with two elements:
#' \itemize{
#'   \item `count`: The total number of triangles found.
#'   \item `triangles`: A list of detected triangles, where each element contains:
#'     \itemize{
#'       \item `nodes`: The labels of the nodes forming the triangle.
#'       \item `weights`: The edge weights between the nodes.
#'       \item `matrix`: The submatrix representing the connections between the
#'       nodes in the triangle.
#'       \item `pie_values`: The initial state probabilities (`Pie` values) for
#'       the nodes in the triangle.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Find and visualize triads in the first cluster
#' triads <- find_triads(tna_model, cluster = 1)
#' }
#'
#' @seealso `qgraph`
#' @export
find_triads <- function(x, cluster = 1, weight_threshold = 0, directed = FALSE,
                        sum_weights = FALSE, plot = FALSE, minimum = 0.05,
                        mar = rep(5, 4), ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  # Extract necessary components from the Model
  weighted_matrix <- x$transits[[cluster]]
  node_colors <- x$colors
  node_labels <- x$labels
  Pie <- x$inits[[cluster]]

  n <- nrow(weighted_matrix)
  triangles <- list()
  triangle_count <- 0

  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      for (k in (j+1):n) {
        if (directed) {
          if ((weighted_matrix[i,j] > weight_threshold &&
               weighted_matrix[j,i] > weight_threshold) &&
              (weighted_matrix[j,k] > weight_threshold &&
               weighted_matrix[k,j] > weight_threshold) &&
              (weighted_matrix[k,i] > weight_threshold &&
               weighted_matrix[i,k] > weight_threshold)) {

            triangle_count <- triangle_count + 1

            if (sum_weights) {
              weights <- c(sum(weighted_matrix[i,j], weighted_matrix[j,i]),
                           sum(weighted_matrix[j,k], weighted_matrix[k,j]),
                           sum(weighted_matrix[k,i], weighted_matrix[i,k]))
            } else {
              weights <- c(weighted_matrix[i,j], weighted_matrix[j,k], weighted_matrix[k,i])
            }

            nodes <- node_labels[c(i, j, k)]

            triangle_colors <- node_colors[match(nodes, node_labels)]

            triangles[[triangle_count]] <- list(
              nodes = nodes,
              weights = weights,
              model = build_tna(weighted_matrix[c(i,j,k), c(i,j,k)], inits = Pie[c(i, j, k)], colors = triangle_colors),
              matrix = weighted_matrix[c(i,j,k), c(i,j,k)],
              pie_values = Pie[c(i, j, k)]  # Extract the corresponding Pie values
            )
          }
        } else {  # undirected
          if (weighted_matrix[i,j] > weight_threshold &&
              weighted_matrix[j,i] > weight_threshold &&
              weighted_matrix[j,k] > weight_threshold &&
              weighted_matrix[k,j] > weight_threshold &&
              weighted_matrix[k,i] > weight_threshold &&
              weighted_matrix[i,k] > weight_threshold) {

            triangle_count <- triangle_count + 1

            if (sum_weights) {
              weights <- c(sum(weighted_matrix[i,j], weighted_matrix[j,i]),
                           sum(weighted_matrix[j,k], weighted_matrix[k,j]),
                           sum(weighted_matrix[k,i], weighted_matrix[i,k]))
            } else {
              weights <- c(weighted_matrix[i,j], weighted_matrix[j,k], weighted_matrix[k,i])
            }

            nodes <- node_labels[c(i, j, k)]
            triangle_colors <- node_colors[match(nodes, node_labels)]

            triangles[[triangle_count]] <- list(
              nodes = nodes,
              weights = weights,
              model = build_tna(weighted_matrix[c(i,j,k), c(i,j,k)], inits = Pie[c(i, j, k)], colors = triangle_colors),
              matrix = weighted_matrix[c(i,j,k), c(i,j,k)],
              pie_values = Pie[c(i, j, k)]  # Extract the corresponding Pie values
            )
          }
        }
      }
    }
  }

  if (triangle_count == 0) {
    warning_("No triangles found in the network.")
  } else {
    info_(paste0("Number of triangles: ", triangle_count, "\n"))
    ol_(id = "triangles")
    for (i in 1:length(triangles)) {
      li_("Triangle", i, ":\n")
      ul_(id = paste0("triangle_",i))
      li_("Nodes: ", paste(triangles[[i]]$nodes, collapse = ", "), "\n")
      li_("Weights: ", paste(round(triangles[[i]]$weights, 3), collapse = ", "), "\n")

      cli_end_(id = paste0("triangle_", i))
    }
    cli_end_(id = "triangles")
    if (plot) {
      # Plot the triangle using qgraph with custom colors and Pie values
      for (i in 1:length(triangles)) {
        plot(triangles[[i]]$model, ...)
      }
    }
  }

  return(list(count = triangle_count, triangles = triangles))
}


#' Find Dyads Across All Clusters
#'
#' This function identifies dyads (sets of two nodes) in all transition matrices
#' for each cluster within a `tna` object. It loops through each cluster and
#' applies the `find_dyads` function to detect dyads.
#'
#' @param x A `tna` object containing transition matrices for multiple clusters.
#' @param ... Additional arguments passed to the `find_dyads` function.
#'
#' @details
#' The function iterates over the transition matrices stored in the `tna` object
#' for each cluster. For each cluster, it prints a message indicating the cluster
#' being processed and then applies the `find_dyads` function to detect dyads
#' in that cluster.
#'
#' @return A list where each element contains the dyads identified for a specific
#' cluster. Each cluster's result is the output of the `find_dyads` function.
#'
#' @examples
#' \dontrun{
#' # Find dyads across all clusters in a `tna` object
#' dyads_result <- find_dyads_clusters(tna_model)
#' }
#' @seealso `find_dyads`
#' @family patterns
#' @author
#' Sonsoles López-Pernas (\email{sonsoles.lopez@uef.fi})
#' @export
find_dyads_clusters <- function(x, ...) {
  result <- list()
  matrices <- x$transits

  for (clus in seq_along(matrices)) {
    info_(paste0("Dyads for cluster ", clus,"\n"))
    result[[clus]] <- find_dyads(x, cluster = clus, ...)
  }
}


#' Find Triads Across All Clusters
#'
#' This function identifies triangles or triads (sets of thre nodes) in all
#' transition matrices for each cluster within a `tna` object. It loops through
#' each cluster and applies the `find_triads` function to detect triads
#'
#' @param x A `tna` object containing transition matrices for multiple clusters.
#' @param ... Additional arguments passed to the `find_triads` function.
#'
#' @details
#' The function iterates over the transition matrices stored in the `tna` object
#' for each cluster. For each cluster, it prints a message indicating the cluster
#' being processed and then applies the `find_triads` function to detect triads in
#' that cluster.
#'
#' @return A list where each element contains the triads identified for a specific
#' cluster. Each cluster's result is the output of the `find_triads` function.
#'
#' @examples
#' \dontrun{
#' # Find triads across all clusters in a `tna` object
#' triads_result <- find_triads_clusters(tna_model)
#' }
#' @family patterns
#' @seealso `find_triads`
#' @author
#' Sonsoles López-Pernas (\email{sonsoles.lopez@uef.fi})
#' @export
find_triads_clusters <- function(x, ...) {
  result <- list()
  matrices <- x$transits

  for (clus in seq_along(matrices)) {
    info_(paste0("Triads for cluster ", clus, "\n"))
    result[[clus]] <- find_triads(x, cluster = clus, ...)
  }
}
