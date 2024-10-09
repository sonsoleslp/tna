#' @export
find_triads <- function(x, cluster = 1, weight_threshold = 0, directed = FALSE, sum_weights = FALSE, minimum = 0.00001,  mar = c(20, 20, 20, 20), ...) {
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

            triangles[[triangle_count]] <- list(
              nodes = nodes,
              weights = weights,
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

            triangles[[triangle_count]] <- list(
              nodes = nodes,
              weights = weights,
              matrix = weighted_matrix[c(i,j,k), c(i,j,k)],
              pie_values = Pie[c(i, j, k)]  # Extract the corresponding Pie values
            )
          }
        }
      }
    }
  }

  if (triangle_count == 0) {
    warning("No triangles found in the network.")
  } else {
    cat("Number of triangles:", triangle_count, "\n")
    cat("Triangles:\n")

    for (i in 1:length(triangles)) {
      cat("Triangle", i, ":\n")
      cat("  Nodes:", triangles[[i]]$nodes, "\n")
      cat("  Weights:", round(triangles[[i]]$weights, 3), "\n")

      # Extract the matrix for the triangle
      triangle_matrix <- triangles[[i]]$matrix

      # Match colors for the nodes in this triangle
      triangle_colors <- node_colors[match(triangles[[i]]$nodes, node_labels)]

      # Extract the Pie values for the nodes in this triangle
      triangle_pie <- triangles[[i]]$pie_values

      # Plot the triangle using qgraph with custom colors and Pie values
      qgraph::qgraph(triangle_matrix,
             labels = triangles[[i]]$nodes,
             edge.labels = TRUE,
             directed = directed,
             edge.label.cex=1.82,
             mar = mar,
             minimum = minimum,
             theme = "colorblind",  # Apply colorblind-friendly theme
             cut = 0.01,  # Set the cut-off threshold
             vsize = 25,   # Set the vertex size
             color = triangle_colors,  # Use custom colors for the nodes
             pie = triangle_pie, # Use Pie values specific to the triangle's nodes
             ...
      )
    }
  }

  return(list(count = triangle_count, triangles = triangles))
}

