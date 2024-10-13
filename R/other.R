#' Normalize `x` to the unit interval from 0 to 1.
#'
#' @param x A `numeric` vector.
#' @param na.rm A `logical` value indicating whether missing values
#'   should be removed.
#' @noRd
ranger <- function(x, na.rm = FALSE) {
  mi <- min(x, na.rm)
  ma <- max(x, na.rm)
  (x + mi) / (ma - mi)
}


#' Check Weak Connectivity of an Adjacency Matrix
#'
#' This function checks if an adjacency matrix represents a weakly connected graph. A graph is considered weakly connected if there is a path between any two vertices when ignoring the direction of edges.
#'
#' @param mat A square adjacency matrix representing the graph.
#'
#' @return A logical value indicating whether the graph is weakly connected (`TRUE`) or not (`FALSE`).
#' @noRd
is_weakly_connected <- function(mat) {
  n <- nrow(mat)
  visited <- rep(FALSE, n)
  stack <- c(1)
  visited[1] <- TRUE

  while (length(stack) > 0) {
    v <- stack[length(stack)]
    stack <- stack[-length(stack)]

    neighbors <- which(mat[v, ] > 0 | mat[, v] > 0)
    for (u in neighbors) {
      if (!visited[u]) {
        visited[u] <- TRUE
        stack <- c(stack, u)
      }
    }
  }

  all(visited)
}



#' Shorthand for `try(., silent = TRUE)`
#'
#' @param expr An \R expression to try.
#' @noRd
try_ <- function(expr) {
  try(expr, silent = TRUE)
}

#' Check that argument is an object of class `"tna"`
#'
#' @param x An \R object.
#' @noRd
is_tna <- function(x) {
  inherits(x, "tna")
}

#' Check that argument is an object of class `"centralities"`
#'
#' @param x An \R object.
#' @noRd
is_centralities <- function(x) {
  inherits(x, "centralities")
}

# Functions borrowed from the `dynamite` package --------------------------
# https://github.com/ropensci/dynamite

#' Shorthand for `if (test) yes else no`
#'
#' @param test A `logical` value of the condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @param no An \R object to return when `test` evaluates to `FALSE`.
#' @noRd
ifelse_ <- function(test, yes, no) {
  if (test) {
    yes
  } else {
    no
  }
}

#' Return `yes` if `test` is `TRUE`, otherwise return `NULL`
#'
#' @param test \[`logical(1)`] Condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @noRd
onlyif <- function(test, yes) {
  if (test) {
    yes
  } else {
    NULL
  }
}

#' Generate a Warning Message
#'
#' @param message See [cli::cli_warn()].
#' @param ... See [cli::cli_warn()].
#' @noRd
warning_ <- function(message, ...) {
  cli::cli_warn(message, ..., .envir = parent.frame())
}


#' Generate an Info Message
#'
#' @param message See [cli::cli_inform()].
#' @param ... See [cli::cli_inform()].
#' @noRd
info_ <- function(message, ...) {
  cli::cli_text(message, ..., .envir = parent.frame())
}

#' Generate a list element Message
#'
#' @param message See [cli::cli_li()].
#' @param ... See [cli::cli_li()].
#' @noRd
li_ <- function(message, ...) {
  cli::cli_li(paste(message, ... , collapse=""), .envir = parent.frame())
}

#' Print message
#'
#' @param message See [cli::cat_print()].
#' @param ... See [cli::cat_print()].
#' @noRd
print_ <- function(message, ...) {
  cli::cat_print(message, ...)
}


#' Generate a list Message
#'
#' @param message See [cli::cli_ul()].
#' @param id See [cli::cli_ul()].
#' @param ... See [cli::cli_ul()].
#' @noRd
ul_ <- function (id, ...) {
  cli::cli_ul(id = id, ..., .envir = parent.frame())
}

#' Generate a numbered list Message
#'
#' @param message See [cli::cli_ol()].
#' @param id See [cli::cli_ol()].
#' @param ... See [cli::cli_ol()].
#' @noRd
ol_ <- function (id, ...) {
  cli::cli_ol(id = id, ..., .envir = parent.frame())
}

#' End a list Message
#'
#' @param id See [cli::cli_end()].
#' @param ... See [cli::cli_end()].
#' @noRd
cli_end_ <- function( id, ...) {
  cli::cli_end(id = id)
}

#' Stop function execution unless a condition is true
#'
#' @param message See [cli::cli_abort()].
#' @param ... See [cli::cli_abort()].
#' @param call See [cli::cli_abort()].
#' @noRd
stopifnot_ <- function(cond, message, ..., call = rlang::caller_env()) {
  if (!cond) {
    cli::cli_abort(message, ..., .envir = parent.frame(), call = call)
  }
}


#' Map community assignments to a color palette.
#'
#' This function takes a vector of community assignments (numeric or categorical) and maps them to corresponding colors
#' from a provided palette. If all values in the input vector are the same, the function maps all of them to the first
#' color in the palette. Otherwise, it normalizes the values to ensure they span across the entire palette.
#'
#' @param x A numeric vector representing the community assignments.
#' @param palette A vector of colors to be used for mapping. The length of the palette defines the range of possible colors.
#'
#' @return A vector of colors corresponding to the input values, either a single color or a gradient of colors based on
#' the values in the input vector.
#'
#' @details
#' - If the input vector `x` contains only one unique value, all elements will be mapped to the first color in the palette.
#' - If the input vector `x` contains multiple unique values, these values are scaled linearly to cover the entire range of
#'   the provided palette, ensuring that higher values in the input correspond to later colors in the palette.
#' - The scaling formula used for normalization is:
#'   \deqn{scaled\_values = \left\lfloor \frac{(x - \text{min}(x))}{(\text{max}(x) - \text{min}(x))} \times (\text{length}(palette) - 1) \right\rfloor + 1}
#'
#' @examples
#' # Example usage with numeric input
#' x <- c(1, 2, 3, 4, 5)
#' palette <- c("red", "green", "blue", "yellow", "purple")
#' colors <- map_to_color(x, palette)
#'
#' # Example with a single unique value
#' x <- c(1, 1, 1)
#' colors <- map_to_color(x, palette)
#'
#' @noRd
map_to_color <- function(x, palette) {
  if (length(unique(x)) == 1) {
    # Handle case where all values are the same
    return(rep(palette[1], length(x)))  # Map to the first color in the palette
  } else {
    # Normalize the numeric values to a range from 1 to the length of the palette
    min_val <- min(x)
    max_val <- max(x)
    scaled_values <- as.integer((x - min_val) / (max_val - min_val) * (length(palette) - 1)) + 1
    return(palette[scaled_values])
  }
}


#' Get the Model from TNA Results
#'
#' This function extracts and returns the `tna` model object from a  results list
#' The `tna` model is typically stored within the results of an analysis
#' conducted with the `tna` package.
#'
#' @param results An object containing the results of a network analysis,
#'   including the model and other relevant information.
#' @return The  `tna`  model object extracted from the results.
#' @examples
#' \dontrun{
#' # Assuming `results` is a tna object
#' model <- get_model(results)
#' }
#' @export
get_model <- function(results) {
  stopifnot_(
    is_tna(results$model),
    "The argument {.arg results} must contain a property named model that is a {.cls tna} object."
  )
  results$model
}

#' Calculate Network Metrics for a Cluster
#'
#' This function calculates a variety of network metrics for a specified cluster
#' in a transition network stored in a `tna` object. It computes key metrics such
#' as node and edge counts, network density, mean distance, strength measures,
#' degree centrality, and reciprocity. A histogram of edge weights is also plotted.
#'
#' @param x A `tna` object
#' @param cluster An integer specifying which cluster to analyze. Defaults to `1`.
#'
#' @details
#' The function extracts the `igraph` network for the specified cluster and computes the following network metrics:
#' \itemize{
#'   \item Node count: Total number of nodes in the network.
#'   \item Edge count: Total number of edges in the network.
#'   \item Network density: Proportion of possible edges that are present in the network.
#'   \item Mean distance: The average shortest path length between nodes.
#'   \item Mean and standard deviation of out-strength and in-strength: Measures of the total weight of outgoing and incoming edges for each node.
#'   \item Mean and standard deviation of out-degree: The number of outgoing edges from each node.
#'   \item Centralization of out-degree and in-degree: Measures of how centralized the network is based on the degrees of nodes.
#'   \item Reciprocity: The proportion of edges that are reciprocated (i.e., mutual edges between nodes).
#' }
#'
#' A summary of the metrics is printed to the console, and a histogram of edge weights (probabilities) is plotted.
#'
#' @return A named list containing the following network metrics:
#' \itemize{
#'   \item `Node_count`: The total number of nodes.
#'   \item `Edge_count`: The total number of edges.
#'   \item `Network_Density`: The density of the network.
#'   \item `Mean_distance`: The mean shortest path length.
#'   \item `Mean_out_strength`: The mean out-strength of nodes.
#'   \item `SD_out_strength`: The standard deviation of out-strength.
#'   \item `Mean_in_strength`: The mean in-strength of nodes.
#'   \item `SD_in_strength`: The standard deviation of in-strength.
#'   \item `Mean_out_degree`: The mean out-degree of nodes.
#'   \item `SD_out_degree`: The standard deviation of out-degree.
#'   \item `Centralization_out_degree`: The centralization of out-degree.
#'   \item `Centralization_in_degree`: The centralization of in-degree.
#'   \item `Reciprocity`: The reciprocity of the network.
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate network metrics for the first cluster
#' network_metrics <- calculate_network_metrics(tna_model, cluster = 1)
#' }
#' @seealso `igraph`
#' @export
calculate_network_metrics <- function(x, cluster = 1) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  network <- x$igraph_network[[cluster]]

  # Node and Edge Counts
  Node_count <- igraph::vcount(network)
  Edge_count <- igraph::ecount(network)

  # Network density and path length
  Network_Density <- min(igraph::edge_density(network), 1)  # Ensure density does not exceed 1
  Mean_distance <- igraph::mean_distance(network)

  # Strength measures (for weighted networks) - Focus on out-strength and in-strength
  Mean_out_strength <- mean(igraph::strength(network, mode = "out"), na.rm = TRUE)
  SD_out_strength <- sd(igraph::strength(network, mode = "out"), na.rm = TRUE)
  Mean_in_strength <- mean(igraph::strength(network, mode = "in"), na.rm = TRUE)
  SD_in_strength <- sd(igraph::strength(network, mode = "in"), na.rm = TRUE)

  # Degree measures (mean for out-degree)
  Mean_out_degree <- mean(igraph::degree(network, mode = "out"))
  SD_out_degree <- stats::sd(igraph::degree(network, mode = "out"))

  # Centralization measures (out-degree and in-degree)
  Centralization_out_degree <- igraph::centr_degree(network, mode = "out", loops = FALSE)$centralization
  Centralization_in_degree <- igraph::centr_degree(network, mode = "in", loops = FALSE)$centralization

  # Reciprocity (proportion of mutual edges)
  Reciprocity <- igraph::reciprocity(network)

  # Print important metrics with informative output (focusing on out and in measures)
  cat("Network Metrics Summary (Out and In Measures):\n")
  cat("--------------------------------------\n")
  cat("Node Count: ", Node_count, "\n")
  cat("Edge Count: ", Edge_count, "\n")
  cat("Network Density: ", round(Network_Density, 4), "\n")
  cat("Mean Distance: ", round(Mean_distance, 4), "\n")
  cat("Mean Out-Strength: ", round(Mean_out_strength, 4), "\n")
  cat("SD Out-Strength: ", round(SD_out_strength, 4), "\n")
  cat("Mean In-Strength: ", round(Mean_in_strength, 4), "\n")
  cat("SD In-Strength: ", round(SD_in_strength, 4), "\n")
  cat("Mean Out-Degree: ", round(Mean_out_degree, 4), "\n")
  cat("SD Out-Degree: ", round(SD_out_degree, 4), "\n")
  cat("Centralization (Out-Degree): ", round(Centralization_out_degree, 4), "\n")
  cat("Centralization (In-Degree): ", round(Centralization_in_degree, 4), "\n")
  cat("Reciprocity: ", round(Reciprocity, 4), "\n")
  cat("--------------------------------------\n\n")

  # Plot histogram of edge weights
  edge_weights <- igraph::E(network)$weight
  hist(edge_weights, breaks = seq(0, 1, by=0.05), col = "lightblue",
       main = "Histogram of Edge Weights", xlab = "Edge Weights (Probabilities)",
       ylab = "Frequency", border = "white")

  # Return the results in a named list for further use
  return(list(
    Node_count = Node_count,
    Edge_count = Edge_count,
    Network_Density = Network_Density,
    Mean_distance = Mean_distance,
    Mean_out_strength = Mean_out_strength,
    SD_out_strength = SD_out_strength,
    Mean_in_strength = Mean_in_strength,
    SD_in_strength = SD_in_strength,
    Mean_out_degree = Mean_out_degree,
    SD_out_degree = SD_out_degree,
    Centralization_out_degree = Centralization_out_degree,
    Centralization_in_degree = Centralization_in_degree,
    Reciprocity = Reciprocity
  ))
}
