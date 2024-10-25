#' Calculate Summary of Network Metrics for a Transition Network
#'
#' This function calculates a variety of network metrics for a `tna` object.
#' It computes key metrics such as node and edge counts, network density,
#' mean distance, strength measures, degree centrality, and reciprocity.
#'
#' @export
#' @param object A `tna` object.
#' @param digits An `integer` specifying how many digits to show.
#' @param ... Ignored
#'
#' @details
#' The function extracts the `igraph` network for the specified cluster and
#' computes the following network metrics:
#'
#'   * Node count: Total number of nodes in the network.
#'   * Edge count: Total number of edges in the network.
#'   * Network density: Proportion of possible edges that
#'     are present in the network.
#'   * Mean distance: The average shortest path length between nodes.
#'   * Mean and standard deviation of out-strength and in-strength: Measures
#'     of the total weight of outgoing and incoming edges for each node.
#'   * Mean and standard deviation of out-degree: The number of outgoing
#'     edges from each node.
#'   * Centralization of out-degree and in-degree: Measures of how
#'     centralized the network is based on the degrees of nodes.
#'   * Reciprocity: The proportion of edges that are reciprocated
#'     (i.e., mutual edges between nodes).
#'
#' A summary of the metrics is printed to the console.
#'
#' @return A named `list` containing the following network metrics (invisibly):
#'
#'   * `node_count`: The total number of nodes.
#'   * `edge_count`: The total number of edges.
#'   * `network_Density`: The density of the network.
#'   * `mean_distance`: The mean shortest path length.
#'   * `mean_out_strength`: The mean out-strength of nodes.
#'   * `sd_out_strength`: The standard deviation of out-strength.
#'   * `mean_in_strength`: The mean in-strength of nodes.
#'   * `sd_in_strength`: The standard deviation of in-strength.
#'   * `mean_out_degree`: The mean out-degree of nodes.
#'   * `sd_out_degree`: The standard deviation of out-degree.
#'   * `centralization_out_degree`: The centralization of out-degree.
#'   * `centralization_in_degree`: The centralization of in-degree.
#'   * `reciprocity`: The reciprocity of the network.
#'
#' @examples
#' model <- tna(engagement)
#' summarys(model)
#'
summary.tna <- function(object,
                        digits =  max(3, getOption("digits") - 3L), ...) {
  stopifnot_(
    is_tna(object),
    "Argument {.arg object} must be a {.cls tna} object."
  )
  weights <- object$weights
  g <- as.igraph(object)
  in_strength <- igraph::strength(g, mode = "out")
  out_strength <- igraph::strength(g, mode = "in")
  out_degree <- igraph::degree(g, mode = "out")
  cent_out <- igraph::centr_degree(g, mode = "out", loops = FALSE)
  cent_in <- igraph::centr_degree(g, mode = "in", loops = FALSE)
  out <- c(
    node_count = ncol(weights),
    edge_count = sum(weights > 0),
    network_density = min(igraph::edge_density(g), 1),
    mean_distance = igraph::mean_distance(g),
    mean_out_strength = mean(out_strength, na.rm = TRUE),
    sd_out_strength = stats::sd(out_strength, na.rm = TRUE),
    mean_in_strength = mean(in_strength, na.rm = TRUE),
    sd_in_strength = stats::sd(in_strength, na.rm = TRUE),
    mean_out_degree = mean(out_degree),
    sd_out_degree = stats::sd(out_degree),
    centralization_out_degree = cent_out$centralization,
    centralization_in_degree = cent_in$centralization,
    reciprocity = igraph::reciprocity(g)
  )
  out_names <- c(
    "Node Count",
    "Edge Count",
    "Network Density",
    "Mean Distance",
    "Mean Out-Strength",
    "SD Out-Strength",
    "Mean In-Strength",
    "SD In-Strength",
    "Mean Out-Degree",
    "SD Out-Degree",
    "Centralization (Out-Degree)",
    "Centralization (In-Degree)",
    "Reciprocity"
  )
  out <- tibble::as_tibble(
    data.frame(metric = out_names, value = round(out, digits))
  )
  out
}