#' Prune a `tna` network based on transition probabilities
#'
#' This function prunes a network represented by a `tna` object by removing edges based on a specified threshold, percentile, or lowest percent of non-zero edge weights. It ensures the network remains weakly connected.
#'
#' @param x An object of class `tna`
#' @param threshold A numeric value specifying the edge weight threshold. Edges with weights below or equal to this threshold will be considered for removal.
#' @param percentile A numeric value specifying the percentile of non-zero edges. Edges with weights below or equal to this percentile will be considered for removal.
#' @param lowest_percent A numeric value specifying the lowest percentage of non-zero edges. This percentage of edges with the lowest weights will be considered for removal.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{pruned}}{The pruned `tna` object with updated transition matrix.}
#'   \item{\code{removed_edges}}{A data frame of edges that were removed.}
#'   \item{\code{num_removed_edges}}{The number of edges removed.}
#'   \item{\code{threshold_used}}{The threshold value used for pruning.}
#'   \item{\code{method_used}}{The method used to determine the threshold.}
#' }
#'
#' @details
#' The function prunes the edges of the network(s) based on the specified threshold, percentile, or lowest percent of non-zero edge weights. It ensures that removing an edge does not disconnect the network (i.e., the network remains weakly connected).
#' \describe{
#'   \item{\code{threshold}}{If specified, transition probabilities that are less than or equal to this value will be considered for removal.}
#'   \item{\code{percentile}}{If specified, transition probabilities below this percentile of non-zero edge weights will be considered for removal.}
#'   \item{\code{lowest_percent}}{If specified, the lowest percent of non-zero transition probabilities will be considered for removal.}
#'   \item{\code{Default threshold}}{If no threshold, percentile, or lowest percent is specified, a default threshold of 0.05 is used.}
#' }
#' The function uses a depth-first search to ensure the network remains weakly connected after pruning.
#'
#' @usage
#' prune(x, threshold = NULL, percentile = NULL, lowest_percent = NULL)
#'
#' @examples
#' \dontrun{
#'   tna_model <- build_tna(engagement)
#'   pruned_result <- prune(tna_model, threshold = 0.1)
#'   pruned_result <- prune(tna_model, percentile = 10)
#'   pruned_result <- prune(tna_model, lowest_percent = 5)
#' }
#'
#' @export
prune <- function(x, threshold = NULL, percentile = NULL, lowest_percent = NULL) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )

  # Ensure only one of the threshold, percentile, or lowest_percent parameters is provided
  stopifnot_(
    sum(!is.null(threshold), !is.null(percentile), !is.null(lowest_percent)) <= 1,
    "Only one of 'threshold', 'percentile', or 'lowest_percent' should be provided."
  )

  # Validate input values
  stopifnot_(
    is.null(threshold) || (is.numeric(threshold) && threshold >= 0),
    "Argument 'threshold' must be a non-negative numeric value."
  )
  stopifnot_(
    is.null(percentile) || (is.numeric(percentile) && percentile > 0 && percentile < 100),
    "Argument 'percentile' must be a numeric value between 0 and 100."
  )
  stopifnot_(
    is.null(lowest_percent) || (is.numeric(lowest_percent) && lowest_percent > 0 && lowest_percent < 100),
    "Argument 'lowest_percent' must be a numeric value between 0 and 100."
  )


  matrices <- x$transits
  removed_edges <- list()
  cut_offs <- list()
  methods <- list()

  info_("\nNetwork Pruning Results: \n")
  info_("------------------------\n")

  for (clus in seq_along(matrices)) {
    # Make a copy of the original matrix
    pruned_matrix <- matrices[[clus]]

    # Get non-zero edges
    non_zero_edges <- pruned_matrix[pruned_matrix > 0]

    # Determine the threshold
    if (!is.null(threshold)) {
      cut_off <- threshold
      method <- "User-specified threshold"
    } else if (!is.null(percentile)) {
      cut_off <- stats::quantile(non_zero_edges, probs = percentile/100)
      method <- paste("Lowest", percentile, "percentile of non-zero edges")
    } else if (!is.null(lowest_percent)) {
      sorted_edges <- sort(non_zero_edges)
      num_edges_to_consider <- ceiling(length(sorted_edges) * lowest_percent / 100)
      cut_off <- sorted_edges[num_edges_to_consider]
      method <- paste("Lowest", lowest_percent, "% of non-zero edges")
    } else {
      cut_off <- 0.05
      method <- "Default threshold"
    }

    # Get indices of edges below or equal to threshold
    edges_to_remove <- which(pruned_matrix > 0 & pruned_matrix <= cut_off, arr.ind = TRUE)

    removed_edges[[clus]] <- data.frame()

    # Check each edge for removal
    for (i in 1:nrow(edges_to_remove)) {
      row <- edges_to_remove[i, 1]
      col <- edges_to_remove[i, 2]

      # Temporarily remove the edge
      temp_value <- pruned_matrix[row, col]
      pruned_matrix[row, col] <- 0

      # If removing the edge disconnects the network, restore it
      if (!is_weakly_connected(pruned_matrix)) {
        pruned_matrix[row, col] <- temp_value
      } else {
        # Edge was successfully removed, add it to the list
        removed_edges[[clus]] <- rbind(removed_edges[[clus]], data.frame(from = row, to = col, weight = temp_value))
      }
    }

    if (length(matrices) > 1) {
      info_(paste0("Cluster:", clus, "\n"))
    }
    info_(paste0("Method used: ", method, "\n"))
    info_(paste0("Number of edges removed: ", nrow(removed_edges[[clus]]), "\n"))
    info_(paste0("Number of edges retained: ", sum(pruned_matrix > 0), "\n"))
    if (nrow(removed_edges[[clus]]) == 0) {
      warning_("No edges were removed")
    } else {
      info_("\nRemoved edges: \n")
      print(removed_edges[[clus]])
    }
    if (length(matrices) > 1) {
      info_("------------------------\n")
    }


    x$transits[[clus]] <- pruned_matrix
    cut_offs[[clus]] <- cut_off
    methods[[clus]] <- method
  }
  # Prepare the result
  result <- list(
    pruned = x,
    removed_edges = removed_edges,
    num_removed_edges =  sapply(removed_edges,nrow),
    threshold_used = cut_offs,
    method_used = methods
  )

  return(result)
}

