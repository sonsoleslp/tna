#' Prune a `tna` network based on transition probabilities
#'
#' This function prunes a network represented by a `tna` object by removing
#' edges based on a specified threshold, lowest percent of non-zero edge
#' weights, or the disparity algorithm TODO cite. It ensures the
#' network remains weakly connected.
#'
#' @param x An object of class `tna`
#' @param method A `character` string describing the pruning method.
#' The available options are `"threshold"`, `"lowest"`, `"percentile"`, and
#' `"disparity"`, corresponding to the methods listed in Details. The default
#' is `"threshold"`.
#' @param threshold A numeric value specifying the edge weight threshold.
#' Edges with weights below or equal to this threshold will be considered for
#' removal.
#' @param lowest A `numeric` value specifying the lowest percentage
#' of non-zero edges. This percentage of edges with the lowest weights will be
#' considered for removal. The default is `0.05`.
#' @param alpha A `numeric` value representing the significance level for the
#' disparity filter. Defaults to `0.5`.
#' @family evaluation
#' @return A list containing:
#'
#'   * `pruned` The pruned `tna` object with updated transition matrix.
#'   * `removed_edges` A data frame of edges that were removed.
#'   * `num_removed_edges` The number of edges removed.
#'
#' @examples
#' \dontrun{
#'   tna_model <- build(engagement)
#'   pruned_threshold <- prune(tna_model, method = "threshold", threshold = 0.1)
#'   pruned_percentile <- prune(tna_model,method = "lowest", lowest = 0.05)
#'   pruned_disparity <- prune(tna_model, "disparity", alpha = 0.5)
#' }
#' @export
prune <- function(x, method = c("threshold", "lowest", "disparity"),
                  threshold = 0.1, lowest = 0.05, alpha = 0.5) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  method <- onlyif(is.character(method), tolower(method))
  method <- try(
    match.arg(method, c("threshold", "lowest", "disparity")),
    silent = TRUE
  )
  stopifnot_(
    !inherits(method, "try-error"),
    "Argument {.arg scales} must be either {.val threshold}, {.val lowest},
     or {.val disparity}."
  )
  stopifnot_(
    is.numeric(threshold) && threshold > 0,
    "Argument {.arg threshold} must be a non-negative numeric value."
  )
  stopifnot_(
    is.numeric(lowest) && lowest > 0 && lowest < 1,
    "Argument 'lowest_percent' must be a numeric value between 0 and 1."
  )
  stopifnot_(
    is.null(attr(x, "pruning")),
    "The model has already been pruned."
  )
  # TODO No lables? when?
  labels <- ifelse_(
    is.null(x$labels),
    seq_len(ncol(x$weights[[1]])),
    x$labels
  )
  tmp <- ifelse_(
    method == "disparity",
    prune_disparity(x, alpha, labels),
    prune_default(x, method, threshold, lowest, labels)
  )
  tmp$original <- x$weights
  tmp$active <- TRUE
  x$weights <- tmp$weights
  attr(x, "pruning") <- tmp
  #info_("\nNetwork Pruning Results: \n")
  #info_("------------------------\n")

  # Prepare the result
  # result <- list(
  #   pruned = x,
  #   removed_edges = removed_edges,
  #   num_removed_edges =  sapply(removed_edges,nrow),
  #   threshold_used = cut_offs,
  #   method_used = methods
  # )

  x
}

prune_default <- function(x, method, threshold, lowest, labels) {
  clusters <- x$weights
  n_clust <- length(clusters)
  cut_offs <- numeric(n_clust)
  removed_edges <- vector(mode = "list", length = n_clust)
  num_removed_edges <- integer(n_clust)
  for (clust in seq_len(n_clust)) {
    pruned_matrix <- clusters[[clust]]
    pos_edges_idx <- pruned_matrix > 0
    pos_edges <- pruned_matrix[pos_edges_idx]
    cut_off <- switch(
      method,
      threshold = threshold,
      lowest = stats::quantile(pos_edges, probs = lowest)
    )
    to_remove_idx <- which(
      pos_edges_idx & pruned_matrix <= cut_off,
      arr.ind = TRUE
    )
    n_remove <- nrow(to_remove_idx)
    to_remove_edges <- data.frame(
      from = labels[to_remove_idx[, 1]],
      to = labels[to_remove_idx[, 2]],
      weight = pruned_matrix[to_remove_idx]
    )
    removed_idx <- logical(n_remove)
    for (i in seq_len(n_remove)) {
      row <- to_remove_idx[i, 1]
      col <- to_remove_idx[i, 2]
      tmp <- pruned_matrix
      tmp[row, col] <- 0
      if (is_weakly_connected(tmp)) {
        pruned_matrix <- tmp
        removed_idx[i] <- TRUE
      }
    }
    clusters[[clust]] <- pruned_matrix
    cut_offs[clust] <- cut_off
    removed_edges[[clust]] <- to_remove_edges[removed_idx, ]
    num_removed_edges[clust] <- sum(removed_idx)
  }
  list(
    weights = clusters,
    cut_offs = cut_offs,
    removed = removed_edges,
    num_removed = num_removed_edges,
    method = method
  )
}


# TODO integrate to pruning
# #' Apply Disparity Filter to Transition Matrix in a tna Object
# #'
# #' The `disparity` function applies a disparity filter to the transition matrix
# #' of a specified cluster within a `tna` object and returns a modified `tna`
# #' object with the filtered transition matrix.
# #'
# #' @param x A `tna` object, which contains transition matrices and other relevant data.
# #' @param cluster A numeric value specifying the cluster for which the transition matrix
# #'   should be extracted and processed. Defaults to `1`.
# #' @param alpha A numeric value representing the significance level for the
# #'   disparity filter. Defaults to `0.5`.
# #'
# #' @details
# #' This function extracts the transition matrix of the specified cluster from
# #' the `tna` object, applies the disparity filter from the `backbone` package
# #' with the specified `alpha`, and then multiplies the filtered result with the
# #' original transition matrix. The result is returned as a new `tna` object with
# #' updated transition data.
# #'
# #' @noRd


prune_disparity <- function(x, alpha, labels) {
  clusters <- x$weights
  n_clust <- length(clusters)
  removed_edges <- vector(mode = "list", length = n_clust)
  num_removed_edges <- integer(n_clust)
  for (clust in seq_len(n_clust)) {
    transition_matrix <- clusters[[clust]]
    disparity_filtered <- backbone::disparity(transition_matrix, alpha)
    pruned_matrix <- disparity_filtered * transition_matrix
    dimnames(pruned_matrix) <- dimnames(transition_matrix)
    clusters[[clust]] <- pruned_matrix
    removed_idx <- which(
      pruned_matrix == 0 & transition_matrix != 0,
      arr.ind = TRUE
    )
    num_removed_edges[clust] <- nrow(removed_idx)
    removed_edges[[clust]] <- data.frame(
      from = labels[removed_idx[, 1]],
      to = labels[removed_idx[, 2]],
      weight = c(transition_matrix[removed_idx])
    )
  }
  list(
    weights = clusters,
    alpha = alpha,
    removed_edges = removed_edges,
    num_removed_edges = num_removed_edges,
    method = "disparity"
  )
}

#' Restore a Pruned Transition Network Analysis Model
#'
#' @rdname deprune
#' @export
#' @param x A `tna` object.
#' @param ... Ignored.
deprune <- function(x, ...) {
  UseMethod("deprune")
}

#' @rdname deprune
#' @export
deprune.tna <- function(x, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  tmp <- attr(x, "pruning")
  stopifnot_(
    !is.null(tmp),
    "Argument {.arg x} must have been pruned."
  )
  stopifnot_(
    tmp$active,
    "Pruning must be active for argument {.arg x}."
  )
  tmp <- attr(x, "pruning")
  tmp$active <- FALSE
  x$weights <- tmp$original
  attr(x, "pruning") <- tmp
}

#' Restore Previous Pruning of a Transition Network Analysis Model
#'
#' @rdname reprune
#' @export
#' @param x A `tna` object.
#' @param ... Ignored.
reprune <- function(x, ...) {
  UseMethod("reprune")
}

#' @rdname deprune
#' @export
reprune.tna <- function(x, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  tmp <- attr(x, "pruning")
  stopifnot_(
    !is.null(tmp),
    "Argument {.arg x} must have been pruned."
  )
  stopifnot_(
    !tmp$active,
    "Pruning must not be active for argument {.arg x}."
  )
  tmp$active <- TRUE
  x$weights <- tmp$weights
  attr(x, "pruning") <- tmp
}
