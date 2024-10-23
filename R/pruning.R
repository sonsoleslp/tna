#' Prune a `tna` network based on transition probabilities
#'
#' This function prunes a network represented by a `tna` object by removing
#' edges based on a specified threshold, lowest percent of non-zero edge
#' weights, or the disparity algorithm TODO cite. It ensures the
#' network remains weakly connected.
#'
#' @param x An object of class `tna`
#' @param method A `character` string describing the pruning method.
#' The available options are `"threshold"`, `"lowest"`, `"bootstrap"` and
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
#' @param boot A `tna_bootstrap` object to be used for pruning with method
#' `"boot"`. The method argument is ignored if this argument is supplied.
#' @param ... Arguments passeed to [bootstrap()] when
#' using `metod = "bootstrap"` and when a `tna_bootstrap` is not supplied.
#' @family evaluation
#' @return A list containing:
#'
#'   * `pruned` The pruned `tna` object with updated transition matrix.
#'   * `removed_edges` A data frame of edges that were removed.
#'   * `num_removed_edges` The number of edges removed.
#'
#' @examples
#' \dontrun{
#'   tna_model <- tna(engagement)
#'   pruned_threshold <- prune(tna_model, method = "threshold", threshold = 0.1)
#'   pruned_percentile <- prune(tna_model,method = "lowest", lowest = 0.05)
#'   pruned_disparity <- prune(tna_model, "disparity", alpha = 0.5)
#' }
#' @export
prune <- function(x, method = "threshold", threshold = 0.1, lowest = 0.05,
                  alpha = 0.5, boot = NULL, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  method <- onlyif(is.character(method), tolower(method))
  method <- try(
    match.arg(method, c("threshold", "lowest", "bootstrap", "disparity")),
    silent = TRUE
  )
  stopifnot_(
    !inherits(method, "try-error"),
    "Argument {.arg method} must be either {.val threshold}, {.val lowest},
     {.val bootstrap}, or {.val disparity}."
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
  tmp <- switch(
    method,
    bootstrap = prune_bootstrap(x, boot, ...),
    disparity = prune_disparity(x, alpha, labels),
    prune_default(x, method, threshold, lowest, labels)
  )
  tmp$original <- x$weights
  tmp$active <- TRUE
  x$weights <- tmp$weights
  attr(x, "pruning") <- tmp
  x
}

prune_default <- function(x, method, threshold, lowest, labels) {
  n_clust <- length(x$weights)
  cut_offs <- numeric(n_clust)
  removed <- vector(mode = "list", length = n_clust)
  pruned <- vector(mode = "list", length = n_clust)
  num_removed <- integer(n_clust)
  num_retained <- integer(n_clust)
  for (i in seq_len(n_clust)) {
    weights <- x$weights[[i]]
    pos_edges_idx <- weights > 0
    n_edges <- sum(pos_edges_idx)
    pos_edges <- weights[pos_edges_idx]
    cut_off <- switch(
      method,
      threshold = threshold,
      lowest = stats::quantile(pos_edges, probs = lowest)
    )
    to_remove_idx <- which(
      pos_edges_idx & weights <= cut_off,
      arr.ind = TRUE
    )
    n_remove <- nrow(to_remove_idx)
    to_remove <- data.frame(
      from = labels[to_remove_idx[, 1]],
      to = labels[to_remove_idx[, 2]],
      weight = weights[to_remove_idx]
    )
    removed_idx <- logical(n_remove)
    for (j in seq_len(n_remove)) {
      row <- to_remove_idx[j, 1]
      col <- to_remove_idx[j, 2]
      tmp <- weights
      tmp[row, col] <- 0
      if (is_weakly_connected(tmp)) {
        weights <- tmp
        removed_idx[j] <- TRUE
      }
    }
    pruned[[i]] <- weights
    cut_offs[i] <- cut_off
    removed[[i]] <- to_remove[removed_idx, ]
    num_removed[i] <- sum(removed_idx)
    num_retained[i] <- n_edges - num_removed[i]
  }
  list(
    weights = pruned,
    method = method,
    threshold = threshold,
    lowest = lowest,
    cut_offs = cut_offs,
    removed = removed,
    num_removed = num_removed,
    num_retained = num_retained
  )
}

prune_bootstrap <- function(x, boot, ...) {
  # TODO multiple clusters for bootstrap
  if (is.null(boot)) {
    boot <- bootstrap(x, ...)
  }
  list(
    weights = list(boot$sig_weights),
    method = "bootstrap",
    removed = boot$combined,
    num_removed = boot$removed_edges_summary$num_removed,
    num_retained = boot$removed_edges_summary$num_retained
  )
}


# TODO integrate documentation to pruning docs
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
  n_clust <- length(x$weights)
  removed <- vector(mode = "list", length = n_clust)
  pruned <- vector(mode = "list", length = n_clust)
  num_removed <- integer(n_clust)
  num_retained <- integer(n_clust)
  for (i in seq_len(n_clust)) {
    weights <- x$weights[[i]]
    n_edges <- sum(weights > 0)
    disparity_filtered <- backbone::disparity(weights, alpha)
    weights_pruned <- disparity_filtered * weights
    dimnames(weights_pruned) <- dimnames(weights)
    pruned[[i]] <- weights_pruned
    removed_idx <- which(
      weights_pruned == 0 & weights != 0,
      arr.ind = TRUE
    )
    num_removed[i] <- nrow(removed_idx)
    num_retained[i] <- n_edges - num_removed[i]
    removed[[i]] <- data.frame(
      from = labels[removed_idx[, 1]],
      to = labels[removed_idx[, 2]],
      weight = c(weights[removed_idx])
    )
  }
  list(
    weights = pruned,
    method = "disparity",
    alpha = alpha,
    removed = removed,
    num_removed = num_removed,
    num_retained = num_retained
  )
}

#' Print Detailed Information on the Pruning Results
#'
#' @rdname pruning_details
#' @export
#' @param x A `tna` object.
#' @param removed_edges Should a `data.frame` of removed edges be printed?
#' The default is `FALSE`.
#' @param ... Ignored.
pruning_details <- function(x, ...) {
  UseMethod("pruning_details")
}

#' @rdname pruning_details
#' @export
pruning_details.tna <- function(x,  removed_edges = TRUE, ...) {
  pruning <- attr(x, "pruning")
  stopifnot_(
    !is.null(pruning),
    "Argument {.arg x} must have been pruned."
  )
  method_txt <- switch(
    pruning$method,
    threshold = paste0("User-specified threshold (", pruning$threshold, ")"),
    lowest = paste0("Lowest ", pruning$lowest * 100, "% of non-zero edges"),
    bootstrap = "Bootstrapping",
    disparity = paste0("Disparity filter (alpha = ", pruning$alpha, ")")
  )
  cat("**Pruning Details**\n")
  cat("\nMethod used:", method_txt)
  cat("\nNumber of removed edges:", cs(pruning$num_removed))
  cat("\nNumber of retained edges:", cs(pruning$num_retained))
  n_clust <- length(x$weights)
  has_clust <- n_clust > 1
  clust_names <- names(x$weights)
  cat(
    "\n\n**Removed edges",
    onlyif(has_clust, " by cluster"),
    "**\n\n",
    sep = ""
  )
  for (i in seq_along(n_clust)) {
    onlyif(has_clust, cat(paste0(clust_names[i], ":\n")))
    print(pruning$removed[[i]])
    onlyif(has_clust, cat("\n"))
  }
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
