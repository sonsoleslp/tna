#' Prune a Transition Network based on Transition Probabilities
#'
#' Prunes a network represented by a `tna` object by removing
#' edges based on a specified threshold, lowest percent of non-zero edge
#' weights, or the disparity filter algorithm (Serrano et al., 2009).
#' It ensures the network remains weakly connected.
#'
#' @export
#' @family validation
#' @param x An object of class `tna` or `group_tna`.
#' @param ... Potential additional arguments passed to the pruning method.
prune <- function(x, ...) {
  UseMethod("prune")
}


#' Prune a Transition Network based on Transition Probabilities
#'
#' Prunes a network represented by a `tna` object by removing
#' edges based on a specified threshold, lowest percent of non-zero edge
#' weights, or the disparity filter algorithm (Serrano et al., 2009).
#' It ensures the network remains weakly connected.
#'
#' @export
#' @family validation
#' @rdname prune
#' @param x An object of class `tna` or `group_tna`
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
#' @param level A `numeric` value representing the significance level for the
#' disparity filter. Defaults to `0.5`.
#' @param boot A `tna_bootstrap` object to be used for pruning with method
#' `"boot"`. The method argument is ignored if this argument is supplied.
#' @param ... Arguments passed to [bootstrap()] when
#' using `method = "bootstrap"` and when a `tna_bootstrap` is not supplied.
#' @return A pruned `tna` or `group_tna` object. Details on the pruning can be
#' viewed with [pruning_details()]. The original model can be restored with
#' [deprune()].
#' @examples
#' model <- tna(group_regulation)
#' pruned_threshold <- prune(model, method = "threshold", threshold = 0.1)
#' pruned_percentile <- prune(model, method = "lowest", lowest = 0.05)
#' pruned_disparity <- prune(model, method = "disparity", level = 0.5)
#'
prune.tna <- function(x, method = "threshold", threshold = 0.1, lowest = 0.05,
                      level = 0.5, boot = NULL, ...) {
  check_missing(x)
  check_class(x, "tna")
  method <- check_match(
    method,
    c("threshold", "lowest", "bootstrap", "disparity")
  )
  check_values(threshold, type = "numeric")
  check_range(lowest)
  check_range(level)
  stopifnot_(
    is.null(attr(x, "pruning")),
    "The model has already been pruned."
  )
  labels <- ifelse_(
    is.null(x$labels),
    seq_len(nodes(x)),
    x$labels
  )
  tmp <- switch(method,
    bootstrap = prune_bootstrap(x, boot, ...),
    disparity = prune_disparity(x, level, labels),
    prune_default(x, method, threshold, lowest, labels)
  )
  tmp$original <- x$weights
  tmp$active <- TRUE
  x$weights <- tmp$weights
  attr(x, "pruning") <- tmp
  x
}

prune_default <- function(x, method, threshold, lowest, labels) {
  weights <- x$weights
  pos_edges_idx <- weights > 0
  n_edges <- sum(pos_edges_idx)
  pos_edges <- weights[pos_edges_idx]
  cut_off <- switch(method,
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
  pruned <- weights
  removed <- to_remove[removed_idx, ]
  num_removed <- sum(removed_idx)
  num_retained <- n_edges - num_removed
  list(
    weights = pruned,
    method = method,
    threshold = threshold,
    lowest = lowest,
    cut_off = cut_off,
    removed = removed,
    num_removed = num_removed,
    num_retained = num_retained
  )
}

prune_bootstrap <- function(x, boot, ...) {
  if (is.null(boot)) {
    boot <- bootstrap(x, ...)
  }
  attr(boot$model, "pruning")
}

prune_disparity <- function(x, level, labels) {
  weights <- x$weights
  n_edges <- sum(weights > 0)
  disparity_filtered <- disparity_filter(weights, level)
  weights_pruned <- disparity_filtered * weights
  dimnames(weights_pruned) <- dimnames(weights)
  pruned <- weights_pruned
  removed_idx <- which(
    weights_pruned == 0 & weights != 0,
    arr.ind = TRUE
  )
  num_removed <- nrow(removed_idx)
  num_retained <- n_edges - num_removed
  removed <- data.frame(
    from = labels[removed_idx[, 1]],
    to = labels[removed_idx[, 2]],
    weight = c(weights[removed_idx])
  )
  list(
    weights = pruned,
    method = "disparity",
    level = level,
    removed = removed,
    num_removed = num_removed,
    num_retained = num_retained
  )
}

#' Print Detailed Information on the Pruning Results
#'
#' @export
#' @family validation
#' @rdname pruning_details
#' @param x A `tna` or `group_tna` object.
#' @param ... Ignored.
#' @return A `data.frame` containing the removed edges if `x` is a `tna` object,
#' or a `list` of `data.frame` objects in the case of `group_tna` object.
#' @examples
#' model <- tna(group_regulation)
#' pruned_threshold <- prune(model, method = "threshold", threshold = 0.1)
#' pruning_details(pruned_threshold)
#'
pruning_details <- function(x, ...) {
  UseMethod("pruning_details")
}

#' @export
#' @rdname pruning_details
pruning_details.tna <- function(x, ...) {
  pruning <- attr(x, "pruning")
  stopifnot_(
    !is.null(pruning),
    "Argument {.arg x} must have been pruned."
  )
  method_txt <- switch(pruning$method,
    threshold = paste0("User-specified threshold (", pruning$threshold, ")"),
    lowest = paste0("Lowest ", pruning$lowest * 100, "% of non-zero edges"),
    bootstrap = "Bootstrapping",
    disparity = paste0("Disparity filter (sig. level = ", pruning$level, ")")
  )

  cat("**Pruning Details**\n")
  cat("\nMethod used:", method_txt)
  cat("\nNumber of removed edges:", cs(pruning$num_removed))
  cat("\nNumber of retained edges:", cs(pruning$num_retained))
  cat("\n\n**Removed edges**\n\n")
  print(pruning$removed)
}

#' Restore a Pruned Transition Network Analysis Model
#'
#' @export
#' @family validation
#' @rdname deprune
#' @param x A `tna` or `group_tna` object.
#' @param ... Ignored.
#' @return A `tna` or `group_tna` object that has not been pruned.
#' @examples
#' model <- tna(group_regulation)
#' pruned_model <- prune(model, method = "threshold", threshold = 0.1)
#' depruned_model <- deprune(pruned_model) # restore original model
#'
deprune <- function(x, ...) {
  UseMethod("deprune")
}

#' @export
#' @rdname deprune
deprune.tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna")
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
  x
}

#' Restore Previous Pruning of a Transition Network Analysis Model
#'
#' @export
#' @family validation
#' @rdname reprune
#' @param x A `tna` or `group_tna` object.
#' @param ... Ignored.
#' @return A `tna` or `group_tna` object that has not been pruned. The previous
#' pruning result can be reactivated with [reprune()].
#' @examples
#' model <- tna(group_regulation)
#' pruned_model <- prune(model, method = "threshold", threshold = 0.1)
#' depruned_model <- deprune(pruned_model) # restore original model
#' repruned_model <- reprune(depruned_model) # reapply the previous pruning
#'
reprune <- function(x, ...) {
  UseMethod("reprune")
}

#' @export
#' @rdname deprune
reprune.tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "tna")
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

#' Disparity Filter Algorithm
#'
#' @param mat A weighted adjacency `matrix` of a directed graph.
#' @param level A `numeric` value for the significance level.
#' @noRd
disparity_filter <- function(mat, level) {
  d <- dim(mat)[2]
  idx_mat <- 1L * (mat > 0)
  out_edges <- mat / .rowSums(mat, m = d, n = d)
  out_degree <- .rowSums(idx_mat, m = d, n = d)
  out_p_values <- (1 - out_edges)^(out_degree - 1)
  in_edges <- t(mat) / .colSums(mat, m = d, n = d)
  in_degree <- .colSums(idx_mat, m = d, n = d)
  in_p_values <- t((1 - in_edges)^(in_degree - 1))
  p_values <- pmin(out_p_values, in_p_values)
  sig <- 1 * (p_values < level)
  diag(sig) <- 0
  sig
}


# Clusters ----------------------------------------------------------------


#' @export
#' @rdname prune
prune.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  structure(
    stats::setNames(
      lapply(x, prune, ...),
      names(x)
    ),
    class = "group_tna"
  )
}

#' @export
#' @rdname pruning_details
pruning_details.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  invisible(Map(
    function(y, i) {
      print(i)
      pruning_details.tna(y, ...)
    },
    x,
    names(x)
  ))
}

#' @export
#' @rdname deprune
deprune.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  structure(
    stats::setNames(
      lapply(x, deprune, ...),
      names(x)
    ),
    class = "group_tna"
  )
}

#' @export
#' @rdname reprune
reprune.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  structure(
    stats::setNames(
      lapply(x, reprune, ...),
      names(x)
    ),
    class = "group_tna"
  )
}
