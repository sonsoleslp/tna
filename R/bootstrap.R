#' Bootstrap Transition Networks from Sequence Data
#'
#' This function performs bootstrapping on transition networks created from
#' sequence data stored in a `tna` object. It resamples the sequences,
#' computes transition matrices, and returns bootstrapped estimates
#' of transitions with confidence intervals and significance testing.
#'
#' @export
#' @family evaluation
#' @param x A `tna` object created from sequence data.
#' @param b An integer specifying the number of bootstrap samples to
#' be generated. Defaults to `1000`.
#' @param level A `numeric` value representing the significance level for
#' hypothesis testing and confidence intervals. Defaults to `0.05`.
#' @param cluster An integer specifying which cluster of the sequence data to
#' use for bootstrapping. Defaults to `1`.
#'
#' @details
#' The function first computes the original transition matrix for the specified
#' cluster from the `tna` object. It then performs bootstrapping by resampling
#' the sequence data and recalculating the transition matrices for each
#' bootstrap sample. The mean and standard deviation of the transitions are
#' computed, and confidence intervals are derived. The function also calculates
#' p-values for each transition and identifies significant transitions based on
#' the specified significance level. A matrix of significant transitions
#' (those with p-values below the significance level) is generated.
#' Additional statistics on removed edges (transitions not considered
#' significant) are provided.
#'
#' All results, including the original transition matrix, bootstrapped
#' estimates, and summary statistics for removed edges, are returned in a
#' structured list.
#'
#' @return A `list` containing:
#'
#' * `model`: A new `tna` object based on the significant transition matrix.
#' * `results`: A list containing the following elements:
#'    * `original_trans`: The original transition `matrix` for the selected
#'    cluster.
#'    * `mean_trans`: The mean transition `matrix` from the bootstrap samples.
#'    * `sd_trans`: The standard deviation `matrix` from the bootstrap samples.
#'    * `ci_lower`: The lower bound `matrix` of the confidence intervals for
#'    the transitions.
#'    * `ci_upper`: The upper bound `matrix` of the confidence intervals for
#'    the transitions.
#'    * `p_values`: The `matrix` of p-values for the transitions.
#'    * `sig_trans`: The `matrix` of significant transitions
#'      (those with p-values below the significance level).
#'    * `combined`: A `data.frame` summarizing the transitions, their weights,
#'      p-values, and confidence intervals.
#'    * `removed_edges_summary`: A list summarizing the number of removed edges
#'      (insignificant transitions), the mean and standard deviation of their
#'      weights, and the range of the removed edge weights.
#'
#' @examples
#' \dontrun{
#' # Bootstrap transition networks for a cluster
#' results <- bootstrap(tna_model, v = 1000, level = 0.05, cluster = 1)
#' }
#'
bootstrap <- function(x, ...) {
  UseMethod("bootstrap")
}

#' @rdname bootstrap
#' @export
bootstrap.tna <- function(x, b = 1000, level = 0.05, cluster = 1, ...) {
  stopifnot_(
    !is.null(x$seq),
    "Argument {.arg x} must be a {.cls tna} object
    created from sequence data."
  )
  d <- x$seq[[cluster]]
  model <- build_markov_model(d, transitions = TRUE)
  trans <- model$trans
  alphabet <- attr(d, "alphabet")
  type <- attr(x, "type")
  dim_names <- list(alphabet, alphabet)
  n <- nrow(d)
  a <- length(alphabet)
  weights <- apply(trans, c(2, 3), sum)
  if (type = "prob") {
    weights <- weights / .rowSums(weights, m = a, n = a)
  }
  dimnames(weights) <- dim_names
  weights_boot <- array(0L, dim = c(b, a, a))
  p_values <- matrix(0, a, a)
  for (i in seq_len(b)) {
    trans_boot <- trans[sample(idx, n, replace = TRUE), , ]
    freq_boot <- apply(trans_boot, c(2, 3), sum)
    if (type == "prob") {
      weights_boot[i, , ] <- freq_boot / .rowSums(freq_boot, m = a, n = a)
    } else {
      weights_boot[i, , ] <- freq_boot
    }
    p_values <- p_values + 1L * (weights_boot[i, , ] >= weights)
  }
  p_values <- p_values / b
  mean_weights <- apply(weights_boot, c(2, 3), mean)
  sd_weights <- apply(weights_boot, c(2, 3), stats::sd)
  ci_lower <- apply(weights_boot, c(2, 3), stats::quantile, probs = level / 2)
  ci_upper <- apply(weights_boot, c(2, 3), stats::quantile, probs = 1 - level / 2)
  sig_weights <- (p_values < level) * weights
  removed <- c(weights[sig_trans == 0])
  n_removed <- length(removed)
  any_removed <- n_removed > 0
  mean_removed <- ifelse_(any_removed, mean(removed), NA)
  sd_removed <- ifelse_(any_removed, stats::sd(removed), NA)
  range_removed <- ifelse_(any_removed, range(removed), c(NA, NA))
  dimnames(p_values) <- dim_names
  dimnames(mean_trans) <- dim_names
  dimnames(sd_trans) <- dim_names
  dimnames(ci_lower) <- dim_names
  dimnames(ci_upper) <- dim_names
  dimnames(sig_trans) <- dim_names
  combined <- data.frame(
    from = rep(alphabet, each = a),
    to = rep(alphabet, times = a),
    edge_weight = as.vector(weights),
    p_value = as.vector(p_values),
    ci_Lower = as.vector(ci_lower),
    ci_Upper = as.vector(ci_upper)
  )
  list(
    results = list(
      original_weights = weights,
      mean_weights = mean_weights,
      sd_weights = sd_weights,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_values = p_values,
      sig_trans = sig_weights,
      combined = combined,
      removed_edges_summary = list(
        n_removed = n_removed,
        mean_removed = mean_removed,
        sd_removed = sd_removed,
        range_removed = range_removed
      )
    )
  )
}
