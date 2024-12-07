#' Bootstrap Transition Networks from Sequence Data
#'
#' Perform bootstrapping on transition networks created from
#' sequence data stored in a `tna` object. Bootstrapped estimates
#' of edge weights are returned with confidence intervals and significance
#' testing.
#'
#' The function first computes the original edge weights for the specified
#' cluster from the `tna` object. It then performs bootstrapping by resampling
#' the sequence data and recalculating the edge weights for each
#' bootstrap sample. The mean and standard deviation of the transitions are
#' computed, and confidence intervals are derived. The function also calculates
#' p-values for each edge and identifies significant edges based on
#' the specified significance level. A matrix of significant edges
#' (those with p-values below the significance level) is generated.
#' Additional statistics on removed edges (those not considered
#' significant) are provided.
#'
#' All results, including the original transition matrix, bootstrapped
#' estimates, and summary statistics for removed edges, are returned in a
#' structured list.
#'
#' @export
#' @family evaluation
#' @param x A `tna` or a `group_tna` object created from sequence data.
#' @param iter An `integer` specifying the number of bootstrap samples to
#' draw. Defaults to `1000`.
#' @param level A `numeric` value representing the significance level for
#' hypothesis testing and confidence intervals. Defaults to `0.05`.
#' @param threshold A `numeric` value to compare edge weights against.
#' The default is the 10th percentile of the edge weights.
#' @param ... Ignored.
#' @return A `tna_bootstrap` object which is a `list` containing the
#' following elements:
#
#'   * `weights_orig`: The original edge weight `matrix`.
#'   * `weights_sig`: The `matrix` of significant transitions
#'     (those with p-values below the significance level).
#'   * `weights_mean`: The mean weight `matrix` from the bootstrap samples.
#'   * `weights_sd`: The standard deviation `matrix` from the bootstrap samples.
#'   * `ci_lower`: The lower bound `matrix` of the confidence intervals for
#'     the edge weights.
#'   * `ci_upper`: The upper bound `matrix` of the confidence intervals for
#'     the edge weights.
#'   * `p_values`: The `matrix` of p-values for the edge weights.
#'   * `summary`: A `data.frame` summarizing the edges, their weights,
#'     p-values, statistical significance and confidence intervals.
#'
#' If `x` is a `group_tna` object, the output is a `group_tna_bootstrap`
#' object, which is a `list` of `tna_bootstrap` objects.
#'
#' @examples
#' model <- tna(engagement)
#' # Small number of iterations for CRAN
#' bootstrap(model, iter = 10)
#'
bootstrap <- function(x, ...) {
  UseMethod("bootstrap")
}

#' @rdname bootstrap
#' @export
bootstrap.tna <- function(x, iter = 1000, level = 0.05, threshold, ...) {
  check_missing(x)
  check_tna_seq(x)
  check_positive(iter)
  check_probability(level)
  if (missing(threshold)) {
    threshold <- unname(stats::quantile(x$weights, probs = 0.1))
  }
  check_nonnegative(threshold, type = "numeric")
  d <- x$data
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  model <- initialize_model(d, type, scaling, transitions = TRUE)
  trans <- model$trans
  alphabet <- attr(d, "alphabet")
  dim_names <- list(alphabet, alphabet)
  n <- nrow(d)
  a <- length(alphabet)
  weights <- compute_weights(trans, type, scaling, a)
  dimnames(weights) <- dim_names
  weights_boot <- array(0L, dim = c(iter, a, a))
  p_values <- matrix(0, a, a)
  idx <- seq_len(n)
  for (i in seq_len(iter)) {
    trans_boot <- trans[sample(idx, n, replace = TRUE), , ]
    weights_boot[i, , ] <- compute_weights(trans_boot, type, scaling, a)
    p_values <- p_values + 1L * (weights_boot[i, , ] < threshold)
  }
  p_values <- p_values / iter
  weights_mean <- apply(weights_boot, c(2, 3), mean, na.rm = TRUE)
  weights_sd <- apply(weights_boot, c(2, 3), stats::sd, na.rm = TRUE)
  ci_lower <- apply(
    weights_boot,
    c(2, 3),
    stats::quantile,
    probs = level / 2,
    na.rm = TRUE
  )
  ci_upper <- apply(
    weights_boot,
    c(2, 3),
    stats::quantile,
    probs = 1 - level / 2,
    na.rm = TRUE
  )
  weights_sig <- (p_values < level) * weights
  dimnames(p_values) <- dim_names
  dimnames(weights_mean) <- dim_names
  dimnames(weights_sd) <- dim_names
  dimnames(weights_sig) <- dim_names
  dimnames(ci_lower) <- dim_names
  dimnames(ci_upper) <- dim_names
  weights_vec <- as.vector(weights)
  combined <- data.frame(
    from = rep(alphabet, each = a),
    to = rep(alphabet, times = a),
    weight = weights_vec,
    p_value = as.vector(p_values),
    sig = as.vector(p_values < level),
    ci_lower = as.vector(ci_lower),
    ci_upper = as.vector(ci_upper)
  )[weights_vec > 0, ]
  structure(
    list(
      weights_orig = weights,
      weights_sig = weights_sig,
      weights_mean = weights_mean,
      weights_sd = weights_sd,
      p_values = p_values,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      summary = combined
    ),
    class = "tna_bootstrap"
  )
}

#' @export
#' @family clusters
#' @rdname bootstrap
bootstrap.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  structure(
    lapply(x, \(i) bootstrap.tna(i, ...)),
    class = "group_tna_bootstrap"
  )
}
