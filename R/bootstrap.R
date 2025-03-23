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
#' computed, and confidence intervals are derived. The function also estimates
#' p-values for each edge and identifies significant edges based on
#' the specified significance level. A matrix of significant edges
#' (those with estimated p-values below the significance level) is generated.
#' Additional statistics on removed edges (those not considered
#' significant) are provided.
#'
#' All results, including the original transition matrix, bootstrapped
#' estimates, and summary statistics for removed edges, are returned in a
#' structured list.
#'
#' @export
#' @family validation
#' @param x A `tna` or a `group_tna` object created from sequence data.
#' @param iter An `integer` specifying the number of bootstrap samples to
#' draw. Defaults to `1000`.
#' @param level A `numeric` value representing the significance level for
#' hypothesis testing and confidence intervals. Defaults to `0.05`.
#' @param method A `character` string. This argument defines the bootstrap
#' test statistic. The `"stability"` option (the default) compares edge weights
#' against a range of "consistent" values defined by `consistency_range`.
#' Weights that fall outside this range are considered insignificant. In other
#' words, an edge is considered significant if its value is within the range
#' in `(1 - level)` * 100% of the bootstrap samples. The `"threshold"` option
#' instead compares the edge weights against a user-specified `threshold` value.
#' @param consistency_range A `numeric` vector of length 2. Determines how much
#' the edge weights may deviate (multiplicatively) from their observed values
#' (below and above) before they are considered insignificant. The default is
#' `c(0.75, 1.25)` which corresponds to a symmetric 25% deviation range. Used
#' only when `method = "stability"`.
#' @param threshold A `numeric` value to compare edge weights against.
#' The default is the 10th percentile of the edge weights. Used only when
#' `method = "threshold"`.
#' @param ... Ignored.
#' @return A `tna_bootstrap` object which is a `list` containing the
#' following elements:
#
#'   * `weights_orig`: The original edge weight `matrix`.
#'   * `weights_sig`: The `matrix` of significant transitions
#'     (those with estimated p-values below the significance level).
#'   * `weights_mean`: The mean weight `matrix` from the bootstrap samples.
#'   * `weights_sd`: The standard deviation `matrix` from the bootstrap samples.
#'   * `cr_lower`: The lower bound `matrix` of the consistency range for the
#'     edge weights.
#'   * `cr_upper`: The upper bound `matrix` of the consistency range for the
#'     edge weights.
#'   * `ci_lower`: The lower bound `matrix` of the bootstrap confidence
#'     intervals for the edge weights.
#'   * `ci_upper`: The upper bound `matrix` of the bootstrap confidence
#'     intervals for the edge weights.
#'   * `p_values`: The `matrix` of estimated p-values for the edge weights.
#'   * `summary`: A `data.frame` summarizing the edges, their weights,
#'     p-values, statistical significance, consistency ranges, and
#'     confidence intervals.
#'
#' If `x` is a `group_tna` object, the output is a `group_tna_bootstrap`
#' object, which is a `list` of `tna_bootstrap` objects.
#'
#' @examples
#' model <- tna(group_regulation)
#' # Small number of iterations for CRAN
#' bootstrap(model, iter = 10)
#'
bootstrap <- function(x, ...) {
  UseMethod("bootstrap")
}

#' @export
#' @rdname bootstrap
bootstrap.tna <- function(x, iter = 1000, level = 0.05, method = "stability",
                          threshold, consistency_range = c(0.75, 1.25), ...) {
  check_missing(x)
  check_tna_seq(x)
  check_values(iter, strict = TRUE)
  check_range(level)
  method <- check_match(method, c("stability", "threshold"))
  if (missing(threshold)) {
    threshold <- unname(stats::quantile(x$weights, probs = 0.1))
  }
  check_values(threshold, type = "numeric")
  stopifnot_(
    checkmate::test_numeric(
      x = consistency_range,
      len = 2L,
      min = 0,
      any.missing = FALSE,
      sorted = TRUE
    ),
    "Argument {.arg consistency_range} must be a sorted {.cls numeric}
     vector of length 2 containing positive values."
  )
  d <- x$data
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")
  model <- initialize_model(d, type, scaling, params, transitions = TRUE)
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
  if (method == "stability") {
    for (i in seq_len(iter)) {
      trans_boot <- trans[sample(idx, n, replace = TRUE), , ]
      weights_boot[i, , ] <- compute_weights(trans_boot, type, scaling, a)
      p_values[] <- p_values +
        1L * (weights_boot[i, , ] <= weights * consistency_range[1]) +
        1L * (weights_boot[i, , ] >= weights * consistency_range[2])
    }
  } else {
    for (i in seq_len(iter)) {
      trans_boot <- trans[sample(idx, n, replace = TRUE), , ]
      weights_boot[i, , ] <- compute_weights(trans_boot, type, scaling, a)
      p_values <- p_values + 1L * (weights_boot[i, , ] < threshold)
    }
  }
  p_values <- (p_values + 1) / (iter + 1)
  weights_mean <- apply(weights_boot, c(2, 3), mean, na.rm = TRUE)
  weights_sd <- apply(weights_boot, c(2, 3), stats::sd, na.rm = TRUE)
  ci_lower <- apply(
    weights_boot,
    c(2, 3),
    stats::quantile,
    probs = level / 2,
    na.rm = TRUE
  )
  ci_upper <-  apply(
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
    from = rep(alphabet, times = a),
    to = rep(alphabet, each = a),
    weight = weights_vec,
    p_value = as.vector(p_values),
    sig = as.vector(p_values < level),
    cr_lower = as.vector(weights * consistency_range[1]),
    cr_upper = as.vector(weights * consistency_range[2]),
    ci_lower = as.vector(ci_lower),
    ci_upper = as.vector(ci_upper)
  )[weights_vec > 0, ]
  model <- x
  tmp <- list(
    weights = weights_sig,
    method = "bootstrap",
    removed = combined[which(!combined$sig), c("from", "to", "weight")],
    num_removed = sum(!combined$sig),
    num_retained = sum(combined$sig)
  )
  tmp$original <- model$weights
  tmp$active <- TRUE
  model$weights <- tmp$weights
  attr(model, "pruning") <- tmp
  structure(
    list(
      weights_orig = weights,
      weights_sig = weights_sig,
      weights_mean = weights_mean,
      weights_sd = weights_sd,
      p_values = p_values,
      cr_lower = weights * consistency_range[1],
      cr_upper = weights * consistency_range[2],
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      summary = combined,
      model = model
    ),
    class = "tna_bootstrap"
  )
}

#' @export
#' @rdname bootstrap
bootstrap.group_tna <- function(x, iter = 1000, level = 0.05,
                                method = "stability", threshold,
                                consistency_range = c(0.75, 1.25), ...) {
  check_missing(x)
  check_class(x, "group_tna")
  stopifnot_(
    length(attr(x, "scaling")) == 0L || attr(x, "groupwise"),
    "Bootstrapping is not supported for
     grouped models with globally scaled edge weights."
  )
  structure(
    stats::setNames(
      lapply(
        x,
        bootstrap,
        iter = iter,
        level = level,
        method = method,
        threshold = threshold,
        consistency_range = consistency_range,
        ...
      ),
      names(x)
    ),
    class = "group_tna_bootstrap"
  )
}
