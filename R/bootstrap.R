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
#'   Grouped models with scaled edge weights, and their networks, are not
#'   resampled (an error of class `tna_error_scaling`).
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
#' @param seed A single `numeric` random seed for reproducible resampling, or
#' `NULL` (the default) to use the current RNG state.
#' @param actor An optional actor identifier for sequences nested in actors
#' (for example, several sessions per student). Either a `character` string
#' naming a non-sequence column of the data the model was built from (such as
#' a column of `meta_data` from [prepare_data()]), or a vector with one
#' identifier per sequence (per sequence of the data the grouped model was
#' built from, for a `group_tna` object). When given, whole actors are
#' resampled with replacement instead of single sequences (a cluster
#' bootstrap; Davison & Hinkley, 1997, Section 3.8), so that sequences nested
#' in actors are not treated as independent. The default `NULL` resamples
#' sequences.
#' @return A `tna_bootstrap` object which is a `list` containing the
#' following elements:
#'
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
#'   * `summary`: A `data.frame` with one row per edge with a positive
#'     weight and the columns `from`, `to`, `weight`, `p_value`, `sig`,
#'     `cr_lower`, `cr_upper`, `ci_lower`, and `ci_upper`. The same table is
#'     returned by [summary.tna_bootstrap()].
#'   * `model`: The `tna` model pruned to the significant edges (see
#'     [prune()] with `method = "bootstrap"`).
#'
#' If `x` is a `group_tna` object, the output is a `group_tna_bootstrap`
#' object, which is a `list` of `tna_bootstrap` objects.
#'
#' @references
#' Davison, A. C., & Hinkley, D. V. (1997). *Bootstrap Methods and their
#' Application*. Cambridge University Press.
#'
#' @examples
#' model <- tna(group_regulation)
#' # Small number of iterations for CRAN
#' bootstrap(model, iter = 10)
#'
bootstrap <- function(x, iter, level, method, threshold, consistency_range,
                      seed = NULL, actor = NULL) {
  UseMethod("bootstrap")
}

#' @export
#' @rdname bootstrap
bootstrap.tna <- function(x, iter = 1000, level = 0.05, method = "stability",
                          threshold, consistency_range = c(0.75, 1.25),
                          seed = NULL, actor = NULL) {
  check_missing(x)
  check_tna_seq(x)
  check_not_scaled_group(x, "Bootstrapping")
  check_values(iter, strict = TRUE)
  check_range(level, lower = 0, upper = 1)
  method <- check_match(method, c("stability", "threshold"))
  threshold <- threshold %m% unname(stats::quantile(x$weights, probs = 0.1))
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
  check_actor(actor)
  if (!is.null(seed)) {
    check_numeric(seed)
    set.seed(seed)
  }
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
  weights <- compute_weights(trans, type, scaling, a, params)
  dimnames(weights) <- dim_names
  weights_boot <- array(0.0, dim = c(iter, a, a))
  p_values <- matrix(0, a, a)
  resample <- bootstrap_sampler(x, actor)
  if (method == "stability") {
    for (i in seq_len(iter)) {
      trans_boot <- trans[resample(), , , drop = FALSE]
      weights_boot[i, , ] <- compute_weights(
        trans_boot, type, scaling, a, params
      )
      p_values[] <- p_values +
        1L * (weights_boot[i, , ] <= weights * consistency_range[1] | 
          weights_boot[i, , ] >= weights * consistency_range[2])
    }
  } else {
    for (i in seq_len(iter)) {
      trans_boot <- trans[resample(), , , drop = FALSE]
      weights_boot[i, , ] <- compute_weights(
        trans_boot, type, scaling, a, params
      )
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
                                consistency_range = c(0.75, 1.25),
                                seed = NULL, actor = NULL) {
  check_missing(x)
  check_class(x, "group_tna")
  check_not_scaled_group(x, "Bootstrapping")
  check_actor(actor)
  actors <- group_actor(x, actor)
  if (!is.null(seed)) {
    check_numeric(seed)
    set.seed(seed)
  }
  structure(
    stats::setNames(
      lapply(
        seq_along(x),
        function(i, ...) bootstrap(x[[i]], actor = actors[[i]], ...),
        iter = iter,
        level = level,
        method = method,
        threshold = threshold,
        consistency_range = consistency_range
      ),
      names(x)
    ),
    class = "group_tna_bootstrap"
  )
}

#' Bootstrap Cliques of Transition Networks from Sequence Data
#'
#' Bootstrap the edge weights of all cliques of a given size in a `tna`
#' model, producing per-clique mean weights, p-values, confidence intervals,
#' and consistency-range bounds.
#'
#' @export
#' @inheritParams cliques
#' @inheritParams bootstrap
#' @return A `data.frame` (also of class `tna_bootstrap_cliques`) with
#'   one row per clique and the columns `clique`, `mean_weight`, `p_values`,
#'   `sig`, `cr_lower`, `cr_upper`, `ci_lower`, `ci_upper`.
#' @examples
#' model <- tna(group_regulation)
#' # Small number of iterations for CRAN
#' boot_cliq <- bootstrap_cliques(model, size = 2, iter = 10)
#'
bootstrap_cliques <- function(x, size, threshold,
                              iter, level, consistency_range, seed = NULL) {
  UseMethod("bootstrap_cliques")
}

#' @export
#' @rdname bootstrap_cliques
bootstrap_cliques.tna <- function(x, size = 2L, threshold = 0,
                                  iter = 1000, level = 0.05,
                                  consistency_range = c(0.75, 1.25),
                                  seed = NULL) {
  check_missing(x)
  check_tna_seq(x)
  check_not_scaled_group(x, "Bootstrapping")
  check_values(threshold, type = "numeric")
  stopifnot_(
    checkmate::test_int(x = size, lower = 2),
    "Argument {.arg size} must be a single {.cls integer}
    between 2 and {nodes(x)}."
  )
  if (!is.null(seed)) {
    check_numeric(seed)
    set.seed(seed)
  }
  check_values(iter, strict = TRUE)
  check_range(level, lower = 0, upper = 1)
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
  cliq <- cliques(
    x = x,
    size = size,
    threshold = threshold,
    sum_weights = TRUE
  )
  n <- length(cliq$weights)
  stopifnot_(
    length(cliq$weights) > 0,
    "No cliques of size {size} were found in the network."
  )
  d <- x$data
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")
  model <- initialize_model(d, type, scaling, params, transitions = TRUE)
  trans <- model$trans
  alphabet <- attr(d, "alphabet")
  dim_names <- list(alphabet, alphabet)
  a <- length(alphabet)
  weights <- compute_weights(trans, type, scaling, a, params)
  clique_weights <- vapply(
    cliq$indices,
    function(y) {
      mean(weights[y, y])
    },
    numeric(1L)
  )
  dimnames(weights) <- dim_names
  weights_boot <- array(0.0, dim = c(iter, a, a))
  clique_weights_boot <- matrix(0.0, iter, n)
  p_values <- numeric(n)
  idx <- seq_len(n)
  for (i in seq_len(iter)) {
    trans_boot <- trans[sample(idx, n, replace = TRUE), , , drop = FALSE]
    weights_boot[i, , ] <- compute_weights(
      trans_boot, type, scaling, a, params
    )
    clique_weights_boot[i, ] <- vapply(
      cliq$indices,
      function(y) {
        mean(weights_boot[i, y, y])
      },
      numeric(1L)
    )
    p_values <- p_values +
      1L * (clique_weights_boot[i, ] <= clique_weights * consistency_range[1] |
        clique_weights_boot[i, ] >= clique_weights * consistency_range[2])
  }
  p_values <- (p_values + 1) / (iter + 1)
  ci_lower <- apply(
    clique_weights_boot,
    2L,
    stats::quantile,
    probs = level / 2,
    na.rm = TRUE
  )
  ci_upper <-  apply(
    clique_weights_boot,
    2L,
    stats::quantile,
    probs = 1 - level / 2,
    na.rm = TRUE
  )
  clique_names <- vapply(
    cliq$indices,
    function(y) {
      paste0(alphabet[y], collapse = "-")
    },
    character(1L)
  )
  structure(
    data.frame(
      clique = clique_names,
      mean_weight = clique_weights,
      p_values = p_values,
      sig = (p_values < level),
      cr_lower = clique_weights * consistency_range[1L],
      cr_upper = clique_weights * consistency_range[2L],
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ),
    class = c("tna_bootstrap_cliques", "data.frame")
  )
}
