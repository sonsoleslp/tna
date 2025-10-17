# Model comparison --------------------------------------------------------

#' Compare Two Matrices or TNA Models with Comprehensive Metrics
#'
#' Various distances, measures of dissimilarity and similarity, correlations
#' and other metrics are computed to compare the models. Optionally, the weight
#' matrices of the models can be scaled before comparison. The resulting object
#' can be used to produce heatmap plots and scatterplots to further illustrate
#' the differences.
#'
#' @export
#' @family comparison
#' @param x A `tna` object or a `matrix` of weights.
#' @param y A `tna` object or a `matrix` of weights.
#' @param scaling A  `character` string naming a scaling method to
#' apply to the weights before comparing them. The supported options are:
#'
#' * `"none"`: No scaling is performed. The weights are used as is.
#' * `"minmax"`: Performs min-max normalization, i.e., the minimum value is
#'   subtracted and the differences are scaled by the range.
#' * `"rank"`: Applies min-max normalization to the ranks of the weights
#'   (computed with `ties.method = "average"`).
#' * `"zscore"`: Computes the standard score, i.e. the mean weight is
#'   subtracted and the differences are scaled by the standard deviation.
#' * `"robust"`: Computes the robust z-score, i.e. the median weight is
#'   subtracted and the differences are scaled by the median absolute deviation
#'   (using [stats::mad]).
#' * `"log"`: Simply the natural logarithm of the weights.
#' * `"log1p"`: As above, but adds 1 to the values before taking the logarithm.
#'    Useful for scenarios with zero weights.
#' * `"softmax"`: Performs softmax normalization.
#' * `"quantile"`: Uses the empirical quantiles of the weights
#'   via [stats::ecdf].
#'
#' @param ... Ignored.
#' @return A `tna_comparison` object, which is a `list` containing the
#' following elements:
#'
#' * `matrices`: A `list` containing the scaled matrices of the input `tna`
#'   objects or the scaled inputs themselves in the case of matrices.
#' * `difference_matrix`: A `matrix` of differences `x - y`.
#' * `edge_metrics`: A `data.frame` of edge-level metrics about the differences.
#' * `summary_metrics`: A `data.frame` of summary metrics of the differences
#'   across all edges.
#' * `network_metrics`: A `data.frame` of network metrics for both `x` and `y`.
#' * `centrality_differences`: A `data.frame` of differences in centrality
#'   measures computes from `x` and `y`.
#' * `centrality_correlations`: A `numeric` vector of correlations of the
#'   centrality measures between `x` and `y`.
#'
#' @examples
#' # Comparing TNA models
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' comp1 <- compare(model_x, model_y)
#'
#' # Comparing matrices
#' mat_x <- model_x$weights
#' mat_y <- model_y$weights
#' comp2 <- compare(mat_x, mat_y)
#'
#' # Comparing a matrix to a TNA model
#' comp3 <- compare(mat_x, model_y)
#'
compare <- function(x, ...) {
  UseMethod("compare")
}

#' @export
#' @rdname compare
compare.tna <- function(x, y, scaling = "none", ...) {
  compare_(x, y, scaling = scaling, ...)
}

#' @export
#' @rdname compare
compare.matrix <- function(x, y, scaling = "none", ...) {
  compare_(x, y, scaling = scaling, ...)
}

#' Compare Grouped TNA Models with Comprehensive Metrics
#'
#' @export
#' @family comparison
#' @param x A `group_tna` object.
#' @param i An `integer` index or the name of the principal cluster as a
#' `character` string.
#' @param j An `integer` index or the name of the secondary cluster as a
#' `character` string.
#' @param scaling See [compare.tna()].
#' @param ... Additional arguments passed to [compare.tna()].
#' @return A `tna_comparison` object. See [compare.tna()] for details.
#' @examples
#' model <- group_model(engagement_mmm)
#' compare(model, i = 1, j = 2)
#'
compare.group_tna <- function(x, i = 1L, j = 2L, scaling = "none", ...) {
  check_missing(x)
  check_class(x, "group_tna")
  check_clusters(x, i, j)
  compare_(x = x[[i]], y = x[[j]], scaling = scaling, ...)
}

#' Internal compare function
#'
#' @inheritParams compare
#' @noRd
compare_ <- function(x, y, scaling = "none", ...) {
  stopifnot_(
    is_tna(x) || is.matrix(x),
    "Argument {.arg x} must be a {.cls tna} object or a numeric {.cls matrix}."
  )
  stopifnot_(
    is_tna(y) || is.matrix(y),
    "Argument {.arg x} must be a {.cls tna} object or a numeric {.cls matrix}."
  )
  metrics_x <- ifelse_(
    is.matrix(x),
    summary(build_model_(x)),
    summary(x)
  )
  metrics_y <- ifelse_(
    is.matrix(y),
    summary(build_model_(y)),
    summary(y)
  )
  x <- ifelse_(is_tna(x), x$weights, x)
  y <- ifelse_(is_tna(y), y$weights, y)
  x_arg <- ifelse_(is_tna(x), "x$weights", "x")
  y_arg <- ifelse_(is_tna(y), "y$weights", "y")
  n <- ncol(x)
  stopifnot_(
    n == nrow(x),
    "The weight matrix {.arg {x_arg}} must be a square matrix."
  )
  stopifnot_(
    identical(dim(x), dim(y)),
    "Weight matrices {.arg {x_arg}} and
    {.arg {y_arg}} must have identical dimensions."
  )
  stopifnot_(
    all(!is.na(x)),
    "Weight matrix {.arg {x_arg}} must not contain missing values."
  )
  stopifnot_(
    all(!is.na(y)),
    "Weight matrix {.arg {y_arg}} must not contain missing values."
  )
  scaling_methods <- list(
    none = identity,
    minmax = ranger,
    rank = function(w) ranger(rank(w, ties.method = "average")),
    zscore = function(w) (w - mean(w)) / stats::sd(w),
    log = log,
    log1p = log1p,
    softmax = function(w) exp(w - log_sum_exp(w)),
    quantile = function(w) stats::ecdf(w)(w)
  )
  scaling <- check_match(scaling, names(scaling_methods))
  x[, ] <- scaling_methods[[scaling]](as.vector(x))
  y[, ] <- scaling_methods[[scaling]](as.vector(y))
  d <- x - y
  x_vec <- as.vector(x)
  y_vec <- as.vector(y)
  abs_diff <- abs(x_vec - y_vec)
  abs_x <- abs(x_vec)
  abs_y <- abs(y_vec)
  pos <- abs_x > 0 & abs_y > 0
  rn <- rownames(x)

  # Edge level metrics
  edges <- expand.grid(source = rn, target = rn)
  edges_x <- edges
  edges_y <- edges
  # edges_diff <- edges
  edges_x$weight <- as.vector(x)
  edges_y$weight <- as.vector(y)
  # edges_diff$difference <- diff
  edges_combined <- edges
  edges_combined$weight_x <- edges_x$weight
  edges_combined$weight_y <- edges_y$weight
  edges_combined$raw_difference <- as.vector(d)
  edges_combined$absolute_difference <- abs_diff
  edges_combined$squared_difference <- abs_diff^2
  edges_combined$relative_difference <- abs_diff / (x_vec + y_vec)
  edges_combined$similarity_strength_index <- x_vec / y_vec
  edges_combined$difference_index <- (x_vec - y_vec) / y_vec
  edges_combined$rank_difference <-
    abs(rank(x_vec, na.last = "keep") - rank(y_vec, na.last = "keep"))
  edges_combined$percentile_difference <-
    abs(stats::ecdf(x_vec)(x_vec) - stats::ecdf(y_vec)(y_vec))
  edges_combined$logarithmic_ratio <- log1p(x_vec) - log1p(y_vec)
  edges_combined$standardized_weight_x <- (x_vec - mean(x)) / stats::sd(x)
  edges_combined$standardized_weight_y <- (y_vec - mean(y)) / stats::sd(y)
  edges_combined$standardized_score_inflation <-
    edges_combined$standardized_weight_x / edges_combined$standardized_weight_y

  # Summary metrics
  weight_dev <- data.frame(
    category = "Weight Deviations",
    metric = c(
      "Mean Abs. Diff.",
      "Median Abs. Diff.",
      "RMS Diff.",
      "Max Abs. Diff.",
      "Rel. Mean Abs. Diff.",
      "CV Ratio"
    ),
    value = c(
      mean(abs_diff),
      stats::median(abs_diff),
      sqrt(mean(abs_diff^2)),
      max(abs_diff),
      mean(abs_diff) / mean(abs_y),
      stats::sd(x_vec) * mean(y_vec) / (mean(x_vec) * stats::sd(y_vec))
    )
  )
  correlations <- data.frame(
    category = "Correlations",
    metric = c("Pearson", "Spearman", "Kendall", "Distance"),
    value = c(
      stats::cor(x_vec, y_vec, method = "pearson", use = "complete.obs"),
      stats::cor(x_vec, y_vec, method = "spearman", use = "complete.obs"),
      stats::cor(x_vec, y_vec, method = "kendall", use = "complete.obs"),
      distance_correlation(x_vec, y_vec)
    )
  )
  dissimilarities <- data.frame(
    category = "Dissimilarities",
    metric = c(
      "Euclidean",
      "Manhattan",
      "Canberra",
      "Bray-Curtis",
      "Frobenius"
    ),
    value = c(
      sqrt(sum(abs_diff^2)),
      sum(abs_diff),
      sum(abs_diff[pos] / (abs_x[pos] + abs_y[pos])),
      sum(abs_diff) / sum(abs_x + abs_y),
      sqrt(sum(abs_diff^2)) / sqrt(n / 2)
    )
  )
  similarities <- data.frame(
    category = "Similarities",
    metric = c("Cosine", "Jaccard", "Dice", "Overlap", "RV"),
    value = c(
      sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))),
      sum(pmin(abs_x, abs_y)) / sum(pmax(abs_x, abs_y)),
      2 * sum(pmin(abs_x, abs_y)) / (sum(abs_x) + sum(abs_y)),
      sum(pmin(abs_x, abs_y)) / min(sum(abs_x), sum(abs_y)),
      rv_coefficient(x, y)
    )
  )
  pattern_metrics <- data.frame(
    category = "Pattern Similarities",
    metric = c("Rank Agreement", "Sign Agreement"),
    value = c(
      mean(sign(diff(x)) == sign(diff(y))),
      mean(sign(x) == sign(y))
    )
  )
  summary_metrics <- rbind(
    weight_dev,
    correlations,
    dissimilarities,
    similarities,
    pattern_metrics
  )

  # Network metrics
  network_metrics <- cbind(metrics_x, metrics_y[, -1L])
  names(network_metrics) <- c("metric", "x", "y")

  # Centralities
  cents_x <- centralities(x) |>
    tidyr::pivot_longer(
      cols = !(!!rlang::sym("state")), names_to = "centrality", values_to = "x"
    )
  cents_y <- centralities(y) |>
    tidyr::pivot_longer(
      cols = !(!!rlang::sym("state")), names_to = "centrality", values_to = "y"
    )
  cents_xy <- cents_x
  cents_xy$y <- cents_y$y
  cents_xy$difference <- cents_xy$x - cents_xy$y
  corr_fun <- function(x, y) {
    out <- try(
      stats::cor(x, y, use = "complete.obs"),
      silent = TRUE
    )
    # Return NA in case of not insufficient pairs
    ifelse_(
      inherits(out, "try-error"),
      NA_real_,
      out
    )
  }
  cents_corr <- cents_xy |>
    dplyr::group_by(!!rlang::sym("centrality")) |>
    dplyr::summarize(
      Centrality = dplyr::first(!!rlang::sym("centrality")),
      correlation = corr_fun(!!rlang::sym("x"), !!rlang::sym("y"))
    )

  structure(
    list(
      matrices = list(x = x, y = y),
      difference_matrix = d,
      edge_metrics = tibble::tibble(edges_combined),
      summary_metrics = tibble::tibble(summary_metrics),
      network_metrics = tibble::tibble(network_metrics),
      centrality_differences = cents_xy,
      centrality_correlations = cents_corr
    ),
    class = "tna_comparison"
  )
}

#' Distance correlation coefficient
#'
#' @param x A `numeric` vector.
#' @param y A `numeric` vector.
#' @noRd
distance_correlation <- function(x, y) {
  dist_x <- as.matrix(stats::dist(x, diag = TRUE, upper = TRUE))
  dist_y <- as.matrix(stats::dist(y, diag = TRUE, upper = TRUE))
  n <- ncol(dist_x)
  dist_row_means_x <- matrix(.rowMeans(dist_x, n, n), n, n)
  dist_row_means_y <- matrix(.rowMeans(dist_y, n, n), n, n)
  dist_col_means_x <- matrix(.colMeans(dist_x, n, n), n, n, byrow = TRUE)
  dist_col_means_y <- matrix(.colMeans(dist_y, n, n), n, n, byrow = TRUE)
  dist_mean_x <- mean(dist_x)
  dist_mean_y <- mean(dist_y)
  xx <- dist_x - dist_row_means_x - dist_col_means_x + dist_mean_x
  yy <- dist_y - dist_row_means_y - dist_col_means_y + dist_mean_y
  v_xy <- n^-2 * sum(xx * yy)
  v_x <- n^-2 * sum(xx^2)
  v_y <- n^-2 * sum(yy^2)
  v_xy / sqrt(v_x * v_y)
}

#' RV Coefficient
#'
#' @param x A `matrix`.
#' @param y A `matrix`.
#' @noRd
rv_coefficient <- function(x, y) {
  x <- scale(x, scale = FALSE)
  y <- scale(y, scale = FALSE)
  xx <- tcrossprod(x)
  yy <- tcrossprod(y)
  tr_xx_yy <- sum(diag(xx %*% yy))
  tr_xx_xx <- sum(diag(xx %*% xx))
  tr_yy_yy <- sum(diag(yy %*% yy))
  tr_xx_yy / sqrt(tr_xx_xx * tr_yy_yy)
}


# Sequence comparison -----------------------------------------------------


#' Compare Sequences Between Groups
#'
#' Performs comprehensive sequence comparison analysis between groups. All
#' patterns of the sequences (subsequences of specific length) are extracted
#' from all sequences in each group. For two groups, the proportions of the
#' patterns between the groups are compared with a z-test for proportions.
#' For more than two groups, the Chi-squared  test is applied. Both tests are
#' applied within patterns of the same length.
#'
#' @export
#' @family comparison
#' @param x A `group_tna` object or a `data.frame` containing sequence data in
#'   wide format.
#' @param group A `vector` indicating the group assignment of each
#'   row of the data/sequence. Must have the same length as the number of
#'   rows/sequences of `x`. Alternatively, a single `character` string giving
#'   the column name of the data that defines the group when `x` is a wide
#'   format `data.frame` or a `tna_data` object.
#' @param sub An `integer` vector of subsequence lengths to analyze.
#'   The default is `2:5`.
#' @param min_freq An `integer` giving the minimum number of times that a
#'   specific pattern has to be observed to be included in the analysis.
#'   The default is `5`.
#' @param correction A `character` string naming the multiple comparison
#'   correction method (default: `"bonferroni"`). Supports all
#'   [stats::p.adjust] methods: `"holm"`, `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param ... Not used.
#' @return A `tna_sequence_comparison` object, which is a `data.frame` with
#'   columns giving the names of the pattern, pattern frequencies, pattern
#'   proportions (within patterns of the same length), test statistic values
#'   and p-values of the tests.
#'
#' @examples
#' group <- c(rep("High", 1000), rep("Low", 1000))
#' comp <- compare_sequences(group_regulation, group)
#'
#' @export
compare_sequences <- function(x, ...) {
  UseMethod("compare_sequences")
}

#' @export
#' @rdname compare_sequences
compare_sequences.default <- function(x, group, sub, min_freq = 5L,
                                      correction = "bonferroni", ...) {
  check_missing(group)
  model <- group_tna(x, group = group)
  compare_sequences.group_tna(
    x = model,
    sub = sub,
    min_freq = min_freq,
    correction = correction,
    ...
  )
}

#' @export
#' @rdname compare_sequences
compare_sequences.group_tna <- function(x, sub, min_freq = 5L,
                                        correction = "bonferroni", ...) {
  check_missing(x)
  check_class(x, "group_tna")
  sub <- sub %m% seq(2, min(5, ncol(x[[1L]]$data)))
  check_range(
    sub,
    type = "integer",
    scalar = FALSE,
    lower = 2L,
    upper = ncol(x[[1L]]$data)
  )
  check_values(min_freq)
  correction <- check_match(correction, stats::p.adjust.methods)
  n_group <- length(x)
  groups <- names(x)
  patterns <- vector(mode = "list", length = n_group)
  names(patterns) <- groups
  for (g in groups) {
    m <- matrix(x[[g]]$labels[x[[g]]$data], nrow = nrow(x[[g]]$data))
    patterns[[g]] <- extract_subsequences(m = m, q = sub)
  }
  compare_sequences_(
    x = patterns,
    q = sub,
    min_freq = min_freq,
    correction = correction
  )
}

# Internal sequence comparison
compare_sequences_ <- function(x, q, min_freq, correction) {
  groups <- names(x)
  n_group <- length(groups)
  p <- length(q)
  patterns <- vector(mode = "list", length = p)
  residuals <- vector(mode = "list", length = p)
  for (j in q) {
    pat <- lapply(x, "[[", j)
    pat_u <- unique(unlist(pat))
    pat <- data.frame(pattern = pat_u)
    total <- numeric(n_group)
    names(total) <- groups
    for (g in groups) {
      freq_col <- paste0("freq_", g)
      prop_col <- paste0("prop_", g)
      freq_tab <- table(x[[g]][[j]])
      pat[[freq_col]] <- as.numeric(freq_tab[pat_u])
      pat[[freq_col]][is.na(pat[[freq_col]])] <- 0
      total[g] <- sum(pat[[freq_col]])
      pat[[prop_col]] <- ifelse_(
        total[g] > 0,
        pat[[freq_col]] / total[g],
        0.0
      )
    }
    n <- nrow(pat)
    freq_cols <- paste0("freq_", groups)
    valid_patterns <- n_group == .rowSums(
      pat[, freq_cols] >= min_freq,
      m = n,
      n = n_group
    )
    pat <- pat[valid_patterns, ]
    n <- nrow(pat)
    if (n > 0L) {
      freq_mat <- as.matrix(pat[, freq_cols])
      resid <- matrix(0.0, nrow = n, ncol = n_group)
      rownames(resid) <- pat$pattern
      colnames(resid) <- groups
      pat$statistic <- NA
      pat$p_value <- NA
      for (i in seq_len(n)) {
        tab <- matrix(c(freq_mat[i, ], total), nrow = 2L, byrow = TRUE)
        chisq <- suppressWarnings(stats::chisq.test(tab))
        pat$statistic[i] <- chisq$statistic
        pat$p_value[i] <- stats::p.adjust(chisq$p.value, method = correction)
        resid[i, ] <- chisq$stdres[1L, ]
      }
      patterns[[j]] <- pat
      residuals[[j]] <- resid
    }
  }
  valid_patterns <- vapply(patterns, function(y) !is.null(y), logical(1L))
  stopifnot_(
    any(valid_patterns),
    "No common patterns with frequency greater than {min_freq} were found."
  )
  structure(
    dplyr::bind_rows(patterns) |>
      dplyr::arrange(!!rlang::sym("p_value")),
    residuals = do.call("rbind", residuals),
    class = c("tna_sequence_comparison", "data.frame")
  )
}

extract_subsequences <- function(m, q) {
  n <- nrow(m)
  p <- ncol(m)
  mis <- is.na(m)
  out <- vector(mode = "list", length = max(q))
  for (j in q) {
    tmp <- character(n * (p - j + 1L))
    idx <- seq(1L, n)
    for (i in seq_len(p - 1L)) {
      if (i + j - 1L > p) {
        next
      }
      pattern_idx <- i:(i + j - 1L)
      pattern <- m[, pattern_idx]
      pattern_mis <- mis[, pattern_idx]
      invalid <- .rowSums(pattern_mis, m = n, n = j) > 0
      subseq <- do.call(
        paste,
        c(as.data.frame(pattern, stringsAsFactors = FALSE), sep = "-")
      )
      subseq[invalid] <- ""
      tmp[idx] <- subseq
      idx <- idx + n
    }
    out[[j]] <- tmp[nzchar(tmp)]
  }
  out
}
