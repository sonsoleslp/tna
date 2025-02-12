#' Compare Two Matrices or Models with Comprehensive Metrics and Visualizations
#'
#' @export
#' @param x A `tna` object or a `matrix` of weights.
#' @param y A `tna` object or a `matrix` of weights.
#' @param ... Ignored.
#' @return A `tna_compare` object, which is a `list` containing the
#' following elements:
#'
#' * `matrices`: A `list` containing the original matrices of the input `tna`
#'   objects or the inputs themselves in case of matrices.
#' * `difference_matrix`: A `matrix` of differences `x - y`.
#' * `edge_metrics`: A `data.frame` of edge-level metrics about the differences.
#' * `summary_metrics`: A `data.frame` of summary metrics of the differences
#'   across all edges.
#' * `centrality_differences`: A `data.frame` of differences in centrality
#'   measures computes from `x` and `y`.
#' * `centrality_correlations`: A `numeric` vector of correlations of the
#'   centrality measures between `x` and `y`.
#'
#' @examples
#' # Create two example matrices
#' matrix1 <- matrix(runif(100, 0, 1), nrow = 10, ncol = 10)
#' matrix2 <- matrix(runif(100, 0, 1), nrow = 10, ncol = 10)
#' rownames(matrix1) <- rownames(matrix2) <- paste0("Gene", 1:10)
#' colnames(matrix1) <- colnames(matrix2) <- paste0("Sample", 1:10)
#' result <- compare(matrix1, matrix2)
#'
compare <- function(x, y, ...) {
  stopifnot_(
    is_tna(x) || is.matrix(x),
    "Argument {.arg x} must be a {.cls tna} object or a numeric {.cls matrix}."
  )
  stopifnot_(
    is_tna(y) || is.matrix(y),
    "Argument {.arg x} must be a {.cls tna} object or a numeric {.cls matrix}."
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

  #check_match(scaling, names(scaling_methods))

  #x[] <- scaling_methods[[type]](as.vector(x))
  #y[] <- scaling_methods[[type]](as.vector(y))
  d <- x - y
  x_vec <- as.vector(x)
  y_vec <- as.vector(y)
  abs_diff <- abs(x_vec - y_vec)
  abs_x <- abs(x_vec)
  abs_y <- abs(y_vec)
  rn <- rownames(x)

  # Edge level metrics
  edges <- expand.grid(Source = rn, Target = rn)
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
  edges_combined$logarithmic_ratio = log1p(x_vec) - log1p(y_vec)
  edges_combined$standardized_weight_x <- (x_vec - mean(x)) / stats::sd(x)
  edges_combined$standardized_weight_y <- (y_vec - mean(y)) / stats::sd(y)
  edges_combined$standardized_score_inflation <-
    edges_combined$standardized_weight_x / edges_combined$standardized_weight_y
  edges_combined$coefficient_variation_inflation <- (stats::sd(x) / mean(x)) /
    (stats::sd(y) / mean(y))

  # Summary metrics
  dissimilarities <- data.frame(
    Category = "Dissimilarities",
    Metric = c("Euclidean", "Manhattan", "Chebyshev", "Canberra", "Bray-Curtis"),
    Value = c(
      sum(sqrt(abs_diff^2)),
      sum(abs_diff),
      max(abs_diff),
      sum(abs_diff / (abs_x + abs_y)),
      sum(abs_diff) / sum(abs_x + abs_y)
    )
  )
  similarities <- data.frame(
    Category = "Similarities",
    Metric = c("Cosine", "Jaccard", "Dice", "Overlap", "RV"),
    Value = c(
      sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))),
      sum(pmin(abs_x, abs_y)) / sum(pmax(abs_x, abs_y)),
      2 * sum(pmin(abs_x, abs_y)) / (sum(abs_x) + sum(abs_y)),
      sum(pmin(abs_x, abs_y)) / min(sum(abs_x), sum(abs_y)),
      rv_coefficient(x, y)
    )
  )
  pattern_metrics <- data.frame(
    Category = "Pattern Similarities",
    Metric = c("Rank Agreement", "Sign Agreement"),
    Value = c(
      mean(sign(diff(x)) == sign(diff(y))),
      mean(sign(x) == sign(y))
    )
  )
  summary_metrics <- rbind(dissimilarities, similarities, pattern_metrics)

  # Centralities
  State <- NULL # for CRAN
  cents_x <- centralities(x) |>
    tidyr::pivot_longer(
      cols = !(!!rlang::sym("State")), names_to = "Centrality", values_to = "x"
    )
  cents_y <- centralities(y) |>
    tidyr::pivot_longer(
      cols = !(!!rlang::sym("State")), names_to = "Centrality", values_to = "y"
    )
  cents_xy <- cents_x
  cents_xy$y <- cents_y$y
  cents_xy$difference <- cents_xy$x - cents_xy$y
  cents_corr <- cents_xy |>
    dplyr::group_by(!!rlang::sym("Centrality")) |>
    dplyr::summarize(
      Centrality = dplyr::first(!!rlang::sym("Centrality")),
      correlation = stats::cor(
        !!rlang::sym("x"),
        !!rlang::sym("y"),
        use = "complete.obs"
      )
    )

  structure(
    list(
      matrices = list(x = x, y = y),
      difference_matrix = d,
      edge_metrics = tibble::tibble(edges_combined),
      summary_metrics = tibble::tibble(summary_metrics),
      centrality_differences = cents_xy,
      centrality_correlations = cents_corr
      #heatmaps = list(matrix1 = heatmap1, matrix2 = heatmap2, difference = heatmap_diff),
      #scatter_plot = scatter_plot,
      #centrality_heatmap = centrality_heatmap,
    ),
    class = "tna_comparison"
  )
}

# scaling_methods <- list(
#   none = function(w) w,
#   linear = function(w) (w - min(w)) / diff(range(w)),
#   equidistant = function(w) {
#     (rank(w, ties.method = "average") - 1) / (length(w) - 1)
#   },
#   zscore = function(w) (w - mean(w)) / stats::sd(w),
#   log = function(w) log1p(w - min(w)),
#   softmax = function(w) exp(w - log_sum_exp(w)),
#   quantile = function(w) ecdf(w)(w)
# )


# distance_correlation <- function(x, y) {
#   dist_x <- as.matrix(dist(x, diag = TRUE, upper = TRUE))
#   dist_y <- as.matrix(dist(y, diag = TRUE, upper = TRUE))
#   n <- ncol(x)
#   dist_row_means_x <- matrix(rowMeans(dist_x), n, n)
#   dist_row_means_y <- matrix(rowMeans(dist_y), n, n)
#   dist_col_means_x <- matrix(colMeans(dist_x), n, n, byrow = TRUE)
#   dist_col_means_y <- matrix(colMeans(dist_y), n, n, byrow = TRUE)
#   dist_mean_x <- mean(dist_x)
#   dist_mean_y <- mean(dist_y)
#   xx <- dist_x - dist_row_means_x - dist_col_means_x + dist_mean_x
#   yy <- dist_y - dist_row_means_y - dist_col_means_y + dist_mean_y
#   v_xy <- n^-2 * sum(xx * yy)
#   v_x <- n^-2 * sum(xx^2)
#   v_y <- n^-2 * sum(yy^2)
#   v_xy / sqrt(v_x * v_y)
# }

rv_coefficient <- function(x, y) {
  x <- scale(x, scale = FALSE)
  y <- scale(y, scale = FALSE)
  xx <- tcrossprod(x)
  yy <- tcrossprod(y)
  tr_xx_yy <- sum(diag(xx %*% yy))
  tr_xx_xx <- sum(diag(xx %*% xx))
  tr_yy_yy <- sum(diag(yy %*% yy))
  tr_xx_yy / (tr_xx_xx * tr_yy_yy)^0.5
}
