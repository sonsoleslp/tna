#' Retrieve Statistics from a Mixture Markov Model (MMM)
#'
#' @export
#' @family clusters
#' @param x A `mhmm` object.
#' @param level A `numeric` value representing the significance level for
#' hypothesis testing and confidence intervals. Defaults to `0.05`.
#' @return A `data.frame` object.
#' @examples
#' mmm_stats(engagement_mmm)
#'
mmm_stats <- function(x, level = 0.05) {
  UseMethod("mmm_stats")
}

#' @export
#' @rdname mmm_stats
mmm_stats.mhmm <- function(x, level = 0.05) {
  stopifnot_(
    requireNamespace("seqHMM", quietly = TRUE),
    "Please install the {.pkg seqHMM} package."
  )
  check_missing(x)
  check_range(level, lower = 0.0, upper = 1.0)
  stopifnot_(
    inherits(x, "mhmm"),
    c(
      "Argument {.arg x} must be a {.cls mhmm} object.",
      `i` = "See the {.pkg seqHMM} package for more information."
    )
  )
  sumr <- summary(x)
  mmm_stats_(sumr$coefficients, sumr$vcov, level)
}

#' Internal function for MMM statistics
#'
#' @param cf A `matrix` of regression coefficients from `coef`.
#' @param vc A variance-covariance matrix from `vcov`.
#' @param level The significance level
#' @noRd
mmm_stats_ <- function(cf, vc, level) {
  coef_flat <- c()
  se_flat <- c()
  cf <- as.matrix(cf)[, -1L, drop = FALSE]
  vcov_diag <- sqrt(diag(vc))
  num_vars <- nrow(cf)
  num_clusters <- ncol(cf)
  cluster_vec <- character(num_clusters)
  variable_vec <- character(num_vars)
  clusters <- colnames(cf)
  variables <- rownames(cf)
  idx <- 0L
  for (cluster in seq_len(num_clusters)) {
    for (var in seq_len(num_vars)) {
      idx <- idx + 1L
      coef_flat <- c(coef_flat, cf[var, cluster])
      se_flat <- c(se_flat, vcov_diag[(cluster - 1L) * num_vars + var])
      cluster_vec[idx] <- clusters[cluster]
      variable_vec[idx] <- variables[var]
    }
  }
  stopifnot_(
    length(coef_flat) == length(se_flat),
    "The lengths of the coefficients and standard errors do not match."
  )
  statistic <- coef_flat / se_flat
  p_value <- 2 * (1 - stats::pnorm(abs(statistic)))
  ci_margin <- stats::qnorm(1 - level / 2.0) * se_flat
  ci_lower <- coef_flat - ci_margin
  ci_upper <- coef_flat + ci_margin
  results <- data.frame(
    cluster = cluster_vec,
    variable = variable_vec,
    estimate = coef_flat,
    std_error = se_flat,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    z_value = statistic,
    p_value = p_value
  )
  rownames(results) <- NULL
  results
}
