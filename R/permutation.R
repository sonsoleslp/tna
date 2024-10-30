#' Compare Two Networks from Sequence Data Using Permutation Tests
#'
#' This function compares two networks built from sequence data using permutation tests.
#' The function builds Markov models for two sequence objects , computes the transition
#' probabilities, and compares them by performing permutation tests. It returns the
#' differences in transition probabilities, effect sizes, p-values, and confidence intervals.
#'
#' @export
#' @family evaluation
#' @param x A `tna` object containing sequence data for the first `tna` model.
#' @param y A `tna` object containing sequence data for the second `tna` model.
#' @param iter An `integer` gicing the number of permutations to perform.
#' The default is 1000.
#' @param paired A `logical` value. If `TRUE`, perform paired permutation tests;
#' if `FALSE`, perform unpaired tests. The default is `FALSE`.
#' @param level A `numeric` value gicing the significance level for the
#' permutation tests. The default is 0.05.
#' @return A `list` containing:
#'
#'   * `edge_statistics` A data frame with edge names, original differences,
#'     and p-values.
#'   * `original_weights_x` The edge weight matrix from
#'     the first `tna` model `x`.
#'   * `original_weights_y` The edge weight matrix from
#'     the second `tna` model `y`.
#'   * `true_diffs` The matrix of differences in weights.
#'   * `sig_diffs` A matrix showing the significant differences
#'     based on the permutation test.
#'
#' @examples
#' model_x <- tna(group_regulation[1:1000,])
#' model_y <- tna(group_regulation[1001:2000,])
#' # Small number of iterations for CRAN
#' permutation_test(model_x, model_y, iter = 50)
#'
permutation_test <- function(x, y, iter = 1000, paired = FALSE, level = 0.05) {
  check_tna_seq(x)
  check_tna_seq(y)
  check_positive(iter)
  check_flag(paired)
  check_probability(level)
  # TODO check that networks can be compared
  data_x <- x$data
  data_y <- y$data
  weights_x <- x$weights
  weights_y <- y$weights
  a <- length(attr(data_x, "alphabet"))
  type <- attr(x, "type")
  diffs_true <- weights_x - weights_y
  diffs_true_abs <- abs(diffs_true)
  edge_names <- expand.grid(
    from = rownames(weights_x),
    to = colnames(weights_y)
  )
  edge_names <- paste0(edge_names$from, " -> ", edge_names$to)
  combined_data <- rbind(data_x, data_y)
  n_data_x <- nrow(data_x)
  n_combined <- n_data_x + nrow(data_y)
  perm_x <- seq_len(n_data_x)
  perm_y <- seq(n_data_x + 1L, n_combined)
  combined_model <- markov_model(combined_data, transitions = TRUE)
  combined_trans <- combined_model$trans
  diffs_perm <- array(0L, dim = c(iter, a, a))
  p_values <- matrix(0L, a, a)
  for (i in seq_len(iter)) {
    if (paired) {
      # For paired data, permute within pairs
      pair_idx <- matrix(seq_len(n_combined), ncol = 2, byrow = TRUE)
      permuted_pairs <- t(apply(pair_idx, 1, sample))
      perm_idx <- c(permuted_pairs)
    } else {
      # For unpaired data, perform complete randomization
      perm_idx <- sample(n_combined)
    }
    trans_perm_x <- combined_trans[perm_idx[perm_x], , ]
    trans_perm_y <- combined_trans[perm_idx[perm_y], , ]
    weights_perm_x <- compute_weights(trans_perm_x, type, s = a)
    weights_perm_y <- compute_weights(trans_perm_y, type, s = a)
    diffs_perm[i, , ] <- weights_perm_x - weights_perm_y
    p_values <- p_values + 1L * (abs(diffs_perm[i, , ]) >= diffs_true_abs)
  }
  p_values <- p_values / iter
  sig_diffs <- diffs_true * (p_values < level)
  edge_stats <- data.frame(
    edge_name = edge_names,
    diff_true = c(diffs_true),
    p_value = c(p_values),
    stringsAsFactors = FALSE
  )
  list(
    edge_statistics = edge_stats,
    original_weights_x = weights_x,
    original_weights_y = weights_y,
    true_diffs = diffs_true,
    sig_diffs = sig_diffs
  )
}
