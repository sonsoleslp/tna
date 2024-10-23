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
#' @param cluster1 An `integer` indicating the cluster to be used for
#' the first `tna` model. Default is 1.
#' @param cluster2 An `integer` indicating the cluster to be used for
#' the second `tna` model. Default is 1.
#' @param iter The number of permutations to perform. Default is 1000.
#' @param paired Logical. If `TRUE`, perform paired permutation tests;
#' if `FALSE`, perform unpaired tests. Default is `FALSE`.
#' @param alpha The significance level for the permutation tests.
#' The default is 0.05.
#' @return A `list` containing:
#'
#'   * `edge_statistics` A data frame with edge names, original differences,
#'   and p-values.
#'   * `original_weights1` The transition probability matrix from
#'   the first `tna` model
#'   * `original_weights2` The transition probability matrix from
#'   the second `tna` model.
#'   * `true_diffs` The matrix of differences in weights.
#'   * `sig_diffs` A matrix showing the significant differences
#'   based on the permutation test.
#'
#' @examples
#' \dontrun{
#' result <- permutation_test(network1, network2, cluster1 = 1, cluster2 = 1, it = 1000)
#' }
#'
permutation_test <- function(x, y, cluster1 = 1, cluster2 = 1, iter = 1000,
                             paired = FALSE, alpha = 0.05) {
  stopifnot_(
    !is.null(x),
    "Argument {.arg x} must be a {.cls tna} object created from the `TraMineR` sequence object."
  )
  stopifnot_(
    !is.null(y),
    "Argument {.arg y} must be a {.cls tna} object created from the `TraMineR` sequence object."
  )
  seq1 <- x$seq[[cluster1]]
  seq2 <- y$seq[[cluster2]]
  weights1 <- x$weights[[cluster1]]
  weights2 <- y$weights[[cluster2]]
  a <- length(attr(seq1, "alphabet"))
  type <- attr(x, "type")
  diffs_true <- weights1 - weights2
  diffs_true_abs <- abs(diffs_true)
  edge_names <- expand.grid(from = rownames(weights1), to = colnames(weights2))
  edge_names <- paste0(edge_names$from, " -> ", edge_names$to)
  combined_seq <- rbind(seq1, seq2)
  n_seq1 <- nrow(seq1)
  n_combined <- n_seq1 + nrow(seq2)
  perm1 <- seq_len(n_seq1)
  perm2 <- seq(n_seq1 + 1L, n_combined)
  combined_model <- markov_model(combined_seq, transitions = TRUE)
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
    trans1_perm <- combined_trans[perm_idx[perm1], , ]
    trans2_perm <- combined_trans[perm_idx[perm2], , ]
    weights1_perm <- compute_weights(trans1_perm, type, s = a)
    weights2_perm <- compute_weights(trans2_perm, type, s = a)
    diffs_perm[i, , ] <- weights1_perm - weights2_perm
    p_values <- p_values + 1L * (abs(diffs_perm[i, , ]) >= diffs_true_abs)
  }
  p_values <- p_values / iter
  sig_diffs <- diffs_true
  sig_diffs[p_values < alpha] <- 0
  edge_stats <- data.frame(
    edge_name = edge_names,
    diff_true = c(diffs_true),
    p_values = c(p_values),
    stringsAsFactors = FALSE
  )
  list(
    edge_statistics = edge_stats,
    original_weights1 = weights1,
    original_weights2 = weights2,
    true_diffs = diffs_true,
    sig_diffs = sig_diffs
  )
}
