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
#'   p-values, effect sizes, and confidence intervals.
#'   * `original_matrix1` The transition probability matrix from
#'   the first `tna` model
#'   * `original_matrix2` The transition probability matrix from
#'   the second `tna` model.
#'   * `difference_matrix` The matrix of differences in transition
#'   probabilities.
#'   * `significant_matrix` A matrix showing the significant edges
#'   based on the permutation tests.
#'   * `significant_edges` A list of significant edges (p < alpha).
#'   * `statistics_summary` A list of summary statistics including
#'   mean absolute differences and effect sizes.
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
  true_diffs <- weights1 - weights2
  true_diffs_abs <- abs(true_diffs)
  edge_names <- expand.grid(from = rownames(weights1), to = colnames(weights2))
  edge_names <- paste0(edge_names$from, " -> ", edge_names$to)
  combined_seq <- rbind(seq1, seq2)
  n_seq1 <- nrow(seq1)
  n_combined <- n_seq1 + nrow(seq2)
  perm1 <- seq_len(n_seq1)
  perm2 <- seq(n_seq1 + 1L, n_combined)
  combined_model <- build_markov_model(combined_seq, transitions = TRUE)
  combined_trans <- combined_model$trans
  perm_diffs <- array(0L, dim = c(iter, a, a))
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
    perm_trans1 <- combined_trans[perm_idx[perm1], , ]
    perm_trans2 <- combined_trans[perm_idx[perm2], , ]
    perm_probs1 <- apply(perm_trans1, c(2, 3), sum)
    perm_probs1 <- perm_probs1 / .rowSums(perm_probs1, m = a, n = a)
    perm_probs2 <- apply(perm_trans2, c(2, 3), sum)
    perm_probs2 <- perm_probs2 / .rowSums(perm_probs2, m = a, n = a)
    perm_diffs[i, , ] <- perm_probs1 - perm_probs2
    p_values <- p_values + 1L * (abs(perm_diffs[i, , ]) >= true_diffs_abs)
  }
  p_values <- p_values / iter
  edge_stats <- data.frame(
    edge_name = edge_names,
    true_diff = c(true_diffs),
    p_values = c(p_values),
    stringsAsFactors = FALSE
  )
  edge_stats

  # Calculate confidence intervals
  # ci_lower <- apply(perm_edge_diffs, 2, stats::quantile, probs = alpha/2)
  # ci_upper <- apply(perm_edge_diffs, 2, stats::quantile, probs = 1 - alpha/2)
  # edge_stats$CI_Lower <- ci_lower
  # edge_stats$CI_Upper <- ci_upper

  # Calculate effect sizes
  # edge_stats$Effect_Size <- edge_diffs_real_flat / apply(perm_edge_diffs, 2, stats::sd)

  # Step 6: Calculate summary statistics
  # abs_diffs <- abs(edge_diffs_real_flat)
  # abs_mean_diff <- mean(abs_diffs)
  # significant_indices <- which(edge_stats$P_Value < alpha)
  # abs_mean_diff_sig <- if(length(significant_indices) > 0) {
  #   mean(abs_diffs[significant_indices])
  # } else {
  #   NA
  # }
  # min_diff <- min(abs_diffs)  # Changed to use absolute differences
  # max_diff <- max(abs_diffs)  # Changed to use absolute differences
  #
  # # Create list of significant edges
  # significant_edges <- edge_names[significant_indices]
  #
  # # Create significant matrix with proper labeling
  # significant_matrix <- matrix(0, nrow = nrow(transits1), ncol = ncol(transits1))
  # rownames(significant_matrix) <- rownames(transits1)
  # colnames(significant_matrix) <- colnames(transits1)
  #
  # if(length(significant_indices) > 0) {
  #   # Convert flat indices to matrix indices
  #   row_indices <- (significant_indices - 1) %/% ncol(significant_matrix) + 1
  #   col_indices <- (significant_indices - 1) %% ncol(significant_matrix) + 1
  #
  #   for(i in seq_along(significant_indices)) {
  #     significant_matrix[ col_indices[i], row_indices[i]] <- edge_diffs_real_flat[significant_indices[i]]
  #   }
  # }

  # # Step 7: Print formatted output
  # info_("\n", create_line(80, "="), "\n")
  # info_("SEQUENCE NETWORK COMPARISON RESULTS\n")
  # info_(create_line(80, "="), "\n\n")
  #
  # # Print Analysis Parameters
  # info_("ANALYSIS PARAMETERS:\n")
  # info_(create_line(40, "-"), "\n")
  # info_(sprintf("%-25s %d\n", "Number of Permutations:", it))
  # info_(sprintf("%-25s %s\n", "Analysis Type:", if(paired) "Paired" else "Unpaired"))
  # info_(sprintf("%-25s %.3f\n", "Significance Level:", alpha))
  # info_("\n")
  #
  # # Print Summary Statistics
  # info_("SUMMARY STATISTICS:\n")
  # info_(create_line(40, "-"), "\n")
  # info_(sprintf("%-35s %.4f\n", "Mean Absolute Difference (Overall):", abs_mean_diff))
  # if (!is.na(abs_mean_diff_sig)) {
  #   info_(sprintf("%-35s %.4f\n", "Mean Absolute Difference (Significant):", abs_mean_diff_sig))
  # }
  # info_(sprintf("%-35s %.4f\n", "Maximum Absolute Difference:", max_diff))
  # info_(sprintf("%-35s %.4f\n", "Minimum Absolute Difference:", min_diff))
  # info_("\n")
  #
  # # Print Transition Matrices
  # info_("SIGNIFICANT TRANSITIONS MATRIX:\n")
  # info_(create_line(40, "-"), "\n")
  # info_(round(significant_matrix, 4))
  # info_("\n")
  #
  # # Print Significant Edges
  # info_("SIGNIFICANT EDGES (p < ", alpha, "):\n", sep="")
  # info_(create_line(40, "-"), "\n")
  # if (length(significant_edges) > 0) {
  #   # Sort by absolute difference
  #   sig_edges_df <- edge_stats[significant_indices, ]
  #   sig_edges_df <- sig_edges_df[order(abs(sig_edges_df$Original_Diff), decreasing = TRUE), ]
  #
  #   # Print header
  #   info_(sprintf("%-20s %10s %10s %12s %10s\n",
  #               "Edge", "Diff", "Effect", "95% CI", "p-value"))
  #   info_(create_line(65, "-"), "\n")
  #
  #   # Print each significant edge
  #   for (i in 1:nrow(sig_edges_df)) {
  #     info_(sprintf("%-20s %10.3f %10.2f [%6.3f,%6.3f] %10s\n",
  #                 substr(sig_edges_df$Edge_Name[i], 1, 20),
  #                 sig_edges_df$Original_Diff[i],
  #                 sig_edges_df$Effect_Size[i],
  #                 sig_edges_df$CI_Lower[i],
  #                 sig_edges_df$CI_Upper[i],
  #                 format_pvalue(sig_edges_df$P_Value[i])))
  #   }
  # } else {
  #   info_("No significant edges found.\n")
  # }
  # info_("\n")
  #
  # # Print Top 10 Non-significant Edges (if any exist)
  # nonsig_indices <- which(edge_stats$P_Value >= alpha)
  # if (length(nonsig_indices) > 0) {
  #   info_("TOP 10 NON-SIGNIFICANT EDGES:\n")
  #   info_(create_line(40, "-"), "\n")
  #
  #   # Sort non-significant edges by absolute difference
  #   nonsig_edges <- edge_stats[nonsig_indices, ]
  #   nonsig_edges <- nonsig_edges[order(abs(nonsig_edges$Original_Diff), decreasing = TRUE), ]
  #   nonsig_edges <- utils::head(nonsig_edges, 10)
  #
  #   # Print header
  #   info_(sprintf("%-20s %10s %10s %12s %10s\n",
  #               "Edge", "Diff", "Effect", "95% CI", "p-value"))
  #   info_(create_line(65, "-"), "\n")
  #
  #   # Print each non-significant edge
  #   for (i in 1:nrow(nonsig_edges)) {
  #     info_(sprintf("%-20s %10.3f %10.2f [%6.3f,%6.3f] %10s\n",
  #                 substr(nonsig_edges$Edge_Name[i], 1, 20),
  #                 nonsig_edges$Original_Diff[i],
  #                 nonsig_edges$Effect_Size[i],
  #                 nonsig_edges$CI_Lower[i],
  #                 nonsig_edges$CI_Upper[i],
  #                 format_pvalue(nonsig_edges$P_Value[i])))
  #   }
  # }
  #
  # info_("\n", create_line(80, "="), "\n")
#
#   # Return results
#   list(
#     edge_statistics = edge_stats,
#     original_weights1 = weights1,
#     original_weights2 = weights2,
#     true_diffs = true_diffs,
#     sig_diffs = sig_diffs,
#     sig_edges = sig_edges,
#     statistics_summary = list(
#       abs_mean_diff = abs_mean_diff,
#       abs_mean_diff_sig = abs_mean_diff_sig,
#       min_abs_diff = min_diff,
#       max_abs_diff = max_diff
#     )
#   ))
}
