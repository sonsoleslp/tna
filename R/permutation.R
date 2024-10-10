# Function to format p-values nicely
format_pvalue <- function(p) {
  if (p < 0.001) return("< 0.001")
  return(sprintf("%.3f", p))
}

# Function to create a horizontal line
create_line <- function(width = 80, char = "-") {
  paste0(rep(char, width), collapse = "")
}


#' Compare Two Networks from Sequence Data Using Permutation Tests
#'
#' This function compares two networks built from sequence data using permutation tests.
#' The function builds Markov models for two sequence objects , computes the transition
#' probabilities, and compares them by performing permutation tests. It returns the
#' differences in transition probabilities, effect sizes, p-values, and confidence intervals.
#'
#' @param x A `tna` object containing sequence data for the first `tna` model.
#' @param y A `tna` object containing sequence data for the second `tna` model.
#' @param cluster1 An integer indicating the cluster to be used from the first `tna` model. Default is 1.
#' @param cluster2 An integer indicating the cluster to be used from the second `tna` model. Default is 1.
#' @param it The number of permutations to perform. Default is 1000.
#' @param paired Logical. If `TRUE`, perform paired permutation tests; if `FALSE`, perform unpaired tests. Default is `FALSE`.
#' @param alpha The significance level for the permutation tests. Default is 0.05.
#' @param progressbar Logical. If `TRUE`, display a progress bar during the permutation process. Default is `TRUE`.
#'
#' @return A list containing:
#'   \item{edge_statistics}{A data frame with edge names, original differences, p-values, effect sizes, and confidence intervals.}
#'   \item{original_matrix1}{The transition probability matrix from the first `tna` model}
#'   \item{original_matrix2}{The transition probability matrix from the second `tna` model.}
#'   \item{difference_matrix}{The matrix of differences in transition probabilities.}
#'   \item{significant_matrix}{A matrix showing the significant edges based on the permutation tests.}
#'   \item{significant_edges}{A list of significant edges (p < alpha).}
#'   \item{statistics_summary}{A list of summary statistics including mean absolute differences and effect sizes.}
#' @author
#' Mohammed Saqr (\email{mohammed.saqr@uef.fi})
#' @examples
#' \dontrun{
#' # Assuming `network1` and `network2` are tna objects containing sequence data
#' result <- compare_tna_networks(network1, network2, cluster1 = 1, cluster2 = 1, it = 1000)
#' }
#' @export
compare_tna_networks <- function(x, y, cluster1 = 1, cluster2 = 1, it = 1000,
                                 paired = FALSE, alpha = 0.05,
                                 progressbar = TRUE) {

  stopifnot_(
    !is.null(x),
    "Argument {.arg x} must be a {.cls tna} object created from the `TraMineR` sequence object."
  )

  stopifnot_(
    !is.null(y),
    "Argument {.arg y} must be a {.cls tna} object created from the `TraMineR` sequence object."
  )

  sequence1 = x$seq[[cluster1]]
  sequence2 = y$seq[[cluster2]]


  # Step 2: Extract transition probabilities
  transits1 <- x$transits[[cluster1]]
  transits2 <- y$transits[[cluster2]]

  # Ensure matrices have row and column names
  if (is.null(rownames(transits1))) {
    rownames(transits1) <- paste0("State", 1:nrow(transits1))
  }
  if (is.null(colnames(transits1))) {
    colnames(transits1) <- paste0("State", 1:ncol(transits1))
  }
  rownames(transits2) <- rownames(transits1)
  colnames(transits2) <- colnames(transits1)

  # Step 3: Compute edge differences
  edge_diffs_real <- transits1 - transits2  # Difference without absolute values
  edge_names <- expand.grid(from = rownames(transits1), to = colnames(transits1))
  edge_names <- paste(edge_names$from, "->", edge_names$to)

  edge_diffs_real_flat <- as.vector(edge_diffs_real)

  # Initialize storage for permutation results
  perm_edge_diffs <- matrix(0, nrow = it, ncol = length(edge_diffs_real_flat))

  # Combine sequences for permutation
  combined_sequences <- rbind(sequence1, sequence2)

  # Progress bar setup
  if (progressbar) {
    pb <- utils::txtProgressBar(min = 0, max = it, style = 3)
  }

  # Step 4: Perform permutation testing
  for (i in 1:it) {
    if (paired) {
      # For paired data, permute within pairs
      pair_indices <- matrix(1:nrow(combined_sequences), ncol = 2, byrow = TRUE)
      permuted_pairs <- t(apply(pair_indices, 1, sample))
      perm_indices <- as.vector(permuted_pairs)
    } else {
      # For unpaired data, perform complete randomization
      perm_indices <- sample(nrow(combined_sequences))
    }

    perm_sequence1 <- combined_sequences[perm_indices[1:nrow(sequence1)], ]
    perm_sequence2 <- combined_sequences[perm_indices[(nrow(sequence1) + 1):nrow(combined_sequences)], ]

    # Build Markov models for permuted sequences
    perm_mkvmodel1 <- seqHMM::build_mm(perm_sequence1)
    perm_mkvmodel2 <- seqHMM::build_mm(perm_sequence2)

    # Extract transition probabilities
    perm_transits1 <- perm_mkvmodel1$transition_probs
    perm_transits2 <- perm_mkvmodel2$transition_probs

    # Store differences (not absolute differences)
    perm_edge_diffs[i, ] <- as.vector(perm_transits1 - perm_transits2)

    if (progressbar) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  if (progressbar) {
    close(pb)
  }

  # Step 5: Create edge statistics data frame
  edge_stats <- data.frame(
    Edge_Name = edge_names,
    Original_Diff = edge_diffs_real_flat,
    stringsAsFactors = FALSE
  )

  # Calculate two-tailed p-values
  edge_stats$P_Value <- sapply(1:ncol(perm_edge_diffs), function(j) {
    more_extreme <- sum(abs(perm_edge_diffs[, j]) >= abs(edge_diffs_real_flat[j]))
    p_value <- more_extreme / it
    return(p_value)
  })

  # Calculate confidence intervals
  ci_lower <- apply(perm_edge_diffs, 2, stats::quantile, probs = alpha/2)
  ci_upper <- apply(perm_edge_diffs, 2, stats::quantile, probs = 1 - alpha/2)
  edge_stats$CI_Lower <- ci_lower
  edge_stats$CI_Upper <- ci_upper

  # Calculate effect sizes
  edge_stats$Effect_Size <- edge_diffs_real_flat / apply(perm_edge_diffs, 2, stats::sd)

  # Step 6: Calculate summary statistics
  abs_diffs <- abs(edge_diffs_real_flat)
  abs_mean_diff <- mean(abs_diffs)
  significant_indices <- which(edge_stats$P_Value < alpha)
  abs_mean_diff_sig <- if(length(significant_indices) > 0) {
    mean(abs_diffs[significant_indices])
  } else {
    NA
  }
  min_diff <- min(abs_diffs)  # Changed to use absolute differences
  max_diff <- max(abs_diffs)  # Changed to use absolute differences

  # Create list of significant edges
  significant_edges <- edge_names[significant_indices]

  # Create significant matrix with proper labeling
  significant_matrix <- matrix(0, nrow = nrow(transits1), ncol = ncol(transits1))
  rownames(significant_matrix) <- rownames(transits1)
  colnames(significant_matrix) <- colnames(transits1)

  if(length(significant_indices) > 0) {
    # Convert flat indices to matrix indices
    row_indices <- (significant_indices - 1) %/% ncol(significant_matrix) + 1
    col_indices <- (significant_indices - 1) %% ncol(significant_matrix) + 1

    for(i in seq_along(significant_indices)) {
      significant_matrix[row_indices[i], col_indices[i]] <- edge_diffs_real_flat[significant_indices[i]]
    }
  }

  # Step 7: Print formatted output
  info_("\n", create_line(80, "="), "\n")
  info_("SEQUENCE NETWORK COMPARISON RESULTS\n")
  info_(create_line(80, "="), "\n\n")

  # Print Analysis Parameters
  info_("ANALYSIS PARAMETERS:\n")
  info_(create_line(40, "-"), "\n")
  info_(sprintf("%-25s %d\n", "Number of Permutations:", it))
  info_(sprintf("%-25s %s\n", "Analysis Type:", if(paired) "Paired" else "Unpaired"))
  info_(sprintf("%-25s %.3f\n", "Significance Level:", alpha))
  info_("\n")

  # Print Summary Statistics
  info_("SUMMARY STATISTICS:\n")
  info_(create_line(40, "-"), "\n")
  info_(sprintf("%-35s %.4f\n", "Mean Absolute Difference (Overall):", abs_mean_diff))
  if (!is.na(abs_mean_diff_sig)) {
    info_(sprintf("%-35s %.4f\n", "Mean Absolute Difference (Significant):", abs_mean_diff_sig))
  }
  info_(sprintf("%-35s %.4f\n", "Maximum Absolute Difference:", max_diff))
  info_(sprintf("%-35s %.4f\n", "Minimum Absolute Difference:", min_diff))
  info_("\n")

  # Print Transition Matrices
  info_("SIGNIFICANT TRANSITIONS MATRIX:\n")
  info_(create_line(40, "-"), "\n")
  info_(round(significant_matrix, 4))
  info_("\n")

  # Print Significant Edges
  info_("SIGNIFICANT EDGES (p < ", alpha, "):\n", sep="")
  info_(create_line(40, "-"), "\n")
  if (length(significant_edges) > 0) {
    # Sort by absolute difference
    sig_edges_df <- edge_stats[significant_indices, ]
    sig_edges_df <- sig_edges_df[order(abs(sig_edges_df$Original_Diff), decreasing = TRUE), ]

    # Print header
    info_(sprintf("%-20s %10s %10s %12s %10s\n",
                "Edge", "Diff", "Effect", "95% CI", "p-value"))
    info_(create_line(65, "-"), "\n")

    # Print each significant edge
    for (i in 1:nrow(sig_edges_df)) {
      info_(sprintf("%-20s %10.3f %10.2f [%6.3f,%6.3f] %10s\n",
                  substr(sig_edges_df$Edge_Name[i], 1, 20),
                  sig_edges_df$Original_Diff[i],
                  sig_edges_df$Effect_Size[i],
                  sig_edges_df$CI_Lower[i],
                  sig_edges_df$CI_Upper[i],
                  format_pvalue(sig_edges_df$P_Value[i])))
    }
  } else {
    info_("No significant edges found.\n")
  }
  info_("\n")

  # Print Top 10 Non-significant Edges (if any exist)
  nonsig_indices <- which(edge_stats$P_Value >= alpha)
  if (length(nonsig_indices) > 0) {
    info_("TOP 10 NON-SIGNIFICANT EDGES:\n")
    info_(create_line(40, "-"), "\n")

    # Sort non-significant edges by absolute difference
    nonsig_edges <- edge_stats[nonsig_indices, ]
    nonsig_edges <- nonsig_edges[order(abs(nonsig_edges$Original_Diff), decreasing = TRUE), ]
    nonsig_edges <- utils::head(nonsig_edges, 10)

    # Print header
    info_(sprintf("%-20s %10s %10s %12s %10s\n",
                "Edge", "Diff", "Effect", "95% CI", "p-value"))
    info_(create_line(65, "-"), "\n")

    # Print each non-significant edge
    for (i in 1:nrow(nonsig_edges)) {
      info_(sprintf("%-20s %10.3f %10.2f [%6.3f,%6.3f] %10s\n",
                  substr(nonsig_edges$Edge_Name[i], 1, 20),
                  nonsig_edges$Original_Diff[i],
                  nonsig_edges$Effect_Size[i],
                  nonsig_edges$CI_Lower[i],
                  nonsig_edges$CI_Upper[i],
                  format_pvalue(nonsig_edges$P_Value[i])))
    }
  }

  info_("\n", create_line(80, "="), "\n")

  # Return results
  return(list(
    edge_statistics = edge_stats,
    original_matrix1 = transits1,
    original_matrix2 = transits2,
    difference_matrix = edge_diffs_real,
    significant_matrix = significant_matrix,
    significant_edges = significant_edges,
    statistics_summary = list(
      abs_mean_diff = abs_mean_diff,
      abs_mean_diff_sig = abs_mean_diff_sig,
      min_abs_diff = min_diff,
      max_abs_diff = max_diff
    )
  ))
}
