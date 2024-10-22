# clean_string <- function(string)
# {
#   # Helper function to convert to snake_case
#   to_snake_case <- function(x) {
#     x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
#     x <- gsub("\\s+", "_", x)
#     x <- tolower(x)
#     x
#   }
#
#   # Default replacement patterns
#   replace <- c(`'` = "", `"` = "", `%` = "_percent_", `#` = "_number_")
#
#   # Replace special characters
#   for (pat in names(replace)) {
#     string <- gsub(pat, replace[[pat]], string, fixed = TRUE)
#   }
#
#   # Transliterate to ASCII
#   string <- iconv(string, to = "ASCII//TRANSLIT", sub = "")
#
#   # Clean up starting and within the string
#   good_start <- gsub("^[^a-zA-Z0-9]*(.*)$", "\\1", string)
#   cleaned_within <- gsub("[^a-zA-Z0-9]+", ".", good_start)
#
#   # Remove trailing periods, if any
#   cleaned_within <- gsub("\\.$", "", cleaned_within)
#
#   # Apply make.names
#   made_names <- make.names(cleaned_within)
#
#   # Convert to snake_case
#   cased_names <- to_snake_case(made_names)
#
#   # Handle duplicated names (allow_dupes = FALSE)
#   while (any(duplicated(cased_names))) {
#     dupe_count <- vapply(seq_along(cased_names), function(i) {
#       sum(cased_names[i] == cased_names[1:i])
#     }, 1L)
#     cased_names[dupe_count > 1] <- paste(cased_names[dupe_count > 1], dupe_count[dupe_count > 1], sep = "_")
#   }
#
#   return(cased_names)
# }


# # Function to compute the transition matrix for a given stslist object
# compute_transition_matrix <- function(sequence_stslist, original_trans_matrix) {
#   trans_matrix <- suppressMessages(TraMineR::seqtrate(sequence_stslist))  # Compute transition matrix
#   colnames(trans_matrix) <- clean_string(colnames(trans_matrix))
#   rownames(trans_matrix) <- clean_string(rownames(trans_matrix))
#   return(as.vector(trans_matrix))  # Convert to vector for easier manipulation
# }
#
# # Function to perform bootstrapping on resampled sequence data
# bootstrap_transitions <- function(data, indices) {
#   resampled_sequence <- data[indices, , drop = FALSE]  # Resample the data
#   resampled_stslist <- suppressMessages(TraMineR::seqdef(resampled_sequence))
#   return(compute_transition_matrix(resampled_stslist))  # Compute transition matrix for resampled data
# }


# # Adds the bootstrapped model's transition matrix to the `tna` object
# bootstrapper <- function(x, new_matrix, cluster) {
#   # Create a new list to store the new model
#   new_model <- x
#
#   # Store the original Model object
#   new_model$original[[cluster]] <- x$transits[[cluster]]
#
#   # Replace Model$transits[[1]] with Results_Bootstrapping$significant_trans_matrix
#   new_model$transits[[cluster]] <- new_matrix
#   new_model$igraph_network[[cluster]] <- igraph::graph_from_adjacency_matrix(new_matrix,
#                                           mode = "directed", weighted = TRUE)
#
#   # Return the new model object
#   return(new_model)
# }



#' Bootstrap Transition Networks from Sequence Data
#'
#' This function performs bootstrapping on transition networks created from
#' sequence data stored in a `tna` object. It resamples the sequences,
#' computes transition matrices, and returns bootstrapped estimates
#' of transitions with confidence intervals and significance testing.
#'
#' @export
#' @param x A `tna` object created from the sequence data.
#' @param b An integer specifying the number of bootstrap samples to
#' be generated. Defaults to `1000`.
#' @param level A `numeric` value representing the significance level for
#' hypothesis testing and confidence intervals. Defaults to `0.05`.
#' @param cluster An integer specifying which cluster of the sequence data to
#' use for bootstrapping. Defaults to `1`.
#'
#' @details
#' The function first computes the original transition matrix for the specified
#' cluster from the `tna` object. It then performs bootstrapping by resampling
#' the sequence data and recalculating the transition matrices for each bootstrap
#' sample. The mean and standard deviation of the transitions are computed, and
#' confidence intervals are derived. The function also calculates p-values for each
#' transition and identifies significant transitions based on the specified
#' significance level. A matrix of significant transitions (those with p-values
#' below the significance level) is generated. Additional statistics on removed
#' edges (transitions not considered significant) are provided.
#'
#' All results, including the original transition matrix, bootstrapped
#' estimates, and summary statistics for removed edges, are returned in a
#' structured list.
#'
#' @return A `list` containing:
#'
#' * `model`: A new `tna` object based on the significant transition matrix.
#' * `results`: A list containing the following elements:
#'    * `original_trans_matrix`:
#'      The original transition matrix for the selected cluster.
#'    * `mean_trans_matrix`:
#'      The mean transition matrix from the bootstrap samples.
#'    * `sd_trans_matrix`:
#'      The standard deviation matrix from the bootstrap samples.
#'    * `ci_lower_matrix`:
#'      The lower bound of the confidence intervals for the transitions.
#'    * `ci_upper_matrix`:
#'      The upper bound of the confidence intervals for the transitions.
#'    * `p_values_matrix`: The matrix of p-values for the transitions.
#'    * `significant_trans_matrix`: The matrix of significant transitions
#'      (those with p-values below the significance level).
#'    * `combined_df`: A data frame summarizing the transitions, their weights,
#'      p-values, and confidence intervals.
#'    * `removed_edges_summary`: A list summarizing the number of removed edges
#'      (insignificant transitions), the mean and standard deviation of their
#'      weights, and the range of the removed edge weights.
#'
#' @family evaluation
#' @examples
#' \dontrun{
#' # Bootstrap transition networks for a cluster
#' results <- bootstrap(tna_model, v = 1000, level = 0.05, cluster = 1)
#' }
#'
bootstrap <- function(x, ...) {
  UseMethod("bootstrap", ...)
}

#' @rdname bootstrap
#' @export
bootstrap.tna <- function(x, b = 1000, level = 0.05, cluster = 1) {
  stopifnot_(
    !is.null(x),
    "Argument {.arg x} must be a {.cls tna} object created from the `TraMineR` sequence object."
  )
  d <- x$seq[[cluster]]
  alphabet <- attr(d, "alphabet")
  obs_matrix <- as.data.frame(d) |>
    mutate(across(everything(), ~ replace(.x, which(!.x %in% alphabet), NA))) |>
    mutate(across(everything(), as.integer)) |>
    as.matrix()
  n <- nrow(obs_matrix)
  p <- ncol(obs_matrix)
  a <- length(alphabet)
  idx <- seq_len(n)
  transitions <- array(0L, dim = c(n, a, a))
  for (i in seq_len(p - 1)) {
    from <- obs_matrix[, i]
    to <- obs_matrix[, i + 1L]
    any_na <- is.na(from) | is.na(to)
    new_transitions <- cbind(idx, from, to)[!any_na, , drop = FALSE]
    transitions[new_transitions] <- transitions[new_transitions] + 1L
  }
  probs <- apply(transitions, c(2, 3), sum)
  probs <- probs / .rowSums(probs, m = a, n = a)
  boot_probs <- array(0L, dim = c(b, a, a))
  p_values <- matrix(0, a, a)
  for (i in seq_len(b)) {
    boot_transitions <- transitions[sample(idx, n, replace = TRUE), , ]
    boot_freq <- apply(boot_transitions, c(2, 3), sum)
    boot_probs[i, , ] <- boot_freq / .rowSums(boot_freq, m = a, n = a)
    p_values <- p_values + 1L * (boot_probs[i, , ] >= probs)
  }
  p_values <- p_values / b
  mean_transitions <- apply(boot_probs, c(2, 3), mean)
  sd_transitions <- apply(boot_probs, c(2, 3), stats::sd)
  ci_lower <- apply(boot_probs, c(2, 3), stats::quantile, probs = level / 2)
  ci_upper <- apply(boot_probs, c(2, 3), stats::quantile, probs = 1 - level / 2)

#
 #  # Significant transitions: keep only those with p-values < significance level
 #  significant_trans_matrix <- matrix(ifelse(p_values < sig_level, original_trans_matrix_vector, 0),
 #                                     nrow = nrow(original_trans_matrix),
 #                                     ncol = ncol(original_trans_matrix))
#
 #  # Identify removed edges and calculate summary statistics for them
 #  removed_edges <- original_trans_matrix_vector[significant_trans_matrix == 0]
 #  n_removed_edges <- length(removed_edges)
 #  mean_removed_edges <- if (n_removed_edges > 0) mean(removed_edges) else NA
 #  sd_removed_edges <- if (n_removed_edges > 0) stats::sd(removed_edges) else NA
 #  range_removed_edges <- if (n_removed_edges > 0) range(removed_edges) else c(NA, NA)
#
 #  # Create matrices for the mean, standard deviation, and confidence intervals
 #  n_states <- nrow(original_trans_matrix)
 #  mean_trans_matrix <- matrix(mean_transitions, nrow = n_states, ncol = n_states)
 #  sd_trans_matrix <- matrix(sd_transitions, nrow = n_states, ncol = n_states)
 #  ci_lower_matrix <- matrix(ci_lower, nrow = n_states, ncol = n_states)
 #  ci_upper_matrix <- matrix(ci_upper, nrow = n_states, ncol = n_states)
 #  p_values_matrix <- matrix(p_values, nrow = n_states, ncol = n_states)
#
 #  # Assign row and column names to all matrices
 #  colnames(mean_trans_matrix) <- colnames(original_trans_matrix)
 #  rownames(mean_trans_matrix) <- rownames(original_trans_matrix)
#
 #  colnames(sd_trans_matrix) <- colnames(original_trans_matrix)
 #  rownames(sd_trans_matrix) <- rownames(original_trans_matrix)
#
 #  colnames(ci_lower_matrix) <- colnames(original_trans_matrix)
 #  rownames(ci_lower_matrix) <- rownames(original_trans_matrix)
#
 #  colnames(ci_upper_matrix) <- colnames(original_trans_matrix)
 #  rownames(ci_upper_matrix) <- rownames(original_trans_matrix)
#
 #  colnames(p_values_matrix) <- colnames(original_trans_matrix)
 #  rownames(p_values_matrix) <- rownames(original_trans_matrix)
#
 #  colnames(significant_trans_matrix) <- colnames(original_trans_matrix)
 #  rownames(significant_trans_matrix) <- rownames(original_trans_matrix)
#
 #  # Combine the transition data into a data frame for easy visualization
 #  combined_df <- data.frame(
 #    From = rep(rownames(original_trans_matrix), each = n_states),
 #    To = rep(colnames(original_trans_matrix), times = n_states),
 #    Edge_Weight = as.vector(original_trans_matrix),
 #    p_value = as.vector(p_values_matrix),
 #    CI_Lower = as.vector(ci_lower_matrix),
 #    CI_Upper = as.vector(ci_upper_matrix)
 #  )
#
 #  new_model <- bootstrapper(x, new_matrix = significant_trans_matrix,
 #                            cluster = cluster)

  # Return all the relevant results in a list
  results <- list(
    model = new_model,
    results = list (
      original_trans_matrix = original_trans_matrix,
      mean_trans_matrix = mean_trans_matrix,
      sd_trans_matrix = sd_trans_matrix,
      ci_lower_matrix = ci_lower_matrix,
      ci_upper_matrix = ci_upper_matrix,
      p_values_matrix = p_values_matrix,
      significant_trans_matrix = significant_trans_matrix,
      combined_df = combined_df,
      removed_edges_summary = list(
        n_removed_edges = n_removed_edges,
        mean_removed_edges = mean_removed_edges,
        sd_removed_edges = sd_removed_edges,
        range_removed_edges = range_removed_edges
      )
    )
  )

  return(results)
}

