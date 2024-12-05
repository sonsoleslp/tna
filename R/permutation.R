#' Compare Two Networks from Sequence Data Using Permutation Tests
#'
#' This function compares two networks built from sequence data using
#' permutation tests. The function builds Markov models for two sequence
#' objects, computes the transition probabilities, and compares them by
#' performing permutation tests. It returns the differences in transition
#' probabilities, effect sizes, p-values, and confidence intervals.
#'
#' @export
#' @family evaluation
#' @param x A `tna` object containing sequence data for the first `tna` model.
#' @param y A `tna` object containing sequence data for the second `tna` model.
#' @param iter An `integer` giving the number of permutations to perform.
#' The default is 1000.
#' @param paired A `logical` value. If `TRUE`, perform paired permutation tests;
#' if `FALSE`, perform unpaired tests. The default is `FALSE`.
#' @param level A `numeric` value giving the significance level for the
#' permutation tests. The default is 0.05.
#' @param measures A `character` vector of centrality measures to test.
#' See [centralities()] for a list of available centrality measures.
#' @param ... Additional arguments passed to [centralities()].
#' @return A `tna_permutation` object which is a `list` with two elements:
#' `edges` and `centralities`, both containing the following elements
#'
#'   * `stats`: A `data.frame` of original differences and p-values for
#'     each edge or centrality measure
#'   * `diffs_true`: A `matrix` of differences in the data.
#'   * `diffs_sig`: A `matrix` showing the significant differences.
#'
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' # Small number of iterations for CRAN
#' permutation_test(model_x, model_y, iter = 20)
#'
permutation_test <- function(x, y, iter = 1000, paired = FALSE, level = 0.05,
                             measures = character(0), ...) {
  check_tna_seq(x)
  check_tna_seq(y)
  check_positive(iter)
  check_flag(paired)
  check_probability(level)
  # TODO check that networks can be compared
  data_x <- x$data
  data_y <- y$data
  combined_data <- rbind(data_x, data_y)
  n_data_x <- nrow(data_x)
  n_combined <- n_data_x + nrow(data_y)
  weights_x <- x$weights
  weights_y <- y$weights
  a <- length(attr(data_x, "alphabet"))
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  n_measures <- length(measures)
  include_centralities <- n_measures > 0L
  if (include_centralities) {
    cent_x <- centralities(x, measures = measures, ...)
    cent_y <- centralities(y, measures = measures, ...)
    cent_diffs_true <- as.matrix(cent_x[, -1L] - cent_y[, -1L])
    cent_diffs_true_abs <- abs(cent_diffs_true)
  }
  edge_diffs_true <- weights_x - weights_y
  edge_diffs_true_abs <- abs(edge_diffs_true)
  edge_names <- expand.grid(
    from = rownames(weights_x),
    to = colnames(weights_y)
  )
  edge_names <- paste0(edge_names$from, " -> ", edge_names$to)
  perm_x <- seq_len(n_data_x)
  perm_y <- seq(n_data_x + 1L, n_combined)
  combined_model <- initialize_model(
    combined_data,
    type,
    scaling,
    transitions = TRUE
  )
  combined_trans <- combined_model$trans
  edge_diffs_perm <- array(0L, dim = c(iter, a, a))
  cent_diffs_perm <- array(0L, dim = c(iter, a, n_measures))
  edge_p_values <- matrix(0L, a, a)
  cent_p_values <- matrix(0L, a, n_measures)
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
    weights_perm_x <- compute_weights(trans_perm_x, type, scaling, s = a)
    weights_perm_y <- compute_weights(trans_perm_y, type, scaling, s = a)
    if (include_centralities) {
      cent_perm_x <- centralities(weights_perm_x, measures = measures, ...)
      cent_perm_y <- centralities(weights_perm_y, measures = measures, ...)
      cent_diffs_perm[i, , ] <- as.matrix(
        cent_perm_x[, -1L] - cent_perm_y[, -1L]
      )
      cent_p_values <- cent_p_values +
        1L * (abs(cent_diffs_perm[i, , ]) >= cent_diffs_true_abs)
    }
    edge_diffs_perm[i, , ] <- weights_perm_x - weights_perm_y
    edge_p_values <- edge_p_values +
      1L * (abs(edge_diffs_perm[i, , ]) >= edge_diffs_true_abs)
  }
  edge_p_values <- edge_p_values / iter
  edge_diffs_sig <- edge_diffs_true * (edge_p_values < level)
  edge_stats <- data.frame(
    edge_name = edge_names,
    diff_true = c(edge_diffs_true),
    p_value = c(edge_p_values),
    stringsAsFactors = FALSE
  )
  out <- list(
    edges = list(
      stats = edge_stats,
      diffs_true = edge_diffs_true,
      diffs_sig = edge_diffs_sig
    )
  )
  if (include_centralities) {
    cent_p_values <- cent_p_values / iter
    cent_diffs_sig <- cent_diffs_true * (cent_p_values < level)
    cent_stats <- expand.grid(State = cent_x$State, Centrality = measures)
    cent_stats$diff_true <- as.vector(cent_diffs_true)
    cent_stats$p_value <- c(cent_p_values)
    cent_diffs_true <- cbind(
      data.frame(State = cent_x$State),
      as.data.frame(cent_diffs_true)
    )
    cent_diffs_sig <- cbind(
      data.frame(State = cent_x$State),
      as.data.frame(cent_diffs_sig)
    )
    out$centralities <- list(
      stats = cent_stats,
      diffs_true = cent_diffs_true,
      diffs_sig = cent_diffs_sig
    )
  }
  structure(
    out,
    labels = x$labels,
    colors = attr(x$data, "colors"),
    class = "tna_permutation"
  )
}
