#' Compare Two Networks from Sequence Data using a Permutation Test
#'
#' This function compares two networks built from sequence data using
#' permutation tests. The function builds Markov models for two sequence
#' objects, computes the transition probabilities, and compares them by
#' performing permutation tests. It returns the differences in transition
#' probabilities, effect sizes, estimated p-values, and confidence intervals.
#'
#' @export
#' @family validation
#' @param x A `tna` object containing sequence data for the first `tna` model.
#' @param y A `tna` object containing sequence data for the second `tna` model.
#' @param adjust A `character` string for the method to adjust p-values with
#' for multiple comparisons. The default is `"none"` for no adjustment.
#' See [stats::p.adjust()] for details and available adjustment methods.
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
#' `edges` and `centralities`, both containing the following elements:
#'
#'   * `stats`: A `data.frame` of original differences, effect sizes, and
#'     estimated p-values for each edge or centrality measure. The effect size
#'     is computed as the observed difference divided by the standard deviation
#'     of the differences of the permuted samples.
#'   * `diffs_true`: A `matrix` of differences in the data.
#'   * `diffs_sig`: A `matrix` showing the significant differences.
#'
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' # Small number of iterations for CRAN
#' permutation_test(model_x, model_y, iter = 20)
#'
permutation_test <- function(x, ...) {
  UseMethod("permutation_test")
}

#' @export
#' @rdname permutation_test
permutation_test.tna <- function(x, y, adjust = "none", iter = 1000,
                                 paired = FALSE, level = 0.05,
                                 measures = character(0), ...) {
  check_missing(x)
  check_missing(y)
  check_tna_seq(x)
  check_tna_seq(y)
  check_values(iter, strict = TRUE)
  check_flag(paired)
  check_range(level)
  permutation_test_(
    x = x,
    y = y,
    adjust = adjust,
    iter = iter,
    paired = paired,
    level = level,
    measures = measures,
    ...
  )
}

#' Compare Networks using a Permutation Test
#'
#' Test edge weight differences between all pairs or a subset of pairs of
#' a `group_tna` object. See [permutation_test.tna()] for more details.
#'
#' @export
#' @family validation
#' @param x A `group_tna` object
#' @param groups An `integer` vector or a `character` vector of group indices
#' or names, respectively, defining which groups to compare. When not provided,
#' all pairs are compared (the default).
#' @inheritParams permutation_test.tna
#' @examples
#' model <- group_model(engagement_mmm)
#' # Small number of iterations for CRAN
#' permutation_test(model, iter = 20)
#'
permutation_test.group_tna <- function(x, groups, adjust = "none",
                                       iter = 1000, paired = FALSE,
                                       level = 0.05, measures = character(0),
                                       ...) {
  check_missing(x)
  check_class(x, "group_tna")
  stopifnot_(
    length(attr(x, "scaling")) == 0L || attr(x, "groupwise"),
    "Permutation test is not supported for
     grouped models with globally scaled edge weights."
  )
  check_values(iter, strict = TRUE)
  check_flag(paired)
  check_range(level)
  x_names <- names(x)
  groups <- ifelse_(missing(groups), seq_along(x), groups)
  check_cluster(x, groups)
  groups <- ifelse_(
    is.character(groups),
    which(x_names %in% groups),
    groups
  )
  n_groups <- length(groups)
  stopifnot_(
    n_groups >= 2L,
    "Argument {.arg groups} must contain at least two groups to compare."
  )
  n_pairs <- (n_groups * (n_groups - 1L)) %/% 2L
  out <- vector(mode = "list", length = n_pairs)
  idx <- 0L
  for (i in seq_len(n_groups - 1L)) {
    for (j in seq(i + 1L, n_groups)) {
      idx <- idx + 1L
      group_i <- groups[i]
      group_j <- groups[j]
      out[[idx]] <- permutation_test_(
        x = x[[group_i]],
        y = x[[group_j]],
        adjust = adjust,
        iter = iter,
        paired = paired,
        level = level,
        measures = measures,
        ...
      )
      names(out)[idx] <- paste0(x_names[group_i], " vs. ", x_names[group_j])
    }
  }
  structure(
    out,
    class = "group_tna_permutation"
  )
}

permutation_test_ <- function(x, y, adjust, iter, paired, level,
                              measures, ...) {
  data_x <- x$data
  data_y <- y$data
  n_x <- nrow(data_x)
  n_y <- nrow(data_y)
  stopifnot_(
    !paired || n_x == n_y,
    "The number of observations must be the same in {.arg x} and {.arg y}
     for a paired test."
  )
  alph_x <- attr(data_x, "alphabet")
  alph_y <- attr(data_y, "alphabet")
  a <- length(alph_x)
  stopifnot_(
    a == length(alph_y),
    "The number of states of {.arg x} and {.arg y} must be the same."
  )
  stopifnot_(
    all(alph_x == alph_y),
    "The state labels of {.arg x} and {.arg y} must be the same
     and in the same order."
  )
  #combined_data <- dplyr::bind_rows(data_x, data_y)
  combined_data <- rbind(data_x, data_y)
  attr(combined_data, "alphabet") <- attr(data_x, "alphabet")
  attr(combined_data, "labels") <- attr(data_x, "labels")
  n_xy <- n_x + n_y
  weights_x <- x$weights
  weights_y <- y$weights
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")
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
  idx_x <- seq_len(n_x)
  idx_y <- seq(n_x + 1L, n_xy)
  combined_model <- initialize_model(
    combined_data,
    type,
    scaling,
    params,
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
      pair_idx <- matrix(seq_len(n_xy), ncol = 2)
      permuted_pairs <- t(apply(pair_idx, 1, sample))
      perm_idx <- c(permuted_pairs)
    } else {
      # For unpaired data, perform complete randomization
      perm_idx <- sample(n_xy)
    }
    trans_perm_x <- combined_trans[perm_idx[idx_x], , ]
    trans_perm_y <- combined_trans[perm_idx[idx_y], , ]
    weights_perm_x <- compute_weights(trans_perm_x, type, scaling, a)
    weights_perm_y <- compute_weights(trans_perm_y, type, scaling, a)
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
  edge_p_values <- (edge_p_values + 1) / (iter + 1)
  edge_p_values[,] <- stats::p.adjust(
    p = edge_p_values,
    method = adjust
  )
  edge_diffs_sd <- apply(edge_diffs_perm, c(2, 3), stats::sd)
  edge_diffs_sig <- edge_diffs_true * (edge_p_values < level)
  edge_stats <- data.frame(
    edge_name = edge_names,
    diff_true = c(edge_diffs_true),
    effect_size = c(edge_diffs_true) / c(edge_diffs_sd),
    p_value = c(edge_p_values)
  )
  out <- list(
    edges = list(
      stats = edge_stats,
      diffs_true = edge_diffs_true,
      diffs_sig = edge_diffs_sig
    )
  )
  if (include_centralities) {
    cent_p_values <- (cent_p_values + 1) / (iter + 1)
    cent_p_values[,] <- stats::p.adjust(
      p = cent_p_values,
      method = adjust
    )
    cent_diffs_sd <- apply(cent_diffs_perm, c(2, 3), stats::sd)
    cent_diffs_sig <- cent_diffs_true * (cent_p_values < level)
    cent_stats <- expand.grid(state = cent_x$state, centrality = measures)
    cent_stats$diff_true <- c(cent_diffs_true)
    cent_stats$effect_size <- c(cent_diffs_true) / c(cent_diffs_sd)
    cent_stats$p_value <- c(cent_p_values)
    cent_diffs_true <- cbind(
      data.frame(state = cent_x$state),
      as.data.frame(cent_diffs_true)
    )
    cent_diffs_sig <- cbind(
      data.frame(state = cent_x$state),
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
