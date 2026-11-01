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
#'   Grouped models with scaled edge weights, and their networks, are not
#'   resampled (an error of class `tna_error_scaling`).
#' @param y A `tna` object containing sequence data for the second `tna` model.
#' @param adjust A `character` string for the method to adjust p-values with
#' for multiple comparisons. The default is `"none"` for no adjustment.
#' See the `method` argument of [stats::p.adjust()] for details and available
#' adjustment methods.
#' @param iter An `integer` giving the number of permutations to perform.
#' The default is 1000.
#' @param paired A `logical` value. If `TRUE`, perform paired permutation tests;
#' if `FALSE`, perform unpaired tests. The default is `FALSE`.
#' @param level A `numeric` value giving the significance level for the
#' permutation tests. The default is 0.05.
#' @param measures A `character` vector of centrality measures to test.
#' See [centralities()] for a list of available centrality measures.
#' @param actor An optional actor identifier for sequences nested in actors
#' (for example, several sessions per student). Either a `character` string
#' naming a non-sequence column of the data the models were built from (such
#' as a column of `meta_data` from [prepare_data()]), or a vector with one
#' identifier per sequence (the sequences of `x` first, then those of `y`).
#' When given, the permutation respects the nesting: actors whose sequences
#' are all in one group are reassigned between the groups as whole units, and
#' the sequences of actors present in both groups are shuffled within the
#' actor (Good, 2005). A warning is given when the actors allow too few
#' distinct permutations to reach `level`. Cannot be combined with
#' `paired = TRUE`. The default `NULL` permutes single sequences.
#' @param ... Additional arguments passed to [centralities()].
#' @return A `tna_permutation` object which is a `list` with two elements:
#' `edges` and `centralities` (`NULL` when `measures` is empty). Both contain
#' the following elements:
#'
#'   * `stats`: A `data.frame` of original differences, effect sizes, and
#'     estimated p-values, with one row per edge (columns `edge_name`,
#'     `diff_true`, `effect_size`, `p_value`) or per state and centrality
#'     measure (columns `state`, `centrality`, `diff_true`, `effect_size`,
#'     `p_value`). The effect size is the observed difference divided by the
#'     standard deviation of the differences of the permuted samples, and
#'     `NA` when the permuted differences do not vary.
#'   * `diffs_true`: The differences in the data: a `matrix` for the edges,
#'     and a `data.frame` with one row per state and one column per measure
#'     for the centralities.
#'   * `diffs_sig`: The same as `diffs_true` with the non-significant
#'     differences set to zero.
#'
#' Use [summary.tna_permutation()] for the results as a tidy table.
#'
#' @references
#' Good, P. (2005). *Permutation, Parametric, and Bootstrap Tests of
#' Hypotheses* (3rd ed.). Springer.
#'
#' @examples
#' model_x <- tna(head(group_regulation, 200))
#' model_y <- tna(tail(group_regulation, 200))
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
                                 measures = character(0), actor = NULL,
                                 ...) {
  check_missing(x)
  check_missing(y)
  check_tna_seq(x)
  check_tna_seq(y)
  check_not_scaled_group(x, "The permutation test")
  check_not_scaled_group(y, "The permutation test")
  check_values(iter, strict = TRUE)
  check_flag(paired)
  check_actor(actor, paired)
  check_range(level, lower = 0, upper = 1)
  adjust <- check_match(adjust, stats::p.adjust.methods, match_case = TRUE)
  actor_x <- actor_y <- actor
  if (!is.null(actor) && !is_actor_column(actor)) {
    n_x <- nrow(x$data)
    n_xy <- n_x + nrow(y$data)
    stopifnot_(
      length(actor) == n_xy,
      "Argument {.arg actor} must have one value per sequence of
       {.arg x} and {.arg y} ({n_xy}), not {length(actor)}."
    )
    actor_x <- actor[seq_len(n_x)]
    actor_y <- actor[-seq_len(n_x)]
  }
  permutation_test_(
    x = x,
    y = y,
    adjust = adjust,
    iter = iter,
    paired = paired,
    level = level,
    measures = measures,
    ids = pair_actor_ids(x, y, actor_x, actor_y),
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
#' @param x A `group_tna` object.
#'   Grouped models with scaled edge weights, and their networks, are not
#'   resampled (an error of class `tna_error_scaling`).
#' @param groups An `integer` vector or a `character` vector of group indices
#' or names, respectively, defining which groups to compare. When not provided,
#' all pairs are compared (the default).
#' @param consecutive A `logical` value. If `FALSE` (the default), all pairwise
#' comparisons are performed in lexicographic order with respect to the order
#' of the groups. If `TRUE`, only comparisons between consecutive pairs
#' of groups are performed.
#' @inheritParams permutation_test.tna
#' @return A `group_tna_permutation` object, which is a `list` of
#'   `tna_permutation` objects (see [permutation_test()]), one per compared
#'   pair of groups, named after the pair.
#' @examples
#' model <- group_model(engagement_mmm)
#' # Small number of iterations for CRAN
#' permutation_test(model, iter = 20)
#'
permutation_test.group_tna <- function(x, groups, adjust = "none",
                                       iter = 1000, paired = FALSE,
                                       level = 0.05, measures = character(0),
                                       consecutive = FALSE, actor = NULL,
                                       ...) {
  check_missing(x)
  check_class(x, "group_tna")
  check_not_scaled_group(x, "The permutation test")
  check_values(iter, strict = TRUE)
  check_flag(paired)
  check_actor(actor, paired)
  check_range(level, lower = 0, upper = 1)
  adjust <- check_match(adjust, stats::p.adjust.methods, match_case = TRUE)
  x_names <- names(x)
  groups <- groups %m% seq_along(x)
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
  actors <- group_actor(x, actor)
  if (consecutive) {
    out <- vector(mode = "list", length = n_groups - 1L)
    for (i in seq_len(n_groups - 1L)) {
      group_i <- groups[i]
      group_j <- groups[i + 1L]
      out[[i]] <- permutation_test_(
        x = x[[group_i]],
        y = x[[group_j]],
        adjust = adjust,
        iter = iter,
        paired = paired,
        level = level,
        measures = measures,
        ids = pair_actor_ids(
          x[[group_i]], x[[group_j]], actors[[group_i]], actors[[group_j]]
        ),
        ...
      )
      names(out)[i] <- paste0(x_names[group_i], " vs. ", x_names[group_j])
    }
  } else {
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
          ids = pair_actor_ids(
            x[[group_i]], x[[group_j]], actors[[group_i]], actors[[group_j]]
          ),
          ...
        )
        names(out)[idx] <- paste0(x_names[group_i], " vs. ", x_names[group_j])
      }
    }
  }
  structure(
    out,
    class = "group_tna_permutation"
  )
}

permutation_test_ <- function(x, y, adjust, iter, paired, level,
                              measures, ids = NULL, ...) {
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
  combined_data <- rbind(data_x, data_y)
  attr(combined_data, "alphabet") <- attr(data_x, "alphabet")
  attr(combined_data, "labels") <- attr(data_x, "labels")
  n_xy <- n_x + n_y
  weights_x <- x$weights
  weights_y <- y$weights
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")
  if ("betweenness" %in% c(type, attr(y, "type"))) {
    stopifnot_(
      identical(type, attr(y, "type")) &&
        identical(params$betweenness, attr(y, "params")$betweenness),
      "Edge betweenness networks {.arg x} and {.arg y} must both be built
       with {.fn betweenness_network} from the same type of model and with
       the same {.arg directed} and {.arg invert} values.",
      class = "tna_error_betweenness"
    )
  }
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
  combined_model <- initialize_model(
    combined_data,
    type,
    scaling,
    params,
    transitions = TRUE
  )
  combined_trans <- combined_model$trans
  design <- onlyif(
    !is.null(ids),
    actor_design(
      ids = ids,
      labels = rep(c(TRUE, FALSE), c(n_x, n_y)),
      level = level
    )
  )
  n_perm_x <- n_x
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
    } else if (is.null(design)) {
      # For unpaired data, perform complete randomization
      perm_idx <- sample(n_xy)
    } else {
      # For nested data, permute within the restrictions set by actors
      perm_x <- actor_permute(design)
      perm_idx <- c(which(perm_x), which(!perm_x))
      n_perm_x <- sum(perm_x)
    }
    idx_x <- seq_len(n_perm_x)
    trans_perm_x <- combined_trans[perm_idx[idx_x], , , drop = FALSE]
    trans_perm_y <- combined_trans[perm_idx[-idx_x], , , drop = FALSE]
    weights_perm_x <- compute_weights(
      trans_perm_x, type, scaling, a, params
    )
    weights_perm_y <- compute_weights(
      trans_perm_y, type, scaling, a, params
    )
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
    effect_size = effect_size(
      c(edge_diffs_true),
      c(edge_diffs_sd),
      c(pmax(abs(weights_x), abs(weights_y)))
    ),
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
    cent_stats$effect_size <- effect_size(
      c(cent_diffs_true),
      c(cent_diffs_sd),
      c(pmax(abs(as.matrix(cent_x[, -1L])), abs(as.matrix(cent_y[, -1L]))))
    )
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

permutation_test_patterns <- function(x, len, iter, adjust, actor = NULL) {
  check_not_scaled_group(x, "The permutation test")
  check_values(iter, strict = TRUE)
  data <- combine_data(x)
  group <- data$.group
  data$.group <- NULL
  m <- as.matrix(data)
  n <- nrow(m)
  k <- length(len)
  labels <- x[[1L]]$labels
  pattern_matrices <- extract_patterns(m, len, labels)
  patterns <- factorize_patterns(pattern_matrices, group)
  stat_true <- lapply(patterns, pattern_statistic)
  # Clusters are combined in order, so their actor ids line up with `group`
  design <- onlyif(
    !is.null(actor),
    actor_design(
      ids = unlist(Map(actor_ids, x, group_actor(x, actor))),
      labels = group,
      level = 0.05
    )
  )
  stat_perm <- vector(mode = "list", length = k)
  stat_p_value <- vector(mode = "list", length = k)
  for (j in seq_len(k)) {
    u <- length(pattern_matrices[[j]]$unique)
    stat_perm[[j]] <- matrix(0, u, iter)
    stat_p_value[[j]] <- numeric(u)
  }
  for (i in seq_len(iter)) {
    perm_group <- ifelse_(
      is.null(design),
      group[sample(n)],
      actor_permute(design)
    )
    perm_patterns <- factorize_patterns(pattern_matrices, perm_group)
    for (j in seq_len(k)) {
      stat_perm[[j]][, i] <- pattern_statistic(perm_patterns[[j]])
      stat_p_value[[j]] <- stat_p_value[[j]] + 1L *
        (stat_perm[[j]][, i] >= stat_true[[j]])
    }
  }
  out <- vector(mode = "list", length = k)
  for (j in seq_len(k)) {
    stat_perm_mean <- apply(stat_perm[[j]], 1, mean)
    stat_perm_sd <- apply(stat_perm[[j]], 1, stats::sd)
    stat_scale <- pmax(abs(stat_true[[j]]), apply(abs(stat_perm[[j]]), 1, max))
    out[[j]] <- data.frame(
      effect_size = effect_size(
        stat_true[[j]] - stat_perm_mean,
        stat_perm_sd,
        stat_scale
      ),
      p_value = stats::p.adjust(
        (stat_p_value[[j]] + 1) / (iter + 1),
        method = adjust
      )
    )
  }
  do.call("rbind", out)
}

pattern_statistic <- function(x) {
  n <- sum(x)
  nr <- nrow(x)
  nc <- ncol(x)
  rs <- .rowSums(x, m = nr, n = nc)
  cs <- .colSums(x, m = nr, n = nc)
  expected <- outer(rs, cs) / n
  sqrt(.rowSums((x - expected)^2, m = nr, n = nc))
}

#' Standardized Effect Size of a Permutation Test
#'
#' @param diff A `numeric` vector of observed differences.
#' @param sd A `numeric` vector of the standard deviations of the permuted
#'   differences.
#' @param scale A `numeric` vector of the magnitudes of the values whose
#'   differences are taken (e.g. the larger absolute weight of the two
#'   networks).
#' @return `diff / sd`, or `NA` where the permuted differences do not vary
#'   (the effect size is then undefined).
#' @noRd
effect_size <- function(diff, sd, scale) {
  # A standard deviation at the rounding level of the compared values is
  # zero; judging it against their own magnitude keeps the rule free of
  # the measurement scale
  defined <- !is.na(sd) & !is.na(scale) &
    sd > sqrt(.Machine$double.eps) * abs(scale)
  out <- rep(NA_real_, length(diff))
  out[defined] <- diff[defined] / sd[defined]
  out
}
