#' Test the Markov Order of Sequence Data
#'
#' Tests whether the sequences of a model are described by a Markov chain of
#' order one, the assumption of a transition network, or whether the next
#' state also depends on states further back.
#'
#' For each order \eqn{k = 1, \ldots,} `max_order`, the test compares order
#' \eqn{k - 1} (the null hypothesis) with order \eqn{k}. Every window of
#' \eqn{k + 1} consecutive states is split into the context \eqn{w} (the
#' \eqn{k - 1} states before the next state), the state \eqn{x} that order
#' \eqn{k} adds (\eqn{k} steps back), and the next state \eqn{s}. Under the
#' null hypothesis, \eqn{s} is independent of \eqn{x} given \eqn{w}. This is
#' tested with the likelihood-ratio statistic
#' \deqn{G^2 = 2 \sum_{w, x, s} N_{wxs}
#'   \log \frac{N_{wxs} N_{w}}{N_{wx} N_{ws}}}
#' with \eqn{\sum_w (r_w - 1)(c_w - 1)} degrees of freedom, where
#' \eqn{r_w} and \eqn{c_w} are the numbers of distinct states \eqn{x} and
#' \eqn{s} observed with context \eqn{w} (Anderson & Goodman, 1957). For
#' \eqn{k = 1} the context is empty, and the test is whether the next state
#' depends on the current state at all.
#'
#' The p-value comes from a conditional permutation test: within each context
#' \eqn{w}, the next states are shuffled, which keeps every count that the
#' null hypothesis conditions on. The windows of a sequence overlap, so the
#' permutation distribution approximates the null distribution but is not
#' exact (an exact test would sample whole sequences with the same counts;
#' Besag & Mondal, 2013). The asymptotic \eqn{\chi^2} p-value is reported
#' alongside.
#'
#' The orders are tested in sequence: the order is raised while the test
#' rejects at level `level`. The selected order is the highest order whose
#' test, and the test of every lower order, rejected. Order 0 means that the
#' next state does not depend on the current state, and `max_order` means
#' that no test failed to reject.
#'
#' The results also contain the log-likelihood, AIC, and BIC of the
#' multi-order model of each order (Scholtes, 2017). This model uses the
#' state frequencies for the first state of a sequence and the highest
#' available order for each following state. With `dof = "paths"`, the
#' parameters of an order are all the transitions of the paths that the
#' first-order transitions allow, observed or not (Scholtes, 2017). With
#' `dof = "observed"`, only the transitions observed in the data are counted.
#' The orders that minimize AIC and BIC are reported next to the selected
#' order, but they do not affect the test.
#'
#' Because the tests stop at the first order that does not reject, a
#' dependence that skips an order (for example, on the state two steps back
#' but not on the current state) is not selected. A warning of class
#' `tna_warning_markov_order` is therefore raised, and a note is printed,
#' when the test of an order above the selected order plus one rejects, or
#' when AIC or BIC select a different order than the tests. The same class
#' of warning is raised when `iter` is too small for any p-value to fall
#' below `level` (fewer than `floor(1 / level)` permutations).
#'
#' Sequences are split at missing values, so that no window spans a missing
#' value, and the parts with fewer than two states are not used. The windows
#' are treated as independent observations, so sequences nested in actors
#' (for example, several sessions per student) are not taken into account.
#'
#' @export
#' @family validation
#' @param x A `tna` or a `group_tna` object created from sequence data.
#' @param max_order An `integer`, the highest order to test. The default is
#'   3. A value that is not lower than the length of the longest sequence is
#'   reduced to that length minus one, with a message.
#' @param iter An `integer`, the number of permutations for each order. The
#'   default is 1000.
#' @param level A `numeric` value, the significance level of the tests. The
#'   default is 0.05.
#' @param dof A `character` string, how the parameters of each order are
#'   counted for AIC and BIC: `"paths"` (the default) or `"observed"`. See
#'   'Details'.
#' @return A `tna_markov_order` object, which is a `list` with the elements:
#'
#'   * `stats`: A `data.frame` with one row per order from 0 to `max_order`
#'     and the columns `order`, `loglik`, `aic`, `bic` (the multi-order
#'     model), `g2` (the \eqn{G^2} statistic), `df` (its degrees of freedom),
#'     `p_value` (the permutation p-value), `p_asymptotic` (the \eqn{\chi^2}
#'     p-value), and `significant` (`p_value < level`). The test columns are
#'     `NA` for order 0 and for an order whose statistic has no degrees of
#'     freedom.
#'   * `selected_order`: The order selected by the sequential tests.
#'   * `aic_order`, `bic_order`: The orders that minimize AIC and BIC.
#'   * `null`: A `list` with the permutation distribution of \eqn{G^2} for
#'     each order.
#'   * `states`: The states observed in the data.
#'   * `n_sequences`, `n_observations`: The numbers of sequences (after
#'     splitting at missing values) and states used.
#'   * `max_order`, `iter`, `level`, `dof`: The settings used.
#'
#'   For a `group_tna` object, a `group_tna_markov_order` object, which is a
#'   `list` of `tna_markov_order` objects, one per group.
#' @references
#' Anderson, T. W., & Goodman, L. A. (1957). Statistical inference about
#' Markov chains. *The Annals of Mathematical Statistics*, 28(1), 89--110.
#' \doi{10.1214/aoms/1177707039}
#'
#' Besag, J., & Mondal, D. (2013). Exact goodness-of-fit tests for Markov
#' chains. *Biometrics*, 69(2), 488--496. \doi{10.1111/biom.12009}
#'
#' Scholtes, I. (2017). When is a network a network? Multi-order graphical
#' model selection in pathways and temporal networks. In *Proceedings of the
#' 23rd ACM SIGKDD International Conference on Knowledge Discovery and Data
#' Mining* (pp. 1037--1046). \doi{10.1145/3097983.3098145}
#' @examples
#' model <- tna(group_regulation)
#' # Small number of permutations for CRAN
#' mo <- markov_order(model, max_order = 2, iter = 50)
#' mo
#' summary(mo)
#' plot(mo)
#' plot(mo, panel = "ic")
#'
markov_order <- function(x, max_order = 3L, iter = 1000L,
                              level = 0.05, dof = "paths") {
  UseMethod("markov_order")
}

#' @export
#' @rdname markov_order
markov_order.tna <- function(x, max_order = 3L, iter = 1000L,
                                  level = 0.05, dof = "paths") {
  out <- markov_order_(x, max_order, iter, level, dof)
  if (warn_markov_iter(iter, level)) {
    warn_markov_disagreement(out)
  }
  out
}

#' Test the Markov Order of the Sequences of One Model
#'
#' @inheritParams markov_order
#' @return A `tna_markov_order` object, see [markov_order()].
#' @noRd
markov_order_ <- function(x, max_order, iter, level, dof) {
  check_missing(x)
  check_tna_seq(x)
  check_values(max_order, strict = TRUE)
  check_values(iter, strict = TRUE)
  check_range(level, lower = 0, upper = 1)
  dof <- check_match(dof, c("paths", "observed"))
  chains <- markov_chains(x$data)
  a <- length(attr(x$data, "alphabet"))
  longest <- max(chains$lengths)
  if (max_order >= longest) {
    max_order <- longest - 1L
    message_(
      "{.arg max_order} was reduced to {max_order}, one less than the
       length of the longest sequence ({longest})."
    )
  }
  # The keys of order max_order encode max_order + 1 states
  stopifnot_(
    a^(max_order + 1L) < 2^53,
    "Argument {.arg max_order} is too high for {a} states.",
    class = "tna_error_markov_order"
  )
  layers <- markov_layers(chains, a, max_order, dof)
  tests <- lapply(
    seq_len(max_order),
    function(k) markov_g2_test(chains, a, k, iter)
  )
  g2 <- vapply(tests, "[[", numeric(1L), "g2")
  df <- vapply(tests, "[[", numeric(1L), "df")
  p_value <- vapply(tests, "[[", numeric(1L), "p_value")
  p_asymptotic <- vapply(tests, "[[", numeric(1L), "p_asymptotic")
  significant <- !is.na(p_value) & p_value < level
  # The order is raised while the test rejects
  selected_order <- ifelse_(
    all(significant),
    max_order,
    which(!significant)[1L] - 1L
  )
  stats <- data.frame(
    order = seq(0L, max_order),
    loglik = layers$loglik,
    aic = layers$aic,
    bic = layers$bic,
    g2 = c(NA_real_, g2),
    df = c(NA_real_, df),
    p_value = c(NA_real_, p_value),
    p_asymptotic = c(NA_real_, p_asymptotic),
    significant = c(NA, ifelse(is.na(p_value), NA, significant))
  )
  null <- lapply(tests, "[[", "null")
  names(null) <- paste0("order_", seq_len(max_order))
  structure(
    list(
      stats = stats,
      selected_order = as.integer(selected_order),
      aic_order = stats$order[which.min(stats$aic)],
      bic_order = stats$order[which.min(stats$bic)],
      null = null,
      states = attr(x$data, "labels")[layers$observed],
      n_sequences = length(chains$lengths),
      n_observations = length(chains$values),
      max_order = max_order,
      iter = iter,
      level = level,
      dof = dof
    ),
    class = "tna_markov_order"
  )
}

#' @export
#' @rdname markov_order
markov_order.group_tna <- function(x, max_order = 3L, iter = 1000L,
                                        level = 0.05, dof = "paths") {
  check_missing(x)
  check_class(x, "group_tna")
  out <- structure(
    lapply(
      x,
      markov_order_,
      max_order = max_order,
      iter = iter,
      level = level,
      dof = dof
    ),
    class = "group_tna_markov_order"
  )
  if (warn_markov_iter(iter, level)) {
    invisible(Map(warn_markov_disagreement, out, names(out)))
  }
  out
}

#' Warn When Too Few Permutations Are Used to Reach the Level
#'
#' @inheritParams markov_order
#' @return `TRUE` when the smallest permutation p-value is below `level`,
#'   `FALSE` (with a warning) otherwise.
#' @noRd
warn_markov_iter <- function(iter, level) {
  smallest <- 1 / (iter + 1)
  enough <- smallest < level
  if (!enough) {
    warning_(
      c(
        "With {.arg iter} = {iter}, the smallest permutation p-value is
         {format(smallest, digits = 2)}, which is not below
         {.arg level} = {level}, so no test can reject.",
        `i` = onlyif(
          level > 0,
          "Use {.arg iter} = {floor(1 / level)} or more."
        )
      ),
      class = "tna_warning_markov_order"
    )
  }
  enough
}

#' Find Where the Order Selections of a Markov Order Test Disagree
#'
#' @param x A `tna_markov_order` object.
#' @return A `character` vector, empty when the sequential tests, AIC, and
#'   BIC agree and no test above the selected order plus one rejects.
#' @noRd
markov_disagreement <- function(x) {
  selected <- x$selected_order
  rejected <- x$stats$order[which(x$stats$significant)]
  skipped <- rejected[rejected > selected + 1L]
  c(
    onlyif(
      length(skipped) > 0L,
      paste0(
        ifelse_(length(skipped) == 1L, "the test of order ", "the tests of orders "),
        paste(skipped, collapse = ", "),
        ifelse_(length(skipped) == 1L, " rejects", " reject")
      )
    ),
    onlyif(x$aic_order != selected, paste0("AIC selects order ", x$aic_order)),
    onlyif(x$bic_order != selected, paste0("BIC selects order ", x$bic_order))
  )
}

#' Describe a Disagreement in the Order Selections of a Markov Order Test
#'
#' @param x A `tna_markov_order` object.
#' @return A `character` string, or `NULL` when the selections agree.
#' @noRd
markov_note <- function(x) {
  reasons <- markov_disagreement(x)
  onlyif(
    length(reasons) > 0L,
    paste0(
      "The sequential tests select order ", x$selected_order, ", but ",
      paste(reasons, collapse = "; "), "."
    )
  )
}

#' Warn About a Disagreement in the Order Selections of a Markov Order Test
#'
#' @param x A `tna_markov_order` object.
#' @param group An optional `character` string, the name of the group.
#' @return `NULL` (invisibly).
#' @noRd
warn_markov_disagreement <- function(x, group = NULL) {
  note <- markov_note(x)
  if (!is.null(note)) {
    prefix <- ifelse_(is.null(group), "", paste0(group, ": "))
    warning_(
      c(
        paste0(prefix, note),
        `i` = "The tests stop at the first order that does not reject, so
               they miss a dependence that skips an order. Inspect the
               table before choosing an order."
      ),
      class = "tna_warning_markov_order"
    )
  }
  invisible(NULL)
}

#' Split Sequence Data into Chains of Observed States
#'
#' @param d A `tna_seq_data` object.
#' @return A `list` with the elements `values` (the state codes of all chains
#'   one after the other), `position` (the position of each state within its
#'   chain), and `lengths` (the chain lengths). Chains are the runs of
#'   observed states of each sequence; chains of length one are dropped.
#' @noRd
markov_chains <- function(d) {
  n <- nrow(d)
  p <- ncol(d)
  values <- as.vector(t(unclass(d)))
  first <- rep(c(TRUE, logical(p - 1L)), n)
  observed <- !is.na(values)
  previous <- c(FALSE, observed[-length(observed)])
  start <- observed & (first | !previous)
  chain <- cumsum(start)[observed]
  values <- values[observed]
  lengths <- tabulate(chain, nbins = sum(start))
  keep <- lengths[chain] >= 2L
  stopifnot_(
    any(keep),
    "No sequence has two consecutive observed states.",
    class = "tna_error_markov_order"
  )
  lengths <- lengths[lengths >= 2L]
  list(
    values = values[keep],
    position = sequence(lengths),
    lengths = lengths
  )
}

#' Encode the States at Given Lags as One Key
#'
#' @param values An `integer` vector of state codes from `markov_chains()`.
#' @param idx An `integer` vector of positions in `values`.
#' @param lags An `integer` vector of lags, each lower than the position of
#'   `idx` within its chain.
#' @param a An `integer`, the number of states.
#' @return A `numeric` vector, one key per element of `idx`, unique for each
#'   combination of states at the lags.
#' @noRd
lag_key <- function(values, idx, lags, a) {
  Reduce(
    function(key, lag) key * a + values[idx - lag] - 1,
    lags,
    numeric(length(idx))
  )
}

#' Count the Occurrences of Each Distinct Key
#'
#' @param key A vector of keys.
#' @return An `integer` vector of the counts of the distinct keys.
#' @noRd
key_counts <- function(key) {
  tabulate(match(key, unique(key)))
}

#' Sum of n log n
#'
#' @param n A `numeric` vector of positive counts.
#' @noRd
xlogx <- function(n) {
  sum(n * log(n))
}

#' Fit the Multi-order Models of a Markov Order Test
#'
#' @param chains The output of `markov_chains()`.
#' @param a An `integer`, the number of states.
#' @inheritParams markov_order
#' @return A `list` with the log-likelihood, AIC, and BIC of the orders from
#'   0 to `max_order`, and the observed state codes.
#' @noRd
markov_layers <- function(chains, a, max_order, dof) {
  values <- chains$values
  position <- chains$position
  state_counts <- tabulate(values, nbins = a)
  observed <- which(state_counts > 0L)
  log_marginal <- log(state_counts / length(values))
  # Order j: the probability of each state given the j states before it
  orders <- lapply(
    seq_len(max_order),
    function(j) {
      idx <- which(position > j)
      context <- lag_key(values, idx, seq_len(j), a)
      pair <- context * a + values[idx] - 1
      context_id <- match(context, unique(context))
      pair_id <- match(pair, unique(pair))
      log_prob <- log(
        tabulate(pair_id)[pair_id] / tabulate(context_id)[context_id]
      )
      list(
        # States whose position makes order j the highest available order
        first = sum(log_prob[position[idx] == j + 1L]),
        all = sum(log_prob),
        dof = max(pair_id) - max(context_id)
      )
    }
  )
  first <- vapply(orders, "[[", numeric(1L), "first")
  all <- vapply(orders, "[[", numeric(1L), "all")
  log_initial <- sum(log_marginal[values[position == 1L]])
  loglik <- c(
    sum(log_marginal[values]),
    log_initial + cumsum(c(0, first))[seq_len(max_order)] + all
  )
  layer_dof <- ifelse_(
    dof == "paths",
    markov_path_dof(chains, a, observed, max_order),
    vapply(orders, "[[", numeric(1L), "dof")
  )
  cum_dof <- cumsum(c(length(observed) - 1, layer_dof))
  list(
    loglik = loglik,
    aic = 2 * cum_dof - 2 * loglik,
    bic = log(length(values)) * cum_dof - 2 * loglik,
    observed = observed
  )
}

#' Count the Parameters of Each Order from the Paths Allowed
#'
#' The parameters of order `k` are the transitions of the paths of length
#' `k` that the observed first-order transitions allow, less one for each
#' path with a successor (Scholtes, 2017, Eq. 7).
#'
#' @inheritParams markov_layers
#' @param observed An `integer` vector of the observed state codes.
#' @return A `numeric` vector of length `max_order`.
#' @noRd
markov_path_dof <- function(chains, a, observed, max_order) {
  values <- chains$values
  idx <- which(chains$position > 1L)
  n_observed <- length(observed)
  adjacency <- matrix(0, n_observed, n_observed)
  code <- match(seq_len(a), observed)
  adjacency[cbind(code[values[idx - 1L]], code[values[idx]])] <- 1
  extra <- pmax(.rowSums(adjacency, n_observed, n_observed) - 1, 0)
  dof <- vapply(
    seq_len(max_order),
    function(k) {
      walks <- Reduce(
        `%*%`,
        rep(list(adjacency), k - 1L),
        diag(1, n_observed, n_observed)
      )
      sum(.colSums(walks, n_observed, n_observed) * extra)
    },
    numeric(1L)
  )
  stopifnot_(
    all(is.finite(dof)),
    c(
      "The number of paths exceeds double precision.",
      `i` = "Use a lower {.arg max_order} or {.code dof = \"observed\"}."
    ),
    class = "tna_error_markov_order"
  )
  dof
}

#' Test One Markov Order Against the Order Below
#'
#' @inheritParams markov_layers
#' @param k An `integer`, the order to test.
#' @inheritParams markov_order
#' @return A `list` with the statistic `g2`, its degrees of freedom `df`, the
#'   permutation and asymptotic p-values, and the permutation distribution
#'   `null`.
#' @noRd
markov_g2_test <- function(chains, a, k, iter) {
  values <- chains$values
  idx <- which(chains$position > k)
  x <- values[idx - k]
  s <- values[idx]
  context <- lag_key(values, idx, seq_len(k - 1L), a)
  w <- match(context, unique(context))
  n_w <- tabulate(w)
  wx <- (w - 1) * a + x
  ws <- (w - 1) * a + s
  r_w <- tabulate(w[!duplicated(wx)], nbins = length(n_w))
  c_w <- tabulate(w[!duplicated(ws)], nbins = length(n_w))
  df <- sum((r_w - 1) * (c_w - 1))
  # N_wx, N_ws and N_w do not change when s is shuffled within w
  fixed <- xlogx(n_w) - xlogx(key_counts(wx)) - xlogx(key_counts(ws))
  g2_of <- function(s) {
    2 * (xlogx(key_counts(((w - 1) * a + x - 1) * a + s)) + fixed)
  }
  g2 <- g2_of(s)
  if (df == 0) {
    # Every context has one x or one s, so the statistic is exactly zero
    return(
      list(
        g2 = 0,
        df = df,
        p_value = NA_real_,
        p_asymptotic = NA_real_,
        null = numeric(0L)
      )
    )
  }
  n <- length(s)
  grouped <- order(w)
  null <- vapply(
    seq_len(iter),
    function(i) {
      # Shuffle the next states within each context
      shuffled <- s
      shuffled[grouped] <- s[order(w, stats::runif(n))]
      g2_of(shuffled)
    },
    numeric(1L)
  )
  # Identical tables can differ in the last bits of their statistic
  tol <- sqrt(.Machine$double.eps) * max(1, g2)
  list(
    g2 = g2,
    df = df,
    p_value = (sum(null >= g2 - tol) + 1) / (iter + 1),
    p_asymptotic = stats::pchisq(g2, df, lower.tail = FALSE),
    null = null
  )
}

#' Summarize a Markov Order Test
#'
#' @export
#' @family validation
#' @param object A `tna_markov_order` or a `group_tna_markov_order` object.
#' @param digits An `integer`, the number of decimals of the numeric columns
#'   other than the p-values. The default is 2.
#' @param ... Ignored.
#' @return A `summary.tna_markov_order` object, which is a `data.frame` with
#'   one row per order (and per group, in the first column `group`, for a
#'   `group_tna_markov_order` object) and the columns of the `stats` element
#'   described in [markov_order()]. The numeric columns other than the
#'   p-values are rounded to `digits` decimals. When the order selected by
#'   the tests and the orders selected by AIC and BIC disagree, a note is
#'   printed below the table.
#' @examples
#' model <- tna(group_regulation)
#' summary(markov_order(model, max_order = 2, iter = 50))
#'
summary.tna_markov_order <- function(object, digits = 2L, ...) {
  check_missing(object)
  check_class(object, "tna_markov_order")
  check_values(digits)
  markov_table(object$stats, digits, notes = markov_note(object))
}

#' @export
#' @rdname summary.tna_markov_order
summary.group_tna_markov_order <- function(object, digits = 2L, ...) {
  check_missing(object)
  check_class(object, "group_tna_markov_order")
  check_values(digits)
  stats <- Map(
    function(y, group) data.frame(group = group, y$stats),
    object,
    names(object)
  )
  notes <- Map(
    function(y, group) {
      note <- markov_note(y)
      onlyif(!is.null(note), paste0(group, ": ", note))
    },
    object,
    names(object)
  )
  markov_table(
    do.call(rbind, unname(stats)),
    digits,
    notes = unlist(notes, use.names = FALSE)
  )
}

#' Round a Markov Order Table for Display
#'
#' @param stats A `data.frame` of Markov order test results.
#' @param digits An `integer`, the number of decimals.
#' @param notes A `character` vector of notes printed below the table, or
#'   `NULL`.
#' @noRd
markov_table <- function(stats, digits, notes = NULL) {
  rounded <- c("loglik", "aic", "bic", "g2")
  stats[rounded] <- lapply(stats[rounded], round, digits = digits)
  rownames(stats) <- NULL
  structure(
    stats,
    digits = digits,
    notes = notes,
    class = c("summary.tna_markov_order", "data.frame")
  )
}

#' Print a Summary of a Markov Order Test
#'
#' @export
#' @family validation
#' @param x A `summary.tna_markov_order` object.
#' @param digits An `integer`, the number of decimals of the numeric columns
#'   other than the p-values, which are shown with three decimals.
#' @param ... Ignored.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(group_regulation)
#' summary(markov_order(model, max_order = 2, iter = 50))
#'
print.summary.tna_markov_order <- function(x, digits = attr(x, "digits"),
                                           ...) {
  check_missing(x)
  check_class(x, "summary.tna_markov_order")
  digits <- digits %||% 2L
  check_values(digits)
  shown <- as.data.frame(unclass(x))
  attr(shown, "digits") <- NULL
  attr(shown, "notes") <- NULL
  numbers <- c("loglik", "aic", "bic", "g2", "df")
  shown[numbers] <- lapply(shown[numbers], format_number, digits = digits)
  p_values <- c("p_value", "p_asymptotic")
  shown[p_values] <- lapply(
    shown[p_values],
    function(p) {
      ifelse(is.na(p), "", formatC(p, digits = 3L, format = "f"))
    }
  )
  shown$significant <- ifelse(
    is.na(shown$significant),
    "",
    ifelse(shown$significant, "yes", "no")
  )
  print.data.frame(shown, row.names = FALSE, right = TRUE)
  notes <- attr(x, "notes")
  if (length(notes) > 0L) {
    cat(paste0("\nNote: ", notes), sep = "")
    cat("\n")
  }
  invisible(x)
}

#' Format Numbers for Printing
#'
#' A vector of whole numbers is shown without decimals, any other vector
#' with `digits` decimals except for exact zeros, which are shown as `0`.
#' Numbers are never shown in scientific notation.
#'
#' @param v A `numeric` vector.
#' @param digits An `integer`, the number of decimals.
#' @noRd
format_number <- function(v, digits) {
  shown <- !is.na(v)
  whole <- abs(v - round(v)) < sqrt(.Machine$double.eps)
  out <- ifelse_(
    all(whole[shown]),
    formatC(round(v), digits = 0L, format = "f"),
    formatC(v, digits = digits, format = "f")
  )
  out[shown & abs(v) < sqrt(.Machine$double.eps)] <- "0"
  out[!shown] <- ""
  out
}

#' Print the Results of a Markov Order Test
#'
#' @export
#' @family validation
#' @param x A `tna_markov_order` or a `group_tna_markov_order` object.
#' @param digits An `integer`, the number of decimals of the numeric columns
#'   other than the p-values. The default is 2.
#' @param ... Ignored.
#' @return `x` (invisibly).
#' @examples
#' model <- tna(group_regulation)
#' markov_order(model, max_order = 2, iter = 50)
#'
print.tna_markov_order <- function(x, digits = 2L, ...) {
  check_missing(x)
  check_class(x, "tna_markov_order")
  cat(
    "Markov order test (", x$iter, " permutations per order, ",
    x$n_sequences, " sequences, ", x$n_observations, " states)\n\n",
    sep = ""
  )
  # The note goes below the selected orders, not below the table
  table <- summary(x, digits = digits)
  attr(table, "notes") <- NULL
  print(table)
  cat(
    "\nSelected order: ", x$selected_order,
    " (sequential permutation tests at level ", x$level, ")\n",
    "Order with the lowest AIC: ", x$aic_order,
    "; lowest BIC: ", x$bic_order, "\n",
    sep = ""
  )
  note <- markov_note(x)
  if (!is.null(note)) {
    cat("Note: ", note, "\n", sep = "")
  }
  invisible(x)
}

#' @export
#' @rdname print.tna_markov_order
print.group_tna_markov_order <- function(x, digits = 2L, ...) {
  check_missing(x)
  check_class(x, "group_tna_markov_order")
  groups <- names(x)
  invisible(
    lapply(
      seq_along(x),
      function(i) {
        cat(ifelse_(i > 1L, "\n", ""), groups[i], ":\n", sep = "")
        print(x[[i]], digits = digits)
      }
    )
  )
  invisible(x)
}

#' Plot the Results of a Markov Order Test
#'
#' A two-panel visualization (the plot of `Nestimate::markov_order_test()`):
#'
#' * Panel A: the log-likelihood, AIC, and BIC across the tested orders, with
#'   the order selected by the permutation tests (dashed line) and the order
#'   that minimizes BIC (dotted line) marked.
#' * Panel B: the permutation null density of each order with the observed
#'   \eqn{G^2} as a vertical line, colored by rejection at `level` and
#'   labelled with its p-value.
#'
#' Uses the Okabe-Ito colorblind-safe palette.
#'
#' @export
#' @family validation
#' @param x A `tna_markov_order` or a `group_tna_markov_order` object.
#' @param panel A `character` string, which panels to draw: `"both"` (the
#'   default), `"ic"`, or `"permutation"`.
#' @param combined A `logical` value. When `panel = "both"` and `combined` is
#'   `TRUE` (the default), the two panels are drawn side by side. When
#'   `FALSE`, the two `ggplot` objects are returned without drawing them.
#'   Ignored when `panel` is not `"both"`.
#' @param title An optional `character` string placed before the panel titles.
#'   For a `group_tna_markov_order` object, one per group (the group names by
#'   default).
#' @param ... Must be empty: an unknown argument, such as a misspelled
#'   `panel`, is an error.
#' @return A `ggplot` object for `panel = "ic"` or `panel = "permutation"`.
#'   For `panel = "both"`, a named `list` of the two `ggplot` objects (`ic`,
#'   `permutation`), invisibly, after drawing them side by side when
#'   `combined = TRUE`. For a `group_tna_markov_order` object, the plots of
#'   the groups are drawn one after another, and a `list` with the result for
#'   each group is returned invisibly.
#' @examples
#' model <- tna(group_regulation)
#' mo <- markov_order(model, max_order = 2, iter = 50)
#' plot(mo)
#' plot(mo, panel = "ic")
#'
plot.tna_markov_order <- function(x, panel = "both", combined = TRUE,
                                  title = NULL, ...) {
  check_missing(x)
  check_class(x, "tna_markov_order")
  rlang::check_dots_empty()
  panel <- check_match(panel, c("both", "ic", "permutation"))
  check_flag(combined)
  prefix <- ifelse_(is.null(title), "", paste0(title, ": "))
  pal <- c(
    reject = "#D55E00", accept = "#0072B2",
    ic_ll = "#009E73", ic_aic = "#E69F00", ic_bic = "#56B4E9",
    selected = "#D55E00"
  )
  tt <- x$stats
  ic_df <- rbind(
    data.frame(order = tt$order, metric = "log-likelihood", value = tt$loglik),
    data.frame(order = tt$order, metric = "AIC", value = tt$aic),
    data.frame(order = tt$order, metric = "BIC", value = tt$bic)
  )
  ic_df$metric <- factor(
    ic_df$metric,
    levels = c("log-likelihood", "AIC", "BIC")
  )
  p_ic <- ggplot2::ggplot(
    ic_df,
    ggplot2::aes(
      x = !!rlang::sym("order"),
      y = !!rlang::sym("value"),
      color = !!rlang::sym("metric")
    )
  ) +
    ggplot2::geom_vline(
      xintercept = x$selected_order,
      linetype = "dashed",
      color = pal["selected"],
      linewidth = 0.6
    ) +
    ggplot2::geom_vline(
      xintercept = x$bic_order,
      linetype = "dotted",
      color = pal["ic_bic"],
      linewidth = 0.6
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_color_manual(
      values = unname(pal[c("ic_ll", "ic_aic", "ic_bic")]),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = tt$order) +
    ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym("metric")),
      scales = "free_y",
      ncol = 1
    ) +
    ggplot2::labs(
      x = "Markov order",
      y = NULL,
      title = paste0(prefix, "Model fit across orders"),
      subtitle = sprintf(
        "Permutation LRT selects order %d (alpha = %.2f); BIC minimized at %d",
        x$selected_order, x$level, x$bic_order
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  if (panel == "ic") {
    return(p_ic)
  }
  # --- Permutation null ---
  null_df <- do.call(
    rbind,
    lapply(
      seq_along(x$null),
      function(k) {
        nn <- x$null[[k]]
        onlyif(length(nn) > 0L, data.frame(order = k, stat = nn))
      }
    )
  )
  stopifnot_(
    !is.null(null_df),
    "No order has a permutation distribution to plot.",
    class = "tna_error_markov_order"
  )
  orders <- seq_len(x$max_order)
  order_levels <- sprintf("order %d vs %d", orders, orders - 1L)
  obs_df <- data.frame(
    order = orders,
    g2 = tt$g2[-1L],
    p = tt$p_value[-1L],
    sig = tt$significant[-1L]
  )
  obs_df$decision <- ifelse(
    !is.na(obs_df$sig) & obs_df$sig,
    "reject H0",
    "accept H0"
  )
  # Label left of the line when the observed value lies beyond the null,
  # where a label on the right would be cut off by the panel edge
  null_max <- vapply(
    x$null,
    function(nn) ifelse_(length(nn) > 0L, max(nn), NA_real_),
    numeric(1L)
  )
  obs_df$hjust <- ifelse(!is.na(null_max) & obs_df$g2 > null_max, 1.1, -0.1)
  null_df$order_fac <- factor(
    sprintf("order %d vs %d", null_df$order, null_df$order - 1L),
    levels = order_levels
  )
  obs_df$order_fac <- factor(
    sprintf("order %d vs %d", obs_df$order, obs_df$order - 1L),
    levels = order_levels
  )
  p_perm <- ggplot2::ggplot(
    null_df,
    ggplot2::aes(x = !!rlang::sym("stat"))
  ) +
    ggplot2::geom_density(fill = "#CCCCCC", color = "#888888", alpha = 0.6) +
    ggplot2::geom_vline(
      data = obs_df,
      ggplot2::aes(
        xintercept = !!rlang::sym("g2"),
        color = !!rlang::sym("decision")
      ),
      linewidth = 0.9
    ) +
    ggplot2::geom_text(
      data = obs_df,
      ggplot2::aes(
        x = !!rlang::sym("g2"),
        y = 0,
        label = sprintf("p = %.3f", !!rlang::sym("p")),
        color = !!rlang::sym("decision"),
        hjust = !!rlang::sym("hjust")
      ),
      vjust = -0.8,
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "reject H0" = unname(pal["reject"]),
        "accept H0" = unname(pal["accept"])
      ),
      name = NULL
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym("order_fac")),
      scales = "free",
      ncol = 1
    ) +
    ggplot2::labs(
      x = expression(G^2 ~ "statistic"),
      y = "density",
      title = paste0(prefix, "Permutation null distributions"),
      subtitle = sprintf(
        paste0(
          "Within-context permutation, iter = %d; ",
          "observed G^2 as vertical line"
        ),
        x$iter
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  if (panel == "permutation") {
    return(p_perm)
  }
  plots <- list(ic = p_ic, permutation = p_perm)
  if (!combined) {
    return(invisible(plots))
  }
  # Side by side with base grid viewports
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1L, 2L)))
  print(p_ic, vp = grid::viewport(layout.pos.row = 1L, layout.pos.col = 1L))
  print(p_perm, vp = grid::viewport(layout.pos.row = 1L, layout.pos.col = 2L))
  grid::popViewport()
  invisible(plots)
}

#' @export
#' @rdname plot.tna_markov_order
plot.group_tna_markov_order <- function(x, panel = "both", combined = TRUE,
                                        title = names(x), ...) {
  check_missing(x)
  check_class(x, "group_tna_markov_order")
  rlang::check_dots_empty()
  panel <- check_match(panel, c("both", "ic", "permutation"))
  title <- rep_len(title, length(x))
  plots <- Map(
    function(y, i) {
      plot.tna_markov_order(y, panel = panel, combined = combined, title = i)
    },
    x,
    title
  )
  # Single panels are ggplot objects that draw only when printed
  if (panel != "both") {
    lapply(plots, print)
  }
  invisible(plots)
}
