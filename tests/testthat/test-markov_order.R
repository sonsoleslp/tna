# Small data where the hand computations below are easy to follow
markov_seqs <- data.frame(
  T1 = c("A", "B", "A", "C", "B", "A", "C", "B"),
  T2 = c("B", "C", "A", "A", "B", "C", "C", "A"),
  T3 = c("A", "A", "B", "B", "C", "C", "A", "B"),
  T4 = c("C", "A", "B", "C", "A", "B", "B", "B"),
  T5 = c("B", "B", "C", "A", "A", "B", "C", "C")
)

# Likelihood-ratio statistic for the independence of x and s within each w
hand_g2 <- function(x, w, s) {
  parts <- vapply(
    split(seq_along(s), w),
    function(i) {
      tab <- table(x[i], s[i])
      tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
      expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
      nz <- tab > 0
      c(
        g2 = 2 * sum(tab[nz] * log(tab[nz] / expected[nz])),
        df = (nrow(tab) - 1) * (ncol(tab) - 1)
      )
    },
    numeric(2L)
  )
  rowSums(parts)
}

# All windows of `k + 1` consecutive states (complete sequences only), one
# window per row of a character matrix
windows <- function(d, k) {
  rows <- lapply(seq_len(nrow(d)), function(i) unlist(d[i, ], use.names = FALSE))
  do.call(
    rbind,
    lapply(rows, function(r) {
      starts <- seq_len(length(r) - k)
      t(vapply(starts, function(t) r[t:(t + k)], character(k + 1L)))
    })
  )
}

test_that("the order 1 and order 2 statistics match a table computation", {
  model <- tna(markov_seqs)
  set.seed(1)
  mo <- markov_order(model, max_order = 2, iter = 20)
  w1 <- windows(markov_seqs, 1)
  ref1 <- hand_g2(x = w1[, 1], w = rep("", nrow(w1)), s = w1[, 2])
  w2 <- windows(markov_seqs, 2)
  ref2 <- hand_g2(x = w2[, 1], w = w2[, 2], s = w2[, 3])
  expect_equal(mo$stats$g2[2:3], unname(c(ref1["g2"], ref2["g2"])))
  expect_equal(mo$stats$df[2:3], unname(c(ref1["df"], ref2["df"])))
  expect_equal(
    mo$stats$p_asymptotic[2:3],
    stats::pchisq(mo$stats$g2[2:3], mo$stats$df[2:3], lower.tail = FALSE)
  )
})

test_that("the log-likelihoods follow the multi-order model", {
  model <- tna(markov_seqs)
  set.seed(2)
  # With observed parameters, AIC prefers order 2 in these small data
  expect_warning(
    mo <- markov_order(model, max_order = 2, iter = 20, dof = "observed"),
    class = "tna_warning_markov_order"
  )
  states <- unlist(markov_seqs)
  p0 <- table(states) / length(states)
  w1 <- windows(markov_seqs, 1)
  p1 <- table(w1[, 1], w1[, 2])
  p1 <- p1 / rowSums(p1)
  w2 <- windows(markov_seqs, 2)
  ctx2 <- paste(w2[, 1], w2[, 2])
  p2 <- table(ctx2, w2[, 3])
  p2 <- p2 / rowSums(p2)
  first <- sum(log(p0[markov_seqs$T1]))
  ll0 <- sum(log(p0[states]))
  ll1 <- first + sum(log(p1[w1]))
  # Order 2 uses order 1 for the second state of each sequence
  ll2 <- first + sum(log(p1[cbind(markov_seqs$T1, markov_seqs$T2)])) +
    sum(log(p2[cbind(ctx2, w2[, 3])]))
  expect_equal(mo$stats$loglik, c(ll0, ll1, ll2))
  # Observed parameters: one less than the successors of each context
  dof <- c(
    length(p0) - 1,
    sum(rowSums(p1 > 0) - 1),
    sum(rowSums(p2 > 0) - 1)
  )
  n_obs <- length(states)
  expect_equal(mo$stats$aic, 2 * cumsum(dof) - 2 * mo$stats$loglik)
  expect_equal(mo$stats$bic, log(n_obs) * cumsum(dof) - 2 * mo$stats$loglik)
})

test_that("path parameters count the paths the transitions allow", {
  model <- tna(markov_seqs)
  set.seed(3)
  mo <- markov_order(model, max_order = 2, iter = 20, dof = "paths")
  adjacency <- 1 * (model$weights > 0)
  extra <- pmax(rowSums(adjacency) - 1, 0)
  dof <- c(2, sum(extra), sum(colSums(adjacency) * extra))
  expect_equal(mo$stats$aic, 2 * cumsum(dof) - 2 * mo$stats$loglik)
})

test_that("path parameters are counted from the paths, not the observations", {
  # Transitions A->B, A->C, B->C, C->A only: A has one extra successor, so
  # order k has one parameter per path of length k - 1 that ends in A.
  # Order 1: A itself (1). Order 2: C->A (1). Order 3: A->C->A, B->C->A (2).
  cycle <- data.frame(
    T1 = c("A", "B", "C", "A", "B", "C"),
    T2 = c("B", "C", "A", "C", "C", "A"),
    T3 = c("C", "A", "C", "A", "A", "B"),
    T4 = c("A", "B", "A", "B", "C", "C"),
    T5 = c("C", "C", "B", "C", "A", "A"),
    T6 = c("A", "A", "C", "A", "B", "C")
  )
  model <- tna(cycle)
  expect_equal(sum(model$weights > 0), 4L)
  set.seed(14)
  mo <- markov_order(model, max_order = 3, iter = 20, dof = "paths")
  dof <- c(2, 1, 1, 2)
  expect_equal(mo$stats$aic, 2 * cumsum(dof) - 2 * mo$stats$loglik)
})

test_that("results do not depend on the state labels or the sequence order", {
  model <- tna(markov_seqs)
  relabeled <- markov_seqs
  relabeled[] <- lapply(relabeled, function(v) c(A = "z", B = "y", C = "x")[v])
  reordered <- markov_seqs[rev(seq_len(nrow(markov_seqs))), ]
  set.seed(4)
  a <- markov_order(model, max_order = 2, iter = 20)$stats
  set.seed(4)
  b <- markov_order(tna(relabeled), max_order = 2, iter = 20)$stats
  set.seed(4)
  d <- markov_order(tna(reordered), max_order = 2, iter = 20)$stats
  deterministic <- c("loglik", "aic", "bic", "g2", "df", "p_asymptotic")
  expect_equal(b[deterministic], a[deterministic])
  expect_equal(d[deterministic], a[deterministic])
})

test_that("sequences are split at missing values", {
  with_gap <- data.frame(
    T1 = c("A", "B", "C"), T2 = c("B", "C", "A"), T3 = c(NA, "A", "B"),
    T4 = c("C", "B", "C"), T5 = c("A", "A", NA)
  )
  # The first sequence becomes two pieces: A B and C A
  pieces <- data.frame(
    T1 = c("A", "C", "B", "C"), T2 = c("B", "A", "C", "A"),
    T3 = c(NA, NA, "A", "B"), T4 = c(NA, NA, "B", "C"),
    T5 = c(NA, NA, "A", NA)
  )
  set.seed(5)
  a <- markov_order(tna(with_gap), max_order = 2, iter = 20)
  set.seed(5)
  b <- markov_order(tna(pieces), max_order = 2, iter = 20)
  expect_equal(a$stats, b$stats)
  expect_equal(a$n_sequences, 4L)
  expect_equal(a$n_observations, 13L)
})

test_that("permutation p-values are valid and reproducible", {
  model <- tna(group_regulation[1:200, ])
  set.seed(6)
  a <- markov_order(model, max_order = 2, iter = 30)
  set.seed(6)
  b <- markov_order(model, max_order = 2, iter = 30)
  expect_identical(a, b)
  p <- a$stats$p_value[-1L]
  expect_true(all(p >= 1 / 31 & p <= 1))
  expect_equal(p * 31, round(p * 31))
  expect_length(a$null$order_1, 30L)
})

test_that("the selected order recovers the order of simulated chains", {
  set.seed(7)
  first_order <- matrix(c(0.8, 0.1, 0.1, 0.1, 0.8, 0.1, 0.1, 0.1, 0.8), 3)
  seqs <- t(replicate(150, {
    v <- integer(12)
    v[1] <- sample.int(3, 1)
    # sequential simulation: each state depends on the previous one
    for (t in 2:12) v[t] <- sample.int(3, 1, prob = first_order[v[t - 1], ])
    v
  }))
  d1 <- as.data.frame(matrix(c("a", "b", "c")[seqs], 150))
  mo1 <- markov_order(tna(d1), max_order = 2, iter = 99)
  expect_equal(mo1$selected_order, 1L)
  # Order 2 is a true null here: the permutation statistics average their df
  expect_equal(mean(mo1$null$order_2), mo1$stats$df[3L], tolerance = 0.25)
  # Second order: the next state repeats the state two steps back
  seqs <- t(replicate(150, {
    v <- integer(12)
    v[1:2] <- sample.int(3, 2, replace = TRUE)
    # sequential simulation: each state depends on the state two steps back
    for (t in 3:12) {
      v[t] <- ifelse(stats::runif(1) < 0.8, v[t - 2], sample.int(3, 1))
    }
    v
  }))
  d2 <- as.data.frame(matrix(c("a", "b", "c")[seqs], 150))
  mo2 <- markov_order(tna(d2), max_order = 3, iter = 99)
  expect_equal(mo2$selected_order, 2L)
})

test_that("max_order is capped by the longest sequence", {
  model <- tna(markov_seqs)
  set.seed(8)
  expect_message(
    mo <- markov_order(model, max_order = 10, iter = 20),
    "reduced to 4"
  )
  expect_equal(mo$max_order, 4L)
  expect_equal(nrow(mo$stats), 5L)
})

test_that("grouped models are tested group by group", {
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  set.seed(9)
  mo <- markov_order(groups, max_order = 2, iter = 20)
  expect_s3_class(mo, "group_tna_markov_order")
  expect_named(mo, c("a", "b"))
  set.seed(9)
  single <- markov_order(groups$a, max_order = 2, iter = 20)
  expect_equal(mo$a, single)
  tab <- summary(mo)
  expect_equal(names(tab)[1L], "group")
  expect_equal(nrow(tab), 6L)
})

test_that("invalid input raises errors", {
  model <- tna(markov_seqs)
  expect_error(markov_order(model, max_order = 0))
  expect_error(markov_order(model, iter = -1))
  expect_error(markov_order(model, dof = "all"))
  # No two observed states are next to each other
  no_pairs <- tna(
    data.frame(T1 = c("A", NA), T2 = c(NA, "B"), T3 = c("A", NA))
  )
  expect_error(
    markov_order(no_pairs, iter = 20),
    class = "tna_error_markov_order"
  )
})

test_that("summary rounds everything but the p-values", {
  model <- tna(group_regulation[1:200, ])
  set.seed(10)
  mo <- markov_order(model, max_order = 2, iter = 20)
  tab <- summary(mo, digits = 1)
  expect_s3_class(tab, "summary.tna_markov_order")
  expect_equal(tab$loglik, round(mo$stats$loglik, 1))
  expect_equal(tab$p_value, mo$stats$p_value)
  expect_equal(nrow(tab), 3L)
})

test_that("print and plot methods work", {
  model <- tna(group_regulation[1:200, ])
  set.seed(11)
  mo <- markov_order(model, max_order = 2, iter = 20)
  expect_output(print(mo), "Selected order")
  expect_output(print(summary(mo)), "p_asymptotic")
  expect_s3_class(plot(mo, panel = "ic"), "ggplot")
  expect_s3_class(plot(mo, panel = "permutation"), "ggplot")
  both <- plot(mo)
  expect_named(both, c("ic", "permutation"))
  separate <- plot(mo, combined = FALSE)
  expect_s3_class(separate$ic, "ggplot")
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  set.seed(12)
  mo_groups <- markov_order(groups, max_order = 2, iter = 20)
  expect_output(print(mo_groups), "a:")
  plots <- plot(mo_groups)
  expect_length(plots, 2L)
})

test_that("printing never uses scientific notation", {
  model <- tna(group_regulation)
  set.seed(13)
  mo <- markov_order(model, max_order = 2, iter = 20)
  out <- capture.output(print(summary(mo)))
  expect_false(any(grepl("e[+-][0-9]", out)))
})

# Constant and alternating sequences in balance: the next state does not
# depend on the current state alone, but on the two previous states
skip_seqs <- as.data.frame(do.call(rbind, c(
  replicate(25, rep("A", 12), simplify = FALSE),
  replicate(25, rep("B", 12), simplify = FALSE),
  replicate(25, rep(c("A", "B"), 6), simplify = FALSE),
  replicate(25, rep(c("B", "A"), 6), simplify = FALSE)
)))

test_that("a dependence that skips an order is flagged, not selected", {
  set.seed(15)
  expect_warning(
    mo <- markov_order(tna(skip_seqs), max_order = 2, iter = 99),
    "test of order 2 rejects",
    class = "tna_warning_markov_order"
  )
  # The sequential rule itself is unchanged
  expect_equal(mo$selected_order, 0L)
  expect_equal(mo$aic_order, 2L)
  expect_equal(mo$bic_order, 2L)
  expect_output(print(mo), "Note: The sequential tests select order 0")
  expect_output(print(summary(mo)), "Note: The sequential tests select order 0")
  # Every group gets half of each kind of sequence
  groups <- group_tna(skip_seqs, group = rep(c("x", "y"), 50))
  set.seed(16)
  messages <- character(0L)
  withCallingHandlers(
    markov_order(groups, max_order = 2, iter = 99),
    tna_warning_markov_order = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(messages, 2L)
  expect_match(messages, "^(x|y): The sequential tests select order")
})

test_that("agreeing order selections raise no warning or note", {
  set.seed(17)
  expect_no_warning(
    mo <- markov_order(tna(group_regulation), max_order = 2, iter = 50)
  )
  expect_equal(mo$selected_order, mo$bic_order)
  expect_false(any(grepl("Note:", capture.output(print(mo)))))
  expect_null(attr(summary(mo), "notes"))
})

test_that("too few permutations to reach the level are flagged once", {
  model <- tna(group_regulation)
  set.seed(18)
  n_warnings <- 0L
  withCallingHandlers(
    mo <- markov_order(model, max_order = 2, iter = 10),
    tna_warning_markov_order = function(w) {
      n_warnings <<- n_warnings + 1L
      expect_match(conditionMessage(w), "no test can reject")
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(n_warnings, 1L)
  expect_true(all(mo$stats$p_value[-1L] >= 0.05))
  # floor(1 / level) permutations are enough
  set.seed(19)
  expect_no_warning(markov_order(model, max_order = 1, iter = 20))
  set.seed(20)
  expect_warning(
    markov_order(model, max_order = 1, iter = 19),
    class = "tna_warning_markov_order"
  )
})

test_that("unknown plot arguments are errors", {
  set.seed(21)
  mo <- markov_order(tna(group_regulation), max_order = 2, iter = 20)
  expect_error(plot(mo, type = "ic"), class = "rlib_error_dots_nonempty")
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  set.seed(22)
  mo_groups <- markov_order(groups, max_order = 1, iter = 20)
  expect_error(
    plot(mo_groups, type = "ic"),
    class = "rlib_error_dots_nonempty"
  )
})
