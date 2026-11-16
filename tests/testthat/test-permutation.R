test_that("permutation test can be applied", {
  model_x <- tna(mock_sequence[1:2, ])
  model_y <- tna(mock_sequence[3:5, ])
  expect_error(
    permutation_test(model_x, model_y, iter = 20),
    NA
  )
})

test_that("paired permutation test can be applied", {
  model_x <- tna(mock_sequence[1:2, ])
  model_y <- tna(mock_sequence[4:5, ])
  expect_error(
    permutation_test(model_x, model_y, paired = TRUE, iter = 20),
    NA
  )
})

test_that("permutation test can be applied with groups", {
  expect_error(
    permutation_test(mmm_model, iter = 20),
    NA
  )
})

test_that("permutation test p-values can be adjusted", {
  expect_error(
    permutation_test(mmm_model, iter = 20, adjust = "holm"),
    NA
  )
  expect_error(
    permutation_test(mmm_model, iter = 20, adjust = "holm", adjust_pairwise = FALSE),
    NA
  )
})

test_that("paired permutation fails when data are incomparable", {
  model_x <- tna(mock_sequence[1:4, ])
  model_y <- tna(mock_sequence[3:5, ])
  model_z <- tna(mock_sequence[c(1, 4, 5), 1:2])
  model_w <- model_z
  attr(model_w$data, "alphabet") <- rev(attr(model_w$data, "alphabet"))
  expect_error(
    permutation_test(model_x, model_y, paired = TRUE),
    "The number of observations must be the same in `x` and `y` for a paired test\\."
  )
  expect_error(
    permutation_test(model_x, model_z),
    "The number of states of `x` and `y` must be the same\\."
  )
  expect_error(
    permutation_test(model_z, model_w),
    "The state labels of `x` and `y` must be the same and in the same order\\."
  )
})

test_that("permutation test p-values can be adjusted", {
  model_x <- tna(mock_sequence[1:2, ])
  model_y <- tna(mock_sequence[3:5, ])
  for (method in stats::p.adjust.methods) {
    expect_error(
      permutation_test(model_x, model_y, adjust = method),
      NA
    )
  }
})

test_that("permutation test pairs can be made consecutive", {
  model <- group_model(mock_sequence, group = c(1, 1, 2, 3, 3))
  expect_error(
    out <- permutation_test(model, consecutive = TRUE, iter = 20),
    NA
  )
  expect_true(length(out) == 2)
})
test_that("effect sizes are NA where the permuted differences do not vary", {
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  set.seed(2)
  perm <- permutation_test(
    betweenness_network(groups$a),
    betweenness_network(groups$b),
    iter = 49
  )
  stats <- perm$edges$stats
  expect_false(any(is.infinite(stats$effect_size)))
  expect_false(any(is.nan(stats$effect_size)))
  # No shortest path runs through a self-loop, so its edge betweenness is
  # zero in every permutation and its effect size is undefined
  loops <- c(diag(nrow(perm$edges$diffs_true)) == 1)
  expect_true(all(is.na(stats$effect_size[loops])))
  expect_true(any(is.finite(stats$effect_size)))
  expect_equal(
    effect_size(c(2, 0, 3), c(0, 0, 1.5), c(4, 0, 6)),
    c(NA, NA, 2)
  )
})

test_that("undefined effect sizes do not depend on the measurement scale", {
  diff <- c(2, 0.5, 1)
  sd <- c(1, 1e-18, 0.25)
  scale <- c(3, 1, 2)
  reference <- effect_size(diff, sd, scale)
  expect_equal(reference, c(2, NA, 4))
  # Multiplying every value by a constant changes no effect size
  vapply(
    c(1e-12, 1e-6, 1e6, 1e12),
    function(k) {
      expect_equal(effect_size(k * diff, k * sd, k * scale), reference)
      TRUE
    },
    logical(1L)
  )
  # Small values that do vary keep their effect size
  expect_equal(effect_size(c(2, 2e-9), c(1, 1e-9), c(5, 5e-9)), c(2, 2))
})

test_that("unknown arguments of compare_sequences() are errors", {
  idx <- c(1:100, 1001:1100)
  group <- rep(c("High", "Low"), each = 100)
  expect_error(
    compare_sequences(
      group_regulation[idx, ],
      group,
      correction = "holm",
      iter = 5
    ),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("summary of a permutation test is a tidy edge table", {
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  set.seed(5)
  perm <- permutation_test(groups$a, groups$b, iter = 9)
  tab <- summary(perm, digits = 3)
  w <- groups$a$weights
  expect_equal(nrow(tab), length(w))
  expect_equal(names(tab), c("from", "to", "diff_true", "effect_size",
                             "p_value"))
  # Each row is the edge from -> to of the weight matrices
  expected <- groups$a$weights - groups$b$weights
  expect_equal(tab$diff_true, round(expected[cbind(tab$from, tab$to)], 3))
  expect_equal(tab$p_value, perm$edges$stats$p_value)
  set.seed(5)
  by_pair <- summary(permutation_test(groups, iter = 9))
  expect_equal(names(by_pair)[1L], "pair")
  expect_equal(nrow(by_pair), length(w))
})

test_that("summary of a permutation test returns the centralities on request", {
  groups <- group_tna(group_regulation, group = rep(c("a", "b"), each = 1000))
  set.seed(31)
  perm <- permutation_test(groups$a, groups$b, iter = 9,
                           measures = c("InStrength", "Betweenness"))
  cent <- summary(perm, what = "centralities", digits = 3)
  expect_named(cent, c("state", "centrality", "diff_true", "effect_size",
                       "p_value"))
  expect_equal(nrow(cent), 9L * 2L)
  expect_equal(cent$p_value, perm$centralities$stats$p_value)
  expect_equal(cent$diff_true, round(perm$centralities$stats$diff_true, 3))
  # The edge table is still the default
  expect_named(summary(perm), c("from", "to", "diff_true", "effect_size",
                                "p_value"))
  set.seed(32)
  by_pair <- summary(
    permutation_test(groups, iter = 9, measures = "InStrength"),
    what = "centralities"
  )
  expect_equal(unique(by_pair$pair), "a vs. b")
  expect_equal(nrow(by_pair), 9L)
  set.seed(33)
  edges_only <- permutation_test(groups$a, groups$b, iter = 9)
  expect_error(
    summary(edges_only, what = "centralities"),
    class = "tna_error_permutation"
  )
})

test_that("pruning does not enter the permutation test", {
  model <- tna(group_regulation[1:200, ])
  pruned <- prune(model, threshold = 0.1)
  set.seed(1)
  res_pruned <- permutation_test(
    model, pruned, iter = 30, measures = c("InStrength", "Betweenness")
  )
  # Both networks come from the same data, so no difference is observed
  expect_true(all(res_pruned$edges$diffs_true == 0))
  expect_true(all(res_pruned$centralities$stats$diff_true == 0))
  set.seed(1)
  res_model <- permutation_test(
    model, model, iter = 30, measures = c("InStrength", "Betweenness")
  )
  expect_equal(res_pruned, res_model)
})

test_that("p-value adjustment counts only the edges tested", {
  # C -> C and B -> B never occur in either group: their p-values are 1 and
  # they do not enter the correction
  # x alternates A and B, y stays in A: the groups differ clearly, so some
  # p-values are small enough for the size of the correction to show
  x <- as.data.frame(matrix(rep(c("A", "B", "A", "B", "C"), each = 20), 20))
  y <- as.data.frame(matrix(rep(c("A", "A", "A", "C", "B"), each = 20), 20))
  mx <- tna(x)
  my <- tna(y)
  tested <- c(mx$weights != 0 | my$weights != 0)
  m <- sum(tested)
  expect_true(m < length(tested))
  set.seed(1)
  raw <- permutation_test(mx, my, iter = 200, adjust = "none")
  expect_true(any(raw$edges$stats$p_value[tested] * m < 1))
  set.seed(1)
  bon <- permutation_test(mx, my, iter = 200, adjust = "bonferroni")
  expect_equal(
    bon$edges$stats$p_value[tested],
    pmin(1, raw$edges$stats$p_value[tested] * m)
  )
  expect_true(all(bon$edges$stats$p_value[!tested] == 1))
})

test_that("a co-occurrence network corrects each pair once", {
  # x alternates A and B, y stays in A: the groups differ clearly, so some
  # p-values are small enough for the size of the correction to show
  x <- as.data.frame(matrix(rep(c("A", "B", "A", "B", "C"), each = 20), 20))
  y <- as.data.frame(matrix(rep(c("A", "A", "A", "C", "B"), each = 20), 20))
  mx <- ctna(x)
  my <- ctna(y)
  tested <- (mx$weights != 0 | my$weights != 0) & upper.tri(mx$weights,
                                                            diag = TRUE)
  m <- sum(tested)
  set.seed(1)
  raw <- permutation_test(mx, my, iter = 200, adjust = "none")
  set.seed(1)
  bon <- permutation_test(mx, my, iter = 200, adjust = "bonferroni")
  p_raw <- matrix(raw$edges$stats$p_value, nrow(mx$weights))
  p_bon <- matrix(bon$edges$stats$p_value, nrow(mx$weights))
  expect_equal(p_bon[tested], pmin(1, p_raw[tested] * m))
  expect_equal(p_bon, t(p_bon))
})

test_that("comparing scaled models informs that it is not warranted", {
  x <- ftna(group_regulation[1:100, ], scaling = "minmax")
  y <- ftna(group_regulation[101:200, ], scaling = "minmax")
  set.seed(1)
  expect_message(
    permutation_test(x, y, iter = 5),
    class = "tna_message_scaling"
  )
  # one scaled model is enough
  set.seed(1)
  expect_message(
    permutation_test(ftna(group_regulation[1:100, ]), y, iter = 5),
    class = "tna_message_scaling"
  )
  set.seed(1)
  expect_no_message(
    permutation_test(
      ftna(group_regulation[1:100, ]),
      ftna(group_regulation[101:200, ]),
      iter = 5
    ),
    class = "tna_message_scaling"
  )
})
