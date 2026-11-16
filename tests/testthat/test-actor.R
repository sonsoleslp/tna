# Sequences nested in actors (`actor` argument) -------------------------------

# Twelve actors with five identical sequences each: maximal nesting.
# Actors 1-4 are in group "x", 5-8 in "y", 9-12 have sequences in both.
nested_wide <- function() {
  patterns <- list(
    c("A", "A", "A", "B"), c("B", "C", "C", "C"),
    c("C", "A", "B", "A"), c("A", "B", "C", "B")
  )
  actor <- rep(seq_len(12L), each = 5L)
  wide <- as.data.frame(do.call(rbind, patterns[(actor - 1L) %% 4L + 1L]))
  names(wide) <- paste0("T", 1:4)
  wide$actor <- actor
  wide$grp <- ifelse(
    actor <= 4L,
    "x",
    ifelse(actor <= 8L, "y", rep(c("x", "y", "y", "x", "y"), 12L))
  )
  wide
}

grl_data <- function() {
  rlang::local_options(rlib_message_verbosity = "quiet")
  prepare_data(
    group_regulation_long,
    actor = "Actor",
    action = "Action",
    time = "Time"
  )
}

# ---- models keep the non-sequence columns ----

test_that("models keep the non-sequence columns, one row per sequence", {
  wide <- nested_wide()
  model <- tna(wide, cols = T1:T4)
  meta <- attr(model$data, "meta")
  expect_identical(names(meta), c("actor", "grp"))
  expect_identical(meta$actor, wide$actor)
  expect_null(attr(tna(mock_sequence)$data, "meta"))
  d <- grl_data()
  expect_identical(
    names(attr(tna(d)$data, "meta")),
    names(d$meta_data)
  )
})

test_that("grouped models split the columns and record their rows", {
  wide <- nested_wide()
  model <- group_model(wide, group = "grp", cols = T1:T4)
  expect_identical(attr(model, "n_rows"), nrow(wide))
  rows_x <- attr(model$x$data, "rows")
  expect_identical(rows_x, which(wide$grp == "x"))
  expect_identical(attr(model$x$data, "meta")$actor, wide$actor[rows_x])
})

test_that("concatenated sequences keep the columns constant per block", {
  wide <- nested_wide()
  wide$row <- seq_len(nrow(wide))
  model <- tna(wide, cols = T1:T4, concat = 5L)
  meta <- attr(model$data, "meta")
  expect_identical(nrow(meta), nrow(model$data))
  expect_identical(meta$actor, seq_len(12L))
  expect_false("row" %in% names(meta))
})

test_that("clustering keeps the metadata of prepared data", {
  d <- grl_data()
  clusters <- cluster_data(d, k = 2)
  model <- group_model(clusters)
  meta <- attr(model[[1L]]$data, "meta")
  expect_true("Group" %in% names(meta))
  rows <- attr(model[[1L]]$data, "rows")
  expect_identical(meta$Group, d$meta_data$Group[rows])
})

# ---- the permutation scheme ----

test_that("two-group scheme keeps pure actors whole and crossed counts", {
  ids <- c("a", "a", "b", "b", "c", "c", "d", "d", "d")
  is_x <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
  design <- actor_design(ids, is_x, level = 0.5)
  set.seed(1)
  perms <- replicate(300, actor_permute(design))
  expect_true(all(colSums(perms[ids == "c", ]) == 1L))
  expect_true(all(colSums(perms[ids == "d", ]) == 1L))
  expect_true(all(perms[1L, ] == perms[2L, ]))
  expect_true(all(perms[3L, ] == perms[4L, ]))
  expect_true(all(perms[1L, ] != perms[3L, ]))
  # 2 (pure) x 2 (c) x 3 (d) arrangements, all reached
  expect_identical(nrow(unique(t(perms))), 12L)
})

test_that("k-group scheme keeps pure actors whole and crossed counts", {
  ids <- c("a", "a", "b", "c", "c", "d", "d", "d")
  labels <- c("g1", "g1", "g2", "g3", "g3", "g1", "g2", "g2")
  design <- actor_design(ids, labels, level = 0.5)
  set.seed(2)
  perms <- replicate(500, actor_permute(design))
  pure <- perms[ids %in% c("a", "b", "c"), ]
  expect_true(all(perms[1L, ] == perms[2L, ]))
  expect_true(all(perms[4L, ] == perms[5L, ]))
  # the pure actors' labels are a rearrangement of g1, g2, g3
  expect_true(all(apply(perms[c(1L, 3L, 4L), ], 2L, sort) == c("g1", "g2", "g3")))
  expect_true(all(colSums(perms[ids == "d", ] == "g1") == 1L))
  # 3! (pure) x 3 (d) arrangements, all reached
  expect_identical(nrow(unique(t(perms))), 18L)
  expect_identical(exp(log_arrangements(c("g1", "g2", "g3"))), 6)
})

test_that("a warning is given when too few arrangements exist", {
  ids <- c("a", "a", "b", "b")
  is_x <- c(TRUE, TRUE, FALSE, FALSE)
  expect_warning(actor_design(ids, is_x, level = 0.05), "2 distinct")
  expect_no_warning(actor_design(ids, is_x, level = 0.6))
})

# ---- permutation_test() ----

test_that("permutation test accepts a column or a vector", {
  wide <- nested_wide()
  model <- group_model(wide, group = "grp", cols = T1:T4)
  set.seed(1)
  by_column <- permutation_test(model, iter = 30, actor = "actor")
  set.seed(1)
  by_vector <- permutation_test(model, iter = 30, actor = wide$actor)
  expect_identical(by_column, by_vector)
  set.seed(1)
  pair <- permutation_test(
    model$x,
    model$y,
    iter = 30,
    actor = c(wide$actor[wide$grp == "x"], wide$actor[wide$grp == "y"])
  )
  expect_identical(pair$edges$stats, by_column[[1L]]$edges$stats)
})

test_that("permutation test leaves the observed differences unchanged", {
  model <- group_model(grl_data(), group = "Achiever")
  set.seed(1)
  plain <- permutation_test(model, iter = 20)
  set.seed(1)
  nested <- permutation_test(model, iter = 20, actor = "Group")
  expect_identical(
    nested[[1L]]$edges$stats$diff_true,
    plain[[1L]]$edges$stats$diff_true
  )
  expect_true(all(nested[[1L]]$edges$stats$p_value > 0))
})

test_that("permutation test works on mixture and cluster models", {
  model <- group_model(engagement_mmm)
  actor <- rep(seq_len(nrow(engagement) / 2), each = 2L)
  set.seed(1)
  expect_s3_class(
    permutation_test(model, iter = 10, actor = actor),
    "group_tna_permutation"
  )
  clusters <- group_model(cluster_data(grl_data(), k = 2))
  set.seed(1)
  expect_s3_class(
    permutation_test(clusters, iter = 10, actor = "Group"),
    "group_tna_permutation"
  )
})

# ---- bootstrap() ----

test_that("one sequence per actor reproduces the ordinary bootstrap", {
  model <- tna(cbind(mock_sequence, id = letters[1:5]), cols = T1:T6)
  expect_identical(
    bootstrap(model, iter = 30, seed = 9, actor = "id"),
    bootstrap(model, iter = 30, seed = 9)
  )
})

test_that("actor bootstrap resamples whole actors", {
  wide <- nested_wide()
  model <- tna(wide, cols = T1:T4)
  boot <- bootstrap(model, iter = 25, seed = 4, actor = "actor")
  # independent reference: refit the model on resampled actors' rows
  set.seed(4)
  units <- split(seq_len(nrow(wide)), wide$actor)
  ref <- vapply(
    seq_len(25),
    function(i) {
      rows <- unlist(units[sample.int(12L, 12L, replace = TRUE)])
      c(tna(wide[rows, ], cols = T1:T4)$weights)
    },
    numeric(9L)
  )
  expect_equal(c(boot$weights_mean), rowMeans(ref))
  expect_identical(
    bootstrap(model, iter = 25, seed = 4, actor = wide$actor),
    boot
  )
})

test_that("actor bootstrap widens intervals under nesting", {
  model <- tna(nested_wide(), cols = T1:T4)
  plain <- bootstrap(model, iter = 200, seed = 1)
  nested <- bootstrap(model, iter = 200, seed = 1, actor = "actor")
  present <- model$weights > 0
  expect_gt(median(nested$weights_sd[present] / plain$weights_sd[present]), 1.5)
  expect_identical(nested$weights_orig, plain$weights_orig)
})

test_that("grouped bootstrap maps an actor vector to each group", {
  wide <- nested_wide()
  model <- group_model(wide, group = "grp", cols = T1:T4)
  expect_identical(
    bootstrap(model, iter = 20, seed = 1, actor = wide$actor),
    bootstrap(model, iter = 20, seed = 1, actor = "actor")
  )
  expect_s3_class(
    bootstrap(model, iter = 5, seed = 1, method = "threshold", actor = "actor"),
    "group_tna_bootstrap"
  )
})

# ---- estimate_cs() and reliability() ----

test_that("one sequence per actor reproduces case-dropping and splits", {
  model <- tna(cbind(group_regulation[1:100, ], id = 1:100), cols = T1:T26)
  set.seed(5)
  plain <- estimate_cs(model, iter = 5, drop_prop = c(0.3, 0.6))
  set.seed(5)
  nested <- estimate_cs(model, iter = 5, drop_prop = c(0.3, 0.6), actor = "id")
  expect_identical(nested, plain)
  set.seed(6)
  plain <- reliability(model, iter = 5)
  set.seed(6)
  nested <- reliability(model, iter = 5, actor = "id")
  expect_identical(nested, plain)
})

test_that("case-dropping drops whole actors", {
  wide <- nested_wide()
  model <- tna(wide, cols = T1:T4)
  set.seed(7)
  stab <- estimate_cs(
    model, iter = 1, drop_prop = 0.5, measures = "InStrength", actor = "actor"
  )
  set.seed(7)
  units <- split(seq_len(nrow(wide)), wide$actor)
  rows <- unlist(units[sample.int(12L, 6L)])
  ref <- stats::cor(
    centralities(tna(wide[rows, ], cols = T1:T4), measures = "InStrength")$InStrength,
    centralities(model, measures = "InStrength")$InStrength
  )
  expect_equal(stab$InStrength$correlations[1L, 1L], ref)
})

test_that("split-half keeps each actor's sequences together", {
  model <- tna(nested_wide(), cols = T1:T4)
  set.seed(8)
  plain <- reliability(model, iter = 50)$summary
  set.seed(8)
  nested <- reliability(model, iter = 50, actor = "actor")$summary
  # halves of identical-within-actor sequences agree less when split by actor
  expect_lt(
    nested$mean[nested$metric == "Pearson"],
    plain$mean[plain$metric == "Pearson"]
  )
})

test_that("grouped case-dropping maps an actor vector to each group", {
  wide <- nested_wide()
  model <- group_model(wide, group = "grp", cols = T1:T4)
  set.seed(9)
  by_vector <- estimate_cs(
    model, iter = 3, drop_prop = 0.5, measures = "InStrength",
    actor = wide$actor
  )
  set.seed(9)
  by_column <- estimate_cs(
    model, iter = 3, drop_prop = 0.5, measures = "InStrength",
    actor = "actor"
  )
  expect_identical(by_vector, by_column)
})

# ---- compare_sequences() ----

test_that("sequence comparison permutes actors", {
  wide <- nested_wide()
  set.seed(10)
  plain <- compare_sequences(
    wide[, 1:4], group = wide$grp, sub = 1:2, min_freq = 1L, iter = 50
  )
  set.seed(10)
  nested <- compare_sequences(
    wide[, 1:4], group = wide$grp, sub = 1:2, min_freq = 1L, iter = 50,
    actor = wide$actor
  )
  expect_setequal(nested$pattern, plain$pattern)
  freq <- nested[match(plain$pattern, nested$pattern), c("freq_x", "freq_y")]
  expect_equal(freq, plain[, c("freq_x", "freq_y")], ignore_attr = TRUE)
  # the null distribution, and so the standardized effect, follows the actors
  expect_false(isTRUE(all.equal(
    nested$effect_size[match(plain$pattern, nested$pattern)],
    plain$effect_size
  )))
})

# ---- pass-through and validation ----

test_that("pruning passes actor to the bootstrap", {
  model <- tna(nested_wide(), cols = T1:T4)
  expect_s3_class(
    prune(model, method = "bootstrap", iter = 10, actor = "actor"),
    "tna"
  )
})

test_that("actor is validated", {
  wide <- nested_wide()
  model <- tna(wide, cols = T1:T4)
  grouped <- group_model(wide, group = "grp", cols = T1:T4)
  expect_error(bootstrap(model, iter = 5, actor = "nope"), "was not found")
  expect_error(bootstrap(tna(mock_sequence), iter = 5, actor = "id"),
               "was not found")
  expect_error(bootstrap(model, iter = 5, actor = 1:3), "one value per")
  expect_error(bootstrap(model, iter = 5, actor = rep(1, 60)), "at least two")
  expect_error(
    bootstrap(model, iter = 5, actor = c(NA, wide$actor[-1L])),
    "missing values"
  )
  expect_error(bootstrap(grouped, iter = 5, actor = 1:3), "grouped data")
  expect_error(
    permutation_test(grouped$x, grouped$y, iter = 5, actor = 1:3),
    "sequence of `x` and `y`"
  )
  expect_error(
    permutation_test(grouped, iter = 5, actor = "actor", paired = TRUE),
    "cannot be combined"
  )
  expect_error(bootstrap(model, iter = 5, actor = list(1)), "column name")
  old <- grouped
  attr(old, "n_rows") <- NULL
  expect_error(bootstrap(old, iter = 5, actor = wide$actor), "does not record")
})
