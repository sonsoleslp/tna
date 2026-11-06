test_that("group_model returns correct type", {
  expect_true(
    inherits(
      group_model(mock_sequence, group = c(1, 1, 2, 2, 2)),
      "group_tna"
    )
  )
})

test_that("group_ftna returns correct type", {
  expect_error(
    ftna_model <- group_ftna(mock_sequence, group = c(1, 1, 2, 2, 2)),
    NA
  )
  expect_equal(
    attr(ftna_model[[1]], "type"),
    "frequency"
  )
})

test_that("group_ctna returns correct type", {
  expect_error(
    ctna_model <- group_ctna(mock_sequence, group = c(1, 1, 2, 2, 2)),
    NA
  )
  expect_equal(
    attr(ctna_model[[1]], "type"),
    "co-occurrence"
  )
})

test_that("group_atna returns correct type", {
  expect_error(
    atna_model <- group_atna(mock_sequence, group = c(1, 1, 2, 2, 2)),
    NA
  )
  expect_equal(
    attr(atna_model[[1]], "type"),
    "attention"
  )
})

test_that("group_model returns correct type", {
  expect_true(
    inherits(
      group_model(mock_sequence, group = c(1, 1, 2, 2, 2))[[1]],
      "tna"
    )
  )
})

test_that("groups can be renamed", {
  expect_error(
    model <- rename_groups(mock_group_tna, c("A", "B")),
    NA
  )
  expect_equal(model$A, model[[1]])
  expect_equal(model$B, model[[2]])
})

test_that("group model can be summarized", {
  expect_error(
    summary(mmm_model),
    NA
  )
  expect_error(
    summary(mmm_model, combined = FALSE),
    NA
  )
})

test_that("grouped state ordering is deterministic for mixed-case labels", {
  data <- data.frame(
    T1 = c("Plan", "discuss", "Review"),
    T2 = c("discuss", "Review", "Plan"),
    T3 = c("Review", "Plan", "discuss")
  )
  model <- group_model(data, group = rep("Group", nrow(data)))
  expect_identical(
    model[[1]]$labels,
    c("Plan", "Review", "discuss")
  )
})

test_that("grouped model can be constructed from tna_data objects", {
  data <- tibble::tibble(
    user = c("A", "A", "A", "B", "B", "C", "C", "C"),
    time = c(
      "2023-01-01 10:00:00", "2023-01-01 10:05:00",
      "2023-01-01 10:20:00", "2023-01-01 12:00:00",
      "2023-01-01 12:02:00", "2023-01-01 14:00:00",
      "2023-01-01 14:05:00", "2023-01-01 14:10:00"
    ),
    action = c(
      "view", "click", "add_cart", "view",
      "checkout", "view", "click", "share"
    ),
    group = c(rep("Group 1", 5), rep("Group 2", 3))
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  results <- prepare_data(
    data, actor = "user", time = "time", action = "action"
  )
  expect_error(
    model <- group_model(results, group = "group"),
    NA
  )
  expect_true(
    length(model) == 2L
  )
  expect_true(
    all(names(model) == c("Group 1", "Group 2"))
  )
})

test_that("missing values in group variable warns", {
  mock_tna_data_mis <- mock_tna_data
  mock_tna_data_mis$meta_data$group[4] <- NA
  expect_warning(
    model <- group_model(mock_tna_data_mis, group = "group"),
    "Column `group` contains missing values\\."
  )
})

test_that("groupwise models can be constructed with scaling", {
  expect_error(
    group_model(engagement_mmm, scaling = c("minmax", "rank", "max")),
    NA
  )
})

test_that("group model can be constructed from a clustering result", {
  clust <- cluster_sequences(engagement, k = 3)
  expect_error(
    group_model(clust),
    NA
  )
})

test_that("character groups keep their own sequences", {
  seqs <- as.data.frame(matrix("A", 20, 3))
  seqs[1:10, ] <- "B"
  # The data start with "zeta", which is not the first group alphabetically
  grp <- rep(c("zeta", "alpha"), each = 10)
  by_character <- group_model(seqs, group = grp)
  by_factor <- group_model(seqs, group = factor(grp))
  expect_named(by_character, c("zeta", "alpha"))
  expect_equal(by_character$zeta$weights["B", "B"], 1)
  expect_equal(by_character$alpha$weights["A", "A"], 1)
  expect_equal(by_character$zeta$weights, by_factor$zeta$weights)
})

test_that("scaling across groups keeps the state names", {
  model <- group_model(engagement_mmm, scaling = "minmax")
  expect_equal(
    dimnames(model[[1L]]$weights),
    list(model[[1L]]$labels, model[[1L]]$labels)
  )
  expect_no_error(communities(model))
  expect_no_error(compare(model, 1, 2))
})

test_that("scaled groups and their networks are not resampled", {
  groups_of <- function(...) {
    group_model(group_regulation, group = rep(c("a", "b"), each = 1000), ...)
  }
  vapply(
    c(FALSE, TRUE),
    function(groupwise) {
      scaled <- groups_of(scaling = "minmax", groupwise = groupwise)
      expect_error(bootstrap(scaled, iter = 3), class = "tna_error_scaling")
      expect_error(
        permutation_test(scaled, iter = 3),
        class = "tna_error_scaling"
      )
      expect_error(
        estimate_cs(scaled, measures = "InStrength", iter = 3),
        class = "tna_error_scaling"
      )
      expect_error(
        compare_sequences(scaled, sub = 2, iter = 3),
        class = "tna_error_scaling"
      )
      # One group's network on its own
      expect_error(bootstrap(scaled$a, iter = 3), class = "tna_error_scaling")
      expect_error(
        bootstrap_cliques(scaled$a, iter = 3),
        class = "tna_error_scaling"
      )
      expect_error(
        permutation_test(scaled$a, scaled$b, iter = 3),
        class = "tna_error_scaling"
      )
      expect_error(
        estimate_cs(scaled$a, measures = "InStrength", iter = 3),
        class = "tna_error_scaling"
      )
      expect_error(reliability(scaled$a, iter = 3), class = "tna_error_scaling")
      # A pruned network is still a network of the scaled group
      expect_error(
        bootstrap(prune(scaled$a, threshold = 0.1), iter = 3),
        class = "tna_error_scaling"
      )
      TRUE
    },
    logical(1L)
  )
  # A scaled group saved before its networks were marked is still refused
  unmarked <- groups_of(scaling = "minmax")
  unmarked[] <- lapply(unmarked, function(y) {
    attr(y, "params")$group_scaling <- NULL
    y
  })
  expect_error(bootstrap(unmarked, iter = 3), class = "tna_error_scaling")
  expect_error(
    permutation_test(unmarked, iter = 3),
    class = "tna_error_scaling"
  )
  expect_error(
    estimate_cs(unmarked, measures = "InStrength", iter = 3),
    class = "tna_error_scaling"
  )
  # Unscaled groups and single scaled models are resampled as before
  plain <- groups_of()
  set.seed(1)
  expect_s3_class(bootstrap(plain, iter = 3), "group_tna_bootstrap")
  expect_null(attr(plain$a, "params")$group_scaling)
  single <- tna(group_regulation, scaling = "minmax")
  set.seed(2)
  expect_s3_class(bootstrap(single, iter = 3), "tna_bootstrap")
})

test_that("unused group factor levels do not become empty groups", {
  d <- head(group_regulation, 200)
  g <- factor(rep(c("X", "Y"), each = 100), levels = c("X", "Y", "Z"))
  model <- group_model(d, group = g)
  expect_named(model, c("X", "Y"))
  inits_finite <- vapply(model, function(m) all(is.finite(m$inits)),
                         logical(1L))
  expect_true(all(inits_finite))
  # Dropping the level changes nothing for the groups that remain
  same <- group_model(d, group = droplevels(g))
  expect_equal(model$X$weights, same$X$weights)
})

test_that("window parameters given for wide data are used in groups", {
  # Blocks {T1, T2} and {T3, T4}: each row gives A-C, A-D, B-C, B-D once.
  d <- data.frame(
    T1 = "A", T2 = "B", T3 = "C", T4 = "D",
    g = c("x", "x", "y"),
    stringsAsFactors = FALSE
  )
  model <- group_model(
    d, group = "g", type = "frequency", cols = T1:T4,
    params = list(window_size = 2L)
  )
  expect_equal(unname(model$x$weights["A", ]), c(0, 0, 2, 2))
  expect_equal(unname(model$y$weights["B", ]), c(0, 0, 1, 1))
  expect_equal(model$x$weights[["A", "B"]], 0)
  expect_true(attr(model$x, "params")$windowed)
})

test_that("groups window the same way as a model of each group", {
  set.seed(4)
  d <- as.data.frame(
    matrix(sample(c("A", "B", "C"), 200L, TRUE), 25L, 8L),
    stringsAsFactors = FALSE
  )
  states <- names(d)
  d$g <- rep(c("x", "y"), length.out = nrow(d))
  params <- list(window_size = 3L)
  ref <- function(fit, g) {
    fit(d[d$g == g, states], params = params)$weights
  }
  grouped <- group_tna(d, group = "g", cols = tidyselect::all_of(states),
                       params = params)
  expect_equal(unclass(grouped$x$weights), unclass(ref(tna, "x")),
               ignore_attr = TRUE)
  expect_equal(unclass(grouped$y$weights), unclass(ref(tna, "y")),
               ignore_attr = TRUE)
  co <- group_ctna(d, group = "g", cols = tidyselect::all_of(states),
                   params = params)
  expect_equal(unclass(co$x$weights), unclass(ref(ctna, "x")),
               ignore_attr = TRUE)
})

test_that("windows recorded by import_onehot are used in groups", {
  set.seed(5)
  d <- data.frame(
    a = rbinom(40L, 1L, 0.5), b = rbinom(40L, 1L, 0.5), c = rbinom(40L, 1L, 0.5)
  )
  onehot <- import_onehot(d, a:c, window_size = 2L)
  onehot$g <- rep(c("x", "y"), length.out = nrow(onehot))
  states <- setdiff(names(onehot), "g")
  attrs <- attributes(onehot)[c("windowed", "window_size", "window_span")]
  attributes(onehot)[names(attrs)] <- attrs
  grouped <- group_ftna(onehot, group = "g", cols = tidyselect::all_of(states))
  x <- onehot[onehot$g == "x", states]
  attributes(x)[names(attrs)] <- attrs
  expect_equal(unclass(grouped$x$weights), unclass(ftna(x)$weights),
               ignore_attr = TRUE)
})
