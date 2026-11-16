test_that("bootstrap can be applied", {
  model <- tna(mock_sequence)
  expect_error(
    bootstrap(model, iter = 20, method = "stability"),
    NA
  )
  expect_error(
    bootstrap(model, iter = 20, method = "threshold"),
    NA
  )
})

test_that("bootstrap results can be summarized", {
  model <- tna(mock_sequence)
  boot <- bootstrap(model, iter = 20)
  expect_error(
    summary.tna_bootstrap(boot),
    NA
  )
})

test_that("bootstrap seed makes network resampling reproducible", {
  model <- tna(mock_sequence)
  first <- bootstrap(model, iter = 30, seed = 265)
  second <- bootstrap(model, iter = 30, seed = 265)
  set.seed(265)
  legacy <- bootstrap(model, iter = 30)

  expect_identical(first, second)
  expect_identical(first, legacy)
  expect_error(bootstrap(model, iter = 10, seed = TRUE), "numeric")
})

test_that("bootstrap can be applied for clusters", {
  expect_error(
    bootstrap(mmm_model, iter = 20),
    NA
  )
})

test_that("bootstrap results can be summarized for clusters", {
  boot <- bootstrap(mmm_model, iter = 10)
  expect_error(
    summary.group_tna_bootstrap(boot),
    NA
  )
})

test_that("bootstrap seed makes grouped resampling reproducible", {
  first <- bootstrap(mmm_model, iter = 10, seed = 265)
  second <- bootstrap(mmm_model, iter = 10, seed = 265)

  expect_identical(first, second)
})

test_that("cliques can be bootstrapped", {
  model <- tna(engagement)
  expect_error(
    bootstrap_cliques(model, iter = 10),
    NA
  )
})

test_that("bootstrap seed makes clique resampling reproducible", {
  model <- tna(engagement)
  first <- bootstrap_cliques(model, iter = 10, seed = 265)
  second <- bootstrap_cliques(model, iter = 10, seed = 265)

  expect_identical(first, second)
})

test_that("bootstrap_cliques resamples sequences, not clique indices", {
  d <- head(group_regulation, 300)
  model <- tna(d)
  iter <- 40L
  boot <- bootstrap_cliques(
    model, size = 2, threshold = 0.1, iter = iter, seed = 9
  )
  cl <- cliques(model, size = 2, threshold = 0.1, sum_weights = TRUE)
  # Oracle: redraw the same bootstrap samples of sequences by hand and
  # rebuild each network with tna()
  set.seed(9)
  n_seq <- nrow(d)
  draws <- vapply(
    seq_len(iter),
    function(i) {
      w <- tna(d[sample.int(n_seq, n_seq, replace = TRUE), ])$weights
      vapply(cl$indices, function(idx) mean(w[idx, idx]), numeric(1L))
    },
    numeric(length(cl$indices))
  )
  draws <- matrix(draws, nrow = length(cl$indices))
  expect_equal(boot$ci_lower, apply(draws, 1L, stats::quantile, 0.025),
               ignore_attr = TRUE)
  expect_equal(boot$ci_upper, apply(draws, 1L, stats::quantile, 0.975),
               ignore_attr = TRUE)
  # A resample of all sequences stays close to the observed clique weight
  expect_true(all(boot$mean_weight >= boot$ci_lower - 0.05))
  expect_true(all(boot$mean_weight <= boot$ci_upper + 0.05))
})
