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
