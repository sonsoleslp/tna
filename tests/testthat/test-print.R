test_that("models can be printed", {
  expect_error(
    capture.output(print.tna(mock_tna)),
    NA
  )
})

test_that("bootstrap results can be printed", {
  model <- tna(mock_sequence)
  boot <- bootstrap(model, iter = 20)
  expect_error(
    capture.output(print.tna_bootstrap(boot)),
    NA
  )
})

test_that("bootstrap summary can be printed", {
  model <- tna(mock_sequence)
  boot <- bootstrap(model, iter = 20)
  summ <- summary.tna_bootstrap(boot)
  expect_error(
    capture.output(print.summary.tna_bootstrap(summ)),
    NA
  )
})

test_that("centralities can be printed", {
  cm <- centralities(mock_tna)
  expect_error(
    capture.output(print.tna_centralities(cm)),
    NA
  )
})

test_that("cliques can be printed", {
  cliq <- cliques(mock_tna, size = 2)
  expect_error(
    capture.output(print.tna_cliques(cliq)),
    NA
  )
})

test_that("zero clique case is considered", {
  cliq <- cliques(mock_tna, size = 2, threshold = 0.5)
  expect_output(
    print.tna_cliques(cliq),
    "No 2-cliques were found in the network\\."
  )
})

test_that("communities can be printed", {
  comm <- communities(mock_tna)
  expect_error(
    capture.output(print.tna_communities(comm)),
    NA
  )
})

test_that("centrality stability coefficients can be pritned", {
  model <- tna(mock_sequence)
  out <- estimate_cs(model, drop_prop = seq(0.3, 0.9, by = 0.2), iter = 10)
  expect_error(
    capture.output(print.tna_stability(out)),
    NA
  )
})

test_that("permutation test results can be printed", {
  model_x <- tna(group_regulation[1:100, ])
  model_y <- tna(group_regulation[1001:1200, ])
  perm <- permutation_test(
    model_x,
    model_y,
    measures = "InStrength", iter = 20)
  expect_error(
    capture.output(print.tna_permutation(perm)),
    NA
  )
})

test_that("model summary can be printed", {
  summ <- summary(mock_tna)
  expect_error(
    capture.output(print.summary.tna(summ)),
    NA
  )
})