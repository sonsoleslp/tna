test_that("models can be printed", {
  expect_error(
    capture.output(print.tna(mock_tna)),
    NA
  )
  expect_error(
    capture.output(print.tna(mock_tna, generic = TRUE)),
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

test_that("bootstrap results can be printed", {
  set.seed(0)
  model <- tna(group_regulation)
  boot <- bootstrap(model, iter = 50)
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

test_that("centrality stability coefficients can be printed", {
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
    measures = "InStrength", iter = 20
  )
  expect_error(
    capture.output(print.tna_permutation(perm)),
    NA
  )
})

test_that("group model can be printed", {
  expect_error(
    capture.output(print.group_tna(mmm_model)),
    NA
  )
})

test_that("group model summary can be printed", {
  summ <- summary(mmm_model)
  expect_error(
    capture.output(print.summary.group_tna(summ)),
    NA
  )
})

test_that("bootstrap results can be printed for clusters", {
  boot <- bootstrap(mmm_model, iter = 10)
  expect_error(
    capture.output(print.group_tna_bootstrap(boot)),
    NA
  )
})

test_that("bootstrap summary can be printed for clusters", {
  boot <- bootstrap(mmm_model, iter = 10)
  summ <- summary.group_tna_bootstrap(boot)
  expect_error(
    capture.output(print.summary.group_tna_bootstrap(summ)),
    NA
  )
})

test_that("centralities can be printed for clusters", {
  cm <- centralities(mmm_model)
  expect_error(
    capture.output(print.group_tna_centralities(cm)),
    NA
  )
})

test_that("cliques can be printed for clusters", {
  cliq <- cliques(mmm_model, size = 2)
  expect_error(
    capture.output(print.group_tna_cliques(cliq)),
    NA
  )
})

test_that("communities can be printed", {
  comm <- communities(mmm_model)
  expect_error(
    capture.output(print.group_tna_communities(comm)),
    NA
  )
})

test_that("centrality stability coefficients can be printed for clusters", {
  out <- estimate_cs(mmm_model, drop_prop = seq(0.3, 0.9, by = 0.2), iter = 10)
  expect_error(
    capture.output(print.group_tna_stability(out)),
    NA
  )
})

test_that("tna_data objects can be printed", {
  data_single_session <- tibble::tibble(
    action = c(
      "view", "click", "add_cart", "view", "checkout", "view", "click", "share"
    )
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  data_out <- prepare_data(data_single_session, action = "action")
  expect_error(
    capture.output(print(data_out, data = "sequence")),
    NA
  )
  expect_error(
    capture.output(print(data_out, data = "meta")),
    NA
  )
  expect_error(
    capture.output(print(data_out, data = "long")),
    NA
  )
})

test_that("comparison results can be printed", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  # Comparing models
  comp <- compare(model_x, model_y)
  expect_error(
    capture.output(print(comp)),
    NA
  )
})

test_that("permutation test results can be printed for clusters", {
  perm <- permutation_test(mmm_model, iter = 50)
  expect_error(
    capture.output(print(perm)),
    NA
  )
})
