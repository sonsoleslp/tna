test_that("transition networks can be simulated", {
  set.seed(0)
  # Special case with only one sequence
  expect_error(
    simulate(mock_tna, max_len = 10),
    NA
  )
  expect_error(
    simulate(mock_tna, nsim = 10, max_len = 10),
    NA
  )
})

test_that("missing values can be included", {
  set.seed(0)
  sim <- simulate(mock_tna, nsim = 100, max_len = 10, na_range = c(1, 3))
  nas <- apply(sim, 1L, function(y) sum(is.na(y)))
  expect_true(all(nas %in% c(1, 2, 3)))
})

test_that("transition matrix is recovered", {
  set.seed(0)
  sim <- simulate(mock_tna, nsim = 1e4, max_len = 100)
  tna_sim <- tna(sim)
  expect_equal(mock_tna$weights, tna_sim$weights, tolerance = 0.01)
})
