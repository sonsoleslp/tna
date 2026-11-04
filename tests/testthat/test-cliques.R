test_that("cliques of different sizes can be found", {
  model <- tna(group_regulation)
  dyads <- cliques(model, size = 2)
  triads <- cliques(model, size = 3)
  expect_length(dyads$weights, 35)
  expect_length(triads$weights, 77)
})

test_that("cliques of can be found with sum of weights", {
  model <- tna(group_regulation)
  dyads <- cliques(model, size = 2, sum_weights = TRUE)
  triads <- cliques(model, size = 3, sum_weights = TRUE)
  expect_length(dyads$weights, 36)
  expect_length(triads$weights, 84)
})

test_that("cliques can be found for clusters", {
  expect_error(
    cliques(mmm_model, size = 2),
    NA
  )
})

test_that("summed clique threshold applies to the sum of both directions", {
  w <- matrix(c(0, 3, 3, 0), 2, dimnames = list(c("A", "B"), c("A", "B")))
  model <- ftna(w, inits = c(0.5, 0.5))
  expect_length(
    cliques(model, size = 2, threshold = 5, sum_weights = TRUE)$indices, 1L
  )
  expect_length(
    cliques(model, size = 2, threshold = 7, sum_weights = TRUE)$indices, 0L
  )
  expect_length(cliques(model, size = 2, threshold = 5)$indices, 0L)
  # Oracle: a pair is a clique when w[i, j] + w[j, i] >= threshold
  model <- tna(group_regulation)
  summed <- model$weights + t(model$weights)
  n_pairs <- sum(summed[upper.tri(summed)] >= 0.3)
  expect_length(
    cliques(model, size = 2, threshold = 0.3, sum_weights = TRUE)$indices,
    n_pairs
  )
})
