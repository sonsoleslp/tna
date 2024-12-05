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
