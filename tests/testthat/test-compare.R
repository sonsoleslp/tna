test_that("models can be compared", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  # Comparing models
  expect_error(
    comp1 <- compare(model_x, model_y),
    NA
  )
  # Comparing matrices
  mat_x <- model_x$weights
  mat_y <- model_y$weights
  expect_error(
    comp2 <- compare(mat_x, mat_y),
    NA
  )
  # Comparing matric to a TNA model
  expect_error(
    comp3 <- compare(mat_x, model_y),
    NA
  )
})

test_that("clusters can be compared", {
  expect_error(
    comp <- compare(mmm_model, i = 1, j = 3),
    NA
  )
})

