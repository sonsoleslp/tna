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

test_that("centralities can be compared", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  # Comparing models
  expect_error(
    comp <- compare(
      model_x,
      model_y,
      measures = c("InStrength", "OutStrength")
    ),
    NA
  )
})

test_that("clusters can be compared", {
  expect_error(
    comp <- compare(mmm_model, i = 1, j = 3),
    NA
  )
})

test_that("sequences can be compared", {
  expect_error(
    compare_sequences(mock_sequence, group = c(1, 1, 2, 2, 2), min_freq = 1L),
    NA
  )
  expect_error(
    compare_sequences(mock_group_tna, min_freq = 1L),
    NA
  )
})

test_that("comparison fails when minimum frequency is not met", {
  expect_error(
    compare_sequences(mock_group_tna),
    "No common patterns with a frequency greater than 5 were found\\."
  )
})

test_that("patterns can be tested", {
  expect_error(
    compare_sequences(mock_group_tna, min_freq = 1L, test = TRUE, iter = 10),
    NA
  )
})
