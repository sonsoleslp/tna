test_that("permutation test can be applied", {
  model_x <- tna(mock_sequence[1:2, ])
  model_y <- tna(mock_sequence[3:5, ])
  expect_error(
    permutation_test(model_x, model_y, iter = 20),
    NA
  )
})

test_that("paired permutation test can be applied", {
  model_x <- tna(mock_sequence[1:2, ])
  model_y <- tna(mock_sequence[4:5, ])
  expect_error(
    permutation_test(model_x, model_y, paired = TRUE, iter = 20),
    NA
  )
})

test_that("permutation test can be applied with groups", {
  expect_error(
    permutation_test(mmm_model, iter = 20),
    NA
  )
})

test_that("permutation test p-values can be adjusted", {
  expect_error(
    permutation_test(mmm_model, iter = 20, adjust = "holm"),
    NA
  )
  expect_error(
    permutation_test(mmm_model, iter = 20, adjust = "holm", adjust_pairwise = FALSE),
    NA
  )
})

test_that("paired permutation fails when data are incomparable", {
  model_x <- tna(mock_sequence[1:4, ])
  model_y <- tna(mock_sequence[3:5, ])
  model_z <- tna(mock_sequence[c(1, 4, 5), 1:2])
  model_w <- model_z
  attr(model_w$data, "alphabet") <- rev(attr(model_w$data, "alphabet"))
  expect_error(
    permutation_test(model_x, model_y, paired = TRUE),
    "The number of observations must be the same in `x` and `y` for a paired test\\."
  )
  expect_error(
    permutation_test(model_x, model_z),
    "The number of states of `x` and `y` must be the same\\."
  )
  expect_error(
    permutation_test(model_z, model_w),
    "The state labels of `x` and `y` must be the same and in the same order\\."
  )
})

test_that("permutation test p-values can be adjusted", {
  model_x <- tna(mock_sequence[1:2, ])
  model_y <- tna(mock_sequence[3:5, ])
  for (method in stats::p.adjust.methods) {
    expect_error(
      permutation_test(model_x, model_y, adjust = method),
      NA
    )
  }
})

test_that("permutation test pairs can be made consecutive", {
  model <- group_model(mock_sequence, group = c(1, 1, 2, 3, 3))
  expect_error(
    out <- permutation_test(model, consecutive = TRUE, iter = 20),
    NA
  )
  expect_true(length(out) == 2)
})