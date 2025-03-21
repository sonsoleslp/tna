test_that("permutation test can be applied", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  expect_error(
    permutation_test(model_x, model_y, iter = 20),
    NA
  )
})

test_that("paired permutation test can be applied", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  expect_error(
    permutation_test(model_x, model_y, paired = TRUE, iter = 20),
    NA
  )
})

test_that("permutation test can be applied with groups", {
  model <- group_tna(engagement_mmm)
  expect_error(
    permutation_test(model, iter = 20),
    NA
  )
})

test_that("paired permutation fails when data are incomparable", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1300, ])
  model_z <- tna(engagement)
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
