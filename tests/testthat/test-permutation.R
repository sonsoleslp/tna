test_that("permutation test can be applied", {
  model_x <- tna(group_regulation[1:100,])
  model_y <- tna(group_regulation[101:200,])
  expect_error(
    permutation_test(model_x, model_y, iter = 50),
    NA
  )
})