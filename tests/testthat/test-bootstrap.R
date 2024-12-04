test_that("bootstrap can be applied", {
  model <- tna(mock_sequence)
  expect_error(
    boot <- bootstrap(model, iter = 20),
    NA
  )
})

test_that("bootstrap results can be summarized", {
  model <- tna(mock_sequence)
  boot <- bootstrap(model, iter = 20)
  expect_error(
    summary.tna_bootstrap(boot),
    NA
  )
})
