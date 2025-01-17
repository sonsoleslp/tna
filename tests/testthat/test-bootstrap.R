test_that("bootstrap can be applied", {
  model <- tna(mock_sequence)
  expect_error(
    bootstrap(model, iter = 20, method = "stability"),
    NA
  )
  expect_error(
    bootstrap(model, iter = 20, method = "threshold"),
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

test_that("bootstrap can be applied for clusters", {
  expect_error(
    bootstrap(mmm_model, iter = 20),
    NA
  )
})

test_that("bootstrap results can be summarized for clusters", {
  boot <- bootstrap(mmm_model, iter = 10)
  expect_error(
    summary.group_tna_bootstrap(boot),
    NA
  )
})
