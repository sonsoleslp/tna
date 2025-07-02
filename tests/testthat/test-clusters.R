test_that("MMM can be fitted via EM", {
  expect_error(
    suppressMessages(cluster_mmm(engagement, k = 3)),
    NA
  )
})

test_that("MMM can be fitted in parallel", {
  expect_error(
    suppressMessages(
      cluster_mmm(
        engagement,
        k = 3,
        parallel = TRUE,
        n_cores = 2L,
        n_starts = 2,
        reltol = 1e-6
      )
    ),
    NA
  )
})

test_that("MMM can be fitted with multiple k values", {
  expect_error(
    suppressMessages(
      cluster_mmm(
        engagement,
        k = 2:4,
        parallel = TRUE,
        n_cores = 2L,
        n_starts = 2,
        reltol = 1e-4
      )
    ),
    NA
  )
})

test_that("MMM can be fitted covariates", {
  d <- engagement
  d$x <- gl(2, 500)
  expect_error(
    suppressMessages(
      cluster_mmm(
        d,
        cols = 1:20,
        formula = ~ x,
        k = 3,
        parallel = TRUE,
        n_cores = 2L,
        n_starts = 2L,
        reltol = 1e-6
      )
    ),
    NA
  )
})
