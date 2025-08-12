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

test_that("MMM fit can be summarized", {
  expect_error(
    summary(engagement_tna_mmm),
    NA
  )
})

test_that("Variance-covariance matrix of a MMM can be obtained", {
  expect_error(
    vcov(engagement_tna_mmm),
    NA
  )
  expect_error(
    vcov(summary(engagement_tna_mmm)),
    NA
  )
  vc <- vcov(engagement_tna_mmm)
  expect_true(ncol(vc) == 2L)
  expect_true(nrow(vc) == 2L)
  expect_true(isSymmetric(vc))
})

test_that("Coefficient estimates of a MMM can be obtained", {
  expect_error(
    coef(engagement_tna_mmm),
    NA
  )
  cf <- coef(engagement_tna_mmm)
  expect_true(nrow(cf) == 1L)
  expect_true(ncol(cf) == 3L)
})
