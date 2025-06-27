core_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
n_cores <- ifelse_(
  nzchar(core_limit) && core_limit == "TRUE",
  2L,
  parallel::detectCores()
)

test_that("MMM can be fitted via EM", {
  expect_error(
    suppressMessages(cluster_mmm(engagement, k = 3)),
    NA
  )
})

test_that("MMM can be fitted in parallel", {
  expect_error(
    suppressMessages(
      cluster_mmm(engagement, k = 3, parallel = TRUE, n_cores = n_cores)
    ),
    NA
  )
})

test_that("MMM can be fitted with multiple k values", {
  expect_error(
    suppressMessages(
      cluster_mmm(engagement, k = 2:4, parallel = TRUE, n_cores = n_cores)
    ),
    NA
  )
})

test_that("MMM can be fitted covariates", {
  skip() # skip for now
  d <- engagement
  d$x <- gl(2, 500)
  expect_error(
    suppressMessages(
      cluster_mmm(
        d, cols = 1:20, formula = ~ x, k = 3, parallel = TRUE, n_cores = n_cores
      )
    ),
    NA
  )
})
