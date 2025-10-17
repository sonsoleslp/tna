# test_that("MMM can be fitted via EM", {
#   expect_error(
#     cluster_mmm(
#       engagement,
#       k = 3,
#       control = list(
#         maxiter = 10,
#         restarts = 2L
#       )
#     ),
#     NA
#   )
# })

# test_that("MMM can be fitted in parallel", {
#   expect_error(
#     suppressMessages(
#       cluster_mmm(
#         engagement,
#         k = 3,
#         parallel = TRUE,
#         n_cores = 2L,
#         control = list(
#           maxiter = 10L,
#           restarts = 2L
#         )
#       )
#     ),
#     NA
#   )
# })

# test_that("MMM can be fitted with multiple k values", {
#   expect_error(
#     suppressMessages(
#       cluster_mmm(
#         engagement,
#         k = 2:4,
#         parallel = TRUE,
#         n_cores = 2L,
#         control = list(
#           maxiter = 10L,
#           restarts = 2L
#         )
#       )
#     ),
#     NA
#   )
# })

# test_that("MMM can be fitted covariates", {
#   d <- engagement
#   d$x <- gl(2, 500)
#   expect_error(
#     suppressMessages(
#       cluster_mmm(
#         d,
#         cols = 1:20,
#         formula = ~ x,
#         k = 3,
#         parallel = TRUE,
#         n_cores = 2L,
#         control = list(
#           maxiter = 10L,
#           restarts = 2L
#         )
#       )
#     ),
#     NA
#   )
# })

# test_that("MMM fit can be summarized", {
#   expect_error(
#     summary(engagement_tna_mmm),
#     NA
#   )
# })

# test_that("variance-covariance matrix of a MMM can be obtained", {
#   expect_error(
#     vc <- vcov(engagement_tna_mmm),
#     NA
#   )
#   expect_error(
#     vcov(summary(engagement_tna_mmm)),
#     NA
#   )
#   expect_true(ncol(vc) == 2L)
#   expect_true(nrow(vc) == 2L)
#   expect_true(isSymmetric(vc))
# })

# test_that("coefficient estimates of a MMM can be obtained", {
#   expect_error(
#     cf <- coef(engagement_tna_mmm),
#     NA
#   )
#   expect_true(nrow(cf) == 1L)
#   expect_true(ncol(cf) == 3L)
# })

test_that("mixture Markov model statistics can be obtained", {
  expect_error(
    mmm_stats(engagement_mmm),
    NA
  )
  # expect_error(
  #   mmm_stats(engagement_tna_mmm),
  #   NA
  # )
})

# test_that("model fit failure is handled", {
#   expect_warning(
#     cluster_mmm(engagement, k = 5, control = list(restarts = 1, seed = 1)),
#     "All EM algorithm runs failed to converge\\."
#   )
# })
#
# test_that("model fit failure warns if only some values of k fail", {
#   expect_warning(
#     cluster_mmm(engagement, k = 4:5, control = list(restarts = 1, seed = 1)),
#     "Fitting the model with k = 5 failed\\."
#   )
# })

