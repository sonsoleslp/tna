# Tests for mmm_stats generic function
test_that("mmm_stats is a generic function", {
  expect_true(is.function(mmm_stats))
  methods_info <- methods("mmm_stats")
  expect_true("mmm_stats.mhmm" %in% methods_info)
})

# Tests for mmm_stats.mhmm method
test_that("mmm_stats.mhmm works with engagement_mmm data", {
  skip_if_not_installed("seqHMM")
  result <- mmm_stats(engagement_mmm)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_named(
    result,
    c(
      "cluster", "variable", "estimate", "std_error",
      "ci_lower", "ci_upper", "z_value", "p_value"
    )
  )
})

test_that("mmm_stats.mhmm returns correct column types", {
  skip_if_not_installed("seqHMM")
  result <- mmm_stats(engagement_mmm)
  expect_type(result$cluster, "character")
  expect_type(result$variable, "character")
  expect_type(result$estimate, "double")
  expect_type(result$std_error, "double")
  expect_type(result$ci_lower, "double")
  expect_type(result$ci_upper, "double")
  expect_type(result$z_value, "double")
  expect_type(result$p_value, "double")
})

test_that("mmm_stats.mhmm respects custom level parameter", {
  skip_if_not_installed("seqHMM")
  result_default <- mmm_stats(engagement_mmm, level = 0.05)
  result_narrow <- mmm_stats(engagement_mmm, level = 0.01)

  # Narrower CI (lower level) should have wider margins
  ci_width_default <- result_default$ci_upper - result_default$ci_lower
  ci_width_narrow <- result_narrow$ci_upper - result_narrow$ci_lower

  # 99% CI should be wider than 95% CI

  expect_true(all(ci_width_narrow >= ci_width_default - 1e-10))
})

test_that("mmm_stats.mhmm validates level parameter", {
  skip_if_not_installed("seqHMM")
  expect_error(mmm_stats(engagement_mmm, level = -0.1))
  expect_error(mmm_stats(engagement_mmm, level = 1.5))
})

test_that("mmm_stats.mhmm rejects non-mhmm objects", {
  skip_if_not_installed("seqHMM")
  expect_error(mmm_stats.mhmm(list(a = 1)), "mhmm")
  expect_error(mmm_stats.mhmm(data.frame(x = 1)), "mhmm")
})

test_that("mmm_stats.mhmm returns p-values between 0 and 1", {
  skip_if_not_installed("seqHMM")
  result <- mmm_stats(engagement_mmm)
  expect_true(all(result$p_value >= 0))
  expect_true(all(result$p_value <= 1))
})

test_that("mmm_stats.mhmm returns confidence intervals containing estimate", {
  skip_if_not_installed("seqHMM")
  result <- mmm_stats(engagement_mmm)
  expect_true(all(result$ci_lower <= result$estimate))
  expect_true(all(result$ci_upper >= result$estimate))
})

test_that("mmm_stats.mhmm returns positive standard errors", {
  skip_if_not_installed("seqHMM")
  result <- mmm_stats(engagement_mmm)
  expect_true(all(result$std_error >= 0))
})

# Tests for mmm_stats_ internal helper function
test_that("mmm_stats_ computes statistics correctly", {
  # Create mock data
  cf <- matrix(
    c(0, 1, 2, 0, 0.5, 1.5),
    nrow = 2,
    ncol = 3,
    dimnames = list(c("(Intercept)", "x"), c("ref", "Cluster 2", "Cluster 3"))
  )
  vc <- diag(c(0.01, 0.04, 0.01, 0.04))

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # 2 vars * 2 clusters (excluding ref)
  expect_named(
    result,
    c(
      "cluster", "variable", "estimate", "std_error",
      "ci_lower", "ci_upper", "z_value", "p_value"
    )
  )
})

test_that("mmm_stats_ computes z-values correctly", {
  cf <- matrix(
    c(0, 0, 0, 2),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("(Intercept)", "x"), c("ref", "Cluster 2"))
  )
  vc <- diag(c(0.25, 0.25))

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  # z = estimate / std_error = 2 / 0.5 = 4
  expect_equal(result$z_value[2], 4)
})

test_that("mmm_stats_ computes p-values correctly", {
  cf <- matrix(
    c(0, 0, 0, 0),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("(Intercept)", "x"), c("ref", "Cluster 2"))
  )
  vc <- diag(c(1, 1))

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  # When z = 0, p-value should be 1 (two-tailed)
  expect_equal(result$p_value, c(1, 1))
})

test_that("mmm_stats_ computes confidence intervals correctly", {
  cf <- matrix(
    c(0, 0, 0, 1),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("(Intercept)", "x"), c("ref", "Cluster 2"))
  )
  vc <- diag(c(1, 1))

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  # 95% CI: estimate +/- 1.96 * se
  # For estimate = 1, se = 1: CI = [1 - 1.96, 1 + 1.96]
  expected_margin <- qnorm(0.975) * 1
  expect_equal(result$ci_lower[2], 1 - expected_margin)
  expect_equal(result$ci_upper[2], 1 + expected_margin)
})

test_that("mmm_stats_ handles different level values", {
  cf <- matrix(
    c(0, 0, 0, 1),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("(Intercept)", "x"), c("ref", "Cluster 2"))
  )
  vc <- diag(c(1, 1))

  result_90 <- tna:::mmm_stats_(cf, vc, level = 0.10)
  result_99 <- tna:::mmm_stats_(cf, vc, level = 0.01)

  # 90% CI should be narrower than 99% CI
  width_90 <- result_90$ci_upper[2] - result_90$ci_lower[2]
  width_99 <- result_99$ci_upper[2] - result_99$ci_lower[2]
  expect_true(width_90 < width_99)
})

test_that("mmm_stats_ preserves cluster and variable names", {
  cf <- matrix(
    c(0, 0, 0, 1, 2, 3),
    nrow = 3,
    ncol = 2,
    dimnames = list(c("(Intercept)", "age", "gender"), c("ref", "Active"))
  )
  vc <- diag(rep(1, 3))

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  expect_equal(unique(result$cluster), "Active")
  expect_equal(result$variable, c("(Intercept)", "age", "gender"))
})

test_that("mmm_stats_ handles multiple clusters", {
  cf <- matrix(
    c(0, 0, 1, 2, 3, 4),
    nrow = 2,
    ncol = 3,
    dimnames = list(c("(Intercept)", "x"), c("ref", "Cluster A", "Cluster B"))
  )
  vc <- diag(rep(1, 4))

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  expect_equal(nrow(result), 4) # 2 vars * 2 clusters
  expect_equal(result$cluster, c("Cluster A", "Cluster A", "Cluster B", "Cluster B"))
  expect_equal(result$variable, c("(Intercept)", "x", "(Intercept)", "x"))
})

test_that("mmm_stats_ returns default row names", {
  cf <- matrix(
    c(0, 0, 0, 1),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("(Intercept)", "x"), c("ref", "Cluster 2"))
  )
  vc <- diag(c(1, 1))

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  # Row names are reset to default sequential integers
  expect_equal(rownames(result), as.character(seq_len(nrow(result))))
})

# Edge case tests
test_that("mmm_stats.mhmm works with edge level values",
  {
  skip_if_not_installed("seqHMM")

  # Level close to 0 (very wide CI)
  result_wide <- mmm_stats(engagement_mmm, level = 0.001)
  expect_s3_class(result_wide, "data.frame")

  # Level close to 1 (very narrow CI)
  result_narrow <- mmm_stats(engagement_mmm, level = 0.999)
  expect_s3_class(result_narrow, "data.frame")
})

test_that("mmm_stats_ handles single variable correctly", {
  cf <- matrix(
    c(0, 1),
    nrow = 1,
    ncol = 2,
    dimnames = list("(Intercept)", c("ref", "Cluster 2"))
  )
  vc <- diag(1)

  result <- tna:::mmm_stats_(cf, vc, level = 0.05)

  expect_equal(nrow(result), 1)
  expect_equal(result$variable, "(Intercept)")
  expect_equal(result$cluster, "Cluster 2")
})
