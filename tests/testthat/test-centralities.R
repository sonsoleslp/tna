library(testthat)
library(igraph)
library(tna)

# Helper function to create a sample transition matrix
create_sample_tna <- function() {
  mat <- matrix(c(
    0, 1, 2, 0,
    0, 0, 1, 1,
    1, 0, 0, 1,
    0, 1, 0, 0
  ), nrow = 4, byrow = TRUE)

  build_tna(mat, inits = c(0.3,0.2,0.3,0.2))
}

# Test centralities with a tna object
test_that("centralities computes correctly for a tna object", {
  tna_model <- create_sample_tna()

  result <- centralities(tna_model)

  expect_s3_class(result, "centralities")
  expect_true(all(c("OutStrength", "InStrength", "ClosenessIn", "ClosenessOut", "Closeness", "Betweenness", "Diffusion", "Clustering") %in% colnames(result)))
})

# Test centralities with loops parameter for tna object
test_that("centralities handles loops correctly in a tna object", {
  tna_model <- create_sample_tna()

  result_with_loops <- centralities(tna_model, loops = TRUE)
  result_without_loops <- centralities(tna_model, loops = FALSE)

  expect_false(!all(result_with_loops$OutStrength == result_without_loops$OutStrength))
})

# Test centralities with normalization for tna object
test_that("centralities normalizes correctly for a tna object", {
  tna_model <- create_sample_tna()

  result_normalized <- centralities(tna_model, normalize = TRUE)
  result_non_normalized <- centralities(tna_model, normalize = FALSE)

  expect_true(!all(result_normalized$OutStrength == result_non_normalized$OutStrength))
})

# Test centralities for a specific cluster in tna object
test_that("centralities computes correctly for a specific cluster in a tna object", {
  tna_model <- create_sample_tna()

  result_cluster1 <- centralities(tna_model, cluster = 1)

  expect_s3_class(result_cluster1, "centralities")
  expect_true(all(c("OutStrength", "InStrength", "ClosenessIn", "ClosenessOut", "Closeness", "Betweenness", "Diffusion", "Clustering") %in% colnames(result_cluster1)))
})

# Test centralities for all clusters in tna object
test_that("centralities computes correctly for all clusters in a tna object", {
  tna_model <- create_sample_tna()

  result_all_clusters <- centralities(tna_model)

  expect_s3_class(result_all_clusters, "centralities")
  expect_true("State" %in% colnames(result_all_clusters))
})
