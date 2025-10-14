test_that("clustering methods can be applied", {
  expect_error(
    cluster_sequences(mock_cluster_data, k = 2, method = "pam"),
    NA
  )
  expect_error(
    cluster_sequences(mock_cluster_data, k = 2, method = "complete"),
    NA
  )
})

test_that("clustering with weighted hamming distance can be applied", {
  expect_error(
    cluster_sequences(
      mock_cluster_data,
      k = 2,
      weighted = TRUE,
      dissimilarity = "hamming"
    ),
    NA
  )
})