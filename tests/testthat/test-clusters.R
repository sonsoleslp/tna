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

test_that("all dissimilarity measures are supported", {
  strings <- seq2chr(mock_cluster_data, na_syms = character(0))
  methods <- c(
    "osa",
    "lv",
    "dl",
    "hamming",
    "lcs",
    "qgram",
    "cosine",
    "jaccard",
    "jw"
  )
  for (d in methods) {
    expect_error(
      dissimilarity_matrix(strings, d, lambda = 0),
      NA
    )
  }
})
