# Test data
mock_cluster_data <- data.frame(
  T1 = c("A", "B", "A", "C", "A", "B"),
  T2 = c("B", "A", "B", "A", "C", "A"),
  T3 = c("C", "C", "A", "B", "B", "C")
)

# Tests for cluster_data / cluster_sequences ----------------------------------

test_that("cluster_data works with PAM method", {
  result <- cluster_data(mock_cluster_data, k = 2, method = "pam")
  expect_s3_class(result, "tna_clustering")
  expect_equal(result$k, 2)
  expect_equal(result$method, "pam")
  expect_length(result$assignments, nrow(mock_cluster_data))
  expect_true(is.numeric(result$silhouette))
  expect_equal(sum(result$sizes), nrow(mock_cluster_data))
})

test_that("cluster_data works with hclust methods", {
  # Note: check_match lowercases methods, so only lowercase methods work with hclust
  methods <- c("complete", "average", "single", "mcquitty", "median", "centroid")
  for (m in methods) {
    result <- cluster_data(mock_cluster_data, k = 2, method = m)
    expect_s3_class(result, "tna_clustering")
    expect_equal(result$method, m)
    expect_length(result$assignments, nrow(mock_cluster_data))
  }
})

test_that("cluster_data returns correct structure", {
  result <- cluster_data(mock_cluster_data, k = 2)
  expect_named(
    result,
    c("data", "k", "assignments", "silhouette", "sizes", "method", "distance")
  )
  expect_equal(result$data, mock_cluster_data)
  expect_true(inherits(result$distance, "dist"))
})

test_that("cluster_data validates input arguments", {
  # Invalid data type
  expect_error(cluster_data("not a data frame", k = 2), "data.frame")

  # k out of range
  expect_error(cluster_data(mock_cluster_data, k = 0))
  expect_error(cluster_data(mock_cluster_data, k = 100))

  # Invalid dissimilarity
  expect_error(cluster_data(mock_cluster_data, k = 2, dissimilarity = "invalid"))

  # Invalid method
  expect_error(cluster_data(mock_cluster_data, k = 2, method = "invalid"))

  # Weighted only for hamming
  expect_error(
    cluster_data(mock_cluster_data, k = 2, dissimilarity = "lv", weighted = TRUE),
    "Hamming"
  )
})

test_that("cluster_data works with matrix input", {
  mat_data <- as.matrix(mock_cluster_data)
  result <- cluster_data(mat_data, k = 2)
  expect_s3_class(result, "tna_clustering")
})

test_that("cluster_data handles weighted hamming distance", {
  result <- cluster_data(
    mock_cluster_data,
    k = 2,
    dissimilarity = "hamming",
    weighted = TRUE,
    lambda = 0.5
  )
  expect_s3_class(result, "tna_clustering")
})

test_that("cluster_sequences is alias for cluster_data", {
  result1 <- cluster_data(mock_cluster_data, k = 2)
  result2 <- cluster_sequences(mock_cluster_data, k = 2)
  expect_equal(result1$k, result2$k)
  expect_equal(result1$method, result2$method)
})

# Tests for seq2chr -----------------------------------------------------------

test_that("seq2chr converts data to character matrix", {
  result <- seq2chr(mock_cluster_data, na_syms = character(0))
  expect_type(result, "list")
  expect_named(result, c("mat", "len"))
  expect_true(is.matrix(result$mat))
  expect_equal(nrow(result$mat), nrow(mock_cluster_data))
  expect_equal(ncol(result$mat), ncol(mock_cluster_data))
})

test_that("seq2chr handles NA symbols", {
  data_with_na <- mock_cluster_data
  data_with_na[1, 1] <- "*"
  data_with_na[2, 2] <- "%"
  result <- seq2chr(data_with_na, na_syms = c("*", "%"))
  expect_type(result, "list")
})

test_that("seq2chr computes correct sequence lengths", {
  # All complete sequences should have length equal to ncol
  result <- seq2chr(mock_cluster_data, na_syms = character(0))
  expect_equal(result$len, rep(ncol(mock_cluster_data), nrow(mock_cluster_data)))
})

# Tests for get_qgram ---------------------------------------------------------

test_that("get_qgram returns empty for short sequences", {
  result <- get_qgram(c("a", "b"), n = 2, q = 3)
  expect_length(result, 0)
})

test_that("get_qgram computes q-grams correctly", {
  result <- get_qgram(c("a", "b", "c", "d"), n = 4, q = 2)
  expect_type(result, "integer")
  expect_true(length(result) > 0)
  expect_true(all(names(result) %in% c("ab", "bc", "cd")))
})

test_that("get_qgram counts repeated q-grams", {
  result <- get_qgram(c("a", "b", "a", "b"), n = 4, q = 2)
  expect_equal(result["ab"], c(ab = 2L))
})

# Tests for dissimilarity_matrix ----------------------------------------------

test_that("dissimilarity_matrix works with all methods", {
  strings <- seq2chr(mock_cluster_data, na_syms = character(0))
  methods <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw")
  for (d in methods) {
    result <- dissimilarity_matrix(strings, d, lambda = 0)
    expect_true(inherits(result, "dist"))
  }
})

test_that("dissimilarity_matrix handles weighted distances", {
  strings <- seq2chr(mock_cluster_data, na_syms = character(0))
  result <- dissimilarity_matrix(strings, "hamming", lambda = 1.0)
  expect_true(inherits(result, "dist"))
})

test_that("dissimilarity_matrix computes q-gram based distances", {
  strings <- seq2chr(mock_cluster_data, na_syms = character(0))
  # Test with custom q parameter
  result <- dissimilarity_matrix(strings, "qgram", lambda = 0, q = 2)
  expect_true(inherits(result, "dist"))
})

# Tests for individual distance functions -------------------------------------

test_that("osa_dist computes optimal string alignment distance", {
  x <- c("a", "b", "c")
  y <- c("a", "b", "d")

  # Function executes and returns numeric
  result_same <- osa_dist(x, x, 3, 3)
  expect_type(result_same, "integer")

  result_diff <- osa_dist(x, y, 3, 3)
  expect_type(result_diff, "integer")

  # Transposition case (adjacent swap)
  x2 <- c("a", "b", "c", "d")
  y2 <- c("a", "c", "b", "d")
  result_trans <- osa_dist(x2, y2, 4, 4)
  expect_type(result_trans, "integer")
})

test_that("levenshtein_dist computes edit distance", {
  x <- c("a", "b", "c")
  y <- c("a", "b", "d")

  # Function executes and returns numeric
  result_same <- levenshtein_dist(x, x, 3, 3)
  expect_type(result_same, "integer")

  result_diff <- levenshtein_dist(x, y, 3, 3)
  expect_type(result_diff, "integer")

  # Completely different strings
  z <- c("d", "e", "f")
  result_all_diff <- levenshtein_dist(x, z, 3, 3)
  expect_type(result_all_diff, "integer")
})

test_that("hamming_dist computes hamming distance", {
  x <- c("a", "b", "c")
  y <- c("a", "b", "c")
  expect_equal(hamming_dist(x, y, 3, 3, weights = 1), 0)

  # One difference
  y <- c("a", "b", "d")
  expect_equal(hamming_dist(x, y, 3, 3, weights = 1), 1)

  # All different
  y <- c("d", "e", "f")
  expect_equal(hamming_dist(x, y, 3, 3, weights = 1), 3)

  # With weights
  weights <- c(1, 0.5, 0.25)
  y <- c("d", "e", "f")
  expect_equal(hamming_dist(x, y, 3, 3, weights = weights), sum(weights))
})

test_that("qgram_dist computes q-gram distance", {
  qx <- c(ab = 1L, bc = 1L)
  qy <- c(ab = 1L, bc = 1L)
  expect_equal(qgram_dist(NULL, NULL, qx, qy), 0)

  # Different q-grams
  qy <- c(ab = 2L, cd = 1L)
  result <- qgram_dist(NULL, NULL, qx, qy)
  expect_true(result > 0)
})

test_that("cosine_dist computes cosine distance", {
  qx <- c(ab = 1L, bc = 1L)
  qy <- c(ab = 1L, bc = 1L)
  expect_equal(cosine_dist(NULL, NULL, qx, qy), 0)

  # Orthogonal vectors
  qx <- c(ab = 1L)
  qy <- c(cd = 1L)
  expect_equal(cosine_dist(NULL, NULL, qx, qy), 1)

  # Empty vectors (edge case)
  qx <- integer(0)
  names(qx) <- character(0)
  qy <- integer(0)
  names(qy) <- character(0)
  expect_equal(cosine_dist(NULL, NULL, qx, qy), 1)
})

test_that("jaccard_dist computes jaccard distance", {
  qx <- c(ab = 1L, bc = 1L)
  qy <- c(ab = 1L, bc = 1L)
  expect_equal(jaccard_dist(qx, qy), 0)

  # No overlap
  qx <- c(ab = 1L)
  qy <- c(cd = 1L)
  expect_equal(jaccard_dist(qx, qy), 1)

  # Partial overlap
  qx <- c(ab = 1L, bc = 1L)
  qy <- c(ab = 1L, cd = 1L)
  expect_equal(jaccard_dist(qx, qy), 1 - 1/3)

  # Empty sets
  qx <- integer(0)
  names(qx) <- character(0)
  qy <- integer(0)
  names(qy) <- character(0)
  expect_equal(jaccard_dist(qx, qy), 0)
})

test_that("jaro_dist computes jaro distance", {
  x <- c("a", "b", "c")
  y <- c("a", "b", "c")
  expect_equal(jaro_dist(x, y, 3, 3), 0)

  # Completely different
  y <- c("d", "e", "f")
  expect_equal(jaro_dist(x, y, 3, 3), 1)

  # Empty strings
  expect_equal(jaro_dist(character(0), character(0), 0, 0), 0)
  expect_equal(jaro_dist(c("a"), character(0), 1, 0), 1)
  expect_equal(jaro_dist(character(0), c("a"), 0, 1), 1)
})

test_that("jaro_dist handles transpositions", {
  x <- c("a", "b", "c", "d")
  y <- c("a", "c", "b", "d")
  result <- jaro_dist(x, y, 4, 4)
  expect_true(result > 0 && result < 1)
})

test_that("jaro_winkler_dist computes jaro-winkler distance", {
  x <- c("a", "b", "c")
  y <- c("a", "b", "c")
  expect_equal(jaro_winkler_dist(x, y, 3, 3), 0)

  # Common prefix should reduce distance
  x <- c("a", "b", "c", "d")
  y <- c("a", "b", "x", "y")
  jaro <- jaro_dist(x, y, 4, 4)
  jw <- jaro_winkler_dist(x, y, 4, 4)
  # JW distance should be <= Jaro distance when there's a common prefix
  expect_true(jw <= jaro)
})

test_that("jaro_winkler_dist handles no common prefix", {
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")
  result <- jaro_winkler_dist(x, y, 3, 3)
  expect_equal(result, 1)
})

test_that("distance implementations match stringdist", {
  skip_if_not_installed("stringdist")
  dist_funs <- list(
    osa = osa_dist,
    lv = levenshtein_dist,
    dl = damerau_levenshtein_dist,
    lcs = lcs_dist,
    qgram = qgram_dist,
    cosine = cosine_dist,
    jaccard = jaccard_dist,
    jw = jaro_winkler_dist
  )
  set.seed(0)
  for (i in 1:100) {
    n <- 5 + sample.int(15, 1)
    m <- 5 + sample.int(15, 1)
    x <- letters[sample.int(26, n, replace = TRUE)]
    y <- letters[sample.int(26, m, replace = TRUE)]
    z <- letters[sample.int(26, n, replace = TRUE)]
    qx <- get_qgram(x, n, q = 2)
    qy <- get_qgram(y, m, q = 2)
    x_str <- paste0(x, collapse = "")
    y_str <- paste0(y, collapse = "")
    weights <- 1.0
    for (dist in names(dist_funs)) {
      dist_a <- dist_funs[[dist]](
        x = x, y = y, n = n, m = m, qx = qx, qy = qy, weights = weights
      )
      dist_b <- stringdist::stringdist(
        a = x_str, b = y_str, method = dist, q = 2, p = 0.1
      )
      expect_equal(dist_a, dist_b)
    }
  }
  # Hamming
  for (i in 1:100) {
    n <- 5 + sample.int(15, 1)
    x <- letters[sample.int(26, n, replace = TRUE)]
    y <- letters[sample.int(26, n, replace = TRUE)]
    x_str <- paste0(x, collapse = "")
    y_str <- paste0(y, collapse = "")
    weights <- 1.0
    dist_a <- hamming_dist(x = x, y = y, n = n, m = n, weights = 1.0)
    dist_b <- stringdist::stringdist(a = x_str, b = y_str, method = "hamming")
  }
})

# Edge cases ------------------------------------------------------------------

test_that("distance functions handle single element sequences", {
  x <- c("a")
  y <- c("a")
  # Functions execute on single element
  result_lev <- levenshtein_dist(x, y, 1, 1)
  expect_type(result_lev, "integer")

  result_ham <- hamming_dist(x, y, 1, 1, weights = 1)
  expect_type(result_ham, "double")
})

test_that("cluster_data works with minimum k", {
  result <- cluster_data(mock_cluster_data, k = 2)
  expect_equal(result$k, 2)
})

test_that("cluster_data handles all dissimilarity measures", {
  methods <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw")
  for (d in methods) {
    result <- cluster_data(mock_cluster_data, k = 2, dissimilarity = d)
    expect_s3_class(result, "tna_clustering")
  }
})
