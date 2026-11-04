test_that("models can be compared", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  # Comparing models
  expect_error(
    comp1 <- compare(model_x, model_y),
    NA
  )
  # Comparing matrices
  mat_x <- model_x$weights
  mat_y <- model_y$weights
  expect_error(
    comp2 <- compare(mat_x, mat_y),
    NA
  )
  # Comparing matric to a TNA model
  expect_error(
    comp3 <- compare(mat_x, model_y),
    NA
  )
})

test_that("centralities can be compared", {
  model_x <- tna(group_regulation[1:200, ])
  model_y <- tna(group_regulation[1001:1200, ])
  # Comparing models
  expect_error(
    comp <- compare(
      model_x,
      model_y,
      measures = c("InStrength", "OutStrength")
    ),
    NA
  )
})

test_that("clusters can be compared", {
  expect_error(
    comp <- compare(mmm_model, i = 1, j = 3),
    NA
  )
})

test_that("sequences can be compared", {
  expect_error(
    compare_sequences(mock_sequence, group = c(1, 1, 2, 2, 2), min_freq = 1L),
    NA
  )
  expect_error(
    compare_sequences(mock_group_tna, min_freq = 1L),
    NA
  )
})

test_that("comparison fails when minimum frequency is not met", {
  expect_error(
    compare_sequences(mock_group_tna),
    "No common patterns with a frequency greater than 5 were found\\."
  )
})

test_that("patterns can be tested", {
  expect_error(
    compare_sequences(mock_group_tna, min_freq = 1L, test = TRUE, iter = 10),
    NA
  )
})

test_that("compare computes Canberra with one-sided zero edges", {
  st <- c("A", "B", "C")
  x <- matrix(c(0.5, 0.5, 0, 0.2, 0.3, 0.5, 0, 0, 1), 3, byrow = TRUE,
              dimnames = list(st, st))
  y <- matrix(c(0.5, 0, 0.5, 0.2, 0.3, 0.5, 0.4, 0, 0.6), 3, byrow = TRUE,
              dimnames = list(st, st))
  cmp <- compare(x, y, network = FALSE)
  canberra <- subset(cmp$summary_metrics, metric == "Canberra")$value
  # Oracle: sum |x - y| / (|x| + |y|) over all cells, 0 / 0 counted as 0.
  # Cells (1,2), (1,3), (3,1) are one-sided zeros and contribute 1 each;
  # (3,3) contributes 0.4 / 1.6; the others 0.
  expect_equal(canberra, 3 + 0.4 / 1.6)
})

test_that("compare reports the distance correlation, not its square", {
  st <- c("A", "B")
  x <- matrix(c(0.1, 0.4, 0.2, 0.9), 2, dimnames = list(st, st))
  y <- matrix(c(0.2, 0.1, 0.5, 0.7), 2, dimnames = list(st, st))
  # Independent double-centering implementation (Szekely et al., 2007)
  dcor_oracle <- function(u, v) {
    a <- abs(outer(u, u, "-"))
    b <- abs(outer(v, v, "-"))
    a <- a - outer(rowMeans(a), colMeans(a), "+") + mean(a)
    b <- b - outer(rowMeans(b), colMeans(b), "+") + mean(b)
    sqrt(mean(a * b) / sqrt(mean(a^2) * mean(b^2)))
  }
  cmp <- compare(x, y, network = FALSE)
  dcor <- subset(cmp$summary_metrics, metric == "Distance")$value
  expect_equal(dcor, dcor_oracle(as.vector(x), as.vector(y)))
  # Reference value from energy::dcor() 1.7.12
  u <- c(0.1, 0.4, 0.2, 0.9, 0.5, 0.3)
  v <- c(0.2, 0.1, 0.5, 0.7, 0.6, 0.2)
  expect_equal(distance_correlation(u, v), 0.723593732938501,
               tolerance = 1e-10)
  # Zero distance variance gives zero, as energy::dcor() does
  expect_equal(distance_correlation(rep(1, 4), 1:4), 0)
})

test_that("compare matches states by name", {
  st <- c("A", "B", "C")
  x <- matrix(c(0.5, 0.5, 0, 0.2, 0.3, 0.5, 0, 0, 1), 3, byrow = TRUE,
              dimnames = list(st, st))
  y <- matrix(c(0.5, 0, 0.5, 0.2, 0.3, 0.5, 0.4, 0, 0.6), 3, byrow = TRUE,
              dimnames = list(st, st))
  perm <- c("C", "A", "B")
  # The same network with its states listed in another order is identical
  same <- compare(x, x[perm, perm], network = FALSE)
  expect_equal(
    subset(same$summary_metrics, metric == "Mean Abs. Diff.")$value, 0
  )
  # Reordering y changes nothing (permutation invariance)
  cmp <- compare(x, y, network = FALSE, measures = "InStrength")
  cmp_perm <- compare(x, y[perm, perm], network = FALSE,
                      measures = "InStrength")
  expect_equal(cmp_perm$summary_metrics, cmp$summary_metrics)
  expect_equal(cmp_perm$edge_metrics, cmp$edge_metrics)
  expect_equal(cmp_perm$centrality_differences, cmp$centrality_differences)
})

test_that("compare rejects different or unnamed states", {
  d1 <- data.frame(T1 = c("A", "B", "C"), T2 = c("B", "C", "A"))
  d2 <- data.frame(T1 = c("A", "B", "D"), T2 = c("B", "D", "A"))
  expect_error(compare(tna(d1), tna(d2)), class = "tna_error_states")
  unnamed <- matrix(c(0.6, 0.3, 0.4, 0.7), 2, 2)
  expect_error(compare(unnamed, unnamed), class = "tna_error_states")
})
