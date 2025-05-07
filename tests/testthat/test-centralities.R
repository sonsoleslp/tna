test_that("centralities computes correctly for a tna object", {
  result <- centralities(mock_tna)
  expect_s3_class(result, "tna_centralities")
  expect_true(all(names(centrality_funs) %in% colnames(result)))
})

test_that("centralities handles loops correctly in a tna object", {
  tna_model <- mock_tna
  result_tna <- centralities(tna_model, loops = FALSE)
  diag(tna_model$weights) <- 0
  result_manual <- centralities(tna_model, loops = TRUE)
  expect_equal(result_tna$OutStrength, result_manual$OutStrength)
  expect_equal(result_tna$InStrength, result_manual$InStrength)
})

test_that("centralities normalizes correctly for a tna object", {
  result_tna <- centralities(mock_tna, normalize = TRUE)
  result_manual <- centralities(mock_tna, normalize = FALSE)
  result_manual[, -1] <- apply(result_manual[, -1], 2, ranger)
  expect_equal(result_tna, result_manual)
})

test_that("centralities can be computed for a matrix", {
  expect_error(
    result_mat <- centralities(mock_matrix),
    NA
  )
})

test_that("centrality stability can be estimated", {
  model <- tna(mock_sequence)
  expect_error(
    estimate_cs(model, drop_prop = seq(0.3, 0.9, by = 0.1), iter = 20),
    NA
  )
})

test_that("centralities can be computed for clusters", {
  expect_error(
    centralities(mmm_model),
    NA
  )
})

test_that("centrality stability can be estimated for clusters", {
  expect_error(
    estimate_cs(
      mmm_model,
      drop_prop = seq(0.3, 0.9, by = 0.1),
      iter = 10
    ),
    NA
  )
})

test_that("progressbar works", {
  expect_error(
    capture.output(
      estimate_cs(
        mmm_model,
        drop_prop = seq(0.3, 0.9, by = 0.1),
        iter = 10,
        progressbar = TRUE
      )
    ),
    NA
  )
})

test_that("no cases dropped warns", {
  expect_warning(
    estimate_cs(
      mmm_model[[1]],
      drop_prop = 0.001,
      iter = 10
    ),
    "No cases dropped for proportion 0\\.001\\. Skipping\\.\\.\\."
  )
})

test_that("RSP is NA with no transitions", {
  mat <- mock_matrix
  mat[1,] <- 0
  expect_equal(rsp_bet(mat), NA)
})
