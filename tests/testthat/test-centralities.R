test_that("centralities computes correctly for a tna object", {
  tna_model <- create_mock_tna()
  result <- centralities(tna_model)
  expect_s3_class(result, "tna_centralities")
  expect_true(all(available_centrality_measures %in% colnames(result)))
})

test_that("centralities handles loops correctly in a tna object", {
  tna_model <- create_mock_tna()
  result_tna <- centralities(tna_model, loops = FALSE)
  diag(tna_model$weights) <- 0
  result_manual <- centralities(tna_model, loops = TRUE)
  expect_equal(result_tna$OutStrength, result_manual$OutStrength)
  expect_equal(result_tna$InStrength, result_manual$InStrength)
})

test_that("centralities normalizes correctly for a tna object", {
  tna_model <- create_mock_tna()
  result_tna <- centralities(tna_model, normalize = TRUE)
  result_manual <- centralities(tna_model, normalize = FALSE)
  result_manual[, -1] <- apply(result_manual[, -1], 2, ranger)
  expect_equal(result_tna, result_manual)
})
