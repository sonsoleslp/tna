test_that("missing arguments fail", {
  expect_error(
    build_model.matrix(inits = 0L),
    "Argument `x` is missing"
  )
})

test_that("single element matrix fails", {
  expect_error(
    build_model.matrix(x = 0L),
    "Argument `x` must have at least two columns"
  )
})

test_that("non-square matrix fails", {
  expect_error(
    build_model.matrix(x = matrix(0, 3, 2)),
    "Argument `x` must be a square <matrix>"
  )
})

test_that("non-coercible arguments fail", {
  expect_error(
    tna(x = identity),
    "Argument `x` must be coercible to a <matrix>"
  )
})

# TODO internal sequence data
#test_that("tna works with sequence data", {
#  seq_data <- TraMineR::seqdef(seqHMM::biofam3c$married)
#  tna_model <- tna(seq_data)
#
#  expect_s3_class(tna_model, "tna")
#  expect_true(is.list(tna_model$transits))
#  expect_true(is.list(tna_model$inits))
#  expect_true(is.character(tna_model$labels))
#  expect_true(is.character(tna_model$colors))
#})

# test_that("tna works with matrix data without inits", {
#   trans_matrix <- create_mock_matrix()
#   expect_error(
#     tna(trans_matrix),
#     "argument \"inits\" is missing, with no default")
# })

test_that("tna works with matrix data with inits", {
  trans_matrix <- create_mock_matrix()
  inits <- c(0.25, 0.25, 0.25, 0.25)
  tna_model <- tna(trans_matrix, inits = inits)
  expect_s3_class(tna_model, "tna")
  expect_true(is.matrix(tna_model$weights))
  expect_true(is.vector(tna_model$inits))
  expect_equal(length(tna_model$inits), ncol(trans_matrix))
  expect_equal(tna_model$labels, colnames(trans_matrix))
})

test_that("tna fails with non-square matrix", {
  trans_matrix <- matrix(c(0.1, 0.2, 0.0, 0.0, 0.2, 0.3), nrow = 2, ncol = 3)
  expect_error(
    tna(trans_matrix),
    "Argument `x` must be a square <matrix>"
  )
})

test_that("tna fails with inits of wrong length", {
  trans_matrix <- create_mock_matrix()
  inits <- c(0.1, 0.2, 0.3)

  expect_error(
    tna(trans_matrix, inits = inits),
    "Argument `inits` must provide initial probabilities for all states."
  )
})

test_that("tna handles missing x argument", {
  expect_error(tna(), "Argument `x` is missing.")
})

