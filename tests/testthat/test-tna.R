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
# test_that("tna works with sequence data", {
#  seq_data <- TraMineR::seqdef(seqHMM::biofam3c$married)
#  tna_model <- tna(seq_data)
#
#  expect_s3_class(tna_model, "tna")
#  expect_true(is.list(tna_model$transits))
#  expect_true(is.list(tna_model$inits))
#  expect_true(is.character(tna_model$labels))
#  expect_true(is.character(tna_model$colors))
# })

# test_that("tna works with matrix data without inits", {
#   trans_matrix <- create_mock_matrix()
#   expect_error(
#     tna(trans_matrix),
#     "argument \"inits\" is missing, with no default")
# })

test_that("tna works with matrix data with inits", {
  inits <- c(0.25, 0.25, 0.25, 0.25)
  tna_model <- tna(mock_matrix, inits = inits)
  expect_s3_class(tna_model, "tna")
  expect_true(is.matrix(tna_model$weights))
  expect_true(is.vector(tna_model$inits))
})

test_that("tna fails with non-square matrix", {
  trans_matrix <- matrix(c(0.1, 0.2, 0.0, 0.0, 0.2, 0.3), nrow = 2, ncol = 3)
  expect_error(
    tna(trans_matrix),
    "Argument `x` must be a square <matrix>"
  )
})

test_that("tna fails with too few inits", {
  expect_error(
    tna(mock_matrix, inits = c(0.1, 0.2, 0.3)),
    "Argument `inits` must provide initial probabilities for all states."
  )
})

test_that("tna warns with too many inits", {
  expect_warning(
    tna(mock_matrix, inits = c(0.1, 0.2, 0.3, 0.4, 0.5)),
    paste0(
      "Argument `inits` contains more values than the number of states\\.\n",
      "i Only the first 4 values will be used\\."
    )
  )
})

test_that("tna handles missing x argument", {
  expect_error(tna(), "Argument `x` is missing.")
})

test_that("tna handles default case", {
  expect_error(build_model.default(mock_matrix), NA)
})

test_that("unnamed matrix gains dimnames", {
  mat <- mock_matrix
  dimnames(mat) <- NULL
  model <- tna(mat)
  expect_equal(
    dimnames(model$weights),
    list(as.character(1:4), as.character(1:4))
  )
})

test_that("tna aliases work", {
  expect_error(ftna(mock_freq_matrix), NA)
  expect_error(ctna(mock_sequence), NA)
})

test_that("scaling options work", {
  model_minmax <- tna(mock_freq_matrix, scaling = "minmax")
  expect_equal(
    range(model_minmax$weights),
    c(0, 1)
  )
  model_max <- tna(mock_freq_matrix, scaling = "max")
  expect_equal(
    model_max$weights,
    model_minmax$weights
  )
  model_rank <- tna(mock_matrix, scaling = "rank")
  expect_equal(
    sort(model_rank$weights),
    seq_len(prod(dim(mock_matrix)))
  )
})

test_that("model summary can be extracted", {
  model <- tna(mock_sequence)
  expect_error(
    summary(model),
    NA
  )
})

test_that("igraph conversion works", {
  model <- tna(mock_sequence)
  expect_error(
    as.igraph(model),
    NA
  )
})

test_that("igraph conversion works for clusters", {
  model <- group_tna(engagement_mmm)
  expect_error(
    as.igraph(model, which = 1),
    NA
  )
})
