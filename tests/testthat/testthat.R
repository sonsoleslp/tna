# test-build_tna.R

library(testthat)
library(tna)
library(seqHMM, quietly = T)


# Helper function to create a mock transition matrix
create_mock_matrix <- function() {
  matrix(c(0.8, 0.2, 0.0, 0.1,
           0.0, 0.2, 0.3, 0.0,
           0.4, 0.0, 0.1, 0.2,
           0.1, 0.1, 0.0, 0.2),
         nrow = 4, ncol = 4, byrow = TRUE,
         dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D")))
}

test_that("build_tna works with sequence data", {
  data(biofam3c)
  seq_data <- seqdef(biofam3c$married)
  tna_model <- build_tna(seq_data)

  expect_s3_class(tna_model, "tna")
  expect_true(is.list(tna_model$transits))
  expect_true(is.list(tna_model$inits))
  expect_true(is.character(tna_model$labels))
  expect_true(is.character(tna_model$colors))
})

test_that("build_tna works with matrix data without inits", {
  trans_matrix <- create_mock_matrix()
  expect_error(build_tna(trans_matrix),"argument \"inits\" is missing, with no default")
})

test_that("build_tna works with matrix data with inits", {
  trans_matrix <- create_mock_matrix()
  inits <- c(0.25, 0.25, 0.25, 0.25)
  tna_model <- build_tna(trans_matrix, inits)

  expect_s3_class(tna_model, "tna")
  expect_true(is.list(tna_model$transits))
  expect_true(is.list(tna_model$inits))
  expect_equal(length(tna_model$inits[[1]]), ncol(trans_matrix))
  expect_equal(tna_model$labels, colnames(trans_matrix))
})

test_that("build_tna fails with non-square matrix", {
  trans_matrix <- matrix(c(0.1, 0.2, 0.0,
                           0.0, 0.2, 0.3),
                         nrow = 2, ncol = 3)
  expect_error(build_tna(trans_matrix), "Argument `x` must be a square <matrix>")
})

test_that("build_tna fails with inits of wrong length", {
  trans_matrix <- create_mock_matrix()
  inits <- c(0.1, 0.2, 0.3)
  expect_error(build_tna(trans_matrix, inits), "Argument `inits` must provide initial probabilities for all states.")
})

test_that("build_tna handles missing x argument", {
  expect_error(build_tna(), "Argument `x` is missing.")
})


