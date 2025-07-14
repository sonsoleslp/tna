test_that("missing argument fails", {
  f <- function(z) {
    check_missing(z)
  }
  expect_error(
    f(),
    "Argument `z` is missing\\."
  )
})

test_that("missing values fail", {
  f <- function(z) {
    check_na(z)
  }
  expect_error(
    f(c(1, 2, NA)),
    "Argument `z` must not contain missing values\\."
  )
})

test_that("invalid model type fails", {
  expect_error(
    check_model_type("unknown"),
    "Argument `type` must be either"
  )
})

test_that("invalid scaling options fail", {
  expect_error(
    check_model_scaling("unknown"),
    "Elements of `scaling` must be either"
  )
})

test_that("invalid class fails", {
  f <- function(z) {
    check_class(z, "test_class")
  }
  expect_error(
    f(z = 1),
    "Argument `z` must be a <test_class> object\\."
  )
})

test_that("non-sequence data tna fails", {
  f <- function(z) {
    check_tna_seq(z)
  }
  expect_error(
    f(z = mock_tna),
    "Argument `z` must be a <tna> object created from sequence data\\."
  )
})

test_that("invalid centrality measures fail", {
  expect_error(
    check_measures("unknown"),
    "Argument `measures` contains invalid centrality measures:"
  )
})

test_that("negative values fail", {
  opts <- expand.grid(
    type = c("integer", "numeric"),
    strict = c(FALSE, TRUE),
    scalar = c(FALSE, TRUE)
  )
  f <- function(i, z) {
    check_values(
      z,
      type = opts[i, 1],
      strict = opts[i, 2],
      scalar = opts[i, 3]
    )
  }
  expect_error(
    f(i = 1, z = -1),
    "Argument `z` must be a non-negative <integer> vector\\."
  )
  expect_error(
    f(i = 2, z = -1),
    "Argument `z` must be a non-negative <numeric> vector\\."
  )
  expect_error(
    f(i = 3, z = -1),
    "Argument `z` must be a positive <integer> vector\\."
  )
  expect_error(
    f(i = 4, z = -1),
    "Argument `z` must be a positive <numeric> vector\\."
  )
  expect_error(
    f(i = 5, z = -1),
    "Argument `z` must be a non-negative <integer>\\."
  )
  expect_error(
    f(i = 6, z = -1),
    "Argument `z` must be a non-negative <numeric> value\\."
  )
  expect_error(
    f(i = 7, z = -1),
    "Argument `z` must be a positive <integer>\\."
  )
  expect_error(
    f(i = 8, z = -1),
    "Argument `z` must be a positive <numeric> value\\."
  )
})

test_that("invalid range check fails", {
  opts <- expand.grid(
    type = c("integer", "numeric"),
    scalar = c(FALSE, TRUE)
  )
  f <- function(i, z) {
    check_range(
      z, type = opts[i, 1], scalar = opts[i, 2], lower = -2, upper = 2
    )
  }
  expect_error(
    f(i = 1, z = 3),
    "Argument `z` must only contain <integer> values between -2 and 2\\."
  )
  expect_error(
    f(i = 2, z = 4),
    "Argument `z` must only contain <numeric> values between -2 and 2\\."
  )
  expect_error(
    f(i = 3, z = -4),
    "Argument `z` must be a single <integer> between -2 and 2\\."
  )
  expect_error(
    f(i = 4, z = -3),
    "Argument `z` must be a single <numeric> value between -2 and 2\\."
  )
})

test_that("invalid logical fails", {
  f <- function(z) {
    check_flag(z)
  }
  expect_error(
    f(data.frame()),
    "Argument `z` must be a single <logical> value\\."
  )
})

test_that("invalid plotting layout fails", {
  expect_error(
    check_layout(mock_tna, "unknown"),
    "A <character> layout must be either \"circle\", \"groups\", \"spring\", or the name of an igraph layout\\."
  )
  expect_error(
    check_layout(mock_tna, matrix(0, 2, 1000)),
    "A <matrix> layout must have two columns:"
  )
  expect_error(
    check_layout(mock_tna, matrix(0, 1000, 2)),
    "A <matrix> layout must have exactly one row for each node"
  )
  expect_error(
    check_layout(mock_tna, data.frame()),
    "Argument `layout` must be a <character> string, a <matrix>, or a <function>\\."
  )
})

test_that("cluster check fails on invalid clusters", {
  expect_error(
    check_clusters(mmm_model, i = 1, j = 1),
    "Arguments `i` and `j` must be different\\."
  )
  expect_error(
    check_clusters(mmm_model, i = 1, j = 4),
    "Argument `j` must be between 1 and 3 when of type <numeric>\\."
  )
  expect_error(
    check_clusters(mmm_model, i = c(2, 3), j = 1),
    "Argument `i` must be a <numeric> or a <character> vector of length 1\\."
  )
  expect_error(
    check_clusters(mmm_model, i = 1, j = "Cluster 4"),
    "Argument `j` must be a name of `x` when of type <character>\\."
  )
})

test_that("range check variants are correct", {
  value <- 1
  expect_error(
    check_range(value, lower = 2),
    "Argument `value` must be a single <numeric> value greater than or equal to 2\\."
  )
  expect_error(
    check_range(value, upper = 0),
    "Argument `value` must be a single <numeric> value less than or equal to 0\\."
  )
  expect_error(
    check_range(value, lower = -1, upper = 0),
    "Argument `value` must be a single <numeric> value between -1 and 0\\."
  )
})
