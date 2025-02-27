test_that("model not found fails", {
  expect_error(
    group_model(),
    "Argument `x` is missing\\."
  )
})

test_that("no group fails", {
  expect_error(
    group_model(mock_tna),
    "Argument `group` is missing\\."
  )
})

test_that("cluster check fails on invalid clusters", {
  model <- group_tna(engagement_mmm)
  expect_error(
    check_clusters(model, i = 1, j = 1),
    "Arguments `i` and `j` must be different\\."
  )
  expect_error(
    check_clusters(model, i = 1, j = 4),
    "Argument `j` must be between 1 and 3 when of type <numeric>\\."
  )
  expect_error(
    check_clusters(model, i = c(2, 3), j = 1),
    "Argument `i` must be a <numeric> or a <character> vector of length 1\\."
  )
  expect_error(
    check_clusters(model, i = 1, j = "Cluster 4"),
    "Argument `j` must be a name of `x` when of type <character>\\."
  )
})


test_that("unsupported date format fails", {
  time <- rep("2025#02#02", 5)
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    parse_time(
      time,
      custom_format = NULL,
      is_unix_time = FALSE,
      unix_time_unit = "secs"
    ),
    "Could not parse time values"
  )
})

test_that("pruning function fails with invalid tna object", {
  invalid_tna_object <- list()
  class(invalid_tna_object) <- "not_tna"
  expect_error(
    prune(invalid_tna_object, threshold = 0.1),
    "no applicable method for 'prune' applied to an object of class \"not_tna\""
  )
})

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

