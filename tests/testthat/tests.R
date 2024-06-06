test_that("missing arguments fail", {
  expect_error(
    build_tna.matrix(inits = 0L),
    "Argument `x` is missing"
  )
})

test_that("single element matrix fails", {
  expect_error(
    build_tna.matrix(x = 0L),
    "Argument `x` must have at least two columns"
  )
})

test_that("non-square matrix fails", {
  expect_error(
    build_tna.matrix(x = matrix(0, 3, 2)),
    "Argument `x` must be a square <matrix>"
  )
})

test_that("non-coercible arguments fail", {
  expect_error(
    build_tna(x = identity),
    "Argument `x` must be coercible to a <matrix>"
  )
})
