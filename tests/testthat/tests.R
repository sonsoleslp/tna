# test_that("missing arguments fail", {
#   expect_error(
#     build_tna.matrix(x = 0L),
#     "Argument `inits` is missing"
#   )
#   expect_error(
#     build_tna.matrix(inits = 0L),
#     "Argument `x` is missing"
#   )
# })

test_that("non-tidygraph or non-stslist tna input fails", {
  expect_error(
    build_tna(0L),
    "no applicable method for 'build_tna'"
  )
})

