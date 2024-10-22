test_that("prune function works with user-specified threshold", {
  tna_object <- create_mock_tna()
  result <- prune(tna_object, threshold = 0.1) |> attr("pruning")

  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed_edges[[1]]))
  expect_equal(result$num_removed_edges[1], 5)
  expect_equal(result$cut_offs[1], 0.1)
  expect_equal(result$method[1], "threshold")
})

test_that("prune function works with lowest percent", {
  tna_object <- create_mock_tna()
  result <- prune(tna_object, method = "lowest", lowest = 0.25) |>
    attr("pruning")

  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed_edges[[1]]))
  expect_equal(result$num_removed_edges[1], 5)
  expect_equal(result$method[1], "lowest")
})

test_that("prune function works with disparity filter", {
  tna_object <- create_mock_tna()
  result <- prune(tna_object, method = "disparity", alpha = 0.5) |>
    attr("pruning")

  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed_edges[[1]]))
  expect_equal(result$num_removed_edges[1], 7)
  expect_equal(result$method[1], "disparity")
})

test_that("prune function ensures weak connectivity", {
  tna_object <- create_mock_tna()
  result <- prune(tna_object, threshold = 0.2) |> attr("pruning")

  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed_edges[[1]]))
  expect_true(sum(result$transits[[1]] > 0) > 0)
  expect_true(is_weakly_connected(result$transits[[1]]))
  expect_equal(result$method[1], "threshold")
})


test_that("prune function fails with invalid tna object", {
  invalid_tna_object <- list()
  class(invalid_tna_object) <- "not_tna"
  expect_error(prune(invalid_tna_object, threshold = 0.1))
})

