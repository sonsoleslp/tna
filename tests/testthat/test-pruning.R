test_that("pruning works with user-specified threshold", {
  result <- prune(mock_tna, threshold = 0.1) |>
    attr("pruning")
  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed))
  expect_true(result$num_removed > 0)
  expect_equal(result$cut_off, 0.1)
  expect_equal(result$method, "threshold")
})

test_that("pruning works with lowest percent", {
  result <- prune(mock_tna, method = "lowest", lowest = 0.25) |>
    attr("pruning")
  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed))
  expect_true(result$num_removed > 0)
  expect_equal(result$method, "lowest")
})

test_that("pruning works with disparity filter", {
  result <- prune(mock_tna, method = "disparity", level = 0.5) |>
    attr("pruning")
  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed))
  expect_true(result$num_removed > 7)
  expect_equal(result$method, "disparity")
})

test_that("pruning works with bootstrap", {
  set.seed(0)
  tna_object <- tna(engagement)
  result <- prune(tna_object, method = "bootstrap", iter = 100) |>
    attr("pruning")
  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed))
  expect_true(result$num_removed > 0)
  expect_equal(result$method, "bootstrap")
})

test_that("pruning function ensures weak connectivity", {
  result <- prune(mock_tna, threshold = 0.2) |>
    attr("pruning")
  expect_true(is.list(result))
  expect_true(is.data.frame(result$removed))
  expect_true(sum(result$weights > 0) > 0)
  expect_true(is_weakly_connected(result$weights))
  expect_equal(result$method, "threshold")
})

test_that("pruning function fails with invalid tna object", {
  invalid_tna_object <- list()
  class(invalid_tna_object) <- "not_tna"
  expect_error(
    prune(invalid_tna_object, threshold = 0.1),
    "no applicable method for 'prune' applied to an object of class \"not_tna\""
  )
})

test_that("pruning details can be obtained", {
  expect_error(
    pruned_model <- prune(mock_tna, threshold = 0.2),
    NA
  )
  expect_error(
    out <- capture.output(pruning_details(pruned_model)),
    NA
  )
})

test_that("pruning can be deactivated", {
  pruned_model <- prune(mock_tna, threshold = 0.2)
  expect_error(
    deprune(pruned_model),
    NA
  )
})

test_that("pruning can be reactivated", {
  pruned_model <- prune(mock_tna, threshold = 0.2)
  depruned_model <- deprune(pruned_model)
  expect_error(
    reprune(depruned_model),
    NA
  )
})

test_that("weights are restored by deprune", {
  pruned_model <- prune(mock_tna, threshold = 0.2)
  depruned_model <- deprune(pruned_model)
  expect_equal(
    mock_tna$weights,
    depruned_model$weights
  )
})

test_that("pruned weights are restored by reprune", {
  pruned_model <- prune(mock_tna, threshold = 0.2)
  depruned_model <- deprune(pruned_model)
  repruned_model <- reprune(depruned_model)
  expect_equal(
    pruned_model$weights,
    repruned_model$weights
  )
})

test_that("pruning can be applied for clusters", {
  expect_error(
    prune(mmm_model, threshold = 0.3),
    NA
  )
})

test_that("pruning details can be obtained for clusters", {
  pruned_model <- prune(mmm_model, threshold = 0.3)
  expect_error(
    out <- capture.output(pruning_details(pruned_model)),
    NA
  )
})

test_that("pruning can be deactivated for clusters", {
  pruned_model <- prune(mmm_model, threshold = 0.3)
  expect_error(
    deprune(pruned_model),
    NA
  )
})

test_that("pruning can be reactivated for clusters", {
  pruned_model <- prune(mmm_model, threshold = 0.3)
  depruned_model <- deprune(pruned_model)
  expect_error(
    reprune(depruned_model),
    NA
  )
})

test_that("weights are restored by deprune for clusters", {
  pruned_model <- prune(mmm_model, threshold = 0.3)
  depruned_model <- deprune(pruned_model)
  expect_equal(
    lapply(mmm_model, "[[", "weights"),
    lapply(depruned_model, "[[", "weights")
  )
})

test_that("pruned weights are restored by reprune for clusters", {
  pruned_model <- prune(mmm_model, threshold = 0.3)
  depruned_model <- deprune(pruned_model)
  repruned_model <- reprune(depruned_model)
  expect_equal(
    lapply(pruned_model, "[[", "weights"),
    lapply(repruned_model, "[[", "weights")
  )
})
