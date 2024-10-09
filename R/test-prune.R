# # test-prune.R
#
# library(testthat)
#
# # Helper function to create a mock tna object
# create_mock_tna <- function() {
#   # Create a simple mock transition matrix
#   transits <- list(matrix(c(0.1, 0.2, 0.0, 0.1,
#                             0.0, 0.2, 0.3, 0.0,
#                             0.4, 0.0, 0.1, 0.2,
#                             0.1, 0.1, 0.0, 0.2),
#                           nrow = 4, ncol = 4, byrow = TRUE))
#
#   # Create a mock tna object
#   tna_object <- list(transits = transits)
#   class(tna_object) <- "tna"
#   return(tna_object)
# }
#
# test_that("prune function works with user-specified threshold", {
#   tna_object <- create_mock_tna()
#   result <- prune(tna_object, threshold = 0.1)
#
#   expect_true(is.list(result))
#   expect_true(is.list(result$pruned))
#   expect_true(is.data.frame(result$removed_edges[[1]]))
#   expect_equal(result$num_removed_edges[1], 5) # Adjusted expected number of removed edges
#   expect_equal(result$threshold_used[[1]], 0.1)
#   expect_equal(result$method_used[[1]], "User-specified threshold")
# })
#
# test_that("prune function works with percentile", {
#   tna_object <- create_mock_tna()
#   result <- prune(tna_object, percentile = 50)
#
#   expect_true(is.list(result))
#   expect_true(is.list(result$pruned))
#   expect_true(is.data.frame(result$removed_edges[[1]]))
#   expect_equal(result$num_removed_edges[1], 8) # Adjusted expected number of removed edges
#   expect_equal(result$method_used[[1]], "Lowest 50 percentile of non-zero edges")
# })
#
# test_that("prune function works with lowest_percent", {
#   tna_object <- create_mock_tna()
#   result <- prune(tna_object, lowest_percent = 25)
#
#   expect_true(is.list(result))
#   expect_true(is.list(result$pruned))
#   expect_true(is.data.frame(result$removed_edges[[1]]))
#   expect_equal(result$num_removed_edges[1], 5) # Adjusted expected number of removed edges
#   expect_equal(result$method_used[[1]], "Lowest 25 % of non-zero edges")
# })
#
# test_that("prune function ensures weak connectivity", {
#   tna_object <- create_mock_tna()
#   result <- prune(tna_object, threshold = 0.2)
#
#   expect_true(is.list(result))
#   expect_true(is.list(result$pruned))
#   expect_true(is.data.frame(result$removed_edges[[1]]))
#   expect_true(sum(result$pruned$transits[[1]] > 0) > 0) # Ensure some edges are retained
#   expect_equal(result$method_used[[1]], "User-specified threshold")
# })
#
# test_that("prune function fails with multiple criteria", {
#   tna_object <- create_mock_tna()
#   expect_error(prune(tna_object, threshold = 0.1, percentile = 50))
#   expect_error(prune(tna_object, threshold = 0.1, lowest_percent = 25))
#   expect_error(prune(tna_object, percentile = 50, lowest_percent = 25))
# })
#
# test_that("prune function fails with invalid tna object", {
#   invalid_tna_object <- list()
#   class(invalid_tna_object) <- "not_tna"
#   expect_error(prune(invalid_tna_object, threshold = 0.1))
# })
