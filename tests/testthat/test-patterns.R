# # Load necessary libraries
# library(testthat)
# library(tna)
#
# mock_tna_object <- tna(matrix(c(0, 0.5, 0.3, 0.2, 0.1, 0.9, 0.4, 0.6, 0),
#                                     nrow = 3, byrow = TRUE),inits=c(0.3, 0.4, 0.3))
# # Mock functions if needed
# clean_string <- function(x) { return(x) }
# compute_transition_matrix <- function(...) { return(runif(9)) }
# bootstrapper <- function(x, new_matrix, cluster) { return(x) }
#
# # Tests for `find_triads`
# test_that("find_triads detects triangles and returns expected structure", {
#   result <- find_triads(mock_tna_object, cluster = 1)
#
#   expect_type(result, "list")
#   expect_equal(names(result), c("count", "triangles"))
#   expect_true(result$count >= 0)
#   expect_type(result$triangles, "list")
# })
#
# # Tests for `find_dyads`
# test_that("find_dyads detects dyads and returns expected structure", {
#   result <- find_dyads(mock_tna_object, cluster = 1)
#
#   expect_type(result, "list")
#   expect_named(result)
# })
#
#
# TODO this file