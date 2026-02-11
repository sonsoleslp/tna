# test_that("sequence indices can be computed", {
#   expect_error(
#     sequence_indices(mock_sequence),
#     NA
#   )
#   expect_error(
#     sequence_indices(engagement),
#     NA
#   )
# })
# 
# test_that("sequence indices can be computed for tna models", {
#   expect_error(
#     sequence_indices(mock_tna_seq),
#     NA
#   )
# })
# 
# test_that("sequence indices can be computed for group tna models", {
#   expect_error(
#     sequence_indices(mock_group_tna),
#     NA
#   )
# })
# 
# test_that("favorable states can be specified", {
#   expect_error(
#     sequence_indices(mock_sequence, favorable = "A"),
#     NA
#   )
# })
