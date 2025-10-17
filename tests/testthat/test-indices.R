test_that("sequence indices can be computed", {
  expect_error(
    sequence_indices(mock_sequence),
    NA
  )
})

test_that("sequence indices can be computed for tna models", {
  expect_error(
    sequence_indices(mock_tna_seq),
    NA
  )
})

test_that("favorable states can be specified", {
  expect_error(
    sequence_indices(mock_sequence, favorable = "A"),
    NA
  )
})
