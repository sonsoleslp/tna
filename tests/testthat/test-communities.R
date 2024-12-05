test_that("communities can be detected", {
  expect_error(
    communities(mock_tna),
    NA
  )
})

test_that("communities can be detected for clusters", {
  expect_error(
    communities(mmm_model),
    NA
  )
})
