test_that("communities can be detected", {
  expect_error(
    comm <- communities(mock_tna),
    NA
  )
})