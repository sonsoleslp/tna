test_that("reliability can be analyzed", {
  expect_error(
    reliability(mock_tna_seq, iter = 50),
    NA
  )
  expect_error(
    reliability(mock_tna_seq, types = c("relative", "frequency"), iter = 50),
    NA
  )
})
