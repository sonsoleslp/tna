test_that("betweenness network can be constructed", {
  expect_error(
    bet_model <- betweenness_network(mock_tna),
    NA
  )
})
