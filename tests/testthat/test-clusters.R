test_that("group not found", {
    expect_error(
      group_model(),
      "Argument `x` is missing."
    )
})

test_that("tna is not group", {
  expect_error(
    group_model(mock_tna),
    "Argument `group` is missing."
  )
})

test_that("group missing", {
  expect_error(
    group_model(mock_tna),
    "Argument `group` is missing."
  )
})


test_that("group_model returns correct type", {
  expect_true(tna:::is_group_model(group_model(mock_sequence, group = c(1,1,2,2))))
})


test_that("group_model returns correct type", {
  expect_true(tna:::is_tna(group_model(mock_sequence, group = c(1,1,2,2))[[1]]))
})

