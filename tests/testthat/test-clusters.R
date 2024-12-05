test_that("model not found fails", {
  expect_error(
    group_model(),
    "Argument `x` is missing\\."
  )
})

test_that("no group fails", {
  expect_error(
    group_model(mock_tna),
    "Argument `group` is missing\\."
  )
})

test_that("group_model returns correct type", {
  expect_true(
    inherits(
      group_model(mock_sequence, group = c(1, 1, 2, 2)),
      "group_tna"
    )
  )
})

test_that("group_ftna returns correct type", {
  expect_error(
    ftna_model <- group_ftna(mock_sequence, group = c(1, 1, 2, 2)),
    NA
  )
  expect_equal(
    attr(ftna_model[[1]], "type"),
    "frequency"
  )
})

test_that("group_ctna returns correct type", {
  expect_error(
    ctna_model <- group_ctna(mock_sequence, group = c(1, 1, 2, 2)),
    NA
  )
  expect_equal(
    attr(ctna_model[[1]], "type"),
    "co-occurrence"
  )
})

test_that("group_model returns correct type", {
  expect_true(
    inherits(
      group_model(mock_sequence, group = c(1, 1, 2, 2))[[1]],
      "tna"
    )
  )
})

test_that("mixed Markov model statistics can be obtained", {
  expect_error(
    mmm_stats(engagement_mmm),
    NA
  )
  expect_error(
    mmm_stats(engagement_mmm, use_t_dist = FALSE),
    NA
  )
})

test_that("groups can be renamed", {
  model <- group_model(mock_sequence, group = c(1, 1, 2, 2))
  expect_error(
    model <- rename_groups(model, c("A", "B")),
    NA
  )
  expect_equal(model$A, model[[1]])
  expect_equal(model$B, model[[2]])
})

test_that("group model can be summarized", {
  model <- group_model(engagement_mmm)
  expect_error(
    summary(model),
    NA
  )
})
