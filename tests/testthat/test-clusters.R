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

test_that("group_atna returns correct type", {
  expect_error(
    atna_model <- group_atna(mock_sequence, group = c(1, 1, 2, 2)),
    NA
  )
  expect_equal(
    attr(atna_model[[1]], "type"),
    "attention"
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
  expect_error(
    summary(mmm_model),
    NA
  )
  expect_error(
    summary(mmm_model, combined = FALSE),
    NA
  )
})

test_that("grouped model can be constructed from tna_data objects", {
  data <- tibble::tibble(
    user = c("A", "A", "A", "B", "B", "C", "C", "C"),
    time = c(
      "2023-01-01 10:00:00", "2023-01-01 10:05:00",
      "2023-01-01 10:20:00", "2023-01-01 12:00:00",
      "2023-01-01 12:02:00", "2023-01-01 14:00:00",
      "2023-01-01 14:05:00", "2023-01-01 14:10:00"
    ),
    action = c(
      "view", "click", "add_cart", "view",
      "checkout", "view", "click", "share"
    ),
    group = c(rep("Group 1", 5), rep("Group 2", 3))
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  results <- prepare_data(
    data, actor = "user", time = "time", action = "action"
  )
  expect_error(
    model <- group_model(results, group = "group"),
    NA
  )
  expect_true(
    length(model) == 2L
  )
  expect_true(
    all(names(model) == c("Group 1", "Group 2"))
  )
})

test_that("missing values in group variable warns", {
  mock_tna_data_mis <- mock_tna_data
  mock_tna_data_mis$meta_data$group[4] <- NA
  expect_warning(
    model <- group_model(mock_tna_data_mis, group = "group"),
    "Column `group` contains missing values\\."
  )
})

test_that("groupwise models can be constructed with scaling", {
  expect_error(
    group_model(engagement_mmm, scaling = c("minmax", "rank", "max")),
    NA
  )
})
