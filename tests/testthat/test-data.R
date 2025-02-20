test_that("data preparation works when actor and time are provided", {
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
    )
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    prepare_data(
      data,
      actor = "user",
      time = "time",
      action = "action"
    ),
    NA
  )
})

test_that("data preparation works when actor and order are provided", {
  data_ordered <- tibble::tibble(
    user = c("A", "A", "A", "B", "B", "C", "C", "C"),
    order = c(1, 2, 3, 1, 2, 1, 2, 3),
    action = c(
      "view", "click", "add_cart", "view",
      "checkout", "view", "click", "share"
    )
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    prepare_data(
      data_ordered,
      actor = "user",
      order = "order",
      action = "action"
    ),
    NA
  )
})

test_that("data preparation works when only action is provided", {
  data_single_session <- tibble::tibble(
    action = c(
      "view", "click", "add_cart", "view", "checkout", "view", "click", "share"
    )
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    prepare_data(data_single_session, action = "action"),
    NA
  )
})

test_that("unix time from character column works", {
  mock_long_unix <- mock_long
  mock_long_unix$time <- as.POSIXct("2023-01-01 00:00:00") |>
    as.numeric() |>
    as.character()
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    data_out <- prepare_data(
      mock_long_unix,
      time = "time",
      actor = "group",
      action = "event"
    ),
    NA
  )
})

test_that("unsupported date format fails", {
  time <- rep("2025#02#02", 5)
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    parse_time(
      time,
      custom_format = NULL,
      is_unix_time = FALSE,
      unix_time_unit = "secs"
    ),
    "Could not parse time values"
  )
})
