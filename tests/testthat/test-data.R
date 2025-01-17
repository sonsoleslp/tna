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
  expect_error(
    prepare_data(
      data,
      actor = "user",
      time = "time",
      action = "action",
      verbose = FALSE
    ),
    NA
  )
  expect_error(
    capture.output(
      prepare_data(
        data,
        actor = "user",
        time = "time",
        action = "action",
        verbose = FALSE
      )
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
  expect_error(
    prepare_data(
      data_ordered,
      actor = "user",
      order = "order",
      action = "action",
      verbose = FALSE
    ),
    NA
  )
  expect_error(
    capture.output(
      prepare_data(
        data_ordered,
        actor = "user",
        order = "order",
        action = "action",
        verbose = FALSE
      )
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
  expect_error(
    prepare_data(data_single_session, action = "action", verbose = FALSE),
    NA
  )
  expect_error(
    capture.output(
      prepare_data(data_single_session, action = "action", verbose = FALSE)
    ),
    NA
  )
})

