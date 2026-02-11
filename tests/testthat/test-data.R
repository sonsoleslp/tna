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

test_that("data preparation works when actor and order are provided", {
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
  data_unarranged <- dplyr::arrange(data, action)
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    prepare_data(
      data,
      actor = "user",
      order = "time",
      action = "action"
    ),
    NA
  )
  expect_equal(
    prepare_data(
      data,
      actor = "user",
      order = "time",
      action = "action"
    ),
    prepare_data(
      data_unarranged,
      actor = "user",
      order = "time",
      action = "action"
    )
  )
})

test_that("data preparation for multiple actors is supported", {
  data_multi_actor <- tibble::tibble(
    user = c("A", "A", "A", "A", "B", "B", "B", "B"),
    session = c(1, 1, 2, 2, 1, 1, 2, 2),
    action = c(
     "view", "click", "add_cart", "view",
     "checkout", "view", "click", "share"
    )
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    prepare_data(
      data_multi_actor, actor = c("user", "session"), action = "action"
    ),
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

test_that("datetime is unaffected", {
  time <- rep(Sys.time(), 5)
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_equal(
    time,
    parse_time(
      time,
      custom_format = NULL,
      is_unix_time = FALSE,
      unix_time_unit = "secs"
    )
  )
})

test_that("missing values informs", {
  time <- as.character(rep(Sys.time(), 5))
  time[c(4, 5)] <- NA
  out <- utils::capture.output(
    utils::capture.output(
      parse_time(
        time,
        custom_format = NULL,
        is_unix_time = FALSE,
        unix_time_unit = "secs"
      ),
      type = "message"
    ),
    type = "output"
  )
  out <- paste0(out, collapse = "")
  expect_true(
    grepl("Found missing or empty time values", out),
  )
})

test_that("parsing with custom time format works", {
  time_raw <- "27---2---2025"
  fmt <- "%d---%m---%Y"
  time <- as.POSIXct(strptime(time_raw, format = fmt))
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_equal(
    time,
    parse_time(
      time_raw,
      custom_format = fmt,
      is_unix_time = FALSE,
      unix_time_unit = "secs"
    )
  )
})

test_that("wide format sequence data can be imported", {
  expect_error(
    long_data <- import_data(
      data = mock_sequence_wide,
      cols = c(feature1, feature2),
      id_cols = c("ID", "Time"),
      window_size = 2,
      replace_zeros = TRUE
    ),
    NA
  )
  expect_equal(
    names(long_data),
    c("feature3", "other_col", "ID", "Time", "action", "value", "order")
  )
})

test_that("one-hot data can be imported", {
  d <- data.frame(
    actor = gl(100, 5),
    session = gl(10, 50),
    feature1 = rbinom(500, 1, prob = 0.33),
    feature2 = rbinom(500, 1, prob = 0.25),
    feature3 = rbinom(500, 1, prob = 0.50)
  )
  expect_error(
    import_onehot(d, feature1:feature3),
    NA
  )
  expect_error(
    import_onehot(d, feature1:feature3, window_size = 5),
    NA
  )
  expect_error(
    import_onehot(d, feature1:feature3, actor = "actor"),
    NA
  )
  expect_error(
    import_onehot(d, feature1:feature3, session = "session"),
    NA
  )
  expect_error(
    import_onehot(d, feature1:feature3, actor = "actor", session = "session"),
    NA
  )
})

test_that("valid column selection works", {
  expect_error(
    cols_1 <- get_cols(rlang::quo(T1:T3), mock_sequence),
    NA
  )
  expect_error(
    cols_2 <- get_cols(rlang::quo(c("T1", "T2", "T3")), mock_sequence),
    NA
  )
  expect_error(
    cols_3 <- get_cols(rlang::quo(1:3), mock_sequence),
    NA
  )
  expect_identical(cols_1, cols_2)
  expect_identical(cols_2, cols_3)
  expect_identical(
    get_cols(rlang::quo(1), mock_sequence),
    "T1"
  )
  expect_identical(
    get_cols(rlang::quo("T1"), mock_sequence),
    "T1"
  )
})

test_that("invalid column selection fails", {
  expect_error(
    get_cols(rlang::quo(TRUE), mock_sequence),
    "Columns must be selected using a tidy selection"
  )
  expect_error(
    get_cols(rlang::quo(1i), mock_sequence),
    "Columns must be selected using a tidy selection"
  )
  expect_error(
    get_cols(rlang::quo("T7"), mock_sequence),
    "Can't select columns that don't exist"
  )
  expect_error(
    get_cols(rlang::quo(7), mock_sequence),
    "Can't select columns that don't exist"
  )
})

# Tests for import_onehot with sliding window
test_that("import_onehot works with sliding window type", {
  d <- data.frame(
    actor = gl(10, 10),
    session = gl(5, 20),
    feature1 = rbinom(100, 1, prob = 0.33),
    feature2 = rbinom(100, 1, prob = 0.25),
    feature3 = rbinom(100, 1, prob = 0.50)
  )
  expect_error(
    result <- import_onehot(
      d,
      feature1:feature3,
      actor = "actor",
      session = "session",
      window_size = 3,
      window_type = "sliding"
    ),
    NA
  )
  expect_s3_class(result, "data.frame")
})

test_that("import_onehot works with aggregate = TRUE", {
  d <- data.frame(
    actor = gl(10, 10),
    session = gl(5, 20),
    feature1 = rbinom(100, 1, prob = 0.33),
    feature2 = rbinom(100, 1, prob = 0.25),
    feature3 = rbinom(100, 1, prob = 0.50)
  )
  expect_error(
    result <- import_onehot(
      d,
      feature1:feature3,
      actor = "actor",
      session = "session",
      window_size = 2,
      aggregate = TRUE
    ),
    NA
  )
  expect_s3_class(result, "data.frame")
})

test_that("import_onehot works with sliding window and aggregate", {
  d <- data.frame(
    actor = gl(10, 10),
    session = gl(5, 20),
    feature1 = rbinom(100, 1, prob = 0.33),
    feature2 = rbinom(100, 1, prob = 0.25),
    feature3 = rbinom(100, 1, prob = 0.50)
  )
  expect_error(
    result <- import_onehot(
      d,
      feature1:feature3,
      window_size = 2,
      window_type = "sliding",
      aggregate = TRUE
    ),
    NA
  )
  expect_s3_class(result, "data.frame")
})

test_that("parse_time handles milliseconds unix time", {
  time <- c(1609459200000, 1609459260000, 1609459320000)
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- tna:::parse_time(
    time,
    custom_format = NULL,
    is_unix_time = TRUE,
    unix_time_unit = "milliseconds"
  )
  expect_s3_class(result, "POSIXct")
})

test_that("parse_time handles microseconds unix time", {
  time <- c(1609459200000000, 1609459260000000, 1609459320000000)
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- tna:::parse_time(
    time,
    custom_format = NULL,
    is_unix_time = TRUE,
    unix_time_unit = "microseconds"
  )
  expect_s3_class(result, "POSIXct")
})
