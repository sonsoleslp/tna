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
    result <- prepare_data(
      data,
      actor = "user",
      time = "time",
      action = "action"
    ),
    NA
  )
  expect_s3_class(result$time_data[[1]], "POSIXct")
  expect_identical(attr(result$time_data[[1]], "tzone"), "UTC")
})

test_that("time_threshold = FALSE disables gap-based session splitting", {
  data <- tibble::tibble(
    user = rep(c("A", "B"), each = 2),
    time = rep(c("2024-01-01 00:00:00", "2024-01-01 01:00:00"), 2),
    action = c("view", "click", "view", "click")
  )
  rlang::local_options(rlib_message_verbosity = "quiet")

  enabled <- prepare_data(
    data, actor = "user", time = "time", action = "action"
  )
  disabled <- prepare_data(
    data, actor = "user", time = "time", action = "action",
    time_threshold = FALSE
  )
  infinite <- prepare_data(
    data, actor = "user", time = "time", action = "action",
    time_threshold = Inf
  )

  expect_identical(enabled$statistics$total_sessions, 4L)
  expect_identical(disabled$statistics$total_sessions, 2L)
  expect_equal(disabled, infinite)
  expect_s3_class(disabled$time_data[[1]], "POSIXct")

  expect_error(
    prepare_data(
      data, actor = "user", time = "time", action = "action",
      time_threshold = TRUE
    )
  )
  expect_error(
    prepare_data(
      data, actor = "user", time = "time", action = "action",
      time_threshold = 0
    )
  )
  expect_error(
    prepare_data(
      data, actor = "user", time = "time", action = "action",
      time_threshold = NA
    )
  )
})

test_that("disabled time splitting keeps missing timestamps in one session", {
  data <- tibble::tibble(
    user = "A",
    time = c("2024-01-01 00:00:00", NA, NA),
    action = c("view", "click", "share"),
    event_order = 1:3
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- prepare_data(
    data,
    actor = "user",
    time = "time",
    action = "action",
    order = "event_order",
    time_threshold = FALSE
  )

  expect_identical(result$statistics$total_sessions, 1L)
  expect_identical(
    as.character(result$sequence_data[1, 1:3]),
    c("view", "click", "share")
  )
  expect_true(is.na(result$time_data[[2]][1]))
  expect_true(is.na(result$time_data[[3]][1]))
})

test_that("composite actor identities cannot collide on separators", {
  data <- tibble::tibble(
    student = c("a-b", "a-b", "a", "a"),
    cohort = c("c", "c", "b-c", "b-c"),
    event_order = c(1, 2, 1, 2),
    action = c("A", "B", "A", "C")
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- prepare_data(
    data,
    actor = c("student", "cohort"),
    order = "event_order",
    action = "action"
  )

  expect_identical(result$statistics$total_sessions, 2L)
  expect_identical(result$statistics$unique_users, 2L)
  expect_identical(anyDuplicated(result$meta_data$.session_id), 0L)
  paths <- sort(apply(result$sequence_data, 1L, paste, collapse = "->"))
  expect_identical(paths, c("A->B", "A->C"))
})

test_that("explicit session columns define actor-session combinations", {
  data <- tibble::tibble(
    student = "s1",
    course = c("math", "math", "bio", "bio"),
    semester = c("fall", "fall", "fall", "spring"),
    event_order = c(1, 2, 1, 1),
    action = c("A", "B", "C", "D")
  )
  rlang::local_options(rlib_message_verbosity = "quiet")

  one_session_column <- prepare_data(
    data,
    actor = "student",
    session = "course",
    order = "event_order",
    action = "action"
  )
  two_session_columns <- prepare_data(
    data,
    actor = "student",
    session = c("course", "semester"),
    order = "event_order",
    action = "action"
  )

  expect_identical(one_session_column$statistics$total_sessions, 2L)
  expect_identical(two_session_columns$statistics$total_sessions, 3L)
  expect_identical(two_session_columns$statistics$unique_users, 1L)
})

test_that("missing actor or session identifiers fail fast", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  actor_missing <- tibble::tibble(
    student = c("s1", NA),
    session = c("a", "b"),
    action = c("A", "B")
  )
  session_missing <- actor_missing
  session_missing$student <- "s1"
  session_missing$session[2] <- NA

  expect_error(
    prepare_data(actor_missing, actor = "student", action = "action"),
    "Missing values in actor"
  )
  expect_error(
    prepare_data(
      session_missing,
      actor = "student",
      session = "session",
      action = "action"
    ),
    "Missing values in actor/session"
  )
})

test_that("high-cardinality actor-session grouping does not overflow", {
  skip_on_cran()
  n <- 47000L
  data <- tibble::tibble(
    student = rep(sprintf("student-%05d", seq_len(n)), each = 2L),
    session = rep(sprintf("session-%05d", seq_len(n)), each = 2L),
    event_order = rep(1:2, n),
    action = rep(c("A", "B"), n)
  )
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- prepare_data(
    data,
    actor = "student",
    session = "session",
    order = "event_order",
    action = "action"
  )

  expect_identical(result$statistics$total_sessions, n)
  expect_identical(nrow(result$sequence_data), n)
  expect_true(all(result$sequence_data[[1]] == "A"))
  expect_true(all(result$sequence_data[[2]] == "B"))
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
  time <- as.POSIXct(strptime(time_raw, format = fmt, tz = "UTC"))
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

test_that("import_onehot can split windows by interval", {
  d <- data.frame(
    actor = gl(10, 10),
    session = gl(5, 20),
    feature1 = rbinom(100, 1, prob = 0.33),
    feature2 = rbinom(100, 1, prob = 0.25),
    feature3 = rbinom(100, 1, prob = 0.50)
  )
  result_orig <- import_onehot(
    d,
    actor = "actor",
    session = "session",
    feature1:feature3,
    window_size = 2,
  )
  result1 <- import_onehot(
    d,
    actor = "actor",
    session = "session",
    feature1:feature3,
    interval = 2,
    window_size = 2,
  )
  result2 <- import_onehot(
    d,
    actor = "actor",
    session = "session",
    feature1:feature3,
    interval = 3,
    window_size = 2,
  )
  result3 <- import_onehot(
    d,
    actor = "actor",
    session = "session",
    feature1:feature3,
    interval = 4,
    window_size = 2,
  )
  result4 <- import_onehot(
    d,
    actor = "actor",
    session = "session",
    feature1:feature3,
    interval = 5,
    window_size = 2,
  )
  expect_equal(ncol(result1), 12)
  expect_equal(ncol(result2), 18)
  expect_equal(ncol(result3), 24)
  expect_equal(ncol(result4), 30)
  expect_equal(result_orig, result4)
})

test_that("parse_time handles milliseconds unix time", {
  time <- c(1609459200000, 1609459260000, 1609459320000)
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- parse_time(
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
  result <- parse_time(
    time,
    custom_format = NULL,
    is_unix_time = TRUE,
    unix_time_unit = "microseconds"
  )
  expect_s3_class(result, "POSIXct")
})

test_that("explicit timezone offsets are honored across system timezones", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  old_timezone <- Sys.getenv("TZ", unset = NA_character_)
  on.exit(
    if (is.na(old_timezone)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_timezone),
    add = TRUE
  )
  time <- c(
    "2024-01-01T00:00:00Z",
    "2024-01-01 00:00:00+00:00",
    "2024-01-01 00:00:00+02:00",
    "2024-01-01T00:00:00.125Z"
  )
  expected <- c(1704067200, 1704067200, 1704060000, 1704067200.125)

  for (system_timezone in c("UTC", "Europe/Helsinki", "America/New_York")) {
    Sys.setenv(TZ = system_timezone)
    result <- parse_time(
      time,
      custom_format = NULL,
      is_unix_time = FALSE,
      unix_time_unit = "seconds"
    )
    expect_equal(as.numeric(result), expected, tolerance = 1e-6)
    expect_identical(attr(result, "tzone"), "UTC")
  }
})

test_that("timezone controls offset-free timestamps", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- parse_time(
    "2024-01-01 00:00:00",
    custom_format = NULL,
    is_unix_time = FALSE,
    unix_time_unit = "seconds",
    timezone = "Europe/Helsinki"
  )
  expect_equal(as.numeric(result), 1704060000)
  expect_identical(attr(result, "tzone"), "Europe/Helsinki")
})

test_that("mixed timestamp formats are parsed completely", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  result <- parse_time(
    c("2024-01-01T00:00:00Z", "2024/01/02 03:04:05", NA),
    custom_format = NULL,
    is_unix_time = FALSE,
    unix_time_unit = "seconds"
  )
  expect_false(anyNA(result[1:2]))
  expect_true(is.na(result[3]))
})

test_that("invalid timezones fail", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  expect_error(
    parse_time(
      "2024-01-01 00:00:00",
      custom_format = NULL,
      is_unix_time = FALSE,
      unix_time_unit = "seconds",
      timezone = "Not/A_Timezone"
    ),
    "must be a valid Olson time zone"
  )
})
