#' Compute User Sessions from Event Data
#'
#' Processes a dataset to create user sessions based on time gaps,
#' ordering columns, or actor groupings. It supports different ways to
#' understand order in user behavior and provides flexibility when
#' widening the data.
#'
#' @export
#' @param data A `data.frame` or containing the action/event data.
#' @param actor A `character` string giving the name of the column that
#' represents a user/actor identifier. If not provided and neither `time` nor
#' `order` is specified, the entire dataset is treated as a single session.
#' @param time A `character` string giving the name of the column representing
#' timestamps of the action events.
#' @param action A `character` string giving the name of the column holding
#' the information about the action taken.
#' @param order A `character` string giving the name of a column with sequence
#' numbers or non-unique orderable values that indicate order within an `actor`
#' group, if not present it will be ordered with all the data if no `actor` is
#' available, used when widening the data. If both `actor` and `time` are
#' specified, then the sequence order should be specified such that it
#' determines the order of events within `actor` and each session.
#' @param time_threshold An `integer` specifying the time threshold in seconds
#' for creating new time-based sessions. Defaults to 900 seconds.
#' @param custom_format A `character` string giving the format used to
#' parse the `time` column.
#' @param is_unix_time A `logical` value indicating whether the `time` column
#' is in Unix time. The default is `FALSE`.
#' @param unix_time_unit A `character` string giving the Unix time unit when
#' `is_unix_time` is `TRUE`. The default is `"seconds"`. Valid options are
#' `"seconds"`, `"milliseconds"`, or `"microseconds"`.
#' @param unused_fn How to handle extra columns when pivoting to wide format.
#' See [tidyr::pivot_wider()]. The default is to keep all columns and to
#' use the first value.
#' @return A `tna_data` object, which is a `list` with the following elements:
#'
#' * `long_data`: The processed data in long format.
#' * `sequence_data`: The processed data on the sequences in wide format,
#' with actions/events as different variables structured with sequences.
#' * `meta_data`: Other variables from the original data in wide format.
#' * `statistics`: A `list` containing summary statistics: total
#' sessions, total actions, unique users, time range (if applicable), and
#' top sessions and user by activities.
#'
#' @examples
#' data <- tibble::tibble(
#'   user = c("A", "A", "A", "B", "B", "C", "C", "C"),
#'   time = c(
#'     "2023-01-01 10:00:00", "2023-01-01 10:05:00",
#'     "2023-01-01 10:20:00", "2023-01-01 12:00:00",
#'     "2023-01-01 12:02:00", "2023-01-01 14:00:00",
#'     "2023-01-01 14:05:00", "2023-01-01 14:10:00"
#'   ),
#'   action = c(
#'     "view", "click", "add_cart", "view",
#'     "checkout", "view", "click", "share"
#'    )
#' )
#' results <- prepare_data(
#'   data, actor = "user", time = "time", action = "action"
#' )
#' print(results$sequence_data)
#' print(results$meta_data)
#' print(results$statistics)
#'
#' data_ordered <- tibble::tibble(
#'    user = c("A", "A", "A", "B", "B", "C", "C", "C"),
#'    order = c(1, 2, 3, 1, 2, 1, 2, 3),
#'    action = c(
#'      "view", "click", "add_cart", "view",
#'      "checkout", "view", "click", "share"
#'    )
#' )
#' results_ordered <- prepare_data(
#'   data_ordered, actor = "user", order = "order", action = "action"
#' )
#' print(results_ordered$sequence_data)
#' print(results_ordered$meta_data)
#' print(results_ordered$statistics)
#'
#' data_single_session <- tibble::tibble(
#'   action = c(
#'     "view", "click", "add_cart", "view", "checkout", "view", "click", "share"
#'    )
#' )
#' results_single <- prepare_data(data_single_session, action = "action")
#' print(results_single$sequence_data)
#' print(results_single$meta_data)
#' print(results_single$statistics)
#'
prepare_data <- function(data, actor, time, action, order,
                         time_threshold = 900, custom_format = NULL,
                         is_unix_time = FALSE, unix_time_unit = "seconds",
                         unused_fn = dplyr::first) {
  check_missing(data)
  check_missing(action)
  check_string(actor)
  check_string(time)
  check_string(action)
  check_string(order)
  check_values(time_threshold, type = "numeric")
  check_flag(is_unix_time)
  check_match(unix_time_unit, c("seconds", "milliseconds", "microseconds"))

  # Create some NULLs for R CMD Check
  .session_id <- .session_nr <- .new_session <- .time_gap <-
    .standardized_time <- .sequence <- n_sessions <- n_actions <- NULL

  message_("\fInitializing session computation...")
  message_(
    "\fInput data dimensions:
    {.val {nrow(data)}} rows, {.val {ncol(data)}} columns"
  )
  data <- tibble::as_tibble(data)

  # Column validation
  cols_req <- c(
    action,
    onlyif(!missing(actor), actor),
    onlyif(!missing(time), time),
    onlyif(!missing(order), order)
  )
  cols_obs <- cols_req %in% names(data)
  cols_mis <- cols_req[!cols_obs]
  stopifnot_(
    all(cols_obs),
    c(
      "\fThe columns {.val {cols_req}} must exist in the data.",
      `x` = "The following columns were not found in the data: {cols_mis}."
    )
  )
  long_data <- data
  default_actor <- FALSE
  default_order <- FALSE
  if (missing(actor)) {
    # Placeholder actor column
    actor <- ".actor"
    long_data$.actor <- "session"
    default_actor <- TRUE
  }
  if (missing(order)) {
    # Placeholder order column
    order <- ".order"
    long_data$.order <- seq_len(nrow(data))
    default_order <- TRUE
  }
  if (!missing(time)) {
    message_("\fParsing time values...")
    message_(
      "\fFirst few time values: {.val {utils::head(data[[time]], 3)}}"
    )
    if (is.numeric(data[[time]])) {
      message_(
        "\fDetected {.cls numeric} time values - treating as Unix timestamp."
      )
      is_unix_time <- TRUE
    }
    parsed_times <- parse_time(
      time = data[[time]],
      custom_format = custom_format,
      is_unix_time = is_unix_time,
      unix_time_unit = unix_time_unit
    )
    message_("\fSample of parsed times: {.val {utils::head(parsed_times, 3)}}")
    message_("\fCreating sessions based on time threshold...")
    message_(
      "\fTime threshold for new session: {.val {time_threshold}} seconds"
    )
    # Create processed data (long format)
    long_data <- long_data |>
      dplyr::mutate(.standardized_time = parsed_times) |>
      dplyr::arrange(
        !!rlang::sym(actor),
        .standardized_time,
        !!rlang::sym(order)
      ) |>
      dplyr::group_by(!!rlang::sym(actor)) |>
      dplyr::mutate(
        .time_gap = as.numeric(
          difftime(
            .standardized_time,
            dplyr::lag(.standardized_time),
            units = "secs"
          )
        ),
        .new_session = is.na(.time_gap) | .time_gap > time_threshold,
        .session_nr = cumsum(.new_session),
        .session_id = ifelse_(
          default_actor,
          paste0("session", .session_nr),
          paste0(!!rlang::sym(actor), " session", .session_nr)
        )
      ) |>
      dplyr::group_by(.session_id) |>
      dplyr::mutate(.sequence = dplyr::row_number()) |>
      dplyr::ungroup()

    long_data$.time_gap <- NULL
    long_data$.new_session <- NULL
  } else {
    msg <- ifelse_(
      default_order,
      paste0(
        "\fNo {.arg time} or {.arg order} column provided. ",
        ifelse_(
          default_actor,
          "Treating the entire dataset as one session.",
          "Using {.arg actor} as a session identifier."
        )
      ),
      "Using provided {.arg order} column to create sequences."
    )
    message_(msg)
    long_data <- long_data |>
      dplyr::arrange(
        !!rlang::sym(actor),
        !!rlang::sym(order)
      ) |>
      dplyr::group_by(!!rlang::sym(actor)) |>
      dplyr::mutate(
        .session_id = !!rlang::sym(actor),
        .sequence = dplyr::row_number()
      ) |>
      dplyr::ungroup()
  }
  if (default_actor) {
    long_data$.actor <- NULL
  }
  if (default_order) {
    long_data$.order <- NULL
  }

  # Create wide format
  message_("\fCreating wide format view of sessions...")
  wide_data <- long_data |>
    tidyr::pivot_wider(
      id_cols = .session_id,
      names_prefix = "T",
      names_from = .sequence,
      values_from = !!rlang::sym(action),
      unused_fn = unused_fn
    ) |>
    dplyr::arrange(.session_id)

  time_cols <- grepl("^T[0-9]+$", names(wide_data), perl = TRUE)
  sequence_data <- wide_data[, time_cols]
  meta_data <- wide_data[, !time_cols]

  # Calculate statistics
  stats <- list(
    total_sessions = dplyr::n_distinct(long_data$.session_id),
    total_actions = nrow(long_data),
    max_sequence_length = max(long_data$.sequence)
  )
  if (!default_actor) {
    stats$unique_users <- dplyr::n_distinct(long_data[[actor]])
    stats$sessions_per_user <- long_data |>
      dplyr::group_by(!!rlang::sym(actor)) |>
      dplyr::summarize(n_sessions = dplyr::n_distinct(.session_id)) |>
      dplyr::arrange(dplyr::desc(n_sessions))
  }
  stats$actions_per_session <- long_data |>
    dplyr::group_by(.session_id) |>
    dplyr::summarize(n_actions = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(n_actions))

  if (!missing(time)) {
    stats$time_range <- range(long_data$.standardized_time)
  }

  # Print summary statistics
  rlang_verbose <- getOption("rlib_message_verbosity")
  message_("\fSession Analysis Summary")
  message_("\f------------------------")
  message_("\fTotal number of sessions: {.val {stats$total_sessions}}")
  if (!default_actor) {
    message_("\fNumber of unique users: {.val {stats$unique_users}}")
  }
  message_("\fTotal number of actions: {.val {stats$total_actions}}")
  message_(
    "\fMaximum sequence length: {.val {stats$max_sequence_length}} actions"
  )
  if (!missing(time) && default_order) {
    message_(
      "\fTime range: {.val {stats$time_range[1]}} to
      {.val {stats$time_range[2]}}"
    )
  }
  if (!default_actor) {
    message_("\fSessions per user:")
    onlyif(
      is.null(rlang_verbose) || isTRUE(rlang_verbose == "verbose"),
      print(stats$sessions_per_user)
    )
  }
  message_("\fTop 5 longest sessions:")
  onlyif(
    is.null(rlang_verbose) || isTRUE(rlang_verbose == "verbose"),
    print(utils::head(stats$actions_per_session, 5))
  )
  structure(
    list(
      long_data = long_data,
      sequence_data = sequence_data,
      meta_data = meta_data,
      statistics = stats
    ),
    class = "tna_data"
  )
}

#' Robustly Parse Date and Time Values
#'
#' This function parses a variety of date and time formats into a standardized
#' POSIXct datetime object in R. It handles different separators,
#' time zone indicators, and partial dates. It also deals with missing
#' values and treats them as NA
#'
#' @param time A `vector` of time values.
#' @inheritParams prepare_data
#' @noRd
parse_time <- function(time, custom_format, is_unix_time, unix_time_unit) {
  message_("\fStarting time parsing process...")
  message_("\fNumber of values to parse: {.val {length(time)}}")
  message_("\fSample values: {.val {utils::head(time, 3)}}")
  # Handle Unix timestamps
  time_original <- time
  if (is.numeric(time) && is_unix_time) {
    parsed_time <- switch(
      unix_time_unit,
      "seconds" = as.POSIXct(time, origin = "1970-01-01"),
      "milliseconds" = as.POSIXct(time / 1000.0, origin = "1970-01-01"),
      "microseconds" = as.POSIXct(time / 1000000.0, origin = "1970-01-01")
    )
    return(parsed_time)
  }
  # If already datetime
  if (inherits(time, c("POSIXct", "POSIXlt"))) {
    return(time)
  }
  time_empty <- is.na(time) | !nzchar(time)
  if (any(time_empty)) {
    message_(
      "Found missing or empty time values; these will be treated as NA."
    )
    time[time_empty] <- NA
  }
  # Remove whitespace
  time <- trimws(as.character(time))
  # Remove timezone indicators like Z
  # or other trailing chars and handle milliseconds
  time <- gsub("(\\.\\d{1,3})?[A-Za-z ]*$", "", time)
  # Try custom format first if provided
  if (!is.null(custom_format)) {
    parsed_time <- as.POSIXct(strptime(time, format = custom_format))
    if (!all(is.na(parsed_time))) {
      message_("Successfully parsed using custom format.")
      return(parsed_time)
    }
  }

  # Comprehensive list of formats to try
  formats <- c(
    # Standard formats with different separators
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y.%m.%d %H:%M:%S",
    "%Y.%m.%d %H:%M",

    "%Y-%m-%dT%H:%M:%S",  # ISO8601 formats
    "%Y-%m-%dT%H:%M",     # ISO8601 formats
    "%Y-%m-%dT%H:%M:%OS", # ISO with optional second fraction
    "%Y-%m-%d %H:%M:%S%z",  # timezone offset with colon like +00:00
    "%Y-%m-%d %H:%M%z", # timezone offset with colon like +00:00 without seconds
    "%Y-%m-%d %H:%M:%S %z", # timezone offset with space colon like  +00:00
    "%Y-%m-%d %H:%M %z", # timezone offset with space and colon like +00:00
    "%Y%m%d%H%M%S",      # compact without separators like 20240201204530
    "%Y%m%d%H%M",        # compact without separators like 202402012045

    # Day first formats
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d.%m.%Y %H:%M:%S",
    "%d.%m.%Y %H:%M",

    "%d-%m-%YT%H:%M:%S",  # ISO8601 dayfirst
    "%d-%m-%YT%H:%M",     # ISO8601 dayfirst

    # Month first formats
    "%m-%d-%Y %H:%M:%S",
    "%m-%d-%Y %H:%M",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m.%d.%Y %H:%M:%S",
    "%m.%d.%Y %H:%M",
    "%m-%d-%YT%H:%M:%S", # ISO8601 month first
    "%m-%d-%YT%H:%M",    # ISO8601 month first

    # Formats with month names
    "%d %b %Y %H:%M:%S",
    "%d %b %Y %H:%M",
    "%d %B %Y %H:%M:%S",
    "%d %B %Y %H:%M",
    "%b %d %Y %H:%M:%S",
    "%b %d %Y %H:%M",
    "%B %d %Y %H:%M:%S",
    "%B %d %Y %H:%M",

    # Date only formats
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%Y.%m.%d",
    "%d-%m-%Y",
    "%d/%m/%Y",
    "%d.%m.%Y",
    "%m-%d-%Y",
    "%m/%d/%Y",
    "%m.%d.%Y",
    "%d %b %Y",
    "%d %B %Y",
    "%b %d %Y",
    "%B %d %Y"
  )

  # Try each format
  for (fmt in formats) {
    parsed_time <- as.POSIXct(strptime(time, format = fmt))
    if (!all(is.na(parsed_time))) {
      message_("\fSuccessfully parsed using format: {.val {fmt}}")
      message_("\fSample parsed time: {.val {format(parsed_time[1])}}")
      return(parsed_time)
    }
  }

  # Finally, try Unix time
  message_(
    "Unable to parse using supported formats.
    Trying to convert to {.cls numeric} and assuming Unix time."
  )
  # TODO suppress for now
  time <- suppressWarnings(try(as.numeric(time), silent = TRUE))
  if (!inherits(time, "try-error") && all(!is.na(time))) {
    parsed_time <- switch(
      unix_time_unit,
      "seconds" = as.POSIXct(time, origin = "1970-01-01"),
      "milliseconds" = as.POSIXct(time / 1000.0, origin = "1970-01-01"),
      "microseconds" = as.POSIXct(time / 1000000.0, origin = "1970-01-01")
    )
    return(parsed_time)
  }

  # If all attempts fail, provide helpful error message
  stop_(
    c(
      "Could not parse time values. Supported formats include:",
      "1. YYYY-MM-DD HH:MM:SS (e.g., 2023-01-09 18:44:00)",
      "2. YYYY/MM/DD HH:MM:SS (e.g., 2023/01/09 18:44:00)",
      "3. DD-MM-YYYY HH:MM:SS (e.g., 09-01-2023 18:44:00)",
      "4. MM-DD-YYYY HH:MM:SS (e.g., 01-09-2023 18:44:00)",
      "5. YYYY-MM-DDTHH:MM:SS (ISO8601 Format)",
      "6. YYYY-MM-DDTHH:MM:SS.milliseconds (ISO8601 with milliseconds)",
      "7. Compact Formats (YYYYMMDDHHMMSS)",
      "8. With or without Timezone offset (e.g, +00:00 or +01:00)",
      "9. Month names (e.g., 09 Jan 2023 18:44:00, January 09 2023 18:44:00)",
      "10. All above formats without seconds (HH:MM)",
      "11. Unix timestamps (numeric)",
      "Sample of problematic values: {.val {utils::head(time_original, 3)}}.",
      "Consider providing a custom format
       using the {.arg custom_format} argument."
    )
  )
}
