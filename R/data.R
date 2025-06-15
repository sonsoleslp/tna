#' Compute User Sessions from Event Data
#'
#' Processes a dataset to create user sessions based on time gaps,
#' ordering columns, or actor groupings. It supports different ways to
#' understand order in user behavior and provides flexibility when
#' widening the data.
#'
#' @export
#' @family data
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
#' results <- prepare_data(
#'   group_regulation_long, actor = "Actor", time = "Time", action = "Action"
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
#'     "view", "click", "add_cart", "view",
#'     "checkout", "view", "click", "share"
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
  check_class(data, "data.frame")
  check_missing(action)
  check_string(actor)
  check_string(time)
  check_string(action)
  check_string(order)
  check_values(time_threshold, type = "numeric")
  check_flag(is_unix_time)
  unix_time_unit <- check_match(
    unix_time_unit,
    c("seconds", "milliseconds", "microseconds")
  )

  # Create some NULLs for R CMD Check
  .session_id <- .session_nr <- .new_session <- .time_gap <-
    .standardized_time <- .sequence <- n_sessions <- n_actions <- NULL

  rlang_verbose <- getOption("rlib_message_verbosity")
  onlyif(
    is.null(rlang_verbose) || isTRUE(rlang_verbose == "verbose"),
    cli::cli_rule(left = "Preparing Data")
  )
  message_(
    c(
      `i` = "Input data dimensions:
      {.val {nrow(data)}} rows, {.val {ncol(data)}} columns"
    )
  )
  data <- tibble::as_tibble(data)
  cols_req <- c(
    action,
    onlyif(!missing(actor), actor),
    onlyif(!missing(time), time),
    onlyif(!missing(order), order)
  )
  check_cols(cols_req, names(data))
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
    message_(
      c(`i` = "First few time values: {.val {utils::head(data[[time]], 3)}}")
    )
    if (is.numeric(data[[time]])) {
      message_(
        c(
          `i` = "Detected {.cls numeric} time values:
          treating as Unix timestamp."
        )
      )
      is_unix_time <- TRUE
    }
    parsed_times <- parse_time(
      time = data[[time]],
      custom_format = custom_format,
      is_unix_time = is_unix_time,
      unix_time_unit = unix_time_unit
    )
    message_(
      c(`i` = "Sample of parsed times: {.val {utils::head(parsed_times, 3)}}")
    )
    message_(
      c(`i` = "Time threshold for new session: {.val {time_threshold}} seconds")
    )
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
        "No {.arg time} or {.arg order} column provided. ",
        ifelse_(
          default_actor,
          "Treating the entire dataset as one session.",
          "Using {.arg actor} as a session identifier."
        )
      ),
      "Using provided {.arg order} column to create sequences."
    )
    message_(c(`i` = msg))
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
  message_(c(`i` = "Total number of sessions: {.val {stats$total_sessions}}"))
  if (!default_actor) {
    message_(c(`i` = "Number of unique users: {.val {stats$unique_users}}"))
  }
  message_(c(`i` = "Total number of actions: {.val {stats$total_actions}}"))
  message_(
    c(
      `i` = "Maximum sequence length:
      {.val {stats$max_sequence_length}} actions"
    )
  )
  if (!missing(time) && default_order) {
    message_(
      c(
        `i` = "Time range: {.val {stats$time_range[1]}} to
        {.val {stats$time_range[2]}}"
      )
    )
  }
  # if (!default_actor) {
  #   message_(c(`i` = "Sessions per user:"))
  #   for (i in seq_len(nrow(stats$sessions_per_user))) {
  #     msg <- paste0(
  #       stats$sessions_per_user[[actor]][i],
  #       ": ",
  #       "{.val {", stats$sessions_per_user$n_sessions[i], "}}"
  #     )
  #     message_(c(` ` = msg))
  #   }
  # }
  # message_(c(`i` = "Top 5 longest sessions:"))
  # max_rows <- min(nrow(stats$actions_per_session), 5L)
  # for (i in seq_len(max_rows)) {
  #   msg <- paste0(
  #     stats$actions_per_session$.session_id[i],
  #     ": ",
  #     "{.val {", stats$actions_per_session$n_actions[i], "}}"
  #   )
  #   message_(c(` ` = msg))
  # }
  structure(
    list(
      long_data = long_data,
      sequence_data = sequence_data,
      meta_data = meta_data,
      statistics = stats
    ),
    type = "eventdata",
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
#' @return A `POSIXct` object.
#' @inheritParams prepare_data
#' @noRd
parse_time <- function(time, custom_format, is_unix_time, unix_time_unit) {
  message_(c(`i` = "Number of values to parse: {.val {length(time)}}"))
  message_(c(`i` = "Sample values: {.val {utils::head(time, 3)}}"))
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
      c(
        `i` = "Found missing or empty time values; these will be treated as NA."
      )
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
      message_(c(`v` = "Successfully parsed using custom format."))
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
      message_(c(`v` = "Successfully parsed using format: {.val {fmt}}"))
      return(parsed_time)
    }
  }

  # Finally, try Unix time
  message_(
    c(
      `x` = "Unable to parse using supported formats.
      Trying to convert to {.cls numeric} and assuming Unix time."
    )
  )
  # TODO suppress for now
  time <- suppressWarnings(try_(as.numeric(time)))
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

#' Import Wide Format Sequence Data as Long Format Sequence Data
#'
#' This function transforms wide format data where features are in separate
#' columns into a long format suitable for sequence analysis. It creates
#' windows of data based on row order and generates sequence order within
#' these windows.
#'
#' @export
#' @family data
#' @param data A `data.frame` in wide format.
#' @param cols An `expression` giving a tidy selection of column names to be
#' transformed into long format (actions). This can be a vector of column names
#' (e.g., `c(feature1, feature2)`) or a range  specified as `feature1:feature6`
#' (without quotes) to include all columns from 'feature1' to 'feature6'
#' in the order they appear in the data frame. For more information on
#' tidy selections, see [dplyr::select()].
#' @param id_cols A `character` vector of column names that uniquely identify
#' each observation (IDs).
#' @param window_size An `integer` specifying the size of the window for
#' sequence grouping. Default is 1 (each row is a separate window).
#' @param replace_zeros A `logical` value indicating whether to replace 0s
#' in `cols` with `NA`. The default is `TRUE`.
#' @return A `data.frame` in long format with added columns for window and
#' sequence order.
#' @examples
#' data <- data.frame(
#'   ID = c("A", "A", "B", "B"),
#'   Time = c(1, 2, 1, 2),
#'   feature1 = c(10, 0, 15, 20),
#'   feature2 = c(5, 8, 0, 12),
#'   feature3 = c(2, 4, 6, 8),
#'   other_col = c("X", "Y", "Z", "W")
#' )
#'
#' # Using a vector
#' long_data1 <- import_data(
#'   data = data,
#'   cols = c(feature1, feature2),
#'   id_cols = c("ID", "Time"),
#'   window_size = 2,
#'   replace_zeros = TRUE
#' )
#'
#' # Using a column range
#' long_data2 <- import_data(
#'   data = data,
#'   cols = feature1:feature3,
#'   id_cols = c("ID", "Time"),
#'   window_size = 2,
#'   replace_zeros = TRUE
#' )
#'
import_data <- function(data, cols, id_cols,
                        window_size = 1, replace_zeros = TRUE) {
  check_missing(data)
  check_class(data, "data.frame")
  check_flag(replace_zeros)
  data_names <- colnames(data)
  missing_ids <- id_cols[!id_cols %in% colnames(data)]
  stopifnot_(
    length(missing_ids) == 0L,
    c(
      "All ID columns specified by {.arg id_cols} must be present in the data.",
      `x` = "The following column{?s} {?was/were} not found: {missing_ids}."
    )
  )
  expr_cols <- rlang::enquo(cols)
  pos <- tidyselect::eval_select(expr_cols, data = data)
  cols <- names(pos)
  out <- data
  n <- nrow(out)
  rownames(out) <- ifelse_(
    is.null(rownames(data)),
    seq_len(n),
    rownames(data)
  )
  # Create some NULLs for R CMD Check
  .original_row <- window_group <- action <- value <- order <- NULL
  out$.original_row <- as.numeric(rownames(out))
  if (replace_zeros) {
    out <- out |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols), ~ifelse(. == 0, NA, .)
        )
      )
  }
  out <- out |>
    dplyr::arrange(.original_row) |>
    dplyr::mutate(window_group = ceiling(seq_len(n) / window_size))
  out_names <- colnames(data)
  extra_cols <- out_names[!(out_names %in% c(cols, id_cols))]
  out |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols),
      names_to = "action",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::arrange(.original_row, action) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c("window_group", id_cols))
      )
    ) |>
    dplyr::mutate(order = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      dplyr::across(
        dplyr::all_of(c(id_cols, "window_group", "order"))
      )
    ) |>
    dplyr::select(
      dplyr::all_of(extra_cols),
      dplyr::all_of(id_cols),
      action,
      value,
      order
    )
}
