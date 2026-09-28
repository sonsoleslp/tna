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
#' @param actor A `character` vector or an `expression` that represents
#' a tidy selection of the names of the columns that
#' represent a user/actor identifiers. If not provided and neither `time` nor
#' `order` is specified, the entire dataset is treated as a single session.
#' In the case of multiple actors, a readable `.actor` column is added while
#' grouping uses the original columns to keep distinct combinations separate.
#' @param time A `character` string or an `expression` giving the name of
#' the column representing timestamps of the action events.
#' @param action A `character` string or an `expression` giving the name of
#' the column holding the information about the action taken.
#' @param order A `character` string or an `expression` giving the name of a
#' column with sequence numbers or non-unique orderable values that indicate
#' order within an `actor` group, if not present it will be ordered with all
#' the data if no `actor` is available, used when widening the data.
#' If both `actor` and `time` are specified, then the sequence order should
#' be specified such that it determines the order of events within `actor`
#' and each session.
#' @param session An optional tidy selection of one or more columns identifying
#' explicit sessions within actors, such as a course or semester. When `time`
#' is also supplied, each actor-session combination can be further split by
#' `time_threshold`.
#' @param time_threshold A positive `numeric` value specifying the time
#' threshold in seconds for creating new time-based sessions. Set to `FALSE`
#' to disable gap-based session splitting, so each actor-session combination
#' forms one session. Defaults to 900 seconds.
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
#' @param timezone An Olson time zone used to interpret timestamps that do not
#' contain an explicit UTC offset. Explicit offsets such as `Z`, `+00:00`, and
#' `-0500` are always honored. The default is `"UTC"` for reproducible results
#' across systems. See [OlsonNames()].
#' @details Session identity is based on observed combinations of the original
#' actor and session columns. Separator characters and high-cardinality
#' marginal levels therefore cannot merge distinct sessions. `.session_id` is
#' the collision-safe key and `.session_label` is its readable display label.
#' @return A `tna_data` object, which is a `list` with the following elements:
#'
#' * `long_data`: The processed data in long format.
#' * `sequence_data`: The processed data on the sequences in wide format,
#' with actions/events as different variables structured with sequences.
#' * `meta_data`: Other variables from the original data in wide format.
#' * `time_data`: Parsed timestamps in wide format when `time` is supplied,
#' or `NULL` otherwise.
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
#' # Custom order column
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
#' # No actor scenario leading to a single session
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
#' # Multiple actors
#' data_multi_actor <- tibble::tibble(
#'   user = c("A", "A", "A", "A", "B", "B", "B", "B"),
#'   session = c(1, 1, 2, 2, 1, 1, 2, 2),
#'   action = c(
#'     "view", "click", "add_cart", "view",
#'     "checkout", "view", "click", "share"
#'   )
#' )
#' results_multi_actor <- prepare_data(
#'   data_multi_actor, actor = c("user", "session"), action = "action"
#' )
#' print(results_multi_actor$sequence_data)
#' print(results_multi_actor$meta_data)
#' print(results_multi_actor$statistics)
#'
prepare_data <- function(data, actor, time, action, order,
                         time_threshold = 900, custom_format = NULL,
                         is_unix_time = FALSE, unix_time_unit = "seconds",
                         unused_fn = dplyr::first, timezone = "UTC",
                         session = NULL) {
  check_missing(data)
  check_class(data, "data.frame")
  sessions_disabled <- isFALSE(time_threshold)
  if (sessions_disabled) {
    time_threshold <- Inf
  }
  check_values(time_threshold, type = "numeric")
  stopifnot_(
    time_threshold > 0,
    "Argument {.arg time_threshold} must be a positive {.cls numeric} value or {.val FALSE}."
  )
  check_flag(is_unix_time)
  unix_time_unit <- check_match(
    unix_time_unit,
    c("seconds", "milliseconds", "microseconds")
  )
  check_timezone(timezone)
  # Create some NULLs for R CMD Check
  .actor_group <- .base_group <- .base_label <- .session_explicit <-
    .session_id <- .session_label <- .session_nr <- .new_session <-
    .time_gap <- .standardized_time <- .sequence <- n_sessions <-
    n_actions <- NULL
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
  actor_missing <- missing(actor)
  time_missing <- missing(time)
  order_missing <- missing(order)
  session_expr <- rlang::enquo(session)
  session_missing <- rlang::quo_is_missing(session_expr) ||
    rlang::quo_is_null(session_expr)
  action <- get_cols(rlang::enquo(action), data)
  actor <- get_cols(rlang::enquo(actor), data)
  time <- get_cols(rlang::enquo(time), data)
  order <- get_cols(rlang::enquo(order), data)
  session <- if (session_missing) {
    character(0L)
  } else {
    get_cols(session_expr, data)
  }
  check_cols(action, missing_ok = FALSE)
  check_cols(time)
  check_cols(order)
  if (!session_missing) {
    check_cols(session, single = FALSE)
  }
  data <- tibble::as_tibble(data)
  long_data <- data
  default_actor <- FALSE
  default_order <- FALSE
  actor_cols <- if (actor_missing) character(0L) else actor
  if (actor_missing) {
    # Placeholder actor column
    actor <- ".actor"
    long_data$.actor <- "session"
    default_actor <- TRUE
  } else if (length(actor) > 1L) {
    long_data$.actor <- group_label(long_data[, actor_cols, drop = FALSE], "-")
    actor <- ".actor"
  }
  actor_key_cols <- if (default_actor) ".actor" else actor_cols
  long_data$.actor_group <- observed_group_id(
    long_data[, actor_key_cols, drop = FALSE],
    context = "actor"
  )
  if (!session_missing) {
    long_data$.session_explicit <- if (length(session) > 1L) {
      group_label(long_data[, session, drop = FALSE])
    } else {
      as.character(long_data[[session]])
    }
  }
  base_key_cols <- c(actor_key_cols, session)
  long_data$.base_group <- observed_group_id(
    long_data[, base_key_cols, drop = FALSE],
    context = "actor/session"
  )
  long_data$.base_label <- if (session_missing) {
    as.character(long_data[[actor]])
  } else {
    group_label(list(long_data[[actor]], long_data$.session_explicit))
  }
  if (order_missing) {
    # Placeholder order column
    order <- ".order"
    long_data$.order <- seq_len(nrow(data))
    default_order <- TRUE
  }
  if (!time_missing) {
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
      unix_time_unit = unix_time_unit,
      timezone = timezone
    )
    message_(
      c(`i` = "Sample of parsed times: {.val {utils::head(parsed_times, 3)}}")
    )
    if (sessions_disabled) {
      message_(c(`i` = "Time-gap session splitting is disabled."))
    } else {
      message_(
        c(
          `i` = "Time threshold for new session: {.val {time_threshold}} seconds"
        )
      )
    }
    long_data <- long_data |>
      dplyr::mutate(.standardized_time = parsed_times) |>
      dplyr::arrange(
        .base_group,
        .standardized_time,
        !!rlang::sym(order)
      ) |>
      dplyr::group_by(.base_group)
    if (sessions_disabled) {
      long_data <- long_data |>
        dplyr::mutate(.session_nr = 1L)
    } else {
      long_data <- long_data |>
        dplyr::mutate(
          .time_gap = as.numeric(
            difftime(
              .standardized_time,
              dplyr::lag(.standardized_time),
              units = "secs"
            )
          ),
          .new_session = is.na(.time_gap) | .time_gap > time_threshold,
          .session_nr = cumsum(.new_session)
        )
    }
    long_data <- long_data |>
      dplyr::mutate(
        .session_id = paste0(.base_group, " s", .session_nr),
        .session_label = paste0(.base_label, " s", .session_nr)
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
        .base_group,
        !!rlang::sym(order)
      ) |>
      dplyr::group_by(.base_group) |>
      dplyr::mutate(
        .session_nr = 1L,
        .session_id = as.character(.base_group),
        .session_label = .base_label,
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
  wide_input <- long_data |>
    dplyr::select(-tidyselect::any_of(c(
      ".actor_group",
      ".base_group",
      ".base_label",
      ".session_explicit"
    )))
  if (!time_missing) {
    wide_data <- wide_input |>
      tidyr::pivot_wider(
        id_cols = .session_id,
        names_prefix = "T",
        names_from = .sequence,
        values_from = c(!!rlang::sym(action), .standardized_time),
        unused_fn = unused_fn
      )
    sequence_cols <- grepl(
      paste0("^", action, "_T[0-9]+$"),
      names(wide_data),
      perl = TRUE
    )
    time_cols <- grepl(
      "^.standardized_time_T[0-9]+$",
      names(wide_data),
      perl = TRUE
    )
    sequence_data <- wide_data[, sequence_cols]
    time_data <- wide_data[, time_cols]
    meta_data <- wide_data[, !(sequence_cols | time_cols)]
  } else {
    wide_data <- wide_input |>
      tidyr::pivot_wider(
        id_cols = .session_id,
        names_prefix = "T",
        names_from = .sequence,
        values_from = !!rlang::sym(action),
        unused_fn = unused_fn
      )
    sequence_cols <- grepl("^T[0-9]+$", names(wide_data), perl = TRUE)
    sequence_data <- wide_data[, sequence_cols]
    meta_data <- wide_data[, !sequence_cols]
    time_data <- NULL
  }

  # Calculate statistics
  stats <- list(
    total_sessions = dplyr::n_distinct(long_data$.session_id),
    total_actions = nrow(long_data),
    max_sequence_length = max(long_data$.sequence)
  )
  if (!default_actor) {
    stats$unique_users <- dplyr::n_distinct(long_data$.actor_group)
    stats$sessions_per_user <- long_data |>
      dplyr::group_by(.actor_group, !!rlang::sym(actor)) |>
      dplyr::summarize(
        n_sessions = dplyr::n_distinct(.session_id),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(n_sessions)) |>
      dplyr::select(-.actor_group)
  }
  stats$actions_per_session <- long_data |>
    dplyr::group_by(.session_id) |>
    dplyr::summarize(n_actions = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(n_actions))

  if (!time_missing) {
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
  if (!time_missing && default_order) {
    message_(
      c(
        `i` = "Time range: {.val {stats$time_range[1]}} to
        {.val {stats$time_range[2]}}"
      )
    )
  }
  long_data$.actor_group <- NULL
  long_data$.base_group <- NULL
  long_data$.base_label <- NULL
  long_data$.session_explicit <- NULL
  structure(
    list(
      long_data = long_data,
      sequence_data = sequence_data,
      meta_data = meta_data,
      time_data = time_data,
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
#' @return A `POSIXct` object.
#' @inheritParams prepare_data
#' @noRd
parse_time <- function(time, custom_format, is_unix_time, unix_time_unit,
                       timezone = "UTC") {
  message_(c(`i` = "Number of values to parse: {.val {length(time)}}"))
  message_(c(`i` = "Sample values: {.val {utils::head(time, 3)}}"))
  check_timezone(timezone)
  # Handle Unix timestamps
  time_original <- time
  if (is.numeric(time) && is_unix_time) {
    parsed_time <- switch(
      unix_time_unit,
      "seconds" = as.POSIXct(time, origin = "1970-01-01", tz = timezone),
      "milliseconds" = as.POSIXct(
        time / 1000.0,
        origin = "1970-01-01",
        tz = timezone
      ),
      "microseconds" = as.POSIXct(
        time / 1000000.0,
        origin = "1970-01-01",
        tz = timezone
      )
    )
    return(parsed_time)
  }
  # If already datetime
  if (inherits(time, c("POSIXct", "POSIXlt"))) {
    return(time)
  }
  time <- trimws(as.character(time))
  time_empty <- is.na(time) | !nzchar(time)
  if (any(time_empty)) {
    message_(
      c(
        `i` = "Found missing or empty time values; these will be treated as NA."
      )
    )
    time[time_empty] <- NA
  }
  time[time_empty] <- NA_character_
  parsed_time <- as.POSIXct(
    rep(NA_real_, length(time)),
    origin = "1970-01-01",
    tz = timezone
  )

  # Try a custom format first. Unmatched values continue through the
  # built-in formats so mixed timestamp columns are handled correctly.
  if (!is.null(custom_format)) {
    custom_parsed <- parse_time_formats(time, custom_format, timezone)
    custom_ok <- !is.na(custom_parsed)
    if (any(custom_ok)) {
      parsed_time[custom_ok] <- custom_parsed[custom_ok]
      message_(c(`v` = "Successfully parsed using custom format."))
    }
  }

  # Normalize ISO-8601 UTC markers and colon-delimited offsets to the form
  # understood consistently by strptime() across supported R versions.
  time_offset <- sub("(?:Z|UTC|GMT)$", "+0000", time,
                     ignore.case = TRUE, perl = TRUE)
  time_offset <- sub(
    "([+-][0-9]{2}):([0-9]{2})$",
    "\\1\\2",
    time_offset,
    perl = TRUE
  )
  has_timezone <- grepl(
    "(?:Z|UTC|GMT|[+-][0-9]{2}:?[0-9]{2}|[A-Za-z]{2,})$",
    time,
    ignore.case = TRUE,
    perl = TRUE
  )

  offset_formats <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS%z",
    "%Y-%m-%dT%H:%M%z",
    "%Y-%m-%d %H:%M%z"
  )
  offset_idx <- which(is.na(parsed_time) & has_timezone & !time_empty)
  if (length(offset_idx) > 0L) {
    offset_parsed <- parse_time_formats(
      time_offset[offset_idx],
      offset_formats,
      timezone
    )
    offset_ok <- !is.na(offset_parsed)
    parsed_time[offset_idx[offset_ok]] <- offset_parsed[offset_ok]
  }

  # Comprehensive list of formats to try
  formats <- c(
    # Standard formats with different separators
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y.%m.%d %H:%M:%S",
    "%Y.%m.%d %H:%M",

    "%Y-%m-%dT%H:%M:%OS", # ISO8601 with optional fractional seconds
    "%Y-%m-%dT%H:%M:%S",  # ISO8601 formats
    "%Y-%m-%dT%H:%M",     # ISO8601 formats
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

  naive_idx <- which(is.na(parsed_time) & !has_timezone & !time_empty)
  if (length(naive_idx) > 0L) {
    naive_parsed <- parse_time_formats(time[naive_idx], formats, timezone)
    naive_ok <- !is.na(naive_parsed)
    parsed_time[naive_idx[naive_ok]] <- naive_parsed[naive_ok]
  }

  # Finally, try unresolved values as Unix time.
  unresolved <- which(is.na(parsed_time) & !time_empty)
  if (length(unresolved) > 0L) {
    numeric_time <- suppressWarnings(as.numeric(time[unresolved]))
    numeric_ok <- !is.na(numeric_time)
    parsed_unix <- switch(
      unix_time_unit,
      "seconds" = as.POSIXct(
        numeric_time,
        origin = "1970-01-01",
        tz = timezone
      ),
      "milliseconds" = as.POSIXct(
        numeric_time / 1000.0,
        origin = "1970-01-01",
        tz = timezone
      ),
      "microseconds" = as.POSIXct(
        numeric_time / 1000000.0,
        origin = "1970-01-01",
        tz = timezone
      )
    )
    parsed_time[unresolved[numeric_ok]] <- parsed_unix[numeric_ok]
  }

  invalid <- which(is.na(parsed_time) & !time_empty)
  if (length(invalid) > 0L) {
    stop_(
      c(
        "Could not parse time values. Supported formats include:",
        "1. YYYY-MM-DD HH:MM:SS (e.g., 2023-01-09 18:44:00)",
        "2. YYYY/MM/DD HH:MM:SS (e.g., 2023/01/09 18:44:00)",
        "3. DD-MM-YYYY HH:MM:SS (e.g., 09-01-2023 18:44:00)",
        "4. MM-DD-YYYY HH:MM:SS (e.g., 01-09-2023 18:44:00)",
        "5. YYYY-MM-DDTHH:MM:SS (ISO8601 Format)",
        "6. YYYY-MM-DDTHH:MM:SS.sss (ISO8601 with fractional seconds)",
        "7. Compact Formats (YYYYMMDDHHMMSS)",
        "8. UTC or numeric offsets (e.g., Z, +00:00, or -0500)",
        "9. Month names (e.g., 09 Jan 2023 18:44:00)",
        "10. All above formats without seconds (HH:MM)",
        "11. Unix timestamps (numeric)",
        "Sample of problematic values: {.val {utils::head(time_original[invalid], 3)}}.",
        "Consider providing a custom format using {.arg custom_format}."
      )
    )
  }
  parsed_time
}

#' Parse Character Timestamps Using Multiple Formats
#'
#' @param time A `character` vector.
#' @param formats A `character` vector of formats attempted in order.
#' @param timezone An Olson time zone.
#' @return A `POSIXct` vector.
#' @noRd
parse_time_formats <- function(time, formats, timezone) {
  out <- rep(NA_real_, length(time))
  for (fmt in formats) {
    idx <- which(is.na(out) & !is.na(time))
    if (length(idx) == 0L) {
      break
    }
    parsed <- suppressWarnings(
      as.POSIXct(strptime(time[idx], format = fmt, tz = timezone))
    )
    ok <- !is.na(parsed)
    out[idx[ok]] <- as.numeric(parsed[ok])
  }
  as.POSIXct(out, origin = "1970-01-01", tz = timezone)
}

#' Validate an Olson Time Zone
#'
#' @param timezone A time zone name.
#' @noRd
check_timezone <- function(timezone) {
  check_string(timezone)
  stopifnot_(
    timezone %in% OlsonNames(),
    "Argument {.arg timezone} must be a valid Olson time zone."
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
#'   transformed into long format (actions). This can be a vector of column
#'   names (e.g., `c(feature1, feature2)`) or a range  specified as
#'   `feature1:feature6` (without quotes) to include all columns from
#'   'feature1' to 'feature6' in the order they appear in the data frame.
#'   For more information on tidy selections, see [dplyr::select()].
#' @param id_cols An `expression` giving a tidy selection of column names that
#'   uniquely identify each observation (IDs).
#' @param window_size An `integer` specifying the size of the window for
#'   sequence grouping. Default is 1 (each row is a separate window).
#' @param replace_zeros A `logical` value indicating whether to replace 0s
#'   in `cols` with `NA`. The default is `TRUE`.
#' @return A `data.frame` in long format with added columns for window and
#'   sequence order.
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
  cols <- get_cols(rlang::enquo(cols), data)
  id_cols <- get_cols(rlang::enquo(id_cols), data) %m% character(0L)
  check_cols(cols, single = FALSE, missing_ok = FALSE)
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

#' Import One-Hot Data
#'
#' @export
#' @family data
#' @param data A `data.frame` in wide format.
#' @param cols An `expression` giving a tidy selection of columns to be
#'   considered as one-hot data.
#' @param actor An optional `character` string giving the column name of
#'   `data` containing the actor identifiers.
#' @param session An optional `character` string giving the column name of
#'   `data` containing the session identifiers.
#' @param interval An `integer` that defines how many windows can appear
#'   at most per one row of the output data. If not provided (the default),
#'   all windows are concatenated per actor/session combination.
#' @param window_size An `integer` specifying the window size for grouping.
#' @param window_type A `character` string. Either `"tumbling"` (the default)
#'   for non-overlapping windows or `"sliding"` for one-step sliding window.
#' @param aggregate A `logical` value that determines how multiple occurrences
#'   of the same event within a window are processed. Option `TRUE` aggregates
#'   multiple occurrences into a single occurrence. Option `FALSE` keeps
#'   all occurrences (the default).
#' @return The processed data as a `data.frame`.
#' @examples
#' d <- data.frame(
#'   actor = gl(100, 5),
#'   session = gl(10, 50),
#'   feature1 = rbinom(500, 1, prob = 0.33),
#'   feature2 = rbinom(500, 1, prob = 0.25),
#'   feature3 = rbinom(500, 1, prob = 0.50)
#' )
#' onehot1 <- import_onehot(d, feature1:feature3)
#' onehot2 <- import_onehot(d, feature1:feature3, "actor", "session")
#'
import_onehot <- function(data, cols, actor, session, interval,
                          window_size = 1L, window_type = "tumbling",
                          aggregate = FALSE) {
  check_missing(data)
  check_class(data, "data.frame")
  check_values(window_size, strict = TRUE)
  check_flag(aggregate)
  window_type <- check_match(window_type, c("sliding", "tumbling"))
  cols <- get_cols(rlang::enquo(cols), data)
  actor <- get_cols(rlang::enquo(actor), data)
  session <- get_cols(rlang::enquo(session), data)
  interval <- interval %m% (ncol(data) * nrow(data))
  check_cols(actor, missing_ok = TRUE)
  check_cols(session, missing_ok = TRUE)
  if (missing(actor)) {
    actor <- ".actor"
    data$.actor <- 1L
  }
  if (missing(session)) {
    session <- ".session"
    data$.session <- 1L
  }
  out <- data |>
    dplyr::select(tidyselect::all_of(c(actor, session, cols))) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(cols),
        ~ ifelse(.x == 1, dplyr::cur_column(), NA_character_)
      )
    ) |>
    dplyr::group_by(!!rlang::sym(actor), !!rlang::sym(session))
  if (window_type == "sliding") {
    for (w in seq(1, window_size - 1)) {
      out <- out |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::all_of(cols),
            ~ ifelse(
              !is.na(.x) | !is.na(dplyr::lag(.x, n = w)),
              dplyr::cur_column(),
              NA_character_
            )
          )
        )
    }
    out <- out |>
      dplyr::slice(-1) |>
      dplyr::mutate(
        .window = floor(seq_len(dplyr::n()) - 1)
      )
  } else {
    out <- out |>
      dplyr::mutate(
        .window = floor((seq_len(dplyr::n()) - 1) / window_size)
      )
  }
  if (aggregate) {
    out <- out |>
      dplyr::group_by(
        !!rlang::sym(actor),
        !!rlang::sym(session),
        !!rlang::sym(".window")
      ) |>
      dplyr::summarise(
        dplyr::across(tidyselect::all_of(cols), ~ stats::na.omit(.x)[1L]),
        .groups = "drop"
      )
  }
  out <- out |>
    dplyr::group_by(!!rlang::sym(actor), !!rlang::sym(session)) |>
    dplyr::mutate(
      .window_grp = !!rlang::sym(".window") %/% interval,
      .window_idx = !!rlang::sym(".window") %% interval
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(cols)
    ) |> dplyr::group_by(
      !!rlang::sym(actor),
      !!rlang::sym(session),
      !!rlang::sym(".window_grp"),
      !!rlang::sym(".window_idx")
    ) |>
    dplyr::mutate(
      .obs = seq_len(dplyr::n())
    )
  out <- out |>
    dplyr::select(-!!rlang::sym("name")) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = tidyselect::all_of(c(actor, session, ".window_grp")),
      names_from = tidyselect::all_of(c(".window_idx", ".obs")),
      names_glue = "W{.window_idx}_T{.obs}",
      values_from = "value"
    )
  out[[actor]] <- NULL
  out[[session]] <- NULL
  out[[".window_grp"]] <- NULL
  # Window properties for modeling
  attr(out, "windowed") <- TRUE
  attr(out, "window_size") <- window_size^(!aggregate)
  attr(out, "window_span") <- length(cols)
  out
}
