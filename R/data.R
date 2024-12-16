#' Parse Event Data Into Sequence Data
#'
#' @export
#' @param data A `data.frame` with three columns for the actor, time, and
#' action.
#' @param actor_col A `character` string naming the actor column.
#' @param time_col A `character` string naming the actor column.
#' @param action_col A `character` string naming the actor column.
#' @param time_threshold TODO
#' @param custom_format A `character` string giving the format used to
#' parse the time column.
#' @param is_unix_time A `logical` value indicating whether the time column
#' is in Unix time. The default is `FALSE`.
#' @param unix_time_unit A `character` string giving the Unix time unit. The
#' default is `"seconds"`,
#' @param verbose A `logical` value indicating whether to print informative
#' messages during the parsing process. The default is `TRUE`.
#' @return TODO.
#' @examples
#' # TODO
#'
event2sequence <- function(data, actor_col, time_col, action_col,
                           time_threshold = 900, custom_format = NULL,
                           is_unix_time = FALSE, unix_time_unit = "seconds",
                           verbose = TRUE) {
  stopifnot_(
    !missing(actor_col),
    "Argument {.arg actor_col} must be provided
     for user/actor identification."
  )
  stopifnot_(
    !missing(time_col),
    "Argument {.arg time_col} must be provided for timestamps."
  )
  stopifnot_(
    !missing(action_col),
    "Argument {.arg action_col} must be provided for activities/actions."
  )
  check_nonnegative(time_threshold, type = "numeric")
  check_flag(is_unix_time)
  check_match(unix_time_unit, c("seconds", "milliseconds", "microseconds"))
  check_flag(verbose)
  if (verbose) {
    message_("Initializing session computation...")
    message_(
      "Input data dimensions:
      {.val {nrow(data)}} rows, {.val {ncol(data)}} columns"
    )
  }
  data <- tibble::as_tibble(data)

  # Enhanced column validation
  cols_req <- c(actor_col, time_col, action_col)
  cols_obs <- cols_req %in% names(data)
  cols_mis <- cols_req[!cols_obs]
  stopifnot_(
    all(cols_obs),
    c(
      "The columns {.val {cols_req}} must exist in the data.",
      `x` = "The following columns were not found in the data: {cols_mis}."
    )
  )

  # Time parsing with enhanced error handling and feedback
  if (verbose) {
    message_("Parsing time values...")
    message_("First few time values: {.val {utils::head(data[[time_col]], 3)}}")
  }
  if (is.numeric(data[[time_col]])) {
    if (verbose) {
      message_(
        "Detected {.cls numeric} time values - treating as Unix timestamp"
      )
    }
    is_unix_time <- TRUE
  }
  parsed_times <- parse_time(
    time = data[[time_col]],
    custom_format = custom_format,
    is_unix_time = is_unix_time,
    unix_time_unit = unix_time_unit,
    verbose = verbose
  )
  if (verbose) {
    message_("Sample of parsed times: {.val {utils::head(parsed_times, 3)}}")
    message_("Creating sessions based on time threshold...")
    message_("Time threshold for new session: {.val {time_threshold}} seconds")
  }

  # Create some NULLs for R CMD Check
  session_id <- session_nr <- new_session <- Time_gap <-
    Standardized_Time <- sequence <- n_sessions <- n_actions <- NULL

  # Create processed data (long format)
  long_format <- data |>
    dplyr::mutate(Standardized_Time = parsed_times) |>
    dplyr::arrange(!!rlang::sym(actor_col), Standardized_Time) |>
    dplyr::group_by(!!rlang::sym(actor_col)) |>
    dplyr::mutate(
      Time_gap = as.numeric(
        difftime(
          Standardized_Time,
          dplyr::lag(Standardized_Time),
          units = "secs"
        )
      ),
      new_session = is.na(Time_gap) | Time_gap > time_threshold,
      session_nr = cumsum(new_session),
      session_id = paste0(!!rlang::sym(actor_col), " session", session_nr)
    ) |>
    dplyr::group_by(session_id) |>
    dplyr::mutate(sequence = dplyr::row_number()) |>
    dplyr::ungroup()

  # Create wide format
  if (verbose) {
    message_("Creating wide format view of sessions...")
  }
  wide_format <- long_format |>
    dplyr::select(session_id, sequence, !!rlang::sym(action_col)) |>
    tidyr::pivot_wider(
      id_cols = session_id,
      names_from = sequence,
      values_from = !!rlang::sym(action_col)
    ) |>
    dplyr::arrange(session_id)

  # Calculate statistics
  stats <- list(
    total_sessions = dplyr::n_distinct(long_format$session_id),
    unique_users = dplyr::n_distinct(long_format[[actor_col]]),
    total_actions = nrow(long_format),
    max_sequence_length = max(long_format$sequence),
    time_range = range(long_format$Standardized_Time),
    sessions_per_user = long_format |>
      dplyr::group_by(!!rlang::sym(actor_col)) |>
      dplyr::summarize(n_sessions = dplyr::n_distinct(session_id)) |>
      dplyr::arrange(dplyr::desc(n_sessions)),
    actions_per_session = long_format |>
      dplyr::group_by(session_id) |>
      dplyr::summarize(n_actions = dplyr::n()) |>
      dplyr::arrange(dplyr::desc(n_actions))
  )

  # Print summary statistics
  if (verbose) {
    cat("\n")
    message_("Session Analysis Summary")
    message_("------------------------")
    message_("Total number of sessions: {.val {stats$total_sessions}}")
    message_("Number of unique users: {.val {stats$unique_users}}")
    message_("Total number of actions: {.val {stats$total_actions}}")
    message_(
      "Maximum sequence length: {.val {stats$max_sequence_length}} actions"
    )
    message_(
      "Time range: {.val {stats$time_range[1]}} to {.val {stats$time_range[2]}}"
    )
    message_("\nSessions per user:")
    print(stats$sessions_per_user)
    message_("Top 5 longest sessions:")
    print(utils::head(stats$actions_per_session, 5))
  }
  list(
    long_format = long_format,
    wide_format = wide_format,
    statistics = stats
  )
}

#' Parse Various Time Formats
#'
#' @param time A `vector` of time values.
#' @inheritParams event2sequence
#' @noRd
parse_time <- function(time, custom_format,
                                is_unix_time, unix_time_unit, verbose) {
  if (verbose) {
    message_("Starting time parsing process...")
    message_("Number of values to parse: {.val {length(time)}}")
    message_("Sample values: {.val {utils::head(time, 3)}}")
  }
  # Handle Unix timestamps
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
  time <- trimws(as.character(time))
  # Try custom format first if provided
  if (!is.null(custom_format)) {
    parsed_time <- as.POSIXct(strptime(time, format = custom_format))
    if (!all(is.na(parsed_time))) {
      if (verbose) {
        message_("Successfully parsed using custom format")
      }
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

    # Day first formats
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d.%m.%Y %H:%M:%S",
    "%d.%m.%Y %H:%M",

    # Month first formats
    "%m-%d-%Y %H:%M:%S",
    "%m-%d-%Y %H:%M",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m.%d.%Y %H:%M:%S",
    "%m.%d.%Y %H:%M",

    # Formats with month names
    "%d %b %Y %H:%M:%S",
    "%d %b %Y %H:%M",
    "%d %B %Y %H:%M:%S",
    "%d %B %Y %H:%M",
    "%b %d %Y %H:%M:%S",
    "%b %d %Y %H:%M",
    "%B %d %Y %H:%M:%S",
    "%B %d %Y %H:%M",

    # Date-only formats
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
      if (verbose) {
        message_("Successfully parsed using format: {.val {fmt}}")
        message_("Sample parsed time: {.val {format(parsed_times[1])}}")
      }
      return(parsed_time)
    }
  }

  # If all attempts fail, provide helpful error message
  stop_(
    c(
      "Could not parse time values. Supported formats include:",
      "1. YYYY-MM-DD HH:MM:SS (e.g., 2023-01-09 18:44:00)",
      "2. YYYY/MM/DD HH:MM:SS (e.g., 2023/01/09 18:44:00)",
      "3. DD-MM-YYYY HH:MM:SS (e.g., 09-01-2023 18:44:00)",
      "4. MM-DD-YYYY HH:MM:SS (e.g., 01-09-2023 18:44:00)",
      "5. Month names (e.g., 09 Jan 2023 18:44:00, January 09 2023 18:44:00)",
      "6. All above formats without seconds (HH:MM)",
      "7. Unix timestamps ({.cls numeric})",
      "Sample of problematic values: {.val {utils::head(time, 3)}}.",
      "Consider providing a custom format
       using the {.arg custom_format} argument."
    )
  )
}
