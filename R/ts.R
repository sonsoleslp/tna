# Data --------------------------------------------------------------------



#' Import and Convert Time-Series Data into Wide Format Sequence Data
#'
#' Imports time-series data as sequence data via discretization.
#' Various methods for discretization are available including gaussian mixtures,
#' K-means clustering and kernel density based binning.
#'
#' @export
#' @family data
#' @param data A `data.frame` containing time-series data in long format, or
#' a `ts` object.
#' @param id_col An optional `character` string naming the column that contains
#' the unique IDs.
#' @param value_col A `character` string naming the column that contains the
#' data values.
#' @param order_col A `character` string naming the column that contains the
#' time variable (not required if the data is already in order),
#' @param n_states An `integer` specifying the number of states.
#' @param method A `character` string defining the discretization method to use.
#' The available options are:
#'
#'   * `kmeans`: for K-means clustering (the default).
#'   * `width`: for equal width binning.
#'   * `quantile`: for quantile-based binning.
#'   * `kde`: for binning based on kernel density estimation.
#'   * `gaussian`: for a Gaussian mixture model.
#'
#' @param labels An `character` vector specifying the names of the states. The
#' length must be `n_states` The defaults is consecutive numbering,
#' i.e. `1:n_states`.
#' @param unused_fn How to handle extra columns when pivoting to wide format.
#' See [tidyr::pivot_wider()]. The default is to keep all columns and to
#' use the first value.
#' @param ... Additional arguments passed to the discretization method
#'   ([stats::kmeans()] for `kmeans`, [stats::density()] and
#'   [pracma::findpeaks()] for `kde`, and
#'   [mclust::Mclust()] for `gaussian`).
#' @return A `tna_data` object, which is a `list` with the following elements:
#'
#' * `long_data`: The processed data in long format.
#' * `sequence_data`: The processed data on the sequences in wide format,
#' with time points as different variables structured with sequences.
#' * `meta_data`: Other variables from the original data in wide format.
#' * `statistics`: A `list` of two `tibble`s, `global` and `local`, that contain
#' descriptive statistics of the `value_col` variable for each state, and
#' for each state an ID of `id_col`, respectively.
#'
#' @importFrom mclust Mclust
#' @importFrom mclust mclustBIC
#' @examples
#' # Long format data
#' ts_data <- data.frame(
#'   id = gl(10, 100),
#'   series = c(
#'     replicate(
#'       10,
#'       stats::arima.sim(list(order = c(2, 1, 0), ar = c(0.5, 0.2)), n = 99)
#'     )
#'   )
#' )
#' data <- prepare_ts(ts_data, "id", "series", n_states = 3)
#'
#' # Time-series data
#' data <- prepare_ts(EuStockMarkets, n_states = 3)
#'
prepare_ts <- function(data, n_states, labels = 1:n_states,
                       method = "kmeans", unused_fn = dplyr::first, ...) {
  UseMethod("prepare_ts")
}

#' @export
#' @rdname prepare_ts
prepare_ts.ts <- function(data, n_states, labels = 1:n_states,
                          method = "kmeans", unused_fn = dplyr::first, ...) {
  df <- data.frame(data)
  df$time <- stats::time(data)
  df_long <- df |>
    tidyr::pivot_longer(
      colnames(data),
      names_to = "series"
    )
  prepare_ts(
    df_long,
    id_col = "series",
    value_col = "value",
    order_col = "time",
    n_states = n_states,
    labels = labels,
    method = method,
    unused_fn = unused_fn,
    ...
  )

}

#' @export
#' @rdname prepare_ts
prepare_ts.default <- function(data, id_col, value_col, order_col, n_states,
                               labels = 1:n_states, method = "kmeans",
                               unused_fn = dplyr::first, ...) {
  check_missing(data)
  check_missing(value_col)
  check_class(data, "data.frame")
  check_string(id_col)
  check_string(value_col)
  labels <- try_(as.character(labels))
  stopifnot_(
    checkmate::test_character(
      x = labels,
      any.missing = FALSE,
      min.len = n_states,
      max.len = n_states
    ),
    "Argument {.arg labels} must be coercible to {.cls character} and
     provide a label for each state."
  )
  stopifnot_(
    checkmate::test_int(x = n_states, lower = 2L),
    "Argument {.arg n_states} must be an integer greater than 1."
  )
  stopifnot_(
    length(labels) == n_states,
    "Argument {.arg labels} must have length {n_states}
    (same as {.arg n_states})."
  )
  method <- check_match(method, names(discretization_funs))
  cols_req <- c(
    value_col,
    onlyif(!missing(id_col), id_col),
    onlyif(!missing(order_col), order_col)
  )
  check_cols(cols_req, names(data))
  id_col <- ifelse_(missing(id_col), ".id", id_col)
  data$.id <- 1L
  complete <- stats::complete.cases(data[, c(id_col, value_col)])
  values <- data[[value_col]][complete]
  # TODO warn if number of states is less than n_states
  discretized <- discretization_funs[[method]](values, n_states, ...)
  states <- discretized$states
  output <- discretized$output
  state_col <- paste0(value_col, "_state")
  data[[state_col]] <- NA
  data[[state_col]][complete] <- states
  data[[state_col]] <- factor(
    data[[state_col]],
    levels = seq_len(n_states),
    labels = labels
  )
  stats <- compute_state_statistics(data, id_col, value_col, state_col)
  timed_data <- NULL
  if (missing(order_col)) {
    timed_data <- data |>
      dplyr::group_by(!!rlang::sym(id_col)) |>
      dplyr::mutate(.time = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::arrange(!!rlang::sym(id_col), .time)
  } else {
    timed_data <- data |>
      dplyr::group_by(!!rlang::sym(id_col)) |>
      dplyr::arrange(!!rlang::sym(id_col), !!rlang::sym(order_col)) |>
      dplyr::mutate(.time = dplyr::row_number()) |>
      dplyr::ungroup()
  }
  wide_data <- tidyr::pivot_wider(
    timed_data,
    id_cols = !!rlang::sym(id_col),
    names_from = !!rlang::sym(".time"),
    names_prefix = "T",
    values_from = !!rlang::sym(state_col),
    unused_fn = unused_fn
  )
  data$.id <- NULL
  wide_data$.id <- NULL
  time_cols <- grepl("^T[0-9]+$", names(wide_data), perl = TRUE)
  sequence_data <- wide_data[, time_cols]
  meta_data <- wide_data[, !time_cols]
  structure(
    list(
      long_data = timed_data,
      sequence_data = sequence_data,
      meta_data = meta_data,
      statistics = stats
    ),
    type = "timeseries",
    id_col = id_col,
    value_col = value_col,
    state_col = state_col,
    time_col = ifelse_(missing(order_col), ".time", order_col),
    output = output,
    class = "tna_data"
  )
}

#' Calculate comprehensive statistics for states
#'
#' @inheritParams prepare_ts
#' @param state_col A `character` string naming the column that contains the
#' state information.
#' @return A `list` of global and local statistics.
#' @noRd
compute_state_statistics <- function(data, id_col, value_col, state_col) {
  # For R CMD Check
  group_size_ <- NULL
  global <- data |>
    dplyr::group_by(!!rlang::sym(state_col)) |>
    dplyr::summarize(
      freq = dplyr::n(),
      prop = dplyr::n() / nrow(data),
      mean = mean(!!rlang::sym(value_col)),
      median = stats::median(!!rlang::sym(value_col)),
      sd = stats::sd(!!rlang::sym(value_col)),
      min = min(!!rlang::sym(value_col)),
      max = max(!!rlang::sym(value_col)),
      q25 = unname(stats::quantile(!!rlang::sym(value_col), 0.25)),
      q75 = unname(stats::quantile(!!rlang::sym(value_col), 0.75)),
    )
  if (id_col == ".id") {
    local <- global
  } else {
    local <- data |>
      dplyr::group_by(!!rlang::sym(id_col)) |>
      dplyr::mutate(
        group_size_ = dplyr::n()
      ) |>
      dplyr::group_by(!!rlang::sym(id_col), !!rlang::sym(state_col)) |>
      dplyr::summarize(
        freq = dplyr::n(),
        prop = dplyr::n() / dplyr::first(group_size_),
        mean = mean(!!rlang::sym(value_col)),
        median = stats::median(!!rlang::sym(value_col)),
        sd = stats::sd(!!rlang::sym(value_col)),
        min = min(!!rlang::sym(value_col)),
        max = max(!!rlang::sym(value_col)),
        q25 = unname(stats::quantile(!!rlang::sym(value_col), 0.25)),
        q75 = unname(stats::quantile(!!rlang::sym(value_col), 0.75)),
      )
  }
  list(global = global, local = local)
}

# Discretization function wrappers --------------------------------------------

discretization_funs <- list()

discretization_funs$width <- function(x, n_states, ...) {
  r <- range(x)
  k <- n_states + 1L
  breaks <- seq(r[1], r[2], length.out = k)
  breaks[1L] <- breaks[1L] - 1.0
  breaks[k] <- breaks[k] + 1.0
  list(
    states = cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE),
    output = breaks
  )
}

discretization_funs$quantile <- function(x, n_states, ...) {
  k <- n_states + 1L
  probs <- seq(0, 1, length.out = k)
  breaks <- stats::quantile(x, probs = probs)
  breaks[1L] <- breaks[1L] - 1.0
  breaks[k] <- breaks[k] + 1.0
  list(
    states = cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE),
    output = breaks
  )
}

discretization_funs$kde <- function(x, n_states, ...) {
  stopifnot_(
    requireNamespace("pracma", quietly = TRUE),
    "Please install the {.pkg pracma} package
     to use discretization based on kernel density estimation."
  )
  dots <- list(...)
  is_arg <- names(dots) %in% methods::formalArgs(pracma::findpeaks)
  density_args <- dots[!is_arg]
  density_args$x <- x
  dens <- do.call(stats::density, args = density_args)
  findpeaks_args <- dots[is_arg]
  findpeaks_args$npeaks <- n_states - 1L
  findpeaks_args$x <- -dens$y
  valleys <- do.call(pracma::findpeaks, args = findpeaks_args)
  breaks <- sort(unique(dens$x[valleys[, 2L]]))
  breaks <- c(min(x) - 1.0, breaks, max(x) + 1.0)
  list(
    states = cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE),
    output = breaks
  )
}

discretization_funs$gaussian <- function(x, n_states, ...) {
  stopifnot_(
    requireNamespace("mclust", quietly = TRUE),
    "Please install the {.pkg mclust} package
     to use gaussian mixture-based discretization."
  )
  model <- mclust::Mclust(
    data = x,
    G = n_states,
    modelNames = "V",
    verbose = FALSE
  )
  ord <- order(model$parameters$mean)
  list(
    states = ord[model$classification],
    output = model
  )
}

discretization_funs$kmeans <- function(x, n_states, ...) {
  km <- stats::kmeans(x, centers = n_states, ...)
  ord <- order(km$centers)
  list(
    states = ord[km$cluster],
    output = km
  )
}


# #' Apply rolling functions
# #'
# #' @param x A vector.
# #' @param width Window width.
# #' @param fun The function to apply.
# #' @param ... Arguments passed to `fun`.
# #' @noRd
# roll <- function(x, width, fun, ...) {
#   n <- length(x)
#   m <- n - width + 1L
#   out <- vector(mode = mode(x), length = m)
#   for (i in seq_len(m)) {
#     w <- seq(i, i + width - 1L)
#     out[i] <- fun(x[w], ...)
#   }
#   out
# }


# Trend -------------------------------------------------------------------

#' Compute trend classification based on various metrics
#'
#' Calculates rolling metrics for a time series and classifies trends
#' as ascending, descending, flat, or turbulent.
#'
#' @export
#' @param data A `ts` object or a `numeric` vector
#' representing the time series.
#' @param window_size An `integer`, window size for metric calculation.
#' If missing, uses adaptive sizing:
#' `max(3, min(length(data), round(length(data)/10)))`.
#' @param method A `character` string, method for trend calculation. Options:
#'   `"slope"` (default), `"ar1"`, `"growth_factor"`.
#' @param slope A `character` string, method for slope calculation if
#'   `method = "slope"`. Options: `"ols"` (ordinary least squares),
#'   `"robust"` (Theil-Sen estimator), `"spearman"`
#'   (Spearman rank correlation based), `"kendall"` (Kendall's tau based).
#'   Default: `"robust"`.
#' @param epsilon A `numeric` value, threshold for defining flat trends based
#'   on the metric value. For `method = "slope"` and `"ar1"`, values between
#'   `(-epsilon, +epsilon)` are considered flat.
#'   For `"growth_factor"`, values between
#'   (1-epsilon) and (1+epsilon) are considered flat. Default: 0.05.
#' @param turbulence_threshold A `numeric` value, baseline threshold for
#'   classifying a segment as "turbulent". Based on a custom combined volatility
#'   metric (CV + 0.5 * range factor of rolling metric values).
#'   Default: 5.
#' @param flat_to_turbulent_factor A `numeric` value, a multiplier for
#'   `turbulence_threshold` when assessing if an already "flat" segment should
#'   be reclassified as "turbulent". A value > 1 makes "flat" trends more
#'   resistant to becoming "turbulent". Default: 1.5.
#' @param align A `character` string, alignment of the window. Options:
#'   "center" (default), "right", "left". The calculated metric is assigned to
#'   the center, rightmost, or leftmost point of the window respectively.
#'
#' @return A data.frame containing:

#' * `data`: The original time series data (as `numeric`).
#' * `time_index`: The time index for the data.
#' * `metric_values`: Calculated metric values for each point where calculable.
#' * `trend_codes`: Character vector of trend classifications: "ascending",
#'     "descending", "flat", "turbulent", "Missing_Data", or "Initial".
#'
#' @details Computes rolling metrics. Trend classifications
#' ("ascending", "descending", "flat") are first determined using `epsilon`.
#' Then, a "turbulent" classification can override these if the rolling
#' volatility of the metric exceeds a dynamically adjusted turbulence threshold.
#' For segments initially classified as "flat", this threshold is
#' `turbulence_threshold * flat_to_turbulent_factor`,
#' making them more stable against reclassification as "turbulent" due to minor
#' noise.
#'
#' @examples
#' set.seed(123)
#' x <- cumsum(rnorm(200)) # Longer series to see more varied trends
#' # Using a slightly larger epsilon to catch more "flat" regions
#' trend <- compute_trend(
#'   x, window = 15, method = "slope",
#'   slope = "ols", epsilon = 0.1,
#'   turbulence_threshold = 5, flat_to_turbulent_factor = 2
#' )
#'
compute_trend <- function(data, window, method = "slope", slope = "robust",
                          epsilon = 0.05, turbulence_threshold = 5,
                          flat_to_turbulent_factor = 1.5, align = "center") {
  stopifnot_(
    is.numeric(data) || stats::is.ts(data),
    "Argument {.arg} data must be a {.cls numeric} vector
    or a {.cls ts} object."
  )
  method <- check_match(method, c("slope", "growth_factor"))
  slope <- check_match(slope, c("ols", "robust", "spearman", "kendall"))
  align <- check_match(align, c("center", "right", "left"))
  check_values(epsilon, type = "numeric")
  check_values(turbulence_threshold, type = "numeric")
  check_values(flat_to_turbulent_factor, type = "numeric")
  n <- length(data)
  window <- ifelse_(missing(window), max(3, min(n, round(n / 10))), window)
  stopifnot_(
    window > 2 && window < n,
    "Argument {.arg window} must be between 2 and {n}
    (the number of observations)."
  )

  time_idx <- ifelse_(stats::is.ts(data), stats::time(data), seq_along(data))
  # TODO try
  data <- as.numeric(data)

  metric_values <- rep(NA_real_, n)
  left <- (window - 1L) %/% 2
  right <- window - 1L - left
  start <- 1L
  end <- n
  if (align == "center") {
    start <- 1 + left;
    end <- n - right
  } else if (align == "right") {
    start <- window_size
  } else {
    end <- n - window_size + 1
  }

  get_window <- function(align, window, index, left, right) {
    switch(align,
      `center` = seq(index - left, index + right),
      `left` = seq(index, index + window - 1L),
      `right` = seq(index - window + 1L, index),
    )
  }

  metric_values <- rep(NA, n)
  window_method <- ifelse_(method == "slope", slope, method)
  data_idx <- seq_len(window)
  for (i in seq(start, end)) {
    window_idx <- get_window(align, window, i, left, right)
    window_data <- data[window_idx]
    window_time <- time_idx[window_idx]
    metric <- try_(
      metric_funs[[window_method]](
        x = window_time,
        y = window_data
      )
    )
    if (!inherits(metric, "try-error") && is.finite(metric)) {
      metric_values[i] <- metric
    }
  }

  if (align == "center" && sum(!is.na(metric_values)) > 0L) {
    first_valid <- min(which(!is.na(metric_values)))
    if (first_valid > 1L) {
      metric_values[seq(1L, first_valid - 1L)] <- metric_values[first_valid]
    }
    last_valid <- max(which(!is.na(metric_values)))
    if (last_valid < n) {
      metric_values[seq(last_valid + 1L, n)] <- metric_values[last_valid]
    }
  }

  trend_codes <- rep("Initial", n)
  data_na <- is.na(data)
  trend_codes[data_na] <- "Missing_Data"
  neutral_val <- ifelse_(method == "growth_factor", 1, 0)
  lower <- neutral_val - epsilon
  upper <- neutral_val + epsilon
  valid <- !is.na(metric_values) & !data_na
  valid_metrics <- metric_values[valid]
  trend_codes[valid] <- ifelse(
    valid_metrics > upper,
    "Ascending",
    ifelse(
      valid_metrics < lower,
      "Descending",
      "Flat"
    )
  )

  volatility_window <- min(max(3, window %/% 2), sum(valid))
  valid_idx <- which(valid)
  n_valid <- length(valid_metrics)
  if (n_valid >= volatility_window) {
    for (i in seq(volatility_window, n_valid)) {
      window_idx <- seq(i - volatility_window + 1L, i)
      window_metric <- valid_metrics[window_idx]
      if (sum(!is.na(window_metric)) < 2L) next
      metric_sd <- stats::sd(window_metric, na.rm = TRUE)
      metric_am <- abs(mean(window_metric, na.rm = TRUE))
      metric_range <- diff(range(window_metric, na.rm = TRUE))
      if (is.na(metric_sd) || is.na(metric_am) || is.na(metric_range)) next
      volatility_cv <- metric_sd / metric_am
      volatility_range_factor <- metric_range / metric_am
      combined_vol <- volatility_cv + 0.5 * volatility_range_factor
      j <- valid_idx[i]
      # Adjust turbulence threshold if current trend is "flat"
      base_trend <- trend_codes[j]
      effective_threshold <- ifelse_(
        base_trend == "Flat",
        turbulence_threshold * flat_to_turbulent_factor,
        turbulence_threshold
      )
      if (base_trend != "Missing_Data" && combined_vol > effective_threshold) {
        trend_codes[j] <- "Turbulent"
      }
    }
  }

  data.frame(
    data = data,
    time_index = time_idx,
    metric_values = metric_values,
    trend_codes = factor(
      trend_codes,
      levels = c(
        "Ascending",
        "Descending",
        "Flat",
        "Turbulent",
        "Missing_Data",
        "Initial"
      )
    )
  )
}


# Metrics -----------------------------------------------------------------

metric_funs <- list()

# metric_funs$ar1 <- function(x, y) {
#   y <- stats::na.omit(y)
#   model <- stats::ar.ols(
#     y, aic = FALSE, order.max = 1, demean = FALSE, intercept = TRUE
#   )
#   model$ar[1L]
# }

metric_funs$growth_factor <- function(x, y) {
  y <- y[!is.na(y)]
  n <- length(y)
  if (n < 2) {
    return(NA_real_)
  }
  y[n] / y[1L]
}

metric_funs$ols <- function(x, y) {
  stats::cov(x, y) / stats::var(x)
}

metric_funs$robust <- function(x, y) {
  n <- length(x)
  s <- numeric(n * (n - 1) %/% 2L)
  idx <- 0L
  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      idx <- idx + 1L
      s[idx] <- (y[j] - y[i]) / (x[j] - x[i])
    }
  }
  stats::median(s, na.rm = TRUE)
}

metric_funs$spearman <- function(x, y) {
  corr <- stats::cor(x, y, method = "spearman", use = "complete.obs")
  corr * stats::sd(y, na.rm = TRUE) / stats::sd(x, na.rm = TRUE)
}

metric_funs$kendall <- function(x, y) {
  corr <- stats::cor(x, y, method = "kendall", use = "complete.obs")
  corr * stats::sd(y, na.rm = TRUE) / stats::sd(x, na.rm = TRUE)
}
