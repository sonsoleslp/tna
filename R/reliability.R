#' Assess Model Reliability
#'
#' Performs reliability analysis and outputs a concise summary of key
#' metrics. The results can also be visualized.
#'
#' @export
#' @family validation
#' @param x A `tna` object.
#'   Grouped models with scaled edge weights, and their networks, are not
#'   resampled (an error of class `tna_error_scaling`).
#' @param types A `character` vector giving the model types to fit.
#'   See [build_model()] for available options.
#' @param split A `numeric` value between `0` and `1` specifying the proportion
#'   of data for the split. The default is `0.5` for an even split.
#' @param iter An `integer` specifying number of iterations (splits). The
#'   default is `1000`.
#' @param scaling A `character` string naming the scaling applied to the
#'   weights of the two halves before they are compared. The default is
#'   `"none"`. See [compare()] for the options.
#' @param actor An optional actor identifier for sequences nested in actors:
#'   a `character` string naming a non-sequence column of the data the model
#'   was built from, or a vector with one identifier per sequence. When given,
#'   the actors are split, so that all sequences of an actor fall in the same
#'   half, and `split` is the proportion of actors. The default `NULL` splits
#'   single sequences.
#' @param ... Ignored.
#' @return A `tna_reliability` object, which is a `list` with two tibbles:
#'
#'   * `metrics`: One row per split and comparison metric, with the columns
#'     `category`, `metric`, and `value` (and `model_type` when several
#'     `types` are given).
#'   * `summary`: One row per metric with the `mean`, `sd`, `median`, `min`,
#'     `max`, and quartiles (`q25`, `q75`) of the metric over the splits.
#'
#'   [summary.tna_reliability()] returns all metrics with their category as
#'   a tidy table.
#' @examples
#' # Small number of iterations for CRAN
#' model <- tna(engagement)
#' rel <- reliability(model, iter = 20)
#'
reliability <- function(x, ...) {
  UseMethod("reliability")
}

#' @export
#' @rdname reliability
reliability.tna <- function(x, types = "relative", split = 0.5, iter = 1000,
                            scaling = "none", actor = NULL, ...) {
  check_tna_seq(x)
  check_not_scaled_group(x, "The reliability analysis")
  check_actor(actor)
  check_values(iter, strict = TRUE)
  check_range(split, lower = 0.0, upper = 1.0)
  d <- x$data
  type <- attr(x, "type")
  scaling_ <- attr(x, "scaling")
  params <- attr(x, "params")
  model <- initialize_model(d, type, scaling_, params, transitions = TRUE)
  trans <- model$trans
  weights_a <- model$weights
  weights_b <- model$weights
  alphabet <- attr(d, "alphabet")
  n <- nrow(d)
  m <- length(types)
  a <- length(alphabet)
  idx <- seq_len(n)
  # Split sequences, or actors with all of their sequences
  units <- ifelse_(is.null(actor), as.list(idx), actor_units(x, actor))
  n_sample <- round(length(units) * split)
  res <- vector(mode = "list", length = length(types))
  for (i in seq_len(m)) {
    check_model_type(types[i])
    comparisons <- vector(mode = "list", length = iter)
    for (j in seq_len(iter)) {
      half <- sample(seq_along(units), n_sample, replace = FALSE)
      idx_a <- unlist(units[half], use.names = FALSE)
      idx_b <- unlist(
        units[setdiff(seq_along(units), half)],
        use.names = FALSE
      )
      trans_a <- trans[idx_a, , , drop = FALSE]
      trans_b <- trans[idx_b, , , drop = FALSE]
      weights_a[] <- compute_weights(trans_a, types[i], scaling_, a)
      weights_b[] <- compute_weights(trans_b, types[i], scaling_, a)
      comparisons[[j]] <- compare(
        x = weights_a,
        y = weights_b,
        scaling = scaling,
        measures = character(0),
        network = FALSE
      )$summary_metrics
    }
    metrics <- dplyr::bind_rows(comparisons)
    metrics$model_type <- types[i]
    res[[i]] <- metrics
  }
  metrics <- dplyr::bind_rows(res)
  metrics_summary <- metrics |>
    dplyr::group_by(!!rlang::sym("model_type"), !!rlang::sym("metric")) |>
    dplyr::summarize(
      mean = mean(!!rlang::sym("value"), na.rm = TRUE),
      sd = stats::sd(!!rlang::sym("value"), na.rm = TRUE),
      median = stats::median(!!rlang::sym("value"), na.rm = TRUE),
      min = min(!!rlang::sym("value"), na.rm = TRUE),
      max = max(!!rlang::sym("value"), na.rm = TRUE),
      q25 = stats::quantile(!!rlang::sym("value"), 0.25, na.rm = TRUE),
      q75 = stats::quantile(!!rlang::sym("value"), 0.75, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  if (m == 1) {
    metrics$model_type <- NULL
    metrics_summary$model_type <- NULL
  }
  structure(
    list(
      metrics = metrics,
      summary = metrics_summary
    ),
    class = "tna_reliability"
  )
}

