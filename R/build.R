#' Build a Transition Network Analysis Model
#'
#' Construct a transition network analysis (TNA) model from sequence data.
#' The function takes a data set of sequence of events or states as input and
#' builds a TNA model. It extracts the edge weights and initial probabilities
#' from the data along with the state labels. The function also accepts weight
#' matrices and initial state probabilities directly.
#'
#' @export
#' @family basic
#' @rdname build_model
#' @param x A `stslist` (from `TraMineR`), `data.frame`, a `matrix`, or
#'   a `tna_data` object (see [prepare_data()]).
#'   For `stslist` and `data.frame` objects `x`
#'   should describe a sequence of events or states to be used for building the
#'   Markov model.  If `x` is a matrix, it is assumed that the element on row
#'   `i` and column `j` is the weight of the edge representing the transition
#'   from state `i` to state `j`. If `x` is a `data.frame`, then
#'   it must be in wide format (see `cols` on how to define columns for the
#'   time points).
#' @param type A `character` string describing the weight matrix type.
#'   Currently supports the following types:
#'
#'   * `"relative"` for relative frequencies (probabilities, the default)
#'   * `"frequency"` for frequencies.
#'   * `"co-occurrence"` for co-occurrences.
#'   * `"n-gram"` for n-gram transitions. Captures higher-order transitions by
#'       considering sequences of n states, useful for identifying longer
#'       patterns.
#'   * `"gap"` allows transitions between non-adjacent states, with
#'       transitions weighted by the gap size.
#'   * `"reverse"` considers the sequences in reverse order
#'       (resulting in what is called a reply network in some contexts).
#'       The resulting weight matrix is the transpose of the `"frequency"`
#'       option.
#'   * `"attention"` aggregates all downstream pairs of states with an
#'       exponential decay for the gap between states. The parameter `lambda`
#'       can be used to control the decay rate (the default is 1)-
#'
#' @param scaling A `character` vector describing how to scale the weights
#'   defined by `type`. When a vector is provided, the scaling options are
#'   applied in the respective order. For example, `c("rank", "minmax")` would
#'   first compute the ranks, then scale them to the unit interval using
#'   min-max normalization. An empty vector corresponds to no scaling.
#'   Currently supports the following options:
#'
#'   * `"minmax"` performs min-max normalization to scale the weights to the
#'       unit interval. Note that if the smallest weight is positive, it
#'       will be zero after scaling.
#'   * `"max"` Multiplies the weights by the reciprocal of the largest weight
#'       to scale the weights to the unit interval. This options preserves
#'       positive ranks, unlike `"minmax"` when all weights are positive.
#'   * `"rank"` Computes the ranks of the weights using [base::rank()] with
#'       `ties.method = "average"`.
#'
#' @param cols An `expression` giving a tidy selection of columns that should
#'   be considered as sequence data. By default, all columns are used.
#' @param params A `list` of additional arguments for models of specific
#'   `type`. The potential elements of this list are:
#'
#'   * `n_gram`: An `integer` for n-gram transitions specifying the number of
#'     adjacent events. The default value is 2.
#'   * `max_gap`: An `integer` for the gap-allowed transitions specifying the
#'     largest allowed gap size. The default is 1.
#'   * `windowed`: Perform the model estimation by window. Supported for
#'     `relative`, `frequency` and `co-occurrence` `type`s.
#'   * `window_size`: An `integer` for the sliding window transitions
#'     specifying the window size. The default is 2.
#'   * `window_type`: A `character` string that defines the window type.
#'     Either `"rolling"` or `"tumbling"`.
#'   * `weighted`: A `logical` value. If `TRUE`, the transitions
#'     are weighted by the inverse of the sequence length. Can be used for
#'     frequency, co-occurrence and reverse model types. The default is
#'      `FALSE`.
#'   * `direction`: A `character` string specifying the direction of attention
#'     for models of `type = "attention"`. The available options are
#'     `"backward"`, `"forward"`, and `"both"`, for backward attention,
#'     forward attention, and bidirectional attention, respectively.
#'     The default is `"forward"`.
#'   * `decay`: A `function` that specifies the decay of the weights between
#'     two time points at a specific distance. The function should take three
#'     arguments: `i`, `j` and `lambda`, where `i` and `j` are `numeric`
#'     vectors of time values, and `lambda` is  a `numeric` value for the
#'     decay rate. The function should return a `numeric` vector of weights.
#'     The default is `function(i, j, lambda) exp(-abs(i - j) / lambda)`.
#'   * `lambda`: A `numeric` value for the decay rate. The default is 1.
#'   * `time`: A `matrix` or a `data.frame` providing the time values
#'     for each sequence and at time index. For `tna_data` objects, this can
#'     also be a logical value, where `TRUE` will use the `time_data` element
#'     of `x` for the time values. `Date` values are converted to `numeric`.
#'   * `duration`: A `matrix` or a `data.frame` providing the
#'     time spent in each state for each sequence and time index.
#'     This is an alternative to `time`.
#'
#' @param concat An `integer` for the number of consecutive sequences
#'   to concatenate. The default is 1 (no concatenation).
#' @param inits An optional `numeric` vector of initial state probabilities
#'   for each state. The vector will be scaled to unity.
#' @param begin_state A `character` string for an additional begin state.
#'   This state is added as the first observation for every sequence to
#'   signify the beginning of the sequence
#' @param end_state A `character` string for an additional end state.
#'   This state is added as the last observation for every sequence to
#'   signify the end of the sequence.
#' @param ... Ignored. For the `build_model` aliases (e.g., `tna`), this
#'   argument matches the actual arguments to `build_model` beside `x`.
#' @return An object of class `tna` which is a `list` containing the
#'   following elements:
#'
#'   * `weights`: An adjacency `matrix` of the model (weight matrix).
#'   * `inits`: A `numeric` vector of initial values for each state.
#'     For `matrix` type `x`, this element will be `NULL` if `inits` is not
#'     directly provided
#'   * `labels`: A `character` vector of the state labels, or `NULL` if
#'     there are no labels.
#'   * `data`: The original sequence data that has been converted to an
#'     internal format used by the package when `x` is a `stslist` or a
#'     `data.frame` object. Otherwise `NULL`.
#'
#' @examples
#' model <- build_model(group_regulation)
#' print(model)
#'
build_model <- function(x, ...) {
  UseMethod("build_model")
}

#' @export
#' @rdname build_model
build_model.default <- function(x, type = "relative", scaling = character(0L),
                                params = list(), inits, ...) {
  check_missing(x)
  check_dots(...)
  x <- try_(as.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls matrix}."
  )
  build_model.matrix(
    x = x,
    type = type,
    scaling = scaling,
    params = params,
    inits = inits
  )
}

#' @export
#' @rdname build_model
build_model.matrix <- function(x, type = "relative", scaling = character(0L),
                               params = list(), inits, ...) {
  check_missing(x)
  check_dots(...)
  x <- try_(data.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to {.cls numeric}."
  )
  check_na(x)
  nc <- ncol(x)
  stopifnot_(
    nc == nrow(x),
    "Argument {.arg x} must be a square {.cls matrix}."
  )
  stopifnot_(
    nc >= 2L,
    "Argument {.arg x} must have at least two columns."
  )
  if (!missing(inits)) {
    stopifnot_(
      length(inits) >= nc,
      "Argument {.arg inits} must provide initial probabilities for all states."
    )
    inits <- try_(as.numeric(inits))
    stopifnot_(
      !inherits(inits, "try-error"),
      "Argument {.arg inits} must be coercible to {.cls numeric}."
    )
    stopifnot_(
      all(inits >= 0),
      "All elements of {.arg inits} must be non-negative."
    )
    if (length(inits) > ncol(x)) {
      warning_(
        c(
          "Argument {.arg inits} contains more values
           than the number of states.",
          `i` = "Only the first {nc} values will be used."
        )
      )
      inits <- inits[seq_len(nc)]
      inits <- inits / sum(inits)
    }
    names(inits) <- colnames(x)
  }
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  x <- check_weights(x, type = type)
  x <- scale_weights(x, type = type, scaling = scaling, a = nc)
  if (is.null(colnames(x))) {
    dimnames(x) <- list(seq_len(nc), seq_len(nc))
  }
  build_model_(
    weights = x,
    inits = inits,
    labels = colnames(x),
    type = type,
    scaling = scaling
  )
}

#' @export
#' @rdname build_model

build_model.stslist <- function(x, type = "relative", scaling = character(0L),
                                cols = tidyselect::everything(),
                                params = list(), concat = 1L,
                                begin_state, end_state, ...) {
  check_missing(x)
  check_dots(...)
  check_class(x, "stslist")
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  cols <- get_cols(rlang::enquo(cols), x)
  x <- create_seqdata(
    x = x,
    cols = cols,
    concat = concat,
    begin_state = begin_state,
    end_state = end_state
  )
  model <- initialize_model(x, type, scaling, params)
  build_model_(
    weights = model$weights,
    inits = model$inits,
    labels = attr(x, "labels"),
    type = type,
    scaling = scaling,
    data = x,
    params = params
  )
}

#' @export
#' @rdname build_model
build_model.data.frame <- function(x, type = "relative",
                                   scaling = character(0L),
                                   cols = tidyselect::everything(),
                                   concat = 1L, params = list(),
                                   begin_state, end_state, ...) {
  check_missing(x)
  check_class(x, "data.frame")
  check_dots(...)
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  cols <- get_cols(rlang::enquo(cols), x)
  params$window_span <- attr(x, "window_span")
  params$window_size <- attr(x, "window_size")
  params$windowed <- !is.null(params$window_size) ||
    !is.null(params$window_span)
  x <- create_seqdata(
    x = x,
    cols = cols,
    concat = concat,
    begin_state = begin_state,
    end_state = end_state
  )
  model <- initialize_model(x, type, scaling, params)
  build_model_(
    weights = model$weights,
    inits = model$inits,
    labels = model$labels,
    type = type,
    scaling = scaling,
    data = x,
    params = params
  )
}

#' @export
#' @rdname build_model
build_model.tna_data <- function(x, type = "relative", scaling = character(0L),
                                 params = list(), concat = 1L,
                                 begin_state, end_state, ...) {
  check_missing(x)
  check_class(x, "tna_data")
  check_dots(...)
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  wide <- cbind(x$sequence_data, x$meta_data)
  if (isTRUE(params$time)) {
    stopifnot_(
      !is.null(x$time_data),
      "No time data available in argument {.arg x}."
    )
    params$time <- as.matrix(
      as.data.frame(
        lapply(
          x$time_data,
          as.numeric
        )
      )
    )
  }
  x <- create_seqdata(
    x = wide,
    cols = names(x$sequence_data),
    concat = concat,
    begin_state = begin_state,
    end_state = end_state
  )
  model <- initialize_model(x, type, scaling, params)
  build_model_(
    weights = model$weights,
    inits = model$inits,
    labels = model$labels,
    type = type,
    scaling = scaling,
    data = x,
    params = params
  )
}

#' @export
#' @rdname build_model
build_model.tsn <- function(x, type = "relative", scaling = character(0L),
                            params = list(), concat = 1L,
                            begin_state, end_state, ...) {
  check_missing(x)
  check_class(x, "tsn")
  check_dots(...)
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  wide <- x |>
    dplyr::select(
      c(!!rlang::sym("id"), !!rlang::sym("state"), !!rlang::sym("time"))
    ) |>
    tidyr::pivot_wider(
      id_cols = "id",
      values_from = !!rlang::sym("state"),
      names_from = !!rlang::sym("time"),
      names_prefix = "T"
    ) |>
    dplyr::select(!(!!rlang::sym("id")))
  x <- create_seqdata(
    x = wide,
    cols = names(wide),
    concat = concat,
    begin_state = begin_state,
    end_state = end_state
  )
  model <- initialize_model(x, type, scaling, params)
  build_model_(
    weights = model$weights,
    inits = model$inits,
    labels = model$labels,
    type = type,
    scaling = scaling,
    data = x,
    params = params
  )
}

#' #' @export
#' #' @rdname build_model
#' build_model.tsn_ews <- function(x, type = "relative", scaling = character(0L),
#'                                 params = list(), concat = 1L,
#'                                 begin_state, end_state, ...) { # nocov start
#'   check_missing(x)
#'   check_class(x, "tsn_ews")
#'   check_dots(...)
#'   type <- check_model_type(type)
#'   scaling <- check_model_scaling(scaling)
#'   id <- attr(x, "id_col")
#'   cls <- attr(x, "classification")
#'   wide <- cls |>
#'     dplyr::select(
#'       c(!!rlang::sym("state"), !!rlang::sym("time"))
#'     ) |>
#'     tidyr::pivot_wider(
#'       values_from = !!rlang::sym("state"),
#'       names_from = !!rlang::sym("time"),
#'       names_prefix = "T"
#'     )
#'   x <- create_seqdata(
#'     x = wide,
#'     cols = seq_len(ncol(wide)),
#'     concat = concat,
#'     begin_state = begin_state,
#'     end_state = end_state
#'   )
#'   model <- initialize_model(x, type, scaling, params)
#'   build_model_(
#'     weights = model$weights,
#'     inits = model$inits,
#'     labels = model$labels,
#'     type = type,
#'     scaling = scaling,
#'     data = x,
#'     params = params
#'   )
#' } # nocov end

# Aliases -----------------------------------------------------------------

#' @export
#' @rdname build_model
#' @examples
#' model <- tna(group_regulation)
#'
tna <- function(x, ...) {
  check_missing(x)
  build_model(x = x, type = "relative", ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- ftna(group_regulation)
#'
ftna <- function(x, ...) {
  build_model(x = x, type = "frequency", ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- ctna(group_regulation)
#'
ctna <- function(x, ...) {
  build_model(x = x, type = "co-occurrence", ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- atna(group_regulation)
#'
atna <- function(x, ...) {
  build_model(x = x, type = "attention", ...)
}

#' @export
#' @rdname build_model
tsn <- function(x, ...) {
  build_model.tsn(x = x, ...)
}

#' Build a Social Network Analysis Model
#'
#' @export
#' @param x A `data.frame` or a `matrix` with three columns: the first two
#' representing the states and the third giving the weights.
#' @param aggregate A `function` to use for aggregating the weights. The
#' default is [sum()].
#' @param ... Additional arguments passed to `aggregate`.
#' @return A `tna` object representing the model.
#' @examples
#' set.seed(123)
#' d <- data.frame(
#'   from = sample(LETTERS[1:4], 100, replace = TRUE),
#'   to = sample(LETTERS[1:4], 100, replace = TRUE),
#'   weight = rexp(100)
#' )
#' model <- sna(d)
#'
sna <- function(x, aggregate = sum, ...) {
  check_missing(x)
  x <- try_(as.data.frame(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls data.frame}."
  )
  check_na(x)
  nc <- ncol(x)
  stopifnot_(
    nc == 3L,
    "Argument {.arg x} must have three columns (from, to, weight)."
  )
  colnames(x) <- c("from", "to", "weight")
  stopifnot_(
    is.function(aggregate),
    "Argument {.arg aggregate} must be a function."
  )
  test_aggregate <- try_(aggregate(stats::runif(3L), ...))
  stopifnot_(
    !inherits(test_aggregate, "try-error") &&
      length(test_aggregate) == 1L &&
      is.numeric(test_aggregate),
    "Argument {.arg aggregate} must be a function that takes a {.cls numeric}
    vector and returns a single {.cls numeric} value."
  )
  x <- x |>
    dplyr::group_by(!!rlang::sym("from"), !!rlang::sym("to")) |>
    dplyr::summarize(weight = aggregate(!!rlang::sym("weight"), ...))
  lab <- unique(unlist(x[, c("from", "to")]))
  n <- length(lab)
  out <- matrix(0.0, n, n, dimnames = list(lab, lab))
  idx <- cbind(match(x$from, lab), match(x$to, lab))
  out[idx] <- x$weight
  build_model_(
    weights = out,
    labels = lab,
    type = "",
    scaling = character(0L)
  )
}

# Internal ----------------------------------------------------------------

#' Build a Transition Network Analysis object
#'
#' @param weights A `matrix` of edge weights.
#' @param inits A `numeric` vector of initial state probabilities.
#' @param labels A `character` vector of state labels.
#' @param type A `character` string defining the network type.
#' @param scaling A `character` string defining the scaling of the weights
#' @param data A `tna_seqdata` object when `weights` is
#'   created from sequence data.
#' @param params A `list` of parameters for computing the transitions
#' @return A `tna` object.
#' @noRd
build_model_ <- function(weights, inits = NULL, labels = NULL,
                         type = NULL, scaling = character(0L), data = NULL,
                         params = NULL) {
  structure(
    list(
      weights = weights,
      inits = onlyif(!missing(inits), inits),
      labels = labels,
      data = data
    ),
    type = type,
    scaling = scaling,
    params = params,
    class = "tna"
  )
}

#' Convert Sequence Data to an Internal Format
#'
#' @param x A `data.frame` or a `stslist` object.
#' @param cols An `character` vector of column names.
#' @param alphabet Optional `character` vector of the alphabet.
#' @param concat How many consecutive sequences should be concatenated?
#' @param begin_state Optional `character` string giving the begin state.
#' @param end_state Optional `character` string giving the end state.
#' @noRd
create_seqdata <- function(x, cols, alphabet, concat = 1L,
                           begin_state, end_state) {
  cols <- which(names(x) %in% cols)
  if (inherits(x, "stslist")) {
    alphabet <- attr(x, "alphabet")
    labels <- attr(x, "labels")
    colors <- ifelse_(
      is.null(attr(x, "cpal")),
      color_palette(length(labels)),
      attr(x, "cpal")
    )
    x <- as.data.frame(x)
  } else if (is.data.frame(x)) {
    if (missing(alphabet)) {
      vals <- sort(unique(unlist(x[, cols])))
      alphabet <- labels <- vals[!is.na(vals)]
    } else {
      labels <- alphabet
    }
    colors <- color_palette(length(labels))
    x[, cols] <- as.data.frame(
      lapply(x[, cols], function(y) factor(y, levels = alphabet))
    )
  }
  x <- as.matrix(
    as.data.frame(
      lapply(
        x[, cols],
        function(y) {
          as.integer(replace(y, which(!y %in% alphabet), NA))
        }
      )
    )
  )
  if (concat > 1L) {
    n <- nrow(x)
    k <- ncol(x)
    m <- n %/% concat
    modulus <- n %% concat
    extra <- modulus > 0L
    x_cc <- matrix(NA_integer_, nrow = m + extra, ncol = concat * ncol(x))
    for (i in seq_len(m)) {
      idx <- seq((i - 1L) * concat + 1L, i * concat)
      x_cc[i, ] <- c(t(x[idx, , drop = FALSE]))
    }
    if (extra) {
      idx <- seq(1L, k)
      for (i in seq_len(modulus)) {
        x_cc[m + 1L, idx] <- x[m * concat + i, , drop = FALSE]
        idx <- idx + k
      }
    }
    x <- x_cc
  }
  if (!missing(begin_state)) {
    x <- cbind(1L, x + 1L)
    alphabet <- c(begin_state, alphabet)
    labels <- c(begin_state, labels)
    colors <- c("darkgray", colors)
  }
  if (!missing(end_state)) {
    last_obs <- max.col(!is.na(x), ties.method = "last")
    new_max <- max(x, na.rm = TRUE) + 1L
    if (any(last_obs == ncol(x))) {
      x <- cbind(x, NA_integer_)
      idx <- cbind(seq_len(nrow(x)), last_obs + 1L)
      x[idx] <- new_max
    }
    alphabet <- c(alphabet, end_state)
    labels <- c(labels, end_state)
    colors <- c(colors, "darkgray")
  }
  structure(
    x,
    class = c("tna_seq_data", "matrix", "array"),
    alphabet = alphabet,
    labels = labels,
    colors = colors
  )
}


#' Compute Edge Weights from Sequence Data
#'
#' @param x A data object from `create_seqdata()`
#' @param type The type of transition network model to build.
#' @param scaling The scaling methods to apply to the weights.
#' @param params A `list` of parameters for the transition model.
#' @param transitions Should the individual-level transitions also be returned?
#' Defaults to `FALSE`.
#' @noRd
initialize_model <- function(x, type, scaling, params, transitions = FALSE) {
  alphabet <- attr(x, "alphabet")
  labels <- attr(x, "labels")
  a <- length(alphabet)
  inits <- factor(x[, 1L], levels = seq_len(a), labels = alphabet)
  inits <- as.vector(table(inits))
  trans <- compute_transitions(x, a, type, params)
  weights <- compute_weights(trans, type, scaling, a)
  inits <- inits / sum(inits)
  names(inits) <- alphabet
  dimnames(weights) <- list(alphabet, alphabet)
  list(
    weights = weights,
    inits = inits,
    labels = labels,
    trans = onlyif(transitions, trans)
  )
}

#' Compute Network Transitions Based on TNA Type
#'
#' @param m A `matrix` of sequences
#' @param a An `integer`, the number of states.
#' @param type Type of the transition network as a `character` string.
#' @param params Parameters for the transition model.
#' @noRd
compute_transitions <- function(m, a, type, params) {
  if (isTRUE(params$windowed)) {
    return(compute_transitions_windowed(m, a, type, params))
  }
  n <- nrow(m)
  p <- ncol(m)
  idx <- seq_len(n)
  trans <- array(0L, dim = c(n, a, a))
  seq_lengths <- .rowSums(!is.na(m), m = n, n = p)
  weight <- ifelse_(isTRUE(params$weighted), 1.0 / seq_lengths, rep(1L, n))
  if (type %in% c("relative", "frequency")) {
    weight <- ifelse_(type == "frequency", weight, rep(1L, n))
    for (i in seq_len(p - 1L)) {
      from <- m[, i]
      to <- m[, i + 1L]
      any_na <- is.na(from) | is.na(to)
      new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
      trans[new_trans] <- trans[new_trans] + weight[!any_na]
    }
  } else if (type == "reverse") {
    for (i in seq_len(p - 1L)) {
      from <- m[, i + 1L]
      to <- m[, i]
      any_na <- is.na(from) | is.na(to)
      new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
      trans[new_trans] <- trans[new_trans] + weight[!any_na]
    }
  } else if (type == "co-occurrence") {
    for (i in seq_len(p - 1L)) {
      for (j in seq(i + 1L, p)) {
        from <- m[, i]
        to <- m[, j]
        any_na <- is.na(from) | is.na(to)
        new_trans <- rbind(
          cbind(idx, from, to)[!any_na, , drop = FALSE],
          cbind(idx, to, from)[!any_na, , drop = FALSE]
        )
        trans[new_trans] <- trans[new_trans] + weight[!any_na]
      }
    }
  } else if (type == "n-gram") {
    n_gram <- params$n_gram %||% 2L
    for (i in seq_len(p - n_gram + 1L)) {
      for (j in seq(i, i + n_gram - 2L)) {
        from <- m[, j]
        to <- m[, j + 1L]
        any_na <- is.na(from) | is.na(to)
        new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
        trans[new_trans] <- trans[new_trans] + 1L
      }
    }
  } else if (type == "gap") {
    max_gap <- params$max_gap %||% 1L
    for (i in seq_len(p - 1L)) {
      max_j <- min(i + max_gap + 1L, p)
      for (j in seq(i + 1L, max_j)) {
        from <- m[, i]
        to <- m[, j]
        any_na <- is.na(from) | is.na(to)
        new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
        trans[new_trans] <- trans[new_trans] + 1.0 / (j - i)
      }
    }
  } else if (type == "attention") {
    lambda <- params$lambda %||% 1.0
    decay <- params$decay %||% function(i, j, lambda) exp(-abs(i - j) / lambda)
    direction <- params$direction %||% "forward"
    stopifnot_(
      is.null(params$time) || is.null(params$duration),
      "Both {.arg time} and {.arg duration} supplied via {.arg params}."
    )
    time <- matrix(rep(seq(1, p), each = n), nrow = n, ncol = p)
    if (!is.null(params$time)) {
      stopifnot_(
        nrow(params$time) == n && ncol(params$time) == p,
        "Argument {.arg params$time} must have the same dimensions as the
         sequence data."
      )
      time <- params$time
    }
    if (!is.null(params$duration)) {
      stopifnot_(
        nrow(params$duration) == n && ncol(params$duration) == p,
        "Argument {.arg params$duration} must have the same dimensions as the
         sequence data."
      )
      time <- cbind(0, t(apply(params$duration, 1L, cumsum))[, -p])
    }
    for (i in seq_len(p)) {
      if (direction %in% c("forward", "both")) {
        if (i < p) {
          for (j in seq(i + 1L, p)) {
            from <- m[, i]
            to <- m[, j]
            any_na <- is.na(from) | is.na(to)
            new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
            d <- decay(time[, i], time[, j], lambda)[!any_na]
            trans[new_trans] <- trans[new_trans] + d
          }
        }
      }
      if (direction %in% c("backward", "both")) {
        if (i > 1) {
          for (j in seq(1L, i)) {
            from <- m[, i]
            to <- m[, j]
            any_na <- is.na(from) | is.na(to)
            new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
            d <- decay(time[, i], time[, j], lambda)[!any_na]
            trans[new_trans] <- trans[new_trans] + d
          }
        }
      }
    }
  }
  trans
}

compute_transitions_windowed <- function(m, a, type, params) {
  n <- nrow(m)
  p <- ncol(m)
  idx <- seq_len(n)
  trans <- array(0L, dim = c(n, a, a))
  seq_lengths <- .rowSums(!is.na(m), m = n, n = p)
  weight <- ifelse_(isTRUE(params$weighted), 1.0 / seq_lengths, rep(1L, n))
  window_size <- params$window_size %||% 2L
  window_span <- params$window_span %||% 1L
  window_size <- window_size * window_span
  divides <- p %% window_size == 0L
  q <- p %/% window_size - 1L * divides
  if (type %in% c("relative", "frequency")) {
    for (i in seq_len(q)) {
      j_idx <- seq(
        (i - 1) * window_size + 1L,
        i * window_size
      )
      for (j in j_idx) {
        from <- m[, j]
        k_idx <- seq(
          i * window_size + 1L,
          min(p, (i + 1) * window_size)
        )
        for (k in k_idx) {
          to <- m[, k]
          any_na <- is.na(from) | is.na(to)
          new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
          trans[new_trans] <- trans[new_trans] + 1L
        }
      }
    }
  } else if (type == "co-occurrence") {
    for (i in seq_len(q + 1)) {
      j_idx <- seq(
        (i - 1) * window_size + 1L,
        min(p, i * window_size)
      )
      for (j in j_idx) {
        from <- m[, j]
        k_idx <- seq(
          (i - 1) * window_size + 1L,
          min(p, i * window_size)
        )
        for (k in k_idx) {
          to <- m[, k]
          any_na <- is.na(from) | is.na(to)
          new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
          trans[new_trans] <- trans[new_trans] + 1L
        }
      }
    }
    # for (i in seq_len(q)) {
    #   for (j in seq(i + 1, q + 1)) {
    #     k_idx <- seq(
    #       (i - 1) * window_size + 1L,
    #       i * window_size
    #     )
    #     for (k in k_idx) {
    #       from <- m[, k]
    #       l_idx <- seq(
    #         (j - 1) * window_size + 1L,
    #         min(p, j * window_size)
    #       )
    #       for (l in l_idx) {
    #         to <- m[, l]
    #         any_na <- is.na(from) | is.na(to)
    #         new_trans <- rbind(
    #           cbind(idx, from, to)[!any_na, , drop = FALSE],
    #           cbind(idx, to, from)[!any_na, , drop = FALSE]
    #         )
    #         trans[new_trans] <- trans[new_trans] + weight[!any_na]
    #       }
    #     }
    #   }
    # }
  }
  trans
}

# Internal function to get all transitions
get_transitions <- function(x) { # nocov start
  m <- x$data
  lab <- x$labels
  n <- nrow(m)
  p <- ncol(m)
  idx <- seq_len(n)
  trans <- vector(mode = "list", length = p - 1L)
  seq_lengths <- .rowSums(!is.na(m), m = n, n = p)
  for (i in seq_len(p - 1L)) {
    from <- m[, i]
    to <- m[, i + 1L]
    any_na <- is.na(from) | is.na(to)
    trans[[i]] <- as.data.frame(
      cbind(idx, from, to)[!any_na, , drop = FALSE]
    )
    names(trans[[i]]) <- c("id", "source", "target")
  }
  out <- dplyr::bind_rows(trans, .id = "time")
  out$source <- lab[out$source]
  out$target <- lab[out$target]
  out$weight <- 1.0
  out
} # nocov end

#' Compute Network Weights Based On TNA Type
#'
#' @param transitions An `array` of the individual-level transitions.
#' @param type Type of the transition network as a `character` string.
#' @param scaling Scaling methods to apply as a `character` vector.
#' @param a An `integer`, the number of states.
#' @return A `matrix` of transition probabilities or frequencies,
#' based on `type`.
#' @noRd
compute_weights <- function(transitions, type, scaling, a) {
  weights <- apply(transitions, c(2, 3), sum)
  scale_weights(weights, type, scaling, a)
}

#' Scale Transition Network Weights
#'
#' @param weights A `matrix` of edge weights
#' @param type Type of the transition network as a `character` string.
#' @param scaling Scaling methods to apply as a `character` vector.
#' @param a An `integer`, the number of states.
#' @noRd
scale_weights <- function(weights, type, scaling, a) {
  if (type == "relative" || "probability" %in% scaling) {
    rs <- .rowSums(weights, m = a, n = a)
    pos <- which(rs > 0)
    weights[pos, ] <- weights[pos, ] / rs[pos]
    weights[!pos, ] <- NA
  }
  for (i in seq_along(scaling)) {
    if (scaling[i] == "minmax") {
      weights[] <- ranger(weights)
    } else if (scaling[i] == "max") {
      weights[] <- weights / max(weights, na.rm = TRUE)
    } else if (scaling[i] == "rank") {
      weights[] <- rank(weights, ties.method = "average")
    }
  }
  weights
}

#' Get Network Node Count
#'
#' @param x A `tna` or a `group_tna` object, or a weight `matrix`.
#' @noRd
nodes <- function(x) {
  if (is_tna(x)) {
    dim(x$weights)[2L]
  } else if (inherits(x, "group_tna")) {
    dim(x[[1L]]$weights)[2L]
  } else {
    ncol(x)
  }
}
