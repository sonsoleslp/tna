#' Build a Transition Network Analysis Model
#'
#' Construct a transition network analysis (TNA) model from sequence data.
#' The function takes a data set of sequence of events or states as input and
#' builds a TNA model. It extracts the edge weights and initial probabilities
#' from the data along with the state labels. THe function also accepts weight
#' matrices and initial state probabilities directly.
#'
#' @export
#' @family core
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
#'   * `"window"` creates transitions between all states within a
#'       sliding window, capturing local relationships
#'       (several sequences together).
#'   * `"reverse"` considers the sequences in reverse order
#'       (resulting in what is called a reply network in some contexts).
#'       The resulting weight matrix is the transpose of the `"frequency"`
#'       option.
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
#' @param inits An optional `numeric` vector of initial state probabilities
#'   for each state. Can be provided only if `x` is a `matrix`. The vector will
#'   be scaled to unity.
#' @param params A `list` of additional arguments for models of specific
#'   `type`. The potential elements of this list are:
#'
#'   * `n_gram`: An `integer` for n-gram transitions specifying the number of
#'       adjacent events. The default value is 2.
#'   * `max_gap`: An `integer` for the gap-allowed transitions specifying the
#'       largest allowed gap size. The default is 1.
#'   * `window_size`: An `integer` for the sliding window transitions
#'       specifying the window size. The default is 2.
#'   * `weighted`: A `logical` value. If `TRUE`, the transitions
#'      are weighted by the inverse of the sequence length. Can be used for
#'      frequency, co-occurrence and reverse model types. The default is
#'      `FALSE`.
#'
#' @param ... Ignored.
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
build_model <- function(x, type = "relative", scaling = character(0L),
                        ...) {
  UseMethod("build_model")
}

#' @export
#' @rdname build_model
build_model.default <- function(x, type = "relative", scaling = character(0L),
                                inits, params = list(), ...) {
  check_missing(x)
  x <- try_(as.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls matrix}."
  )
  build_model.matrix(x, type, scaling, inits, params, ...)
}

#' @export
#'
#' @rdname build_model
build_model.matrix <- function(x, type = "relative", scaling = character(0L),
                               inits, ...) {
  check_missing(x)
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
#' @param cols An `integer`/`character` vector giving the indices/names of the
#' columns that should be considered as sequence data.
#' Defaults to all columns, i.e., `seq(1, ncol(x))`.
build_model.stslist <- function(x, type = "relative", scaling = character(0L),
                                cols = seq(1, ncol(x)), params = list(), ...) {
  check_missing(x)
  check_class(x, "stslist")
  check_range(cols, type = "integer", scalar = FALSE, min = 1L, max = ncol(x))
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  x <- create_seqdata(x, cols)
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
                                   cols = seq(1, ncol(x)),
                                   params = list(), ...) {
  check_missing(x)
  check_class(x, "data.frame")
  check_range(cols, type = "integer", scalar = FALSE, min = 1L, max = ncol(x))
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  x <- create_seqdata(x, cols = cols)
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
build_model.tna_data <- function(x, type = "relative", scaling = character(0),
                                 params = list(), ...) {
  check_missing(x)
  check_class(x, "tna_data")
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  wide <- cbind(x$sequence_data, x$meta_data)
  x <- create_seqdata(wide, cols = seq_len(ncol(x$sequence_data)))
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

# Aliases -----------------------------------------------------------------

#' @export
#' @rdname build_model
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
#' @examples
#' model <- tna(group_regulation)
#'
tna <- function(x, ...) {
  check_missing(x)
  build_model(x = x, type = "relative", ...)
}

#' @export
#' @rdname build_model
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
#' @examples
#' model <- ftna(group_regulation)
#'
ftna <- function(x, ...) {
  build_model(x = x, type = "frequency", ...)
}

#' @export
#' @rdname build_model
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
#' model <- ctna(group_regulation)
#'
ctna <- function(x, ...) {
  build_model(x = x, type = "co-occurrence", ...)
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
      # TODO can inits be missing?
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
#' @param cols An `integer` vector of column indices or a `character` vector
#' of column names.
#' @param alphabet Optional `character` vector of the alphabet.
#' @noRd
create_seqdata <- function(x, cols, alphabet) {
  cols <- ifelse_(
    is.character(cols),
    which(names(x) %in% cols),
    cols
  )
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
  x[, cols] <- as.data.frame(
    lapply(
      x[, cols],
      function(y) {
        as.integer(replace(y, which(!y %in% alphabet), NA))
      }
    )
  )
  structure(
    x,
    class = "data.frame",
    alphabet = alphabet,
    labels = labels,
    colors = colors,
    cols = cols
  )
}

#' Compute Edge Weights from Sequence Data
#'
#' @param x A data object from `create_seqdata()`
#' @param type The type of transition network model to build.
#' @param scaling The scaling methods to apply to the weights.
#' @param params A list of parameters for the transition model.
#' @param transitions Should the individual-level transitions also be returned?
#' Defaults to `FALSE`.
#' @noRd
initialize_model <- function(x, type, scaling, params, transitions = FALSE) {
  alphabet <- attr(x, "alphabet")
  labels <- attr(x, "labels")
  cols <- attr(x, "cols")
  m <- as.matrix(x[, cols])
  a <- length(alphabet)
  inits <- factor(m[, 1L], levels = seq_len(a), labels = alphabet)
  inits <- as.vector(table(inits))
  trans <- compute_transitions(m, a, type, params)
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
  n <- nrow(m)
  p <- ncol(m)
  idx <- seq_len(n)
  trans <- array(0L, dim = c(n, a, a))
  seq_lengths <- apply(m, 1L, function(x) sum(!is.na(x)))
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
  }
  else if (type == "co-occurrence") {
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
  } else if (type == "window") {
    window_size <- params$window_size %||% 2L
    for (i in seq_len(p - window_size + 1L)) {
      for (j in seq(i, i + window_size - 2L)) {
        from <- m[, j]
        for (k in seq(j + 1L, i + window_size - 1L)) {
          to <- m[, k]
          any_na <- is.na(from) | is.na(to)
          new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
          trans[new_trans] <- trans[new_trans] + 1L
        }
      }
    }
  }
  trans
}

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
  if (type == "relative") {
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
#' @param x A `tna` object or a weight `matrix`.
#' @noRd
nodes <- function(x) {
  if (is_tna(x)) {
    dim(x$weights)[2L]
  } else {
    ncol(x)
  }
}
