#' Build a Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model from a
#' given sequence. It takes a sequence of events or states and builds a Markov
#' model. It extracts the transition probabilities
#' and initial probabilities from the model and stores them in a `list` along
#' with the state labels. Additionally, it creates a transition matrix with
#' zero diagonal entries (without loops). Also accepts matrices of transition
#' probabilities and initial state probabilities directly.
#'
#' @export
#' @family core
#' @rdname build_model
#' @param x A `stslist` (from `TraMineR`), `data.frame`, or a `matrix`.
#'   For `stslist` and `data.frame` objects `x`
#'   should describe a sequence of events or states to be used for building the
#'   Markov model.  If `x` is a matrix, it is assumed that the element on row
#'   `i` and column `j` is the weight of the edge representing the transition
#'   from state `i` to state `j`. If `x` is a `data.frame`, then
#'   it must be in wide format (each column is a time point with
#'   no extra columns).
#' @param type A `character` string describing the weight matrix type.
#'   Currently supports the following types:
#'
#'   * `"relative"` for relative frequencies (probabilities, the default)
#'   * `"frequency"` for frequencies.
#'   * `"co-occurrence"` for co-occurrences.
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
#'   * `"rank"` Computes the ranks of the weights using [rank()] with
#'       `ties.method = "average"`.
#'
#' @param inits An optional `numeric` vector of initial state probabilities
#'   for each state. Can be provided only if `x` is a `matrix`. The vector will
#'   be scaled to unity.
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
#' model <- build_model(engagement)
#' print(model)
#'
build_model <- function(x, type = "relative", scaling = character(0L), ...) {
  UseMethod("build_model")
}

#' @export
#' @rdname build_model
build_model.default <- function(x, type = "relative", scaling = character(0L),
                                inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  x <- try_(as.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls matrix}."
  )
  build_model.matrix(x, type, scaling, inits, ...)
}

#' @export
#' @rdname build_model
build_model.matrix <- function(x, type = "relative", scaling = character(0L),
                               inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
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
  x <- scale_weights(x, scaling = scaling)
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
                                ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  x <- create_seqdata(x)
  model <- initialize_model(x, type, scaling, ...)
  build_model_(
    weights = model$weights,
    inits = model$inits,
    labels = attr(x, "labels"),
    type = type,
    scaling = scaling,
    data = x
  )
}

#' @export
#' @rdname build_model
build_model.data.frame <- function(x, type = "relative",
                                   scaling = character(0L), ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  type <- check_model_type(type)
  scaling <- check_model_scaling(scaling)
  x <- create_seqdata(x)
  model <- initialize_model(x, type, scaling, ...)
  build_model_(
    weights = model$weights,
    inits = model$inits,
    labels = model$labels,
    type = type,
    scaling = scaling,
    data = x
  )
}

# Aliases -----------------------------------------------------------------

#' @export
#' @rdname build_model
#' @examples
#' model <- tna(engagement)
#'
tna <- function(x, scaling = character(0L), ...) {
  check_missing(x)
  build_model(x = x, type = "relative", scaling = scaling, ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- ftna(engagement)
#'
ftna <- function(x, scaling = character(0L), ...) {
  build_model(x = x, type = "frequency", scaling = scaling, ...)
}

#' @export
#' @rdname build_model
#' @examples
#' model <- ctna(engagement)
#'
ctna <- function(x, scaling = character(0L), ...) {
  build_model(x = x, type = "co-occurrence", scaling = scaling, ...)
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
#' @return A `tna` object.
#' @noRd
build_model_ <- function(weights, inits = NULL, labels = NULL,
                         type, scaling = character(0L), data = NULL) {
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
    class = "tna"
  )
}

#' Convert Sequence Data to an Internal Format
#'
#' @param x A `data.frame` or a `stslist` object.
#' @noRd
create_seqdata <- function(x) {
  if (inherits(x, "stslist")) {
    alphabet <- attr(x, "alphabet")
    labels <- attr(x, "labels")
    colors <- attr(x, "cpal")
    colors <- ifelse_(
      is.null(colors),
      color_palette(length(labels)),
      colors
    )
    out <- as.data.frame(x)
  } else if (is.data.frame(x)) {
    vals <- sort(unique(unlist(x)))
    alphabet <- labels <- vals[!is.na(vals)]
    colors <- color_palette(length(labels))
    out <- x |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ factor(.x, levels = vals)
        )
      )
  }
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), ~ replace(.x, which(!.x %in% alphabet), NA)
      )
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))
  structure(
    out,
    class = "data.frame",
    alphabet = alphabet,
    labels = labels,
    colors = colors
  )
}

#' Compute Edge Weights from Sequence Data
#'
#' @param x A data object from `create_seqdata()`
#' @param type The type of transition network model to build.
#' @param type The type of scaling to apply to the weights
#' @param transitions Should the individual-level transitions also be returned?
#' Defaults to `FALSE`.
#' @noRd
initialize_model <- function(x, type, scaling, transitions = FALSE) {
  alphabet <- attr(x, "alphabet")
  labels <- attr(x, "labels")
  m <- as.matrix(x)
  n <- nrow(m)
  p <- ncol(m)
  a <- length(alphabet)
  idx <- seq_len(n)
  trans <- array(0L, dim = c(n, a, a))
  inits <- factor(m[, 1L], levels = seq_len(a), labels = alphabet)
  inits <- as.vector(table(inits))
  if (type == "co-occurrence") {
    for (i in seq_len(p - 1)) {
      for (j in seq(i + 1, p)) {
        from <- m[, i]
        to <- m[, j]
        any_na <- is.na(from) | is.na(to)
        new_trans <- rbind(
          cbind(idx, from, to)[!any_na, , drop = FALSE],
          cbind(idx, to, from)[!any_na, , drop = FALSE]
        )
        trans[new_trans] <- trans[new_trans] + 1L
      }
    }
  } else {
    for (i in seq_len(p - 1)) {
      from <- m[, i]
      to <- m[, i + 1L]
      any_na <- is.na(from) | is.na(to)
      new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
      trans[new_trans] <- trans[new_trans] + 1L
    }
  }
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

#' Compute Network Weights Based On TNA Type
#'
#' @param transitions An `array` of the individual-level transitions.
#' @param type Type of the transition network as a `character` string.
#' @param scaling Scalings to apply as a `character` vectors.
#' @param s An `integer`, the number of states.
#' @return A `matrix` of transition probabilities or frequencies,
#' based on `type`.
#' @noRd
compute_weights <- function(transitions, type, scaling, s) {
  weights <- apply(transitions, c(2, 3), sum)
  if (type == "relative") {
    rs <- .rowSums(weights, m = s, n = s)
    pos <- which(rs > 0)
    weights[pos, ] <- weights[pos, ] / rs[pos]
    weights[!pos, ] <- NA
  }
  scale_weights(weights, scaling)
}

#' Scale Transition Network Weights
#'
#' @param weights A `matrix` of edge weights
#' @param scaling Scalings to apply as a `character` vector.
#' @noRd
scale_weights <- function(weights, scaling) {
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

#' Get `tna` Node Count
#'
#' @param x A `tna` object.
#' @noRd
nodes <- function(x) {
  dim(x$weights)[2L]
}