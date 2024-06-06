#' Build a Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model from a
#' given sequence. It takes a sequence of events or states and builds a Markov
#' model using [seqHMM::build_mm()]. It extracts the transition probabilities
#' and initial probabilities from the model and stores them in a `list` along
#' with the state labels. Additionally, it creates a transition matrix with
#' zero diagonal entries (without loops). Also accepts matrices of transition
#' probabilities and initial state probabilities directly.
#'
#' @export
#' @rdname build_tna
#' @param x A `stslist` object created with [seqHMM::seqdef()]
#'   describing a sequence of events or states to be used for building
#'   the Markov model or a `matrix` of transition probabilities with column
#'   names describing the states. If `x` is a matrix, it is assumed that the
#'   element on row `i` and column `j` is the transition probability (or weight)
#'   from state `i` to state `j`.
#' @param inits An optional `numeric` vector of initial state probabilities
#'   for each state. Can be provided only if `x` is a `matrix`. The vector will
#'   be scaled to unity.
#' @param ... Additional arguments passed to [seqHMM::build_mm()] when `x` is
#'   a `stslist` object.
#' @return An object of class `tna` which is a `list` containing the
#'   following elements:
#'
#'   * `adjacency`\cr The adjacency matrix of the model with transition
#'     probabilities as the weights.
#'   * `inits`\cr A vector of initial state probabilities. For matrcies,
#'     this element will be `NULL` if `inits` is not directly provided
#'   * `labels`\cr A `character` vector of the state labels, or `NULL` if there
#'     are no labels.
#'   * `colors`\cr A `character` vector of the state colors, or `NULL`.
#'
#' @examples
#' tna_model <- build_tna(engagement)
#' print(tna_model)
#'
build_tna <- function(x, ...) {
  UseMethod("build_tna")
}

#' @export
#' @rdname build_tna
build_tna.default <- function(x, inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  x <- try_(as.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls matrix}."
  )
  build_tna.matrix(x, inits, ...)
}

#' @export
#' @rdname build_tna
build_tna.matrix <- function(x, inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  x <- try_(data.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to {.cls numeric}."
  )
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
      "Elements of {.arg inits} must be non-negative."
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
    }
  }
  build_tna_(x, inits)
}

#' @export
#' @rdname build_tna
build_tna.stslist <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  mkvmodel <- seqHMM::build_mm(x, ...)
  build_tna_(
    transit_probs = mkvmodel$transition_probs,
    initial_probs = mkvmodel$initial_probs,
    colors = attr(x, "cpal")
  )
}

#' Build a Transition Network Analysis object
#'
#' @param transit_probs A `matrix` of transition probabilities.
#' @param initial_probs A `matrix` of initial state probabilities.
#' @param colors A `character` vector of color values to use for the states.
#' @return A `tna` object.
#' @noRd
build_tna_ <- function(transit_probs, initial_probs, colors) {
  structure(
    list(
      adjacency = transit_probs,
      inits = onlyif(!missing(initial_probs), initial_probs),
      labels = colnames(transit_probs),
      colors = onlyif(!missing(colors), colors)
    ),
    class = "tna"
  )
}
