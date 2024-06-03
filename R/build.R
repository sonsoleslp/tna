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
#'   element on row `i` and column `j` is the transition probability from
#'   state `i` to state `j`. For matrices, the elements of `x` will be scaled
#'   such that the column sums are all equal to one.
#' @param inits A `numeric` vector of initial state probabilities for each
#'   state. Should be provided only if `x` is a `matrix`. The elements will be
#'   scaled such that they sum to one.
#' @param ... Additional arguments passed to [seqHMM::build_mm()] when `x` is
#'   a `stslist` object.
#' @return An object of class `tna` which is a `list` containing the
#'   following elements:
#'
#'   * `matrix`\cr A matrix of transition probabilities.
#'   * `matrix0`\cr A transition probability matrix without loops
#'     (the diagonal is set to zero).
#'   * `inits`\cr A vector of initial state probabilities.
#'   * `labels`\cr The labels of the states.
#'
#' @examples
#' tna_model <- build_tna(engagement)
#' print(tna_model)
#'
build_tna <- function(x, ...) {
  UseMethod("build_tna", x)
}

#' @export
#' @rdname build_tna
build_tna.matrix <- function(x, inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    !missing(inits),
    "Argument {.arg inits} is missing."
  )
  x <- try_(as.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls matrix}."
  )
  x <- try_(as.numeric(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to {.cls numeric}."
  )
  stopifnot_(
    !is.null(colnames(x)),
    "Argument {.arg x} must have column names."
  )
  nc <- ncol(x)
  stopifnot_(
    nc != nrow(x),
    "Argument {.arg x} must be a square matrix."
  )
  stopifnot_(
    nc < 2L,
    "Argument {.arg x} must have at least two columns."
  )
  x <- x / .colSums(x, nc, nc)
  stopifnot_(
    all(x >= 0),
    "Elements of {.arg x} must be non-negative."
  )
  stopifnot_(
    length(inits) < nc,
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
        "Argument {.arg inits} contains more values than the number of states.",
        `i` = "Only the first {nc} values will be used."
      )
    )
    inits <- inits[seq_len(nc)]
  }
  build_tna_(x, inits)
}

#' @export
#' @rdname build_tna
build_tna.stslist <- function(x, inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  mkvmodel <- seqHMM::build_mm(x, ...)
  build_tna_(mkvmodel$transition_probs, mkvmodel$initial_probs)
}

#' Build a Transition Network Analysis object
#'
#' @param transit_probs A `matrix` of transition probabilities.
#' @param intiial_probs A `matrix` of initial state probabilities.
#' @return A `tna` object
#' @noRd
build_tna_ <- function(transit_probs, initial_probs) {
  transit_probs_loopless <- transit_probs
  diag(transit_probs_loopless) <- 0
  structure(
    list(
      matrix = transit_probs,
      matrix0 = transit_probs_loopless,
      inits = initial_probs,
      labels = colnames(transit_probs)
    ),
    class = "tna"
  )
}
