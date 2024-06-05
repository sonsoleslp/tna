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
#'   the Markov model or a tidy graph (`tbl_graph`) depicting the transition
#'   network. The edge data should have a column named `weight` giving the
#'   transition probabilities. The probabilities can be unnormalized.
#' @param inits A `numeric` vector of initial state probabilities for each
#'   state. Should be provided only if `x` is a tidy graph. The elements will
#'   be scaled such that they sum to one.
#' @param ... Additional arguments passed to [seqHMM::build_mm()] when `x` is
#'   a `stslist` object.
#' @return An object of class `tna` which is a tidy graph with an additional
#'   node attribute `inits` describing the initial state probabilities
#' @examples
#' tna_model <- build_tna(engagement)
#' print(tna_model)
#'
build_tna <- function(x, ...) {
  UseMethod("build_tna")
}

#' @export
#' @rdname build_tna
build_tna.tbl_graph <- function(x, inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    !missing(inits),
    "Argument {.arg inits} is missing."
  )
  stopifnot_(
    tidygraph::with_graph(x, tidygraph::graph_is_directed()),
    "Argument {.arg x} must be a directed graph."
  )
  edges <- data.frame(tidygraph::activate(x, "edges"))
  stopifnot_(
    !is.null(edges$weight),
    "Argument {.arg x} must be have edge weights."
  )
  stopifnot_(
    all(edges$weight >= 0),
    "All edge weights of {.arg x} must be non-negative."
  )
  x <- tidygraph::activate(x, "edges") %>%
    dplyr::group_by(!!rlang::sym("from")) %>%
    dplyr::mutate(
      weight = !!rlang::sym("weight") / sum(!!rlang::sym("weight"))
    ) %>%
    dplyr::ungroup()
  nodes <- data.frame(tidygraph::activate(x, "nodes"))
  n <- nrow(nodes)
  stopifnot_(
    length(inits) >= n,
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
  if (length(inits) > n) {
    warning_(
      c(
        "Argument {.arg inits} contains more values than the number of states.",
        `i` = "Only the first {n} values will be used."
      )
    )
    inits <- inits[seq_len(n)]
  }
  structure(
    tidygraph::activate(x, "nodes") %>%
      dplyr::mutate(inits = inits),
    class = c("tna", "tbl_graph", "igraph")
  )
}

#' @export
#' @rdname build_tna
build_tna.stslist <- function(x, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  mkvmodel <- seqHMM::build_mm(x, ...)
  build_tna(
    x = tidygraph::as_tbl_graph(mkvmodel$transition_probs),
    inits = mkvmodel$initial_probs
  )
}
