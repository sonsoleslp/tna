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
#' @rdname tna
#' @param x An `stslist` object describing a sequence of events or states to
#'   be used for building the Markov model or a `matrix` of weights with column
#'   names describing the states. If `x` is a matrix, it is assumed that the
#'   element on row `i` and column `j` is the weight of the edge representing
#'   the transition from state `i` to state `j`. The argument `x` also accepts
#'   a `data.frame` object in wide format
#'.  (each column is a timepoint with no extra columns).
#' @param type A `character` string describing the weight matrix type.
#'   Currently supports `"relative"` for relative frequencies
#'   (probabilities, the default), `"scaled"` for frequencies scaled to the
#'   unit interval, `"ranked"` for ranks of the weights scaled to the unit
#'   interval, and `"absolute"` for frequencies.
#' @param inits An optional `numeric` vector of initial state probabilities
#'   for each state. Can be provided only if `x` is a `matrix`. The vector will
#'   be scaled to unity.
#' @param ... Ignored.
#' @return An object of class `tna` which is a `list` containing the
#'   following elements:
#'
#'   * `weights`\cr An adjacency `matrix` of the model (weight matrix).
#'   * `inits`\cr A `numeric` vector of initial values for each state.
#'     For `matrix` type `x`, this element will be `NULL` if `inits` is not
#'     directly provided
#'   * `labels`\cr A `character` vector of the state labels, or `NULL` if
#'     there are no labels.
#'   * `data`\cr The original sequence data that has been converted to an
#'     internal format used by the package when `x` is a `stslist` or a
#'     `data.frame` object. Otherwise `NULL`.
#'
#' @examples
#' model <- tna(engagement)
#' print(model)
#'
tna <- function(x, ...) {
  UseMethod("tna")
}

#' @export
#' @rdname tna
tna.default <- function(x, type = "relative", inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  x <- try_(as.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls matrix}."
  )
  tna.matrix(x, type, inits, ...)
}

#' @export
#' @rdname tna
tna.matrix <- function(x, type = "relative", inits, ...) {
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
    }
    names(inits) <- colnames(x)
  }
  type <- check_tna_type(type)
  tna_(
    weights = x,
    inits = inits,
    labels = colnames(x),
    type = type
  )
}

#' @export
#' @rdname tna
tna.stslist <- function(x, type = "relative", ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  type <- check_tna_type(type)
  x <- create_seqdata(x)
  model <- markov_model(x, type, ...)
  tna_(
    weights = model$weights,
    inits = model$inits,
    labels = attr(x, "labels"),
    type = type,
    data = x
  )
}

#' @export
#' @rdname tna
tna.data.frame <- function(x, type = "relative", ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  type <- check_tna_type(type)
  x <- create_seqdata(x)
  model <- markov_model(x, type, ...)
  tna_(
    weights = model$weights,
    inits = model$inits,
    labels = model$labels,
    type = type,
    data = x
  )
}

# TODO tna_cluster
#' #' @export
#' #' @rdname tna
#' tna.mhmm <- function(x, type = "relative", ...) {
#'   stopifnot_(
#'     !missing(x),
#'     "Argument {.arg x} is missing."
#'   )
#'   stopifnot_(
#'     attr(x, "type") == "mmm",
#'     "Argument {.arg x} must be a mixed Markov model fit."
#'   )
#'   check_tna_type(type)
#'   clusters <- names(x$transition_probs)
#'   n_clust <- length(clusters)
#'   seq <- vector(mode = "list", length = n_clust)
#'   cluster_assignment <- summary(x)$most_probable_cluster
#'   for (i in clusters) {
#'     seq[[i]] <- x$observations[cluster_assignment == i, ]
#'   }
#'   tna_(
#'     weights = x$transition_probs,
#'     inits = x$initial_probs,
#'     labels = attr(x$observations, "labels"),
#'     type = type,
#'     seq = seq
#'   )
#' }

#' Build a Transition Network Analysis object
#'
#' @param weights A `matrix` of edge weights.
#' @param type A `character` string defining the network type.
#' @param inits A `numeric` vector of initial state probabilities.
#' @param labels A `character` vector of state labels.
#' @param data A `tna_seqdata` object when `weights` is
#'   created from sequence data.
#' @return A `tna` object.
#' @noRd
tna_ <- function(weights, type, inits = NULL, labels = NULL, data = NULL) {
  structure(
    list(
      weights = weights,
      # TODO can inits be missing?
      inits = onlyif(!missing(inits), inits),
      labels = labels,
      data = data
    ),
    type = type,
    class = "tna"
  )
}

#' Build and Visualize a Network with Edge Betweenness
#'
#' This function builds a network from a transition matrix in a `tna` object
#' and computes edge betweenness for the network. Optionally, it visualizes the
#' network using the `qgraph` package, with the edge thickness representing the
#' edge betweenness values.
#'
#' @export
#' @param x A `tna` object containing transition matrices and
#' associated metadata.
#' @param ... Ignored.
#'
#' @details
#' The function first converts the transition matrix for the specified cluster
#' into a directed graph using the `igraph` package. It then calculates the
#' edge betweenness of the graph, which is a measure of how often an edge lies
#' on the shortest paths between pairs of nodes.
#'
#' If `plot = TRUE`, the function uses `qgraph` to visualize the network,
#' where edge thickness is proportional to edge betweenness, node colors are
#' derived from the `tna` object, and `Pie` values from the `tna` object are
#' displayed on the nodes.
#'
#' The layout of the network can be customized via the `layout` parameter,
#' which can either be a predefined layout from `qgraph` or a user-specified
#' matrix of node positions.
#'
#' @return A `tna` object where edge betweenness represents the edge weights.
#'
#' @examples
#' model <- tna(group_regulation)
#' betweenness_network(model)
#'
betweenness_network <- function(x, ...) {
  UseMethod("betweenness_network")
}

#' @rdname betweenness_network
#' @export
betweenness_network.tna <- function(x, ...) {
  check_tna(x)
  weights <- x$weights
  g <- as.igraph(x)
  betweenness <- igraph::edge_betweenness(g, directed = TRUE)
  weights[weights > 0] <- betweenness
  tna_(
    weights = weights,
    type = "betweenness",
    inits = x$inits,
    labels = x$labels,
    data = x$data
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

#' Build a Markov Model from Sequence Data
#'
#' @param x A data object from `create_seqdata()`
#' @param type The type of transition network model to build.
#' @param transitions Should the individual-level transitions also be returned?
#' Defaults to `FALSE`.
#' @noRd
markov_model <- function(x, type = "relative", transitions = FALSE) {
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
  for (i in seq_len(p - 1)) {
    from <- m[, i]
    to <- m[, i + 1L]
    any_na <- is.na(from) | is.na(to)
    new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
    trans[new_trans] <- trans[new_trans] + 1L
  }
  weights <- compute_weights(trans, type, a)
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
#' @param s An `integer`, the number of states.
#' @return A `matrix` of transition probabilities or frequencies,
#' based on `type`.
#' @noRd
compute_weights <- function(transitions, type, s) {
  weights <- apply(transitions, c(2, 3), sum)
  if (type == "relative") {
    weights <- weights / .rowSums(weights, m = s, n = s)
  } else if (type == "scaled") {
    weights[] <- ranger(weights)
  } else if (type == "ranked") {
    weights[] <- (rank(weights, ties.method = "first") - 1) / (s^2 - 1)
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
