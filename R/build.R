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
#' @rdname build_tna
#' @param x A `stslist` object describing a sequence of events or states to
#'   be used for building
#'   the Markov model or a `matrix` of transition probabilities with column
#'   names describing the states. If `x` is a matrix, it is assumed that the
#'   element on row `i` and column `j` is the transition probability (or weight)
#'   from state `i` to state `j`. It also accepts a `data.frame` object in wide format
#'.  (each column is a timepoint with no extra columns)
#' @param type A `character` string describing the weight matrix type.
#'   Currently supports `"prop"` for proportions (probabilities) and
#'   `"freq"` for frequencies.
#' @param inits An optional `numeric` vector of initial state probabilities
#'   for each state. Can be provided only if `x` is a `matrix`. The vector will
#'   be scaled to unity.
#' @param ... Ignored.
#' @return An object of class `tna` which is a `list` containing the
#'   following elements:
#'
#'   * `weights`\cr A `list` of adjacency matrices of the model
#'     (weight matrices) for each cluster.
#'   * `inits`\cr A `list` of initial weight vectors for each
#'     cluster. For matrices, this element will be `NULL` if `inits` is not
#'     directly provided
#'   * `labels`\cr A `character` vector of the state labels, or `NULL` if there
#'     are no labels.
#'   * `seq`\cr The original sequence data converted to an internal format
#'     used by the package when `x` is a `stslist` or a `data.frame` object.
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
build_tna.default <- function(x, type = "prop", inits, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  x <- try_(as.matrix(x))
  stopifnot_(
    !inherits(x, "try-error"),
    "Argument {.arg x} must be coercible to a {.cls matrix}."
  )
  build_tna.matrix(x, type, inits, ...)
}

#' @export
#' @rdname build_tna
build_tna.matrix <- function(x, type = "prop", inits, ...) {
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
      "Argument {.arg inits} must provide initial weights for all states."
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
  names(inits) <- colnames(x)
  check_tna_type(type)
  check_tna_inits(inits, type)
  build_tna_(
    weights = list(x),
    inits = list(inits),
    labels = colnames(x),
    type = type
  )
}

#' @export
#' @rdname build_tna
build_tna.stslist <- function(x, type = "prop", ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  check_tna_type(type)
  x <- create_seqdata(x)
  model <- build_markov_model(x, type, ...)
  build_tna_(
    weights = list(model$weights),
    inits = list(model$inits),
    labels = attr(x, "labels"),
    type = type,
    seq = list(x),
  )
}


#' @export
#' @rdname build_tna
build_tna.data.frame <- function(x, type = "prop", ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  check_tna_type(type)
  x <- create_seqdata(x)
  model <- build_markov_model(x, type, ...)
  build_tna_(
    weights = list(model$weights),
    inits = list(model$inits),
    labels = model$labels,
    type = type,
    seq = list(x)
  )
}

#' @export
#' @rdname build_tna
build_tna.mhmm <- function(x, type = "prop", ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    attr(x, "type") == "mmm",
    "Argument {.arg x} must be a mixed Markov model fit."
  )
  check_tna_type(type)
  clusters <- names(x$transition_probs)
  n_clust <- length(clusters)
  seq <- vector(mode = "list", length = n_clust)
  cluster_assignment <- summary(x)$most_probable_cluster
  for (i in clusters) {
    seq[[i]] <- x$observations[cluster_assignment == i, ]
  }
  build_tna_(
    weights = x$transition_probs,
    inits = x$initial_probs,
    labels = attr(x$observations, "labels"),
    type = type,
    seq = seq
  )
}

#' Build a Transition Network Analysis object
#'
#' @param weights A `list` of `matrix` of transition probabilities.
#' @param inits A `list` of `matrix` of initial state probabilities.
#' @param labels A `character` vector of state labels.
#' @param seq A `list` of `tna_seqdata` objects when created from sequence data.
#' @return A `tna` object.
#' @noRd
build_tna_ <- function(weights, inits, labels, type, seq = NULL) {
  structure(
    list(
      weights = weights,
      inits = onlyif(!missing(inits), inits),
      labels = labels,
      seq = seq
    ),
    type = type,
    class = "tna"
  )
}

#' Build and Visualize a Network with Edge Betweenness
#'
#' This function builds a network from a transition matrix in a `tna` object and computes edge betweenness for the network. Optionally, it visualizes the network using the `qgraph` package, with the edge thickness representing the edge betweenness values.
#'
#' @param x A `tna` object containing transition matrices and associated metadata.
#' @param cluster An integer specifying which cluster to analyze. Defaults to `1`.
#' @param layout A numeric matrix or character string specifying the layout of the network for the visualization. If `NULL`, a default layout is used. Defaults to `NULL`.
#' @param plot A logical value indicating whether to visualize the network. Defaults to `TRUE`.
#'
#' @details
#' The function first converts the transition matrix for the specified cluster into a directed graph using the `igraph` package. It then calculates the edge betweenness of the graph, which is a measure of how often an edge lies on the shortest paths between pairs of nodes.
#'
#' If `plot = TRUE`, the function uses `qgraph` to visualize the network, where edge thickness is proportional to edge betweenness, node colors are derived from the `tna` object, and `Pie` values from the `tna` object are displayed on the nodes.
#'
#' The layout of the network can be customized via the `layout` parameter, which can either be a predefined layout from `qgraph` or a user-specified matrix of node positions.
#'
#' @return A data frame where each row represents an edge, and columns include the source and target nodes, and the calculated edge betweenness.
#'
#' @examples
#' # Build and visualize the network for the first cluster
#' \dontrun{
#' edge_betweenness_df <- build_network_with_edge_betweenness(tna_model)
#'
#' # Build the network without visualization
#' edge_betweenness_df <- build_network_with_edge_betweenness(tna_model, plot = FALSE)
#' }
#' @export
build_network_with_edge_betweenness <- function(x, cluster = 1, layout = NULL, plot = TRUE) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  adjacency_matrix <- x$weights[[cluster]]
  init_values <- x$inits[[cluster]]
  colors <- x$colors
  # Create a graph from the adjacency matrix
  bg <- igraph::graph_from_adjacency_matrix(adjacency_matrix, weighted = TRUE, mode = "directed")

  # Calculate edge betweenness and prepare the edge data frame
  Edge_betweeness <- cbind(igraph::as_data_frame(bg),
                           Edge_betweenness = igraph::edge_betweenness(bg, directed = TRUE)) |>
    dplyr::select(-weight) |>
    dplyr::rename(weight = 3)

  if(plot) {
    # Plot the network with qgraph
    print(qgraph::qgraph(
      Edge_betweeness,
      theme = "colorblind",
      layout = onlyif(!is.null(layout), layout),
      minimum = 0.03,
      mar = c(4, 4, 4, 4),
      cut = 5,
      edge.labels = TRUE,
      title = "",
      colors = onlyif(!is.null(colors), colors),
      pie = init_values,
      pieBorder = 0.2,
      edge.label.cex = 1.5,
      maximum = 0.6,
      vsize = 10
    ))
  }
  return(Edge_betweeness)
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
    out <- as.data.frame(x)
  } else if (is.data.frame(x)) {
    vals <- sort(unique(unlist(x)))
    alphabet <- labels <- vals[!is.na(vals)]
    # TODO default colors for data.frame
    colors <- NULL
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
    alphabet = alphabet,
    labels = labels,
    colors = colors,
    class = "data.frame"
  )
}

#' Build a Markov Model from Sequence Data
#'
#' @param x A data object from `create_seqdata()`
#' @param type The type of transition network model to build
#' @param transitions Should the individual-level transitions also be returned?
#' Defaults to `FALSE`.
#' @noRd
build_markov_model <- function(x, type = c("prop"), transitions = FALSE) {
  alphabet <- attr(x, "alphabet")
  labels <- attr(x, "labels")
  m <- as.matrix(x)
  n <- nrow(m)
  p <- ncol(m)
  a <- length(alphabet)
  idx <- seq_len(n)
  trans <- array(0L, dim = c(n, a, a))
  inits <- table(m[, 1L])
  for (i in seq_len(p - 1)) {
    from <- m[, i]
    to <- m[, i + 1L]
    any_na <- is.na(from) | is.na(to)
    new_trans <- cbind(idx, from, to)[!any_na, , drop = FALSE]
    trans[new_trans] <- trans[new_trans] + 1L
  }
  weights <- compute_weights(trans, type, a)
  if (type == "prop") {
    inits <- inits / sum(inits)
  }
  dimnames(weights) <- list(alphabet, alphabet)
  list(
    weights = weights,
    inits = inits,
    labels = labels,
    trans = onlyif(transitions, trans)
  )
}

#' Check Transition Network Type for Validity
#'
#' @param type Type of the transition network.
#' @noRd
check_tna_type <- function(type) {
  type <- onlyif(is.character(type), tolower(type))
  type <- try(match.arg(type, c("prop", "freq")), silent = TRUE)
  stopifnot_(
    !inherits(type, "try-error"),
    "Argument {.arg type} must be either {.val prop} or {.val freq}."
  )
}

#' Check Transition Network Initial Values for Validity
#'
#' @param inits A `numeric` vector of initial values.
#' @param type Type of the transition network as a `character` string.
#' @noRd
check_tna_inits <- function(inits, type) {
  if (!missing(inits) && type == "prop") {
    stopifnot_(
      all(inits <= 1),
      "Argument {.arg inits} must contain numeric values between 0 and 1 when
      {.arg type} is {.val prop}."
    )
  }
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
  if (type == "prop") {
    weights <- weights / .rowSums(weights, m = s, n = s)
  }
  weights
}
