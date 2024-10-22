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
#'   from state `i` to state `j`. It also accepts a `data.frame` object in wide format
#'.  (each column is a timepoint with no extra columns)
#' @param inits An optional `numeric` vector of initial state probabilities
#'   for each state. Can be provided only if `x` is a `matrix`. The vector will
#'   be scaled to unity.
#' @param colors List of colors to use as nodes of the network
#' @param ... Additional arguments passed to [seqHMM::build_mm()] when `x` is
#'   a `stslist` object or `data.frame`.
#' @return An object of class `tna` which is a `list` containing the
#'   following elements:
#'
#'   * `transits`\cr A `list` of adjacency matrices of the model
#'     (transition matrices) for each cluster.
#'   * `inits`\cr A `list` of initial state probability vectors for each
#'     cluster. For matrices, this element will be `NULL` if `inits` is not
#'     directly provided
#'   * `labels`\cr A `character` vector of the state labels, or `NULL` if there
#'     are no labels.
#'   * `colors`\cr A `character` vector of the state colors, or `NULL`.
#'   * `igraph_network`\cr A `list` of `igraph` directed graphs based on each
#'      transition probability matrix
#'
#' @family core
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
build_tna.matrix <- function(x, inits, colors = rep("white", nrow(x)), ...) {
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
  names(inits) <- colnames(x)
  build_tna_(
    transit_probs = list(x),
    igraph_network = list(igraph::graph_from_adjacency_matrix(x, mode = "directed", weighted = TRUE)),
    initial_probs = list(inits),
    colors = colors,
    labels = colnames(x)
  )
}

#' @export
#' @rdname build_tna
build_tna.stslist <- function(x, colors, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  mkvmodel <- seqHMM::build_mm(x, ...)
  cpal <- attr(x, "cpal")
  if (!missing(colors)) {
    cpal <- colors
  }
  build_tna_(
    transit_probs = list(mkvmodel$transition_probs),
    initial_probs = list(mkvmodel$initial_probs),
    igraph_network = list(igraph::graph_from_adjacency_matrix(mkvmodel$transition_probs, mode = "directed", weighted = TRUE)),
    labels = attr(x, "labels"),
    colors = cpal,
    seq = list(x)
  )
}


#' @export
#' @rdname build_tna
build_tna.data.frame <- function(x, colors, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  sequ <- TraMineR::seqdef(x)
  mkvmodel <- seqHMM::build_mm(sequ, ...)
  cpal <- attr(sequ, "cpal")
  if (!missing(colors)) {
    cpal <- colors
  }
  build_tna_(
    transit_probs = list(mkvmodel$transition_probs),
    initial_probs = list(mkvmodel$initial_probs),
    igraph_network = list(
      igraph::graph_from_adjacency_matrix(
        mkvmodel$transition_probs,
        mode = "directed",
        weighted = TRUE
      )
    ),
    labels = attr(sequ, "labels"),
    colors = cpal,
    seq = list(sequ)
  )
}

#' @export
#' @rdname build_tna
build_tna.mhmm <- function(x, colors, ...) {
  stopifnot_(
    !missing(x),
    "Argument {.arg x} is missing."
  )
  stopifnot_(
    attr(x, "type") == "mmm",
    "Argument {.arg x} must be a mixed Markov model fit."
  )

  igraph_network <- list()
  seq <- list()
  cluster_assignment <- summary(x)$most_probable_cluster
  for (i in names(x$transition_probs)) {
    igraph_network[[i]] <- igraph::graph_from_adjacency_matrix(x$transition_probs[[i]], mode = "directed", weighted = TRUE)
    seq[[i]] <- x$observations[cluster_assignment == i,]
  }
  cpal <- attr(x$observations, "cpal")
  if (!missing(colors)) {
    cpal <- colors
  }
  build_tna_(
    transit_probs = x$transition_probs,
    initial_probs = x$initial_probs,
    igraph_network = igraph_network,
    seq = seq,
    labels = attr(x$observations, "labels"),
    colors = cpal
  )
}

#' Build a Transition Network Analysis object
#'
#' @param transit_probs A `list` of `matrix` of transition probabilities.
#' @param initial_probs A `list` of `matrix` of initial state probabilities.
#' @param labels A `character` vector of state labels.
#' @param igraph_network A `list`  of `igraph` graphs for each transition matrix.
#' @param colors A `character` vector of color values to use for the states.
#' @return A `tna` object.
#' @noRd
build_tna_ <- function(transit_probs, initial_probs, labels, igraph_network, colors, seq = NULL) {

  structure(
    list(
      transits = transit_probs,
      inits = onlyif(!missing(initial_probs), initial_probs),
      labels = labels,
      igraph_network = igraph_network,
      colors = onlyif(!missing(colors), colors),
      seq = seq
    ),
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
#' @seealso `qgraph`, `igraph`
#' @export
build_network_with_edge_betweenness <- function(x, cluster = 1, layout = NULL, plot = TRUE) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  adjacency_matrix <- x$transits[[cluster]]
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

build_model <- function(observations, extract = "proportions", by_timepoint = FALSE, n_timepoints = NULL, with_missing = FALSE) {
  # Function to get ordered unique values
  ordered_unique <- function(x) {
    ux <- unique(x)
    ux[order(match(ux, x))]
  }

  # Validate input
  if (!inherits(observations, "stslist") && !is.matrix(observations) && !is.data.frame(observations)) {
    stop("Observations must be an stslist object from TraMineR or a wide-format data frame/matrix.")
  }

  # Convert to matrix if necessary
  obs_matrix <- ifelse_(
    is.matrix(observations),
    observations,
    as.matrix(observations)
  )

  # Handle n_timepoints
  if (!is.null(n_timepoints)) {
    if (n_timepoints > ncol(obs_matrix)) {
      stop("n_timepoints cannot be greater than the number of columns in observations.")
    }
    obs_matrix <- obs_matrix[, seq_len(n_timepoints), drop = FALSE]
  }

  # Determine state names
  all_state_names <- ordered_unique(as.vector(obs_matrix))

  # If with.missing is FALSE, remove NA, NaN, and "%" from state names
  if (!with_missing) {
    all_state_names <- all_state_names[!is.na(all_state_names) & all_state_names != "%" & all_state_names != "NaN"]
  }

  # Calculate initial probabilities
  initial_states <- factor(obs_matrix[, 1], levels = all_state_names)
  initial_freq <- tabulate(initial_states, nbins = length(all_state_names))
  initial_probs <- initial_freq / sum(initial_freq)

  # Initialize transition matrices
  n_states <- length(all_state_names)
  transition_freq <- matrix(0, nrow = n_states, ncol = n_states, dimnames = list(all_state_names, all_state_names))
  timepoint_matrices <- if(by_timepoint) vector("list", ncol(obs_matrix) - 1) else NULL

  # Calculate transition frequencies
  for (i in 1:(ncol(obs_matrix) - 1)) {
    from_states <- obs_matrix[, i]
    to_states <- obs_matrix[, i + 1]

    if (!with.missing) {
      valid_transitions <- from_states %in% all_state_names & to_states %in% all_state_names
      from_states <- from_states[valid_transitions]
      to_states <- to_states[valid_transitions]
    }

    from_states <- factor(from_states, levels = all_state_names)
    to_states <- factor(to_states, levels = all_state_names)

    transitions <- table(from_states, to_states)
    transition_freq <- transition_freq + transitions

    if (by_timepoint) {
      timepoint_matrices[[i]] <- transitions
    }
  }

  # Calculate transition probabilities
  transition_probs <- transition_freq / rowSums(transition_freq)
  transition_probs[is.nan(transition_probs)] <- 0

  # Prepare result
  result <- list(
    state_names = all_state_names,
    initial_probs = initial_probs
  )

  if (extract == "proportions") {
    result$transition_probs <- transition_probs
    result$freq_transition_probs <- transition_freq
  } else if (extract == "frequencies") {
    result$transition_probs <- transition_freq
    result$prob_transition_probs <- transition_probs
    result$freq_initial_probs <- initial_freq
  }

  if (by_timepoint) {
    if (extract == "proportions") {
      result$timepoint_matrices <- lapply(timepoint_matrices, function(x) {
        probs <- x / rowSums(x)
        probs[is.nan(probs)] <- 0
        probs
      })
      result$freq_timepoint_matrices <- timepoint_matrices
    } else if (extract == "frequencies") {
      result$timepoint_matrices <- timepoint_matrices
      result$prob_timepoint_matrices <- lapply(timepoint_matrices, function(x) {
        probs <- x / rowSums(x)
        probs[is.nan(probs)] <- 0
        probs
      })
    }
  }

  # If with.missing is TRUE, remove NA, NaN, and "%" from final matrices
  if (with.missing) {
    valid_states <- !is.na(all_state_names) & all_state_names != "%" & all_state_names != "NaN"
    result$state_names <- all_state_names[valid_states]
    result$initial_probs <- result$initial_probs[valid_states]

    if (extract == "proportions") {
      result$transition_probs <- result$transition_probs[valid_states, valid_states]
      result$freq_transition_probs <- result$freq_transition_probs[valid_states, valid_states]
    } else if (extract == "frequencies") {
      result$transition_probs <- result$transition_probs[valid_states, valid_states]
      result$prob_transition_probs <- result$prob_transition_probs[valid_states, valid_states]
      result$freq_initial_probs <- result$freq_initial_probs[valid_states]
    }

    if (by_timepoint) {
      if (extract == "proportions") {
        result$timepoint_matrices <- lapply(result$timepoint_matrices, function(x) x[valid_states, valid_states])
        result$freq_timepoint_matrices <- lapply(result$freq_timepoint_matrices, function(x) x[valid_states, valid_states])
      } else if (extract == "frequencies") {
        result$timepoint_matrices <- lapply(result$timepoint_matrices, function(x) x[valid_states, valid_states])
        result$prob_timepoint_matrices <- lapply(result$prob_timepoint_matrices, function(x) x[valid_states, valid_states])
      }
    }
  }

  return(result)
}


