#' Community Detection for Transition Networks
#'
#' This function detects communities within the transition networks
#' (represented by the `tna` object).
#' It uses various algorithms to find communities in the graph representation
#' of transitions and returns a `list` of communities for each cluster or a
#' specified cluster. If multiple transition matrices exist, the function
#' iterates over each cluster in the `tna` object to find communities using
#' different algorithms. The function uses the `igraph` package to convert
#' the transition matrices into graphs and then applies community detection
#' algorithms (e.g., Walktrap, Fast Greedy, Label Propagation, Infomap,
#' Edge Betweenness, Leading Eigenvector, and Spin Glass).
#'
#' @export
#' @family communities
#' @rdname communities
#' @param x A `tna` or a `group_tna` object.
#' @param methods A `character` vector of community detection algorithms to
#' apply to the network. The supported options are:
#'
#'   * `"walktrap"`: A community detection method using short random walks.
#'   * `"fast_greedy"`: A method based on modularity optimization.
#'   * `"label_prop"`: A method that uses label propagation.
#'   * `"infomap"`: A method that uses information flow to detect communities.
#'   * `"edge_betweenness"`: A method that uses edge betweenness to find
#'     communities.
#'   * `"leading_eigen"`: A method using the leading eigenvector of the
#'     modularity matrix.
#'   * `"spinglass"`: A method based on the spinglass model.
#'
#' If not provided, all methods are applied.
#' @param gamma A `numeric` value depicting a parameter that affects the
#' behavior of certain algorithms like the Spin Glass method. Defaults to `1`.
#' @param ... Ignored.
#' @return An object of class `tna_communities` which is a `list` with an
#'   element for each cluster containing:
#'
#'   * `counts`: A `list` with the number of communities found
#'     by each algorithm.
#'   * `assignments`: A `data.frame` where each row corresponds to a
#'     node and each column to a community detection algorithm,
#'     with color-coded community assignments.
#'
#' If `x` is a `group_tna` object, a `group_tna_communities` object is returned
#' instead, which is a `list` of `tna_communities` objects.
#'
#' @examples
#' model <- tna(group_regulation)
#' comm <- communities(model)
#'
communities <- function(x, ...) {
  UseMethod("communities")
}

#' @export
#' @rdname communities
communities.tna <- function(x, methods, gamma = 1, ...) {
  check_missing(x)
  check_class(x, "tna")
  if (missing(methods)) {
    methods <- names(supported_communities)
  }
  methods <- check_match(
    methods,
    names(supported_communities),
    several.ok = TRUE
  )
  stopifnot_(
    checkmate::test_number(x = gamma),
    "Argument {.arg gamma} must be a single {.cls numeric} value."
  )
  g <- as.igraph(x)
  g_un <- as.igraph(x, mode = "plus")
  communities <- list()
  mapping <- list()
  w <- igraph::E(g)$weight
  w_un <- igraph::E(g_un)$weight
  for (method in methods) {
    directed <- supported_communities[[method]]$directed
    args <- list(
      graph = ifelse_(directed, g, g_un),
      weights = ifelse_(directed, w, w_un)
    )
    args$gamma <- onlyif(
      !is.null(supported_communities[[method]]$gamma),
      gamma
    )
    if (!is.null(supported_communities[[method]]$e_weights)) {
      args$e.weights <- args$weights
      args$weights <- NULL
    }
    if (method == "edge_betweenness") {
      # TODO suppress for now
      communities[[method]] <- suppressWarnings(
        communities[[method]] <- do.call(
          supported_communities[[method]]$fun,
          args = args
        )
      )
    } else {
      communities[[method]] <- do.call(
        supported_communities[[method]]$fun,
        args = args
      )
    }
    mapping[[method]] <- igraph::membership(communities[[method]]) |>
      as.numeric()
  }
  structure(
    list(
      counts = lengths(communities),
      assignments = as.data.frame(
        c(
          list(state = igraph::V(g)$name),
          mapping
        )
      )
    ),
    class = "tna_communities",
    tna = x
  )
}

#' @export
#' @family clusters
#' @rdname communities
communities.group_tna <- function(x, methods, gamma = 1, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  if (missing(methods)) {
    methods <- names(supported_communities)
  }
  structure(
    stats::setNames(
      lapply(x, communities, methods = methods, gamma = gamma, ...),
      names(x)
    ),
    class = "group_tna_communities"
  )
}


# Supported community detection algorithms --------------------------------

supported_communities <- list(
  `walktrap` = list(
    fun = igraph::cluster_walktrap,
    directed = TRUE
  ),
  `fast_greedy` = list(
    fun = igraph::cluster_fast_greedy,
    directed = FALSE
  ),
  `label_prop` = list(
    fun = igraph::cluster_label_prop,
    directed = TRUE
  ),
  `infomap` = list(
    fun = igraph::cluster_infomap,
    directed = TRUE,
    e_weights = TRUE
  ),
  `edge_betweenness` = list(
    fun = igraph::cluster_edge_betweenness,
    directed = TRUE
  ),
  `leading_eigen` = list(
    fun = igraph::cluster_leading_eigen,
    directed = FALSE
  ),
  `spinglass` = list(
    fun = igraph::cluster_spinglass,
    directed = FALSE,
    gamma = TRUE
  )
)
