#' Calculate Centralities for a Transition Matrix
#'
#' This function calculates several centrality measures. See 'Details' for
#' information about the measures.
#'
#' The following measures are provided:
#'
#'   * `OutStrength`\cr Outgoing strength centrality, calculated using
#'     [igraph::strength()] with `mode = "out"`. It measures the total weight
#'     of the outgoing edges from each node.
#'   * `InStrength`\cr Incoming strength centrality, calculated using
#'     [igraph::strength()] with `mode = "in"`. It measures the total weight
#'     of the incoming edges to each node.
#'   * `ClosenessIn`\cr Closeness centrality (incoming), calculated using
#'     [igraph::closeness()] with `mode = "in"`. It measures how close a node
#'     is to all other nodes based on the incoming paths.
#'   * `ClosenessOut`\cr Closeness centrality (outgoing), calculated using
#'     [igraph::closeness()] with `mode = "out"`. It measures how close a node
#'     is to all other nodes based on the outgoing paths.
#'   * `Closeness`\cr Closeness centrality (overall), calculated using
#'     [igraph::closeness()] with `mode = "all"`. It measures how close a node
#'     is to all other nodes based on both incoming and outgoing paths.
#'   * `Betweenness`\cr Betweenness centrality based on randomized shortest
#'     paths (Kivimäki et al. 2016). It measures the extent to which a
#'     node lies on the shortest paths between other nodes.
#'   * `Diffusion`\cr Diffusion centrality of Banerjee et.al. (2014).
#'     It measures the influence of a node in spreading information through
#'     the network.
#'   * `Clustering`\cr Signed clustering coefficient of Zhang and Horvath (2005)
#'     based on the symmetric adjacency matrix (sum of the adjacency matrix
#'     and its transpose). It measures the degree to which nodes tend to
#'     cluster together.
#'
#' @export
#' @rdname centralities
#' @param x A square matrix representing transition probabilities or adjacency,
#'   or a `tna` object.
#' @param measures A `character` vector indicating which centrality
#'   measures should be computed. If `NULL`, all available measures are
#'   returned. See 'Details' for available measures. The elements are partially
#'   matched ignoring case.
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures (default is `FALSE`).
#' @param normalize  A `logical` value indicating whether the centralities
#'   should be normalized (default is `FALSE`).
#' @param cluster Index of the cluster for which to compute the centralities or
#'   `NULL` if there are no clusters or if centralities should be computed for
#'   all clusters.
#' @param ... Ignored.
#' @return A `centralities` object which is a tibble (`tbl_df`)
#'   containing centrality measures for each state.
#' @references
#' Banerjee, A., A. Chandrasekhar, E. Duflo, and M. Jackson (2014).
#' Gossip: Identifying Central Individuals in a Social Network.
#' Working Paper.
#'
#' Kivimaki, I., Lebichot, B., Saramaki, J., & Saerens, M. (2016).
#' Two betweenness centrality measures based on Randomized Shortest Paths.
#' Scientific Reports, 6, 19668.
#'
#' Zhang, B., & Horvath, S. (2005).
#' A general framework for weighted gene co-expression network analysis.
#' Statistical Applications in Genetics and Molecular Biology, 4(1).
#'
#' @examples
#' tna_model <- build_tna(engagement)
#'
#' # Centrality measures including loops in the network
#' centralities(tna_model)
#'
#' # Centrality measures excluding loops in the network
#' centralities(tna_model, loops = FALSE)
#'
#' # Centrality measures normalized
#' centralities(tna_model, normalize = TRUE)
#'
centralities <- function(x, loops = FALSE, cluster = NULL,
                         normalize = FALSE, measures = NULL, ...) {
  UseMethod("centralities")
}

#' @export
#' @rdname centralities
centralities.tna <- function(x, loops = FALSE, cluster = NULL,
                             normalize = FALSE, measures = NULL, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  if (length(x$transits) == 1L) {
    centralities_(
      x$transits[[1]],
      loops = loops,
      normalize = normalize,
      measures = measures
    )
  } else if (length(x$transits) > 1L && !is.null(cluster)) {
    centralities_(
      x$transits[[cluster]],
      loops = loops,
      normalize = normalize,
      measures = measures
    )
  } else if (length(x$transits) > 1L) {
    centrality_list <- list()
    clusternames <- names(x$transits)
    for (i in seq_along(x$transits)){
      centrality_list[[i]] <- centralities_(
        x$transits[[i]],
        loops = loops,
        normalize = normalize,
        measures = measures
      )
      centrality_list[[i]]$Cluster <- clusternames[i]
    }
    structure(
      dplyr::bind_rows(centrality_list) |>
        dplyr::mutate(
          Cluster = factor(!!rlang::sym("Cluster"), levels = clusternames)
        ),
      class = c("centralities", "tbl_df", "tbl", "data.frame")
    )
  }
}

#' @export
#' @rdname centralities
centralities.matrix <- function(x, loops = FALSE, cluster = NULL,
                                normalize = FALSE, measures = NULL, ...) {
  stopifnot_(
    is.matrix(x),
    "Argument {.arg x} must be a {.cls matrix}."
  )
  centralities_(x, loops, normalize, measures)
}

#' Internal function to calculate various centrality measures
#'
#' @param x An adjacency matrix of a directed weighted graph
#' @noRd
centralities_ <- function(x, loops, normalize, measures) {
  stopifnot_(
    checkmate::test_flag(x = loops),
    "Argument {.arg loops} must be a single {.cls logical} value."
  )
  stopifnot_(
    checkmate::test_flag(x = normalize),
    "Argument {.arg normalize} must be a single {.cls logical} value."
  )
  default_measures <- c(
    "OutStrength",
    "InStrength",
    "ClosenessIn",
    "ClosenessOut",
    "Closeness",
    "Betweenness",
    "Diffusion",
    "Clustering"
  )
  measures <- ifelse_(is.null(measures), default_measures, measures)
  stopifnot_(
    checkmate::test_character(
      x = measures,
      any.missing = FALSE,
      unique = TRUE,
    ),
    "Argument {.arg measures} must be a {.cls character} vector."
  )
  lower_measures <- tolower(measures)
  lower_defaults <- tolower(default_measures)
  measures_match <- pmatch(lower_measures, lower_defaults)
  no_match <- is.na(measures_match)
  invalid_measures <- measures[no_match]
  valid_measures <- measures_match[!no_match]
  stopifnot_(
    length(invalid_measures) == 0L,
    c(
      "Argument {.arg measures} contains invalid centrality measures:",
      `x` = "Measure{?s} {.val {invalid_measures}} {?is/are} not recognized."
    )
  )
  diag(x) <- ifelse_(loops, diag(x), 0)
  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = x,
    mode = "directed",
    weighted = TRUE
  )
  OutStrength <- igraph::strength(g, mode = "out")
  InStrength <- igraph::strength(g, mode = "in")
  ClosenessIn <- igraph::closeness(g, mode = "in")
  ClosenessOut <- igraph::closeness(g, mode = "out")
  Closeness <- igraph::closeness(g, mode = "all")
  Betweenness <- rsp_bet(x)
  Diffusion <- diffusion(x)
  Clustering <- wcc(x + t(x))

  out <- data.frame(
    OutStrength,
    InStrength,
    ClosenessIn,
    ClosenessOut,
    Closeness,
    Betweenness,
    Diffusion,
    Clustering
  )[valid_measures]

  if (normalize) {
    out <- out |>
      dplyr::mutate_at(dplyr::vars(measures), ranger)
  }
  structure(
    tibble::rownames_to_column(out, "State") |>
      dplyr::mutate(
        State = factor(!!rlang::sym("State"), levels = rownames(out))
      ),
    class = c("centralities", "tbl_df", "tbl", "data.frame")
  )
}


#' Convert a dataframe to a centralities object
#'
#' @export
#' @param df A dataframe.
#' @rdname as_centralities
as_centralities <- function(df) {
  structure(df, class = c("centralities", "tbl_df", "tbl", "data.frame"))
}



#' Compute diffusion centrality measure
#'
#' @param mat A transition probability matrix.
#' @noRd
diffusion <- function(mat) {
  s <- 0
  n <- ncol(mat)
  p <- diag(1, n, n)
  for (i in seq_len(n)) {
    p <- p %*% mat
    s <- s + p
  }
  .rowSums(s, n, n)
}

#' Compute randomized shortest path betweenness centrality measure
#'
#' @param mat A transition probability matrix.
#' @noRd
rsp_bet <- function(mat, beta = 0.01) {
  n <- ncol(mat)
  W <- mat * exp(-beta * mat^-1)
  Z <- solve(diag(1, n, n) - W)
  Zrecip <- Z^-1
  Zrecip_diag <- diag(Zrecip) * diag(1, n, n)
  out <- diag(tcrossprod(Z, Zrecip - n * Zrecip_diag) %*% Z)
  out <- round(out)
  out <- out - min(out) + 1
  out
}

#' Compute signed clustering coefficient
#'
#' @param mat A transition probability matrix.
#' @noRd
wcc <- function(mat) {
  diag(mat) <- 0
  n <- ncol(mat)
  num <- diag(mat %*% mat %*% mat)
  den <- .colSums(mat, n, n)^2 - .colSums(mat^2, n, n)
  num / den
}
