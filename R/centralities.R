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
#'     paths (Kivimäki et al. 2014). It measures the extent to which a
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
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures (default is `TRUE`).
#' @param ... Ignored.
#' @return A `centralities` object which is a tibble (`tbl_df`)
#'   containing centrality measures for each interaction.
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
centralities <- function(x, ...) {
  UseMethod("centralities", x)
}

#' @export
#' @rdname centralities
centralities.tna <- function(x, loops = TRUE, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  stopifnot_(
    checkmate::test_flag(x = loops),
    "Argument {.arg loops} must be a single {.cls logical} value."
  )
  ifelse_(
    loops,
    centralities_(x$matrix),
    centralities_(x$matrix0)
  )
}

#' @export
#' @rdname centralities
centralities.matrix <- function(x, loops = TRUE, ...) {
  stopifnot_(
    is.matrix(x),
    "Argument {.arg x} must be a {.cls matrix}."
  )
  if (loops) {
    centralities_(x)
  } else {
    diag(x) <- 0
    centralities_(x)
  }
}

#' Internal function to calculate various centrality measures
#'
#' @param mat An adjacency matrix of a directed weighted graph
#' @noRd
centralities_ <- function(mat) {
  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = mat,
    mode = "directed",
    weighted = TRUE
  )
  OutStrength <- igraph::strength(g, mode = "out")
  InStrength <- igraph::strength(g, mode = "in")
  ClosenessIn <- igraph::closeness(g, mode = "in")
  ClosenessOut <- igraph::closeness(g, mode = "out")
  Closeness <- igraph::closeness(g, mode = "all")
  Betweenness <- rsp_bet(mat)
  Diffusion <- diffusion(mat)
  Clustering <- wcc(mat + t(mat))
  structure(
    tibble::rownames_to_column(
      data.frame(
        OutStrength,
        InStrength,
        ClosenessIn,
        ClosenessOut,
        Closeness,
        Betweenness,
        Diffusion,
        Clustering
      ),
      "Interaction"
    ),
    class = c("centralities", "tbl_df", "tbl", "data.frame")
  )
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
