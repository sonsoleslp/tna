#' Calculate Centralities for a Transition Matrix
#'
#' This function calculates several centrality measures using the \code{igraph}, \code{NetworkToolbox}, \code{keyplayer}, and \code{qgraph} packages.
#' The measures include:
#'
#'   * `OutStrength` Outgoing strength centrality, calculated using [igraph::strength()] with `mode = "out"`.
#'     It measures the total weight of the outgoing edges from each node.
#'   * `InStrength` Incoming strength centrality, calculated using [igraph::strength()] with `mode = "in"`.
#'     It measures the total weight of the incoming edges to each node.
#'   * `ClosenessIn` Closeness centrality (incoming), calculated using [igraph::closeness()] with `mode = "in"`.
#'     It measures how close a node is to all other nodes based on the incoming paths.
#'   * `ClosenessOut` Closeness centrality (outgoing), calculated using [igraph::closeness()] with `mode = "out"`.
#'     It measures how close a node is to all other nodes based on the outgoing paths.
#'   * `Closeness` Closeness centrality (overall), calculated using [igraph::closeness()] with `mode = "all"`.
#'     It measures how close a node is to all other nodes based on both incoming and outgoing paths.
#'   * `Betweenness` Betweenness centrality based on randomized shortest paths (Kivimäki et al. 2014).
#'     It measures the extent to which a node lies on the shortest paths between other nodes.
#'   * `Diffusion` Diffusion centrality of Banerjee et.al. (2014).
#'     It measures the influence of a node in spreading information through the network.
#'   * `Clustering` Signed clustering coefficient of Zhang and Horvath (2005)
#'     based on the symmetric adjacency matrix (sum of the adjacency matrix and its transpose).
#'     It measures the degree to which nodes tend to cluster together.
#'
#' @export
#' @rdname centralities
#' @param x A square matrix representing transition probabilities or adjacency,
#'   or a `tna` object.
#' @param ... Ignored.
#' @return A `data.frame` containing centrality measures for each interaction.
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
#' @examples
#' \dontrun{
#'   library(TraMineR)
#'   data(biofam3c)
#'
#'   # Preparing the sequence data
#'   seq_data <- seqdef(biofam3c$biofam)
#'
#'   # Building a transition matrix from the sequence data
#'   tna_model <- build.tna(seq_data)
#'   transition_matrix <- tna_model$Matrix
#'
#'   # Calculating the centralities
#'   calculate.centralities(transition_matrix)
#'
#'   # Building a transition matrix from the sequence data without loops
#'   transition_matrix0 <- tna_model$Matrix0
#'
#'   # Calculating the centralities for the matrix without loops
#'   calculate.centralities(transition_matrix0)
#' }
#'
centralities <- function(x, ...) {
  UseMethod("centralities", x)
}

#' @export
#' @rdname centralities
centralities.tna <- function(x, ...) {
  centralities_(x$matrix)
}

#' @export
#' @rdname centralities
centralities.matrix <- function(x, ...) {
  centralities_(x)
}

#' Internal function to calculate various centrality measures
#'
#' @param mat An adjacency matrix of a directed weighted graph
#' @noRd
centralities_ <- function(mat) {
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)
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
