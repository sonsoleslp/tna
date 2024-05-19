#' Calculate Centralities for a Transition Matrix
#'
#' This function calculates various centrality measures for a given transition matrix.
#'
#' @param mat A square matrix representing transition probabilities or adjacency.
#'
#' @return A data frame containing centrality measures for each interaction.
#'
#' @details
#' This function calculates several centrality measures using the \code{igraph}, \code{NetworkToolbox}, \code{keyplayer}, and \code{qgraph} packages.
#' The measures include:
#' \describe{
#'   \item{\code{OutStrength}}{Outgoing strength centrality, calculated using \code{strength} with \code{mode = "out"} from \code{igraph}. It measures the total weight of the outgoing edges from each node.}
#'   \item{\code{InStrength}}{Incoming strength centrality, calculated using \code{strength} with \code{mode = "in"} from \code{igraph}. It measures the total weight of the incoming edges to each node.}
#'   \item{\code{ClosenessIn}}{Closeness centrality (incoming), calculated using \code{closeness} with \code{mode = "in"} from \code{igraph}. It measures how close a node is to all other nodes based on the incoming paths.}
#'   \item{\code{ClosenessOut}}{Closeness centrality (outgoing), calculated using \code{closeness} with \code{mode = "out"} from \code{igraph}. It measures how close a node is to all other nodes based on the outgoing paths.}
#'   \item{\code{Closeness}}{Closeness centrality (overall), calculated using \code{closeness} with \code{mode = "all"} from \code{igraph}. It measures how close a node is to all other nodes based on both incoming and outgoing paths.}
#'   \item{\code{Betweenness}}{Betweenness centrality, calculated using \code{rspbc} from \code{NetworkToolbox}. It measures the extent to which a node lies on the shortest paths between other nodes.}
#'   \item{\code{Diffusion}}{Diffusion centrality, calculated using \code{diffusion} from \code{keyplayer}. It measures the influence of a node in spreading information through the network.}
#'   \item{\code{Clustering}}{Clustering coefficient, calculated using \code{clustcoef_auto} from \code{qgraph} on the symmetric adjacency matrix obtained via \code{as.symmetricAdjacencyMatrix} with \code{rule = "weak"} from \code{DCG}. It measures the degree to which nodes tend to cluster together.}
#' }
#'
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
#' @importFrom igraph graph.adjacency strength closeness
#' @importFrom NetworkToolbox rspbc
#' @importFrom keyplayer diffusion
#' @importFrom DCG as.symmetricAdjacencyMatrix
#' @importFrom qgraph clustcoef_auto
#' @importFrom dplyr pull
#' @importFrom tibble rownames_to_column
#' @export
calculate.centralities <- function(mat) {
  OutStrength <- mat |> igraph::graph.adjacency(weighted = TRUE) |> igraph::strength(mode = "out")
  InStrength <- mat |> igraph::graph.adjacency(weighted = TRUE) |> igraph::strength(mode = "in")
  ClosenessIn <- mat |> igraph::graph.adjacency(weighted = TRUE) |> igraph::closeness(mode = "in")
  ClosenessOut <- mat |> igraph::graph.adjacency(weighted = TRUE) |> igraph::closeness(mode = "out")
  Closeness <- mat |> igraph::graph.adjacency(weighted = TRUE) |> igraph::closeness(mode = "all")
  Betweenness <- NetworkToolbox::rspbc(mat)
  Diffusion <- keyplayer::diffusion(mat) |> data.frame() |> dplyr::pull(diffusion)
  Clustering <- DCG::as.symmetricAdjacencyMatrix(mat, rule = "weak", weighted = TRUE) |> qgraph::clustcoef_auto() |> dplyr::pull(clustZhang)
  data.frame(OutStrength, InStrength, ClosenessIn, ClosenessOut, Closeness, Betweenness, Diffusion, Clustering) |> tibble::rownames_to_column("Interaction")
}



#' Plot Centralities for a Transition Matrix
#'
#' This function plots various centrality measures for a given transition matrix using a lollipop chart.
#'
#' @param mat A square matrix representing transition probabilities or adjacency.
#'
#' @return A ggplot object displaying the lollipop charts for each centrality measure.
#'
#' @details
#' This function calculates several centrality measures using the \code{\link{calculate.centralities}} function and then plots these measures using the `lollipop_chart` from `ggcharts`.
#' The centrality measures include OutStrength, InStrength, ClosenessIn, ClosenessOut, Closeness, Betweenness, Diffusion, and Clustering.
#' The resulting plot includes facets for each centrality measure, showing the values for each interaction. The returned plot is a `ggplot2` object, so it can be easily modified and styled.
#'
#' @usage
#' plot.centralities(mat)
#'
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
#'   # Plotting the centralities
#'   plot.centralities(transition_matrix)
#' }
#'
#' @importFrom dplyr mutate across
#' @importFrom tidyr pivot_longer
#' @importFrom ggcharts lollipop_chart
#' @importFrom ggplot2 theme element_rect xlab ylab
#' @export
plot.centralities <- function(mat) {
  cent.table <- calculate.centralities(mat)
  cent.table |> dplyr::mutate(dplyr::across(ClosenessIn:Clustering, Ranger)) |>
    tidyr::pivot_longer(OutStrength:Clustering) |>
    ggcharts::lollipop_chart(x = Interaction, y = value, facet = name, line_size = 2, font_size = 3) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::xlab("") + ggplot2::ylab("")
}
