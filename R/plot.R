#' Plot Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using the \code{qgraph} package.
#'
#' @param model A list containing the TNA model components, typically the output of the \code{\link{build.tna}} function.
#' @param ... Additional arguments passed to the \code{qgraph} function.
#'
#' @return A plot of the transition network.
#'
#' @usage
#' plot.tna(model)
#'
#' @details
#' This function uses the \code{qgraph} package to visualize the transition probabilities in the TNA model. The nodes in the graph
#' represent states, with node sizes corresponding to initial state probabilities. Edges between nodes represent transition
#' probabilities, and the graph is colored according to the state colors specified in the model.
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
#'   plot.tna(tna_model)
#' }
#'
#' @importFrom qgraph qgraph
#' @export
plot.tna <- function(model, pie = model$inits, labels = model$Labels, color = model$colors, edge.labels = TRUE, ...) {
  qgraph::qgraph(model$Matrix, pie = pie, labels = labels, color = color, edge.labels = edge.labels, ...)
}
