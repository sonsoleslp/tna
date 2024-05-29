#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using the `qgraph` package.
#' The nodes in the graph represent states, with node sizes corresponding to initial state probabilities.
#' Edges between nodes represent transition probabilities, and the graph is colored according to the state colors specified in the model.
#'
#' @export
#' @param x A `tna` object from [tna::build_tna()].
#' @param pie See [qgraph::qgraph()].
#' @param labels See [qgraph::qgraph()].
#' @param color See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `ggplot` of the transition network.
#' @examples
#' \dontrun{
#'   library("TraMineR")
#'   data(biofam3c, package = "seqHMM")
#'
#'   # Preparing the sequence data
#'   seq_data <- seqdef(biofam3c)
#'
#'   # Building a transition matrix from the sequence data
#'   tna_model <- build_tna(seq_data)
#'   plot(tna_model)
#' }
#'
plot.tna <- function(x, pie = x$inits, labels = x$labels,
                     color = x$colors, edge.labels = TRUE, ...) {
  qgraph::qgraph(
    x$matrix,
    pie = pie,
    labels = labels,
    color = color,
    edge.labels = edge.labels,
    ...
  )
}

#' Plot Centralities for a Transition Matrix
#'
#' Calculates several centrality measures using the [centralities()] method
#' and then plots these measures using [ggcharts::lollipop_chart()].
#' The resulting plot includes facets for each centrality measure, showing the values for each interaction.
#' The returned plot is a `ggplot2` object, so it can be easily modified and styled.
#' See [centralities()] for details on the centrality measures.
#'
#' @export
#' @param x An object of class `centralities`.
#' @param ... Arguments passed to [ggcharts::lollipop_chart()].
#' @return A `ggplot` object displaying the lollipop charts for each centrality measure.
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
#' @export
plot.centralities <- function(x, ...) {
  x[-1] <- lapply(x[-1], ranger)
  x <- stats::reshape(
    as.data.frame(x),
    idvar = "Interaction",
    ids = x[["Interaction"]],
    times = names(x)[-1L],
    timevar = "name",
    drop = "Interaction",
    varying = list(names(x)[-1L]),
    direction = "long",
    v.names = "value"
  )
  ggcharts::lollipop_chart(
      data = x,
      x = !!rlang::sym("Interaction"),
      y = !!rlang::sym("value"),
      facet = !!rlang::sym("name"),
      line_size = 2,
      font_size = 3,
      ...
  ) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::xlab("") +
    ggplot2::ylab("")
}