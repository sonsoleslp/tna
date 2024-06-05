#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using
#' the `qgraph` package. The nodes in the graph represent states, with node
#' sizes corresponding to initial state probabilities. Edges between nodes
#' represent the transition probabilities.
#'
#' @export
#' @param x A `tna` object from [tna::build_tna()].
#' @param pie See [qgraph::qgraph()].
#' @param labels See [qgraph::qgraph()].
#' @param mar See [qgraph::qgraph()].
#' @param theme See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `ggplot` of the transition network.
#' @examples
#' tna_model <- build_tna(engagement)
#' plot(tna_model)
#'
plot.tna <- function(x, pie = x$inits, labels = x$labels,
                     mar = rep(5, 4), theme = "gray",
                     edge.labels = TRUE, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  qgraph::qgraph(
    x$matrix,
    pie = pie,
    labels = labels,
    mar = mar,
    theme = theme,
    edge.labels = edge.labels,
    ...
  )
}

#' Plot Centralities for a Transition Matrix
#'
#' Calculates several centrality measures using the [centralities()] method
#' and then plots these measures using [ggcharts::lollipop_chart()].
#' The resulting plot includes facets for each centrality measure, showing the
#' values for each interaction. The returned plot is a `ggplot2` object, so it
#' can be easily modified and styled. See [centralities()] for details on the
#' centrality measures.
#'
#' @export
#' @inheritParams ggcharts::lollipop_chart
#' @param x An object of class `centralities`.
#' @param font_size A `numeric` value describing the font size.
#' @param ... Arguments passed to [ggcharts::lollipop_chart()].
#' @return A `ggplot` object displaying the lollipop charts for each centrality
#'   measure.
#' @examples
#' tna_model <- build_tna(engagement)
#' cm <- centralities(tna_model)
#' plot(cm)
#'
plot.centralities <- function(x, line_color = "black", line_size = 2,
                              font_size = 3, ...) {
  stopifnot_(
    is_centralities(x),
    "Argument {.arg x} must be a {.cls centralities} object."
  )
  x[-1L] <- lapply(x[-1L], ranger)
  x <- stats::reshape(
    as.data.frame(x),
    idvar = "State",
    ids = x[["State"]],
    times = names(x)[-1L],
    timevar = "name",
    drop = "State",
    varying = list(names(x)[-1L]),
    direction = "long",
    v.names = "value"
  )
  ggcharts::lollipop_chart(
      data = x,
      x = !!rlang::sym("State"),
      y = !!rlang::sym("value"),
      facet = !!rlang::sym("name"),
      line_color = line_color,
      line_size = line_size,
      font_size = font_size,
      ...
  ) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::xlab("") +
    ggplot2::ylab("")
}
