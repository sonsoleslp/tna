#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using
#' the `qgraph` package. The nodes in the graph represent states, with node
#' sizes corresponding to initial state probabilities. Edges between nodes
#' represent the transition probabilities.
#'
#' @export
#' @param x A `tna` object from [tna::build_tna()].
#' @param color See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param labels See [qgraph::qgraph()].
#' @param layout See [qgraph::qgraph()].
#' @param mar See [qgraph::qgraph()].
#' @param pie See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `ggplot` of the transition network.
#' @examples
#' tna_model <- build_tna(engagement)
#' plot(tna_model)
#'
plot.tna <- function(x, color = x$colors, edge.labels = TRUE, labels = x$labels,
                     layout = "circle", mar = rep(5, 4), pie = x$inits, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  dots <- list(...)
  dots[["theme"]] <- ifelse_(
    !"theme" %in% names(dots),
    ifelse_(all(x$adjacency >= 0), "gray", "default"),
    dots[["theme"]]
  )
  qgraph_args <- c(
    list(
      input = x$adjacency,
      color = color,
      edge.labels = edge.labels,
      labels = labels,
      layout = layout,
      pie = pie,
      mar = mar
    ),
    dots
  )
  do.call(qgraph::qgraph, args = qgraph_args)
}

#' Plot Centrality Measures
#'
#' Plots the centrality measures of a `centralities` object as a lollipop chart.
#' The resulting plot includes facets for each centrality measure, showing the
#' values for each state. The returned plot is a `ggplot2` object, so it can be
#' easily modified and styled. See [centralities()] for details on the
#' centrality measures.
#'
#' @export
#' @param x An object of class `centralities`.
#' @param ncol Number of columns to use for the facets.
#' @param scales Either `"fixed"` or `"free"` (the default). If `"free"`, the
#'   horizontal axis is scaled individually in each facet. If `"fixed"`, the
#'   same values are used for all axes.
#' @param ... Ignored.
#' @return A `ggplot` object displaying the lollipop charts for each centrality
#'   measure.
#' @examples
#' tna_model <- build_tna(engagement)
#' cm <- centralities(tna_model)
#' plot(cm)
#'
plot.centralities <- function(x, ncol = 3, scales = "free", ...) {
  stopifnot_(
    is_centralities(x),
    "Argument {.arg x} must be a {.cls centralities} object."
  )
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
  ggplot2::ggplot(x) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = !!rlang::sym("State"),
        xend = !!rlang::sym("State"),
        y = 0,
        yend = !!rlang::sym("value")
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = !!rlang::sym("State"),
        y = !!rlang::sym("value")),
      size = 3
    ) +
    ggplot2::coord_flip()+
    ggplot2::facet_wrap(~name, ncol = ncol, scales = scales) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("")
}
