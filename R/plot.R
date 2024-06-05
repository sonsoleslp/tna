#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using
#' the `qgraph` package. The nodes in the graph represent states, with node
#' sizes corresponding to initial state probabilities. Edges between nodes
#' represent the transition probabilities.
#'
#' @export
#' @param x A `tna` object from [tna::build_tna()].
#' @param digits An `integer` defining how many digits to show for transition
#'   probabilities
#' @param ... Ignored.
#' @return A `ggplot` of the transition network.
#' @examples
#' tna_model <- build_tna(engagement)
#' plot(tna_model)
#'
plot.tna <- function(x, digits = 2, ...) {
  xlen <- length(x)
  ggraph::ggraph(x, layout = "circle") +
    ggraph::geom_edge_arc(
      ggplot2::aes(
        label = round(!!rlang::sym("weight"), digits),
        alpha = !!rlang::sym("weight"),
        width = !!rlang::sym("weight")
      ),
      angle_calc = "along",
      label_dodge = ggplot2::unit(0.025, "native"),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.025, "native")),
      start_cap = ggraph::circle(0.18, unit = "native"),
      end_cap = ggraph::circle(0.18, unit = "native"),
      strength = 0.25,
      show.legend = FALSE
    ) +
    ggraph::geom_edge_loop(
      ggplot2::aes(
        label = round(!!rlang::sym("weight"), 2),
        direction = (!!rlang::sym("from") - 1) * 360 / xlen,
        alpha = !!rlang::sym("weight"),
        width = !!rlang::sym("weight"),
        span = 100
      ),
      angle_calc = "along",
      label_dodge = ggplot2::unit(0.025, "native"),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.025, "native")),
      start_cap = ggraph::circle(0.18, unit = "native"),
      end_cap = ggraph::circle(0.18, unit = "native"),
      show.legend = FALSE
    ) +
    ggraph::geom_node_circle(ggplot2::aes(r = 0.18), fill = "white") +
    ggraph::geom_node_circle(ggplot2::aes(r = 0.15)) +
    ggforce::geom_arc_bar(
      ggplot2::aes(
        x0 = !!rlang::sym("x"),
        y0 = !!rlang::sym("y"),
        r0 = 0.15,
        r = 0.18,
        start = 0,
        end = !!rlang::sym("inits") * base::pi * 2
      ),
      fill = "gray"
    ) +
    ggfittext::geom_fit_text(
      ggplot2::aes(
        label = !!rlang::sym("name"),
        xmin = !!rlang::sym("x") - 0.14,
        xmax = !!rlang::sym("x") + 0.14,
        y = !!rlang::sym("y")
      )
    ) +
    ggraph::scale_edge_width(range = c(0.5, 2)) +
    ggraph::scale_edge_alpha(range = c(0.2, 1)) +
    # ggplot2::guides(fill = guide_legend(title = "State")) +
    ggplot2::coord_fixed() +
    ggraph::theme_graph()
}

#' Plot Centralities for a Transition Matrix
#'
#' Calculates several centrality measures using the [centralities()] method
#' and then plots these measures as a lollipop chart. The resulting plot
#' includes facets for each centrality measure, showing the values for each
#' interaction. The returned plot is a `ggplot2` object, so it can be easily
#' modified and styled. See [centralities()] for details on the centrality
#' measures.
#'
#' @export
#' @param x An object of class `centralities`.
#' @param ncol Number of columns to use for the facets.
#' @param scales Either `"fixed"` or `"free"` to be used for the x-axis of the
#'   facet scales.
#' @param ... Ignored.
#' @return A `ggplot` object displaying the lollipop charts for each centrality
#'   measure.
#' @examples
#' tna_model <- build_tna(engagement)
#' cm <- centralities(tna_model)
#' plot(cm)
#'
plot.centralities <- function(x, ncol = 3, scales = "free", ...) {
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
