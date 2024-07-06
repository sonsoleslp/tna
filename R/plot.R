#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using
#' the `qgraph` package. The nodes in the graph represent states, with node
#' sizes corresponding to initial state probabilities. Edges between nodes
#' represent the transition probabilities.
#'
#' @export
#' @param x A `tna` object from [tna::build_tna()].
#' @param cluster Index of the primary cluster to visualize.
#'   Defaults to the first cluster.
#' @param cluster2 Optional index of the secondary cluster. If specified,
#'   The difference between the transition probabilities of `cluster` and
#'   `cluster2` will be plotted.
#' @param color See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param labels See [qgraph::qgraph()].
#' @param layout See [qgraph::qgraph()].
#' @param mar See [qgraph::qgraph()].
#' @param pie See [qgraph::qgraph()].
#' @param cut See [qgraph::qgraph()].
#' @param minimum See [qgraph::qgraph()].
#' @param theme See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` plot of the transition network.
#' @examples
#' tna_model <- build_tna(engagement)
#' plot(tna_model)
#'
plot.tna <- function(x, cluster = 1, cluster2, color = x$colors,
                     edge.labels = TRUE, labels = x$labels, layout = "circle",
                     mar = rep(5, 4), pie = x$inits[[cluster]],
                     cut = 0.1, minimum = 0.05, theme = "colorblind", ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  stopifnot_(
    checkmate::test_integerish(
      x = cluster,
      lower = 1,
      upper = length(x$transits),
      any.missing = FALSE,
      len = 1,
      null.ok = FALSE
    ),
    "Argument {.arg cluster} must be a single integer value between 1 and
     the number of clusters."
  )
  stopifnot_(
    missing(cluster2) || checkmate::test_integerish(
      x = cluster2,
      lower = 1,
      upper = length(x$transits),
      any.missing = FALSE,
      len = 1,
      null.ok = FALSE
    ),
    "Argument {.arg cluster2} must be a single integer value between 1 and
     the number of clusters."
  )
  cluster <- as.integer(cluster)
  cluster2 <- onlyif(!missing(cluster2), as.integer(cluster2))
  qgraph::qgraph(
    input = ifelse_(
      is.null(cluster2),
      x$transits[[cluster]],
      x$transits[[cluster]] - x$transits[[cluster2]]
    ),
    color = color,
    minimum = minimum,
    cut = cut,
    edge.labels = edge.labels,
    labels = labels,
    layout = layout,
    pie = onlyif(is.null(cluster2), pie),
    mar = mar,
    theme = theme,
    ...
  )
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
#' @param ncol Number of columns to use for the facets. The default is 3.
#' @param scales Either `"fixed"` or `"free_x"` (the default). If `"free_x"`,
#'   the horizontal axis is scaled individually in each facet. If `"fixed"`,
#'   the same values are used for all axes.
#' @param reorder A `logical` value indicating whether to reorder the values
#'   for each centrality in a descending order. The default is `FALSE`.
#' @param line_color The color for the line segments (default is `"black"`)
#' @param point_color The color for the dots (default is `"black"`).
#' @param labels A `logical` value indicating whether to show the centrality
#'   numeric values. The default is `TRUE`.
#' @param ... Ignored.
#' @return A `ggplot` object displaying the lollipop charts for each centrality
#'   measure.
#' @examples
#' tna_model <- build_tna(engagement)
#' cm <- centralities(tna_model)
#' plot(cm)
#' plot(cm, ncol = 4, reorder = TRUE)
#'
plot.centralities <- function(x, ncol = 3, scales = c("free_x", "fixed"),
                              reorder = FALSE, line_color = "black",
                              point_color = "black", labels = TRUE, ...) {
  stopifnot_(
    is_centralities(x),
    "Argument {.arg x} must be a {.cls centralities} object."
  )
  stopifnot_(
    checkmate::test_flag(x = reorder),
    "Argument {.arg reorder} must be a single {.cls logical} value."
  )
  stopifnot_(
    checkmate::test_flag(x = labels),
    "Argument {.arg labels} must be a single {.cls logical} value."
  )
  line_color <- rep(line_color, length.out = length(unique(x$State)))
  point_color <- rep(point_color, length.out = length(unique(x$State)))
  scales <- onlyif(is.character(scales), tolower(scales))
  scales <- try(match.arg(scales, c("free_x", "fixed")), silent = TRUE)
  stopifnot_(
    !inherits(scales, "try-error"),
    "Argument {.arg scales} must be either {.val free_x} or {.val fixed}."
  )
  scales <- ifelse_(scales == "free_x", "free", "free_y")
  if ("Cluster" %in% names(x)) {
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
    themeasures <- names(x)[names(x) %in% default_measures]
    dplyr::mutate(x, Cluster = factor(!!rlang::sym("Cluster"))) |>
      data.frame() |>
      stats::reshape(
        varying = themeasures,
        v.names = "value",
        timevar = "name",
        times = themeasures,
        direction = "long"
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = !!rlang::sym("value"),
          y = !!rlang::sym("State"),
          color = !!rlang::sym("Cluster"),
          group = !!rlang::sym("Cluster")
        )
      ) +
      ggplot2::facet_wrap("name", ncol = 4) +
      ggplot2::geom_path() +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::xlab("Centrality") +
      ggplot2::ylab("") +
      ggplot2::theme(
        panel.spacing = ggplot2::unit(1, "lines"),
        legend.position = "bottom"
      )
  } else {
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
    if (reorder) {
      x <- dplyr::arrange(
        x, !!rlang::sym("name"), !!rlang::sym("value")
      ) |>
        dplyr::mutate(rank = dplyr::row_number())
    } else {
      x <- dplyr::arrange(
        x, !!rlang::sym("name"), dplyr::desc(!!rlang::sym("State"))
      ) |>
        dplyr::mutate(rank = dplyr::row_number())
    }
    ggplot2::ggplot(x) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = !!rlang::sym("rank"),
          xend = !!rlang::sym("rank"),
          y = 0,
          yend = !!rlang::sym("value"),
          color = !!rlang::sym("State")
        ),
        size = 1
      ) +
      ggplot2::scale_color_manual(values = line_color) +
      ggplot2::geom_point(
        ggplot2::aes(
          fill = !!rlang::sym("State"),
          x = !!rlang::sym("rank"),
          y = !!rlang::sym("value")
        ),
        size = 4,
        shape = 21,
        stroke = NA
      ) +
      ggplot2::scale_fill_manual(values = point_color) +
      ggplot2::coord_flip(clip = "off") +
      onlyif(
        labels,
        ggplot2::geom_text(
          ggplot2::aes(
            label = round(!!rlang::sym("value"), 2),
            x = !!rlang::sym("rank"),
            y = !!rlang::sym("value")
          ),
          vjust = 2,
          hjust = 0.8,
          size = 3
        )
      ) +
      ggplot2::facet_wrap(~name, ncol = ncol, scales = scales) +
      ggplot2::scale_x_continuous(
        name = NULL,
        expand = c(0, 0.5),
        breaks = x$rank,
        labels = x$State,
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold", size = 12),
        axis.text.y = ggplot2::element_text(size = 8),
        panel.spacing = ggplot2::unit(2, "lines"),
        plot.margin = ggplot2::margin(5.5, 11, 5.5, 5.5, "points")
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("")
  }
}

#' Plot the difference network between two models
#'
#' Plots the difference network between model `x` and model `y`. The edges are
#' computed from subtracting the two models. The pie chart is the difference in
#' initial probabilities between model `x` and model `y`. Green color indicates
#' that `x`is greater than `y`and red indicates otherwise.
#'
#' @export
#' @rdname plot_compare
#' @param x An object of class `tna`. It will be the principal model.
#' @param y An object of class `tna`. It will be the model subtracted from the
#'   principal model.
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object displaying the difference network between the
#'   two models.
#' @examples
#' tna_model_1 <- build_tna(engagement[engagement[, 1] == "Active", ])
#' tna_model_2 <- build_tna(engagement[engagement[, 1] != "Active", ])
#' plot_compare(tna_model_1, tna_model_2)
#'
plot_compare = function(x, y, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  stopifnot_(
    is_tna(y),
    "Argument {.arg y} must be a {.cls tna} object."
  )
  stopifnot_(
    all(x$labels == y$labels),
    "{.arg x} and {.arg y} must have the same labels."
  )
  pie <- abs(x$inits[[1]] - y$inits[[1]])
  piesign <- ifelse(x$inits[[1]] > y$inits[[1]], "#009900", "red")
  posCol <- c("#009900", "darkgreen")
  negCol <- c("#BF0000", "red")

  diff <- build_tna(x$transits[[1]] - y$transits[[1]], pie)

  plot.tna(diff, pie = pie, pieColor = piesign, color = x$colors, theme = NULL, palette = "colorblind", ...)
}
