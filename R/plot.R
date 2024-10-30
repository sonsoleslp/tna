#' Plot a Histogram of Edge Weights in the Network
#'
#' @inheritParams graphics::hist
#' @param ... Additional arguments passed to [graphics::hist()].
#' @param main A `character` string defining the title of the plot.
#' @param xlab A `character` string defining the vertical axis label.
#' @export
hist.tna <- function(x, breaks, col = "lightblue",
                     main, xlab, border = "white", ...) {
  check_tna(x)
  w <- c(x$weights)
  type <- attr(x, "type")
  xlab_missing <- missing(xlab)
  if (xlab_missing) {
    xlab <- paste0(
      "Edge Weights (",
      switch(
        type,
        relative = "Probabilities",
        scaled = "Scaled Frequencies",
        ranked = "Scaled Frequency Ranks",
        absolute = "Frequencies"
      ),
      ")"
    )
  }
  if (missing(main)) {
    main <- ifelse_(
      xlab_missing,
      paste0("Histogram of ", xlab),
      "Histogram of Edge Weights"
    )
  }
  if (missing(breaks)) {
    breaks <- ifelse_(
      type == "absolute",
      seq(0, max(w), length.out = 20),
      seq(0, 1, length.out = 20)
    )
  }
  graphics::hist(
    x = w,
    breaks = breaks,
    col = col,
    main = main,
    xlab = xlab,
    border = border,
    ...
  )
}

#' Plot a Transition Network Analysis Model
#'
#' This function plots a transition network analysis (TNA) model using
#' the `qgraph` package. The nodes in the graph represent states, with node
#' sizes corresponding to initial state probabilities. Edge labels represent
#' the edge weights of the network.
#'
#' @export
#' @param x A `tna` object from [tna()].
#' @param colors See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param labels See [qgraph::qgraph()].
#' @param layout See [qgraph::qgraph()].
#' @param mar See [qgraph::qgraph()].
#' @param pie See [qgraph::qgraph()].
#' @param cut See [qgraph::qgraph()]. Defaults to the smallest 20th percentile
#'   of the weights.
#' @param minimum See [qgraph::qgraph()]. Defaults to the smallest 10th
#'   percentile of the weights.
#' @param theme See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` plot of the transition network.
#' @family core
#' @examples
#' tna_model <- tna(engagement)
#' plot(tna_model)
#'
plot.tna <- function(x, labels, colors, pie,
                     edge.labels = TRUE, layout = "circle",
                     mar = rep(5, 4), cut, minimum,
                     theme = "colorblind", ...) {
  check_tna(x)
  if (missing(pie)) {
    pie <- x$inits
  }
  if (missing(labels)) {
    labels <- x$labels
  }
  if (missing(colors)) {
    colors <- ifelse_(
      is.null(x$data),
      color_palette(length(x$labels)),
      attr(x$data, "colors")
    )
  }
  # abs here for plot_compare()
  weights_abs <- abs(x$weights)
  q <- stats::quantile(weights_abs, probs = c(0.05, 0.1))
  minimum <- ifelse_(missing(minimum), q[1L], minimum)
  cut <- ifelse_(missing(cut), q[2L], cut)
  # TODO qgraph produces error if no edges are above cut/minimum, check
  qgraph::qgraph(
    input = x$weights,
    color = colors,
    minimum = minimum,
    cut = cut,
    edge.labels = edge.labels,
    labels = labels,
    layout = layout,
    pie = pie,
    mar = mar,
    theme = theme,
    ...
  )
  invisible(x)
}

# TODO is this needed
# #' Plot Transition Networks for All Clusters
# #'
# #' This function plots the transition networks for each cluster in a `tna` object.
# #' It iterates through the transition matrices for each cluster and generates
# #' corresponding plots using the `plot.tna` function.
# #'
# #' @param x A `tna` object containing transition matrices for different clusters.
# #' @param ... Additional arguments to be passed to the `plot.tna` function.
# #'
# #' @return A series of plots showing the transition networks for each cluster.
# #' @family core
# #' @examples
# #' \dontrun{
# #' # Assuming `tna_model` is a tna object containing transition matrices
# #' plot_clusters(tna_model)
# #' }
# #' @export
# plot_clusters <- function(x, ...) {
#   stopifnot_(
#     is_tna(x),
#     "Argument {.arg x} must be a {.cls tna} object."
#   )
#   result <- list()
#   matrices <- x$transits
#   for (clus in seq_along(matrices)) {
#     plot.tna(x, cluster = clus, ...)
#   }
# }


#' Plot Centrality Measures
#'
#' Plots the centrality measures of a `tna_centralities` object as a
#' lollipop chart. The resulting plot includes facets for each centrality
#' measure, showing the values for each state. The returned plot is a
#' `ggplot2` object, so it can be easily modified and styled. See
#' [centralities()] for details on the centrality measures.
#'
#' @export
#' @family core
#' @param x An object of class `tna_centralities`.
#' @param ncol Number of columns to use for the facets. The default is 3.
#' @param scales Either `"fixed"` or `"free_x"` (the default). If `"free_x"`,
#'   the horizontal axis is scaled individually in each facet. If `"fixed"`,
#'   the same values are used for all axes.
#' @param reorder A `logical` value indicating whether to reorder the values
#'   for each centrality in a descending order. The default is `TRUE`.
#' @param model An object of class `tna`.
#' @param colors The colors for each node (default is the model colors
#'  if the `tna` model object is passed, otherwise `"black"`).
#' @param labels A `logical` value indicating whether to show the centrality
#'   numeric values. The default is `TRUE`.
#' @param ... Ignored.
#' @return A `ggplot` object displaying the lollipop charts for each centrality
#'   measure.
#' @examples
#' tna_model <- tna(engagement)
#' cm <- centralities(tna_model)
#' plot(cm)
#' plot(cm, ncol = 4, reorder = TRUE)
#'
plot.tna_centralities <- function(x, model = NULL, reorder = TRUE,
                                  ncol = 3, scales = c("free_x", "fixed"),
                                  colors, labels = TRUE, ...) {
  stopifnot_(
    is_tna_centralities(x),
    "Argument {.arg x} must be a {.cls tna_centralities} object."
  )
  check_flag(reorder)
  check_flag(labels)
  if (missing(colors) && !is.null(attr(x, "colors"))) {
    colors <- attr(x, "colors")
  }
  if (missing(colors)) {
    colors <- rep("black", length.out = length(unique(x$State)))
  } else if (!is.list(colors) && length(colors) == 1) {
    colors <- rep(colors, length.out = length(unique(x$State)))
  }
  scales <- onlyif(is.character(scales), tolower(scales))
  scales <- try(match.arg(scales, c("free_x", "fixed")), silent = TRUE)
  stopifnot_(
    !inherits(scales, "try-error"),
    "Argument {.arg scales} must be either {.val free_x} or {.val fixed}."
  )
  scales <- ifelse_(scales == "free_x", "free", "free_y")
  ifelse_(
    "Cluster" %in% names(x),
    plot_centralities_multiple(x, ncol, scales, colors, labels),
    plot_centralities_single(x, reorder, ncol, scales, colors, labels)
  )
}

#' Plot Cliques of a TNA Network
#'
#' @inheritParams print.tna_cliques
#' @inheritParams plot.tna
#' @param show_loops A `logical` value indicating whether to include loops
#' in the plots or not.
#' @param minimum See [qgraph::qgraph()].
#' @param ask A `logical` value. When `TRUE`, showw plots one by one and asks
#' to plot the next plot in interactive mode.
#' @export
plot.tna_cliques <- function(x, n = 6, first = 1, show_loops = FALSE,
                             minimum = 0.00001, mar = rep(5, 4),
                             ask = TRUE, ...) {
  stopifnot_(
    is_tna_cliques(x),
    "Argument {.arg x} must be a {.cls tna_cliques} object."
  )
  n_cliques <- length(x$weights)
  size <- attr(x, "size")
  if (n_cliques == 0) {
    warning_("No {size}-cliques were found in the network.")
    return()
  }
  colors <- attr(x, "colors")
  labels <- attr(x, "labels")
  max_cliques <- min(first + n - 1L, n_cliques)
  if (interactive()) {
    op <- graphics::par(ask = ask)
    on.exit(graphics::par(op))
  }
  for (i in seq(first, max_cliques)) {
    clique_weights <- x$weights[[i]]
    diag(clique_weights) <- ifelse_(
      show_loops,
      diag(clique_weights),
      0
    )
    plot_args <- list(
      input = clique_weights,
      labels = colnames(clique_weights),
      edge.labels = TRUE,
      directed = !attr(x, "sum_weights"),
      #edge.label.cex = 1.82,
      mar = mar,
      minimum = minimum,
      theme = "colorblind",
      cut = 0.01,
      normalize = TRUE,
      layout = "circle",
      color = colors[match(rownames(clique_weights), labels)],
      pie = x$inits[[i]]
    )
    plot_args <- utils::modifyList(plot_args, list(...))
    do.call(qgraph::qgraph, args = plot_args)
    # TODO should already work with 1 clique?
    #if ((max_cliques - first) == 0) {
    #  return (do.call(qgraph::qgraph, args = plot_args))
    #}
  }
}

#' Plot Centrality Stability Results
#'
#' This function visualizes the centrality stability results produced by the
#' `estimate_centrality_stability` function. It shows how different centrality
#' measures' correlations change as varying proportions of cases are dropped,
#' along with their confidence intervals (CIs).
#'
#' @export
#' @param x A `tna_stability` object produced by `estimate_cs`.
#' @param level A `numeric` value representing the significance level for
#' the confidence intervals. Defaults to `0.05`.
#' @param ... Ignored.
#'
#' @details
#' The function aggregates the results for each centrality measure across
#' multiple proportions of dropped cases (e.g., 0.1, 0.2, ..., 0.9) and
#' calculates the mean and the desired quantiles for each proportion.
#' The confidence intervals (CIs) are computed based on the quantiles
#' and displayed in the plot.
#'
#' If no valid data is available for a centrality measure
#' (e.g., missing or NA values), the function skips that measure with a warning.
#'
#' The plot includes:
#'
#' * The mean correlation for each centrality measure as a function of the
#'   proportion of dropped cases.
#' * Shaded confidence intervals representing CIs for each centrality measure.
#' * A horizontal dashed line at the threshold value used for calculating
#'   the CS-coefficient.
#' * A subtitle listing the CS-coefficients for each centrality measure.
#'
#' @return A `ggplot` object displaying the stability analysis plot.
#' @examples
#' model <- tna(engagement)
#' cs <- estimate_cs(model, iter = 10)
#' plot(cs)
#'
plot.tna_stability <- function(x, level = 0.05, ...) {
  stopifnot_(
    is_tna_stability(x),
    "Argument {.arg x} must be a {.cls tna_stability} object."
  )
  check_probability(level)
  x$detailed_results <- NULL
  x_names <- names(x)
  drop_prop <- attr(x, "drop_prop")
  threshold <- attr(x, "threshold")
  measure_data <- vector(mode = "list", length = length(x))
  cs_subtitle <- character(length(x))
  for (i in seq_along(x)) {
    measure <- x_names[i]
    corr <- x[[measure]]$correlations
    # Check if measure_results has valid dimensions
    if (is.null(dim(corr)) || nrow(corr) == 0 || ncol(corr) == 0) {
      warning_(
        c("Warning: No valid data for measure ", measure, ". Skipping.")
      )
      next
    }
    means <- apply(corr, 2, mean, na.rm = TRUE)
    ci_lower <- apply(corr, 2, stats::quantile, probs = level / 2)
    ci_upper <- apply(corr, 2, stats::quantile, probs = 1 - level / 2)
    measure_data[[i]] <- data.frame(
      measure = measure,
      proportion = drop_prop,
      correlation = means,
      lower = ci_lower,
      upper = ci_upper
    )
    cs_coef <- x[[measure]]$cs_coefficient
    cs_subtitle[i] <- paste0(
      measure,
      " CS = ",
      round(cs_coef, 2)
    )
  }
  plot_data <- dplyr::bind_rows(measure_data)
  stopifnot_(
    nrow(plot_data) > 0,
    "No valid data to plot."
  )
  cs_subtitle <- paste0(cs_subtitle, collapse = "; ")
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = !!rlang::sym("proportion"),
      y = !!rlang::sym("correlation"),
      color = !!rlang::sym("measure")
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = !!rlang::sym("lower"),
        ymax = !!rlang::sym("upper"),
        fill = !!rlang::sym("measure")
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(
      yintercept = threshold,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = "gray25"
    ) +
    ggplot2::labs(
      title = "Centrality Stability Analysis",
      subtitle = paste0("CS-Coeficients: ", cs_subtitle),
      x = "Proportion of Cases Dropped",
      y = "Correlation with Original Centrality",
      color = "Centrality Measure",
      fill = "Centrality Measure"
    ) +
    ggplot2::scale_x_continuous(breaks = drop_prop) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(-1, 1)
}

#' Plot Communities
#'
#' This function visualizes the communities detected within a `tna` object
#' based on different community detection algorithms and their corresponding
#' color mappings.
#'
#' @export
#' @family patterns
#' @param x A `communities` object generated by the `find_communities` method.
#' Each community detection method maps nodes or points in to a specific
#' communities.
#' @param cluster An `integer` index of the cluster for which to produce the
#' plot. Defaults to the first cluster.
#' @param colors A `character` vector of color values used for visualizing
#' community assignments.
#' @param method A `character` string naming a community detection method to
#' use for coloring the plot. This can be one of the following:
#' @param ... Additional arguments passed to [qgraph::qgraph].
#'
#' * `"walktrap"`: A community detection method using short random walks.
#' * `"fast_greedy"`: A method based on modularity optimization.
#' * `"label_prop"`: A method that uses label propagation.
#' * `"infomap"`: A method that uses information flow to detect communities.
#' * `"edge_betweenness"`: A method that uses edge betweenness to find
#'   communities.
#' * `"leading_eigen"`: A method using the leading eigenvector of the
#'   modularity matrix.
#' * `"spinglass"`: A method based on the spinglass model.
#'
#' @examples
#' model <- tna(group_regulation)
#' comm <- communities(model)
#' plot(comm, method = "leading_eigen")
#'
plot.tna_communities <- function(x, cluster = 1L, colors,
                                 method = "spinglass", ...) {
  stopifnot_(
    is_tna_communities(x),
    "Argument {.arg x} must be a {.cls tna_communities} object."
  )
  y <- attr(x, "tna")
  colors <- ifelse_(
    missing(colors),
    default_colors,
    colors
  )
  plot(y, colors = map_to_color(x$assignment[, method], colors), ...)
}

plot_centralities_single <- function(x, reorder, ncol, scales, colors, labels) {
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
  x <- ifelse_(
    reorder,
    dplyr::arrange(
      x, !!rlang::sym("name"), !!rlang::sym("value")
    ),
    dplyr::arrange(
      x, !!rlang::sym("name"), dplyr::desc(!!rlang::sym("State"))
    )
  ) |>
    dplyr::mutate(rank = dplyr::row_number())

  ggplot2::ggplot(x) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_col(
      ggplot2::aes(
        fill = !!rlang::sym("State"),
        x = !!rlang::sym("rank"),
        y = !!rlang::sym("value")
      ),
      size = 4
    ) +
    ggplot2::coord_flip(clip = "off") +
    onlyif(
      labels,
      ggplot2::geom_text(
        ggplot2::aes(
          label = round(!!rlang::sym("value"), 2),
          x = !!rlang::sym("rank"),
          y = 0.98 * !!rlang::sym("value")
        ),
        vjust = 0.3,
        hjust = 1,
        size = 3
      )
    ) +
    ggplot2::facet_wrap(~name, ncol = ncol, scales = scales) +
    ggplot2::scale_x_continuous(
      name = NULL,
      expand = c(0, 0.5),
      breaks = x$rank,
      labels = x$State
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(size = 8, vjust = 0.2),
      panel.spacing = ggplot2::unit(2, "lines"),
      plot.margin = ggplot2::margin(5.5, 11, 5.5, 5.5, "points")
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("")
}

plot_centralities_multiple <- function(x, ncol, scales, colors, labels) {
  # TODO handle colors and test
  measures <- names(x)
  n_clusters <- length(unique(x$Cluster))
  dplyr::mutate(x, Cluster = factor(!!rlang::sym("Cluster"))) |>
    data.frame() |>
    stats::reshape(
      varying = measures,
      v.names = "value",
      timevar = "name",
      times = measures,
      direction = "long"
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!rlang::sym("value"),
        y = !!rlang::sym("State"),
        color = !!rlang::sym("Cluster"),
        fill = !!rlang::sym("Cluster"),
        group = !!rlang::sym("Cluster")
      )
    ) +
    ggplot2::facet_wrap("name", ncol = 4) +
    ggplot2::geom_path() +
    ifelse_(
      length(unique(colors)) == n_clusters,
      ggplot2::scale_color_manual(values = colors),
      ggplot2::scale_color_discrete()
    ) +
    ggplot2::geom_point(size = 2, shape = 21, stroke = NA) +
    ifelse_(
      length(unique(colors)) == n_clusters,
      ggplot2::scale_fill_manual(values = colors),
      ggplot2::scale_fill_discrete()
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Centrality") +
    ggplot2::ylab("") +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(1, "lines"),
      legend.position = "bottom"
    )
}

#' Plot the difference network between two models
#'
#' Plots the difference network between model `x` and model `y`. The edges are
#' computed from subtracting the two models. The pie chart is the difference in
#' initial probabilities between model `x` and model `y`. Green color indicates
#' that `x`is greater than `y`and red indicates otherwise.
#'
#' @export
#' @family core
#' @param x An object of class `tna`. It will be the principal model.
#' @param y An object of class `tna`. It will be the model subtracted from the
#'   principal model.
#' @param cut See [qgraph::qgraph()].
#' @param minimum See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object displaying the difference network between the
#'   two models.
#' @examples
#' model_x <- tna(engagement[engagement[, 1] == "Active", ])
#' model_y <- tna(engagement[engagement[, 1] != "Active", ])
#' plot_compare(model_x, model_y)
#'
plot_compare <- function(x, y, cut, minimum, ...) {
  check_tna(x)
  check_tna(y)
  stopifnot_(
    all(x$labels == y$labels),
    "{.arg x} and {.arg y} must have the same labels."
  )
  colors <- rlang::missing_arg()
  pie <- abs(x$inits - y$inits)
  piesign <- ifelse(x$inits > y$inits, "#009900", "red")
  #pos_col <- c("#009900", "darkgreen")
  #neg_col <- c("#BF0000", "red")
  diff <- tna(x$weights - y$weights, inits = pie)
  if (!is.null(x$data)) {
    colors <- attr(x$data, "colors")
  }
  plot.tna(
    x = diff,
    pie = pie,
    pieColor = piesign,
    colors = colors,
    theme = NULL,
    palette = "colorblind",
    ...
  )
}

