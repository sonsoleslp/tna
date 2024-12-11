#' Plot a Histogram of Edge Weights in the Network
#'
#' @export
#' @inheritParams graphics::hist
#' @param ... Additional arguments passed to [graphics::hist()].
#' @param main A `character` string defining the title of the plot.
#' @param xlab A `character` string defining the vertical axis label.
#' @return A `histogram` object of edge weights.
#' @examples
#' model <- tna(engagement)
#' hist(model)
#'
hist.tna <- function(x, breaks, col = "lightblue",
                     main, xlab, border = "white", ...) {
  check_missing(x)
  check_class(x, "tna")
  w <- c(x$weights)
  type <- attr(x, "type")
  xlab_missing <- missing(xlab)
  if (xlab_missing) {
    xlab <- paste0(
      "Edge Weights (",
      switch(type,
        `relative` = "Probabilities",
        `frequency` = "Frequencies",
        `co-occurrence` = "Co-occurrences"
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
      type == "relative",
      seq(0, 1, length.out = 20),
      seq(0, max(w), length.out = 20)
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
#' @family core
#' @param x A `tna` object from [tna()].
#' @param colors See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param labels See [qgraph::qgraph()].
#' @param layout One of the following:
#'   * A `character` string describing a `qgraph` layout.
#'   * A `matrix` of node positions to use, with a row for each node and
#'     `x` and `y` columns for the node positions.
#'   * A layout function from `igraph`.
#' @param layout_args A `list` of arguments to pass to the `igraph` layout
#'   function when `layout` is a function.
#' @param mar See [qgraph::qgraph()].
#' @param pie See [qgraph::qgraph()].
#' @param theme See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` plot of the transition network.
#' @examples
#' model <- tna(engagement)
#' plot(model)
#'
plot.tna <- function(x, labels, colors, pie,
                     edge.labels = TRUE, layout = "circle",
                     layout_args = list(), mar = rep(5, 4),
                     theme = "colorblind", ...) {
  check_missing(x)
  check_class(x, "tna")
  layout <- check_layout(x, layout, layout_args)
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
  # weights_abs <- abs(x$weights)
  # q <- stats::quantile(weights_abs, probs = c(0.2, 0.3))
  # minimum <- ifelse_(missing(minimum), q[1L], minimum)
  # cut <- ifelse_(missing(cut), q[2L], cut)
  qgraph::qgraph(
    input = x$weights,
    color = colors,
    edge.labels = edge.labels,
    labels = labels,
    layout = layout,
    pie = pie,
    mar = mar,
    theme = theme,
    ...
  )
}

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
#' plot(cm, ncol = 4, reorder = TRUE)
#'
plot.tna_centralities <- function(x, reorder = TRUE, ncol = 3,
                                  scales = c("free_x", "fixed"),
                                  colors, labels = TRUE, ...) {
  check_class(x, "tna_centralities")
  plot_centralities_(x, reorder, ncol, scales, colors, labels)
}

#' Plot Cliques of a TNA Network
#'
#' @export
#' @inheritParams print.tna_cliques
#' @inheritParams plot.tna
#' @param show_loops A `logical` value indicating whether to include loops
#' in the plots or not.
#' @param minimum See [qgraph::qgraph()].
#' @param ask A `logical` value. When `TRUE`, show plots one by one and asks
#' to plot the next plot in interactive mode.
#' @return `NULL` (invisibly).
#' @examples
#' model <- tna(engagement)
#' cliq <- cliques(model, size = 2)
#' plot(cliq, n = 1)
#'
plot.tna_cliques <- function(x, n = 6, first = 1, show_loops = FALSE,
                             minimum = 0.00001, mar = rep(5, 4),
                             ask = TRUE, ...) {
  check_class(x, "tna_cliques")
  n_cliques <- length(x$weights)
  size <- attr(x, "size")
  if (n_cliques == 0) {
    warning_("No {size}-cliques were found in the network.")
    return(invisible(NULL))
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
      # edge.label.cex = 1.82,
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
    # if ((max_cliques - first) == 0) {
    #  return (do.call(qgraph::qgraph, args = plot_args))
    # }
  }
  invisible(NULL)
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
#' @return A `qgraph` object in which the nodes are colored by community.
#' @examples
#' model <- tna(group_regulation)
#' comm <- communities(model)
#' plot(comm, method = "leading_eigen")
#'
plot.tna_communities <- function(x, cluster = 1L, colors,
                                 method = "spinglass", ...) {
  check_class(x, "tna_communities")
  y <- attr(x, "tna")
  colors <- ifelse_(
    missing(colors),
    default_colors,
    colors
  )
  plot(y, colors = map_to_color(x$assignment[, method], colors), ...)
}

#' Plot the Significant Differences from a Permutation Test
#'
#' @export
#' @param x A `tna_permutation` object.
#' @param ... Arguments passed to [plot_model()].
#' @return A `qgraph` object containing only the significant edges according
#' to the permutation test.
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' # Small number of iterations for CRAN
#' perm <- permutation_test(model_x, model_y, iter = 20)
#' plot(perm)
#'
plot.tna_permutation <- function(x, ...) {
  check_class(x, "tna_permutation")
  plot_model(
    x$edges$diffs_sig,
    labels = attr(x, "labels"),
    colors = attr(x, "colors"),
    ...
  )
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
  check_class(x, "tna_stability")
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
        paste0("Warning: No valid data for measure ", measure, ". Skipping.")
      )
      next
    }
    means <- apply(corr, 2, mean, na.rm = TRUE)
    ci_lower <- apply(
      corr,
      2L,
      stats::quantile,
      probs = level / 2.0,
      na.rm = TRUE
    )
    ci_upper <- apply(
      corr,
      2L,
      stats::quantile,
      probs = 1.0 - level / 2.0,
      na.rm = TRUE
    )
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
      round(cs_coef, 2L)
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

#' Plot Centrality Measures
#'
#' @inheritParams plot.tna_centralities
#' @noRd
plot_centralities_ <- function(x, reorder, ncol, scales, colors, labels) {
  check_flag(reorder)
  check_flag(labels)
  scales <- check_match(scales, c("free_x", "fixed"))
  scales <- ifelse_(scales == "free_x", "free", "free_y")
  if (missing(colors) && !is.null(attr(x, "colors"))) {
    colors <- attr(x, "colors")
  }
  if (missing(colors)) {
    colors <- rep("black", length.out = length(unique(x$State)))
  } else if (!is.list(colors) && length(colors) == 1) {
    colors <- rep(colors, length.out = length(unique(x$State)))
  }
  ifelse_(
    is_tna_centralities(x),
    plot_centralities_single(x, reorder, ncol, scales, colors, labels),
    plot_centralities_multiple(x, reorder, ncol, scales, colors, labels)
  )
}

#' Plot Centralities for a Single Cluster
#'
#' @inheritParams plot.tna_centralities
#' @noRd
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
      linewidth = 4
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

plot_centralities_multiple <- function(x, reorder, ncol,
                                       scales, colors, labels) {
  measures <- names(x)[3:ncol(x)]
  n_clusters <- length(unique(x$Group))
  dplyr::mutate(x, State = factor(!!rlang::sym("State"))) |>
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
        color = !!rlang::sym("Group"),
        fill = !!rlang::sym("Group"),
        group = !!rlang::sym("Group")
      )
    ) +
    ggplot2::facet_wrap("name", ncol = ncol, scales = scales) +
    ggplot2::geom_path() +
    ifelse_(
      !is.null(colors) & (length(unique(colors)) == n_clusters),
      ggplot2::scale_color_manual(values = colors),
      ggplot2::scale_color_discrete()
    ) +
    ggplot2::geom_point(size = 2, shape = 21, stroke = NA) +
    ifelse_(
      !is.null(colors) & (length(unique(colors)) == n_clusters),
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
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object displaying the difference network between the
#'   two models.
#' @examples
#' model_x <- tna(engagement[engagement[, 1] == "Active", ])
#' model_y <- tna(engagement[engagement[, 1] != "Active", ])
#' plot_compare(model_x, model_y)
#'
plot_compare <- function(x, y, ...) {
  check_class(x, "tna")
  check_class(y, "tna")
  # TODO check that x and y are comparable
  stopifnot_(
    all(x$labels == y$labels),
    "{.arg x} and {.arg y} must have the same labels."
  )
  colors <- rlang::missing_arg()
  pie <- abs(x$inits - y$inits)
  piesign <- ifelse(x$inits > y$inits, "#009900", "red")
  # pos_col <- c("#009900", "darkgreen")
  # neg_col <- c("#BF0000", "red")
  diff <- build_model_(
    x$weights - y$weights,
    type = attr(x, "type"),
    labels = x$labels,
    inits = pie
  )
  if (!is.null(x$data)) {
    colors <- attr(x$data, "colors")
  }
  weights_abs <- abs(x$weights)
  q <- stats::quantile(weights_abs, probs = c(0.2, 0.3))
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

#' Plot a Transition Network Model from a Matrix of Edge Weights
#'
#' @export
#' @param x A square `matrix` of edge weights.
#' @inheritParams plot.tna
#' @return See [plot.tna()].
#' @examples
#' m <- matrix(rexp(25), 5, 5)
#' plot_model(m)
#'
plot_model <- function(x, labels, colors,
                       edge.labels = TRUE, layout = "circle",
                       mar = rep(5, 4), theme = "colorblind", ...) {
  stopifnot_(
    is.matrix(x) && ncol(x) == nrow(x),
    "Argument {.arg x} must be a square matrix."
  )
  nc <- ncol(x)
  if (missing(labels)) {
    labels <- seq_len(nc)
  }
  if (missing(colors)) {
    colors <- color_palette(nc)
  }
  qgraph::qgraph(
    input = x,
    color = colors,
    edge.labels = edge.labels,
    labels = labels,
    layout = layout,
    mar = mar,
    theme = theme,
    ...
  )
}

# Clusters ----------------------------------------------------------------

#' Plot a Histogram of Edge Weights for a `group_tna` Object.
#'
#' @export
#' @family clusters
#' @param x A `group_tna` object.
#' @param ... Additional arguments passed to [graphics::hist()].
#' @return A `list` (invisibly) of `histogram` objects of the edge weights of
#' each cluster.
#' @examples
#' model <- group_model(engagement_mmm)
#' hist(model)
#'
hist.group_tna <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  invisible(lapply(x, \(i) hist.tna(i, ...)))
}

#' Plot a grouped Transition Network Analysis Model
#'
#' Plots a transition network of each cluster using `qgraph`.
#'
#' @export
#' @family clusters
#' @param x A `group_model` object.
#' @param title A title for each plot. It can be a single string (the same one
#'  will be used for all plots) or a list (one per group)
#' @param ... Same as [plot.tna()].
#' @return `NULL` (invisibly).
#' @examples
#' model <- group_model(engagement_mmm)
#' plot(model)
#'
plot.group_tna <- function(x, title, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  if (missing(title)) {
    title <- names(x)
  } else if (length(title) == 1) {
    title <- rep(title, length(x))
  }
  for (i in seq_along(x)) {
    plot(x[[i]], title = title[i], ...)
  }
  invisible(NULL)
}

#' Plot Centrality Measures
#'
#' @export
#' @family clusters
#' @param x A `group_tna_centralities` object.
#' @inheritParams plot.tna_centralities
#' @return A `ggplot` object displaying a line chart for each centrality
#' with one line per cluster.
#' @examples
#' model <- group_model(engagement_mmm)
#' cm <- centralities(model)
#' plot(cm)
#'
plot.group_tna_centralities <- function(x, reorder = TRUE, ncol = 4,
                                        scales = c("free_x", "fixed"),
                                        colors, labels = TRUE, ...) {
  check_missing(x)
  check_class(x, "group_tna_centralities")
  plot_centralities_(x, reorder, ncol, scales, colors, labels)
}

#' Plot Found Cliques
#'
#' @export
#' @family clusters
#' @param x A `group_tna_cliques` object.
#' @param title A `character` vector of titles to use for each plot.
#' @param ... Arguments passed to [plot.tna_cliques()].
#' @return A `list` (invisibly) with one element per cluster. Each element
#' contains a `qgraph` plot when only one clique is present per cluster,
#' otherwise the element is `NULL`.
#' @examples
#' model <- group_model(engagement_mmm)
#' cliq <- cliques(model, size = 2)
#' plot(cliq)
#'
plot.group_tna_cliques <- function(x, title, ...) {
  check_missing(x)
  check_class(x, "group_tna_cliques")
  if (missing(title)) {
    title <- names(x)
  } else if (length(title) == 1) {
    title <- rep(title, length(x))
  }
  invisible(
    Map(function(y, i) plot.tna_cliques(y, title = i, ...), x, title)
  )
}

#' Plot Found Communities
#'
#' @export
#' @family clusters
#' @param x A `group_tna_communities` object.
#' @param title A `character` vector of titles to use for each plot.
#' @param colors A `character` vector of colors to use.
#' @param ... Arguments passed to [plot.tna_communities()].
#' @return A `list` (invisibly) of `qgraph` objects in which the nodes are
#' colored by community for each cluster.
#' @examples
#' model <- group_model(engagement_mmm)
#' comm <- communities(model)
#' plot(comm)
#'
plot.group_tna_communities <- function(x, title = names(x), colors, ...) {
  check_missing(x)
  check_class(x, "group_tna_communities")
  colors <- ifelse_(
    missing(colors),
    lapply(x, \(x) default_colors),
    ifelse_(
      is.vector(colors) & is.atomic(colors),
      lapply(x, function(x) colors),
      colors
    )
  )
  if (is.null(title) ||
    (is.vector(title) & is.atomic(title) & (length(title) == 1))) {
    title <- lapply(x, \(x) title)
  }
  invisible(
    Map(
      function(y, i, j) {
        plot(y, title = i, colors = j, ...)
      },
      x,
      title,
      colors
    )
  )
}

#' Plot Centrality Stability Results
#'
#' @export
#' @family clusters
#' @param x A `group_tna_stability` object.
#' @param ... Arguments passed to [plot.tna_stability()].
#' @return A `list` (invisibly) of `ggplot` objects displaying the stability
#' analysis plot.
#' @examples
#' model <- group_model(engagement_mmm)
#' # Low number of iterations for CRAN
#' stability <- estimate_cs(
#'   model,
#'   drop_prop = c(0.3, 0.5, 0.7, 0.9),
#'   iter = 10
#' )
#' plot(stability)
#'
plot.group_tna_stability <- function(x, ...) {
  check_missing(x)
  check_class(x, "group_tna_stability")
  invisible(lapply(x, \(i) plot.tna_stability(i, ...)))
}
