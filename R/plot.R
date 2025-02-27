#' Plot a Histogram of Edge Weights in the Network
#'
#' @export
#' @inheritParams graphics::hist
#' @param ... Additional arguments passed to [graphics::hist()].
#' @param main A `character` string defining the title of the plot.
#' @param xlab A `character` string defining the vertical axis label.
#' @return A `histogram` object of edge weights.
#' @examples
#' model <- tna(group_regulation)
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
#' @param show_pruned A `logical` value indicating if pruned edges removed by
#' [prune()] should be shown in the plot.  The default is `TRUE`, and the
#' edges are drawn as dashed with a different color to distinguish them.
#' @param pruned_edge_color A `character` string for the color to use for
#' pruned edges when `show_pruned = TRUE`. The default is `"red"`.
#' @param edge.color See [qgraph::qgraph()].
#' @param edge.labels See [qgraph::qgraph()].
#' @param edge.label.position See [qgraph::qgraph()].
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
#' model <- tna(group_regulation)
#' plot(model)
#'
plot.tna <- function(x, labels, colors, pie,
                     show_pruned = TRUE, pruned_edge_color = "red",
                     edge.color = NA, edge.labels = TRUE,
                     edge.label.position = 0.65, layout = "circle",
                     layout_args = list(), mar = rep(5, 4),
                     theme = "colorblind", ...) {
  check_missing(x)
  check_class(x, "tna")
  check_flag(show_pruned)
  check_flag(edge.labels)
  check_range(edge.label.position, scalar = FALSE)
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
  lty <- 1
  if (!is.null(attr(x, "pruning")) && show_pruned) {
    lty <- x$weights
    lty[x$weights == 0] <- 2
    lty[x$weights > 0] <- 1
    edge_color_mat <- attr(x, "pruning")$original
    edge_color_mat[x$weights == 0] <- pruned_edge_color
    edge_color_mat[x$weights > 0] <- edge.color
    edge.color <- edge_color_mat
  }
  weights <- ifelse_(
    !is.null(attr(x, "pruning")) && show_pruned,
    attr(x, "pruning")$original,
    x$weights
  )
  qgraph::qgraph(
    input = weights,
    color = colors,
    edge.color = edge.color,
    edge.labels = edge.labels,
    edge.label.position = edge.label.position,
    labels = labels,
    layout = layout,
    theme = theme,
    pie = pie,
    mar = mar,
    lty = lty,
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
#' tna_model <- tna(group_regulation)
#' cm <- centralities(tna_model)
#' plot(cm, ncol = 3, reorder = TRUE)
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
#' @param cut See [qgraph::qgraph()].
#' @param normalize See [qgraph::qgraph()].
#' @param show_loops A `logical` value indicating whether to include loops
#' in the plots or not.
#' @param minimum See [qgraph::qgraph()].
#' @param ask A `logical` value. When `TRUE`, show plots one by one and asks
#' to plot the next plot in interactive mode.
#' @return `NULL` (invisibly).
#' @examples
#' model <- tna(group_regulation)
#' cliq <- cliques(model, size = 2)
#' plot(cliq, n = 1, ask = FALSE)
#'
plot.tna_cliques <- function(x, n = 6, first = 1, show_loops = FALSE,
                             edge.labels = TRUE, edge.label.position = 0.65,
                             minimum = 0.00001, mar = rep(5, 4),
                             layout = "circle", layout_args = list(),
                             cut = 0.01, normalize = TRUE,
                             ask = TRUE, colors, theme = "colorblind", ...) {
  check_class(x, "tna_cliques")
  n_cliques <- length(x$weights)
  size <- attr(x, "size")
  if (n_cliques == 0) {
    warning_("No {size}-cliques were found in the network.")
    return(invisible(NULL))
  }
  colors <- ifelse_(
    missing(colors),
    attr(x, "colors"),
    colors
  )
  labels <- attr(x, "labels")
  max_cliques <- min(first + n - 1L, n_cliques)
  if (interactive()) { # nocov start
    op <- graphics::par(ask = ask)
    on.exit(graphics::par(op))
  } # nocov end
  for (i in seq(first, max_cliques)) {
    clique_weights <- x$weights[[i]]
    directed <- !attr(x, "sum_weights")
    diag(clique_weights) <- ifelse_(
      show_loops,
      diag(clique_weights),
      0
    )
    layout <- check_layout(
      x = clique_weights,
      layout = layout,
      args = layout_args,
      directed = directed
    )
    plot_args <- list(
      input = clique_weights,
      labels = colnames(clique_weights),
      edge.labels = edge.labels,
      edge.label.position = edge.label.position,
      directed = directed,
      mar = mar,
      minimum = minimum,
      theme = theme,
      cut = cut,
      normalize = normalize,
      layout = layout,
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
#' @param colors A `character` vector of color values used for visualizing
#' community assignments.
#' @param method A `character` string naming a community detection method to
#' use for coloring the plot. See [communities()] for details.
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object in which the nodes are colored by community.
#' @examples
#' model <- tna(group_regulation)
#' comm <- communities(model)
#' plot(comm, method = "leading_eigen")
#'
plot.tna_communities <- function(x, colors, method = "spinglass", ...) {
  check_class(x, "tna_communities")
  available_methods <- intersect(
    names(x$assignment),
    names(supported_communities)
  )
  stopifnot_(
    method %in% available_methods,
    "The {.val {method}} method is not available in {.arg x}."
  )
  y <- attr(x, "tna")
  colors <- ifelse_(
    missing(colors),
    default_colors,
    colors
  )
  plot(y, colors = map_to_color(x$assignment[[method]], colors), ...)
}

#' Plot the results of comparing two `tna` models or matrices
#'
#' @export
#' @family patterns
#' @param x A `tna_comparison` object.
#' @param type A `character` string naming the type of plot to produce. The
#' available options are `"heatmap"` (the default), `"scatterplot"`,
#' `"centrality_heatmap"`, and `"weight_density"`.
#' @param population A `"character"` string naming the population for which
#' to produce the heatmaps, i.e, one of `"x"`, `"y"`, or `"difference"` for the
#' differences. Ignored for `type = "scatterplot"`. Defaults to `"diff"`.
#' @param method A `character` string naming the correlation coefficient to
#' use when plotting a scatterplot. The available options are `"pearson"`
#' (the default), `"kendall"`, `"spearman"`, and `"distance"`. The final option
#' is the distance correlation coefficient of
#' Szekely, Rizzo, and Bakirov (2007). See also the `energy` package for
#' further information on this measure.
#' @param name_x An optional `character` string to use as the name of the
#' first population in the plots. The default is `"x"`.
#' @param name_y An optional `character` string to use as the name of the
#' second population in the plots. The default is `"y"`.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @references
#' Szekely, G.J., Rizzo, M.L., and Bakirov, N.K. (2007),
#' Measuring and Testing Dependence by Correlation of Distances,
#' *Annals of Statistics*, Vol. 35 No. 6, pp. 2769-2794.
#' doi:10.1214/009053607000000505
#' @examples
#' model_x <- tna(group_regulation[1:200, ])
#' model_y <- tna(group_regulation[1001:1200, ])
#' comp <- compare(model_x, model_y)
#' plot(comp)
#'
plot.tna_comparison <- function(x, type = "heatmap",
                                population = "difference", method = "pearson",
                                name_x = "x", name_y = "y", ...) {
  check_class(x, "tna_comparison")
  check_string(name_x)
  check_string(name_y)
  type <- check_match(
    type,
    c("heatmap", "scatterplot", "centrality_heatmap", "weight_density")
  )
  if (type == "heatmap") {
    population <- check_match(population, c("x", "y", "difference"))
    weight_col <- switch(
      population,
      x = "weight_x",
      y = "weight_y",
      difference = "raw_difference"
    )
    title <- switch(
      population,
      x = paste0("Heatmap ", name_x),
      y = paste0("Heatmap ", name_y),
      difference = paste0(
        "Difference Matrix Heatmap (", name_x, " vs. ", name_y, ")"
      )
    )
    edges <- x$edge_metrics[, c("source", "target", weight_col)]
    names(edges)[3] <- "value"
    return(create_heatmap(edges, title))
  }
  if (type == "scatterplot") {
    method <- check_match(
      method,
      c("pearson", "kendall", "spearman", "distance")
    )
    edges <- x$edge_metrics[, c("source", "target", "weight_x", "weight_y")]
    metric_idx <- tolower(x$summary_metrics$metric) == method
    corr <- round(x$summary_metrics$value[metric_idx], 3)
    # switch tracking does not seem to work here
    corr_subtitle <- switch( # nocov start
      method,
      pearson = bquote("Pearson's" ~ {rho} ~~ "=" ~~ .(corr)),
      spearman = bquote("Spearman's" ~ {rho} ~~ "=" ~~ .(corr)),
      kendall = bquote("Kendall's" ~ {tau} ~~ "=" ~~ .(corr)),
      distance = paste0("Distance correlation = ", corr)
    ) # nocov end
    out <-
      ggplot2::ggplot(
        edges,
        ggplot2::aes(
          !!rlang::sym("weight_x"),
          !!rlang::sym("weight_y")
        )
      ) +
      ggplot2::geom_point(alpha = 0.6, color = "blue") +
      ggplot2::geom_smooth(
        formula = y ~ x, method = "lm", color = "red", linetype = "dashed"
      ) +
      ggplot2::labs(
        title = paste0("Correlation between ", name_x, " and ", name_y),
        subtitle = corr_subtitle,
        x = paste0("Weights (", name_x, ")"),
        y = paste0("Weights (", name_y, ")")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 10))
    return(out)
  }
  if (type == "centrality_heatmap") {
    out <-
      ggplot2::ggplot(
        x$centrality_differences,
        ggplot2::aes(
          x = !!rlang::sym("centrality"),
          y = !!rlang::sym("state"),
          fill = !!rlang::sym("difference")
        )
      ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(
        low = "blue",
        high = "red",
        mid = "white"
      ) +
      ggplot2::labs(
        title = paste0(
          "Centrality Difference Heatmap (", name_x, " vs ", name_y, ")"
        ),
        x = "Centrality Measure",
        y = "Node"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 12, face = "bold")
      )
    return(out)
  }
  if (type == "weight_density") {
    edges <- x$edge_metrics[, c("source", "target", "weight_x", "weight_y")]
    out <-
      ggplot2::ggplot(
        edges,
        ggplot2::aes(x = !!rlang::sym("weight_x"), color = name_x)
      ) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_density(
        ggplot2::aes(x = !!rlang::sym("weight_y"), color = name_y),
        alpha = 0.5
      ) +
      ggplot2::labs(
        title = "Density Plot of Model Weights",
        x = "Weight",
        y = "Density",
        color = "Model"
      ) +
      ggplot2::theme_minimal()
    return(out)
  }
}

#' Plot the Significant Differences from a Permutation Test
#'
#' @export
#' @param x A `tna_permutation` object.
#' @param colors See [qgraph::qgraph()].
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
plot.tna_permutation <- function(x, colors, ...) {
  check_class(x, "tna_permutation")
  colors <- ifelse_(
    missing(colors),
    attr(x, "colors"),
    colors
  )
  plot_model(
    x$edges$diffs_sig,
    labels = attr(x, "labels"),
    colors = colors,
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
#' model <- tna(group_regulation)
#' cs <- estimate_cs(model, iter = 10)
#' plot(cs)
#'
plot.tna_stability <- function(x, level = 0.05, ...) {
  check_class(x, "tna_stability")
  check_range(level)
  x$detailed_results <- NULL
  x_names <- names(x)
  drop_prop <- attr(x, "drop_prop")
  threshold <- attr(x, "threshold")
  measure_data <- vector(mode = "list", length = length(x))
  cs_subtitle <- character(length(x))
  for (i in seq_along(x)) {
    measure <- x_names[i]
    corr <- x[[measure]]$correlations
    # TODO Check if measure_results has valid dimensions?
    # if (is.null(dim(corr)) || nrow(corr) == 0 || ncol(corr) == 0) {
    #   warning_(
    #     paste0("Warning: No valid data for measure ", measure, ". Skipping.")
    #   )
    #   next
    # }
    means <- apply(corr, 2L, mean, na.rm = TRUE)
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
    colors <- rep("black", length.out = length(unique(x$state)))
  } else if (!is.list(colors) && length(colors) == 1) {
    colors <- rep(colors, length.out = length(unique(x$state)))
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
    idvar = "state",
    ids = x[["state"]],
    times = names(x)[-1L],
    timevar = "name",
    drop = "state",
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
      x, !!rlang::sym("name"), dplyr::desc(!!rlang::sym("state"))
    )
  )
  x$rank <- dplyr::row_number(x)

  ggplot2::ggplot(x) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_col(
      ggplot2::aes(
        fill = !!rlang::sym("state"),
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
      labels = x$state
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
  n_clusters <- length(unique(x$group))
  x$state <- factor(x$state)
  x |>
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
        y = !!rlang::sym("state"),
        color = !!rlang::sym("group"),
        fill = !!rlang::sym("group"),
        group = !!rlang::sym("group")
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
#' @param x A `tna` object. It will be the principal model.
#' @param y A `tna` object. It will be the model subtracted from the
#'   principal model.
#' @param theme See [qgraph::qgraph()].
#' @param palette See [qgraph::qgraph()].
#' @param ... Additional arguments passed to [qgraph::qgraph()].
#' @return A `qgraph` object displaying the difference network between the
#'   two models.
#' @examples
#' model_x <- tna(group_regulation[group_regulation[, 1] == "plan", ])
#' model_y <- tna(group_regulation[group_regulation[, 1] != "plan", ])
#' plot_compare(model_x, model_y)
#'
plot_compare <- function(x, ...) {
  UseMethod("plot_compare")
}

#' @export
#' @rdname plot_compare
plot_compare.tna <- function(x, y, theme = NULL, palette = "colorblind", ...) {
  check_class(x, "tna")
  check_class(y, "tna")
  # TODO check that x and y are comparable
  stopifnot_(
    all(x$labels == y$labels),
    "{.arg x} and {.arg y} must have the same labels."
  )
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
  weights_abs <- abs(x$weights)
  q <- stats::quantile(weights_abs, probs = c(0.2, 0.3))
  plot.tna(
    x = diff,
    pie = pie,
    pieColor = piesign,
    palette = palette,
    theme = theme,
    ...
  )
}

#' Plot a Transition Network Model from a Matrix of Edge Weights
#'
#' @export
#' @param x A square `matrix` of edge weights.
#' @inheritParams plot.tna
#' @keywords internal
#' @return See [plot.tna()].
#' @examples
#' m <- matrix(rexp(25), 5, 5)
#' plot_model(m)
#'
plot_model <- function(x, labels, colors,
                       edge.labels = TRUE, edge.label.position = 0.65,
                       layout = "circle", layout_args = list(),
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
  layout <- check_layout(x, layout, layout_args)
  qgraph::qgraph(
    input = x,
    color = colors,
    edge.labels = edge.labels,
    edge.label.position = edge.label.position,
    labels = labels,
    layout = layout,
    theme = theme,
    mar = mar,
    ...
  )
}

#' Create a mosaic plot of transitions
#'
#' @export
#' @param x A `tna` or a `group_tna` object.
#' @param digits An `integer` that determines the number of digits to use
#' for the chi-square test statistic and the p-value in the plot.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' ftna_model <- ftna(group_regulation)
#' plot_mosaic(ftna_model)
#'
plot_mosaic <- function(x, ...) {
  UseMethod("plot_mosaic")
}

#' @export
#' @rdname plot_mosaic
plot_mosaic.tna <- function(x, digits = 1, ...) {
  # from https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2,
  # Based on the code by Jake Fisher and cpsyctc.
  check_missing(x)
  check_class(x, "tna")
  stopifnot_(
    attr(x, "type") %in% c("frequency", "co-occurrence"),
    "Mosaic plots are supported only for integer-valued weight matrices."
  )
  plot_mosaic_(
    as.table(t(x$weights)),
    digits,
    title = "Mosaic plot of outgoing against incoming transitions:",
    xlab = "Incoming",
    ylab = "Outgoing"
  )
}

plot_mosaic_ <- function(tab, digits, title, xlab, ylab) {
  n <- nrow(tab)
  m <- ncol(tab)
  widths <- c(0, cumsum(apply(tab, 1L, sum))) / sum(tab)
  heights <- apply(tab, 1L, function(y) c(0, cumsum(y / sum(y))))
  d <- data.frame(xmin = rep(0, n * m), xmax = 0, ymin = 0, ymax = 0)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      row <- (i - 1) * m + j
      row_offset <- (i - 1) * n * 0.0025
      col_offset <- (j - 1) * m * 0.0025
      d[row, "xmin"] <- widths[i] + row_offset
      d[row, "xmax"] <- widths[i + 1] + row_offset
      d[row, "ymin"] <- heights[j, i] + col_offset
      d[row, "ymax"] <- heights[j + 1, i] + col_offset
      d[row, "freq"] <- tab[i, j]
    }
  }
  d$row <- rep(dimnames(tab)[[1]], m)
  d$col <- rep(dimnames(tab)[[2]], each = n)
  # TODO suppress for now
  chisq <- suppressWarnings(stats::chisq.test(tab))
  df <- chisq$parameter
  pval <- chisq$p.value
  chisqval <- chisq$statistic
  # stdResids <- chisq$stdres
  d$xcent <- (d$xmin + d$xmax) / 2
  d$ycent <- (d$ymin + d$ymax) / 2
  d$stdres <- as.vector(t(chisq$stdres))
  d$sig <- cut(
    d$stdres,
    breaks = c(-Inf, -4, -2, 0, 2, 4, Inf),
    labels = c("<-4", "-4:-2", "-2:0", "0:2", "2:4", ">4"),
    ordered_result = TRUE
  )
  title_chi <- bquote(
    .(title) ~~
      {chi^2}[.(df)] ~ " = " ~
      .(round(chisqval, digits)) * ", p =" ~ .(format.pval(pval, digits))
  )
  out <-
    ggplot2::ggplot(
      d,
      ggplot2::aes(
        xmin = !!rlang::sym("xmin"),
        xmax = !!rlang::sym("xmax"),
        ymin = !!rlang::sym("ymin"),
        ymax = !!rlang::sym("ymax"),
        fill = !!rlang::sym("sig"),
        linetype = !!rlang::sym("sig")
      )
    ) +
    ggplot2::geom_rect(color = "black", show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      name = "Standardized\nresidual",
      values = c(
        "#D33F6A", "#E495A5", "#E2E2E2", "#E2E2E2", "#9DA8E2", "#4A6FE3"
      ),
      guide = ggplot2::guide_legend(reverse = TRUE),
      drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      name = "Standardized\nresidual",
      values = c(2, 2, 2, 1, 1, 1),
      guide = ggplot2::guide_legend(reverse = TRUE),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = unique(d$xcent),
      labels = dimnames(tab)[[1]],
      expand = c(0.01, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = d$ycent[d$xmin == 0],
      labels = dimnames(tab)[[2]],
      expand = c(0.01, 0)
    ) +
    ggplot2::ggtitle(title_chi) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0.40)
    ) +
    ggplot2::labs(x = xlab, y = ylab)
  out
}


#' Create a heatmap from edgelist data
#'
#' @param data A `data.frame` with source and target columns and edge weights.
#' @param title A `character` string giving the plot title.
#' @noRd
create_heatmap <- function(data, title) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!rlang::sym("target"),
      y = !!rlang::sym("source"),
      fill = !!rlang::sym("value")
  )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      limits = c(-1, 1),
      na.value = "grey50"
    ) +
    ggplot2::labs(title = title, x = "Target", y = "Source") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 12, face = "bold")
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
  invisible(lapply(x, hist.tna, ...))
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
plot.group_tna_centralities <- function(x, reorder = TRUE, ncol = 3,
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
#' plot(cliq, ask = F)
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
  n <- length(x)
  colors <- ifelse_(
    missing(colors),
    replicate(n, default_colors, simplify = FALSE),
    ifelse_(
      is.vector(colors) && is.atomic(colors),
      replicate(n, colors, simplify = FALSE),
      colors
    )
  )
  if (is.null(title) ||
    (is.vector(title) && is.atomic(title) && (length(title) == 1))) {
    title <- replicate(n, title, simplify = FALSE)
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
  invisible(lapply(x, plot.tna_stability, ...))
}

#' Plot the difference network between two clusters
#'
#' @export
#' @family clusters
#' @param x A `group_tna` object.
#' @param i An `integer` index or the name of the principal cluster as a
#' `character` string.
#' @param j An `integer` index or the name of the secondary cluster as a
#' `character` string.
#' @param ... Additional arguments passed to [plot_compare.tna()].
#' @return A `qgraph` object displaying the difference network between the
#'   two clusters
#' @examples
#' model <- group_model(engagement_mmm)
#' plot_compare(model)
#'
plot_compare.group_tna <- function(x, i = 1L, j = 2L, ...) {
  check_missing(x)
  check_class(x, "group_tna")
  check_clusters(x, i, j)
  plot_compare(x = x[[i]], y = x[[j]], ...)
}

#' Plot state frequencies as a mosaic between two groups
#'
#' @export
#' @family clusters
#' @param x A `tna_data` object.
#' @param digits An `integer` that determines the number of digits to use
#' for the chi-square test statistic and the p-value in the plot.
#' @param group A `character` string giving the column name of the (meta) data
#' to contrast the frequencies with or a vector of group indicators with the
#' the same length as the number of rows in the sequence data.
#' @param label An optional `character` string that specifies a label for the
#' grouping variable when `group` is not a column name of the data.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' d <- data.frame(
#'   time = rep(1:5, rep = 4),
#'   group = rep(1:4, each = 5),
#'   event = sample(LETTERS[1:3], 20, replace = TRUE)
#' )
#' sequence_data <- prepare_data(
#'   d,
#'   time = "time",
#'   actor = "group",
#'   action = "event"
#' )
#' plot_mosaic(sequence_data, group = "group")
#'
plot_mosaic.tna_data <- function(x, group, label = "Group", digits = 1, ...) {
  check_missing(x)
  check_class(x, "tna_data")
  check_missing(group)
  check_string(label)
  group_len <- length(group)
  stopifnot_(
    group_len == nrow(x$sequence_data) || group_len == 1L,
    "Argument {.arg group} must be of length one or the same length as the
     number of rows/sequences in {.arg x}."
  )
  if (group_len == 1L) {
    stopifnot_(
      group %in% names(x$meta_data),
      "Argument {.arg group} must be a column name of the input data
       when of length one."
    )
    label <- group
    group <- x$meta_data[[group]]
  }
  group <- ifelse_(
    is.factor(group),
    group,
    factor(group)
  )
  wide <- cbind(x$sequence_data, group)
  names(wide) <- c(names(x$sequence_data), label)
  long <- wide |>
    tidyr::pivot_longer(cols = !(!!rlang::sym(label))) |>
    tidyr::drop_na()
  tab <- table(long[[label]], long$value)
  plot_mosaic_(
    tab,
    digits,
    title = paste0("State frequency by ", label),
    xlab = label,
    ylab = "State"
  )
}

#' Plot state frequencies as a mosaic between two groups
#'
#' @export
#' @family clusters
#' @param x A `group_tna` object.
#' @param label An optional `character` string that can be provided to specify
#' the grouping factor name if `x` was not constructed using a column name of
#' the original data.
#' @inheritParams plot_mosaic.tna_data
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @examples
#' model <- group_model(engagement_mmm)
#' plot_mosaic(model)
#'
plot_mosaic.group_tna <- function(x, label, digits = 1, ...) {
  check_class(x, "group_tna")
  cols <- attr(x, "cols")
  labels <- x[[1L]]$labels
  levs <- attr(x, "levels")
  groups <- attr(x, "groups")
  group_var <- attr(x, "group_var")
  data <- dplyr::bind_rows(
    lapply(x, function(y) y$data[, cols])
  )
  data[[group_var]] <- unlist(groups)
  label <- ifelse_(
    !missing(label),
    label,
    ifelse_(
      group_var == ".group",
      "Cluster",
      group_var
    )
  )
  check_string(label)
  names(data) <- c(names(data)[-ncol(data)], label)
  long <- data |>
    tidyr::pivot_longer(cols = !(!!rlang::sym(label))) |>
    dplyr::filter(!is.na(!!rlang::sym("value")))
  use_na <- ifelse_(attr(x, "na.rm"), "no", "ifany")
  tab <- table(long[[label]], long$value, useNA = use_na)
  dimnames(tab) <- list(levs, labels)
  plot_mosaic_(
    tab,
    digits,
    title = paste0("State frequency by ", label),
    xlab = label,
    ylab = "State"
  )
}
